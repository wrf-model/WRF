!WRF:MEDIATION_LAYER:PHYSICS
!
MODULE module_surface_driver
CONTAINS

   SUBROUTINE surface_driver(                                         &
     &           acgrdflx,achfx,aclhf                                 &
     &          ,acsnom,acsnow,akhs,akms,albedo,br,canwat             &
     &          ,chklowq,dt,dx,dz8w,dzs,glw                           &
     &          ,grdflx,gsw,swdown,gz1oz0,hfx,ht,ifsnow,isfflx        &
     &          ,fractional_seaice                                    &
     &          ,isltyp,itimestep,julian_in,ivgtyp,lowlyr,mavail,rmol &
     &          ,num_soil_layers,p8w,pblh,pi_phy,pshltr,psih          &
#if (NMM_CORE==1)
     &          ,psim,p_phy,q10,q2,qfx,taux,tauy,qsfc,qshltr,qz0      &
#else
     &          ,psim,p_phy,q10,q2,qfx,qsfc,qshltr,qz0                &
#endif
     &          ,raincv,rho,sfcevp,sfcexc,sfcrunoff                   &
     &          ,smois,smstav,smstot,snoalb,snow,snowc,snowh,stepbl   &
     &          ,smcrel                                               &
     &          ,th10,th2,thz0,th_phy,tmn,tshltr,tsk,tslb             &
     &          ,tyr,tyra,tdly,tlag,lagday,nyear,nday,tmn_update,yr   &
     &          ,t_phy,u10,udrunoff,ust,uz0,u_frame,u_phy,v10,vegfra  &
     &          ,vz0,v_frame,v_phy,warm_rain,wspd,xice,xland,z,znt,zs &
#if (NMM_CORE==1)
     &          ,xicem,isice,iswater,ct,tke_myj,sfenth                &
#else
     &          ,xicem,isice,iswater,ct,tke_myj                       &
#endif
     &          ,albbck,embck,lh,sh2o,shdmax,shdmin,z0                &
     &          ,flqc,flhc,psfc,sst,sstsk,dtw,sst_update,sst_skin,t2,emiss               &
     &          ,sf_sfclay_physics,sf_surface_physics,ra_lw_physics   &
     &          ,landusef,soilctop,soilcbot,ra,rs,nlcat,nscat,vegf_px & ! PX-LSM
     &          ,snowncv, anal_interval, lai, pxlsm_smois_init        & ! PX-LSM
     &          ,pxlsm_soil_nudge                                     & ! PX-LSM
#if ( EM_CORE==1)
     &          ,ch,tsq,qsq,cov                                       & ! MYNN
#endif
            !  Optional urban
     &          ,slope_rad,topo_shading,shadowmask                    & !I solar
     &          ,swnorm,slope,slp_azi                                 & !I solar
     &          ,declin,solcon,coszen,hrang,xlat_urb2d                & !I solar/urban
     &          ,num_roof_layers, num_wall_layers                     & !I urban
     &          ,num_road_layers, dzr, dzb, dzg                       & !I urban
     &          ,tr_urb2d,tb_urb2d,tg_urb2d,tc_urb2d,qc_urb2d         & !H urban
     &          ,uc_urb2d                                             & !H urban
     &          ,xxxr_urb2d,xxxb_urb2d,xxxg_urb2d,xxxc_urb2d          & !H urban
     &          ,trl_urb3d,tbl_urb3d,tgl_urb3d                        & !H urban
     &          ,sh_urb2d,lh_urb2d,g_urb2d,rn_urb2d,ts_urb2d          & !H urban
     &          ,frc_urb2d, utype_urb2d                               & !H urban
     &          ,cmr_sfcdif,chr_sfcdif,cmc_sfcdif,chc_sfcdif          &
     &          , ids,ide,jds,jde,kds,kde                             &
     &          , ims,ime,jms,jme,kms,kme                             &
     &          , i_start,i_end,j_start,j_end,kts,kte,num_tiles       &
             !  Optional moisture tracers
     &           ,qv_curr, qc_curr, qr_curr                           &
     &           ,qi_curr, qs_curr, qg_curr                           &
             !  Optional moisture tracer flags
     &           ,f_qv,f_qc,f_qr                                      &
     &           ,f_qi,f_qs,f_qg                                      &
             !  Other optionals (more or less em specific)
     &          ,capg,hol,mol                                         &
     &          ,rainncv,rainbl,regime,thc                            &
     &          ,qsg,qvg,qcg,soilt1,tsnav                             &
     &          ,smfr3d,keepfr3dflag,dew                              &
             !  Other optionals (more or less nmm specific)
     &          ,potevp,snopcx,soiltb,sr                              &
             !  Optional observation PX LSM surface nudging
     &          ,t2_ndg_old, q2_ndg_old, t2_ndg_new, q2_ndg_new       &
     &          ,sn_ndg_old, sn_ndg_new                               &
     &          ,t2obs, q2obs                                         &
             !  Optional observation nudging
     &          ,uratx,vratx,tratx                                    &
             !  Optional simple oml model
     &          ,omlcall,oml_hml0,oml_gamma                           &
     &          ,tml,t0ml,hml,h0ml,huml,hvml,f,tmoml                  &
     &          ,ustm,ck,cka,cd,cda,isftcflx,iz0tlnd                  &
     &         ,isurban, mminlu                                       &
     &          ,snotime                                              &
     &           ,rdlai2d                                             &
     &          ,usemonalb                                            &
     &          ,noahres                                              &
             !  Optional adaptive time step
     &          ,bldt,curr_secs,adapt_step_flag                       &
         ! Optional urban with BEP
     &          ,sf_urban_physics,gmt,xlat,xlong,julday               &
     &          ,num_urban_layers                                     & !multi-layer urban
     &          ,trb_urb4d,tw1_urb4d,tw2_urb4d,tgb_urb4d              & !multi-layer urban
     &          ,tlev_urb3d,qlev_urb3d                                & !multi-layer urban
     &          ,tw1lev_urb3d,tw2lev_urb3d                            & !multi-layer urban
     &          ,tglev_urb3d,tflev_urb3d                              & !multi-layer urban
     &          ,sf_ac_urb3d,lf_ac_urb3d,cm_ac_urb3d                  & !multi-layer urban
     &          ,sfvent_urb3d,lfvent_urb3d                            & !multi-layer urban 
     &          ,sfwin1_urb3d,sfwin2_urb3d                            & !multi-layer urban       
     &          ,sfw1_urb3d,sfw2_urb3d,sfr_urb3d,sfg_urb3d            & !multi-layer urban
     &          ,a_u_bep,a_v_bep,a_t_bep,a_q_bep                      &
     &          ,b_u_bep,b_v_bep,b_t_bep,b_q_bep                      &
     &          ,sf_bep,vl_bep                                        &
     &          ,a_e_bep,b_e_bep,dlg_bep                              &
     &          ,dl_u_bep                                             &                          
         ! Optional urban Bep end
     &                                                             )
              
#if ( ! NMM_CORE == 1 )
   USE module_state_description, ONLY : SFCLAYSCHEME              &
                                       ,MYJSFCSCHEME              &
                                       ,QNSESFCSCHEME             &
                                       ,GFSSFCSCHEME              &
                                       ,PXSFCSCHEME               &
                                       ,SLABSCHEME                &
                                       ,LSMSCHEME                 &
                                       ,RUCLSMSCHEME              &
                                       ,PXLSMSCHEME               &
                                       ,MYNNSFCSCHEME             
#else
   USE module_state_description, ONLY : SFCLAYSCHEME              &
                                       ,MYJSFCSCHEME              &
                                       ,QNSESFCSCHEME             &
                                       ,GFSSFCSCHEME              &
                                       ,PXSFCSCHEME               &
                                       ,SLABSCHEME                &
                                       ,LSMSCHEME                 &
                                       ,RUCLSMSCHEME              &
                                       ,PXLSMSCHEME               &
                                       ,GFDLSFCSCHEME             &
                                       ,GFDLSLAB 


#endif
   USE module_model_constants
! *** add new modules of schemes here

   USE module_sf_sfclay
   USE module_sf_myjsfc
   USE module_sf_qnsesfc
   USE module_sf_gfs
   USE module_sf_noahdrv
   USE module_sf_ruclsm
   USE module_sf_pxsfclay
   USE module_sf_pxlsm
#if ( EM_CORE==1)
   USE module_sf_mynn
   USE module_sf_oml
#endif

#if ( NMM_CORE == 1 )
   USE module_sf_gfdl
#endif

   USE module_sf_slab
!
   USE module_sf_sfcdiags
   USE module_sf_sfcdiags_ruclsm
   USE module_sf_sstskin
   USE module_sf_tmnupdate
!
!  This driver calls subroutines for the surface parameterizations.
!
!  surface layer: (between surface and pbl)
!      1. sfclay
!      2. myjsfc
!      7. Pleim surface layer
!      5. MYNN surface layer
!  surface: ground temp/lsm scheme:
!      1. slab
!      2. Noah LSM
!      7. Pleim-Xiu LSM
!
!  surface: ground temp/lsm scheme for urban:
!      2.  BEP
!
!  ocean mixed layer model
!      omlcall = 1
!------------------------------------------------------------------
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
! Theta      potential temperature (K)
! Qv         water vapor mixing ratio (kg/kg)
! Qc         cloud water mixing ratio (kg/kg)
! Qr         rain water mixing ratio (kg/kg)
! Qi         cloud ice mixing ratio (kg/kg)
! Qs         snow mixing ratio (kg/kg)
!-----------------------------------------------------------------
!-- itimestep     number of time steps
!-- GLW           downward long wave flux at ground surface (W/m^2)
!-- GSW           net short wave flux at ground surface (W/m^2)
!-- SWDOWN        downward short wave flux at ground surface (W/m^2)
!-- EMISS         surface emissivity (between 0 and 1)
!-- TSK           surface temperature (K)
!-- TMN           soil temperature at lower boundary (K)
!-- TYR           annual mean surface temperature of previous year (K)
!-- TYRA          accumulated surface temperature in the current year (K)
!-- TLAG          mean surface temperature of previous 140 days (K)
!-- TDLY          accumulated daily mean surface temperature of the current day (K)
!-- XLAND         land mask (1 for land, 2 for water)
!-- ZNT           time-varying roughness length (m)
!-- Z0            background roughness length (m)
!-- MAVAIL        surface moisture availability (between 0 and 1)
!-- UST           u* in similarity theory (m/s)
!-- MOL           T* (similarity theory) (K)
!-- HOL           PBL height over Monin-Obukhov length
!-- PBLH          PBL height (m)
!-- CAPG          heat capacity for soil (J/K/m^3)
!-- THC           thermal inertia (Cal/cm/K/s^0.5)
!-- SNOWC         flag indicating snow coverage (1 for snow cover)
!-- HFX           net upward heat flux at the surface (W/m^2)
!-- QFX           net upward moisture flux at the surface (kg/m^2/s)
!-- TAUX          RHO*U**2 for ocean coupling
!-- TAUY          RHO*U**2 for ocean coupling
!-- LH            net upward latent heat flux at surface (W/m^2)
!-- REGIME        flag indicating PBL regime (stable, unstable, etc.)
!-- tke_myj       turbulence kinetic energy from Mellor-Yamada-Janjic (MYJ) (m^2/s^2)
!-- akhs          sfc exchange coefficient of heat/moisture from MYJ
!-- akms          sfc exchange coefficient of momentum from MYJ
!-- thz0          potential temperature at roughness length (K)
!-- uz0           u wind component at roughness length (m/s)
!-- vz0           v wind component at roughness length (m/s)
!-- qsfc          specific humidity at lower boundary (kg/kg)
!-- uratx         ratio of u over u10 (Added for obs-nudging)
!-- vratx         ratio of v over v10 (Added for obs-nudging)
!-- tratx         ratio of t over th2 (Added for obs-nudging)
!-- u10           diagnostic 10-m u component from surface layer
!-- v10           diagnostic 10-m v component from surface layer
!-- th2           diagnostic 2-m theta from surface layer and lsm
!-- t2            diagnostic 2-m temperature from surface layer and lsm
!-- q2            diagnostic 2-m mixing ratio from surface layer and lsm
!-- tshltr        diagnostic 2-m theta from MYJ
!-- th10          diagnostic 10-m theta from MYJ
!-- qshltr        diagnostic 2-m specific humidity from MYJ
!-- q10           diagnostic 10-m specific humidity from MYJ
!-- lowlyr        index of lowest model layer above ground
!-- rr            dry air density (kg/m^3)
!-- u_phy         u-velocity interpolated to theta points (m/s)
!-- v_phy         v-velocity interpolated to theta points (m/s)
!-- th_phy        potential temperature (K)
!-- moist         moisture array (4D - last index is species) (kg/kg)
!-- p_phy         pressure (Pa)
!-- pi_phy        exner function (dimensionless)
!-- pshltr        diagnostic shelter (2m) pressure from MYJ (Pa)
!-- p8w           pressure at full levels (Pa)
!-- t_phy         temperature (K)
!-- dz8w          dz between full levels (m)
!-- z             height above sea level (m)
!-- DX            horizontal space interval (m)
!-- DT            time step (second)
!-- PSFC          pressure at the surface (Pa)
!-- SST           sea-surface temperature (K)
!-- SSTSK         skin sea-surface temperature (K)
!-- DTW           warm layer temp diff (K)
!-- TSLB
!-- ZS
!-- DZS
!-- num_soil_layers number of soil layer
!-- IFSNOW      ifsnow=1 for snow-cover effects
!-- omlcall       whether to call simple ocean mixed layer model from slab (1 = use oml)
!-- oml_hml0      initial mixed layer depth (if real-data not available, default 50 m)
!-- oml_gamma     lapse rate below mixed layer in ocean (default 0.14 K m-1)
!-- ck            enthalpy exchange coeff at 10 meters
!-- cd            momentum exchange coeff at 10 meters
!-- cka           enthalpy exchange coeff at the lowest model level
!-- cda           momentum exchange coeff at the lowest model level
!!!!!!!!!!!!!!
!
!
!-- LANDUSEF     Landuse fraction                      ! P-X LSM
!-- SOILCTOP     Top soil fraction                     ! P-X LSM
!-- SOILCBOT     Bottom soil fraction                  ! P-X LSM
!-- RA           Aerodynamic resistence                        ! P-X LSM
!-- RS           Stomatal resistence                   ! P-X LSM
!-- NLCAT        Number of landuse categories          ! P-X LSM
!-- NSCAT        Number of soil categories             ! P-X LSM
!-- ch - drag coefficient for heat/moisture            ! MYNN LSM

!
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
!******************************************************************
!------------------------------------------------------------------

   INTEGER, INTENT(IN) ::                                             &
     &           ids,ide,jds,jde,kds,kde                              &
     &          ,ims,ime,jms,jme,kms,kme                              &
     &          ,kts,kte,num_tiles

   INTEGER, INTENT(IN)::   FRACTIONAL_SEAICE

   INTEGER, INTENT(IN)::   NLCAT
   INTEGER, INTENT(IN)::   NSCAT

   INTEGER, INTENT(IN) :: sf_sfclay_physics, sf_surface_physics,      &
                          sf_urban_physics,ra_lw_physics, sst_update
   INTEGER, INTENT(IN),OPTIONAL :: sst_skin, tmn_update

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
     &           i_start,i_end,j_start,j_end

   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT )::  ISLTYP
   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   IVGTYP
   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   LOWLYR
   INTEGER, INTENT(IN )::   IFSNOW
   INTEGER, INTENT(IN )::   ISFFLX
   INTEGER, INTENT(IN )::   ITIMESTEP
   INTEGER, INTENT(IN )::   NUM_SOIL_LAYERS
   REAL,    INTENT(IN ),OPTIONAL ::   JULIAN_in
   INTEGER, INTENT(IN )::   LAGDAY
   INTEGER, INTENT(IN )::   STEPBL
   INTEGER, INTENT(IN )::   ISICE
   INTEGER, INTENT(IN )::   ISWATER
   INTEGER, INTENT(IN ), OPTIONAL :: ISURBAN
   CHARACTER(LEN=*), INTENT(IN ), OPTIONAL :: MMINLU
   LOGICAL, INTENT(IN )::   WARM_RAIN
   INTEGER, INTENT(INOUT ),OPTIONAL ::   NYEAR
   INTEGER, INTENT(INOUT ),OPTIONAL ::   NDAY
   INTEGER, INTENT(IN ),OPTIONAL ::   YR
   REAL , INTENT(IN )::   U_FRAME
   REAL , INTENT(IN )::   V_FRAME
#if (NMM_CORE==1)
   real , intent(IN )::   SFENTH
#endif
   REAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), INTENT(INOUT)::   SMOIS
   REAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), INTENT(INOUT)::   TSLB
   REAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), INTENT(OUT)  ::   SMCREL
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   GLW
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   GSW,SWDOWN
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   HT
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   RAINCV
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   SST
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT ),OPTIONAL ::   SSTSK
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT ),OPTIONAL ::   DTW
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   TMN
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT ),OPTIONAL ::   TYR
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT ),OPTIONAL ::   TYRA
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT ),OPTIONAL ::   TDLY
   REAL, DIMENSION( ims:ime , 1:lagday , jms:jme ), INTENT(INOUT ),OPTIONAL ::   TLAG
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   VEGFRA
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT )::   XICE
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   XLAND
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   XICEM
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   MAVAIL
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   SNOALB
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   ACSNOW
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   SNOTIME
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   AKHS
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   AKMS
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   ALBEDO
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   CANWAT

   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   GRDFLX
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   HFX
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   RMOL
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   PBLH
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   Q2
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   QFX
#if (NMM_CORE==1)
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT):: TAUX
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT):: TAUY
#endif
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   QSFC
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   QZ0
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SFCRUNOFF
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SMSTAV
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SMSTOT
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SNOW
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SNOWC
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SNOWH
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   TH2
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   THZ0
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   TSK
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   UDRUNOFF
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   UST
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   UZ0
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   VZ0
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   WSPD
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   ZNT
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   BR
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   CHKLOWQ
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   GZ1OZ0
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   PSHLTR
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   PSIH
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   PSIM
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   Q10
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   QSHLTR
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   TH10
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   TSHLTR
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   U10
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   V10
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   PSFC
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   ACSNOM
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   SFCEVP
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT),OPTIONAL ::   ACHFX
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT),OPTIONAL ::   ACLHF
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT),OPTIONAL ::   ACGRDFLX
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   SFCEXC
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   FLHC
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   FLQC
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::   CT
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   DZ8W
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   P8W
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   PI_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   P_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   RHO
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   TH_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   T_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   U_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   V_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   Z

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::   TKE_MYJ
   REAL, DIMENSION(1:num_soil_layers), INTENT(IN)::   DZS
   REAL, DIMENSION(1:num_soil_layers), INTENT(IN)::   ZS
   REAL, INTENT(IN )::   DT
   REAL, INTENT(IN )::   DX
   REAL,       INTENT(IN   ),OPTIONAL    ::     bldt
   REAL,       INTENT(IN   ),OPTIONAL    ::     curr_secs
   LOGICAL,    INTENT(IN   ),OPTIONAL    ::     adapt_step_flag

!  arguments for NCAR surface physics

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT )::   ALBBCK  ! INOUT needed for NMM
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT )::   EMBCK
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT )::   LH
   REAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), INTENT(INOUT)::   SH2O
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   SHDMAX
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   SHDMIN
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT )::   Z0

! Variables for multi-layer UCM
   REAL, OPTIONAL, INTENT(IN  )   ::                                   GMT 
   INTEGER, OPTIONAL, INTENT(IN  ) ::                               JULDAY
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )        ::XLAT, XLONG
   INTEGER, INTENT(IN )::   NUM_URBAN_LAYERS
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: trb_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw1_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw2_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tgb_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tlev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: qlev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw1lev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw2lev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tglev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tflev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: lf_ac_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: sf_ac_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: cm_ac_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: sfvent_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: lfvent_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfwin1_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfwin2_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfw1_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfw2_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfr_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfg_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_u_bep   !Implicit momemtum component X-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_v_bep   !Implicit momemtum component Y-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_t_bep   !Implicit component pot. temperature
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_e_bep   !Implicit component TKE
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_q_bep   !Implicit component TKE
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_u_bep   !Explicit momentum component X-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_v_bep   !Explicit momentum component Y-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_t_bep   !Explicit component pot. temperature
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_e_bep   !Explicit component TKE
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_q_bep   !Explicit component TKE
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::vl_bep    !Fraction air volume in grid cell
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::dlg_bep   !Height above ground
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::sf_bep  !Fraction air at the face of grid cell
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::dl_u_bep  !Length scale

! Optional
!
!  arguments for Ocean Mixed Layer Model
   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL, INTENT(INOUT )::   TML, T0ML, HML, H0ML, HUML, HVML
   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL, INTENT(IN    )::   F, TMOML
   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL, INTENT(OUT   )::   CK, CKA, CD, CDA, USTM

#if ( EM_CORE==1)
   REAL, DIMENSION( ims:ime , jms:jme ), &
        &OPTIONAL, INTENT(INOUT   ):: ch
   
   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ), &
        &OPTIONAL, INTENT(IN   ):: tsq,qsq,cov
#endif


   INTEGER, OPTIONAL, INTENT(IN )::   slope_rad, topo_shading
   INTEGER, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN):: shadowmask
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT):: swnorm
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN):: slope,slp_azi

   INTEGER, OPTIONAL, INTENT(IN )::   ISFTCFLX,IZ0TLND
   INTEGER, OPTIONAL, INTENT(IN )::   OMLCALL
   REAL   , OPTIONAL, INTENT(IN )::   OML_HML0
   REAL   , OPTIONAL, INTENT(IN )::   OML_GAMMA
!
!  Observation nudging
!
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(OUT)::   uratx  !Added for obs-nudging
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(OUT)::   vratx  !Added for obs-nudging
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(OUT)::   tratx  !Added for obs-nudging
!
!  PX LSM Surface Grid Analysis nudging
!
   INTEGER, OPTIONAL, INTENT(IN)    :: pxlsm_smois_init, pxlsm_soil_nudge, ANAL_INTERVAL
   REAL, DIMENSION( ims:ime, NLCAT, jms:jme ) , OPTIONAL, INTENT(INOUT)::   LANDUSEF
   REAL, DIMENSION( ims:ime, NSCAT, jms:jme ) , OPTIONAL, INTENT(INOUT)::   SOILCTOP, SOILCBOT
   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL, INTENT(INOUT)::   VEGF_PX
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   RA
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   RS
   REAL, DIMENSION( ims:ime, jms:jme ) , OPTIONAL, INTENT(INOUT)::   LAI
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(OUT)::   T2OBS
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(OUT)::   Q2OBS

   REAL,       DIMENSION( ims:ime,  jms:jme ),                           &
               OPTIONAL, INTENT(INOUT)    ::      t2_ndg_old,            &
                                                  q2_ndg_old,            &
                                                  t2_ndg_new,            &
                                                  q2_ndg_new,            &
                                                  sn_ndg_old,            &
                                                  sn_ndg_new
!
!
! Flags relating to the optional tendency arrays declared above
! Models that carry the optional tendencies will provdide the
! optional arguments at compile time; these flags all the model
! to determine at run-time whether a particular tracer is in
! use or not.
!
   LOGICAL, INTENT(IN), OPTIONAL ::                             &
                                                      f_qv      &
                                                     ,f_qc      &
                                                     ,f_qr      &
                                                     ,f_qi      &
                                                     ,f_qs      &
                                                     ,f_qg

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         OPTIONAL, INTENT(INOUT) ::                              &
                      ! optional moisture tracers
                      ! 2 time levels; if only one then use CURR
                      qv_curr, qc_curr, qr_curr                  &
                     ,qi_curr, qs_curr, qg_curr
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN)   ::   snowncv
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   capg
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   emiss
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   hol
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   mol
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   regime
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN )::     rainncv
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   RAINBL
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   t2
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN )::     thc
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   qsg
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   qvg
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   qcg
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   dew
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   soilt1
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   tsnav
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   potevp ! NMM LSM
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   snopcx ! NMM LSM
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   soiltb ! NMM LSM
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT)::   sr ! NMM and RUC LSM
   REAL, DIMENSION( ims:ime, 1:num_soil_layers, jms:jme ), OPTIONAL, INTENT(INOUT)::   smfr3d
   REAL, DIMENSION( ims:ime, 1:num_soil_layers, jms:jme ), OPTIONAL, INTENT(INOUT)::   keepfr3dflag

   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT), OPTIONAL  ::   NOAHRES

!  LOCAL  VAR

   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ) ::v_phytmp
   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ) ::u_phytmp

   REAL,       DIMENSION( ims:ime, jms:jme )          ::  ZOL

   REAL,       DIMENSION( ims:ime, jms:jme )          ::          &
                                                             QGH, &
                                                             CHS, &
                                                             CPM, &
                                                            CHS2, &
                                                            CQS2

   REAL    :: DTMIN,DTBL
!
   INTEGER :: i,J,K,NK,jj,ij,n
   INTEGER :: gfdl_ntsflg
   LOGICAL :: radiation, myj, frpcpn, isisfc
   LOGICAL, INTENT(in), OPTIONAL :: rdlai2d
   LOGICAL, INTENT(in), OPTIONAL :: usemonalb
   REAL    :: julian
   REAL    :: total_depth,mid_point_depth
   REAL    :: tconst,tprior,tnew,yrday,deltat
   REAL    :: SWSAVE
   REAL,       DIMENSION( ims:ime, jms:jme )          ::  GSWSAVE
!-------------------------------------------------
! urban related variables are added to declaration
!-------------------------------------------------
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMC_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHC_SFCDIF
     REAL, OPTIONAL, INTENT(IN) :: DECLIN, SOLCON
     REAL, OPTIONAL , DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: COSZEN
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: HRANG
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: XLAT_URB2D  !urban
     INTEGER,  INTENT(IN) :: num_roof_layers                         !urban
     INTEGER,  INTENT(IN) :: num_wall_layers                         !urban
     INTEGER,  INTENT(IN) :: num_road_layers                         !urban
     REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(IN) :: DZR          !urban
     REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(IN) :: DZB          !urban
     REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(IN) :: DZG          !urban

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: TR_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: TB_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: TG_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: TC_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: QC_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: UC_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: XXXR_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: XXXB_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: XXXG_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: XXXC_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), &       !urban
           INTENT(INOUT)  :: TRL_URB3D                                 !urban
     REAL, OPTIONAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), &       !urban
           INTENT(INOUT)  :: TBL_URB3D                                 !urban
     REAL, OPTIONAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), &       !urban
           INTENT(INOUT)  :: TGL_URB3D                                 !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: SH_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: LH_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: G_URB2D  !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: RN_URB2D !urban
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT):: TS_URB2D !urban
!
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: FRC_URB2D  !urban
     INTEGER, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: UTYPE_URB2D  !urban

     REAL,  DIMENSION( ims:ime, jms:jme )  :: PSIM_URB2D  !urban local var
     REAL,  DIMENSION( ims:ime, jms:jme )  :: PSIH_URB2D  !urban local var
     REAL,  DIMENSION( ims:ime, jms:jme )  :: GZ1OZ0_URB2D  !urban local var
!m     REAL, DIMENSION( ims:ime, jms:jme ) :: AKHS_URB2D  !urban local var
     REAL,  DIMENSION( ims:ime, jms:jme )  :: AKMS_URB2D  !urban local var
     REAL,  DIMENSION( ims:ime, jms:jme )  :: U10_URB2D   !urban local var
     REAL,  DIMENSION( ims:ime, jms:jme )  :: V10_URB2D   !urban local var
     REAL,  DIMENSION( ims:ime, jms:jme )  :: TH2_URB2D   !urban local var
     REAL,  DIMENSION( ims:ime, jms:jme )  :: Q2_URB2D    !urban local var
     REAL,  DIMENSION( ims:ime, jms:jme )  :: UST_URB2D  !urban local var

!
     REAL, DIMENSION( ims:ime, jms:jme ) :: HFX_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: QFX_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: LH_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: QSFC_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: TSK_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: ZNT_SEA

     REAL, DIMENSION( ims:ime, jms:jme ) :: CHS_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: CHS2_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: CQS2_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: CPM_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: FLHC_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: FLQC_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: QGH_SEA
!
     REAL, DIMENSION( ims:ime, jms:jme ) :: PSIH_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: PBLH_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: RMOL_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: UST_SEA
     REAL, DIMENSION( ims:ime, jms:jme ) :: QZ0_SEA
!
   REAL :: xice_threshold
!


!------------------------------------------------------------------
   CHARACTER*256 :: message
   REAL    :: next_bl_time
   LOGICAL :: run_param
   LOGICAL :: do_adapt
!
!
!------------------------------------------------------------------
!


  if (sf_sfclay_physics .eq. 0) return
! if (sf_sfclay_physics .eq. 0 .or. sf_surface_physics.eq.0) return

  if ( fractional_seaice == 0 ) then
     xice_threshold = 0.5
  else if ( fractional_seaice == 1 ) then
     xice_threshold = 0.02
  endif


  v_phytmp = 0.
  u_phytmp = 0.
  ZOL = 0.
  QGH = 0.
  CHS = 0.
  CPM = 0.
  CHS2 = 0.
  DTMIN = 0.
  DTBL = 0.

! RAINBL in mm (Accumulation between PBL calls)

  IF ( PRESENT( rainncv ) .AND. PRESENT( rainbl ) ) THEN
    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij, i, j, k )
    DO ij = 1 , num_tiles
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         RAINBL(i,j) = RAINBL(i,j) + RAINCV(i,j) + RAINNCV(i,j)
         RAINBL(i,j) = MAX (RAINBL(i,j), 0.0)
      ENDDO
      ENDDO
    ENDDO
    !$OMP END PARALLEL DO
  ELSE IF ( PRESENT( rainbl ) ) THEN
    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij, i, j, k )
    DO ij = 1 , num_tiles
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         RAINBL(i,j) = RAINBL(i,j) + RAINCV(i,j)
         RAINBL(i,j) = MAX (RAINBL(i,j), 0.0)
      ENDDO
      ENDDO
    ENDDO
    !$OMP END PARALLEL DO
  ENDIF
! Update SST
  IF (sst_update .EQ. 1) THEN
    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij, i, j, k )
    DO ij = 1 , num_tiles
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)

         IF ( FRACTIONAL_SEAICE == 1 ) then
            IF ( ( XICE(I,J) .NE. XICEM(I,J) ) .AND. ( XICEM(I,J) .GT. XICE_THRESHOLD ) ) THEN
               ! Fractional values of ALBEDO and EMISSIVITY are valid according to the 
               ! earlier fractional seaice value, XICEM.  Recompute them for the new 
               ! seaice value XICE.
               ALBEDO(I,J) = 0.08 + XICE(I,J)/XICEM(I,J) * ( ALBEDO(I,J) - 0.08 )
               EMISS (I,J) = 0.98 + XICE(I,J)/XICEM(I,J) * ( EMISS (I,J) - 0.98 )
            ENDIF
         ENDIF

        IF ( XLAND(i,j) .GT. 1.5 .AND. XICE(I,J) .GE. XICE_THRESHOLD .AND. XICEM(I,J) .LT. XICE_THRESHOLD ) THEN
! water point turns to sea-ice point
          XICEM(I,J) = XICE(I,J)
          XLAND(I,J) = 1.
          IVGTYP(I,J) = ISICE
          ISLTYP(I,J) = 16
          VEGFRA(I,J) = 0.
          TMN(I,J) = 271.4
          ! Over new ice, initial guesses of ALBEDO and EMISS are
          ! based on default water and ice values for albedo and
          ! emissivity.  The land-surface schemes can update these
          ! values
          ALBEDO(I,J) = 0.80 * XICE(I,J) + 0.08 * ( 1.0-XICE(I,J) )
          ALBBCK(I,J) = 0.80
          EMISS(I,J)  = 0.98 * XICE(I,J) + 0.98 * ( 1.0-XICE(I,J) )
          EMBCK(I,J)  = 0.98
          DO nk = 1, num_soil_layers
            TSLB(I,NK,J) = TSK(I,J)
            SMOIS(I,NK,J) = 1.0
            SH2O(I,NK,J) = 0.0
          ENDDO
        ENDIF
        IF(XLAND(i,j) .GT. 1.5) THEN
          TSK(i,j)   =SST(i,j)
          TSLB(i,1,j)=SST(i,j)
        ENDIF
        IF ( XLAND(i,j) .LT. 1.5 .AND. XICEM(I,J) .GE. XICE_THRESHOLD .AND. XICE(I,J) .LT. XICE_THRESHOLD ) THEN
! sea-ice point turns to water point
          XICEM(I,J) = XICE(I,J)
          XLAND(I,J) = 2.
          IVGTYP(I,J) = ISWATER
          ISLTYP(I,J) = 14
          VEGFRA(I,J) = 0.
          SNOW(I,J)  = 0.
          SNOWC(I,J) = 0.
          SNOWH(I,J) = 0.
          TMN(I,J) = SST(I,J)
          ALBEDO(I,J) = 0.08
          ALBBCK(I,J) = 0.08
          EMISS(I,J)  = 0.98
          EMBCK(I,J)  = 0.98
          DO nk = 1, num_soil_layers
            TSLB(I,NK,J) = SST(I,J)
            SMOIS(I,NK,J) = 1.0
            SH2O(I,NK,J) = 1.0
          ENDDO
        ENDIF

        XICEM(i,j) = XICE(i,j)

      ENDDO
      ENDDO
    ENDDO
    !$OMP END PARALLEL DO
  ENDIF

  IF(PRESENT(SST_SKIN))THEN
    IF (sst_skin .EQ. 1) THEN
! Calculate skin sst based on Zeng and Beljaars (2005)
      CALL wrf_debug( 100, 'in SST_SKIN_UPDATE' )
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij, i, j, k )
      DO ij = 1 , num_tiles
        CALL sst_skin_update(xland,glw,gsw,hfx,qfx,tsk,ust,         &
                emiss,dtw,sstsk,dt,stbolt,                          &
                ids, ide, jds, jde, kds, kde,                       &
                ims, ime, jms, jme, kms, kme,                       &
                i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte   )
        DO j=j_start(ij),j_end(ij)
          DO i=i_start(ij),i_end(ij)
            IF(XLAND(i,j) .GT. 1.5)TSK(i,j)=SSTSK(i,j)
          ENDDO
        ENDDO
      ENDDO
    !$OMP END PARALLEL DO
    ENDIF
  ENDIF

  IF(PRESENT(TMN_UPDATE))THEN
  IF (tmn_update .EQ. 1) THEN
      CALL wrf_debug( 100, 'in TMN_UPDATE' )
      CALL tmnupdate(tsk,tmn,tlag,tyr,tyra,tdly,nday,nyear,lagday, &
                julian_in, dt, yr,                                  &
                ids, ide, jds, jde, kds, kde,                       &
                ims, ime, jms, jme, kms, kme,                       &
                i_start,i_end, j_start,j_end, kts,kte, num_tiles   )

  ENDIF
  ENDIF
!
! Modified for adaptive time step
!

  IF ( (itimestep .EQ. 1) .OR. (MOD(itimestep,STEPBL) .EQ. 0) ) THEN
    run_param = .TRUE.
  ELSE
    run_param = .FALSE.
  ENDIF
  IF (PRESENT(adapt_step_flag)) THEN
    IF ((adapt_step_flag)) THEN
      IF ( (itimestep .EQ. 1) .OR. (bldt .EQ. 0) .OR. &
           ( CURR_SECS + dt >= ( INT( CURR_SECS / ( bldt * 60 ) + 1 ) * bldt * 60) ) ) THEN
        run_param = .TRUE.
      ELSE
        run_param = .FALSE.
      ENDIF
    ENDIF
  ENDIF

  IF ( run_param ) then

! IF (itimestep .eq. 1 .or. mod(itimestep,STEPBL) .eq. 0) THEN

  radiation = .false.
  frpcpn = .false.
  myj    = ((sf_sfclay_physics .EQ. MYJSFCSCHEME) .OR. &
            (sf_sfclay_physics .EQ. QNSESFCSCHEME) )
  isisfc = ( FRACTIONAL_SEAICE .EQ. 1  .AND. (          &
            (sf_sfclay_physics .EQ. SFCLAYSCHEME ) .OR. &
            (sf_sfclay_physics .EQ. PXSFCSCHEME  ) .OR. &
            (sf_sfclay_physics .EQ. MYJSFCSCHEME ) .OR. &
            (sf_sfclay_physics .EQ. GFSSFCSCHEME ) )    &
           )

  IF (ra_lw_physics .gt. 0) radiation = .true.

  IF( PRESENT(slope_rad).AND. radiation )THEN
! topographic slope effects modify SWDOWN and GSW here
    IF (slope_rad .EQ. 1) THEN
    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij, i, j, k )
    DO ij = 1 , num_tiles
           CALL TOPO_RAD_ADJ_DRVR (XLAT,XLONG,COSZEN,             &
                    shadowmask,                                   &
                    declin,                                       &
                    SWDOWN,GSW,SWNORM,GSWSAVE,solcon,hrang,       &
                    slope,slp_azi,                                &
                ids, ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                     &
                i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte   )
    ENDDO
    !$OMP END PARALLEL DO

    ENDIF
  ENDIF
!----
! CALCULATE CONSTANT

     DTMIN=DT/60.
! Surface schemes need PBL time step for updates and accumulations
! Assume these schemes provide no tendencies

    if (PRESENT(adapt_step_flag)) then
       if (adapt_step_flag) then
          do_adapt = .TRUE.
       else
          do_adapt = .FALSE.
       endif
    else
       do_adapt = .FALSE.
    endif

    if (PRESENT(BLDT)) then
       if (bldt .eq. 0) then
          DTBL = dt
       ELSE
          if (do_adapt) then
             call wrf_message("WARNING: When using an adaptive time-step the boundary layer"// &
                              " time-step should be 0 (i.e., equivalent to model time-step).  "// &
                              "In order to proceed, for boundary layer calculations, the "// &
                              "boundary layer time-step"// &
                              " will be rounded to the nearest minute, possibly resulting in"// &
                              " innacurate results.")
             DTBL=bldt*60
          else
             DTBL=DT*STEPBL
          endif
       endif
    else
       DTBL=DT*STEPBL
    endif

! SAVE OLD VALUES


     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, i, j, k )
     DO ij = 1 , num_tiles
       DO j=j_start(ij),j_end(ij)
       DO i=i_start(ij),i_end(ij)
! PSFC : in Pa
          PSFC(I,J)=p8w(I,kts,J)
! REVERSE ORDER IN THE VERTICAL DIRECTION
          DO k=kts,kte
            v_phytmp(i,k,j)=v_phy(i,k,j)+v_frame
            u_phytmp(i,k,j)=u_phy(i,k,j)+u_frame
          ENDDO
       ENDDO
       ENDDO
     ENDDO
     !$OMP END PARALLEL DO

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, i, j, k )
     DO ij = 1 , num_tiles
     sfclay_select: SELECT CASE(sf_sfclay_physics)

     CASE (SFCLAYSCHEME)
! DX varies spatially in NMM, therefore, SFCLAY cannot be called
! because it takes a scalar DX. NMM passes in a dummy value for this
! scalar.  NEEDS FURTHER ATTENTION. JM 20050215
       IF (PRESENT(qv_curr)                            .AND.    &
           PRESENT(mol)        .AND.  PRESENT(regime)  .AND.    &
                                                      .TRUE. ) THEN
         CALL wrf_debug( 100, 'in SFCLAY' )
         IF ( FRACTIONAL_SEAICE == 1 ) THEN
            CALL SFCLAY_SEAICE_WRAPPER(u_phytmp,v_phytmp,t_phy,qv_curr,&
                 p_phy,dz8w,cp,g,rcp,r_d,xlv,psfc,chs,chs2,cqs2,cpm, &
                 znt,ust,pblh,mavail,zol,mol,regime,psim,psih,       &
                 xland,hfx,qfx,lh,tsk,flhc,flqc,qgh,qsfc,rmol,       &
                 u10,v10,th2,t2,q2,                                  &
                 gz1oz0,wspd,br,isfflx,dx,                           &
                 svp1,svp2,svp3,svpt0,ep_1,ep_2,karman,eomeg,stbolt, &
                 P1000mb,                                            &
                 XICE,SST,TSK_SEA,                                                  &
                 CHS2_SEA,CHS_SEA,CPM_SEA,CQS2_SEA,FLHC_SEA,FLQC_SEA,               &
                 HFX_SEA,LH_SEA,QFX_SEA,QGH_SEA,QSFC_SEA,ZNT_SEA,                   &
                 ITIMESTEP,XICE_THRESHOLD,                                          &
                 ids,ide, jds,jde, kds,kde,                          &
                 ims,ime, jms,jme, kms,kme,                          &
                 i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte,    &
                 ustm,ck,cka,cd,cda,isftcflx,iz0tlnd                 )
         ELSE
         CALL SFCLAY(u_phytmp,v_phytmp,t_phy,qv_curr,&
               p_phy,dz8w,cp,g,rcp,r_d,xlv,psfc,chs,chs2,cqs2,cpm, &
               znt,ust,pblh,mavail,zol,mol,regime,psim,psih,       &
               xland,hfx,qfx,lh,tsk,flhc,flqc,qgh,qsfc,rmol,       &
               u10,v10,th2,t2,q2,                                  &
               gz1oz0,wspd,br,isfflx,dx,                           &
               svp1,svp2,svp3,svpt0,ep_1,ep_2,karman,eomeg,stbolt, &
               P1000mb,                                            &
               ids,ide, jds,jde, kds,kde,                          &
               ims,ime, jms,jme, kms,kme,                          &
               i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte,    &
               ustm,ck,cka,cd,cda,isftcflx,iz0tlnd                 )
#if ( EM_CORE==1)
           DO j = j_start(ij),j_end(ij)
           DO i = i_start(ij),i_end(ij)
             ch(i,j) = chs (i,j)
!!             ch(i,j) = flhc(i,j)/( cpm(i,j)*rho(i,kts,j) )
           end do
           end do
#endif
         ENDIF
       ELSE
         CALL wrf_error_fatal('Lacking arguments for SFCLAY in surface driver')
       ENDIF

     CASE (PXSFCSCHEME)
#if (NMM_CORE != 1)
       IF (PRESENT(qv_curr)                            .AND.    &
           PRESENT(mol)        .AND.  PRESENT(regime)  .AND.    &
                                                      .TRUE. ) THEN
         CALL wrf_debug( 100, 'in PX Surface Layer scheme' )
         IF ( FRACTIONAL_SEAICE == 1 ) THEN
            CALL WRF_ERROR_FATAL("PXSFCLAY not adapted for FRACTIONAL_SEAICE=1 option")
            CALL PXSFCLAY_SEAICE_WRAPPER(u_phytmp,v_phytmp,t_phy,th_phy,qv_curr,&
                 p_phy,dz8w,cp,g,rcp,r_d,xlv,psfc,chs,chs2,cqs2,cpm, &
                 znt,ust,pblh,mavail,zol,mol,regime,psim,psih,       &
                 xland,hfx,qfx,lh,tsk,flhc,flqc,qgh,qsfc,rmol,       &
                 u10,v10,                                            &
                 gz1oz0,wspd,br,isfflx,dx,                           &
                 svp1,svp2,svp3,svpt0,ep_1,ep_2,karman,              &
                 XICE, SST, ITIMESTEP, XICE_THRESHOLD,                              &
                 CHS_SEA, CHS2_SEA, CPM_SEA, CQS2_SEA,FLHC_SEA,FLQC_SEA,            &
                 HFX_SEA, LH_SEA, QFX_SEA, QGH_SEA, QSFC_SEA, TSK_SEA,  &
                 ids,ide, jds,jde, kds,kde,                          &
                 ims,ime, jms,jme, kms,kme,                          &
                 i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )
         ELSE
         CALL PXSFCLAY(u_phytmp,v_phytmp,t_phy,th_phy,qv_curr,&
               p_phy,dz8w,cp,g,rcp,r_d,xlv,psfc,chs,chs2,cqs2,cpm, &
               znt,ust,pblh,mavail,zol,mol,regime,psim,psih,       &
               xland,hfx,qfx,lh,tsk,flhc,flqc,qgh,qsfc,rmol,       &
               u10,v10,                                            &
               gz1oz0,wspd,br,isfflx,dx,                           &
               svp1,svp2,svp3,svpt0,ep_1,ep_2,karman,              &
               ids,ide, jds,jde, kds,kde,                          &
               ims,ime, jms,jme, kms,kme,                          &
               i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )
         ENDIF
       ELSE
         CALL wrf_error_fatal('Lacking arguments for PX Surface Layer in surface driver')
       ENDIF
#else
       CALL wrf_error_fatal('PX Surface Layer scheme cannot be used with NMM')
#endif

      CASE (MYJSFCSCHEME)
       IF (PRESENT(qv_curr)    .AND.  PRESENT(qc_curr) .AND.    &
                                                      .TRUE. ) THEN

        CALL wrf_debug(100,'in MYJSFC')
        IF ( FRACTIONAL_SEAICE == 1 ) THEN
           CALL MYJSFC_SEAICE_WRAPPER(itimestep,ht,dz8w,             &
                p_phy,p8w,th_phy,t_phy,                              &
                qv_curr,qc_curr,                                     &
                u_phy,v_phy,tke_myj,                                 &
                tsk,qsfc,thz0,qz0,uz0,vz0,                           &
                lowlyr,                                              &
                xland,ivgtyp,isurban,iz0tlnd,                        &
                XICE_THRESHOLD,                                      & ! Extra for wrapper.
                XICE, SST,                                           & ! Extra for wrapper.
                CHS_SEA, CHS2_SEA, CQS2_SEA, CPM_SEA,            &
                FLHC_SEA, FLQC_SEA, QSFC_SEA, &
                QGH_SEA, QZ0_SEA, HFX_SEA, QFX_SEA, LH_SEA,         &
                TSK_SEA,                                             &
                ust,znt,z0,pblh,mavail,rmol,                         &
                akhs,akms,                                           &
                br,                                                 &
                chs,chs2,cqs2,hfx,qfx,lh,flhc,flqc,qgh,cpm,ct,       &
                u10,v10,t2,th2,tshltr,th10,q2,qshltr,q10,pshltr,               &
                p1000mb,                                             &
                ids,ide, jds,jde, kds,kde,                           &
                ims,ime, jms,jme, kms,kme,                           &
                i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )
        ELSE
            CALL MYJSFC(itimestep,ht,dz8w,                         &
              p_phy,p8w,th_phy,t_phy,                              &
              qv_curr,qc_curr,                                      &
              u_phy,v_phy,tke_myj,                                 &
              tsk,qsfc,thz0,qz0,uz0,vz0,                           &
              lowlyr,                                              &
              xland,ivgtyp,isurban,iz0tlnd,                        &
              ust,znt,z0,pblh,mavail,rmol,                         &
              akhs,akms,                                           &
              br,                                                 &
              chs,chs2,cqs2,hfx,qfx,lh,flhc,flqc,qgh,cpm,ct,       &
              u10,v10,t2,th2,tshltr,th10,q2,qshltr,q10,pshltr,               &
              p1000mb,                                             &
              ids,ide, jds,jde, kds,kde,                           &
              ims,ime, jms,jme, kms,kme,                           &
              i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )
#if ( EM_CORE==1)
         DO j = j_start(ij),j_end(ij)
            DO i = i_start(ij),i_end(ij)
               wspd(i,j) = MAX(SQRT(u_phy(i,kts,j)**2+v_phy(i,kts,j)**2),0.001)
               ch(i,j) = chs (i,j)
!!           ch(i,j) = flhc(i,j)/( cpm(i,j)*rho(i,kts,j) )
            END DO
         END DO
#endif         

        ENDIF
       ELSE
         CALL wrf_error_fatal('Lacking arguments for MYJSFC in surface driver')
       ENDIF

      CASE (QNSESFCSCHEME)
       IF (PRESENT(qv_curr)    .AND.  PRESENT(qc_curr) .AND.    &
                                                      .TRUE. ) THEN
            CALL wrf_debug(100,'in QNSESFC')
            CALL QNSESFC(itimestep,ht,dz8w,                         &
              p_phy,p8w,th_phy,t_phy,                              &
              qv_curr,qc_curr,                                     &
              u_phy,v_phy,tke_myj,                                 &
              tsk,qsfc,thz0,qz0,uz0,vz0,                           &
              lowlyr,                                              &
              xland,                                               &
              ust,znt,z0,pblh,mavail,rmol,                         &
              akhs,akms,                                           &
              br,                                                 &
              chs,chs2,cqs2,hfx,qfx,lh,flhc,flqc,qgh,cpm,ct,       &
              u10,v10,tshltr,th10,qshltr,q10,pshltr,               &
              ids,ide, jds,jde, kds,kde,                           &
              ims,ime, jms,jme, kms,kme,                           &
              i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )
       ELSE
         CALL wrf_error_fatal('Lacking arguments for QNSESFC in surface driver')
       ENDIF

     CASE (GFSSFCSCHEME)
       IF (PRESENT(qv_curr) .AND. .TRUE. ) THEN
       CALL wrf_debug( 100, 'in GFSSFC' )
       IF (FRACTIONAL_SEAICE == 1) THEN
          CALL SF_GFS_SEAICE_WRAPPER(u_phytmp,v_phytmp,t_phy,qv_curr, &
               p_phy,CP,RCP,R_d,XLV,PSFC,CHS,CHS2,CQS2,CPM,        &
               ZNT,UST,PSIM,PSIH,                                  &
               XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,                     &
               QGH,QSFC,U10,V10,                                   &
               GZ1OZ0,WSPD,BR,ISFFLX,                              &
               EP_1,EP_2,KARMAN,itimestep,                         &
               XICE_THRESHOLD,                              &
               CHS_SEA, CHS2_SEA, CPM_SEA, CQS2_SEA,        &
               FLHC_SEA, FLQC_SEA,                          &
               HFX_SEA, LH_SEA, QFX_SEA, QGH_SEA, QSFC_SEA, &
               UST_SEA, ZNT_SEA, SST, XICE,                 &
               ids,ide, jds,jde, kds,kde,                          &
               ims,ime, jms,jme, kms,kme,                          &
               i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )
      ELSE
         CALL SF_GFS(u_phytmp,v_phytmp,t_phy,qv_curr,              &
               p_phy,CP,RCP,R_d,XLV,PSFC,CHS,CHS2,CQS2,CPM,        &
               ZNT,UST,PSIM,PSIH,                                  &
               XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,                     &
               QGH,QSFC,U10,V10,                                   &
               GZ1OZ0,WSPD,BR,ISFFLX,                              &
               EP_1,EP_2,KARMAN,itimestep,                         &
               ids,ide, jds,jde, kds,kde,                          &
               ims,ime, jms,jme, kms,kme,                          &
               i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )
      ENDIF
        CALL wrf_debug(100,'in SFCDIAGS')
       ELSE
         CALL wrf_error_fatal('Lacking arguments for SF_GFS in surface driver')
      ENDIF

#if ( EM_CORE==1)
    CASE(MYNNSFCSCHEME)

       IF (PRESENT(qv_curr)    .AND.  PRESENT(qc_curr)     &
            & .AND.  PRESENT(qcg) ) THEN
          
          CALL wrf_debug(100,'in MYNNSFC')          

          CALL SFCLAY_mynn(u_phytmp,v_phytmp,t_phy,qv_curr,&
               p_phy,dz8w,cp,g,rcp,r_d,xlv,psfc,chs,chs2,cqs2,cpm, &
               znt,ust,pblh,mavail,zol,mol,regime,psim,psih, &
               xland,hfx,qfx,lh,tsk,flhc,flqc,qgh,qsfc,rmol,       &
               u10,v10,th2,t2,q2,                                  &
               gz1oz0,wspd,br,isfflx,dx,                           &
               svp1,svp2,svp3,svpt0,ep_1,ep_2,karman,eomeg,stbolt, &
               &itimestep,ch,th_phy,pi_phy,qc_curr,&
               &tsq,qsq,cov,qcg,&
               ids,ide, jds,jde, kds,kde,                          &
               ims,ime, jms,jme, kms,kme,                          &
               i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )

       ELSE
          CALL wrf_error_fatal('Lacking arguments for SFCLAY_mynn in surface driver')
 
       ENDIF
#endif

#if (NMM_CORE==1)

    CASE (GFDLSFCSCHEME)
       CALL wrf_debug( 100, 'in GFDLSFC' )

      IF(sf_surface_physics .eq. 88)THEN
        GFDL_NTSFLG=1
      ELSE
        GFDL_NTSFLG=0
      ENDIF

      CALL SF_GFDL(u_phytmp,v_phytmp,t_phy,qv_curr,p_phy, &
                   CP,RCP,R_d,XLV,PSFC,CHS,CHS2,CQS2,CPM,                 &
                   DTBL, SMOIS,num_soil_layers,ISLTYP,ZNT,UST,PSIM,PSIH,                          &  !DT & MAVAIL
                   XLAND,HFX,QFX,TAUX,TAUY,LH,GSW,GLW,TSK,FLHC,FLQC,  & ! gopal's doing for Ocean coupling
                   QGH,QSFC,U10,V10,                              &
                   GZ1OZ0,WSPD,BR,ISFFLX,                         &
                   EP_1,EP_2,KARMAN,GFDL_NTSFLG,SFENTH,           &
                   ids,ide, jds,jde, kds,kde,                     &
                   ims,ime, jms,jme, kms,kme,                             &
                   i_start(ij),i_end(ij),j_start(ij),j_end(ij),kts,kte    )
           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)
              CHKLOWQ(I,J)= 1.0
           ENDDO
           ENDDO

#endif
     CASE DEFAULT

       WRITE( message , * )                                &
   'The sfclay option does not exist: sf_sfclay_physics = ', sf_sfclay_physics
       CALL wrf_error_fatal ( message )

     END SELECT sfclay_select

!  Compute uratx, vratx, tratx for obs nudging
     IF(PRESENT(uratx) .and. PRESENT(vratx) .and. PRESENT(tratx))THEN
        DO J=j_start(ij),j_end(ij)
        DO I=i_start(ij),i_end(ij)
           IF(ABS(U10(I,J)) .GT. 1.E-10) THEN
              uratx(I,J) = U_PHYTMP(I,1,J)/U10(I,J)
           ELSE
              uratx(I,J) = 1.2
           END IF
           IF(ABS(V10(I,J)) .GT. 1.E-10) THEN
              vratx(I,J) = V_PHYTMP(I,1,J)/V10(I,J)
           ELSE
              vratx(I,J) = 1.2
           END IF
! (Quotient P1000mb/P_PHY must be conditioned due to large value of P1000mb)
           tratx(I,J) = (T_PHY(I,1,J)*(P1000mb*0.001/(P_PHY(I,1,J)/1000.))**RCP)  &
                        /TH2(I,J)
        ENDDO
        ENDDO
     ENDIF

     ENDDO
     !$OMP END PARALLEL DO

     IF (ISFFLX.EQ.0 ) GOTO 430
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, i, j, k )
     DO ij = 1 , num_tiles

     sfc_select: SELECT CASE(sf_surface_physics)

     CASE (SLABSCHEME)

       IF (PRESENT(qv_curr)                            .AND.    &
           PRESENT(capg)        .AND.    &
                                                      .TRUE. ) THEN
           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)
!          CQS2 ACCOUNTS FOR MAVAIL FOR SFCDIAGS 2M Q
              CQS2(I,J)= CQS2(I,J)*MAVAIL(I,J)
           ENDDO
           ENDDO

           IF ( FRACTIONAL_SEAICE == 1 ) THEN
              CALL wrf_error_fatal('SLAB scheme cannot be used with fractional seaice')
           ENDIF
        CALL wrf_debug(100,'in SLAB')
          CALL SLAB(t_phy,qv_curr,p_phy,flhc,flqc,  &
             psfc,xland,tmn,hfx,qfx,lh,tsk,qsfc,chklowq,          &
             gsw,glw,capg,thc,snowc,emiss,mavail,                 &
             dtbl,rcp,xlv,dtmin,ifsnow,                           &
             svp1,svp2,svp3,svpt0,ep_2,karman,eomeg,stbolt,       &
             tslb,zs,dzs,num_soil_layers,radiation,               &
             p1000mb,                                             &
             ids,ide, jds,jde, kds,kde,                           &
             ims,ime, jms,jme, kms,kme,                           &
             i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte)

           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)
              SFCEVP(I,J)= SFCEVP(I,J) + QFX(I,J)*DTBL
              IF(PRESENT(ACHFX))ACHFX(I,J)=ACHFX(I,J) + HFX(I,J)*DT
              IF(PRESENT(ACLHF))ACLHF(I,J)=ACLHF(I,J) + LH(I,J)*DT
           ENDDO
           ENDDO

        CALL wrf_debug(100,'in SFCDIAGS')
          CALL SFCDIAGS(hfx,qfx,tsk,qsfc,chs2,cqs2,t2,th2,q2,      &
                     psfc,cp,r_d,rcp,                              &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
             i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )

       ENDIF

     CASE (LSMSCHEME)

       IF (PRESENT(qv_curr)    .AND.  PRESENT(rainbl)        .AND.    &
!          PRESENT(emiss)      .AND.  PRESENT(t2)            .AND.    &
!          PRESENT(declin) .AND.  PRESENT(coszen)    .AND.    &
!          PRESENT(hrang)  .AND. PRESENT( xlat_urb2d)    .AND.    &
!          PRESENT(dzr)       .AND.    &
!          PRESENT( dzb)            .AND. PRESENT(dzg)       .AND.    &
!          PRESENT(tr_urb2d) .AND. PRESENT(tb_urb2d)         .AND.    &
!          PRESENT(tg_urb2d) .AND. PRESENT(tc_urb2d) .AND.            &
!          PRESENT(qc_urb2d) .AND. PRESENT(uc_urb2d) .AND.            &
!          PRESENT(xxxr_urb2d) .AND. PRESENT(xxxb_urb2d) .AND.        &
!          PRESENT(xxxg_urb2d) .AND.                                  &
!          PRESENT(xxxc_urb2d) .AND. PRESENT(trl_urb3d) .AND.         &
!          PRESENT(tbl_urb3d)   .AND. PRESENT(tgl_urb3d)  .AND.       &
!          PRESENT(sh_urb2d) .AND. PRESENT(lh_urb2d)  .AND.           &
!          PRESENT(g_urb2d)   .AND. PRESENT(rn_urb2d) .AND.           &
!          PRESENT(ts_urb2d)                          .AND.           &
!          PRESENT(frc_urb2d) .AND. PRESENT(utype_urb2d)   .AND.      &
                                                      .TRUE. ) THEN
!------------------------------------------------------------------
         IF( PRESENT(sr) ) THEN
           frpcpn=.true.
         ENDIF
         IF ( FRACTIONAL_SEAICE == 1) THEN
            ! The fields passed to LSM need to represent the full ice values, not
            ! the fractional values.  Convert ALBEDO and EMISS from the blended value 
            ! to a value representing only the sea-ice portion.   Albedo over open 
            ! water is taken to be 0.08. Emissivity over open water is taken to be 0.98
            DO j = j_start(ij) , j_end(ij)
               DO i = i_start(ij) , i_end(ij)
                  IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1 ) ) THEN
                     ALBEDO(I,J) = (ALBEDO(I,J)-(1.-XICE(I,J))*0.08)/XICE(I,J)
                     EMISS(I,J) = (EMISS(I,J)-(1.-XICE(I,J))*0.98)/XICE(I,J)
                  ENDIF
               ENDDO
            ENDDO

            IF ( isisfc ) THEN
               ! Use surface layer routine values from the ice portion of grid point
            ELSE
               !
               ! We don't have surface layer routine values at this time, so
               ! just use what we have.  Use ice component of TSK
               !
               DO j = j_start(ij) , j_end(ij)
                  DO i = i_start(ij) , i_end(ij)
                     IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1 ) ) THEN
                        IF ( SST(i,j) .LT. 271.4 ) THEN
                           SST(i,j) = 271.4
                        ENDIF
                        TSK_SEA(i,j) = SST(i,j)
                        ! Convert TSK from our ice/water average value to value good for solid-ice surface.
                        TSK(i,j) = ( TSK(i,j) - (1.-XICE(i,j)) *SST(i,j) ) / XICE(i,j)
                        IF (XICE(i,j).lt.0.2 .and. TSK(i,j).lt.253.15) THEN
                           TSK(i,j) = 253.15
                        ENDIF
                        IF (XICE(i,j).lt.0.1 .and. TSK(i,j).lt.263.15) THEN
                           TSK(i,j) = 263.15
                        ENDIF
                     ELSE
                        TSK_SEA(i,j) = TSK(i,j)
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
         ENDIF

         CALL wrf_debug(100,'in NOAH DRV')
         CALL lsm(dz8w,qv_curr,p8w,t_phy,tsk,                 &
                hfx,qfx,lh,grdflx,qgh,gsw,swdown,glw,smstav,smstot,    &
                sfcrunoff,udrunoff,ivgtyp,isltyp,isurban,isice,vegfra,        &
                albedo,albbck,znt,z0, tmn,xland,xice, emiss, embck,    &
                snowc,qsfc,rainbl,                              &
                mminlu,                                         &
                num_soil_layers,dtbl,dzs,itimestep,             &
                smois,tslb,snow,canwat,                         &
                chs, chs2, cqs2, cpm,rcp,SR,chklowq,lai,qz0,    &
                myj,frpcpn,                                     &
		sh2o,snowh,                                     & !h
                u_phy,v_phy,                                    & !I
                snoalb,shdmin,shdmax,                           & !i
                snotime,                                        & !o
                acsnom,acsnow,                                  & !o
                snopcx,                                         & !o
                potevp,                                         & !o
                smcrel,                                         & !o
                xice_threshold,                                 &
                rdlai2d,usemonalb,                              &
                br,                                             & !?
                  NOAHRES,                                      &
                ids,ide, jds,jde, kds,kde,                      &
                ims,ime, jms,jme, kms,kme,                      &
                i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte,    &
                sf_urban_physics                                &
!Optional urban
                ,cmr_sfcdif,chr_sfcdif,cmc_sfcdif,chc_sfcdif    &
                ,tr_urb2d,tb_urb2d,tg_urb2d,tc_urb2d,qc_urb2d,  & !H urban
                uc_urb2d,                                       & !H urban
                xxxr_urb2d,xxxb_urb2d,xxxg_urb2d,xxxc_urb2d,    & !H urban
                trl_urb3d,tbl_urb3d,tgl_urb3d,                  & !H urban
                sh_urb2d,lh_urb2d,g_urb2d,rn_urb2d,ts_urb2d,    & !H urban
                psim_urb2d,psih_urb2d,u10_urb2d,v10_urb2d,      & !O urban
                GZ1OZ0_urb2d, AKMS_URB2D,                       & !O urban
                th2_urb2d,q2_urb2d,ust_urb2d,                   & !O urban
                declin,coszen,hrang,                            & !I solar
                xlat_urb2d,                                     & !I urban
                num_roof_layers, num_wall_layers,               & !I urban
                num_road_layers, DZR, DZB, DZG,                 & !I urban
                FRC_URB2D, UTYPE_URB2D,                         & !I urban
                num_urban_layers,                               & !I multi-layer urban
                trb_urb4d,tw1_urb4d,tw2_urb4d,tgb_urb4d,        & !H multi-layer urban
                tlev_urb3d,qlev_urb3d,                          & !H multi-layer urban
                tw1lev_urb3d,tw2lev_urb3d,                      & !H multi-layer urban
                tglev_urb3d,tflev_urb3d,                        & !H multi-layer urban
                sf_ac_urb3d,lf_ac_urb3d,cm_ac_urb3d,            & !H multi-layer urban
                sfvent_urb3d,lfvent_urb3d,                      & !H multi-layer urban
                sfwin1_urb3d,sfwin2_urb3d,                      & !H multi-layer urban
                sfw1_urb3d,sfw2_urb3d,sfr_urb3d,sfg_urb3d,      & !H multi-layer urban
                th_phy,rho,p_phy,ust,                           & !I multi-layer urban
                gmt,julday,xlong,xlat,                          & !I multi-layer urban
                a_u_bep,a_v_bep,a_t_bep,a_q_bep,                & !O multi-layer urban 
                a_e_bep,b_u_bep,b_v_bep,                        & !O multi-layer urban
                b_t_bep,b_q_bep,b_e_bep,dlg_bep,                & !O multi-layer urban
                dl_u_bep,sf_bep,vl_bep                          & !O multi-layer urban
                )
         
         IF ( FRACTIONAL_SEAICE == 1 ) THEN
            ! LSM Returns full land/ice values, no fractional values.
            ! We return to a fractional component here.  SFLX currently hard-wires
            ! emissivity over sea ice to 0.98, the same value as over open water, so
            ! the fractional consideration doesn't have any effect for emissivity.
            DO j=j_start(ij),j_end(ij)
               DO i=i_start(ij),i_end(ij)
                  IF ( ( XICE(I,J) .GE. XICE_THRESHOLD) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                     albedo(i,j) = ( albedo(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * 0.08  )
                     emiss(i,j) = ( emiss(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * 0.98  )
                  ENDIF
               ENDDO
            ENDDO

            IF ( isisfc ) THEN
               DO j=j_start(ij),j_end(ij)
                  DO i=i_start(ij),i_end(ij)
                     IF ( ( XICE(I,J) .GE. XICE_THRESHOLD) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                        !  Weighted average of fields between ice-cover values and open-water values.
                        flhc(i,j) = ( flhc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * flhc_sea(i,j) )
                        flqc(i,j) = ( flqc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * flqc_sea(i,j) )
                        cpm(i,j)  = ( cpm(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * cpm_sea(i,j)  )
                        cqs2(i,j) = ( cqs2(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * cqs2_sea(i,j) )
                        chs2(i,j) = ( chs2(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * chs2_sea(i,j) )
                        chs(i,j)  = ( chs(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * chs_sea(i,j)  )
                        qsfc(i,j) = ( qsfc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * qsfc_sea(i,j) )
                        qgh(i,j)  = ( qgh(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * qgh_sea(i,j)  )
                        qz0(i,j)  = ( qz0(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * qz0_sea(i,j)  )
                        hfx(i,j)  = ( hfx(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * hfx_sea(i,j)  )
                        qfx(i,j)  = ( qfx(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * qfx_sea(i,j)  )
                        lh(i,j)   = ( lh(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * lh_sea(i,j)   )
                        tsk(i,j)  = ( tsk(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * tsk_sea(i,j)  )
                     ENDIF
                  ENDDO
               ENDDO
            ELSE
               DO j = j_start(ij) , j_end(ij)
                  DO i = i_start(ij) , i_end(ij)
                     IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                        ! Compute TSK as the open-water and ice-cover average
                        tsk(i,j) = ( tsk(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * tsk_sea(i,j) )
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
         ENDIF
           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)
!              CHKLOWQ(I,J)= 1.0
               SFCEVP(I,J)= SFCEVP(I,J) + QFX(I,J)*DTBL
               SFCEXC(I,J)= CHS(I,J)
               IF(PRESENT(ACHFX))ACHFX(I,J)=ACHFX(I,J) + HFX(I,J)*DT
               IF(PRESENT(ACLHF))ACLHF(I,J)=ACLHF(I,J) + LH(I,J)*DT
               IF(PRESENT(ACGRDFLX))ACGRDFLX(I,J)=ACGRDFLX(I,J) + GRDFLX(I,J)*DT
           ENDDO
           ENDDO

          CALL SFCDIAGS(HFX,QFX,TSK,QSFC,CHS2,CQS2,T2,TH2,Q2,      &
                     PSFC,CP,R_d,RCP,                              &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
             i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )
!urban
     IF(SF_URBAN_PHYSICS.eq.1) THEN
       DO j=j_start(ij),j_end(ij)                             !urban
         DO i=i_start(ij),i_end(ij)                           !urban
          IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == 31 .or. &  !urban
              IVGTYP(I,J) == 32 .or. IVGTYP(I,J) == 33 ) THEN !urban
!             TH2(I,J)  = TH2_URB2D(I,J)                       !urban
!             T2(I,J)   = TH2_URB2D(I,J)/(1.E5/PSFC(I,J))**RCP !urban
!m             T2(I,J)   = TH2_URB2D(I,J)                       !urban
!             T2(I,J)   = FRC_URB2D(i,j)*TH2_URB2D(I,J) + (1-FRC_URB2D(i,j))*T2(I,J) !urban
!             TH2(I,J) = T2(I,J)*(1.E5/PSFC(I,J))**RCP                               !urban
!m             Q2(I,J)   = Q2_URB2D(I,J)                                            !urban
!             Q2(I,J)   = FRC_URB2D(i,j)*Q2_URB2D(I,J) +(1-FRC_URB2D(i,j))* Q2(I,J)  !urban
             U10(I,J)  = U10_URB2D(I,J)                       !urban
             V10(I,J)  = V10_URB2D(I,J)                       !urban
             PSIM(I,J) = PSIM_URB2D(I,J)                      !urban
             PSIH(I,J) = PSIH_URB2D(I,J)                      !urban
             GZ1OZ0(I,J) = GZ1OZ0_URB2D(I,J)                  !urban
!m             AKHS(I,J) = AKHS_URB2D(I,J)                    !urban
             AKHS(I,J) = CHS(I,J)                             !urban
             AKMS(I,J) = AKMS_URB2D(I,J)                      !urban
           END IF                                             !urban
         ENDDO                                                !urban
       ENDDO                                                  !urban
     ENDIF
! urban BEP
     IF((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.eq.3)) THEN
       DO j=j_start(ij),j_end(ij)                             !urban
         DO i=i_start(ij),i_end(ij)                           !urban
          IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == 31 .or. &  !urban
              IVGTYP(I,J) == 32 .or. IVGTYP(I,J) == 33 ) THEN !urban
            T2(I,J)   = TH_PHY(i,1,j)/((1.E5/PSFC(I,J))**RCP) !urban
            TH2(I,J) = TH_PHY(i,1,j) !urban
            Q2(I,J)   = qv_curr(i,1,j)  !urban
            U10(I,J)  = U_phy(I,1,J)                       !urban
            V10(I,J)  = V_phy(I,1,J)                       !urban
           END IF                                             !urban
         ENDDO                                                !urban
       ENDDO                                                  !urban
     ENDIF

!------------------------------------------------------------------

       ELSE
         CALL wrf_error_fatal('Lacking arguments for LSM in surface driver')
       ENDIF

     CASE (RUCLSMSCHEME)
       IF (PRESENT(qv_curr)    .AND.  PRESENT(qc_curr) .AND.    &
!           PRESENT(emiss)      .AND.  PRESENT(t2)      .AND.    &
           PRESENT(qsg)        .AND.  PRESENT(qvg)     .AND.    &
           PRESENT(qcg)        .AND.  PRESENT(soilt1)  .AND.    &
           PRESENT(tsnav)      .AND.  PRESENT(smfr3d)  .AND.    &
           PRESENT(keepfr3dflag) .AND. PRESENT(rainbl) .AND.    &
           PRESENT(dew)                                .AND.    &
                                                      .TRUE. ) THEN

           IF( PRESENT(sr) ) THEN
               frpcpn=.true.
           ELSE
               SR = 1.
           ENDIF
           CALL wrf_debug(100,'in RUC LSM')
           IF ( FRACTIONAL_SEAICE == 1 ) THEN
              ! The fields passed to LSMRUC need to represent the full ice values, not
              ! the fractional values.  Convert ALBEDO and EMISS from the blended value 
              ! to a value representing only the sea-ice portion.   Albedo over open 
              ! water is taken to be 0.08. Emissivity over open water is taken to be 0.98
              DO j = j_start(ij) , j_end(ij)
                 DO i = i_start(ij) , i_end(ij)
                    IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1 ) ) THEN
                       ALBEDO(I,J) = (ALBEDO(I,J) - (1.-XICE(I,J))*0.08) / XICE(I,J)
                       EMISS(I,J)  = (EMISS(I,J)  - (1.-XICE(I,J))*0.98) / XICE(I,J)
                    ENDIF
                 ENDDO
              ENDDO

              IF ( isisfc ) THEN
                 !
                 ! use surface layer routine values from the ice portion of grid point
                 !
              ELSE
                 !
                 ! don't have srfc layer routine values at this time, so just use what you have
                 ! use ice component of TSK
                 !
                 DO j = j_start(ij) , j_end(ij)
                    DO i = i_start(ij) , i_end(ij)
                       IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                          TSK_SEA(i,j) = SST(i,j)
                          IF ( SST(i,j) .LT. 271. ) THEN
                             SST(i,j) = 271.4
                             TSK_SEA(i,j) = SST(i,j)
                          endif
                          TSK(i,j) = ( TSK(i,j) - (1.-XICE(i,j)) *SST(i,j) ) / XICE(i,j)
                          IF ( ( XICE(i,j) .LT. 0.2 ) .AND. ( TSK(i,j) .LT. 253.15 ) ) THEN
                             TSK(i,j) = 253.15
                          ENDIF
                          IF ( ( XICE(i,j).LT.0.1 ) .AND. ( TSK(i,j).lt.263.15 ) ) THEN
                             TSK(i,j) = 263.15
                          ENDIF
                       ELSE
                          TSK_SEA(i,j) = TSK(i,j)
                       ENDIF
                    ENDDO
                 ENDDO
              ENDIF
           ENDIF

           CALL LSMRUC(dtbl,itimestep,num_soil_layers,          &
                zs,rainbl,snow,snowh,snowc,sr,frpcpn,           &
                dz8w,p8w,t_phy,qv_curr,qc_curr,rho,             & !p8w in [pa]
                glw,gsw,emiss,chklowq,                          &
                chs,flqc,flhc,mavail,canwat,vegfra,albedo,znt,  &
                z0,snoalb, albbck,                              &   !new
                qsfc,qsg,qvg,qcg,dew,soilt1,tsnav,              &
                tmn,ivgtyp,isltyp,xland,                        &
                isice,xice,xice_threshold,                      &
                cp,rovcp,g,xlv,stbolt,                          &
                smois,sh2o,smstav,smstot,tslb,tsk,hfx,qfx,lh,   &
                sfcrunoff,udrunoff,sfcexc,                      &
                sfcevp,grdflx,acsnow,acsnom,                    &
                smfr3d,keepfr3dflag,                            &
                myj,                                            &
                ids,ide, jds,jde, kds,kde,                      &
                ims,ime, jms,jme, kms,kme,                      &
                i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )

           IF ( FRACTIONAL_SEAICE == 1 ) THEN
              ! LSMRUC Returns full land/ice values, no fractional values.
              ! We return to a fractional component here.
              DO j=j_start(ij),j_end(ij)
                 DO i=i_start(ij),i_end(ij)
                    IF ( ( XICE(I,J) .GE. XICE_THRESHOLD) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                       albedo(i,j) = ( albedo(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * 0.08  )
                       emiss(i,j)  = ( emiss(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * 0.98  )
                    ENDIF
                 ENDDO
              ENDDO
              if ( isisfc ) then
                 !
                 !  back to ice and ocean average
                 !
                 DO j=j_start(ij),j_end(ij)
                    DO i=i_start(ij),i_end(ij)
                       IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
                          flhc(i,j) = ( flhc(i,j) * XICE(i,j) ) + ( (1.-XICE(i,j)) * flhc_sea(i,j) )
                          flqc(i,j) = ( flqc(i,j) * XICE(i,j) ) + ( (1.-XICE(i,j)) * flqc_sea(i,j) )
                          cpm(i,j)  = ( cpm(i,j)  * XICE(i,j) ) + ( (1.-XICE(i,j)) * cpm_sea(i,j)  )
                          cqs2(i,j) = ( cqs2(i,j) * XICE(i,j) ) + ( (1.-XICE(i,j)) * cqs2_sea(i,j) )
                          chs2(i,j) = ( chs2(i,j) * XICE(i,j) ) + ( (1.-XICE(i,j)) * chs2_sea(i,j) )
                          chs(i,j)  = ( chs(i,j)  * XICE(i,j) ) + ( (1.-XICE(i,j)) * chs_sea(i,j)  )
                          qsfc(i,j) = ( qsfc(i,j) * XICE(i,j) ) + ( (1.-XICE(i,j)) * QSFC_SEA(i,j) )
                          qgh(i,j)  = ( qgh(i,j)  * XICE(i,j) ) + ( (1.-XICE(i,j)) * qgh_sea(i,j)  )
                          hfx(i,j)  = ( hfx(i,j)  * XICE(i,j) ) + ( (1.-XICE(i,j)) * HFX_SEA(i,j)  )
                          qfx(i,j)  = ( qfx(i,j)  * XICE(i,j) ) + ( (1.-XICE(i,j)) * QFX_SEA(i,j)  )
                          lh(i,j)   = ( lh(i,j)   * XICE(i,j) ) + ( (1.-XICE(i,j)) * LH_SEA(i,j)   )
                          tsk(i,j)  = ( tsk(i,j)  * XICE(i,j) ) + ( (1.-XICE(i,j)) * TSK_SEA(i,j)  )
                       ENDIF
                    ENDDO
                 ENDDO
              else
                 !
                 ! tsk back to liquid and ice average
                 !
                 DO j = j_start(ij) , j_end(ij)
                    DO i = i_start(ij) , i_end(ij)
                       IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
                          tsk(i,j) = ( tsk(i,j) * XICE(i,j) ) + ( (1.-XICE(i,j)) * TSK_SEA(i,j) )
                       ENDIF
                    ENDDO
                 ENDDO
              endif
           ENDIF

          CALL SFCDIAGS_RUCLSM(HFX,QFX,TSK,QSFC,CHS2,CQS2,T2,TH2,Q2,      &
                     T_PHY,QV_CURR,RHO,P8W,                              &
                     PSFC,CP,R_d,RCP,                                    &
                     ids,ide, jds,jde, kds,kde,                          &
                     ims,ime, jms,jme, kms,kme,                          &
                     i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )


       ELSE
         CALL wrf_error_fatal('Lacking arguments for RUCLSM in surface driver')
       ENDIF

     CASE (PXLSMSCHEME)
       IF (PRESENT(qv_curr)    .AND.  PRESENT(qc_curr) .AND.    &
           PRESENT(emiss)      .AND.  PRESENT(t2)      .AND.    &
           PRESENT(rainbl) .AND.    &
                                                      .TRUE. ) THEN
          IF ( FRACTIONAL_SEAICE == 1 ) THEN

             CALL WRF_ERROR_FATAL("PXLSM not adapted for FRACTIONAL_SEAICE=1 option")

             IF ( isisfc ) THEN
                !
                ! use surface layer routine values from the ice portion of grid point
                !
             ELSE
                !
                ! don't have srfc layer routine values at this time, so just use what you have
                ! use ice component of TSK
                !
                DO j = j_start(ij) , j_end(ij)
                   DO i=i_start(ij) , i_end(ij)
                      IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                         TSK_SEA(i,j) = SST(i,j)
                         IF ( SST(i,j) .LT. 271. ) THEN
                            SST(i,j) = 271.4
                            TSK_SEA(i,j) = SST(i,j)
                         ENDIF
                         TSK(i,j) = ( TSK(i,j) - (1.-XICE(i,j)) *SST(i,j) ) / XICE(i,j)
                         IF ( ( XICE(i,j) .LT. 0.2 ) .AND. ( TSK(i,j) .lt. 253.15 ) ) THEN
                            TSK(i,j) = 253.15
                         ENDIF
                         IF ( ( XICE(i,j) .LT. 0.1 ) .AND. ( TSK(i,j) .LT. 263.15 ) ) THEN
                            TSK(i,j) = 263.15
                         ENDIF
                      ELSE
                         TSK_SEA(i,j) = TSK(i,j)
                      ENDIF
                   ENDDO
                ENDDO
             ENDIF
          ENDIF
          CALL wrf_debug(100,'in P-X LSM')
          CALL PXLSM(u_phy, v_phy, dz8w, qv_curr, t_phy, th_phy, rho,&
                     psfc, gsw, glw, rainbl, emiss,                  &
                     ITIMESTEP, num_soil_layers, DT, anal_interval,  &
                     xland, xice, albbck, albedo, snoalb, smois, tslb, &
                     mavail,T2, Q2,                                  &
                     zs, dzs, psih,                                  &
                     landusef,soilctop,soilcbot,vegfra, vegf_px,     &
                     isltyp,ra,rs,lai,nlcat,nscat,                   &
                     hfx,qfx,lh,tsk,sst,znt,canwat,                  &
                     grdflx,shdmin,shdmax,                           &
                     snowc,pblh,rmol,ust,capg,dtbl,                  &
                     t2_ndg_old,t2_ndg_new,q2_ndg_old,q2_ndg_new,    &
                     sn_ndg_old, sn_ndg_new, snow, snowh,snowncv,    &
                     t2obs, q2obs, pxlsm_smois_init, pxlsm_soil_nudge, &
                     ids,ide, jds,jde, kds,kde,                      &
                     ims,ime, jms,jme, kms,kme,                      &
                     i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte)
          IF ( FRACTIONAL_SEAICE == 1 ) THEN
             IF ( isisfc ) THEN
                !
                !  back to ice and ocean average
                !
                DO j = j_start(ij) , j_end(ij)
                   DO i = i_start(ij) , i_end(ij)
                      IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                         flhc(i,j) = ( flhc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * flhc_sea(i,j) )
                         flqc(i,j) = ( flqc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * flqc_sea(i,j) )
                         cpm(i,j)  = ( cpm(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * cpm_sea(i,j)  )
                         cqs2(i,j) = ( cqs2(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * cqs2_sea(i,j) )
                         chs2(i,j) = ( chs2(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * chs2_sea(i,j) )
                         chs(i,j)  = ( chs(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * chs_sea(i,j)  )
                         qsfc(i,j) = ( qsfc(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * QSFC_SEA(i,j) )
                         qgh(i,j)  = ( qgh(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * QGH_SEA(i,j)  )
                         hfx(i,j)  = ( hfx(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * HFX_SEA(i,j)  )
                         qfx(i,j)  = ( qfx(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * QFX_SEA(i,j)  )
                         lh(i,j)   = ( lh(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * LH_SEA(i,j)   )
                         tsk(i,j)  = ( tsk(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * TSK_SEA(i,j)  )
                         psih(i,j) = ( psih(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * PSIH_SEA(i,j) )
                         pblh(i,j) = ( pblh(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * PBLH_SEA(i,j) )
                         rmol(i,j) = ( rmol(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * RMOL_SEA(i,j) )
                         ust(i,j)  = ( ust(i,j)  * XICE(i,j) ) + ( (1.0-XICE(i,j)) * UST_SEA(i,j)  )
                      ENDIF
                   ENDDO
                ENDDO
             ELSE
                !
                ! tsk back to liquid and ice average
                !
                DO j=j_start(ij),j_end(ij)
                   DO i=i_start(ij),i_end(ij)
                      IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
                         tsk(i,j)=tsk(i,j)*XICE(i,j)+(1.0-XICE(i,j))*TSK_SEA(i,j)
                      ENDIF
                   ENDDO
                ENDDO
             ENDIF
          ENDIF
           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)
              CHKLOWQ(I,J)= 1.0
              TH2(I,J) = T2(I,J)*(1.E5/PSFC(I,J))**RCP
              SFCEVP(I,J)= SFCEVP(I,J) + QFX(I,J)*DTBL
           ENDDO
           ENDDO

       ELSE
         CALL wrf_error_fatal('Lacking arguments for P-X LSM in surface driver')
       ENDIF

     CASE DEFAULT

       IF ( itimestep .eq. 1 ) THEN
       WRITE( message , * ) &
        'No land surface physics option is used: sf_surface_physics = ', sf_surface_physics
        CALL wrf_message ( message )
       ENDIF

     END SELECT sfc_select

     ENDDO
     !$OMP END PARALLEL DO

 430 CONTINUE

#if ( EM_CORE==1)
   IF (omlcall .EQ. 1) THEN
! simple ocean mixed layer model based Pollard, Rhines and Thompson (1973)
     CALL wrf_debug( 100, 'Call OCEANML' )
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , num_tiles
        CALL oceanml(tml,t0ml,hml,h0ml,huml,hvml,ust,u_phy,v_phy, &
                     tmoml,f,g,oml_gamma,                         &
                     xland,hfx,lh,tsk,gsw,glw,emiss,              &
                     dtbl,STBOLT,                                 &
                     ids,ide, jds,jde, kds,kde,                   &
                     ims,ime, jms,jme, kms,kme,                   &
                     i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte)
     ENDDO
     !$OMP END PARALLEL DO
   ENDIF
#endif

! Reset RAINBL in mm (Accumulation between PBL calls)

     IF ( PRESENT( rainbl ) ) THEN
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij, i, j, k )
       DO ij = 1 , num_tiles
         DO j=j_start(ij),j_end(ij)
         DO i=i_start(ij),i_end(ij)
            RAINBL(i,j) = 0.
         ENDDO
         ENDDO
       ENDDO
       !$OMP END PARALLEL DO
     ENDIF

     IF( PRESENT(slope_rad).AND. radiation )THEN
! topographic slope effects removed from SWDOWN and GSW here for output
       IF (slope_rad .EQ. 1) THEN

       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij, i, j, k )
       DO ij = 1 , num_tiles
         DO j=j_start(ij),j_end(ij)
         DO i=i_start(ij),i_end(ij)
         IF(SWNORM(I,J) .GT. 1.E-3)THEN  ! daytime
            SWSAVE = SWDOWN(i,j)
! SWDOWN contains unaffected SWDOWN in output
            SWDOWN(i,j) = SWNORM(i,j)
! SWNORM contains slope-affected SWDOWN in output
            SWNORM(i,j) = SWSAVE
            GSW(i,j) = GSWSAVE(i,j)
         ENDIF
         ENDDO
         ENDDO
       ENDDO
       !$OMP END PARALLEL DO

       ENDIF
     ENDIF

   ENDIF

   END SUBROUTINE surface_driver

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

   subroutine myjsfc_seaice_wrapper(ITIMESTEP,HT,DZ,      &
        &     PMID,PINT,TH,T,QV,QC,U,V,Q2,                &
        &     TSK,QSFC,THZ0,QZ0,UZ0,VZ0,                  &
        &     LOWLYR,XLAND,IVGTYP,ISURBAN,IZ0TLND,        &
        &     XICE_THRESHOLD,                             &  ! Extra for wrapper
        &     XICE,SST,                                   &  ! Extra for wrapper
        &     CHS_SEA, CHS2_SEA, CQS2_SEA, CPM_SEA,       &  ! Extra for wrapper
        &     FLHC_SEA, FLQC_SEA, QSFC_SEA,               &  ! Extra for wrapper
        &     QGH_SEA, QZ0_SEA, HFX_SEA, QFX_SEA,         &  ! Extra for wrapper
        &     FLX_LH_SEA, TSK_SEA,                        &  ! Extra for wrapper
        &     USTAR,ZNT,Z0BASE,PBLH,MAVAIL,RMOL,          &
        &     AKHS,AKMS,                                  &
        &     BR,                                         &
        &     CHS,CHS2,CQS2,HFX,QFX,FLX_LH,FLHC,FLQC,     &
        &     QGH,CPM,CT,                                 &
        &     U10,V10,T02,TH02,TSHLTR,TH10,Q02,QSHLTR,Q10,PSHLTR,          &
        &     P1000,                                        &
        &     IDS,IDE,JDS,JDE,KDS,KDE,                        &
        &     IMS,IME,JMS,JME,KMS,KME,                        &
        &     ITS,ITE,JTS,JTE,KTS,KTE )
!     USE module_model_constants
     USE module_sf_myjsfc

     IMPLICIT NONE

     INTEGER,                                INTENT(IN)    :: ITIMESTEP
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: HT
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: DZ
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: PMID
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: PINT
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: TH
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: T
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: QV
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: QC
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: U
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: V
     REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN)    :: Q2   ! Q2 is TKE?

     ! REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: TSK
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT)    :: TSK

     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: QSFC
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: THZ0
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: QZ0
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: UZ0
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: VZ0
     INTEGER,DIMENSION(IMS:IME,JMS:JME),     INTENT(IN)    :: LOWLYR
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: XLAND
     INTEGER,DIMENSION(IMS:IME,JMS:JME),     INTENT(IN)    :: IVGTYP
     INTEGER                                               :: ISURBAN
     INTEGER                                               :: IZ0TLND
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: XICE       ! Extra for wrapper
     ! REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: SST        ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: SST        ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: BR
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CHS_SEA    ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CHS2_SEA   ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CQS2_SEA   ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CPM_SEA    ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QZ0_SEA   ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QSFC_SEA   ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QGH_SEA   ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLHC_SEA  ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLQC_SEA  ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: HFX_SEA    ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QFX_SEA    ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLX_LH_SEA ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: TSK_SEA    ! Extra for wrapper
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: USTAR
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: ZNT
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: Z0BASE
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: PBLH
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(IN)    :: MAVAIL
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: RMOL
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: AKHS
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(INOUT) :: AKMS
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CHS
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CHS2
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CQS2
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: HFX
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QFX
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLX_LH
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLHC
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: FLQC
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QGH
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CPM
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: CT
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: U10
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: V10
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: T02
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: TH02
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: TSHLTR
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: TH10
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: Q02
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: QSHLTR
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: Q10
     REAL,DIMENSION(IMS:IME,JMS:JME),        INTENT(OUT)   :: PSHLTR
     REAL,                                   INTENT(IN)    :: P1000
     REAL,                                   INTENT(IN)    :: XICE_THRESHOLD
     INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,       &
          &                IMS,IME,JMS,JME,KMS,KME,       &
          &                ITS,ITE,JTS,JTE,KTS,KTE


     ! Local
     INTEGER :: i
     INTEGER :: j
     REAL, DIMENSION( ims:ime, jms:jme ) :: ct_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: u10_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: v10_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: t02_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: th02_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: tshltr_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: pshltr_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: qshltr_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: th10_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: q02_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: q10_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: thz0_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: uz0_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: vz0_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: ustar_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: pblh_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: rmol_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: akhs_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: akms_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: xland_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: mavail_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: znt_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: z0base_sea
     REAL, DIMENSION( ims:ime, jms:jme ) :: br_sea

     REAL, DIMENSION( ims:ime, jms:jme ) :: QSFC_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: QZ0_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: THZ0_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: UZ0_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: VZ0_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: USTAR_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: ZNT_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: PBLH_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: RMOL_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: AKHS_HOLD
     REAL, DIMENSION( ims:ime, jms:jme ) :: AKMS_HOLD
     REAL :: PSFC

     ! Set things up for the frozen-surface call to myjsfc
     ! Is SST local here, or are the changes to be fed back to the calling routines?

     ! We want a TSK valid for the ice-covered regions of the grid cell.
     DO j = JTS , JTE
        DO i = ITS , ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN

              TSK_SEA(i,j) = SST(i,j)

              IF ( SST(i,j) .LT. 271.4 ) THEN
                 SST(i,j) = 271.4
                 TSK_SEA(i,j) = SST(i,j)
              ENDIF

              IF ( ( SST(i,j) .GT. 273.0 ) .AND. ( itimestep .LE. 3 ) ) THEN
                 ! Why the dependence on the time step count, here?
                 IF ( XICE(i,j) .GE. 0.6 ) THEN
                    SST(i,j) = 271.4
                    TSK_SEA(i,j) = SST(i,j)
                 ELSEIF ( XICE(i,j) .GE. 0.4 ) THEN
                    SST(i,j) = 273.
                    TSK_SEA(i,j) = SST(i,j)
                 ELSEIF ( ( XICE(i,j) .GE. 0.2 ) .and. ( SST(i,j).GT.275. ) ) THEN
                    SST(i,j) = 275.
                    TSK_SEA(i,j) = SST(i,j)
                 ELSEIF (SST(i,j).GT.278.) THEN
                    SST(i,j) = 278.
                    TSK_SEA(i,j) = SST(i,j)
                 ENDIF
              ENDIF

              ! Change the TSK value here, to recover the value valid for
              ! ice-covered portions of the grid cell.

              ! The original TSK is taken to represent the blended result of the
              ! open-water values (SST) and the ice-covered value (the new TSK we
              ! derive here).
              TSK(i,j) = ( TSK(i,j) - (1.0-XICE(i,j)) *SST(i,j) ) / XICE(i,j)

              IF (XICE(i,j).lt.0.2 .and. TSK(i,j).lt.253.15) THEN
                 TSK(i,j) = 253.15
              ENDIF
              IF (XICE(i,j).lt.0.1 .and. TSK(i,j).lt.263.15) THEN
                 TSK(i,j) = 263.15
              ENDIF

              ! Over fractional sea-ice points, back out an ice portion of QSFC as well.
              ! QSFC_SEA calculation as done in myjsfc for open water points
              PSFC = PINT(I,LOWLYR(I,J),J)
              QSFC_SEA(i,j) = PQ0SEA/PSFC*EXP(A2S*(TSK(i,j)-A3S)/(TSK(i,j)-A4S))
              QSFC(i,j) = QSFC(i,j) - (1.0-XICE(i,j)) * QSFC_SEA(i,j) / XICE(i,j)
!
              HFX_SEA(i,j)  = HFX(i,j)
              QFX_SEA(i,j)  = QFX(i,j)
              FLX_LH_SEA(i,j)   = FLX_LH(i,j)
!
           ELSE
              TSK_SEA(i,j) = TSK(i,j)
           ENDIF
        ENDDO
     ENDDO

!
! frozen ocean call for sea ice points
!

! Strictly INTENT(IN) to MYJSFC, should be unchanged by call.

     ! DZ
     ! HT
     ! LOWLYR
     ! MAVAIL
     ! PINT
     ! PMID
     ! QC
     ! QV
     ! Q2
     ! T
     ! TH
     ! TSK
     ! U
     ! V
     ! XLAND
     ! Z0BASE

! INTENT (INOUT),  updated by MYJSFC.  Values will need to be saved before the first call to MYJSFC, so that
! the second call to MYJSFC does not double-count the effect.

     ! Save INTENT(INOUT) variables before the frozen-water/true-land call to MYJSFC:
     QSFC_HOLD  = QSFC
     QZ0_HOLD   = QZ0
     THZ0_HOLD  = THZ0
     UZ0_HOLD   = UZ0
     VZ0_HOLD   = VZ0
     USTAR_HOLD = USTAR
     ZNT_HOLD   = ZNT
     PBLH_HOLD  = PBLH
     RMOL_HOLD  = RMOL
     AKHS_HOLD  = AKHS
     AKMS_HOLD  = AKMS

! Strictly INTENT(OUT):  Set by MYJSFC

     ! CHS
     ! CHS2
     ! CPM
     ! CQS2
     ! CT
     ! FLHC
     ! FLQC
     ! FLX_LH
     ! HFX
     ! PSHLTR
     ! QFX
     ! QGH
     ! QSHLTR
     ! Q02
     ! Q10
     ! TH02
     ! TH10
     ! TSHLTR
     ! T02
     ! U10
     ! V10

     ! Frozen-water/true-land call.
     CALL MYJSFC ( ITIMESTEP, HT, DZ,                              &  ! I,I,I,
          &        PMID, PINT, TH, T, QV, QC, U, V, Q2,            &  ! I,I,I,I,I,I,I,I,I,
          &        TSK, QSFC, THZ0, QZ0, UZ0, VZ0,                 &  ! I,IO,IO,IO,IO,IO,
          &        LOWLYR, XLAND, IVGTYP, ISURBAN, IZ0TLND,        &  ! I,I,I,I,I
          &        USTAR, ZNT, Z0BASE, PBLH, MAVAIL, RMOL,         &  ! IO,IO,I,IO,I,IO,
          &        AKHS, AKMS,                                     &  ! IO,IO,
          &        BR,                                             &  ! O
          &        CHS, CHS2, CQS2, HFX, QFX, FLX_LH, FLHC, FLQC,  &  ! O,O,O,0,0,0,0,0,
          &        QGH, CPM, CT, U10, V10, T02,                    &  ! 0,0,0,0,0,0,
          &        TH02, TSHLTR, TH10, Q02,                        &  ! 0,0,0,0,
          &        QSHLTR, Q10, PSHLTR,                            &  ! 0,0,0,
          &        P1000,                                        &  ! I
          &        ids,ide, jds,jde, kds,kde,                      &
          &        ims,ime, jms,jme, kms,kme,                      &
          &        its,ite, jts,jte, kts,kte    )

     ! Set up things for the open ocean call.
     DO j = JTS, JTE
        DO i = ITS, ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) ) THEN
              XLAND_SEA(i,j)=2.
              MAVAIL_SEA(I,J)  = 1.
              ZNT_SEA(I,J) = 0.0001
              Z0BASE_SEA(I,J) = ZNT_SEA(I,J)
              IF ( SST(i,j) .LT. 271.4 ) THEN
                 SST(i,j) = 271.4
              ENDIF
              TSK_SEA(i,j) = SST(i,j)
              PSFC = PINT(I,LOWLYR(I,J),J)
              QSFC_SEA(I,J) = PQ0SEA/PSFC*EXP(A2S*(TSK_SEA(i,j)-A3S)/(TSK_SEA(i,j)-A4S))
           ELSE
              ! This should be a land point or a true open water point
              XLAND_SEA(i,j)=xland(i,j)
              MAVAIL_SEA(i,j) = mavail(i,j)
              ZNT_SEA(I,J)    = ZNT_HOLD(I,J)
              Z0BASE_SEA(I,J) = Z0BASE(I,J)
              TSK_SEA(i,j)  = TSK(i,j)
              QSFC_SEA(i,j) = QSFC_HOLD(i,j)
           ENDIF
        ENDDO
     ENDDO

     QZ0_SEA  = QZ0_HOLD
     THZ0_SEA = THZ0_HOLD
     UZ0_SEA  = UZ0_HOLD
     VZ0_SEA  = VZ0_HOLD
     USTAR_SEA = USTAR_HOLD
     PBLH_SEA = PBLH_HOLD
     RMOL_SEA = RMOL_HOLD
     AKHS_SEA = AKHS_HOLD
     AKMS_SEA = AKMS_HOLD

!
! open water call
!
     CALL MYJSFC ( ITIMESTEP, HT, DZ,                                                          & ! I,I,I,
          &        PMID, PINT, TH, T, QV, QC, U, V, Q2,                                        & ! I,I,I,I,I,I,I,I,I,
          &        TSK_SEA, QSFC_SEA, THZ0_SEA, QZ0_SEA, UZ0_SEA, VZ0_SEA,                     & ! I,IO,IO,IO,IO,IO,
          &        LOWLYR, XLAND_SEA, IVGTYP, ISURBAN, IZ0TLND,                                & ! I,I,I,I,I,
          &        USTAR_SEA, ZNT_SEA, Z0BASE_SEA, PBLH_SEA, MAVAIL_SEA, RMOL_SEA,             & ! IO,IO,I,IO,I,IO,
          &        AKHS_SEA, AKMS_SEA,                                                         & ! IO,IO,
          &        BR_SEA,                                                                     & ! dummy space holder
          &        CHS_SEA, CHS2_SEA, CQS2_SEA, HFX_SEA, QFX_SEA, FLX_LH_SEA, FLHC_SEA,        & ! 0,0,0,0,0,0,0,
          &        FLQC_SEA, QGH_SEA, CPM_SEA, CT_SEA, U10_SEA, V10_SEA, T02_SEA, TH02_SEA,    & ! 0,0,0,0,0,0,0,0,
          &        TSHLTR_SEA, TH10_SEA, Q02_SEA, QSHLTR_SEA, Q10_SEA, PSHLTR_SEA,             & ! 0,0,0,0,0,0,
          &        p1000,                                                                    & ! I
          &        ids,ide, jds,jde, kds,kde,                                                  &
          &        ims,ime, jms,jme, kms,kme,                                                  &
          &        its,ite, jts,jte, kts,kte    )

!
! Scale the appropriate terms between open-water values and ice-covered values
!

     DO j = JTS, JTE
        DO i = ITS, ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
              ! Over sea-ice points, blend the results.

              ! INTENT(OUT) from MYJSFC
              ! CHS  wait
              ! CHS2 wait
              ! CPM  wait
              ! CQS2 wait
              CT(i,j)     = CT(i,j)     * XICE(i,j) + (1.0-XICE(i,j)) * CT_SEA (i,j)
              ! FLHC(i,j)   = FLHC(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * FLHC_SEA (i,j)
              ! FLQC(i,j)   = FLQC(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * FLQC_SEA (i,j)
              ! FLX_LH wait
              ! HFX  wait
              PSHLTR(i,j) = PSHLTR(i,j) * XICE(i,j) + (1.0-XICE(i,j)) * PSHLTR_SEA(i,j)
              ! QFX  wait
              ! QGH  wait
              QSHLTR(i,j) = QSHLTR(i,j) * XICE(i,j) + (1.0-XICE(i,j)) * QSHLTR_SEA(i,j)
              Q02(i,j)    = Q02(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * Q02_SEA(i,j)
              Q10(i,j)    = Q10(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * Q10_SEA(i,j)
              TH02(i,j)   = TH02(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * TH02_SEA(i,j)
              TH10(i,j)   = TH10(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * TH10_SEA(i,j)
              TSHLTR(i,j) = TSHLTR(i,j) * XICE(i,j) + (1.0-XICE(i,j)) * TSHLTR_SEA(i,j)
              T02(i,j)    = T02(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * T02_SEA(i,j)
              U10(i,j)    = U10(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * U10_SEA(i,j)
              V10(i,j)    = V10(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * V10_SEA(i,j)

              ! INTENT(INOUT):  updated by MYJSFC
              ! QSFC:  wait
              THZ0(i,j)   = THZ0(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * THZ0_SEA(i,j)
              ! qz0 wait
              UZ0(i,j)    = UZ0(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * UZ0_SEA(i,j)
              VZ0(i,j)    = VZ0(i,j)    * XICE(i,j) + (1.0-XICE(i,j)) * VZ0_SEA(i,j)
              USTAR(i,j)  = USTAR(i,j)  * XICE(i,j) + (1.0-XICE(i,j)) * USTAR_SEA(i,j)
              ! ZNT wait
              PBLH(i,j)   = PBLH(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * PBLH_SEA(i,j)
              RMOL(i,j)   = RMOL(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * RMOL_SEA(i,j)
              AKHS(i,j)   = AKHS(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * AKHS_SEA(i,j)
              AKMS(i,j)   = AKMS(i,j)   * XICE(i,j) + (1.0-XICE(i,j)) * AKMS_SEA(i,j)

              !         tsk(i,j) = tsk(i,j)*XICE(i,j) + (1.0-XICE(i,j))*TSK_SEA(i,j)
           ELSE
              ! We're not over sea ice.  Take the results from the first call.
           ENDIF
        ENDDO
     ENDDO

   END SUBROUTINE myjsfc_seaice_wrapper

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

   SUBROUTINE sf_gfs_seaice_wrapper(U3D,V3D,T3D,QV3D,P3D,        &
		 CP,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,          &
                     ZNT,UST,PSIM,PSIH,                          &
                     XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,             &
                     QGH,QSFC,U10,V10,                           &
                     GZ1OZ0,WSPD,BR,ISFFLX,                      &
                     EP1,EP2,KARMAN,itimestep,                   &
                     XICE_THRESHOLD,                             &
                     CHS_SEA, CHS2_SEA, CPM_SEA, CQS2_SEA,       &
                     FLHC_SEA, FLQC_SEA,                         &
                     HFX_SEA, LH_SEA, QFX_SEA, QGH_SEA, QSFC_SEA,&
                     UST_SEA, ZNT_SEA, SST, XICE,                &
                     ids,ide, jds,jde, kds,kde,                  &
                     ims,ime, jms,jme, kms,kme,                  &
                     its,ite, jts,jte, kts,kte                   )
     USE module_sf_gfs
     implicit none

     INTEGER, INTENT(IN) ::             ids,ide, jds,jde, kds,kde,      &
                                        ims,ime, jms,jme, kms,kme,      &
                                        its,ite, jts,jte, kts,kte,      &
                                        ISFFLX,itimestep

      REAL,    INTENT(IN) ::                                            &
                                        CP,                             &
                                        EP1,                            &
                                        EP2,                            &
                                        KARMAN,                         &
                                        R,                              &
                                        ROVCP,                          &
                                        XLV

      REAL,    DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) ::      &
                                        P3D,                            &
                                        QV3D,                           &
                                        T3D,                            &
                                        U3D,                            &
                                        V3D

      REAL,    DIMENSION(ims:ime, jms:jme), INTENT(IN) ::               &
                                        TSK,                            &
                                        PSFC,                           &
                                        XLAND

      REAL,    DIMENSION(ims:ime, jms:jme), INTENT(INOUT) ::            &
                                        UST,                            &
                                        ZNT

      REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT) ::                 &
                                        BR,                             &
                                        CHS,                            &
                                        CHS2,                           &
                                        CPM,                            &
                                        CQS2,                           &
                                        FLHC,                           &
                                        FLQC,                           &
                                        GZ1OZ0,                         &
                                        HFX,                            &
                                        LH,                             &
                                        PSIM,                           &
                                        PSIH,                           &
                                        QFX,                            &
                                        QGH,                            &
                                        QSFC,                           &
                                        U10,                            &
                                        V10,                            &
                                        WSPD

      REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) ::                  &
                                        XICE
      REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT) ::                 &
                                        CHS_SEA,                        &
                                        CHS2_SEA,                       &
                                        CPM_SEA,                        &
                                        CQS2_SEA,                       &
                                        FLHC_SEA,                       &
                                        FLQC_SEA,                       &
                                        HFX_SEA,                        &
                                        LH_SEA,                         &
                                        QFX_SEA,                        &
                                        QGH_SEA,                        &
                                        QSFC_SEA,                       &
                                        UST_SEA,                        &
                                        ZNT_SEA
      REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) ::               &
                                        SST

      REAL,                              INTENT(IN)    ::               &
                                        XICE_THRESHOLD

!-------------------------------------------------------------------------
!   Local
!-------------------------------------------------------------------------
      INTEGER :: I
      INTEGER :: J
      REAL, DIMENSION(ims:ime, jms:jme) ::                              &
                                        BR_SEA,                         &
                                        GZ1OZ0_SEA,                     &
                                        PSIM_SEA,                       &
                                        PSIH_SEA,                       &
                                        U10_SEA,                        &
                                        V10_SEA,                        &
                                        WSPD_SEA,                       &
                                        XLAND_SEA,                &
                                        TSK_SEA,                        &
                                        UST_HOLD,                       &
                                        ZNT_HOLD,                       &
                                        TSK_LOCAL


     DO j = JTS , JTE
        DO i = ITS , ITE
           IF ( ( XICE(i,j) .GE. XICE_THRESHOLD ) .and. ( XICE(I,J) .LE. 1.0 ) ) THEN
              ! Sea-ice point

              IF ( SST(i,j) .LT. 271.4 ) THEN
                 SST(i,j) = 271.4
              ENDIF

              IF ( SST(i,j) .GT. 273. .and. itimestep .le. 3) then
                 ! Why the dependence on the time step count, here?
                 IF ( XICE(i,j) .GE. 0.6 ) THEN
                    SST(i,j) = 271.4
                 ELSEIF ( XICE(i,j) .GE. 0.4 ) THEN
                    SST(i,j) = 273.
                 ELSEIF (XICE(i,j).GE.0.2 .and. SST(i,j).GT.275.) THEN
                    SST(i,j) = 275.
                 ELSEIF (SST(i,j).GT.278.) THEN
                    SST(i,j) = 278.
                 ENDIF
              ENDIF
              TSK_SEA(i,j) = SST(i,j)

              ! The original TSK is taken to represent the blended
              ! result of the open-water values (SST) and the
              ! ice-covered value (the local TSK we derive here).

              TSK_LOCAL(i,j) = ( TSK(i,j) - (1.0-XICE(i,j)) * SST(i,j) ) / XICE(i,j)

              IF ( ( XICE(i,j) .LT. 0.2 ) .AND. ( TSK_LOCAL(i,j) .LT. 253.15 ) ) THEN
                 TSK_LOCAL(i,j) = 253.15
              ENDIF
              IF ( ( XICE(i,j) .LT. 0.1 ) .and. ( TSK_LOCAL(i,j) .LT. 263.15 ) ) THEN
                 TSK_LOCAL(i,j) = 263.15
              ENDIF

           ELSE
              ! land/open-water point
              TSK_LOCAL(i,j) = TSK(i,j)
           ENDIF

        ENDDO
     ENDDO

!
! Set up for frozen ocean call for sea ice points
!

! Strictly INTENT(IN), Should be unchanged by SF_GFS:
!     CP
!     EP1
!     EP2
!     KARMAN
!     R
!     ROVCP
!     XLV
!     P3D
!     QV3D
!     T3D
!     U3D
!     V3D
!     TSK
!     PSFC
!     XLAND
!     ISFFLX
!     ITIMESTEP


! Intent (INOUT), original value is used and changed by SF_GFS.
!     UST
!     ZNT

     ZNT_HOLD = ZNT
     UST_HOLD = UST

! Strictly INTENT (OUT), set by SF_GFS:
!     BR
!     CHS     -- used by LSM routines
!     CHS2    -- used by LSM routines
!     CPM     -- used by LSM routines
!     CQS2    -- used by LSM routines
!     FLHC
!     FLQC
!     GZ1OZ0
!     HFX     -- used by LSM routines
!     LH      -- used by LSM routines
!     PSIM
!     PSIH
!     QFX     -- used by LSM routines
!     QGH     -- used by LSM routines
!     QSFC    -- used by LSM routines
!     U10
!     V10
!     WSPD

!
! Frozen ocean / true land call.
!
     CALL SF_GFS(U3D,V3D,T3D,QV3D,P3D,                  &
          CP,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM_SEA,    &
          ZNT,UST,PSIM,PSIH,                            &
          XLAND,HFX,QFX,LH,TSK_LOCAL,FLHC,FLQC,         &
          QGH,QSFC,U10,V10,                             &
          GZ1OZ0,WSPD,BR,ISFFLX,                        &
          EP1,EP2,KARMAN,ITIMESTEP,                     &
          ids,ide, jds,jde, kds,kde,                    &
          ims,ime, jms,jme, kms,kme,                    &
          its,ite, jts,jte, kts,kte                     )

! Set up for open-water call

     DO j = JTS , JTE
        DO i = ITS , ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
              ! Sets up things for open ocean fraction of sea-ice points
              XLAND_SEA(i,j)=2.
              ZNT_SEA(I,J) = 0.0001
              IF ( SST(i,j) .LT. 271.4 ) THEN
                 SST(i,j) = 271.4
              ENDIF
              TSK_SEA(i,j) = SST(i,j)
           ELSE
              ! Fully open ocean or true land points
              XLAND_SEA(i,j)=xland(i,j)
              ZNT_SEA(I,J) = ZNT_HOLD(I,J)
              UST_SEA(i,j) = UST_HOLD(i,j)
              TSK_SEA(i,j) = TSK(i,j)
           ENDIF
        ENDDO
     ENDDO

     ! Open-water call
     ! _SEA variables are held for later use as the result of the open-water call.
     CALL SF_GFS(U3D,V3D,T3D,QV3D,P3D,                  &
          CP,ROVCP,R,XLV,PSFC,CHS_SEA,CHS2_SEA,CQS2_SEA,CPM,        &
          ZNT_SEA,UST_SEA,PSIM_SEA,PSIH_SEA,                        &
          XLAND,HFX_SEA,QFX_SEA,LH_SEA,TSK_SEA,FLHC_SEA,FLQC_SEA,   &
          QGH_SEA,QSFC_SEA,U10_SEA,V10_SEA,                         &
          GZ1OZ0_SEA,WSPD_SEA,BR_SEA,ISFFLX,                        &
          EP1,EP2,KARMAN,ITIMESTEP,                     &
          ids,ide, jds,jde, kds,kde,                    &
          ims,ime, jms,jme, kms,kme,                    &
          its,ite, jts,jte, kts,kte                     )

! Weighting, after our two calls to SF_GFS

     DO j = JTS , JTE
        DO i = ITS , ITE
           ! Over sea-ice points, weight the results.  Otherwise, just take the results from the
           ! first call to SF_GFS_
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
              ! Weight a number of fields (between open-water results
              ! and full ice results) by sea-ice fraction.

              BR(i,j)     = ( BR(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * BR_SEA(i,j)     )
              ! CHS, used by the LSM routines, is not updated yet.  Return results from both calls in separate variables
              ! CHS2, used by the LSM routines, is not updated yet.  Return results from both calls in separate variables
              ! CPM, used by the LSM routines, is not updated yet.  Return results from both calls in separate variables
              ! CQS2, used by the LSM routines, is not updated yet.  Return results from both calls in separate variables
              ! FLHC(i,j) = ( FLHC(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * FLHC_SEA(i,j) )
              ! FLQC(i,j) = ( FLQC(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * FLQC_SEA(i,j) )
              GZ1OZ0(i,j) = ( GZ1OZ0(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * GZ1OZ0_SEA(i,j) )
              ! HFX, used by the LSM routines, is not updated yet.  Return results from both calls in separate variables
              ! LH, used by the LSM routines, is not updated yet.  Return results from both calls in separate variables
              PSIM(i,j)   = ( PSIM(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * PSIM_SEA(i,j)   )
              PSIH(i,j)   = ( PSIH(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * PSIH_SEA(i,j)   )
              ! QFX, used by the LSM routines, is not updated yet.  Return results from both calls in separate variables
              ! QGH, used by the LSM routines, is not updated yet.  Return results from both calls in separate variables
              ! QSFC, used by the LSM routines, is not updated yet.  Return results from both calls in separate variables
              U10(i,j)    = ( U10(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * U10_SEA(i,j)    )
              V10(i,j)    = ( V10(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * V10_SEA(i,j)    )
              WSPD(i,j)   = ( WSPD(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * WSPD_SEA(i,j)   )
              ! UST, used by the LSM routines, is not updated yet.  Return results from both calls in separate variables
              ! ZNT, used by the LSM routines, is not updated yet.  Return results from both calls in separate variables

           ENDIF
        ENDDO
     ENDDO

   END SUBROUTINE sf_gfs_seaice_wrapper

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

   SUBROUTINE sfclay_seaice_wrapper(U3D,V3D,T3D,QV3D,P3D,dz8w,     &
                     CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,      &
                     ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                     XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,QGH,QSFC,RMOL, &
                     U10,V10,TH2,T2,Q2,                            &
                     GZ1OZ0,WSPD,BR,ISFFLX,DX,                     &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                     KARMAN,EOMEG,STBOLT,                          &
                     P1000,                                      &
XICE,SST,TSK_SEA,                                                  &
CHS2_SEA,CHS_SEA,CPM_SEA,CQS2_SEA,FLHC_SEA,FLQC_SEA,               &
HFX_SEA,LH_SEA,QFX_SEA,QGH_SEA,QSFC_SEA,ZNT_SEA,                   &
ITIMESTEP,XICE_THRESHOLD,                                          &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte,                    &
                     ustm,ck,cka,cd,cda,isftcflx,iz0tlnd           )
     USE module_sf_sfclay
     implicit none

     INTEGER,  INTENT(IN )   ::        ids,ide, jds,jde, kds,kde,  &
                                       ims,ime, jms,jme, kms,kme,  &
                                       its,ite, jts,jte, kts,kte

     INTEGER,  INTENT(IN )   ::        ISFFLX
     REAL,     INTENT(IN )   ::        SVP1,SVP2,SVP3,SVPT0
     REAL,     INTENT(IN )   ::        EP1,EP2,KARMAN,EOMEG,STBOLT
     REAL,     INTENT(IN )   ::        P1000

     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                           dz8w

     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                           QV3D, &
                                                             P3D, &
                                                             T3D

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(IN   )               ::             MAVAIL, &
                                                            PBLH, &
                                                           XLAND, &
                                                             TSK
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(OUT  )               ::                U10, &
                                                             V10, &
                                                             TH2, &
                                                              T2, &
                                                              Q2, &
                                                            QSFC
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)               ::             REGIME, &
                                                             HFX, &
                                                             QFX, &
                                                              LH, &
                                                         MOL,RMOL

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                 GZ1OZ0,WSPD,BR, &
                                                        PSIM,PSIH

     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                            U3D, &
                                                             V3D

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(IN   )               ::               PSFC

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                            ZNT, &
                                                             ZOL, &
                                                             UST, &
                                                             CPM, &
                                                            CHS2, &
                                                            CQS2, &
                                                             CHS

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                      FLHC,FLQC

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                                 &
                                                              QGH

     REAL,     INTENT(IN   )               ::   CP,G,ROVCP,R,XLV,DX

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme )              , &
               INTENT(OUT)     ::              ck,cka,cd,cda,ustm

     INTEGER,  OPTIONAL,  INTENT(IN )   ::     ISFTCFLX,IZ0TLND

!--------------------------------------------------------------------
!    New for wrapper
!--------------------------------------------------------------------
     INTEGER,  INTENT(IN)               ::      ITIMESTEP
     REAL,     INTENT(IN)               ::      XICE_THRESHOLD
     REAL,     DIMENSION( ims:ime, jms:jme ),                     &
               INTENT(IN)               ::      XICE
     REAL,     DIMENSION( ims:ime, jms:jme ),                     &
               INTENT(INOUT)            ::      SST
     REAL,     DIMENSION( ims:ime, jms:jme ),                     &
               INTENT(OUT)              ::      TSK_SEA,          &
                                                CHS2_SEA,         &
                                                CHS_SEA,          &
                                                CPM_SEA,          &
                                                CQS2_SEA,         &
                                                FLHC_SEA,         &
                                                FLQC_SEA,         &
                                                HFX_SEA,          &
                                                LH_SEA,           &
                                                QFX_SEA,          &
                                                QGH_SEA,          &
                                                QSFC_SEA,         &
                                                ZNT_SEA

!--------------------------------------------------------------------
!    Local
!--------------------------------------------------------------------
     INTEGER :: I, J
     REAL,     DIMENSION( ims:ime, jms:jme ) :: XLAND_SEA,        &
                                                MAVAIL_sea,       &
                                                TSK_LOCAL,        &
                                                BR_HOLD,          &
                                                CHS2_HOLD,        &
                                                CHS_HOLD,         &
                                                CPM_HOLD,         &
                                                CQS2_HOLD,        &
                                                FLHC_HOLD,        &
                                                FLQC_HOLD,        &
                                                GZ1OZ0_HOLD,      &
                                                HFX_HOLD,         &
                                                LH_HOLD,          &
                                                MOL_HOLD,         &
                                                PSIH_HOLD,        &
                                                PSIM_HOLD,        &
                                                QFX_HOLD,         &
                                                QGH_HOLD,         &
                                                REGIME_HOLD,      &
                                                RMOL_HOLD,        &
                                                UST_HOLD,         &
                                                WSPD_HOLD,        &
                                                ZNT_HOLD,         &
                                                ZOL_HOLD,         &
                                                CD_SEA,           &
                                                CDA_SEA,          &
                                                CK_SEA,           &
                                                CKA_SEA,          &
                                                Q2_SEA,           &
                                                T2_SEA,           &
                                                TH2_SEA,          &
                                                U10_SEA,          &
                                                USTM_SEA,         &
                                                V10_SEA

     REAL,     DIMENSION( ims:ime, jms:jme ) ::                   &
                                                BR_SEA,           &
                                                GZ1OZ0_SEA,       &
                                                MOL_SEA,          &
                                                PSIH_SEA,         &
                                                PSIM_SEA,         &
                                                REGIME_SEA,       &
                                                RMOL_SEA,         &
                                                UST_SEA,          &
                                                WSPD_SEA,         &
                                                ZOL_SEA
! INTENT(IN) to SFCLAY; unchanged by the call
      ! ISFFLX
      ! SVP1,SVP2,SVP3,SVPT0
      ! EP1,EP2,KARMAN,EOMEG,STBOLT
      ! CP,G,ROVCP,R,XLV,DX
      ! ISFTCFLX,IZ0TLND
      ! P1000
      ! dz8w
      ! QV3D
      ! P3D
      ! T3D
      ! MAVAIL
      ! PBLH
      ! XLAND
      ! TSK
      ! U3D
      ! V3D
      ! PSFC

     DO j = JTS , JTE
        DO i = ITS , ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) )  THEN

              IF ( SST(i,j) .LT. 271.4 ) THEN
                 SST(i,j) = 271.4
              ENDIF
              IF ( SST(i,j) .GT. 273. .AND. itimestep .le. 3) THEN
                 IF ( XICE(i,j) .GE. 0.6 ) THEN
                    SST(i,j) = 271.4
                 ELSEIF ( XICE(i,j) .GE. 0.4 ) THEN
                    SST(i,j) = 273.
                 ELSEIF (XICE(i,j).GE.0.2 .and. SST(i,j).GT.275.) THEN
                    SST(i,j) = 275.
                 ELSEIF (SST(i,j).GT.278.) THEN
                    SST(i,j) = 278.
                 ENDIF
              ENDIF
              TSK_SEA(i,j) = SST(i,j)

              TSK_LOCAL(i,j) = ( TSK(i,j) - (1.0-XICE(i,j)) *SST(i,j) ) / XICE(i,j)
              IF (XICE(i,j) .lt. 0.2 .and. TSK(i,j) .lt. 253.15) THEN
                 TSK_LOCAL(i,j) = 253.15
              ENDIF
              IF (XICE(i,j) .lt. 0.1 .and. TSK(i,j) .lt. 263.15) THEN
                 TSK_LOCAL(i,j) = 263.15
              ENDIF
           ELSE
              TSK_SEA(i,j) = TSK(i,j)
              TSK_LOCAL(i,j) = TSK(i,j)
           ENDIF
        ENDDO
     ENDDO


! INTENT (INOUT) to SFCLAY:  Save the variables before the first call
! (for land/frozen water) to SFCLAY, to keep from double-counting the
! effects of that routine
     BR_HOLD   = BR
     CHS2_HOLD = CHS2
     CHS_HOLD  = CHS
     CPM_HOLD  = CPM
     CQS2_HOLD = CQS2
     FLHC_HOLD = FLHC
     FLQC_HOLD = FLQC
     GZ1OZ0_HOLD = GZ1OZ0
     HFX_HOLD  = HFX
     LH_HOLD   = LH
     MOL_HOLD  = MOL
     PSIH_HOLD = PSIH
     PSIM_HOLD = PSIM
     QFX_HOLD  = QFX
     QGH_HOLD  = QGH
     REGIME_HOLD = REGIME
     RMOL_HOLD = RMOL
     UST_HOLD  = UST
     WSPD_HOLD = WSPD
     ZNT_HOLD  = ZNT
     ZOL_HOLD  = ZOL

! INTENT(OUT) from SFCLAY.  Input shouldn't matter, but we'll want to
! keep things around for weighting after the second call to SFCLAY.
     ! CD
     ! CDA
     ! CK
     ! CKA
     ! Q2
     ! QSFC
     ! T2
     ! TH2
     ! U10
     ! USTM
     ! V10


     ! land/frozen-water call
     call sfclay(U3D,V3D,T3D,QV3D,P3D,dz8w,                    & ! I
                 CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,      & ! I,I,I,I,I,I,IO,IO,IO,IO,
                 ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                 XLAND,HFX,QFX,LH,TSK_LOCAL,FLHC,FLQC,QGH,QSFC,RMOL, &
                 U10,V10,TH2,T2,Q2,                            &
                 GZ1OZ0,WSPD,BR,ISFFLX,DX,                     &
                 SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                 KARMAN,EOMEG,STBOLT,                          &
                 P1000,                                      &
                 ids,ide, jds,jde, kds,kde,                    &
                 ims,ime, jms,jme, kms,kme,                    &
                 its,ite, jts,jte, kts,kte,                    &
                 ustm,ck,cka,cd,cda,isftcflx,iz0tlnd           )

     ! Set up for open-water call
     DO j = JTS , JTE
        DO i = ITS , ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
              XLAND_SEA(i,j)=2.
              MAVAIL_SEA(I,J)  =1.
              ZNT_SEA(I,J) = 0.0001
              TSK_SEA(i,j) = SST(i,j)
              IF ( SST(i,j) .LT. 271.4 ) THEN
                 SST(i,j) = 271.4
                 TSK_SEA(i,j) = SST(i,j)
              ENDIF
           ELSE
              XLAND_SEA(i,j) = XLAND(i,j)
              MAVAIL_SEA(i,j) = MAVAIL(i,j)
              ZNT_SEA(i,j)  = ZNT_HOLD(i,j)
              TSK_SEA(i,j) = TSK_LOCAL(i,j)
           ENDIF
        ENDDO
     ENDDO

     ! Restore the values from before the land/frozen-water call
     BR_SEA   = BR_HOLD
     CHS2_SEA = CHS2_HOLD
     CHS_SEA  = CHS_HOLD
     CPM_SEA  = CPM_HOLD
     CQS2_SEA = CQS2_HOLD
     FLHC_SEA = FLHC_HOLD
     FLQC_SEA = FLQC_HOLD
     GZ1OZ0_SEA = GZ1OZ0_HOLD
     HFX_SEA  = HFX_HOLD
     LH_SEA   = LH_HOLD
     MOL_SEA  = MOL_HOLD
     PSIH_SEA = PSIH_HOLD
     PSIM_SEA = PSIM_HOLD
     QFX_SEA  = QFX_HOLD
     QGH_SEA  = QGH_HOLD
     REGIME_SEA = REGIME_HOLD
     RMOL_SEA = RMOL_HOLD
     UST_SEA  = UST_HOLD
     WSPD_SEA = WSPD_HOLD
     ZOL_SEA  = ZOL_HOLD

     ! open-water call
     call sfclay(U3D,V3D,T3D,QV3D,P3D,dz8w,                    & ! I
                 CP,G,ROVCP,R,XLV,PSFC,                        & ! I
                 CHS_SEA,CHS2_SEA,CQS2_SEA,CPM_SEA,            & ! I/O
                 ZNT_SEA,UST_SEA,                              & ! I/O
                 PBLH,MAVAIL_SEA,                              & ! I
                 ZOL_SEA,MOL_SEA,REGIME_SEA,PSIM_SEA,PSIH_SEA, & ! I/O
                 XLAND_SEA,                              & ! I
                 HFX_SEA,QFX_SEA,LH_SEA,                       & ! I/O
                 TSK_SEA,                                      & ! I
                 FLHC_SEA,FLQC_SEA,QGH_SEA,QSFC_sea,RMOL_SEA,  & ! I/O
                 U10_sea,V10_sea,TH2_sea,T2_sea,Q2_sea,        & ! O
                 GZ1OZ0_SEA,WSPD_SEA,BR_SEA,                   & ! I/O
                 ISFFLX,DX,                                    &
                 SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                 KARMAN,EOMEG,STBOLT,                          &
                 P1000,                                      &
                 ids,ide, jds,jde, kds,kde,                    &
                 ims,ime, jms,jme, kms,kme,                    &
                 its,ite, jts,jte, kts,kte,                    & ! 0
                 ustm_sea,ck_sea,cka_sea,cd_sea,cda_sea,isftcflx,iz0tlnd )

     DO j = JTS , JTE
        DO i = ITS, ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD )  .and.( XICE(i,j) .LE. 1.0 ) ) THEN
              ! weighted average for sea ice points
              br(i,j)     = ( br(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * br_sea(i,j)     )
              ! CHS2 -- wait
              ! CHS  -- wait
              ! CPM  -- wait
              ! CQS2 -- wait
              ! FLHC -- wait
              ! FLQC -- wait
              gz1oz0(i,j) = ( gz1oz0(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * gz1oz0_sea(i,j) )
              ! HFX  -- wait
              ! LH   -- wait
              mol(i,j)    = ( mol(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * mol_sea(i,j)    )
              psih(i,j)   = ( psih(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * psih_sea(i,j)   )
              psim(i,j)   = ( psim(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * psim_sea(i,j)   )
              ! QFX  -- wait
              ! QGH  -- wait
              if ( XICE(i,j).GE. 0.5 ) regime(i,j) = regime_hold(i,j)
              rmol(i,j)   = ( rmol(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * rmol_sea(i,j)   )
              ust(i,j)    = ( ust(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * ust_sea(i,j)    )
              wspd(i,j)   = ( wspd(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * wspd_sea(i,j)   )
              zol(i,j)    = ( zol(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * zol_sea(i,j)    )
              ! INTENT(OUT) --------------------------------------------------------------------
              IF ( PRESENT ( CD ) ) THEN
                 CD(i,j)     = ( CD(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * CD_sea(i,j)     )
              ENDIF
              IF ( PRESENT ( CDA ) ) THEN
                 CDA(i,j)     = ( CDA(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * CDA_sea(i,j)     )
              ENDIF
              IF ( PRESENT ( CK ) ) THEN
                 CK(i,j)     = ( CK(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * CK_sea(i,j)     )
              ENDIF
              IF ( PRESENT ( CKA ) ) THEN
                 CKA(i,j)     = ( CKA(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * CKA_sea(i,j)     )
              ENDIF
              q2(i,j)     = ( q2(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * q2_sea(i,j)     )
              ! QSFC -- wait
              t2(i,j)     = ( t2(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * t2_sea(i,j)     )
              th2(i,j)    = ( th2(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * th2_sea(i,j)    )
              u10(i,j)    = ( u10(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * u10_sea(i,j)    )
              IF ( PRESENT ( USTM ) ) THEN
                 USTM(i,j)    = ( USTM(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * USTM_sea(i,j)    )
              ENDIF
              v10(i,j)    = ( v10(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * v10_sea(i,j)    )
           ENDIF
        END DO
     END DO
!
!         tsk(i,j) = tsk(i,j)*XICE(i,j) + (1.0-XICE(i,j))*TSK_SEA(i,j)
!
   END SUBROUTINE sfclay_seaice_wrapper

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

   SUBROUTINE pxsfclay_seaice_wrapper(U3D,V3D,T3D,TH3D,QV3D,P3D,dz8w, &
                     CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,      &
                     ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                     XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,QGH,QSFC,RMOL, &
                     U10,V10,                                      &
                     GZ1OZ0,WSPD,BR,ISFFLX,DX,                     &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,          &
XICE, SST, ITIMESTEP, XICE_THRESHOLD,                              &
CHS_SEA, CHS2_SEA, CPM_SEA, CQS2_SEA, FLHC_SEA, FLQC_SEA,          &
HFX_SEA, LH_SEA, QFX_SEA, QGH_SEA, QSFC_SEA, TSK_SEA,  &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )
     USE module_sf_pxsfclay
     implicit none
     INTEGER,  INTENT(IN )   ::        ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte

     INTEGER,  INTENT(IN )   ::        ISFFLX
     REAL,     INTENT(IN )   ::        SVP1,SVP2,SVP3,SVPT0
     REAL,     INTENT(IN )   ::        EP1,EP2,KARMAN

     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                           dz8w

     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                           QV3D, &
                                                             P3D, &
                                                             T3D, &
                                                            TH3D

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(IN   )               ::             MAVAIL, &
                                                            PBLH, &
                                                           XLAND, &
                                                             TSK
     REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )           , &
               INTENT(IN   )   ::                            U3D, &
                                                             V3D

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(IN   )               ::               PSFC

     REAL,     INTENT(IN   )                  ::   CP,G,ROVCP,R,XLV,DX

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(OUT  )               ::                U10, &
                                                             V10, &
                                                            QSFC
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)               ::             REGIME, &
                                                             HFX, &
                                                             QFX, &
                                                              LH, &
                                                         MOL,RMOL
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                 GZ1OZ0,WSPD,BR, &
                                                       PSIM,PSIH

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                            ZNT, &
                                                             ZOL, &
                                                             UST, &
                                                             CPM, &
                                                            CHS2, &
                                                            CQS2, &
                                                             CHS

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                      FLHC,FLQC

     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)   ::                            QGH

!--------------------------------------------------------------------
!    For wrapper
!--------------------------------------------------------------------

     INTEGER,  INTENT(IN)                           :: ITIMESTEP
     REAL,     INTENT(IN)                           :: XICE_THRESHOLD
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(IN)                           ::      XICE
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(OUT)                        ::     TSK_SEA
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)              ::                 SST

!--------------------------------------------------------------------
!    Local
!--------------------------------------------------------------------
     INTEGER :: I, J
     REAL,     DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(OUT)    ::                         CHS_SEA, &
                                                        CHS2_SEA, &
                                                         CPM_SEA, &
                                                        CQS2_SEA, &
                                                        FLHC_SEA, &
                                                        FLQC_SEA, &
                                                         HFX_SEA, &
                                                          LH_SEA, &
                                                         QFX_SEA, &
                                                         QGH_SEA, &
                                                        QSFC_SEA

     REAL,     DIMENSION( ims:ime, jms:jme ) ::          BR_HOLD, &
                                                        CHS_HOLD, &
                                                       CHS2_HOLD, &
                                                        CPM_HOLD, &
                                                       CQS2_HOLD, &
                                                       FLHC_HOLD, &
                                                       FLQC_HOLD, &
                                                     GZ1OZ0_HOLD, &
                                                        HFX_HOLD, &
                                                         LH_HOLD, &
                                                        MOL_HOLD, &
                                                       PSIH_HOLD, &
                                                       PSIM_HOLD, &
                                                        QFX_HOLD, &
                                                        QGH_HOLD, &
                                                     REGIME_HOLD, &
                                                       RMOL_HOLD, &
                                                        UST_HOLD, &
                                                       WSPD_HOLD, &
                                                        ZNT_HOLD, &
                                                        ZOL_HOLD, &
                                                       TSK_LOCAL

     REAL,     DIMENSION( ims:ime, jms:jme ) ::        XLAND_SEA, &
                                                      MAVAIL_SEA, &
                                                          BR_SEA, &
                                                      GZ1OZ0_SEA, &
                                                         MOL_SEA, &
                                                        PSIH_SEA, &
                                                        PSIM_SEA, &
                                                      REGIME_SEA, &
                                                        RMOL_SEA, &
                                                         UST_SEA, &
                                                        WSPD_SEA, &
                                                         ZNT_SEA, &
                                                         ZOL_SEA, &
                                                         U10_SEA, &
                                                         V10_SEA

     DO j = JTS , JTE
        DO i = ITS , ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .AND. ( XICE(i,j) .LE. 1.0 ) )  THEN

              IF ( SST(i,j) .LT. 271.4 ) THEN
                 SST(i,j) = 271.4
              ENDIF
              IF ( SST(i,j) .GT. 273. .AND. itimestep .le. 3) THEN
                 IF ( XICE(i,j) .GE. 0.6 ) THEN
                    SST(i,j) = 271.4
                 ELSEIF ( XICE(i,j) .GE. 0.4 ) THEN
                    SST(i,j) = 273.
                 ELSEIF (XICE(i,j).GE.0.2 .and. SST(i,j).GT.275.) THEN
                    SST(i,j) = 275.
                 ELSEIF (SST(i,j).GT.278.) THEN
                    SST(i,j) = 278.
                 ENDIF
              ENDIF
              TSK_SEA(i,j) = SST(i,j)

              TSK_LOCAL(i,j) = ( TSK(i,j) - (1.0-XICE(i,j)) *SST(i,j) ) / XICE(i,j)
              IF (XICE(i,j) .lt. 0.2 .and. TSK(i,j) .lt. 253.15) THEN
                 TSK_LOCAL(i,j) = 253.15
              ENDIF
              IF (XICE(i,j) .lt. 0.1 .and. TSK(i,j) .lt. 263.15) THEN
                 TSK_LOCAL(i,j) = 263.15
              ENDIF
           ELSE
              TSK_SEA(i,j) = TSK(i,j)
              TSK_LOCAL(i,j) = TSK(i,j)
           ENDIF
        ENDDO
     ENDDO
!
! INTENT (INOUT) to PXSFCLAY:  Save the variables before the first call
! (for land/frozen water) to SFCLAY, to keep from double-counting the
! effects of that routine
!
     BR_HOLD     = BR
     CHS_HOLD    = CHS
     CHS2_HOLD   = CHS2
     CPM_HOLD    = CPM
     CQS2_HOLD   = CQS2
     FLHC_HOLD   = FLHC
     FLQC_HOLD   = FLQC
     GZ1OZ0_HOLD = GZ1OZ0
     HFX_HOLD    = HFX
     LH_HOLD     = LH
     MOL_HOLD    = MOL
     PSIH_HOLD   = PSIH
     PSIM_HOLD   = PSIM
     QFX_HOLD    = QFX
     QGH_HOLD    = QGH
     REGIME_HOLD = REGIME
     RMOL_HOLD   = RMOL
     UST_HOLD    = UST
     WSPD_HOLD   = WSPD
     ZNT_HOLD    = ZNT
     ZOL_HOLD    = ZOL

! INTENT(OUT) from PXSFCLAY.  Input shouldn't matter, but we'll want to
! keep things around for weighting after the second call to PXSFCLAY.
     ! U10
     ! V10
     ! QSFC

! Land/frozen-water call.
     CALL pxsfclay(U3D,V3D,T3D,TH3D,QV3D,P3D,dz8w,                 &
                     CP,G,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,      &
                     ZNT,UST,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                     XLAND,HFX,QFX,LH,TSK_LOCAL,FLHC,FLQC,QGH,QSFC,RMOL, &
                     U10,V10,                                      &
                     GZ1OZ0,WSPD,BR,ISFFLX,DX,                     &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,          &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )

     DO j = JTS , JTE
        DO i= ITS , ITE
           IF( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
              ! Sets up things for open ocean.
              XLAND_SEA(i,j)=2.
              MAVAIL_SEA(I,J)  =1.
              ZNT_SEA(I,J) = 0.0001
              TSK_SEA(i,j)  = SST(i,j)
              if ( SST(i,j) .LT. 271.4 ) then
                 SST(i,j) = 271.4
                 TSK_SEA(i,j) = SST(i,j)
              endif
           ELSE
              XLAND_SEA(i,j)=xland(i,j)
              MAVAIL_SEA(i,j) = mavail(i,j)
              ZNT_SEA(I,J)  = ZNT_HOLD(I,J)
              TSK_SEA(i,j)  = TSK(i,j)
           ENDIF
        ENDDO
     ENDDO

     ! INTENT(INOUT) variables held over from before the first call to PXSFCLAY:
     BR_SEA     = BR_HOLD
     CHS_SEA    = CHS_HOLD
     CHS2_SEA   = CHS2_HOLD
     CPM_SEA    = CPM_HOLD
     CQS2_SEA   = CQS2_HOLD
     FLHC_SEA   = FLHC_HOLD
     FLQC_SEA   = FLQC_HOLD
     GZ1OZ0_SEA = GZ1OZ0_HOLD
     HFX_SEA    = HFX_HOLD
     LH_SEA     = LH_HOLD
     MOL_SEA    = MOL_HOLD
     PSIH_SEA   = PSIH_HOLD
     PSIM_SEA   = PSIM_HOLD
     QFX_SEA    = QFX_HOLD
     QGH_SEA    = QGH_HOLD
     REGIME_SEA = REGIME_HOLD
     RMOL_SEA   = RMOL_HOLD
     UST_SEA    = UST_HOLD
     WSPD_SEA   = WSPD_HOLD
     ZOL_SEA    = ZOL_HOLD

! Open-water call.
     ! Variables newly set (INTENT(OUT)) or changed (INTENT(INOUT)) by
     ! PXSFCLAY are here appended with the "_SEA" label.
     ! Special intent(IN) variables here:  XLAND_SEA, MAVAIL_SEA, TSK_SEA
     CALL pxsfclay(U3D,V3D,T3D,TH3D,QV3D,P3D,dz8w,                 &
                     CP,G,ROVCP,R,XLV,PSFC,CHS_SEA,CHS2_SEA,CQS2_SEA,CPM_SEA,      &
                     ZNT_SEA,UST_SEA,PBLH,MAVAIL_SEA,ZOL_SEA,MOL_SEA,REGIME_SEA,PSIM_SEA,PSIH_SEA, &
                     XLAND_SEA,HFX_SEA,QFX_SEA,LH_SEA,TSK_SEA,FLHC_SEA,FLQC_SEA,QGH_SEA,QSFC_SEA,RMOL_SEA, &
                     U10_SEA,V10_SEA,                              &
                     GZ1OZ0_SEA,WSPD_SEA,BR_SEA,ISFFLX,DX,         &
                     SVP1,SVP2,SVP3,SVPT0,EP1,EP2,KARMAN,          &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )

     DO j = JTS , JTE
        DO i = ITS , ITE
           IF ( ( XICE(I,J) .GE. XICE_THRESHOLD ) .and. ( XICE(i,j) .LE. 1.0 ) ) THEN
              ! INTENT (INOUT) for PXSFCLAY:
              br(i,j)     = ( br(i,j)     * XICE(i,j) ) + ( (1.0-XICE(i,j)) * br_sea(i,j)     )
              gz1oz0(i,j) = ( gz1oz0(i,j) * XICE(i,j) ) + ( (1.0-XICE(i,j)) * gz1oz0_sea(i,j) )
              mol(i,j)    = ( mol(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * mol_sea(i,j)    )
              psih(i,j)   = ( psih(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * psih_sea(i,j)   )
              psim(i,j)   = ( psim(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * psim_sea(i,j)   )
              rmol(i,j)   = ( rmol(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * rmol_sea(i,j)   )
              ust(i,j)    = ( ust(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * ust_sea(i,j)    )
              wspd(i,j)   = ( wspd(i,j)   * XICE(i,j) ) + ( (1.0-XICE(i,j)) * wspd_sea(i,j)   )
              zol(i,j)    = ( zol(i,j)    * XICE(i,j) ) + ( (1.0-XICE(i,j)) * zol_sea(i,j)    )
              ! REGIME:  Special case for this variable.  Just take the land values.
              ! CHS -- wait
              ! CHS2 -- wait
              ! CPM -- wait
              ! CQS2 -- wait
              ! FLHC -- wait
              ! FLQC -- wait
              ! HFX -- wait
              ! LH -- wait
              ! QFX -- wait
              ! QGH -- wait

              ! INTENT (OUT) from PXSFCLAY:
              u10(i,j) = ( u10(i,j)       * XICE(i,j) ) + ( (1.0-XICE(i,j)) * u10_sea(i,j)    )
              v10(i,j) = ( v10(i,j)       * XICE(i,j) ) + ( (1.0-XICE(i,j)) * v10_sea(i,j)    )
              ! QSFC -- wait
           ENDIF
        ENDDO
     ENDDO

   END SUBROUTINE pxsfclay_seaice_wrapper

!-------------------------------------------------------------------------

   SUBROUTINE TOPO_RAD_ADJ_DRVR (XLAT,XLONG,COSZEN,               &
                    shadowmask,                                   &
                    declin,                                       &
                    SWDOWN,GSW,SWNORM,GSWSAVE,solcon,hrang2d,     &
                    slope_in,slp_azi_in,                          &
                ids, ide, jds, jde, kds, kde,                     &
                ims, ime, jms, jme, kms, kme,                     &
                its, ite, jts, jte, kts, kte                      )
!------------------------------------------------------------------
   IMPLICIT NONE
!------------------------------------------------------------------
   INTEGER, INTENT(IN)   ::       its,ite,jts,jte,kts,kte,        &
                                  ims,ime,jms,jme,kms,kme,        &
                                  ids,ide,jds,jde,kds,kde
   INTEGER, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(IN)      ::       shadowmask
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN   )   ::       XLAT,XLONG
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(INOUT)   ::       SWDOWN,GSW,SWNORM,GSWSAVE
   real,intent(in)  :: solcon   
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   ) :: hrang2d,coszen 


   REAL, INTENT(IN    )  ::       declin
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   ) :: slope_in,slp_azi_in


! LOCAL VARS
   integer    :: i,j
   real       :: pi,degrad
   integer    :: shadow
   real       :: swdown_teradj,swdown_in,xlat1,xlong1

!------------------------------------------------------------------

     pi = 4.*atan(1.)
     degrad=pi/180.

       DO J=jts,jte
       DO I=its,ite
         SWNORM(i,j) = SWDOWN(i,j)     ! save
         IF(SWDOWN(I,J) .GT. 1.E-3)THEN  ! daytime
             shadow = shadowmask(i,j)

         SWDOWN_IN = SWDOWN(i,j)
         XLAT1 = XLAT(i,j)
         XLONG1 = XLONG(i,j)
         CALL TOPO_RAD_ADJ (XLAT1,XLONG1,COSZEN(i,j),             &
                    DECLIN,DEGRAD,                                &
                    SWDOWN_IN,solcon,hrang2d(i,j),SWDOWN_teradj,  &
                    kts,kte,                                      &
                    slope_in(i,j),slp_azi_in(i,j),                &
                    shadow , i,j                                  &
                    )

         GSWSAVE(I,J) = GSW(I,J)       ! save
         GSW(I,J) = GSW(I,J)*SWDOWN_teradj/SWDOWN(i,j)
         SWDOWN(i,j) = SWDOWN_teradj

         ENDIF ! daytime
       ENDDO  ! i_loop
       ENDDO  ! j_loop


   END SUBROUTINE TOPO_RAD_ADJ_DRVR
!------------------------------------------------------------------
!------------------------------------------------------------------
   SUBROUTINE TOPO_RAD_ADJ (XLAT1,XLONG1,COSZEN,                 &
                    DECLIN,DEGRAD,                               &
                    SWDOWN_IN,solcon,hrang,SWDOWN_teradj,        &
                    kts,kte,                                     &
                    slope,slp_azi,                               &
                    shadow                                       &
                    ,i,j)

!------------------------------------------------------------------
   IMPLICIT NONE
!------------------------------------------------------------------
  INTEGER, INTENT(IN)       :: kts,kte
  REAL, INTENT(IN)          :: COSZEN,DECLIN,              &
                               XLAT1,XLONG1,DEGRAD
  REAL, INTENT(IN)          :: SWDOWN_IN,solcon,hrang
  INTEGER, INTENT(IN)       :: shadow
  REAL, INTENT(IN)          :: slp_azi,slope

  REAL, INTENT(OUT)         :: SWDOWN_teradj

! LOCAL VARS
   REAL            :: XT24,TLOCTM,CSZA,XXLAT
   REAL            :: diffuse_frac,corr_fac,csza_slp
   integer         :: i,j


!------------------------------------------------------------------

     SWDOWN_teradj=SWDOWN_IN

     CSZA=COSZEN
     XXLAT=XLAT1*DEGRAD

! RETURN IF NIGHT
         IF(CSZA.LE.1.E-9) return 
        
!  Parameterize diffuse fraction of global solar radiation as a function of the ratio between TOA radiation and surface global radiation
              diffuse_frac = min(1.,1./(max(0.1,2.1-2.8*log(log(csza*solcon/max(SWDOWN_IN,1.e-3))))))
        if ((slope.eq.0).or.(diffuse_frac.eq.1).or.(csza.lt.1.e-2)) then  ! no topographic effects when all radiation diffuse or sun too close to horizon
          corr_fac = 1
          goto 140
        endif

! cosine of zenith angle over sloping topography
        csza_slp = ((SIN(XXLAT)*COS(HRANG))*                                          &
                    (-cos(slp_azi)*sin(slope))-SIN(HRANG)*(sin(slp_azi)*sin(slope))+  &
                    (COS(XXLAT)*COS(HRANG))*cos(slope))*                              &
                   COS(DECLIN)+(COS(XXLAT)*(cos(slp_azi)*sin(slope))+                 &
                   SIN(XXLAT)*cos(slope))*SIN(DECLIN)
        IF(csza_slp.LE.1.E-4) csza_slp = 0

! Topographic shading
        if (shadow.eq.1) csza_slp = 0

! Correction factor for sloping topography; the diffuse fraction of solar radiation is assumed to be unaffected by the slope
        corr_fac = diffuse_frac + (1-diffuse_frac)*csza_slp/csza

 140        continue

      SWDOWN_teradj=(1.)*SWDOWN_IN*corr_fac

   END SUBROUTINE TOPO_RAD_ADJ

!-------------------------------------------------------------------------

END MODULE module_surface_driver
