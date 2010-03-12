!WRF:MEDIATION_LAYER:SOLVER

#define BENCH_START(A)
#define BENCH_END(A)

MODULE module_first_rk_step_part1

CONTAINS

  SUBROUTINE first_rk_step_part1 (   grid , config_flags              &
                             , moist , moist_tend               &
                             , chem  , chem_tend                &
                             , tracer, tracer_tend              &
                             , scalar , scalar_tend             &
                             , fdda3d, fdda2d                   &
                             , ru_tendf, rv_tendf               &
                             , rw_tendf, t_tendf                &
                             , ph_tendf, mu_tendf               &
                             , tke_tend                         &
                             , adapt_step_flag , curr_secs      &
                             , psim , psih , wspd , gz1oz0 , br , chklowq &
                             , cu_act_flag , hol , th_phy        &
                             , pi_phy , p_phy , t_phy , u_phy , v_phy     &
                             , dz8w , p8w , t8w , rho_phy , rho           &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe     &
                             , imsx,imex,jmsx,jmex,kmsx,kmex    &
                             , ipsx,ipex,jpsx,jpex,kpsx,kpex    &
                             , imsy,imey,jmsy,jmey,kmsy,kmey    &
                             , ipsy,ipey,jpsy,jpey,kpsy,kpey    &
                             , k_start , k_end                  &
                             , f_flux                           &
                            )
    USE module_state_description
    USE module_model_constants
    USE module_domain, ONLY : domain, domain_clock_get, get_ijk_from_subgrid
    USE module_configure, ONLY : grid_config_rec_type, model_config_rec
    USE module_radiation_driver, ONLY : pre_radiation_driver, radiation_driver
    USE module_surface_driver, ONLY : surface_driver
    USE module_cumulus_driver, ONLY : cumulus_driver
    USE module_pbl_driver, ONLY : pbl_driver
    USE module_fr_sfire_driver, ONLY : sfire_driver_em_step
    USE module_fddagd_driver, ONLY : fddagd_driver
    USE module_em, ONLY : init_zero_tendency
    USE module_force_scm
    USE module_big_step_utilities_em, ONLY : phy_prep
#ifdef DM_PARALLEL
    USE module_dm, ONLY : local_communicator, mytask, ntasks, ntasks_x, ntasks_y, local_communicator_periodic, wrf_dm_maxval
    USE module_comm_dm, ONLY : halo_em_phys_a_sub,halo_em_fdda_sfc_sub
#endif
    USE module_utility
    IMPLICIT NONE

    TYPE ( domain ), INTENT(INOUT) :: grid
    TYPE ( grid_config_rec_type ), INTENT(IN) :: config_flags
    TYPE(WRFU_Time)                :: currentTime

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           ips, ipe, jps, jpe, kps, kpe,     &
                           imsx,imex,jmsx,jmex,kmsx,kmex,    &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex,    &
                           imsy,imey,jmsy,jmey,kmsy,kmey,    &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey


    LOGICAL ,INTENT(IN)                        :: adapt_step_flag
    REAL, INTENT(IN)                           :: curr_secs

    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme,num_moist),INTENT(INOUT)   :: moist
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme,num_moist),INTENT(INOUT)   :: moist_tend
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme,num_chem),INTENT(INOUT)   :: chem
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme,num_chem),INTENT(INOUT)   :: chem_tend
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme,num_tracer),INTENT(INOUT)   :: tracer
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme,num_tracer),INTENT(INOUT)   :: tracer_tend
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme,num_scalar),INTENT(INOUT)   :: scalar
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme,num_scalar),INTENT(INOUT)   :: scalar_tend
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme,num_fdda3d),INTENT(INOUT)  :: fdda3d
    REAL    ,DIMENSION(ims:ime,1:1,jms:jme,num_fdda2d),INTENT(INOUT)      :: fdda2d
    REAL    ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: psim
    REAL    ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: psih
    REAL    ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: wspd
    REAL    ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: gz1oz0
    REAL    ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: br
    REAL    ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: chklowq
    LOGICAL ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: cu_act_flag
    REAL    ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: hol

    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: th_phy
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: pi_phy
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: p_phy
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: t_phy
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: u_phy
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: v_phy
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: dz8w
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: p8w
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: t8w
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: rho_phy
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: rho

    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: ru_tendf
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: rv_tendf
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: rw_tendf
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: ph_tendf
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: t_tendf
    REAL    ,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: tke_tend

    REAL    ,DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: mu_tendf

    INTEGER, INTENT(IN)                           ::  k_start, k_end
    LOGICAL, INTENT(IN), OPTIONAL                 ::  f_flux

! Local

    REAL, DIMENSION( ims:ime, jms:jme ) :: ht_loc, mixht
    INTEGER                             :: ij
    INTEGER  num_roof_layers
    INTEGER  num_wall_layers
    INTEGER  num_road_layers
    INTEGER  iswater
    LOGICAL  :: l_flux
    INTEGER  :: isurban
    INTEGER  rk_step
    INTEGER                         :: yr, month, day, hr, minute, sec, rc
    CHARACTER*80                    :: mesg

   INTEGER                         :: sids , side , sjds , sjde , skds , skde , &
                                      sims , sime , sjms , sjme , skms , skme , &
                                      sips , sipe , sjps , sjpe , skps , skpe

   CHARACTER (LEN=256) :: mminlu

#if ( WRF_DFI_RADAR == 1 )
    INTEGER  do_capsupress   ! =1 do CAP supress, other = don't
#endif

  CALL get_ijk_from_subgrid (  grid ,                   &
                            sids, side, sjds, sjde, skds, skde,    &
                            sims, sime, sjms, sjme, skms, skme,    &
                            sips, sipe, sjps, sjpe, skps, skpe    )

 ! initialize all tendencies to zero in order to update physics
 ! tendencies first (separate from dry dynamics).

   l_flux=.FALSE.
   if (present(f_flux)) l_flux=f_flux

    rk_step = 1

BENCH_START(init_zero_tend_tim)
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

         CALL wrf_debug ( 200 , ' call init_zero_tendency' )
         CALL init_zero_tendency ( ru_tendf, rv_tendf, rw_tendf,     &
                                   ph_tendf, t_tendf, tke_tend,      &
                                   mu_tendf,                         &
                                   moist_tend,chem_tend,scalar_tend, &
                                   tracer_tend,num_tracer,           &
                                   num_moist,num_chem,num_scalar,    &
                                   rk_step,                          &
                                   ids, ide, jds, jde, kds, kde,     &
                                   ims, ime, jms, jme, kms, kme,     &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start, k_end                   )

       END DO
       !$OMP END PARALLEL DO
BENCH_END(init_zero_tend_tim)

#ifdef DM_PARALLEL
#     include "HALO_EM_PHYS_A.inc"
#endif

      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )
      DO ij = 1 , grid%num_tiles

        CALL wrf_debug ( 200 , ' call phy_prep' )
        CALL phy_prep ( config_flags,                           &
                        grid%mut, grid%muu, grid%muv, grid%u_2, &
                        grid%v_2, grid%p, grid%pb, grid%alt,              &
                        grid%ph_2, grid%phb, grid%t_2, grid%tsk, moist, num_moist,   &
                        rho,th_phy, p_phy, pi_phy, u_phy, v_phy,    &
                        p8w, t_phy, t8w, grid%z, grid%z_at_w, dz8w, &
                        grid%p_hyd, grid%p_hyd_w,                   &
                        grid%fnm, grid%fnp, grid%znw, grid%p_top,   &
                        grid%rthraten,                          &
                        grid%rthblten, grid%rublten, grid%rvblten,             &
                        grid%rqvblten, grid%rqcblten, grid%rqiblten,           &
                        grid%rthcuten, grid%rqvcuten, grid%rqccuten,           &
                        grid%rqrcuten, grid%rqicuten, grid%rqscuten,           &
                        grid%rthften,  grid%rqvften,                      &
                        grid%RUNDGDTEN, grid%RVNDGDTEN, grid%RTHNDGDTEN,       &
                        grid%RPHNDGDTEN,grid%RQVNDGDTEN, grid%RMUNDGDTEN,                 &
                        ids, ide, jds, jde, kds, kde,           &
                        ims, ime, jms, jme, kms, kme,           &
                        grid%i_start(ij), grid%i_end(ij),       &
                        grid%j_start(ij), grid%j_end(ij),       &
                        k_start, k_end                         )
      ENDDO
      !$OMP END PARALLEL DO

BENCH_END(phy_prep_tim)

! radiation
     CALL domain_clock_get( grid, current_time=currentTime, &
                            current_timestr=mesg )
     CALL WRFU_TimeGet( currentTime, YY=yr, dayOfYear=day, H=hr, M=minute, S=sec, rc=rc)
         IF( rc/= WRFU_SUCCESS)THEN
         CALL wrf_error_fatal('WRFU_TimeGet failed')
         ENDIF

! this driver is only needed to handle non-local shadowing effects
      CALL pre_radiation_driver ( grid, config_flags                        &
     &        ,itimestep=grid%itimestep, ra_call_offset=grid%ra_call_offset    &
     &        ,XLAT=grid%xlat, XLONG=grid%xlong, GMT=grid%gmt                  &
     &        ,julian=grid%julian, xtime=grid%xtime, RADT=grid%radt            &
     &        ,STEPRA=grid%stepra                                              &
     &        ,ht=grid%ht,dx=grid%dx,dy=grid%dy,sina=grid%sina,cosa=grid%cosa  &
     &        ,shadowmask=grid%shadowmask,slope_rad=config_flags%slope_rad     &
     &        ,topo_shading=config_flags%topo_shading                          &
     &        ,shadlen=config_flags%shadlen,ht_shad=grid%ht_shad,ht_loc=ht_loc &
     &        ,ht_shad_bxs=grid%ht_shad_bxs, ht_shad_bxe=grid%ht_shad_bxe      &
     &        ,ht_shad_bys=grid%ht_shad_bys, ht_shad_bye=grid%ht_shad_bye      &
     &        ,nested=config_flags%nested, min_ptchsz=grid%min_ptchsz          &
     &        ,spec_bdy_width=config_flags%spec_bdy_width                      &
            ! indexes
     &        ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde          &
     &        ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme          &
     &        ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe          &
     &        ,i_start=grid%i_start,i_end=min(grid%i_end, ide-1)          &
     &        ,j_start=grid%j_start,j_end=min(grid%j_end, jde-1)          &
     &        ,kts=k_start, kte=min(k_end,kde-1)                          &
     &        ,num_tiles=grid%num_tiles                                   )

      CALL wrf_debug ( 200 , ' call radiation_driver' )
BENCH_START(rad_driver_tim)

      CALL radiation_driver(                                                  &
     &         ACFRCV=grid%acfrcv      ,ACFRST=grid%acfrst      ,ALBEDO=grid%albedo  &
     &        ,CFRACH=grid%cfrach      ,CFRACL=grid%cfracl      ,CFRACM=grid%cfracm  &
     &        ,CUPPT=grid%cuppt        ,CZMEAN=grid%czmean      ,DT=grid%dt          &
     &        ,DZ8W=dz8w               ,EMISS=grid%emiss        ,GLW=grid%glw        &
     &        ,GMT=grid%gmt            ,GSW=grid%gsw            ,HBOT=grid%hbot      &
     &        ,HTOP=grid%htop          ,HBOTR=grid%hbotr        ,HTOPR=grid%htopr    &
     &        ,ICLOUD=config_flags%icloud                                            &
     &        ,ITIMESTEP=grid%itimestep,JULDAY=grid%julday      , JULIAN=grid%julian &
     &        ,JULYR=grid%julyr        ,LW_PHYSICS=config_flags%ra_lw_physics        &
     &        ,NCFRCV=grid%ncfrcv      ,NCFRST=grid%ncfrst      ,NPHS=1              &
     &        ,P8W=grid%p_hyd_w        ,P=grid%p_hyd            ,PI=pi_phy           &
     &        ,RADT=grid%radt          ,RA_CALL_OFFSET=grid%ra_call_offset           &
     &        ,RHO=rho                 ,RLWTOA=grid%rlwtoa                           &
     &        ,RSWTOA=grid%rswtoa      ,RTHRATEN=grid%rthraten                       &
     &        ,RTHRATENLW=grid%rthratenlw       ,RTHRATENSW=grid%rthratensw          &
     &        ,SNOW=grid%snow          ,STEPRA=grid%stepra      ,SWDOWN=grid%swdown  &
     &        ,SWDOWNC=grid%swdownc    ,SW_PHYSICS=config_flags%ra_sw_physics        &
     &        ,T8W=t8w                 ,T=t_phy                 ,TAUCLDC=grid%taucldc &
     &        ,TAUCLDI=grid%taucldi    ,TSK=grid%tsk            ,VEGFRA=grid%vegfra  &
     &        ,WARM_RAIN=grid%warm_rain ,XICE=grid%xice         ,XLAND=grid%xland    &
     &        ,XLAT=grid%xlat          ,XLONG=grid%xlong        ,YR=yr               &
!Optional solar variables
     &        ,DECLINX=grid%declin ,SOLCONX=grid%solcon ,COSZEN=grid%coszen ,HRANG=grid%hrang    &
!
     &        ,Z=grid%z                                                   &
     &        ,LEVSIZ=grid%levsiz, N_OZMIXM=num_ozmixm                    &
     &        ,N_AEROSOLC=num_aerosolc                                    &
     &        ,PAERLEV=grid%paerlev                                       &
     &        ,CAM_ABS_DIM1=grid%cam_abs_dim1, CAM_ABS_DIM2=grid%cam_abs_dim2 &
     &        ,CAM_ABS_FREQ_S=grid%cam_abs_freq_s                         &
     &        ,XTIME=grid%xtime                                                &
              ,CURR_SECS=curr_secs, ADAPT_STEP_FLAG=adapt_step_flag       &
            ! indexes
     &        ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde          &
     &        ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme          &
     &        ,i_start=grid%i_start,i_end=min(grid%i_end, ide-1)          &
     &        ,j_start=grid%j_start,j_end=min(grid%j_end, jde-1)          &
     &        ,kts=k_start, kte=min(k_end,kde-1)                          &
     &        ,num_tiles=grid%num_tiles                                   &
            ! Optional
     &        , CLDFRA=grid%cldfra                                        &
     &        , PB=grid%pb                                                     &
     &        , F_ICE_PHY=grid%f_ice_phy,F_RAIN_PHY=grid%f_rain_phy                 &
     &        , QV=moist(ims,kms,jms,P_QV), F_QV=F_QV                     &
     &        , QC=moist(ims,kms,jms,P_QC), F_QC=F_QC                     &
     &        , QR=moist(ims,kms,jms,P_QR), F_QR=F_QR                     &
     &        , QI=moist(ims,kms,jms,P_QI), F_QI=F_QI                     &
     &        , QS=moist(ims,kms,jms,P_QS), F_QS=F_QS                     &
     &        , QG=moist(ims,kms,jms,P_QG), F_QG=F_QG                     &
     &        , QNDROP=scalar(ims,kms,jms,P_QNDROP), F_QNDROP=F_QNDROP    &
     &        ,ACSWUPT=grid%acswupt    ,ACSWUPTC=grid%acswuptc            &
     &        ,ACSWDNT=grid%acswdnt    ,ACSWDNTC=grid%acswdntc            &
     &        ,ACSWUPB=grid%acswupb    ,ACSWUPBC=grid%acswupbc            &
     &        ,ACSWDNB=grid%acswdnb    ,ACSWDNBC=grid%acswdnbc            &
     &        ,ACLWUPT=grid%aclwupt    ,ACLWUPTC=grid%aclwuptc            &
     &        ,ACLWDNT=grid%aclwdnt    ,ACLWDNTC=grid%aclwdntc            &
     &        ,ACLWUPB=grid%aclwupb    ,ACLWUPBC=grid%aclwupbc            &
     &        ,ACLWDNB=grid%aclwdnb    ,ACLWDNBC=grid%aclwdnbc            &
     &        ,SWUPT=grid%swupt    ,SWUPTC=grid%swuptc                    &
     &        ,SWDNT=grid%swdnt    ,SWDNTC=grid%swdntc                    &
     &        ,SWUPB=grid%swupb    ,SWUPBC=grid%swupbc                    &
     &        ,SWDNB=grid%swdnb    ,SWDNBC=grid%swdnbc                    &
     &        ,LWUPT=grid%lwupt    ,LWUPTC=grid%lwuptc                    &
     &        ,LWDNT=grid%lwdnt    ,LWDNTC=grid%lwdntc                    &
     &        ,LWUPB=grid%lwupb    ,LWUPBC=grid%lwupbc                    &
     &        ,LWDNB=grid%lwdnb    ,LWDNBC=grid%lwdnbc                    &
     &        ,LWCF=grid%lwcf                                                  &
     &        ,SWCF=grid%swcf                                                  &
     &        ,OLR=grid%olr                                                    &
     &        ,OZMIXM=grid%ozmixm, PIN=grid%pin                                     &
     &        ,M_PS_1=grid%m_ps_1, M_PS_2=grid%m_ps_2, AEROSOLC_1=grid%aerosolc_1        &
     &        ,AEROSOLC_2=grid%aerosolc_2, M_HYBI0=grid%m_hybi                      &
     &        ,ABSTOT=grid%abstot, ABSNXT=grid%absnxt, EMSTOT=grid%emstot                &
#ifdef WRF_CHEM
     &        ,CU_RAD_FEEDBACK=config_flags%cu_rad_feedback                &
     &        ,AER_RA_FEEDBACK=config_flags%aer_ra_feedback                &
     &        ,QC_ADJUST=grid%GD_CLOUD_B , QI_ADJUST=grid%GD_CLOUD2_B         &
     &        ,PM2_5_DRY=grid%pm2_5_dry, PM2_5_WATER=grid%pm2_5_water               &
     &        ,PM2_5_DRY_EC=grid%pm2_5_dry_ec                                  &
     &        ,TAUAER300=grid%tauaer1, TAUAER400=grid%tauaer2 & ! jcb
     &        ,TAUAER600=grid%tauaer3, TAUAER999=grid%tauaer4 & ! jcb
     &        ,GAER300=grid%gaer1, GAER400=grid%gaer2, GAER600=grid%gaer3, GAER999=grid%gaer4 & ! jcb
     &        ,WAER300=grid%waer1, WAER400=grid%waer2, WAER600=grid%waer3, WAER999=grid%waer4 & ! jcb
#endif
     &         ,slope_rad=config_flags%slope_rad,topo_shading=config_flags%topo_shading     &
     &         ,shadowmask=grid%shadowmask,ht=grid%ht,dx=grid%dx,dy=grid%dy    )

BENCH_END(rad_driver_tim)

!********* Surface driver
! surface

BENCH_START(surf_driver_tim)

!-----------------------------------------------------------------
! urban related variable are added to arguments of surface_driver
!-----------------------------------------------------------------
      num_roof_layers = grid%num_soil_layers !urban
      num_wall_layers = grid%num_soil_layers !urban
      num_road_layers = grid%num_soil_layers !urban
      CALL nl_get_iswater(grid%id, iswater)
      CALL nl_get_isurban(grid%id, isurban)
      call nl_get_mminlu(grid%id, mminlu)

      CALL wrf_debug ( 200 , ' call surface_driver' )

      CALL surface_driver(                                                &
     &         ACGRDFLX=grid%acgrdflx  ,ACHFX=grid%achfx        ,ACLHF=grid%aclhf        &
     &        ,ACSNOM=grid%acsnom      ,ACSNOW=grid%acsnow      ,AKHS=grid%akhs          &
     &        ,AKMS=grid%akms          ,ALBBCK=grid%albbck      ,ALBEDO=grid%albedo      &
     &        ,EMBCK=grid%embck                                                          &
     &        ,BR=br              ,CANWAT=grid%canwat      ,CHKLOWQ=chklowq    &
     &        ,CT=grid%ct              ,DT=grid%dt         ,DX=grid%dx         &
     &        ,DZ8W=dz8w          ,DZS=grid%dzs            ,FLHC=grid%flhc          &
     &        ,FLQC=grid%flqc          ,GLW=grid%glw            ,GRDFLX=grid%grdflx      &
     &        ,GSW=grid%gsw    ,SWDOWN=grid%swdown        ,GZ1OZ0=gz1oz0      ,HFX=grid%hfx              &
     &        ,HT=grid%ht              ,IFSNOW=config_flags%ifsnow      ,ISFFLX=config_flags%isfflx      &
     &        ,FRACTIONAL_SEAICE=config_flags%fractional_seaice           &
     &        ,ISLTYP=grid%isltyp      ,ITIMESTEP=grid%itimestep, JULIAN_IN=grid%julian                  &
     &        ,IVGTYP=grid%ivgtyp      ,LH=grid%lh              ,LOWLYR=grid%lowlyr      &
     &        ,MAVAIL=grid%mavail      ,NUM_SOIL_LAYERS=config_flags%num_soil_layers        &
     &        ,P8W=grid%p_hyd_w            ,PBLH=grid%pblh          ,PI_PHY=pi_phy      &
     &        ,PSFC=grid%psfc          ,PSHLTR=grid%pshltr      ,PSIH=psih          &
     &        ,BLDT=grid%bldt     ,CURR_SECS=curr_secs, ADAPT_STEP_FLAG=adapt_step_flag  &
     &        ,PSIM=psim          ,P_PHY=grid%p_hyd        ,Q10=grid%q10            &
     &        ,Q2=grid%q2              ,QFX=grid%qfx            ,QSFC=grid%qsfc          &
     &        ,QSHLTR=grid%qshltr      ,QZ0=grid%qz0            ,RAINCV=grid%raincv      &
     &        ,RA_LW_PHYSICS=config_flags%ra_lw_physics            ,RHO=rho            &
     &        ,RMOL=grid%rmol          ,SFCEVP=grid%sfcevp      ,SFCEXC=grid%sfcexc      &
     &        ,SFCRUNOFF=grid%sfcrunoff                                        &
     &        ,SF_SFCLAY_PHYSICS=config_flags%sf_sfclay_physics                        &
     &        ,SF_SURFACE_PHYSICS=config_flags%sf_surface_physics  ,SH2O=grid%sh2o          &
     &        ,SHDMAX=grid%shdmax      ,SHDMIN=grid%shdmin      ,SMOIS=grid%smois        &
     &        ,SMSTAV=grid%smstav      ,SMSTOT=grid%smstot      ,SNOALB=grid%snoalb      &
     &        ,SNOW=grid%snow          ,SNOWC=grid%snowc        ,SNOWH=grid%snowh        &
     &        ,SMCREL=grid%smcrel                                                        &
     &        ,SST=grid%sst            ,SST_UPDATE=grid%sst_update                  &
     &        ,SSTSK=grid%sstsk        ,DTW=grid%dtw            ,SST_SKIN=grid%sst_skin  &
     &        ,STEPBL=grid%stepbl      ,TH10=grid%th10          ,TH2=grid%th2            &
     &        ,THZ0=grid%thz0          ,TH_PHY=th_phy      ,TKE_MYJ=grid%tke_myj    &
     &        ,TMN=grid%tmn            ,TSHLTR=grid%tshltr      ,TSK=grid%tsk            &
     &        ,TYR=grid%tyr            ,TYRA=grid%tyra          ,TDLY=grid%tdly          &
     &        ,TLAG=grid%tlag          ,LAGDAY=config_flags%lagday      ,NYEAR=grid%nyear        &
     &        ,NDAY=grid%nday          ,TMN_UPDATE=grid%tmn_update      ,YR=yr            &
     &        ,TSLB=grid%tslb          ,T_PHY=t_phy        ,U10=grid%u10            &
     &        ,URATX=grid%uratx        ,VRATX=grid%vratx   ,TRATX=grid%tratx        &
     &        ,UDRUNOFF=grid%udrunoff  ,UST=grid%ust       ,UZ0=grid%uz0            &
     &        ,U_FRAME=grid%u_frame    ,U_PHY=u_phy        ,V10=grid%v10            &
     &        ,VEGFRA=grid%vegfra      ,VZ0=grid%vz0       ,V_FRAME=grid%v_frame    &
     &        ,V_PHY=v_phy             ,WARM_RAIN=grid%warm_rain                    &
     &        ,WSPD=wspd               ,XICE=grid%xice     ,XLAND=grid%xland        &
     &        ,Z0=grid%z0              ,Z=grid%z        ,ZNT=grid%znt            &
     &        ,ZS=grid%zs              ,XICEM=grid%xicem   ,ISICE=grid%landuse_isice&
     &        ,USTM=grid%ustm          ,CK=grid%ck         ,CKA=grid%cka            &
     &                                 ,CD=grid%cd         ,CDA=grid%cda            &
     &        ,ISFTCFLX=config_flags%isftcflx, IZ0TLND=config_flags%iz0tlnd         &
     &        ,OMLCALL=config_flags%omlcall ,OML_HML0=config_flags%oml_hml0 ,OML_GAMMA=config_flags%oml_gamma &
     &        ,TML=grid%tml, T0ML=grid%t0ml, HML=grid%hml, H0ML=grid%h0ml, HUML=grid%huml, HVML=grid%hvml, F=grid%f &
     &        ,TMOML=grid%TMOML,ISWATER=iswater &
     &        ,SLOPE_RAD=config_flags%slope_rad,TOPO_SHADING=config_flags%topo_shading & ! solar
     &        ,SHADOWMASK=grid%shadowmask                                              & ! solar
     &        ,SLOPE=grid%slope, SLP_AZI=grid%slp_azi, SWNORM=grid%swnorm              & ! solar
     &        ,DECLIN=grid%declin ,SOLCON=grid%solcon ,COSZEN=grid%coszen ,HRANG=grid%hrang    &
     &        ,xlat_urb2d=grid%XLAT                                       & !I urban
     &        ,NUM_ROOF_LAYERS=num_roof_layers                            & !I urban
     &        ,NUM_WALL_LAYERS=num_wall_layers                            & !I urban
     &        ,NUM_ROAD_LAYERS=num_road_layers                            &
     &        ,DZR=grid%dzr ,DZB=grid%dzb ,DZG=grid%dzg                   & !I urban
     &        ,TR_URB2D=grid%tr_urb2d ,TB_URB2D=grid%tb_urb2d             &
     &        ,TG_URB2D=grid%tg_urb2d                                     & !H urban
     &        ,TC_URB2D=grid%tc_urb2d ,QC_URB2D=grid%qc_urb2d             & !H urban
     &        ,UC_URB2D=grid%uc_urb2d                                     & !H urban
     &        ,XXXR_URB2D=grid%xxxr_urb2d                                 &
     &        ,XXXB_URB2D=grid%xxxb_urb2d                                 & !H urban
     &        ,XXXG_URB2D=grid%xxxg_urb2d                                 &
     &        ,XXXC_URB2D=grid%xxxc_urb2d                                 & !H urban
     &        ,TRL_URB3D=grid%trl_urb3d   ,TBL_URB3D=grid%tbl_urb3d       & !H urban
     &        ,TGL_URB3D=grid%tgl_urb3d                                   & !H urban
     &        ,SH_URB2D=grid%sh_urb2d     ,LH_URB2D=grid%lh_urb2d         &
     &        ,G_URB2D=grid%g_urb2d                                       & !H urban
     &        ,RN_URB2D=grid%rn_urb2d     , TS_URB2D=grid%ts_urb2d        & !H urban
     &        ,FRC_URB2D=grid%frc_urb2d                                   & !H urban
     &        ,UTYPE_URB2D=grid%utype_urb2d                               & !H urban
         ! Optional urban for BEP scheme
     &        ,SF_URBAN_PHYSICS=config_flags%sf_urban_physics             &
     &        ,NUM_URBAN_LAYERS=config_flags%num_urban_layers             & !multi-layer urban
     &        ,TRB_URB4D=grid%trb_urb4d,TW1_URB4D=grid%tw1_urb4d          & !multi-layer urban
     &        ,TW2_URB4D=grid%tw2_urb4d,TGB_URB4D=grid%tgb_urb4d          & !multi-layer urban
     &        ,TLEV_URB3D=grid%tlev_urb3d                               & !multi-layer urban
     &        ,QLEV_URB3D=grid%qlev_urb3d                               & !multi-layer urban
     &        ,TW1LEV_URB3D=grid%tw1lev_urb3d                           & !multi-layer urban
     &        ,TW2LEV_URB3D=grid%tw2lev_urb3d                           & !multi-layer urban
     &        ,TGLEV_URB3D=grid%tglev_urb3d                             & !multi-layer urban
     &        ,TFLEV_URB3D=grid%tflev_urb3d                             & !multi-layer urban
     &        ,SF_AC_URB3D=grid%sf_ac_urb3d                             & !multi-layer urban
     &        ,LF_AC_URB3D=grid%lf_ac_urb3d                             & !multi-layer urban
     &        ,CM_AC_URB3D=grid%cm_ac_urb3d                             & !multi-layer urban
     &        ,SFVENT_URB3D=grid%sfvent_urb3d                           & !multi-layer urban
     &        ,LFVENT_URB3D=grid%lfvent_urb3d                           & !multi-layer urban
     &        ,SFWIN1_URB3D=grid%sfwin1_urb3d                           & !multi-layer urban
     &        ,SFWIN2_URB3D=grid%sfwin2_urb3d                           & !multi-layer urban
     &        ,SFW1_URB3D=grid%sfw1_urb3d,SFW2_URB3D=grid%sfw2_urb3d      & !multi-layer urban
     &        ,SFR_URB3D=grid%sfr_urb3d,SFG_URB3D=grid%sfg_urb3d          & !multi-layer urban
     &        ,GMT=grid%gmt,XLAT=grid%xlat,XLONG=grid%xlong,JULDAY=grid%julday  &
     &        ,A_U_BEP=grid%a_u_bep,A_V_BEP=grid%a_v_bep,A_T_BEP=grid%a_t_bep   &
     &        ,A_Q_BEP=grid%a_q_bep                                             &
     &        ,B_U_BEP=grid%b_u_bep,B_V_BEP=grid%b_v_bep,B_T_BEP=grid%b_t_bep   &
     &        ,B_Q_BEP=grid%b_q_bep                                             &
     &        ,SF_BEP=grid%sf_bep,VL_BEP=grid%vl_bep                            &
     &        ,A_E_BEP=grid%a_e_bep,B_E_BEP=grid%b_e_bep,DLG_BEP=grid%dlg_bep   &
     &        ,DL_U_BEP=grid%dl_u_bep                                           &
     &        ,CMR_SFCDIF=grid%cmr_sfcdif, CHR_SFCDIF=grid%chr_sfcdif     & !I/O urban
     &        ,CMC_SFCDIF=grid%cmc_sfcdif, CHC_SFCDIF=grid%chc_sfcdif     & !I/O urban
           ! P-X LSM Variables
     &        ,LANDUSEF=grid%landusef, SOILCTOP=grid%soilctop             &   ! P-X LSM
     &        ,SOILCBOT=grid%soilcbot                                     &   ! P-X LSM
     &        ,RA=grid%ra, RS=grid%rs, LAI=grid%lai                       &   ! P-X LSM
     &        ,NLCAT=grid%num_land_cat,  NSCAT=grid%num_soil_cat          &   ! P-X LSM
     &        ,VEGF_PX=grid%vegf_px, SNOWNCV=grid%snowncv                 &   ! P-X LSM
     &        ,ANAL_INTERVAL=config_flags%auxinput9_interval_s+config_flags%auxinput9_interval_m*60  &   ! P-X LSM
     &        ,PXLSM_SMOIS_INIT=config_flags%pxlsm_smois_init             &   ! P-X LSM
     &        ,PXLSM_SOIL_NUDGE=config_flags%grid_sfdda                   &   ! P-X LSM
           ! Optional PX LSM nudging
     &        ,t2_ndg_old=grid%t2_ndg_old                  &
     &        ,q2_ndg_old=grid%q2_ndg_old                  &
     &        ,t2_ndg_new=grid%t2_ndg_new                  &
     &        ,q2_ndg_new=grid%q2_ndg_new                  &
     &        ,sn_ndg_old=grid%sn_ndg_old                  &
     &        ,sn_ndg_new=grid%sn_ndg_new                  &
           ! Indexes
     &        ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde          &
     &        ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme          &
     &        , I_START=grid%i_start,I_END=min(grid%i_end, ide-1)         &
     &        , J_START=grid%j_start,J_END=min(grid%j_end, jde-1)         &
     &        , KTS=k_start, KTE=min(k_end,kde-1)                         &
     &        , NUM_TILES=grid%num_tiles                                  &
           ! Optional
     &        ,QV_CURR=moist(ims,kms,jms,P_QV), F_QV=F_QV                 &
     &        ,QC_CURR=moist(ims,kms,jms,P_QC), F_QC=F_QC                 &
     &        ,QR_CURR=moist(ims,kms,jms,P_QR), F_QR=F_QR                 &
     &        ,QI_CURR=moist(ims,kms,jms,P_QI), F_QI=F_QI                 &
     &        ,QS_CURR=moist(ims,kms,jms,P_QS), F_QS=F_QS                 &
     &        ,QG_CURR=moist(ims,kms,jms,P_QG), F_QG=F_QG                 &
     &        ,CAPG=grid%capg, EMISS=grid%emiss, HOL=hol,MOL=grid%mol     &
     &        ,T2OBS=grid%t2obs, Q2OBS=grid%q2obs                         &
     &        ,RAINBL=grid%rainbl,SR=grid%sr                                              &
     &        ,RAINNCV=grid%rainncv,REGIME=grid%regime,T2=grid%t2,THC=grid%thc            &
     &        ,QSG=grid%qsg,QVG=grid%qvg,QCG=grid%qcg,SOILT1=grid%soilt1,TSNAV=grid%tsnav & ! ruc lsm
     &        ,SMFR3D=grid%smfr3d,KEEPFR3DFLAG=grid%keepfr3dflag,DEW=grid%dew             & ! ruc lsm
     &        ,POTEVP=grid%POTEVP, SNOPCX=grid%SNOPCX, SOILTB=grid%SOILTB                 & ! ruc lsm
     &        ,ISURBAN=isurban, MMINLU=TRIM(mminlu)                       &
     &        ,SNOTIME = grid%SNOTIME                                     &
     &        ,RDLAI2D=config_flags%rdlai2d                               &
     &        ,usemonalb=config_flags%usemonalb                           &
     &        ,NOAHRES=grid%noahres                                       &
!mynn mp@
     &         ,ch=grid%ch,tsq=grid%tsq,qsq=grid%qsq,cov=grid%cov &
     &                                                              )
BENCH_END(surf_driver_tim)

!*********
! pbl

      CALL wrf_debug ( 200 , ' call pbl_driver' )
BENCH_START(pbl_driver_tim)
      CALL pbl_driver(                                                    &
     &         AKHS=grid%akhs          ,AKMS=grid%akms                              &
     &        ,BL_PBL_PHYSICS=config_flags%bl_pbl_physics                 &
     &        ,BLDT=grid%bldt, CURR_SECS=curr_secs, ADAPT_STEP_FLAG=adapt_step_flag &
     &        ,BR=br              ,CHKLOWQ=chklowq    ,CT=grid%ct         &
     &        ,DT=grid%dt              ,DX=grid%dx              ,DZ8W=dz8w          &
     &        ,EL_MYJ=grid%el_myj      ,EXCH_H=grid%exch_h     ,EXCH_M=grid%exch_m   &
     &        ,F=grid%f  ,GRDFLX=grid%grdflx      &
     &        ,GZ1OZ0=gz1oz0      ,HFX=grid%hfx            ,HT=grid%ht              &
     &        ,ITIMESTEP=grid%itimestep                    ,KPBL=grid%kpbl          &
     &        ,LH=grid%lh              ,LOWLYR=grid%lowlyr      ,P8W=grid%p_hyd_w   &
     &        ,PBLH=grid%pblh          ,PI_PHY=pi_phy      ,PSIH=psih          &
     &        ,PSIM=psim          ,P_PHY=grid%p_hyd        ,QFX=grid%qfx            &
     &        ,QSFC=grid%qsfc          ,QZ0=grid%qz0  ,MIXHT=mixht                  &
     &        ,RA_LW_PHYSICS=config_flags%ra_lw_physics                   &
     &        ,RHO=rho            ,RQCBLTEN=grid%rqcblten  ,RQIBLTEN=grid%rqiblten  &
     &        ,RQVBLTEN=grid%rqvblten  ,RTHBLTEN=grid%rthblten  ,RUBLTEN=grid%rublten    &
     &        ,RVBLTEN=grid%rvblten    ,SNOW=grid%snow          ,STEPBL=grid%stepbl      &
     &        ,THZ0=grid%thz0          ,TH_PHY=th_phy      ,TKE_MYJ=grid%tke_myj    &
     &        ,TSK=grid%tsk            ,T_PHY=t_phy        ,UST=grid%ust            &
     &        ,U10=grid%u10 ,UZ0=grid%uz0      ,U_FRAME=grid%u_frame    ,U_PHY=u_phy        &
     &        ,V10=grid%v10 ,VZ0=grid%vz0      ,V_FRAME=grid%v_frame    ,V_PHY=v_phy        &
     &        ,WARM_RAIN=grid%warm_rain                    ,WSPD=wspd          &
     &        ,XICE=grid%xice          ,XLAND=grid%xland        ,Z=grid%z                &
     &        ,ZNT=grid%znt                                                    &
! variables added for BEP
     &          ,FRC_URB2D=grid%frc_urb2d                                                  &
     &          ,A_U_BEP=grid%a_u_bep,A_V_BEP=grid%a_v_bep,A_T_BEP=grid%a_t_bep          &
     &          ,A_Q_BEP=grid%a_q_bep                                                    &
     &          ,B_U_BEP=grid%b_u_bep,B_V_BEP=grid%b_v_bep,B_T_BEP=grid%b_t_bep          &
     &          ,B_Q_BEP=grid%b_q_bep                                                    &
     &          ,SF_BEP=grid%sf_bep,VL_BEP=grid%vl_bep                                   &
     &          ,A_E_BEP=grid%a_e_bep,B_E_BEP=grid%b_e_bep,DLG_BEP=grid%dlg_bep          &
     &          ,DL_U_BEP=grid%dl_u_bep                                                  &
     &          ,SF_SFCLAY_PHYSICS=config_flags%sf_sfclay_physics                        &
     &          ,SF_URBAN_PHYSICS=config_flags%sf_urban_physics                          &
! Bep changes end
! add tke_pbl, and turbulent fluxes
     &          ,TKE_PBL=grid%tke_pbl,EL_PBL=grid%el_pbl,WU_TUR=grid%wu_tur,WV_tur=grid%wv_tur,WT_tur=grid%wt_tur,WQ_tur=grid%wq_tur   &
! end add tke_pbl, and turbulent fluxes
     &        ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde          &
     &        ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme          &
     &        ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)          &
     &        ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)          &
     &        ,KTS=k_start, KTE=min(k_end,kde-1)                          &
     &        ,NUM_TILES=grid%num_tiles                                   &
          ! Variables Required by ACM PBL  - jp
     &        ,ZNU=grid%znu,ZNW=grid%znw,MUT=grid%mut,P_TOP=grid%p_top    &
          ! optional
     &        ,QV_CURR=moist(ims,kms,jms,P_QV), F_QV=F_QV                 &
     &        ,QC_CURR=moist(ims,kms,jms,P_QC), F_QC=F_QC                 &
     &        ,QR_CURR=moist(ims,kms,jms,P_QR), F_QR=F_QR                 &
     &        ,QI_CURR=moist(ims,kms,jms,P_QI), F_QI=F_QI                 &
     &        ,QS_CURR=moist(ims,kms,jms,P_QS), F_QS=F_QS                 &
     &        ,QG_CURR=moist(ims,kms,jms,P_QG), F_QG=F_QG                 &
     &        ,HOL=HOL, MOL=grid%mol, REGIME=grid%REGIME                  &
!mynn mp@
     &        ,qke=grid%qke, tsq=grid%tsq, qsq=grid%qsq, cov=grid%cov     &
     &        ,rmol=grid%rmol, ch=grid%ch                                 &
     &        ,qcg=grid%qcg, grav_settling=grid%grav_settling             &
!    &        ,K_m=grid%K_m, K_h=grid%K_h, K_q=grid%K_q                   &
!GWD for ARW
     &        ,GWD_OPT=config_flags%gwd_opt &
     &        ,DUSFCG=grid%dusfcg,DVSFCG=grid%dvsfcg &
     &        ,VAR2D=grid%var2d,OC12D=grid%oc12d     &
     &        ,OA1=grid%oa1,OA2=grid%oa2,OA3=grid%oa3,OA4=grid%oa4        &
     &        ,OL1=grid%ol1,OL2=grid%ol2,OL3=grid%ol3,OL4=grid%ol4        &
     &                                                          )

BENCH_END(pbl_driver_tim)

!*****
! fire 

! Jan Mandel's call to SFIRE

      IF ((grid%sr_x > 0 .OR. grid%sr_y > 0) .AND. config_flags%ifire == 2) THEN

BENCH_START(fire_driver_tim)
        if(config_flags%ifire.eq.2)then 
            ! initialization moved to start_em:start_domain_em
!            if(grid%initestep.eq.1) &
!            call sfire_driver_em_init (  grid , config_flags    &
!            ,ids,ide, kds,kde, jds,jde                              &
!            ,ims,ime, kms,kme, jms,jme                              &
!            ,ips,ipe, kps,kpe, jps,jpe  )
            ! one timestep of the fire model
            call sfire_driver_em_step (  grid , config_flags    &
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,rho,grid%z_at_w,dz8w)
        endif

BENCH_END(fire_driver_tim)
       ENDIF


! cumulus para.

      CALL wrf_debug ( 200 , ' call cumulus_driver' )

#if ( WRF_DFI_RADAR == 1 )
      do_capsupress=0
      if (config_flags%dfi_radar == 1) then
         if(grid%dfi_stage == DFI_FWD ) do_capsupress=1
         if(grid%itimestep <= 31 .and. grid%dfi_stage == DFI_FST ) do_capsupress=1
      endif
#endif

BENCH_START(cu_driver_tim)
      CALL cumulus_driver(grid                                             &
                 ! Prognostic variables
     &             ,U=u_phy   ,V=v_phy   ,TH=th_phy  ,T=t_phy             &
     &             ,W=grid%w_2     ,P=grid%p_hyd   ,PI=pi_phy  ,RHO=rho             &
                 ! Other arguments
     &             ,ITIMESTEP=grid%itimestep ,DT=grid%dt      ,DX=grid%dx                &
     &             ,CUDT=grid%cudt,CURR_SECS=curr_secs,ADAPT_STEP_FLAG=adapt_step_flag &
     &             ,RAINC=grid%rainc   ,RAINCV=grid%raincv   ,PRATEC=grid%pratec         &
     &             , NCA=grid%nca                                         &
     &             ,HTOP=grid%cutop     ,HBOT=grid%cubot       ,KPBL=grid%kpbl             &
     &             ,DZ8W=dz8w     ,P8W=grid%p_hyd_w                                &
     &             ,W0AVG=grid%w0avg   ,STEPCU=grid%stepcu                          &
     &             ,CLDEFI=grid%cldefi ,LOWLYR=grid%lowlyr ,XLAND=grid%xland             &
     &             ,APR_GR=grid%apr_gr ,APR_W=grid%apr_w   ,APR_MC=grid%apr_mc           &
     &             ,APR_ST=grid%apr_st ,APR_AS=grid%apr_as ,APR_CAPMA=grid%apr_capma     &
     &             ,APR_CAPME=grid%apr_capme          ,APR_CAPMI=grid%apr_capmi     &
     &             ,MASS_FLUX=grid%mass_flux          ,XF_ENS=grid%xf_ens           &
     &             ,PR_ENS=grid%pr_ens ,HT=grid%ht,EDT_OUT=grid%edt_out             &
     &             ,imomentum=grid%imomentum,clos_choice=grid%clos_choice       &
     &             ,ishallow=config_flags%ishallow                                  &
     &             ,cugd_tten=grid%cugd_tten,cugd_qvten=grid%cugd_qvten,cugd_qcten=grid%cugd_qcten   &
     &             ,cugd_ttens=grid%cugd_ttens,cugd_qvtens=grid%cugd_qvtens   &
     &             ,ENSDIM=config_flags%ensdim ,MAXIENS=config_flags%maxiens ,MAXENS=config_flags%maxens         &
     &             ,MAXENS2=config_flags%maxens2                ,MAXENS3=config_flags%maxens3       &
     &             ,CU_ACT_FLAG=cu_act_flag   ,WARM_RAIN=grid%warm_rain        &
     &             ,GSW=grid%gsw,cugd_avedx=config_flags%cugd_avedx &
     &             ,k22_shallow=grid%k22_shallow,kbcon_shallow=grid%kbcon_shallow            &
     &             ,ktop_shallow=grid%ktop_shallow,xmb_shallow=grid%xmb_shallow            &
     &             ,PERIODIC_X=(config_flags%polar .OR. config_flags%periodic_x)  &
     &             ,PERIODIC_Y=config_flags%periodic_y  &
                 ! Selection flag
     &             ,CU_PHYSICS=config_flags%cu_physics                    &
                 ! Dimension arguments
     &             ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde     &
     &             ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme     &
     &             ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe     &
     &             ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)     &
     &             ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)     &
     &             ,KTS=k_start, KTE=min(k_end,kde-1)                     &
     &             ,NUM_TILES=grid%num_tiles                              &
                 ! Moisture tendency arguments
     &             ,RQVCUTEN=grid%rqvcuten , RQCCUTEN=grid%rqccuten                 &
     &             ,RQSCUTEN=grid%rqscuten , RQICUTEN=grid%rqicuten                 &
     &             ,RQRCUTEN=grid%rqrcuten , RQVBLTEN=grid%rqvblten                 &
     &             ,RQVFTEN=grid%rqvften                                       &
                 ! Other tendency arguments
     &             ,RTHRATEN=grid%rthraten , RTHBLTEN=grid%rthblten                 &
     &             ,RTHCUTEN=grid%rthcuten , RTHFTEN=grid%rthften                   &
                 ! Moisture tracer arguments
     &             ,QV_CURR=moist(ims,kms,jms,P_QV), F_QV=F_QV            &
     &             ,QC_CURR=moist(ims,kms,jms,P_QC), F_QC=F_QC            &
     &             ,QR_CURR=moist(ims,kms,jms,P_QR), F_QR=F_QR            &
     &             ,QI_CURR=moist(ims,kms,jms,P_QI), F_QI=F_QI            &
     &             ,QS_CURR=moist(ims,kms,jms,P_QS), F_QS=F_QS            &
     &             ,QG_CURR=moist(ims,kms,jms,P_QG), F_QG=F_QG            &
#ifdef WRF_CHEM
     &             ,GD_CLOUD=grid%GD_CLOUD,GD_CLOUD2=grid%GD_CLOUD2       &
#endif
#if ( WRF_DFI_RADAR == 1 )
     &             ,DO_CAPSUPPRESS=do_capsupress                          &
#endif
     &             ,cfu1=grid%cfu1,cfd1=grid%cfd1,dfu1=grid%dfu1,efu1=grid%efu1,dfd1=grid%dfd1,efd1=grid%efd1,f_flux=l_flux)
BENCH_END(cu_driver_tim)

! JPH call force_scm to update bl tendencies
      CALL force_scm(itimestep=grid%itimestep,dt=grid%dt                  &
     &             ,scm_force=config_flags%scm_force                      &
     &             ,dx=config_flags%scm_force_dx                          &
     &             ,num_force_layers=grid%num_force_layers                &
     &             ,scm_th_adv=config_flags%scm_th_adv                    &
     &             ,scm_qv_adv=config_flags%scm_qv_adv                    &
     &             ,scm_wind_adv=config_flags%scm_wind_adv                &
     &             ,scm_vert_adv=config_flags%scm_vert_adv                &
     &             ,u_base=grid%u_base,v_base=grid%v_base                 &
     &             ,z_base=grid%z_base                                    &
     &             ,z_force=grid%z_force,z_force_tend=grid%z_force_tend   &
     &             ,u_g=grid%u_g,v_g=grid%v_g                             &
     &             ,u_g_tend=grid%u_g_tend,v_g_tend=grid%v_g_tend         &
     &             ,w_subs=grid%w_subs, w_subs_tend=grid%w_subs_tend      &
     &             ,th_upstream_x=grid%th_upstream_x                      &
     &             ,th_upstream_x_tend=grid%th_upstream_x_tend            &
     &             ,th_upstream_y=grid%th_upstream_y                      &
     &             ,th_upstream_y_tend=grid%th_upstream_y_tend            &
     &             ,qv_upstream_x=grid%qv_upstream_x                      &
     &             ,qv_upstream_x_tend=grid%qv_upstream_x_tend            &
     &             ,qv_upstream_y=grid%qv_upstream_y                      &
     &             ,qv_upstream_y_tend=grid%qv_upstream_y_tend            &
     &             ,u_upstream_x=grid%u_upstream_x                        &
     &             ,u_upstream_x_tend=grid%u_upstream_x_tend              &
     &             ,u_upstream_y=grid%u_upstream_y                        &
     &             ,u_upstream_y_tend=grid%u_upstream_y_tend              &
     &             ,v_upstream_x=grid%v_upstream_x                        &
     &             ,v_upstream_x_tend=grid%v_upstream_x_tend              &
     &             ,v_upstream_y=grid%v_upstream_y                        &
     &             ,v_upstream_y_tend=grid%v_upstream_y_tend              &
     &             ,tau_x=grid%tau_x                                      &
     &             ,tau_x_tend=grid%tau_x_tend                            &
     &             ,tau_y=grid%tau_y                                      &
     &             ,tau_y_tend=grid%tau_y_tend                            &
     &             ,z=grid%z,z_at_w=grid%z_at_w                           &
     &             ,th=th_phy, qv=moist(ims,kms,jms,P_QV)                 &
     &             ,u=u_phy, v=v_phy                                      &
     &             ,thten=grid%rthblten, qvten=grid%rqvblten              &
     &             ,uten=grid%rublten, vten=grid%rvblten                  &
     &             ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde     &
     &             ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme     &
     &             ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe     &
     &             ,KTS=k_start, KTE=min(k_end,kde-1)                     &
     &              )

#ifdef DM_PARALLEL
#     include "HALO_EM_FDDA_SFC.inc"
#endif
      CALL wrf_debug ( 200 , ' call fddagd_driver' )

BENCH_START(fdda_driver_tim)
      CALL fddagd_driver(itimestep=grid%itimestep,dt=grid%dt,xtime=grid%XTIME,      &
                  id=grid%id,                                                       &
                  RUNDGDTEN=grid%rundgdten,RVNDGDTEN=grid%rvndgdten,                &
                  RTHNDGDTEN=grid%rthndgdten,RPHNDGDTEN=grid%rphndgdten,            &
                  RQVNDGDTEN=grid%rqvndgdten,RMUNDGDTEN=grid%rmundgdten,            &
                  u_ndg_old=fdda3d(ims,kms,jms,P_u_ndg_old),                        &
                  v_ndg_old=fdda3d(ims,kms,jms,P_v_ndg_old),                        &
                  t_ndg_old=fdda3d(ims,kms,jms,P_t_ndg_old),                        &
                  ph_ndg_old=fdda3d(ims,kms,jms,P_ph_ndg_old),                      &
                  q_ndg_old=fdda3d(ims,kms,jms,P_q_ndg_old),                        &
                  mu_ndg_old=fdda2d(ims,1,jms,P_mu_ndg_old),                        &
                  u_ndg_new=fdda3d(ims,kms,jms,P_u_ndg_new),                        &
                  v_ndg_new=fdda3d(ims,kms,jms,P_v_ndg_new),                        &
                  t_ndg_new=fdda3d(ims,kms,jms,P_t_ndg_new),                        &
                  ph_ndg_new=fdda3d(ims,kms,jms,P_ph_ndg_new),                      &
                  q_ndg_new=fdda3d(ims,kms,jms,P_q_ndg_new),                        &
                  mu_ndg_new=fdda2d(ims,1,jms,P_mu_ndg_new),                        &
                  u3d=grid%u_2,v3d=grid%v_2,th_phy=th_phy,                          &
                  ph=grid%ph_2,rho=rho,moist=moist,                                 &
                  p_phy=p_phy,pi_phy=pi_phy,p8w=p8w,t_phy=t_phy,                    &
                  dz8w=dz8w,z=grid%z,z_at_w=grid%z_at_w,                            &
                  grid=grid,config_flags=config_flags,dx=grid%DX,n_moist=num_moist, &
                  STEPFG=grid%STEPFG,                                               &
                  pblh=grid%pblh,ht=grid%ht,REGIME=grid%regime,ZNT=grid%znt         &
                   ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde               &
                   ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme               &
                   ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)               &
                   ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)               &
                   ,KTS=k_start, KTE=min(k_end,kde-1)                               &
                   , num_tiles=grid%num_tiles,                                      &
                   u10=grid%u10, v10=grid%v10, th2=grid%th2, q2=grid%q2,            &
                   u10_ndg_old=grid%u10_ndg_old,                    &
                   v10_ndg_old=grid%v10_ndg_old,                    &
                   t2_ndg_old=grid%t2_ndg_old,                      &
                   th2_ndg_old=grid%th2_ndg_old,                    &
                   q2_ndg_old=grid%q2_ndg_old,                      &
                   rh_ndg_old=grid%rh_ndg_old,                      &
                   psl_ndg_old=grid%psl_ndg_old,                    &
                   ps_ndg_old=grid%ps_ndg_old,                      &
                   tob_ndg_old=grid%tob_ndg_old,                                    &
                   odis_ndg_old=grid%odis_ndg_old,                                  &
                   u10_ndg_new=grid%u10_ndg_new,                    &
                   v10_ndg_new=grid%v10_ndg_new,                    &
                   t2_ndg_new=grid%t2_ndg_new,                      &
                   th2_ndg_new=grid%th2_ndg_new,                    &
                   q2_ndg_new=grid%q2_ndg_new,                      &
                   rh_ndg_new=grid%rh_ndg_new,                      &
                   psl_ndg_new=grid%psl_ndg_new,                    &
                   ps_ndg_new=grid%ps_ndg_new,                      &
                   tob_ndg_new=grid%tob_ndg_new,                                    &
                   odis_ndg_new=grid%odis_ndg_new                                   &
                   ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe               &
                   ,IMSX=imsx,IMEX=imex,JMSX=jmsx,JMEX=jmex,KMSX=kmsx,KMEX=kmex     &
                   ,IPSX=ipsx,IPEX=ipex,JPSX=jpsx,JPEX=jpex,KPSX=kpsx,KPEX=kpex     &
                   ,IMSY=imsy,IMEY=imey,JMSY=jmsy,JMEY=jmey,KMSY=kmsy,KMEY=kmey     &
                   ,IPSY=ipsy,IPEY=ipey,JPSY=jpsy,JPEY=jpey,KPSY=kpsy,KPEY=kpey     )

BENCH_END(fdda_driver_tim)

  END SUBROUTINE first_rk_step_part1

END MODULE module_first_rk_step_part1

