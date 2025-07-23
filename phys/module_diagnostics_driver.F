!WRF:MODEL_LAYER:PHYSICS

MODULE module_diagnostics_driver

CONTAINS

   !  This subroutine is the driver for the diagnostics packages.


   SUBROUTINE diagnostics_driver ( grid, config_flags,                  &
                                   moist, chem, tracer, scalar,         &
                                   th_phy, pi_phy, p_phy,               & 
                                   p8w, t8w, dz8w,                      &
                                   curr_secs, curr_secs2,               &
                                   diag_flag,                           &
                                   ids,  ide,  jds,  jde,  kds,  kde,   &
                                   ims,  ime,  jms,  jme,  kms,  kme,   &
                                   ips,  ipe,  jps,  jpe,  kps,  kpe,   &
                                   imsx, imex, jmsx, jmex, kmsx, kmex,  &
                                   ipsx, ipex, jpsx, jpex, kpsx, kpex,  &
                                   imsy, imey, jmsy, jmey, kmsy, kmey,  &
                                   ipsy, ipey, jpsy, jpey, kpsy, kpey   )


      !=============================================================
      !  USE Association for Generic WRF Infrastructure
      !=============================================================

      !  Pick up the number of members for each of the 4d arrays - for declaration purposes.

      USE module_state_description, ONLY: num_moist, num_chem, num_tracer, num_scalar, &
                                          SKIP_PRESS_DIAGS, SKIP_Z_DIAGS,              &
                                          DO_TRAD_FIELDS,                              &
                                          DO_SOLAR_OUTPUT,                             &
                                          PARAM_FIRST_SCALAR,                          &
                                          P_QG, P_QH, P_QV, P_QC, P_QI, P_QS,          &
#ifdef CORDEXDIAG
! L. Fita. CIMA Dec. 2017
                                          P_QR,                                        &
#endif
                                          P_QNG, P_QH, P_QNH, P_QR, P_QNR, P_QVOLG,    &
                                          F_QV, F_QC, F_QI, F_QS,                      &
                     KESSLERSCHEME, LINSCHEME, SBU_YLINSCHEME, WSM3SCHEME, WSM5SCHEME, &
                     WSM6SCHEME, ETAMPNEW, THOMPSON, THOMPSONAERO, THOMPSONGH,         &
                     MORR_TWO_MOMENT, GSFCGCESCHEME, WDM5SCHEME, WDM6SCHEME,           &
                     MILBRANDT2MOM , CAMMGMPSCHEME, FAST_KHAIN_LYNN_SHPUND, FULL_KHAIN_LYNN,  &
                     NSSL_2MOM, MORR_TM_AERO !TWG add    !,MILBRANDT3MOM, MORR_MILB_P3

      USE module_driver_constants, ONLY: max_plevs, max_zlevs

      !  From where we preferably are pulling g, Cp, etc.

      USE module_model_constants, ONLY: g, R_v, R_d, Cp, T0, RCP

      !  This gives us the type definition for grid (domain) and some clock information.

!     USE module_domain, ONLY : domain, domain_clock_get, get_ijk_from_subgrid
      USE module_domain, ONLY : domain, domain_clock_get, domain_get_current_time

      !  All of the information from the namelist is in config_flags.  The
      !  type declaration for this puppy must be available.  While each domain
      !  has a config_flags, together they are stored in model_config_rec.

      USE module_configure, ONLY : grid_config_rec_type, &
                                   model_config_rec

      USE module_streams
#ifdef CORDEXDIAG
      USE module_utility
#else
      USE module_utility, ONLY : WRFU_Time
#endif
      !=============================================================
      !  USE Association for the Diagnostic Packages
      !=============================================================
      
      USE module_lightning_driver, ONLY : lightning_driver      
      USE module_diag_misc, ONLY : diagnostic_output_calc, noahmp_output_calc
      USE module_diag_nwp, ONLY : diagnostic_output_nwp
      USE module_diag_cl, ONLY : clwrf_output_calc
      USE module_diag_pld, ONLY : pld
      USE module_diag_zld, ONLY : zld
      USE module_diag_afwa, ONLY : afwa_diagnostics_driver
      USE module_diag_rasm, ONLY : mean_output_calc, diurnalcycle_output_calc
      USE module_diag_hailcast, ONLY : hailcast_diagnostic_driver
      USE module_diag_solar, ONLY : solar_diag
      USE module_trad_fields, ONLY : trad_fields
#ifdef CORDEXDIAG
! CORDEX diagnostics. CIMA Dec.17
      USE module_diag_cordex, ONLY: cordex_output_calc
#endif

      IMPLICIT NONE


      !=============================================================
      !  Subroutine Arguments
      !=============================================================

      !  Arguments passed in.  All of the diagnostics are part of the grid structure, so
      !  even though we are not changing any of the fundamental variables, we are computing
      !  the diagnostics.  Therefore grid is INOUT.

      TYPE ( domain ), INTENT(INOUT) :: grid

      !  We are not changing any of the namelist settings.

      TYPE ( grid_config_rec_type ), INTENT(IN) :: config_flags

      !  The 4d arrays are input only, no mods to them.

      REAL , DIMENSION(ims:ime,kms:kme,jms:jme,num_moist ) , INTENT(IN) :: moist
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme,num_chem  ) , INTENT(IN) :: chem
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme,num_tracer) , INTENT(IN) :: tracer
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme,num_scalar) , INTENT(IN) :: scalar

      !  A few handy 3d arrays computed for the physics scheme: pressure (Pa) and
      !  temperature (K), on both half (_phy) and full levels.

      REAL , DIMENSION(ims:ime,kms:kme,jms:jme)            , INTENT(IN) :: th_phy  , &
                                                                           p_phy   , &
                                                                           pi_phy  , &
                                                                           dz8w    , &
                                                                           p8w     , &
                                                                           t8w

      !  Time (s) since the beginning of the simulation, restart.

      REAL(8) :: curr_secs
      REAL :: curr_secs2

      !  Is this to be a history output time?  If so, compute the diagnostics.

      LOGICAL :: diag_flag

      !  The sundry dimensions required to keep a model running smoothly:
      !     The first letter:
      !        i: refers to the nominally west east direction, the inner-most (fastest)
      !           incrementing index
      !        j: refers to the nominally south north direction, the outer-most (slowest)
      !           incrementing index
      !        k: refers to the vertical direction form bottom to top, the second dimension
      !           in all 3d arrays
      !     The second letter: 
      !        d: refers to the domain size, the geophysical extent of the entire domain,
      !           not used in dimensions or looping, used to determine when we are close to
      !           the edge of the boundary
      !        m: refers to the memory size size, all 2d and 3d arrays from the Registry
      !           (passed into here via the grid structure or the I1 variables [such as
      !           p_phy, for example]) use these values for dimensioning
      !        p: refers to the patch size, the extent over which computational loops run

      INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe

      !  Hopefully unnecessary, these are the filtered dimensions.

      INTEGER , INTENT(IN) :: imsx,imex,jmsx,jmex,kmsx,kmex,    &
                              ipsx,ipex,jpsx,jpex,kpsx,kpex,    &
                              imsy,imey,jmsy,jmey,kmsy,kmey,    &
                              ipsy,ipey,jpsy,jpey,kpsy,kpey


      !=============================================================
      !  Local Variables
      !=============================================================

      !  Handy little character string for use instead of print statements.

      CHARACTER (LEN=1000) :: diag_message

      !  OpenMP indexing of tiles.

      INTEGER :: ij

      !  Vertical indexing that only goes up to the half levels.

      INTEGER :: k_start, k_end

      ! Current time associated with current simulation step (RASM_DIAGS)

      TYPE(WRFU_Time) :: currentTime
#ifdef CORDEXDIAG
! CORDEX diagnostics. CIMA Dec.17
      INTEGER                                            :: rc
      LOGICAL                                            :: is_output_t, is_pinterp_t
      TYPE(WRFU_Time)                                    :: hist_time, aux9_time, aux23_time,         &
        CurrTime, StartTime
      TYPE(WRFU_TimeInterval)                            :: dtint, histint, aux9int, aux23int
      CHARACTER(len=256)                                 :: message, timestr
      REAL                                               :: inczsoil
#ifdef CDXWRF
      INTEGER                                            :: i
      CHARACTER(len=1024)                                :: msg
#endif

#endif

      !=============================================================
      !  Start of executable code
      !=============================================================

      CALL wrf_debug ( 100 , '--> TOP OF DIAGNOSTICS PACKAGE' )

      !  Some routine initializations.

      k_start = kps
      k_end   = kpe-1

      !  There are some fields that were defined in the first RK loop for
      !  physics and are now a time-step old.

      CALL update_phys_fields ( grid, config_flags, moist,           &
                                ids,  ide,  jds,  jde,  kds,  kde,   &
                                ims,  ime,  jms,  jme,  kms,  kme,   &
                                ips,  ipe,  jps,  jpe,  kps,  kpe    )

      !  Lightning flash rate diagnostic production.

      LIGHTNING: IF ( config_flags%lightning_option /= 0 ) THEN 
         CALL wrf_debug ( 100 , '--> CALL DIAGNOSTICS PACKAGE: LIGHTNING_DRIVER' )
         CALL lightning_driver ( &
          ! Frequently used prognostics
            curr_secs, grid%dt, grid%dx, grid%dy,              &
            grid%xlat, grid%xlong, grid%xland, grid%ht,        &
            grid%t_phy, p_phy, grid%rho,                       &
            grid%u_phy, grid%v_phy, grid%w_2,                  &    
            th_phy,     pi_phy,dz8w,                           &  
            grid%z, moist,                                     &
          ! Scheme specific prognostics
            grid%ktop_deep, grid%refl_10cm,                    &
            domain_get_current_time( grid ),                   &
          ! Flashrate namelist inputs
            config_flags%lightning_option,                     &
            config_flags%lightning_dt,                         &
            config_flags%lightning_start_seconds,              &
            grid%ltngacttime,                                  &
            config_flags%flashrate_factor,                     &
          ! IC:CG namelist settings
            config_flags%iccg_method,                          &
            config_flags%iccg_prescribed_num,                  &
            config_flags%iccg_prescribed_den,                  &
          ! IC:CG inputs
            grid%iccg_in_num, grid%iccg_in_den,                &
          ! Scheme specific namelist inputs
            config_flags%cellcount_method,                     &
            config_flags%cldtop_adjustment,                    &
          ! Order dependent args for domain, mem, and tile dims 
            ids, ide, jds, jde, kds, kde,         &
            ims, ime, jms, jme, kms, kme,         &
            ips, ipe, jps, jpe, kps, kpe,         &
          ! Mandatory outputs for all quantitative schemes
            grid%ic_flashcount, grid%ic_flashrate,          &
            grid%cg_flashcount, grid%cg_flashrate,          &
            grid%lpi                                        &   
      )    
      END IF LIGHTNING


      !WRF-HAILCAST diagnostic - hail size prediction
      HAILCAST: IF ( config_flags%hailcast_opt /= 0 ) THEN

      IF ( ( config_flags%history_interval == 0 ) ) THEN
            WRITE (diag_message , * ) &
            "HAILCAST Error : No 'history_interval' defined in namelist"
            CALL wrf_error_fatal ( diag_message )
        END IF

        !$OMP PARALLEL DO   &
        !$OMP PRIVATE ( ij )
        DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 100 ,                                             &
             '--> CALL DIAGNOSTICS PACKAGE: HAILCAST_DIAGNOSTIC_DRIVER' )

           CALL hailcast_diagnostic_driver (    grid , config_flags           &
                         ,moist, grid%rho                                     &
                         ,ids, ide, jds, jde, kds, kde                        &
                         ,ims, ime, jms, jme, kms, kme                        &
                         ,ips, ipe, jps, jpe, kps, kpe                        &
                         ,ITS=grid%i_start(ij),ITE=grid%i_end(ij)             &
                         ,JTS=grid%j_start(ij),JTE=grid%j_end(ij)             &
                         ,K_START=k_start,K_END=k_end                         &
                         ,dt=grid%dt, itimestep=grid%itimestep                &
                         ,haildt=grid%haildt, curr_secs=curr_secs2            &
                         ,haildtacttime=grid%haildtacttime                    )

        END DO
        !$OMP END PARALLEL DO
      END IF HAILCAST

      TRADITIONAL_FIELDS: IF ( config_flags%diag_nwp2 == do_trad_fields ) THEN 
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles

            CALL wrf_debug ( 100 , '--> CALL DIAGNOSTICS PACKAGE: TRAD_FIELDS' )
            CALL trad_fields ( &
               !  Input data for computing
               U=grid%u_2                                           &
               ,V=grid%v_2                                          &
               ,W=grid%w_2                                          &
               ,t=grid%th_phy_m_t0                                  &
               ,qv=moist(:,:,:,P_QV)                                &
               ,zp=grid%ph_2                                        &
               ,zb=grid%phb                                         &
               ,pp=grid%p                                           &
               ,pb=grid%pb                                          &
               ,p=grid%p_hyd                                        &
               ,pw=grid%p_hyd_w                                     &
               !  Map factors, coriolis for diags
               ,msfux=grid%msfux                                    &
               ,msfuy=grid%msfuy                                    &
               ,msfvx=grid%msfvx                                    &
               ,msfvy=grid%msfvy                                    &
               ,msftx=grid%msftx                                    &
               ,msfty=grid%msfty                                    &
               ,f=grid%f                                            &
               ,e=grid%e                                            &
               ,sina=grid%sina                                      &
               ,cosa=grid%cosa                                      &
               !  Input model diagnostic vraiables 
               ,rho=grid%rho                                        &
               ,dz8w=dz8w                                           &
               ,qc=moist(:,:,:,P_QC)                                &
               ,rainc=grid%rainc                                    &
               ,rainnc=grid%rainnc                                  &
               ,snownc=grid%snownc                                  &
               ,graupelnc=grid%graupelnc                            &
               ,hailnc=grid%hailnc                                  & 
               !  Terrestrial data                
               ,ht=grid%ht                                          &
               !  Namelist info
               ,use_theta_m=config_flags%use_theta_m                &
               !  The diagnostic output variables
               ,sealevelp=grid%sealevelp                            &
               ,temperature=grid%temperature                        &
               ,pressure=grid%pressure                              &
               ,geoheight=grid%geoheight                            &
               ,umet=grid%umet                                      &
               ,vmet=grid%vmet                                      &
               ,speed=grid%speed                                    &
               ,dir=grid%dir                                        &
               ,psfc=grid%psfc                                      &
               ,rain=grid%rain                                      &
               ,liqrain=grid%liqrain                                &
               ,tpw=grid%tpw                                        &
               ,potential_t=grid%potential_t                        &
               ,rh=grid%rh                                          &
               !  Various indexes for declarations, loop bounds
               ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde   &
               ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme   &
               ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe   &
               ,ITS=grid%i_start(ij),ITE=grid%i_end(ij)             &
               ,JTS=grid%j_start(ij),JTE=grid%j_end(ij)             &
               ,kts=kps,kte=kpe                                     )
          END DO
        !$OMP END PARALLEL DO
      END IF TRADITIONAL_FIELDS

      !  Mostly surface values, precip, column integrated quantities.

      CALL wrf_debug ( 100 , '--> CALL DIAGNOSTICS PACKAGE: ACCUMULATED AND BUCKET DIAGNOSTICS' )

      CALL diagnostic_output_calc(                                   &
                 DPSDT=grid%dpsdt   ,DMUDT=grid%dmudt                &
                ,P8W=p8w   ,PK1M=grid%pk1m                           &
                ,MU_2=grid%mu_2  ,MU_2M=grid%mu_2m                   &
                ,RAINCV=grid%raincv    ,RAINNCV=grid%rainncv         &
                ,RAINC=grid%rainc    ,RAINNC=grid%rainnc             &
                ,I_RAINC=grid%i_rainc    ,I_RAINNC=grid%i_rainnc     &
                ,HFX=grid%hfx   ,SFCEVP=grid%sfcevp    ,LH=grid%lh   &    
                ,T2=grid%t2                                          &    
                ,DT=grid%dt                                          &    
                ,XTIME=grid%xtime                                    &
           ,ACSWUPT=grid%acswupt    ,ACSWUPTC=grid%acswuptc          &
           ,ACSWDNT=grid%acswdnt    ,ACSWDNTC=grid%acswdntc          &
           ,ACSWUPB=grid%acswupb    ,ACSWUPBC=grid%acswupbc          &
           ,ACSWDNB=grid%acswdnb    ,ACSWDNBC=grid%acswdnbc          &
           ,ACLWUPT=grid%aclwupt    ,ACLWUPTC=grid%aclwuptc          &
           ,ACLWDNT=grid%aclwdnt    ,ACLWDNTC=grid%aclwdntc          &
           ,ACLWUPB=grid%aclwupb    ,ACLWUPBC=grid%aclwupbc          &
           ,ACLWDNB=grid%aclwdnb    ,ACLWDNBC=grid%aclwdnbc          &
         ,I_ACSWUPT=grid%i_acswupt  ,I_ACSWUPTC=grid%i_acswuptc      &
         ,I_ACSWDNT=grid%i_acswdnt  ,I_ACSWDNTC=grid%i_acswdntc      &
         ,I_ACSWUPB=grid%i_acswupb  ,I_ACSWUPBC=grid%i_acswupbc      &
         ,I_ACSWDNB=grid%i_acswdnb  ,I_ACSWDNBC=grid%i_acswdnbc      &
         ,I_ACLWUPT=grid%i_aclwupt  ,I_ACLWUPTC=grid%i_aclwuptc      &
         ,I_ACLWDNT=grid%i_aclwdnt  ,I_ACLWDNTC=grid%i_aclwdntc      &
         ,I_ACLWUPB=grid%i_aclwupb  ,I_ACLWUPBC=grid%i_aclwupbc      &
         ,I_ACLWDNB=grid%i_aclwdnb  ,I_ACLWDNBC=grid%i_aclwdnbc      &
           ,athmp=grid%athmpten     ,aqvmp=grid%aqvmpten             &
           ,athcu=grid%athcuten     ,aqvcu=grid%aqvcuten             &
           ,aucu=grid%aucuten       ,avcu=grid%avcuten               &
           ,athsh=grid%athshten     ,aqvsh=grid%aqvshten             &
           ,aush=grid%aushten       ,avsh=grid%avshten               &
           ,athbl=grid%athblten     ,aqvbl=grid%aqvblten             &
           ,aubl=grid%aublten       ,avbl=grid%avblten               &
           ,athralw=grid%athratenlw ,athrasw=grid%athratensw         &
           ,h_diabatic=grid%h_diabatic ,qv_diabatic=grid%qv_diabatic &
           ,rthcuten=grid%rthcuten  ,rqvcuten=grid%rqvcuten          &
           ,rucuten=grid%rucuten    ,rvcuten=grid%rvcuten            &
           ,rthshten=grid%rthshten  ,rqvshten=grid%rqvshten          &
           ,rushten=grid%rushten    ,rvshten=grid%rvshten            &
           ,rthblten=grid%rthblten  ,rqvblten=grid%rqvblten          &
           ,rublten=grid%rublten    ,rvblten=grid%rvblten            &
           ,rthratenlw=grid%rthratenlw  ,rthratensw=grid%rthratensw  &
      ! Selection flag 
                ,DIAG_PRINT=config_flags%diag_print                  &
                ,BUCKET_MM=config_flags%bucket_mm                    &
                ,BUCKET_J =config_flags%bucket_J                     &
                ,SNOWNCV=grid%snowncv, SNOW_ACC_NC=grid%snow_acc_nc  &    
                ,PREC_ACC_C=grid%prec_acc_c                          &
                ,PREC_ACC_NC=grid%prec_acc_nc                        &
                ,PREC_ACC_DT=config_flags%prec_acc_dt                &
                ,CURR_SECS2=curr_secs2                               &
                ,HISTORY_INTERVAL=grid%history_interval              &
                ,ITIMESTEP=grid%itimestep                            &
                ,cu_used=grid%cu_used                                &
                ,shcu_used=grid%shcu_used                            &
                ,acc_phy_tend=grid%acc_phy_tend                      &
      ! Dimension arguments
                ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde   &
                ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme   &
                ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe   &
                ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)   &
                ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)   &
                ,KTS=k_start, KTE=min(k_end,kde-1)                   &
                ,NUM_TILES=grid%num_tiles                            &
                                                                    )

      NWPDIAGS: IF ( config_flags%nwp_diagnostics == 1 ) THEN
      CALL wrf_debug ( 100 , '--> CALL DIAGNOSTICS PACKAGE: NWP DIAGNOSTICS' )

      mp_select: SELECT CASE(config_flags%mp_physics)

        CASE (LINSCHEME, WSM6SCHEME, WDM6SCHEME, GSFCGCESCHEME)

      CALL diagnostic_output_nwp(                                    &
                 config_flags=config_flags,                          &
                 U=grid%u_2    ,V=grid%v_2                           &
                ,TEMP=grid%t_phy ,P8W=p8w                            &
                ,DT=grid%dt      ,SBW=config_flags%spec_bdy_width    &    
                ,XTIME=grid%xtime                                    &
      ! Selection flag 
                ,MPHYSICS_OPT=config_flags%mp_physics                &  !  gthompsn
                ,GSFCGCE_HAIL=config_flags%gsfcgce_hail              &  !  gthompsn
                ,GSFCGCE_2ICE=config_flags%gsfcgce_2ice              &  !  gthompsn
                ,MPUSE_HAIL=config_flags%hail_opt                    &  !  gthompsn
                ,NSSL_ALPHAH=config_flags%nssl_alphah                &  !  gthompsn
                ,NSSL_ALPHAHL=config_flags%nssl_alphahl              &  !  gthompsn
                ,NSSL_CNOH=config_flags%nssl_cnoh                    &  !  gthompsn
                ,NSSL_CNOHL=config_flags%nssl_cnohl                  &  !  gthompsn
                ,NSSL_RHO_QH=config_flags%nssl_rho_qh                &  !  gthompsn
                ,NSSL_RHO_QHL=config_flags%nssl_rho_qhl              &  !  gthompsn
                ,CURR_SECS2=curr_secs2                               &
                ,NWP_DIAGNOSTICS=config_flags%nwp_diagnostics        &
                ,DIAGFLAG=diag_flag                                  &
                ,HISTORY_INTERVAL=grid%history_interval              &
                ,ITIMESTEP=grid%itimestep                            &
                ,U10=grid%u10,V10=grid%v10,W=grid%w_2                &
                ,WSPD10MAX=grid%wspd10max                            &
                ,UP_HELI_MAX=grid%up_heli_max                        &
                ,W_UP_MAX=grid%w_up_max,W_DN_MAX=grid%w_dn_max       &
                ,ZNW=grid%znw,W_COLMEAN=grid%w_colmean               &
                ,NUMCOLPTS=grid%numcolpts,W_MEAN=grid%w_mean         &
                ,GRPL_MAX=grid%grpl_max,GRPL_COLINT=grid%grpl_colint &
                ,REFD_MAX=grid%refd_max                              &
                ,refl_10cm=grid%refl_10cm                            &
                ,HAIL_MAXK1=grid%hail_maxk1,HAIL_MAX2D=grid%hail_max2d &  !  gthompsn
                ,QG_CURR=moist(ims,kms,jms,P_QG)                     &
                ,RHO=grid%rho,PH=grid%ph_2,PHB=grid%phb,G=g          &
      ! Dimension arguments
                ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde   &
                ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme   &
                ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe   &
                ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)   &
                ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)   &
                ,KTS=k_start, KTE=min(k_end,kde-1)                   &
                ,NUM_TILES=grid%num_tiles                            &
                ,MAX_TIME_STEP=grid%max_time_step                    &
                ,ADAPTIVE_TS=config_flags%use_adaptive_time_step     &
                                                                    )

        CASE (THOMPSON, THOMPSONAERO)

      CALL diagnostic_output_nwp(                                   &
                 config_flags=config_flags,                          &
                 U=grid%u_2    ,V=grid%v_2                           &
                ,TEMP=grid%t_phy ,P8W=p8w                            &
                ,DT=grid%dt      ,SBW=config_flags%spec_bdy_width    &    
                ,XTIME=grid%xtime                                    &
      ! Selection flag 
                ,MPHYSICS_OPT=config_flags%mp_physics                &  !  gthompsn
                ,GSFCGCE_HAIL=config_flags%gsfcgce_hail              &  !  gthompsn
                ,GSFCGCE_2ICE=config_flags%gsfcgce_2ice              &  !  gthompsn
                ,MPUSE_HAIL=config_flags%hail_opt                    &  !  gthompsn
                ,NSSL_ALPHAH=config_flags%nssl_alphah                &  !  gthompsn
                ,NSSL_ALPHAHL=config_flags%nssl_alphahl              &  !  gthompsn
                ,NSSL_CNOH=config_flags%nssl_cnoh                    &  !  gthompsn
                ,NSSL_CNOHL=config_flags%nssl_cnohl                  &  !  gthompsn
                ,NSSL_RHO_QH=config_flags%nssl_rho_qh                &  !  gthompsn
                ,NSSL_RHO_QHL=config_flags%nssl_rho_qhl              &  !  gthompsn
                ,CURR_SECS2=curr_secs2                               &
                ,NWP_DIAGNOSTICS=config_flags%nwp_diagnostics        &
                ,DIAGFLAG=diag_flag                                  &
                ,HISTORY_INTERVAL=grid%history_interval              &
                ,ITIMESTEP=grid%itimestep                            &
                ,U10=grid%u10,V10=grid%v10,W=grid%w_2                &
                ,WSPD10MAX=grid%wspd10max                            &
                ,UP_HELI_MAX=grid%up_heli_max                        &
                ,W_UP_MAX=grid%w_up_max,W_DN_MAX=grid%w_dn_max       &
                ,ZNW=grid%znw,W_COLMEAN=grid%w_colmean               &
                ,NUMCOLPTS=grid%numcolpts,W_MEAN=grid%w_mean         &
                ,GRPL_MAX=grid%grpl_max,GRPL_COLINT=grid%grpl_colint &
                ,REFD_MAX=grid%refd_max                              &
                ,refl_10cm=grid%refl_10cm                            &
                ,HAIL_MAXK1=grid%hail_maxk1,HAIL_MAX2D=grid%hail_max2d &  !  gthompsn
                ,QG_CURR=moist(ims,kms,jms,P_QG)                     &
                ,QR_CURR=moist(ims,kms,jms,P_QR)                     &  !  gthompsn
                ,NR_CURR=scalar(ims,kms,jms,P_QNR)                   &  !  gthompsn
                ,RHO=grid%rho,PH=grid%ph_2,PHB=grid%phb,G=g          &
      ! Dimension arguments
                ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde   &
                ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme   &
                ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe   &
                ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)   &
                ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)   &
                ,KTS=k_start, KTE=min(k_end,kde-1)                   &
                ,NUM_TILES=grid%num_tiles                            &
                ,MAX_TIME_STEP=grid%max_time_step                    &
                ,ADAPTIVE_TS=config_flags%use_adaptive_time_step     &
                                                                    )

        CASE (THOMPSONGH)

      CALL diagnostic_output_nwp(                                   &
                 config_flags=config_flags,                          &
                 U=grid%u_2    ,V=grid%v_2                           &
                ,TEMP=grid%t_phy ,P8W=p8w                            &
                ,DT=grid%dt      ,SBW=config_flags%spec_bdy_width    &    
                ,XTIME=grid%xtime                                    &
      ! Selection flag 
                ,MPHYSICS_OPT=config_flags%mp_physics                &  !  gthompsn
                ,GSFCGCE_HAIL=config_flags%gsfcgce_hail              &  !  gthompsn
                ,GSFCGCE_2ICE=config_flags%gsfcgce_2ice              &  !  gthompsn
                ,MPUSE_HAIL=config_flags%hail_opt                    &  !  gthompsn
                ,NSSL_ALPHAH=config_flags%nssl_alphah                &  !  gthompsn
                ,NSSL_ALPHAHL=config_flags%nssl_alphahl              &  !  gthompsn
                ,NSSL_CNOH=config_flags%nssl_cnoh                    &  !  gthompsn
                ,NSSL_CNOHL=config_flags%nssl_cnohl                  &  !  gthompsn
                ,NSSL_RHO_QH=config_flags%nssl_rho_qh                &  !  gthompsn
                ,NSSL_RHO_QHL=config_flags%nssl_rho_qhl              &  !  gthompsn
                ,CURR_SECS2=curr_secs2                               &
                ,NWP_DIAGNOSTICS=config_flags%nwp_diagnostics        &
                ,DIAGFLAG=diag_flag                                  &
                ,HISTORY_INTERVAL=grid%history_interval              &
                ,ITIMESTEP=grid%itimestep                            &
                ,U10=grid%u10,V10=grid%v10,W=grid%w_2                &
                ,WSPD10MAX=grid%wspd10max                            &
                ,UP_HELI_MAX=grid%up_heli_max                        &
                ,W_UP_MAX=grid%w_up_max,W_DN_MAX=grid%w_dn_max       &
                ,ZNW=grid%znw,W_COLMEAN=grid%w_colmean               &
                ,NUMCOLPTS=grid%numcolpts,W_MEAN=grid%w_mean         &
                ,GRPL_MAX=grid%grpl_max,GRPL_COLINT=grid%grpl_colint &
                ,REFD_MAX=grid%refd_max                              &
                ,refl_10cm=grid%refl_10cm                            &
                ,HAIL_MAXK1=grid%hail_maxk1,HAIL_MAX2D=grid%hail_max2d &  !  gthompsn
                ,QG_CURR=moist(ims,kms,jms,P_QG)                     &
                ,QR_CURR=moist(ims,kms,jms,P_QR)                     &  !  gthompsn
                ,NR_CURR=scalar(ims,kms,jms,P_QNR)                   &  !  gthompsn
                ,NG_CURR=scalar(ims,kms,jms,P_QNG)                   &  ! THOMPSONGH
                ,QVOLG_CURR=scalar(ims,kms,jms,P_QVOLG)              &  ! THOMPSONGH
                ,RHO=grid%rho,PH=grid%ph_2,PHB=grid%phb,G=g          &
      ! Dimension arguments
                ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde   &
                ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme   &
                ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe   &
                ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)   &
                ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)   &
                ,KTS=k_start, KTE=min(k_end,kde-1)                   &
                ,NUM_TILES=grid%num_tiles                            &
                ,MAX_TIME_STEP=grid%max_time_step                    &
                ,ADAPTIVE_TS=config_flags%use_adaptive_time_step     &
                                                                    )

        CASE (MORR_TWO_MOMENT, MORR_TM_AERO)  ! TWG add

      CALL diagnostic_output_nwp(                                   &
                 config_flags=config_flags,                          &
                 U=grid%u_2    ,V=grid%v_2                           &
                ,TEMP=grid%t_phy ,P8W=p8w                            &
                ,DT=grid%dt      ,SBW=config_flags%spec_bdy_width    &    
                ,XTIME=grid%xtime                                    &
      ! Selection flag 
                ,MPHYSICS_OPT=config_flags%mp_physics                &  !  gthompsn
                ,GSFCGCE_HAIL=config_flags%gsfcgce_hail              &  !  gthompsn
                ,GSFCGCE_2ICE=config_flags%gsfcgce_2ice              &  !  gthompsn
                ,MPUSE_HAIL=config_flags%morr_rimed_ice              &  !  gthompsn
                ,NSSL_ALPHAH=config_flags%nssl_alphah                &  !  gthompsn
                ,NSSL_ALPHAHL=config_flags%nssl_alphahl              &  !  gthompsn
                ,NSSL_CNOH=config_flags%nssl_cnoh                    &  !  gthompsn
                ,NSSL_CNOHL=config_flags%nssl_cnohl                  &  !  gthompsn
                ,NSSL_RHO_QH=config_flags%nssl_rho_qh                &  !  gthompsn
                ,NSSL_RHO_QHL=config_flags%nssl_rho_qhl              &  !  gthompsn
                ,CURR_SECS2=curr_secs2                               &
                ,NWP_DIAGNOSTICS=config_flags%nwp_diagnostics        &
                ,DIAGFLAG=diag_flag                                  &
                ,HISTORY_INTERVAL=grid%history_interval              &
                ,ITIMESTEP=grid%itimestep                            &
                ,U10=grid%u10,V10=grid%v10,W=grid%w_2                &
                ,WSPD10MAX=grid%wspd10max                            &
                ,UP_HELI_MAX=grid%up_heli_max                        &
                ,W_UP_MAX=grid%w_up_max,W_DN_MAX=grid%w_dn_max       &
                ,ZNW=grid%znw,W_COLMEAN=grid%w_colmean               &
                ,NUMCOLPTS=grid%numcolpts,W_MEAN=grid%w_mean         &
                ,GRPL_MAX=grid%grpl_max,GRPL_COLINT=grid%grpl_colint &
                ,REFD_MAX=grid%refd_max                              &
                ,refl_10cm=grid%refl_10cm                            &
                ,HAIL_MAXK1=grid%hail_maxk1,HAIL_MAX2D=grid%hail_max2d &  !  gthompsn
                ,QG_CURR=moist(ims,kms,jms,P_QG)                     &
                ,NG_CURR=scalar(ims,kms,jms,P_QNG)                   &  !  gthompsn
                ,RHO=grid%rho,PH=grid%ph_2,PHB=grid%phb,G=g          &
      ! Dimension arguments
                ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde   &
                ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme   &
                ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe   &
                ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)   &
                ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)   &
                ,KTS=k_start, KTE=min(k_end,kde-1)                   &
                ,NUM_TILES=grid%num_tiles                            &
                ,MAX_TIME_STEP=grid%max_time_step                    &
                ,ADAPTIVE_TS=config_flags%use_adaptive_time_step     &
                                                                    )


        CASE (MILBRANDT2MOM, NSSL_2MOM)

      CALL diagnostic_output_nwp(                                   &
                 config_flags=config_flags,                          &
                 U=grid%u_2    ,V=grid%v_2                           &
                ,TEMP=grid%t_phy ,P8W=p8w                            &
                ,DT=grid%dt      ,SBW=config_flags%spec_bdy_width    &    
                ,XTIME=grid%xtime                                    &
      ! Selection flag 
                ,MPHYSICS_OPT=config_flags%mp_physics                &  !  gthompsn
                ,GSFCGCE_HAIL=config_flags%gsfcgce_hail              &  !  gthompsn
                ,GSFCGCE_2ICE=config_flags%gsfcgce_2ice              &  !  gthompsn
                ,MPUSE_HAIL=config_flags%hail_opt                    &  !  gthompsn
                ,NSSL_ALPHAH=config_flags%nssl_alphah                &  !  gthompsn
                ,NSSL_ALPHAHL=config_flags%nssl_alphahl              &  !  gthompsn
                ,NSSL_CNOH=config_flags%nssl_cnoh                    &  !  gthompsn
                ,NSSL_CNOHL=config_flags%nssl_cnohl                  &  !  gthompsn
                ,NSSL_RHO_QH=config_flags%nssl_rho_qh                &  !  gthompsn
                ,NSSL_RHO_QHL=config_flags%nssl_rho_qhl              &  !  gthompsn
                ,CURR_SECS2=curr_secs2                               &
                ,NWP_DIAGNOSTICS=config_flags%nwp_diagnostics        &
                ,DIAGFLAG=diag_flag                                  &
                ,HISTORY_INTERVAL=grid%history_interval              &
                ,ITIMESTEP=grid%itimestep                            &
                ,U10=grid%u10,V10=grid%v10,W=grid%w_2                &
                ,WSPD10MAX=grid%wspd10max                            &
                ,UP_HELI_MAX=grid%up_heli_max                        &
                ,W_UP_MAX=grid%w_up_max,W_DN_MAX=grid%w_dn_max       &
                ,ZNW=grid%znw,W_COLMEAN=grid%w_colmean               &
                ,NUMCOLPTS=grid%numcolpts,W_MEAN=grid%w_mean         &
                ,GRPL_MAX=grid%grpl_max,GRPL_COLINT=grid%grpl_colint &
                ,REFD_MAX=grid%refd_max                              &
                ,refl_10cm=grid%refl_10cm                            &
                ,HAIL_MAXK1=grid%hail_maxk1,HAIL_MAX2D=grid%hail_max2d &  !  gthompsn
                ,QG_CURR=moist(ims,kms,jms,P_QG)                     &
                ,NG_CURR=scalar(ims,kms,jms,P_QNG)                   &  !  gthompsn
                ,QH_CURR=moist(ims,kms,jms,P_QH)                     &  !  gthompsn
                ,NH_CURR=scalar(ims,kms,jms,P_QNH)                   &  !  gthompsn
                ,RHO=grid%rho,PH=grid%ph_2,PHB=grid%phb,G=g          &
      ! Dimension arguments
                ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde   &
                ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme   &
                ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe   &
                ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)   &
                ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)   &
                ,KTS=k_start, KTE=min(k_end,kde-1)                   &
                ,NUM_TILES=grid%num_tiles                            &
                ,MAX_TIME_STEP=grid%max_time_step                    &
                ,ADAPTIVE_TS=config_flags%use_adaptive_time_step     &
                                                                    )

        !..The remaining microphysics schemes do not have graupel, but
        !..P_QG will just be empty and the remaining NWP-diagnostics can
        !..still be computed, so go ahead, under DEFAULT, not their own.

!       CASE (KESSLERSCHEME)

!       CASE (WDM5SCHEME)

!       CASE (SBU_YLINSCHEME)

!       CASE (ETAMPNEW)

!       CASE (MILBRANDT3MOM)

!       CASE (MORR_MILB_P3)

!       CASE (CAMMGMPSCHEME)

!       CASE (FULL_KHAIN_LYNN)

!       CASE (FAST_KHAIN_LYNN_SHPUND)

!       CASE (WSM3SCHEME)

!       CASE (WSM5SCHEME)

        CASE DEFAULT

      CALL diagnostic_output_nwp(                                    &
                 config_flags=config_flags,                          &
                 U=grid%u_2    ,V=grid%v_2                           &
                ,TEMP=grid%t_phy ,P8W=p8w                            &
                ,DT=grid%dt      ,SBW=config_flags%spec_bdy_width    &    
                ,XTIME=grid%xtime                                    &
      ! Selection flag 
                ,MPHYSICS_OPT=config_flags%mp_physics                &  !  gthompsn
                ,GSFCGCE_HAIL=config_flags%gsfcgce_hail              &  !  gthompsn
                ,GSFCGCE_2ICE=config_flags%gsfcgce_2ice              &  !  gthompsn
                ,MPUSE_HAIL=config_flags%hail_opt                    &  !  gthompsn
                ,NSSL_ALPHAH=config_flags%nssl_alphah                &  !  gthompsn
                ,NSSL_ALPHAHL=config_flags%nssl_alphahl              &  !  gthompsn
                ,NSSL_CNOH=config_flags%nssl_cnoh                    &  !  gthompsn
                ,NSSL_CNOHL=config_flags%nssl_cnohl                  &  !  gthompsn
                ,NSSL_RHO_QH=config_flags%nssl_rho_qh                &  !  gthompsn
                ,NSSL_RHO_QHL=config_flags%nssl_rho_qhl              &  !  gthompsn
                ,CURR_SECS2=curr_secs2                               &
                ,NWP_DIAGNOSTICS=config_flags%nwp_diagnostics        &
                ,DIAGFLAG=diag_flag                                  &
                ,HISTORY_INTERVAL=grid%history_interval              &
                ,ITIMESTEP=grid%itimestep                            &
                ,U10=grid%u10,V10=grid%v10,W=grid%w_2                &
                ,WSPD10MAX=grid%wspd10max                            &
                ,UP_HELI_MAX=grid%up_heli_max                        &
                ,W_UP_MAX=grid%w_up_max,W_DN_MAX=grid%w_dn_max       &
                ,ZNW=grid%znw,W_COLMEAN=grid%w_colmean               &
                ,NUMCOLPTS=grid%numcolpts,W_MEAN=grid%w_mean         &
                ,GRPL_MAX=grid%grpl_max,GRPL_COLINT=grid%grpl_colint &
                ,REFD_MAX=grid%refd_max                              &
                ,refl_10cm=grid%refl_10cm                            &
                ,HAIL_MAXK1=grid%hail_maxk1,HAIL_MAX2D=grid%hail_max2d &  !  gthompsn
                ,QG_CURR=moist(ims,kms,jms,P_QG)                     &
                ,RHO=grid%rho,PH=grid%ph_2,PHB=grid%phb,G=g          &
      ! Dimension arguments
                ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde   &
                ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme   &
                ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe   &
                ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)   &
                ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)   &
                ,KTS=k_start, KTE=min(k_end,kde-1)                   &
                ,NUM_TILES=grid%num_tiles                            &
                ,MAX_TIME_STEP=grid%max_time_step                    &
                ,ADAPTIVE_TS=config_flags%use_adaptive_time_step     &
                                                                    )

   END SELECT mp_select
   END IF NWPDIAGS


      !  Climate-oriented diagnostic quantities.

      CLIMATE_DIAGS : IF ( config_flags%output_diagnostics == 1 ) THEN

         IF ( ( config_flags%auxhist3_interval == 0 ) ) THEN
            WRITE (diag_message , * ) &
            "CLWRF: ERROR -- error -- ERROR -- error : NO 'auxhist3_interval' has been defined in 'namelist.input'"
            CALL wrf_error_fatal ( diag_message )
         END IF

         CALL wrf_debug ( 100 , '--> CALL DIAGNOSTICS PACKAGE: CLIMATE DIAGNOSTICS' )

         CALL clwrf_output_calc(                                           &
                     is_restart=config_flags%restart                       &
                    ,clwrfH=config_flags%auxhist3_interval                 &
                    ,T2=grid%t2, Q2=grid%q2, U10=grid%u10, V10=grid%v10    &
                    ,SKINTEMP=grid%tsk                                     &
                    ,T2CLMIN=grid%t2min, T2CLMAX=grid%t2max                &
                    ,TT2CLMIN=grid%tt2min, TT2CLMAX=grid%tt2max            &
                    ,T2CLMEAN=grid%t2mean, T2CLSTD=grid%t2std              &
                    ,Q2CLMIN=grid%q2min, Q2CLMAX=grid%q2max                &
                    ,TQ2CLMIN=grid%tq2min, TQ2CLMAX=grid%tq2max            &
                    ,Q2CLMEAN=grid%q2mean, Q2CLSTD=grid%q2std              &
                    ,U10CLMAX=grid%u10max, V10CLMAX=grid%v10max            &
                    ,SPDUV10CLMAX=grid%spduv10max                          &
                    ,TSPDUV10CLMAX=grid%tspduv10max                        &
                    ,U10CLMEAN=grid%u10mean, V10CLMEAN=grid%v10mean        &
                    ,SPDUV10CLMEAN=grid%spduv10mean                        &
                    ,U10CLSTD=grid%u10std, V10CLSTD=grid%v10std            &
                    ,SPDUV10CLSTD=grid%spduv10std                          &
                    ,RAINCCLMAX=grid%raincvmax                             &
                    ,RAINNCCLMAX=grid%rainncvmax                           &
                    ,TRAINCCLMAX=grid%traincvmax                           &
                    ,TRAINNCCLMAX=grid%trainncvmax                         &
                    ,RAINCCLMEAN=grid%raincvmean                           &
                    ,RAINNCCLMEAN=grid%rainncvmean                         &
                    ,RAINCCLSTD=grid%raincvstd                             &
                    ,RAINNCCLSTD=grid%rainncvstd                           &
                    ,SKINTEMPCLMIN=grid%skintempmin                        &
                    ,SKINTEMPCLMAX=grid%skintempmax                        &
                    ,TSKINTEMPCLMIN=grid%tskintempmin                      &
                    ,TSKINTEMPCLMAX=grid%tskintempmax                      &
                    ,SKINTEMPCLMEAN=grid%skintempmean                      &
                    ,SKINTEMPCLSTD=grid%skintempstd                        &
                    ,RAINCV=grid%raincv    ,RAINNCV=grid%rainncv           &
                    ,DT=grid%dt                                            &
                    ,XTIME=grid%xtime,CURR_SECS2=curr_secs2                &
                    ,NSTEPS=grid%nsteps                                    &
         ! Dimension arguments
                    ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde     &
                    ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme     &
                    ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe     &
                    ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)     &
                    ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)     &
                    ,KTS=k_start, KTE=k_end                                &
                    ,NUM_TILES=grid%num_tiles                              &
                                                                   )
      END IF CLIMATE_DIAGS





      !  Pressure level diagnostics.


      PL_DIAGNOSTICS : IF ( config_flags%p_lev_diags .NE. SKIP_PRESS_DIAGS ) THEN

      !  Process the diags if this is the correct time step OR
      !  if this is an adaptive timestep forecast.

         TIME_TO_DO_PL_DIAGS : IF ( ( ( MOD(NINT(curr_secs2+grid%dt),NINT(config_flags%p_lev_interval)) .EQ. 0 ) ) .OR. &
               ( config_flags%use_adaptive_time_step ) ) THEN

            !$OMP PARALLEL DO   &
            !$OMP PRIVATE ( ij )
            DO ij = 1 , grid%num_tiles

               CALL wrf_debug ( 100 , '--> CALL DIAGNOSTICS PACKAGE: PRESSURE LEVEL DIAGNOSTICS' )

               CALL pld (                                                   &
               !  Input data for computing
                       U=grid%u_2                                           &
                      ,V=grid%v_2                                           &
                      ,W=grid%w_2                                           &
                      ,t=grid%th_phy_m_t0                                   &
                      ,qv=moist(:,:,:,P_QV)                                 &
                      ,zp=grid%ph_2                                         &
                      ,zb=grid%phb                                          &
                      ,pp=grid%p                                            &
                      ,pb=grid%pb                                           &
                      ,p=grid%p_hyd                                         &
                      ,pw=grid%p_hyd_w                                      &
               !  Map factors, coriolis for diags
                      ,msfux=grid%msfux                                     &
                      ,msfuy=grid%msfuy                                     &
                      ,msfvx=grid%msfvx                                     &
                      ,msfvy=grid%msfvy                                     &
                      ,msftx=grid%msftx                                     &
                      ,msfty=grid%msfty                                     &
                      ,f=grid%f                                             &
                      ,e=grid%e                                             &
               !  Namelist info
                      ,use_tot_or_hyd_p=config_flags%use_tot_or_hyd_p       &
                      ,extrap_below_grnd=config_flags%extrap_below_grnd     &
                      ,missing=config_flags%p_lev_missing                   &
               !  The diagnostics, mostly output variables
                      ,num_press_levels=config_flags%num_press_levels       &
                      ,max_press_levels=max_plevs                           &
                      ,press_levels=model_config_rec%press_levels           &
                      ,p_pl  = grid%p_pl                                    &
                      ,u_pl  = grid%u_pl                                    &
                      ,v_pl  = grid%v_pl                                    &
                      ,t_pl  = grid%t_pl                                    &
                      ,rh_pl = grid%rh_pl                                   &
                      ,ght_pl= grid%ght_pl                                  &
                      ,s_pl  = grid%s_pl                                    &
                      ,td_pl = grid%td_pl                                   &
                      ,q_pl = grid%q_pl                                     &
#ifdef CORDEXDIAG
                      ,sina = grid%sina                                     &
                      ,cosa = grid%cosa                                     &
                      ,psfc = grid%psfc                                     &
                      ,topo = grid%ht                                       &
                      ,alt = grid%alt                                       &
                      ,p_top = config_flags%p_top_requested                 &
                      ,iqc=moist(:,:,:,P_QC)                                &
                      ,iqr=moist(:,:,:,P_QR)                                &
                      ,iqs=moist(:,:,:,P_QS)                                &
                      ,iqi=moist(:,:,:,P_QI)                                &
                      ,iqg=moist(:,:,:,P_QG)                                &
                      ,iqh=moist(:,:,:,P_QH)                                &
                      ,hus_pl = grid%hus_pl                                 &
                      ,w_pl = grid%w_pl                                     &
                      ,uer_pl = grid%uer_pl                                 &
                      ,ver_pl = grid%ver_pl                                 &
                      ,ws_pl = grid%ws_pl                                   &
                      ,qc_pl = grid%qc_pl                                   &
                      ,qr_pl = grid%qr_pl                                   &
                      ,qs_pl = grid%qs_pl                                   &
                      ,qi_pl = grid%qi_pl                                   &
                      ,qg_pl = grid%qg_pl                                   &
                      ,qh_pl = grid%qh_pl                                   &
                      ,qv_pl_mc = grid%qv_pl_mc                             &
                      ,qc_pl_mc = grid%qc_pl_mc                             &
                      ,qr_pl_mc = grid%qr_pl_mc                             &
                      ,qs_pl_mc = grid%qs_pl_mc                             &
                      ,qi_pl_mc = grid%qi_pl_mc                             &
                      ,qg_pl_mc = grid%qg_pl_mc                             &
                      ,qh_pl_mc = grid%qh_pl_mc                             &
#endif
               !  Dimension arguments
                      ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde    &
                      ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme    &
                      ,ITS=grid%i_start(ij),ITE=grid%i_end(ij)              &
                      ,JTS=grid%j_start(ij),JTE=grid%j_end(ij)              &
                      ,KTS=k_start,KTE=k_end+1                              )
            END DO
            !$OMP END PARALLEL DO
         END IF TIME_TO_DO_PL_DIAGS
      END IF PL_DIAGNOSTICS





      !  Height level and AGL diagnostics.


      ZL_DIAGNOSTICS : IF ( config_flags%z_lev_diags .NE. SKIP_Z_DIAGS ) THEN

      !  Process the diags if this is the correct time step OR
      !  if this is an adaptive timestep forecast.

         TIME_TO_DO_ZL_DIAGS : IF ( ( ( MOD(NINT(curr_secs2+grid%dt),NINT(config_flags%z_lev_interval)) .EQ. 0 ) ) .OR. &
               ( config_flags%use_adaptive_time_step ) ) THEN

            !$OMP PARALLEL DO   &
            !$OMP PRIVATE ( ij )
            DO ij = 1 , grid%num_tiles

               CALL wrf_debug ( 100 , '--> CALL DIAGNOSTICS PACKAGE: HEIGHT LEVEL AND AGL DIAGNOSTICS' )

               CALL zld (                                                   &
               !  Input data for computing
                       U=grid%u_2                                           &
                      ,V=grid%v_2                                           &
                      ,W=grid%w_2                                           &
                      ,t=grid%th_phy_m_t0                                   &
                      ,qv=moist(:,:,:,P_QV)                                 &
                      ,zp=grid%ph_2                                         &
                      ,zb=grid%phb                                          &
                      ,pp=grid%p                                            &
                      ,pb=grid%pb                                           &
                      ,p=grid%p_hyd                                         &
                      ,pw=grid%p_hyd_w                                      &
               !  Map factors, coriolis for diags
                      ,msfux=grid%msfux                                     &
                      ,msfuy=grid%msfuy                                     &
                      ,msfvx=grid%msfvx                                     &
                      ,msfvy=grid%msfvy                                     &
                      ,msftx=grid%msftx                                     &
                      ,msfty=grid%msfty                                     &
                      ,f=grid%f                                             &
                      ,e=grid%e                                             &
                      ,ht=grid%ht                                           &
               !  Namelist info
                      ,use_tot_or_hyd_p=config_flags%use_tot_or_hyd_p       &
                      ,extrap_below_grnd=config_flags%extrap_below_grnd     &
                      ,missing=config_flags%z_lev_missing                   &
               !  The diagnostics, mostly output variables
                      ,num_z_levels=config_flags%num_z_levels               &
                      ,max_z_levels=max_zlevs                               &
                      ,z_levels=model_config_rec%z_levels                   &
                      ,z_zl  = grid%z_zl                                    &
                      ,u_zl  = grid%u_zl                                    &
                      ,v_zl  = grid%v_zl                                    &
                      ,t_zl  = grid%t_zl                                    &
                      ,rh_zl = grid%rh_zl                                   &
                      ,ght_zl= grid%ght_zl                                  &
                      ,s_zl  = grid%s_zl                                    &
                      ,td_zl = grid%td_zl                                   &
                      ,q_zl = grid%q_zl                                     &
                      ,p_zl = grid%p_zl                                     &
               !  Dimension arguments
                      ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde    &
                      ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme    &
                      ,ITS=grid%i_start(ij),ITE=grid%i_end(ij)              &
                      ,JTS=grid%j_start(ij),JTE=grid%j_end(ij)              &
                      ,KTS=k_start,KTE=k_end+1                              )
            END DO
            !$OMP END PARALLEL DO
         END IF TIME_TO_DO_ZL_DIAGS
      END IF ZL_DIAGNOSTICS




      !  AFWA diagnostic package.

      AFWA_DIAGS : IF ( config_flags%afwa_diag_opt == 1 ) THEN

         IF ( ( config_flags%history_interval == 0 ) ) THEN
            WRITE (diag_message , * ) &
            "AFWA Diagnostics Error : No 'history_interval' defined in namelist"
            CALL wrf_error_fatal ( diag_message )
         END IF

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles

            CALL wrf_debug ( 100 , '--> CALL DIAGNOSTICS PACKAGE: AFWA DIAGNOSTICS' )

            CALL afwa_diagnostics_driver (   grid , config_flags              &
                         ,moist                                               &
                         ,scalar                                              &
                         ,chem                                                &
                         ,th_phy , pi_phy , p_phy                             &
                         ,grid%u_phy , grid%v_phy                             &
                         ,dz8w , p8w , t8w , grid%rho                         &
                         ,ids, ide, jds, jde, kds, kde                        &
                         ,ims, ime, jms, jme, kms, kme                        &
                         ,ips, ipe, jps, jpe, kps, kpe                        &
                         ,ITS=grid%i_start(ij),ITE=grid%i_end(ij)             &
                         ,JTS=grid%j_start(ij),JTE=grid%j_end(ij)             &
                         ,K_START=k_start,K_END=k_end                         )

            END DO
            !$OMP END PARALLEL DO
      ENDIF AFWA_DIAGS




      !  RASM Climate Diagnostics - mean output

      RASM_DIAGS_MEAN : IF ( config_flags%mean_diag == 1 ) THEN

         !IF ( ( config_flags%auxhist3_interval == 0 ) ) THEN
         !   WRITE (diag_message , * ) &
         !   "CLWRF: ERROR -- error -- ERROR -- error : NO 'auxhist3_interval' has been defined in 'namelist.input'"
         !   CALL wrf_error_fatal ( diag_message )
         !END IF

         CALL wrf_debug ( 100 , '--> CALL DIAGNOSTICS PACKAGE: RASM DIAGNOSTICS - MEAN' )

         CALL domain_clock_get ( grid, current_time=currentTime)

         CALL mean_output_calc(                                               &
                        is_restart=config_flags%restart                       &
                       ,CURRENTTIME=currentTime                               &
                       ,stats_interval=config_flags%mean_interval             &
                       ,output_freq=config_flags%mean_freq                    &
                       ,run_days=config_flags%run_days                        &
                       ,DT=grid%dt, XTIME=grid%xtime                          &
                       ,PSFC=grid%psfc, PSFC_MEAN=grid%psfc_mean              &
                       ,TSK=grid%tsk, TSK_MEAN=grid%tsk_mean                  &
                       ,PMSL_MEAN=grid%pmsl_mean                              &
                       ,T2=grid%t2, T2_MEAN=grid%t2_mean                      &
                       ,T=grid%th_phy_m_t0, P=grid%p, PB=grid%pb              &
                       ,MOIST=grid%moist(:,:,:,P_QV), HT=grid%ht              &
                       ,TH2=grid%th2, TH2_MEAN=grid%th2_mean                  &
                       ,Q2=grid%q2, Q2_MEAN=grid%q2_mean                      &
                       ,U10=grid%u10, U10_MEAN=grid%u10_mean                  &
                       ,V10=grid%v10, V10_MEAN=grid%v10_mean                  &           
                       ,HFX=grid%hfx, HFX_MEAN=grid%hfx_mean                  &
                       ,LH=grid%lh, LH_MEAN=grid%lh_mean                      &
                       ,SWDNB=grid%swdnb, SWDNB_MEAN=grid%swdnb_mean          &
                       ,GLW=grid%glw , GLW_MEAN=grid%glw_mean                 &
                       ,LWUPB=grid%lwupb, LWUPB_MEAN=grid%lwupb_mean          &
                       ,SWUPB=grid%swupb, SWUPB_MEAN=grid%swupb_mean          &
                       ,SWUPT=grid%swupt, SWUPT_MEAN=grid%swupt_mean          &
                       ,SWDNT=grid%swdnt, SWDNT_MEAN=grid%swdnt_mean          &
                       ,LWUPT=grid%lwupt, LWUPT_MEAN=grid%lwupt_mean          &
                       ,LWDNT=grid%lwdnt, LWDNT_MEAN=grid%lwdnt_mean          &
                       ,AVGOUTALARM=grid%alarms(AUXHIST5_ALARM)               &
                       ,AVGOUTDATESTR=grid%OUTDATE_MEAN                       &
                       ,NSTEPS=grid%NSTEPS_MEAN                               &
         ! Dimension arguments
                       ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde     &
                       ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme     &
                       ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe     &
                       ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)     &
                       ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)     &
                       ,NUM_TILES=grid%num_tiles                              &
                                                                      )
      END IF RASM_DIAGS_MEAN




      !  RASM Climate Diagnostics - diurnal output

      RASM_DIAGS_DIURNAL : IF ( config_flags%diurnal_diag == 1 ) THEN

         !IF ( ( config_flags%auxhist3_interval == 0 ) ) THEN
         !   WRITE (diag_message , * ) &
         !   "CLWRF: ERROR -- error -- ERROR -- error : NO 'auxhist3_interval' has been defined in 'namelist.input'"
         !   CALL wrf_error_fatal ( diag_message )
         !END IF

         CALL wrf_debug ( 100 , '--> CALL DIAGNOSTICS PACKAGE: RASM DIAGNOSTICS - DIURNAL' )

         CALL domain_clock_get ( grid, current_time=currentTime)

         CALL diurnalcycle_output_calc(                                       &
            is_restart=config_flags%restart                                   &
           ,CURRENTTIME=currentTime                                           &
           ,DT=grid%dt, XTIME=grid%xtime                                      &
           ,PSFC=grid%psfc, PSFC_DTMP=grid%psfc_dtmp                          &
           ,TSK=grid%tsk, TSK_DTMP=grid%tsk_dtmp                              &
           ,T2=grid%t2, T2_DTMP=grid%t2_dtmp                                  &
           ,T=grid%th_phy_m_t0, P=grid%p, PB=grid%pb                          &
           ,MOIST=grid%moist(:,:,:,P_QV)                                     &
           ,TH2=grid%th2, TH2_DTMP=grid%th2_dtmp                              &
           ,Q2=grid%q2, Q2_DTMP=grid%q2_dtmp                                  &
           ,U10=grid%u10, U10_DTMP=grid%u10_dtmp                              &
           ,V10=grid%v10, V10_DTMP=grid%v10_dtmp                              &
           ,HFX=grid%hfx, HFX_DTMP=grid%hfx_dtmp                              &
           ,LH=grid%lh, LH_DTMP=grid%lh_dtmp                                  &
           ,SWDNB=grid%swdnb, SWDNB_DTMP=grid%swdnb_dtmp                      &
           ,GLW=grid%glw, GLW_DTMP=grid%glw_dtmp                              &
           ,LWUPB=grid%lwupb, LWUPB_DTMP=grid%lwupb_dtmp                      &
           ,SWUPB=grid%swupb, SWUPB_DTMP=grid%swupb_dtmp                      &
           ,SWUPT=grid%swupt, SWUPT_DTMP=grid%swupt_dtmp                      &
           ,SWDNT=grid%swdnt, SWDNT_DTMP=grid%swdnt_dtmp                      &
           ,LWUPT=grid%lwupt, LWUPT_DTMP=grid%lwupt_dtmp                      &
           ,LWDNT=grid%lwdnt, LWDNT_DTMP=grid%lwdnt_dtmp                      &
           ,AVGOUTALARM=grid%alarms(AUXHIST6_ALARM)                           &
           ,DIURNOUTDATESTR=grid%OUTDATE_DIURN                                &
           ,AVG_NSTEPS=grid%NSTEPSMEAN_DIURN                                  &
           ,DIURNAL_NSTEPS=grid%NSTEPS_DIURN                                  &
           ,PSFC_DIURN=grid%PSFC_DIURN                                        &
           ,TSK_DIURN=grid%TSK_DIURN, T2_DIURN=grid%T2_DIURN                  &
           ,TH2_DIURN=grid%TH2_DIURN, Q2_DIURN=grid%Q2_DIURN                  &
           ,U10_DIURN=grid%U10_DIURN, V10_DIURN=grid%V10_DIURN                &
           ,HFX_DIURN=grid%HFX_DIURN, LH_DIURN=grid%LH_DIURN                  &
           ,SWDNB_DIURN=grid%SWDNB_DIURN, GLW_DIURN=grid%GLW_DIURN            &
           ,LWUPB_DIURN=grid%LWUPB_DIURN, SWUPB_DIURN=grid%SWUPB_DIURN        &
           ,SWUPT_DIURN=grid%SWUPT_DIURN, SWDNT_DIURN=grid%SWDNT_DIURN        &
           ,LWUPT_DIURN=grid%LWUPT_DIURN, LWDNT_DIURN=grid%LWDNT_DIURN        &
         ! Dimension arguments
           ,IDS=ids, IDE=ide, JDS=jds, JDE=jde, KDS=kds, KDE=kde              &
           ,IMS=ims, IME=ime, JMS=jms, JME=jme, KMS=kms, KME=kme              & 
           ,IPS=ips, IPE=ipe, JPS=jps, JPE=jpe, KPS=kps, KPE=kpe              &         
           ,I_START=grid%i_start, I_END=min(grid%i_end, ide-1)                &
           ,J_START=grid%j_start, J_END=min(grid%j_end, jde-1)                &
           ,NUM_TILES=grid%num_tiles                                          &
                                                                   )
      END IF RASM_DIAGS_DIURNAL


      !  Noah-MP accumulation output

      NOAHMP_DIAGS : IF ( config_flags%sf_surface_physics == 4 .and. config_flags%noahmp_output == 3  ) THEN

         CALL wrf_debug ( 100 , '--> CALL DIAGNOSTICS PACKAGE: NOAHMP_DIAGS' )

         CALL domain_clock_get ( grid, current_time=currentTime)

         CALL noahmp_output_calc(                                          &
           ims=ims, ime=ime, jms=jms, jme=jme                             ,&
           i_start=grid%i_start, i_end=min(grid%i_end, ide-1)             ,&
           j_start=grid%j_start, j_end=min(grid%j_end, jde-1)             ,&
           num_tiles=grid%num_tiles                                       ,&
           dt=grid%dt                                                     ,&
           noahmp_acc_dt=config_flags%noahmp_acc_dt                       ,&
           opt_run=config_flags%opt_run                                   ,&
           stepwtd=grid%stepwtd                                           ,&
           itimestep=grid%itimestep                                       ,&
           curr_secs2=curr_secs2                                          ,&
           acsagb=grid%acsagb    ,  acsagv=grid%acsagv  , sag=grid%sagxy  ,&
            acirg=grid%acirg     ,     irg=grid%irgxy                     ,&
            acshg=grid%acshg     ,     shg=grid%shgxy                     ,&
            acevg=grid%acevg     ,     evg=grid%evgxy                     ,&
            acghv=grid%acghv     ,     ghv=grid%ghvxy                     ,&
           acpahg=grid%acpahg    ,    pahg=grid%pahgxy                    ,&
            acirb=grid%acirb     ,     irb=grid%irbxy                     ,&
            acshb=grid%acshb     ,     shb=grid%shbxy                     ,&
            acevb=grid%acevb     ,     evb=grid%evbxy                     ,&
            acghb=grid%acghb     ,     ghb=grid%ghbxy                     ,&
           acpahb=grid%acpahb    ,    pahb=grid%pahbxy                    ,&
            acsav=grid%acsav     ,     sav=grid%savxy                     ,&
            acirc=grid%acirc     ,   ircmp=grid%ircxy                     ,&
            acshc=grid%acshc     ,     shc=grid%shcxy                     ,&
            acevc=grid%acevc     ,     evc=grid%evcxy                     ,&
             actr=grid%actr      ,      tr=grid%trxy                      ,&
           acpahv=grid%acpahv    ,    pahv=grid%pahvxy                    ,&
        acswdnlsm=grid%acswdnlsm ,  swdown=grid%swnorm                    ,&
        acswuplsm=grid%acswuplsm ,  albedo=grid%albedo                    ,&
        aclwdnlsm=grid%aclwdnlsm ,   emiss=grid%emiss   , glw=grid%glw    ,&
        aclwuplsm=grid%aclwuplsm ,    trad=grid%tradxy                    ,&
        acshflsm=grid%acshflsm   ,aclhflsm=grid%aclhflsm                  ,&
        acghflsm=grid%acghflsm   ,acpahlsm=grid%acpahlsm                  ,&
           acints=grid%acints    ,   qints=grid%qintsxy                   ,&
           acintr=grid%acintr    ,   qintr=grid%qintrxy                   ,&
          acdrips=grid%acdrips   ,  qdrips=grid%qdripsxy                  ,&
          acdripr=grid%acdripr   ,  qdripr=grid%qdriprxy                  ,&
          acthros=grid%acthros   ,  qthros=grid%qthrosxy                  ,&
          acthror=grid%acthror   ,  qthror=grid%qthrorxy                  ,&
          acsnsub=grid%acsnsub   ,  qsnsub=grid%qsnsubxy                  ,&
          acsnfro=grid%acsnfro   ,  qsnfro=grid%qsnfroxy                  ,&
           acsubc=grid%acsubc    ,   qsubc=grid%qsubcxy                   ,&
           acfroc=grid%acfroc    ,   qfroc=grid%qfrocxy                   ,&
           acevac=grid%acevac    ,   qevac=grid%qevacxy                   ,&
           acdewc=grid%acdewc    ,   qdewc=grid%qdewcxy                   ,&
           acfrzc=grid%acfrzc    ,   qfrzc=grid%qfrzcxy                   ,&
          acmeltc=grid%acmeltc   ,  qmeltc=grid%qmeltcxy                  ,&
          acsnbot=grid%acsnbot   ,  qsnbot=grid%qsnbotxy                  ,&
         acsnmelt=grid%acsnmelt  ,   qmelt=grid%qmeltxy                   ,&
        acponding=grid%acponding , ponding=grid%pondingxy                 ,&
        acrainlsm=grid%acrainlsm , rainlsm=grid%rainlsm                   ,&
        acsnowlsm=grid%acsnowlsm , snowlsm=grid%snowlsm                   ,&
       acrainsnow=grid%acrainsnow,   isnow=grid%isnowxy                   ,&
          acrunsb=grid%acrunsb   ,   runsb=grid%runsbxy                   ,&
          acrunsf=grid%acrunsf   ,   runsf=grid%runsfxy                   ,&
           acecan=grid%acecan    ,    ecan=grid%ecanxy                    ,&
          acetran=grid%acetran   ,   etran=grid%etranxy                   ,&
           acedir=grid%acedir    ,    edir=grid%edirxy                    ,&
          acetlsm=grid%acetlsm                                            ,&
           acqlat=grid%acqlat    ,    qlat=grid%qlatxy                    ,&
            acqrf=grid%acqrf     ,     qrf=grid%qrfxy                     ,&
           acrech=grid%acrech    ,deeprech=grid%deeprechxy                ,&
        acqspring=grid%acqspring , qspring=grid%qspringxy                 ,&
            fpice=grid%fpicexy   ,    fveg=grid%fvegxy                    ,&
          aceflxb=grid%aceflxb   ,   eflxb=grid%eflxbxy                   ,&
          accanhs=grid%accanhs   ,   canhs=grid%canhsxy                   )

      END IF NOAHMP_DIAGS


      SOLAR_FIELDS: IF (config_flags%solar_diagnostics == do_solar_output) THEN
          CALL wrf_debug (100 , '--> CALL DIAGNOSTICS PACKAGE: SOLAR_DIAG')

          CALL solar_diag (dt=grid%dt, &
              rho=grid%rho, dz8w=dz8w, ph=grid%ph_2, phb=grid%phb,          &
              cldfrac3d=grid%cldfra, coszen=grid%coszen, swdnb=grid%swdnb,  &
              swdnt=grid%swdnt, param_first_scalar=param_first_scalar,      &
              p_qc=p_qc, p_qi=p_qi, p_qs=p_qs, qv=moist(ims,kms,jms,P_QV),  &
              qc=moist(ims,kms,jms,P_QC), qi=moist(ims,kms,jms,P_QI),       &
              qs=moist(ims,kms,jms,P_QS), qc_tot=grid%qc_tot,               &
              qi_tot=grid%qi_tot,                                           &
              has_reqc=grid%has_reqc, has_reqi=grid%has_reqi,               &
              has_reqs=grid%has_reqs, f_qv=f_qv, f_qc=f_qc, f_qi=f_qi,      &
              f_qs=f_qs, re_cloud=grid%re_cloud, re_ice=grid%re_ice,        &
              re_snow=grid%re_snow, cldfrac2d=grid%cldfrac2d,               &
              wvp2d=grid%wvp, lwp2d=grid%lwp, iwp2d=grid%iwp,               &
              swp2d=grid%swp, wp2d_sum=grid%wp_sum, lwp2d_tot=grid%lwp_tot, &
              iwp2d_tot=grid%iwp_tot, wp2d_tot_sum=grid%wp_tot_sum,         &
              re_cloud_path=grid%re_qc, re_ice_path=grid%re_qi,             &
              re_snow_path=grid%re_qs, re_cloud_path_tot=grid%re_qc_tot,    &
              re_ice_path_tot=grid%re_qi_tot, tau_qc=grid%tau_qc,           &
              tau_qi=grid%tau_qi, tau_qs=grid%tau_qs,                       &
              tau_qc_tot=grid%tau_qc_tot, tau_qi_tot=grid%tau_qi_tot,       &
              cbase=grid%cbaseht, ctop=grid%ctopht,                         &
              cbase_tot=grid%cbaseht_tot, ctop_tot=grid%ctopht_tot,         &
              clrnidx=grid%clrnidx, sza=grid%sza, ghi_accum=grid%ghi_accum, &
              ids=ids, ide=ide, jds=jds, jde=jde, kds=kds, kde=kde,         &
              ims=ims, ime=ime, jms=jms, jme=jme, kms=kms, kme=kme,         &
              ips=ips, ipe=ipe, jps=jps, jpe=jpe, kps=kps, kpe=kpe,         &
              i_start=grid%i_start, i_end=min(grid%i_end, ide-1),           &
              j_start=grid%j_start, j_end=min(grid%j_end, jde-1),           &
              kts=k_start, kte=min(k_end,kde-1), num_tiles=grid%num_tiles   )
      END IF SOLAR_FIELDS

#ifdef CORDEXDIAG
! CORDEX diagnostics. CIMA Dec.17

      !  CORDEX-oriented diagnostic quantities.

      CORDEX_DIAGS : IF ( config_flags%output_cordex == 1 ) THEN

         IF ( ( config_flags%auxhist9_interval == 0 ) ) THEN
            WRITE (diag_message , * ) &
            "CORDEX: ERROR -- error -- ERROR -- error : NO 'auxhist9_interval' has been defined in 'namelist.input'"
            CALL wrf_error_fatal ( diag_message )
         END IF

         CALL wrf_debug ( 100 , '--> CALL DIAGNOSTICS PACKAGE: CORDEX DIAGNOSTICS' )
         ! Do not forget, CDXWRF initialization in phys/module_physics_init.F at SUBROUTINE cdxwrf_init
         
         ! From AFWA diagnostics ...
         CALL WRFU_ALARMGET( grid%alarms( HISTORY_ALARM ), prevringtime=hist_time,                    &
           ringinterval=histint)
         CALL WRFU_ALARMGET( grid%alarms(AUXHIST9_ALARM), prevringtime=aux9_time, ringinterval=aux9int)
         CALL domain_clock_get ( grid, current_time=CurrTime, simulationStartTime=StartTime,          &
           current_timestr=timestr, time_step=dtint )
         !is_output_t = ( Currtime .GE. hist_time + histint - dtint .OR.                               & 
         !  Currtime .ge. aux9_time + aux9int - dtint )
         is_output_t = ( Currtime .ge. aux9_time + aux9int - dtint )
         ! Complementary way...
         !is_output_t=(MOD(NINT((curr_secs2+grid%dt)/grid%dt),NINT(config_flags%auxhist9_interval*60./grid%dt))==0 .OR. curr_secs2==0.)

         IF (is_output_t) THEN
           grid%cdxlon = grid%xlong
           grid%cdxlat = grid%xlat
           grid%cdxps = grid%psfc
           grid%cdxts = grid%tsk
#ifdef CDXWRF
#if CDXWRF>=4
      !! INSTVALS: Instantaneous values (uncomment and re-compile)
!           grid%cdxdiabh = grid%diab_h
#endif
#endif
         END IF
         
         CALL cordex_output_calc(                                          &
                     is_restart=config_flags%restart                       &
                    ,is_output_time = is_output_t                          &
                    ,cdxH=config_flags%auxhist9_interval                   &
!                    ,DT=grid%dt                                            &
                    ,DT=REAL(model_config_rec%dt(grid%id))                 &
                    ,XTIME=grid%xtime,CURR_SECS2=curr_secs2                &
                    ,RADT=config_flags%radt                                &
                    ,PSL_DIAG=config_flags%psl_diag                        &
                    ,WSGS_DIAG=config_flags%wsgs_diag                      &
                    ,PTARGET=config_flags%ptarget                          &
                    ,PSMOOTH=config_flags%psmooth                          &
                    ,WBOUT = config_flags%output_wb                        &
                    ,WSZ100_DIAG=config_flags%wsz100_diag                  &
                    ,ZHGTZW = config_flags%z100m_wind                      &
                    ,Z50_DIAG=config_flags%z50m                            &
                    ,DQV = config_flags%zmlagen_dqv                        &
                    ,DTHETA = config_flags%zmlagen_dtheta                  &
                    ,POTEVAP_DIAG = config_flags%potevp_diag               &
                    ,CONVXTRM_DIAG = config_flags%convxtrm_diag            &
                    ,VISIBILITY_DIAG = config_flags%fogvisibility_diag     &
                    ,FOGVARS = config_flags%fogvars                        &
                    ,ZMLAGEN_DIAG = config_flags%zmlagen_diag              &
                    ,TIMEDBG = config_flags%timedbg                        &
                    ,LONDBG = config_flags%londbg                          &
                    ,LATDBG = config_flags%latdbg                          &
                    ,IDBG=grid%ijdbg(1), JDBG=grid%ijdbg(2)                &
                    ,WGSTS_WBDSWSSTRES = config_flags%wgsts_wbdswsstres    &
#ifdef CDXWRF
#if CDXWRF>=3
                    ,NHTASRNG = config_flags%nhtasrng                      &
                    ,NHHURSRNG = config_flags%nhhursrng                    &
                    ,NLTASRNG = config_flags%nltasrng                      &
                    ,NLHURSRNG = config_flags%nlhursrng                    &
                    ,NWBDSRNG = config_flags%nwbdsrng                      &
                    ,NWSSRNG = config_flags%nwssrng                        &
                    ,NHTASHURSRNG = config_flags%nhtashursrng              &
                    ,NLTASHURSRNG = config_flags%nltashursrng              &
                    ,NWBDSWSSRNG = config_flags%nwbdswssrng                &
! Should I keep it this?
!                    ,TASHRNG = grid%htasbins(1:model_config_rec%nhtasrng-1)    &
!                    ,HURSHRNG = grid%hhursbins(1:model_config_rec%nhhursrng-1) &
!                    ,TASLRNG = grid%ltasbins(1:model_config_rec%nltasrng-1)    &
!                    ,HURSLRNG = grid%lhursbins(1:model_config_rec%nlhursrng-1) &
                    ,TASHRNG = model_config_rec%htasrng(1:model_config_rec%nhtasrng)                &
                    ,HURSHRNG = model_config_rec%hhursrng(1:model_config_rec%nhhursrng)             &
                    ,TASLRNG = model_config_rec%ltasrng(1:model_config_rec%nltasrng)                &
                    ,HURSLRNG = model_config_rec%lhursrng(1:model_config_rec%nlhursrng)             &
                    ,WBDSRNG = model_config_rec%wbdsrng(1:model_config_rec%nwbdsrng)                &
                    ,WSSRNG = model_config_rec%wssrng(1:model_config_rec%nwssrng)                   &
#endif
#endif

         ! Input
                    ,LON=grid%xlong, LAT=grid%xlat                         &
                    ,CLDFRA=grid%cldfra, p8w=p8w, P=grid%p, PB=grid%pb     &
                    ,PCLT=grid%pclt                                        &
                    ,PCLL=grid%pcll, PCLM=grid%pclm, PCLH=grid%pclh        &
                    ,QV=moist(:,:,:,P_QV), QC=moist(:,:,:,P_QC)            &
                    ,QR=moist(:,:,:,P_QR), QS=moist(:,:,:,P_QS)            &
                    ,QI=moist(:,:,:,P_QI), QG=moist(:,:,:,P_QG)            &
                    ,QH=moist(:,:,:,P_QH)                                  &
                    ,MU=grid%mu_2, MUB=grid%mub, DETA=-grid%dnw            &
                    ,SINA=grid%sina, COSA=grid%cosa                        &
! Which one should be used?                    
!                    ,U=grid%u_2, V=grid%v_2, T=grid%t_2                    &
! t_phy already as temperature!! not potential temperature!
                    ,U=grid%u_phy, V=grid%v_phy, T=grid%t_phy              &
                    ,THp=grid%th_phy_m_t0                                  &
                    ,PH=grid%ph_2, PHB=grid%phb, P_PHY=p_phy               &
                    ,U10=grid%u10, V10=grid%v10                            &
                    ,Q2=grid%q2, T2=grid%t2, TH2=grid%th2                  &
                    ,PS=grid%psfc, QSFC=grid%qsfc                          &
                    ,HGT=grid%ht, Z=grid%z                                 &
                    ,DZS=grid%dzs, SMOIS=grid%smois, SH2O=grid%sh2o        &
                    ,TKE=grid%tke_pbl, PBLH=grid%pblh                      &
                    ,PRC=grid%raincv, PRNC=grid%rainncv, PRSH=grid%rainshv &
                    ,UST=grid%ust, ZNT=grid%znt, ROL=grid%rmol             &
                    ,SWDOWN=grid%swdown                                    &
                    ,CD=grid%cd                                            &
                    ,GLW=grid%glw, LH=grid%lh, HFX=grid%hfx                &
                    ,SWUPB=grid%swupb, LWUPB=grid%lwupb, QFX=grid%qfx      &
                    ,SNOW=grid%snow, SNOWH=grid%snowh, SNOWC=grid%snowc    &
                    ,SFCRUNOFF=grid%sfcrunoff, UDRUNOFF=grid%udrunoff      &
                    ,RHO=grid%rho                                          &
                    ,SWDNT=grid%swdnt, SWUPT=grid%swupt, LWUPT=grid%lwupt  &
                    ,TS=grid%tsk, ALBEDO=grid%albedo                       &
                    ,EMISS=grid%emiss                                      &
                    ,SR=grid%sr                                            &
                    ! we are going to assume that drag coefficients for momentum and heat are the same 
                    ! it is scheme dependent, but at his level we are going simple
                    ! From Registry/Regisyty,EM_COMMON
                    !   "ch"                "surface exchange coeff for heat"    "m s-1"
                    ! We need to transform from surface exchange coefficients to drag coefficients for 
                    !   the tas_hm, qvs_hm diagnostics
                    !      QCDRAGH = CHS/wspd
                    ! WRF does not provide the surface exchange coefficient of momentum (not for all 
                    !   sfclay schemes), therefore the generic one (already in CDXWRF as cdgen) will 
                    !   be used as
                    !      CDM = (ust/wss)**2
                    ,CHS=grid%ch, CDM=grid%cd, BR=grid%br, ZOL=grid%zol    &
                    ,FRACSOIL10CM=grid%fracsoil10cm                        &
#ifdef CDXWRF
#if CDXWRF>=1

#elif CDXWRF>=4
                    ,DIABH=grid%h_diabatic                                 &
                    ,QVTTEND=grid%qvttend, QCTTEND=grid%qcttend            & 
                    ,QRTTEND=grid%qrttend, QSTTEND=grid%qsttend            &
                    ,QITTEND=grid%qittend, QGTTEND=grid%qgttend            &
                    ,QHTTEND=grid%qhttend                                  &
                    ,QVHADV=grid%qv_hadv, QCHADV=grid%qc_hadv              &
                    ,QRHADV=grid%qr_hadv, QSHADV=grid%qs_hadv              &
                    ,QIHADV=grid%qi_hadv, QGHADV=grid%qg_hadv              &
                    ,QHHADV=grid%qh_hadv                                   &
                    ,QVZADV=grid%qv_zadv, QCZADV=grid%qc_zadv              &
                    ,QRZADV=grid%qr_zadv, QSZADV=grid%qs_zadv              &
                    ,QIZADV=grid%qi_zadv, QGZADV=grid%qg_zadv              &
                    ,QHZADV=grid%qh_zadv                                   &
#endif
#endif
         ! Output
                    ,NSTEPS=grid%cdxnsteps                                 &
                    ,CLTMEAN=grid%cltmean, CLLMEAN=grid%cllmean            &
                    ,CLMMEAN=grid%clmmean, CLHMEAN=grid%clhmean            &
                    ,PRW=grid%prw, CLWVI=grid%clwvi, CLIVI=grid%clivi      &
                    ,UAS=grid%uas, VAS=grid%vas                            &
                    ,WSS=grid%wss, WBDS=grid%wbds                          &
                    ,HURS=grid%hurs ,HUSS=grid%huss                        &
                    ,PSL=grid%psl                                          &
                    ,WSGSMAX=grid%wsgsmax                                  & 
                    ,UGSMAX=grid%ugsmax, VGSMAX=grid%vgsmax                &
                    ,PERCENGUST=grid%wsgspercen                            &
                    ,TOTWSGSMAX=grid%totwsgsmax                            & 
                    ,TOTUGSMAX=grid%totugsmax, TOTVGSMAX=grid%totvgsmax    &
                    ,TOTPERCENGUST=grid%totwsgspercen                      &
                    ,WSZ100=grid%wsz100                                    & 
                    ,UZ100WIND=grid%uz100, VZ100WIND=grid%vz100            &
                    ,WSZ50=grid%wsz50                                      & 
                    ,UZ50WIND=grid%uz50, VZ50WIND=grid%vz50                &
                    ,TAZ50=grid%taz50, QVZ50=grid%qvz50                    &
                    ,WSZ100MAX=grid%wsz100max                              & 
                    ,UZ100MAX=grid%uz100max, VZ100MAX=grid%vz100max        &
                    ,SUNSHINE=grid%sund                                    &
                    ,TAUU=grid%tauu, TAUV=grid%tauv                        &
                    ,TAUUGEN=grid%tauugen, TAUVGEN=grid%tauvgen            &
                    ,RSDS=grid%rsds, RLDS=grid%rlds, HFLS=grid%hfls        &
                    ,HFSS=grid%hfss, RSUS=grid%rsus, RLUS=grid%rlus        &
                    ,RSUSGEN=grid%rsusgen, RLUSGEN=grid%rlusgen            &
                    ,EVSPSBL=grid%evspsbl, EVSPSBLPOT=grid%evspsblpot      &
                    ,EVSPSBLPOTGEN=grid%evspsblpotgen                      &
                    ,CDCDX=grid%cdcdx, CDGEN=grid%cdgen                    &
                    ,SNC=grid%snc, SND=grid%snd                            &
                    ,PMRROS=grid%pmrros, PMRRO=grid%pmrro                  &
                    ,MRROS=grid%mrros, MRRO=grid%mrro, MRSO=grid%mrso      &
                    ,MRSOL=grid%mrsol, MRSOS=grid%mrsos                    &
                    ,MRSLL=grid%mrsll, MRLSO=grid%mrlso,MRLSOS=grid%mrlsos &
                    ,MRSFL=grid%mrsfl, MRFSO=grid%mrfso,MRFSOS=grid%mrfsos &
                    ,MRSOMEAN=grid%mrsomean, MRSOLMEAN=grid%mrsolmean      &
                    ,MRSOSMEAN=grid%mrsosmean                              &
                    ,MRSLLMEAN=grid%mrsllmean, MRLSOMEAN=grid%mrlsomean    &
                    ,MRLSOSMEAN=grid%mrlsosmean                            &
                    ,MRSFLMEAN=grid%mrsflmean, MRFSOMEAN=grid%mrfsomean    &
                    ,MRFSOSMEAN=grid%mrfsosmean                            &
                    ,FLXPR=grid%pr, FLXPRL=grid%prl,FLXPRC=grid%prc        &
                    ,FLXPRSH=grid%prsh                                     &
                    ,FLXPRSN=grid%prsn                                     &
                    ,SNW=grid%snw                                          &
                    ,RSDT=grid%rsdt, RSUT=grid%rsut, RLUT=grid%rlut        &
                    ,TAS_HM=grid%tas_hm, QVS_HM=grid%qvs_hm                &
                    ,COLMAX=grid%colmax, HFPRFLX=grid%prhf                 &

#ifdef CDXWRF
#if CDXWRF>=1
                    ,CLGVI=grid%clgvi, CLHVI=grid%clhvi                    &
                    ,ZMLA=grid%zmlagen                                     &
                    ,CAPEMIN=grid%capemin, CINMIN=grid%cinmin              &
                    ,ZLFCMIN=grid%lfczmin, PLFCMIN=grid%lfcpmin            &
                    ,LIDXMIN=grid%limin                                    &
                    ,CAPEMAX=grid%capemax, CINMAX=grid%cinmax              &
                    ,ZLFCMAX=grid%lfczmax, PLFCMAX=grid%lfcpmax            &
                    ,CAPEMEAN=grid%capemean, CINMEAN=grid%cinmean          &
                    ,ZLFCMEAN=grid%lfczmean, PLFCMEAN=grid%lfcpmean        &
                    ,LIDXMAX=grid%limax                                    &
                    ,LIDXMEAN=grid%limean                                  &
                    ,IUTMEAN=grid%iutmean, IVTMEAN=grid%ivtmean            &
                    ,ZEROITH=grid%zeroisotherm                             &
#endif
#if CDXWRF>=2
                    ,UA=grid%ua, VA=grid%va, WS=grid%ws, TA=grid%ta        &
                    ,PRESS=grid%press, ZG=grid%zg                          &
                    ,HUR=grid%hur, HUS=grid%hus                            &
                    ,TFOG=grid%tfog, FOGVISBLTYMIN=grid%fogvisbltymin      &
                    ,FOGVISBLTYMAX=grid%fogvisbltymax                      &
                    ,FOGVISBLTYMEAN=grid%fogvisbltymean                    &
                    ,TDSMIN=grid%tdsmin, TDSMAX=grid%tdsmax                &
                    ,TDSMEAN=grid%tdsmean                                  &
                    ,TWSMIN=grid%twsmin, TWSMAX=grid%twsmax                &
                    ,TWSMEAN=grid%twsmean                                  &
#endif
#if CDXWRF>=4
                    !! Water-budget
                    ,WBACDH=grid%wbacdiabh                                 & 
                    ,WBACPW=grid%wbacpw, WBACPWC=grid%wbacpwc              &
                    ,WBACPWR=grid%wbacpwr, WBACPWS=grid%wbacpws            &
                    ,WBACPWI=grid%wbacpwi, WBACPWG=grid%wbacpwg            &
                    ,WBACPWH=grid%wbacpwh                                  &
                    ,WBACF=grid%wbacf, WBACFC=grid%wbacfc                  &
                    ,WBACFR=grid%wbacfr, WBACFS=grid%wbacfs                &
                    ,WBACFI=grid%wbacfi, WBACFG=grid%wbacfg                &
                    ,WBACFH=grid%wbacfh                                    &
                    ,WBACZ=grid%wbacz, WBACZC=grid%wbaczc                  &
                    ,WBACZR=grid%wbaczr, WBACZS=grid%wbaczs                &
                    ,WBACZI=grid%wbaczi, WBACZG=grid%wbaczg                &
                    ,WBACZH=grid%wbaczh                                    &
                    ! Low-medium-high
                    ,WBACDHL=grid%wbacdiabhl, WBACDHM=grid%wbacdiabhm      & 
                    ,WBACDHH=grid%wbacdiabhh                               &
                    ,WBACPWLV=grid%wbacpwlv, WBACPWMV=grid%wbacpwmv        &
                    ,WBACPWHV=grid%wbacpwhv, WBACFLV=grid%wbacflv          &
                    ,WBACFMV=grid%wbacfmv, WBACFHV=grid%wbacfhv            &
                    ,WBACZLV=grid%wbaczlv, WBACZMV=grid%wbaczmv            &
                    ,WBACZHV=grid%wbaczhv                                  &
                    ,WBACPWLC=grid%wbacpwlc, WBACPWMC=grid%wbacpwmc        &
                    ,WBACPWHC=grid%wbacpwhc, WBACFLC=grid%wbacflc          &
                    ,WBACFMC=grid%wbacfmc, WBACFHC=grid%wbacfhc            &
                    ,WBACZLC=grid%wbaczlc, WBACZMC=grid%wbaczmc            &
                    ,WBACZHC=grid%wbaczhc                                  &
                    ,WBACPWLR=grid%wbacpwlr, WBACPWMR=grid%wbacpwmr        &
                    ,WBACPWHR=grid%wbacpwhr, WBACFLR=grid%wbacflr          &
                    ,WBACFMR=grid%wbacfmr, WBACFHR=grid%wbacfhr            &
                    ,WBACZLR=grid%wbaczlr, WBACZMR=grid%wbaczmr            &
                    ,WBACZHR=grid%wbaczhr                                  &
                    ,WBACPWLS=grid%wbacpwls, WBACPWMS=grid%wbacpwms        &
                    ,WBACPWHS=grid%wbacpwhs, WBACFLS=grid%wbacfls          &
                    ,WBACFMS=grid%wbacfms, WBACFHS=grid%wbacfhs            &
                    ,WBACZLS=grid%wbaczls, WBACZMS=grid%wbaczms            &
                    ,WBACZHS=grid%wbaczhs                                  &
                    ,WBACPWLI=grid%wbacpwli, WBACPWMI=grid%wbacpwmi        &
                    ,WBACPWHI=grid%wbacpwhi, WBACFLI=grid%wbacfli          &
                    ,WBACFMI=grid%wbacfmi, WBACFHI=grid%wbacfhi            &
                    ,WBACZLI=grid%wbaczli, WBACZMI=grid%wbaczmi            &
                    ,WBACZHI=grid%wbaczhi                                  &
                    ,WBACPWLG=grid%wbacpwlg, WBACPWMG=grid%wbacpwmg        &
                    ,WBACPWHG=grid%wbacpwhg, WBACFLG=grid%wbacflg          &
                    ,WBACFMG=grid%wbacfmg, WBACFHG=grid%wbacfhg            &
                    ,WBACZLG=grid%wbaczlg, WBACZMG=grid%wbaczmg            &
                    ,WBACZHG=grid%wbaczhg                                  &
                    ,WBACPWLH=grid%wbacpwlh, WBACPWMH=grid%wbacpwmh        &
                    ,WBACPWHH=grid%wbacpwhh, WBACFLH=grid%wbacflh          &
                    ,WBACFMH=grid%wbacfmh, WBACFHH=grid%wbacfhh            &
                    ,WBACZLH=grid%wbaczlh, WBACZMH=grid%wbaczmh            &
                    ,WBACZHH=grid%wbaczhh                                  &
#endif
#if CDXWRF>=3
                    ,TASHURSTRESHIGHRES=grid%tashurstreshighres            &
                    ,TASHURSTRESLOWRES=grid%tashurstreslowres              &
                    ,WBDSWSSTRES=grid%wbdswsstres                          &
#endif
#endif
         ! Dimension arguments
                    ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde     &
                    ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme     &
                    ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe     &
                    ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)     &
                    ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)     &
                    ,KTS=k_start, KTE=k_end                                &
                    ,NUM_TILES=grid%num_tiles                              &
                    ,NUM_SOIL_LAYERS=grid%num_soil_layers                  &
                    ,N_MOIST=num_moist                                     &
                    ,M_QV=p_qv, M_QC=p_qc, M_QR=p_qr, M_QS=p_qs            &
                    ,M_QI=p_qi, M_QG=p_qg, M_QH=p_qh                       &
!! INSTVALS: Instantaneous values (uncomment and re-compile)
!                    ,CLT=grid%clt                                          &
!                    ,CLL=grid%cll, CLM=grid%clm, CLH=grid%clh              &
!                    ,WSGS=grid%wsgs, UGUSTWIND=grid%usgs                   &
!                    ,VGUSTWIND=grid%vsgs, GUSTPOINT=grid%gustpoint         &
!                    ,TOTWSGS=grid%totwsgs, TOTUGUSTWIND=grid%totusgs       &
!                    ,TOTVGUSTWIND=grid%totvsgs                             &
!                    ,TOTGUSTPOINT=grid%totgustpoint                        &
!                    ,POTEVAPO=grid%potevapo                                &
!                    ,POTEVAPOGEN=grid%potevapogen                          &
#ifdef CDXWRF
#if CDXWRF>=1
!                    ,CAPE=grid%cdxcape                                     &
!                    ,CIN=grid%cin, ZLFC=grid%lfcz                          &
!                    ,PLFC=grid%lfcp, LIDX=grid%li                          &
#endif
#if CDXWRF>=2
!                    ,FOG=grid%fog, FOGVISBLTY=grid%fogvisblty              & 
!                    ,TDS=grid%tds, TWS=grid%tws                            & 
#endif
#endif
!! End of INSTVALS
                  )
                  

!!
! High-frequency CORDEX diagnostics
!!
         ! Only to be computed if an interval is provided
         IF ( ( config_flags%auxhist8_interval /= 0 ) ) THEN

           CALL wrf_debug ( 100 , '--> CALL DIAGNOSTICS PACKAGE: High-Frequency CORDEX DIAGNOSTICS' )
         
           ! From AFWA diagnostics ...
           CALL WRFU_ALARMGET( grid%alarms( HISTORY_ALARM ), prevringtime=hist_time,                    &
             ringinterval=histint)
           CALL WRFU_ALARMGET( grid%alarms(AUXHIST8_ALARM), prevringtime=aux8_time, ringinterval=aux8int)
           CALL domain_clock_get ( grid, current_time=CurrTime, simulationStartTime=StartTime,          &
             current_timestr=timestr, time_step=dtint )
           is_output_t = ( Currtime .ge. aux8_time + aux8int - dtint )

           IF (is_output_t) THEN
             grid%cdxlon = grid%xlong
             grid%cdxlat = grid%xlat
           END IF
         
           CALL cordex_hf_output_calc(                                     &
                     is_restart=config_flags%restart                       &
                    ,is_output_time = is_output_t                          &
                    ,cdxH=config_flags%auxhist8_interval                   &
!                    ,DT=grid%dt                                            &
                    ,DT=REAL(model_config_rec%dt(grid%id))                 &
                    ,XTIME=grid%xtime,CURR_SECS2=curr_secs2                &

         ! Input
                    ,LON=grid%xlong, LAT=grid%xlat                         &
                    ,QR=moist(:,:,:,P_QR), QS=moist(:,:,:,P_QS)            &
                    ,QG=moist(:,:,:,P_QG)                                  &
                    ,T=grid%t_phy                                          &
                    ,PRC=grid%raincv, PRNC=grid%rainncv, PRSH=grid%rainshv &
                    ,RHO=grid%rho                                          &
         ! Output
                    ,nsteps=grid%cdxhfnsteps                               &
                    ,FLXPR=grid%prhf, COLMAX=grid%colmax                   &

          ! Dimension arguments
                    ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde     &
                    ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme     &
                    ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe     &
                    ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)     &
                    ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)     &
                    ,KTS=k_start, KTE=k_end                                &
                    ,NUM_TILES=grid%num_tiles                              &
                  )

         ! End if for auxhist8 value
         END IF

      END IF CORDEX_DIAGS

#endif

   END SUBROUTINE diagnostics_driver


   SUBROUTINE update_phys_fields ( grid, config_flags, moist,           &
                                   ids,  ide,  jds,  jde,  kds,  kde,   &
                                   ims,  ime,  jms,  jme,  kms,  kme,   &
                                   ips,  ipe,  jps,  jpe,  kps,  kpe    ) 

      USE module_domain, ONLY : domain
      USE module_configure, ONLY : grid_config_rec_type
      USE module_state_description, ONLY: num_moist, P_Qv
      USE module_model_constants, ONLY: g, R_v, R_d, Cp, T0, RCP

      TYPE ( domain ), INTENT(INOUT) :: grid

      !  We are not changing any of the namelist settings.

      TYPE ( grid_config_rec_type ), INTENT(IN) :: config_flags

      !  The 4d arrays are input only, no mods to them.

      REAL , DIMENSION(ims:ime,kms:kme,jms:jme,num_moist ) , INTENT(IN) :: moist

      !  Domain indices - no change to these puppies.

      INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe

      !  Local variables

      INTEGER :: i, j, k 

      !  Moist or dry theta

      IF ( config_flags%use_theta_m .EQ. 1 ) THEN
         DO j = jps, MIN(jde-1, jpe)
            DO k = kps, kde-1
               DO i = ips, MIN(ide-1, ipe)
                  grid%th_phy_m_t0(i,k,j) = (grid%t_2(i,k,j)+T0) / &
                                            (1.+(R_v/R_d)*moist(i,k,j,P_Qv)) - T0
               END  DO
            END  DO
         END  DO
      ELSE
         DO j = jps, MIN(jde-1, jpe)
            DO k = kps, kde-1
               DO i = ips, MIN(ide-1, ipe)
                  grid%th_phy_m_t0(i,k,j) =  grid%t_2(i,k,j)
               END  DO
            END  DO
         END  DO
      END IF
   END SUBROUTINE update_phys_fields

END MODULE module_diagnostics_driver
