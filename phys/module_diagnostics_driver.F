#if (NMM_CORE == 1)
MODULE module_diagnostics_driver
CONTAINS
   SUBROUTINE diagnostics_driver_stub
   END SUBROUTINE diagnostics_driver_stub
END MODULE module_diagnostics_driver
#else
!WRF:MODEL_LAYER:PHYSICS

MODULE module_diagnostics_driver

CONTAINS

   !  This subroutine is the driver for the diagnostics packages.


   SUBROUTINE diagnostics_driver ( grid, config_flags,                  &
                                   moist, chem, tracer, scalar,         &
                                   th_phy, pi_phy, p_phy, rho_phy,      & 
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
                                          P_QG, P_QH, P_QV, P_QC, P_QI, P_QS,          &
                                          P_QNG, P_QH, P_QNH, P_QR, P_QNR,             &
                                          F_QV, F_QC, F_QI, F_QS,                      &
                     KESSLERSCHEME, LINSCHEME, SBU_YLINSCHEME, WSM3SCHEME, WSM5SCHEME, &
                     WSM6SCHEME, ETAMPNEW, THOMPSON, THOMPSONAERO,                     &
                     MORR_TWO_MOMENT, GSFCGCESCHEME, WDM5SCHEME, WDM6SCHEME,           &
                     NSSL_2MOM, NSSL_2MOMCCN, NSSL_1MOM, NSSL_1MOMLFO,                 &
                     MILBRANDT2MOM , CAMMGMPSCHEME, FAST_KHAIN_LYNN_SHPUND, FULL_KHAIN_LYNN,  &
                     MORR_TM_AERO !TWG add    !,MILBRANDT3MOM, NSSL_3MOM, MORR_MILB_P3

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
      USE module_utility, ONLY : WRFU_Time 

      !=============================================================
      !  USE Association for the Diagnostic Packages
      !=============================================================
      
      USE module_lightning_driver, ONLY : lightning_driver      
      USE module_diag_misc, ONLY : diagnostic_output_calc
      USE module_diag_nwp, ONLY : diagnostic_output_nwp
      USE module_diag_cl, ONLY : clwrf_output_calc
      USE module_diag_pld, ONLY : pld
      USE module_diag_zld, ONLY : zld
      USE module_diag_afwa, ONLY : afwa_diagnostics_driver
      USE module_diag_rasm, ONLY : mean_output_calc, diurnalcycle_output_calc
      USE module_diag_hailcast, ONLY : hailcast_diagnostic_driver
      USE module_diag_solar, ONLY : solar_diag
      USE module_trad_fields, ONLY : trad_fields

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
                                                                           rho_phy , &
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

        CASE (LINSCHEME, WSM6SCHEME, WDM6SCHEME, GSFCGCESCHEME, NSSL_1MOMLFO)

      CALL diagnostic_output_nwp(                                   &
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

        CASE (MORR_TWO_MOMENT, MORR_TM_AERO)  ! TWG add

      CALL diagnostic_output_nwp(                                   &
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

        CASE (NSSL_1MOM)

      CALL diagnostic_output_nwp(                                   &
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
                ,QH_CURR=moist(ims,kms,jms,P_QH)                     &  !  gthompsn
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

        CASE (MILBRANDT2MOM, NSSL_2MOM, NSSL_2MOMCCN)

      CALL diagnostic_output_nwp(                                   &
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

!       CASE (NSSL_3MOM)

!       CASE (MILBRANDT3MOM)

!       CASE (MORR_MILB_P3)

!       CASE (CAMMGMPSCHEME)

!       CASE (FULL_KHAIN_LYNN)

!       CASE (FAST_KHAIN_LYNN_SHPUND)

!       CASE (WSM3SCHEME)

!       CASE (WSM5SCHEME)

        CASE DEFAULT

      CALL diagnostic_output_nwp(                                    &
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
                         ,dz8w , p8w , t8w , rho_phy, grid%rho                &
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


      SOLAR_FIELDS: IF (config_flags%solar_diagnostics == do_solar_output) THEN
          CALL wrf_debug (100 , '--> CALL DIAGNOSTICS PACKAGE: SOLAR_DIAG')
          CALL solar_diag (                                                 &
              rho=grid%rho, dz8w=dz8w, ph=grid%ph_2, phb=grid%phb,          &
              cldfrac3d=grid%cldfra, coszen=grid%coszen, swdnb=grid%swdnb,  &
              swdnt=grid%swdnt, qv=moist(ims,kms,jms,P_QV),                 &
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
              clrnidx=grid%clrnidx, sza=grid%sza,                           &
              ids=ids, ide=ide, jds=jds, jde=jde, kds=kds, kde=kde,         &
              ims=ims, ime=ime, jms=jms, jme=jme, kms=kms, kme=kme,         &
              ips=ips, ipe=ipe, jps=jps, jpe=jpe, kps=kps, kpe=kpe,         &
              i_start=grid%i_start, i_end=min(grid%i_end, ide-1),           &
              j_start=grid%j_start, j_end=min(grid%j_end, jde-1),           &
              kts=k_start, kte=min(k_end,kde-1), num_tiles=grid%num_tiles   )
      END IF SOLAR_FIELDS


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
#endif
