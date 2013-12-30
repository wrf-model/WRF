#if (NMM_CORE == 1)
MODULE module_diag_afwa
CONTAINS
   SUBROUTINE diag_afwa_stub
   END SUBROUTINE diag_afwa_stub
END MODULE module_diag_afwa
#else

!WRF:MEDIATION_LAYER:PHYSICS

MODULE module_diag_afwa

CONTAINS

  SUBROUTINE afwa_diagnostics_driver (   grid , config_flags     &
                             , moist                             &
                             , scalar                            &
                             , chem                              &
                             , th_phy , pi_phy , p_phy           &
                             , dz8w , p8w , t8w , rho_phy        &
                             , ids, ide, jds, jde, kds, kde      &
                             , ims, ime, jms, jme, kms, kme      &
                             , ips, ipe, jps, jpe, kps, kpe      &
                             , its, ite, jts, jte                &
                             , k_start, k_end               )

    !USE module_domain, ONLY : domain
    USE module_domain
    USE module_configure, ONLY : grid_config_rec_type, model_config_rec
    USE module_state_description
    USE module_model_constants
#ifdef DM_PARALLEL
    USE module_dm, ONLY: wrf_dm_sum_real, wrf_dm_maxval
#endif

    IMPLICIT NONE

    TYPE ( domain ), INTENT(INOUT) :: grid
    TYPE ( grid_config_rec_type ), INTENT(IN) :: config_flags

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe
    INTEGER             :: k_start , k_end, its, ite, jts, jte

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme , num_moist),    &
         INTENT(IN   ) ::                                moist

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme , num_scalar),    &
         INTENT(IN   ) ::                                scalar

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme , num_chem),     &
         INTENT(IN   ) ::                                 chem

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN   ) ::                               th_phy  &
                                              ,         pi_phy  &
                                              ,          p_phy  &
                                              ,           dz8w  &
                                              ,            p8w  &
                                              ,            t8w  &
                                              ,        rho_phy

    ! Local
    ! -----
    CHARACTER*256 :: message, timestr 
    INTEGER :: i,j,k
    INTEGER :: icing_opt
    REAL :: bdump
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) ::      qrain  &
                                              ,          qsnow  &
                                              ,          qgrpl  &
                                              ,          qvapr  &
                                              ,         qcloud  &
                                              ,           qice  &
                                              ,         ncloud  &
                                              ,             rh  &
                                              ,           ptot  &
                                              ,            z_e  &
                                              ,           zagl

    REAL, DIMENSION( ims:ime, jms:jme, 5 ) ::            dustc
    REAL, DIMENSION( ims:ime, jms:jme ) ::                rh2m  &
                                              ,        wind10m

    ! Timing
    TYPE(WRFU_Time) :: hist_time, aux2_time, CurrTime
    TYPE(WRFU_TimeInterval) :: dtint, histint, aux2int
    LOGICAL :: is_after_history_dump, is_output_timestep

    ! Chirp the routine name for debugging purposes
    ! ---------------------------------------------
    write ( message, * ) 'inside afwa_diagnostics_driver'
    CALL wrf_debug( 100 , message )

    ! Get timing info 
    ! Want to know if when the last history output was
    ! Check history and auxhist2 alarms to check last ring time and how often
    ! they are set to ring
    ! -----------------------------------------------------------------------
    CALL WRFU_ALARMGET( grid%alarms( HISTORY_ALARM ), prevringtime=hist_time, &
         ringinterval=histint)
    CALL WRFU_ALARMGET( grid%alarms( AUXHIST2_ALARM ), prevringtime=aux2_time, &
         ringinterval=aux2int)

    ! Get domain clock
    ! ----------------
    CALL domain_clock_get ( grid, current_time=CurrTime, &
         current_timestr=timestr, time_step=dtint )

    ! Set some booleans for use later
    ! Following uses an overloaded .lt.
    ! ---------------------------------
    is_after_history_dump = ( Currtime .lt. hist_time + dtint )

    ! Following uses an overloaded .ge.
    ! ---------------------------------
    is_output_timestep = (Currtime .ge. hist_time + histint - dtint .or. &
                         Currtime .ge. aux2_time + aux2int - dtint )
    write ( message, * ) 'is output timestep? ', is_output_timestep
    CALL wrf_debug( 100 , message )
        
    ! 3-D arrays for moisture variables
    ! ---------------------------------
    DO i=ims, ime
      DO k=kms, kme
        DO j=jms, jme
          qvapr(i,k,j) = moist(i,k,j,P_QV)
          qrain(i,k,j) = moist(i,k,j,P_QR)
          qsnow(i,k,j) = moist(i,k,j,P_QS)
          qgrpl(i,k,j) = moist(i,k,j,P_QG)
          qcloud(i,k,j) = moist(i,k,j,P_QC)
          qice(i,k,j) = moist(i,k,j,P_QI)
          ncloud(i,k,j) = scalar(i,k,j,P_QNC)
        ENDDO
      ENDDO
    ENDDO
    
    ! Total pressure
    ! -------------- 
    DO i=ims, ime
      DO k=kms, kme
        DO j=jms, jme
          ptot(i,k,j)=grid%pb(i,k,j)+grid%p(i,k,j)
        ENDDO
      ENDDO
    ENDDO

    ! Calculate relative humidity and mid-level relative humidity
    ! -----------------------------------------------------------
    DO i=ims,ime
      DO k=kms,kme    
        DO j=jms,jme
          rh(i,k,j)=calc_rh(ptot(i,k,j),grid%t_phy(i,k,j), qvapr(i,k,j))
        ENDDO
      ENDDO
    ENDDO

#ifdef WRF_CHEM
    ! Surface dust concentration array (ug m-3)
    ! ----------------------------------------- 
    DO i=ims, ime
      DO j=jms, jme
        dustc(i,j,1)=chem(i,k_start,j,p_dust_1)*grid%rho(i,k_start,j)
        dustc(i,j,2)=chem(i,k_start,j,p_dust_2)*grid%rho(i,k_start,j)
        dustc(i,j,3)=chem(i,k_start,j,p_dust_3)*grid%rho(i,k_start,j)
        dustc(i,j,4)=chem(i,k_start,j,p_dust_4)*grid%rho(i,k_start,j)
        dustc(i,j,5)=chem(i,k_start,j,p_dust_5)*grid%rho(i,k_start,j)
      ENDDO
    ENDDO
#else
    dustc(ims:ime,jms:jme,:)=0.
#endif
   
    ! Calculate severe weather diagnostics.  These variables should only be
    ! output at highest frequency output.  (e.g. auxhist2)
    ! ---------------------------------------------------------------------
    IF ( config_flags % afwa_severe_opt == 1 ) THEN

      ! After each history dump, reset max/min value arrays
      ! Note: This resets up_heli_max which is currently calculated within
      ! rk_first_rk_step_part2.F, may want to move to this diagnostics package
      ! later
      ! ----------------------------------------------------------------------
      IF ( is_after_history_dump ) THEN
        DO j = jms, jme
          DO i = ims, ime
            grid%wspd10max(i,j) = 0.
            grid%w_up_max(i,j) = 0.
            grid%w_dn_max(i,j) = 0.
            grid%tcoli_max(i,j) = 0.
            grid%up_heli_max(i,j) = 0.
            grid%refd_max(i,j) = 0.
            grid%afwa_llws(i,j) = 0.
            grid%afwa_hail(i,j) = 0.
            grid%afwa_tornado(i,j) = 0.
            grid%midrh_min_old(i,j) = grid%midrh_min(i,j) ! Save old midrh_min
            grid%midrh_min(i,j) = 999.
          ENDDO
        ENDDO
      ENDIF  ! is_after_history_dump

      CALL severe_wx_diagnostics ( grid % wspd10max             &
                             , grid % w_up_max                  &
                             , grid % w_dn_max                  &
                             , grid % up_heli_max               &
                             , grid % tcoli_max                 &
                             , grid % midrh_min_old             &
                             , grid % midrh_min                 &
                             , grid % afwa_hail                 &
                             , grid % afwa_cape                 &
                             , grid % afwa_zlfc                 &
                             , grid % afwa_plfc                 &
                             , grid % afwa_llws                 &
                             , grid % afwa_tornado              &
                             , grid % u10                       &
                             , grid % v10                       &
                             , grid % w_2                       &
                             , grid % uh                        &
                             , grid % t_phy                     &
                             , grid % t2                        &
                             , grid % z                         &
                             , grid % ht                        &
                             , grid % u_phy                     &
                             , grid % v_phy                     &
                             , ptot                             &
                             , qice                             &
                             , qsnow                            &
                             , qgrpl                            &
                             , grid % rho                       &
                             , dz8w                             &
                             , rh                               &
                             , ims, ime, jms, jme, kms, kme     &
                             , its, ite, jts, jte               &
                             , k_start, k_end               )
    ENDIF   ! afwa_severe_opt == 1

    ! Calculate precipitation type diagnostics
    ! ----------------------------------------
    IF ( config_flags % afwa_ptype_opt == 1 ) THEN
    
      ! First initialize precip buckets
      ! -------------------------------
      IF ( grid % itimestep .eq. 1) THEN
        DO i=ims,ime
          DO j=jms,jme
            grid % afwa_rain(i,j)=0.
            grid % afwa_snow(i,j)=0.
            grid % afwa_ice(i,j)=0.
            grid % afwa_fzra(i,j)=0.
            grid % afwa_snowfall(i,j)=0.
          ENDDO
        ENDDO
      ENDIF
  
      ! Time-step precipitation (convective + nonconvective)
      ! ------------------------------------------------------
      DO i=ims,ime
        DO j=jms,jme
          grid%afwa_precip(i,j)=grid%raincv(i,j)+grid%rainncv(i,j)
        ENDDO
      ENDDO

      ! Diagnose precipitation type
      ! ---------------------------
      CALL precip_type_diagnostics ( grid % t_phy               &
                             , grid % t2                        &
                             , rh                               &
                             , grid % z                         &
                             , grid % ht                        &
                             , grid % afwa_precip               &
                             , grid % swdown                    &
                             , grid % afwa_rain                 &
                             , grid % afwa_snow                 &
                             , grid % afwa_ice                  &
                             , grid % afwa_fzra                 &
                             , grid % afwa_snowfall             &
                             , grid % afwa_ptype_ccn_tmp        &
                             , grid % afwa_ptype_tot_melt       &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )
    ENDIF  ! afwa_ptype_opt == 1
  
    ! The following packages are calculated only on output timesteps
    ! --------------------------------------------------------------
    IF ( is_output_timestep ) THEN      

      ! Calculate equivalent radar reflectivity factor (z_e) using 
      ! old RIP code (2004) if running radar or VIL packages.
      ! ----------------------------------------------------------
      IF ( config_flags % afwa_radar_opt == 1 .or. &
         config_flags % afwa_vil_opt == 1 ) THEN
        write ( message, * ) 'Calculating Radar'
        CALL wrf_debug( 100 , message )
        CALL wrf_dbzcalc ( grid%rho                             &
                             , grid%t_phy                       &
                             , qrain                            &
                             , qsnow                            &
                             , qgrpl                            &
                             , z_e                              &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF  ! afwa_radar_opt == 1 .or. afwa_vil_opt == 1

      ! Calculate derived radar variables
      ! ---------------------------------
      IF ( config_flags % afwa_radar_opt == 1 ) THEN
        write ( message, * ) 'Calculating derived radar variables'
        CALL wrf_debug( 100 , message )
        CALL radar_diagnostics ( grid % refd                    &
                             , grid % refd_com                  &
                             , grid % refd_max                  &
                             , grid % echotop                   &
                             , grid % z                         &
                             , z_e                              &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF  ! afwa_radar_opt == 1

      ! Calculate VIL and reflectivity every history output timestep
      ! ------------------------------------------------------------
      IF ( config_flags % afwa_vil_opt == 1 ) THEN
        write ( message, * ) 'Calculating VIL'
        CALL wrf_debug( 100 , message )
        CALL vert_int_liquid_diagnostics ( grid % vil           &
                             , grid % radarvil                  &
                             , grid % t_phy                     &
                             , qrain                            &
                             , qsnow                            &
                             , qgrpl                            &
                             , z_e                              &
                             , dz8w                             &
                             , grid % rho                       &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF  ! afwa_vil_opt ==1 

      ! Calculate icing and freezing level
      ! ----------------------------------
      IF ( config_flags % afwa_icing_opt == 1 ) THEN

        ! Determine icing option from microphysics scheme
        ! -----------------------------------------------
        
        IF ( config_flags % mp_physics == GSFCGCESCHEME ) THEN
          icing_opt=1
        ELSEIF ( config_flags % mp_physics == ETAMPNEW ) THEN
          icing_opt=2
        ELSEIF ( config_flags % mp_physics == THOMPSON ) THEN
          icing_opt=3
        ELSEIF ( config_flags % mp_physics == WSM5SCHEME .OR.   &
                 config_flags % mp_physics == WSM6SCHEME ) THEN
          icing_opt=4
        ELSEIF ( config_flags % mp_physics == MORR_TWO_MOMENT ) THEN
          !-->RAS13.2
          !Is this run with prognostic cloud droplets or no?
          IF (config_flags % progn > 0) THEN
             icing_opt=6
          ELSE
             icing_opt=5
          ENDIF
        ELSEIF ( config_flags % mp_physics == WDM6SCHEME ) THEN
          icing_opt=7
        ELSE
          icing_opt=0  ! Not supported
        ENDIF
 
        write ( message, * ) 'Calculating Icing with icing opt ',icing_opt 
        CALL wrf_debug( 100 , message )
        CALL icing_diagnostics ( icing_opt                      &
                             , grid % fzlev                     &
                             , grid % icing_lg                  &
                             , grid % icing_sm                  &
                             , grid % qicing_lg_max             &
                             , grid % qicing_sm_max             &
                             , grid % qicing_lg                 &
                             , grid % qicing_sm                 &
                             , grid % icingtop                  &
                             , grid % icingbot                  &
                             , grid % t_phy                     &
                             , grid % z                         &
                             , dz8w                             &
                             , grid % rho                       &
                             , qrain                            &
                             , qcloud                           &
                             , ncloud                           &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF  ! afwa_icing_opt

      IF ( config_flags % afwa_vis_opt == 1 ) THEN
   
        ! Calculate 2 meter relative humidity
        ! -----------------------------------
        DO i=ims,ime
          DO j=jms,jme
            rh2m(i,j)=calc_rh(grid%psfc(i,j), grid%t2(i,j), grid%q2(i,j))
          ENDDO
        ENDDO
      
        ! Calculate 10 meter winds
        ! ------------------------
        DO i=ims,ime
          DO j=jms,jme
            wind10m(i,j)=uv_wind(grid%u10(i,j),grid%v10(i,j))
          ENDDO
        ENDDO

        write ( message, * ) 'Calculating visibility'
        CALL wrf_debug( 100 , message )
        CALL vis_diagnostics ( qcloud(ims:ime,k_start,jms:jme)  &
                             , qrain(ims:ime,k_start,jms:jme)   &
                             , qice(ims:ime,k_start,jms:jme)    &
                             , qsnow(ims:ime,k_start,jms:jme)   &
                             , wind10m                          &
                             , rh2m                             &
                             , dustc                            &
                             , grid % afwa_vis                  &
                             , grid % afwa_vis_dust             &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe ) 
      ENDIF

      IF ( config_flags % afwa_cloud_opt == 1 ) THEN
        CALL cloud_diagnostics (qcloud                          &
                             , qice                             &
                             , qsnow                            &
                             , rh                               &
                             , dz8w                             &
                             , grid % rho                       &
                             , grid % z                         &
                             , grid % ht                        &
                             , grid % afwa_cloud                &
                             , grid % afwa_cloud_ceil           &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF

    ENDIF  ! is_output_timestep

  END SUBROUTINE afwa_diagnostics_driver



  SUBROUTINE severe_wx_diagnostics ( wspd10max                  &
                             , w_up_max                         &
                             , w_dn_max                         &
                             , up_heli_max                      &
                             , tcoli_max                        &
                             , midrh_min_old                    &
                             , midrh_min                        &
                             , afwa_hail                        &
                             , cape                             &
                             , zlfc                             &
                             , plfc                             &
                             , llws_max                         &
                             , afwa_tornado                     &
                             , u10                              &
                             , v10                              &
                             , w_2                              &
                             , uh                               &
                             , t_phy                            &
                             , t2                               &
                             , z                                &
                             , ht                               &
                             , u_phy                            &
                             , v_phy                            &
                             , p                                &
                             , qi                               &
                             , qs                               &
                             , qg                               &
                             , rho                              &
                             , dz8w                             &
                             , rh                               &
                             , ims, ime, jms, jme, kms, kme     &
                             , its, ite, jts, jte               &
                             , k_start, k_end               )

    INTEGER, INTENT(IN) :: its, ite, jts, jte, k_start, k_end   &
                         , ims, ime, jms, jme, kms, kme

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN   ) ::                                    p  &
                                              ,            w_2  &
                                              ,          t_phy  &
                                              ,          u_phy  &
                                              ,          v_phy  &
                                              ,             qi  &
                                              ,             qs  &
                                              ,             qg  &
                                              ,            rho  &
                                              ,              z  &
                                              ,           dz8w  &
                                              ,             rh


    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(IN   ) ::                                  u10  &
                                              ,            v10  &
                                              ,             uh  &
                                              ,             t2  &
                                              ,             ht  &
                                              ,  midrh_min_old  &
                                              ,    up_heli_max


    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(INOUT) ::                            wspd10max  &
                                              ,       w_up_max  &
                                              ,       w_dn_max  &
                                              ,      tcoli_max  &
                                              ,      midrh_min  &
                                              ,       llws_max  &
                                              ,      afwa_hail  &
                                              ,   afwa_tornado

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(  OUT) ::                                 cape  &
                                              ,           zlfc  &
                                              ,           plfc

    ! Local
    ! -----
    INTEGER :: i,j,k
    INTEGER :: kts,kte
    REAL    :: zagl, zlfc_msl, melt_term, midrh_term, hail, midrh
    REAL    :: tornado, lfc_term, shr_term, midrh2_term, uh_term
    REAL    :: u2000, v2000, us, vs
    REAL    :: wind_vel, p_tot, tcoli
    INTEGER :: nz, ostat
    LOGICAL :: is_target_level
    REAL, DIMENSION( ims:ime, jms:jme ) ::                w_up  &
                                              ,           w_dn  &
                                              ,           llws
                         

    ! Calculate midlevel relative humidity minimum
    ! --------------------------------------------
    DO i=ims,ime
      DO j=jms,jme
        is_target_level=.false.
        DO k=kms,kme    
          zagl = z(i,k,j) - ht(i,j)
          IF ( ( zagl >= 3500. ) .and. &
               ( .NOT. is_target_level ) .and. &
               ( k .ne. kms ) ) THEN
            is_target_level = .true.
            midrh = rh(i,k-1,j) + (3500. - (z(i,k-1,j) - ht(i,j))) &
                    * ((rh(i,k,j) - rh(i,k-1,j))/(z(i,k,j) - z(i,k-1,j)))
            IF ( midrh .lt. midrh_min(i,j) ) THEN
              midrh_min(i,j) = midrh
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ENDDO

    ! Calculate the max 10 m wind speed between output times
    ! ------------------------------------------------------
    DO j = jts, jte
      DO i = its, ite
        !wind_vel = sqrt( u10(i,j)*u10(i,j) + v10(i,j)*v10(i,j) )
        wind_vel = uv_wind ( u10(i,j) , v10(i,j) )
        IF ( wind_vel .GT. wspd10max(i,j) ) THEN
          wspd10max(i,j) = wind_vel
        ENDIF
      ENDDO
    ENDDO
 
    ! Vertical velocity quantities between output times
    ! -------------------------------------------------
    w_up=0.
    w_dn=0.
    DO j = jts, jte
      DO k = k_start, k_end
        DO i = its, ite
          p_tot = p(i,k,j) / 100.
 
          ! Check vertical velocity field below 400 mb
          !IF ( p_tot .GT. 400. .AND. w_2(i,k,j) .GT. w_up_max(i,j) ) THEN
          !  w_up_max(i,j) = w_2(i,k,j)
          !ENDIF
          !IF ( p_tot .GT. 400. .AND. w_2(i,k,j) .LT. w_dn_max(i,j) ) THEN
          !  w_dn_max(i,j) = w_2(i,k,j)
          !ENDIF

          ! Check vertical velocity field below 400 mb
          IF ( p_tot .GT. 400. .AND. w_2(i,k,j) .GT. w_up(i,j) ) THEN
            w_up(i,j) = w_2(i,k,j)
            IF ( w_up(i,j) .GT. w_up_max(i,j) ) THEN
              w_up_max(i,j) = w_up(i,j)
            ENDIF
          ENDIF
          IF ( p_tot .GT. 400. .AND. w_2(i,k,j) .LT. w_dn(i,j) ) THEN
            w_dn(i,j) = w_2(i,k,j)
            IF ( w_dn(i,j) .GT. w_dn_max(i,j) ) THEN
              w_dn_max(i,j) = w_dn(i,j)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ENDDO
   
    ! Hail diameter in millimeters (Weibull distribution)
    ! ---------------------------------------------------
    DO j = jts, jte
      DO i = its, ite
        melt_term=max(t2(i,j)-288.15,0.)
        midrh_term=max(2*(min(midrh_min(i,j),midrh_min_old(i,j))-70.),0.)
        hail=max((w_up(i,j)/1.4)**1.25-melt_term-midrh_term,0.)
        IF ( hail .gt. afwa_hail(i,j) ) THEN
          afwa_hail(i,j)=hail
        ENDIF
      ENDDO
    ENDDO

    ! Lightning (total column-integrated cloud ice)
    ! Note this formula is basically stolen from the VIL calculation.
    ! ---------------------------------------------------------------
    DO j = jts, jte
      DO i = its, ite
        tcoli=0.
        DO k = k_start, k_end
          tcoli =  tcoli + &
          (qi (i,k,j) + &
           qs (i,k,j) + &
           qg (i,k,j))  &
           *dz8w (i,k,j) * rho(i,k,j)
        ENDDO
        IF ( tcoli .GT. tcoli_max(i,j) ) THEN
          tcoli_max(i,j) = tcoli
        ENDIF
      ENDDO
    ENDDO

    ! Calculate buoyancy parameters.
    ! ------------------------------
    nz = k_end - k_start
    DO j = jts, jte
      DO i = its, ite
        ostat = Buoyancy (                                   nz &
                                     , t_phy(i,kms:kme      ,j) &
                                     ,    rh(i,kms:kme      ,j) &
                                     ,     p(i,kms:kme      ,j) &
                                     ,     z(i,kms:kme      ,j) &
                                     ,                        1 &
                                     ,                cape(i,j) &
                                     ,                 zlfc_msl &
                                     ,                plfc(i,j) &
                                     ,                        3 ) !Surface
        IF ( ostat /= 0 ) then
          WRITE (*,*) "something went wrong with buoyancy calc at i=",i," j=",j
        ENDIF
        
        ! Subtract terrain height to convert ZLFC from MSL to AGL
        ! -------------------------------------------------------
        zlfc(i,j)=zlfc_msl-ht(i,j)

      ENDDO
    ENDDO

    ! Calculate 0-2000 foot (0 - 609.6 meter) shear.
    ! ----------------------------------------------
    DO j = jts, jte
      DO i = its, ite
        is_target_level=.false.
        DO k=kms,kme    
          zagl = z(i,k,j) - ht(i,j)
          IF ( ( zagl >= 609.6 ) .and. &
               ( .NOT. is_target_level ) .and. &
               ( k .ne. kms ) ) THEN
            is_target_level = .true.
            u2000 = u_phy(i,k-1,j) + (609.6 - (z(i,k-1,j) - ht(i,j))) &
                    * ((u_phy(i,k,j) - u_phy(i,k-1,j))/(z(i,k,j) - z(i,k-1,j)))
            v2000 = v_phy(i,k-1,j) + (609.6 - (z(i,k-1,j) - ht(i,j))) &
                    * ((v_phy(i,k,j) - v_phy(i,k-1,j))/(z(i,k,j) - z(i,k-1,j)))
            us = u2000 - u10(i,j) 
            vs = v2000 - v10(i,j) 
            llws(i,j) = uv_wind ( us , vs )
            IF ( llws(i,j) .gt. llws_max(i,j) ) THEN
              llws_max(i,j) = llws(i,j)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ENDDO

    ! Maximum tornado wind speed in ms-1.
    ! -----------------------------------
    DO j = jts, jte
      DO i = its, ite
        IF ( zlfc(i,j) .ge. 0. ) THEN
          !uh_term = min(max((up_heli_max(i,j) - 25.) / 50., 0.), 1.)
          uh_term = min(max((uh(i,j) - 25.) / 50., 0.), 1.)
          shr_term = min(max((llws(i,j) - 2.) / 10., 0.), 1.)
          lfc_term = min(max((3000. - zlfc(i,j)) / 1500., 0.), 1.)
          midrh2_term = min(max((90. - min(midrh_min(i,j),midrh_min_old(i,j))) / 30., 0.), 1.)
          tornado = 50. * uh_term * shr_term * lfc_term * midrh2_term
          IF (tornado .gt. afwa_tornado(i,j)) THEN
            afwa_tornado(i,j) = tornado
          ENDIF
        ENDIF
      ENDDO
    ENDDO
    

  END SUBROUTINE severe_wx_diagnostics



  SUBROUTINE vert_int_liquid_diagnostics ( vil                  &
                             , radarvil                         &
                             , t_phy                            &
                             , qrain                            &
                             , qsnow                            &
                             , qgrpl                            &
                             , z_e                              &
                             , dz8w                             &
                             , rho                              &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN) ::                                     rho  &
                                              ,          qrain  &
                                              ,          qsnow  &
                                              ,          qgrpl  & 
                                              ,          t_phy  &
                                              ,            z_e  & 
                                              ,           dz8w

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(INOUT) ::                                  vil  &
                                              ,       radarvil

    ! Local
    ! -----
    INTEGER :: i,j,k,ktime

    ! Calculate vertically integrated liquid water (though its mostly not
    ! "liquid" now is it?)
    ! -------------------------------------------------------------------
    DO i = ips, MIN(ipe,ide-1)
    DO j = jps, MIN(jpe,jde-1)
      vil (i,j) = 0.0
      DO k = kps, MIN(kpe,kde-1)
        vil (i,j) =  vil (i,j) + &
         (qrain (i,k,j) + &
          qsnow (i,k,j) + &
          qgrpl (i,k,j))  &
          *dz8w (i,k,j) * rho(i,k,j)
      ENDDO
    ENDDO
    ENDDO

    ! Diagnose "radar-derived VIL" from equivalent radar reflectivity
    ! radarVIL = (integral of LW*dz )/1000.0  (in kg/m^2)
    ! LW = 0.00344 * z_e** (4/7)  in g/m^3
    ! ---------------------------------------------------------------
    DO i = ips, MIN(ipe,ide-1)
    DO j = jps, MIN(jpe,jde-1)
      radarvil (i,j) = 0.0
      DO k = kps, MIN(kpe,kde-1)
        radarvil (i,j) = radarvil (i,j) + &
        0.00344 * z_e(i,k,j)**0.57143 &
        *dz8w (i,k,j)/1000.0
      END DO
    END DO
    END DO

  END SUBROUTINE vert_int_liquid_diagnostics



  SUBROUTINE icing_diagnostics ( icing_opt                      &
                             , fzlev                            &
                             , icing_lg                         &
                             , icing_sm                         & 
                             , qicing_lg_max                    &
                             , qicing_sm_max                    &
                             , qicing_lg                        &
                             , qicing_sm                        &
                             , icingtop                         &
                             , icingbot                         &
                             , t_phy                            &
                             , z                                &
                             , dz8w                             &
                             , rho                              &
                             , qrain                            &
                             , qcloud                           &
                             , ncloud                           &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    INTEGER, INTENT(IN) :: icing_opt

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN) ::                                       z  &
                                              ,          qrain  &
                                              ,         qcloud  &
                                              ,         ncloud  &
                                              ,            rho  &
                                              ,           dz8w  &
                                              ,          t_phy

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(  OUT) ::                                fzlev  &
                                              ,       icing_lg  &
                                              ,       icing_sm  &
                                              ,  qicing_lg_max  &
                                              ,  qicing_sm_max  &
                                              ,       icingtop  &
                                              ,       icingbot

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(  OUT) ::                            qicing_lg  &
                                              ,      qicing_sm
         

    ! Local
    ! -----
    INTEGER :: i,j,k,ktime,ktop,kbot
    REAL    :: qcfrac_lg, qcfrac_sm, qc, qr, small, all

    ! Initializations
    ! ---------------
    fzlev (ips:ipe,jps:jpe) = -999.        ! Arbitrary unset/initial value
    icingtop (ips:ipe,jps:jpe) = -999.     ! Arbitrary unset/initial value
    icingbot (ips:ipe,jps:jpe) = -999.     ! Arbitrary unset/initial value
    icing_lg (ips:ipe,jps:jpe) = 0.        
    icing_sm (ips:ipe,jps:jpe) = 0.
    qicing_lg_max (ips:ipe,jps:jpe) = 0. 
    qicing_sm_max (ips:ipe,jps:jpe) = 0. 
    qicing_sm(ips:ipe,kps:kpe,jps:jpe)=0.
    qicing_lg(ips:ipe,kps:kpe,jps:jpe)=0.   

    ! Loop through i and j
    ! --------------------
    DO i = ips, MIN(ipe,ide-1)
    DO j = jps, MIN(jpe,jde-1)

      ! Go up the column and look for sub freezing temperatures
      ! -------------------------------------------------------
      ktop=-1
      kbot=-1
      DO k = kps, MIN(kpe,kde-1)
        IF (t_phy(i,k,j) .lt. 273.15) THEN

          ! Any cloud water we find will be supercooled.
          ! Based on microphysics scheme, determine the fraction of
          ! large (>50 um) supercooled cloud water drops.
          ! Source: Becky Selin, 16WS
          ! -------------------------------------------------------
          qc = qcloud (i,k,j)
          qr = qrain (i,k,j)
          nc = ncloud(i,k,j)
          den = rho(i,k,j)
          qcfrac_lg = 0.
          qcfrac_sm = 0.
          
          ! Eta (Ferrier)
          ! -------------
          IF (icing_opt .eq. 2) THEN
            IF (qc .lt. 2.5E-4) THEN
              qcfrac_lg = 395000. * qc**2. + 102.9 * qc
            ELSEIF (qc .lt. 1.4E-3) THEN
              qcfrac_lg = 276.1 * qc - 0.01861
            ELSE
              qcfrac_lg = 0.3 * log(641.789 * qc) + 0.4
            ENDIF

          ! Thompson
          ! --------
          ! RAS13.2 Per James McCormick's stats, more large supercooled
          ! drops are needed from the Thompson members.  Changing 
          ! calculation to be like WSM5/6 members.
          !ELSEIF (icing_opt .eq. 3) THEN
          !  IF (qc .lt. 1.0E-3) THEN
          !    qcfrac_lg = 2205.0 * qc**2. + 3.232 * qc
          !  ELSEIF (qc .lt. 3.0E-3) THEN
          !    qcfrac_lg = 24.1 * qc - 0.01866
          !  ELSE
          !    qcfrac_lg = 0.127063 * log(550.0 * qc) - 0.01
          !  ENDIF

          ! Thompson or WSM5/6
          ! ------------------
          !ELSEIF (icing_opt .eq. 4) THEN
          ELSEIF ((icing_opt .eq. 3) .OR. (icing_opt .eq. 4)) THEN
            IF (qc .lt. 5.E-4) THEN
              qcfrac_lg = 50420.0 * qc**2. + 29.39 * qc
            ELSEIF (qc .lt. 1.4E-3) THEN
              qcfrac_lg = 97.65 * qc - 0.02152
            ELSE
              qcfrac_lg = 0.2 * log(646.908 * qc) + 0.135
            ENDIF

          ! Morrison 2-moment, constant CCN
          ! -------------------------------
          ELSEIF (icing_opt .eq. 5) THEN
            IF (qc .lt. 1.4E-3) THEN
              qcfrac_lg = 28000. * qc**2. + 0.1 * qc 
            ELSEIF (qc .lt. 2.6E-3) THEN
              qcfrac_lg = 112.351 * qc - 0.102272
            ELSE 
              qcfrac_lg = 0.3 * log(654.92 * qc) * 0.301607
            ENDIF

          ! WDM6 or Morrison 2-moment w/ prognostic CCN
          ! -------------------------------------------
          ELSEIF ((icing_opt .eq. 6) .OR. (icing_opt .eq. 7)) THEN
            IF ((qc .gt. 1.0E-12) .and. (nc .gt. 1.0E-12)) THEN
               small = -nc * exp(-nc*3141.59265*(5.E-5)**3./(6000.*den*qc))+nc
               all = -nc * exp(-nc*3141.59265*(2.)**3./(6000.*den*qc))+nc
               qcfrac_lg = 1. - (small / all)
            ELSE
               qcfac_lg = 0.
            ENDIF
          ENDIF
          qcfrac_lg = max(qcfrac_lg, 0.)
          
          ! Small (<50 um) supercooled cloud water drop fraction (1 - large).
          ! -----------------------------------------------------------------
          IF (icing_opt .ne. 0 ) THEN
            qcfrac_sm = 1 - qcfrac_lg
          ENDIF

          ! Supercooled drop mixing ratio
          ! -----------------------------
          qicing_lg (i,k,j) = max(qr + qcfrac_lg * qc, 0.)
          qicing_sm (i,k,j) = max(qcfrac_sm * qc, 0.)        

          ! Column integrated icing
          ! -----------------------
          icing_lg (i,j) = icing_lg (i,j) + qicing_lg (i,k,j) &
                            * dz8w (i,k,j) * rho(i,k,j)
          icing_sm (i,j) = icing_sm (i,j) + qicing_sm (i,k,j) &
                            * dz8w (i,k,j) * rho(i,k,j)

          ! Column maximum supercooled drop mixing ratio 
          ! --------------------------------------------
          IF ( qicing_lg(i,k,j) .gt. qicing_lg_max(i,j) ) THEN
            qicing_lg_max (i,j) = qicing_lg(i,k,j)
          ENDIF
          IF ( qicing_sm(i,k,j) .gt. qicing_sm_max(i,j) ) THEN
            qicing_sm_max (i,j) = qicing_sm(i,k,j)
          ENDIF
           
          ! Freezing level calculation
          ! --------------------------
          IF (fzlev (i,j) .eq. -999.) THEN  ! At freezing level
            IF (k .ne. kps) THEN  ! If not at surface, interpolate.      
              fzlev (i,j) = z (i,k-1,j) + &
                             ((273.15 - t_phy (i,k-1,j)) &
                            /(t_phy (i,k,j) - t_phy (i,k-1,j))) &
                            *(z (i,k,j) - z (i,k-1,j))
            ELSE  ! If at surface, use first level.
              fzlev(i,j) = z (i,k,j)
            ENDIF
          ENDIF

          ! Icing layer top and bottom indices (where icing > some arbitrary
          ! small value). Set bottom index of icing layer to current k index 
          ! if not yet set. Set top index of icing layer to current k index.
          ! ----------------------------------------------------------------
          IF ((qicing_lg (i,k,j) + qicing_sm (i,k,j)) .ge. 1.E-5) THEN
            IF (kbot .eq. -1) kbot = k  
            ktop=k
          ENDIF
        ENDIF
      END DO

      ! Interpolate bottom of icing layer from kbot (bottom index of icing
      ! layer). Icing bottom should not go below freezing level.
      ! ------------------------------------------------------------------
      IF (kbot .ne. -1) THEN
        IF (kbot .ne. kps) THEN ! If not at surface, interpolate
          icingbot (i,j) = z (i,kbot-1,j) + ((1.E-5 - &
                   (qicing_lg (i,kbot-1,j) + qicing_sm (i,kbot-1,j))) &
                  / ((qicing_lg (i,kbot,j) + qicing_sm (i,kbot,j)) &
                  - (qicing_lg (i,kbot-1,j) + qicing_sm (i,kbot-1,j)))) &
                  * (z (i,kbot,j) - z (i,kbot-1,j))
          icingbot (i,j) = MAX(icingbot (i,j), fzlev (i,j))
        ELSE  ! If at surface use first level.
          icingbot (i,j) = z(i,kbot,j)
        ENDIF
      ENDIF

      ! Interpolate top of icing layer from ktop (top index of icing layer).
      ! Icing top should not go below icing bottom (obviously).
      ! --------------------------------------------------------------------
      IF (ktop .ne. -1 .and. ktop .ne. kpe) THEN ! If not undefined or model top
        icingtop (i,j) = z (i,ktop,j) + ((1.E-5 - &
                 (qicing_lg (i,ktop,j) + qicing_sm (i,ktop,j))) &
                 / ((qicing_lg (i,ktop+1,j) + qicing_sm (i,ktop+1,j)) &
                 - (qicing_lg (i,ktop,j) + qicing_sm (i,ktop,j)))) &
                 * (z (i,ktop+1,j) - z (i,ktop,j))
        icingtop (i,j) = MAX(icingtop (i,j), icingbot (i,j))
      ENDIF
    END DO
    END DO

  END SUBROUTINE icing_diagnostics



  SUBROUTINE radar_diagnostics ( refd                           &
                             , refd_com                         &
                             , refd_max                         &
                             , echotop                          &
                             , z                                &
                             , z_e                              &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN) ::                                       z  &
                                              ,            z_e

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(INOUT) ::                                 refd  &
                                              ,       refd_com  &
                                              ,       refd_max  &
                                              ,        echotop

    ! Local
    ! -----
    INTEGER :: i,j,k,ktime
    
    DO j = jps, MIN(jpe,jde-1)
    DO i = ips, MIN(ipe,ide-1)
      ktop = -1  ! Undefined
      echotop (i,j) = 0.
      refd_com (i,j) = 0.
      refd (i,j) = 0.
      DO k = kps, MIN(kpe,kde-1)
        IF (z_e(i,k,j) .gt. 1.e-20) THEN

          ! Reflectivity (first level)
          ! --------------------------
          IF (k == kps) refd(i,j) = MAX(10.0 * log10(z_e(i,k,j)),0.)
   
          ! Max reflectivity over the output interval
          ! -----------------------------------------
          IF (refd(i,j) .gt. refd_max(i,j)) refd_max(i,j) = refd(i,j)

          ! Composite reflectivity calc (max reflectivity in the column)
          ! ------------------------------------------------------------
          IF (10.0 * log10(z_e(i,k,j)) .gt. refd_com(i,j)) THEN
            refd_com(i,j) = 10.0 * log10(z_e(i,k,j))
          ENDIF
        ENDIF
        
        ! Echo top - the highest level w/ dBZ > 18 (z_e > 63.0957)
        ! --------------------------------------------------------
        IF ( z_e(i,k,j) .gt. 63.0957) THEN
          ktop = k
        ENDIF
      END DO
      IF ( ktop .ne. -1 ) THEN  ! Interpolate to echo top height (GAC)
        echotop (i,j) = z (i,ktop,j) + &
                          ((63.0957 - z_e (i,ktop,j)) &
                         /(z_e (i,ktop+1,j) - z_e (i,ktop,j))) &
                         *(z (i,ktop+1,j) - z (i,ktop,j))
      ENDIF
    END DO
    END DO

  END SUBROUTINE radar_diagnostics



  SUBROUTINE wrf_dbzcalc( rho                                   &
                             , t_phy                            &
                             , qr                               &
                             , qs                               &
                             , qg                               &
                             , z_e                              &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN   ) ::                                  rho  &
                                              ,          t_phy  &
                                              ,             qr  &
                                              ,             qs  &
                                              ,             qg

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(  OUT) ::                                  z_e

    REAL :: factor_r, factor_s, factor_g, factorb_s, factorb_g, ronv, sonv, gonv
    REAL :: temp_c, rhoair, qgr, qra, qsn
    INTEGER :: i, j, k

    INTEGER, PARAMETER :: iBrightBand = 1
    REAL, PARAMETER :: T_0 = 273.15
    REAL, PARAMETER :: PI = 3.1415926536
    REAL, PARAMETER :: rgas=287.04, gamma_seven = 720.0, alpha2 = 0.224

    ! Densities of rain, snow, graupel, and cloud ice.
    ! ------------------------------------------------
    REAL, PARAMETER :: rho_w = 1000.0, rho_r = 1000.0, rho_s = 100.0
    REAL, PARAMETER :: rho_g = 400.0, rho_i = 890.0
    REAL, PARAMETER :: ron=8.e6, son=2.e7, gon=5.e7, r1=1.e-15
    REAL, PARAMETER :: ron_min = 8.e6, ron2=1.e10
    REAL, PARAMETER :: ron_qr0 = 0.0001, ron_delqr0 = 0.25*ron_qr0
    REAL, PARAMETER :: ron_const1r = (ron2-ron_min)*0.5
    REAL, PARAMETER :: ron_const2r = (ron2+ron_min)*0.5

    ! Constant intercepts
    ! -------------------
    ronv = 8.e6    ! m^-4
    sonv = 2.e7    ! m^-4
    gonv = 4.e6    ! m^-4

    factor_r = gamma_seven * 1.e18 * (1./(pi*rho_r))**1.75
    factor_s = gamma_seven * 1.e18 * (1./(pi*rho_s))**1.75  &
              * (rho_s/rho_w)**2 * alpha2
    factor_g = gamma_seven * 1.e18 * (1./(pi*rho_g))**1.75  &
              * (rho_g/rho_w)**2 * alpha2

    ! For each grid point
    ! -------------------
    DO j = jps, jpe
    DO k = kps, kpe
    DO i = ips, ipe

      factorb_s = factor_s
      factorb_g = factor_g

      ! In this case snow or graupel particle scatters like liquid
      ! water because it is assumed to have a liquid skin
      ! ----------------------------------------------------------
      IF( iBrightBand == 1 ) THEN
        IF (t_phy(i,k,j) > T_0) THEN
          factorb_s = factor_s /alpha2
          factorb_g = factor_g /alpha2
        ENDIF
      ENDIF
 
      ! Calculate variable intercept parameters
      ! ---------------------------------------
      temp_c = amin1(-0.001, t_phy(i,k,j)- T_0)
      sonv = amin1(2.0e8, 2.0e6*exp(-0.12*temp_c))
      gonv = gon
      qgr = QG(i,k,j)
      qra = QR(i,k,j)
      qsn = QS(i,k,j)
      IF (qgr.gt.r1) THEN
        gonv = 2.38*(pi*rho_g/(rho(i,k,j)*qgr))**0.92
        gonv = max(1.e4, min(gonv,gon))
      ENDIF
      ronv = ron2
      IF (qra.gt. r1) THEN
        ronv = ron_const1r*tanh((ron_qr0-qra)/ron_delqr0) + ron_const2r
      ENDIF
 
      IF (qra < 0.0 ) qra = 0.0
      IF (qsn < 0.0 ) qsn = 0.0
      IF (qgr < 0.0 ) qgr = 0.0
      z_e(i,k,j) = factor_r * (rho(i,k,j) * qra)**1.75 / ronv**.75 + &
                     factorb_s * (rho(i,k,j) * qsn)**1.75 / sonv**.75 + &
                     factorb_g * (rho(i,k,j) * qgr)**1.75 / gonv**.75
 
      IF ( z_e(i,k,j) < 0.0 ) z_e(i,k,j) = 0.0
 
    END DO
    END DO
    END DO

  END SUBROUTINE wrf_dbzcalc



  SUBROUTINE precip_type_diagnostics ( t_phy                    &
                             , t2                               &
                             , rh                               &
                             , z                                &
                             , ht                               &
                             , precip                           &
                             , swdown                           &
                             , rain                             &
                             , snow                             &
                             , ice                              &
                             , frz_rain                         &
                             , snowfall                         &
                             , ccn_tmp                          &
                             , total_melt                       &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN   ) ::                                t_phy  &
                                              ,             rh  &
                                              ,              z
    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(IN   ) ::                                   t2  &
                                              ,             ht  &
                                              ,         precip  &
                                              ,         swdown
    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(INOUT) ::                             snowfall  &
                                              ,           rain  &
                                              ,       frz_rain  &
                                              ,           snow  &
                                              ,            ice
    REAL, INTENT(IN) :: ccn_tmp
    REAL, INTENT(IN) :: total_melt

    ! Local
    ! -----
    REAL, DIMENSION( ims:ime, jms:jme ) ::                      &
                                     melt                       &
                                   , mod_2m_tmp                 &
                                   , cloud_top_tmp              &
                                   , maxtmp

    INTEGER, DIMENSION( ims:ime, jms:jme ) ::                   &
                                     cloud_top_k_index          &
                                   , precip_type

    LOGICAL, DIMENSION (ims:ime, jms:jme ) ::                   &
                                     saturation 

    REAL, PARAMETER :: snow_ratio=5.0

    ! Loop through all points
    ! Search vertically twice--first to find the cloud top temperature and the 
    ! maximum temperature. Second, determine if any melting or re-freezing will
    ! occur to make ice pellets or freezing rain
    ! -------------------------------------------------------------------------
    DO i=ips,ipe
    DO j=jps,jpe
  
      saturation(i,j)=.false.
      melt(i,j)=0.0 
      precip_type(i,j)=0
        
      ! Modify surface temperature for solar insolation (W/m2)
      ! Set max temperature in the atmopshere
      ! ------------------------------------------------------
      mod_2m_tmp(i,j)=t2(i,j)+(swdown(i,j)/100.0)
      maxtmp(i,j)=mod_2m_tmp(i,j)
  
      ! Only look at points that have precip and are not warm at the surface
      ! --------------------------------------------------------------------
      IF (precip(i,j) .gt. 0.0) THEN
        !IF (mod_2m_tmp(i,j) .gt. 277.15) THEN
        IF (mod_2m_tmp(i,j) .gt. 275.15) THEN
          precip_type(i,j)=1  ! Rain
        ELSE
  
          ! Check sounding from top for saturation (RH-water gt 80%)--this is 
          ! the cloud top. Erase saturation if RH lt 70% (spurious moist layer
          ! aloft)
          ! ------------------------------------------------------------------
          cloud_top_k_index(i,j)=kpe
          DO k=kpe,kps,-1
            IF ((z(i,k,j)-ht(i,j)) .gt. 0.0) THEN
              IF (t_phy(i,k,j) .gt. maxtmp(i,j)) THEN
                maxtmp(i,j)=t_phy(i,k,j)
              ENDIF
              IF ( ( rh(i,k,j) .gt. 80 ) .and. & 
                   ( .NOT. saturation(i,j) ) ) THEN
                cloud_top_tmp(i,j)=t_phy(i,k,j)
                cloud_top_k_index(i,j)=k
                saturation(i,j)=.true.
                precip_type(i,j)=2 ! Snow
              ENDIF
              IF ( ( rh(i,k,j) .le. 70 ) .and. &
                   ( saturation(i,j) ) ) THEN
                saturation(i,j)=.false.
              ENDIF
            ENDIF
          ENDDO

          ! Perform simple check to assign types with no melting layer
          ! shenanigans going on
          ! ----------------------------------------------------------------
          IF (cloud_top_tmp(i,j) .le. ccn_tmp .and. &
          maxtmp(i,j) .le. 273.15) THEN
            precip_type(i,j)=2  ! Snow
          ENDIF

          ! ELSE, have to go through the profile again to see if snow melts, 
          ! and if anything re-freezes
          ! ----------------------------------------------------------------
          DO k=cloud_top_k_index(i,j),kps,-1
            IF ((z(i,k,j)-ht(i,j)) .gt. 0.0) THEN
 
              ! Condition 0--assign falling rain when we get to the 
              ! supercooled temperature if too warm
              ! ---------------------------------------------------
              IF (cloud_top_tmp(i,j) .eq. t_phy(i,k,j) .and. &
              cloud_top_tmp(i,j) .gt. ccn_tmp) THEN
                 precip_type(i,j)=1  ! Rain
              ENDIF

              ! Condition 1--falling frozen precip that will start to melt
              ! Add up melting energy over warm layers--if enough, turn to 
              ! liquid
              ! ----------------------------------------------------------
              IF ((precip_type(i,j) .eq. 2 .or. precip_type(i,j) .eq. 3) .and. &
              t_phy(i,k,j) .gt. 273.15) THEN
                melt(i,j)=melt(i,j)+9.8*(((t_phy(i,k,j)-273.15)/273.15)* &
                          (z(i,k,j)-z(i,k-1,j)))
                IF (melt(i,j) .gt. total_melt) THEN
                  precip_type(i,j)=1  ! Rain
                  melt(i,j)=0.0  ! Reset melting energy in case it re-freezes
                ENDIF
              ENDIF

              ! Condition 2--falling partially melted precip encounters 
              ! sub-freezing air. Snow will be converted to ice pellets if 
              ! at least 1/4 of it melted. Instantaneous freeze-up, simplistic 
              ! --------------------------------------------------------------
              IF (t_phy(i,k,j) .le. 273.15 .and. &
              melt(i,j) .gt. total_melt/4.0 .and. &
              (precip_type(i,j) .eq. 2 .or. precip_type(i,j) .eq. 3)) THEN
                precip_type(i,j)=3  ! Ice
                melt(i,j)=0.0
              ENDIF
             
              ! Condition 3--falling liquid that will re-freeze--must reach 
              ! nucleation temperature
              ! -----------------------------------------------------------
              IF (precip_type(i,j) .eq. 1) THEN
                IF (t_phy(i,k,j) .le. ccn_tmp) THEN
                  precip_type(i,j)=3  ! Ice
                ENDIF
              ENDIF
            ENDIF  ! End if (z-ht)>0
          ENDDO  ! End do k=kpe,kps,-1
        ENDIF  ! End if mod_2m_tmp>273.15

        ! Accumulate precip according to precip_type
        ! ------------------------------------------
        IF (precip_type(i,j) .eq. 3) THEN 
          ice(i,j)=ice(i,j)+precip(i,j)
        ENDIF
        IF (precip_type(i,j) .eq. 2) THEN
          snow(i,j)=snow(i,j)+precip(i,j)
          snowfall(i,j)=snowfall(i,j)+snow_ratio*precip(i,j) &
                        *(5.-mod_2m_tmp(i,j)+273.15)**0.5
        ENDIF
        IF (precip_type(i,j) .eq. 1) THEN
          IF (mod_2m_tmp(i,j) .gt. 273.15) THEN
            rain(i,j)=rain(i,j)+precip(i,j)
          ELSE
            frz_rain(i,j)=frz_rain(i,j)+precip(i,j)
          ENDIF
        ENDIF

      ENDIF  ! End if precip>0

    ENDDO  ! End do j=jps,jpe
    ENDDO  ! End do i=ips,ipe

  END SUBROUTINE precip_type_diagnostics



  SUBROUTINE vis_diagnostics ( qcloud                           &
                             , qrain                            &
                             , qice                             &
                             , qsnow                            &
                             , wind10m                          &
                             , rh2m                             &
                             , dustc                            &
                             , vis                              &
                             , vis_dust                         &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    INTEGER, PARAMETER :: ndust=5

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(IN   ) ::                               qcloud  &
                                              ,          qrain  &
                                              ,           qice  & 
                                              ,          qsnow  & 
                                              ,        wind10m  & 
                                              ,           rh2m
    REAL, DIMENSION( ims:ime, jms:jme, ndust ),                 &
         INTENT(IN   ) ::                                dustc
    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(  OUT) ::                                  vis  &
                                              ,       vis_dust

    ! Local
    ! -----
    INTEGER :: i,j,k,d
    REAL, PARAMETER :: visfactor=3.912
    REAL, DIMENSION (ndust) :: dustfact
    REAL :: bc, br, bi, bs, dust_extcoeff, hydro_extcoeff, extcoeff, vis_haze

    ! Dust factor based on 5 bin AFWA dust scheme.  This is a simplification
    ! of the scheme in WRFPOST.  More weight is applied to smaller particles.
    ! -----------------------------------------------------------------------
    dustfact=(/1.470E-6,7.877E-7,4.623E-7,2.429E-7,1.387E-7/)

    DO i=ims,ime
      DO j=jms,jme

        ! Hydrometeor extinction coefficient
        ! ----------------------------------
        bc=144.7*qcloud(i,j)**0.88
        br=2.240*qrain(i,j)**0.75
        bi=327.8*qice(i,j)
        bs=10.36*qsnow(i,j)**0.78
        hydro_extcoeff=bc+br+bi+bs

        ! Dust extinction coefficient
        ! ---------------------------
        dust_extcoeff=0.
        DO d=1,ndust
          dust_extcoeff=dust_extcoeff+dustfact(d)*dustc(i,j,d)
        ENDDO

        ! Visibility due to haze obscuration
        ! ----------------------------------
        vis_haze=1500.*(105.-rh2m(i,j)+wind10m(i,j))
        
        ! Calculate total visibility
        ! Take minimum visibility from hydro/lithometeors and haze
        ! Define maximum visibility as 20 km (UPDATE: 999.999 km)
        ! --------------------------------------------------------
        extcoeff=hydro_extcoeff+dust_extcoeff
        IF (extcoeff .gt. 0.) THEN
          vis(i,j)=MIN(visfactor/extcoeff,vis_haze)
        ELSE
          vis(i,j)=999999.
        ENDIF

        ! Calculate dust visibility
        ! Again, define maximum visibility as 20 km
        ! -----------------------------------------
        IF (dust_extcoeff .gt. 0.) THEN
          vis_dust(i,j)=MIN(visfactor/dust_extcoeff,999999.)
        ELSE
          vis_dust(i,j)=999999.
        ENDIF
      ENDDO
    ENDDO

  END SUBROUTINE vis_diagnostics
  
  

  SUBROUTINE cloud_diagnostics (qcloud                          &
                             , qice                             &
                             , qsnow                            &
                             , rh                               &
                             , dz8w                             &
                             , rho                              &
                             , z                                &
                             , ht                               &
                             , cloud                            &
                             , cloud_ceil                       &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN   ) ::                               qcloud  &
                                              ,           qice  & 
                                              ,          qsnow  & 
                                              ,             rh  & 
                                              ,           dz8w  & 
                                              ,            rho  &
                                              ,              z

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(IN   ) ::                                   ht

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(  OUT) ::                                cloud  &
                                              ,     cloud_ceil

    ! Local
    ! -----
    INTEGER :: i, j, k
    REAL    :: tot_cld_cond, maxrh, cld_frm_cnd, cld_frm_rh

    ! Calculate cloud cover based on total cloud condensate, or if none
    ! present, from maximum relative humidity in the column.
    ! -----------------------------------------------------------------
    DO i=ims,ime
      DO j=jms,jme
        tot_cld_cond = 0.
        maxrh = -9999.
        cloud_ceil(i,j) = -9999.
        DO k=kms,kme

          ! Total cloud condensate
          ! ----------------------
          tot_cld_cond = tot_cld_cond + (qcloud (i,k,j) + qice (i,k,j) &
                         + qsnow (i,k,j)) * dz8w (i,k,j) * rho(i,k,j)

          ! Maximum column relative humidity
          ! --------------------------------
          IF (rh (i,k,j) .gt. maxrh) THEN
            maxrh = rh (i,k,j)
          ENDIF
          
          ! Cloud cover parameterization. Take maximum value
          ! from condensate and relative humidity terms.
          ! ------------------------------------------------
          cld_frm_cnd = 50. * tot_cld_cond
          cld_frm_rh = MAX(((maxrh - 70.) / 30.),0.)
          cloud (i,j) = MAX(cld_frm_cnd,cld_frm_rh)

          ! Calculate cloud ceiling, the level at which
          ! parameterization of cloud cover exceeds 80%
          ! -------------------------------------------
          IF ( cloud_ceil (i,j) .eq. -9999. .and. cloud (i,j) .gt. 0.8 ) THEN
            cloud_ceil (i,j) = z (i,k,j) - ht (i,j)
          ENDIF
        ENDDO
      ENDDO
    ENDDO

  END SUBROUTINE cloud_diagnostics



  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~ 
  !~ Name:
  !~    calc_rh
  !~
  !~ Description:
  !~    This function calculates relative humidity given pressure, 
  !~    temperature, and water vapor mixing ratio.
  !~ 
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION calc_rh ( p, t, qv ) result ( rh )
    
    IMPLICIT NONE
 
    REAL, INTENT(IN) :: p, t, qv
    REAL :: rh

    ! Local
    ! -----
    REAL, PARAMETER :: pq0=379.90516
    REAL, PARAMETER :: a2=17.2693882
    REAL, PARAMETER :: a3=273.16
    REAL, PARAMETER :: a4=35.86
    REAL, PARAMETER :: rhmin=1.
    REAL :: q, qs
    INTEGER :: i,j,k
  
    ! Following algorithms adapted from WRFPOST
    ! May want to substitute with another later
    ! -----------------------------------------
      q=qv/(1.0+qv)
      qs=pq0/p*exp(a2*(t-a3)/(t-a4))
      rh=100.*q/qs
      IF (rh .gt. 100.) THEN
        rh=100.
      ELSE IF (rh .lt. rhmin) THEN
        rh=rhmin
      ENDIF

  END FUNCTION calc_rh



  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~ 
  !~ Name:
  !~    uv_wind
  !~
  !~ Description:
  !~    This function calculates the wind speed given U and V wind
  !~    components.
  !~ 
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION uv_wind ( u, v ) result ( wind_speed )
 
    IMPLICIT NONE
 
    REAL, INTENT(IN) :: u, v
    REAL :: wind_speed

    wind_speed = sqrt( u*u + v*v )

  END FUNCTION uv_wind


  
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~
  !~ Name:
  !~    Theta
  !~
  !~ Description:
  !~    This function calculates potential temperature as defined by
  !~    Poisson's equation, given temperature and pressure ( hPa ).
  !~
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION Theta ( t, p )
  IMPLICIT NONE

     !~ Variable declaration
     !  --------------------
     REAL, INTENT ( IN ) :: t
     REAL, INTENT ( IN ) :: p
     REAL                :: theta

     REAL :: Rd ! Dry gas constant
     REAL :: Cp ! Specific heat of dry air at constant pressure
     REAL :: p0 ! Standard pressure ( 1000 hPa )
  
     Rd =  287.04
     Cp = 1004.67
     p0 = 1000.00

     !~ Poisson's equation
     !  ------------------
     theta = t * ( (p0/p)**(Rd/Cp) )
  
  END FUNCTION Theta



  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~
  !~ Name:
  !~    Thetae
  !~
  !~ Description:
  !~    This function returns equivalent potential temperature using the 
  !~    method described in Bolton 1980, Monthly Weather Review, equation 43.
  !~
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION Thetae ( tK, p, rh, mixr )
  IMPLICIT NONE

     !~ Variable Declarations
     !  ---------------------
     REAL :: tK        ! Temperature ( K )
     REAL :: p         ! Pressure ( hPa )
     REAL :: rh        ! Relative humidity
     REAL :: mixr      ! Mixing Ratio ( kg kg^-1)
     REAL :: te        ! Equivalent temperature ( K )
     REAL :: thetae    ! Equivalent potential temperature
  
     REAL, PARAMETER :: R  = 287.04         ! Universal gas constant (J/deg kg)
     REAL, PARAMETER :: P0 = 1000.0         ! Standard pressure at surface (hPa)
     REAL, PARAMETER :: lv = 2.54*(10**6)   ! Latent heat of vaporization
                                            ! (J kg^-1)
     REAL, PARAMETER :: cp = 1004.67        ! Specific heat of dry air constant
                                            ! at pressure (J/deg kg)
     REAL :: tlc                            ! LCL temperature
  
     !~ Calculate the temperature of the LCL
     !  ------------------------------------
     tlc = TLCL ( tK, rh )
  
     !~ Calculate theta-e
     !  -----------------
     thetae = (tK * (p0/p)**( (R/Cp)*(1.- ( (.28E-3)*mixr*1000.) ) ) )* &
                 exp( (((3.376/tlc)-.00254))*&
                    (mixr*1000.*(1.+(.81E-3)*mixr*1000.)) )
  
  END FUNCTION Thetae



  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~
  !~ Name:
  !~    The2T.f90
  !~
  !~ Description:
  !~    This function returns the temperature at any pressure level along a
  !~    saturation adiabat by iteratively solving for it from the parcel
  !~    thetae.
  !~
  !~ Dependencies:
  !~    function thetae.f90
  !~
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION The2T ( thetaeK, pres, flag ) result ( tparcel )
  IMPLICIT NONE
  
     !~ Variable Declaration
     !  --------------------
     REAL,    INTENT     ( IN ) :: thetaeK
     REAL,    INTENT     ( IN ) :: pres
     LOGICAL, INTENT ( INOUT )  :: flag
     REAL                       :: tparcel
  
     REAL :: thetaK
     REAL :: tovtheta
     REAL :: tcheck
     REAL :: svpr, svpr2
     REAL :: smixr, smixr2
     REAL :: thetae_check, thetae_check2
     REAL :: tguess_2, correction
  
     LOGICAL :: found
     INTEGER :: iter
  
     REAL :: R     ! Dry gas constant
     REAL :: Cp    ! Specific heat for dry air
     REAL :: kappa ! Rd / Cp
     REAL :: Lv    ! Latent heat of vaporization at 0 deg. C
  
     R     = 287.04
     Cp    = 1004.67
     Kappa = R/Cp
     Lv    = 2.500E+6

     !~ Make initial guess for temperature of the parcel
     !  ------------------------------------------------
     tovtheta = (pres/100000.0)**(r/cp)
     tparcel  = thetaeK/exp(lv*.012/(cp*295.))*tovtheta

     iter = 1
     found = .false.
     flag = .false.

     DO
        IF ( iter > 105 ) EXIT

        tguess_2 = tparcel + REAL ( 1 )

        svpr   = 6.122 * exp ( (17.67*(tparcel-273.15)) / (tparcel-29.66) )
        smixr  = ( 0.622*svpr ) / ( (pres/100.0)-svpr )
        svpr2  = 6.122 * exp ( (17.67*(tguess_2-273.15)) / (tguess_2-29.66) )
        smixr2 = ( 0.622*svpr2 ) / ( (pres/100.0)-svpr2 )

        !  ------------------------------------------------------------------ ~!
        !~ When this function was orinially written, the final parcel         ~!
        !~ temperature check was based off of the parcel temperature and      ~!
        !~ not the theta-e it produced.  As there are multiple temperature-   ~!
        !~ mixing ratio combinations that can produce a single theta-e value, ~!
        !~ we change the check to be based off of the resultant theta-e       ~!
        !~ value.  This seems to be the most accurate way of backing out      ~!
        !~ temperature from theta-e.                                          ~!
        !~                                                                    ~!
        !~ Rentschler, April 2010                                             ~!
        !  ------------------------------------------------------------------  !

        !~ Old way...
        !thetaK = thetaeK / EXP (lv * smixr  /(cp*tparcel) )
        !tcheck = thetaK * tovtheta

        !~ New way
        thetae_check  = Thetae ( tparcel,  pres/100., 100., smixr  )
        thetae_check2 = Thetae ( tguess_2, pres/100., 100., smixr2 )

        !~ Whew doggies - that there is some accuracy...
        !IF ( ABS (tparcel-tcheck) < .05) THEN
        IF ( ABS (thetaeK-thetae_check) < .001) THEN
           found = .true.
           flag  = .true.
           EXIT
        END IF

        !~ Old
        !tparcel = tparcel + (tcheck - tparcel)*.3

        !~ New
        correction = ( thetaeK-thetae_check ) / ( thetae_check2-thetae_check )
        tparcel = tparcel + correction

        iter = iter + 1
     END DO

     IF ( .not. found ) THEN
        print*, "Warning! Thetae to temperature calculation did not converge!"
        print*, "Thetae ", thetaeK, "Pressure ", pres
     END IF

  END FUNCTION The2T



  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~
  !~ Name:
  !~    VirtualTemperature
  !~
  !~ Description:
  !~    This function returns virtual temperature given temperature ( K )
  !~    and mixing ratio.
  !~
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION VirtualTemperature ( tK, w ) result ( Tv )
  IMPLICIT NONE

     !~ Variable declaration
     real, intent ( in ) :: tK !~ Temperature
     real, intent ( in ) :: w  !~ Mixing ratio ( kg kg^-1 )
     real                :: Tv !~ Virtual temperature

     Tv = tK * ( 1.0 + (w/0.622) ) / ( 1.0 + w )

  END FUNCTION VirtualTemperature




  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~
  !~ Name:
  !~    SaturationMixingRatio
  !~
  !~ Description:
  !~    This function calculates saturation mixing ratio given the
  !~    temperature ( K ) and the ambient pressure ( Pa ).  Uses 
  !~    approximation of saturation vapor pressure.
  !~
  !~ References:
  !~    Bolton (1980), Monthly Weather Review, pg. 1047, Eq. 10
  !~
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION SaturationMixingRatio ( tK, p ) result ( ws )

    IMPLICIT NONE

    REAL, INTENT ( IN ) :: tK
    REAL, INTENT ( IN ) :: p
    REAL                :: ws

    REAL :: es

    es = 6.122 * exp ( (17.67*(tK-273.15))/ (tK-29.66) )
    ws = ( 0.622*es ) / ( (p/100.0)-es )

  END FUNCTION SaturationMixingRatio



  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~                                                                     
  !~ Name:                                                                
  !~    tlcl                                                               
  !~                                                                        
  !~ Description:                                                            
  !~    This function calculates the temperature of a parcel of air would have
  !~    if lifed dry adiabatically to it's lifting condensation level (lcl).  
  !~                                                                          
  !~ References:                                                              
  !~    Bolton (1980), Monthly Weather Review, pg. 1048, Eq. 22
  !~                                                                          
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION TLCL ( tk, rh )
    
    IMPLICIT NONE
 
    REAL, INTENT ( IN ) :: tK   !~ Temperature ( K )
    REAL, INTENT ( IN ) :: rh   !~ Relative Humidity ( % )
    REAL                :: tlcl
    
    REAL :: denom, term1, term2

    term1 = 1.0 / ( tK - 55.0 )
    IF ( rh > REAL (0) ) THEN
      term2 = ( LOG (rh/100.0)  / 2840.0 )
    ELSE
      term2 = ( LOG (0.001/1.0) / 2840.0 )
    END IF
    denom = term1 - term2
    tlcl = ( 1.0 / denom ) + REAL ( 55 ) 

  END FUNCTION TLCL



  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~                                                                          ~!
  !~ Name:                                                                    ~!
  !~    Buoyancy                                                              ~!
  !~                                                                          ~!
  !~ Description:                                                             ~!
  !~    This function computes Convective Available Potential Energy (CAPE)   ~!
  !~    with inhibition as a result of water loading given the data required  ~!
  !~    to run up a sounding.                                                 ~!
  !~                                                                          ~!
  !~    Additionally, since we are running up a sounding anyways, this        ~!
  !~    function returns the height of the Level of Free Convection (LFC) and ~!
  !~    the pressure at the LFC.  That-a-ways, we don't have to run up a      ~!
  !~    sounding later, saving a relatively computationally expensive         ~!
  !~    routine.                                                              ~!
  !~                                                                          ~!
  !~ Usage:                                                                   ~!
  !~    ostat = Buoyancy ( tK, rh, p, hgt, sfc, CAPE, ZLFC, PLFC, parcel )    ~!
  !~                                                                          ~!
  !~ Where:                                                                   ~!
  !~                                                                          ~!
  !~    IN                                                                    ~!
  !~    --                                                                    ~!
  !~    tK   = Temperature ( K )                                              ~!
  !~    rh   = Relative Humidity ( % )                                        ~!
  !~    p    = Pressure ( Pa )                                                ~!
  !~    hgt  = Geopotential heights ( m )                                     ~!
  !~    sfc  = integer rank within submitted arrays that represents the       ~!
  !~           surface                                                        ~!
  !~                                                                          ~!
  !~    OUT                                                                   ~!
  !~    ---                                                                   ~!
  !~    ostat         INTEGER return status. Nonzero is bad.                  ~!
  !~    CAPE ( J/kg ) Convective Available Potential Energy                   ~!
  !~    ZLFC ( gpm )  Height at the LFC                                       ~!
  !~    PLFC ( Pa )   Pressure at the LFC                                     ~!
  !~                                                                          ~!
  !~    tK, rh, p, and hgt are all REAL arrays, arranged from lower levels    ~!
  !~    to higher levels.                                                     ~!
  !~                                                                          ~!
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
    FUNCTION Buoyancy ( nz, tk, rh, p, hgt, sfc, cape, zlfc, plfc, parcel ) &
               result (ostat)
  
      IMPLICIT NONE
  
      INTEGER, INTENT ( IN )  :: nz          !~ Number of vertical levels
      INTEGER, INTENT ( IN )  :: sfc         !~ Surface level in the profile
      REAL,    INTENT ( IN )  :: tk   ( nz ) !~ Temperature profile ( K )
      REAL,    INTENT ( IN )  :: rh   ( nz ) !~ Relative Humidity profile ( % )
      REAL,    INTENT ( IN )  :: p    ( nz ) !~ Pressure profile ( Pa )
      REAL,    INTENT ( IN )  :: hgt  ( nz ) !~ Height profile ( gpm )
      REAL,    INTENT ( OUT ) :: cape        !~ CAPE ( J kg^-1 )
      REAL,    INTENT ( OUT ) :: zlfc        !~ LFC Height ( gpm )
      REAL,    INTENT ( OUT ) :: plfc        !~ LFC Pressure ( Pa )
      INTEGER                 :: ostat       !~ Function return status
                                             !~ Nonzero is bad.

      INTEGER, INTENT ( IN  ) :: parcel      !~ Most Unstable = 1 (default)
                                             !~ Mean layer    = 2
                                             !~ Surface based = 3
  
      !~ Derived profile variables
      !  -------------------------
      REAL                    :: ws   ( nz ) !~ Saturation mixing ratio
      REAL                    :: w    ( nz ) !~ Mixing ratio
      REAL                    :: buoy ( nz ) !~ Buoyancy
      REAL                    :: tlclK       !~ LCL temperature ( K )
      REAL                    :: plcl        !~ LCL pressure ( Pa )
      REAL                    :: nbuoy       !~ Negative buoyancy
      REAL                    :: pbuoy       !~ Positive buoyancy
  
      !~ Source parcel information
      !  -------------------------
      REAL                    :: srctK       !~ Source parcel temperature ( K )
      REAL                    :: srcrh       !~ Source parcel rh ( % )
      REAL                    :: srcws       !~ Source parcel sat. mixing ratio
      REAL                    :: srcw        !~ Source parcel mixing ratio
      REAL                    :: srcp        !~ Source parcel pressure ( Pa )
      REAL                    :: srctheta    !~ Source parcel theta ( K )
      REAL                    :: srcthetaeK  !~ Source parcel theta-e ( K )
      INTEGER                 :: srclev      !~ Level of the source parcel
      INTEGER                 :: sfcoff      !~ Surface offset
      REAL                    :: spdiff      !~ Pressure difference
   
      !~ Parcel variables
      !  ----------------
      REAL                    :: ptK        !~ Parcel temperature ( K )
      REAL                    :: ptvK       !~ Parcel virtual temperature ( K )
      REAL                    :: tvK        !~ Ambient virtual temperature ( K )
      REAL                    :: pw         !~ Parcel mixing ratio
  
      !~ Other utility variables
      !  -----------------------
      INTEGER                 :: i, j, k    !~ Dummy iterator
      INTEGER                 :: lfclev     !~ Level of LFC
      INTEGER                 :: prcl       !~ Internal parcel type indicator
      INTEGER                 :: mlev       !~ Level for ML calculation
      INTEGER                 :: lyrcnt     !~ Number of layers in mean layer
      LOGICAL                 :: flag       !~ Dummy flag
      LOGICAL                 :: wflag      !~ Saturation flag
      REAL                    :: freeze     !~ Water loading multiplier
      REAL                    :: CIN        !~ Convective inhibition
      REAL                    :: pdiff      !~ Pressure difference between levs 
  
      !~ Thermo / dynamical constants
      !  ----------------------------
      REAL                    :: Rd         !~ Dry gas constant
         PARAMETER ( Rd = 287.058 )         !~ J deg^-1 kg^-1
      REAL                    :: Cp         !~ Specific heat constant pressure
         PARAMETER ( Cp = 1004.67 )         !~ J deg^-1 kg^-1
      REAL                    :: g          !~ Acceleration due to gravity
         PARAMETER ( g  = 9.80665 )         !~ m s^-2
      REAL                    :: RUNDEF
         PARAMETER ( RUNDEF = -9.999E30 )
  
      !~ Initialize variables
      !  --------------------
      ostat  = 0
      CAPE   = REAL ( 0 )
      ZLFC   = RUNDEF
      PLFC   = RUNDEF
  
      !~ Look for submitted parcel definition
      !~ 1 = Most unstable
      !~ 2 = Mean layer
      !~ 3 = Surface based
      !  -------------------------------------
      IF ( parcel > 3 .or. parcel < 1 ) THEN
         !WRITE( *,* ) ' WARNING: User submitted parcel not valid.'
         !WRITE( *,* ) ' Defaulting to MU parcel.'
         !return
         prcl = 1
      ELSE
         prcl =  parcel
      END IF
  
      !~ Initalize our parcel to be (sort of) surface based.  Because of
      !~ issues we've been observing in the WRF model, specifically with
      !~ excessive surface moisture values at the surface, using a true
      !~ surface based parcel is resulting a more unstable environment
      !~ than is actually occuring.  To address this, our surface parcel
      !~ is now going to be defined as the parcel between 25-50 hPa
      !~ above the surface.
      !  ----------------------------------------------------------------
  
      !~ Compute mixing ratio values for the layer
      !  -----------------------------------------
      DO k = sfc, nz
        ws  ( k )   = SaturationMixingRatio ( tK(k), p(k) )
        w   ( k )   = ( rh(k)/100.0 ) * ws ( k )
      END DO
  
      sfcoff=0
      DO k = 2, nz
         spdiff = ( p (1) - p (k) ) / REAL ( 100 )
         IF ( spdiff >= 25. .and. spdiff <= 50. ) THEN
            sfcoff = ( k - 1 )
            EXIT
         END IF
      END DO
   
      sfcoff = 0  ! This negates the 25-50 hPa work-around above
   
      srclev      = sfc+sfcoff
      srctK       = tK    ( sfc+sfcoff )
      srcrh       = rh    ( sfc+sfcoff )
      srcp        = p     ( sfc+sfcoff )
      srcws       = ws    ( sfc+sfcoff )
      srcw        = w     ( sfc+sfcoff )
      srctheta    = Theta ( tK(sfc+sfcoff), p(sfc+sfcoff)/100.0 )
   
      !~ Compute the profile mixing ratio.  If the parcel is the MU parcel,
      !~ define our parcel to be the most unstable parcel below 700 hPa
      !  -------------------------------------------------------------------
      mlev = sfc + 1
      DO k = sfc + 1, nz
   
         !~ Identify the last layer within 100 hPa of the surface
         !  -----------------------------------------------------
         pdiff = ( p (k) - p (sfc) ) / REAL ( 100 )
         IF ( pdiff <= REAL (100) ) mlev = k
   
         IF ( prcl == 1 ) THEN
            IF ( (p(k) > 70000.0) .and. (w(k) > srcw) ) THEN
               srctheta = Theta ( tK(k), p(k)/100.0 )
               srcw = w ( k )
               srclev  = k
               srctK   = tK ( k )
               srcrh   = rh ( k )
               srcp    = p  ( k )
            END IF
         END IF
   
      END DO
   
      !~ If we want the mean layer parcel, compute the mean values in the
      !~ lowest 100 hPa.
      !  ----------------------------------------------------------------
      lyrcnt =  mlev - sfc + 1
      IF ( prcl == 2 ) THEN
   
         srclev   = sfc
         srctK    = SUM ( tK (sfc:mlev) ) / REAL ( lyrcnt )
         srcw     = SUM ( w  (sfc:mlev) ) / REAL ( lyrcnt )
         srcrh    = SUM ( rh (sfc:mlev) ) / REAL ( lyrcnt )
         srcp     = SUM ( p  (sfc:mlev) ) / REAL ( lyrcnt )
         srctheta = Theta ( srctK, srcp/100. )
   
      END IF
   
      !~ Chirp status as necessary.
      !  --------------------------
  !       WRITE ( *,* ) ''
  !       WRITE ( *,* ) ' ==================================== '
  !       WRITE ( *,* ) ' Now in Buoyancy '
  !       WRITE ( *,* ) ''
  !       WRITE ( *,* ) ' User submitted data: '
  !       WRITE ( *,'(a,I7)' ) '  Number of vertical levels: ', nz
  !       WRITE ( *,'(6A12)' ) 'Level', 'Temp', 'RH', 'Pres', 'Hgt', 'MixRat'
  !       DO i = 1, nz
  !          WRITE ( *,'(i12,5f12.3)' ) i, tK ( i ), rh ( i ) &
  !                , p ( i )/REAL ( 100 ), hgt ( i ), w ( i )*REAL ( 1000 )
  !       END DO
  !       WRITE ( *,* ) ' Surface level: ', sfc
  !       WRITE ( *,* ) ''
   
      srcthetaeK = Thetae ( srctK, srcp/100.0, srcrh, srcw )
   
      !~ Chirp status again
      !  ------------------
  !    10 FORMAT ( A15,F12.3,A8 )
  !       WRITE ( *,* ) 'Source parcel values: '
  !       WRITE ( *,'(A15,I8)' ) ' Source parcel level: ', srclev
  !       WRITE ( *,FMT=10 ) ' Mixing Ratio:', srcw * REAL ( 1000 ), 'g/kg'
  !       WRITE ( *,FMT=10 ) ' Temperature:', srctK, 'K'
  !       WRITE ( *,FMT=10 ) ' RH:',srcrh, '%'
  !       WRITE ( *,FMT=10 ) ' Pressure:', srcp/REAL (100), 'hPa'
  !       WRITE ( *,FMT=10 ) ' Theta-E:', srcthetaeK, 'K'
  !       WRITE ( *,* ) ''
   
   
      !~ Calculate temperature and pressure of the LCL
      !  ---------------------------------------------
      tlclK = TLCL ( tK(srclev), rh(srclev) )
      plcl  = p(srclev) * ( (tlclK/tK(srclev))**(Cp/Rd) )
   
      !~ Chirp
      !  -----
  !       WRITE ( *,* ) ' LCL Temperature: ', tlclK
  !       WRITE ( *,* ) ' LCL Pressure:    ', plcl / REAL ( 100 )
  !       WRITE ( *,* ) ''
  !       WRITE ( *,* ) ' Now lifting parcel...'
  !       WRITE ( *,'(7A15)') 'Level', 'Pressure', 'Parcel Tmp', 'Parcel Mixr' &
  !                         , 'Parcel Tv', 'Ambient Tv', 'Buoyancy'
   
   
      buoy  = REAL ( 0 )
      pw    = srcw
      wflag = .false.
      DO k  = srclev, nz
         IF ( tK (k) < 253.15 ) EXIT
         IF ( p (k) <= plcl ) THEN
   
            !~ The first level after we pass the LCL, we're still going to
            !~ lift the parcel dry adiabatically, as we haven't added the
            !~ the required code to switch between the dry adiabatic and moist
            !~ adiabatic cooling.  Since the dry version results in a greater
            !~ temperature loss, doing that for the first step so we don't over
            !~ guesstimate the instability.
            !  ----------------------------------------------------------------
   
            IF ( wflag ) THEN
               flag  = .false.
   
               !~ Above the LCL, our parcel is now undergoing moist adiabatic
               !~ cooling.  Because of the latent heating being undergone as
               !~ the parcel rises above the LFC, must iterative solve for the
               !~ parcel temperature using equivalant potential temperature,
               !~ which is conserved during both dry adiabatic and
               !~ pseudoadiabatic displacements.
               !  --------------------------------------------------------------
               ptK   = The2T ( srcthetaeK, p(k), flag )
   
               !~ Calculate the parcel mixing ratio, which is now changing
               !~ as we condense moisture out of the parcel, and is equivalent
               !~ to the saturation mixing ratio, since we are, in theory, at
               !~ saturation.
               !  ------------------------------------------------------------
               pw = SaturationMixingRatio ( ptK, p(k) )
   
               !~ Now we can calculate the virtual temperature of the parcel
               !~ and the surrounding environment to assess the buoyancy.
               !  ----------------------------------------------------------
               ptvK  = VirtualTemperature ( ptK, pw )
               tvK   = VirtualTemperature ( tK (k), w (k) )
   
               !~ Calculate the buoyancy at the level
               !  -----------------------------------
               !buoy ( k ) = g * ( (ptvK - tvK)/tvK )
   
               !~ Modification to account for water loading
               !  -----------------------------------------
               freeze = 0.033 * ( 263.15 - pTvK )
               IF ( freeze > 1.0 ) freeze = 1.0
               IF ( freeze < 0.0 ) freeze = 0.0
   
               !~ Approximate how much of the water vapor has condensed out
               !~ of the parcel at this level
               !  ---------------------------------------------------------
               freeze = freeze * 333700.0 * ( srcw - pw ) / 1005.7
   
               pTvK = pTvK - pTvK * ( srcw - pw ) + freeze
               buoy ( k ) = g * ( (ptvK - tvK)/tvK )
   
            ELSE
   
               !~ Since the theta remains constant whilst undergoing dry
               !~ adiabatic processes, can back out the parcel temperature
               !~ from potential temperature below the LCL
               !  --------------------------------------------------------
               ptK   = srctheta / ( 100000.0/p(k) )**(Rd/Cp)
   
               !~ Grab the parcel virtual temperture, can use the source
               !~ mixing ratio since we are undergoing dry adiabatic cooling
               !  ----------------------------------------------------------
               ptvK  = VirtualTemperature ( ptK, srcw )
   
               !~ Virtual temperature of the environment
               !  --------------------------------------
               tvK   = VirtualTemperature ( tK (k), w (k) )
   
               !~ Buoyancy at this level
               !  ----------------------
               buoy ( k ) = g * ( (ptvK - tvK)/tvK )
   
               wflag = .true.
   
            END IF
   
         ELSE
   
            !~ Since the theta remains constant whilst undergoing dry
            !~ adiabatic processes, can back out the parcel temperature
            !~ from potential temperature below the LCL
            !  --------------------------------------------------------
            ptK   = srctheta / ( 100000.0/p(k) )**(Rd/Cp)
   
            !~ Grab the parcel virtual temperture, can use the source
            !~ mixing ratio since we are undergoing dry adiabatic cooling
            !  ----------------------------------------------------------
            ptvK  = VirtualTemperature ( ptK, srcw )
   
            !~ Virtual temperature of the environment
            !  --------------------------------------
            tvK   = VirtualTemperature ( tK (k), w (k) )
   
            !~ Buoyancy at this level
            !  ---------------------
            buoy ( k ) = g * ( (ptvK - tvK)/tvK )
   
         END IF
   
         !~ Chirp
         !  -----
  !          WRITE ( *,'(I15,6F15.3)' )k,p(k)/100.,ptK,pw*1000.,ptvK,tvK,buoy(k)
   
      END DO
   
      !~ Add up the buoyancies, find the LFC
      !  -----------------------------------
      flag   = .false.
      lfclev = -1
      nbuoy  = REAL ( 0 )
      pbuoy = REAL ( 0 )
      DO k = sfc + 1, nz
         IF ( tK (k) < 253.15 ) EXIT
         CAPE = CAPE + MAX ( buoy (k), 0.0 ) * ( hgt (k) - hgt (k-1) )
         CIN  = CIN  + MIN ( buoy (k), 0.0 ) * ( hgt (k) - hgt (k-1) )
   
         !~ If we've already passed the LFC
         !  -------------------------------
         IF ( flag .and. buoy (k) > REAL (0) ) THEN
            pbuoy = pbuoy + buoy (k)
         END IF
   
         !~ We are buoyant now - passed the LFC
         !  -----------------------------------
         IF ( .not. flag .and. buoy (k) > REAL (0) .and. p (k) < plcl ) THEN
            flag = .true.
            pbuoy = pbuoy + buoy (k)
            lfclev = k
         END IF
   
         !~ If we think we've passed the LFC, but encounter a negative layer
         !~ start adding it up.
         !  ----------------------------------------------------------------
         IF ( flag .and. buoy (k) < REAL (0) ) THEN
            nbuoy = nbuoy + buoy (k)

            !~ If the accumulated negative buoyancy is greater than the
            !~ positive buoyancy, then we are capped off.  Got to go higher
            !~ to find the LFC. Reset positive and negative buoyancy summations
            !  ----------------------------------------------------------------
            IF ( ABS (nbuoy) > pbuoy ) THEN
               flag   = .false.
               nbuoy  = REAL ( 0 )
               pbuoy  = REAL ( 0 )
               lfclev = -1
            END IF
         END IF
   
      END DO
   
      !~ Assuming the the LFC is at a pressure level for now
      !  ---------------------------------------------------
      IF ( lfclev > 0 ) THEN
         PLFC = p   ( lfclev )
         ZLFC = hgt ( lfclev )
      END IF
   
      IF ( PLFC /= PLFC .OR. PLFC < REAL (0) ) THEN
         PLFC = REAL ( -1 )
         ZLFC = REAL ( -1 )
      END IF
   
      IF ( CAPE /= CAPE ) cape = REAL ( 0 )
   
      !~ Chirp
      !  -----
  !       WRITE ( *,* ) ' CAPE: ', cape, ' CIN:  ', cin
  !       WRITE ( *,* ) ' LFC:  ', ZLFC, ' PLFC: ', PLFC
  !       WRITE ( *,* ) ''
  !       WRITE ( *,* ) ' Exiting buoyancy.'
  !       WRITE ( *,* ) ' ==================================== '
  !       WRITE ( *,* ) ''
   
  END FUNCTION Buoyancy 

END MODULE module_diag_afwa
#endif
