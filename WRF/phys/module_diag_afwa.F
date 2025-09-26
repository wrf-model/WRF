#if (NMM_CORE == 1)
MODULE module_diag_afwa
CONTAINS
   SUBROUTINE diag_afwa_stub
   END SUBROUTINE diag_afwa_stub
END MODULE module_diag_afwa
#else

!WRF:MEDIATION_LAYER:PHYSICS

MODULE module_diag_afwa

    USE diag_functions

CONTAINS

  SUBROUTINE afwa_diagnostics_driver (   grid , config_flags     &
                             , moist                             &
                             , scalar                            &
                             , chem                              &
                             , th_phy , pi_phy , p_phy           &
                             , u_phy , v_phy                     &
                             , dz8w , p8w , t8w , rho_phy , rho  &
                             , ids, ide, jds, jde, kds, kde      &
                             , ims, ime, jms, jme, kms, kme      &
                             , ips, ipe, jps, jpe, kps, kpe      &
                             , its, ite, jts, jte                &
                             , k_start, k_end               )

    USE module_domain, ONLY : domain , domain_clock_get
    USE module_configure, ONLY : grid_config_rec_type, model_config_rec
    USE module_state_description
    USE module_model_constants
    USE module_utility
    USE module_streams, ONLY: history_alarm, auxhist2_alarm
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
                                              ,          u_phy  &
                                              ,          v_phy  &
                                              ,           dz8w  &
                                              ,            p8w  &
                                              ,            t8w  &
                                              ,        rho_phy  &
                                              ,            rho

    ! Local
    ! -----
    CHARACTER*512 :: message
    CHARACTER*256 :: timestr 
    INTEGER :: i,j,k,nz,ostat
    INTEGER :: icing_opt
    REAL :: bdump
    INTEGER :: i_start, i_end, j_start, j_end
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) ::      qrain  &
                                              ,          qsnow  &
                                              ,          qgrpl  &
                                              ,          qvapr  &
                                              ,         qcloud  &
                                              ,           qice  &
                                              ,         ncloud  &
                                              ,         ngraup  &
                                              ,             rh  &
                                              ,         rh_cld  &
                                              ,           ptot  &
                                              ,            z_e  &
                                              ,           zagl

    REAL, DIMENSION( ims:ime, jms:jme, 5 ) ::            dustc
    REAL, DIMENSION( ims:ime, jms:jme ) ::                rh2m  &
                                              ,          rh20m  &
                                              ,           tv2m  &
                                              ,          tv20m  &
                                              ,        wind10m  &
                                              ,       wup_mask  &
                                              ,       wind125m  &
                                              ,           llws  &
                                              ,         pwater

    LOGICAL :: do_buoy_calc
    REAL :: zlfc_msl, dum1, dum2, dum3, wind_vel, wind_blend
    REAL :: prate_mm_per_hr, factor
    REAL :: u1km, v1km, ublend, vblend, u2000, v2000, us, vs
    LOGICAL :: is_target_level

    ! Timing
    ! ------
    TYPE(WRFU_Time) :: hist_time, aux2_time, CurrTime, StartTime
    TYPE(WRFU_TimeInterval) :: dtint, histint, aux2int
    LOGICAL :: is_after_history_dump, is_output_timestep, is_first_timestep
    INTEGER , PARAMETER :: DEBUG_LEVEL = 1

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
         simulationStartTime=StartTime, &            
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

    ! Following uses an overloaded .eq.
    ! ---------------------------------
    is_first_timestep = ( Currtime .eq. StartTime + dtint )
        
    ! Here is an optional check for bad data in case the model has gone
    ! off the hinges unchecked until now.  This happens under certain
    ! configurations and can lead to very wrong data writes.  This is just
    ! a simple check for sane winds and potential temperatures.
    ! --------------------------------------------------------------------
    IF ( config_flags%afwa_bad_data_check .GT. 0 ) THEN
      IF ( ( is_output_timestep ) .AND. ( .NOT. is_first_timestep ) ) THEN      
        DO i=its, MIN( ide-1, ite )
          DO k=k_start, k_end
            DO j=jts, MIN( jde-1, jte )
              IF ( ( u_phy(i,k,j)  .GT.  300.  ) .OR. &
                   ( u_phy(i,k,j)  .LT. -300.  ) .OR. &
                   ( v_phy(i,k,j)  .GT.  300.  ) .OR. &
                   ( v_phy(i,k,j)  .LT. -300.  ) .OR. &
                   ( th_phy(i,k,j) .GT.  9999. ) .OR. &
                   ( th_phy(i,k,j) .LT.  99.   ) ) THEN
                write ( message, * ) "AFWA Diagnostics: ERROR - Model winds and/or " // &
                "potential temperature appear to be bad. If you do not want this check, " // &
                "set afwa_bad_data_check=0. i=",i,", j=",j,", k=",k,", u_phy=",u_phy(i,k,j), &
                ", v_phy=", v_phy(i,k,j),", th_phy=",th_phy(i,k,j)
                CALL wrf_error_fatal( message )
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
    ENDIF

    ! 3-D arrays for moisture variables
    ! ---------------------------------

    IF ( F_QV ) THEN
      DO i=ims, ime
        DO k=kms, kme
          DO j=jms, jme
            qvapr(i,k,j) = moist(i,k,j,P_QV)
          ENDDO
        ENDDO
      ENDDO
    END IF
    IF ( F_QR ) THEN
      DO i=ims, ime
        DO k=kms, kme
          DO j=jms, jme
            qrain(i,k,j) = moist(i,k,j,P_QR)
          ENDDO
        ENDDO
      ENDDO
    END IF
    IF ( F_QS ) THEN
      DO i=ims, ime
        DO k=kms, kme
          DO j=jms, jme
            qsnow(i,k,j) = moist(i,k,j,P_QS)
          ENDDO
        ENDDO
      ENDDO
    END IF
    IF ( F_QG ) THEN
      DO i=ims, ime
        DO k=kms, kme
          DO j=jms, jme
            qgrpl(i,k,j) = moist(i,k,j,P_QG)
          ENDDO
        ENDDO
      ENDDO
    END IF
    IF ( F_QC ) THEN
      DO i=ims, ime
        DO k=kms, kme
          DO j=jms, jme
            qcloud(i,k,j) = moist(i,k,j,P_QC)
          ENDDO
        ENDDO
      ENDDO
    END IF
    IF ( F_QI ) THEN
      DO i=ims, ime
        DO k=kms, kme
          DO j=jms, jme
            qice(i,k,j) = moist(i,k,j,P_QI)
          ENDDO
        ENDDO
      ENDDO
    END IF
    IF ( F_QNC ) THEN
      DO i=ims, ime
        DO k=kms, kme
          DO j=jms, jme
            ncloud(i,k,j) = scalar(i,k,j,P_QNC)
          ENDDO
        ENDDO
      ENDDO
    END IF
    IF ( F_QNG ) THEN
      DO i=ims, ime
        DO k=kms, kme
          DO j=jms, jme
            ngraup(i,k,j) = scalar(i,k,j,P_QNG)
          ENDDO
        ENDDO
      ENDDO
    END IF
    
    ! Total pressure
    ! -------------- 
    DO i=ims, ime
      DO k=kms, kme
        DO j=jms, jme
          ptot(i,k,j)=grid%pb(i,k,j)+grid%p(i,k,j)
        ENDDO
      ENDDO
    ENDDO

    ! ZAGL (height above ground)
    ! --------------------------
    DO i=ims, ime
      DO k=kms, kme
        DO j=jms, jme
          zagl(i,k,j)=grid%z(i,k,j)-grid%ht(i,j)
        ENDDO
      ENDDO
    ENDDO

    ! Calculate relative humidity
    ! ---------------------------
    IF ( F_QV ) THEN
      DO i=ims,ime
        DO k=kms,kme    
          DO j=jms,jme
            rh(i,k,j)=calc_rh(ptot(i,k,j),grid%t_phy(i,k,j), qvapr(i,k,j))
          ENDDO
        ENDDO
      ENDDO
    ELSE
      CALL wrf_debug ( DEBUG_LEVEL, 'Option rh requires: QV' )
    END IF
    IF ( F_QV .AND. F_QC .AND. F_QI ) THEN
      DO i=ims,ime
        DO k=kms,kme    
          DO j=jms,jme
            rh_cld(i,k,j)=calc_rh(ptot(i,k,j),grid%t_phy(i,k,j), qvapr(i,k,j)+qcloud(i,k,j)+qice(i,k,j))
          ENDDO
        ENDDO
      ENDDO
    ELSE
      CALL wrf_debug ( DEBUG_LEVEL, 'Option rh_cld requires: QV, QC, QI' )
    END IF

    ! Time-step precipitation (convective + nonconvective)
    ! --------------------------------------------------------------
    DO i=ims,ime
      DO j=jms,jme
        grid % afwa_precip(i,j) = grid%raincv(i,j) + grid%rainncv(i,j)
        grid % afwa_totprecip(i,j) = grid%rainc(i,j) + grid%rainnc(i,j)
      ENDDO
    ENDDO

    ! Calculate precipitable water
    ! ----------------------------
    IF ( F_QV .AND. F_QC ) THEN
      nz=kme-kms+1
      DO i=ims,ime
        DO j=jms,jme
          grid % afwa_pwat ( i, j ) = Pwat( nz,                  &
                                            qvapr(i,kms:kme,j),  &
                                            qcloud(i,kms:kme,j), &
                                            dz8w(i,kms:kme,j),   &
                                            rho(i,kms:kme,j) )
        ENDDO
      ENDDO
    ELSE
      CALL wrf_debug ( DEBUG_LEVEL, 'Option pwat requires: QV, QC' )
    END IF

    ! After each history dump, reset max/min value arrays
    ! ----------------------------------------------------------------------
    IF ( is_after_history_dump ) THEN
      IF ( config_flags % afwa_severe_opt == 1 ) THEN
        DO j = jms, jme
          DO i = ims, ime
            grid % wspd10max(i,j) = 0.
            grid % afwa_llws(i,j) = 0.
          ENDDO
        ENDDO
      ENDIF
    ENDIF 

    ! Calculate the max 10 m wind speed between output times
    ! ------------------------------------------------------
    ! UPDATE 20150112 - GAC
    ! Diagnose from model 10 m winds, and blend with 1 km AGL
    ! winds when precipitation rate is > 50 mm/hr to account
    ! for increased surface wind gust potential when precip
    ! is heavy and when winds aloft are strong.  Will use the
    ! higher of the surface and the blended winds. Blending
    ! is linear weighted between 50-150 mm/hr precip rates.
    ! -------------------------------------------------------
    IF ( config_flags % afwa_severe_opt == 1 ) THEN
      DO j = jms, jme
        DO i = ims, ime
          wind_vel = uv_wind ( grid % u10(i,j) , grid % v10(i,j) )
          prate_mm_per_hr = ( grid % afwa_precip(i,j) / grid % dt ) * 3600.
  
          ! Is this an area of heavy precip?  Calculate 1km winds to blend down
          ! -------------------------------------------------------------------
          IF ( prate_mm_per_hr .GT. 50. ) THEN
            is_target_level=.false.
            DO k=kms,kme    
              IF ( ( zagl(i,k,j) >= 1000. ) .and. &
                   ( .NOT. is_target_level ) .and. &
                   ( k .ne. kms ) ) THEN
                is_target_level = .true.
                u1km = u_phy(i,k-1,j) + (1000. - (zagl(i,k-1,j))) &
                       * ((u_phy(i,k,j) - u_phy(i,k-1,j))/(zagl(i,k,j)))
                v1km = v_phy(i,k-1,j) + (1000. - (zagl(i,k-1,j))) &
                       * ((v_phy(i,k,j) - v_phy(i,k-1,j))/(zagl(i,k,j)))
                EXIT ! We've found our level, break the loop
              ENDIF
            ENDDO
            
            ! Compute blended wind
            ! --------------------
            factor = MAX ( ( ( 150. - prate_mm_per_hr ) / 100. ), 0. )
            ublend = grid % u10(i,j) * factor + u1km * (1. - factor)
            vblend = grid % v10(i,j) * factor + v1km * (1. - factor)
            wind_blend = uv_wind ( ublend, vblend )
  
            ! Set the surface wind to the blended wind if higher
            ! --------------------------------------------------
            IF ( wind_blend .GT. wind_vel ) THEN
              wind_vel = wind_blend
            ENDIF
          ENDIF
  
          IF ( wind_vel .GT. grid % wspd10max(i,j) ) THEN
            grid % wspd10max(i,j) = wind_vel
          ENDIF
        ENDDO
      ENDDO
    ENDIF

    ! Calculate 0-2000 foot (0 - 609.6 meter) shear.
    ! ----------------------------------------------
    IF ( config_flags % afwa_severe_opt == 1 ) THEN
    DO j = jts, jte
      DO i = its, ite
        is_target_level=.false.
        DO k=kms,kme    
          IF ( ( zagl(i,k,j) >= 609.6 ) .and. &
               ( .NOT. is_target_level ) .and. &
               ( k .ne. kms ) ) THEN
            is_target_level = .true.
            u2000 = u_phy(i,k-1,j) + (609.6 - (zagl(i,k-1,j))) &
                    * ((u_phy(i,k,j) - u_phy(i,k-1,j))/(zagl(i,k,j)))
            v2000 = v_phy(i,k-1,j) + (609.6 - (zagl(i,k-1,j))) &
                    * ((v_phy(i,k,j) - v_phy(i,k-1,j))/(zagl(i,k,j)))
            us = u2000 - grid % u10(i,j) 
            vs = v2000 - grid % v10(i,j) 
            llws(i,j) = uv_wind ( us , vs )
            IF ( llws(i,j) .gt. grid % afwa_llws(i,j) ) THEN
              grid % afwa_llws(i,j) = llws(i,j)
            ENDIF
            EXIT ! We've found our level, break the loop
          ENDIF
        ENDDO
      ENDDO
    ENDDO
    ENDIF

#if ( WRF_CHEM == 1 )
    ! Surface dust concentration array (ug m-3)
    ! ----------------------------------------- 
    DO i=ims, ime
      DO j=jms, jme
        dustc(i,j,1)=chem(i,k_start,j,p_dust_1)*rho(i,k_start,j)
        dustc(i,j,2)=chem(i,k_start,j,p_dust_2)*rho(i,k_start,j)
        dustc(i,j,3)=chem(i,k_start,j,p_dust_3)*rho(i,k_start,j)
        dustc(i,j,4)=chem(i,k_start,j,p_dust_4)*rho(i,k_start,j)
        dustc(i,j,5)=chem(i,k_start,j,p_dust_5)*rho(i,k_start,j)
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
            grid%w_up_max(i,j) = 0.
            grid%w_dn_max(i,j) = 0.
            grid%tcoli_max(i,j) = 0.
            grid%grpl_flx_max(i,j) = 0.
            grid%up_heli_max(i,j) = 0.
            grid%afwa_tornado(i,j) = 0.
            grid%midrh_min_old(i,j) = grid%midrh_min(i,j) ! Save old midrh_min
            grid%midrh_min(i,j) = 999.
            grid%afwa_hail(i,j) = 0.
          ENDDO
        ENDDO
      ENDIF  ! is_after_history_dump

      IF ( ( is_first_timestep ) .OR. ( is_output_timestep ) ) THEN
        do_buoy_calc = .true.
      ELSE
        do_buoy_calc = .false.
      ENDIF

      IF ( F_QV .AND. F_QR .AND. F_QC .AND. F_QI .AND. F_QS .AND. F_QG .AND. F_QNG ) THEN
        !-->RAS
        ! We need to do some neighboring gridpoint comparisons in this next function;
        ! set these values so we don't go off the edges of the domain.  Updraft
        ! duration on domain edges will always be 0.
        ! ----------------------------------------------------------------------
        i_start = its
        i_end   = ite
        j_start = jts
        j_end   = jte

        IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
             config_flags%nested) i_start = MAX( ids+1, its )
        IF ( config_flags%open_xe .OR. config_flags%specified .OR. &
             config_flags%nested) i_end   = MIN( ide-1, ite )
        IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
             config_flags%nested) j_start = MAX( jds+1, jts )
        IF ( config_flags%open_ye .OR. config_flags%specified .OR. &
             config_flags%nested) j_end   = MIN( jde-1, jte )
        IF ( config_flags%periodic_x ) i_start = its
        IF ( config_flags%periodic_x ) i_end = ite

        CALL severe_wx_diagnostics ( grid % wspd10max             &
                               , grid % w_up_max                  &
                               , grid % w_dn_max                  &
                               , grid % up_heli_max               &
                               , grid % tcoli_max                 &
                               , grid % midrh_min_old             &
                               , grid % midrh_min                 &
                               , grid % afwa_hail                 &
                               , grid % afwa_cape                 &
                               , grid % afwa_cin                  &
                               , grid % afwa_zlfc                 &
                               , grid % afwa_plfc                 &
                               , grid % afwa_lidx                 &
                               , llws                             &
                               , grid % afwa_tornado              &
                               , grid % grpl_flx_max              &
                               , grid % u10                       &
                               , grid % v10                       &
                               , grid % w_2                       &
                               , grid % uh                        &
                               , grid % t_phy                     &
                               , grid % t2                        &
                               , grid % z                         &
                               , grid % ht                        &
                               , grid % tornado_mask              &
                               , grid % tornado_dur               &
                               , grid % dt                        &
                               , grid % afwa_pwat                 &
                               , u_phy                            &
                               , v_phy                            &
                               , ptot                             &
                               , qice                             &
                               , qsnow                            &
                               , qgrpl                            &
                               , ngraup                           &
                               , qvapr, qrain, qcloud             &
                               , rho                              &
                               , dz8w                             &
                               , rh                               &
                               , do_buoy_calc                     &
                               , ims, ime, jms, jme, kms, kme     &
                               , its, ite, jts, jte               &
                               , k_start, k_end                   &
                               , j_start, j_end, i_start, i_end   )

      ELSE
        CALL wrf_debug ( DEBUG_LEVEL, 'Option severe_wx_diagnostics requires: QV, QR, QC, QI, QS, QG, QNG' )
      END IF
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
  
      ! Diagnose precipitation type
      ! ---------------------------
      CALL precip_type_diagnostics ( grid % t_phy               &
                             , grid % t2                        &
                             , rh                               &
                             , grid % z                         &
                             , dz8w                             &
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
  
    ! --------------------------------------------------------------
    ! The following packages are calculated only on output timesteps
    ! --------------------------------------------------------------
    IF ( is_output_timestep ) THEN      

      ! Calculate mean sea level pressure
      ! ---------------------------------
      DO j = jms, jme
        DO i = ims, ime
          grid % afwa_mslp  ( i, j ) =   MSLP ( grid % ht ( i, j )           & 
                                              , grid % psfc ( i, j )         &
                                              , grid % z ( i, kms, j )       & 
                                              , qvapr ( i, kms, j )          &  
                                              , grid % t_phy ( i, kms, j ) )
        ENDDO
      ENDDO

      ! Calculate 10 meter winds
      ! ------------------------
      DO i=ims,ime
        DO j=jms,jme
          wind10m(i,j)=uv_wind(grid%u10(i,j),grid%v10(i,j))
        ENDDO
      ENDDO

      ! Calculate 2 meter relative humidity/Tv
      ! --------------------------------------
      DO i=ims,ime
        DO j=jms,jme
          rh2m(i,j)=calc_rh(grid%psfc(i,j), grid%t2(i,j), grid%q2(i,j))
          tv2m(i,j)=grid%t2(i,j) * (1 + 0.61 * grid%q2(i,j))
        ENDDO
      ENDDO
 
      ! Calculate buoyancy parameters.
      ! ------------------------------
      IF ( config_flags % afwa_buoy_opt == 1 ) THEN
        nz = k_end - k_start + 1

        ! Calculate buoyancy for surface parcel
        ! -------------------------------------
        DO j = jts, jte
          DO i = its, ite
            ostat = Buoyancy (                                   nz &
                                     ,      grid%t_phy(i,kms:kme,j) &
                                     ,              rh(i,kms:kme,j) &
                                     ,            ptot(i,kms:kme,j) &
                                     ,        grid % z(i,kms:kme,j) &
                                     ,                            1 &
                                     ,        grid % afwa_cape(i,j) &
                                     ,         grid % afwa_cin(i,j) &
                                     ,                     zlfc_msl &
                                     ,        grid % afwa_plfc(i,j) &
                                     ,        grid % afwa_lidx(i,j) &
                                     ,                        3 ) !Surface
        
            ! Subtract terrain height to convert ZLFC from MSL to AGL
            ! -------------------------------------------------------
            IF ( zlfc_msl .ge. grid % ht ( i, j ) ) THEN
              grid % afwa_zlfc ( i, j ) = zlfc_msl - grid % ht ( i, j )
            ELSE
              grid % afwa_zlfc( i, j ) = -1.
            ENDIF

            ! Add 273.15 to lifted index per some standard I don't understand
            ! ---------------------------------------------------------------
            IF ( grid % afwa_lidx ( i, j ) .ne. 999. ) THEN
              grid % afwa_lidx ( i, j ) = grid % afwa_lidx ( i, j ) + 273.15
            ENDIF
 
            ! Calculate buoyancy again for most unstable layer
            ! ------------------------------------------------
            ostat = Buoyancy (                                   nz &
                                     ,      grid%t_phy(i,kms:kme,j) &
                                     ,              rh(i,kms:kme,j) &
                                     ,            ptot(i,kms:kme,j) &
                                     ,        grid % z(i,kms:kme,j) &
                                     ,                            1 &
                                     ,     grid % afwa_cape_mu(i,j) &
                                     ,      grid % afwa_cin_mu(i,j) &
                                     ,                         dum1 &
                                     ,                         dum2 &
                                     ,                         dum3 &
                                     ,                        1 ) !Most unstable
        
          ENDDO
        ENDDO
      ENDIF  ! afwa_buoy_opt == 1

      IF ( config_flags % afwa_therm_opt == 1 ) THEN
        write ( message, * ) 'Calculating thermal indices'
        CALL wrf_debug( 100 , message )
        CALL thermal_diagnostics ( grid % t2                        &
                                 , grid % psfc                      &
                                 , rh2m                             &
                                 , wind10m                          &
                                 , grid % afwa_heatidx              &
                                 , grid % afwa_wchill               &
                                 , grid % afwa_fits                 &
                                 , ids, ide, jds, jde, kds, kde     &
                                 , ims, ime, jms, jme, kms, kme     &
                                 , ips, ipe, jps, jpe, kps, kpe )
      ENDIF  ! afwa_therm_opt == 1

      IF ( config_flags % afwa_turb_opt == 1 ) THEN
        write ( message, * ) 'Calculating turbulence indices'

        !~ For now, hard code turbulence layer bottom and top AGL heights
        !  --------------------------------------------------------------
        grid % afwa_tlyrbot = (/ 1500., 3000., 4600., 6100., 7600., 9100.,   &
                                  10700. /)
        grid % afwa_tlyrtop = (/ 3000., 4600., 6100., 7600., 9100., 10700.,  &
                                  12700. /)
        call turbulence_diagnostics ( u_phy                     &
                             , v_phy                            &
                             , grid % t_phy                     &
                             , ptot                             &
                             , zagl                             &
                             , grid % defor11                   &
                             , grid % defor12                   &
                             , grid % defor22                   &
                             , grid % afwa_turb                 &
                             , grid % afwa_llturb               &
                             , grid % afwa_llturblgt            &
                             , grid % afwa_llturbmdt            &
                             , grid % afwa_llturbsvr            &
                             !, config_flags % num_turb_layers   &
                             , 7                                &
                             , grid % afwa_tlyrbot              &
                             , grid % afwa_tlyrtop              &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF  ! afwa_turb_opt == 1

      ! Calculate equivalent radar reflectivity factor (z_e) using 
      ! old RIP code (2004) if running radar or VIL packages.
      ! ----------------------------------------------------------
      IF ( config_flags % afwa_radar_opt == 1 .or. &
         config_flags % afwa_vil_opt == 1 ) THEN
        write ( message, * ) 'Calculating Radar'
        CALL wrf_debug( 100 , message )
        CALL wrf_dbzcalc ( rho                                  &
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
      ! UPDATE: removed refd_max calculation, which was not working correctly.
      ! To correctly calculate refd_max, this routine could be called every
      ! time step, but instead, we are only going to calculate reflectivity
      ! on output time steps and avoid cost of calculating refd_max. GAC2014
      ! ----------------------------------------------------------------------
      IF ( config_flags % afwa_radar_opt == 1 ) THEN
        write ( message, * ) 'Calculating derived radar variables'
        CALL wrf_debug( 100 , message )
        CALL radar_diagnostics ( grid % refd                    &
                             , grid % refd_com                  &
!                             , grid % refd_max                  &
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
                             , rho                              &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF  ! afwa_vil_opt ==1 

      IF ( F_QR .AND. F_QC .AND. F_QNC ) THEN
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
          ELSEIF ( config_flags % mp_physics == MORR_TWO_MOMENT .OR. &
                   config_flags % mp_physics == MORR_TM_AERO ) THEN  !TWG add 2017
  
            ! Is this run with prognostic cloud droplets or no?
            ! -------------------------------------------------
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
   
          IF ( icing_opt .NE. 0 ) THEN
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
                                 , rho                              &
                                 , qrain                            &
                                 , qcloud                           &
                                 , ncloud                           &
                                 , ids, ide, jds, jde, kds, kde     &
                                 , ims, ime, jms, jme, kms, kme     &
                                 , ips, ipe, jps, jpe, kps, kpe )
          ELSE
            CALL wrf_debug ( DEBUG_LEVEL, 'Icing diagnostics not processed due to unknown MP scheme' )
          END IF
        ENDIF  ! afwa_icing_opt
      ELSE
        CALL wrf_debug ( DEBUG_LEVEL, 'Option icing_diagnostics requires: QC, QR, QNC' )
      END IF

      ! Calculate visiblility diagnostics
      ! ---------------------------------
      IF ( config_flags % afwa_vis_opt == 1 ) THEN
   
        ! Interpolate 20m temperature and RH
        ! ----------------------------------
        DO i=ims,ime
          DO j=jms,jme
            tv20m(i,j) = -999.
            rh20m(i,j) = -999.
            DO k = kps, MIN(kpe,kde-1)
              IF (tv20m (i,j) .eq. -999. .AND. zagl (i,k,j) .ge. 20.) THEN

                ! If the lowest model level > 20 m AGL, interpolate using 2-m temperature/RH
                ! --------------------------------------------------------------------------
                IF (k .eq. kps) THEN
                  tv20m(i,j) = tv2m(i,j) + &
                               (20. - 2.) / &
                               (zagl(i,k,j) - 2.) * &
                               (grid%t_phy(i,k,j) * (1 + 0.61 * qvapr(i,k,j)) - tv2m(i,j))
                  rh20m(i,j) = rh2m(i,j) + &
                               (20. - 2.) / &
                               (zagl(i,k,j) - 2.) * &
                               (rh(i,k,j) - rh2m(i,j))
                ELSE
                  tv20m(i,j) = grid%t_phy(i,k-1,j) * (1 + 0.61 * qvapr(i,k-1,j)) + &
                               ((20. - zagl(i,k-1,j)) / &
                               (zagl(i,k,j) - zagl(i,k-1,j))) * &
                               (grid%t_phy(i,k,j) * (1 + 0.61 * qvapr(i,k,j)) - &
                               grid%t_phy(i,k-1,j) * (1 + 0.61 * qvapr(i,k-1,j)))
                  rh20m(i,j) = rh (i,k-1,j) + &
                               ((20. - zagl (i,k-1,j)) / &
                               (zagl (i,k,j) - zagl (i,k-1,j))) * &
                               (rh (i,k,j) - rh (i,k-1,j))
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO

        ! Calculate 125 meter winds
        ! -------------------------
        DO i=ims,ime
          DO j=jms,jme
            wind125m(i,j) = -999.
            DO k = kps, MIN(kpe,kde-1)
              IF (wind125m (i,j) .eq. -999. .AND. zagl (i,k,j) .ge. 125.) THEN

                ! If the lowest model level > 125 m AGL, use level 1 wind
                ! -------------------------------------------------------
                IF (k .eq. kps) THEN
                  wind125m(i,j) = uv_wind(u_phy(i,k,j),v_phy(i,k,j))
                ELSE
                  wind125m(i,j) = uv_wind(u_phy(i,k-1,j),v_phy(i,k-1,j)) + &
                               ((125. - zagl(i,k-1,j)) / &
                               (zagl(i,k,j) - zagl(i,k-1,j))) * &
                               (uv_wind(u_phy(i,k,j),v_phy(i,k,j)) - &
                               uv_wind(u_phy(i,k-1,j),v_phy(i,k-1,j)))
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO

        IF ( F_QC .AND. F_QR .AND. F_QI .AND. F_QS .AND. F_QG ) THEN
          write ( message, * ) 'Calculating visibility'
          CALL wrf_debug( 100 , message )
          CALL vis_diagnostics ( qcloud(ims:ime,k_start,jms:jme)  &
                               , qrain(ims:ime,k_start,jms:jme)   &
                               , qice(ims:ime,k_start,jms:jme)    &
                               , qsnow(ims:ime,k_start,jms:jme)   &
                               , qgrpl(ims:ime,k_start,jms:jme)   &
                               , rho(ims:ime,k_start,jms:jme)     &
                               , wind10m                          &
                               , wind125m                         &
                               , grid % afwa_pwat                 &
                               , grid % q2                        &
                               , rh2m                             &
                               , rh20m                            &
                               , tv2m                             &
                               , tv20m                            &
                               , dustc                            &
                               , grid % afwa_vis                  &
                               , grid % afwa_vis_dust             &
                               , grid % afwa_vis_alpha            &
                               , ids, ide, jds, jde, kds, kde     &
                               , ims, ime, jms, jme, kms, kme     &
                               , ips, ipe, jps, jpe, kps, kpe ) 
        ELSE
          CALL wrf_debug ( DEBUG_LEVEL, 'Option vis_diagnostics requires: QC, QR, QI, QS, QG' )
        END IF
      ENDIF

      IF ( F_QC .AND. F_QI .AND. F_QS ) THEN
        ! Calculate cloud diagnostics
        ! ---------------------------
        IF ( config_flags % afwa_cloud_opt == 1 ) THEN
          write ( message, * ) 'Calculating cloud'
          CALL wrf_debug( 100 , message )
          CALL cloud_diagnostics (qcloud                          &
                               , qice                             &
                               , qsnow                            &
                               , rh_cld                           &
                               , dz8w                             &
                               , rho                              &
                               , grid % z                         &
                               , grid % ht                        &
                               , grid % afwa_cloud                &
                               , grid % afwa_cloud_ceil           &
                               , ids, ide, jds, jde, kds, kde     &
                               , ims, ime, jms, jme, kms, kme     &
                               , ips, ipe, jps, jpe, kps, kpe )
        ENDIF
      ELSE
        CALL wrf_debug ( DEBUG_LEVEL, 'Option cloud_diagnostics requires: QC, QI, QS' )
      END IF

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
                             , cin                              &
                             , zlfc                             &
                             , plfc                             &
                             , lidx                             &
                             , llws                             &
                             , afwa_tornado                     &
                             , grpl_flx_max                     &
                             , u10                              &
                             , v10                              &
                             , w_2                              &
                             , uh                               &
                             , t_phy                            &
                             , t2                               &
                             , z                                &
                             , ht                               &
                             , tornado_mask                     &
                             , tornado_dur                      &
                             , dt                               &
                             , pwat                             &
                             , u_phy                            &
                             , v_phy                            &
                             , p                                &
                             , qi                               &
                             , qs                               &
                             , qg                               &
                             , ng                               &
                             , qv, qr, qc                       &
                             , rho                              &
                             , dz8w                             &
                             , rh                               &
                             , do_buoy_calc                     &
                             , ims, ime, jms, jme, kms, kme     &
                             , its, ite, jts, jte               &
                             , k_start, k_end                   &
                             , j_start, j_end, i_start, i_end   )


    INTEGER, INTENT(IN) :: its, ite, jts, jte, k_start, k_end   &
                         , ims, ime, jms, jme, kms, kme         &
                         , j_start, j_end, i_start, i_end


    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN   ) ::                                    p  &
                                              ,            w_2  &
                                              ,          t_phy  &
                                              ,          u_phy  &
                                              ,          v_phy  &
                                              ,             qi  &
                                              ,             qs  &
                                              ,             qg  &
                                              ,             ng  &
                                              ,     qv, qr, qc  &
                                              ,            rho  &
                                              ,              z  &
                                              ,           dz8w  &
                                              ,             rh


    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(IN   ) ::                                  u10  &
                                              ,            v10  &
                                              ,      wspd10max  &
                                              ,             uh  &
                                              ,             t2  &
                                              ,             ht  &
                                              ,  midrh_min_old  &
                                              ,    up_heli_max  &
                                              ,           llws  &
                                              ,           pwat


    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(INOUT) ::                             w_up_max  & 
                                              ,       w_dn_max  &
                                              ,      tcoli_max  &
                                              ,      midrh_min  &
                                              ,      afwa_hail  &
                                              ,   afwa_tornado  &
                                              ,   grpl_flx_max  &
                                              ,   tornado_mask  &
                                              ,    tornado_dur 


    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(INOUT) ::                                 cape  &
                                              ,            cin  &
                                              ,           zlfc  &
                                              ,           plfc  &
                                              ,           lidx

    REAL, INTENT(IN) ::                                     dt
    LOGICAL, INTENT(IN) ::                        do_buoy_calc

    ! Local
    ! -----
    INTEGER :: i,j,k
    INTEGER :: kts,kte
    REAL    :: zagl, zlfc_msl, melt_term, midrh_term, hail, midrh
    REAL    :: dum1, dum2, dum3
    REAL    :: tornado, lfc_term, shr_term, midrh2_term, uh_term
    REAL    :: wind_vel, p_tot, tcoli, grpl_flx, w_n15, qg_n15
    INTEGER :: nz, ostat
    REAL, DIMENSION( ims:ime, jms:jme ) ::                w_up  &
                                              ,           w_dn  &
                                           , tornado_mask_prev  &
                                           ,  tornado_dur_prev
    REAL :: time_factor, time_factor_prev
    LOGICAL :: is_target_level

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
    !DO j = jts, jte
    !  DO i = its, ite
    !    wind_vel = uv_wind ( u10(i,j) , v10(i,j) )
    !    IF ( wind_vel .GT. wspd10max(i,j) ) THEN
    !      wspd10max(i,j) = wind_vel
    !    ENDIF
    !  ENDDO
    !ENDDO
 
    ! Vertical velocity quantities between output times
    ! -------------------------------------------------
    w_up=0.
    w_dn=0.
    DO j = jts, jte
      DO k = k_start, k_end
        DO i = its, ite
          p_tot = p(i,k,j) / 100.
 
          ! Check vertical velocity field below 400 mb
          IF ( p_tot .GT. 400. .AND. w_2(i,k,j) .GT. w_up(i,j) ) THEN
            w_up(i,j) = w_2(i,k,j)
            IF ( w_up(i,j) .GT. w_up_max(i,j) ) THEN
              w_up_max(i,j) = w_up(i,j)
            ENDIF
          ENDIF
          IF ( p_tot .GT. 400. .AND. w_2(i,k,j) .LT. w_dn(i,j) ) THEN
            w_dn(i,j) = w_2(i,k,j)
            IF ( w_dn(i,j) .LT. w_dn_max(i,j) ) THEN
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
        ! Change exponent to 1.1 to reduce probabilities for large sizes
        !hail=max((w_up(i,j)/1.4)**1.25-melt_term-midrh_term,0.)
        hail=max((w_up(i,j)/1.4)**1.1-melt_term-midrh_term,0.)
        hail=hail*((uh(i,j)/100)+0.25)
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

    ! Lighting (pseudo graupel flux calculation)
    ! Model graupel mixing ration (g/kg) times w (m/s) at the -15C level
    ! Values should range from around 25 in marginal lightning situations,
    ! to over 400 in situations with very frequent lightning.
    ! --------------------------------------------------------------------
    DO j = jts, jte
      DO i = its, ite
        grpl_flx=0
        w_n15=-999.
        DO k = k_start, k_end
          
          ! Interpolate w and qg to -15C level and calculate graupel flux
          ! as simply graupel x vertical velocity at -15C
          ! -------------------------------------------------------------
          IF ( t_phy (i,k,j) .LE. 258.15 .AND. w_n15 .EQ. -999. .AND.   &
               k .GT. k_start .AND. qg (i,k,j) .GT. 1.E-20 ) THEN
            w_n15 = w_2 (i,k,j)
            qg_n15 = 1000. * qg (i,k,j) ! g/kg
            grpl_flx =  qg_n15 * w_n15   
          ENDIF
        ENDDO
        IF ( grpl_flx .GT. grpl_flx_max(i,j) ) THEN
          grpl_flx_max(i,j) = grpl_flx
        ENDIF
      ENDDO
    ENDDO

    ! Update buoyancy parameters.
    ! ---------------------------
    IF ( do_buoy_calc ) THEN
      nz = k_end - k_start + 1
      DO j = jts, jte
        DO i = its, ite
          ostat = Buoyancy (                                   nz &
                                       , t_phy(i,kms:kme      ,j) &
                                       ,    rh(i,kms:kme      ,j) &
                                       ,     p(i,kms:kme      ,j) &
                                       ,     z(i,kms:kme      ,j) &
                                       ,                        1 &
                                       ,                cape(i,j) &
                                       ,                 cin(i,j) &
                                       ,                 zlfc_msl &
                                       ,                plfc(i,j) &
                                       ,                lidx(i,j) &
                                       ,                        3 ) !Surface

          ! Add 273.15 to lifted index per some standard I don't understand
          ! ---------------------------------------------------------------
          IF ( lidx ( i, j ) .ne. 999. ) lidx ( i, j ) = lidx ( i, j ) + 273.15
  
          ! Subtract terrain height to convert ZLFC from MSL to AGL
          ! -------------------------------------------------------
          IF ( zlfc_msl .ge. 0. ) THEN
            zlfc ( i, j ) = zlfc_msl - ht ( i, j )
          ELSE
            zlfc( i, j ) = -1.
          ENDIF

        ENDDO
      ENDDO
    ENDIF

    ! Maximum tornado wind speed in ms-1.
    ! First, save off old tornado mask and duration arrays
    ! ----------------------------------------------------
    tornado_dur_prev(:,:)=tornado_dur(:,:)
    tornado_mask_prev(:,:)=tornado_mask(:,:)

    ! Initialize all tornado variables
    ! --------------------------------
    tornado_mask(:,:)=0.
    tornado_dur(:,:)=0.

    DO j = j_start, j_end
      DO i = i_start, i_end
        tornado = 0.

        ! Current tornado algorithm
        ! -------------------------
        IF ( zlfc(i,j) .ge. 0. ) THEN
          uh_term = min(max((uh(i,j) - 25.) / 50., 0.), 1.)
          shr_term = min(max((llws(i,j) - 2.) / 10., 0.), 1.)
          lfc_term = min(max((3000. - zlfc(i,j)) / 1500., 0.), 1.)
          midrh2_term = min(max((90. - &
                        min(midrh_min(i,j),midrh_min_old(i,j))) / 30., 0.), 1.)
          tornado = 50. * uh_term * shr_term * lfc_term * midrh2_term
        ENDIF

        ! Does tornado algorithm indicate all possible ingredients
        ! for a minimum tornado, if so turn on mask
        ! --------------------------------------------------------
        IF (tornado .GT. 0.) THEN
          tornado_mask(i,j) = 1.
        ENDIF
  
        ! Update the duration of this tornado if there was previously
        ! a tornado mask at this or an adjacent point
        ! -----------------------------------------------------------
        IF ( ( tornado_mask(i,j) .GT. 0.5 )  .OR. &
           ( MAXVAL(tornado_mask_prev(i-1:i+1,j-1:j+1)) .GT. 0.5 ) ) THEN
          tornado_dur(i,j) = MAXVAL(tornado_dur_prev(i-1:i+1,j-1:j+1)) + dt
        ENDIF

        ! Ramp up value of tornado beta value linearly in time until &
        ! it has been sustained 5 minutes
        ! ------------------------------------------------------------
        time_factor = MIN(tornado_dur(i,j)/900.,1.)
        tornado = tornado * time_factor

        ! OPTIONAL
        ! Adjust previous max tornado wind upward to account for longer 
        ! duration - if after 5 minutes, no adjustment made as
        ! time_factor/time_factor_prev=1
        ! -------------------------------------------------------------
        !time_factor_prev = MIN((tornado_dur(i,j) - dt)/900.,1.)
        !IF ( ( time_factor_prev .GT. 0. ) .AND. &
        !   ( time_factor_prev .LT. 1. ) ) THEN
        !  afwa_tornado(i,j) = afwa_tornado(i,j) * time_factor/time_factor_prev
        !ENDIF

        ! Now that we are comparing apples to apples, see if current tornado
        ! wind is higher than previous highest value for this point
        ! ------------------------------------------------------------------
        IF ( tornado .GT. afwa_tornado(i,j) ) THEN
          afwa_tornado(i,j) = tornado
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
    ! "liquid" now is it?) [kg/m^2]
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
          ! RAS Per James McCormick's stats, more large supercooled
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
!                             , refd_max                         &
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
!                                              ,       refd_max  &
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
   
!          ! Max reflectivity over the output interval
!          ! -----------------------------------------
!          IF (refd(i,j) .gt. refd_max(i,j)) refd_max(i,j) = refd(i,j)

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
                             , dz8w                             &
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
                                              ,              z  &
                                              ,           dz8w
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
              IF (rh(i,k,j) .gt. 80 .and. saturation(i,j) .eqv. .false.) THEN
                cloud_top_tmp(i,j)=t_phy(i,k,j)
                cloud_top_k_index(i,j)=k
                saturation(i,j)=.true.
                precip_type(i,j)=2 ! Snow
              ENDIF
              IF (rh(i,k,j) .le. 70 .and. saturation(i,j) .eqv. .true.) THEN
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
                          (dz8w(i,k,j)))
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
                        *(5.-mod_2m_tmp(i,j)+273.15)**0.4
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
                             , qgrpl                            &
                             , rho                              &
                             , wind10m                          &
                             , wind125m                         &
                             , pwater                           &
                             , q2m                              &
                             , rh2m                             &
                             , rh20m                            &
                             , tv2m                             &
                             , tv20m                            &
                             , dustc                            &
                             , vis                              &
                             , vis_dust                         &
                             , vis_alpha                        &
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
                                              ,          qgrpl  & 
                                              ,            rho  & 
                                              ,        wind10m  & 
                                              ,       wind125m  & 
                                              ,         pwater  & 
                                              ,           rh2m  &
                                              ,            q2m  &
                                              ,          rh20m  &
                                              ,           tv2m  &
                                              ,          tv20m
    REAL, DIMENSION( ims:ime, jms:jme, ndust ),                 &
         INTENT(IN   ) ::                                dustc
    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(  OUT) ::                                  vis  &
                                              ,       vis_dust  &
                                              ,      vis_alpha

    ! Local
    ! -----
    INTEGER :: i,j,k,d
    REAL, PARAMETER :: visfactor=3.912
    REAL, DIMENSION (ndust) :: dustfact
    REAL :: bc, br, bi, bs, dust_extcoeff, hydro_extcoeff, extcoeff, vis_haze
    REAL :: tvd, rh, prob_ext_coeff_gt_p29, haze_ext_coeff
    REAL :: vis_hydlith, alpha_haze

    ! Dust factor based on 5 bin AFWA dust scheme.  This is a simplification
    ! of the scheme in WRFPOST.  More weight is applied to smaller particles.
    ! -----------------------------------------------------------------------
    dustfact=(/1.470E-6,7.877E-7,4.623E-7,2.429E-7,1.387E-7/)

    DO i=ims,ime
      DO j=jms,jme

        ! Hydrometeor extinction coefficient
        ! For now, lump graupel in with rain
        ! -------------------------------------------------------------------
        ! Update: GAC 20131213  Our test results with surface cloud and ice
        ! are very unfavorable.  Model doesn't do a great job handling clouds
        ! at the model surface.  Therefore, we will not trust surface
        ! cloud water/ice.  (Commented out below).
        ! -------------------------------------------------------------------
        !br=2.240*qrain(i,j)**0.75
        !bs=10.36*qsnow(i,j)**0.78
        !bc=144.7*qcloud(i,j)**0.88
        !bi=327.8*qice(i,j)
        !hydro_extcoeff=bc+br+bi+bs
        !br=2.240*(qrain(i,j)+qgrpl(i,j))**0.75
        !bs=10.36*(qsnow(i,j)*rho(i,j))**0.78
        ! Update: moisture variables should be in mass concentration (g m^-3)
        br=1.1*(1000.*rho(i,j)*(qrain(i,j)+qgrpl(i,j)))**0.75
        bs=10.36*(1000.*rho(i,j)*qsnow(i,j))**0.78
        hydro_extcoeff=(br+bs)/1000.   ! m^-1

        ! Dust extinction coefficient
        ! ---------------------------
        dust_extcoeff=0.
        DO d=1,ndust
          dust_extcoeff=dust_extcoeff+dustfact(d)*dustc(i,j,d)
        ENDDO

        ! UPDATE: GAC 20131213 Old algorithm commented out below
        !! Visibility due to haze obscuration
        !! -------------------------------------------------------
        !vis_haze=1500.*(105.-rh2m(i,j)+wind10m(i,j))
        !
        !! Calculate total visibility
        !! Take minimum visibility from hydro/lithometeors and haze
        !! Define maximum visibility as 20 km (UPDATE: 999.999 km)
        !! --------------------------------------------------------
        !extcoeff=hydro_extcoeff+dust_extcoeff
        !IF (extcoeff .gt. 0.) THEN
        !  vis(i,j)=MIN(visfactor/extcoeff,vis_haze)
        !ELSE
        !  vis(i,j)=999999.
        !ENDIF

        ! Update: GAC 20131213  New haze/fog visibility algorithm
        ! Start with relative humidity predictor.  Increase 
        ! predicted visibility as mixing ratio decreases (as
        ! there is less water to condense).
        ! -------------------------------------------------------
        vis_haze=999999.
        IF (q2m(i,j) .gt. 0.) THEN
          !vis_haze=1500.*(105.-rh2m(i,j))*(15./(1000.*q2m(i,j)))
          vis_haze=1500.*(105.-rh2m(i,j))*(5./min(1000.*q2m(i,j),5.))
        ENDIF
 
        ! Calculate a Weibull function "alpha" term.  This can be
        ! used later with visibility (which acts as the "beta" term
        ! in the Weibull function) to create a probability distribution
        ! for visibility. Alpha can be thought of as the "level of
        ! certainty" that beta (model visibility) is correct. Fog is
        ! notoriously difficult to model. In the below algorithm,
        ! the alpha value (certainty) decreases as PWAT, mixing ratio,
        ! or winds decrease (possibly foggy conditions), but increases
        ! if RH decreases (more certainly not foggy).  If PWAT is lower
        ! then there is a higher chance of radiation fog because there
        ! is less insulating cloud above.
        ! -------------------------------------------------------------
        alpha_haze=3.6
        IF (q2m(i,j) .gt. 0.) THEN
          alpha_haze=0.1 + pwater(i,j)/25.     + wind125m(i,j)/3. + &
                          (100.-rh2m(i,j))/10. + 1./(1000.*q2m(i,j))
          alpha_haze=min(alpha_haze,3.6)
        ENDIF
        
        ! Calculate visibility from hydro/lithometeors
        ! Maximum visibility -> 999999 meters
        ! --------------------------------------------
        extcoeff=hydro_extcoeff+dust_extcoeff
        IF (extcoeff .gt. 0.) THEN
          vis_hydlith=min(visfactor/extcoeff, 999999.)
        ELSE
          vis_hydlith=999999.
        ENDIF

        ! Calculate total visibility
        ! Take minimum visibility from hydro/lithometeors and haze
        ! Set alpha to be alpha_haze if haze dominates, or 3.6
        ! (a Guassian distribution) when hydro/lithometeors dominate
        ! ----------------------------------------------------------
        IF (vis_hydlith < vis_haze) THEN
           vis(i,j)=vis_hydlith
           vis_alpha(i,j)=3.6
        ELSE
           vis(i,j)=vis_haze
           vis_alpha(i,j)=alpha_haze
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
    REAL    :: tot_cld_cond, maxrh, cld_frm_cnd, cld_frm_rh, z_maxrh
    REAL    :: snow_extcoeff, vis_snow, cloud_lo, zagl_up, zagl_lo
    REAL, PARAMETER :: min_ceil = 125.    ! Minimum ceiling of 125 m

    ! Calculate cloud cover based on total cloud condensate, or if none
    ! present, from maximum relative humidity in the column.
    ! -----------------------------------------------------------------
    DO i=ims,ime
      DO j=jms,jme

        ! Initialize some key variables
        ! -----------------------------
        tot_cld_cond = 0.
        cloud(i,j) = 0.
        maxrh = -9999.
        cloud_ceil(i,j) = -9999.
        cloud_lo = 0.

        ! Now go up the column to find our cloud base
        ! -------------------------------------------
        DO k=kms,kme

          ! Total cloud condensate
          ! Don't trust modeled cloud below 125 m.  We
          ! let the alpha curve determine probabilities
          ! below 125 m...
          ! ---------------------------------------------
          IF ( z(i,k,j) - ht (i,j) .gt. min_ceil ) THEN

            ! Maximum column relative humidity
            ! --------------------------------
            IF (rh (i,k,j) .gt. maxrh) THEN
              maxrh = rh (i,k,j)
              z_maxrh = z(i,k,j)
            ENDIF

!            ! Cloud cover parameterization. Take maximum value
!            ! from condensate and relative humidity terms.
!            ! ------------------------------------------------
!            tot_cld_cond = tot_cld_cond + (qcloud (i,k,j) + qice (i,k,j) &
!                           + qsnow (i,k,j)) * dz8w (i,k,j) * rho(i,k,j)
!            cld_frm_cnd = 50. * tot_cld_cond
!            cld_frm_rh = MAX(((maxrh - 70.) / 30.),0.)
!            cloud (i,j) = MAX(cld_frm_cnd,cld_frm_rh)

            ! Calculate cloud cover beta value by summing 
            ! relative humidity above 70% as we go up the
            ! column.  Assume a higher probability of a
            ! cloud if we have an accumulation of high
            ! relative humidity over a typical cloud
            ! depth of 500m. (Note dz8w is distance 
            ! between full eta levels. Note also that rh
            ! is derived from the sum of qvapor, qcloud,
            ! and qcloud, with a maximum of 100%). 
            ! -------------------------------------------
            cld_frm_rh = MAX(((rh (i,k,j) - 90.) / 10.),0.)
            cloud (i,j) = cloud (i,j) + ( cld_frm_rh * dz8w (i,k,j) ) / 250.

            ! Calculate cloud ceiling, the level at which
            ! parameterization of cloud cover exceeds 80%
            ! Once we exceed the 80% threshold, we will
            ! interpolate downward to find the ceiling.
            ! If this is the lowest level, then we will
            ! simply set ceiling to that level.  After
            ! we interpolate, if ceiling is below the 
            ! minimum we trust, we set it to the minimum.
            ! -------------------------------------------
            IF ( cloud_ceil (i,j) .eq. -9999. .and. cloud (i,j) .gt. 0.8 ) THEN
              zagl_up = z (i,k,j) - ht (i,j)
              IF ( k .EQ. kps ) THEN
                cloud_ceil (i,j) = zagl_up
              ELSE
                zagl_lo = z (i,k-1,j) - ht (i,j)
                cloud_ceil (i,j) = zagl_lo + &
                             ((0.8 - cloud_lo) / &
                             (cloud (i,j) - cloud_lo)) * &
                             (zagl_up - zagl_lo)
                cloud_ceil (i,j) = MAX(cloud_ceil (i,j),ceil_min)
              ENDIF
            ENDIF
            
            ! Save cloud amount here to use for interpolation
            ! -----------------------------------------------
            cloud_lo=cloud(i,j)
          ENDIF
        ENDDO

        ! If we did not encounter any definitive cloud earlier, we set
        ! cloud ceiling to the level of maximum relative humidity. (If
        ! there is any cloud, this is our best guess as to where it
        ! will reside in the vertical). Height is AGL.
        ! ------------------------------------------------------------
        IF (cloud_ceil (i,j) .eq. -9999.) THEN
          cloud_ceil (i,j) = z_maxrh - ht (i,j)
        ENDIF

        ! Compare cloud ceiling to surface visibility reduction due to snow
        ! Note difference from horizontal visibility algorithm for snow
        ! -----------------------------------------------------------------
        IF (qsnow (i,1,j) .GT. 0. .AND. rho (i,1,j) .GT. 0.) THEN
          snow_extcoeff = 25. * (1000. * rho(i,1,j) * qsnow (i,1,j))**0.78
          snow_extcoeff = snow_extcoeff / 1000.
          vis_snow = 3.912 / snow_extcoeff
          IF (vis_snow .LT. cloud_ceil (i,j)) cloud_ceil (i,j) = vis_snow
        ENDIF

      ENDDO
    ENDDO

  END SUBROUTINE cloud_diagnostics



  SUBROUTINE thermal_diagnostics ( t2                           &
                             , psfc                             &
                             , rh2m                             &
                             , wind10m                          &
                             , heatidx                          &
                             , wchill                           &
                             , fits                             &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(IN   ) ::                                   t2  &
                                              ,           psfc  &
                                              ,           rh2m  & 
                                              ,        wind10m

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(  OUT) ::                              heatidx  &
                                              ,         wchill  & 
                                              ,           fits

    ! Local
    ! -----
    INTEGER :: i, j

    DO i=ims,ime
      DO j=jms,jme
       
        ! Heat Index
        ! ----------
        heatidx ( i, j ) = calc_hi   (   t2      ( i, j )    &
                                       , rh2m    ( i, j ) )

        ! Wind Chill
        ! ----------
        wchill  ( i, j ) = calc_wc   (   t2      ( i, j )    &
                                       , wind10m ( i, j ) )

        ! Fighter Index of Thermal Stress
        ! -------------------------------
        fits    ( i, j ) = calc_fits (   psfc    ( i, j )    &
                                       , t2      ( i, j )    &
                                       , rh2m    ( i, j ) )

      ENDDO
    ENDDO

  END SUBROUTINE thermal_diagnostics



  SUBROUTINE turbulence_diagnostics ( u_phy                     &
                             , v_phy                            &
                             , t_phy                            &
                             , p                                &
                             , zagl                             &
                             , defor11                          &
                             , defor12                          &
                             , defor22                          &
                             , turb                             &
                             , llturb                           &
                             , llturblgt                        &
                             , llturbmdt                        &
                             , llturbsvr                        &
                             , nlyrs                            &
                             , lyrbot                           &
                             , lyrtop                           &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    INTEGER, INTENT(IN) :: nlyrs

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN   ) ::                                u_phy  &
                                              ,          v_phy  &
                                              ,          t_phy  & 
                                              ,              p  & 
                                              ,           zagl  &
                                              ,        defor11  & 
                                              ,        defor12  & 
                                              ,        defor22

    REAL, DIMENSION( nlyrs ),                                   &
         INTENT(IN   ) ::                               lyrtop  &
                                              ,         lyrbot

    REAL, DIMENSION( ims:ime, nlyrs, jms:jme ),                 &
         INTENT(  OUT) ::                                 turb   

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(  OUT) ::                               llturb  &
                                              ,      llturblgt  &
                                              ,      llturbmdt  & 
                                              ,      llturbsvr

    ! Local
    ! -----
    INTEGER :: i, j, k, n, bot, top, nlayer

    REAL ::                                            ugrdtop  &
                                              ,        ugrdbot  &
                                              ,        vgrdtop  &
                                              ,        vgrdbot  &
                                              ,     defor11top  &
                                              ,     defor11bot  &
                                              ,     defor12top  &
                                              ,     defor12bot  &
                                              ,     defor22top  &
                                              ,     defor22bot

    REAL, DIMENSION( kms:kme )            ::         this_zagl  &
                                              ,        this_tK  &
                                              ,         this_p  &
                                              ,         this_u  &
                                              ,         this_v

    REAL :: wind, therm, mtn_wave, tpd_wave

    !~ Initialize variables.
    !  ----------------------
    turb = REAL ( 0 )
    llturb = REAL ( 0 )
    llturblgt = REAL ( 0 )
    llturbsvr = REAL ( 0 )

    !~ Loop through the grid.
    !  ----------------------
    DO i=ims,ime
      DO j=jms,jme
       
        !~ Loop through the turbulence layers
        !  ----------------------------------
        DO n = 1, nlyrs

          !~ Interpolate relevent variables to turbulence layers
          !  ---------------------------------------------------
          ugrdtop    = lin_interp (     zagl ( i, kms:kme-1, j )  &
                                   ,   u_phy ( i, kms:kme-1, j )  &
                                   ,             lyrtop  ( n ) )
          ugrdbot    = lin_interp (     zagl ( i, kms:kme-1, j )  &
                                   ,   u_phy ( i, kms:kme-1, j )  &
                                   ,             lyrbot  ( n ) )
          vgrdtop    = lin_interp (     zagl ( i, kms:kme-1, j )  &
                                   ,   v_phy ( i, kms:kme-1, j )  &
                                   ,             lyrtop  ( n ) )
          vgrdbot    = lin_interp (     zagl ( i, kms:kme-1, j )  &
                                   ,   v_phy ( i, kms:kme-1, j )  &
                                   ,             lyrbot  ( n ) )
          defor11top = lin_interp (     zagl ( i, kms:kme-1, j )  &
                                   , defor11 ( i, kms:kme-1, j )  &
                                   ,             lyrtop  ( n ) )
          defor11bot = lin_interp (     zagl ( i, kms:kme-1, j )  &
                                   , defor11 ( i, kms:kme-1, j )  &
                                   ,             lyrbot  ( n ) )
          defor12top = lin_interp (     zagl ( i, kms:kme-1, j )  &
                                   , defor12 ( i, kms:kme-1, j )  &
                                   ,             lyrtop  ( n ) )
          defor12bot = lin_interp (     zagl ( i, kms:kme-1, j )  &
                                   , defor12 ( i, kms:kme-1, j )  &
                                   ,             lyrbot  ( n ) )
          defor22top = lin_interp (     zagl ( i, kms:kme-1, j )  &
                                   , defor22 ( i, kms:kme-1, j )  &
                                   ,             lyrtop  ( n ) )
          defor22bot = lin_interp (     zagl ( i, kms:kme-1, j )  &
                                   , defor22 ( i, kms:kme-1, j )  &
                                   ,             lyrbot  ( n ) )

          !~ Compute Knapp-Ellrod clear air turbulence
          !  -----------------------------------------
          turb ( i, n, j ) = CATTurbulence (                       ugrdbot  &
                                               ,                   ugrdtop  &
                                               ,                   vgrdbot  &
                                               ,                   vgrdtop  &
                                               ,                defor11bot  &
                                               ,                defor11top  &
                                               ,                defor12bot  &
                                               ,                defor12top  &
                                               ,                defor22bot  &
                                               ,                defor22top  &
                                               ,                lyrbot (n)  &
                                               ,                lyrtop (n) )

        ENDDO
 
        !~ Get top and bottom index of 0-1500 m AGL layer
        !  ----------------------------------------------
        bot = kms
        top = kms
        DO k=kms+1,kme
          IF ( zagl ( i, k, j ) .gt. 1500. ) THEN
            top = k
            EXIT
          ENDIF
        ENDDO
        nlayer = top - bot + 1  !~ Number of layers from bottom to top

        !~ Copy current column at this i,j point into working arrays
        !  ---------------------------------------------------------
        this_zagl = zagl  ( i, kms:kme, j )
        this_tK   = t_phy ( i, kms:kme, j )
        this_p    = p     ( i, kms:kme, j )
        this_u    = u_phy ( i, kms:kme, j )
        this_v    = v_phy ( i, kms:kme, j )
                           
        !~ Interpolate req'd vars to the 1500 m level
        !~ Overwrite the "top" index with these values
        !  -------------------------------------------
        this_zagl ( top ) = 1500.
        this_tK ( top )   = lin_interp (  zagl ( i, kms:kme-1, j )  &
                                     ,   t_phy ( i, kms:kme-1, j )  &
                                     ,           this_zagl (top) )
        this_p ( top )    = lin_interp (  zagl ( i, kms:kme-1, j )  &
                                     ,       p ( i, kms:kme-1, j )  &
                                     ,           this_zagl (top) )
        this_u ( top )    = lin_interp (  zagl ( i, kms:kme-1, j )  &
                                     ,   u_phy ( i, kms:kme-1, j )  &
                                     ,           this_zagl (top) )
        this_v ( top )    = lin_interp (  zagl ( i, kms:kme-1, j )  &
                                     ,   v_phy ( i, kms:kme-1, j )  &
                                     ,           this_zagl (top) )

        !~ Compute the low level turbulence index (from 0 - 1500 m AGL)
        !~ There are four components to this index: a wind speed term,
        !~ thermodynamic term, mountain wave term, and trapped wave term.
        !~ These terms will utilize the working arrays we have just
        !~ defined, using only the portion of the array valid in the
        !~ 0-1500 m layer, e.g. this_u (bot:top).
        !~ The algorithm itself was developed by:
        !~ 
        !~ Mr. James McCormick
        !~ Aviation Hazards Team
        !~ Air Force Weather Agency 16WS/WXN
        !~ DSN: 271-1689 Comm: (402) 294-1689
        !~ James.McCormick.ctr@offutt.af.mil
        !  --------------------------------------------------------------  !
        !~ Step 1: Compute the wind speed term                            ~!
        !  --------------------------------------------------------------  !
        wind = LLT_WindSpeed ( nlayer, this_u (bot:top) &
                                , this_v (bot:top) )

        !  --------------------------------------------------------------  !
        !~ Step 2: Compute the thermodynamic term                         ~!
        !  --------------------------------------------------------------  !
        therm = LLT_Thermodynamic ( nlayer, this_tK(bot:top) &
                                , this_zagl(bot:top) )

        !  --------------------------------------------------------------  !
        !~ Step 3: Compute the mountain wave term                         ~!
        !  --------------------------------------------------------------  !
        mtn_wave = LLT_MountainWave ( nlayer, terrain_dx, terrain_dy &
                                , this_u(bot:top), this_v(bot:top)   &
                                , this_tK(bot:top), this_zagl(bot:top) )

        !  --------------------------------------------------------------  !
        !~ Step 4: Compute the trapped wave term                          ~!
        !  --------------------------------------------------------------  !
        tpd_wave = LLT_TrappedWave ( nlayer, this_u(bot:top) &
                                , this_v(bot:top), this_p(bot:top) )

        !  --------------------------------------------------------------  !
        !~ Step 5: Combine the above and arrive at the turbulence index.  ~!
        !  --------------------------------------------------------------  !
        llturb ( i,j ) = 1.-((1.-wind)*(1.-therm)*(1.-mtn_wave)*(1.-tpd_wave))

        !~ Compute probabilities of light, moderate, and severe LLT
        !  --------------------------------------------------------
        llturblgt ( i,j ) = (((((((llturb (i,j) * REAL (100))-REAL (30)) &
                                        *2.5)*.01)**2)*0.75)*REAL(100))
        IF ( llturb (i,j) < 0.3   ) llturblgt ( i,j ) = REAL ( 0 )
        IF ( llturblgt (i,j) > REAL (90) ) llturblgt ( i,j ) = REAL ( 90 )

        llturbmdt ( i,j ) = (((((((llturb (i,j) * REAL (100))-REAL (35)) &
                                     *2.22222)*.01)*0.75)**2)*88.88888)
        IF ( llturb (i,j) < 0.35  ) llturbmdt ( i,j ) = REAL ( 0 )
        IF ( llturbmdt (i,j) > REAL (70) ) llturbmdt ( i,j ) = REAL ( 70 )

        llturbsvr ( i,j ) = (((((((llturb (i,j) * REAL (100))-REAL (40)) &
                                     *REAL(2))*.01)*0.5)**2)*REAL(100))
        IF ( llturb (i,j) < 0.40  ) llturbsvr ( i,j ) = REAL ( 0 )
        IF ( llturbsvr (i,j) > REAL (35) ) llturbsvr ( i,j ) = REAL ( 35 )

      ENDDO
    ENDDO

  END SUBROUTINE turbulence_diagnostics

END MODULE module_diag_afwa

#endif
