!
!WRF:MEDIATION_LAYER:IO
!
#if (DA_CORE != 1)

SUBROUTINE med_calc_model_time ( grid , config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain, domain_clock_get
   USE module_configure , ONLY : grid_config_rec_type
  ! Model layer
   USE module_date_time

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local data
   REAL                                       :: time 

! this is now handled by with calls to time manager
!   time = head_grid%dt * head_grid%total_time_steps
!   CALL calc_current_date (grid%id, time)


END SUBROUTINE med_calc_model_time

SUBROUTINE med_before_solve_io ( grid , config_flags )
  ! Driver layer
   USE module_state_description
   USE module_domain    , ONLY : domain, domain_clock_get
   USE module_configure , ONLY : grid_config_rec_type
   USE module_streams
  ! Model layer
   USE module_utility
#if (WRFPLUS == 1)
   USE module_domain    , ONLY : domain_clock_set, get_ijk_from_grid
   USE module_configure , ONLY : model_config_rec
   USE mediation_pertmod_io , ONLY : save_tl_pert, read_ad_forcing, save_xtraj, read_xtraj, read_xtraj_reverse
   USE module_bc_em, ONLY : rk_phys_bc_dry_2
   USE module_bc, ONLY : set_physical_bc3d
   USE esmf_timemod, ONLY : ESMF_TimeInc

#ifdef DM_PARALLEL
   USE module_dm, ONLY : ntasks_x, ntasks_y, &
        local_communicator, mytask, ntasks
   USE module_comm_dm, ONLY : halo_em_init_1_sub, halo_em_init_2_sub, halo_em_init_5_sub
#endif
#endif
   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                                    :: ialarm
   INTEGER                                    :: rc
   TYPE(WRFU_Time) :: currTime, startTime
#if ( HWRF == 1 )
   INTEGER :: hr, min, sec, ms,julyr,julday
   REAL :: GMT
#endif

   CHARACTER*256          :: message
#if (WRFPLUS == 1)
   INTEGER                :: ids , ide , jds , jde , kds , kde , &
                             ims , ime , jms , jme , kms , kme , &
                             ips , ipe , jps , jpe , kps , kpe, ij, im
   CHARACTER*80           :: timestr
   REAL, POINTER :: moist(:,:,:,:), scalar(:,:,:,:), chem(:,:,:,:), tracer(:,:,:,:)

! In WRFPLUS NL run, boundary should be read before save xtraj.
! - RESTART OUTPUT
   CALL WRFU_ClockGet( grid%domain_clock, CurrTime=currTime, StartTime=startTime )
   IF ( ( WRFU_AlarmIsRinging( grid%alarms( RESTART_ALARM ), rc=rc ) ) .AND. &
        ( currTime .NE. startTime ) ) THEN
     IF ( grid%id .EQ. 1 ) THEN
       ! Only the parent initiates the restart writing. Otherwise, different
       ! domains may be written out at different times and with different 
       ! time stamps in the file names.
       CALL med_restart_out ( grid , config_flags )
     ENDIF
     CALL WRFU_AlarmRingerOff( grid%alarms( RESTART_ALARM ), rc=rc )
   ELSE
     CALL WRFU_AlarmRingerOff( grid%alarms( RESTART_ALARM ), rc=rc )
   ENDIF

! - Look for boundary data after writing out history and restart files
   CALL med_latbound_in ( grid , config_flags )
#endif

 CALL WRFU_ClockGet( grid%domain_clock, CurrTime=currTime, StartTime=startTime )

 IF( (WRFU_AlarmIsRinging( grid%alarms( HISTORY_ALARM ), rc=rc ) .AND. &
     (grid%dfi_write_dfi_history .OR. grid%dfi_stage == DFI_FST .OR. grid%dfi_opt == DFI_NODFI) )) THEN
     IF       ( ( config_flags%restart )                    .AND. &
                ( config_flags%write_hist_at_0h_rst )       .AND. &
                ( currTime .EQ. startTime )                       ) THEN
#if ( NMM_CORE == 1 )
! NMM-only: outputs boundary arrays of certain variables:
!      call med_boundary_out(grid,config_flags)
#endif


       !  output history at beginning of restart if alarm is ringing
       CALL med_hist_out ( grid , HISTORY_ALARM, config_flags )
      
     ELSE IF  ( ( config_flags%restart )                    .AND. &
                ( .NOT. config_flags%write_hist_at_0h_rst ) .AND. &
                ( currTime .EQ. startTime )                       ) THEN
       !  we do not do anything
     ELSE
       CALL med_hist_out ( grid , HISTORY_ALARM, config_flags )
       
     END IF
     CALL WRFU_AlarmRingerOff( grid%alarms( HISTORY_ALARM ), rc=rc )
   ELSE IF  ( (config_flags%restart) .AND. ( currTime .EQ. startTime ) .AND. &
              ( config_flags%write_hist_at_0h_rst ) ) THEN
     !  output history at beginning of restart even if alarm is not ringing
     CALL med_hist_out ( grid , HISTORY_ALARM, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( HISTORY_ALARM ), rc=rc )
   ENDIF

   IF( WRFU_AlarmIsRinging( grid%alarms( INPUTOUT_ALARM ), rc=rc ) ) THEN
     CALL med_filter_out  ( grid , config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( INPUTOUT_ALARM ), rc=rc )
   ENDIF

   DO ialarm = first_auxhist, last_auxhist
     IF ( .FALSE.) THEN
       rc = 1  ! dummy statement
     ELSE IF( WRFU_AlarmIsRinging( grid%alarms( ialarm ), rc=rc ) ) THEN
#if (WRFPLUS == 1)
       IF ( .NOT. grid%trajectory_io ) THEN
         ! For wrfplus, only allow tl and nl output histoty files.
         IF ( config_flags%dyn_opt .EQ. dyn_em_tl .OR. config_flags%dyn_opt .EQ. dyn_em ) THEN
            CALL med_hist_out ( grid , ialarm, config_flags )
         ENDIF
       ELSE
         IF ( ialarm .EQ. auxhist8_only ) THEN ! TL perturbation only
            CALL domain_clock_get ( grid, current_timestr=message )
            IF ( config_flags%dyn_opt .EQ. dyn_em_tl .and. .not. config_flags%tl_standalone ) THEN
               CALL save_tl_pert  ( message )
            ELSEIF ( config_flags%dyn_opt .EQ. dyn_em_tl .and. config_flags%tl_standalone ) THEN
               CALL med_hist_out ( grid , ialarm, config_flags )
            ENDIF
         ENDIF
         IF ( ialarm .EQ. auxhist6_only ) THEN ! trajectory only
            CALL domain_clock_get ( grid, current_timestr=message )
            IF ( config_flags%dyn_opt .EQ. dyn_em ) THEN
               CALL save_xtraj  (  message )
            ENDIF
         ENDIF
       ENDIF
#else
!----------------------------------------------------------------------
! RASM Climate Diagnostics - JR, AS, MS  - October 2016
!----------------------------------------------------------------------
       IF  ( (ialarm .EQ.  AUXHIST5_ALARM) .AND. (config_flags%restart) .AND. ( currTime .EQ. startTime ) ) THEN
         !  no AVG history output on the first time of the restart
       ELSE IF  ( (ialarm .EQ.  AUXHIST6_ALARM) .AND. (config_flags%restart) .AND. ( currTime .EQ. startTime ) ) THEN
         !  no DIURNAL history output on the first time of the restart
       ELSE IF ( grid%dfi_stage == DFI_FST .OR. grid%dfi_opt == DFI_NODFI ) THEN
         CALL med_hist_out ( grid , ialarm, config_flags )
       ENDIF
!----------------------------------------------------------------------
! end RASM Climate Diagnostics
!----------------------------------------------------------------------
#endif
       CALL WRFU_AlarmRingerOff( grid%alarms( ialarm ), rc=rc )
     ENDIF
   ENDDO

   DO ialarm = first_auxinput, last_auxinput
     IF ( .FALSE.) THEN
       rc = 1  ! dummy statement
#if ( WRF_CHEM == 1 )
! - Get chemistry data
     ELSE IF( ialarm .EQ. AUXINPUT5_ALARM .AND. config_flags%chem_opt > 0 ) THEN
       IF( config_flags%emiss_inpt_opt /= 0 ) THEN
         IF( WRFU_AlarmIsRinging( grid%alarms( ialarm ), rc=rc ) .OR.  &
         ((config_flags%restart) .AND. ( currTime .EQ. startTime ))) THEN
           call wrf_debug(15,' CALL med_read_wrf_chem_emiss ')
           CALL med_read_wrf_chem_emiss ( grid , config_flags )
           CALL WRFU_AlarmRingerOff( grid%alarms( ialarm ), rc=rc )
           call wrf_debug(15,' Back from CALL med_read_wrf_chem_emiss ')
         ENDIF
       ELSE
         IF( WRFU_AlarmIsRinging( grid%alarms( ialarm ), rc=rc ) ) THEN
           CALL med_auxinput_in ( grid, ialarm, config_flags )
           CALL WRFU_AlarmRingerOff( grid%alarms( ialarm ), rc=rc )
         ENDIF
       ENDIF
     ELSE IF(( ialarm .EQ. AUXINPUT7_ALARM .AND. config_flags%chem_opt > 0 ) .or.  &
             ( ialarm .EQ. AUXINPUT7_ALARM .AND. config_flags%tracer_opt > 0 ) )THEN
       IF( config_flags%biomass_burn_opt /= 0 ) THEN
         IF( WRFU_AlarmIsRinging( grid%alarms( ialarm ), rc=rc ) .OR.  &
         ((config_flags%restart) .AND. ( currTime .EQ. startTime ))) THEN
           CALL med_auxinput_in ( grid, ialarm, config_flags )
           WRITE ( message , FMT='(A,i3,A,i3)') 'Input data processed for aux input ',&
                  ialarm - first_auxinput + 1, ' for domain ',grid%id
           CALL wrf_debug ( 15 , message )
           CALL WRFU_AlarmRingerOff( grid%alarms( ialarm ), rc=rc )
         ENDIF
       ELSE
        IF( WRFU_AlarmIsRinging( grid%alarms( ialarm ), rc=rc ) ) THEN
          CALL med_auxinput_in ( grid, ialarm, config_flags )
          CALL WRFU_AlarmRingerOff( grid%alarms( ialarm ), rc=rc )
        ENDIF
       ENDIF
     ELSE IF( ialarm .EQ. AUXINPUT13_ALARM .AND. config_flags%chem_opt > 0 ) THEN
       IF( config_flags%emiss_opt_vol /= 0 ) THEN
         IF( WRFU_AlarmIsRinging( grid%alarms( ialarm ), rc=rc ) ) THEN
           call wrf_debug(15,' CALL med_read_wrf_volc_emiss ')
           CALL med_read_wrf_volc_emiss ( grid , config_flags )
           CALL WRFU_AlarmRingerOff( grid%alarms( ialarm ), rc=rc )
           call wrf_debug(15,' Back from CALL med_read_wrf_volc_emiss ')
         ENDIF
       ELSE
         IF( WRFU_AlarmIsRinging( grid%alarms( ialarm ), rc=rc ) ) THEN
           CALL med_auxinput_in ( grid, ialarm, config_flags )
           CALL WRFU_AlarmRingerOff( grid%alarms( ialarm ), rc=rc )
         ENDIF
       ENDIF
#endif
#if ( EM_CORE == 1 )
     ELSE IF( ialarm .EQ. AUXINPUT11_ALARM ) THEN
       IF( config_flags%obs_nudge_opt .EQ. 1) THEN
         CALL med_fddaobs_in ( grid , config_flags )
       ENDIF
#endif
     ELSE IF( WRFU_AlarmIsRinging( grid%alarms( ialarm ), rc=rc ) ) THEN
#if ( WRFPLUS == 1 )
       IF ( .NOT. grid%trajectory_io ) THEN
          ! For adjoint integration with disk IO, the basic state is one step before the domain time.
          IF ( config_flags%dyn_opt .EQ. dyn_em_ad .AND. ialarm .EQ. auxinput6_only  ) THEN
             grid%domain_clock%clockint%CurrTime =  ESMF_TimeInc (grid%domain_clock%clockint%CurrTime, &
                                grid%domain_clock%clockint%TimeStep )
          ENDIF
          CALL med_auxinput_in ( grid, ialarm, config_flags )
          IF ( config_flags%dyn_opt .EQ. dyn_em_ad .AND. ialarm .EQ. auxinput6_only ) THEN
             CALL domain_clock_get( grid, current_time=currTime, start_time=startTime, &
                                    current_timestr=timestr )
             WRITE(message, FMT='(A,A)') 'read xtraj from file at time stamp:', TRIM(timestr)
             CALL wrf_debug ( 1 , message )
          ENDIF
          IF ( config_flags%dyn_opt .EQ. dyn_em_ad .AND. ialarm .EQ. auxinput7_only ) THEN
             CALL domain_clock_get( grid, current_time=currTime, start_time=startTime, &
                                    current_timestr=timestr )
             WRITE(message, FMT='(A,A)') 'read ad. forcing from file at time stamp:', TRIM(timestr)
             CALL wrf_debug ( 1 , message )
          ENDIF
          IF ( ( config_flags%dyn_opt .EQ. dyn_em_ad .OR. config_flags%dyn_opt .EQ. dyn_em_tl ) &
               .AND. ialarm .EQ. auxinput6_only ) THEN
             CALL get_ijk_from_grid (  grid ,                   &
                                       ids, ide, jds, jde, kds, kde,    &
                                       ims, ime, jms, jme, kms, kme,    &
                                       ips, ipe, jps, jpe, kps, kpe    )

#ifdef DM_PARALLEL
             moist  => grid%moist
             scalar => grid%scalar
             chem   => grid%chem
             tracer => grid%tracer
#  include "HALO_EM_INIT_1.inc"
#  include "HALO_EM_INIT_2.inc"
#  include "HALO_EM_INIT_5.inc"
#endif
             DO ij = 1 , grid%num_tiles
                CALL rk_phys_bc_dry_2( config_flags,                     &
                                       grid%u_2, grid%v_2, grid%w_2,     &
                                       grid%t_2, grid%ph_2, grid%mu_2,   &
                                       ids, ide, jds, jde, kds, kde,     &
                                       ims, ime, jms, jme, kms, kme,     &
                                       ips, ipe, jps, jpe, kps, kpe,     &
                                       grid%i_start(ij), grid%i_end(ij), &
                                       grid%j_start(ij), grid%j_end(ij), &
                                       kps    , kpe               )
                DO im = PARAM_FIRST_SCALAR , num_moist
                   CALL set_physical_bc3d( grid%moist(:,:,:,im), 'p', config_flags,   &
                                           ids, ide, jds, jde, kds, kde,             &
                                           ims, ime, jms, jme, kms, kme,             &
                                           ips, ipe, jps, jpe, kps, kpe,             &
                                           grid%i_start(ij), grid%i_end(ij),         &
                                           grid%j_start(ij), grid%j_end(ij),         &
                                           kps    , kpe                       )
                ENDDO
                CALL set_physical_bc3d( grid%tke_2 , 'p', config_flags,  &
                                        ids, ide, jds, jde, kds, kde,            &
                                        ims, ime, jms, jme, kms, kme,            &
                                        ips, ipe, jps, jpe, kps, kpe,            &
                                        grid%i_start(ij), grid%i_end(ij),        &
                                        grid%j_start(ij), grid%j_end(ij),        &
                                        kps    , kpe                     )

             ENDDO
          ENDIF
       ELSE
         IF ( ialarm .EQ. auxinput6_only ) THEN ! Read Trajectory only
           IF ( config_flags%dyn_opt .EQ. dyn_em_ad  ) THEN
              grid%domain_clock%clockint%CurrTime =  ESMF_TimeInc (grid%domain_clock%clockint%CurrTime, &
                                grid%domain_clock%clockint%TimeStep )
           ENDIF
           CALL domain_clock_get ( grid, current_timestr=message )
           IF ( config_flags%dyn_opt .EQ. dyn_em_ad ) THEN
             CALL read_xtraj  ( message )
           ELSEIF ( config_flags%dyn_opt .EQ. dyn_em_tl ) THEN
             CALL read_xtraj_reverse  ( message )
           ENDIF
         ENDIF
         IF ( ialarm .EQ. auxinput7_only ) THEN ! Read Adjoint Forcing only
           CALL domain_clock_get ( grid, current_timestr=message )
           IF ( config_flags%dyn_opt .EQ. dyn_em_ad ) THEN
             CALL read_ad_forcing  ( message )
           ENDIF
         ENDIF
       ENDIF
       IF ( .NOT. grid%trajectory_io ) THEN
          WRITE ( message , FMT='(A,i3,A,i3)' )  'Input data processed from aux input ' , &
             ialarm - first_auxinput + 1, ' for domain ',grid%id
          CALL wrf_debug ( 1 , message )
       ENDIF
       ! Reverse the current time 
       IF ( config_flags%dyn_opt .EQ. dyn_em_ad .AND. ialarm .EQ. auxinput6_only  ) THEN
          CALL domain_clock_set( grid, time_step_seconds=model_config_rec%time_step )
          grid%domain_clock%clockint%CurrTime = ESMF_TimeInc (grid%domain_clock%clockint%CurrTime, &
                             grid%domain_clock%clockint%TimeStep )
          CALL domain_clock_set( grid, time_step_seconds=-1*model_config_rec%time_step )
       ENDIF
#else
       CALL med_auxinput_in ( grid, ialarm, config_flags )
       WRITE ( message , FMT='(A,i3,A,i3)' )  'Input data processed for aux input ' , &
          ialarm - first_auxinput + 1, ' for domain ',grid%id
       CALL wrf_debug ( 0 , message )
#endif
       CALL WRFU_AlarmRingerOff( grid%alarms( ialarm ), rc=rc )
     ENDIF
   ENDDO

#if (WRFPLUS != 1)
! - RESTART OUTPUT
   CALL WRFU_ClockGet( grid%domain_clock, CurrTime=currTime, StartTime=startTime )
   IF ( ( WRFU_AlarmIsRinging( grid%alarms( RESTART_ALARM ), rc=rc ) ) .AND. &
        ( currTime .NE. startTime ) ) THEN
#if ( HWRF == 1 )
!zhang's doing
     CALL domain_clock_get( grid, current_time=CurrTime )
     CALL WRFU_TimeGet( CurrTime, YY=julyr, dayOfYear=julday, H=hr, M=min, S=sec, MS=ms, rc=rc)
     gmt=hr+real(min)/60.+real(sec)/3600.+real(ms)/(1000*3600)
      if (grid%id .eq. 2) call med_namelist_out ( grid , config_flags )
!end of zhang's doing
#endif
     IF ( grid%id .EQ. 1 ) THEN
       ! Only the parent initiates the restart writing. Otherwise, different
       ! domains may be written out at different times and with different 
       ! time stamps in the file names.
       CALL med_restart_out ( grid , config_flags )
     ENDIF
     CALL WRFU_AlarmRingerOff( grid%alarms( RESTART_ALARM ), rc=rc )
   ELSE
     CALL WRFU_AlarmRingerOff( grid%alarms( RESTART_ALARM ), rc=rc )
   ENDIF

! - Look for boundary data after writing out history and restart files
   CALL med_latbound_in ( grid , config_flags )
#endif

   RETURN
END SUBROUTINE med_before_solve_io

#if (WRFPLUS == 1)
SUBROUTINE med_last_ad_solve_io ( grid , config_flags )
  ! Driver layer
   USE module_state_description
   USE module_domain	, ONLY : domain, domain_clock_get
   USE module_configure	, ONLY : grid_config_rec_type
   USE module_streams
   USE mediation_pertmod_io , ONLY : read_ad_forcing
  ! Model layer
   USE module_utility

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                                    :: ialarm
   INTEGER                                    :: rc

   TYPE(WRFU_Time)        :: currTime, startTime
   CHARACTER*256          :: message
   CHARACTER*80           :: timestr

   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT7_ALARM ), rc=rc ) ) THEN
     IF ( .NOT. grid%trajectory_io ) THEN
        CALL med_auxinput_in ( grid, AUXINPUT7_ALARM, config_flags )
     ELSE
        CALL domain_clock_get ( grid, current_timestr=message )
        CALL read_ad_forcing  ( message )
     ENDIF
     IF ( .NOT. grid%trajectory_io ) THEN
        CALL WRFU_ClockGet( grid%domain_clock, CurrTime=currTime, StartTime=startTime )
        CALL domain_clock_get( grid, current_time=currTime, start_time=startTime, &
                               current_timestr=timestr )
        WRITE(message, FMT='(A,A)') 'read ad. forcing from file at time stamp:', TRIM(timestr)
        CALL wrf_debug ( 1 , message )
        WRITE ( message , FMT='(A,i3,A,i3)' )  'Input data processed from aux input ' , &
             AUXINPUT7_ALARM - first_auxinput + 1, ' for domain ',grid%id
        CALL wrf_debug ( 1 , message )
     ENDIF
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT7_ALARM ), rc=rc )
   ENDIF

   RETURN
END SUBROUTINE med_last_ad_solve_io
#endif

SUBROUTINE med_after_solve_io ( grid , config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain
   USE module_timing
   USE module_configure , ONLY : grid_config_rec_type
  ! Model layer

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

   ! Compute time series variables
   CALL calc_ts(grid)

   ! Compute track variables 
   CALL track_driver(grid)

   RETURN
END SUBROUTINE med_after_solve_io

SUBROUTINE med_pre_nest_initial ( parent , newid , config_flags )
  ! Driver layer
#ifdef MOVE_NESTS
   USE module_domain    , ONLY : domain, domain_clock_get
#else
   USE module_domain    , ONLY : domain
#endif
#ifdef ESMFIO
   USE module_utility   , ONLY : WRFU_Time 
#else
   USE module_utility   , ONLY : WRFU_Time, WRFU_TimeEQ
#endif
   USE module_timing
   USE module_io_domain
   USE module_configure , ONLY : grid_config_rec_type
  ! Model layer

   IMPLICIT NONE

  ! Arguments
   TYPE(domain) , POINTER                      :: parent
   INTEGER, INTENT(IN)                         :: newid
   TYPE (grid_config_rec_type) , INTENT(INOUT) :: config_flags
   TYPE (grid_config_rec_type)                 :: nest_config_flags

  ! Local
   INTEGER                :: itmp, fid, ierr, icnt
   CHARACTER*256          :: rstname, message, timestr

   TYPE(WRFU_Time)        :: strt_time, cur_time

#ifdef MOVE_NESTS

   CALL domain_clock_get( parent, current_timestr=timestr, start_time=strt_time, current_time=cur_time )
   CALL construct_filename2a ( rstname , config_flags%rst_inname , newid , 2 , timestr )

#ifdef ESMFIO
    IF ( config_flags%restart .AND. (cur_time .EQ. strt_time) ) THEN
#else
    IF ( config_flags%restart .AND. WRFU_TimeEQ(cur_time,strt_time) ) THEN
#endif
     WRITE(message,*)'RESTART: nest, opening ',TRIM(rstname),' for reading header information only'
     CALL wrf_message ( message )
  ! note that the parent pointer is not strictly correct, but nest is not allocated yet and
  ! only the i/o communicator fields are used from "parent" (and those are dummies in current
  ! implementation.
     CALL open_r_dataset ( fid , TRIM(rstname) , parent , config_flags , "DATASET=RESTART", ierr )
     IF ( ierr .NE. 0 ) THEN
       WRITE( message , '("program wrf: error opening ",A32," for reading")') TRIM(rstname)
       CALL WRF_ERROR_FATAL ( message )
     ENDIF

  ! update the values of parent_start that were read in from the namelist (nest may have moved)
     CALL wrf_get_dom_ti_integer ( fid , 'I_PARENT_START' ,  itmp  , 1 , icnt, ierr )
     IF ( ierr .EQ. 0 ) THEN
       config_flags%i_parent_start = itmp
       CALL nl_set_i_parent_start ( newid , config_flags%i_parent_start )
     ENDIF
     CALL wrf_get_dom_ti_integer ( fid , 'J_PARENT_START' ,  itmp  , 1 , icnt, ierr )
     IF ( ierr .EQ. 0 ) THEN
       config_flags%j_parent_start = itmp
       CALL nl_set_j_parent_start ( newid , config_flags%j_parent_start )
     ENDIF

     CALL close_dataset ( fid , config_flags , "DATASET=RESTART" )
   ENDIF
#endif

END SUBROUTINE med_pre_nest_initial


SUBROUTINE med_nest_initial ( parent , nest , config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain , domain_clock_get , get_ijk_from_grid
   USE module_timing
   USE module_io_domain
   USE module_configure , ONLY : grid_config_rec_type
   USE module_utility
#ifdef DM_PARALLEL
   USE module_dm, ONLY : local_communicator,                                     &
                         mpi_comm_to_mom, mpi_comm_to_kid, which_kid
#endif
  ! Model layer

   IMPLICIT NONE

  ! Arguments
   TYPE(domain) , POINTER                     :: parent, nest
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   TYPE (grid_config_rec_type)                :: nest_config_flags

  ! Local
   LOGICAL, EXTERNAL      :: wrf_dm_on_monitor
   TYPE(WRFU_Time)        :: strt_time, cur_time
   CHARACTER * 256        :: rstname , timestr
   CHARACTER * 256        :: message
   INTEGER                :: fid
   INTEGER                :: ierr
   INTEGER                :: i , j, rc
   INTEGER                :: ids , ide , jds , jde , kds , kde , &
                             ims , ime , jms , jme , kms , kme , &
                             ips , ipe , jps , jpe , kps , kpe

#if (EM_CORE == 1)
#ifdef MOVE_NESTS
   TYPE (WRFU_TimeInterval) :: interval, TimeSinceStart
   INTEGER :: vortex_interval , n
#endif
   INTEGER                :: save_itimestep ! This is a kludge, correct fix will 
                                            ! involve integrating the time-step
                                            ! counting into the time manager.
                                            ! JM 20040604
   REAL, ALLOCATABLE, DIMENSION(:,:) ::   save_acsnow             &
                                         ,save_acsnom             &
                                         ,save_cuppt              &
                                         ,save_rainc              &
                                         ,save_rainnc             &
                                         ,save_sfcevp             &
                                         ,save_sfcrunoff          &
                                         ,save_udrunoff


   INTERFACE
     SUBROUTINE med_interp_domain ( parent , nest )
        USE module_domain       , ONLY : domain
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_interp_domain

    !KAL
     SUBROUTINE init_domain_vert_nesting ( parent, nest, use_baseparam_fr_nml )
    !KAL this is a driver to initialize the vertical coordinates for the nest when vertical nesting is used.
       USE module_domain, ONLY : domain
       IMPLICIT NONE
       TYPE(domain), POINTER                      :: parent, nest
       LOGICAL                                    :: use_baseparam_fr_nml
     END SUBROUTINE init_domain_vert_nesting

     SUBROUTINE med_interp_domain_small ( parent , nest )
        USE module_domain       , ONLY : domain
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_interp_domain_small

     SUBROUTINE med_initialdata_input_ptr( nest , config_flags )
        USE module_domain       , ONLY : domain
        USE module_configure    , ONLY : grid_config_rec_type
        TYPE (grid_config_rec_type), INTENT(IN) :: config_flags
        TYPE(domain) , POINTER :: nest
     END SUBROUTINE med_initialdata_input_ptr

     SUBROUTINE med_nest_feedback ( parent , nest , config_flags )
       USE module_domain        , ONLY : domain
       USE module_configure     , ONLY : grid_config_rec_type
       TYPE (domain), POINTER ::  nest , parent
       TYPE (grid_config_rec_type), INTENT(IN) :: config_flags
     END SUBROUTINE med_nest_feedback

     SUBROUTINE start_domain ( grid , allowed_to_move )
        USE module_domain       , ONLY : domain
        TYPE(domain) :: grid
        LOGICAL, INTENT(IN) :: allowed_to_move
     END SUBROUTINE start_domain

     SUBROUTINE  blend_terrain ( ter_interpolated , ter_input , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           ips , ipe , jps , jpe , kps , kpe )
       INTEGER                           :: ids , ide , jds , jde , kds , kde , &
                                            ims , ime , jms , jme , kms , kme , &
                                            ips , ipe , jps , jpe , kps , kpe
       REAL , DIMENSION(ims:ime,jms:jme) :: ter_interpolated
       REAL , DIMENSION(ims:ime,jms:jme) :: ter_input
     END SUBROUTINE blend_terrain

     SUBROUTINE  copy_3d_field ( ter_interpolated , ter_input , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           ips , ipe , jps , jpe , kps , kpe )
       INTEGER                           :: ids , ide , jds , jde , kds , kde , &
                                            ims , ime , jms , jme , kms , kme , &
                                            ips , ipe , jps , jpe , kps , kpe
       REAL , DIMENSION(ims:ime,jms:jme) :: ter_interpolated
       REAL , DIMENSION(ims:ime,jms:jme) :: ter_input
     END SUBROUTINE copy_3d_field

     SUBROUTINE  input_terrain_rsmas ( grid ,                  &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           ips , ipe , jps , jpe , kps , kpe )
       USE module_domain        , ONLY : domain
       TYPE ( domain ) :: grid
       INTEGER                           :: ids , ide , jds , jde , kds , kde , &
                                            ims , ime , jms , jme , kms , kme , &
                                            ips , ipe , jps , jpe , kps , kpe
     END SUBROUTINE input_terrain_rsmas

     SUBROUTINE wrf_tsin ( grid , ierr )
        USE module_domain
        TYPE ( domain ), INTENT(INOUT) :: grid
        INTEGER, INTENT(INOUT) :: ierr
     END SUBROUTINE wrf_tsin

   END INTERFACE

   CALL interp_init

   CALL domain_clock_get( parent, start_time=strt_time, current_time=cur_time )

   IF ( .not. ( config_flags%restart .AND. strt_time .EQ. cur_time ) ) THEN
     nest%first_force = .true.

     IF ( nest%active_this_task ) THEN
! initialize nest with interpolated data from the parent
       nest%imask_nostag = 1
       nest%imask_xstag = 1
       nest%imask_ystag = 1
       nest%imask_xystag = 1
     ENDIF

#ifdef MOVE_NESTS
     parent%nest_pos = parent%ht
     where ( parent%nest_pos .gt. 0. ) parent%nest_pos = parent%nest_pos + 500.  ! make a cliff
#endif

! initialize some other constants (and 1d arrays in z)
     CALL init_domain_constants ( parent, nest )

     if (nest%e_vert /= parent%e_vert) then
         ! set up coordinate variables for nest with vertical grid refinement (1d variables in z are done later in med_interp_domain)
       CALL init_domain_vert_nesting ( parent, nest, config_flags%use_baseparam_fr_nml )
     endif


! fill in entire fine grid domain with interpolated coarse grid data
     CALL med_interp_domain( parent, nest )

!  De-reference dimension information stored in the grid data structure.
     CALL get_ijk_from_grid (  nest ,                   &
                               ids, ide, jds, jde, kds, kde,    &
                               ims, ime, jms, jme, kms, kme,    &
                               ips, ipe, jps, jpe, kps, kpe    )
  
! get the nest config flags
     CALL model_to_grid_config_rec ( nest%id , model_config_rec , nest_config_flags )

     IF ( nest_config_flags%input_from_file .OR. nest_config_flags%input_from_hires ) THEN

       WRITE(message,FMT='(A,I2,A)') '*** Initializing nest domain #',nest%id,&
                                      ' from an input file. ***'
       CALL wrf_debug ( 0 , message )

! Store horizontally interpolated terrain-based fields in temp location if the input
! data is from a pristine, un-cycled model input file.  For the original topo from
! the real program, we will need to adjust the terrain (and a couple of other base-
! state fields) so reflect the smoothing and matching between the parent and child
! domains.

       CALL  copy_3d_field ( nest%ht_int  , nest%ht , &
                             ids , ide , jds , jde , 1   , 1   , &
                             ims , ime , jms , jme , 1   , 1   , &
                             ips , ipe , jps , jpe , 1   , 1   )
       CALL  copy_3d_field ( nest%mub_fine , nest%mub , &
                             ids , ide , jds , jde , 1   , 1   , &
                             ims , ime , jms , jme , 1   , 1   , &
                             ips , ipe , jps , jpe , 1   , 1   )
       CALL  copy_3d_field ( nest%phb_fine , nest%phb , &
                             ids , ide , jds , jde , kds , kde , &
                             ims , ime , jms , jme , kms , kme , &
                             ips , ipe , jps , jpe , kps , kpe )

       IF ( nest_config_flags%input_from_file ) THEN
! read input from dataset
          CALL med_initialdata_input_ptr( nest , nest_config_flags )

       ELSE IF ( nest_config_flags%input_from_hires ) THEN
! read in high res topography
          CALL  input_terrain_rsmas ( nest,                               &
                                      ids , ide , jds , jde , 1   , 1   , &
                                      ims , ime , jms , jme , 1   , 1   , &
                                      ips , ipe , jps , jpe , 1   , 1   )
       ENDIF

       ! save elevation and mub for temp and qv adjustment

       CALL  copy_3d_field ( nest%ht_fine , nest%ht , &
                             ids , ide , jds , jde , 1   , 1   , &
                             ims , ime , jms , jme , 1   , 1   , &
                             ips , ipe , jps , jpe , 1   , 1   )
       CALL  copy_3d_field ( nest%mub_save , nest%mub , &
                             ids , ide , jds , jde , 1   , 1   , &
                             ims , ime , jms , jme , 1   , 1   , &
                             ips , ipe , jps , jpe , 1   , 1   )

! blend parent and nest fields: terrain, mub, and phb.  The ht, mub and phb are used in start_domain.

       IF ( nest%save_topo_from_real == 1 ) THEN
          CALL  blend_terrain ( nest%ht_int  , nest%ht , &
                                ids , ide , jds , jde , 1   , 1   , &
                                ims , ime , jms , jme , 1   , 1   , &
                                ips , ipe , jps , jpe , 1   , 1   )
          CALL  blend_terrain ( nest%mub_fine , nest%mub , &
                                ids , ide , jds , jde , 1   , 1   , &
                                ims , ime , jms , jme , 1   , 1   , &
                                ips , ipe , jps , jpe , 1   , 1   )
          CALL  blend_terrain ( nest%phb_fine , nest%phb , &
                                ids , ide , jds , jde , kds , kde , &
                                ims , ime , jms , jme , kms , kme , &
                                ips , ipe , jps , jpe , kps , kpe )
       ENDIF

       !  adjust temp and qv

       CALL adjust_tempqv ( nest%mub , nest%mub_save , &
                            nest%c3h , nest%c4h , nest%znw , nest%p_top , &
                            nest%t_2 , nest%p , nest%moist(ims,kms,jms,P_QV) , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            ips , ipe , jps , jpe , kps , kpe )

     ELSE
       WRITE(message,FMT='(A,I2,A,I2,A)') '*** Initializing nest domain #',nest%id,&
                                     ' by horizontally interpolating parent domain #' ,parent%id, &
                                     '. ***'
       CALL wrf_debug ( 0 , message )

#if (DA_CORE != 1)
       ! For nests without an input file, we still need to read time series locations
       !   from the tslist file
       IF ( nest%active_this_task) THEN
          CALL push_communicators_for_domain( nest%id )
          CALL wrf_tsin( nest , ierr )
          CALL pop_communicators_for_domain
       ENDIF
#endif
     END IF

! feedback, mostly for this new terrain, but it is the safe thing to do
     parent%ht_coarse = parent%ht

     CALL med_nest_feedback ( parent , nest , config_flags )

! This is the new interpolation for specific 3d arrays that are sensitive to the
! topography diffs betwixt the CG and the FG.
     IF ( config_flags%nest_interp_coord .EQ. 1 ) THEN
       call wrf_debug(1,'mediation_integrate.F, calling   med_interp_domain_small')
       CALL med_interp_domain_small( parent, nest )
       call wrf_debug(1,'mediation_integrate.F, back from med_interp_domain_small')
     END IF

! set some other initial fields, fill out halos, base fields; re-do parent due
! to new terrain elevation from feedback

     IF ( nest%active_this_task) THEN
       nest%imask_nostag = 1
       nest%imask_xstag = 1
       nest%imask_ystag = 1
       nest%imask_xystag = 1
       nest%press_adj = .TRUE.
       CALL push_communicators_for_domain( nest%id )
       CALL start_domain ( nest , .TRUE. )
       CALL pop_communicators_for_domain
     ENDIF

#if defined(DM_PARALLEL) && ! defined(STUBMPI)
     CALL push_communicators_for_domain( parent%id )
     CALL MPI_Barrier( local_communicator, ierr )     
     CALL pop_communicators_for_domain
#endif

     IF ( parent%active_this_task ) THEN
     CALL push_communicators_for_domain( parent%id )
! kludge: 20040604
     CALL get_ijk_from_grid (  parent ,                   &
                               ids, ide, jds, jde, kds, kde,    &
                               ims, ime, jms, jme, kms, kme,    &
                               ips, ipe, jps, jpe, kps, kpe    )
  
     ALLOCATE( save_acsnow(ims:ime,jms:jme) )
     ALLOCATE( save_acsnom(ims:ime,jms:jme) )
     ALLOCATE( save_cuppt(ims:ime,jms:jme) )
     ALLOCATE( save_rainc(ims:ime,jms:jme) )
     ALLOCATE( save_rainnc(ims:ime,jms:jme) )
     ALLOCATE( save_sfcevp(ims:ime,jms:jme) )
     ALLOCATE( save_sfcrunoff(ims:ime,jms:jme) )
     ALLOCATE( save_udrunoff(ims:ime,jms:jme) )
     save_acsnow       = parent%acsnow
     save_acsnom       = parent%acsnom
     save_cuppt        = parent%cuppt
     save_rainc        = parent%rainc
     save_rainnc       = parent%rainnc
     save_sfcevp       = parent%sfcevp
     save_sfcrunoff    = parent%sfcrunoff
     save_udrunoff     = parent%udrunoff
     save_itimestep    = parent%itimestep
     parent%imask_nostag = 1
     parent%imask_xstag = 1
     parent%imask_ystag = 1
     parent%imask_xystag = 1

     parent%press_adj = .FALSE.
     CALL start_domain ( parent , .TRUE. )

     parent%acsnow     = save_acsnow
     parent%acsnom     = save_acsnom
     parent%cuppt      = save_cuppt
     parent%rainc      = save_rainc
     parent%rainnc     = save_rainnc
     parent%sfcevp     = save_sfcevp
     parent%sfcrunoff  = save_sfcrunoff
     parent%udrunoff   = save_udrunoff
     parent%itimestep  = save_itimestep
     DEALLOCATE( save_acsnow )
     DEALLOCATE( save_acsnom )
     DEALLOCATE( save_cuppt )
     DEALLOCATE( save_rainc )
     DEALLOCATE( save_rainnc )
     DEALLOCATE( save_sfcevp )
     DEALLOCATE( save_sfcrunoff )
     DEALLOCATE( save_udrunoff )
! end of kludge: 20040604
     CALL pop_communicators_for_domain
     ENDIF

  ELSE  ! restart

!TODO -- have to look at restarts yet

     IF ( wrf_dm_on_monitor() ) CALL start_timing

     CALL domain_clock_get( nest, current_timestr=timestr )
     CALL construct_filename2a ( rstname , config_flags%rst_inname , nest%id , 2 , timestr )

     WRITE(message,*)'RESTART: nest, opening ',TRIM(rstname),' for reading'
     CALL wrf_message ( message )
     CALL model_to_grid_config_rec ( nest%id , model_config_rec , nest_config_flags )
     CALL open_r_dataset ( fid , TRIM(rstname) , nest , nest_config_flags , "DATASET=RESTART", ierr )
     IF ( ierr .NE. 0 ) THEN
       WRITE( message , '("program wrf: error opening ",A32," for reading")') TRIM(rstname)
       CALL WRF_ERROR_FATAL ( message )
     ENDIF
     CALL input_restart ( fid,   nest , nest_config_flags , ierr )
     CALL close_dataset ( fid , nest_config_flags , "DATASET=RESTART" )

     IF ( wrf_dm_on_monitor() ) THEN
       WRITE ( message , FMT = '("processing restart file for domain ",I8)' ) nest%id
       CALL end_timing ( TRIM(message) )
     ENDIF

     nest%imask_nostag = 1
     nest%imask_xstag = 1
     nest%imask_ystag = 1
     nest%imask_xystag = 1
     nest%press_adj = .FALSE.
     CALL start_domain ( nest , .TRUE. )
#ifndef MOVE_NESTS
! this doesn't need to be done for moving nests, since ht_coarse is part of the restart
     parent%ht_coarse = parent%ht
#else
#  if 1
! In case of a restart, assume that the movement has already occurred in the previous
! run and turn off the alarm for the starting time. We must impose a requirement that the
! run be restarted on-interval.  Test for that and print a warning if it isn't.
! Note, simulation_start, etc. should be available as metadata in the restart file, and
! these will have gotten, set, and retrievable as rconfig data been set in share/input_wrf.F
! using the nl_get routines below.  JM 20060314

     CALL nl_get_vortex_interval ( nest%id , vortex_interval )
     CALL WRFU_TimeIntervalSet( interval, M=vortex_interval, rc=rc )

     CALL domain_clock_get( nest, timeSinceSimulationStart=TimeSinceStart )
     n = WRFU_TimeIntervalDIVQuot( TimeSinceStart , interval )
     IF ( ( interval * n ) .NE. TimeSinceStart ) THEN
       CALL wrf_message('WARNING: Restart is not on a vortex_interval time boundary.')
       CALL wrf_message('The code will work but results will not agree exactly with a ')
       CALL wrf_message('a run that was done straight-through, without a restart.') 
     ENDIF
!! In case of a restart, assume that the movement has already occurred in the previous
!! run and turn off the alarm for the starting time. We must impose a requirement that the
!! run be restarted on-interval.  Test for that and print a warning if it isn't.
!! Note, simulation_start, etc. should be available as metadata in the restart file, and
!! these will have gotten, set, and retrievable as rconfig data been set in share/input_wrf.F
!! using the nl_get routines below.  JM 20060314
!     CALL WRFU_AlarmRingerOff( nest%alarms( COMPUTE_VORTEX_CENTER_ALARM ), rc=rc )

#  else
! this code, currently commented out, is an attempt to have the
! vortex centering interval be set according to simulation start
! time (rather than run start time) in case of a restart. But
! there are other problems (the WRF clock is currently using
! run-start as it's start time) so the alarm still would not fire
! right if the model were started off-interval.  Leave it here and
! enable when the clock is changed to use sim-start for start time.
! JM 20060314
     CALL nl_get_vortex_interval ( nest%id , vortex_interval )
     CALL WRFU_TimeIntervalSet( interval, M=vortex_interval, rc=rc )

     CALL domain_clock_get( nest, timeSinceSimulationStart=TimeSinceStart )

     CALL domain_alarm_create( nest,  COMPUTE_VORTEX_CENTER_ALARM, interval  )
     CALL WRFU_AlarmEnable( nest%alarms( COMPUTE_VORTEX_CENTER_ALARM ), rc=rc )
     n = WRFU_TimeIntervalDIVQuot( TimeSinceStart , interval )
     IF ( ( interval * n ) .EQ. TimeSinceStart ) THEN
       CALL WRFU_AlarmRingerOn( nest%alarms( COMPUTE_VORTEX_CENTER_ALARM ), rc=rc )
     ELSE 
       CALL WRFU_AlarmRingerOff( nest%alarms( COMPUTE_VORTEX_CENTER_ALARM ), rc=rc )
     ENDIF
#  endif
#endif

  ENDIF

#endif

#if (NMM_CORE == 1 && NMM_NEST == 1)
!===================================================================================
!  Added for the NMM core. This is gopal's doing.
!===================================================================================

   INTERFACE

     SUBROUTINE med_nest_egrid_configure ( parent , nest )
        USE module_domain       , ONLY : domain
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_nest_egrid_configure 

     SUBROUTINE med_construct_egrid_weights ( parent , nest )
        USE module_domain       , ONLY : domain
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_construct_egrid_weights

     SUBROUTINE BASE_STATE_PARENT ( Z3d,Q3d,T3d,PSTD,        &
                                    PINT,T,Q,CWM,            &
                                    FIS,QSH,PD,PDTOP,PTOP,   &
                                    ETA1,ETA2,               &
                                    DETA1,DETA2,             &
                                    IDS,IDE,JDS,JDE,KDS,KDE, &
                                    IMS,IME,JMS,JME,KMS,KME, &
                                    IPS,IPE,JPS,JPE,KPS,KPE  )
!

         USE MODULE_MODEL_CONSTANTS
         IMPLICIT NONE
         INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
         INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
         INTEGER,    INTENT(IN   )                            :: IPS,IPE,JPS,JPE,KPS,KPE
         REAL,       INTENT(IN   )                            :: PDTOP,PTOP
         REAL, DIMENSION(KMS:KME),                 INTENT(IN) :: ETA1,ETA2,DETA1,DETA2
         REAL, DIMENSION(IMS:IME,JMS:JME),         INTENT(IN) :: FIS,PD,QSH
         REAL, DIMENSION(IMS:IME,JMS:JME,KMS:KME), INTENT(IN) :: PINT,T,Q,CWM
         REAL, DIMENSION(KMS:KME)                , INTENT(OUT):: PSTD
         REAL, DIMENSION(IMS:IME,JMS:JME,KMS:KME), INTENT(OUT):: Z3d,Q3d,T3d

     END SUBROUTINE BASE_STATE_PARENT

     SUBROUTINE NEST_TERRAIN ( nest, config_flags )
       USE module_domain        , ONLY : domain
       USE module_configure     , ONLY : grid_config_rec_type
       TYPE(domain) , POINTER                        :: nest
       TYPE(grid_config_rec_type) , INTENT(IN)       :: config_flags
     END SUBROUTINE NEST_TERRAIN

    SUBROUTINE med_interp_domain ( parent , nest )
        USE module_domain       , ONLY : domain
        TYPE(domain) , POINTER                 :: parent , nest
    END SUBROUTINE med_interp_domain

    SUBROUTINE med_init_domain_constants_nmm ( parent, nest )
        USE module_domain       , ONLY : domain
        TYPE(domain) , POINTER                    :: parent , nest
    END SUBROUTINE med_init_domain_constants_nmm

    SUBROUTINE start_domain ( grid , allowed_to_move )
        USE module_domain       , ONLY : domain
        TYPE(domain) :: grid
        LOGICAL, INTENT(IN) :: allowed_to_move
    END SUBROUTINE start_domain

   END INTERFACE

#if ( HWRF == 1 )
!zhang's doing test
   if (config_flags%restart .or. nest%analysis) then
   nest%first_force = .true.
   else
   nest%first_force = .false.
   endif
!end of zhang's doing

   ! Do we run the MOIST and SCALAR interp/smooth functions? (u=(), d=(), f=() and s=())

   ! Only run it if a non-bulk scheme is in use, since bulk schemes do
   ! not use MOIST and SCALAR as prognostic variables (they are
   ! recalculated on the fly every timestep):
!zhang's doing for analysis option
  IF(.not. nest%analysis .and. .not. config_flags%restart)THEN    ! initialize for cold-start
#endif

!----------------------------------------------------------------------------
!  initialize nested domain configurations including setting up wbd,sbd, etc 
!----------------------------------------------------------------------------

   CALL med_nest_egrid_configure ( parent , nest )

!-------------------------------------------------------------------------
!  initialize lat-lons and determine weights 
!-------------------------------------------------------------------------

    CALL med_construct_egrid_weights ( parent, nest )
!
!
!  De-reference dimension information stored in the grid data structure.
!
!  From the hybrid, construct the GPMs on isobaric surfaces and then interpolate those
!  values on to the nested domain. 23 standard prssure levels are assumed here. For
!  levels below ground, lapse rate atmosphere is assumed before the use of vertical
!  spline interpolation 
!


    IDS = parent%sd31
    IDE = parent%ed31
    JDS = parent%sd32
    JDE = parent%ed32
    KDS = parent%sd33
    KDE = parent%ed33

    IMS = parent%sm31
    IME = parent%em31
    JMS = parent%sm32
    JME = parent%em32
    KMS = parent%sm33
    KME = parent%em33

    IPS = parent%sp31
    IPE = parent%ep31
    JPS = parent%sp32
    JPE = parent%ep32
    KPS = parent%sp33
    KPE = parent%ep33

    ! CALL BASE_STATE_PARENT ( parent%Z3d,parent%Q3d,parent%T3d,parent%PSTD,  &
    !                          parent%PINT,parent%T,parent%Q,parent%CWM,      &
    !                          parent%FIS,parent%QSH,parent%PD,parent%pdtop,parent%pt,   &
    !                          parent%ETA1,parent%ETA2,                               &
    !                          parent%DETA1,parent%DETA2,                             &
    !                          IDS,IDE,JDS,JDE,KDS,KDE,                                       &
    !                          IMS,IME,JMS,JME,KMS,KME,                                       &
    !                          IPS,IPE,JPS,JPE,KPS,KPE                                        )

!  
!   Set new terrain. Since some terrain adjustment is done within the interpolation calls
!   at the next step, the new terrain over the nested domain has to be called here.
!
    IDS = nest%sd31
    IDE = nest%ed31
    JDS = nest%sd32
    JDE = nest%ed32
    KDS = nest%sd33
    KDE = nest%ed33

    IMS = nest%sm31
    IME = nest%em31
    JMS = nest%sm32
    JME = nest%em32
    KMS = nest%sm33
    KME = nest%em33

    IPS = nest%sp31
    IPE = nest%ep31
    JPS = nest%sp32
    JPE = nest%ep32
    KPS = nest%sp33
    KPE = nest%ep33


    IF ( nest%active_this_task ) THEN
    CALL NEST_TERRAIN ( nest, config_flags )
    ENDIF

!   Initialize some more constants required especially for terrain adjustment processes

    IF ( nest%active_this_task .AND. parent%active_this_task ) THEN
    nest%PSTD=parent%PSTD
    ENDIF
#ifdef DM_PARALLEL
    IF ( nest%active_this_task .OR. parent%active_this_task ) THEN
      IF ( parent%active_this_task ) THEN
        CALL BYTE_BCAST( parent%PSTD, KME*RWORDSIZE, mpi_comm_to_kid( which_kid( nest%id ) , parent%id ) )
      ELSE
        CALL BYTE_BCAST( nest%PSTD, KME*RWORDSIZE, mpi_comm_to_mom( nest%id ) )
      ENDIF
    ENDIF
#endif

    IF ( nest%active_this_task ) THEN
      nest%KZMAX=KME
      parent%KZMAX=KME  ! just for safety

      DO J = JPS, MIN(JPE,JDE-1)
        DO I = IPS, MIN(IPE,IDE-1)
         nest%fis(I,J)=nest%hres_fis(I,J)
       ENDDO
      ENDDO

!--------------------------------------------------------------------------
!  interpolation call
!--------------------------------------------------------------------------

! initialize nest with interpolated data from the parent

      nest%imask_nostag = 0 
      nest%imask_xstag  = 0 
      nest%imask_ystag  = 0 
      nest%imask_xystag = 0 
    ENDIF

#if ( HWRF == 1 )
   CALL med_interp_domain( parent, nest )
#else
    CALL domain_clock_get( parent, start_time=strt_time, current_time=cur_time )

    IF ( .not. ( config_flags%restart .AND. strt_time .EQ. cur_time ) ) THEN

write(0,*)__FILE__,__LINE__,parent%id,nest%id
     CALL med_interp_domain( parent, nest )
write(0,*)__FILE__,__LINE__

    ELSE

     CALL domain_clock_get( nest, current_timestr=timestr )
     CALL construct_filename2a ( rstname , config_flags%rst_inname , nest%id , 2 , timestr )

     WRITE(message,*)'RESTART: nest, opening ',TRIM(rstname),' for reading'
     CALL wrf_message ( message )
     CALL model_to_grid_config_rec ( nest%id , model_config_rec , nest_config_flags )
     CALL open_r_dataset ( fid , TRIM(rstname) , nest , nest_config_flags , "DATASET=RESTART", ierr )
     IF ( ierr .NE. 0 ) THEN
       WRITE( message , '("program wrf: error opening ",A32," for reading")') TRIM(rstname)
       CALL WRF_ERROR_FATAL ( message )
     ENDIF
     CALL input_restart ( fid,   nest , nest_config_flags , ierr )
     CALL close_dataset ( fid , nest_config_flags , "DATASET=RESTART" )

    END IF

#endif
!------------------------------------------------------------------------------
!  set up constants (module_initialize_real.F for nested nmm domain)
!-----------------------------------------------------------------------------

    CALL med_init_domain_constants_nmm ( parent, nest )

!--------------------------------------------------------------------------------------
! set some other initial fields, fill out halos, etc. 
!--------------------------------------------------------------------------------------

    IF ( nest%active_this_task) THEN
      CALL push_communicators_for_domain( nest%id )
      CALL start_domain ( nest, .TRUE.)
      CALL pop_communicators_for_domain
    ENDIF
    

#if ( HWRF == 1 )
!zhang's doing: else for analysis or restart option

!zhang test
    CALL nl_set_isice ( nest%id , config_flags%isice )   
    CALL nl_set_isoilwater ( nest%id , config_flags%isoilwater )   
    CALL nl_set_isurban ( nest%id , config_flags%isurban )   
    CALL nl_set_gmt    ( nest%id , config_flags%gmt    )   
    CALL nl_set_julyr (nest%id, config_flags%julyr)       
    CALL nl_set_julday ( nest%id , config_flags%julday )
!zhang test ends
    IF ( nest%active_this_task) THEN
     CALL model_to_grid_config_rec ( nest%id , model_config_rec , nest_config_flags )
     CALL push_communicators_for_domain( nest%id )
     CALL med_analysis_out ( nest, nest_config_flags )
     CALL pop_communicators_for_domain
    ENDIF

   ELSE

!------------------------------------------------------------------------------------
!  read in analysis (equivalent of restart for the nested domains)
!------------------------------------------------------------------------------------

!zhang's doing
  IF ( nest%active_this_task) THEN
  IF( nest%analysis .and. .not. config_flags%restart)THEN
   CALL push_communicators_for_domain( nest%id )
   CALL med_analysis_in ( nest, config_flags )
   CALL pop_communicators_for_domain
  ELSE IF (config_flags%restart)THEN
   CALL push_communicators_for_domain( nest%id )
   CALL med_restart_in ( nest, config_flags )
   CALL pop_communicators_for_domain
  ENDIF
  ENDIF
!end of zhang's doing

!----------------------------------------------------------------------------
!  initialize nested domain configurations including setting up wbd,sbd, etc
!----------------------------------------------------------------------------

   CALL med_nest_egrid_configure ( parent , nest )

!-------------------------------------------------------------------------
!  initialize lat-lons and determine weights (overwrite for safety)
!-------------------------------------------------------------------------

   CALL med_construct_egrid_weights ( parent, nest )

   nest%imask_nostag = 0
   nest%imask_xstag  = 0
   nest%imask_ystag  = 0
   nest%imask_xystag = 0

!------------------------------------------------------------------------------
!  set up constants (module_initialize_real.F for nested nmm domain)
!-----------------------------------------------------------------------------

    CALL med_init_domain_constants_nmm ( parent, nest )

!--------------------------------------------------------------------------------------
! set some other initial fields, fill out halos, etc. (again, safety sake only)
! Also, in order to accomodate some physics initialization after nest move, set
! analysis back to false for future use
!--------------------------------------------------------------------------------------


  IF ( nest%active_this_task) THEN
    CALL push_communicators_for_domain( nest%id )
    CALL start_domain ( nest, .TRUE.)
    CALL pop_communicators_for_domain
  ENDIF

    nest%analysis=.FALSE.
    CALL nl_set_analysis( nest%id, nest%analysis)

  ENDIF

#endif

!===================================================================================
!  Added for the NMM core. End of gopal's doing.
!===================================================================================
#endif
  RETURN
END SUBROUTINE med_nest_initial

SUBROUTINE init_domain_constants ( parent , nest )
   USE module_domain    , ONLY : domain
   IMPLICIT NONE
   TYPE(domain) :: parent , nest
#if (EM_CORE == 1)
   CALL init_domain_constants_em ( parent, nest )
#endif
END SUBROUTINE init_domain_constants


SUBROUTINE med_nest_force ( parent , nest )
  ! Driver layer
   USE module_domain    , ONLY : domain
   USE module_timing
   USE module_configure , ONLY : grid_config_rec_type
  ! Model layer
  ! External
   USE module_utility

   IMPLICIT NONE

  ! Arguments
   TYPE(domain) , POINTER                     :: parent, nest
  ! Local
   INTEGER                                    :: idum1 , idum2 , fid, rc

#if (NMM_CORE == 1 && NMM_NEST == 1)
   INTEGER                  :: IDS,IDE,JDS,JDE,KDS,KDE     ! gopal
   INTEGER                  :: IMS,IME,JMS,JME,KMS,KME
   INTEGER                  :: ITS,ITE,JTS,JTE,KTS,KTE
#endif

   INTERFACE
     SUBROUTINE med_force_domain ( parent , nest )
        USE module_domain       , ONLY : domain
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_force_domain
     SUBROUTINE med_interp_domain ( parent , nest )
        USE module_domain       , ONLY : domain
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_interp_domain
#if (NMM_CORE == 1 && NMM_NEST == 1)
!===================================================================================
!  Added for the NMM core. This is gopal's doing.
!===================================================================================

     SUBROUTINE BASE_STATE_PARENT ( Z3d,Q3d,T3d,PSTD,        &
                                    PINT,T,Q,CWM,            &
                                    FIS,QSH,PD,PDTOP,PTOP,   &
                                    ETA1,ETA2,               &
                                    DETA1,DETA2,             &
                                    IDS,IDE,JDS,JDE,KDS,KDE, &
                                    IMS,IME,JMS,JME,KMS,KME, &
                                    ITS,ITE,JTS,JTE,KTS,KTE  )
!

         USE MODULE_MODEL_CONSTANTS
         IMPLICIT NONE
         INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
         INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
         INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
         REAL,       INTENT(IN   )                            :: PDTOP,PTOP
         REAL, DIMENSION(KMS:KME),                 INTENT(IN) :: ETA1,ETA2,DETA1,DETA2
         REAL, DIMENSION(IMS:IME,JMS:JME),         INTENT(IN) :: FIS,PD,QSH
         REAL, DIMENSION(IMS:IME,JMS:JME,KMS:KME), INTENT(IN) :: PINT,T,Q,CWM
         REAL, DIMENSION(KMS:KME)                , INTENT(OUT):: PSTD
         REAL, DIMENSION(IMS:IME,JMS:JME,KMS:KME), INTENT(OUT):: Z3d,Q3d,T3d

     END SUBROUTINE BASE_STATE_PARENT

#endif
   END INTERFACE

#if (NMM_CORE == 1 && NMM_NEST == 1)

!  De-reference dimension information stored in the grid data structure.

    IDS = parent%sd31
    IDE = parent%ed31
    JDS = parent%sd32
    JDE = parent%ed32
    KDS = parent%sd33
    KDE = parent%ed33

    IMS = parent%sm31
    IME = parent%em31
    JMS = parent%sm32
    JME = parent%em32
    KMS = parent%sm33
    KME = parent%em33

    ITS = parent%sp31
    ITE = parent%ep31
    JTS = parent%sp32
    JTE = parent%ep32
    KTS = parent%sp33
    KTE = parent%ep33


    ! CALL BASE_STATE_PARENT ( parent%Z3d,parent%Q3d,parent%T3d,parent%PSTD, &
    !                          parent%PINT,parent%T,parent%Q,parent%CWM,     &
    !                          parent%FIS,parent%QSH,parent%PD,parent%pdtop,parent%pt,  &
    !                          parent%ETA1,parent%ETA2,                              &
    !                          parent%DETA1,parent%DETA2,                            &
    !                          IDS,IDE,JDS,JDE,KDS,KDE,                                      &
    !                          IMS,IME,JMS,JME,KMS,KME,                                      &
    !                          ITS,ITE,JTS,JTE,KTS,KTE                                       )

#endif

   IF ( .NOT. WRFU_ClockIsStopTime(nest%domain_clock ,rc=rc) ) THEN
! initialize nest with interpolated data from the parent
     IF ( nest%active_this_task ) THEN
       nest%imask_nostag = 1
       nest%imask_xstag = 1
       nest%imask_ystag = 1
       nest%imask_xystag = 1
     ENDIF
     CALL med_force_domain( parent, nest )
   ENDIF

! might also have calls here to do input from a file into the nest

   RETURN
END SUBROUTINE med_nest_force

SUBROUTINE med_nest_feedback ( parent , nest , config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain , get_ijk_from_grid
   USE module_timing
   USE module_configure , ONLY : grid_config_rec_type
  ! Model layer
  ! External
   USE module_utility
   IMPLICIT NONE


  ! Arguments
   TYPE(domain) , POINTER                     :: parent, nest
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                                    :: idum1 , idum2 , fid, rc
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe
   INTEGER i,j

   INTERFACE
     SUBROUTINE med_feedback_domain ( parent , nest )
        USE module_domain       , ONLY : domain
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_feedback_domain
   END INTERFACE

! feedback nest to the parent
    IF ( config_flags%feedback .NE. 0 ) THEN
      CALL med_feedback_domain( parent, nest )
#ifdef MOVE_NESTS
      IF ( parent%active_this_task) THEN
      CALL get_ijk_from_grid (  parent ,                         &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )
! gopal's change- added ifdef
#if ( EM_CORE == 1 )
      DO j = jps, MIN(jpe,jde-1)
      DO i = ips, MIN(ipe,ide-1)
        IF      ( parent%nest_pos(i,j) .EQ. 9021000. ) THEN
          parent%nest_pos(i,j) = parent%ht(i,j)*1.5 + 1000.
        ELSE IF ( parent%ht(i,j) .NE. 0. ) THEN
          parent%nest_pos(i,j) = parent%ht(i,j) + 500.
        ELSE 
          parent%nest_pos(i,j) = 0.
        ENDIF
      ENDDO
      ENDDO
#endif
      ENDIF
#endif
    END IF

   RETURN
END SUBROUTINE med_nest_feedback

SUBROUTINE med_last_solve_io ( grid , config_flags )
  ! Driver layer
   USE module_state_description
   USE module_domain    , ONLY : domain, domain_clock_get
   USE module_configure , ONLY : grid_config_rec_type
   USE module_utility
   USE module_streams
#if ( WRFPLUS == 1 )
   USE mediation_pertmod_io , ONLY : save_xtraj, save_tl_pert
#endif
  ! Model layer

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                                    :: rc
   CHARACTER*256                              :: message
#if ( HWRF == 1 )
!zhang's doing
   TYPE(WRFU_Time) :: CurrTime  !zhang new
   INTEGER :: hr, min, sec, ms,julyr,julday
   REAL :: GMT
!end of zhang's doing
#endif

! #if (EM_CORE == 1)
   IF( WRFU_AlarmIsRinging( grid%alarms( HISTORY_ALARM ), rc=rc ) .AND. &
       (grid%dfi_write_dfi_history .OR. grid%dfi_stage == DFI_FST .OR. grid%dfi_opt == DFI_NODFI) ) THEN
! #else
!    IF( WRFU_AlarmIsRinging( grid%alarms( HISTORY_ALARM ), rc=rc )) THEN
! #endif
     CALL med_hist_out ( grid , HISTORY_ALARM , config_flags )
   ENDIF

   IF( WRFU_AlarmIsRinging( grid%alarms( INPUTOUT_ALARM ), rc=rc ) ) THEN
     CALL med_filter_out  ( grid , config_flags )
   ENDIF

! registry-generated file of the following 
!   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST1_ALARM ), rc=rc ) ) THEN
!     CALL med_hist_out ( grid , AUXHIST1_ALARM , config_flags )
!   ENDIF
   IF ( grid%dfi_stage == DFI_FST .OR. grid%dfi_opt == DFI_NODFI ) THEN
#include "med_last_solve_io.inc"
   END IF

! - RESTART OUTPUT
   IF( WRFU_AlarmIsRinging( grid%alarms( RESTART_ALARM ), rc=rc ) ) THEN
#if ( HWRF == 1 )
!zhang's doing
!zhang new     CALL ESMF_TimeGet( grid%current_time, YY=julyr, dayOfYear=julday, H=hr, M=min, S=sec, MS=ms, rc=rc)
     CALL domain_clock_get( grid, current_time=CurrTime )
     CALL WRFU_TimeGet( CurrTime, YY=julyr, dayOfYear=julday, H=hr, M=min, S=sec, MS=ms, rc=rc)
     gmt=hr+real(min)/60.+real(sec)/3600.+real(ms)/(1000*3600)
      if (grid%id .eq. 2) call med_namelist_out ( grid , config_flags )
!end of zhang's doing
#endif
     IF ( grid%id .EQ. 1 ) THEN
       CALL med_restart_out ( grid , config_flags )
     ENDIF
   ENDIF

   ! Write out time series
   CALL write_ts( grid )

   RETURN
END SUBROUTINE med_last_solve_io

#endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#if ( HWRF == 1 )
!==================================================================================
!  Added for the NMM 3d var. This is simply an extension of med_restart_out.
!  The file is simply called wrfanal***. This is gopal's doing
!===================================================================================
!
SUBROUTINE med_analysis_in ( grid , config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain, domain_clock_get
   USE module_io_domain
   USE module_timing
  ! Model layer
   USE module_configure , ONLY : grid_config_rec_type
   USE module_bc_time_utilities
!zhang   USE WRF_ESMF_MOD

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*256                          :: rstname , outname
   INTEGER                                :: fid , rid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc
!zhang old    TYPE(ESMF_Time)                        :: CurrTime
   TYPE(WRFU_Time)                        :: CurrTime
   CHARACTER*80                           :: timestr

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

   rid=grid%id

!zhang's doing   CALL ESMF_ClockGet( grid%domain_clock, CurrTime=CurrTime, rc=ierr )
!zhang's doing   CALL wrf_timetoa ( CurrTime, timestr )
   CALL domain_clock_get( grid, current_timestr=timestr )
   CALL construct_filename2a ( rstname ,config_flags%anl_outname, grid%id , 2 , timestr )

   WRITE( message , '("med_analysis_in: opening ",A," for reading")' ) TRIM ( rstname )
   CALL wrf_debug( 1 , message )
   CALL open_r_dataset ( rid, TRIM(rstname), grid , &
                         config_flags , "DATASET=RESTART", ierr )

   IF ( ierr .NE. 0 ) THEN
      ! Could not open the analysis file, so notify user and abort.

      write(message,'(A,I0,A,A,A)') 'ERROR: Domain ',grid%id,' analysis file ',trim(rstname),' is missing.'
      call wrf_error_fatal(message)

      ! It is unsafe to continue with a cold start when the analysis
      ! file is missing because the model expects that it is being
      ! restart if an analysis=T.  Hence, some variables will not be
      ! correctly initialized.

      ! Thus, we never reach this line:
      write(message,'(A,I0,A)') '-------> Domain ',grid%id,' running as a cold start (interp from parent).'
      call wrf_message(message)

      if(wrf_dm_on_monitor()) then
         WRITE ( message , FMT = '("Failing to read restart for domain ",I8)' ) grid%id
         CALL end_timing ( TRIM(message) )
      endif

      return
   ELSE
      ! Was able to open the analysis file.  Read it as a restart file.

      CALL input_restart ( rid, grid , config_flags , ierr )
      IF ( wrf_dm_on_monitor() ) THEN
         WRITE ( message , FMT = '("Reading restart for domain ",I8)' ) grid%id
         CALL end_timing ( TRIM(message) )
      END IF
      CALL close_dataset ( rid , config_flags , "DATASET=RESTART" )
   ENDIF

   RETURN

END SUBROUTINE med_analysis_in
!=========================================================================================================
!=========================================================================================================
SUBROUTINE med_analysis_out ( grid , config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain, domain_clock_get
   USE module_io_domain
   USE module_timing
  ! Model layer
   USE module_configure , ONLY : grid_config_rec_type
   USE module_bc_time_utilities
!zhang   USE WRF_ESMF_MOD

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*256                          :: rstname , outname
   INTEGER                                :: fid , rid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc
!zhang   TYPE(ESMF_Time)                        :: CurrTime
   TYPE(WRFU_Time)                        :: CurrTime
   CHARACTER*80                           :: timestr

   if(.not. config_flags%write_analysis) then
      write(message,'("Writing of an analysis file is disabled for domain ",I0," because write_analysis=F")') grid%id
      call wrf_debug(1,message)
      return
   endif

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

   rid=grid%id

!zhang's doing   CALL ESMF_ClockGet( grid%domain_clock, CurrTime=CurrTime, rc=ierr )
!zhang's doing   CALL wrf_timetoa ( CurrTime, timestr )
   CALL domain_clock_get( grid, current_timestr=timestr )
   CALL construct_filename2a ( rstname ,config_flags%anl_outname, grid%id , 2 , timestr )

   WRITE( message , '("med_analysis_out: opening ",A," for writing")' ) TRIM ( rstname )
   CALL wrf_debug( 1 , message )
   CALL open_w_dataset ( rid, TRIM(rstname), grid , &
                         config_flags , output_restart , "DATASET=RESTART", ierr )

   IF ( ierr .NE. 0 ) THEN
     CALL WRF_message( message )
   ENDIF
   CALL output_restart ( rid, grid , config_flags , ierr )
   IF ( wrf_dm_on_monitor() ) THEN
     WRITE ( message , FMT = '("Writing restart for domain ",I8)' ) grid%id
     CALL end_timing ( TRIM(message) )
   END IF
   CALL close_dataset ( rid , config_flags , "DATASET=RESTART" )
   RETURN
END SUBROUTINE med_analysis_out

#endif 

RECURSIVE SUBROUTINE med_restart_out ( grid , config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain , domain_clock_get
   USE module_io_domain
   USE module_timing
   USE module_configure , ONLY : grid_config_rec_type
  ! Model layer
!   USE module_bc_time_utilities
   USE module_utility

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*256                          :: rstname , outname
   INTEGER                                :: fid , rid, kid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc
   CHARACTER*80                           :: timestr
   TYPE (grid_config_rec_type)            :: kid_config_flags

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

! take this out - no effect - LPC
!   rid=grid%id !zhang's doing

   ! write out this domains restart file first

   CALL domain_clock_get( grid, current_timestr=timestr )
   CALL construct_filename2a ( rstname , config_flags%rst_outname , grid%id , 2 , timestr )

   WRITE( message , '("med_restart_out: opening ",A," for writing")' ) TRIM ( rstname )
   CALL wrf_debug( 1 , message )
   CALL open_w_dataset ( rid, TRIM(rstname), grid , &
                         config_flags , output_restart , "DATASET=RESTART", ierr )

   IF ( ierr .NE. 0 ) THEN
     CALL WRF_message( message )
   ENDIF
   CALL output_restart ( rid, grid , config_flags , ierr )
   IF ( wrf_dm_on_monitor() ) THEN
     WRITE ( message , FMT = '("Writing restart for domain ",I8)' ) grid%id
     CALL end_timing ( TRIM(message) )
   END IF
   CALL close_dataset ( rid , config_flags , "DATASET=RESTART" )

   ! call recursively for children, (if any)
   DO kid = 1, max_nests
      IF ( ASSOCIATED( grid%nests(kid)%ptr ) ) THEN
        CALL model_to_grid_config_rec ( grid%nests(kid)%ptr%id , model_config_rec , kid_config_flags )
        CALL med_restart_out ( grid%nests(kid)%ptr , kid_config_flags ) 
      ENDIF
   ENDDO

   RETURN
END SUBROUTINE med_restart_out

#if ( NMM_CORE == 1 && NMM_NEST == 1)
#ifdef EXTRA_HWRF_DEBUG_STUFF
SUBROUTINE med_boundary_out ( grid , config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain , domain_clock_get
   USE module_io_domain
   USE module_timing
   USE module_configure , ONLY : grid_config_rec_type
  ! Model layer
!   USE module_bc_time_utilities
   USE module_utility
   use module_bdywrite

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*256                          :: rstname , outname
   INTEGER                                :: fid , rid, kid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc
   CHARACTER*80                           :: timestr
   TYPE (grid_config_rec_type)            :: kid_config_flags

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

! take this out - no effect - LPC
!   rid=grid%id !zhang's doing

   ! write out this domains boundary file first

   CALL domain_clock_get( grid, current_timestr=timestr )
   CALL construct_filename2a ( rstname , 'sambdyout_d<domain>_<date>' , grid%id , 2 , timestr )

   call bdywrite(grid,rstname)

   IF ( wrf_dm_on_monitor() ) THEN
     CALL end_timing('Sam''s Special Boundary Output (TM)')
   END IF
   RETURN
END SUBROUTINE med_boundary_out
#endif
#endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#if ( HWRF == 1 )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!zhang's doing
SUBROUTINE med_restart_in ( grid , config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain, domain_clock_get
   USE module_io_domain
   USE module_timing
  ! Model layer
   USE module_configure , ONLY : grid_config_rec_type
   USE module_bc_time_utilities

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*256                          :: rstname , outname
   INTEGER                                :: fid , rid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc
!zhang old    TYPE(ESMF_Time)                        :: CurrTime
   TYPE(WRFU_Time)                        :: CurrTime
   CHARACTER*80                           :: timestr

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

   rid=grid%id

!zhang's doing   CALL ESMF_ClockGet( grid%domain_clock, CurrTime=CurrTime, rc=ierr )
!zhang's doing   CALL wrf_timetoa ( CurrTime, timestr )
   CALL domain_clock_get( grid, current_timestr=timestr )
   CALL construct_filename2a ( rstname ,config_flags%rst_outname, grid%id , 2 , timestr )

   WRITE( message , '("med_restart_in: opening ",A," for reading")' ) TRIM ( rstname )
   CALL wrf_debug( 1 , message )
   CALL open_r_dataset ( rid, TRIM(rstname), grid , &
                         config_flags , "DATASET=RESTART", ierr )

   IF ( ierr .NE. 0 ) THEN
!    CALL WRF_message( message )
     CALL WRF_ERROR_FATAL('NESTED DOMAIN ERROR: FOR ANALYSIS SET TO TRUE, YOU NEED wrfanal FILE')
   ENDIF
   CALL input_restart ( rid, grid , config_flags , ierr )
   IF ( wrf_dm_on_monitor() ) THEN
     WRITE ( message , FMT = '("Reading restart for domain ",I8)' ) grid%id
     CALL end_timing ( TRIM(message) )
   END IF
   CALL close_dataset ( rid , config_flags , "DATASET=RESTART" )
   RETURN

END SUBROUTINE med_restart_in
!end of zhang's doing
#endif

SUBROUTINE med_hist_out ( grid , stream, config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain
   USE module_timing
   USE module_io_domain
   USE module_configure , ONLY : grid_config_rec_type
   USE module_dm, ONLY : intercomm_active
!   USE module_bc_time_utilities
   USE module_utility

   IMPLICIT NONE
  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   INTEGER , INTENT(IN)                       :: stream
  ! Local
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*256                          :: fname, n2
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr

   IF ( .NOT. grid%active_this_task ) RETURN

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

   IF ( stream .LT. first_history .OR. stream .GT. last_auxhist ) THEN
     WRITE(message,*)'med_hist_out: invalid history stream ',stream
     CALL wrf_error_fatal( message )
   ENDIF

#if ( HWRF == 1 )
       ! HWRF special: auxhist2 and auxhist3 are duplicates of
       ! history (0), so there is no point in outputting more than one of
       ! them at the same time.  Prefer 0 over 2, and 2 over 3:
      if ( (stream==HISTORY_ALARM .or. stream==AUXHIST2_ALARM) .and. &
           WRFU_AlarmIsRinging( grid%alarms(AUXHIST3_ALARM) ) ) then
         CALL WRFU_AlarmRingerOff(grid%alarms(AUXHIST3_ALARM))
      endif
      if ( stream==HISTORY_ALARM .and. &
           WRFU_AlarmIsRinging( grid%alarms(AUXHIST2_ALARM) ) ) then
         CALL WRFU_AlarmRingerOff(grid%alarms(AUXHIST2_ALARM))
      endif
#endif

   SELECT CASE( stream )
     CASE ( HISTORY_ALARM )
       CALL open_hist_w( grid, config_flags, stream, HISTORY_ALARM, &
                         config_flags%history_outname, grid%oid,    &
                         output_history, fname, n2, ierr )
       CALL output_history ( grid%oid, grid , config_flags , ierr )

! registry-generated selections and calls top open_hist_w for aux streams
#include "med_hist_out_opens.inc"

   END SELECT

   WRITE(message,*)'med_hist_out: opened ',TRIM(fname),' as ',TRIM(n2)
   CALL wrf_debug( 1, message )

   grid%nframes(stream) = grid%nframes(stream) + 1

   SELECT CASE( stream )
     CASE ( HISTORY_ALARM )
       IF ( grid%nframes(stream) >= config_flags%frames_per_outfile ) THEN
write(0,*)__FILE__,__LINE__,trim(n2)
write(0,*)__FILE__,__LINE__,' grid%id ',grid%id,' grid%oid ',grid%oid
         CALL close_dataset ( grid%oid , config_flags , n2 ) 
         grid%oid = 0
         grid%nframes(stream) = 0
       ENDIF
! registry-generated selections and calls top close_dataset for aux streams
#include "med_hist_out_closes.inc"

   END SELECT
   IF ( wrf_dm_on_monitor() ) THEN
     WRITE ( message , FMT = '("Writing ",A," for domain ",I8)' )TRIM(fname),grid%id
     CALL end_timing ( TRIM(message) )
   END IF

#if (NMM_CORE == 1)
   ! Reset tornado genesis fields after output:
   call nmm_request_tg_reset(grid,config_flags,stream)
#endif

   RETURN
END SUBROUTINE med_hist_out

#if (DA_CORE != 1)
SUBROUTINE med_fddaobs_in ( grid , config_flags )
   USE module_domain    , ONLY : domain
   USE module_configure , ONLY : grid_config_rec_type
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL wrf_fddaobs_in( grid, config_flags )
   RETURN
END SUBROUTINE med_fddaobs_in
#endif

SUBROUTINE med_auxinput_in ( grid , stream, config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain
   USE module_io_domain
  ! Model layer
   USE module_configure , ONLY : grid_config_rec_type
!   USE module_bc_time_utilities
   USE module_utility

   IMPLICIT NONE
  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   INTEGER , INTENT(IN)                       :: stream
  ! Local
   CHARACTER (LEN=256)                        :: message
   INTEGER :: ierr

   IF ( stream .LT. first_auxinput .OR. stream .GT. last_auxinput ) THEN
     WRITE(message,*)'med_auxinput_in: invalid input stream ',stream
     CALL wrf_error_fatal( message )
   ENDIF

   grid%nframes(stream) = grid%nframes(stream) + 1

   SELECT CASE( stream )
! registry-generated file of calls to open filename
!     CASE ( AUXINPUT1_ALARM )
!       CALL open_aux_u( grid, config_flags, stream, AUXINPUT1_ALARM,       &
!                        config_flags%auxinput1_inname, grid%auxinput1_oid, &
!                        input_auxinput1, ierr )
!       CALL input_auxinput1 ( grid%auxinput1_oid, grid , config_flags , ierr )
#include "med_auxinput_in.inc"
   END SELECT

   SELECT CASE( stream )
! registry-generated selections and calls top close_dataset for aux streams
#include "med_auxinput_in_closes.inc"
   END SELECT

   RETURN
END SUBROUTINE med_auxinput_in

SUBROUTINE med_filter_out ( grid , config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain , domain_clock_get
   USE module_io_domain
   USE module_timing
   USE module_configure , ONLY : grid_config_rec_type
  ! Model layer
   USE module_bc_time_utilities

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*256                          :: rstname , outname
   INTEGER                                :: fid , rid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc
   CHARACTER*80                           :: timestr

   IF ( config_flags%write_input ) THEN

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

     CALL domain_clock_get( grid, current_timestr=timestr )
     CALL construct_filename2a ( outname , config_flags%input_outname , grid%id , 2 , timestr )

     WRITE ( message , '("med_filter_out 1: opening ",A," for writing. ")') TRIM ( outname )
     CALL wrf_debug( 1, message )

     CALL open_w_dataset ( fid, TRIM(outname), grid ,  &
                           config_flags , output_input , "DATASET=INPUT", ierr )
     IF ( ierr .NE. 0 ) THEN
       CALL wrf_error_fatal( message )
     ENDIF

     IF ( ierr .NE. 0 ) THEN
       CALL wrf_error_fatal( message )
     ENDIF

   CALL output_input ( fid, grid , config_flags , ierr )
   CALL close_dataset ( fid , config_flags , "DATASET=INPUT" )

   IF ( wrf_dm_on_monitor() ) THEN
     WRITE ( message , FMT = '("Writing filter output for domain ",I8)' ) grid%id
     CALL end_timing ( TRIM(message) )
   END IF
   ENDIF

   RETURN
END SUBROUTINE med_filter_out

SUBROUTINE med_latbound_in ( grid , config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain , domain_clock_get, head_grid
   USE module_io_domain
   USE module_timing
   USE module_configure , ONLY : grid_config_rec_type
  ! Model layer
!   USE module_bc_time_utilities
   USE module_utility

   IMPLICIT NONE

#include "wrf_status_codes.h"

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local data
   LOGICAL, EXTERNAL                      :: wrf_dm_on_monitor
   LOGICAL                                :: lbc_opened
   INTEGER                                :: idum1 , idum2 , ierr , open_status , fid, rc
   REAL                                   :: bfrq
   CHARACTER (LEN=256)                    :: message
   CHARACTER (LEN=256)                    :: bdyname
   Type (WRFU_Time )                      :: startTime, stopTime, currentTime
   Type (WRFU_TimeInterval )              :: stepTime
integer myproc,i,j,k
#ifdef _MULTI_BDY_FILES_
   CHARACTER(LEN=80)                      :: timestr
#endif

#include "wrf_io_flags.h"

   CALL wrf_debug ( 200 , 'in med_latbound_in' )

! #if (EM_CORE == 1)
   ! Avoid trying to re-read the boundary conditions if we are doing DFI integration
   !    and do not expect to find boundary conditions for the current time
   IF ( (grid%dfi_opt .EQ. DFI_DDFI .OR. grid%dfi_opt .EQ. DFI_TDFI) .AND. grid%dfi_stage .EQ. DFI_FWD ) RETURN
! #endif

   IF ( grid%active_this_task .AND. grid%id .EQ. 1 .AND. config_flags%specified .AND. config_flags%io_form_boundary .GT. 0 ) THEN

     CALL domain_clock_get( grid, current_time=currentTime, &
                                  start_time=startTime,     &
                                  stop_time=stopTime,       &
                                  time_step=stepTime )

!jm 20110828
!jm The test below never worked because set_time_time_read_again is never called to store a 
!jm time that lbc_read_time can compare with currentTime (see module_bc_time_utilities). This means 
!jm lbc_read_time will never return anything but false -- will also generate an ESMF error that the 
!jm stored time was never initialized.  Removing that branch from the conditional.
!jm     IF ( ( lbc_read_time( currentTime ) ) .AND. &
!jm          ( currentTime + stepTime .GE. stopTime ) .AND. &
!jm          ( currentTime .NE. startTime ) ) THEN
!jm       CALL wrf_debug( 100 , 'med_latbound_in: Skipping attempt to read lateral boundary file during last time step ' )
!jm
!jm     ELSE IF ( WRFU_AlarmIsRinging( grid%alarms( BOUNDARY_ALARM ), rc=rc ) ) THEN
!jm 20110828
     IF ( WRFU_AlarmIsRinging( grid%alarms( BOUNDARY_ALARM ), rc=rc ) ) THEN
       CALL wrf_debug ( 100 , 'in med_latbound_in preparing to read' )
       CALL WRFU_AlarmRingerOff( grid%alarms( BOUNDARY_ALARM ), rc=rc )
       IF ( wrf_dm_on_monitor() ) CALL start_timing

#ifdef _MULTI_BDY_FILES_
       ! Possibility to have a <date> as part of the bdy_inname.
       CALL domain_clock_get( grid, current_timestr=timestr )
       CALL construct_filename2a ( bdyname , config_flags%bdy_inname , grid%id , 2 , timestr )
#else
! typically a <date> wouldn't be part of the bdy_inname, so just pass a dummy
       CALL construct_filename2a ( bdyname , config_flags%bdy_inname , grid%id , 2 , " " )
#endif

       CALL wrf_inquire_opened(grid%lbc_fid , TRIM(bdyname) , open_status , ierr ) 
       IF ( open_status .EQ. WRF_FILE_OPENED_FOR_READ ) THEN
         lbc_opened = .TRUE.
       ELSE
         lbc_opened = .FALSE.
       ENDIF
       CALL wrf_dm_bcast_bytes ( lbc_opened , LWORDSIZE )
       IF ( .NOT. lbc_opened ) THEN
#ifdef _MULTI_BDY_FILES_
         CALL construct_filename2a ( bdyname , config_flags%bdy_inname , grid%id , 2 , timestr )
#else
         CALL construct_filename2a ( bdyname , config_flags%bdy_inname , grid%id , 2 , " " )
#endif
          WRITE(message,*)'Opening: ',TRIM(bdyname)
          CALL wrf_debug(100,TRIM(message))
          CALL open_r_dataset ( grid%lbc_fid, TRIM(bdyname) , grid , config_flags , "DATASET=BOUNDARY", ierr )
          IF ( ierr .NE. 0 ) THEN
            WRITE( message, * ) 'med_latbound_in: error opening ',TRIM(bdyname), ' for reading. IERR = ',ierr
            CALL WRF_ERROR_FATAL( message )
          ENDIF
       ELSE
         CALL wrf_debug( 100 , bdyname // ' is already opened' )
       ENDIF
       CALL wrf_debug( 100 , 'med_latbound_in: calling input_boundary ' )
       CALL input_boundary ( grid%lbc_fid, grid , config_flags , ierr )

! #if (EM_CORE == 1)
       IF ( (config_flags%dfi_opt .NE. DFI_NODFI) .AND. (head_grid%dfi_stage .NE. DFI_FST) ) THEN
          CALL wrf_debug( 100 , 'med_latbound_in: closing boundary file ' )
          CALL close_dataset ( grid%lbc_fid , config_flags , "DATASET=BOUNDARY" )
       END IF
! #endif

       CALL domain_clock_get( grid, current_time=currentTime )
#if ( WRFPLUS == 1 )
       IF ( config_flags%dyn_opt .NE. dyn_em_ad ) THEN
          DO WHILE (currentTime .GE. grid%next_bdy_time )         ! next_bdy_time is set by input_boundary from bdy file
             CALL wrf_debug( 100 , 'med_latbound_in: calling input_boundary ' )
             CALL input_boundary ( grid%lbc_fid, grid , config_flags , ierr )
          ENDDO
       ELSE
          DO WHILE (currentTime .GT. grid%next_bdy_time )         ! next_bdy_time is set by input_boundary from bdy file
             CALL wrf_debug( 100 , 'med_latbound_in: calling input_boundary ' )
             CALL input_boundary ( grid%lbc_fid, grid , config_flags , ierr )
          ENDDO
       ENDIF
#else
       DO WHILE (currentTime .GE. grid%next_bdy_time )         ! next_bdy_time is set by input_boundary from bdy file
         CALL wrf_debug( 100 , 'med_latbound_in: calling input_boundary ' )
         CALL input_boundary ( grid%lbc_fid, grid , config_flags , ierr )
       ENDDO
#endif
#ifdef _MULTI_BDY_FILES_
       ! Close the bdy file so that next time around, we'll open it again.
       CALL close_dataset ( grid%lbc_fid , config_flags , "DATASET=BOUNDARY" )
#endif
#if ( WRFPLUS == 1 )
       IF ( config_flags%dyn_opt .NE. dyn_em_ad ) THEN
          CALL WRFU_AlarmSet( grid%alarms( BOUNDARY_ALARM ), RingTime=grid%next_bdy_time, rc=rc )
       ELSE
          CALL WRFU_AlarmSet( grid%alarms( BOUNDARY_ALARM ), RingTime=grid%this_bdy_time, rc=rc )
          CALL wrf_inquire_opened(grid%lbc_fid , TRIM(bdyname) , open_status , ierr ) 
          IF ( open_status .EQ. WRF_FILE_OPENED_FOR_READ ) THEN
             CALL wrf_debug( 100 , 'med_latbound_in: closing boundary file ' )
             CALL close_dataset ( grid%lbc_fid , config_flags , "DATASET=BOUNDARY" )
          ENDIF
       ENDIF
#else
       CALL WRFU_AlarmSet( grid%alarms( BOUNDARY_ALARM ), RingTime=grid%next_bdy_time, rc=rc )
#endif

       IF ( ierr .NE. 0 .and. ierr .NE. WRF_WARN_NETCDF ) THEN
         WRITE( message, * ) 'med_latbound_in: error reading ',TRIM(bdyname), ' IERR = ',ierr
         CALL WRF_ERROR_FATAL( message )
       ENDIF
       IF ( currentTime .EQ. grid%this_bdy_time ) grid%dtbc = 0.
  
       IF ( wrf_dm_on_monitor() ) THEN
         WRITE ( message , FMT = '("processing lateral boundary for domain ",I8)' ) grid%id
         CALL end_timing ( TRIM(message) )
       ENDIF
     ENDIF
   ENDIF
   RETURN
END SUBROUTINE med_latbound_in

SUBROUTINE med_setup_step ( grid , config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain
   USE module_configure , ONLY : grid_config_rec_type
  ! Model layer

   IMPLICIT NONE
!<DESCRIPTION>
!
!The driver layer routine integrate() calls this mediation layer routine
!prior to initiating a time step on the domain specified by the argument
!grid.  This provides the model-layer contributor an opportunity to make
!any pre-time-step initializations that pertain to a particular model
!domain.  In WRF, this routine is used to call
!set_scalar_indices_from_config for the specified domain.
!
!</DESCRIPTION>

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                                    :: idum1 , idum2

   CALL set_scalar_indices_from_config ( grid%id , idum1 , idum2 )

   RETURN

END SUBROUTINE med_setup_step

SUBROUTINE med_endup_step ( grid , config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain
   USE module_configure , ONLY : grid_config_rec_type, model_config_rec
  ! Model layer

   IMPLICIT NONE
!<DESCRIPTION>
!
!The driver layer routine integrate() calls this mediation layer routine
!prior to initiating a time step on the domain specified by the argument
!grid.  This provides the model-layer contributor an opportunity to make
!any pre-time-step initializations that pertain to a particular model
!domain.  In WRF, this routine is used to call
!set_scalar_indices_from_config for the specified domain.
!
!</DESCRIPTION>

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(OUT)   :: config_flags
  ! Local
   INTEGER                                    :: idum1 , idum2

   IF ( grid%id .EQ. 1 ) THEN
     ! turn off the restart flag after the first mother-domain step is finished
     model_config_rec%restart = .FALSE.
     config_flags%restart = .FALSE.
     CALL nl_set_restart(1, .FALSE.)

   ENDIF

   RETURN

END SUBROUTINE med_endup_step

SUBROUTINE open_aux_u ( grid , config_flags, stream, alarm_id, &
                        auxinput_inname, oid, insub, ierr )
  ! Driver layer
   USE module_domain    , ONLY : domain , domain_clock_get
   USE module_io_domain
  ! Model layer
   USE module_configure , ONLY : grid_config_rec_type
!   USE module_bc_time_utilities
   USE module_utility

   IMPLICIT NONE
  ! Arguments
   TYPE(domain)                                :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)    :: config_flags
   INTEGER ,                     INTENT(IN)    :: stream
   INTEGER ,                     INTENT(IN)    :: alarm_id
   CHARACTER*(*) ,               INTENT(IN)    :: auxinput_inname
   INTEGER ,                     INTENT(INOUT) :: oid
   EXTERNAL                                       insub
   INTEGER ,                     INTENT(OUT)   :: ierr
  ! Local
   INTEGER                                :: stream_l
   CHARACTER*256                          :: fname, n2
   CHARACTER (LEN=256)                    :: message
   CHARACTER*80                           :: timestr
   TYPE(WRFU_Time)                        :: ST,CT
   LOGICAL                                :: adjust

   IF ( stream .LT. first_stream .OR. stream .GT. last_stream ) THEN
     WRITE(message,*)'open_aux_u: invalid input stream ',stream
     CALL wrf_error_fatal( message )
   ENDIF

   ierr = 0

   IF ( oid .eq. 0 ) THEN
     CALL domain_clock_get( grid, current_time=CT, start_time=ST, &
                            current_timestr=timestr )
     CALL nl_get_adjust_input_times( grid%id, adjust )
     IF ( adjust ) THEN 
       CALL adjust_io_timestr( grid%io_intervals( alarm_id ), CT, ST, timestr )
     ENDIF
     CALL construct_filename2a ( fname , auxinput_inname, &
                                 grid%id , 2 , timestr )
     stream_l = stream-auxinput1_only+1
     IF ( stream_l .GE. 10 ) THEN
       WRITE(n2,'("DATASET=AUXINPUT",I2)')stream_l
     ELSE
       WRITE(n2,'("DATASET=AUXINPUT",I1)')stream_l
     ENDIF
     WRITE ( message , '("open_aux_u : opening ",A," for reading. DATASET ",A)') TRIM ( fname ),TRIM(n2)
     CALL wrf_debug( 1, message )
!<DESCRIPTION>
!
!Open_u_dataset is called rather than open_r_dataset to allow interfaces
!that can do blending or masking to update an existing field. (MCEL IO does this).
!No effect for other interfaces; open_u_dataset is equivalent to open_r_dataset 
!in those cases.
!
!</DESCRIPTION>
     CALL open_u_dataset ( oid, TRIM(fname), grid ,  &
                           config_flags , insub , n2, ierr )
   ENDIF
   IF ( ierr .NE. 0 ) THEN
     WRITE ( message , '("open_aux_u : error opening ",A," for reading. ",I3)') &
       TRIM ( fname ), ierr
     CALL wrf_message( message )
   ENDIF
   RETURN
END SUBROUTINE open_aux_u

SUBROUTINE open_hist_w ( grid , config_flags, stream, alarm_id, &
                         hist_outname, oid, outsub, fname, n2, ierr )
  ! Driver layer
   USE module_domain    , ONLY : domain , domain_clock_get
   USE module_io_domain
  ! Model layer
   USE module_configure , ONLY : grid_config_rec_type
!   USE module_bc_time_utilities
   USE module_utility
   IMPLICIT NONE
  ! Arguments
   TYPE(domain)                                :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)    :: config_flags
   INTEGER ,                     INTENT(IN)    :: stream
   INTEGER ,                     INTENT(IN)    :: alarm_id
   CHARACTER*(*) ,               INTENT(IN)    :: hist_outname
   INTEGER ,                     INTENT(INOUT) :: oid
   EXTERNAL                                       outsub
   CHARACTER*(*) ,               INTENT(OUT)   :: fname, n2
   INTEGER ,                     INTENT(OUT)   :: ierr
  ! Local
   INTEGER                                :: len_n2
   INTEGER                                :: stream_l
   CHARACTER (LEN=256)                    :: message
   CHARACTER*80                           :: timestr
   TYPE(WRFU_Time)                        :: ST,CT
   LOGICAL                                :: adjust

   IF ( stream .LT. first_history .OR. stream .GT. last_history ) THEN
     WRITE(message,*)'open_hist_w: invalid history stream ',stream
     CALL wrf_error_fatal( message )
   ENDIF

   ierr = 0

   ! Note that computation of fname and n2 are outside of the oid IF statement 
   ! since they are OUT args and may be used by callers even if oid/=0.  
   CALL domain_clock_get( grid, current_time=CT, start_time=ST, &
                          current_timestr=timestr )
   CALL nl_get_adjust_output_times( grid%id, adjust )
   IF ( adjust ) THEN 
     CALL adjust_io_timestr( grid%io_intervals( alarm_id ), CT, ST, timestr )
   ENDIF
#if (DA_CORE != 1 && NMM_CORE != 1)
!----------------------------------------------------------------------
! RASM Climate Diagnostics - JR, AS, MS  - October 2016
!----------------------------------------------------------------------
   IF( alarm_id .EQ. AUXHIST5_ALARM .AND. config_flags%mean_diag .EQ. 1 ) THEN
      WRITE(message, *) "RASM STATS: MEAN AUXHIST5 oid=", oid, " fname=", trim(fname), " alarmI_id=", alarm_id, " Time_outNow=", timestr
      CALL wrf_debug(200,  message )
      WRITE(message, *) "RASM STATS: MEAN AUXHIST5 Time_outbefore =...", trim(grid%OUTDATE_MEAN)
      CALL wrf_debug(200,  message )
      timestr = grid%OUTDATE_MEAN  
   ELSE IF( alarm_id .EQ. AUXHIST6_ALARM .AND. config_flags%diurnal_diag .EQ. 1 ) THEN
      WRITE(message, *) "RASM STATS: DIURNAL AUXHIST6 oid=", oid, " fname=", trim(fname), " alarmI_id=", alarm_id, " Time_outNow=", timestr
      CALL wrf_debug(200,  message )
      WRITE(message, *) "RASM STATS: DIURNAL AUXHIST6 Time_outbefore =...", trim(grid%OUTDATE_DIURN)
      CALL wrf_debug(200,  message )
      timestr = grid%OUTDATE_DIURN 
   ENDIF
!----------------------------------------------------------------------
! end RASM Climate Diagnostics
!----------------------------------------------------------------------
#endif
   CALL construct_filename2a ( fname , hist_outname, &
                               grid%id , 2 , timestr )
   stream_l = stream-auxhist1_only+1
   IF ( stream .EQ. history_only ) THEN
     WRITE(n2,'("DATASET=HISTORY")')
   ELSE IF ( stream_l .GE. 10 ) THEN
     WRITE(n2,'("DATASET=AUXHIST",I2)')stream_l
   ELSE
     WRITE(n2,'("DATASET=AUXHIST",I1)')stream_l
   ENDIF
   IF ( oid .eq. 0 ) THEN
     WRITE ( message , '("open_hist_w : opening ",A," for writing. ")') TRIM ( fname )
     CALL wrf_debug( 1, message )
!<DESCRIPTION>
!
!Open_u_dataset is called rather than open_r_dataset to allow interfaces
!that can do blending or masking to update an existing field. (MCEL IO does this).
!No effect for other interfaces; open_u_dataset is equivalent to open_r_dataset 
!in those cases.
!
!</DESCRIPTION>
     CALL open_w_dataset ( oid, TRIM(fname), grid ,  &
                           config_flags , outsub , n2, ierr )
   ENDIF
   IF ( ierr .NE. 0 ) THEN
     WRITE ( message , '("open_hist_w : error opening ",A," for writing. ",I3)') &
       TRIM ( fname ), ierr
     CALL wrf_message( message )
   ENDIF

   RETURN
END SUBROUTINE open_hist_w

 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#if ( WRF_CHEM == 1 )

SUBROUTINE med_read_wrf_chem_input ( grid , config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain , domain_clock_get
   USE module_io_domain
   USE module_timing
   USE module_configure , ONLY : grid_config_rec_type
  ! Model layer
   USE module_bc_time_utilities
#ifdef DM_PARALLEL
   USE module_dm
#endif
   USE module_date_time
   USE module_utility

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid

   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local data
   LOGICAL, EXTERNAL                      :: wrf_dm_on_monitor

   INTEGER                                :: ierr, efid
   REAL                                   :: time, tupdate
   real, allocatable :: dumc0(:,:,:)
   CHARACTER (LEN=256)                    :: message, current_date_char, date_string
   CHARACTER (LEN=256)                    :: inpname

#include "wrf_io_flags.h"
!   IF ( grid%id .EQ. 1 ) THEN

      CALL domain_clock_get( grid, current_timestr=current_date_char )

      CALL construct_filename1 ( inpname , config_flags%auxinput12_inname , grid%id , 2 )
      WRITE(message,*)'mediation_integrate: med_read_wrf_chem_input: Open file ',TRIM(inpname)
      CALL wrf_message( TRIM(message) )

     if( grid%auxinput12_oid .NE. 0 ) then
       CALL close_dataset ( grid%auxinput12_oid , config_flags , "DATASET=AUXINPUT12" )
     endif

      CALL open_r_dataset ( grid%auxinput12_oid, TRIM(inpname) , grid , config_flags, &
                              "DATASET=AUXINPUT12", ierr )
        IF ( ierr .NE. 0 ) THEN
           WRITE( message , * ) 'med_read_wrf_chem_input error opening ', TRIM( inpname )
           CALL wrf_error_fatal( TRIM( message ) )
        ENDIF

         WRITE(message,*)'mediation_integrate: med_read_wrf_chem_input: Read chemistry from wrfout at time ',&
         TRIM(current_date_char)
         CALL wrf_message( TRIM(message) )

         CALL wrf_debug (100 , 'mediation_integrate: calling input_auxinput12' )
         CALL input_auxinput12 ( grid%auxinput12_oid, grid , config_flags , ierr )

         CALL close_dataset ( grid%auxinput12_oid , config_flags , "DATASET=AUXINPUT12" )

!  ENDIF
   CALL wrf_debug (100 , 'mediation_integrate: med_read_wrf_chem_input: exit' )

END SUBROUTINE med_read_wrf_chem_input
!------------------------------------------------------------------------
! Chemistry emissions input control. Three options are available and are
! set via the namelist variable io_style_emissions:
!
!   0 = Emissions are not read in from a file. They will contain their
!       default values, which can be set in the Registry.
!       (Intended for debugging of chem code)
!
!   1 = Emissions are read in from two 12 hour files that are cycled.
!       With this choice, auxinput5_inname should be set to
!       the value "wrfchemi_hhZ_d<domain>". 
!
!   2 = Emissions are read in from files identified by date and that have
!       a length defined by frames_per_auxinput5. Both
!       auxinput5_inname should be set to 
!       "wrfchemi_d<domain>_<date>".
!------------------------------------------------------------------------
SUBROUTINE med_read_wrf_chem_emiss ( grid , config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain , domain_clock_get
   USE module_io_domain
   USE module_timing
   USE module_configure , ONLY : grid_config_rec_type
  ! Model layer
   USE module_bc_time_utilities
#ifdef DM_PARALLEL
   USE module_dm
#endif
   USE module_date_time
   USE module_utility

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid

!  TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   TYPE (grid_config_rec_type)            :: config_flags
   Type (WRFU_Time )                      :: stopTime, currentTime
   Type (WRFU_TimeInterval )              :: stepTime

  ! Local data
   LOGICAL, EXTERNAL                      :: wrf_dm_on_monitor

   INTEGER                                :: ierr, efid
   INTEGER                                :: ihr, ihrdiff, i
   REAL                                   :: time, tupdate
   real, allocatable :: dumc0(:,:,:)
   CHARACTER (LEN=256)                    :: message, current_date_char, date_string
   CHARACTER (LEN=256)                    :: inpname

#include "wrf_io_flags.h"

     CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

! This "if" should be commented out when using emission files for nested
! domains. Also comment out the "ENDIF" line noted below.
!    IF ( grid%id .EQ. 1 ) THEN  

      CALL domain_clock_get( grid, current_time=currentTime,          &
                                   current_timestr=current_date_char, &
                                   stop_time=stopTime,                &
                                   time_step=stepTime )

      time = float(grid%itimestep) * grid%dt

!---
! io_style_emissions option 0: no emissions read in...
!---
      if( config_flags%io_style_emissions == 0 ) then
         ! Do nothing.
!---
! io_style_emissions option 1: cycle through two 12 hour input files...
!---
      else if( config_flags%io_style_emissions == 1 ) then

         tupdate = mod( time, (12. * 3600.) )
         read(current_date_char(12:13),'(I2)') ihr
         ihr = MOD(ihr,24)
         ihrdiff = 0  

         IF( tupdate .LT. grid%dt ) THEN
            tupdate = 0.
         ENDIF
         IF( ihr .EQ. 00 .OR. ihr .EQ. 12 ) THEN 
            tupdate = 0. 
         ENDIF

         IF( currentTime + stepTime .GE. stopTime .AND. &
              grid%auxinput5_oid .NE. 0 ) THEN
            CALL close_dataset ( grid%auxinput5_oid , config_flags , "DATASET=AUXINPUT5" )
            tupdate = 1.
         ENDIF

!        write(message,FMT='(A,F10.1,A)') ' EMISSIONS UPDATE TIME ',time,TRIM(current_date_char(12:13))
!        CALL wrf_message( TRIM(message) )

         IF ( tupdate .EQ. 0. .AND.  ihr .LT. 12 ) THEN 
            ihrdiff = ihr  
            CALL construct_filename1 ( inpname , 'wrfchemi_00z' , grid%id , 2 )
            WRITE(message,*)'mediation_integrate: med_read_wrf_chem_emissions: Open file ',TRIM(inpname)
            CALL wrf_message( TRIM(message) )

            if( grid%auxinput5_oid .NE. 0 ) then
               CALL close_dataset ( grid%auxinput5_oid , config_flags , "DATASET=AUXINPUT5" )
            endif

            CALL open_r_dataset ( grid%auxinput5_oid, TRIM(inpname) , grid , config_flags, &
                 "DATASET=AUXINPUT5", ierr )
            IF ( ierr .NE. 0 ) THEN
               WRITE( message , * ) 'med_read_wrf_chem_emissions: error opening ', TRIM( inpname )
               CALL wrf_error_fatal( TRIM( message ) )
            ENDIF

          ELSE IF ( tupdate .EQ. 0. .AND. ihr .GE. 12 ) THEN
             ihrdiff = ihr - 12

            CALL construct_filename1 ( inpname , 'wrfchemi_12z' , grid%id , 2 )
            WRITE(message,*)'mediation_integrate: med_read_wrf_chem_emissions: Open file ',TRIM(inpname)
            CALL wrf_message( TRIM(message) )

            if( grid%auxinput5_oid .NE. 0 ) then
               CALL close_dataset ( grid%auxinput5_oid , config_flags , "DATASET=AUXINPUT5" )
            endif

            CALL open_r_dataset ( grid%auxinput5_oid, TRIM(inpname) , grid , config_flags, &
                 "DATASET=AUXINPUT5", ierr )
            IF ( ierr .NE. 0 ) THEN
               WRITE( message , * ) 'med_read_wrf_chem_emissions: error opening ', TRIM( inpname )
               CALL wrf_error_fatal( TRIM( message ) )
            ENDIF
          ENDIF

         WRITE( message, '(A,2F10.1)' ) ' HOURLY EMISSIONS UPDATE TIME ',time,mod(time,3600.)
         CALL wrf_message( TRIM(message) )
!
! hourly updates to emissions
         IF ( ( mod( time, 3600. ) .LT. grid%dt   ) .AND. &
              ( currentTime + stepTime .LT. stopTime ) ) THEN
!           IF ( wrf_dm_on_monitor() ) CALL start_timing

            WRITE(message,'(A,A)')'mediation_integrate: med_read_wrf_chem_emissions: Read emissions for time ',TRIM(current_date_char)
            CALL wrf_message( TRIM(message) )

            IF ( tupdate .EQ. 0. .AND. ihrdiff .GT. 0) THEN
               IF( ihrdiff .GT. 12) THEN
                 WRITE(message,'(A)')'mediation_integrate: med_read_wrf_chem_emissions: Error in emissions time, skipping all times in file '
                 CALL wrf_message( TRIM(message) )
               ENDIF
               DO i=1,ihrdiff
                 WRITE(message,'(A,I4)')'mediation_integrate: med_read_wrf_chem_emissions: Skip emissions ',i
                 CALL wrf_message( TRIM(message) )
                 CALL input_auxinput5 ( grid%auxinput5_oid, grid , config_flags , ierr )
               ENDDO
            ENDIF

            CALL wrf_debug (100 , 'mediation_integrate: calling input_auxinput5' )
            CALL input_auxinput5 ( grid%auxinput5_oid, grid , config_flags , ierr )
         ELSE
            CALL wrf_debug (100 , 'mediation_integrate: med_read_wrf_chem_emissions: Do not read emissions' )
         ENDIF

!---
! io_style_emissions option 2: use dated emission files whose length is
!                             set via frames_per_auxinput5...
!---
      else if( config_flags%io_style_emissions == 2 ) then
         WRITE(message,*)'mediation_integrate: med_read_wrf_chem_emissions: Read emissions for time ',TRIM(current_date_char)
         CALL wrf_message( TRIM(message) )
!
! Code to read hourly emission files...
!
         if( grid%auxinput5_oid == 0 ) then
            CALL construct_filename2a(inpname , grid%emi_inname, grid%id , 2, current_date_char)
            WRITE(message,*)'mediation_integrate: med_read_wrf_chem_emissions: Open file ',TRIM(inpname)
            CALL wrf_message( TRIM(message) )
            CALL open_r_dataset ( grid%auxinput5_oid, TRIM(inpname) , grid , config_flags, &
                 "DATASET=AUXINPUT5", ierr )
            IF ( ierr .NE. 0 ) THEN
               WRITE( message , * ) 'med_read_wrf_chem_emissions: error opening ', TRIM( inpname )
               CALL wrf_error_fatal( TRIM( message ) )
            ENDIF
         end if
!
! Read the emissions data.
!
         CALL wrf_debug (100 , 'mediation_integrate: calling input_auxinput5' )
         CALL input_auxinput5 ( grid%auxinput5_oid, grid , config_flags , ierr )
!
! If reached the indicated number of frames in the emissions file, close it.
!
         grid%emissframes = grid%emissframes + 1
         IF ( grid%emissframes >= config_flags%frames_per_auxinput5 ) THEN
            CALL close_dataset ( grid%auxinput5_oid , config_flags , "DATASET=AUXINPUT5" )
            grid%emissframes = 0
            grid%auxinput5_oid = 0
         ENDIF

!---
! unknown io_style_emissions option...
!---
      else
         call wrf_error_fatal("Unknown emission style selected via io_style_emissions.")
      end if

! The following line should be commented out when using emission files
! for nested domains. Also comment out the "if" noted above.
!   ENDIF

   CALL wrf_debug (100 , 'mediation_integrate: med_read_wrf_chem_emissions: exit' )

END SUBROUTINE med_read_wrf_chem_emiss

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE med_read_wrf_chem_bioemiss ( grid , config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain , domain_clock_get
   USE module_io_domain
   USE module_timing
   USE module_configure , ONLY : grid_config_rec_type
  ! Model layer
   USE module_bc_time_utilities
#ifdef DM_PARALLEL
   USE module_dm
#endif
   USE module_date_time
   USE module_utility

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid

   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local data
   LOGICAL, EXTERNAL                      :: wrf_dm_on_monitor

   INTEGER                                :: ierr, efid
   REAL                                   :: time, tupdate
   real, allocatable :: dumc0(:,:,:)
   CHARACTER (LEN=256)                    :: message, current_date_char, date_string
   CHARACTER (LEN=256)                    :: inpname

#include "wrf_io_flags.h"
!   IF ( grid%id .EQ. 1 ) THEN

      CALL domain_clock_get( grid, current_timestr=current_date_char )

      CALL construct_filename1 ( inpname , 'wrfbiochemi' , grid%id , 2 )
      WRITE(message,*)'mediation_integrate: med_read_wrf_chem_bioemissions: Open file ',TRIM(inpname)
      CALL wrf_message( TRIM(message) )

     if( grid%auxinput6_oid .NE. 0 ) then
       CALL close_dataset ( grid%auxinput6_oid , config_flags , "DATASET=AUXINPUT6" )
     endif

      CALL open_r_dataset ( grid%auxinput6_oid, TRIM(inpname) , grid , config_flags, &
                              "DATASET=AUXINPUT6", ierr )
        IF ( ierr .NE. 0 ) THEN
           WRITE( message , * ) 'med_read_wrf_chem_bioemissions: error opening ', TRIM( inpname )
           CALL wrf_error_fatal( TRIM( message ) )
        ENDIF

         WRITE(message,*)'mediation_integrate: med_read_wrf_chem_bioemissions: Read biogenic emissions at time ',&
         TRIM(current_date_char)
         CALL wrf_message( TRIM(message) )

         CALL wrf_debug (100 , 'mediation_integrate: calling input_auxinput6' )
         CALL input_auxinput6 ( grid%auxinput6_oid, grid , config_flags , ierr )

         CALL close_dataset ( grid%auxinput6_oid , config_flags , "DATASET=AUXINPUT6" )

!  ENDIF
   CALL wrf_debug (100 , 'mediation_integrate: med_read_wrf_chem_bioemissions: exit' )

END SUBROUTINE med_read_wrf_chem_bioemiss
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE med_read_wrf_chem_emissopt4 ( grid , config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain , domain_clock_get
   USE module_io_domain
   USE module_timing
   USE module_configure , ONLY : grid_config_rec_type
  ! Model layer
   USE module_bc_time_utilities
#ifdef DM_PARALLEL
   USE module_dm
#endif
   USE module_date_time
   USE module_utility

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid

   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local data
   LOGICAL, EXTERNAL                      :: wrf_dm_on_monitor

   INTEGER                                :: ierr, efid
   REAL                                   :: time, tupdate
   real, allocatable :: dumc0(:,:,:)
   CHARACTER (LEN=256)                    :: message, current_date_char, date_string
   CHARACTER (LEN=256)                    :: inpname

#include "wrf_io_flags.h"
!   IF ( grid%id .EQ. 1 ) THEN

      CALL domain_clock_get( grid, current_timestr=current_date_char )

      CALL construct_filename1 ( inpname , 'wrfchemi' , grid%id , 2 )
      WRITE(message,*)'mediation_integrate: med_read_wrf_chem_emissions: Open file ',TRIM(inpname)
      CALL wrf_message( TRIM(message) )

     if( grid%auxinput5_oid .NE. 0 ) then
       CALL close_dataset ( grid%auxinput5_oid , config_flags , "DATASET=AUXINPUT5" )
     endif

      CALL open_r_dataset ( grid%auxinput5_oid, TRIM(inpname) , grid , config_flags, &
                              "DATASET=AUXINPUT5", ierr )
        IF ( ierr .NE. 0 ) THEN
           WRITE( message , * ) 'med_read_wrf_chem_emissions: error opening ', TRIM( inpname )
           CALL wrf_error_fatal( TRIM( message ) )
        ENDIF

         WRITE(message,*)'mediation_integrate: med_read_wrf_chem_emissions: Read biogenic emissions at time ',&
         TRIM(current_date_char)
         CALL wrf_message( TRIM(message) )

         CALL wrf_debug (100 , 'mediation_integrate: calling input_auxinput5' )
         CALL input_auxinput5 ( grid%auxinput5_oid, grid , config_flags , ierr )

         CALL close_dataset ( grid%auxinput5_oid , config_flags , "DATASET=AUXINPUT5" )

!  ENDIF
   CALL wrf_debug (100 , 'mediation_integrate: med_read_wrf_chem_emissions: exit' )

END SUBROUTINE med_read_wrf_chem_emissopt4

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE med_read_wrf_chem_dms_emiss ( grid , config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain , domain_clock_get
   USE module_io_domain
   USE module_timing
   USE module_configure , ONLY : grid_config_rec_type
  ! Model layer
   USE module_bc_time_utilities
#ifdef DM_PARALLEL
   USE module_dm
#endif
   USE module_date_time
   USE module_utility

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid

   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local data
   LOGICAL, EXTERNAL                      :: wrf_dm_on_monitor

   INTEGER                                :: ierr, efid
   REAL                                   :: time, tupdate
   real, allocatable :: dumc0(:,:,:)
   CHARACTER (LEN=256)                    :: message, current_date_char, date_string
   CHARACTER (LEN=256)                    :: inpname

#include "wrf_io_flags.h"
!   IF ( grid%id .EQ. 1 ) THEN

      CALL domain_clock_get( grid, current_timestr=current_date_char )

      CALL construct_filename1 ( inpname , 'wrfchemi_dms' , grid%id , 2 )
      WRITE(message,*)'mediation_integrate: med_read_wrf_chem_dms_emiss: Open file ',TRIM(inpname)
      CALL wrf_message( TRIM(message) )

     if( grid%auxinput7_oid .NE. 0 ) then
       CALL close_dataset ( grid%auxinput7_oid , config_flags , "DATASET=AUXINPUT7" )
     endif

      CALL open_r_dataset ( grid%auxinput7_oid, TRIM(inpname) , grid , config_flags, &
                              "DATASET=AUXINPUT7", ierr )
        IF ( ierr .NE. 0 ) THEN
           WRITE( message , * ) 'med_read_wrf_chem_dms_emiss: error opening ', TRIM( inpname )
           CALL wrf_error_fatal( TRIM( message ) )
        ENDIF

         WRITE(message,*)'mediation_integrate: med_read_wrf_chem_dms_emiss: Read dms reference fields',&
         TRIM(current_date_char)
         CALL wrf_message( TRIM(message) )

         CALL wrf_debug (100 , 'mediation_integrate: calling input_auxinput7' )
         CALL input_auxinput7 ( grid%auxinput7_oid, grid , config_flags , ierr )

         CALL close_dataset ( grid%auxinput7_oid , config_flags , "DATASET=AUXINPUT7" )

!  ENDIF
   CALL wrf_debug (100 , 'mediation_integrate: med_read_wrf_chem_dms_emiss: exit' )
 
END SUBROUTINE  med_read_wrf_chem_dms_emiss

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE med_read_wrf_chem_gocart_bg ( grid , config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain , domain_clock_get
   USE module_io_domain
   USE module_timing
   USE module_configure , ONLY : grid_config_rec_type
  ! Model layer
   USE module_bc_time_utilities
#ifdef DM_PARALLEL
   USE module_dm
#endif
   USE module_date_time
   USE module_utility

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid

   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local data
   LOGICAL, EXTERNAL                      :: wrf_dm_on_monitor

   INTEGER                                :: ierr, efid
   REAL                                   :: time, tupdate
   real, allocatable :: dumc0(:,:,:)
   CHARACTER (LEN=256)                    :: message, current_date_char, date_string
   CHARACTER (LEN=256)                    :: inpname

#include "wrf_io_flags.h"
!   IF ( grid%id .EQ. 1 ) THEN

      CALL domain_clock_get( grid, current_timestr=current_date_char )

      CALL construct_filename1 ( inpname , 'wrfchemi_gocart_bg' , grid%id , 2 )
      WRITE(message,*)'mediation_integrate: med_read_wrf_chem_gocart_bg: Open file ',TRIM(inpname)
      CALL wrf_message( TRIM(message) )

     if( grid%auxinput8_oid .NE. 0 ) then
       CALL close_dataset ( grid%auxinput8_oid , config_flags , "DATASET=AUXINPUT8" )
     endif

      CALL open_r_dataset ( grid%auxinput8_oid, TRIM(inpname) , grid , config_flags, &
                              "DATASET=AUXINPUT8", ierr )
        IF ( ierr .NE. 0 ) THEN
           WRITE( message , * ) 'med_read_wrf_chem_gocart_bg: error opening ', TRIM( inpname )
           CALL wrf_error_fatal( TRIM( message ) )
        ENDIF

         WRITE(message,*)'mediation_integrate: med_read_wrf_chem_gocart_bg: Read gocart_bg at time ',&
         TRIM(current_date_char)
         CALL wrf_message( TRIM(message) )

         CALL wrf_debug (100 , 'mediation_integrate: calling input_auxinput8' )
         CALL input_auxinput8 ( grid%auxinput8_oid, grid , config_flags , ierr )

         CALL close_dataset ( grid%auxinput8_oid , config_flags , "DATASET=AUXINPUT8" )

!
!         CALL wrf_global_to_patch_real ( backg_no3_io , grid%backg_no3 , grid%domdesc, ' ' , 'xyz' ,         &
!                                         ids, ide-1 , jds , jde-1 , kds , kde-1, &
!                                         ims, ime   , jms , jme   , kms , kme  , &
!                                         ips, ipe   , jps , jpe   , kps , kpe    )
!
!  ENDIF
   CALL wrf_debug (100 , 'mediation_integrate: med_read_wrf_chem_gocart_bg: exit' )
 
END SUBROUTINE  med_read_wrf_chem_gocart_bg
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE med_read_wrf_volc_emiss ( grid , config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain , domain_clock_get
   USE module_io_domain
   USE module_timing
   USE module_configure , ONLY : grid_config_rec_type
  ! Model layer
   USE module_bc_time_utilities
#ifdef DM_PARALLEL
   USE module_dm
#endif
   USE module_date_time
   USE module_utility

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid

   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local data
   LOGICAL, EXTERNAL                      :: wrf_dm_on_monitor

   INTEGER                                :: ierr, efid
   REAL                                   :: time, tupdate
   real, allocatable :: dumc0(:,:,:)
   CHARACTER (LEN=256)                    :: message, current_date_char, date_string
   CHARACTER (LEN=256)                    :: inpname

#include "wrf_io_flags.h"
      CALL domain_clock_get( grid, current_timestr=current_date_char )

      CALL construct_filename1 ( inpname , 'wrfchemv' , grid%id , 2 )
      WRITE(message,*)'mediation_integrate: med_read_wrf_volc_emiss: Open file ',TRIM(inpname)
      CALL wrf_message( TRIM(message) )

     if( grid%auxinput13_oid .NE. 0 ) then
       CALL close_dataset ( grid%auxinput13_oid , config_flags , "DATASET=AUXINPUT13" )
     endif

      CALL open_r_dataset ( grid%auxinput13_oid, TRIM(inpname) , grid , config_flags, &
                              "DATASET=AUXINPUT13", ierr )
        IF ( ierr .NE. 0 ) THEN
           WRITE( message , * ) 'med_read_wrf_volc_emiss: error opening ', TRIM( inpname )
           CALL wrf_error_fatal( TRIM( message ) )
        ENDIF

         WRITE(message,*)'mediation_integrate: med_read_wrf_volc_emiss: Read volcanic ash emissions',&
         TRIM(current_date_char)
         CALL wrf_message( TRIM(message) )

         CALL wrf_debug (100 , 'mediation_integrate: calling input_auxinput13' )
         CALL input_auxinput13 ( grid%auxinput13_oid, grid , config_flags , ierr )

         CALL close_dataset ( grid%auxinput13_oid , config_flags , "DATASET=AUXINPUT13" )

   CALL wrf_debug (100 , 'mediation_integrate: med_read_wrf_volc_emiss: exit' )
 
END SUBROUTINE  med_read_wrf_volc_emiss

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE med_read_wrf_chem_emissopt3 ( grid , config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain , domain_clock_get
   USE module_io_domain
   USE module_timing
   USE module_configure , ONLY : grid_config_rec_type
  ! Model layer
   USE module_bc_time_utilities
#ifdef DM_PARALLEL
   USE module_dm
#endif
   USE module_date_time
   USE module_utility

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid

   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local data
   LOGICAL, EXTERNAL                      :: wrf_dm_on_monitor

   INTEGER                                :: ierr, efid
   REAL                                   :: time, tupdate
   real, allocatable :: dumc0(:,:,:)
   CHARACTER (LEN=256)                    :: message, current_date_char, date_string
   CHARACTER (LEN=256)                    :: inpname

#include "wrf_io_flags.h"
!   IF ( grid%id .EQ. 1 ) THEN

      CALL domain_clock_get( grid, current_timestr=current_date_char )

      CALL construct_filename1 ( inpname , 'wrffirechemi' , grid%id , 2 )
      WRITE(message,*)'mediation_integrate: med_read_wrf_chem_fireemissions: Open file ',TRIM(inpname)
      CALL wrf_message( TRIM(message) )

     if( grid%auxinput7_oid .NE. 0 ) then
       CALL close_dataset ( grid%auxinput7_oid , config_flags , "DATASET=AUXINPUT7" )
     endif

      CALL open_r_dataset ( grid%auxinput7_oid, TRIM(inpname) , grid , config_flags, &
                              "DATASET=AUXINPUT7", ierr )
        IF ( ierr .NE. 0 ) THEN
           WRITE( message , * ) 'med_read_wrf_chem_fireemissions: error opening ', TRIM( inpname )
           CALL wrf_error_fatal( TRIM( message ) )
        ENDIF

         WRITE(message,*)'mediation_integrate: med_read_wrf_chem_fireemissions: Read fire emissions at time ',&
         TRIM(current_date_char)
         CALL wrf_message( TRIM(message) )

         CALL wrf_debug (00 , 'mediation_integrate: calling input_auxinput7' )
         CALL input_auxinput7 ( grid%auxinput7_oid, grid , config_flags , ierr )

         CALL close_dataset ( grid%auxinput7_oid , config_flags , "DATASET=AUXINPUT7" )

!  ENDIF
   CALL wrf_debug (00 , 'mediation_integrate: med_read_wrf_chem_fireemissions: exit' )

END SUBROUTINE med_read_wrf_chem_emissopt3
#endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#if ( HWRF == 1 )
!zhang's doing for outputing restart namelist parameters
RECURSIVE SUBROUTINE med_namelist_out ( grid , config_flags )
  ! Driver layer
   USE module_domain    , ONLY : domain, domain_clock_get
   USE module_io_domain
   USE module_timing
  ! Model layer
   USE module_configure , ONLY : grid_config_rec_type
   USE module_bc_time_utilities
!zhang new   USE WRF_ESMF_MOD
   USE module_utility
!zhang new ends

   IMPLICIT NONE

  ! Arguments
   TYPE(domain), INTENT(IN)                   :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local
!zhang new   TYPE(ESMF_Time)                        :: CurrTime
   TYPE(WRFU_Time) :: CurrTime
   INTEGER                                :: nout,rc,kid
   INTEGER                                :: hr, min, sec, ms,julyr,julday
   REAL                                   :: GMT
   CHARACTER*256                          :: prefix, outname
   CHARACTER*80                           :: timestr
   LOGICAL                                :: exist
   LOGICAL,EXTERNAL :: wrf_dm_on_monitor

   TYPE (grid_config_rec_type)            :: kid_config_flags

   prefix = "wrfnamelist_d<domain>_<date>"
   nout = 99

!zhang new   CALL ESMF_ClockGet( grid%domain_clock, CurrTime=CurrTime, rc=rc )
!zhang new   CALL wrf_timetoa ( CurrTime, timestr )
   CALL domain_clock_get( grid, current_timestr=timestr )
!zhang new ends
   CALL construct_filename2a ( outname , prefix, grid%id , 2 , timestr )

   IF ( wrf_dm_on_monitor() ) THEN

   CLOSE (NOUT)
   OPEN ( FILE   = trim(outname) , UNIT   = nout, STATUS = 'UNKNOWN', FORM   = 'FORMATTED')
!zhang new   CALL ESMF_TimeGet( grid%current_time, YY=julyr, dayOfYear=julday, H=hr, M=min, S=sec, MS=ms, rc=rc)
   CALL domain_clock_get( grid, current_time=CurrTime )
   CALL WRFU_TimeGet( CurrTime, YY=julyr, dayOfYear=julday, H=hr, M=min, S=sec, MS=ms, rc=rc)
!zhang new ends
   gmt=hr+real(min)/60.+real(sec)/3600.+real(ms)/(1000*3600)
   WRITE(NOUT,*) grid%i_parent_start
   WRITE(NOUT,*) grid%j_parent_start
   WRITE(NOUT,*) julyr
   WRITE(NOUT,*) julday
   WRITE(NOUT,*) gmt

   CLOSE (NOUT)
   ENDIF

   ! call recursively for children, (if any)
   DO kid = 1, max_nests
      IF ( ASSOCIATED( grid%nests(kid)%ptr ) ) THEN
        CALL model_to_grid_config_rec ( grid%nests(kid)%ptr%id , model_config_rec , kid_config_flags )
        CALL med_namelist_out ( grid%nests(kid)%ptr , kid_config_flags )
      ENDIF
   ENDDO

   RETURN
END SUBROUTINE med_namelist_out
!end of zhang's doing
#endif
