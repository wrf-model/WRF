!
!WRF:MEDIATION_LAYER:IO
!

SUBROUTINE med_calc_model_time ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_configure
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
   USE module_domain
   USE module_configure
  ! Model layer
   USE module_utility

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                                    :: rc

   IF( WRFU_AlarmIsRinging( grid%alarms( HISTORY_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 0, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( HISTORY_ALARM ), rc=rc )
   ENDIF

   IF( WRFU_AlarmIsRinging( grid%alarms( INPUTOUT_ALARM ), rc=rc ) ) THEN
     CALL med_filter_out  ( grid , config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( INPUTOUT_ALARM ), rc=rc )
   ENDIF

! - AUX HISTORY OUTPUT 
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST1_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 1, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST1_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST2_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 2, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST2_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST3_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 3,  config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST3_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST4_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 4, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST4_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST5_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 5, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXHIST5_ALARM ), rc=rc )
   ENDIF

! - AUX INPUT INPUT
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT1_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput1_in ( grid , config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT1_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT2_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput2_in ( grid , config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT2_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT3_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput3_in ( grid , config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT3_ALARM ), rc=rc )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT4_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput4_in ( grid , config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT4_ALARM ), rc=rc )
   ENDIF

! this needs to be looked at again so we can get rid of the special
! handling of AUXINPUT5 but for now...

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! add for wrf_chem emiss input
! - Get chemistry data
  IF( config_flags%chem_opt > 0 ) THEN
#ifdef WRF_CHEM
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT5_ALARM ), rc=rc ) ) THEN
    call wrf_debug(15,' CALL med_read_wrf_chem_emiss ')
     CALL med_read_wrf_chem_emiss ( grid , config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT5_ALARM ), rc=rc )
    call wrf_debug(15,' Back from CALL med_read_wrf_chem_emiss ')

   ENDIF
! end for wrf chem emiss input
#endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ELSE
#ifndef WRF_CHEM
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXINPUT5_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput5_in ( grid , config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( AUXINPUT5_ALARM ), rc=rc )
   ENDIF
#endif
  ENDIF

! - RESTART OUTPUT
   IF( WRFU_AlarmIsRinging( grid%alarms( RESTART_ALARM ), rc=rc ) ) THEN
     IF ( grid%id .EQ. 1 ) THEN
       ! Only the parent initiates the restart writing. Otherwise, different
       ! domains may be written out at different times and with different 
       ! time stamps in the file names.
       CALL med_restart_out ( grid , config_flags )
     ENDIF
     CALL WRFU_AlarmRingerOff( grid%alarms( RESTART_ALARM ), rc=rc )
   ENDIF

! - Look for boundary data after writing out history and restart files
   CALL med_latbound_in ( grid , config_flags )

   RETURN
END SUBROUTINE med_before_solve_io

SUBROUTINE med_after_solve_io ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_timing
   USE module_configure
  ! Model layer

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

   RETURN
END SUBROUTINE med_after_solve_io

SUBROUTINE med_pre_nest_initial ( parent , newid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_timing
   USE module_io_domain
   USE module_configure
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

#ifdef MOVE_NESTS

   CALL domain_clock_get( parent, current_timestr=timestr )
   CALL construct_filename2a ( rstname , config_flags%rst_inname , newid , 2 , timestr )

   IF ( config_flags%restart ) THEN
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
   USE module_domain
   USE module_timing
   USE module_io_domain
   USE module_configure
   USE module_utility
  ! Model layer

   IMPLICIT NONE

  ! Arguments
   TYPE(domain) , POINTER                     :: parent, nest
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   TYPE (grid_config_rec_type)                :: nest_config_flags

#if (EM_CORE == 1)
  ! Local
#ifdef MOVE_NESTS
   TYPE (WRFU_Time) :: simStartTime, StartTime
   TYPE (WRFU_TimeInterval) :: interval, TimeSinceStart
   INTEGER :: simulation_start_year, simulation_start_month, simulation_start_day, &
              simulation_start_hour, simulation_start_minute, simulation_start_second
   INTEGER :: start_year, start_month, start_day, &
              start_hour, start_minute, start_second
   INTEGER :: vortex_interval , n
#endif
   INTEGER                                    :: idum1 , idum2 , fid, ierr
   INTEGER                                    :: i , j, rc
   INTEGER                                    :: ids , ide , jds , jde , kds , kde , &
                                                 ims , ime , jms , jme , kms , kme , &
                                                 ips , ipe , jps , jpe , kps , kpe
   CHARACTER * 80         :: rstname , timestr
   CHARACTER * 256        :: message
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
        USE module_domain
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_interp_domain

     SUBROUTINE med_init_domain_constants ( parent , nest )
        USE module_domain
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_init_domain_constants

     SUBROUTINE med_initialdata_input_ptr( nest , config_flags )
        USE module_domain
        USE module_configure
        TYPE (grid_config_rec_type) :: config_flags
        TYPE(domain) , POINTER :: nest
     END SUBROUTINE med_initialdata_input_ptr

     SUBROUTINE med_nest_feedback ( parent , nest , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain), POINTER ::  nest , parent
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_nest_feedback

     SUBROUTINE start_domain ( grid , allowed_to_move )
        USE module_domain
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

     SUBROUTINE  store_terrain ( ter_interpolated , ter_input , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           ips , ipe , jps , jpe , kps , kpe )
       INTEGER                           :: ids , ide , jds , jde , kds , kde , &
                                            ims , ime , jms , jme , kms , kme , &
                                            ips , ipe , jps , jpe , kps , kpe
       REAL , DIMENSION(ims:ime,jms:jme) :: ter_interpolated
       REAL , DIMENSION(ims:ime,jms:jme) :: ter_input
     END SUBROUTINE store_terrain

     SUBROUTINE  input_terrain_rsmas ( grid ,                  &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           ips , ipe , jps , jpe , kps , kpe )
       USE module_domain
       TYPE ( domain ) :: grid
       INTEGER                           :: ids , ide , jds , jde , kds , kde , &
                                            ims , ime , jms , jme , kms , kme , &
                                            ips , ipe , jps , jpe , kps , kpe
     END SUBROUTINE input_terrain_rsmas

   END INTERFACE

   IF ( .not. config_flags%restart ) THEN
     nest%first_force = .true.

! initialize nest with interpolated data from the parent
     nest%imask_nostag = 1
     nest%imask_xstag = 1
     nest%imask_ystag = 1
     nest%imask_xystag = 1

#ifdef MOVE_NESTS
     parent%nest_pos = parent%ht
     where ( parent%nest_pos .gt. 0. ) parent%nest_pos = parent%nest_pos + 500.  ! make a cliff
#endif

     CALL med_interp_domain( parent, nest )

!  De-reference dimension information stored in the grid data structure.
     CALL get_ijk_from_grid (  nest ,                   &
                               ids, ide, jds, jde, kds, kde,    &
                               ims, ime, jms, jme, kms, kme,    &
                               ips, ipe, jps, jpe, kps, kpe    )
  
! initialize some other constants (and 1d arrays in z)
     CALL init_domain_constants ( parent, nest )

! get the nest config flags
     CALL model_to_grid_config_rec ( nest%id , model_config_rec , nest_config_flags )

     IF ( nest_config_flags%input_from_file .OR. nest_config_flags%input_from_hires ) THEN

       WRITE(message,FMT='(A,I2,A)') '*** Initializing nest domain #',nest%id,&
                                      ' from an input file. ***'
       CALL wrf_debug ( 0 , message )

! store horizontally interpolated terrain in temp location
       CALL  store_terrain ( nest%ht_fine , nest%ht , &
                             ids , ide , jds , jde , 1   , 1   , &
                             ims , ime , jms , jme , 1   , 1   , &
                             ips , ipe , jps , jpe , 1   , 1   )
       CALL  store_terrain ( nest%em_mub_fine , nest%em_mub , &
                             ids , ide , jds , jde , 1   , 1   , &
                             ims , ime , jms , jme , 1   , 1   , &
                             ips , ipe , jps , jpe , 1   , 1   )
       CALL  store_terrain ( nest%em_phb_fine , nest%em_phb , &
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

! blend parent and nest fields: terrain, mub, and phb.  THe mub and phb are used in start_domain.
       CALL  blend_terrain ( nest%ht_fine , nest%ht , &
                             ids , ide , jds , jde , 1   , 1   , &
                             ims , ime , jms , jme , 1   , 1   , &
                             ips , ipe , jps , jpe , 1   , 1   )
       CALL  blend_terrain ( nest%em_mub_fine , nest%em_mub , &
                             ids , ide , jds , jde , 1   , 1   , &
                             ims , ime , jms , jme , 1   , 1   , &
                             ips , ipe , jps , jpe , 1   , 1   )
       CALL  blend_terrain ( nest%em_phb_fine , nest%em_phb , &
                             ids , ide , jds , jde , kds , kde , &
                             ims , ime , jms , jme , kms , kme , &
                             ips , ipe , jps , jpe , kps , kpe )

     ELSE
       WRITE(message,FMT='(A,I2,A,I2,A)') '*** Initializing nest domain #',nest%id,&
                                     ' by horizontally interpolating parent domain #' ,parent%id, &
                                     '. ***'
       CALL wrf_debug ( 0 , message )
     END IF


! feedback, mostly for this new terrain, but it is the safe thing to do
     parent%ht_coarse = parent%ht

     CALL med_nest_feedback ( parent , nest , config_flags )

! set some other initial fields, fill out halos, base fields; re-do parent due
! to new terrain elevation from feedback
     nest%imask_nostag = 1
     nest%imask_xstag = 1
     nest%imask_ystag = 1
     nest%imask_xystag = 1
     CALL start_domain ( nest , .TRUE. )
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


  ELSE  ! restart

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

     nest%imask_nostag = 1
     nest%imask_xstag = 1
     nest%imask_ystag = 1
     nest%imask_xystag = 1
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

     CALL nl_get_simulation_start_year   ( nest%id, simulation_start_year   )
     CALL nl_get_simulation_start_month  ( nest%id, simulation_start_month  )
     CALL nl_get_simulation_start_day    ( nest%id, simulation_start_day    )
     CALL nl_get_simulation_start_hour   ( nest%id, simulation_start_hour   )
     CALL nl_get_simulation_start_minute ( nest%id, simulation_start_minute )
     CALL nl_get_simulation_start_second ( nest%id, simulation_start_second )
     CALL WRFU_TimeSet( simStartTime , YY=simulation_start_year,  &
                                       MM=simulation_start_month, &
                                       DD=simulation_start_day,   &
                                       H=simulation_start_hour,   &
                                       M=simulation_start_minute, &
                                       S=simulation_start_second, rc=rc )
     CALL nl_get_start_year   ( nest%id, start_year   )
     CALL nl_get_start_month  ( nest%id, start_month  )
     CALL nl_get_start_day    ( nest%id, start_day    )
     CALL nl_get_start_hour   ( nest%id, start_hour   )
     CALL nl_get_start_minute ( nest%id, start_minute )
     CALL nl_get_start_second ( nest%id, start_second )
     CALL WRFU_TimeSet( StartTime , YY=start_year,  &
                                    MM=start_month, &
                                    DD=start_day,   &
                                    H=start_hour,   &
                                    M=start_minute, &
                                    S=start_second, rc=rc )
     TimeSinceStart = StartTime - simStartTime
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

     CALL nl_get_simulation_start_year   ( nest%id, simulation_start_year   )
     CALL nl_get_simulation_start_month  ( nest%id, simulation_start_month  )
     CALL nl_get_simulation_start_day    ( nest%id, simulation_start_day    )
     CALL nl_get_simulation_start_hour   ( nest%id, simulation_start_hour   )
     CALL nl_get_simulation_start_minute ( nest%id, simulation_start_minute )
     CALL nl_get_simulation_start_second ( nest%id, simulation_start_second )
     CALL WRFU_TimeSet( simStartTime , YY=simulation_start_year,  &
                                       MM=simulation_start_month, &
                                       DD=simulation_start_day,   &
                                       H=simulation_start_hour,   &
                                       M=simulation_start_minute, &
                                       S=simulation_start_second, rc=rc )
     CALL nl_get_start_year   ( nest%id, start_year   )
     CALL nl_get_start_month  ( nest%id, start_month  )
     CALL nl_get_start_day    ( nest%id, start_day    )
     CALL nl_get_start_hour   ( nest%id, start_hour   )
     CALL nl_get_start_minute ( nest%id, start_minute )
     CALL nl_get_start_second ( nest%id, start_second )
     CALL WRFU_TimeSet( StartTime , YY=start_year,  &
                                    MM=start_month, &
                                    DD=start_day,   &
                                    H=start_hour,   &
                                    M=start_minute, &
                                    S=start_second, rc=rc )
     CALL domain_alarm_create( nest,  COMPUTE_VORTEX_CENTER_ALARM, interval  )
     CALL WRFU_AlarmEnable( nest%alarms( COMPUTE_VORTEX_CENTER_ALARM ), rc=rc )

     TimeSinceStart = StartTime - simStartTime
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

  RETURN
END SUBROUTINE med_nest_initial

SUBROUTINE init_domain_constants ( parent , nest )
   USE module_domain
   IMPLICIT NONE
   TYPE(domain) :: parent , nest
#if (EM_CORE == 1)
   INTERFACE 
     SUBROUTINE med_init_domain_constants ( parent , nest )
        USE module_domain
        TYPE(domain) :: parent , nest
     END SUBROUTINE med_init_domain_constants
   END INTERFACE 
   CALL init_domain_constants_em ( parent, nest )
#endif
END SUBROUTINE init_domain_constants


SUBROUTINE med_nest_force ( parent , nest )
  ! Driver layer
   USE module_domain
   USE module_timing
   USE module_configure
  ! Model layer
  ! External
   USE module_utility

   IMPLICIT NONE

  ! Arguments
   TYPE(domain) , POINTER                     :: parent, nest
  ! Local
   INTEGER                                    :: idum1 , idum2 , fid, rc

   INTERFACE
     SUBROUTINE med_force_domain ( parent , nest )
        USE module_domain
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_force_domain
     SUBROUTINE med_interp_domain ( parent , nest )
        USE module_domain
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_interp_domain
   END INTERFACE

   IF ( .NOT. WRFU_ClockIsStopTime(nest%domain_clock ,rc=rc) ) THEN
! initialize nest with interpolated data from the parent
     nest%imask_nostag = 1
     nest%imask_xstag = 1
     nest%imask_ystag = 1
     nest%imask_xystag = 1
     CALL med_force_domain( parent, nest )
   ENDIF

! might also have calls here to do input from a file into the nest

   RETURN
END SUBROUTINE med_nest_force

SUBROUTINE med_nest_feedback ( parent , nest , config_flags )
  ! Driver layer
   USE module_domain
   USE module_timing
   USE module_configure
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
        USE module_domain
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_feedback_domain
   END INTERFACE

! feedback nest to the parent
    IF ( .NOT. WRFU_ClockIsStopTime(nest%domain_clock ,rc=rc) .AND. &
         config_flags%feedback .NE. 0 ) THEN
      CALL med_feedback_domain( parent, nest )
#ifdef MOVE_NESTS
      CALL get_ijk_from_grid (  parent ,                         &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )
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
    END IF

   RETURN
END SUBROUTINE med_nest_feedback

SUBROUTINE med_last_solve_io ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_configure
  ! Model layer

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                                    :: rc

   IF( WRFU_AlarmIsRinging( grid%alarms( HISTORY_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 0 , config_flags )
   ENDIF

   IF( WRFU_AlarmIsRinging( grid%alarms( INPUTOUT_ALARM ), rc=rc ) ) THEN
     CALL med_filter_out  ( grid , config_flags )
   ENDIF

   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST1_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 1 , config_flags )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST2_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 2 , config_flags )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST3_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 3 , config_flags )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST4_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 4 , config_flags )
   ENDIF
   IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST5_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 5 , config_flags )
   ENDIF

! - RESTART OUTPUT
   IF( WRFU_AlarmIsRinging( grid%alarms( RESTART_ALARM ), rc=rc ) ) THEN
     IF ( grid%id .EQ. 1 ) THEN
       CALL med_restart_out ( grid , config_flags )
     ENDIF
   ENDIF

   RETURN
END SUBROUTINE med_last_solve_io

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

RECURSIVE SUBROUTINE med_restart_out ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
   USE module_timing
   USE module_configure
  ! Model layer
   USE module_bc_time_utilities
   USE module_utility

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*80                           :: rstname , outname
   INTEGER                                :: fid , rid, kid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc
   CHARACTER*80                           :: timestr
   TYPE (grid_config_rec_type)            :: kid_config_flags

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE med_hist_out ( grid , stream, config_flags )
  ! Driver layer
   USE module_domain
   USE module_timing
   USE module_io_domain
   USE module_configure
   USE module_bc_time_utilities
   USE module_utility

   IMPLICIT NONE
  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   INTEGER , INTENT(IN)                       :: stream
  ! Local
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*80                           :: fname, n1, n2
   INTEGER                                :: fid , rid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc
   CHARACTER*80                           :: timestr
   TYPE(WRFU_Time)                        :: ST,CT
   INTEGER                                :: n
   LOGICAL                                :: adjust

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

   IF ( stream .LT. 0 .OR. stream .GT. 5 ) THEN
     WRITE(message,*)'med_hist_out: invalid history stream ',stream
     CALL wrf_error_fatal( message )
   ENDIF
   CALL nl_get_adjust_output_times( grid%id, adjust )
   CALL domain_clock_get( grid, current_time=CT, start_time=ST, current_timestr=timestr )

   SELECT CASE( stream )
     CASE ( 0 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( HISTORY_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( fname , config_flags%history_outname , grid%id , 2 , timestr )
     CASE ( 1 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXHIST1_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( fname , config_flags%auxhist1_outname , grid%id , 2 , timestr )
     CASE ( 2 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXHIST2_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( fname , config_flags%auxhist2_outname , grid%id , 2 , timestr )
     CASE ( 3 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXHIST3_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( fname , config_flags%auxhist3_outname , grid%id , 2 , timestr )
     CASE ( 4 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXHIST4_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( fname , config_flags%auxhist4_outname , grid%id , 2 , timestr )
     CASE ( 5 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXHIST5_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( fname , config_flags%auxhist5_outname , grid%id , 2 , timestr )
   END SELECT

   IF (      ( stream .eq. 0 .and. grid%oid          .eq. 0 )    &
        .or. ( stream .eq. 1 .and. grid%auxhist1_oid .eq. 0 )    &
        .or. ( stream .eq. 2 .and. grid%auxhist2_oid .eq. 0 )    &
        .or. ( stream .eq. 3 .and. grid%auxhist3_oid .eq. 0 )    &
        .or. ( stream .eq. 4 .and. grid%auxhist4_oid .eq. 0 )    &
        .or. ( stream .eq. 5 .and. grid%auxhist5_oid .eq. 0 )    &
      ) THEN

     WRITE(n2,'("DATASET=AUXHIST",I1)')stream  ! may be overwritten, below, if stream is 0
     WRITE ( message , '("med_hist_out : opening ",A," for writing. ",I3)') TRIM ( fname ), ierr
     CALL wrf_debug( 1, message )
     SELECT CASE( stream )
       CASE ( 0 )
         CALL open_w_dataset ( grid%oid, TRIM(fname), grid ,  &
                               config_flags , output_history , 'DATASET=HISTORY' , ierr )
       CASE ( 1 )
         CALL open_w_dataset ( grid%auxhist1_oid, TRIM(fname), grid ,  &
                               config_flags , output_aux_hist1 , n2, ierr )
       CASE ( 2 )
         CALL open_w_dataset ( grid%auxhist2_oid, TRIM(fname), grid ,  &
                               config_flags , output_aux_hist2 , n2, ierr )
       CASE ( 3 )
         CALL open_w_dataset ( grid%auxhist3_oid, TRIM(fname), grid ,  &
                               config_flags , output_aux_hist3 , n2, ierr )
       CASE ( 4 )
         CALL open_w_dataset ( grid%auxhist4_oid, TRIM(fname), grid ,  &
                               config_flags , output_aux_hist4 , n2, ierr )
       CASE ( 5 )
         CALL open_w_dataset ( grid%auxhist5_oid, TRIM(fname), grid ,  &
                               config_flags , output_aux_hist5 , n2, ierr )
     END SELECT
     IF ( ierr .NE. 0 ) THEN
       CALL wrf_message( message )
     ENDIF
   END IF
   SELECT CASE( stream )
     CASE ( 0 )
       CALL output_history ( grid%oid, grid , config_flags , ierr )
     CASE ( 1 )
       CALL output_aux_hist1 ( grid%auxhist1_oid, grid , config_flags , ierr )
     CASE ( 2 )
       CALL output_aux_hist2 ( grid%auxhist2_oid, grid , config_flags , ierr )
     CASE ( 3 )
       CALL output_aux_hist3 ( grid%auxhist3_oid, grid , config_flags , ierr )
     CASE ( 4 )
       CALL output_aux_hist4 ( grid%auxhist4_oid, grid , config_flags , ierr )
     CASE ( 5 )
       CALL output_aux_hist5 ( grid%auxhist5_oid, grid , config_flags , ierr )
   END SELECT

   grid%nframes(stream) = grid%nframes(stream) + 1

   SELECT CASE( stream )
     CASE ( 0 )
       IF ( grid%nframes(stream) >= config_flags%frames_per_outfile ) THEN
         CALL close_dataset ( grid%oid , config_flags , "DATASET=HISTORY" ) 
         grid%oid = 0
         grid%nframes(stream) = 0
       ENDIF
     CASE ( 1 )
       IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist1 ) THEN
         CALL close_dataset ( grid%auxhist1_oid , config_flags , n2 ) 
         grid%auxhist1_oid = 0
         grid%nframes(stream) = 0
       ENDIF
     CASE ( 2 )
       IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist2 ) THEN
         CALL close_dataset ( grid%auxhist2_oid , config_flags , n2 ) 
         grid%auxhist2_oid = 0
         grid%nframes(stream) = 0
       ENDIF
     CASE ( 3 )
       IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist3 ) THEN
         CALL close_dataset ( grid%auxhist3_oid , config_flags , n2 ) 
         grid%auxhist3_oid = 0
         grid%nframes(stream) = 0
       ENDIF
     CASE ( 4 )
       IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist4 ) THEN
         CALL close_dataset ( grid%auxhist4_oid , config_flags , n2 ) 
         grid%auxhist4_oid = 0
         grid%nframes(stream) = 0
       ENDIF
     CASE ( 5 )
       IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist5 ) THEN
         CALL close_dataset ( grid%auxhist5_oid , config_flags , n2 ) 
         grid%auxhist5_oid = 0
         grid%nframes(stream) = 0
       ENDIF
   END SELECT
   IF ( wrf_dm_on_monitor() ) THEN
     WRITE ( message , FMT = '("Writing ",A30," for domain ",I8)' )TRIM(fname),grid%id
     CALL end_timing ( TRIM(message) )
   END IF
   RETURN

   RETURN
END SUBROUTINE med_hist_out

SUBROUTINE med_auxinput1_in ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxinput_in( grid , 1 , config_flags )
   RETURN
END SUBROUTINE med_auxinput1_in

SUBROUTINE med_auxinput2_in ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxinput_in( grid , 2 , config_flags )
   RETURN
END SUBROUTINE med_auxinput2_in

SUBROUTINE med_auxinput3_in ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxinput_in( grid , 3 , config_flags )
   RETURN
END SUBROUTINE med_auxinput3_in

SUBROUTINE med_auxinput4_in ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxinput_in( grid , 4 , config_flags )
   RETURN
END SUBROUTINE med_auxinput4_in

SUBROUTINE med_auxinput5_in ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxinput_in( grid , 5 , config_flags )
   RETURN
END SUBROUTINE med_auxinput5_in

SUBROUTINE med_auxinput_in ( grid , stream, config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities
   USE module_utility

   IMPLICIT NONE
  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   INTEGER , INTENT(IN)                       :: stream
  ! Local
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*80                           :: rstname , outname, auxname, n1, n2
   INTEGER                                :: fid , rid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc
   CHARACTER*80                           :: timestr
   TYPE(WRFU_Time)                        :: ST,CT
   INTEGER                                :: n
   LOGICAL                                :: adjust

   CALL nl_get_adjust_input_times( grid%id, adjust )

   IF ( stream .LT. 1 .OR. stream .GT. 5 ) THEN
     WRITE(message,*)'med_auxinput_in: invalid inputory stream ',stream
     CALL wrf_error_fatal( message )
   ENDIF
   CALL domain_clock_get( grid, current_time=CT, start_time=ST, current_timestr=timestr )
   SELECT CASE( stream )
     CASE ( 1 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXINPUT1_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( auxname , config_flags%auxinput1_inname, grid%id , 2 , timestr )
     CASE ( 2 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXINPUT2_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( auxname , config_flags%auxinput2_inname , grid%id , 2 , timestr )
     CASE ( 3 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXINPUT3_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( auxname , config_flags%auxinput3_inname , grid%id , 2 , timestr )
     CASE ( 4 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXINPUT4_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( auxname , config_flags%auxinput4_inname , grid%id , 2 , timestr )
     CASE ( 5 )
       IF ( adjust ) CALL adjust_io_timestr( grid%io_intervals( AUXINPUT5_ALARM ), CT, ST, timestr )
       CALL construct_filename2a ( auxname , config_flags%auxinput5_inname , grid%id , 2 , timestr )
   END SELECT
   IF (      ( stream .eq. 1 .and. grid%auxinput1_oid .eq. 0 )    &
        .or. ( stream .eq. 2 .and. grid%auxinput2_oid .eq. 0 )    &
        .or. ( stream .eq. 3 .and. grid%auxinput3_oid .eq. 0 )    &
        .or. ( stream .eq. 4 .and. grid%auxinput4_oid .eq. 0 )    &
        .or. ( stream .eq. 5 .and. grid%auxinput5_oid .eq. 0 )    &
      ) THEN

     WRITE(n2,'("DATASET=AUXINPUT",I1)')stream
     WRITE ( message , '("med_auxinput_in : opening ",A," for reading. ",I3)') TRIM ( auxname ), ierr
     CALL wrf_debug( 1, message )

!<DESCRIPTION>
!
!Open_u_dataset is called rather than open_r_dataset to allow interfaces
!that can do blending or masking to update an existing field. (MCEL IO does this).
!No effect for other interfaces; open_u_dataset is equivalent to open_r_dataset 
!in those cases.
!
!</DESCRIPTION>

     SELECT CASE( stream )
       CASE ( 1 )
         CALL open_u_dataset ( grid%auxinput1_oid, TRIM(auxname), grid ,  &
                               config_flags , input_aux_model_input1 , n2, ierr )
       CASE ( 2 )
         CALL open_u_dataset ( grid%auxinput2_oid, TRIM(auxname), grid ,  &
                               config_flags , input_aux_model_input2 , n2, ierr )
       CASE ( 3 )
         CALL open_u_dataset ( grid%auxinput3_oid, TRIM(auxname), grid ,  &
                               config_flags , input_aux_model_input3 , n2, ierr )
       CASE ( 4 )
         CALL open_u_dataset ( grid%auxinput4_oid, TRIM(auxname), grid ,  &
                               config_flags , input_aux_model_input4 , n2, ierr )
       CASE ( 5 )
         CALL open_u_dataset ( grid%auxinput5_oid, TRIM(auxname), grid ,  &
                               config_flags , input_aux_model_input5 , n2, ierr )
     END SELECT
     IF ( ierr .NE. 0 ) THEN
       CALL wrf_message( message )
     ENDIF
   END IF
   SELECT CASE( stream )
     CASE ( 1 )
       CALL input_aux_model_input1 ( grid%auxinput1_oid, grid , config_flags , ierr )
     CASE ( 2 )
       CALL input_aux_model_input2 ( grid%auxinput2_oid, grid , config_flags , ierr )
     CASE ( 3 )
       CALL input_aux_model_input3 ( grid%auxinput3_oid, grid , config_flags , ierr )
     CASE ( 4 )
       CALL input_aux_model_input4 ( grid%auxinput4_oid, grid , config_flags , ierr )
     CASE ( 5 )
       CALL input_aux_model_input5 ( grid%auxinput5_oid, grid , config_flags , ierr )
   END SELECT
   RETURN
END SUBROUTINE med_auxinput_in

SUBROUTINE med_filter_out ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
   USE module_timing
   USE module_configure
  ! Model layer
   USE module_bc_time_utilities

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*80                           :: rstname , outname
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

     WRITE ( message , '("med_filter_out 1: opening ",A," for writing. ",I3)') TRIM ( outname ), ierr
     CALL wrf_debug( 1, message )

     CALL open_w_dataset ( fid, TRIM(outname), grid ,  &
                           config_flags , output_model_input , "DATASET=INPUT", ierr )
     IF ( ierr .NE. 0 ) THEN
       CALL wrf_error_fatal( message )
     ENDIF

     IF ( ierr .NE. 0 ) THEN
       CALL wrf_error_fatal( message )
     ENDIF

   CALL output_model_input ( fid, grid , config_flags , ierr )
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
   USE module_domain
   USE module_io_domain
   USE module_timing
   USE module_configure
  ! Model layer
   USE module_bc_time_utilities
   USE module_utility

   IMPLICIT NONE

#include <wrf_status_codes.h>

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local data
   LOGICAL, EXTERNAL                      :: wrf_dm_on_monitor
   LOGICAL                                :: lbc_opened
   INTEGER                                :: idum1 , idum2 , ierr , open_status , fid, rc
   REAL                                   :: bfrq
   CHARACTER (LEN=256)                    :: message
   CHARACTER (LEN=80)                     :: bdyname
   Type (WRFU_Time )                      :: startTime, stopTime, currentTime
   Type (WRFU_TimeInterval )              :: stepTime

#include <wrf_io_flags.h>

   CALL wrf_debug ( 1 , 'in med_latbound_in' )

   IF ( grid%id .EQ. 1 .AND. config_flags%specified .AND. config_flags%io_form_boundary .GT. 0 ) THEN

     CALL domain_clock_get( grid, current_time=currentTime, &
                                  start_time=startTime,     &
                                  stop_time=stopTime,       &
                                  time_step=stepTime )

     IF ( ( lbc_read_time( currentTime ) ) .AND. &
          ( currentTime + stepTime .GE. stopTime ) .AND. &
          ( currentTime .NE. startTime ) ) THEN
       CALL wrf_debug( 100 , 'med_latbound_in: Skipping attempt to read lateral boundary file during last time step ' )

     ELSE IF ( WRFU_AlarmIsRinging( grid%alarms( BOUNDARY_ALARM ), rc=rc ) ) THEN
       CALL wrf_debug ( 1 , 'in med_latbound_in preparing to read' )
       CALL WRFU_AlarmRingerOff( grid%alarms( BOUNDARY_ALARM ), rc=rc )
       IF ( wrf_dm_on_monitor() ) CALL start_timing

! typically a <date> wouldn't be part of the bdy_inname, so just pass a dummy
       CALL construct_filename2a ( bdyname , config_flags%bdy_inname , grid%id , 2 , 'dummydate' )

       CALL wrf_inquire_opened(head_grid%lbc_fid , TRIM(bdyname) , open_status , ierr ) 
       IF ( open_status .EQ. WRF_FILE_OPENED_FOR_READ ) THEN
         lbc_opened = .TRUE.
       ELSE
         lbc_opened = .FALSE.
       ENDIF
       CALL wrf_dm_bcast_bytes ( lbc_opened , LWORDSIZE )
       IF ( .NOT. lbc_opened ) THEN
         CALL construct_filename1 ( bdyname , 'wrfbdy' , grid%id , 2 )
         CALL open_r_dataset ( head_grid%lbc_fid, TRIM(bdyname) , grid , config_flags , "DATASET=BOUNDARY", ierr )
          IF ( ierr .NE. 0 ) THEN
            WRITE( message, * ) 'med_latbound_in: error opening ',TRIM(bdyname), ' for reading. IERR = ',ierr
            CALL WRF_ERROR_FATAL( message )
          ENDIF
       ELSE
         CALL wrf_debug( 100 , bdyname // 'already opened' )
       ENDIF
       CALL wrf_debug( 100 , 'med_latbound_in: calling input_boundary ' )
       CALL input_boundary ( grid%lbc_fid, grid , config_flags , ierr )

       CALL domain_clock_get( grid, current_time=currentTime )
       DO WHILE (currentTime .GE. grid%next_bdy_time )         ! next_bdy_time is set by input_boundary from bdy file
         CALL wrf_debug( 100 , 'med_latbound_in: calling input_boundary ' )
         CALL input_boundary ( grid%lbc_fid, grid , config_flags , ierr )
       ENDDO
       CALL WRFU_AlarmSet( grid%alarms( BOUNDARY_ALARM ), RingTime=grid%next_bdy_time, rc=rc )

       IF ( ierr .NE. 0 .and. ierr .NE. WRF_WARN_NETCDF ) THEN
         WRITE( message, * ) 'med_latbound_in: error reading ',TRIM(bdyname), ' IERR = ',ierr
         CALL WRF_ERROR_FATAL( message )
       ENDIF
       IF ( currentTime .EQ. grid%this_bdy_time ) grid%dtbc = 0.
  
       IF ( wrf_dm_on_monitor() ) THEN
         WRITE ( message , FMT = '("processing lateral boundary for domain ",I8)' ) grid%id
         CALL end_timing ( TRIM(message) )
       ENDIF

!#if 0
     ENDIF
!#endif
   ENDIF
   RETURN
END SUBROUTINE med_latbound_in

SUBROUTINE med_setup_step ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_configure
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
   USE module_domain
   USE module_configure
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

 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifdef WRF_CHEM
!------------------------------------------------------------------------
! Chemistry emissions input control. Three options are available and are
! set via the namelist variable io_style_emissions:
!
!   0 = Emissions are not read in from a file. They will contain their
!       default values, which can be set in the Registry.
!       (Intended for debugging of chem code)
!
!   1 = Emissions are read in from two 12 hour files that are cycled.
!       With this choice, emi_inname and emi_outname should be set to
!       the value "wrfchemi_d<domain>". The value of frames_per_emissfile
!       is ignored.
!
!   2 = Emissions are read in from files identified by date and that have
!       a length defined by frames_per_emissfile (in hours). Both
!       emi_inname and emi_outname should be set to 
!       "wrfchemi_d<domain>_<date>".
!------------------------------------------------------------------------
SUBROUTINE med_read_wrf_chem_emiss ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
   USE module_timing
   USE module_configure
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
   REAL                                   :: time, tupdate
   real, allocatable :: dumc0(:,:,:)
   CHARACTER (LEN=256)                    :: message, current_date_char, date_string
   CHARACTER (LEN=80)                     :: inpname

#include <wrf_io_flags.h>

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
         IF( currentTime + stepTime .GE. stopTime .AND. &
              grid%auxinput5_oid .NE. 0 ) THEN
            CALL close_dataset ( grid%auxinput5_oid , config_flags , "DATASET=AUXINPUT5" )
            tupdate = 1.
         ENDIF

         print *,' EMISSIONS UPDATE TIME ',time,TRIM(current_date_char(12:13))
         print *,tupdate

         IF ( tupdate .EQ. 0. .AND.  current_date_char(12:13) .EQ. '00' ) THEN
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
         ELSE IF ( tupdate .EQ. 0. .AND. current_date_char(12:13) .EQ. '12' ) THEN
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

         print *,' HOURLY EMISSIONS UPDATE TIME ',time,mod(time,3600.)
!         print *, currentTime + stepTime, stopTime
! hourly updates to emissions
         IF ( ( mod( time, 3600. ) .EQ. 0   ) .AND. &
              ( currentTime + stepTime .LT. stopTime ) ) THEN
!           IF ( wrf_dm_on_monitor() ) CALL start_timing

            WRITE(message,*)'mediation_integrate: med_read_wrf_chem_emissions: Read emissions for time ',TRIM(current_date_char)
            CALL wrf_message( TRIM(message) )

            CALL wrf_debug (100 , 'mediation_integrate: calling input_aux_model_input5' )
            CALL input_aux_model_input5 ( grid%auxinput5_oid, grid , config_flags , ierr )
         ELSE
            CALL wrf_debug (100 , 'mediation_integrate: med_read_wrf_chem_emissions: Do not read emissions' )
         ENDIF


!---
! io_style_emissions option 2: use dated emission files whose length is
!                             set via frames_per_emissfile...
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
         CALL wrf_debug (100 , 'mediation_integrate: calling input_aux_model_input5' )
         CALL input_aux_model_input5 ( grid%auxinput5_oid, grid , config_flags , ierr )
!
! If reached the indicated number of frames in the emissions file, close it.
!
         grid%emissframes = grid%emissframes + 1
         IF ( grid%emissframes >= config_flags%frames_per_emissfile ) THEN
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
   USE module_domain
   USE module_io_domain
   USE module_timing
   USE module_configure
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
   CHARACTER (LEN=80)                     :: inpname

#include <wrf_io_flags.h>
    IF ( grid%id .EQ. 1 ) THEN

      CALL domain_clock_get( grid, current_timestr=current_date_char )

      CALL construct_filename1 ( inpname , 'wrfbiochemi' , grid%id , 2 )
      WRITE(message,*)'mediation_integrate: med_read_wrf_chem_bioemissions: Open file ',TRIM(inpname)
      CALL wrf_message( TRIM(message) )

     if( grid%auxinput4_oid .NE. 0 ) then
       CALL close_dataset ( grid%auxinput4_oid , config_flags , "DATASET=AUXINPUT4" )
     endif

      CALL open_r_dataset ( grid%auxinput4_oid, TRIM(inpname) , grid , config_flags, &
                              "DATASET=AUXINPUT4", ierr )
        IF ( ierr .NE. 0 ) THEN
           WRITE( message , * ) 'med_read_wrf_chem_bioemissions: error opening ', TRIM( inpname )
           CALL wrf_error_fatal( TRIM( message ) )
        ENDIF

         WRITE(message,*)'mediation_integrate: med_read_wrf_chem_bioemissions: Read biogenic emissions at time ',&
         TRIM(current_date_char)
         CALL wrf_message( TRIM(message) )

         CALL wrf_debug (100 , 'mediation_integrate: calling input_aux_model_input4' )
         CALL input_aux_model_input4 ( grid%auxinput4_oid, grid , config_flags , ierr )

         CALL close_dataset ( grid%auxinput4_oid , config_flags , "DATASET=AUXINPUT4" )

   ENDIF
   CALL wrf_debug (100 , 'mediation_integrate: med_read_wrf_chem_bioemissions: exit' )

END SUBROUTINE med_read_wrf_chem_bioemiss
#endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
