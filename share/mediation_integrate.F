!
!WRF:MEDIATION_LAYER:IO
!

SUBROUTINE med_calc_model_time ( grid , config_flags )
  ! Driver layer
   USE module_domain
  ! Model layer
   USE module_configure
   USE module_date_time

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local data
   REAL                                       :: time 

! this is now handled by with calls to ESMF time manager
!   time = head_grid%dt * head_grid%total_time_steps
!   CALL calc_current_date (grid%id, time)


END SUBROUTINE med_calc_model_time

SUBROUTINE med_before_solve_io ( grid , config_flags )
  ! Driver layer
   USE module_domain
  ! Model layer
   USE module_configure
   USE esmf_mod

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                                    :: rc

   IF( ESMF_AlarmIsRinging( grid%alarms( HISTORY_ALARM ), rc=rc ) ) THEN
     CALL med_history_out ( grid , config_flags )
     CALL ESMF_AlarmTurnOff( grid%alarms( HISTORY_ALARM ), rc=rc )
   ENDIF

   IF( ESMF_AlarmIsRinging( grid%alarms( INPUTOUT_ALARM ), rc=rc ) ) THEN
     CALL med_filter_out  ( grid , config_flags )
     CALL ESMF_AlarmTurnOff( grid%alarms( INPUTOUT_ALARM ), rc=rc )
   ENDIF

! - AUX HISTORY OUTPUT 
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST1_ALARM ), rc=rc ) ) THEN
     CALL med_auxhist1_out ( grid , config_flags )
     CALL ESMF_AlarmTurnOff( grid%alarms( AUXHIST1_ALARM ), rc=rc )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST2_ALARM ), rc=rc ) ) THEN
     CALL med_auxhist2_out ( grid , config_flags )
     CALL ESMF_AlarmTurnOff( grid%alarms( AUXHIST2_ALARM ), rc=rc )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST3_ALARM ), rc=rc ) ) THEN
     CALL med_auxhist3_out ( grid , config_flags )
     CALL ESMF_AlarmTurnOff( grid%alarms( AUXHIST3_ALARM ), rc=rc )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST4_ALARM ), rc=rc ) ) THEN
     CALL med_auxhist4_out ( grid , config_flags )
     CALL ESMF_AlarmTurnOff( grid%alarms( AUXHIST4_ALARM ), rc=rc )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST5_ALARM ), rc=rc ) ) THEN
     CALL med_auxhist5_out ( grid , config_flags )
     CALL ESMF_AlarmTurnOff( grid%alarms( AUXHIST5_ALARM ), rc=rc )
   ENDIF

! - AUX INPUT INPUT
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXINPUT1_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput1_in ( grid , config_flags )
     CALL ESMF_AlarmTurnOff( grid%alarms( AUXINPUT1_ALARM ), rc=rc )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXINPUT2_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput2_in ( grid , config_flags )
     CALL ESMF_AlarmTurnOff( grid%alarms( AUXINPUT2_ALARM ), rc=rc )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXINPUT3_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput3_in ( grid , config_flags )
     CALL ESMF_AlarmTurnOff( grid%alarms( AUXINPUT3_ALARM ), rc=rc )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXINPUT4_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput4_in ( grid , config_flags )
     CALL ESMF_AlarmTurnOff( grid%alarms( AUXINPUT4_ALARM ), rc=rc )
   ENDIF

! this needs to be looked at again so we can get rid of the special
! handling of AUXINPUT5 but for now...

#ifdef WRF_CHEM
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! add for wrf_chem emiss input
! - Get chemistry data
  IF( config_flags%chem_opt > 0 ) THEN
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXINPUT5_ALARM ), rc=rc ) ) THEN
    print *,' CALL med_read_wrf_chem_emiss '
     CALL med_read_wrf_chem_emiss ( grid , config_flags )
     CALL ESMF_AlarmTurnOff( grid%alarms( AUXINPUT5_ALARM ), rc=rc )
    print *,' back from CALL med_read_wrf_chem_emiss '

   ENDIF
! end for wrf chem emiss input
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ELSE
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXINPUT5_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput5_in ( grid , config_flags )
     CALL ESMF_AlarmTurnOff( grid%alarms( AUXINPUT5_ALARM ), rc=rc )
   ENDIF
  ENDIF
#endif

! - RESTART OUTPUT
   IF( ESMF_AlarmIsRinging( grid%alarms( RESTART_ALARM ), rc=rc ) ) THEN
     CALL med_restart_out ( grid , config_flags )
     CALL ESMF_AlarmTurnOff( grid%alarms( RESTART_ALARM ), rc=rc )
   ENDIF

! - Look for boundary data after writing out history and restart files
   CALL med_latbound_in ( grid , config_flags )

   RETURN
END SUBROUTINE med_before_solve_io

SUBROUTINE med_after_solve_io ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_timing
  ! Model layer
   USE module_configure

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

   RETURN
END SUBROUTINE med_after_solve_io

SUBROUTINE med_nest_initial ( parent , nest , config_flags )
  ! Driver layer
   USE module_domain
   USE module_timing
   USE module_io_domain
  ! Model layer
   USE module_configure

   IMPLICIT NONE

  ! Arguments
   TYPE(domain) , POINTER                     :: parent, nest
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   TYPE (grid_config_rec_type)                :: nest_config_flags

#if (EM_CORE == 1)
  ! Local
   INTEGER                                    :: idum1 , idum2 , fid, ierr
   INTEGER                                    :: i , j
   INTEGER                                    :: ids , ide , jds , jde , kds , kde , &
                                                 ims , ime , jms , jme , kms , kme , &
                                                 ips , ipe , jps , jpe , kps , kpe
   CHARACTER * 80         :: rstname , timestr
   CHARACTER * 256        :: message
   TYPE(ESMF_Time)        :: CurrTime
   INTEGER                :: save_itimestep ! This is a kludge, correct fix will 
                                            ! involve integrating the time-step
                                            ! counting into the ESMF time manager.
                                            ! JM 20040604
   REAL, ALLOCATABLE, DIMENSION(:,:) ::   save_acsnow             &
                                         ,save_acsnom             &
                                         ,save_cuppt              &
                                         ,save_rainc              &
                                         ,save_rainnc             &
                                         ,save_sfcevp             &
                                         ,save_sfcrunoff          &
                                         ,save_udrunoff

integer myproc

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
   END INTERFACE

   nest%first_force = .true.

   IF ( .not. config_flags%restart ) THEN

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

     IF ( nest_config_flags%input_from_file ) THEN
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

! read input from dataset
       CALL med_initialdata_input_ptr( nest , nest_config_flags )

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


  ELSE

    CALL ESMF_ClockGetCurrTime( nest%domain_clock, CurrTime=CurrTime, rc=ierr )
    CALL ESMF_TimeGetString( CurrTime, timestr, rc=ierr )
    CALL construct_filename2a ( rstname , config_flags%rst_inname , nest%id , 2 , timestr )

    WRITE(message,*)'opening ',TRIM(rstname),' for reading'
    CALL wrf_debug ( 1 , message )
    CALL open_r_dataset ( fid , TRIM(rstname) , nest , config_flags , "DATASET=RESTART", ierr )
    IF ( ierr .NE. 0 ) THEN
      WRITE( message , '("program wrf: error opening ",A32," for reading")') TRIM(rstname)
      CALL WRF_ERROR_FATAL ( message )
    ENDIF
    CALL input_restart ( fid,   nest , config_flags , ierr )
    CALL close_dataset ( fid , config_flags , "DATASET=RESTART" )

    nest%imask_nostag = 1
    nest%imask_xstag = 1
    nest%imask_ystag = 1
    nest%imask_xystag = 1
    CALL start_domain ( nest , .TRUE. )

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

#ifdef MOVE_NESTS
SUBROUTINE med_nest_move ( parent, nest )
  ! Driver layer
   USE module_domain
   USE module_timing
   USE module_configure
   USE module_io_domain
   TYPE(domain) , POINTER                     :: parent, nest, grid
   INTEGER dx, dy       ! number of parent domain points to move
  ! Local 
   INTEGER i, j, p, parent_grid_ratio
   INTEGER px, py       ! number and direction of nd points to move
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe
   INTEGER ierr, fid

#if ( EM_CORE == 1 && defined(DM_PARALLEL) )
   INTERFACE
     SUBROUTINE med_interp_domain ( parent , nest )
        USE module_domain
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_interp_domain
     SUBROUTINE start_domain ( grid , allowed_to_move )
        USE module_domain
        TYPE(domain) :: grid
        LOGICAL, INTENT(IN) :: allowed_to_move
     END SUBROUTINE start_domain
     SUBROUTINE shift_domain_em ( grid, disp_x, disp_y,  &
!
# include <em_dummy_args.inc>
!
                           )
        USE module_domain
        USE module_configure
        USE module_timing
        IMPLICIT NONE
        INTEGER disp_x, disp_y
        TYPE(domain) , POINTER                 :: grid
#include <em_dummy_decl.inc>
     END SUBROUTINE shift_domain_em
     LOGICAL FUNCTION TIME_FOR_MOVE ( grid , dx , dy )
        USE module_domain
        USE esmf_mod
        TYPE(domain) , POINTER    :: grid
        INTEGER, INTENT(OUT)      :: dx , dy
     END FUNCTION TIME_FOR_MOVE
   END INTERFACE

#ifdef DEREF_KLUDGE
   ! see <a href="www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm">here</a>
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif

  ! set grid pointer for code in deref_kludge (if used)
  grid => nest
#include "deref_kludge.h"

! mask should be defined in nest domain

   check_for_move: IF ( time_for_move ( nest , dx, dy ) ) THEN

     IF ( dx .gt. 1 .or. dy .gt. 1 ) THEN
       CALL wrf_error_fatal( 'med_nest_move' )
     ENDIF


     CALL get_ijk_from_grid (  grid ,                   &
                               ids, ide, jds, jde, kds, kde,    &
                               ims, ime, jms, jme, kms, kme,    &
                               ips, ipe, jps, jpe, kps, kpe    )

#ifdef RSL
! tell RSL about the move; this needs to be moved into external directory
     CALL rsl_move_nest ( parent%domdesc, nest%intermediate_grid%domdesc, dx, dy )
     CALL adjust_domain_dims_for_move( nest%intermediate_grid , dx, dy )
#endif

     CALL get_ijk_from_grid (  grid ,                   &
                               ids, ide, jds, jde, kds, kde,    &
                               ims, ime, jms, jme, kms, kme,    &
                               ips, ipe, jps, jpe, kps, kpe    )

     grid => nest 

     CALL shift_domain_em( grid, dx, dy,  &
!
# include <em_actual_args.inc>
!
                         )

     px = grid%parent_grid_ratio*dx
     py = grid%parent_grid_ratio*dy

     grid%i_parent_start = grid%i_parent_start + px / grid%parent_grid_ratio 
     CALL nl_set_i_parent_start( grid%id, grid%i_parent_start )
     grid%j_parent_start = grid%j_parent_start + py / grid%parent_grid_ratio
     CALL nl_set_j_parent_start( grid%id, grid%j_parent_start )

     CALL med_interp_domain( parent, nest )

     nest%moved = .true.
! masks associated with nest will have been set by shift_domain_em above
     CALL start_domain ( nest , .FALSE. )
     nest%moved = .false.

! copy time level 2 to time level 1 in new regions of multi-time level fields
! this should be registry generated.
     do k = kms,kme
       where ( nest%imask_xstag  .EQ. 1 ) nest%em_u_1(:,k,:)   = nest%em_u_2(:,k,:)
       where ( nest%imask_ystag  .EQ. 1 ) nest%em_v_1(:,k,:)   = nest%em_v_2(:,k,:)
       where ( nest%imask_nostag .EQ. 1 ) nest%em_t_1(:,k,:)   = nest%em_t_2(:,k,:)
       where ( nest%imask_nostag .EQ. 1 ) nest%em_w_1(:,k,:)   = nest%em_w_2(:,k,:)
       where ( nest%imask_nostag .EQ. 1 ) nest%em_ph_1(:,k,:)  = nest%em_ph_2(:,k,:)
       where ( nest%imask_nostag .EQ. 1 ) nest%em_tp_1(:,k,:)  = nest%em_tp_2(:,k,:)
       where ( nest%imask_nostag .EQ. 1 ) nest%em_tke_1(:,k,:) = nest%em_tke_2(:,k,:)
     enddo
     do p = PARAM_FIRST_SCALAR , num_moist
       do k = kms,kme
         where ( nest%imask_nostag .EQ. 1 ) nest%moist_1(:,k,:,p) = nest%moist_2(:,k,:,p)
       enddo
     enddo
     do p = PARAM_FIRST_SCALAR , num_chem
       do k = kms,kme
         where ( nest%imask_nostag .EQ. 1 ) nest%chem_1(:,k,:,p) = nest%chem_2(:,k,:,p)
       enddo
     enddo
     where ( nest%imask_nostag .EQ. 1 ) nest%em_mu_1(:,:)  = nest%em_mu_2(:,:)
!

   ENDIF check_for_move

#endif

END SUBROUTINE med_nest_move

LOGICAL FUNCTION time_for_move ( grid , move_cd_x, move_cd_y )
  ! Driver layer
   USE module_domain
   USE module_configure
   USE esmf_mod
   IMPLICIT NONE
! arguments
   TYPE(domain) , POINTER    :: grid
   INTEGER, INTENT(OUT)      :: move_cd_x , move_cd_y
! local
   INTEGER  i, num_moves, rc
   INTEGER  move_interval , move_id
   TYPE(ESMF_Time) :: ct, st
   TYPE(ESMF_TimeInterval) :: ti
   CHARACTER*256 mess

   TIME_FOR_MOVE = .FALSE.
   CALL ESMF_ClockGetCurrTime( grid%domain_clock, CurrTime=ct, rc=rc )
   CALL ESMF_ClockGetStartTime( grid%domain_clock, StartTime=st, rc=rc )
   CALL nl_get_num_moves( 1, num_moves )
   IF ( num_moves .GT. max_moves ) THEN
     WRITE(mess,*)'mediation_integrate: time_for_move: num_moves (',num_moves,') .GT. max_moves (',max_moves,')'
     CALL wrf_error_fatal( TRIM(mess) )
   ENDIF
   DO I = 1, num_moves
     CALL nl_get_move_id( i, move_id )
     IF ( move_id .EQ. grid%id ) THEN
       CALL nl_get_move_interval( i, move_interval )
       IF ( move_interval .LT. 999999999 ) THEN
         CALL ESMF_TimeIntervalSet ( ti, M=move_interval, rc=rc )
         IF ( ct .GE. grid%start_time + ti ) THEN
           CALL nl_get_move_cd_x ( i, move_cd_x )
           CALL nl_get_move_cd_y ( i, move_cd_y )
           CALL nl_set_move_interval ( i, 999999999 )
           TIME_FOR_MOVE = .TRUE.
           EXIT
         ENDIF
       ENDIF
     ENDIF
   ENDDO
   RETURN

END FUNCTION time_for_move

#endif

#if ( EM_CORE == 1 && defined(DM_PARALLEL) && MOVE_NESTS )

SUBROUTINE shift_domain_em ( grid , disp_x, disp_y, &
!
# include <em_dummy_args.inc>
!
                           )
   USE module_domain
   USE module_timing
   USE module_configure
   USE module_dm
   USE module_timing
   IMPLICIT NONE
  ! Arguments
   INTEGER disp_x, disp_y       ! number of parent domain points to move
   TYPE(domain) , POINTER                     :: grid
  ! Local 
   INTEGER i, j, ii
   INTEGER px, py       ! number and direction of nd points to move
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe
   TYPE (grid_config_rec_type)  :: config_flags
#ifdef DEREF_KLUDGE
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
#endif

   !  Definitions of dummy arguments to solve
#include <em_dummy_decl.inc>

#define COPY_IN
#include <em_scalar_derefs.inc>
#include "deref_kludge.h"
#ifdef DM_PARALLEL
#      include <em_data_calls.inc>
#endif

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   CALL get_ijk_from_grid (  grid ,                   &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )

   px = isign(parent_grid_ratio,disp_x)
   py = isign(parent_grid_ratio,disp_y)

   imask_nostag = 1
   imask_xstag = 1
   imask_ystag = 1
   imask_xystag = 1

   imask_nostag(ips:min(ide-1,ipe),jps:min(jde-1,jpe)) = 0
   imask_xstag(ips:ipe,jps:min(jde-1,jpe)) = 0
   imask_ystag(ips:min(ide-1,ipe),jps:jpe) = 0
   imask_xystag(ips:ipe,jps:jpe) = 0

#if 1

! shift the nest domain in x
   do ii = 1,abs(disp_x)
#include <em_shift_halo_x.inc>
   enddo

! shift the nest domain in y
   do ii = 1,abs(disp_y)
#include <em_shift_halo_y.inc>
   enddo

#endif

#define COPY_OUT
#include <em_scalar_derefs.inc>

END SUBROUTINE shift_domain_em
#endif

SUBROUTINE med_nest_force ( parent , nest )
  ! Driver layer
   USE module_domain
   USE module_timing
  ! Model layer
   USE module_configure
  ! External
   USE esmf_mod

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

   IF ( .NOT. ESMF_ClockIsStopTime(nest%domain_clock ,rc=rc) ) THEN
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
   USE esmf_mod
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
    IF ( .NOT. ESMF_ClockIsStopTime(nest%domain_clock ,rc=rc) .AND. &
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
  ! Model layer
   USE module_configure

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                                    :: rc

   IF( ESMF_AlarmIsRinging( grid%alarms( HISTORY_ALARM ), rc=rc ) ) THEN
     CALL med_history_out ( grid , config_flags )
   ENDIF

   IF( ESMF_AlarmIsRinging( grid%alarms( INPUTOUT_ALARM ), rc=rc ) ) THEN
     CALL med_filter_out  ( grid , config_flags )
   ENDIF

   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST1_ALARM ), rc=rc ) ) THEN
     CALL med_auxhist1_out ( grid , config_flags )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST2_ALARM ), rc=rc ) ) THEN
     CALL med_auxhist2_out ( grid , config_flags )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST3_ALARM ), rc=rc ) ) THEN
     CALL med_auxhist3_out ( grid , config_flags )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST4_ALARM ), rc=rc ) ) THEN
     CALL med_auxhist4_out ( grid , config_flags )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST5_ALARM ), rc=rc ) ) THEN
     CALL med_auxhist5_out ( grid , config_flags )
   ENDIF

! - RESTART OUTPUT
   IF( ESMF_AlarmIsRinging( grid%alarms( RESTART_ALARM ), rc=rc ) ) THEN
     CALL med_restart_out ( grid , config_flags )
   ENDIF

   RETURN
END SUBROUTINE med_last_solve_io

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE med_restart_out ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
   USE module_timing
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities
   USE ESMF_Mod

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*80                           :: rstname , outname
   INTEGER                                :: fid , rid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc
   TYPE(ESMF_Time)                        :: CurrTime
   CHARACTER*80                           :: timestr

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

   CALL ESMF_ClockGetCurrTime( grid%domain_clock, CurrTime=CurrTime, rc=ierr )
   CALL ESMF_TimeGetString( CurrTime, timestr, rc=ierr )
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
   RETURN
END SUBROUTINE med_restart_out

SUBROUTINE med_history_out ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
   USE module_timing
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities
   USE ESMF_Mod

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  ! Local
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*80                           :: rstname , outname
   INTEGER                                :: fid , rid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc
   TYPE(ESMF_Time)                        :: CurrTime
   CHARACTER*80                           :: timestr

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

   IF ( grid%oid .eq. 0 ) THEN

     CALL ESMF_ClockGetCurrTime( grid%domain_clock, CurrTime=CurrTime, rc=ierr )
     CALL ESMF_TimeGetString( CurrTime, timestr, rc=ierr )
     CALL construct_filename2a ( outname , config_flags%history_outname , grid%id , 2 , timestr )

     WRITE ( message , '("med_history_out 1: opening ",A," for writing. ",I3)') TRIM ( outname ), ierr
     CALL wrf_debug( 1, message )

     CALL open_w_dataset ( grid%oid, TRIM(outname), grid ,  &
                           config_flags , output_history , "DATASET=HISTORY", ierr )
     WRITE ( message , '("med_history_out 1: back from opening ",A," for writing. ",I3)') TRIM ( outname ), ierr
     CALL wrf_debug( 1, message )
     IF ( ierr .NE. 0 ) THEN
       CALL wrf_message( message )
     ENDIF
   END IF

   CALL output_history ( grid%oid, grid , config_flags , ierr )

   grid%nframes = grid%nframes + 1
   IF ( grid%nframes >= config_flags%frames_per_outfile ) THEN
     CALL close_dataset ( grid%oid , config_flags , "DATASET=HISTORY" )
     grid%nframes = 0
     grid%oid = 0
   ENDIF
   IF ( wrf_dm_on_monitor() ) THEN
     WRITE ( message , FMT = '("Writing output for domain ",I8)' ) grid%id
     CALL end_timing ( TRIM(message) )
   END IF
   RETURN
END SUBROUTINE med_history_out

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE med_auxhist1_out ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxhist_out( grid , 1 , config_flags )
   RETURN
END SUBROUTINE med_auxhist1_out

SUBROUTINE med_auxhist2_out ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxhist_out( grid , 2 , config_flags )
   RETURN
END SUBROUTINE med_auxhist2_out

SUBROUTINE med_auxhist3_out ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxhist_out( grid , 3 , config_flags )
   RETURN
END SUBROUTINE med_auxhist3_out

SUBROUTINE med_auxhist4_out ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxhist_out( grid , 4 , config_flags )
   RETURN
END SUBROUTINE med_auxhist4_out

SUBROUTINE med_auxhist5_out ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL med_auxhist_out( grid , 5 , config_flags )
   RETURN
END SUBROUTINE med_auxhist5_out

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE med_auxhist_out ( grid , stream, config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
   USE module_configure
   USE module_bc_time_utilities

   USE ESMF_Mod

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
   TYPE(ESMF_Time)                        :: CurrTime
   CHARACTER*80                           :: timestr

   IF ( stream .LT. 1 .OR. stream .GT. 5 ) THEN
     WRITE(message,*)'med_auxhist_out: invalid history stream ',stream
     CALL wrf_error_fatal( message )
   ENDIF
   IF (      ( stream .eq. 1 .and. grid%auxhist1_oid .eq. 0 )    &
        .or. ( stream .eq. 2 .and. grid%auxhist2_oid .eq. 0 )    &
        .or. ( stream .eq. 3 .and. grid%auxhist3_oid .eq. 0 )    &
        .or. ( stream .eq. 4 .and. grid%auxhist4_oid .eq. 0 )    &
        .or. ( stream .eq. 5 .and. grid%auxhist5_oid .eq. 0 )    &
      ) THEN
     CALL ESMF_ClockGetCurrTime( grid%domain_clock, CurrTime=CurrTime, rc=ierr )
     CALL ESMF_TimeGetString( CurrTime, timestr, rc=ierr )
     SELECT CASE( stream )
       CASE ( 1 )
         CALL construct_filename2a ( auxname , config_flags%auxhist1_outname , grid%id , 2 , timestr )
       CASE ( 2 )
         CALL construct_filename2a ( auxname , config_flags%auxhist2_outname , grid%id , 2 , timestr )
       CASE ( 3 )
         CALL construct_filename2a ( auxname , config_flags%auxhist3_outname , grid%id , 2 , timestr )
       CASE ( 4 )
         CALL construct_filename2a ( auxname , config_flags%auxhist4_outname , grid%id , 2 , timestr )
       CASE ( 5 )
         CALL construct_filename2a ( auxname , config_flags%auxhist5_outname , grid%id , 2 , timestr )
     END SELECT
     WRITE(n2,'("DATASET=AUXHIST",I1)')stream
     WRITE ( message , '("med_auxhist_out : opening ",A," for writing. ",I3)') TRIM ( auxname ), ierr
     CALL wrf_debug( 1, message )
     SELECT CASE( stream )
       CASE ( 1 )
         CALL open_w_dataset ( grid%auxhist1_oid, TRIM(auxname), grid ,  &
                               config_flags , output_aux_hist1 , n2, ierr )
       CASE ( 2 )
         CALL open_w_dataset ( grid%auxhist2_oid, TRIM(auxname), grid ,  &
                               config_flags , output_aux_hist2 , n2, ierr )
       CASE ( 3 )
         CALL open_w_dataset ( grid%auxhist3_oid, TRIM(auxname), grid ,  &
                               config_flags , output_aux_hist3 , n2, ierr )
       CASE ( 4 )
         CALL open_w_dataset ( grid%auxhist4_oid, TRIM(auxname), grid ,  &
                               config_flags , output_aux_hist4 , n2, ierr )
       CASE ( 5 )
         CALL open_w_dataset ( grid%auxhist5_oid, TRIM(auxname), grid ,  &
                               config_flags , output_aux_hist5 , n2, ierr )
     END SELECT
     IF ( ierr .NE. 0 ) THEN
       CALL wrf_message( message )
     ENDIF
   END IF
   SELECT CASE( stream )
     CASE ( 1 )
       CALL output_aux_hist1 ( grid%auxhist1_oid, grid , config_flags , ierr )
     CASE ( 2 )
       CALL output_aux_hist2 ( grid%auxhist2_oid, grid , config_flags , ierr )
     CASE ( 3 )
       CALL output_aux_hist2 ( grid%auxhist3_oid, grid , config_flags , ierr )
     CASE ( 4 )
       CALL output_aux_hist2 ( grid%auxhist4_oid, grid , config_flags , ierr )
     CASE ( 5 )
       CALL output_aux_hist2 ( grid%auxhist5_oid, grid , config_flags , ierr )
   END SELECT
   RETURN
END SUBROUTINE med_auxhist_out

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
   USE ESMF_Mod

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
   TYPE(ESMF_Time)                        :: CurrTime
   CHARACTER*80                           :: timestr

   IF ( stream .LT. 1 .OR. stream .GT. 5 ) THEN
     WRITE(message,*)'med_auxinput_in: invalid inputory stream ',stream
     CALL wrf_error_fatal( message )
   ENDIF
   IF (      ( stream .eq. 1 .and. grid%auxinput1_oid .eq. 0 )    &
        .or. ( stream .eq. 2 .and. grid%auxinput2_oid .eq. 0 )    &
        .or. ( stream .eq. 3 .and. grid%auxinput3_oid .eq. 0 )    &
        .or. ( stream .eq. 4 .and. grid%auxinput4_oid .eq. 0 )    &
        .or. ( stream .eq. 5 .and. grid%auxinput5_oid .eq. 0 )    &
      ) THEN
     CALL ESMF_ClockGetCurrTime( grid%domain_clock, CurrTime=CurrTime, rc=ierr )
     CALL ESMF_TimeGetString( CurrTime, timestr, rc=ierr )
     SELECT CASE( stream )
       CASE ( 1 )
         CALL construct_filename2a ( auxname , config_flags%auxinput1_inname, grid%id , 2 , timestr )
       CASE ( 2 )
         CALL construct_filename2a ( auxname , config_flags%auxinput2_inname , grid%id , 2 , timestr )
       CASE ( 3 )
         CALL construct_filename2a ( auxname , config_flags%auxinput3_inname , grid%id , 2 , timestr )
       CASE ( 4 )
         CALL construct_filename2a ( auxname , config_flags%auxinput4_inname , grid%id , 2 , timestr )
       CASE ( 5 )
         CALL construct_filename2a ( auxname , config_flags%auxinput5_inname , grid%id , 2 , timestr )
     END SELECT
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
  ! Model layer
   USE module_configure
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
   TYPE(ESMF_Time)                        :: CurrTime
   CHARACTER*80                           :: timestr

   IF ( config_flags%write_input ) THEN

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

     CALL ESMF_ClockGetCurrTime( grid%domain_clock, CurrTime=CurrTime, rc=ierr )
     CALL ESMF_TimeGetString( CurrTime, timestr, rc=ierr )
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
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities
   USE esmf_mod

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
   Type (ESMF_Time )                      :: time, btime
   Type (ESMF_Time )                      :: current_time

#include <wrf_io_flags.h>

   CALL wrf_debug ( 1 , 'in med_latbound_in' )

   IF ( grid%id .EQ. 1 .AND. config_flags%specified ) THEN

     IF ( ( lbc_read_time( grid%current_time ) ) .AND. &
          ( grid%current_time + grid%step_time .GE. grid%stop_time ) .AND. &
          ( grid%current_time .NE. grid%start_time ) ) THEN
       CALL wrf_debug( 100 , 'med_latbound_in: Skipping attempt to read lateral boundary file during last time step ' )

     ELSE IF ( ESMF_AlarmIsRinging( grid%alarms( BOUNDARY_ALARM ), rc=rc ) ) THEN
       CALL wrf_debug ( 1 , 'in med_latbound_in preparing to read' )
       CALL ESMF_AlarmTurnOff( grid%alarms( BOUNDARY_ALARM ), rc=rc )
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

       CALL ESMF_ClockGetCurrTime( grid%domain_clock, current_time, rc=rc)
       DO WHILE (current_time .GE. grid%next_bdy_time )         ! next_bdy_time is set by input_boundary from bdy file
         CALL wrf_debug( 100 , 'med_latbound_in: calling input_boundary ' )
         CALL input_boundary ( grid%lbc_fid, grid , config_flags , ierr )
       ENDDO
       CALL ESMF_AlarmSet( grid%alarms( BOUNDARY_ALARM ), RingTime=grid%next_bdy_time, rc=rc )

       IF ( ierr .NE. 0 .and. ierr .NE. WRF_WARN_NETCDF ) THEN
         WRITE( message, * ) 'med_latbound_in: error reading ',TRIM(bdyname), ' IERR = ',ierr
         CALL WRF_ERROR_FATAL( message )
       ENDIF
       IF ( grid%current_time .EQ. grid%this_bdy_time ) grid%dtbc = 0.
  
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
  ! Model layer
   USE module_configure

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
 
