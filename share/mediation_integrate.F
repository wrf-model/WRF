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
   USE WRF_ESMF_MOD

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                                    :: rc

   IF( ESMF_AlarmIsRinging( grid%alarms( HISTORY_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 0, config_flags )
     CALL ESMF_AlarmRingerOff( grid%alarms( HISTORY_ALARM ), rc=rc )
   ENDIF

   IF( ESMF_AlarmIsRinging( grid%alarms( INPUTOUT_ALARM ), rc=rc ) ) THEN
     CALL med_filter_out  ( grid , config_flags )
     CALL ESMF_AlarmRingerOff( grid%alarms( INPUTOUT_ALARM ), rc=rc )
   ENDIF

! - AUX HISTORY OUTPUT 
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST1_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 1, config_flags )
     CALL ESMF_AlarmRingerOff( grid%alarms( AUXHIST1_ALARM ), rc=rc )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST2_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 2, config_flags )
     CALL ESMF_AlarmRingerOff( grid%alarms( AUXHIST2_ALARM ), rc=rc )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST3_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 3,  config_flags )
     CALL ESMF_AlarmRingerOff( grid%alarms( AUXHIST3_ALARM ), rc=rc )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST4_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 4, config_flags )
     CALL ESMF_AlarmRingerOff( grid%alarms( AUXHIST4_ALARM ), rc=rc )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST5_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 5, config_flags )
     CALL ESMF_AlarmRingerOff( grid%alarms( AUXHIST5_ALARM ), rc=rc )
   ENDIF

! - AUX INPUT INPUT
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXINPUT1_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput1_in ( grid , config_flags )
     CALL ESMF_AlarmRingerOff( grid%alarms( AUXINPUT1_ALARM ), rc=rc )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXINPUT2_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput2_in ( grid , config_flags )
     CALL ESMF_AlarmRingerOff( grid%alarms( AUXINPUT2_ALARM ), rc=rc )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXINPUT3_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput3_in ( grid , config_flags )
     CALL ESMF_AlarmRingerOff( grid%alarms( AUXINPUT3_ALARM ), rc=rc )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXINPUT4_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput4_in ( grid , config_flags )
     CALL ESMF_AlarmRingerOff( grid%alarms( AUXINPUT4_ALARM ), rc=rc )
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
     CALL ESMF_AlarmRingerOff( grid%alarms( AUXINPUT5_ALARM ), rc=rc )
    print *,' back from CALL med_read_wrf_chem_emiss '

   ENDIF
! end for wrf chem emiss input
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ELSE
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXINPUT5_ALARM ), rc=rc ) ) THEN
     CALL med_auxinput5_in ( grid , config_flags )
     CALL ESMF_AlarmRingerOff( grid%alarms( AUXINPUT5_ALARM ), rc=rc )
   ENDIF
  ENDIF
#endif

! - RESTART OUTPUT
   IF( ESMF_AlarmIsRinging( grid%alarms( RESTART_ALARM ), rc=rc ) ) THEN
     CALL med_restart_out ( grid , config_flags )
     CALL ESMF_AlarmRingerOff( grid%alarms( RESTART_ALARM ), rc=rc )
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

     IF ( nest_config_flags%input_from_file .OR. nest_config_flags%input_from_hires ) THEN

        WRITE(message,FMT='(A,I2,A)') '*** Initializing nest domain #',nest%id,&
                                      ' from an input file. ***'
        CALL wrf_debug ( 0 , message )
       IF ( nest_config_flags%input_from_file ) THEN

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

       ELSE IF ( nest_config_flags%input_from_hires ) THEN
! read in high res topography
          CALL  input_terrain_rsmas ( nest,                               &
                                      ids , ide , jds , jde , 1   , 1   , &
                                      ims , ime , jms , jme , 1   , 1   , &
                                      ips , ipe , jps , jpe , 1   , 1   )
       ENDIF


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

     CALL ESMF_ClockGet( nest%domain_clock, CurrTime=CurrTime, rc=ierr )
     CALL wrf_timetoa ( CurrTime, timestr )
     CALL construct_filename2a ( rstname , config_flags%rst_inname , nest%id , 2 , timestr )

     WRITE(message,*)'RESTART: nest, opening ',TRIM(rstname),' for reading'
     CALL wrf_message ( message )
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
   LOGICAL input_from_hires

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
     LOGICAL FUNCTION time_for_move ( parent , nest , dx , dy )
        USE module_domain
        USE WRF_ESMF_MOD
        TYPE(domain) , POINTER    :: parent , nest
        INTEGER, INTENT(OUT)      :: dx , dy
     END FUNCTION time_for_move
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

write(0,*)'in med_nest_move nest%id ',nest%id

   check_for_move: IF ( time_for_move ( parent , nest , dx, dy ) ) THEN

     IF ( (dx .gt. 1 .and. dx .lt. -1 ) .or.  &
          (dy .gt. 1 .and. dy .lt. -1 ) ) THEN
       CALL wrf_error_fatal( 'med_nest_move' )
     ENDIF

write(0,*)' moving ',grid%id,dx,dy

     CALL get_ijk_from_grid (  grid ,                   &
                               ids, ide, jds, jde, kds, kde,    &
                               ims, ime, jms, jme, kms, kme,    &
                               ips, ipe, jps, jpe, kps, kpe    )

#ifdef RSL
! tell RSL about the move; this needs to be moved into external directory
     CALL rsl_move_nest ( parent%domdesc, nest%intermediate_grid%domdesc, dx, dy )
#endif
     CALL adjust_domain_dims_for_move( nest%intermediate_grid , dx, dy )

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

     CALL nl_get_input_from_hires( nest%id , input_from_hires ) 
     IF ( input_from_hires ) THEN
       CALL  input_terrain_rsmas ( nest,                               &
                                   ids , ide , jds , jde , 1   , 1   , &
                                   ims , ime , jms , jme , 1   , 1   , &
                                   ips , ipe , jps , jpe , 1   , 1   )
     ENDIF

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

LOGICAL FUNCTION time_for_move2 ( parent , grid , move_cd_x, move_cd_y )
  ! Driver layer
   USE module_domain
   USE module_configure
   USE module_compute_geop
   USE module_dm
   USE WRF_ESMF_MOD
   IMPLICIT NONE
! Arguments
   TYPE(domain) , POINTER    :: parent, grid
   INTEGER, INTENT(OUT)      :: move_cd_x , move_cd_y
! Local
   INTEGER  num_moves, rc
   INTEGER  move_interval , move_id
   TYPE(ESMF_Time) :: ct, st
   TYPE(ESMF_TimeInterval) :: ti
   CHARACTER*256 mess
   INTEGER     :: ids, ide, jds, jde, kds, kde, &
                  ims, ime, jms, jme, kms, kme, &
                  ips, ipe, jps, jpe, kps, kpe
   INTEGER :: is, ie, js, je, ierr
   REAL    :: ipbar, pbar, jpbar, fact
   REAL    :: last_vc_i , last_vc_j

   REAL, ALLOCATABLE, DIMENSION(:,:) :: height_l, height
   REAL :: minh, maxh
   INTEGER :: mini, minj, maxi, maxj, i, j, pgr
   REAL :: disp_x, disp_y, lag, radius, center_i, center_j, dx
   REAL :: dijsmooth, vmax, vmin, a, b
   REAL :: dc_i, dc_j   ! domain center

   INTEGER :: fje, fjs, fie, fis, fimloc, fjmloc, imloc, jmloc
   INTEGER :: max_vortex_speed, vortex_interval  ! meters per second and seconds
   REAL    :: rsmooth = 100.  ! kilometers?

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

character*256 message, message2

integer myproc

!#define VORTEX_CENTER
#define MOVING_DIAGS
#ifdef VORTEX_CENTER


   CALL get_ijk_from_grid (  grid ,                        &
                             ids, ide, jds, jde, kds, kde, &
                             ims, ime, jms, jme, kms, kme, &
                             ips, ipe, jps, jpe, kps, kpe  )

! If the alarm is ringing, recompute the Vortex Center (VC); otherwise
! use the previous position of VC.  VC is not recomputed ever step to
! save on cost for global collection of height field and broadcast
! of new center.

#ifdef MOVING_DIAGS
write(0,*)'Check to see if COMPUTE_VORTEX_CENTER_ALARM is ringing? '
#endif
   CALL nl_get_parent_grid_ratio ( grid%id , pgr )
   CALL nl_get_dx ( grid%id , dx )

   IF ( ESMF_AlarmIsRinging( grid%alarms( COMPUTE_VORTEX_CENTER_ALARM ), rc=rc ) ) THEN

#ifdef MOVING_DIAGS
write(0,*)'COMPUTE_VORTEX_CENTER_ALARM is ringing  '
#endif
     CALL ESMF_AlarmRingerOff( grid%alarms( COMPUTE_VORTEX_CENTER_ALARM ), rc=rc )

     last_vc_i = grid%vc_i
     last_vc_j = grid%vc_j

     ALLOCATE ( height_l ( ims:ime , jms:jme ), STAT=ierr )
     IF ( ierr .ne. 0 ) CALL wrf_error_fatal ('allocating height_l in time_for_move2')
     IF ( wrf_dm_on_monitor() ) THEN
       ALLOCATE ( height   ( ids:ide , jds:jde ), STAT=ierr )
       IF ( ierr .ne. 0 ) CALL wrf_error_fatal ('allocating height in time_for_move2')
     ELSE
       ALLOCATE ( height   ( 1:1 , 1:1 ), STAT=ierr )
     ENDIF

     CALL compute_500mb_height ( grid%em_ph_2 , grid%em_phb, grid%em_p, grid%em_pb, height_l , &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe  )

     CALL wrf_patch_to_global_real ( height_l , height , grid%domdesc, "z", "xy", &
                                     ids, ide-1 , jds , jde-1 , 1 , 1 , &
                                     ims, ime   , jms , jme   , 1 , 1 , &
                                     ips, ipe   , jps , jpe   , 1 , 1   )

     monitor_only : IF ( wrf_dm_on_monitor() ) THEN



!
! This vortex center finding code adapted from the Hurricane version of MM5,
! Courtesy:
!
!   Shuyi Chen et al., Rosenstiel School of Marine and Atmos. Sci., U. Miami.
!   Spring, 2005
!
! Get the first guess vortex center about which we do our search
! as mini and minh; minimum value is minh
!

       CALL nl_get_vortex_interval( grid%id , vortex_interval )
       CALL nl_get_max_vortex_speed( grid%id , max_vortex_speed )

       IF ( grid%vc_i < 0. .AND. grid%vc_j < 0. ) THEN
          ! first time through
          is = ids
          ie = ide-1
          js = jds
          je = jde-1
       ELSE
          ! limit the search to an area around the vortex
          ! that is limited by 40 meters per second from 
          ! previous location  (note hard-coded 15 mins)
          is = max( grid%vc_i - vortex_interval * max_vortex_speed / dx , 1.0 * ids )
          js = max( grid%vc_j - vortex_interval * max_vortex_speed / dx , 1.0 * jds )
          ie = min( grid%vc_i + vortex_interval * max_vortex_speed / dx , 1.0 * (ide-1) )
          je = min( grid%vc_j + vortex_interval * max_vortex_speed / dx , 1.0 * (jde-1) )
       ENDIF

#ifdef MOVING_DIAGS
write(0,*)'search set around last position '
write(0,*)'   is, ids-1,  ie,  ide-1 ', is, ids-1, ie, ide-1
write(0,*)'   js, jds-1,  je,  jde-1 ', js, jds-1, je, jde-1
#endif

       ! find local min, max
       vmin =  1000000.0
       vmax = -1000000.0
       DO j = js, je
       DO i = is, ie
         IF ( height(i,j) .LT. vmin ) THEN
           vmin = height(i,j)
           imloc = i
           jmloc = j
         ENDIF
         IF ( height(i,j) .GT. vmax ) THEN
           vmax = height(i,j)
           maxi = i
           maxj = j
         ENDIF
       ENDDO
       ENDDO

       fimloc = imloc
       fjmloc = jmloc

       if ( grid%xi .EQ. -1. ) grid%xi = fimloc
       if ( grid%xj .EQ. -1. ) grid%xj = fjmloc

       dijsmooth = rsmooth / dx

       fjs = max(fjmloc-dijsmooth,1.0)
       fje = min(fjmloc+dijsmooth,jde-2.0)
       fis = max(fimloc-dijsmooth,1.0)
       fie = min(fimloc+dijsmooth,ide-2.0)
       js = fjs
       je = fje
       is = fis
       ie = fie

       vmin =  1000000.0
       vmax = -1000000.0
       DO j = js, je
       DO i = is, ie
         IF ( height(i,j) .GT. vmax ) THEN
           vmax = height(i,j)
         ENDIF
       ENDDO
       ENDDO

       pbar  = 0.0
       ipbar = 0.0
       jpbar = 0.0

       do j=js,je
          do i=is,ie
             fact = vmax - height(i,j)
             pbar  = pbar + fact
             ipbar = ipbar + fact*(i-is)
             jpbar = jpbar + fact*(j-js)
          enddo
       enddo

!     Compute an adjusted, smoothed, vortex center location in cross
!     point index space.
!     Time average. A is coef for old information; B is new
       a = 0.0
       b = 1.0
       grid%xi =  (a * grid%xi + b * (is + ipbar / pbar))  / ( a + b )
       grid%xj =  (a * grid%xj + b * (js + jpbar / pbar))  / ( a + b )

       grid%vc_i = grid%xi + .5
       grid%vc_j = grid%xj + .5

#ifdef MOVING_DIAGS
write(0,*)'computed grid%vc_i, grid%vc_j ',grid%vc_i, grid%vc_j
i = grid%vc_i ; j = grid%vc_j ; height( i,j ) = height(i,j) * 1.2   !mark the center
CALL wrf_timetoa ( grid%current_time, message2 )
WRITE ( message , FMT = '(A," on domain ",I3)' ) TRIM(message2), grid%id
write(70,*)ide-ids,jde-jds,TRIM(message)
do j = jds,jde-1
do i = ids,ide-1
write(70,*)height(i,j)
enddo
enddo
call flush_(70)
#endif


     ENDIF monitor_only

     DEALLOCATE ( height )
     DEALLOCATE ( height_l )

     CALL wrf_dm_bcast_real( grid%vc_i , 1 )
     CALL wrf_dm_bcast_real( grid%vc_j , 1 )

#ifdef MOVING_DIAGS
write(0,*)'after bcast : grid%vc_i, grid%vc_j ',grid%vc_i, grid%vc_j
#endif


   ENDIF   ! COMPUTE_VORTEX_CENTER_ALARM ringing

#ifdef MOVING_DIAGS
write(0,*)'After ENDIF : grid%vc_i, grid%vc_j ',grid%vc_i, grid%vc_j
#endif

   dc_i = (ide-ids+1)/2.    ! domain center
   dc_j = (jde-jds+1)/2.

   disp_x = grid%vc_i - dc_i * 1.0
   disp_y = grid%vc_j - dc_j * 1.0

   radius = 2 * pgr

   if ( disp_x .GT. 0 ) disp_x = min( disp_x , radius )
   if ( disp_y .GT. 0 ) disp_y = min( disp_y , radius )

   if ( disp_x .LT. 0 ) disp_x = max( disp_x , -radius )
   if ( disp_y .LT. 0 ) disp_y = max( disp_y , -radius )

   move_cd_x = int ( disp_x  / pgr )
   move_cd_y = int ( disp_y  / pgr )

   IF ( move_cd_x .GT. 0 ) move_cd_x = min ( move_cd_x , 1 )
   IF ( move_cd_y .GT. 0 ) move_cd_y = min ( move_cd_y , 1 )
   IF ( move_cd_x .LT. 0 ) move_cd_x = max ( move_cd_x , -1 )
   IF ( move_cd_y .LT. 0 ) move_cd_y = max ( move_cd_y , -1 )

#ifdef MOVING_DIAGS
write(0,*)'vortex center : ',grid%vc_i, grid%vc_j
write(0,*)'grid   center : ',     dc_i,      dc_j
write(0,*)'disp          : ',   disp_x,    disp_y
write(0,*)'move (rel cd) : ',move_cd_x, move_cd_y
#endif

   grid%vc_i = grid%vc_i - move_cd_x * pgr
   grid%vc_j = grid%vc_j - move_cd_y * pgr

#ifdef MOVING_DIAGS
write(0,*)' changing grid%vc_i,  move_cd_x * pgr ', grid%vc_i, move_cd_x * pgr, move_cd_x, pgr
#endif

   IF ( ( move_cd_x .NE. 0 ) .OR. ( move_cd_y .NE. 0 ) ) THEN
     time_for_move2 = .TRUE.
   ELSE
     time_for_move2 = .FALSE.
   ENDIF

#else
! from namelist
   time_for_move2 = .FALSE.
   CALL ESMF_ClockGet( grid%domain_clock, CurrTime=ct, StartTime=st, rc=rc )
   CALL nl_get_num_moves( 1, num_moves )
   IF ( num_moves .GT. max_moves ) THEN
     WRITE(mess,*)'time_for_moves2: num_moves (',num_moves,') .GT. max_moves (',max_moves,')'
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
           time_for_move2 = .TRUE.
           EXIT
         ENDIF
       ENDIF
     ENDIF
   ENDDO
#endif
   RETURN
END FUNCTION time_for_move2

LOGICAL FUNCTION time_for_move ( parent , grid , move_cd_x, move_cd_y )
   USE module_domain
   USE module_configure
USE module_timing
   USE WRF_ESMF_MOD
   IMPLICIT NONE
! arguments
   TYPE(domain) , POINTER    :: parent, grid
   INTEGER, INTENT(OUT)      :: move_cd_x , move_cd_y
! local
   INTEGER     :: coral_dist, kid
   INTEGER     :: dw, de, ds, dn, pgr
   INTEGER     :: cids, cide, cjds, cjde, ckds, ckde, &
                  cims, cime, cjms, cjme, ckms, ckme, &
                  cips, cipe, cjps, cjpe, ckps, ckpe, &
                  nids, nide, njds, njde, nkds, nkde, &
                  nims, nime, njms, njme, nkms, nkme, &
                  nips, nipe, njps, njpe, nkps, nkpe
! interface
   INTERFACE
     LOGICAL FUNCTION time_for_move2 ( parent , nest , dx , dy )
        USE module_domain
        USE WRF_ESMF_MOD
        TYPE(domain) , POINTER    :: parent , nest
        INTEGER, INTENT(OUT)      :: dx , dy
     END FUNCTION time_for_move2
   END INTERFACE
! executable
! 
! Simplifying assumption: domains in moving nest simulations have only 
! one parent and only one child.
   IF   ( grid%num_nests .GT. 1 ) THEN
     CALL wrf_error_fatal ( 'domains in moving nest simulations can have only 1 nest' )
   ENDIF
   kid = 1
!
! find out if this is the innermost nest (will not have kids)
   IF   ( grid%num_nests .EQ. 0 ) THEN
     ! code that executes on innermost nest
     time_for_move = time_for_move2 ( parent , grid , move_cd_x, move_cd_y )
   ELSE
     ! code that executes on parent to see if parent needs to move
     ! get closest number of cells we'll allow nest edge to approach parent bdy
     CALL nl_get_coral_dist ( grid%id , coral_dist )
     ! get dims
     CALL get_ijk_from_grid (  grid%nests(kid)%ptr ,               &
                               nids, nide, njds, njde, nkds, nkde, &
                               nims, nime, njms, njme, nkms, nkme, &
                               nips, nipe, njps, njpe, nkps, nkpe  )
     CALL get_ijk_from_grid (  grid ,                              &
                               cids, cide, cjds, cjde, ckds, ckde, &
                               cims, cime, cjms, cjme, ckms, ckme, &
                               cips, cipe, cjps, cjpe, ckps, ckpe  )
     CALL nl_get_parent_grid_ratio ( grid%nests(kid)%ptr%id , pgr )
     ! perform measurements...
     !  from western boundary
     dw = grid%nests(kid)%ptr%i_parent_start - 1
     !  from southern boundary
     ds = grid%nests(kid)%ptr%j_parent_start - 1
     !  from eastern boundary
     de = cide - ( grid%nests(kid)%ptr%i_parent_start + (nide-nids+1)/pgr )
write(0,*)' de = ',de,cide,( grid%nests(kid)%ptr%i_parent_start + (nide-nids+1)/pgr )
write(0,*)'   i_parent_start = ',grid%nests(kid)%ptr%i_parent_start
     !  from northern boundary
     dn = cjde - ( grid%nests(kid)%ptr%j_parent_start + (njde-njds+1)/pgr )
write(0,*)' dn = ',dn,cjde,( grid%nests(kid)%ptr%j_parent_start + (njde-njds+1)/pgr )
write(0,*)'   j_parent_start = ',grid%nests(kid)%ptr%j_parent_start

     ! move this domain (the parent containing the moving nest)
     ! in a direction that reestablishes the distance from 
     ! the boundary.
     move_cd_x = 0
     move_cd_y = 0
     if ( dw .LE. coral_dist ) move_cd_x = move_cd_x - 1
     if ( de .LE. coral_dist ) move_cd_x = move_cd_x + 1
     if ( ds .LE. coral_dist ) move_cd_y = move_cd_y - 1
     if ( dn .LE. coral_dist ) move_cd_y = move_cd_y + 1

     time_for_move = ( move_cd_x .NE. 0 ) .OR. ( move_cd_y .NE. 0 )

     IF ( time_for_move ) THEN
       IF ( grid%id .EQ. 1 ) THEN
         CALL wrf_message( 'DANGER: Nest has moved too close to boundary of outermost domain.' )
         time_for_move = .FALSE.
       ELSE
         ! need to adjust the intermediate domain of the nest in relation to this
         ! domain since we're moving

#ifdef RSL
! tell RSL about the move; this needs to be moved into external directory
         CALL start_timing
         write(0,*)'moving the nest backwards in this domain', -move_cd_x*pgr, -move_cd_y*pgr
         CALL rsl_move_nest ( grid%domdesc, grid%nests(kid)%ptr%intermediate_grid%domdesc, -move_cd_x*pgr, -move_cd_y*pgr )
#endif
         write(0,*)'calling adjust_domain_dims_for_move '
         CALL adjust_domain_dims_for_move( grid%nests(kid)%ptr%intermediate_grid , -move_cd_x*pgr, -move_cd_y*pgr )
         CALL end_timing('moving the thing')
         grid%nests(kid)%ptr%i_parent_start = grid%nests(kid)%ptr%i_parent_start - move_cd_x*pgr
         CALL nl_set_i_parent_start( grid%nests(kid)%ptr%id, grid%nests(kid)%ptr%i_parent_start )
         grid%nests(kid)%ptr%j_parent_start = grid%nests(kid)%ptr%j_parent_start - move_cd_y*pgr
         CALL nl_set_j_parent_start( grid%nests(kid)%ptr%id, grid%nests(kid)%ptr%j_parent_start )

       ENDIF
     ENDIF 

   ENDIF

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
! see <a href="www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm">here</a>
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
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
   USE WRF_ESMF_MOD

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
   USE WRF_ESMF_MOD
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
     CALL med_hist_out ( grid , 0 , config_flags )
   ENDIF

   IF( ESMF_AlarmIsRinging( grid%alarms( INPUTOUT_ALARM ), rc=rc ) ) THEN
     CALL med_filter_out  ( grid , config_flags )
   ENDIF

   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST1_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 1 , config_flags )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST2_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 2 , config_flags )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST3_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 3 , config_flags )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST4_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 4 , config_flags )
   ENDIF
   IF( ESMF_AlarmIsRinging( grid%alarms( AUXHIST5_ALARM ), rc=rc ) ) THEN
     CALL med_hist_out ( grid , 5 , config_flags )
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
   USE WRF_ESMF_MOD

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

   CALL ESMF_ClockGet( grid%domain_clock, CurrTime=CurrTime, rc=ierr )
   CALL wrf_timetoa ( CurrTime, timestr )
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE med_hist_out ( grid , stream, config_flags )
  ! Driver layer
   USE module_domain
   USE module_timing
   USE module_io_domain
   USE module_configure
   USE module_bc_time_utilities
   USE WRF_ESMF_MOD

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
   TYPE(ESMF_Time)                        :: CurrTime
   CHARACTER*80                           :: timestr

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

   IF ( stream .LT. 0 .OR. stream .GT. 5 ) THEN
     WRITE(message,*)'med_hist_out: invalid history stream ',stream
     CALL wrf_error_fatal( message )
   ENDIF
   CALL ESMF_ClockGet( grid%domain_clock, CurrTime=CurrTime, rc=ierr )
   CALL wrf_timetoa ( CurrTime, timestr )
   SELECT CASE( stream )
     CASE ( 0 )
       CALL construct_filename2a ( fname , config_flags%history_outname , grid%id , 2 , timestr )
     CASE ( 1 )
       CALL construct_filename2a ( fname , config_flags%auxhist1_outname , grid%id , 2 , timestr )
     CASE ( 2 )
       CALL construct_filename2a ( fname , config_flags%auxhist2_outname , grid%id , 2 , timestr )
     CASE ( 3 )
       CALL construct_filename2a ( fname , config_flags%auxhist3_outname , grid%id , 2 , timestr )
     CASE ( 4 )
       CALL construct_filename2a ( fname , config_flags%auxhist4_outname , grid%id , 2 , timestr )
     CASE ( 5 )
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
   USE WRF_ESMF_MOD

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
   CALL ESMF_ClockGet( grid%domain_clock, CurrTime=CurrTime, rc=ierr )
   CALL wrf_timetoa ( CurrTime, timestr )
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

     CALL ESMF_ClockGet( grid%domain_clock, CurrTime=CurrTime, rc=ierr )
     CALL wrf_timetoa ( CurrTime, timestr )
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
   USE WRF_ESMF_MOD

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

   IF ( grid%id .EQ. 1 .AND. config_flags%specified .AND. config_flags%io_form_boundary .GT. 0 ) THEN

     IF ( ( lbc_read_time( grid%current_time ) ) .AND. &
          ( grid%current_time + grid%step_time .GE. grid%stop_time ) .AND. &
          ( grid%current_time .NE. grid%start_time ) ) THEN
       CALL wrf_debug( 100 , 'med_latbound_in: Skipping attempt to read lateral boundary file during last time step ' )

     ELSE IF ( ESMF_AlarmIsRinging( grid%alarms( BOUNDARY_ALARM ), rc=rc ) ) THEN
       CALL wrf_debug ( 1 , 'in med_latbound_in preparing to read' )
       CALL ESMF_AlarmRingerOff( grid%alarms( BOUNDARY_ALARM ), rc=rc )
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

       CALL ESMF_ClockGet( grid%domain_clock, CurrTime=current_time, rc=rc)
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
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE med_read_wrf_chem_emiss ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
   USE module_timing
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities
#ifdef DM_PARALLEL
   USE module_dm
#endif
   USE module_date_time
   USE WRF_ESMF_MOD

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid

!  TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   TYPE (grid_config_rec_type)                :: config_flags
   Type (ESMF_Time )                          :: start_time, stop_time, current_time
   TYPE(ESMF_TimeInterval)                    :: time_interval

  ! Local data
   LOGICAL, EXTERNAL                      :: wrf_dm_on_monitor

   INTEGER                                :: ierr, efid
   REAL                                   :: time, tupdate
   real, allocatable :: dumc0(:,:,:)
   CHARACTER (LEN=256)                    :: message, current_date_char, date_string
   CHARACTER (LEN=80)                     :: inpname

#include <wrf_io_flags.h>

     CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

    IF ( grid%id .EQ. 1 ) THEN

      CALL ESMF_ClockGet( grid%domain_clock, CurrTime=grid%current_time, rc=ierr )
      CALL wrf_timetoa ( grid%current_time, current_date_char )

      time = float(grid%itimestep) * grid%dt
      tupdate = mod( time, (12. * 3600.) )
      IF( grid%current_time + grid%step_time .GE. grid%stop_time .AND.                  &
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
!      print *, grid%current_time + grid%step_time, grid%stop_time
! hourly updates to emissions
      IF ( ( mod( time, 3600. ) .EQ. 0   ) .AND. &
           ( grid%current_time + grid%step_time .LT. grid%stop_time ) ) THEN
!        IF ( wrf_dm_on_monitor() ) CALL start_timing

         WRITE(message,*)'mediation_integrate: med_read_wrf_chem_emissions: Read emissions for time ',TRIM(current_date_char)
         CALL wrf_message( TRIM(message) )

         CALL wrf_debug (100 , 'mediation_integrate: calling input_aux_model_input5' )
         CALL input_aux_model_input5 ( grid%auxinput5_oid, grid , config_flags , ierr )
      ELSE
         CALL wrf_debug (100 , 'mediation_integrate: med_read_wrf_chem_emissions: Do not read emissions' )
      ENDIF

!     ! Close the data file at 11z and 23z
!     IF ( ( time - 11. * 3600. ) .GE. 0.   ) THEN
!        CALL close_dataset ( grid%auxinput5_oid , config_flags , "DATASET=AUXINPUT5" )
!     ENDIF

   ENDIF
   CALL wrf_debug (100 , 'mediation_integrate: med_read_wrf_chem_emissions: exit' )

END SUBROUTINE med_read_wrf_chem_emiss

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE med_read_wrf_chem_bioemiss ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
   USE module_timing
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities
#ifdef DM_PARALLEL
   USE module_dm
#endif
   USE module_date_time
   USE WRF_ESMF_MOD

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid

   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   Type (ESMF_Time )                          :: start_time, stop_time, current_time
   TYPE(ESMF_TimeInterval)                    :: time_interval

  ! Local data
   LOGICAL, EXTERNAL                      :: wrf_dm_on_monitor

   INTEGER                                :: ierr, efid
   REAL                                   :: time, tupdate
   real, allocatable :: dumc0(:,:,:)
   CHARACTER (LEN=256)                    :: message, current_date_char, date_string
   CHARACTER (LEN=80)                     :: inpname

#include <wrf_io_flags.h>
    IF ( grid%id .EQ. 1 ) THEN

      CALL ESMF_ClockGet( grid%domain_clock, CurrTime=grid%current_time, rc=ierr )
      CALL wrf_timetoa ( grid%current_time, current_date_char )

!     IF( grid%current_time + grid%step_time .GE. grid%stop_time ) then
!       return
!     ENDIF

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
