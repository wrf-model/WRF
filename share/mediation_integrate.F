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

     SUBROUTINE start_domain ( grid )
        USE module_domain
        TYPE(domain) :: grid
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
     nest%imask = 1
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
    END IF

! feedback, mostly for this new terrain, but it is the safe thing to do
    CALL med_nest_feedback ( parent , nest , config_flags )

! set some other initial fields, fill out halos, base fields; re-do parent due
! to new terrain elevation from feedback
    CALL start_domain ( nest )
    CALL start_domain ( parent )

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

    CALL start_domain ( nest )

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

SUBROUTINE med_nest_move ( parent, nest, dx, dy, config_flags )
  ! Driver layer
   USE module_domain
   USE module_timing
   USE module_configure
   USE module_io_domain
   TYPE(domain) , POINTER                     :: parent, nest, grid
   TYPE (grid_config_rec_type)                :: config_flags
   INTEGER dx, dy       ! number of parent domain points to move
  ! Local 
   INTEGER i, j, parent_grid_ratio
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
     SUBROUTINE shift_domain_em ( grid, disp_x, disp_y, config_flags, &
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
        TYPE (grid_config_rec_type)                :: config_flags
#include <em_dummy_decl.inc>
     END SUBROUTINE shift_domain_em
   END INTERFACE

#ifdef DEREF_KLUDGE
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif

  grid => nest
#include "deref_kludge.h"

! mask should be defined in nest domain

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
#endif

   CALL get_ijk_from_grid (  grid ,                   &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )

   grid => nest 

   CALL shift_domain_em( grid, dx, dy, config_flags,  &
!
# include <em_actual_args.inc>
!
                       )

   CALL med_interp_domain( parent, nest )

   px = isign(grid%parent_grid_ratio,dx)
   py = isign(grid%parent_grid_ratio,dy)
   grid%i_parent_start = grid%i_parent_start + px / grid%parent_grid_ratio 
   CALL set_i_parent_start( grid%id, grid%i_parent_start )
   grid%j_parent_start = grid%j_parent_start + py / grid%parent_grid_ratio
   CALL set_j_parent_start( grid%id, grid%j_parent_start )

   CALL start_domain ( nest )

#endif

END SUBROUTINE med_nest_move

#if ( EM_CORE == 1 && defined(DM_PARALLEL) )

SUBROUTINE shift_domain_em ( grid , disp_x, disp_y, config_flags, &
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
   INTEGER disp_x, disp_y       ! number of parent domain points to move
  ! Local 
integer myproc
   INTEGER i, j
   INTEGER px, py       ! number and direction of nd points to move
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe
   TYPE(domain) , POINTER                     :: grid
   TYPE (grid_config_rec_type)                :: config_flags


   !  Definitions of dummy arguments to solve
#include <em_dummy_decl.inc>

#define COPY_IN
#include <em_scalar_derefs.inc>
#ifdef DM_PARALLEL
#      include <em_data_calls.inc>
#endif

   CALL get_ijk_from_grid (  grid ,                   &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )

   px = isign(parent_grid_ratio,disp_x)
   py = isign(parent_grid_ratio,disp_y)

   imask = 1
   imask(ips:ipe,jps:jpe) = 0

#if 1

! shift the nest domain in x
   do i = 1,abs(disp_x)
#include <em_shift_halo_x.inc>
   enddo

! shift the nest domain in y
   do i = 1,abs(disp_y)
#include <em_shift_halo_y.inc>
   enddo

#endif

#define COPY_OUT
#include <em_scalar_derefs.inc>

END SUBROUTINE shift_domain_em
#endif

SUBROUTINE med_nest_force ( parent , nest , config_flags )
  ! Driver layer
   USE module_domain
   USE module_timing
  ! Model layer
   USE module_configure

   IMPLICIT NONE

  ! Arguments
   TYPE(domain) , POINTER                     :: parent, nest
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                                    :: idum1 , idum2 , fid

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

   CALL med_force_domain( parent, nest )

! might also have calls here to do input from a file into the nest

   RETURN
END SUBROUTINE med_nest_force

SUBROUTINE med_nest_feedback ( parent , nest , config_flags )
  ! Driver layer
   USE module_domain
   USE module_timing
  ! Model layer
   USE module_configure

   IMPLICIT NONE

  ! Arguments
   TYPE(domain) , POINTER                     :: parent, nest
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                                    :: idum1 , idum2 , fid

   INTERFACE
     SUBROUTINE med_feedback_domain ( parent , nest )
        USE module_domain
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_feedback_domain
   END INTERFACE

! feedback nest with interpolated data from the parent
    IF ( config_flags%feedback .NE. 0 ) THEN
      CALL med_feedback_domain( parent, nest )
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
   grid%write_metadata = .false.
   CALL open_w_dataset ( rid, TRIM(rstname), grid , &
                         config_flags , output_restart , "DATASET=RESTART", ierr )

   IF ( ierr .NE. 0 ) THEN
     CALL WRF_message( message )
   ENDIF
   grid%write_metadata = .true.
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

     grid%write_metadata = .false.
     CALL open_w_dataset ( grid%oid, TRIM(outname), grid ,  &
                           config_flags , output_history , "DATASET=HISTORY", ierr )
     IF ( ierr .NE. 0 ) THEN
       CALL wrf_message( message )
     ENDIF
     grid%write_metadata = .true.
   ELSE
     grid%write_metadata = .false.
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

SUBROUTINE med_auxhist_out ( grid , stream, config_flags )
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
     grid%write_metadata = .false.
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
     grid%write_metadata = .true.
   ELSE
     grid%write_metadata = .false.
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

  ! Local
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

     grid%write_metadata = .false.
     CALL open_w_dataset ( fid, TRIM(outname), grid ,  &
                           config_flags , output_model_input , "DATASET=INPUT", ierr )
     IF ( ierr .NE. 0 ) THEN
       CALL wrf_error_fatal( message )
     ENDIF

     IF ( ierr .NE. 0 ) THEN
       CALL wrf_error_fatal( message )
     ENDIF
     grid%write_metadata = .true.

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

   IF ( grid%id .EQ. 1 .AND. config_flags%specified ) THEN

     IF ( ( lbc_read_time( grid%current_time ) ) .AND. &
          ( grid%current_time + grid%step_time .GE. grid%stop_time ) ) THEN
       CALL wrf_debug( 100 , 'med_latbound_in: Skipping attempt to read lateral boundary file during last time step ' )

     ELSE IF ( ESMF_AlarmIsRinging( grid%alarms( BOUNDARY_ALARM ), rc=rc ) ) THEN
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

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                                    :: idum1 , idum2

   CALL set_scalar_indices_from_config ( grid%id , idum1 , idum2 )

   RETURN

END SUBROUTINE med_setup_step

