!WRF:MEDIATION_LAYER:
!

SUBROUTINE med_initialdata_input_ptr ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE (domain) , POINTER :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   INTERFACE 
      SUBROUTINE med_initialdata_input ( grid , config_flags )
         USE module_domain
         USE module_configure
         TYPE (domain) :: grid
         TYPE (grid_config_rec_type) , INTENT(IN) :: config_flags
      END SUBROUTINE med_initialdata_input
   END INTERFACE
   CALL  med_initialdata_input ( grid , config_flags )
END SUBROUTINE med_initialdata_input_ptr

SUBROUTINE med_initialdata_input ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
   USE module_timing
use module_io
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities
   USE module_utility

   IMPLICIT NONE

  ! Interface 
   INTERFACE
     SUBROUTINE start_domain ( grid , allowed_to_read )  ! comes from module_start in appropriate dyn_ directory
       USE module_domain
       TYPE (domain) grid
       LOGICAL, INTENT(IN) :: allowed_to_read 
     END SUBROUTINE start_domain
   END INTERFACE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                :: fid , ierr , myproc
   CHARACTER (LEN=256)    :: inpname , rstname, timestr
   CHARACTER (LEN=80)     :: message
   LOGICAL                :: restart
   LOGICAL, EXTERNAL      :: wrf_dm_on_monitor

   CALL nl_get_restart( 1, restart )
   IF ( .NOT. restart ) THEN
     !  Initialize the mother domain.
     grid%input_from_file = .true.
     IF ( grid%input_from_file ) THEN

        CALL       wrf_debug ( 1 , 'wrf main: calling open_r_dataset for wrfinput' )

        IF ( wrf_dm_on_monitor() ) CALL start_timing

! typically <date> will not be part of input_inname but allow for it
        CALL domain_clock_get( grid, current_timestr=timestr )
        CALL construct_filename2a ( inpname , config_flags%input_inname , grid%id , 2 , timestr )

        CALL open_r_dataset ( fid, TRIM(inpname) , grid , config_flags , "DATASET=INPUT", ierr )
        IF ( ierr .NE. 0 ) THEN
          WRITE( wrf_err_message , * ) 'program wrf: error opening ',TRIM(inpname),' for reading ierr=',ierr
          CALL WRF_ERROR_FATAL ( wrf_err_message )
        ENDIF
! registry-generated code that reads the variable set defined on a given stream
#include "fine_stream_input.inc"

        CALL close_dataset ( fid , config_flags , "DATASET=INPUT" )
        IF ( wrf_dm_on_monitor() ) THEN
          WRITE ( message , FMT = '("processing wrfinput file (stream 0) for domain ",I8)' ) grid%id
          CALL end_timing ( TRIM(message) )
        ENDIF

!gmm add input for noamp hydro model here
     IF ( config_flags%opt_run.eq.5 ) THEN

        CALL construct_filename2a ( inpname , config_flags%auxinput7_inname &
                                 ,grid%id , 2 , timestr)

     if( grid%auxinput7_oid .NE. 0 ) then
       CALL close_dataset ( grid%auxinput7_oid , config_flags , "DATASET=AUXINPUT7" )
     endif
        
        CALL open_r_dataset ( grid%auxinput7_oid, TRIM(inpname) , grid , config_flags , "DATASET=AUXINPUT7", ierr )

!        call set_first_operation(grid%auxinput6_oid)

        
        IF ( ierr .NE. 0 ) THEN
          WRITE( wrf_err_message , * ) 'program wrf: error opening ',TRIM(inpname),' for reading ierr=',ierr
          CALL WRF_ERROR_FATAL ( wrf_err_message )
        ENDIF
           
           CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_aux_model_input7' )
           CALL input_auxinput7 ( grid%auxinput7_oid ,   grid , config_flags , ierr )
           CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_aux_model_input7' )
        
        CALL close_dataset ( grid%auxinput7_oid , config_flags , "DATASET=AUXINPUT7" )
       
       ENDIF
!gmm

#ifdef MOVE_NESTS
#if ( EM_CORE == 1 )
        grid%nest_pos = grid%ht
        where ( grid%nest_pos .gt. 0 ) grid%nest_pos = grid%nest_pos + 500.  ! make a cliff
#endif
#endif
     ENDIF
     grid%imask_nostag = 1
     grid%imask_xstag = 1
     grid%imask_ystag = 1
     grid%imask_xystag = 1
#if (EM_CORE == 1)
     grid%press_adj = .FALSE.
#endif
     CALL start_domain ( grid , .TRUE. )
   ELSE

     IF ( wrf_dm_on_monitor() ) CALL start_timing

     CALL domain_clock_get( grid, current_timestr=timestr )
     CALL construct_filename2a ( rstname , config_flags%rst_inname , grid%id , 2 , timestr )

     WRITE(message,*)'RESTART run: opening ',TRIM(rstname),' for reading'
     CALL wrf_message (  message )
     CALL open_r_dataset ( fid , TRIM(rstname) , grid , config_flags , "DATASET=RESTART", ierr )
     IF ( ierr .NE. 0 ) THEN
       WRITE( message , '("program wrf: error opening ",A32," for reading")') TRIM(rstname)
       CALL WRF_ERROR_FATAL ( message )
     ENDIF
     CALL input_restart ( fid,   grid , config_flags , ierr )
     CALL close_dataset ( fid , config_flags , "DATASET=RESTART" )

     IF ( wrf_dm_on_monitor() ) THEN
       WRITE ( message , FMT = '("processing restart file for domain ",I8)' ) grid%id
       CALL end_timing ( TRIM(message) )
     ENDIF

     grid%imask_nostag = 1
     grid%imask_xstag = 1
     grid%imask_ystag = 1
     grid%imask_xystag = 1
#if (EM_CORE == 1)
     grid%press_adj = .FALSE.
#endif
     CALL start_domain ( grid , .TRUE. )
   ENDIF

   RETURN
END SUBROUTINE med_initialdata_input

SUBROUTINE med_shutdown_io ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
  ! Model layer
   USE module_configure
   USE module_dm, ONLY : domain_active_this_task

   IMPLICIT NONE
   INTERFACE
     RECURSIVE SUBROUTINE med_shutdown_io_recurse ( grid , config_flags )
       USE module_domain
       USE module_configure
       TYPE (domain) , POINTER :: grid
       TYPE (grid_config_rec_type), INTENT(IN) :: config_flags
     END SUBROUTINE med_shutdown_io_recurse
   END INTERFACE

  ! Arguments
   TYPE(domain), TARGET                       :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   TYPE(domain),POINTER                       :: grid_ptr
   CHARACTER (LEN=80)      :: message
   INTEGER                 :: id, ierr

   IF ( grid%lbc_fid > 0 ) CALL close_dataset ( grid%lbc_fid , config_flags , "DATASET=BOUNDARY" )

   grid_ptr => grid
   CALL med_shutdown_io_recurse ( grid_ptr , config_flags )

#if ( HWRF == 1 ) 
   DO id = 1, max_domains
     IF( domain_active_this_task(id) ) THEN
       CALL push_communicators_for_domain(id)
       CALL wrf_ioexit( ierr )    ! shut down the quilt I/O
       CALL pop_communicators_for_domain
     ENDIF
   ENDDO
#else
   CALL wrf_ioexit( ierr )    ! shut down the quilt I/O
#endif

   RETURN

END SUBROUTINE med_shutdown_io

RECURSIVE SUBROUTINE med_shutdown_io_recurse ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
  ! Model layer
   USE module_configure

   IMPLICIT NONE

  ! Arguments
   TYPE(domain), POINTER                      :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   TYPE(domain), POINTER                      :: grid_ptr
   CHARACTER (LEN=80)      :: message
   INTEGER                 :: kid
   INTEGER                 :: ierr

   IF ( ASSOCIATED( grid ) ) THEN
     CALL push_communicators_for_domain(grid%id)
     IF ( grid%oid > 0 ) CALL close_dataset ( grid%oid , config_flags , "DATASET=HISTORY" )
! registry generated closes for auxhist streams
#    include "shutdown_closes.inc"
     grid_ptr => grid
     DO WHILE ( ASSOCIATED( grid_ptr ) )
       DO kid = 1, max_nests
         IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) ) THEN
           CALL med_shutdown_io_recurse ( grid_ptr%nests(kid)%ptr, config_flags )
         ENDIF
       ENDDO
       grid_ptr => grid_ptr%sibling
     ENDDO
     CALL pop_communicators_for_domain
   ENDIF
   RETURN
END SUBROUTINE med_shutdown_io_recurse


SUBROUTINE med_add_config_info_to_grid ( grid )

   USE module_domain
   USE module_configure
 
   IMPLICIT NONE

   !  Input data.

   TYPE(domain) , TARGET          :: grid

#define SOURCE_RECORD model_config_rec %
#define SOURCE_REC_DEX (grid%id)
#define DEST_RECORD   grid %
#include "config_assigns.inc"

   RETURN

END SUBROUTINE med_add_config_info_to_grid

