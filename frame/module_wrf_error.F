!WRF:DRIVER_LAYER:UTIL
!

MODULE module_wrf_error
  INTEGER           :: wrf_debug_level = 0
  CHARACTER*256     :: wrf_err_message
!$OMP THREADPRIVATE (wrf_err_message)
CONTAINS

  LOGICAL FUNCTION wrf_at_debug_level ( level )
    IMPLICIT NONE
    INTEGER , INTENT(IN) :: level
    wrf_at_debug_level = ( level .LE. wrf_debug_level )
    RETURN
  END FUNCTION wrf_at_debug_level

  SUBROUTINE init_module_wrf_error
  END SUBROUTINE init_module_wrf_error

END MODULE module_wrf_error

! wrf_message: ordinary message
!   Write to stderr to ensure immediate output
!   Write to stdout for buffered output.
!   When running in quiet mode (WRF_QUIETLY), stderr is disabled.
SUBROUTINE wrf_message( str )
#ifdef ESMFIO
  USE ESMF
#endif
  IMPLICIT NONE
  CHARACTER*(*) str
#if defined( DM_PARALLEL ) && ! defined( STUBMPI) 
# if ! defined( WRF_QUIETLY )
  write(0,*) TRIM(str)
# endif
# ifdef _WIN32
  FLUSH(0)
# endif
#endif
#ifdef ESMFIO
  CALL ESMF_LogWrite(TRIM(str),ESMF_LOGMSG_INFO)
#endif
  print*, TRIM(str)
END SUBROUTINE wrf_message

! Intentionally write to stderr only
! This is set to stderr, even in quiet mode (WRF_QUIETLY), because
! it is used for potentially fatal error or warning messages and
! we want the message to get to the log file before any crash 
! or MPI_Abort happens.
SUBROUTINE wrf_message2( str )
#ifdef ESMFIO
  USE ESMF
#endif
  IMPLICIT NONE
  CHARACTER*(*) str
  write(0,*) str
# ifdef _WIN32
  FLUSH(0)
# endif
#ifdef ESMFIO
  CALL ESMF_LogWrite(TRIM(str),ESMF_LOGMSG_INFO)
#endif
END SUBROUTINE wrf_message2

SUBROUTINE wrf_error_fatal3( file_str, line, str )
  USE module_wrf_error
#ifdef ESMFIO
! 5.2.0r  USE ESMF_Mod
  USE ESMF
#endif
  IMPLICIT NONE
  CHARACTER*(*) file_str
  INTEGER , INTENT (IN) :: line  ! only print file and line if line > 0
  CHARACTER*(*) str
  CHARACTER*256 :: line_str

  write(line_str,'(i6)') line

#if defined(WRF_QUIETLY)
 ! ------------------------- QUIET MODE ERRORS -------------------------
 ! In quiet mode, print to stderr, then stdout.  Fatal errors are
 ! intentionally not quiet.  Two calls are needed since wrf_message
 ! only sends to stdout in quiet mode.  Note that MPI and non-MPI
 ! quiet modes are both handled here.

  CALL wrf_message2( '-------------- FATAL CALLED ---------------' )
  CALL wrf_message( '-------------- FATAL CALLED ---------------' )
  ! only print file and line if line is positive
  IF ( line > 0 ) THEN
    CALL wrf_message2( 'FATAL CALLED FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
    CALL wrf_message( 'FATAL CALLED FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
  ENDIF
  CALL wrf_message2( trim(str) )
  CALL wrf_message( str )
  CALL wrf_message2( '-------------------------------------------' )
  CALL wrf_message( '-------------------------------------------' )

  ! Flush stdout, otherwise some messages may not be printed since
  ! stdout is usually buffered
  flush(6)

  ! Flush stderr just in case
  flush(0)
#else

# if defined( DM_PARALLEL ) && ! defined( STUBMPI )
  ! -------------------------- MPI MODE ERRORS --------------------------
  ! In non-quiet MPI mode, print to stdout and stderr, but we only
  ! need a single wrf_message call for this.

  CALL wrf_message( '-------------- FATAL CALLED ---------------' )
  ! only print file and line if line is positive
  IF ( line > 0 ) THEN
    CALL wrf_message( 'FATAL CALLED FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
  ENDIF
  CALL wrf_message( str )
  CALL wrf_message( '-------------------------------------------' )

# else
  ! ------------------------ NON-MPI MODE ERRORS ------------------------
  ! In non-MPI, non-quiet mode, send everything to stderr, except for
  CALL wrf_message2( '-------------- FATAL CALLED ---------------' )
  ! only print file and line if line is positive
  IF ( line > 0 ) THEN
    CALL wrf_message( 'FATAL CALLED FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
  ENDIF
  CALL wrf_message2( str )
  CALL wrf_message2( '-------------------------------------------' )
# endif
#endif
#ifdef ESMFIO
! 5.2.0r  CALL esmf_finalize(terminationflag=ESMF_ABORT)
  CALL esmf_finalize(endflag=ESMF_END_ABORT)
#endif
  CALL wrf_abort
END SUBROUTINE wrf_error_fatal3

SUBROUTINE wrf_error_fatal( str )
  USE module_wrf_error
  IMPLICIT NONE
  CHARACTER*(*) str
  CALL wrf_error_fatal3 ( ' ', 0, str )
END SUBROUTINE wrf_error_fatal

! Check to see if expected value == actual value
! If not, print message and exit.  
SUBROUTINE wrf_check_error( expected, actual, str, file_str, line )
  USE module_wrf_error
  IMPLICIT NONE
  INTEGER , INTENT (IN) :: expected
  INTEGER , INTENT (IN) :: actual
  CHARACTER*(*) str
  CHARACTER*(*) file_str
  INTEGER , INTENT (IN) :: line
  CHARACTER (LEN=512)   :: rc_str
  CHARACTER (LEN=512)   :: str_with_rc

  IF ( expected .ne. actual ) THEN
    WRITE (rc_str,*) '  Routine returned error code = ',actual
    str_with_rc = TRIM(str // rc_str)
    CALL wrf_error_fatal3 ( file_str, line, str_with_rc )
  ENDIF
END SUBROUTINE wrf_check_error


