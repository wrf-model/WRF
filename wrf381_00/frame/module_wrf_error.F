!WRF:DRIVER_LAYER:UTIL
!

MODULE module_wrf_error
  INTEGER           :: wrf_debug_level = 0
  CHARACTER*256     :: wrf_err_message

  ! LOGICAL silence -- if TRUE (non-zero), this MPI rank does not send
  !   messages via wrf_message, end_timing, wrf_debug, atm_announce,
  !   cmp_announce, non-fatal glob_abort, or the like.  If FALSE, this
  !   MPI rank DOES send messages.  Regardless of this setting, fatal
  !   errors (wrf_error_fatal or fatal glob_aborts) and anything sent to
  !   write or print will be sent.
#if defined(DM_PARALLEL)
  integer, save :: silence=0
#else
  integer, PARAMETER :: silence=0 ! Per-rank silence requires MPI
#endif

  ! LOGICAL buffered -- if TRUE, messages are buffered via clog_write.
  !   Once the buffer is full, messages are sent to stdout.  This does
  !   not apply to WRF_MESSAGE2, WRF_ERROR_FATAL, or anything sent to
  !   write or print.  The buffering implementation will not write
  !   partial lines, and buffer size is specified via namelist (see
  !   init_module_wrf_error).
  !   If FALSE, messages are send directly to WRITE.
  !
  !   This must be enabled at compile time by setting $WRF_LOG_BUFFERING

#if defined(WRF_LOG_BUFFERING)
  integer :: buffered=0
#else
  integer, PARAMETER :: buffered=0 ! buffering disabled at compile time
#endif

  ! LOGICAL stderrlog -- if TRUE, messages are sent to stderr via
  !   write(0,...).  If FALSE, messages are not sent to stderr.
  !   This is set to FALSE automatically when buffering is enabled.

  ! Defaults: Non-MPI configurations and HWRF turn OFF stderr.
  !    MPI configurations other than HWRF turn ON stderr.

#if defined( DM_PARALLEL ) && ! defined( STUBMPI ) && !(HWRF == 1)
  integer :: stderrlog=1 ! 1/T = send to write(0,...) if buffered=0
#else
  integer :: stderrlog=0! 1/T = send to write(0,...) if buffered=0
#endif

  INTEGER, PARAMETER :: wrf_log_flush=0, wrf_log_set_buffer_size=1, &
                        wrf_log_write=2

  !NOTE: Make sure silence, buffered and stderrlog defaults here match
  ! the namelist defaults in init_module_wrf_error.

! min_allowed_buffer_size: requested buffer sizes smaller than this
! will simply result in disabling of log file buffering.  This number
! should be larger than any line WRF prints frequently.  If you set it 
! too small, the buffering code will still work.  However, any line 
! that is larger than the buffer may result in two writes: one for 
! the message and one for the end-of-line character at the end (if the
! message didn't already have one).
  integer, parameter :: min_allowed_buffer_size=200

!$OMP THREADPRIVATE (wrf_err_message)
CONTAINS

! ------------------------------------------------------------------------------

  LOGICAL FUNCTION wrf_at_debug_level ( level )
    IMPLICIT NONE
    INTEGER , INTENT(IN) :: level
    wrf_at_debug_level = ( level .LE. wrf_debug_level )
    RETURN
  END FUNCTION wrf_at_debug_level

! ------------------------------------------------------------------------------

  SUBROUTINE init_module_wrf_error(on_io_server)
    IMPLICIT NONE
    LOGICAL,OPTIONAL,INTENT(IN) :: on_io_server
#if defined(DM_PARALLEL)
    LOGICAL, EXTERNAL :: wrf_dm_on_monitor
#endif
    LOGICAL :: compute_tasks_silent
    LOGICAL :: io_servers_silent
    INTEGER :: buffer_size,iostat,stderr_logging
    namelist /logging/ buffer_size,compute_tasks_silent, &
                       io_servers_silent,stderr_logging

    ! MAKE SURE THE NAMELIST DEFAULTS MATCH THE DEFAULT VALUES
    ! AT THE MODULE LEVEL

    ! Default: original behavior.  No buffering, all ranks talk
    compute_tasks_silent=.false.
    io_servers_silent=.false.
    buffer_size=0

    ! MPI configurations default to stderr logging, except for HWRF.
    ! Non-MPI does not log to stderr.  (Note that fatal errors always
    ! are sent to both stdout and stderr regardless of config.)
#if defined( DM_PARALLEL ) && ! defined( STUBMPI ) && !(HWRF == 1)
    stderr_logging=1
#else
    stderr_logging=0
#endif
500 format(A)
    ! Open namelist.input using the same unit used by module_io_wrf 
    ! since we know nobody else will use that unit:
    OPEN(unit=27, file="namelist.input", form="formatted", status="old")
    READ(27,nml=logging,iostat=iostat)
    if(iostat /= 0) then
#if (DA_CORE!=1)
       CALL wrf_debug ( 1 , 'Namelist logging not found in namelist.input. ' )
       CALL wrf_debug ( 1 , ' --> Using registry defaults for variables in logging.' )
#else
       write(0,*) 'Namelist logging not found in namelist.input. Using registry defaults for variables in logging.'
       write(6,*) 'Namelist logging not found in namelist.input. Using registry defaults for variables in logging.'
#endif
#      ifdef _WIN32
          FLUSH(0)
#      endif
       close(27)
       return
    endif
    CLOSE(27)

#if defined(WRF_LOG_BUFFERING)
    ! Forbid small buffers.  See the comment above for min_allowed_buffer_size:
    if(buffer_size>=min_allowed_buffer_size) then
       call wrf_log_action(wrf_log_set_buffer_size,buffer_size,' ')
       buffered=1
    else
       buffered=0
    endif
#else
    if(buffer_size>=min_allowed_buffer_size) then
       write(0,500) 'Forcing disabling of buffering due to compile-time configuration.'
       write(6,500) 'Forcing disabling of buffering due to compile-time configuration.'
    endif
#endif

    stderrlog=stderr_logging
    if(buffered/=0 .and. stderrlog/=0) then
       write(0,500) 'Disabling stderr logging since buffering is enabled.'
       write(6,500) 'Disabling stderr logging since buffering is enabled.'
#      ifdef _WIN32
          FLUSH(0)
#      endif
       stderrlog=0
    endif

#if defined(DM_PARALLEL)
    silence=0
    if(present(on_io_server)) then
       if(on_io_server) then
          if(io_servers_silent) &
               silence=1
          return
       endif
    endif
    if(compute_tasks_silent) then
       if(wrf_dm_on_monitor()) then
          silence=0
       else
          silence=1
       endif
    endif
#endif
  END SUBROUTINE init_module_wrf_error

END MODULE module_wrf_error

! ------------------------------------------------------------------------------
! ------------------------  GLOBAL SCOPE SUBROUTINES  --------------------------
! ------------------------------------------------------------------------------
#if defined(WRF_LOG_BUFFERING)
SUBROUTINE wrf_log_action( act,int,str )
  ! The underlying clog.c is not thread-safe, so this wrapper subroutine
  ! ensures that only one thread accesses clog.c at a time.

  ! NOTE: This routine only exists if WRF_LOG_BUFFERING is defined at
  ! compile time.
  use module_wrf_error
  implicit none
  integer, intent(in) :: int,act
  character(*), intent(in) :: str
!$OMP CRITICAL(wrf_log_action_critical)
  if(act==wrf_log_flush) then
     call clog_flush(int)
  elseif(act==wrf_log_set_buffer_size) then
     call clog_set_buffer_len(int)
  elseif(act==wrf_log_write) then
     call clog_write(int,str)
  endif
!$OMP END CRITICAL(wrf_log_action_critical)
END SUBROUTINE wrf_log_action
#endif
! ------------------------------------------------------------------------------

! wrf_message: ordinary message
!   Write to stderr if stderrlog=T to ensure immediate output
!   Write to stdout for buffered output.
SUBROUTINE wrf_message( str )
#ifdef ESMFIO
  USE ESMF
#endif
  use module_wrf_error, only: silence, buffered, stderrlog, wrf_log_write
  IMPLICIT NONE

  CHARACTER*(*) str
  if(silence/=0) return
  if(buffered/=0) then
#if defined(WRF_LOG_BUFFERING)
     call wrf_log_action(wrf_log_write,len_trim(str),str)
#endif
  else
!$OMP MASTER
     if(stderrlog/=0) then
300     format(A)
        write(0,300) trim(str)
# ifdef _WIN32
  FLUSH(0)
# endif
     endif
     print 300,trim(str)
!$OMP END MASTER
  endif

#ifdef ESMFIO
  CALL ESMF_LogWrite(TRIM(str),ESMF_LOGMSG_INFO)
#endif
END SUBROUTINE wrf_message

! ------------------------------------------------------------------------------

! Intentionally write to stderr only
! This is set to stderr, even in silent mode, because
! it is used for potentially fatal error or warning messages and
! we want the message to get to the log file before any crash 
! or MPI_Abort happens.
SUBROUTINE wrf_message2( str )
#ifdef ESMFIO
  USE ESMF
#endif
  IMPLICIT NONE
  CHARACTER*(*) str
!$OMP MASTER
400 format(A)
  write(0,400) str
# ifdef _WIN32
  FLUSH(0)
# endif
!$OMP END MASTER
#ifdef ESMFIO
  CALL ESMF_LogWrite(TRIM(str),ESMF_LOGMSG_INFO)
#endif
END SUBROUTINE wrf_message2

! ------------------------------------------------------------------------------

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

  ! Fatal errors are printed to stdout and stderr regardless of
  ! any &logging namelist settings.

  CALL wrf_message( '-------------- FATAL CALLED ---------------' )
  ! only print file and line if line is positive
  IF ( line > 0 ) THEN
    CALL wrf_message( 'FATAL CALLED FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
  ENDIF
  CALL wrf_message( str )
  CALL wrf_message( '-------------------------------------------' )

  force_stderr: if(stderrlog==0) then
  CALL wrf_message2( '-------------- FATAL CALLED ---------------' )
  ! only print file and line if line is positive
  IF ( line > 0 ) THEN
        CALL wrf_message2( 'FATAL CALLED FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
  ENDIF
     CALL wrf_message2( trim(str) )
  CALL wrf_message2( '-------------------------------------------' )
  endif force_stderr

  ! Flush all streams.
  flush(6)
#if defined(WRF_LOG_BUFFERING)
  if(buffered/=0) call wrf_log_action(wrf_log_flush,1,' ')
# endif
  flush(0)

#ifdef ESMFIO
! 5.2.0r  CALL esmf_finalize(terminationflag=ESMF_ABORT)
  CALL esmf_finalize(endflag=ESMF_END_ABORT)
#endif

#ifdef TRACEBACKQQ
  CALL tracebackqq
#endif
  CALL wrf_abort
END SUBROUTINE wrf_error_fatal3

! ------------------------------------------------------------------------------

SUBROUTINE wrf_error_fatal( str )
  USE module_wrf_error
  IMPLICIT NONE
  CHARACTER*(*) str
  CALL wrf_error_fatal3 ( ' ', 0, str )
END SUBROUTINE wrf_error_fatal

! ------------------------------------------------------------------------------

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

! ------------------------------------------------------------------------------

!  Some compilers do not yet support the entirety of the Fortran 2003 standard.
!  This is a small patch to pick up the two most common events.  Most xlf 
!  compilers have an extension fflush.  That is available here.  For other older
!  compilers with no flush capability at all, we just stub it out completely.
!  These CPP ifdefs are defined in the configure file.

#ifdef USE_FFLUSH
SUBROUTINE flush ( iunit )
  IMPLICIT NONE
  INTEGER :: iunit
  CALL fflush ( iunit ) 
END SUBROUTINE flush
#endif

#ifdef NO_FLUSH_SUPPORT
SUBROUTINE flush ( iunit )
  IMPLICIT NONE
  INTEGER :: iunit
  RETURN
END SUBROUTINE flush
#endif



