!WRF:DRIVER_LAYER:UTIL
!

MODULE module_wrf_error
  INTEGER           :: wrf_debug_level = 0
  CHARACTER*256     :: wrf_err_message
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

  SUBROUTINE set_wrf_debug_level ( level )
    USE module_wrf_error
    IMPLICIT NONE
    INTEGER , INTENT(IN) :: level
    wrf_debug_level = level
    RETURN
  END SUBROUTINE set_wrf_debug_level

  SUBROUTINE get_wrf_debug_level ( level )
    USE module_wrf_error
    IMPLICIT NONE
    INTEGER , INTENT(OUT) :: level
    level = wrf_debug_level
    RETURN
  END SUBROUTINE get_wrf_debug_level


SUBROUTINE wrf_debug( level , str )
  USE module_wrf_error
  IMPLICIT NONE
  CHARACTER*(*) str
  INTEGER , INTENT (IN) :: level
  INTEGER               :: debug_level
  CALL get_wrf_debug_level( debug_level )
  IF ( level .LE. debug_level ) THEN
    CALL wrf_message( str )
  ENDIF
  RETURN
END SUBROUTINE wrf_debug

SUBROUTINE wrf_message( str )
  USE module_wrf_error
  IMPLICIT NONE
  CHARACTER*(*) str
#if defined( DM_PARALLEL ) && ! defined( STUBMPI) 
  write(0,*) str
#endif
  print*, str
END SUBROUTINE wrf_message

! intentionally write to stderr only
SUBROUTINE wrf_message2( str )
  USE module_wrf_error
  IMPLICIT NONE
  CHARACTER*(*) str
  write(0,*) str
END SUBROUTINE wrf_message2

SUBROUTINE wrf_error_fatal( str )
  USE module_wrf_error
  IMPLICIT NONE
  CHARACTER*(*) str
  
#if defined( DM_PARALLEL ) && ! defined( STUBMPI )
  CALL wrf_message( '-------------- FATAL CALLED ---------------' )
  CALL wrf_message( str )
  CALL wrf_message( '-------------------------------------------' )
#else
  CALL wrf_message2( '-------------- FATAL CALLED ---------------' )
  CALL wrf_message2( str )
  CALL wrf_message2( '-------------------------------------------' )
#endif
  CALL wrf_abort
END SUBROUTINE wrf_error_fatal
