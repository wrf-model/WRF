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
  CHARACTER (LEN=256) :: time_str
  CHARACTER (LEN=256) :: grid_str
  CHARACTER (LEN=512) :: out_str
  CALL get_wrf_debug_level( debug_level )
  IF ( level .LE. debug_level ) THEN
    ! old behavior
!      CALL wrf_message( str )
    ! new behavior:  include domain name and time-stamp
    CALL get_current_time_string( time_str )
    CALL get_current_grid_name( grid_str )
    out_str = TRIM(grid_str)//' '//TRIM(time_str)//' '//TRIM(str)
    CALL wrf_message( TRIM(out_str) )
  ENDIF
  RETURN
END SUBROUTINE wrf_debug
