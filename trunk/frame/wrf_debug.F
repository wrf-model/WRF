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
  INTEGER :: debug_level 
  CHARACTER (LEN=256) :: time_str 
  CHARACTER (LEN=256) :: grid_str 
  CHARACTER (LEN=512) :: out_str 
  if(silence/=0) return
  CALL get_wrf_debug_level( debug_level ) 
  IF ( level .LE. debug_level ) THEN 
#ifdef _OPENMP 
  ! old behavior 
  CALL wrf_message( str ) 
#else 
  ! TBH: This fails on pgf90 6.1-4 when built with OpenMP and using more 
  ! TBH: than one thread. It works fine multi-threaded on AIX with xlf 
  ! TBH: 10.1.0.0 . Hence the cpp nastiness. 
  ! new behavior: include domain name and time-stamp 
  CALL get_current_time_string( time_str ) 
  CALL get_current_grid_name( grid_str ) 
  out_str = TRIM(grid_str)//' '//TRIM(time_str)//' '//TRIM(str) 
  CALL wrf_message( TRIM(out_str) ) 
#endif 
  ENDIF 
  RETURN 
END SUBROUTINE wrf_debug 
