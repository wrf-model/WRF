!WRF:DRIVER_LAYER:UTIL
!

MODULE module_timing

   INTEGER, PARAMETER, PRIVATE :: cnmax = 30
   INTEGER, PRIVATE, DIMENSION(cnmax) :: count_int1 , count_rate_int1 , count_max_int1
   INTEGER, PRIVATE, DIMENSION(cnmax) :: count_int2 , count_rate_int2 , count_max_int2
   INTEGER, PRIVATE :: cn = 0 
   REAL, PRIVATE    :: elapsed_seconds , elapsed_seconds_total = 0
   REAL, PRIVATE    :: cpu_1 , cpu_2 , cpu_seconds , cpu_seconds_total = 0

CONTAINS

   SUBROUTINE init_module_timing
      cn = 0
   END SUBROUTINE init_module_timing


   SUBROUTINE start_timing

      IMPLICIT NONE

      cn = cn + 1
      IF ( cn .gt. cnmax ) THEN
        CALL wrf_error_fatal( 'module_timing: clock nesting error (too many nests)' )
        RETURN
      ENDIF
      CALL SYSTEM_CLOCK ( count_int1(cn) , count_rate_int1(cn) , count_max_int1(cn) )
!     CALL CPU_TIME ( cpu_1 )

   END SUBROUTINE start_timing


   SUBROUTINE end_timing ( string )
   
      IMPLICIT NONE

      CHARACTER *(*) :: string

      IF ( cn .lt. 1 ) THEN
        CALL wrf_error_fatal( 'module_timing: clock nesting error, cn<1' ) 
      ELSE IF ( cn .gt. cnmax ) THEN
        CALL wrf_error_fatal( 'module_timing: clock nesting error, cn>cnmax' ) 
      ENDIF

      CALL SYSTEM_CLOCK ( count_int2(cn) , count_rate_int2(cn) , count_max_int2(cn) )
!     CALL CPU_TIME ( cpu_2 )

      IF ( count_int2(cn) < count_int1(cn) ) THEN
         count_int2(cn) = count_int2(cn) + count_max_int2(cn)
      ENDIF

      count_int2(cn) = count_int2(cn) - count_int1(cn)
      elapsed_seconds = REAL(count_int2(cn)) / REAL(count_rate_int2(cn))
      elapsed_seconds_total = elapsed_seconds_total + elapsed_seconds

      WRITE(6,'(A,A,A,F10.5,A)') 'Timing for ',TRIM(string),': ',elapsed_seconds,' elapsed seconds.'
#if defined(DM_PARALLEL) && ! defined(STUBMPI)
      WRITE(0,'(A,A,A,F10.5,A)') 'Timing for ',TRIM(string),': ',elapsed_seconds,' elapsed seconds.'
#endif

!     cpu_seconds = cpu_2 - cpu_1
!     cpu_seconds_total = cpu_seconds_total + cpu_seconds
!     PRINT '(A,A,A,F10.5,A)' ,'Timing for ',TRIM(string),': ',cpu_seconds,' cpu seconds.'

      cn = cn - 1

   END SUBROUTINE end_timing

END MODULE module_timing

