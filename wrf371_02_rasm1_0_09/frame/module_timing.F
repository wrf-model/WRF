!WRF:DRIVER_LAYER:UTIL
!

MODULE module_timing

   INTEGER, PARAMETER, PRIVATE :: cnmax = 30
   INTEGER, PRIVATE :: cn = 0 
   REAL, PRIVATE    :: elapsed_seconds , elapsed_seconds_total = 0

#if defined(OLD_TIMERS)
   INTEGER, PRIVATE, DIMENSION(cnmax) :: count_int1 , count_rate_int1 , count_max_int1
   INTEGER, PRIVATE, DIMENSION(cnmax) :: count_int2 , count_rate_int2 , count_max_int2
   REAL, PRIVATE    :: cpu_1 , cpu_2 , cpu_seconds , cpu_seconds_total = 0
#else
   REAL(kind=8) :: epoch_seconds_hires(cnmax)
#endif

CONTAINS

   SUBROUTINE init_module_timing
#if defined(OLD_TIMERS)
      ! Nothing to do here.
#else
      ! Initialize the high-res timer.  This is optional, but will allow
      ! higher precision.  Read hires_timer.c for details.
      call init_hires_timer()
#endif
      cn = 0
   END SUBROUTINE init_module_timing


   SUBROUTINE start_timing
     use module_wrf_error, only: silence

      IMPLICIT NONE

      if(silence/=0) return
      cn = cn + 1
      IF ( cn .gt. cnmax ) THEN
        CALL wrf_error_fatal( 'module_timing: clock nesting error (too many nests)' )
        RETURN
      ENDIF

#if defined(OLD_TIMERS)
      CALL SYSTEM_CLOCK ( count_int1(cn) , count_rate_int1(cn) , count_max_int1(cn) )
!     CALL CPU_TIME ( cpu_1 )
#else
      call hires_timer(epoch_seconds_hires(cn))
#endif

   END SUBROUTINE start_timing


   SUBROUTINE end_timing ( string )
     use module_wrf_error, only: silence, stderrlog, buffered
   
      IMPLICIT NONE
      REAL(kind=8) :: now_hires
      CHARACTER *(*) :: string
      character*512 :: buf

      if(silence/=0) return

      IF ( cn .lt. 1 ) THEN
        CALL wrf_error_fatal( 'module_timing: clock nesting error, cn<1' ) 
      ELSE IF ( cn .gt. cnmax ) THEN
        CALL wrf_error_fatal( 'module_timing: clock nesting error, cn>cnmax' ) 
      ENDIF

#if defined(OLD_TIMERS)
      CALL SYSTEM_CLOCK ( count_int2(cn) , count_rate_int2(cn) , count_max_int2(cn) )
!     CALL CPU_TIME ( cpu_2 )

      IF ( count_int2(cn) < count_int1(cn) ) THEN
         count_int2(cn) = count_int2(cn) + count_max_int2(cn)
      ENDIF

      count_int2(cn) = count_int2(cn) - count_int1(cn)
      elapsed_seconds = REAL(count_int2(cn)) / REAL(count_rate_int2(cn))
#else
      call hires_timer(now_hires)
      ! The REAL() here should convert to default real from REAL(kind=8)
      elapsed_seconds = REAL(now_hires-epoch_seconds_hires(cn))
#endif
      elapsed_seconds_total = elapsed_seconds_total + elapsed_seconds

3031 format("Timing for ",A,": ",F10.5," elapsed seconds")
      if(buffered/=0) then
         write(buf,3031) TRIM(string),elapsed_seconds
         call wrf_message(buf)
      else
         if(stderrlog/=0) &
              write(0,3031) TRIM(string),elapsed_seconds
         write(6,3031) TRIM(string),elapsed_seconds
      endif

!     cpu_seconds = cpu_2 - cpu_1
!     cpu_seconds_total = cpu_seconds_total + cpu_seconds
!     PRINT '(A,A,A,F10.5,A)' ,'Timing for ',TRIM(string),': ',cpu_seconds,' cpu seconds.'

      cn = cn - 1

   END SUBROUTINE end_timing

   FUNCTION now_time() result(timef)
     ! This is a simple subroutine that returns the current time in
     ! seconds since some arbitrary reference point.  This routine is
     ! meant to be used to accumulate timing information.  See solve_nmm
     ! for examples.
     implicit none
     real*8 :: timef
#if defined(OLD_TIMERS)
     integer :: ic,ir
     call system_clock(count=ic,count_rate=ir)
     timef=real(ic)/real(ir)
#else
     call hires_timer(timef)
#endif
   END FUNCTION now_time

END MODULE module_timing

