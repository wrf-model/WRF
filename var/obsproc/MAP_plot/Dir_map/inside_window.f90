SUBROUTINE inside_window (time_obs, time_window_min, time_window_max, &
                          inside, iunit)

!  This routine determines if an observation with the input time
!  is within the analysis time window ]time_window_min, time_window_max]

   USE module_date

   IMPLICIT NONE

   CHARACTER (LEN = 19) :: time_obs
   CHARACTER (LEN = 19) :: time_window_min
   CHARACTER (LEN = 19) :: time_window_max
   LOGICAL, INTENT(OUT) :: inside
   INTEGER, INTENT(IN), OPTIONAL :: iunit
   CHARACTER (LEN = 19) :: time_obs_long
   INTEGER :: itb, ita
   INTEGER :: iiunit
   LOGICAL :: date1_correct, date2_correct

   IF (PRESENT (iunit)) THEN
       iiunit = iunit
   ELSE
       iiunit = 0
   ENDIF

!  WRITE (time_obs_long, FMT='(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)')    &
!         time_obs ( 1: 4), time_obs ( 5: 6), time_obs ( 7: 8), &
!         time_obs ( 9:10), time_obs (11:12), time_obs (13:14)

   time_obs_long = time_obs
!  time_obs - Time_window_min (IN >= 0, OUT < 0)

   CALL GETH_IDTS (time_obs_long, time_window_min, itb, date1_correct, iiunit)

!  time_obs - Time_window_max (IN =< 0, OUT > 0)

   CALL GETH_IDTS (time_obs_long, time_window_max, ita, date2_correct, iiunit)

   IF (((itb .LT. 0) .OR. (ita .GT. 0)) .OR. &
       ((.NOT. date1_correct) .OR. (.NOT. date2_correct))) THEN
       inside = .FALSE.             ! Observation in time window
   ELSE
       inside = .TRUE.              ! Observation out of time window
   ENDIF

   RETURN

END SUBROUTINE inside_window
