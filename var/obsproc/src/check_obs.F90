!---------------------------------------------------------------------------

SUBROUTINE check_obs (nobs_max, obs, number_of_obs, variable)

!-------------------------------------------------------------------------------
! Check if level contains at least one valid data. When level is uncomplete:
! The all station is removed for single level station.
! The uncomplete level only is removed for multi level station.
!-------------------------------------------------------------------------------

  USE module_type
  USE module_func
  USE module_per_type

  IMPLICIT NONE

  INTEGER, INTENT (in)                                :: nobs_max
  TYPE (report), DIMENSION (nobs_max), INTENT (inout) :: obs
  INTEGER, INTENT (in)                                :: number_of_obs
  CHARACTER (LEN = *), INTENT (in), OPTIONAL          :: variable

  TYPE (measurement), POINTER :: current
  TYPE (measurement), POINTER :: previous, temp
  INTEGER                     :: loop_index
  INTEGER                     :: nsurfaces, nuppers
  INTEGER                     :: iunit, io_error
  LOGICAL                     :: found = .FALSE.
  LOGICAL                     :: ok_miss, ok_qc, ok
  CHARACTER (LEN = 80)        :: title, fmt_found
  CHARACTER (LEN = 80)        :: filename
  CHARACTER (LEN = 32)        :: proc_name = 'check_completness: '
  LOGICAL                     :: connected
  LOGICAL                     :: print_uncomplete = .TRUE.

  INCLUDE 'missing.inc'
  INCLUDE 'platform_interface.inc'

!------------------------------------------------------------------------------!

      WRITE (0,'(A)')  &
'------------------------------------------------------------------------------'
      WRITE (UNIT = 0, FMT = '(A,/)') "CHECK PRESENCE OF HEIGHT OR/AND PRESSURE"


! 1. OPEN DIAGNOSTIC FILE
! =======================

      IF (print_uncomplete) THEN

      IF (PRESENT (variable)) THEN
          filename = "obs_check_"//TRIM (variable)//".diag"
      ELSE
          filename = 'obs_check.diag'
      ENDIF

      iunit    = 999

      INQUIRE (UNIT = iunit, OPENED = connected)

      IF (connected) CLOSE (iunit)

      OPEN (UNIT = iunit , FILE = filename , FORM = 'FORMATTED'  , &
            ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error )

      IF (io_error .NE. 0) THEN
          CALL error_handler (proc_name,&
         "Unable to open output diagnostic file. " , filename, .TRUE.)
      ELSE
          WRITE (UNIT = 0, FMT = '(A,A,/)') &
         "Diagnostics in file ", TRIM (filename)
      ENDIF

          WRITE (UNIT = iunit , FMT = '(A67)', ADVANCE = 'no') filename

      ENDIF


! 2.  LOOP OVER STATIONS
! ======================

! 2.1 Initialize counter
!     -----------------

stations:&
      DO loop_index = 1, number_of_obs


! 2.2 Check if station is valid
!     --------------------------

! YRG(05/02/2003): GPSPW (FM-111) is not allowed to do the check:

      IF (obs (loop_index) % info % discard .or. &
          (obs (loop_index) % info % platform(4:6) == '111' .or. &  ! GPS PW
           obs (loop_index) % info % platform(4:6) == '114')) THEN  ! GPS ZTD 

          CYCLE  stations

      ENDIF

! 2.3 Initialize pointer to surface
!     -----------------------------

      current => obs (loop_index) % surface

! 2.4 Some observations (GPS) don't have any levels, skip them
!     --------------------------------------------------------

      IF (.NOT. ASSOCIATED (current)) THEN
!           DEALLOCATE (current)
           CYCLE stations
      ENDIF

! 2.5 Write station ID
!     ----------------

      IF (print_uncomplete) THEN

          IF (.NOT. found) THEN
              fmt_found = '(TL67,A20,A5,1X,A23,2F9.3)'
          ELSE
              fmt_found = '(//,A20,A5,1X,A23,2F9.3)'
          ENDIF

          WRITE (UNIT = iunit , FMT = TRIM (fmt_found), ADVANCE = 'no') &
         'Found Name and ID = ' ,                                       &
          TRIM  (obs (loop_index) % location % id ) ,                   &
          TRIM  (obs (loop_index) % location % name),                   &
                 obs (loop_index) % location % latitude,                &
                 obs (loop_index) % location % longitude

!SDH010706          found = .FALSE.
          found = .TRUE.

      ENDIF

! 3.  TEST
! ========

! 3.1 Loop over upper levels
!     ----------------------

upper: &
      DO WHILE (ASSOCIATED (current))

! 3.2 Test both height and pressure
!     -----------------------------

         IF ((eps_equal (current % meas % height   % data, missing_r, 1.)).AND.&
             (eps_equal (current % meas % pressure % data, missing_r, 1.))) THEN

! 3.2.1 Print
!       -----

         IF (print_uncomplete) THEN

! 3.2.1 Platform type
!       -------------

           READ (obs (loop_index) % info % platform (4:6), '(I3)') fm

           CALL fm_decoder (fm, platform)

! 3.2.2 Print bad height
!       ----------------

           title = '...Missing pressure and height '//TRIM (platform)
           CALL PRINT_BAD (iunit, title, current)
           found = .TRUE.

           STOP 'in check_obs.F90'

         ENDIF

! 3.3 Test height or pressure
!     -----------------------

         ELSE &
         IF ((eps_equal (current % meas % height   % data, missing_r, 1.)) .OR.&
             (eps_equal (current % meas % pressure % data, missing_r, 1.))) THEN

! 3.3.1 Print
!       -----

         IF (print_uncomplete) THEN

! 3.3.2 Platform type
!       -------------

           READ (obs (loop_index) % info % platform (4:6), '(I3)') fm

           CALL fm_decoder (fm, platform)

! 3.3.3 Print bad height
!       ----------------

           IF (eps_equal (current % meas % height   % data, missing_r, 1.))&
           THEN

               title = '...Missing height '//TRIM (platform)

               CALL PRINT_BAD (iunit, title, current)

               found = .TRUE.

               IF (PRESENT (variable)) THEN
                   IF ((TRIM (variable) == "HEIGHT")  .OR. &
                       (TRIM (variable) == "height")) THEN
                        STOP 'in check_obs.F90'
                    ENDIF
              ENDIF
           ENDIF

! 3.3.4 Print bad pressure
!       ------------------

           IF (eps_equal (current % meas % pressure   % data, missing_r, 1.))&
           THEN

               title = '...Missing pressure '//TRIM (platform)

               CALL PRINT_BAD (iunit, title, current)

               found = .TRUE.

               IF (PRESENT (variable)) THEN
                   IF ((TRIM (variable) == "PRESSURE")  .OR. &
                       (TRIM (variable) == "pressure")) THEN
                        STOP 'in check_obs.F90'
                    ENDIF
              ENDIF

           ENDIF

        ENDIF

      ENDIF

! 3.4 Go to next level
!     --------------

        current => current % next 


     ENDDO upper

     ENDDO stations

     IF (print_uncomplete) CLOSE (iunit)

END SUBROUTINE check_obs 

 SUBROUTINE print_bad (iunit, title, current)
!------------------------------------------------------------------------------!

      USE module_type

      IMPLICIT NONE

      INTEGER,              INTENT (in) :: iunit
      CHARACTER (LEN = 80), INTENT (in) :: title
      TYPE (measurement) :: current
!------------------------------------------------------------------------------!
               WRITE (UNIT = iunit, FMT = '(/,2A)', ADVANCE = 'no') &
               TRIM  (title)

               WRITE (UNIT = iunit, FMT = '(7(/,A,1X,F12.3,1X,I8,:))',&
                      ADVANCE = 'no')     &
           '   Height      = ',current % meas % height      % data,      &
                               current % meas % height      % qc,        &
           '   Pressure    = ',current % meas % pressure    % data,      &
                               current % meas % pressure    % qc

!          '   Speed       = ',current % meas % speed       % data,      &
!                              current % meas % speed       % qc,        &
!          '   Direction   = ',current % meas % direction   % data,      &
!                              current % meas % direction   % qc,        &
!          '   Temperature = ',current % meas % temperature % data,      &
!                              current % meas % temperature % qc,        &
!          '   Dew Point   = ',current % meas % dew_point   % data,      &
!                              current % meas % dew_point   % qc,        &
!          '   Humidity    = ',current % meas % rh          % data,      &
!                              current % meas % rh          % qc

!------------------------------------------------------------------------------!
 END SUBROUTINE print_bad
