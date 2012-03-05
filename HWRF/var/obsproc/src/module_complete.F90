
MODULE module_complete

!-----------------------------------------------------------------------------!
! Check if observations at any level got:
!
! - At least one piece of information: either wind speed, wind direction, 
!                                     temperature, dew point, relative humidity
! - Qc is set to missing for missing information
! - Below model lid
!
! Levels that fail the check are removed, 
! Stations with all its failing levels are removed
!-----------------------------------------------------------------------------!
!
!  HISTORY: 
!
!         F. VANDENBERGHE, March 2001
!
!         01/13/2003 - Updated for Profiler obs.           S. R. H. Rizvi
!
!         02/04/2003 - Updated for Buoy     obs.           S. R. H. Rizvi
!
!         02/11/2003 - Reviewed and modified for Profiler
!                      and Buoy obs.                       Y.-R. Guo
!         06/30/2006 -   Updated for AIRS retrievals       Syed  RH  Rizvi
!------------------------------------------------------------------------------

CONTAINS
!------------------------------------------------------------------------------!
!
! SUBROUTINE check_completness (nobs_max, obs, number_of_obs, print_uncomplete)
! FUNCTION   check_level (current, missing_r) RESULT (ok)
! FUNCTION   check_qc (current) RESULT (ok)
!
!---------------------------------------------------------------------------

SUBROUTINE check_completness (nobs_max, obs, number_of_obs, remove_above_lid, &
                              print_uncomplete)

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
  LOGICAL, INTENT (in)                                :: remove_above_lid
  LOGICAL, INTENT (in)                                :: print_uncomplete

  TYPE (measurement), POINTER :: current
  TYPE (measurement), POINTER :: previous, temp
  INTEGER                     :: loop_index
  INTEGER                     :: nsurfaces, nuppers
  INTEGER                     :: iunit, io_error
  LOGICAL                     :: found = .FALSE.
  LOGICAL                     :: ok_miss, ok_qc, ok, lpw, ltb
  CHARACTER (LEN = 80)        :: title, fmt_found
  CHARACTER (LEN = 80)        :: filename
  CHARACTER (LEN = 32)        :: proc_name = 'check_completness: '
  LOGICAL                     :: connected

  INCLUDE 'missing.inc'
  INCLUDE 'platform_interface.inc'

!------------------------------------------------------------------------------!

   WRITE (UNIT = 0, FMT = '(A)')  &
'------------------------------------------------------------------------------'
   WRITE (UNIT = 0, FMT = '(A,/)') 'LOOK FOR UNCOMPLETE DATA:'


! 1. OPEN DIAGNOSTIC FILE
! =======================

      IF (print_uncomplete) THEN

      filename = 'obs_uncomplete.diag'
      iunit    = 999

      INQUIRE (UNIT = iunit, OPENED = connected)

      IF (connected) CLOSE (iunit)

      OPEN (UNIT = iunit , FILE = filename , FORM = 'FORMATTED'  , &
            ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error )

      IF (io_error .NE. 0) THEN
          CALL error_handler (proc_name,&
         "Unable to open output diagnostic file. " , filename, .TRUE.)
      ELSE
          WRITE (UNIT = 0, FMT = '(A,A)') &
         "Diagnostics in file ", TRIM (filename)
      ENDIF

          WRITE (UNIT = iunit , FMT = '(A67)', ADVANCE = 'no') filename
      ENDIF


! 2.  LOOP OVER STATIONS
! ======================

! 2.1 Initialize counter
!     -----------------

      nsurfaces = 0
      nuppers   = 0


stations: DO loop_index = 1, number_of_obs


! 2.2 Check if station is valid
!     --------------------------

stations_valid: IF (obs (loop_index) % info % discard ) THEN

                 CYCLE  stations

      ELSE stations_valid

! 2.4 Some observations (GPS) don't have any levels, skip them
!     --------------------------------------------------------

      IF (.NOT. ASSOCIATED (obs (loop_index) % surface)) THEN
          CYCLE stations
      ENDIF

      READ (obs (loop_index) % info % platform (4:6), '(I3)') fm

! 2.5 If GPS PW is present in ground info, obs must not be discard
!     ------------------------------------------------------------

     IF (eps_equal (obs (loop_index) % ground % pw % data, missing_r, 1.)) THEN 
         lpw =.FALSE.
     ELSE
         lpw =.TRUE.
     ENDIF

! 2.5 If Brightness Temp is present in ground info, obs must not be discard
!     ---------------------------------------------------------------------

     IF ((eps_equal (obs (loop_index) % ground % tb19v % data,missing_r,1.)).AND.&
         (eps_equal (obs (loop_index) % ground % tb19h % data,missing_r,1.)).AND.&
         (eps_equal (obs (loop_index) % ground % tb22v % data,missing_r,1.)).AND.&
         (eps_equal (obs (loop_index) % ground % tb37v % data,missing_r,1.)).AND.&
         (eps_equal (obs (loop_index) % ground % tb37h % data,missing_r,1.)).AND.&
         (eps_equal (obs (loop_index) % ground % tb85v % data,missing_r,1.)).AND.&
         (eps_equal (obs (loop_index) % ground % tb85h % data,missing_r,1.))) THEN
         ltb =.FALSE.
     ELSE
         ltb =.TRUE.
     ENDIF

! 2.6 Write station ID
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

          found = .TRUE.

      ENDIF

! 2.3 Initialize pointer to surface
!     -----------------------------

1000  continue

      current => obs (loop_index) % surface

! 3.  SINGLE LEVEL STATION 
! ========================

! 3.1 Check if at least one datum is valid
!     ------------------------------------

      ok_miss = check_level (current, missing_r)
!      ok_qc   = check_qc    (current) .AND. remove_above_lid
      ok_qc = .true.
      if (remove_above_lid) ok_qc   = check_qc    (current)
      ok      = ok_miss .AND. ok_qc

! 3.2 If 1st level is single and all data are missing, remove complete station
!     ------------------------------------------------------------------------

single_level:&
      IF (      ASSOCIATED (current)        .AND.  &
          .NOT. ASSOCIATED (current % next) .AND. &
         (.NOT. ok_miss .OR. .NOT. ok_qc))  THEN

! 3.2.1 Platform type
!       -------------

           READ (obs (loop_index) % info % platform (4:6), '(I3)') fm

           CALL fm_decoder (fm, platform, &
                            synop=nsynops (icor), ship =nshipss (icor), &
                            metar=nmetars (icor), pilot=npilots (icor), &
                            sound=nsounds (icor), satem=nsatems (icor), &
                            satob=nsatobs (icor), airep=naireps (icor), &
                            other=nothers (icor), gpspw=ngpspws (icor), &
                            gpszd=ngpsztd (icor), gpsrf=ngpsref (icor), &
                            amdar=namdars (icor), qscat=nqscats (icor), &
                            profl=nprofls (icor), buoy=nbuoyss  (icor), &
                            bogus=nboguss (icor), gpsep=ngpseph (icor), &
                            airs=nairss(icor),tamdar=ntamdar(icor) )

! 3.2.2 Print removed station
!       ---------------------

           IF (print_uncomplete) THEN

               IF (.NOT. ok_miss) THEN
                    title = '...Discard empty surface station '//TRIM (platform)
               ELSE
                    title='...Discard out of domain surface station '//TRIM (platform)
               ENDIF

               CALL PRINT_BAD (iunit, title,current)

               found = .TRUE.

           ENDIF

! 3.2.3 Deallocate level pointer
!       ------------------------

           DEALLOCATE (current)
           NULLIFY    (obs (loop_index) % surface)

           nuppers = nuppers + 1

! 3.2.3 Discard obs only if PW is not present
!       -------------------------------------

           IF ((.NOT. lpw) .AND. (.NOT. ltb)) THEN
               obs (loop_index) % info % discard = .TRUE.
               nsurfaces = nsurfaces + 1
               nuppers   = nuppers - 1
           ENDIF

! 3.2.4 Go to next station
!       ------------------

           CYCLE stations


! 3.3 If station has several levels and first is empty, remove level only
!     -------------------------------------------------------------------

      ELSE IF (ASSOCIATED (current)        .AND.  &
               ASSOCIATED (current % next) .AND.  &
              (.NOT. ok_miss .OR. .NOT. ok_qc))  THEN single_level

           IF (print_uncomplete) THEN

! 3.2.1 Platform type
!       -------------

               READ (obs (loop_index) % info % platform (4:6), '(I3)') fm

               CALL fm_decoder (fm, platform)

               IF (.NOT. ok_miss)   THEN
                   title = '...Remove empty level '//TRIM (platform)
               ELSE
                   title = '...Remove out of domain level '//TRIM (platform)
               ENDIF

               CALL PRINT_BAD (iunit, title, current)

               found = .TRUE.

           ENDIF

! 2.2.3 Remove level
!       ------------

           temp => obs (loop_index) % surface
           obs (loop_index) % surface => current % next

           DEALLOCATE (temp)

           nuppers = nuppers + 1

           go to 1000

! 2.3 If first level is valid and, go inspect upper levels
!     ----------------------------------------------------

      ELSE IF (ASSOCIATED (current)        .AND.  &
               ASSOCIATED (current % next)) THEN single_level

! 3.  PROCESS UPPER LEVELS
! ========================

upper_levels:DO

      ok = .TRUE.

! 3.1 Initialize on previous level (1st level)
!     ----------------------------------------

      previous => obs (loop_index) % surface
      current  => previous % next

! 3.2 Loop over upper levels
!     ----------------------

associated_pt:&

      DO WHILE (ASSOCIATED (current))

! 3.3 Check level
!     -----------

      ok_miss = check_level (current, missing_r)
!      ok_qc   = check_qc    (current) .AND. remove_above_lid
      ok_qc = .true.
      if (remove_above_lid) ok_qc   = check_qc    (current)
      ok      = ok_miss .AND. ok_qc

! 3.4 If level OK, go to next
!     -----------------------

         IF (ok_miss .AND. ok_qc)  THEN

             previous => current
             current  => current % next

! 3.5 If level not OK, exit delete it
!     -------------------------------

         ELSE

            IF (print_uncomplete) THEN

               READ (obs (loop_index) % info % platform (4:6), '(I3)') fm

               CALL fm_decoder (fm, platform)

               IF (.NOT. ok_miss)   THEN
                    title = '...Remove empty level '//TRIM (platform)
               ELSE
                    title = '...Remove out of domain level '//TRIM (platform)
               ENDIF

               CALL PRINT_BAD (iunit, title, current)

               found = .TRUE.

            ENDIF

            nuppers = nuppers + 1

            EXIT associated_pt

         ENDIF

      ENDDO associated_pt


! 4.  DELETE BAD LEVEL
! ====================

      IF (.NOT. ok_miss .OR. .NOT. ok_qc)  THEN

! 4.1 If intermediate level, delete and go back to upper level loop
!     -------------------------------------------------------------

          IF (ASSOCIATED (current % next)) THEN

              previous % next => current % next

              DEALLOCATE (current)

              CYCLE upper_levels

! 4.2 if run out of data, delete and go back to station loop
!     ------------------------------------------------------

          ELSE

              DEALLOCATE (previous % next)
              EXIT upper_levels

          ENDIF


      ELSE

! 4.3 If all levels are OK, go to next station
!     ----------------------------------------

               EXIT upper_levels

      ENDIF

      ENDDO upper_levels

      ENDIF single_level

! 5.  RECOUNT LEVELS AND UPDATE STATION INFO
! ==========================================

! 5.1 Number of vertical levels
!     -------------------------

      obs (loop_index) % info % levels = info_levels (obs(loop_index)%surface)

! 5.2 Sounding have at least two levels
!     ----------------------------------

      IF      (obs (loop_index) % info % levels .GT. 1) THEN
               obs (loop_index) % info % is_sound = .TRUE.
      ELSE IF (obs (loop_index) % info % levels .LE. 1) THEN
               obs (loop_index) % info % is_sound = .FALSE.
      ENDIF

      ENDIF stations_valid

      ENDDO stations

!     IF (print_uncomplete) WRITE (0,'(A)') ' '

! 4.4 Close diagnostic file
!     ---------------------

      IF (print_uncomplete) CLOSE (iunit)


! 5.  PRINT DIAGNOSTIC
! ====================
 
      WRITE (UNIT = 0 , FMT = '(2(A,I5,A,/))' ) &
     "Remove  ",nsurfaces," surface stations.", &
     "Remove  ",nuppers,  " upper-air levels."


END SUBROUTINE CHECK_COMPLETNESS
!
!---------------------------------------------------------------------------
FUNCTION check_level (current, missing_r) RESULT (ok)

  USE module_type
  USE module_func

  IMPLICIT NONE

  TYPE (measurement),   POINTER  :: current
  REAL                           :: missing_r
  LOGICAL                        :: ok
!------------------------------------------------------------------------------!

     ok  = .TRUE.  

     IF (ASSOCIATED (current)) THEN

     IF (eps_equal (current % meas % speed       % data, missing_r, 1.)  .AND.&
         eps_equal (current % meas % direction   % data, missing_r, 1.)  .AND.&
         eps_equal (current % meas % temperature % data, missing_r, 1.)  .AND.&
         eps_equal (current % meas % thickness   % data, missing_r, 1.)  .AND.&
         eps_equal (current % meas % dew_point   % data, missing_r, 1.)  .AND.&
         eps_equal (current % meas % rh          % data, missing_r, 1.) .or.  &
         (current % meas % pressure % qc < 0 .and.                            &
          current % meas % height   % qc < 0) ) THEN

         ok = .FALSE.

      ENDIF

      ENDIF

END FUNCTION check_level

!---------------------------------------------------------------------------
FUNCTION check_qc (current) RESULT (ok)

  USE module_type
  USE module_func

  IMPLICIT NONE

  TYPE (measurement),   POINTER  :: current
  LOGICAL                        :: ok

  INCLUDE 'missing.inc'
!------------------------------------------------------------------------------!

     ok  = .TRUE.  

     IF (ASSOCIATED (current)) THEN

     IF (current % meas % height % qc .GE. above_model_lid .or. &
         current % meas %pressure% qc .GE. above_model_lid) THEN

         ok = .FALSE.

      ENDIF

      ENDIF

END FUNCTION check_qc
!------------------------------------------------------------------------------!
END MODULE module_complete
