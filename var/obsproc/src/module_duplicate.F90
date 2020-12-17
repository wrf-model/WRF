MODULE module_duplicate

!-----------------------------------------------------------------------------!
! Sort observations by location and time,
! Merge space duplicate stations (same type, same location, same time),
! Remove time duplicate stations in time (same type & location, different 
! time),
!
!  HISTORY: 
!
! D. GILL,         April 1998
! F. VANDENBERGHE, March 2001
!
!         01/13/2003 - Updated for Profiler obs.           S. R. H. Rizvi
!
!         02/04/2003 - Updated for Buoy     obs.           S. R. H. Rizvi
!
!         02/11/2003 - Reviewed and modified for Profiler
!                      and Buoy obs.                       Y.-R. Guo
!
!         08/31/2004 - Corrected check_duplicate_time for retaining obs 
!                      closer to analysis time             S. R. H. Rizvi
!         09/02/2004 - Reviewed the above correction, and add the
!                      modifications for GPS Ref.          Y.-R. Guo
!         06/30/2006 -   Updated for AIRS retrievals       Syed  RH  Rizvi
!
!         11/09/2006 - add the modifications for GPS Excess Phase  Y.-R. Guo
!------------------------------------------------------------------------------

USE module_type
USE module_func

CONTAINS

!-----------------------------------------------------------------------------!
! SUBROUTINE check_duplicate_loc  (obs,index,num_obs,total_dups,time_analysis, 
!                                  print_duplicate
! SUBROUTINE check_duplicate_time (obs,index,num_obs,total_dups,time_analysis,
!                                  print_duplicate)
!
! -------------------------------------------------------------------------

SUBROUTINE check_duplicate_loc(obs, index, num_obs, total_dups, time_analysis,&
                                print_duplicate)

!  Checks array of reports (obs), which has a sorted index to the reports,
!  to determine if any reports are for the same time/location.  If so,
!  and the data is duplicated exactly in all fields, one is discarded.  If
!  they are from same time/location and data is not identical, data from
!  two reports is merged:  'missing' is replaced by known values; data at
!  different levels is merged into one linked list.

   USE module_date
   USE module_obs_merge
   USE module_per_type

   IMPLICIT NONE

   TYPE ( report ) , INTENT ( INOUT ) , DIMENSION ( : ) :: obs
   INTEGER         , INTENT ( IN )    , DIMENSION ( : ) :: index
   INTEGER         , INTENT ( IN )                      :: num_obs 

   INTEGER                                :: current , &
                                             next    , & 
                                             first   , &
                                             second
   INTEGER         , INTENT ( OUT )       :: total_dups
!  INTEGER         , INTENT ( IN  )       :: date    , &
!                                            time
   LOGICAL,              INTENT (IN)      :: print_duplicate
   CHARACTER (LEN = 19)                   :: time_analysis
   INTEGER                                :: total_valid

   INTEGER :: century_year, month, day
   INTEGER :: hour, minute, seconds
   INTEGER :: date, time
   INTEGER :: iunit, io_error

   CHARACTER (LEN =  80):: filename
   CHARACTER (LEN =  80):: proc_name = "check_duplicate_ob"
   CHARACTER (LEN = 160):: error_message
   LOGICAL              :: fatal, connected

   INTEGER              :: fma, fmb
   CHARACTER (LEN = 40) :: platforma, platformb
   INTEGER              :: nsynopb, nmetarb, nshipsb, &
                           nsoundb, npilotb, nairepb, &
                           nsatemb, nsatobb, ngpspwb, &
                           nssmt1b, nssmt2b, nssmib,  &
                           ntovsb,  notherb, namdarb, &
                           nqscatb, nproflb, ngpsepb, nbuoysb, &
                           ngpszdb, ngpsrfb, nbogusb, &
                           nairsb,  ntamdarb
   INTEGER              :: nsynopa, nmetara, nshipsa, &
                           nsounda, npilota, nairepa, &
                           nsatema, nsatoba, ngpspwa, &
                           nssmt1a, nssmt2a, nssmia,  &
                           ntovsa,  nothera, namdara, &
                           nqscata, nprofla, ngpsepa, nbuoysa, &
                           ngpszda, ngpsrfa, nbogusa, &
                           nairsa,  ntamdara

   INCLUDE 'platform_interface.inc'

!------------------------------------------------------------------------------!

   WRITE (0,'(A)')  &
'------------------------------------------------------------------------------'
   WRITE ( UNIT = 0, FMT = '(A,/)') 'REMOVE DUPLICATE STATIONS BY LOCATION:'

      !  Open diagnostic file

      IF (print_duplicate) THEN

      filename = 'obs_duplicate_loc.diag'
      iunit    = 999

      INQUIRE ( UNIT = iunit, OPENED = connected )

      IF (connected) CLOSE (iunit)

      OPEN (UNIT = iunit , FILE = filename , FORM = 'FORMATTED'  , &
            ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error )

      IF (io_error .NE. 0) THEN
          CALL error_handler (proc_name, &
         "Unable to open output diagnostic file. ", filename, .TRUE.)
      ELSE
          WRITE (UNIT = 0, FMT = '(A,A,/)') &
         "Diagnostics in file ", TRIM (filename)
      ENDIF

      ENDIF

   !  Reset counters

   nsynopb = 0; nmetarb = 0; nshipsb = 0;
   nsoundb = 0; npilotb = 0; nairepb = 0;
   nsatemb = 0; nsatobb = 0; ngpspwb = 0; 
   nssmt1b = 0; nssmt2b = 0; nssmib  = 0;
   ntovsb  = 0; notherb = 0; namdarb = 0;
   nqscatb = 0; nproflb = 0; nbuoysb = 0;
   nqscatb = 0; nproflb = 0; nbuoysb = 0;
   nairsb =0; nairsa = 0
   nsynopa = 0; nmetara = 0; nshipsa = 0;
   nsounda = 0; npilota = 0; nairepa = 0;
   nsatema = 0; nsatoba = 0; ngpspwa = 0;
   nssmt1a = 0; nssmt2a = 0; nssmia  = 0;
   ntovsa  = 0; nothera = 0; namdara = 0;
   nqscata = 0; nprofla = 0; nbuoysa = 0;
   ngpszda = 0; ngpszdb = 0; nbogusa = 0;
   ngpsrfa = 0; ngpsrfb = 0; nbogusb = 0;
   ngpsepa = 0; ngpsepb = 0
   ntamdara= 0; ntamdarb= 0
   !  Count obs per type before merging

count_before:&
   DO current = 1 , num_obs

      first = index(current)

      !  If this obs has been merged with another obs or discarded, skip it.

      IF ( obs(first)%info%discard ) THEN
         CYCLE count_before
      END IF

      !  Count obs present per type before merging

      READ (obs(first)  % info % platform (4:6), '(I3)') fmb

      CALL fm_decoder (fmb, platformb, &
                       synop=nsynopb, ship =nshipsb, metar=nmetarb,&
                       pilot=npilotb, sound=nsoundb, satem=nsatemb,&
                       satob=nsatobb, airep=nairepb, gpspw=ngpspwb,&
                       gpszd=ngpszdb, gpsrf=ngpsrfb, gpsep=ngpsepb,&
                       bogus=nbogusb, &
                       ssmt1=nssmt1b, ssmt2=nssmt2b, ssmi =nssmib, &
                       tovs =ntovsb,  other=notherb, amdar=namdarb,&
                       qscat=nqscatb, profl=nproflb, buoy = nbuoysb,&
                       airs=nairsb,tamdar=ntamdarb)

   ENDDO count_before

   !  Break analysis time into ccyymmdd and hhmnss

   CALL split_date_char (time_analysis, &
                         century_year, month, day, hour, minute, seconds )

   date = century_year * 10000 + month  * 100 + day
   time = hour         * 10000 + minute * 100 + seconds

   !  Count the total number of duplicate reports.

   total_dups  = 0
   total_valid = 0

   !  Merge obs

obsloop:&
   DO current = 1 , num_obs - 1

      first = index(current)

      !  If this obs has been merged with another obs or discarded, skip it.

      IF ( obs(first)%info%discard ) THEN
         CYCLE obsloop
      END IF

      total_valid = total_valid + 1

      !  Get second obs to compare with first; compare first obs to second obs 
      !  until next obs does not match.

      compare: DO next = current + 1 , num_obs

         second = index(next)

         ! Sorted by location, so if locations NE, then no chance of any
         ! more matches with first.

! foo
!        IF (.NOT. (obs(first)%location .EQ. obs(second)%location )) THEN
         IF (.NOT. loc_eq (obs(first), obs(second))) THEN
            CYCLE obsloop
         END IF

         !  If this obs has been merged with another obs or discarded, skip it.

         IF (obs(second)%info%discard) THEN
            CYCLE compare
         END IF

         !  If time fields are not completely identical, go to next observation.
         !  Sort is by location ONLY, not by time; so next+1 may be identical
         !  even though next has different time.

! This statements modifies the obs date and time
!        IF (.NOT. time_eq (obs(first)%valid_time, obs(second)%valid_time, &
!            date, time)) THEN

         IF (.NOT. time_eq_old (obs(first)%valid_time, obs(second)%valid_time))&
         THEN

            IF (print_duplicate) THEN

            error_message  = ' Found multiple times for ' &
            // TRIM ( obs(first)%location%id ) // ' ' &
            // TRIM ( obs(first)%location%name ) // ', ' &
            // TRIM ( obs(first)%valid_time%date_char )  // ' and ' &
            // TRIM ( obs(second)%valid_time%date_char ) // '.'

            WRITE (UNIT = iunit, FMT = '(A)') TRIM (error_message)

!           fatal = .false.
!           CALL error_handler (proc_name,  error_message , "", fatal)

            ENDIF

            CYCLE compare

         END IF

         !  Observations are from same location and time, so merge them.

         CALL merge_obs ( obs(first) , obs(second), print_duplicate, iunit)

         !  Mark second of pair as discarded; data is put in 'first'.  
         !  Note that a duplicate has been found by incrementing the counter.

         obs(second)%info%discard  = .true.
         obs(first)%info%num_dups  = obs(first)%info%num_dups + 1
         total_dups = total_dups + 1

         !  Free up the space for the observation report that is discarded.
         !  Unfortunately, OR NOT!  

! foo
!        CALL dealloc_meas ( obs(second)%surface ) 
         NULLIFY ( obs(second)%surface ) 

!        obs (second)%info%discard = .TRUE.

      END DO compare

   END DO obsloop

   total_valid = total_valid + 1

   !  Count obs per type after merging

count_after:&
   DO current = 1 , num_obs

       first = index(current)

      !  If this obs has been merged with another obs or discarded, skip it.

      IF ( obs(first)%info%discard ) THEN
         CYCLE count_after
      END IF

      !  Count obs present per type before merging

      READ (obs(first)  % info % platform (4:6), '(I3)') fma

      CALL fm_decoder (fma, platforma, &
                       synop=nsynopa, ship =nshipsa, metar=nmetara,&
                       pilot=npilota, sound=nsounda, satem=nsatema,&
                       satob=nsatoba, airep=nairepa, gpspw=ngpspwa,&
                       gpszd=ngpszda, gpsrf=ngpsrfa, gpsep=ngpsepa,&
                       bogus=nbogusa, &
                       ssmt1=nssmt1a, ssmt2=nssmt2a, ssmi =nssmia, &
                       tovs =ntovsa,  other=nothera, amdar=namdara,&
                       qscat=nqscata, profl=nprofla, buoy = nbuoysa, &
                       airs=nairsa, tamdar=ntamdara)

   ENDDO count_after

   nsynops (icor) = nsynopb - nsynopa 
   nmetars (icor) = nmetarb - nmetara
   nshipss (icor) = nshipsb - nshipsa
   nsounds (icor) = nsoundb - nsounda
   namdars (icor) = namdarb - namdara
   npilots (icor) = npilotb - npilota
   naireps (icor) = nairepb - nairepa  
   ntamdar (icor) = ntamdarb- ntamdara
   nsatems (icor) = nsatemb - nsatema
   nsatobs (icor) = nsatobb - nsatoba
   ngpspws (icor) = ngpspwb - ngpspwa
   ngpsztd (icor) = ngpszdb - ngpszda
   ngpsref (icor) = ngpsrfb - ngpsrfa
   ngpseph (icor) = ngpsepb - ngpsepa
   nssmt1s (icor) = nssmt1b - nssmt1a
   nssmt2s (icor) = nssmt2b - nssmt2a
   nssmis  (icor) = nssmib  - nssmia
   ntovss  (icor) = ntovsb  - ntovsa
   nqscats (icor) = nqscatb - nqscata
   nprofls (icor) = nproflb - nprofla
   nbuoyss (icor) = nbuoysb - nbuoysa
   nboguss (icor) = nbogusb - nbogusa
   nairss  (icor) = nairsb  - nairsa 
   nothers (icor) = notherb - nothera

   WRITE (UNIT = 0 , FMT = '(A,I7,A,/)' ) &
  "Found ",total_dups," location duplicate stations that have been merged."

   IF (print_duplicate) CLOSE (iunit)

END SUBROUTINE check_duplicate_loc

!
! -----------------------------------------------------------------------

SUBROUTINE check_duplicate_time (obs, index, num_obs, total_dups, time_analysis,print_duplicate)

!  Checks array of reports (obs), which has a sorted index to the reports,
!  to determine if any reports are for the same location but different time.
!  -If both observations are soundings, then the data closest to the analysis 
!   time is kept
!  -If time differences are equal (obs before and after the analysis time),
!   then the obs valid after the analysis time is kept. 
!  -If one is a sounding and the other a surface observation, then the sounding
!   is kept whatever the time differences are. 
!  
   USE module_date
   USE module_per_type

   IMPLICIT NONE

   TYPE (report),        INTENT (INOUT), DIMENSION (:) :: obs   
   INTEGER,              INTENT (IN),    DIMENSION (:) :: index 
   INTEGER,              INTENT (IN)                   :: num_obs 
   CHARACTER (LEN = 19), INTENT (INOUT)                :: time_analysis
   INTEGER,              INTENT (OUT)                  :: total_dups
   LOGICAL,              INTENT (IN)                   :: print_duplicate
   INTEGER                                             :: total_valid

   INTEGER :: current, next, first, second
   CHARACTER (LEN = 19) :: time_first, time_second
   INTEGER :: itfirst, itsecond
   LOGICAL :: llfirst, llsecond

   TYPE (report)               :: obs_tmp
   TYPE (measurement), POINTER :: current_tmp
   LOGICAL                     :: remove_duplicate = .TRUE.

   CHARACTER (LEN = 80)        :: filename
   CHARACTER (LEN = 32 ), PARAMETER :: proc_name = 'check_duplicate_time '
   LOGICAL                     :: connected
   INTEGER                     :: iunit, io_error

   INCLUDE 'platform_interface.inc'
!------------------------------------------------------------------------------!

              WRITE (0,'(A)')  &
'------------------------------------------------------------------------------'
      WRITE ( UNIT = 0, FMT = '(A,/)') 'REMOVE DUPLICATE STATIONS BY TIME:'

      !  Open diagnostic file

      IF (print_duplicate) THEN

      filename = 'obs_duplicate_time.diag_'//time_analysis
      iunit    = 999

      INQUIRE ( UNIT = iunit, OPENED = connected )

      IF (connected) CLOSE (iunit)

      OPEN (UNIT = iunit , FILE = filename , FORM = 'FORMATTED'  , &
            ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error )

      IF (io_error .NE. 0) THEN
          CALL error_handler (proc_name, &
         "Unable to open output diagnostic file. ", filename, .TRUE.)
      ELSE
          WRITE (UNIT = 0, FMT = '(A,A,/)') &
         "Diagnostics in file ", TRIM (filename)
      ENDIF

      ENDIF

   !  Count the total number of duplicate reports.

   total_valid = 0
   total_dups  = 0

   obsloop: DO current = 1 , num_obs - 1

      first = index(current)

      !  If this obs has been merged with another obs or discarded, skip it.

      IF ( obs(first)%info%discard ) THEN
         CYCLE obsloop
      END IF

      total_valid = total_valid + 1

      !  Get second obs to compare with first; compare first obs to second obs 
      !  until next obs does not match.

      compare: DO next = current + 1 , num_obs

         second = index(next)

         ! Sorted by location, so if locations NE, then no chance of any
         ! more matches with first.

         IF ( .NOT. loc_eq ( obs(first) , obs(second) ) ) THEN
            CYCLE obsloop
         END IF

         !  If this obs has been merged with another obs or discarded, skip it.

         IF ( obs(second)%info%discard ) THEN
            CYCLE compare
         END IF

         !  If time fields are not completely identical, they are duplicated

time_difference: &
         IF (.NOT. time_eq_old (obs(first)%valid_time, obs(second)%valid_time))&
         THEN

         total_dups = total_dups + 1
         llfirst  = .FALSE.
         llsecond = .FALSE.

         IF (print_duplicate) THEN

         WRITE (UNIT = iunit, FMT = '(/,A)') 'Found duplicated stations:'

         WRITE (UNIT = iunit , FMT = '(A,2x,A,A5,A,A23,2F9.3,A,L10)') &
        'Station 1 name and ID = ' , &
         TRIM (obs(first)%info%platform),       &
         TRIM (obs(first)%location%id ) , ' ' , &
         TRIM (obs(first)%location%name ) ,     &
               obs(first)%location%latitude ,   &
               obs(first)%location%longitude, ' ',&
               obs (first)%info%is_sound

         WRITE (UNIT = iunit , FMT = '(A,2x,A,A5,A,A23,2F9.3,A,L10)') &
        'Station 2 name and ID = ' , &
         TRIM (obs(second)%info%platform),       &
         TRIM (obs(second)%location%id ) , ' ' , &
         TRIM (obs(second)%location%name ) ,     &
               obs(second)%location%latitude ,   &
               obs(second)%location%longitude,' ',&
               obs(second)%info%is_sound

         ENDIF

         ! First we check the nature of the observation: sounding or surface

is_sound:IF (      obs (first)  % info % is_sound .AND.  &
              .NOT. obs (second) % info % is_sound) THEN

             llfirst  = .TRUE.
             llsecond = .FALSE.

         ELSE IF (.NOT. obs (first)  % info % is_sound .AND. & 
                        obs (second) % info % is_sound) THEN

             llfirst  = .FALSE.
             llsecond = .TRUE.

         ELSE is_sound

         ! Second we test the time difference between the analysis time and 
         ! the observations time 
         ! (negative for before analysis time, positive for after analysis time)

           WRITE (time_first, FMT='(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)') &
            obs (first) % valid_time % date_char ( 1: 4), &
            obs (first) % valid_time % date_char ( 5: 6), &
            obs (first) % valid_time % date_char ( 7: 8), &
            obs (first) % valid_time % date_char ( 9:10), &
            obs (first) % valid_time % date_char (11:12), &
            obs (first) % valid_time % date_char (13:14)

           WRITE (time_second, FMT='(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)') &
            obs (second) % valid_time % date_char ( 1: 4), &
            obs (second) % valid_time % date_char ( 5: 6), &
            obs (second) % valid_time % date_char ( 7: 8), &
            obs (second) % valid_time % date_char ( 9:10), &
            obs (second) % valid_time % date_char (11:12), &
            obs (second) % valid_time % date_char (13:14)

            CALL GETH_IDTS (time_first,  time_analysis, itfirst)
            CALL GETH_IDTS (time_second, time_analysis, itsecond)

            IF (print_duplicate) THEN

            WRITE (UNIT = iunit, FMT = '(2A)') 'Analysis  time = ',time_analysis

            IF (itfirst .GE. 0) THEN
            WRITE (UNIT = iunit, FMT = '(3A,I6,A)') &
                                 'Station 1 time = ',time_first, &
                                               ' = ta + ',itfirst,'s'
            ELSE
            WRITE (UNIT = iunit, FMT = '(3A,I6,A)') &
                                 'Station 1 time = ',time_first, &
                                               ' = ta - ',ABS (itfirst),'s'
            ENDIF

            IF (itsecond .GE. 0) THEN
            WRITE (UNIT = iunit, FMT = '(3A,I6,A)') &
                                 'Station 2 time = ',time_second,&
                                               ' = ta + ',itsecond,'s'
            ELSE
            WRITE (UNIT = iunit, FMT = '(3A,I6,A)') &
                                 'Station 2 time = ',time_second,&
                                               ' = ta - ',ABS (itsecond),'s'
            ENDIF

            ENDIF

            ! Time difference must be different

time_equal: IF (itfirst .EQ. itsecond) THEN
                WRITE (0,'(A)')  ' Internal error:'
                WRITE (0,'(2A)') ' first_time  = ',time_first
                WRITE (0,'(2A)') ' second_time = ',time_second
                STOP ' in check_duplicate_time.F'
            ENDIF time_equal

time_different: IF (abs(itfirst) .LT. abs(itsecond)) THEN
                ! first obs is close to analysis time and so retain first 
                    llfirst  = .TRUE.
                    llsecond = .FALSE.
                ELSE IF (abs(itfirst) .GT. abs(itsecond)) THEN
                ! second obs is close to analysis time and so retain second
                    llfirst  = .FALSE.
                    llsecond = .TRUE.
                ELSE IF (abs(itfirst) .EQ. abs(itsecond)) THEN
               ! Two obervations are at exactly time from analysis time but on opposit side
               ! Retain the one which is after analysis time
                    IF ( itfirst >= 0.)  THEN
                    llfirst  = .TRUE.
                    llsecond = .FALSE.
                    ELSE
                    llfirst  = .FALSE.
                    llsecond = .TRUE.
                    END IF
                ENDIF time_different

         END IF is_sound

         !  Remove duplicate sounding

         IF (remove_duplicate) THEN

         IF (llfirst) THEN

             IF (print_duplicate) THEN
              WRITE (UNIT = iunit, FMT = '(A)') &
             'Keep station 1 and reject station 2.'
             ENDIF

             READ (obs(second) % info % platform (4:6), '(I3)') fm

             CALL fm_decoder (fm, platform, &
                              synop=nsynops (icor), ship =nshipss (icor), &
                              metar=nmetars (icor), pilot=npilots (icor), &
                              sound=nsounds (icor), satem=nsatems (icor), &
                              satob=nsatobs (icor), airep=naireps (icor), &
                              gpspw=ngpspws (icor), gpszd=ngpsztd (icor), &
                              gpsrf=ngpsref (icor), gpsep=ngpseph (icor), &
                              ssmt1=nssmt1s (icor), bogus=nboguss (icor), &
                              ssmt2=nssmt2s (icor), ssmi =nssmis  (icor), &
                              tovs =ntovss  (icor), other=nothers (icor), &
                              amdar=namdars (icor), qscat=nqscats (icor), &
                              profl=nprofls (icor), buoy =nbuoyss (icor), &
                              airs =nairss (icor) , tamdar=ntamdar(icor)  )

             obs (second)%info%discard  = .true.
             obs (first)%info%num_dups  = obs (first)%info%num_dups + 1

             NULLIFY (obs(second)%surface ) 

             CYCLE compare


         ELSE IF (llsecond) THEN

             IF (print_duplicate) THEN
              Write (UNIT = iunit, FMT = '(A)') &
             'Keep station 2 and reject station 1.'
             ENDIF


             READ (obs(first) % info % platform (4:6), '(I3)') fm

             CALL fm_decoder (fm, platform, &
                              synop=nsynops (icor), ship =nshipss (icor), &
                              metar=nmetars (icor), pilot=npilots (icor), &
                              sound=nsounds (icor), satem=nsatems (icor), &
                              satob=nsatobs (icor), airep=naireps (icor), &
                              gpspw=ngpspws (icor), gpszd=ngpsztd (icor), &
                              gpsrf=ngpsref (icor), gpsep=ngpseph (icor), &
                              ssmt1=nssmt1s (icor), bogus=nboguss (icor), &
                              ssmt2=nssmt2s (icor), ssmi =nssmis  (icor), &
                              tovs =ntovss  (icor), other=nothers (icor), &
                              amdar=namdars (icor), qscat=nqscats (icor), &
                              profl=nprofls (icor), buoy =nbuoyss (icor), &
                              airs =nairss (icor),  tamdar=ntamdar(icor)  )

             obs (first)%info%discard    = .true.
             obs (second)%info%num_dups  = obs (second)%info%num_dups + 1

             NULLIFY ( obs(first)%surface ) 

             CYCLE obsloop

         ENDIF

         ELSE

         !  Order duplicate soundings by time incresing

         IF (llfirst) THEN

         !  If first observation is before second, do nothing

         ELSE IF (llsecond) THEN

         !  If second observation is before first, swap

              obs_tmp      = obs (second)
              obs (second) = obs (first)
              obs (first)  = obs_tmp

         ENDIF

         ENDIF

      ENDIF time_difference

       !  Free up the space for the observation report that is discarded.
       !  Unfortunately, OR NOT!  
! foo
!        CALL dealloc_meas ( obs(second)%surface ) 
!        NULLIFY ( obs(second)%surface ) 


      END DO compare

   END DO obsloop

   IF (print_duplicate) CLOSE (iunit)

   total_valid = total_valid + 1

   WRITE (UNIT = 0 , FMT = '(A,I7,A,/)' ) &
  "Found ",total_dups," time duplicate stations that have been removed."

END SUBROUTINE check_duplicate_time

END MODULE module_duplicate
