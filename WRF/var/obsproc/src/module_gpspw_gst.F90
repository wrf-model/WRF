MODULE module_gpspw_gst

!------------------------------------------------------------------------------!
! Read in integrated precipitable water measurement from GPS stations coming
! from GST (gem file).
!
! F. VANDENBERGHE, March 2001
!------------------------------------------------------------------------------!

USE module_date
USE module_type
USE module_inside
USE module_per_type


!------------------------------------------------------------------------------!

!   Maximum Number of stations to be read

    INTEGER, PARAMETER      :: nstations = 62

!     Format of GPS precipitable water observations

      CHARACTER (LEN=80)    :: fmt_read
      PARAMETER (fmt_read = '(1X,A4,2X,A13,2X,F6.2,5(2X,F6.1),:)')

!   Missing flag at reading

    REAL, PARAMETER         :: missing_read = -9999.0

!   Data structure for input file reading

    TYPE station_gpspw_gst

           CHARACTER (LEN =  4)      :: name
           CHARACTER (LEN = 13)      :: date
           REAL                      :: pw_mm
           REAL                      :: err_pw
           REAL                      :: zwd_mm 
           REAL                      :: pres_mb 
           REAL                      :: tem_c
           REAL                      :: rh

    END TYPE station_gpspw_gst


CONTAINS
!------------------------------------------------------------------------------!

SUBROUTINE read_gpspw_gst (filename, filenum, obs, n_obs,                  &
                           total_number_of_obs, fatal_if_exceed_max_obs,   &
                           time_window_min, time_analysis, time_window_max,&
                           ins, jew, missing_flag, print_gpspw_read)
!------------------------------------------------------------------------------!

      IMPLICIT NONE

      CHARACTER (LEN=*),            INTENT (in)  :: filename
      INTEGER,                      INTENT (in)  :: filenum
      INTEGER,                      INTENT (inout) :: n_obs
      INTEGER,                      INTENT (in)  :: total_number_of_obs
      LOGICAL,                      INTENT (iN)  :: fatal_if_exceed_max_obs
      TYPE (report), DIMENSION (:), INTENT (out) :: obs
      INTEGER,                      INTENT (in)  :: ins, jew
      CHARACTER (LEN = 19),         INTENT (in)  :: time_window_min
      CHARACTER (LEN = 19),         INTENT (in)  :: time_analysis
      CHARACTER (LEN = 19),         INTENT (in)  :: time_window_max
      REAL,                         INTENT (out) :: missing_flag
      LOGICAL,                      INTENT (in)  :: print_gpspw_read

      CHARACTER (LEN = 80)                       :: header
      CHARACTER (LEN = 19)                       :: date_mm5
      CHARACTER (LEN = 14)                       :: date_char
      CHARACTER (LEN =  4)                       :: previous_name
      LOGICAL                                    :: outside_domain
      LOGICAL                                    :: outside_window
      LOGICAL                                    :: outside, fatal
      INTEGER                                    :: is, io_error
      INTEGER                                    :: obs_num, ilines
      INTEGER                                    :: num_empty, num_outside
      INTEGER                                    :: num_unknown
      INTEGER                                    :: ccyy, mm, dd, julian
      REAL                                       :: lat, lon, elv
      REAL                                       :: yic, xjc, yid, xjd

      CHARACTER (LEN = 80)                       :: file_name
      CHARACTER (LEN = 32), PARAMETER            :: proc_name ='read_gpspw_gst '
      CHARACTER ( LEN = 160)                     :: error_message
      INTEGER                                    :: iunit

      TYPE (station_gpspw_gst)                   :: read_level

      INCLUDE 'missing.inc'

!------------------------------------------------------------------------------!

             WRITE (UNIT = 0, FMT = '(A)')  &
"------------------------------------------------------------------------------"

      WRITE (UNIT = 0, FMT = '(A,A,/)') &
     "READ GPS OBSERVATIONS IN FILE: ", TRIM (filename)


! 0.  OPEN DIGANOSTIC FILE
! ========================

      IF (print_gpspw_read) THEN

          file_name = 'obs_gpspw_read.diag'
          iunit    = 999

          OPEN (UNIT = iunit , FILE = file_name , FORM = 'FORMATTED'  , &
                ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error )

          IF (io_error .NE. 0 ) THEN
              CALL error_handler (331001, proc_name(1:32) //  &
             "Unable to open output diagnostic file " // TRIM (file_name), &
              .true., .false.)
          ELSE
              WRITE (UNIT = 0, FMT = '(A,A,/)') &
             "Diagnostics in file ", TRIM (file_name)
         ENDIF

      ENDIF

! 1. OPEN INPUT FILE AND READ HEADER 
! ==================================

! 1.1 Open input file
!     ---------------

      OPEN (UNIT = filenum, FILE = filename, FORM='formatted', ACTION = 'read',&
            IOSTAT = io_error)

      IF (io_error .NE. 0 ) THEN
          CALL error_handler (331001, proc_name(1:32) //  &
         "Unable to open input file " // TRIM (filename), .true., .false.)
      ENDIF


! 1.2 Skip first line header
!     ----------------------

      REWIND (filenum)

      READ (UNIT = filenum, FMT = '(A)', IOSTAT = io_error) header

      !  One line read

      ilines = 1

! 2.  READ DATA
! =============

! 2.1 Reset read variables
!     --------------------

      previous_name = "****"
      obs_num       = n_obs
      num_empty     = 0
      num_outside   = 0
      num_unknown   = 0


! 2.  READ PW DATA:
! =================

read_obs:&

      DO WHILE (io_error .EQ. 0)

! 2.1 Read first time level of station
!     --------------------------------

      READ (UNIT = filenum, FMT = fmt_read, IOSTAT = io_error) read_level
!     WRITE (UNIT = iunit, FMT = fmt_read) read_level

      IF (io_error /= 0 ) THEN 
          EXIT read_obs
      ENDIF

      !  1 line read

      ilines = ilines + 1

! 2.2 Station code
!     ------------

      CALL name_to_number (is, read_level % name, lat, lon, elv)


! 2.3 If code unrecognized skip it
!     ----------------------------

      IF ((is .LE. 0) .OR. (is .GT. nstations)) THEN

          IF ((print_gpspw_read) .AND. &
              (read_level % name /= previous_name)) THEN

               WRITE (UNIT = iunit, FMT = '(A,A5,2X,A5,18X,2F9.3,1X,A)') &
                    'Found Name and ID =' , &
                     TRIM (read_level % name), 'GTS00', lat, lon, '=> UNNOWN'

               num_unknown   = num_unknown + 1        

               previous_name = read_level % name

          ENDIF


          CYCLE read_obs

      ENDIF

! 2.5 Correct date (remove 60mn, 24h etc.)
!     ------------------------------------

      CALL correct_date (read_level % date)

! 2.6 Record date at format CCYYMMDDHH AND CCYY-MM-DD_HH:MN:SS
!     ---------------------------------------------------------

      WRITE (date_char,&
           '(A4,A2,A2,A2,A2,A2)') &
             read_level % date (1:4), read_level % date (5:6), &
             read_level % date (7:8), read_level % date (10:11),&
             read_level % date (12:13), '00'


      WRITE (date_mm5,&
           '(A4,"-",A2,"-",A2,"_",A2,":",A2,":"A2)') &
             read_level % date (1:4), read_level % date (5:6), &
             read_level % date (7:8), read_level % date (10:11),&
             read_level % date (12:13), '00'

! 2.7 Julian days
!     -----------

      READ (read_level % date (1:4),'(I2)') ccyy
      READ (read_level % date (5:6),'(I2)') mm
      READ (read_level % date (7:8),'(I2)') dd

      CALL julian_day (ccyy,mm,dd,julian,1)

! 2.8 Check domain and time window
!     ----------------------------

      CALL inside_domain (lat, lon, ins , jew , outside_domain , &
                          xjc, yic, xjd, yid)

      CALL inside_window (date_char, time_window_min, time_window_max, &
                          outside_window, iunit)

      outside = outside_domain .OR. outside_window

! 2.9 If station is not in domain or record not within time window, skip it
!     ---------------------------------------------------------------------

      IF (outside) THEN

          num_outside = num_outside + 1

          CYCLE read_obs

      ENDIF

! 2.10 If station is empty, skip it
!      ----------------------------

       IF (read_level % pw_mm <= missing_read) then

           num_empty = num_empty + 1

           CYCLE read_obs

       ENDIF

! 2.11 If station is in domain and record within time window and can be stored
!      -----------------------------------------------------------------------

      IF ((obs_num .GE. total_number_of_obs) .AND.  &
          (fatal_if_exceed_max_obs)) THEN

            error_message(1:60)  = &
           'Too many obs for the NAMELIST value of max_number_of_obs = '
            WRITE (error_message(61:67),'(I7)')  total_number_of_obs
            fatal = .TRUE.
            CALL error_handler (proc_name, &
                 error_message (1:60), error_message (61:),fatal)

      ELSE IF ((obs_num .GE. total_number_of_obs) .AND. &
               (.NOT. fatal_if_exceed_max_obs))    THEN

            error_message(1:60)  = &
           'Too many obs for the NAMELIST value of max_number_of_obs = '
            WRITE (error_message(61:67),'(I7)')  total_number_of_obs
            fatal = .FALSE.

            CALL error_handler (proc_name, &
                 error_message (1:60), error_message (61:),fatal)

            CLOSE ( iunit )
            IF (print_gpspw_read) CLOSE ( iunit )
            EXIT read_obs

      END IF


! 2.12 If station is in domain and record within time window, store it
!      ---------------------------------------------------------------

       obs_num = obs_num + 1
 
! 2.13 Station name, ID and location
!      ----------------------------

       WRITE (obs (obs_num) % location % id, '(A3,I2)') 'GTS',is
       obs (obs_num) % location % name =  TRIM (read_level % name)

       obs (obs_num) % location % latitude  = lat
       obs (obs_num) % location % longitude = lon
       obs (obs_num) % location % xjc = xjc
       obs (obs_num) % location % xjd = xjd
       obs (obs_num) % location % yic = yic
       obs (obs_num) % location % yid = yid

! 2.14 Station general info
!      --------------------

       obs (obs_num) % info % platform  = 'FM-111 GPSPW'
       obs (obs_num) % info % source    = 'TWP FROM GST'
       obs (obs_num) % info % elevation =  elv
       obs (obs_num) % info % discard   = .false.
       obs (obs_num) % info % is_sound  = .false.

! 2.15 Record date and time
!      --------------------

       obs (obs_num) % valid_time % date_char = date_char
       obs (obs_num) % valid_time % date_mm5  = date_mm5
       obs (obs_num) % valid_time % julian    = julian

! 2.16 Total precipitable water from mm to cm
!      --------------------------------------

       IF (read_level % pw_mm <= missing_read) then

           num_empty = num_empty + 1

           obs(obs_num) % ground % pw % data  =  missing_r
           obs(obs_num) % ground % pw % qc    =  missing 
           obs(obs_num) % ground % pw % error =  missing_r

       ELSE

           obs(obs_num) % ground % pw % data  =  read_level % pw_mm / 10.
           obs(obs_num) % ground % pw % qc    =  0
           obs(obs_num) % ground % pw % error =  read_level % err_pw / 10.

       ENDIF


! 2.17 Set sea level pressure to surface pressure
!      ------------------------------------------

       IF (read_level % pres_mb <= missing_read) then

           obs(obs_num) % ground % slp % data  =  missing_r
           obs(obs_num) % ground % slp % qc    =  missing 
           obs(obs_num) % ground % slp % error =  missing_r

       ELSE

           obs(obs_num) % ground % slp % data  =  read_level % pres_mb * 100.
           obs(obs_num) % ground % slp % qc    =  0
           obs(obs_num) % ground % slp % error =  200.
 
       ENDIF


! 2.20 There's no upper level for GPSPW, unlink pointer
!      ------------------------------------------------

       NULLIFY (obs (obs_num) % surface)

! 2.21 Print station information
!      -------------------------

       IF ((print_gpspw_read) .AND. &
           (read_level % name /= previous_name)) THEN

            WRITE (UNIT = iunit, FMT = '(A,A5,1X,A23,2F9.3,1X,A)') &
                  'Found Name and ID = ' , &
                   obs (obs_num) % location % name,        &
                   obs (obs_num) % location % id,          &
                   obs (obs_num) % location % latitude,    &
                   obs (obs_num) % location % longitude,   &
                  '=> GPS PW'
       ENDIF

      previous_name = read_level % name

      ENDDO read_obs


! 3.  READ IS COMPLETE, CLOSE INPUT FILE AND WRITE DIAGNOSTIC
! ===========================================================

! 3.1 Close input file
!     ----------------

      WRITE (UNIT = 0, FMT = '(A)') 'Have reached end of observations file.'

      CLOSE (filenum)

! 3.2 Total number of observation accumulated per type
!     ------------------------------------------------

      ngpspws (icor+0) = obs_num+num_empty+num_outside-n_obs
      ngpspws (icor+1) = num_empty
      ngpspws (icor+2) = num_outside

! 3.3 Print number of reports
!     -----------------------

      WRITE (UNIT = 0, FMT = '(/,A)')  &
'------------------------------------------------------------------------------'

      WRITE (UNIT = 0, FMT = '(A)') 'GPS PW OBSERVATIONS READ:'

      WRITE (UNIT = 0,FMT = '(/,A,I6)') &
     'Number of gpspw reports: ',ngpspws (icor)

      WRITE (UNIT = 0 , FMT = '(/,4(A,I8,/))' ) &

          "Number of observations read:          ",obs_num+    &
                                                   num_empty+  &
                                                   num_outside-&
                                                   n_obs,      &
          "Number of empty observations:         ",num_empty,  &
          "Number of out of domain observations: ",num_outside,&
          "Number of observations for ingestion: ",obs_num-n_obs


! 3.4 Total number of observation accumulated
!     ---------------------------------------

      n_obs = obs_num

! 3.5 Update missing data flag
!     ------------------------

      missing_flag = missing_r

! 3.6 Close diagnostic file, if open
!     ------------------------------

      IF (print_gpspw_read) CLOSE (iunit)

    END SUBROUTINE read_gpspw_gst


!------------------------------------------------------------------------------!

SUBROUTINE CORRECT_DATE (DATE)

CHARACTER (LEN = 13) :: DATE

SELECT CASE (DATE (10:13))

 CASE ('0060'); DATE (10:13) = '0100'
 CASE ('0160'); DATE (10:13) = '0200'
 CASE ('0260'); DATE (10:13) = '0300'
 CASE ('0360'); DATE (10:13) = '0400'
 CASE ('0460'); DATE (10:13) = '0500'
 CASE ('0560'); DATE (10:13) = '0600'
 CASE ('0660'); DATE (10:13) = '0700'
 CASE ('0760'); DATE (10:13) = '0800'
 CASE ('0860'); DATE (10:13) = '0900'
 CASE ('0960'); DATE (10:13) = '1000'
 CASE ('1060'); DATE (10:13) = '1100'
 CASE ('1160'); DATE (10:13) = '1200'
 CASE ('1260'); DATE (10:13) = '1300'
 CASE ('1360'); DATE (10:13) = '1400'
 CASE ('1460'); DATE (10:13) = '1500'
 CASE ('1560'); DATE (10:13) = '1600'
 CASE ('1660'); DATE (10:13) = '1700'
 CASE ('1760'); DATE (10:13) = '1800'
 CASE ('1860'); DATE (10:13) = '1900'
 CASE ('1960'); DATE (10:13) = '2000'
 CASE ('2060'); DATE (10:13) = '2100'
 CASE ('2160'); DATE (10:13) = '2200'
 CASE ('2260'); DATE (10:13) = '2300'
 CASE ('2360');
      WRITE (0,'(/,A,A,/)') ' Cannot correct date ', TRIM (date)
 END SELECT

 RETURN

END SUBROUTINE CORRECT_DATE
!------------------------------------------------------------------------------!
SUBROUTINE number_to_name (is,nam,lat,lon,elv)

    INTEGER             :: is
    CHARACTER (LEN = 8) :: nam
    REAL                :: lat, lon, elv

! '(A9,3(A8;13.8)'

!SITE       LAT          LON       MSL[m] +- 7 cm

SELECT CASE (is)

! name="cors.msl"

 CASE ( 1); nam = 'CCV3'; lat =   28.46022742; lon =  -80.54523032; elv =    5.73440000
 CASE ( 2); nam = 'MOB1'; lat =   30.22751840; lon =  -88.02410277; elv =    9.31860000
 CASE ( 3); nam = 'MNP1'; lat =   41.06710924; lon =  -71.86044494; elv =   21.34320000
 CASE ( 4); nam = 'ARP3'; lat =   27.83835188; lon =  -97.05895714; elv =   10.35250000
 CASE ( 5); nam = 'GAL1'; lat =   29.32988075; lon =  -94.73680822; elv =    9.93060000
 CASE ( 6); nam = 'ENG1'; lat =   29.87896260; lon =  -89.94172710; elv =    7.80510000
 CASE ( 7); nam = 'EKY1'; lat =   27.60041756; lon =  -82.76032221; elv =    7.36550000
 CASE ( 8); nam = 'MLF1'; lat =   32.09026075; lon =  -87.39181193; elv =   37.15070000
 CASE ( 9); nam = 'MIA3'; lat =   25.73280971; lon =  -80.16016933; elv =   11.71400000
 CASE (10); nam = 'MEM2'; lat =   35.46570750; lon =  -90.20604725; elv =   74.79740000
 CASE (11); nam = 'VIC1'; lat =   32.33141910; lon =  -90.91979003; elv =   36.74060000
 CASE (12); nam = 'SHK1'; lat =   40.47149210; lon =  -74.01159656; elv =    8.28870000
 CASE (13); nam = 'KYW1'; lat =   24.58227239; lon =  -81.65303210; elv =   10.03320000
 CASE (14); nam = 'CHA1'; lat =   32.75756603; lon =  -79.84287364; elv =    5.41540000
 CASE (15); nam = 'FMC1'; lat =   34.69739589; lon =  -76.68311967; elv =   23.57790000
 CASE (16); nam = 'MIA4'; lat =   25.73280980; lon =  -80.16016956; elv =   11.72390000
 CASE (17); nam = 'MOR1'; lat =   40.78952494; lon =  -72.74628989; elv =    8.12490000
 CASE (18); nam = 'DRV1'; lat =   36.95865624; lon =  -76.55664184; elv =   13.90030000
 CASE (19); nam = 'CLK1'; lat =   44.93562098; lon =  -97.96068617; elv =  438.95100000
 CASE (20); nam = 'WHN1'; lat =   42.73931476; lon = -103.32877941; elv = 1069.94000000
 CASE (21); nam = 'SAV1'; lat =   32.13859989; lon =  -81.69633426; elv =   39.46080000
 CASE (22); nam = 'BLKV'; lat =   37.20601771; lon =  -80.41452462; elv =  637.70400000

!name=ffsl.llh"

 CASE (23); nam = 'NLGN'; lat =   42.20674343; lon = -97.79529896 ; elv =  523.03000000
 CASE (24); nam = 'SLAI'; lat =   41.90058327; lon = -93.69887039 ; elv =  317.44000000
 CASE (25); nam = 'LMNO'; lat =   36.68544654; lon = -97.48073652 ; elv =  306.45000000
 CASE (26); nam = 'AZCN'; lat =   36.83979329; lon = -107.91096134; elv = 1881.19000000
 CASE (27); nam = 'BLK0'; lat =   37.20601771; lon =  -80.41452462; elv =  637.62000000
 CASE (28); nam = 'BLMM'; lat =   36.87978303; lon =  -89.97252840; elv =  128.19000000
 CASE (29); nam = 'BLRW'; lat =   43.23025339; lon =  -90.53077843; elv =  222.27000000
 CASE (30); nam = 'CNWM'; lat =   37.52283396; lon =  -92.70349065; elv =  388.02000000
 CASE (31); nam = 'DQUA'; lat =   34.11065741; lon =  -94.28989785; elv =  196.58000000
 CASE (32); nam = 'DSRC'; lat =   39.99143134; lon = -105.26103159; elv = 1669.06000000
 CASE (33); nam = 'FBYN'; lat =   40.07687182; lon =  -97.31281998; elv =  430.46000000
 CASE (34); nam = 'GDAC'; lat =   37.77545215; lon = -102.18001253; elv = 1155.85000000
 CASE (35); nam = 'HBRK'; lat =   38.30464964; lon =  -97.29352379; elv =  439.85000000
 CASE (36); nam = 'HKLO'; lat =   35.68275454; lon =  -95.86338381; elv =  218.93000000
 CASE (37); nam = 'HVLK'; lat =   37.65146778; lon =  -99.10675913; elv =  646.27000000
 CASE (38); nam = 'JTNT'; lat =   33.01720036; lon = -100.97714513; elv =  709.24000000
 CASE (39); nam = 'LKWY'; lat =   44.56507693; lon = -110.40021843; elv = 2430.74000000
 CASE (40); nam = 'LTHM'; lat =   39.57595873; lon =  -94.17015452; elv =  293.41000000
 CASE (41); nam = 'MBWW'; lat =   41.90358636; lon = -106.18653971; elv = 1996.07000000
 CASE (42); nam = 'MRRN'; lat =   42.90428687; lon = -101.69641978; elv =  988.53000000
 CASE (43); nam = 'NDBC'; lat =   30.35628084; lon =  -89.61027680; elv =   14.37000000
 CASE (44); nam = 'NDSK'; lat =   37.38077849; lon =  -95.63823621; elv =  254.72000000
 CASE (45); nam = 'OKOM'; lat =   34.09026377; lon =  -88.86247819; elv =  131.87000000
 CASE (46); nam = 'PATT'; lat =   31.77827283; lon =  -95.71850951; elv =  119.80000000
 CASE (47); nam = 'PLTC'; lat =   40.18159312; lon = -104.72593635; elv = 1518.71000000
 CASE (48); nam = 'PRCO'; lat =   34.97986594; lon =  -97.51924837; elv =  329.58000000
 CASE (49); nam = 'RWDN'; lat =   40.08670106; lon = -100.65348014; elv =  796.73000000
 CASE (50); nam = 'SEAW'; lat =   47.68700546; lon = -122.25630089; elv =   16.42000000
 CASE (51); nam = 'SIO3'; lat =   32.86470196; lon = -117.25040572; elv =   69.58000000
 CASE (52); nam = 'SYCN'; lat =   43.11572518; lon =  -76.09339750; elv =  122.14000000
 CASE (53); nam = 'TCUN'; lat =   35.08501981; lon = -103.60911953; elv = 1240.06000000
 CASE (54); nam = 'VCIO'; lat =   36.07170788; lon =  -99.21731449; elv =  650.17000000
 CASE (55); nam = 'WDLM'; lat =   44.67221307; lon =  -95.44858064; elv =  319.43000000
 CASE (56); nam = 'WLCI'; lat =   40.80840811; lon =  -87.05198607; elv =  212.42000000
 CASE (57); nam = 'WNCI'; lat =   39.66461362; lon =  -90.47674775; elv =  172.03000000
 CASE (58); nam = 'WNFL'; lat =   31.89722826; lon =  -92.78191095; elv =   92.70000000
 CASE (59); nam = 'WSMN'; lat =   32.40713676; lon = -106.34982961; elv = 1226.17000000
 CASE (60); nam = 'CENA'; lat =   65.49816527; lon = -144.67763059; elv =  271.78000000
 CASE (61); nam = 'GNAA'; lat =   62.11238351; lon = -145.97021718; elv =  584.78000000
 CASE (62); nam = 'TLKA'; lat =   62.30765593; lon = -150.42029462; elv =  151.28000000

CASE DEFAULT; nam = '0000'; lat =    0.00000000; lon =    0.00000000; elv =    0.00000000
!             WRITE (0,'(A,I3)') ' Unknown station sequence number ',is
!             STOP               ' in station_sequence'

END SELECT

RETURN 

END SUBROUTINE number_to_name                        
!------------------------------------------------------------------------------!

SUBROUTINE name_to_number (is,nam,lat,lon,elv)

    CHARACTER (LEN = 4) :: nam
    INTEGER             :: is
    REAL                :: lat, lon, elv

! '(A9,3(A8;13.8)'

!SITE       LAT          LON       MSL[m] +- 7 cm

SELECT CASE (nam)

! name="cors.msl"

 CASE ('CCV3'); is =  1; lat =   28.46022742; lon =  -80.54523032; elv =    5.73440000
 CASE ('MOB1'); is =  2; lat =   30.22751840; lon =  -88.02410277; elv =    9.31860000
 CASE ('MNP1'); is =  3; lat =   41.06710924; lon =  -71.86044494; elv =   21.34320000
 CASE ('ARP3'); is =  4; lat =   27.83835188; lon =  -97.05895714; elv =   10.35250000
 CASE ('GAL1'); is =  5; lat =   29.32988075; lon =  -94.73680822; elv =    9.93060000
 CASE ('ENG1'); is =  6; lat =   29.87896260; lon =  -89.94172710; elv =    7.80510000
 CASE ('EKY1'); is =  7; lat =   27.60041756; lon =  -82.76032221; elv =    7.36550000
 CASE ('MLF1'); is =  8; lat =   32.09026075; lon =  -87.39181193; elv =   37.15070000
 CASE ('MIA3'); is =  9; lat =   25.73280971; lon =  -80.16016933; elv =   11.71400000
 CASE ('MEM2'); is = 10; lat =   35.46570750; lon =  -90.20604725; elv =   74.79740000
 CASE ('VIC1'); is = 11; lat =   32.33141910; lon =  -90.91979003; elv =   36.74060000
 CASE ('SHK1'); is = 12; lat =   40.47149210; lon =  -74.01159656; elv =    8.28870000
 CASE ('KYW1'); is = 13; lat =   24.58227239; lon =  -81.65303210; elv =   10.03320000
 CASE ('CHA1'); is = 14; lat =   32.75756603; lon =  -79.84287364; elv =    5.41540000
 CASE ('FMC1'); is = 15; lat =   34.69739589; lon =  -76.68311967; elv =   23.57790000
 CASE ('MIA4'); is = 16; lat =   25.73280980; lon =  -80.16016956; elv =   11.72390000
 CASE ('MOR1'); is = 17; lat =   40.78952494; lon =  -72.74628989; elv =    8.12490000
 CASE ('DRV1'); is = 18; lat =   36.95865624; lon =  -76.55664184; elv =   13.90030000
 CASE ('CLK1'); is = 19; lat =   44.93562098; lon =  -97.96068617; elv =  438.95100000
 CASE ('WHN1'); is = 20; lat =   42.73931476; lon = -103.32877941; elv = 1069.94000000
 CASE ('SAV1'); is = 21; lat =   32.13859989; lon =  -81.69633426; elv =   39.46080000
 CASE ('BLKV'); is = 22; lat =   37.20601771; lon =  -80.41452462; elv =  637.70400000

!name=ffsl.llh"

 CASE ('NLGN'); is = 23; lat =   42.20674343; lon = -97.79529896 ; elv =  523.03000000
 CASE ('SLAI'); is = 24; lat =   41.90058327; lon = -93.69887039 ; elv =  317.44000000
 CASE ('LMNO'); is = 25; lat =   36.68544654; lon = -97.48073652 ; elv =  306.45000000
 CASE ('AZCN'); is = 26; lat =   36.83979329; lon = -107.91096134; elv = 1881.19000000
 CASE ('BLK0'); is = 27; lat =   37.20601771; lon =  -80.41452462; elv =  637.62000000
 CASE ('BLMM'); is = 28; lat =   36.87978303; lon =  -89.97252840; elv =  128.19000000
 CASE ('BLRW'); is = 29; lat =   43.23025339; lon =  -90.53077843; elv =  222.27000000
 CASE ('CNWM'); is = 30; lat =   37.52283396; lon =  -92.70349065; elv =  388.02000000
 CASE ('DQUA'); is = 31; lat =   34.11065741; lon =  -94.28989785; elv =  196.58000000
 CASE ('DSRC'); is = 32; lat =   39.99143134; lon = -105.26103159; elv = 1669.06000000
 CASE ('FBYN'); is = 33; lat =   40.07687182; lon =  -97.31281998; elv =  430.46000000
 CASE ('GDAC'); is = 34; lat =   37.77545215; lon = -102.18001253; elv = 1155.85000000
 CASE ('HBRK'); is = 35; lat =   38.30464964; lon =  -97.29352379; elv =  439.85000000
 CASE ('HKLO'); is = 36; lat =   35.68275454; lon =  -95.86338381; elv =  218.93000000
 CASE ('HVLK'); is = 37; lat =   37.65146778; lon =  -99.10675913; elv =  646.27000000
 CASE ('JTNT'); is = 38; lat =   33.01720036; lon = -100.97714513; elv =  709.24000000
 CASE ('LKWY'); is = 39; lat =   44.56507693; lon = -110.40021843; elv = 2430.74000000
 CASE ('LTHM'); is = 40; lat =   39.57595873; lon =  -94.17015452; elv =  293.41000000
 CASE ('MBWW'); is = 41; lat =   41.90358636; lon = -106.18653971; elv = 1996.07000000
 CASE ('MRRN'); is = 42; lat =   42.90428687; lon = -101.69641978; elv =  988.53000000
 CASE ('NDBC'); is = 43; lat =   30.35628084; lon =  -89.61027680; elv =   14.37000000
 CASE ('NDSK'); is = 44; lat =   37.38077849; lon =  -95.63823621; elv =  254.72000000
 CASE ('OKOM'); is = 45; lat =   34.09026377; lon =  -88.86247819; elv =  131.87000000
 CASE ('PATT'); is = 46; lat =   31.77827283; lon =  -95.71850951; elv =  119.80000000
 CASE ('PLTC'); is = 47; lat =   40.18159312; lon = -104.72593635; elv = 1518.71000000
 CASE ('PRCO'); is = 48; lat =   34.97986594; lon =  -97.51924837; elv =  329.58000000
 CASE ('RWDN'); is = 49; lat =   40.08670106; lon = -100.65348014; elv =  796.73000000
 CASE ('SEAW'); is = 50; lat =   47.68700546; lon = -122.25630089; elv =   16.42000000
 CASE ('SIO3'); is = 51; lat =   32.86470196; lon = -117.25040572; elv =   69.58000000
 CASE ('SYCN'); is = 52; lat =   43.11572518; lon =  -76.09339750; elv =  122.14000000
 CASE ('TCUN'); is = 53; lat =   35.08501981; lon = -103.60911953; elv = 1240.06000000
 CASE ('VCIO'); is = 54; lat =   36.07170788; lon =  -99.21731449; elv =  650.17000000
 CASE ('WDLM'); is = 55; lat =   44.67221307; lon =  -95.44858064; elv =  319.43000000
 CASE ('WLCI'); is = 56; lat =   40.80840811; lon =  -87.05198607; elv =  212.42000000
 CASE ('WNCI'); is = 57; lat =   39.66461362; lon =  -90.47674775; elv =  172.03000000
 CASE ('WNFL'); is = 58; lat =   31.89722826; lon =  -92.78191095; elv =   92.70000000
 CASE ('WSMN'); is = 59; lat =   32.40713676; lon = -106.34982961; elv = 1226.17000000
 CASE ('CENA'); is = 60; lat =   65.49816527; lon = -144.67763059; elv =  271.78000000
 CASE ('GNAA'); is = 61; lat =   62.11238351; lon = -145.97021718; elv =  584.78000000
 CASE ('TLKA'); is = 62; lat =   62.30765593; lon = -150.42029462; elv =  151.28000000

CASE DEFAULT;   is =  0; lat =    0.00000000; lon =    0.00000000; elv =    0.00000000
!               WRITE (0,'(A,A4)') ' Unknown station sequence name ',nam
!               STOP               ' in station_sequence'

END SELECT

RETURN 

END SUBROUTINE name_to_number
!------------------------------------------------------------------------------!
END MODULE module_gpspw_gst
