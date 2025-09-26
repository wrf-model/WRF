MODULE module_gpspw_caa

!------------------------------------------------------------------------------!
! Read in integrated precipitable water measurement from GPS stations coming
! from GST (gem file).
!
!  Y.-H. GUO, March 2001
!------------------------------------------------------------------------------!

USE module_date
USE module_type
USE module_inside
USE module_per_type

INCLUDE 'missing.inc'

!------------------------------------------------------------------------------!

    TYPE station_name
         CHARACTER (LEN=10) :: full_name
         CHARACTER (LEN= 4) :: abbreviate_name
    END  TYPE station_name

!   Number of stations in thre file

    INTEGER, PARAMETER      :: ns = 10      

!   Name of station assigned as obs ID (5 digits)

    TYPE (station_name), PARAMETER, DIMENSION (ns) :: station = &
                                  (/station_name ('BANCHAU   ','banc'), &
                                    station_name ('CHIAYI    ','chia'), &
                                    station_name ('CHENG-KUNG','chen'), &
                                    station_name ('HENGCHU   ','henc'), &
                                    station_name ('HSINCHU   ','hsin'), &
                                    station_name ('HUALIEN   ','hual'), &
                                    station_name ('ILAN      ','ilan'), &
                                    station_name ('PANGHU    ','pang'), &
                                    station_name ('SUNYI     ','sunm'), &
                                    station_name ('SUAO      ','suao')/)


CONTAINS
!------------------------------------------------------------------------------!

SUBROUTINE read_gpspw_caa (filename, filenum, obs, n_obs,                  &
                           total_number_of_obs, fatal_if_exceed_max_obs,   &
                           time_window_min, time_analysis, time_window_max,&
                           ins, jew, missing_flag, &
                           print_gpspw_read)
!------------------------------------------------------------------------------!

      IMPLICIT NONE

      CHARACTER (LEN=*),            INTENT (in)  :: filename
      INTEGER,                      INTENT (in)  :: filenum
      INTEGER,                      INTENT (inout) :: n_obs
      INTEGER,                      INTENT (in)  :: total_number_of_obs
      LOGICAL,                      INTENT (in)  :: fatal_if_exceed_max_obs
      TYPE (report), DIMENSION (:), INTENT (out) :: obs
      INTEGER,                      INTENT (in)  :: ins, jew
      CHARACTER (LEN = 19),         INTENT (in)  :: time_window_min
      CHARACTER (LEN = 19),         INTENT (in)  :: time_analysis
      CHARACTER (LEN = 19),         INTENT (in)  :: time_window_max
      REAL,                         INTENT (out) :: missing_flag
      LOGICAL,                      INTENT (in)  :: print_gpspw_read

      INTEGER                                 :: nyear
      INTEGER                                 :: io_error, n, ii
      INTEGER                                 :: length, sites_n
      INTEGER                                 :: time_n, juln_day, seconds
      INTEGER                                 :: obs_num, num_empty, num_outside
      CHARACTER (LEN=110)                     :: chr
      CHARACTER (LEN= 28)                     :: st_info
      REAL                                    :: lat_d,lat_m,lat_s, &
                                                 lon_d,lon_m,lon_s
      LOGICAL                                 :: outside_domain, outside_window
      LOGICAL                                 :: outside

      REAL,                DIMENSION (ns)     :: height, lat, lon
      CHARACTER (LEN=10), DIMENSION (ns)      :: st_name
      INTEGER,             DIMENSION (300)    :: nyr, nmo, ndy, nhr, nmn, nsc
      REAL,                DIMENSION (300)    :: time
      REAL,                DIMENSION (ns,300) :: pw

      CHARACTER (LEN = 80)                    :: file_name
      CHARACTER (LEN = 32), PARAMETER         :: proc_name = 'read_gpspw_caa '
      INTEGER                                 :: iunit


!------------------------------------------------------------------------------!

             WRITE (UNIT = 0, FMT = '(A)')  &
'------------------------------------------------------------------------------'

      WRITE (UNIT = 0, FMT = '(A,A,/)') &
     'READ GPS OBSERVATIONS IN FILE: ', TRIM (filename)

! 0.  OPEN DIGANOSTIC FILE
! ========================

      IF (print_gpspw_read) THEN

          file_name = 'obs_gpspw_read.diag'
          iunit    = 999

          OPEN (UNIT = iunit , FILE = file_name , FORM = 'FORMATTED'  , &
                ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error )

          IF (io_error .NE. 0 ) THEN
              CALL error_handler (proc_name, &
             'Unable to open output diagnostic file. ' , file_name, .TRUE.)
          ELSE
              WRITE (UNIT = 0, FMT = '(A,A,/)') &
             "Diagnostics in file ", TRIM (file_name)
         ENDIF

      ENDIF

! 1.  OPEN INPUT FILE AND RESET
! =============================

 
      OPEN (UNIT = filenum, FILE = filename, FORM='formatted', ACTION = 'read',&
            IOSTAT = io_error )

      REWIND (filenum)

      !  Year of data

      READ (time_analysis,'(I4)') nyear

      pw       =  999.990
      time     =  999.990
      time_n   = -9999
      sites_n  = 0
      io_error = 0

      obs_num = n_obs + 1

read: DO WHILE ( io_error >= 0 )

      READ (filenum,'(A)', iostat = io_error) CHR
    
! 1.  READ STATION INFORMATION:
! =============================

      !  Get the station name:

      IF (LEN_TRIM (chr) < 15 .AND. LEN_TRIM (chr) > 0) THEN

          !  Search the index based on the station name:

          ii = 0

search:   DO n = 1,ns

             IF (chr(4:13) == station(n) % full_name) THEN
                 ii = n
                 EXIT search
             ENDIF

          ENDDO search
 
          !  If there is no data for this station, go to read next:

          IF (ii > ns .OR. ii < 1) CYCLE read

              !  Keep the station name, and counter + 1:

              st_name(ii) = chr(4:13)
              sites_n     = sites_n + 1

          ELSE IF ((ii>0 .AND. ii<=ns) .AND. time_n == -9999) THEN

              !  Get the height, latitude, and longitude for the station:

              st_info = chr(21:48)

stn_info:     SELECT CASE (st_info(1:2))

              CASE ('HE')  stn_info

                           READ (st_info,'(12X,F12.4)') height(ii)
                           if (height(ii) < 0.0) height(ii) = missing_r

              CASE ('LA')  stn_info 

                           READ (st_info,'(12X,2F3.0,G10.6)') lat_d,lat_m,lat_s
                           lat (ii) = lat_d + lat_m/60. + lat_s/3600.

              CASE ('LO')  stn_info

                           read(st_info,'(12X,2F3.0,G10.6)') lon_d,lon_m,lon_s
                           lon(ii) = lon_d + lon_m/60. + lon_s/3600.

              CASE DEFAULT  stn_info

                      IF (print_gpspw_read) THEN
                      WRITE (UNIT = iunit, FMT = &
                      &    '(''II='',I2,2X,''site_n='',I2,2X,''STN_INFO:'',&
                      &    A,'' HEIGHT, LAT, and LON. ARE MISSING'',/)')  &
                      &    ii,sites_n,TRIM (st_info)
                      ENDIF

                      ii = 0

              END SELECT  stn_info

          ENDIF



! 2.  READ PW DATA:
! =================

      IF (chr(1:4) == '====') THEN

           time_n = 0

      ELSE IF (time_n >= 0 .and. io_error >= 0) THEN

           time_n = time_n + 1

           READ (chr,'(11F9.3)') time(time_n),(pw(n,time_n),n=1,ns)

           nyr (time_n) = nyear
           juln_day     = INT (time(time_n))
           seconds      = (time(time_n) - Juln_day) * 86400.

           CALL julian_day    (nyear, nmo (time_n), ndy (time_n), juln_day, 2)
           CALL sec_to_hhmmss (nhr (time_n), nmn (time_n), nsc (time_n), &
                               seconds, 2)

      ENDIF

      ENDDO read

      num_empty   = 0
      num_outside = 0

n_time:&
      DO ii = 1, time_n

stns:    DO n = 1,ns

            !  Fill GPS PW data structure

            obs(obs_num) % location % id        = adjustl (station(n)%abbreviate_name)
            obs(obs_num) % location % name      = adjustl (station(n)%full_name)
            obs(obs_num) % location % latitude  = lat(n)
            obs(obs_num) % location % longitude = lon(n)

            obs(obs_num) % info % platform      = 'FM-111 GPSPW'
            obs(obs_num) % info % source        = 'TWP FROM TAIWAN'
            obs(obs_num) % info % elevation     =  height(n)
            obs(obs_num) % info % discard       = .false.
            obs(obs_num) % info % is_sound      = .false.

            obs(obs_num) % valid_time % julian  = time(ii)

            WRITE (obs(obs_num) % valid_time % date_char,'(I4,5I2.2)') &
                   nyr (ii), nmo (ii), ndy (ii), nhr (ii), nmn (ii), nsc (ii)

            WRITE (obs(obs_num) % valid_time % date_mm5,'(I4,5('':'',I2.2))') &
                   nyr (ii), nmo (ii), ndy (ii), nhr (ii), nmn (ii), nsc (ii)

            IF (pw(n,ii) == 0.000 .or. pw(n,ii) ==  999.990 ) then
               obs(obs_num) % ground % pw % qc   =  missing 
               obs(obs_num) % ground % pw % data =  missing_r
               obs(obs_num) % ground % pw % error=  missing_r
            ELSE
               obs(obs_num) % ground % pw % data =  pw(n,ii)
               obs(obs_num) % ground % pw % qc   =  0
               obs(obs_num) % ground % pw % error=  0.2
            ENDIF

            !  Reset slp data structure

            obs(obs_num) % ground % slp % qc     =  missing
            obs(obs_num) % ground % slp % data   =  missing_r
            obs(obs_num) % ground % slp % error  =  missing_r

            !  Check domain and time window

            CALL inside_domain (obs(obs_num)%location%latitude  , &
                                obs(obs_num)%location%longitude , &
                                ins , jew , outside_domain , &
                                obs(obs_num)%location%xjc, &
                                obs(obs_num)%location%yic, &
                                obs(obs_num)%location%xjd, &
                                obs(obs_num)%location%yid)

            CALL inside_window (obs(obs_num)%valid_time%date_char, &
                                time_window_min, time_window_max, &
                                outside_window, iunit)

            outside = outside_domain .OR. outside_window


            IF (.NOT.outside) THEN

                !  Unlink upper levels

                 NULLIFY (obs (obs_num) % surface)

                 !  Print station information:

                 IF (print_gpspw_read) THEN

                     WRITE (UNIT = iunit, FMT = '(A,A5,1X,A23,2F9.3)', &
                            ADVANCE = 'no' )&
                           'Found Name and ID = ' , &
                            obs (obs_num) % location % id,          &
                            obs (obs_num) % location % name,        &
                            obs (obs_num) % location % latitude,    &
                            obs (obs_num) % location % longitude
                            WRITE (UNIT = iunit , FMT = '(A)' ) ' => GPS PW'

                 ENDIF

                 !  Increment the number of read station

                 obs_num = obs_num + 1

                 !  Check if there's enough memory to read a new data

                 IF  ((obs_num .GT. total_number_of_obs) .AND.  &
                      (fatal_if_exceed_max_obs)) THEN

                 CALL error_handler (proc_name, &
                "Too many obs increase the max_number_of_obs_nml in namelist",&
                 "",.true.)

                 ELSE IF ((obs_num .GT. total_number_of_obs) .AND. &
                          (.NOT. fatal_if_exceed_max_obs))    THEN

                 CALL error_handler (proc_name, &
                "Too many obs increase the max_number_of_obs_nml in namelist.",&
                 .false.)

                 EXIT  n_time

                 ENDIF

            ELSE
                 num_outside = num_outside + 1
            ENDIF

        ENDDO stns

      ENDDO n_time

      WRITE (UNIT = 0, FMT = '(A)') 'Have reached end of observations file.'

      CLOSE (filenum)

      obs_num  = obs_num - 1

      !  Total number of observation accumulated per type

      ngpspws (icor+0) = obs_num+num_empty+num_outside-n_obs
      ngpspws (icor+1) = num_empty
      ngpspws (icor+2) = num_outside

      !  Print number of reports

      WRITE (UNIT = 0, FMT = '(/,A)')  &
'------------------------------------------------------------------------------'

      WRITE (UNIT = 0, FMT = '(A)') 'GPS PW OBSERVATIONS READ:'

      WRITE (UNIT = 0,FMT = '(/,A,I6)') &
     'Number of GPSPW reports: ',ngpspws (icor)

      WRITE (UNIT = 0 , FMT = '(/,4(A,I8,/))' ) &

          "Number of observations read:          ",obs_num+    &
                                                   num_empty+  &
                                                   num_outside-&
                                                   n_obs,      &
          "Number of empty observations:         ",num_empty,  &
          "Number of out of domain observations: ",num_outside,&
          "Number of observations for ingestion: ",obs_num-n_obs


      !  Total number of observation accumulated

      n_obs = obs_num


      !  Missing data flag

      missing_flag = missing_r

      IF (print_gpspw_read) CLOSE (iunit)

    END SUBROUTINE read_gpspw_caa

END MODULE module_gpspw_caa
