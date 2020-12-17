MODULE module_gpspw

!------------------------------------------------------------------------------!
! Read in integrated precipitable water measurement from GPS stations coming
! from data files provided by CAA or GST. 
!
!  F. VANDENBERGHE, March 2001
!------------------------------------------------------------------------------!

CONTAINS
!------------------------------------------------------------------------------!

 SUBROUTINE read_obs_gpspw (filename, filenum, obs, n_obs,                  &
                            total_number_of_obs, fatal_if_exceed_max_obs,   &
                            time_window_min, time_analysis, time_window_max,&
                            ins, jew, missing_flag, print_gpspw_read)

!SUBROUTINE read_obs_gpspw (filename, filenum, gpspw_file_format)
!------------------------------------------------------------------------------!

      USE module_gpspw_caa
      USE module_gpspw_gst

      IMPLICIT NONE

      CHARACTER (LEN = 80),         INTENT (in)  :: filename
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
!     CHARACTER (LEN=*),            INTENT (out) :: gpspw_file_format

      INTEGER                                    :: io_error
      CHARACTER (LEN = 80)                       :: header
      CHARACTER (LEN = 32), PARAMETER            :: proc_name ='read_obs_gpspw '

!------------------------------------------------------------------------------!

!            WRITE (UNIT = 0, FMT = '(A)')  &
!'-----------------------------------------------------------------------------'

!     WRITE (UNIT = 0, FMT = '(A,A,/)') &
!    'READ GPS OBSERVATIONS IN FILE: ', TRIM (filename)


! 1.  OPEN INPUT FILE AND RED HEADER
! ==================================

! 1.1 Open input file
!     ---------------

      OPEN (UNIT = filenum, FILE = filename, FORM='formatted', ACTION = 'read',&
            IOSTAT = io_error)

      IF (io_error .NE. 0 ) THEN
          CALL error_handler (proc_name, "Unable to open input file ", &
                              filename, .TRUE.)
!     ELSE
!         WRITE (UNIT = 0, FMT = '(A,A,/)') &
!        "Diagnostics in file ", TRIM (filename)
       ENDIF

! 1.2 Read header
!     -----------

      REWIND (filenum)

      READ (UNIT = filenum, FMT = '(A)', IOSTAT = io_error) header

      IF (io_error .NE. 0 ) THEN
          CALL error_handler (proc_name, "Unable to read input file ", &
                              filename, .TRUE.)
      ENDIF

! 1.3 Close input file
!     ----------------

      CLOSE (filenum)


! 2.  CALL GPSPW READ ROUTINE UPON HEADER
! ======================================

! 2.1 CAA GPS PW data 
!     ---------------

      IF (TRIM (header) == 'GPS STATIONS IN TAIWAN:') THEN

          CALL read_gpspw_caa (filename, filenum, obs, n_obs, &
                               total_number_of_obs, fatal_if_exceed_max_obs,   &
                               time_window_min, time_analysis, time_window_max,&
                               ins, jew, missing_flag, print_gpspw_read)

!         gpspw_file_format = 'CAA' 

! 2.1 GST GPS PW data 
!     ---------------

      ELSE IF (TRIM (header) == &
    ' STN   YYYYMMDD/HHMM  PWV[mm]  ERR[mm]  ZWD[mm]  PALT    TMPC   RELH') THEN

          CALL read_gpspw_gst (filename, filenum, obs, n_obs, &
                               total_number_of_obs,  fatal_if_exceed_max_obs,  &
                               time_window_min, time_analysis, time_window_max,&
                               ins, jew, missing_flag, print_gpspw_read)

!         gpspw_file_format = 'GST' 

      ELSE

          CALL error_handler (proc_name, "Don`t know how to read file ", & 
                              filename, .TRUE.)

!         gpspw_file_format = 'UNKNOWN' 

      ENDIF

END SUBROUTINE read_obs_gpspw

END MODULE module_gpspw

