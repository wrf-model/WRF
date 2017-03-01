
MODULE module_decoded

!--------------------------------------------------------------------------
! Read decoded observations (output format of the MM5 "gts_decoder" and "fetch"
!                            facilities, which is also the input format
!                            of MM5 "rawins" and "little_r" programs).
!
! Keep only records with at least height or pressure in MM5 horizontal domain 
!                           as defined in namelist
!
! Fill the observation data structure
!--------------------------------------------------------------------------
!
!  HISTORY: 
!
!  D. GILL,         April 1998
!  F. VANDENBERGHE, March 2001
!
!         01/13/2003 - Updated for Profiler obs.           S. R. H. Rizvi
!
!         02/04/2003 - Updated for Buoy     obs.           S. R. H. Rizvi
!
!         02/11/2003 - Reviewed and modified for Profiler
!                      and Buoy obs.                       Y.-R. Guo
!         03/30/2005 - Updated for MODIS    obs.           Syed RH  Rizvi
!         06/30/2006 - Updated for AIRS retrievals         Syed  RH  Rizvi
!         10/09/2006 - Updated for GPS Excess Phase        Y.-R. Guo
!-----------------------------------------------------------------------------
!                          DATA STRUCTURES
!--------------------------------------------------------------------------

   USE module_type
   use module_stntbl

!------------------------------------------------------------------------
!                            FUNCTION
!------------------------------------------------------------------------

   USE module_func

!------------------------------------------------------------------------
!                            EXTERNAL
!------------------------------------------------------------------------

  USE module_inside
  use module_namelist, only: gts_from_mmm_archive, calc_psfc_from_QNH

!--------------------------------------------------------------------------
!                            PARAMETERS
!---------------------------------------------------------------------------

   INCLUDE 'missing.inc'

   !  Define error return codes used by 'read_measurements' routine.

   INTEGER , PARAMETER                            ::  ok       = 0 , &
                                                      eof_err  = 1 , &
                                                      no_data  = 2 , &
                                                      read_err = 3 , &

!     ssmi_qc_index is the index for good quality of SSMI Tb data from
!                      the SSMI decoder program. (Y.-R. Guo, 09/03/2003)

                                                      ssmi_qc_index = 0
!                                                
!  FORMAT STRINGS for input/output of data.
!  These format strings correspond to the data structures in this file
!  and are used for input and output of values in the 'report' structure
!  (first format string) and the 'measurement' structure (second format).
!  Note that report struct contains the first of a linked list of
!  measurements; this first meas is read using the 'meas_format'.

   CHARACTER ( LEN = 120 ) , PARAMETER :: rpt_format =  &
                ' (2F20.5 , 2A40 , '              & ! format for location_type
             // ' 2A40 , 1F20.5 , 5I10 , 3L10 , ' & ! format for source_info
             // ' 2I10 , A20 , '                  & ! fmt for valid_time
             // ' 13(F13.5 , I7),'                & ! fmt for 'terrestrial'
             // '1(:,F13.5 , I7),'                & ! fmt for PW other than GPS
             // '7(:,F13.5 , I7))'                  ! fmt for Brightness Temp

   CHARACTER ( LEN = 120 ) , PARAMETER :: meas_format = &
                ' ( 10( F13.5 , I7 ) ) '            ! fmt for measurement rcd

   CHARACTER ( LEN = 120 ) , PARAMETER :: end_format = &
                ' ( 3 ( I7 ) ) '                    ! fmt for end record

   INTEGER       :: N_air = 0, N_air_cut = 0

! -------------------------------------------------------------------------
!                            ROUTINES
! -------------------------------------------------------------------------

CONTAINS

! SUBROUTINE read_obs_gts ( file_name , file_num , obs , n_obs , &
! SUBROUTINE read_measurements (file_num, surface, location, bad_data, &
! SUBROUTINE dealloc_meas ( head )
! SUBROUTINE surf_first ( surface , elevation )
! SUBROUTINE diagnostics ( new , longitude )
! SUBROUTINE insert_at ( surface , new , elevation )

!
!-------------------------------------------------------------------------


SUBROUTINE read_obs_gts ( file_name,  obs , n_obs , &
total_number_of_obs , fatal_if_exceed_max_obs , print_gts_read , &
ins , jew , &
time_window_min, time_window_max, map_projection , missing_flag)

!  This routine opens file 'file_name' and reads all observations, and 
!  measurements at all levels, from the file into the 'obs' observation array.
!  For each observation, calls read_measurements to read all measurements for
!  that one observation.

   USE module_date
   USE module_per_type

   IMPLICIT NONE

   CHARACTER ( LEN = * ) , INTENT ( IN )          :: file_name
   INTEGER, INTENT (INOUT)                        :: n_obs
   INTEGER, INTENT ( IN )                         :: total_number_of_obs
   LOGICAL, INTENT ( IN )                         :: fatal_if_exceed_max_obs
   LOGICAL, INTENT ( IN )                         :: print_gts_read
   TYPE (report), DIMENSION (:), INTENT (OUT)     :: obs
   INTEGER, INTENT ( IN )                         :: ins, jew

   CHARACTER (LEN = 19) , INTENT (IN)             :: time_window_min
   CHARACTER (LEN = 19) , INTENT (IN)             :: time_window_max
   INTEGER                                        :: map_projection
   REAL,    INTENT (OUT)                          :: missing_flag

   INTEGER                              :: file_num
   CHARACTER ( LEN = 32 ) , PARAMETER   :: proc_name = 'read_obs_gts '
   INTEGER                              :: io_error, platform_error
   INTEGER                              :: obs_num
   INTEGER                              :: error_ret

   INTEGER                              :: num_empty , num_outside , bad_count
   LOGICAL                              :: outside_domain, outside_window
   LOGICAL                              :: outside

   TYPE(meas_data)                      :: dummy_middle

   INTEGER                              :: icrs, k, iunit, levels
   CHARACTER ( LEN =  80)               :: filename
   CHARACTER ( LEN = 160)               :: error_message
   CHARACTER ( LEN =  14)               :: newstring
   INTEGER                              :: nlevels, num_unknown, m_miss
   TYPE ( measurement ) , POINTER       :: current
   real :: qnh, elev
   real :: xla, xlo
   integer :: ipos
!-----------------------------------------------------------------------------!
  INCLUDE 'platform_interface.inc'
!-----------------------------------------------------------------------------!

!  INCLUDE 'error.inc'
!  INTERFACE
!     INCLUDE 'error.int'
!  END INTERFACE

!------------------------------------------------------------------------------!

             WRITE (UNIT = 0, FMT = '(A)')  &
'------------------------------------------------------------------------------'

      WRITE (UNIT = 0, FMT = '(A,A,/)') &
    'READ GTS OBSERVATIONS IN FILE ', TRIM (file_name)

   !  empty or outside of the domain.
   !  Initialize a couple of counters for how many observations are either
   !  empty or outside of the domain.

   num_unknown = 0
   missing_flag = missing_r
   num_empty = 0
   num_outside = 0
   m_miss = 0

   !  Open file for writing diagnostics
   IF (print_gts_read) THEN

        filename = 'obs_gts_read.diag'
        iunit    = 999

        OPEN ( UNIT = iunit , FILE = filename , FORM = 'FORMATTED'  , &
               ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error ) 

        IF ( io_error .NE. 0 ) THEN
             CALL error_handler (proc_name, &
            'Unable to open output diagnostic file:', filename, .true.)
        ELSE
             WRITE (UNIT = 0, FMT = '(A,A,/)') &
            "Diagnostics in file ", TRIM (filename)
        ENDIF

   ENDIF

   !  Open file for reading; handle any errors in open by quitting since
   !  this is probably a simple-to-fix user mistake.

   file_num = 99
   OPEN ( UNIT = file_num , FILE = file_name , FORM = 'FORMATTED'  , &
          ACTION = 'READ' , IOSTAT = io_error ) 

   IF ( io_error .NE. 0 ) THEN
      CALL error_handler (proc_name, & 
             'Unable to open gts input observations file: ',file_name, .true.)
   ENDIF

!
!     allow changing the obs date for testing
!
      CALL GETENV('OBSPROC_TEST_DATE',newstring)

   !  While we are not at the end of the observation file, keep reading data.

!  obs_num = 1
   obs_num = n_obs + 1

   read_obs : DO while ( io_error == 0 ) 
      !  This is an array that we are filling.  Are we beyond that limit yet?

      IF (obs_num .GT. total_number_of_obs) THEN

            error_message(1:60)  = &
           'Too many obs for the NAMELIST value of max_number_of_obs = '

            WRITE (error_message(61:67),'(I7)')  total_number_of_obs

            CALL error_handler (proc_name, error_message (1:60), &
                 error_message(61:67),fatal_if_exceed_max_obs)
            
            ! If fatal, code will stop above, otherwise the following lines exit
            ! do loop and close file read

            CLOSE ( file_num ) 
            IF (print_gts_read) CLOSE ( iunit ) 
            EXIT read_obs

      END IF

      ! initialize the vaiable that will be used later for checking
      ! if the pw info is read or not
      obs(obs_num)%ground%pw%data = missing_r

      !  The first read is the "once only" type of information.

      READ ( file_num , IOSTAT = io_error , FMT = rpt_format ) &
      obs(obs_num)%location % latitude,     obs(obs_num)%location % longitude, &
      obs(obs_num)%location % id,           obs(obs_num)%location % name,      &
      obs(obs_num)%info % platform,         obs(obs_num)%info % source,        &
      obs(obs_num)%info % elevation,        obs(obs_num)%info % num_vld_fld,   &
      obs(obs_num)%info % num_error,        obs(obs_num)%info % num_warning,   &
      obs(obs_num)%info % seq_num,          obs(obs_num)%info % num_dups,      &
      obs(obs_num)%info % is_sound,         obs(obs_num)%info % bogus,         &
      obs(obs_num)%info % discard,                                             &
      obs(obs_num)%valid_time % sut,        obs(obs_num)%valid_time % julian,  &
      obs(obs_num)%valid_time % date_char,                                     &
      obs(obs_num)%ground%slp%data,         obs(obs_num)%ground%slp%qc,        &
      obs(obs_num)%ground%ref_pres%data,    obs(obs_num)%ground%ref_pres%qc,   &
      obs(obs_num)%ground%ground_t%data,    obs(obs_num)%ground%ground_t%qc,   &
      obs(obs_num)%ground%sst%data,         obs(obs_num)%ground%sst%qc,        &
      obs(obs_num)%ground%psfc%data,        obs(obs_num)%ground%psfc%qc,       &
      obs(obs_num)%ground%precip%data,      obs(obs_num)%ground%precip%qc,     &
      obs(obs_num)%ground%t_max%data,       obs(obs_num)%ground%t_max%qc,      &
      obs(obs_num)%ground%t_min%data,       obs(obs_num)%ground%t_min%qc,      &
      obs(obs_num)%ground%t_min_night%data, obs(obs_num)%ground%t_min_night%qc,&
      obs(obs_num)%ground%p_tend03%data,    obs(obs_num)%ground%p_tend03%qc,   &
      obs(obs_num)%ground%p_tend24%data,    obs(obs_num)%ground%p_tend24%qc,   &
      obs(obs_num)%ground%cloud_cvr%data,   obs(obs_num)%ground%cloud_cvr%qc,  &
      obs(obs_num)%ground%ceiling%data,     obs(obs_num)%ground%ceiling%qc,    &
      obs(obs_num)%ground%pw     %data,     obs(obs_num)%ground%pw%qc,         &
      obs(obs_num)%ground%tb19v  %data,     obs(obs_num)%ground%tb19v%qc,      &
      obs(obs_num)%ground%tb19h  %data,     obs(obs_num)%ground%tb19h%qc,      &
      obs(obs_num)%ground%tb22v  %data,     obs(obs_num)%ground%tb22v%qc,      &
      obs(obs_num)%ground%tb37v  %data,     obs(obs_num)%ground%tb37v%qc,      &
      obs(obs_num)%ground%tb37h  %data,     obs(obs_num)%ground%tb37h%qc,      &
      obs(obs_num)%ground%tb85v  %data,     obs(obs_num)%ground%tb85v%qc,      &
      obs(obs_num)%ground%tb85h  %data,     obs(obs_num)%ground%tb85h%qc

      if ( io_error < 0 ) then ! end-of-file
         write(unit=0, fmt='(A)') 'Have reached the end of observation file.'
         close(file_num)
         if ( print_gts_read ) close(iunit)
         exit read_obs
      end if
!
!     allow changing the obs date for testing  (GETENV used above)
!
      if (len_trim(newstring) .ne. 0 ) then
        obs(obs_num)%valid_time%date_char = newstring 
      endif

!------------------------------------------------------------------------------
! If a new data type without a WMO code (like the example of 'AWS SURFACE '
! below) need to be processed, a assumed WMO code (like FM-16) must be
! assigned, otherwise this new data type will be ignored. 
!
!     For AFWA AWS SURFACE in platform, there is no WMO code given. 
!     Here assume that it is FM-16 similar to metar data:
!
      if (obs(obs_num)%info % platform(1:12) == 'AWS SURFACE ') then
          obs(obs_num)%info % platform(1:12) =  'FM-16 AWSSFC'
      endif
      if (obs(obs_num)%info % platform(1:14) == 'FM-32 PROFILER') then
          obs(obs_num)%info % platform(1:14) =  'FM-132 PROFILER'
      endif
! .............................................................................
!  Treatment of MODIS winds 
     if(index(obs(obs_num)%location%id,   'MODIS') > 0 .or. &
        index(obs(obs_num)%location%id,   'modis') > 0 .or. &
        index(obs(obs_num)%info%source,   'MODIS') > 0 .or. &
        index(obs(obs_num)%info%source,   'modis') > 0      ) then

      if( index(obs(obs_num)%info%platform(1:11), 'FM-88') > 0 ) then
        obs(obs_num)%info%platform(1:11) = 'FM-88 MODIS'                             
        !hcl-note: below might be specific to NCAR/MMM dataset
        obs(obs_num)%location%name       =  obs(obs_num)%location%id
      end if

     end if
! .............................................................................

      if ( gts_from_mmm_archive ) then
         ! distinguish BUOY and SHIP (both from BBXX reports assigned FM-13 SHIP)
         ! by the name. A trick done in NCAR gts_decoder to leave a clue for
         ! subsequent data processing
         if ( obs(obs_num)%info%platform(1:10) == 'FM-13 SHIP' ) then
            if ( index(obs(obs_num)%location%name, 'Platform Id >>>') > 0 ) then
               obs(obs_num)%info%platform(1:10) = 'FM-18 BUOY'
               obs(obs_num)%location%id = obs(obs_num)%location%name(17:21)
            end if
         end if
      end if

      call print_extra_obs
! .............................................................................
! To ignore the data type without WMO code:

         if (IO_error > 0 ) then  ! error reading header info
           write(0,'("IO_ERROR=",i2,1x,i7,1x,a,2(f9.3,a),1x,a,1x,f11.3)') &
                             io_error, obs_num, obs(obs_num)%info % platform, &
                                          obs(obs_num)%location%latitude, 'N',&
                                          obs(obs_num)%location%longitude,'E ', &
                                          obs(obs_num)%valid_time%date_char,    &
                                          obs(obs_num)%info % elevation

            !hcl obs(obs_num)%info % elevation = missing_r
            obs(obs_num)%info % discard = .true.
         endif

         READ (obs(obs_num) % info % platform (4:6), '(I3)', &
                                          IOSTAT = platform_error) fm
         if (platform_error /= 0) then
            write(0,'(A)') &
              "***WARNING: NO WMO CODE FOR THIS NEW TYPE OF OBS***"
            write(0,'(A,I8,A,I4,A,A,A,A)') " ==> obs_num=",obs_num, &
               " platform_error=",platform_error," platform=",      &
               obs(obs_num) % info % platform (1:12), " ID=",       &
               obs(obs_num) % location % id(1:6)
         else if (fm <= 0) then
            write(0,'(A,I8,A,A,A,A)') " ==> obs_num=",obs_num, &
               " INVALID WMO CODE: platform=",      &
               obs(obs_num) % info % platform (1:12), " ID=",       &
               obs(obs_num) % location % id(1:6)
            platform_error = -99
!         else if (fm == 0) then
!            obs(obs_num)%info % platform(1:12) =  'FM-42 AMDAR'
         endif

!-----------------------------------------------------------------------------

      ! Print read results

      if ( print_gts_read ) then
      WRITE (UNIT = iunit , FMT = '(A,1X,A,1X,A,1X,A,1X,2(F8.3,A),A,1X)',&
             ADVANCE='no') 'Found' ,           &
             obs(obs_num)%location%id   (1: 5),&
             obs(obs_num)%location%name (1:20),&
             obs(obs_num)%info%platform (1: 12),&
             obs(obs_num)%location%latitude, 'N',&
             obs(obs_num)%location%longitude,'E ', &
             obs(obs_num)%valid_time%date_char
      end if
!
! To avoid the floating error in latlon_to_ij calculation: (Guo 01/08/2005)
      IF (IPROJ > 0) THEN
         if (truelat1 > 0.0) then
            if (obs(obs_num)%location%latitude == -90.0) then
              write(0,'(/i7,1x,"modified the original lat =",f8.2," to -89.5"/)') &
                     obs_num, obs(obs_num)%location%latitude  
                     obs(obs_num)%location%latitude = -89.5
            endif
         else if (truelat1 < 0.0) then
            if (obs(obs_num)%location%latitude == 90.0) then
              write(0,'(/i7,1x,"modified the original lat =",f8.2," to  89.5"/)') &
                     obs_num, obs(obs_num)%location%latitude  
                     obs(obs_num)%location%latitude =  89.5
                 endif
         endif
      ENDIF
 
      !  If PW obs weren't read, arrays are reset to 0, set to missing

      IF (obs(obs_num)%ground%pw%data .LE. 0.) THEN
          obs(obs_num)%ground%pw%data  = missing_r
          obs(obs_num)%ground%pw%qc    = missing
          obs(obs_num)%ground%pw%error = missing_r
      ELSE
         !------------------------------------------------------------ 
         !  GPSPW  is in cm and its QC-error in 0.1 mm
         !
         ! Note: the variable: pw used for either GPSPW or GPSZTD.
         !       We NEVER DO the assimilation of both GPSPW or GPSZTD.
         !                                  Y.-R. Guo  08/13/2003
         !
         ! From GPSPW_decoder, GPSZTD is also in cm and its QC-error 
         ! in the unit of 0.1mm
         !                                  Y.-R. Guo   05/09/2008.
         ! 
         !------------------------------------------------------------

          IF (fm == 111 .or. fm == 114) THEN    
             obs(obs_num)%ground%pw%error =    &
                         REAL(obs(obs_num)%ground%pw%qc)/100. ! GPSPW, ZTD
          ELSE
             obs(obs_num)%ground%pw%error = missing_r
          ENDIF
          obs(obs_num)%ground%pw%qc    = 0
      ENDIF

      !  PW QC for SSMI (AFWA data only)

!     IF (obs(obs_num)%ground%pw%qc .EQ. 5) obs(obs_num)%ground%pw%qc = 0

      !  If Tb's obs weren't read, arrays are reset to 0, set to missing
      !  If Tb were read, keep only data with QC = 5 (over water).

      IF ((obs(obs_num)%ground%tb19v%data .LE. 0.) .OR. &
          (obs(obs_num)%ground%tb19v%qc   .NE. ssmi_qc_index)) THEN
           obs(obs_num)%ground%tb19v%data  = missing_r
           obs(obs_num)%ground%tb19v%qc    = missing
           obs(obs_num)%ground%tb19v%error = missing_r
      ELSE
           obs(obs_num)%ground%tb19v%error = missing_r
           obs(obs_num)%ground%tb19v%qc    = 0
      ENDIF

      IF ((obs(obs_num)%ground%tb19h%data .LE. 0.) .OR. &
          (obs(obs_num)%ground%tb19h%qc   .NE. ssmi_qc_index)) THEN
           obs(obs_num)%ground%tb19h%data  = missing_r
           obs(obs_num)%ground%tb19h%qc    = missing
           obs(obs_num)%ground%tb19h%error = missing_r
      ELSE
           obs(obs_num)%ground%tb19h%error = missing_r
           obs(obs_num)%ground%tb19h%qc    = 0
      ENDIF

      IF ((obs(obs_num)%ground%tb22v%data .LE. 0.) .OR. &
          (obs(obs_num)%ground%tb22v%qc   .NE. ssmi_qc_index)) THEN
           obs(obs_num)%ground%tb22v%data  = missing_r
           obs(obs_num)%ground%tb22v%qc    = missing
           obs(obs_num)%ground%tb22v%error = missing_r
      ELSE
           obs(obs_num)%ground%tb22v%error = missing_r
           obs(obs_num)%ground%tb22v%qc    = 0
      ENDIF

      IF ((obs(obs_num)%ground%tb37v%data .LE. 0.) .OR. &
          (obs(obs_num)%ground%tb37v%qc   .NE. ssmi_qc_index)) THEN
           obs(obs_num)%ground%tb37v%data  = missing_r
           obs(obs_num)%ground%tb37v%qc    = missing
           obs(obs_num)%ground%tb37v%error = missing_r
      ELSE
           obs(obs_num)%ground%tb37v%error = missing_r
           obs(obs_num)%ground%tb37v%qc    = 0
      ENDIF

      IF ((obs(obs_num)%ground%tb37h%data .LE. 0.) .OR. &
          (obs(obs_num)%ground%tb37h%qc   .NE. ssmi_qc_index)) THEN
           obs(obs_num)%ground%tb37h%data  = missing_r
           obs(obs_num)%ground%tb37h%qc    = missing
           obs(obs_num)%ground%tb37h%error = missing_r
      ELSE
           obs(obs_num)%ground%tb37h%error = missing_r
           obs(obs_num)%ground%tb37h%qc    = 0
      ENDIF

      IF ((obs(obs_num)%ground%tb85v%data .LE. 0.) .OR. &
          (obs(obs_num)%ground%tb85v%qc   .NE. ssmi_qc_index)) THEN
           obs(obs_num)%ground%tb85v%data  = missing_r
           obs(obs_num)%ground%tb85v%qc    = missing
           obs(obs_num)%ground%tb85v%error = missing_r
      ELSE
           obs(obs_num)%ground%tb85v%error = missing_r
           obs(obs_num)%ground%tb85v%qc    = 0
      ENDIF

      IF ((obs(obs_num)%ground%tb85h%data .LE. 0.) .OR. &
          (obs(obs_num)%ground%tb85h%qc   .NE. ssmi_qc_index)) THEN
           obs(obs_num)%ground%tb85h%data  = missing_r
           obs(obs_num)%ground%tb85h%qc    = missing
           obs(obs_num)%ground%tb85h%error = missing_r
      ELSE
           obs(obs_num)%ground%tb85h%error = missing_r
           obs(obs_num)%ground%tb85h%qc    = 0
      ENDIF

      ! Initialize model coordinates yi and xj

      obs(obs_num)%location%yic = missing_r
      obs(obs_num)%location%yid = missing_r
      obs(obs_num)%location%xjc = missing_r
      obs(obs_num)%location%xjd = missing_r

      ! The number of upper levels must be 0 before reading

      obs (obs_num) % info % levels = 0

      !  If there are troubles with the "once only" type of data, we keep trying
      !  until we either come to the end of this report (and cycle) or we come
      !  to the end of all of the data (exit).

      IF ( io_error .GT. 0 .or. platform_error /= 0) THEN

         WRITE (UNIT = 0, FMT = '(A,A,A,A)') 'Troubles with first line ', &
              TRIM ( obs(obs_num)%location%id ) , &
         ' ', TRIM ( obs(obs_num)%location%name ) 

         !  Keep track of how many loops we are taking so this is not infinite.

         bad_count = 0

         DO WHILE ( io_error .GE. 0 )

         bad_count = bad_count + 1

         IF     (bad_count .LT. 1000 ) THEN
!          READ (file_num, IOSTAT = io_error, FMT = meas_format) dummy_middle
           READ (file_num, IOSTAT = io_error, FMT = meas_format)               &
           dummy_middle % pressure    % data, dummy_middle % pressure    % qc, &
           dummy_middle % height      % data, dummy_middle % height      % qc, &
           dummy_middle % temperature % data, dummy_middle % temperature % qc, &
           dummy_middle % dew_point   % data, dummy_middle % dew_point   % qc, &
           dummy_middle % speed       % data, dummy_middle % speed       % qc, &
           dummy_middle % direction   % data, dummy_middle % direction   % qc, &
           dummy_middle % u           % data, dummy_middle % u           % qc, &
           dummy_middle % v           % data, dummy_middle % v           % qc, &
           dummy_middle % rh          % data, dummy_middle % rh          % qc, &
           dummy_middle % thickness   % data, dummy_middle % thickness   % qc

           IF (eps_equal (dummy_middle%pressure%data, end_data_r , 1.)) THEN
               READ (file_num , IOSTAT = io_error , FMT = end_format ) &
                     obs(obs_num)%info%num_vld_fld , &
                     obs(obs_num)%info%num_error , &  
                     obs(obs_num)%info%num_warning    
                WRITE (UNIT = 0, FMT = '(A)') 'Starting to READ a new report.'
                CYCLE read_obs
           END IF

         ELSE

              WRITE (UNIT = 0, FMT = '(A)')&
             'Too many attempts to read the data correctly.  Exiting read loop.'
              CLOSE ( file_num ) 
              IF (print_gts_read) CLOSE ( iunit ) 
              EXIT read_obs

         END IF

         END DO 

         !  While trying to find a good read, we came to the end of the file.  
         !  It happens to the best of us.

         IF ( io_error .LT. 0 ) THEN

            WRITE (UNIT = 0, FMT='(A)') 'Have reached end of observations file.'
            CLOSE ( file_num ) 
            IF (print_gts_read) CLOSE ( iunit ) 
            EXIT read_obs

         END IF 

      ! this scenario should be handled right after the first read
      !ELSE IF ( io_error .LT. 0 ) THEN

      !   !  No errors.  This is the intended way to find the end of data mark.

      !   WRITE (UNIT = 0, FMT = '(A)') 'Have reached end of observations file. '

      !   CLOSE ( file_num ) 
      !   IF (print_gts_read) CLOSE ( iunit ) 
      !   EXIT read_obs

      ELSE IF ( io_error .EQ. 0 ) THEN 

         IF ( domain_check_h ) then

           CALL inside_domain (obs(obs_num)%location%latitude  , &
                               obs(obs_num)%location%longitude , &
                               ins , jew , outside_domain , &
                               obs(obs_num)%location%xjc, &
                               obs(obs_num)%location%yic, &
                               obs(obs_num)%location%xjd, &
                               obs(obs_num)%location%yid)
         else
           outside_domain = .FALSE.
         endif
          
         !  The previous read ("once only" data) was valid.  
         !  If any of the data is suspicious, the easiest place to clean 
         !  it up is as soon as we read it in, so we do not have to track 
         !  through the array or accidently hit undefined values that are 
         !   not exactly "our" undefined values.

         !  Sometimes the date and time are ingested in a silly way.  
         !  Set the valid time to a time guaranteed not to be within 
         !  the time window.

         IF      (INDEX (obs(obs_num)%valid_time%date_char , '*' ) .GT. 0 ) THEN
                     obs(obs_num)%valid_time%date_char = '19000101000000'
         ELSE IF  ((obs(obs_num)%valid_time%date_char( 1: 2) .NE. '19' ) .AND. &
                   (obs(obs_num)%valid_time%date_char( 1: 2) .NE. '20' ) ) THEN
                    obs(obs_num)%valid_time%date_char = '19000101000000'
         ELSE IF  ((obs(obs_num)%valid_time%date_char( 5: 6) .LT. '01' ) .OR.  &
                   (obs(obs_num)%valid_time%date_char( 5: 6) .GT. '12' )) THEN
                    obs(obs_num)%valid_time%date_char = '19000101000000'
         ELSE IF  ((obs(obs_num)%valid_time%date_char( 7: 8) .LT. '01' ) .OR.  &
                   (obs(obs_num)%valid_time%date_char( 7: 8) .GT. '31' )) THEN
                    obs(obs_num)%valid_time%date_char = '19000101000000'
         ELSE IF  ((obs(obs_num)%valid_time%date_char( 9:10) .LT. '00' ) .OR.  &
                   (obs(obs_num)%valid_time%date_char( 9:10) .GT. '23' )) THEN
                    obs(obs_num)%valid_time%date_char = '19000101000000'
         ELSE IF  ((obs(obs_num)%valid_time%date_char(11:12) .LT. '00' ) .OR.  &
                   (obs(obs_num)%valid_time%date_char(11:12) .GT. '59' )) THEN
                    obs(obs_num)%valid_time%date_char = '19000101000000'
         ELSE IF  ((obs(obs_num)%valid_time%date_char(13:14) .LT. '00' ) .OR.  &
                   (obs(obs_num)%valid_time%date_char(13:14) .GT. '59' )) THEN
                    obs(obs_num)%valid_time%date_char = '19000101000000'
         ELSE IF (((obs(obs_num)%valid_time%date_char( 5: 6) .EQ. '04' ) .OR.  &
                   (obs(obs_num)%valid_time%date_char( 5: 6) .EQ. '06' ) .OR.  &
                   (obs(obs_num)%valid_time%date_char( 5: 6) .EQ. '09' ) .OR.  &
                   (obs(obs_num)%valid_time%date_char( 5: 6) .EQ. '11' )) .AND.&
                   (obs(obs_num)%valid_time%date_char( 7: 8) .GT. '30' ) ) THEN
                    obs(obs_num)%valid_time%date_char = '19000101000000'
         ELSE IF  ((obs(obs_num)%valid_time%date_char( 5: 6) .EQ. '02' ) .AND. &
                   (nfeb_ch ( obs(obs_num)%valid_time%date_char( 1: 4) ) .LT.  &
                    obs(obs_num)%valid_time%date_char( 7: 8) ) ) THEN
                    obs(obs_num)%valid_time%date_char = '19000101000000'
         END IF

         CALL inside_window (obs(obs_num)%valid_time%date_char, &
                              time_window_min, time_window_max, &
                              outside_window, iunit)

         outside = outside_domain .OR. outside_window
    
         !  Date at MM5 V3 format

         WRITE (obs(obs_num)%valid_time%date_mm5, &
                FMT='(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)') &
                obs(obs_num)%valid_time%date_char ( 1: 4),     &
                obs(obs_num)%valid_time%date_char ( 5: 6),     &
                obs(obs_num)%valid_time%date_char ( 7: 8),     &
                obs(obs_num)%valid_time%date_char ( 9:10),     &
                obs(obs_num)%valid_time%date_char (11:12),     &
                obs(obs_num)%valid_time%date_char (13:14)

         !  These tests are for the ground type data.  
         !  Missing data is OK, but sometimes it comes in as undefined, 
         !  which meant bad data.  Set it all to missing.

         IF ((obs(obs_num)%ground%slp%data .GT. (undefined1_r - 1.))  .OR.     &
             (obs(obs_num)%ground%slp%data .LT. (undefined2_r + 1.))) THEN
              obs(obs_num)%ground%slp%data  = missing_r
              obs(obs_num)%ground%slp%qc    = missing
         END IF
         IF ((obs(obs_num)%ground%ref_pres%data .GT. (undefined1_r - 1.)) .OR. &
             (obs(obs_num)%ground%ref_pres%data .LT. (undefined2_r + 1.))) THEN
              obs(obs_num)%ground%ref_pres%data = missing_r
              obs(obs_num)%ground%ref_pres%qc   = missing
         END IF
         IF ((obs(obs_num)%ground%ground_t%data .GT. (undefined1_r - 1.)) .OR. &
             (obs(obs_num)%ground%ground_t%data .LT. (undefined2_r + 1.))) THEN
              obs(obs_num)%ground%ground_t%data = missing_r
              obs(obs_num)%ground%ground_t%qc   = missing
         END IF
         IF ((obs(obs_num)%ground%sst%data .GT. (undefined1_r - 1.))  .OR.     &
             (obs(obs_num)%ground%sst%data .LT. (undefined2_r + 1.))) THEN
              obs(obs_num)%ground%sst%data = missing_r
              obs(obs_num)%ground%sst%qc   = missing
         END IF
         IF ((obs(obs_num)%ground%psfc%data .GT. (undefined1_r - 1.))  .OR.    &
             (obs(obs_num)%ground%psfc%data .LT. (undefined2_r + 1.))) THEN 
              obs(obs_num)%ground%psfc%data = missing_r
              obs(obs_num)%ground%psfc%qc   = missing
         END IF
         IF ((obs(obs_num)%ground%precip%data .GT. (undefined1_r - 1.))  .OR.  &
             (obs(obs_num)%ground%precip%data .LT. (undefined2_r + 1.))) THEN
              obs(obs_num)%ground%precip%data = missing_r
              obs(obs_num)%ground%precip%qc   = missing
         END IF
         IF ((obs(obs_num)%ground%t_max%data .GT. (undefined1_r - 1.))  .OR.   &
             (obs(obs_num)%ground%t_max%data .LT. (undefined2_r + 1.))) THEN
              obs(obs_num)%ground%t_max%data = missing_r
              obs(obs_num)%ground%t_max%qc   = missing
         END IF
         IF ((obs(obs_num)%ground%t_min%data .GT. (undefined1_r - 1.))  .OR.   &
             (obs(obs_num)%ground%t_min%data .LT. (undefined2_r + 1.))) THEN
              obs(obs_num)%ground%t_min%data = missing_r
              obs(obs_num)%ground%t_min%qc   = missing
         END IF
         IF ((obs(obs_num)%ground%t_min_night%data .GT. (undefined1_r - 1.))   &
        .OR. (obs(obs_num)%ground%t_min_night%data .LT. (undefined2_r + 1.)))  &
        THEN
              obs(obs_num)%ground%t_min_night%data = missing_r
              obs(obs_num)%ground%t_min_night%qc   = missing
         END IF
         IF ((obs(obs_num)%ground%p_tend03%data .GT. (undefined1_r - 1.)) .OR. &
             (obs(obs_num)%ground%p_tend03%data .LT. (undefined2_r + 1.))) THEN
              obs(obs_num)%ground%p_tend03%data = missing_r
              obs(obs_num)%ground%p_tend03%qc   = missing
         END IF
         IF ((obs(obs_num)%ground%p_tend24%data .GT. (undefined1_r - 1.)) .OR. &
             (obs(obs_num)%ground%p_tend24%data .LT. (undefined2_r + 1.))) THEN
              obs(obs_num)%ground%p_tend24%data = missing_r
              obs(obs_num)%ground%p_tend24%qc   = missing
         END IF
         IF ((obs(obs_num)%ground%cloud_cvr%data .GT. (undefined1_r - 1.)) .OR.&
             (obs(obs_num)%ground%cloud_cvr%data .LT. (undefined2_r + 1.))) THEN
              obs(obs_num)%ground%cloud_cvr%data = missing_r
              obs(obs_num)%ground%cloud_cvr%qc   = missing
         END IF
         IF ((obs(obs_num)%ground%ceiling%data .GT. (undefined1_r - 1.)) .OR.  &
             (obs(obs_num)%ground%ceiling%data .LT. (undefined2_r + 1.))) THEN
              obs(obs_num)%ground%ceiling%data = missing_r
              obs(obs_num)%ground%ceiling%qc   = missing
         END IF

         !  Write obs id in diagnostic file

         IF (print_gts_read) THEN
             IF (outside_domain) THEN
                 WRITE (UNIT = iunit , FMT = '(A)' ) '=> OUT DOMAIN'
             ELSE IF (outside_window) THEN
                 WRITE (UNIT = iunit , FMT = '(A)' ) '=> OUT WINDOW'
             ELSE
                 WRITE (UNIT = iunit , FMT = '(A)' ) ''
             ENDIF
         ENDIF

         !  Sort observation per type

         READ (obs(obs_num) % info % platform (4:6), '(I3)')fm
         CALL fm_decoder (fm, platform, &
                          synop=nsynops (icor), ship =nshipss (icor), &
                          metar=nmetars (icor), pilot=npilots (icor), &
                          sound=nsounds (icor), satem=nsatems (icor), &
                          satob=nsatobs (icor), airep=naireps (icor), &
                          gpspw=ngpspws (icor), gpszd=ngpsztd (icor), &
                          gpsrf=ngpsref (icor), gpsep=ngpseph (icor), &
                          ssmt1=nssmt1s (icor), &
                          ssmt2=nssmt2s (icor), ssmi =nssmis  (icor), &
                          tovs =ntovss  (icor), other=nothers (icor), &
                          amdar=namdars (icor), qscat=nqscats (icor), &
                          profl=nprofls (icor), buoy =nbuoyss (icor), &
                          bogus=nboguss (icor), airs = nairss(icor),tamdar=ntamdar(icor) )

         !  Since no I/O errors, read 1 or more measurements.
         !  Note that obs(obs_num)%surface is pointer to first node in linked 
         !  list, so it is initially not pointing to anything.  

         NULLIFY (obs(obs_num)%surface)

         CALL read_measurements( file_num , obs(obs_num)%surface , &
         obs(obs_num)%location , obs(obs_num)%info, outside , error_ret ,& 
         ins , jew , map_projection,      &
         obs (obs_num) % info % elevation, obs (obs_num) % info % levels, &
         iunit, print_gts_read)
         
         !  An error in the measurements read is handled in a couple of ways.  
         !  A flat out error in the read requires the process to start again 
         !  (cycle to read_obs).  If there was no data, we need to clean up 
         !  a bit of stuff, and read the famous last three integers that 
         !  have some QC information.

!         write(0,'("N=",I6,"  info%levels=",3I8)') &
!                   obs_num, obs (obs_num) % info % levels

         IF (error_ret .EQ. read_err ) THEN

            IF (ASSOCIATED (obs(obs_num)%surface ) ) THEN
               !  dealloc entire linked list if it exists
               CALL dealloc_meas (obs(obs_num)%surface)
            END IF

            WRITE (UNIT = 0, FMT =  '(A,A,A,1X,A)')   &
                  "Troubles with measurement lines ", &
                   TRIM (obs(obs_num)%location%id),   &
                   TRIM (obs(obs_num)%location%name) 

            bad_count = 0
            io_error  = 0

            DO WHILE (io_error .GE. 0)

            bad_count = bad_count + 1

            IF ( bad_count .LT. 1000 ) THEN

!           READ (file_num , IOSTAT = io_error , FMT = meas_format) dummy_middle
            READ (file_num , IOSTAT = io_error , FMT = meas_format)      &
            dummy_middle % pressure    % data, dummy_middle % pressure    % qc,&
            dummy_middle % height      % data, dummy_middle % height      % qc,&
            dummy_middle % temperature % data, dummy_middle % temperature % qc,&
            dummy_middle % dew_point   % data, dummy_middle % dew_point   % qc,&
            dummy_middle % speed       % data, dummy_middle % speed       % qc,&
            dummy_middle % direction   % data, dummy_middle % direction   % qc,&
            dummy_middle % u           % data, dummy_middle % u           % qc,&
            dummy_middle % v           % data, dummy_middle % v           % qc,&
            dummy_middle % rh          % data, dummy_middle % rh          % qc,&
            dummy_middle % thickness   % data, dummy_middle % thickness   % qc

            IF (eps_equal (dummy_middle%pressure%data, end_data_r , 1.)) THEN

                READ (file_num , IOSTAT = io_error , FMT = end_format ) &
                      obs(obs_num)%info%num_vld_fld , &
                      obs(obs_num)%info%num_error , &  
                      obs(obs_num)%info%num_warning    

                WRITE (UNIT = 0, FMT = '(A)') 'Starting to READ a new report.'
                CYCLE read_obs

            END IF

            ELSE

                WRITE (UNIT = 0, FMT = '(A)') &
               'Too many attempts to read the measurement data correctly.',&
               'Exiting read loop.'

                CLOSE ( file_num ) 
                IF (print_gts_read) CLOSE ( iunit ) 

                EXIT read_obs
            END IF

            END DO 

            IF (io_error .LT. 0) THEN
                CLOSE ( file_num ) 
                IF (print_gts_read) CLOSE ( iunit ) 
                EXIT read_obs
            END IF

         ELSE IF (error_ret .EQ. no_data .and. &
                  eps_equal(obs(obs_num)%ground%pw %data,missing_r,1.0) .and.&
                        obs(obs_num)%ground%tb19v%qc .ne. 0  .and. &
                        obs(obs_num)%ground%tb19h%qc .ne. 0  .and. &
                        obs(obs_num)%ground%tb22v%qc .ne. 0  .and. &
                        obs(obs_num)%ground%tb37v%qc .ne. 0  .and. &
                        obs(obs_num)%ground%tb37h%qc .ne. 0  .and. &
                        obs(obs_num)%ground%tb85v%qc .ne. 0  .and. &
                        obs(obs_num)%ground%tb85h%qc .ne. 0  .and. &
                  eps_equal(obs(obs_num)%ground%slp%data,missing_r,1.0) ) THEN

!           IF (print_gts_read ) THEN
!              WRITE (UNIT = 0 , FMT = '(A)' ) ' => NO DATA'
!           END IF 

            READ (file_num , IOSTAT = io_error , FMT = end_format ) &
                  obs(obs_num)%info%num_vld_fld , &
                  obs(obs_num)%info%num_error , &  
                  obs(obs_num)%info%num_warning    

            READ (obs(obs_num) % info % platform (4:6), '(I3)') fm

            CALL fm_decoder (fm, platform, &
                             synop=nsynops (icor+1), ship =nshipss (icor+1), &
                             metar=nmetars (icor+1), pilot=npilots (icor+1), &
                             sound=nsounds (icor+1), satem=nsatems (icor+1), &
                             satob=nsatobs (icor+1), airep=naireps (icor+1), &
                             gpspw=ngpspws (icor+1), gpszd=ngpsztd (icor+1), &
                             gpsrf=ngpsref (icor+1), gpsep=ngpseph (icor+1), &
                             ssmt1=nssmt1s (icor+1), &
                             ssmt2=nssmt2s (icor+1), ssmi =nssmis  (icor+1), &
                             tovs =ntovss  (icor+1), other=nothers (icor+1), &
                             amdar=namdars (icor+1), qscat=nqscats (icor+1), &
                             profl=nprofls (icor+1), buoy =nbuoyss (icor+1), &
                             bogus=nboguss (icor+1), airs =nairss  (icor+1), tamdar =ntamdar (icor+1)  )

            IF ( ASSOCIATED (obs(obs_num)%surface)) THEN
               !  dealloc entire linked list if it exists
               CALL dealloc_meas ( obs(obs_num)%surface)
            END IF

            num_empty = num_empty + 1

            CYCLE read_obs

         END IF

         !  We can compare the observation location with the spacial domain 
         !  and time window that the analysis will require. 
         !  If we are significantly outside,  we toss out the observation.

         IF (outside) THEN

!           IF (print_gts_read) THEN
!              WRITE (UNIT = 0 , FMT = '(A)' ) ' => OUTSIDE'
!           END IF 

            READ (file_num , IOSTAT = io_error , FMT = end_format ) &
                  obs(obs_num)%info%num_vld_fld , &
                  obs(obs_num)%info%num_error , &  
                  obs(obs_num)%info%num_warning    

            READ (obs(obs_num) % info % platform (4:6), '(I3)') fm

            CALL fm_decoder (fm, platform, &
                             synop=nsynops (icor+2), ship =nshipss (icor+2), &
                             metar=nmetars (icor+2), pilot=npilots (icor+2), &
                             sound=nsounds (icor+2), satem=nsatems (icor+2), &
                             satob=nsatobs (icor+2), airep=naireps (icor+2), &
                             gpspw=ngpspws (icor+2), gpszd=ngpsztd (icor+2), &
                             gpsrf=ngpsref (icor+2), gpsep=ngpseph (icor+2), &
                             ssmt1=nssmt1s (icor+2), &
                             ssmt2=nssmt2s (icor+2), ssmi =nssmis  (icor+2), &
                             tovs =ntovss  (icor+2), other=nothers (icor+2), &
                             amdar=namdars (icor+2), qscat=nqscats (icor+2), &
                             profl=nprofls (icor+2), buoy =nbuoyss (icor+2), &
                             bogus=nboguss (icor+2), airs =nairss  (icor+2),tamdar =ntamdar (icor+2) )

            IF ( ASSOCIATED (obs(obs_num)%surface)) THEN
               !  dealloc entire linked list if it exists
               CALL dealloc_meas ( obs(obs_num)%surface)
            END IF

            num_outside = num_outside + 1

            CYCLE read_obs

         ELSE

      ! Elevation can sometimes be undefined

          IF ( (obs(obs_num)%info%elevation .GT. (undefined1_r - 1.))  .OR. &
               (obs(obs_num)%info%elevation .LT. (undefined2_r + 1.)) ) THEN

             obs(obs_num)%info % elevation  = missing_r

             ! set elevation to 0.0 for marine reports, excluding Great Lakes
             if ( fm .eq. 13 .or. fm .eq. 18 .or. fm .eq. 19 .or.   &
                  fm .eq. 33 .or. fm .eq. 36 ) then
                if ( obs(obs_num)%location%latitude .lt. 41. .or. &
                     obs(obs_num)%location%latitude .gt. 50. .or. &
                     obs(obs_num)%location%longitude .lt. -95. .or. &  ! ncep used -93
                     obs(obs_num)%location%longitude .gt. -75. ) then
                   obs(obs_num)%info % elevation = 0.
                end if
             else if (fm < 39) then
                m_miss = m_miss + 1
                write(0,'(I7,1X,A,1X,A,1X,A,1X,A,1X,2(F8.3,A),A,1X,f11.3)')&
                   m_miss,'Missing elevation(id,name,platform,lat,lon,date,elv:',  &
                   obs(obs_num)%location%id   (1: 5),&
                   obs(obs_num)%location%name (1:20),&
                   obs(obs_num)%info%platform (1: 12),&
                   obs(obs_num)%location%latitude, 'N',&
                   obs(obs_num)%location%longitude,'E ', &
                   obs(obs_num)%valid_time%date_char,    &
                   obs(obs_num)%info % elevation
             endif

             ! assigning elevation info for ships and buoys located in the Great
             ! Lakes.
             !http://www.nco.ncep.noaa.gov/pmb/codes/nwprod/decoders/decod_dcmsfc/sorc/maelev.f
             xla = obs(obs_num)%location%latitude
             xlo = obs(obs_num)%location%longitude
             if ( fm .eq. 13 .or. fm .eq. 33 .or. fm .eq. 36 ) then
                ! for ships
                if ( ( xlo .ge. -92.5  .and. xlo .le. -84.52 )  .and.    &
                     ( xla .ge.  46.48 .and. xla .le.  49.0 ) ) then
                   !Ship located in Lake Superior
                   obs(obs_num)%info % elevation = 183.
                else if ( ( xlo .ge. -88.1 .and. xlo .le. -84.8 ) .and.  &
                          ( xla .ge. 41.2  .and. xla .le.  46.2 ) ) then
                   !Ship located in Lake Michigan
                   obs(obs_num)%info % elevation = 176.
                else if ( ( xlo .ge. -84.8 .and. xlo .le. -79.79 ) .and. &
                          ( xla .ge. 43.0  .and. xla .le.  46.48 ) ) then
                   !Ship located in Lake Huron or Georgian Bay
                   obs(obs_num)%info % elevation = 176.
                else if ( ( xlo .ge. -84.0 .and. xlo .le. -78.0 ) .and.  &
                          ( xla .ge. 41.0  .and. xla .le.  42.9 ) ) then
                   !Ship located in Lake Erie
                   obs(obs_num)%info % elevation = 174.
                else if ( ( xlo .ge. -80.0 .and. xlo .le. -76.0 ) .and.  &
                       ( xla .ge. 43.1  .and. xla .le.  44.23 ) ) then
                   !Ship located in Lake Ontario
                   obs(obs_num)%info % elevation = 74.
                end if
             end if !end if ships
             if ( fm .eq. 18 .and. obs(obs_num)%location%id(1:2) .eq. '45' ) then
                ! for Great Lakes fixed buoys
                ! get station elevation from station table if available
                if ( use_msfc_tbl .and. num_stations_msfc > 0 ) then
                   ipos = 0
                   do while ( ipos < num_stations_msfc )
                      ipos = ipos + 1
                      if ( obs(obs_num)%location%id(1:5) == id_tbl(ipos) ) then
                         obs(obs_num)%info % elevation = elev_tbl(ipos)
                         exit
                      end if
                   end do
                end if !table info available
             end if ! end if buoys
          END IF ! missing elevation

         END IF  ! not outside

         ! for gts_from_mmm_archive:
         ! METARs (FM-15) never report surface pressure. METARs usually
         ! report altimeter setting. The decoder places the altimeter
         ! setting into the little_r surface pressure data record.
         ! U.S. and Canadian METARs often contain SLP and temperatures
         ! in tenths of degree in the remarks section (RMK). These are
         ! decoded. JFB

         IF ((obs (obs_num)%info%platform(1:12) .EQ. 'FM-15 METAR ' .or. &
              obs (obs_num)%info%platform(1:12) .EQ. 'FM-16 SPECI ') .AND. &
             (ASSOCIATED (obs (obs_num)%surface ) ) ) THEN
            if ( calc_psfc_from_QNH .and. gts_from_mmm_archive ) then
               if ( obs(obs_num)%ground%psfc%data > 0.0 .and. &
                    obs(obs_num)%info%elevation > 0.0 ) then
                  QNH  = obs(obs_num)%ground%psfc%data * 0.01 ! Pa to hPa
                  elev = obs(obs_num)%info%elevation
                  obs(obs_num)%ground%psfc%data = psfc_from_QNH(QNH,elev) &
                                                  * 100.0  ! hPa to Pa
                  obs(obs_num)%ground%psfc%qc   = 0
                  if ( associated(obs(obs_num)%surface) ) then
                     obs(obs_num)%surface%meas%pressure%data = &
                        obs(obs_num)%ground%psfc%data
                     obs(obs_num)%surface%meas%pressure%qc   = &
                        obs(obs_num)%ground%psfc%qc
                  end if  ! associated data
               end if  ! valid QNH and elev
            else
               obs(obs_num)%ground%psfc%data = missing_r
               obs(obs_num)%ground%psfc%qc   = missing
            end if  ! calc_psfc_from_QNH and gts_from_mmm_archive
         END IF  ! metar

         ! for gts_from_mmm_archive:
         ! For ship data (FM-13 or FM-18), the elevation is missing and
         ! surface (or station) pressure is set to 1013.01 hPa. This
         ! value is a flag to indicate a suface report (the elevation of
         ! a ship or buoy does not have to be at sea level as in the case
         ! of Lake Michigan). JFB

         !  If this is a ship observation, we need to define the elevation 
         !  as identical to the geopotential height, and set the height QC flag
         !  to ok.  This is the only way to get SHIP data into the surface 
         !  analysis.  Since we are at sea level, we also set the pressure 
         !  to equal to the sea level pressure.

         IF ( (obs(obs_num)%info%platform(1:10) == 'FM-13 SHIP') .or. &
              (obs(obs_num)%info%platform(1:10) == 'FM-18 BUOY') ) then
            if ( ASSOCIATED(obs(obs_num)%surface) ) then
               obs(obs_num)%surface%meas%height%data   = &
                  obs(obs_num)%info%elevation
               obs(obs_num)%surface%meas%height%qc     = 0
               if ( (obs(obs_num)%info%elevation == 0.0) ) then
                  !obs(obs_num)%surface%meas%height%data   = &
                  !   obs(obs_num)%info%elevation
                  !obs(obs_num)%surface%meas%height%qc     = 0
                  obs(obs_num)%surface%meas%pressure%data = &
                     obs(obs_num)%ground%slp%data
                  obs(obs_num)%surface%meas%pressure%qc   = 0
               else
                  if ( eps_equal(obs(obs_num)%surface%meas%pressure%data, &
                                 101301.000, 1.) ) then
                     ! replace 1013.01 with missing value
                     obs(obs_num)%surface%meas%pressure%data = missing_r
                     obs(obs_num)%surface%meas%pressure%qc   = missing
                  end if
               end if  ! elev is 0
            end if  ! has associated data
         end if  ! end if ship or buoy

         ! FENG GAO 03/07/2014
         ! QuikSCAT nominal mission ended on November 23, 2009
         ! NOW FM-281 denote qscat only for research and
         ! ASCAT 10-m ocean surface wind for research and application

         IF ((obs (obs_num)%info%platform(1:6) .EQ. 'FM-281' ) .and. &
             (ASSOCIATED (obs (obs_num)%surface ) ) ) THEN
             if (obs(obs_num)%info%elevation .LT. 0.0) then
                 obs(obs_num)%surface%meas%height%data = 10.0
             else
                 obs(obs_num)%surface%meas%height%data = &
                 obs(obs_num)%info%elevation
             end if
             obs(obs_num)%surface%meas%height%qc = 0
          END IF

         ! for gts_from_mmm_archive:
         ! SYNOP reports (FM-12) have a flag of 1013.01 in the surface
         ! pressure field to indicate surface data. However, the SYNOP
         ! code does include a station pressure group. The observed
         ! station pressure will be decoded and replace 1013.01 if it
         ! is available.  JFB

         ! YRG 04/04/2009
         ! For SYNOP, if surface%meas%pressure%data = 101301.000 
         ! (101301.000 is a fake value in NCAR archived LITTLE_R file)
         ! and the slp is missing (note if SLP is available, WRFVar 
         ! will use the SLP to derive Psfc and ignore the original Psfc, 
         ! see da_tools/da_obs_sfc_correction.inc),  
         ! fill in surface%meas%pressure%data with ground%psfc%data:

         ! hcl-note: disagrees with the above slp to psfc procedure.
         !     if both slp and psfc are available for synop reports,
         !     psfc should be used in DA system
         ! hcl-note2: 101301 is simply a flag, real reports do not 
         !     have 1/100 precision. Just replace 101301 with missing value

         IF ( (obs(obs_num)%info%platform(1:5).EQ.'FM-12') .and.  &
              (ASSOCIATED(obs(obs_num)%surface)) ) THEN
            if ( eps_equal(obs(obs_num)%surface%meas%pressure%data, &
                                          101301.000, 1.) ) then
               obs(obs_num)%surface%meas%pressure%data = missing_r
               obs(obs_num)%surface%meas%pressure%qc   = missing
            endif
         ENDIF  !end if FM-12 synop

         !  This may be wasted print-out, but it is comforting to see.

!        IF (print_gts_read ) THEN

!            IF (obs(obs_num)%info%is_sound) THEN
!                WRITE (UNIT =  0, FMT = '(A)') ' => SOUNDING'
!            ELSE
!                WRITE (UNIT =  0, FMT = '(A)') ' => SURFACE'
!            ENDIF
                 
!        END IF

      END IF

      !  We have now read ground info and soundings, what follows in the
      !  standard format are three integers describing information gleaned
      !  from the program that generated the observational data.

      READ (file_num , IOSTAT = io_error , FMT = end_format ) &
            obs(obs_num)%info%num_vld_fld , &
            obs(obs_num)%info%num_error , &  
            obs(obs_num)%info%num_warning    

      !  Once again, after a read, was it successful.  If not toss the whole 
      !  thing out (this is handled through the dealloc_meas routine if any 
      !  upper-air data was encountered).  Discarding all of the ingested 
      !  data may be a bit much,  which is why the error print-out is provided.
      !  After the error is processed, the reading process is re-started.

      IF ( io_error .NE. 0 ) THEN

         error_message = &
              'Error trying to read last 3 integers in observation '&
              // TRIM ( obs(obs_num)%location%id ) &               
              // TRIM ( obs(obs_num)%location%name ) // '.'

         CALL error_handler (proc_name, error_message, &
                           ' Discarding entire and continuing.', .FALSE.)

         IF ( ASSOCIATED ( obs(obs_num)%surface ) ) THEN
            CALL dealloc_meas ( obs(obs_num)%surface)
         END IF

         CYCLE read_obs

      END IF
   
      !  Before we leave this loop, we make sure the surface level is the first
      !  level in the sounding.

      !  This test might results in removing levels

      IF (.NOT. eps_equal(obs(obs_num)%info%elevation, missing_r, 1.)) &
      CALL surf_first ( obs(obs_num)%surface , obs(obs_num)%info%elevation )

      ! Height and pressure when missing were replaced by ICAO values, restore

      CALL missing_hp ( obs(obs_num)%surface )

      !  Update the info % levels after surf_first test

      obs (obs_num) % info % levels = info_levels (obs(obs_num)%surface)

      !  Update the info % is_sound on the updated info % levels 

      IF      (obs (obs_num) % info % levels .GT. 1) THEN
               obs (obs_num) % info % is_sound = .TRUE.
      ELSE IF (obs (obs_num) % info % levels .EQ. 1) THEN
               obs (obs_num) % info % is_sound = .FALSE.

! Guo, 02/26/2003 -- When slp or pw is not available, then ...
 
      ELSE IF (eps_equal(obs(obs_num)%ground%pw %data,missing_r,1.0) .and.&
                         obs(obs_num)%ground%tb19v%qc .ne. 0  .and. &
                         obs(obs_num)%ground%tb19h%qc .ne. 0  .and. &
                         obs(obs_num)%ground%tb22v%qc .ne. 0  .and. &
                         obs(obs_num)%ground%tb37v%qc .ne. 0  .and. &
                         obs(obs_num)%ground%tb37h%qc .ne. 0  .and. &
                         obs(obs_num)%ground%tb85v%qc .ne. 0  .and. &
                         obs(obs_num)%ground%tb85h%qc .ne. 0  .and. &
               eps_equal(obs(obs_num)%ground%slp%data,missing_r,1.0) ) THEN
          ! Station are expected with at least one level here
          obs(obs_num) % info % discard = .TRUE.
          CYCLE read_obs
      ENDIF

      !  Have read observation and its measurement(s) without error
      !  so continue to next observation.

      obs_num = obs_num + 1

   END DO read_obs


   !  The end of the observation file has been reached.  Decrement the counter 
   !  to get the total number of observations successfully read by the program.
   !  Output this information to the outside world.  We can also provide 
   !  the information on the observations that are NOT included in the analysis.

   obs_num = obs_num - 1

   !  Print out number of read observations  per type

   WRITE (UNIT = 0, FMT = '(/,A)')  &
'------------------------------------------------------------------------------'
   WRITE (UNIT = 0, FMT = '(A)') 'GTS OBSERVATIONS READ:'

   WRITE (UNIT = 0, FMT = '(A)')
   WRITE (UNIT = 0, FMT = '(A,I7)') ' SYNOP reports:',nsynops (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' SHIPS reports:',nshipss (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' BUOYS reports:',nbuoyss (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' BOGUS reports:',nboguss (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' METAR reports:',nmetars (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' PILOT reports:',npilots (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' SOUND reports:',nsounds (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' AMDAR reports:',namdars (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' SATEM reports:',nsatems (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' SATOB reports:',nsatobs (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' GPSPW reports:',ngpspws (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' GPSZD reports:',ngpsztd (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' GPSRF reports:',ngpsref (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' GPSEP reports:',ngpseph (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' AIREP reports:',naireps (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') 'TAMDAR reports:',ntamdar (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' SSMT1 reports:',nssmt1s (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' SSMT2 reports:',nssmt2s (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' SSMI  reports:',nssmis  (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' TOVS  reports:',ntovss  (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' QSCAT reports:',nqscats (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' PROFL reports:',nprofls (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' AIRST reports:',nairss  (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' OTHER reports:',nothers (0)
   WRITE (UNIT = 0, FMT = '(A,I7)') ' Total reports:', &
          nsynops (0) + nshipss (0) + nmetars (0) + npilots (0) + nsounds (0)+&
          nsatems (0) + nsatobs (0) + naireps (0) +  ntamdar (0)+ ngpspws (0) + ngpsztd (0)+&
          ngpsref (0) + ngpseph (0) + &
          nssmt1s (0) + nssmt2s (0) + nssmis  (0) + ntovss  (0) + nboguss (0)+&
          nothers (0) + namdars (0) + nqscats (0) + nprofls(0)  + nbuoyss(0) +&
          nairss(0)

   !  Print number of observation ingested

!  WRITE (0,'(/,A)')  &
!'------------------------------------------------------------------------------'
!   WRITE (UNIT = 0, FMT = '(A)') 'INGESTED GTS OBSERVATIONS:'
    
    WRITE (UNIT = 0, FMT = '(/,4(A,i8,/))' ) &

          "Number of observations read:          ",obs_num+    &
                                                   num_empty+  &
                                                   num_outside-&
                                                   n_obs,      &
          "Number of empty observations:         ",num_empty,  &
          "Number of observations out of domain: ",num_outside,&
          "Number of observations for ingestion: ",obs_num-n_obs

   !  Total number of observation accumulated

    n_obs = obs_num

    write(0,'(/"AIRCRAFT DATA: Total=",I7,"  Above cut_height=",I7)')&
                                                     N_air, N_air_cut
contains

subroutine print_extra_obs

  READ (obs(obs_num) % info % platform (4:6), '(I3)', &
                                          IOSTAT = platform_error) fm
!   synop    12,14       'SYNOP','SYNOP MOBIL'
  if (fm == 12 .or. fm ==14) return

!   ship     13          'SHIP'
  if (fm == 13) return

!   metar    15,16       'METAR','SPECI'
  if (fm == 15 .or. fm == 16) return

!   buoy     18          'BUOY'
  if (fm == 18 .or. fm == 19) return

!   pilot    32,33,34    'PILOT','PILOT SHIP','PILOT MOBIL'
  if (fm >= 32 .and. fm <= 34) return

!   sound    35,36,37,38 'TEMP','TEMP SHIP, 'TEMP DROP','TEMP MOBIL'
  if (fm >= 35 .and. fm <= 38) return

!   amdar    42          'AMDAR'
  if (fm == 42) return

!   satem    86          'SATEM'
  if (fm == 86) return

!   satob    88          'SATOB'
  if (fm == 88) return

!   airep    96,97       'AIREP'
  if (fm == 96 .or. fm == 97) return

!   tamdar   101         'TAMDAR'
  if (fm == 101) return

!   gpspw    111         'GPSPW'
  if (fm == 111) return

!   gpsztd   114         'GPSZD'
  if (fm == 114) return

!   gpsref   116         'GPSRF'
  if (fm == 116) return

!   gpseph   118         'GPSEP'
  if (fm == 118) return

!   ssmt1    121         'SSMT1'
  if (fm == 121) return

!   ssmt2    122         'SSMT2'
  if (fm == 122) return

!   ssmi     125,126     'SSMI'
  if (fm == 125 .or. fm == 126) return

!   tovs     131         'TOVS'
  if (fm == 131) return

!   qscat    281         'Quikscat'
  if (fm == 281) return

!   profl    132         'Profilers'
  if (fm == 132) return

!   bogus    135         'Bogus'
  if (fm == 135) return
!   AIRSRET  133         'Bogus'
  if (fm == 133) return


!   other Any other code 'UNKNOWN'
  
   num_unknown = num_unknown + 1
   write(0,'(2I8," ID=",a," Name=",a," Platform=",a)') &
                          num_unknown, obs_num, &
                          obs(obs_num)%location % id(1:15), &
                          obs(obs_num)%location % name, &
                          obs(obs_num)%info % platform
end subroutine print_extra_obs
 
END SUBROUTINE read_obs_gts

!
!---------------------------------------------------------------------------

SUBROUTINE read_measurements (file_num, surface, location, info, bad_data, &
                              error, ins, jew, &
                              map_projection, elevation, nlevels, iunit,&
                              print_gts_read)

!  This routine reads in 'measurements' at as many levels as there are in
!  the report, then stops and returns when an end-of-measurements flag is
!  found.  If any reads produce error, return error code which causes entire
!  observation to be discarded (ob is not discarded on eof error).

   USE module_icao

   IMPLICIT NONE 

   INTEGER , INTENT ( IN )                      :: file_num   ! file to read  
   TYPE ( measurement ) , POINTER               :: surface    ! ptr to 1st msmt
   TYPE ( location_type ) , INTENT ( IN )       :: location   ! 5 digit ID, name
   TYPE ( source_info ) ,   INTENT ( INOUT )    :: info       ! 5 digit ID, name
   LOGICAL , INTENT ( IN )                      :: bad_data   ! read, not store
   INTEGER , INTENT ( OUT )                     :: error      ! err and type 

   INTEGER                                      :: ins , jew, k
   INTEGER                                      :: map_projection

   CHARACTER ( LEN = 32 ) , PARAMETER    :: proc_name = 'read_measurements'
   INTEGER                                      :: meas_count
   INTEGER                                      :: io_error
   TYPE ( measurement ) , POINTER               :: current

   CHARACTER ( LEN = 40 )                       :: location_id , &
                                                   location_name
   REAL , INTENT(IN)                            :: elevation
   REAL                                         :: new_press, new_heightt, &
                                                   ref_h
   INTEGER, INTENT (out)                        :: nlevels
   INTEGER, INTENT (in)                         :: iunit
   LOGICAL, INTENT (in)                         :: print_gts_read
   LOGICAL                                      :: no_height, no_pressure
   LOGICAL                                      :: no_temperature

   INTEGER :: icrs, fm

!------------------------------------------------------------------------------!

!  INCLUDE 'error.inc'
!  INTERFACE
!     INCLUDE 'error.int'
!  END INTERFACE

   !  Initialize dummy pointers and counters and observation names, and such.

   ALLOCATE ( current )
   NULLIFY ( current%next )
   NULLIFY ( surface )
   error = ok
   meas_count = 0
   location_id   = TRIM ( location%id )
   location_name = TRIM ( location%name )

   !  This loop continues until either an error occurs, or until the end of
   !  the measurement tag is found (the graceful exit).

   read_meas: DO 

      !  Currently, this read puts in 12 pairs of data, a real observation
      !  value and the accompanying QC flag. 

!FV     READ ( file_num , IOSTAT = io_error , FMT = meas_format )  &
!FV            current%meas

      READ ( file_num , IOSTAT = io_error , FMT = meas_format )  &
      current % meas % pressure    % data, current % meas % pressure    % qc, &
      current % meas % height      % data, current % meas % height      % qc, &
      current % meas % temperature % data, current % meas % temperature % qc, &
      current % meas % dew_point   % data, current % meas % dew_point   % qc, & 
      current % meas % speed       % data, current % meas % speed       % qc, & 
      current % meas % direction   % data, current % meas % direction   % qc, &
      current % meas % u           % data, current % meas % u           % qc, & 
      current % meas % v           % data, current % meas % v           % qc, &
      current % meas % rh          % data, current % meas % rh          % qc, &
      current % meas % thickness   % data, current % meas % thickness   % qc

      !  An error < 0 means the end of the file (usually), and an error > 0
      !  is just a broken read.  Describe the read error so that the calling
      !  routine knows what happened, then exit this loop (which is exiting
      !  this routine, basically).

      IF (io_error .GT. 0 ) THEN
         error = read_err
!        CLOSE ( file_num ) 
         EXIT read_meas
      ELSE IF (io_error .LT. 0 ) THEN
         error = eof_err
         CLOSE ( file_num ) 
         IF (print_gts_read) CLOSE ( iunit ) 
         EXIT read_meas
      END IF

      !  If we know a priori that this data is bad, no tests are necessary on
      !  the various flags values.

      bad_loop_1 : IF (.NOT. bad_data) THEN
   
         !  A successful read, yahoo!  As the data may not have the flags 
         !  set up the way we want, go through directly after this read 
         !  and make sure that any special values are all set to missing.
   
         IF ((current%meas%pressure%data    .GE. ( undefined1_r - 10. ) ) .OR. &
             (current%meas%pressure%data    .LE. ( undefined2_r + 10. ) ) .OR. &
             (current%meas%pressure%data    .LE. 0.0)                      )THEN
              current%meas%pressure%data    = missing_r
              current%meas%pressure%qc      = missing
         END IF
         IF ((current%meas%height%data      .GT. ( undefined1_r - 1. ) )  .OR. &
             (current%meas%height%data      .LT. ( undefined2_r + 1. ) )  .OR. &
             (current%meas%height%data      .GT. ( height_max_icao - 1.))  .OR. &
             (current%meas%height%data      .GT. ( ABS (missing_r) - 1. ))) THEN
              current%meas%height%data      = missing_r
              current%meas%height%qc        = missing
         END IF
         IF ((current%meas%temperature%data .GT. ( undefined1_r - 1. ) )  .OR. &
             (current%meas%temperature%data .LT. ( undefined2_r + 1. ) ) ) THEN
              current%meas%temperature%data = missing_r
              current%meas%temperature%qc   = missing
         END IF
         IF  (current%meas%temperature%data .GT. (    99999.0   - 1. ) )   THEN
              current%meas%temperature%data = missing_r
              current%meas%temperature%qc   = missing
         END IF
         IF ((current%meas%dew_point%data   .GT. ( undefined1_r - 1. ) )  .OR. &
             (current%meas%dew_point%data   .LT. ( undefined2_r + 1. ) ) ) THEN
              current%meas%dew_point%data   = missing_r
              current%meas%dew_point%qc     = missing
         END IF
         IF ((current%meas%speed%data       .GT. ( undefined1_r - 1. ) )  .OR. &
             (current%meas%speed%data       .LT. ( undefined2_r + 1. ) ) ) THEN
              current%meas%speed%data       = missing_r
              current%meas%speed%qc         = missing
         END IF
         IF ((current%meas%direction%data   .GT. ( undefined1_r - 1. ) )  .OR. &
             (current%meas%direction%data   .LT. ( undefined2_r + 1. ) ) ) THEN
              current%meas%direction%data   = missing_r
              current%meas%direction%qc     = missing
         END IF
         IF ((current%meas%u%data           .GT. ( undefined1_r - 1. ) )  .OR. &
             (current%meas%u%data           .LT. ( undefined2_r + 1. ) ) ) THEN
              current%meas%u%data           = missing_r
              current%meas%u%qc             = missing
         END IF
         IF ((current%meas%v%data           .GT. ( undefined1_r - 1. ) )  .OR. &
             (current%meas%v%data           .LT. ( undefined2_r + 1. ) ) ) THEN
              current%meas%v%data           = missing_r
              current%meas%v%qc             = missing
         END IF
         IF ((current%meas%rh%data          .GT. ( undefined1_r - 1. ) )  .OR. &
             (current%meas%rh%data          .LT. ( undefined2_r + 1. ) ) ) THEN
              current%meas%rh%data          = missing_r
              current%meas%rh%qc            = missing
         END IF
         IF ((current%meas%thickness%data   .GT. ( undefined1_r - 1. ) )  .OR. &
             (current%meas%thickness%data   .LT. ( undefined2_r + 1. ) ) ) THEN
              current%meas%thickness%data   = missing_r
              current%meas%thickness%qc     = missing
         END IF

              current%meas%qv%data = missing_r
              current%meas%qv%qc   = missing

      END IF bad_loop_1

      !  The data we just read in could have been the flag for the end of 
      !  the measurement. This is the graceful way to exit this routine.  
      !  If this is the end of the measurement section for this observation, 
      !  set all of the data to the same end of measurement value, 
      !  just in case there were some stray unset values in the  generating 
      !  program.

      IF (eps_equal (current%meas%pressure%data , end_data_r , 1.) .OR. &
          eps_equal (current%meas%height%data   , end_data_r , 1.)) THEN
          current%meas%pressure%data    = end_data_r
          current%meas%height%data      = end_data_r
          current%meas%temperature%data = end_data_r
          current%meas%dew_point%data   = end_data_r
          current%meas%speed%data       = end_data_r
          current%meas%direction%data   = end_data_r
          current%meas%u%data           = end_data_r
          current%meas%v%data           = end_data_r
          current%meas%rh%data          = end_data_r
          current%meas%thickness%data   = end_data_r
          current%meas%pressure%qc      = end_data  
          current%meas%height%qc        = end_data  
          current%meas%temperature%qc   = end_data  
          current%meas%dew_point%qc     = end_data  
          current%meas%speed%qc         = end_data  
          current%meas%direction%qc     = end_data  
          current%meas%u%qc             = end_data  
          current%meas%v%qc             = end_data  
          current%meas%rh%qc            = end_data  
          current%meas%thickness%qc     = end_data  
          current%meas%qv%data          = end_data
          current%meas%qv%qc            = end_data
          error = ok

          EXIT read_meas

      !  Don't copy record if either press. or height missing, and 
      !                       wind, temp, dew point and rh are all missing 

      ELSEIF ((eps_equal(current%meas%pressure%data, missing_r , 1.) .OR. &
               eps_equal(current%meas%pressure%data, missing_r , 1.))   .AND. &
              eps_equal (current%meas%speed%data,       missing_r , 1.) .AND. &
              eps_equal (current%meas%direction%data,   missing_r , 1.) .AND. &
              eps_equal (current%meas%temperature%data, missing_r , 1.) .AND. &
              eps_equal (current%meas%dew_point%data,   missing_r , 1.) .AND. &
              eps_equal (current%meas%rh%data,          missing_r , 1.)) THEN

         CYCLE read_meas

      END IF

      !  If this is bad data, we needed to make sure that the ending measurement
      !  is the famous end_data flag so that we hit a correct exit from this
      !  loop and routine.  We can just cycle the read loop again.

      IF (bad_data) THEN
          CYCLE read_meas
      END IF

      !
      ! If both pressure and height are missing, throw out data at this level  
      !

      IF ((eps_equal ( current%meas%pressure%data , missing_r , 1.)) .AND. &
          (eps_equal  ( current%meas%height  %data , missing_r , 1.))) THEN
          CYCLE read_meas
      END IF

      if ( print_gts_read ) then
      IF ((     eps_equal (current%meas%dew_point%data , missing_r , 1.)) .AND.&
          (.NOT.eps_equal (current%meas%rh       %data , missing_r , 1.))) THEN
           WRITE (iunit,'(A,F10.2,/,A,F10.2)') &
          " Td = ",current%meas%dew_point%data,&
          " Rh = ",current%meas%rh%data  
      ENDIF
      end if

      !
      !  Assign the SSMI error (AFWA only)
      !

      ! initialize the variable that might be used in module_err_ncep.F90
      ! for checking if the error is pre-assigned
      current%meas%speed%error = 0.0

      READ (info % platform (4:6), '(I3)') fm

      IF ((fm .EQ. 125) .AND. (current%meas%speed%qc .GT. missing)) THEN 

      SELECT CASE (current%meas%speed%qc)

             CASE (0)
                 current%meas%speed%error = 2.  !m/s
             CASE (1)
                 current%meas%speed%error = 5.  !m/s
             CASE (2)
                 current%meas%speed%error = 10. !m/s
             CASE (3)
                 current%meas%speed%error = 20. !m/s
             CASE DEFAULT
                 current%meas%speed%error = 20. !m/s
      END SELECT

      current%meas%speed%qc = 0

      ELSE IF ((fm == 97 .or. fm == 96 .or. fm == 42) .and. &
               (current%meas%height%qc  == 0 ) ) then

               N_air = N_air + 1
               if (current%meas%height%data > aircraft_cut) then

! To convert the Aircraft observed height (> cutoff_height=3000m) to pressure:
! and discarded the observed height:
                  N_air_cut = N_air_cut + 1
             call Aircraft_pressure(current%meas%height, current%meas%pressure)
               endif

! Y.-R. Guo, 03/20/2008: In RTOBS 2006091300 data:obs.2006091300.gz, there are
!    two levels obs in FM-13 SHIP causing troubles in wrfvar.
! SHIP and BUOY, if pressure < 85000.0 Pa, discarded.
      ELSE IF ( fm == 13 .or. fm == 18 .or. fm == 19 ) THEN
           if (current%meas%pressure%data < 85000.0 .and. &
               current%meas%pressure%qc >= 0) then 
               write(0,'(a,3x,a,2x,a,2x,2f13.5,2x,"Pressure=",f10.1,a,i8)') &
                   'Discarded:', info%platform(1:12), trim(location%id), &
                   location%latitude,   location%longitude, &
                   current%meas%pressure%data, " < 85000.0 Pa, qc=", &
                   current%meas%pressure%qc 
              CYCLE read_meas
           endif
      
      ENDIF

      !  Some pressure and height is needed for vertically inserting data

      IF (ASSOCIATED (current)) THEN
        
! Guo 01/26/2004: Very grossly h/p consistency check in case of both p and h
!                 are reported as good data (qc=0):
! hcl-note: disagree with checking qc in input data
! hcl-note: qc should be assigned by obsproc after
! hcl-note: applying quality check (if there is any)
!-----------------------------------------------------------------------------
! Do no perform gross check on height for
!    Sounde (FM-35) & AIRS (FM-133) retrievals profile data
!    07/07/2006   Syed RH Rizvi
!
!    Why ???? YRG modified again 11/09/2006
!-----------------------------------------------------------------------------
      IF ( current%meas%height%qc == 0 .and. current%meas%pressure%qc == 0 &
      .and. .NOT.eps_equal(current%meas%height  %data, missing_r, 1.) .and.&
            .NOT.eps_equal(current%meas%pressure%data, missing_r, 1.) )THEN

! ......if Pressure < 500 Pa, the reference height will be unrealistically
!       decreased from the function of "Ref_height":

        if (current%meas%pressure%data >= 500.0) then
          Ref_h = Ref_height (current%meas%pressure%data)
        else
          Ref_h = Ref_height (500.0)
        endif
! ..........if the difference between the reported height and the reference 
!           height is greater than 12000m, discarded this level data:

         if (abs(Ref_h-current%meas%height%data) > 12000) then
             write(0,'("??? Pressure or Height reported inconsistent:")')
             write(0,'(3x,a,2x,a,2x,2f13.5)') &
                   info%platform(1:12), trim(location%id), &
                   location%latitude,        location%longitude
             write(0,'("(height-Ref_h) > 12000m, p,h,ref_h:",3e15.5/)') &
             current%meas%pressure%data, current%meas%height%data, Ref_h 
             CYCLE read_meas
         endif
      ENDIF
      
      IF (eps_equal (current%meas%pressure%data , missing_r , 1.)) THEN
           !hcl what is this?
           if (current%meas%height%data > (htop+100.))   CYCLE read_meas 
           current%meas%pressure%data = Ref_pres (current%meas%height%data)
           current%meas%pressure%qc   = missing
      ENDIF
      IF (eps_equal (current%meas%height%data , missing_r , 1.)) THEN
           !hcl what is this?
           if (current%meas%pressure%data < (ptop-500.)) CYCLE read_meas 
           current%meas%height%data = Ref_height (current%meas%pressure%data)
           current%meas%height%qc   = missing
      ENDIF
      ENDIF

      IF (ASSOCIATED (surface)) THEN
      IF (eps_equal (surface%meas%pressure%data , missing_r , 1.)) THEN
           surface%meas%pressure%data = Ref_pres (surface%meas%height%data)
           surface%meas%pressure%qc   = missing
      ENDIF

      IF (eps_equal (surface%meas%height%data , missing_r , 1.)) THEN
           surface%meas%height%data = Ref_height (surface%meas%pressure%data)
           surface%meas%height%qc   = missing
      ENDIF
      ENDIF

      !  Since it seems that everything went ok, insert this measurement ordered
      !  by pressure.

      meas_count = meas_count + 1

      !  Insert now
     
      CALL insert_at (surface , current , elevation)

      !  One level has been sucessfuly read

      nlevels = nlevels + 1 


      !  Allocate space for another measurement, so we can go try and 
      !  read another level in this routine.  
      !  Initialize it to pointing to nothing.

      ALLOCATE ( current )
      NULLIFY ( current%next )

   END DO read_meas  

   !  The last allocated measurement is not used (no matter how loop is exited)
   !  so deallocate space.

   DEALLOCATE ( current )

   !  If unable to read in at least one measurement, return error so that
   !  entire observation is discarded.  If this was bad data, we forced it
   !  to skip over the observations without storing any data.  That will be
   !  handled in the calling routine.

   IF ( ( meas_count .LT. 1  ) .AND. &
        ( error      .EQ. ok ) .AND. &
        ( .NOT. bad_data     ) ) THEN
          nlevels = 0
          error = no_data
   END IF 

   !  This is some diagnostic print-out to state the problems encountered.  
   !  Anything that is not expected issues a message.  Any read errors mean 
   !  to throw away the observation, though if there was no data, 
   !  there was nothing to toss out anyways.  If the error condition is 
   !  not recognized, the program will stop from this routine.

   SELECT CASE ( error )

      CASE ( eof_err )
         CALL error_handler (proc_name, &
                           ' Found EOF, expected measurement.  Continuing.  ', &
                            TRIM(location_id) // ' ' // TRIM(location_name),   &
                            .FALSE.)

      CASE ( read_err )
         CALL error_handler (proc_name, &
                           ' Error in measurement read. ' // &
                           ' Discarding entire observation and continuing. ', &
                             TRIM(location_id) // ' ' // TRIM(location_name), &
                            .FALSE.)
         CALL dealloc_meas (surface)

      CASE (no_data , ok)

      CASE DEFAULT

        CALL error_handler (proc_name," Internal error: ","bad error number.",&
            .TRUE.)

   END SELECT

END SUBROUTINE read_measurements

!
! -----------------------------------------------------------------------

SUBROUTINE dealloc_meas ( head )

!  This deallocates all nodes in a linked list of measurements.

   IMPLICIT NONE 

   TYPE ( measurement ) , POINTER           :: head     ! head of linked list

   TYPE ( measurement ) , POINTER           :: previous &
                                             , temp
   INTEGER                                  :: status

   !  Start at the head, kill everything that is pointed to.  After the list is
   !  fully deallocated, disassociate the head pointer.

   IF ( ASSOCIATED ( head ) ) THEN

      previous => head
      list_loop : DO WHILE ( ASSOCIATED ( previous%next ) )
         temp => previous
         previous => previous%next
         DEALLOCATE ( temp , STAT = status) 
         IF (status .NE. 0 ) THEN
             WRITE (UNIT = 0, FMT = '(A)') &
          'Error in DEALLOCATE, continuing by stopping DEALLOCATE on this list.'
            EXIT list_loop
         END IF
      END DO list_loop

      NULLIFY ( head )
   END IF

END SUBROUTINE dealloc_meas

!
!---------------------------------------------------------------------------

SUBROUTINE sub_info_levels ( surface, levels )


!  This routine takes the sounding and makes sure that if a surface
!  level exists, that it is the first level.

   IMPLICIT NONE

   TYPE ( measurement ) ,  POINTER         :: surface
   INTEGER , INTENT(OUT)                   :: levels

   TYPE ( measurement ) , POINTER          :: current

   !  Um, is there any data at all?

   levels = 0

   IF ( ASSOCIATED ( surface ) ) THEN

      levels = levels + 1 

      current  => surface%next

      DO WHILE ( ASSOCIATED ( current ) ) 

         levels = levels + 1 
         current => current%next

      END DO

   END IF

END SUBROUTINE sub_info_levels
!---------------------------------------------------------------------------

SUBROUTINE missing_hp ( surface )


!  This routine takes the sounding and makes sure that if a surface
!  level exists, that it is the first level.

   IMPLICIT NONE

   TYPE ( measurement ) ,  POINTER         :: surface
   TYPE ( measurement ) , POINTER          :: current

   !  Um, is there any data at all?

   IF ( ASSOCIATED ( surface ) ) THEN

      !  Alrighty, we have data, so loop through the sounding to see if their is
      !  a surface observation. We can't very well have the surface be the first
      !  level if we don't have one.  Also, start looking at location #2 
      !  (surface%next) so that we don't "fix" the ones that aren't broken.

      IF (surface%meas%height%qc     == missing) &
          surface%meas%height%data   =  missing_r
      IF (surface%meas%pressure%qc   == missing) &
          surface%meas%pressure%data =  missing_r

          current  => surface%next

      DO WHILE ( ASSOCIATED ( current ) ) 

         IF (current%meas%height%qc     == missing) &
             current%meas%height%data   =  missing_r
         IF (current%meas%pressure%qc   == missing) &
             current%meas%pressure%data =  missing_r

             current => current%next

      END DO

   END IF

END SUBROUTINE missing_hp

!---------------------------------------------------------------------------

SUBROUTINE surf_first ( surface , elevation )


!  This routine takes the sounding and makes sure that if a surface
!  level exists, that it is the first level.

   IMPLICIT NONE

   TYPE ( measurement ) ,  POINTER         :: surface
   REAL , INTENT(IN)                       :: elevation

   TYPE ( measurement ) , POINTER          :: current

   !  Um, is there any data at all?

   IF ( ASSOCIATED ( surface ) ) THEN

      !  Alrighty, we have data, so loop through the sounding to see if their is
      !  a surface observation. We can't very well have the surface be the first
      !  level if we don't have one.  Also, start looking at location #2 
      !  (surface%next) so that we don't "fix" the ones that aren't broken.

      current  => surface%next

      find_sfc : DO WHILE ( ASSOCIATED ( current ) ) 

         IF ( eps_equal ( current%meas%height%data , elevation , 1. ) ) THEN
            surface => current
            EXIT find_sfc
         END IF

         current => current%next

      END DO find_sfc

   END IF

END SUBROUTINE surf_first

!---------------------------------------------------------------------------

SUBROUTINE insert_at ( surface , new , elevation)

!  This takes a new measurement (new) and inserts it in a linked list
!  of measurements (surface points to first in list) in decreasing order of
!  pressure value.  If two levels' pressures are 'eps_equal', the levels
!  are merged instead of being linked.

  USE module_obs_merge

   IMPLICIT NONE

   TYPE ( measurement ) ,  POINTER         :: surface , new
   REAL , INTENT(IN)                       :: elevation

   TYPE ( measurement ) , POINTER          :: current , previous , oldptr
   REAL                                    :: new_pres , new_height
   CHARACTER ( LEN = 32 ) , PARAMETER      :: name = 'insert_at'


!  INCLUDE 'error.inc'
!  INTERFACE
!     INCLUDE 'error.int'
!  END INTERFACE

   !  Initialize the variable to test the pressure and the place where the
   !  to-be-inserted measurement points.

   new_pres   = new%meas%pressure%data
   new_height = new%meas%height%data

   NULLIFY ( new%next )

   !  The first check is to see if we are at the head of the linked list.  
   !  This drops us through to exit the routine.

   IF ( .NOT. ASSOCIATED ( surface ) ) THEN

      surface => new

   !  We are either between a couple of values, after a last value, 
   !  or we could need to be merged with a level.  
   !  All those tests are handled in this else block.

   ELSE

      !  Initialize some dummy pointers to traverse to where we need to be.

      previous => surface 
      current => surface

      !  Loop to find correct location to link in 'new'.  
      !  The pressure is monotonically decreasing, so as soon as we find one 
      !  where the current pressure is less than the new pressure, 
      !  the new pressure goes just before it (or we run out of data looking!).
      !  Additionally, if both of the heights are equal AND the heights are
      !  the same as the input elevation of the station, then these need to be 
      !  merged surface observations.

      still_some_data : DO WHILE ( ASSOCIATED ( current ) )

         IF ( current%meas%pressure%data .LT. new_pres ) EXIT still_some_data

         previous => current
         current  => current%next

      END DO still_some_data 

      !  There are several cases:
      !  1) the new value has the same pressure as the previous value, or
      !     both heights are equal to the station elevation: merge them
      !  2) ran out of data finding where to insert level: add it to the end
      !  3) the new value has the same pressure as the current pressure value, 
      !     or both heights are equal to the station elevation: merge them
      !  4) new pressure is < the previous value: stick it at end of previous
      !  5) new pressure > than previous: put at head of list
      !     ***** THE ORDER OF THE TESTS IS IMPORTANT *****

      IF ((eps_equal (previous%meas%pressure%data, new_pres   , 1. ))  .OR.  &
         ((eps_equal (previous%meas%height%data  , new_height , 1. ))  .AND. &
          (eps_equal (previous%meas%height%data  , elevation  , 1. )))) THEN

         CALL merge_measurements (previous%meas , new%meas , 1)
         DEALLOCATE (new)

      ELSE IF (.NOT. ASSOCIATED (current)) THEN

                previous%next => new

      ELSE IF ((eps_equal (current%meas%pressure%data, new_pres   , 1.)) .OR.  &
              ((eps_equal (current%meas%height%data  , new_height , 1.)) .AND. &
               (eps_equal (current%meas%height%data  , elevation  , 1.)))) THEN

                CALL merge_measurements (current%meas, new%meas , 1)

                DEALLOCATE (new)

      ELSE IF  (previous%meas%pressure%data .GT. new_pres) THEN

                oldptr => previous%next
                previous%next => new
                new%next => oldptr

      ELSE IF  (previous%meas%pressure%data .LT. new_pres) THEN

               ! If we aren't at head of list, have some internal (fatal) error.

           IF (.NOT. ASSOCIATED (previous, surface)) THEN
                CALL error_handler (name, 'Logic error in IF' ,"", .TRUE.)
           ELSE
                oldptr => surface
                surface => new
                new%next => oldptr
           END IF 

      ELSE

         !  One of those "should never get here" logic errors, fatal.

        CALL error_handler (name, "Logic error in IF test: ",&
            "for where to put the new observation level.", .TRUE.)

      END IF

   END IF

END SUBROUTINE insert_at

!
! -------------------------------------------------------------------------

SUBROUTINE output_obs ( obs , unit , file_name , num_obs , out_opt, forinput )

!  Take the array of observations and write them including measurements
!  at all levels.  The two options (out_opt and forinput) are described
!  below.

   !  If ( out_opt is 0 ) , write everything
   !                > 0   , write only non-discard data
   !                < 0   , write only discarded data  
   
   !  If ( forinput is true ) output can be pipe back for input.

   IMPLICIT NONE

   TYPE ( report ) , INTENT ( IN ) , DIMENSION ( : ) :: obs
   INTEGER , INTENT ( IN )                           :: num_obs
   INTEGER , INTENT ( IN )                           :: out_opt   
   INTEGER , INTENT ( IN )                           :: unit
   CHARACTER ( LEN = * ) , INTENT ( IN )             :: file_name
   LOGICAL , INTENT ( IN )                           :: forinput

   INTEGER                                           :: i , iout
   TYPE ( measurement ) , POINTER                    :: next
   TYPE ( meas_data   )                              :: end_meas
 
   end_meas%pressure%data    = end_data_r
   end_meas%height%data      = end_data_r
   end_meas%temperature%data = end_data_r
   end_meas%dew_point%data   = end_data_r
   end_meas%speed%data       = end_data_r
   end_meas%direction%data   = end_data_r
   end_meas%u%data           = end_data_r
   end_meas%v%data           = end_data_r
   end_meas%rh%data          = end_data_r
   end_meas%thickness%data   = end_data_r
   end_meas%pressure%qc      = end_data  
   end_meas%height%qc        = end_data  
   end_meas%temperature%qc   = end_data  
   end_meas%dew_point%qc     = end_data  
   end_meas%speed%qc         = end_data  
   end_meas%direction%qc     = end_data  
   end_meas%u%qc             = end_data  
   end_meas%v%qc             = end_data  
   end_meas%rh%qc            = end_data  
   end_meas%thickness%qc     = end_data  

   OPEN ( UNIT = unit , FILE = file_name ,  ACTION = 'write' , FORM = 'formatted' )

   iout = 0

   DO i = 1 , num_obs

      IF (   out_opt .EQ. 0                                   .OR. &
           ( out_opt .GT. 0 .AND. .NOT. obs(i)%info%discard ) .OR. &
           ( out_opt .LT. 0 .AND.       obs(i)%info%discard ) ) THEN

         iout = iout + 1
         IF ( .NOT. forinput ) write(unit,*) '**************** Next Observation *******************'
         WRITE ( UNIT = unit , FMT = rpt_format ) &
            obs(i)%location % latitude,     obs(i)%location % longitude, &
            obs(i)%location % id,           obs(i)%location % name, &
            obs(i)%info % platform,         obs(i)%info % source, &
            obs(i)%info % elevation,        obs(i)%info % num_vld_fld, &
            obs(i)%info % num_error,        obs(i)%info % num_warning, &
            obs(i)%info % seq_num,          obs(i)%info % num_dups, &
            obs(i)%info % is_sound,         obs(i)%info % bogus, &
            obs(i)%info % discard, & 
            obs(i)%valid_time % sut,        obs(i)%valid_time % julian, &
            obs(i)%valid_time % date_char,  &
            obs(i)%ground%slp%data,         obs(i)%ground%slp%qc,&
            obs(i)%ground%ref_pres%data,    obs(i)%ground%ref_pres%qc,&
            obs(i)%ground%ground_t%data,    obs(i)%ground%ground_t%qc,&
            obs(i)%ground%sst%data,         obs(i)%ground%sst%qc,&
            obs(i)%ground%psfc%data,        obs(i)%ground%psfc%qc,&
            obs(i)%ground%precip%data,      obs(i)%ground%precip%qc,&
            obs(i)%ground%t_max%data,       obs(i)%ground%t_max%qc,&
            obs(i)%ground%t_min%data,       obs(i)%ground%t_min%qc,&
            obs(i)%ground%t_min_night%data, obs(i)%ground%t_min_night%qc,&
            obs(i)%ground%p_tend03%data,    obs(i)%ground%p_tend03%qc,&
            obs(i)%ground%p_tend24%data,    obs(i)%ground%p_tend24%qc, &
            obs(i)%ground%cloud_cvr%data,   obs(i)%ground%cloud_cvr%qc, &
            obs(i)%ground%ceiling%data,     obs(i)%ground%ceiling%qc
!           obs(i)%location, 
!           obs(i)%valid_time,
!           obs(i)%ground,

         next => obs(i)%surface
         DO WHILE ( ASSOCIATED ( next ) )
            if ( obs(i)%info%discard ) exit 
!           WRITE ( UNIT = unit , FMT = meas_format )  next%meas
            WRITE ( UNIT = unit , FMT = meas_format )  &
            next%meas % pressure    % data, next%meas % pressure    % qc, &
            next%meas % height      % data, next%meas % height      % qc, &
            next%meas % temperature % data, next%meas % temperature % qc, &
            next%meas % dew_point   % data, next%meas % dew_point   % qc, &
            next%meas % speed       % data, next%meas % speed       % qc, &
            next%meas % direction   % data, next%meas % direction   % qc, &
            next%meas % u           % data, next%meas % u           % qc, &
            next%meas % v           % data, next%meas % v           % qc, &
            next%meas % rh          % data, next%meas % rh          % qc, &
            next%meas % thickness   % data, next%meas % thickness   % qc

            next => next%next
         END DO
!        WRITE ( UNIT = unit , FMT = meas_format ) end_meas
         WRITE ( UNIT = unit , FMT = meas_format ) &
            end_meas % pressure    % data, end_meas % pressure    % qc, &
            end_meas % height      % data, end_meas % height      % qc, &
            end_meas % temperature % data, end_meas % temperature % qc, &
            end_meas % dew_point   % data, end_meas % dew_point   % qc, &
            end_meas % speed       % data, end_meas % speed       % qc, &
            end_meas % direction   % data, end_meas % direction   % qc, &
            end_meas % u           % data, end_meas % u           % qc, &
            end_meas % v           % data, end_meas % v           % qc, &
            end_meas % rh          % data, end_meas % rh          % qc, &
            end_meas % thickness   % data, end_meas % thickness   % qc

         WRITE ( UNIT = unit , FMT = end_format ) obs(i)%info%num_vld_fld, &
            obs(i)%info%num_error, obs(i)%info%num_warning
         IF ( .NOT. forinput ) &
            write(unit,*) 'End of measurements for observation ' , i

      END IF

   END DO

   IF ( .NOT. forinput ) THEN
      write(unit,*) '======================================================='
      write(unit,*) 'Total Number of Measurements output ' , iout
   ENDIF

   !  This routine may be called again, with the same unit number, so CLOSE
   !  up the file so everything is handled cleanly.

   CLOSE ( unit )

END SUBROUTINE output_obs

Subroutine Aircraft_pressure(hh, pp)

!     --------------------------------------------------------------- 
!     NOW COMPUTE THE PRESSURE OF THE FLIGHT LEVEL USING THE STANDARD 
!     ATMOSPHERE COMPUTATION, SET THE HEIGHT TO MISSING, 
!     THEN GROSS CHECK FOR METEOROLOGICAL LIMITS. 
!     --------------------------------------------------------------- 

      implicit none

      Type (field), intent(inout)  :: hh
      Type (field), intent(out)    :: pp
 
      IF (HH%data > 11000.0) THEN 

         PP%data =  226.3 * EXP(1.576106E-4 * (11000.00 - HH%data)) 
 
      ELSE IF (HH%data <= 11000.0) THEN 

         PP%data = 1013.25 * (((288.15 - (0.0065 * HH%data))/288.15)**5.256) 

      END IF 
      PP%data = PP%data * 100.
      PP%qc   = 0

      !hcl-note: disagree with throwing away the original flight level information
      !hcl HH%data = missing_r
      !hcl HH%qc   = missing

end Subroutine Aircraft_pressure

function psfc_from_QNH(alt, elev) result(psfc)
   real,  intent(in) :: alt  ! altimeter setting/QNH
   real,  intent(in) :: elev ! elevation
   real              :: psfc
   psfc = (alt**0.190284-(((1013.25**0.190284)*0.0065/288.15)*elev))**5.2553026
end function psfc_from_QNH

END MODULE module_decoded

