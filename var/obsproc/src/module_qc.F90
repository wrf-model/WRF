MODULE module_qc

!-----------------------------------------------------------------------------!
! Vertical sounding checks: correct spikes and super-adiabatic rate
! Height checks: flag level above the model lid
!
!  HISTORY: 
!
!  D. GILL,         April 1998
!  F. VANDENBERGHE, May 2000
!  Y.-G. GUO,       September 2000
!
!         01/13/2003 - Updated for Profiler obs.           S. R. H. Rizvi
!
!         02/04/2003 - Updated for Buoy     obs.           S. R. H. Rizvi
!
!         02/11/2003 - Reviewed and modified for Profiler
!                      and Buoy obs.                       Y.-R. Guo
!         06/30/2006 -   Updated for AIRS retrievals       Syed  RH  Rizvi
!         11/09/2006 -   Updated for GPS RO                Y.-R. Guo
!------------------------------------------------------------------------------
   

   INTEGER :: nsynops_qc (2), nshipss_qc (2), nmetars_qc (2), &
              npilots_qc (2), nsounds_qc (2), nsatems_qc (2), &
              nsatobs_qc (2), naireps_qc (2), ngpspws_qc (2), &
              nssmt1s_qc (2), nssmt2s_qc (2), nssmis_qc  (2), &
              ntovss_qc  (2), nothers_qc (2), namdars_qc (2), &
              nqscats_qc (2), nbuoyss_qc (2), nprofls_qc (2), &
              ngpsztd_qc (2), ngpsref_qc (2), ngpseph_qc (2), &
              nboguss_qc (2), nairss_qc(2)  , ntamdar_qc (2)

CONTAINS

!------------------------------------------------------------------------------
SUBROUTINE proc_qc1 ( max_number_of_obs , obs , number_of_obs,           &
                     qc_test_vert_consistency , qc_test_convective_adj , &
                     print_qc_vert     , print_qc_conv)

! Driver routine for QC
!   
!    This routine is a driver routine for quality control ( or QC )
!       of observations. It includes: 
!       1. vertical sounding checks
!
!    This procedure for quality control was applied before those
!    calculations related to the model background fields: 
!    surface_correction, innovation, etc.
!
!                          Modified by Yong-Run Guo
!                               11/09/00
!

   USE module_type
   USE module_func
   USE module_per_type
                         

   IMPLICIT NONE

   INTEGER,       INTENT ( IN )                :: max_number_of_obs
   TYPE (report), DIMENSION (max_number_of_obs):: obs
   INTEGER , INTENT ( IN )                     :: number_of_obs
   LOGICAL , INTENT ( IN )                     :: qc_test_vert_consistency, &
                                                  qc_test_convective_adj
   LOGICAL , INTENT ( IN )                     :: print_qc_vert,           &
                                                  print_qc_conv
   TYPE (measurement), POINTER                 :: current
   REAL                                        :: err, err_max
   INTEGER                                     :: qc_flag
   INTEGER                                     :: i
   LOGICAL                                     :: corrected, failed 

   CHARACTER (LEN=80)                          :: filename
   LOGICAL                                     :: connected
   INTEGER                                     :: iunit, io_error
   CHARACTER (LEN = 32)                        :: proc_name = 'proc_qc: '

   INCLUDE 'platform_interface.inc'

   INCLUDE 'missing.inc'

!------------------------------------------------------------------------------!
!             WRITE (0,'(A)')  &
!'------------------------------------------------------------------------------'
!     WRITE ( UNIT = 0, FMT = '(A)') 'DATA CHECK AND QC:'

      !  Open diagnostic file

      IF (print_qc_vert      .OR. print_qc_conv) THEN

      filename = 'obs_qc1.diag'
      iunit    = 999

      INQUIRE (UNIT = iunit, OPENED = connected )

      IF (connected) CLOSE (iunit)

      OPEN (UNIT = iunit , FILE = filename , FORM = 'FORMATTED'  , &
            ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error )

      IF (io_error .NE. 0) THEN
          CALL error_handler (proc_name, &
         "Unable to open output diagnostic file. ", filename, .TRUE.)
!     ELSE
!         WRITE (UNIT = 0, FMT = '(A,A,/)') &
!        "Diagnostics in file ", TRIM (filename)
      ENDIF

      ENDIF


   !  The first quality control (QC) that can be performed is with
   !  data that is vertically stacked.  These reports have the temperature,
   !  speed and direction compared against reasonable benchmarks.
   !  Bad values have flags set, though switching the sign of the temperature
   !  (in degrees C) is allowed.  This test is not performed if the entire
   !  report was discarded, and no error checks are performed on bogus
   !  data types.  Since the vertical consistency check and the dry 
   !  convective adjustment are for vertically stacked data, we can ignore 
   !  these test for data with only one level.

consistency_check: &

   IF ( qc_test_vert_consistency ) THEN

              WRITE (0,'(A)')  &
'------------------------------------------------------------------------------'
      WRITE (UNIT = 0, FMT = '(A,/)') 'VERTICAL CONSISTENCY TEST QC:'

      IF (print_qc_vert) THEN
          WRITE (UNIT = 0, FMT = '(A,A,/)')  &
         "Diagnostics in file ", TRIM (filename)
      ENDIF

      loop_all_1 : DO i = 1, number_of_obs

! For GPS Refractivity (FM=116) and GPS Excess Phase (FM=118), no check applied:
         if ((obs(i)%info%platform(4:6) == '116')  &
             .or. (obs(i)%info%platform(4:6) == '118')) cycle loop_all_1

         valid_ob_1 : IF ( (.NOT. obs(i)%info%discard     )   .AND. &
                           (.NOT. obs(i)%info%bogus     )  .AND. &
                           (obs(i)%info%is_sound          )   .AND. &
                           (.NOT. obs(i)%location%name=='OSSEs').AND. &
                           (ASSOCIATED ( obs(i)%surface ) ) ) THEN

         failed = .FALSE.

         CALL vert_cons_check ( obs ( i ) , i , print_qc_vert, iunit, failed )

         IF (failed) THEN

             READ (obs (i) % info % platform (4:6), '(I3)') fm

             CALL fm_decoder (fm, platform,        bogus=nboguss_qc(1), &
                              synop=nsynops_qc(1), ship =nshipss_qc(1), &
                              metar=nmetars_qc(1), pilot=npilots_qc(1), &
                              sound=nsounds_qc(1), satem=nsatems_qc(1), &
                              satob=nsatobs_qc(1), airep=naireps_qc(1), &
                              gpspw=ngpspws_qc(1), gpszd=ngpsztd_qc(1), &
                              gpsrf=ngpsref_qc(1), ssmt1=nssmt1s_qc(1), &
                              ssmt2=nssmt2s_qc(1), gpsep=ngpseph_qc(1), &
                              ssmi =nssmis_qc (1), &
                              tovs =ntovss_qc (1), other=nothers_qc(1), &
                              amdar=namdars_qc(1), qscat=nqscats_qc(1), &
                              profl=nprofls_qc(1), buoy =nbuoyss_qc(1), &
                              airs =nairss_qc(1) , tamdar=ntamdar_qc(1) )


         ENDIF

         END IF valid_ob_1

      END DO loop_all_1

   END IF consistency_check

   !  The second QC check that is available to be performed on the 
   !  the observations is the convective adjustment.  This is used
   !  only for the upper level temperature reports.

convective_check: &

   IF (qc_test_convective_adj) THEN

              WRITE (0,'(A)')  &
'------------------------------------------------------------------------------'
      WRITE (UNIT = 0, FMT = '(A,/)') 'CONVECTIVE ADJUSTEMENT TEST QC:'

      IF (print_qc_conv) THEN
          WRITE (UNIT = 0, FMT = '(A,A,/)') &
         "Diagnostics in file ", TRIM (filename)
      ENDIF

      loop_all_2 : DO i = 1, number_of_obs

! For GPS Refractivity (FM=116) and GPS Excess Phase (FM=118), no check applied:
         if ((obs(i)%info%platform(4:6) == '116') &
             .or.(obs(i)%info%platform(4:6) == '118')) cycle loop_all_2

         valid_ob_2 : IF ( (.NOT. obs(i)%info%discard     )   .AND. &
                           (.NOT. obs(i)%info%bogus     )  .AND. &
                           (obs(i)%info%is_sound          )   .AND. &
                           (.NOT. obs(i)%location%name=='OSSEs').AND. &
                           (ASSOCIATED ( obs(i)%surface ) ) ) THEN

         corrected = .FALSE.

         CALL dry_convective_adjustment (obs (i), i, &
                                         print_qc_conv, iunit, corrected)

         IF (corrected) THEN

             READ (obs (i) % info % platform (4:6), '(I3)') fm

             CALL fm_decoder (fm, platform,        bogus=nboguss_qc(2), &
                              synop=nsynops_qc(2), ship =nshipss_qc(2), &
                              metar=nmetars_qc(2), pilot=npilots_qc(2), &
                              sound=nsounds_qc(2), satem=nsatems_qc(2), &
                              satob=nsatobs_qc(2), airep=naireps_qc(2), &
                              gpspw=ngpspws_qc(2), gpszd=ngpsztd_qc(2), &
                              gpsrf=ngpsref_qc(2), gpsep=ngpseph_qc(2), &
                              ssmt1=nssmt1s_qc(2), &
                              ssmt2=nssmt2s_qc(2), ssmi =nssmis_qc (2), &
                              tovs =ntovss_qc (2), other=nothers_qc(2), &
                              amdar=namdars_qc(2), qscat=nqscats_qc(2), &
                              profl=nprofls_qc(2), buoy =nbuoyss_qc(2), &
                              airs =nairss_qc(2) , tamdar=ntamdar_qc(2) )

         ENDIF

         END IF valid_ob_2

      END DO loop_all_2

   END IF convective_check

   !  Close diagnostic file

   IF (print_qc_vert      .OR. print_qc_conv) THEN
       CLOSE (iunit)
   ENDIF

END SUBROUTINE proc_qc1
!------------------------------------------------------------------------------
SUBROUTINE modify_qc_flag ( qc_flag , err , factored_difference , test_number ) 

   IMPLICIT NONE

   INTEGER    :: test_number
   INTEGER    :: qc_flag
   REAL       :: err,  factored_difference 

   INTEGER    :: qc_small, qc_large

   !  If the difference between observation and analysis at obs location
   !  (err) exceeds errmx, mark those obs as flagged by the error checking
   !  routines.

   IF ( ABS ( err ) .GT. factored_difference ) THEN

      qc_small = mod ( qc_flag , test_number ) 
      qc_large = ( qc_flag / ( test_number * 2 ) ) * 2
      qc_flag = qc_large*test_number + qc_small + test_number

   ENDIF
      
END SUBROUTINE modify_qc_flag

!------------------------------------------------------------------------------

SUBROUTINE dry_convective_adjustment (obs , counter , print_dry , iunit, &
                                      corrected) 

!  This subroutine performs the dry convective adjustment for a sounding 
!  and prints out the points and levels where the adjustment have been
!  invoked. The adjustment removes any super-adiabatic lapse rate in the
!  sounding by making use of the conservation of dry static energy.

   USE module_type
   USE module_func


   IMPLICIT NONE

   INTEGER        :: iunit
   INTEGER        :: counter, nlevel
   INTEGER        :: k, n1, kk, klop,& ! parameters for do loops
                     numb, num1, i,  &
                     kst,            & ! kst is the starting level of adjustment
                     ked               !  ked is the ending level of adjustment

   REAL              :: esum, & ! sum of static energy between 2 adjacent levels
                        eav, &  ! mean static energy between 2 adjacent levels
                        psfc    !  surface pressure

   REAL , DIMENSION ( : ) , ALLOCATABLE  :: p , &  !  pressure 
                                            t , &  !  temperature
                                            td, &  !  dew point
                                            dp, &  !  dew point depression
                                            z , &  !  height
                                            zt, &  !  height calculated 
                                                   !  from temperature
                                            e      !  dry static energy
   REAL , DIMENSION ( : ) , ALLOCATABLE  :: theta  !  potential temperature
   INTEGER                               :: index  !  number of vertical levels
   TYPE ( measurement ) , POINTER        :: current
   TYPE ( report )                       :: obs
   CHARACTER ( LEN = 120)                :: message
   LOGICAL                               :: print_dry
   LOGICAL                               :: corrected
   REAL                                  :: es, qs

   INCLUDE 'constants.inc'
!  INCLUDE 'error.inc'
   INCLUDE 'missing.inc'

!  INTERFACE
!     INCLUDE 'error.int'
!  END INTERFACE

   ! Try to find out the surface level first

   psfc = missing_r

   NULLIFY ( current )

   nlevel = 0
   current => obs%surface

loop_psfc:   DO WHILE ( ASSOCIATED ( current ) )

!      IF (eps_equal (current%meas%height%data, obs%info%elevation, 0.1)) THEN
      IF (.NOT. eps_equal (current%meas%height%data  , missing_r, 1.0) .AND. &
          .NOT. eps_equal (current%meas%pressure%data, missing_r, 1.0) ) THEN
            psfc = current%meas%pressure%data
!
!      Write out the stations with (h < elevation):
!
            if ( print_dry ) then
            if (current%meas%height%data < obs%info%elevation) &
              write(iunit,'(/A,A,2X,A,2X,A/A,2I6,3f12.2)')           & 
                "Platform, ID, NAME: ",                              &
                                         obs % info % platform,      &
                                         obs % location % id(1:10),  &
                                         obs % location % name,      & 
                "PSFC found ==> I, nlevel, psfc, height, elevation:",&
              counter, nlevel, psfc, current % meas % height%data,   &
                                     obs % info % elevation
            end if !print

            EXIT loop_psfc

      ELSE

         ! Do nothing

      END IF

      current => current%next

   END DO loop_psfc

   !  Find out the number of vertical levels, then ALLOCATE arrays.

   NULLIFY ( current )

   index = 1

   current => obs%surface

   DO WHILE ( ASSOCIATED ( current ) )

      IF ((.NOT. eps_equal (current%meas%pressure%data, missing_r, 1. )).AND. & 
          (.NOT. eps_equal (psfc ,                      missing_r, 1. )).AND. & 
          (current%meas%pressure%data .LE. psfc ) .AND. & 
          (.NOT. eps_equal (current%meas%temperature%data, missing_r, 1.)))THEN

           index = index + 1

      ELSE

         ! Do nothing, missing or bad data

      END IF

      current => current%next

   END DO

   index = index - 1

   IF ((index .LE. 3) .OR. (eps_equal (psfc, missing_r, 1.))) THEN

      !   The vertical levels are less than 3, do nothing

   ELSE ! Make dry adjustment for temperature

      ALLOCATE ( P ( index ) )
      ALLOCATE ( t ( index ) )
      ALLOCATE ( td( index ) )
      ALLOCATE ( dp( index ) )
      ALLOCATE ( z ( index ) )
      ALLOCATE ( zt( index ) )
      ALLOCATE ( e ( index ) )
      ALLOCATE ( theta ( index ) )
      NULLIFY  ( current )

      !  Read data set into array and perform quality check.

      index = 1

      current => obs%surface

      DO WHILE ( ASSOCIATED ( current ) )

         IF ((.NOT. eps_equal (current%meas%pressure%data, missing_r, 1.)).AND.&
             (                 current%meas%pressure%data .LE. psfc) .AND. & 
             (.NOT. eps_equal (current%meas%temperature%data, missing_r, 1.)))&
         THEN

            p ( index ) = current%meas%pressure%data
            t ( index ) = current%meas%temperature%data
            td( index ) = current%meas%dew_point%data

            IF (.NOT. eps_equal (current%meas%dew_point%data, missing_r, 1.)) &
            THEN
               dp (index) = t (index) - td (index)
            ELSE
               dp (index) = missing_r
            ENDIF
            z (index) = current%meas%height%data
            index = index + 1
         ELSE
            ! Do nothing, missing or bad data
         END IF

         current => current%next

      END DO

      index = index - 1
      n1    = index - 1

      IF (.NOT. eps_equal ( z (1) , missing_r, 1.)) THEN

         !  We have the first level height, go ahead make dry adjustment.

         zt (1) = z (1)

         !  Compute potential temperature.

         DO k = 1, index
            theta ( k ) = t ( k ) * ( 100000.0 / p ( k ) ) ** rcp
         END DO

         loop_adjust: DO k = 1 , index - 2
      
            IF (theta (k) .LE. theta (k+1))  THEN

               !  Stable layer, do nothing.

               CYCLE loop_adjust 

            ELSE

               !  Compute the height z at all levels by taking 
               !  a mean temperature of the layer.
              
               DO kk = 2 , n1
                  zt (kk) = zt (kk - 1) + 0.5 * (t (kk - 1) + t (kk)) *   &
                                 (gasr/g) * LOG (p (kk -1) / p (kk))
               END DO

               !  Compute the dry static energy.

               DO kk = 1 , n1
                  e (kk) = g * zt (kk) + cp * t (kk)
               END DO

               esum = e (k) + e (k + 1)
               eav  = esum * 0.5

               !  Downward checking the layer in which superadiabat exists.

               numb = 2

               loop_down: DO klop = k - 1 , 1 , -1

                  IF (klop .LT. 1) THEN
                      EXIT loop_down
                  ELSE

                     IF (eav  .GE. e (klop) ) THEN
                         EXIT loop_down
                     ELSE
                         esum = esum + e ( klop )
                         numb = numb + 1
                         eav = esum / REAL ( numb )
                     END IF

                  END IF

               END DO loop_down

               num1 = numb
                      
               !  Upward checking the layer in which superadiabat exists.

               loop_up: DO klop = k + 2 , index , 1

                  IF (klop .GT. n1) THEN

                      EXIT loop_up

                  ELSE

                     IF (eav .LE. e (klop)) THEN
                         EXIT loop_up
                     ELSE
                         esum = esum + e (klop)
                         numb = numb + 1
                         eav = esum / REAL (numb)
                     END IF

                  END IF 

               END DO  loop_up

               !  Define the starting and ending levels.

               kst = k + 2 - num1
               ked = kst + numb -1

               corrected = .TRUE.

               IF (print_dry) THEN

                   WRITE  (message, FMT = '(&
                   &     "Dry convective adjustment",    &
                   &     " ID: ",A5,", NAME: ",A8,         &
                   &     ", from k = ",I2,  " to ",I2,     &
                   &     ", from P = ",F6.0," to ",F6.0," Pa." )') &
                   &       obs%location%id, obs%location%name, &
                   &       kst, ked, p(kst)/100., p(ked)/100.

                   WRITE (iunit,'(A)') message

               ENDIF

               !  Evaluates the new temperature profile according to 
               !  the mean static energy of the layer.

               DO kk = kst , ked
                  t (kk) = ((eav - g * zt (kst)) / cp ) *  &
                            (p (kk) / p (kst))  ** rcp
               END DO

            END IF

         END DO loop_adjust

      ELSE
         ! We do not have the first level height, do nothing.
      END IF   

      current => obs%surface

      looptt: DO WHILE ( ASSOCIATED ( current ) )

         DO i = 1, index

            IF  ((eps_equal (p (i), current%meas%pressure%data, 0.1)) .AND. &
           (.NOT. eps_equal (t (i), current%meas%temperature%data, 0.1))) THEN 

                 IF (print_dry) THEN

                     WRITE (message , FMT =                        &
                     &    '("T dry conv adj: ",                    &
                     &      " ID = ",A5,", NAME = ",A8,            &
                     &      ", T old = " ,F5.1," (K),",            &
                     &      ", T new = " ,F5.1," (K),",            &
                     &      ", P = ",     F6.1," (hPa)")')         &
                     &        obs%location%id, obs%location%name,  &
                     &        current%meas%temperature%data, t(i), p(i)/100.

                     WRITE (iunit,'(A)') message

                 ENDIF

                 !  Correct t, td, rh and qv

                 IF (.NOT. eps_equal &
                    (current%meas%temperature%data,missing_r, 1.)) THEN
                     current%meas%temperature%data = t (i)
                 ENDIF

                 IF (.NOT. eps_equal &
                    (current%meas%dew_point%data, missing_r,1.)) THEN

                     current%meas%dew_point%data = t ( i ) - dp ( i)

                     current%meas%rh%data = 100. * exp (5418.12 &
                                                 * (1./t(i) - 1./(t(i)-dp(i))))

                 IF (.NOT.eps_equal (current%meas%qv%data, missing_r, 1.)) THEN

                      es = 6.112 * EXP (17.67 * (t (i) - 273.15) &
                                              / (t (i) - 273.15+243.5))
                      qs = 0.622 * es /(p (i)/100. -es)

                      current%meas%qv%data = MAX (0.01*current%meas%rh%data*qs,&
                                                  0.);
                  ENDIF

               ENDIF

               !  Update QC

               IF (current%meas%temperature%qc .NE. missing ) THEN
                   current%meas%temperature%qc = convective_adjustment &
                 + current%meas%temperature%qc
               END IF

               IF (current%meas%dew_point%qc .NE. missing ) THEN
                   current%meas%dew_point%qc = convective_adjustment &
                 + current%meas%dew_point%qc
               END IF

               IF (current%meas%rh%qc .NE. missing ) THEN
                   current%meas%rh%qc = convective_adjustment &
                 + current%meas%rh%qc
               END IF

               IF (current%meas%qv%qc .NE. missing ) THEN
                   current%meas%qv%qc = convective_adjustment &
                 + current%meas%qv%qc
               END IF

            ELSE

               ! Do nothing

            END IF

         END DO

         current => current%next

      END DO looptt

      DEALLOCATE ( P     )
      DEALLOCATE ( t     )
      DEALLOCATE ( td    )
      DEALLOCATE ( dp    )
      DEALLOCATE ( z     )
      DEALLOCATE ( zt    )
      DEALLOCATE ( e     )
      DEALLOCATE ( theta )
      NULLIFY    ( current )

   END IF

END SUBROUTINE dry_convective_adjustment 

!-------------------------------------------------------------------------------

SUBROUTINE vert_cons_check ( obs , counter , print_vert, iunit, failed ) 

!  This subroutine is called to perform vertical 
!  consistency check for a single sounding.     

   USE module_type
   USE module_func

   IMPLICIT NONE

   INTEGER                                :: i, index, iflag , counter, nlevel
   REAL   , DIMENSION ( : ), ALLOCATABLE  :: p, t, td, ws, wd, u, v, rh, qv, &
                                             work1, work2
   INTEGER, DIMENSION ( : ), ALLOCATABLE  :: tqc, tdqc, wsqc, wdqc, uqc, vqc, &
                                             rhqc, qvqc, iindex
   TYPE ( measurement ), POINTER          :: current
   TYPE ( report )                        :: obs
   REAL                                   :: d1, d2, che1, che2, th, th1, th2, &
                                             psfc, depression
   REAL                                   :: es, qs
   CHARACTER ( LEN = 120 )                :: proc_name = "vert_cons_check"
   CHARACTER ( LEN = 120 )                :: message
   LOGICAL                                :: print_vert
   REAL                                   :: p1 , p2 , h1 , h2
   LOGICAL                                :: found
   LOGICAL                                :: failed
   INTEGER                                :: iunit

!  INCLUDE 'error.inc'
   INCLUDE 'constants.inc'
   INCLUDE 'missing.inc'

!  INTERFACE
!     INCLUDE 'error.int'
!  END INTERFACE

   !  Check for spikes in wind curve. 
   !  Both the lapse rate check and potential temperature check will be 
   !  performed.
   !  Is there a duplicate surface value?  
   !  We can check the pressure and height values.
   !  If we find two (or more) levels with the height equal to the surface 
   !  elevation,then we set all of the information in the duplicate level to 
   !  missing.

   NULLIFY ( current )

   current => obs%surface

   IF (ASSOCIATED ( current ) ) THEN

       p1 = current%meas%pressure%data
       h1 = current%meas%height%data

   END IF

   current => current%next

   found = .FALSE.

   DO WHILE ( ASSOCIATED ( current ) )

      IF (eps_equal (current%meas%height%data, obs%info%elevation, 0.1) &
          .AND. .NOT.eps_equal (obs%info%elevation, missing_r, 0.1) ) THEN

          p2 = current%meas%pressure%data
          h2 = current%meas%height%data

          IF ((      eps_equal (h1 , h2 , 0.1 )) .AND. &
              (.NOT. eps_equal (p1 , p2 , 0.1 ))) THEN

             if ( print_vert ) then
               WRITE (message, FMT = '(" Duplicate surface found at ",A8,A8)') &
               TRIM  (obs%location%id),  TRIM (obs%location%name)
               WRITE (iunit, '(A)') TRIM (message)
             end if

! To discard the OBS:                
               obs%info % discard = .TRUE.

               current%meas%pressure%data     = missing_r
               current%meas%height%data       = missing_r
               current%meas%temperature%data  = missing_r
               current%meas%dew_point%data    = missing_r
               current%meas%speed%data        = missing_r
               current%meas%direction%data    = missing_r
               current%meas%rh%data           = missing_r
               current%meas%qv%data           = missing_r
               current%meas%u%data            = missing_r
               current%meas%v%data            = missing_r
               current%meas%pressure%qc       = missing
               current%meas%height%qc         = missing
               current%meas%temperature%qc    = missing
               current%meas%dew_point%qc      = missing
               current%meas%speed%qc          = missing
               current%meas%direction%qc      = missing
               current%meas%rh%qc             = missing
               current%meas%qv%qc             = missing
               current%meas%u%qc              = missing
               current%meas%v%qc              = missing

         END IF

      END IF

      current => current%next

   END DO

   !  Try to find out the surface level first

   psfc = missing_r

   NULLIFY (current)

   nlevel = 0
   current => obs%surface

loop_psfc: DO WHILE (ASSOCIATED (current))

      nlevel = nlevel + 1

!      IF (eps_equal (current%meas%height%data, obs%info%elevation, 0.1 ) ) THEN
      IF (.NOT. eps_equal (current%meas%height%data  , missing_r, 1.0) .AND. &
          .NOT. eps_equal (current%meas%pressure%data, missing_r, 1.0) ) THEN
            psfc = current%meas%pressure%data
!
!      Write out the stations with (h < elevation):
!
            if ( print_vert ) then
            if (current%meas%height%data < obs%info%elevation) &
              write(iunit,'(/A,A,2X,A,2X,A/A,2I6,3f12.2)')           & 
                "Platform, ID, NAME: ",                              &
                                         obs % info % platform,      &
                                         obs % location % id(1:10),  &
                                         obs % location % name,      & 
                "PSFC found ==> I, nlevel, psfc, height, elevation:",&
              counter, nlevel, psfc, current % meas % height%data,   &
                                     obs % info % elevation

            end if !print
            EXIT loop_psfc
      ELSE
           ! Do nothing
      END IF

      current => current%next

   END DO loop_psfc

   !  Find out the vertical level number, then ALLOCATE arrays

   NULLIFY ( current )

   index = 1

   current => obs%surface

   DO WHILE ( ASSOCIATED ( current ) )
      IF ((.NOT. eps_equal (current%meas%pressure%data, missing_r, 1.)) .AND. & 
          (.NOT. eps_equal (psfc,                       missing_r, 1.)) .AND. & 
                           (current%meas%pressure%data .LE. psfc)       .AND. & 
          (.NOT. eps_equal (current%meas%temperature%data, missing_r, 1.))) THEN

           index = index + 1

      ELSE

         ! Do nothing, missing or bad data

      END IF

      current => current%next

   END DO

   index = index - 1

   IF ((index .LE. 3 ) .OR. (eps_equal (psfc, missing_r, 1.))) THEN

      !   The vertical levels are less than 3, do nothing

   ELSE ! Check superadiabatic and inversion in temperature curve

      ALLOCATE (      P ( index ) )
      ALLOCATE (      t ( index ) )
      ALLOCATE (     td ( index ) )
      ALLOCATE (     rh ( index ) )
      ALLOCATE (     qv ( index ) )
      ALLOCATE (    tqc ( index ) )
      ALLOCATE (   tdqc ( index ) )
      ALLOCATE (   rhqc ( index ) )
      ALLOCATE (   qvqc ( index ) )
      ALLOCATE ( iindex ( index ) )
      NULLIFY  ( current )

      !  Read data set into array and perform quality check

      index = 1
      current => obs%surface
      DO WHILE ( ASSOCIATED ( current ) )
         IF ((.NOT. eps_equal (current%meas%pressure%data, missing_r, 1.)).AND.&
                              (current%meas%pressure%data .LE. psfc ) .AND. & 
             (.NOT. eps_equal (current%meas%temperature%data, missing_r, 1.))) &
         THEN

            p    (index) = current%meas%pressure%data
            t    (index) = current%meas%temperature%data
            td   (index) = current%meas%dew_point%data
            rh   (index) = current%meas%rh%data
            qv   (index) = current%meas%qv%data
            tqc  (index) = 0
            tdqc (index) = 0
            rhqc (index) = 0
            qvqc (index) = 0
            index = index + 1

         ELSE

            ! Do nothing, missing or bad data

         END IF

         current => current%next

      END DO

      index = index - 1

      loop1: DO i = 1, index
             iindex ( i ) = 0
      END DO loop1
         
      ! First level

      che1 = LOG (t (2) / t (1)) / LOG (p (2) / p (1)) 
      che2 = LOG (t (3) / t (2)) / LOG (p (3) / p (2)) 

      IF (((che1 .LT. -1.5 ) .OR.  (che1 .GT. 0.9)) .AND.             &
          ((che2 .GT. -1.5 ) .AND. (che2 .LT. 0.9))) iindex (1) = 1
!     IF (((che1 .LT. -1.5 ) .OR.  (che1 .GT. 0.9)) .AND.             &
!         ((che2 .LT. -1.5 ) .OR.  (che2 .GT. 0.9))) iindex (1) = 0

      ! Last level
      che1 = LOG (t (index - 1) / t (index - 2)) / LOG (p (index - 1) &
                                / p (index - 2)) 
      che2 = LOG (t (index    ) / t (index - 1)) / LOG (p (index    ) &
                                / p (index - 1)) 

      IF (((che2 .LT. -0.9) .OR.  (che2 .GT. 0.9)) .AND.              &
          ((che1 .GT. -0.9) .AND. (che1 .LT. 0.9))) iindex (index) = 1
!     IF (((che2 .LT. -0.9) .OR.  (che2 .GT. 0.9)) .AND.              &
!         ((che1 .LT. -0.9) .OR.  (che1 .GT. 0.9))) iindex (index) = 0

      loop2: DO i = 2, index - 1

         che1 = LOG (t (i + 1) / t (i    )) / LOG (p (i + 1) / p (i     )) 
         che2 = LOG (t (i    ) / t (i - 1)) / LOG (p (i    ) / p (i - 1 )) 
         th1  = t (i - 1) * (100000.0 / p (i - 1)) ** RCP
         th   = t (i    ) * (100000.0 / p (i    )) ** RCP
         th2  = t (i + 1) * (100000.0 / p (i + 1)) ** RCP

         IF ((((che1 .LT. -0.9) .OR. (che1 .GT. 0.9))  .AND.             &
              ((che2 .LT. -0.9) .OR. (che2 .GT. 0.9))) .OR.              &
              ((th .LT. (th1 - 10.0)) .AND. (th .LT. (th2 - 10.0))) .OR. &
              ((th .GT. (th1 + 10.0)) .AND. (th .GT. (th2 + 10.0))))     &

                iindex ( i ) = 1

         IF ((i .GE. 3) .AND. ((che1 .GT. -0.6) .AND. (che1 .LT. 0.6)) .AND. &
            ((che2 .GT. -0.6 ) .AND. (che2 .LT. 0.6)) .AND.                  &
            ((che1 * che2) .LT. 0.0)) THEN 

              iindex (i) = 2
             
         ENDIF

      END DO loop2
   
      loop3: DO i = 2, index - 1

         IF (iindex (i) .EQ. 0) THEN

            ! Do nothing

         ELSE

            ! Change the sign of temperature ( in Celsius )

            che1 = LOG (t (i + 1) / (-t (i) + 2. * 273.15)) &
                                  / LOG (p (i + 1) / p (i)) 
            che2 = LOG ((-t (i) + 2. * 273.15) / t (i - 1)) &
                                  / LOG (p (i) / p ( i - 1)) 

            th1 = t (i - 1) * (100000.0 / p (i - 1)) ** RCP
            th  = (-t ( i)  + 2. * 273.15 ) * (100000.0 / p (i)) ** RCP
            th2 = t (i + 1) * (100000.0 / p (i + 1)) ** RCP

            IF (iindex ( i ) .EQ. 1) THEN

               IF ((((che1 .LT. -0.6) .OR. (che1 .GT. 0.6))   .OR. &
                    ((che2 .LT. -0.6) .OR. ( che2 .GT. 0.6))) .OR. &
                      ((th .GT. ( th1 + 10.0)) .AND. (th .GT. (th2 + 10.0)))) &
                  THEN    

                  ! Do nothing

               ELSE

                  tqc  ( i ) = wrong_t_sign
                  tdqc ( i ) = wrong_t_sign
                  rhqc ( i ) = wrong_t_sign
                  qvqc ( i ) = wrong_t_sign

                  IF ( .NOT. eps_equal ( td ( i ) , missing_r , 1. ) ) THEN
                     depression = t(i) - td(i)
                  END IF

                  t  ( i ) = -t  ( i ) + 2. * 273.15

                  IF ( .NOT. eps_equal (td (i), missing_r, 1.)) THEN
                     td (i) = t (i) - depression
                     rh (i) = 100. * exp ( 5418.12 * ( 1./t(i) - 1./td(i)))
                  END IF

                  IF (.NOT. eps_equal (qv (i), missing_r, 1.)) THEN

                      es = 6.112 * EXP (17.67 * (t (i) - 273.15) &
                                              / (t (i) - 273.15+243.5))
                      qs = 0.622 * es / (p (i)/100. - es)

                      qv (i) = MAX (0.01*rh (i)*qs, 0.);

                  ENDIF

                  IF (print_vert) THEN

                      WRITE (message , FMT = '(          &
                 &   "Failed super-adiabatic check: ",   &
                 &  " ID = ",A5,","," NAME = "  ,A8,", Modified", & 
                 &  " T = " ,F5.1," (K)",","," P = ",F6.1, " (hPa)", &
                 &  " I =",I3," iindex=",I4)') &
                 &    TRIM (obs%location%id), TRIM (obs%location%name), &
                 &    t (i), p (i)/100., i, iindex(i)

                      WRITE (iunit, '(A)') message

                  ENDIF
                  iindex ( i ) = 0

               END IF

            ELSE

               IF (((che1 .GT. -0.6 ) .AND. (che1 .LT. 0.6)) .AND.  &
                   ((che2 .GT. -0.6 ) .AND. (che2 .LT. 0.6)) .AND.  &
                   ((che1 * che2) .GT. 0.0))  THEN

                     tqc  ( i ) = wrong_t_sign
                     tdqc ( i ) = wrong_t_sign
                     rhqc ( i ) = wrong_t_sign
                     qvqc ( i ) = wrong_t_sign

                     IF (.NOT. eps_equal ( td ( i ) , missing_r , 1. ) ) THEN
                         depression = t(i) - td(i)
                     END IF

                     t  ( i ) = -t ( i ) + 2. * 273.15

                     IF (.NOT. eps_equal ( td ( i ) , missing_r , 1. ) ) THEN
                         td (i) = t(i) - depression
                         rh (i) = 100. * exp ( 5418.12 * ( 1./t(i) - 1./td(i)))
                     END IF

                     IF (print_vert) THEN

                         WRITE (message , FMT = '(          &
                   &    "Failed super-adiabatic check: ",   &
                   &   " ID = ",A5,","," NAME = "  ,A8,", Modified", & 
                   &   " T = " ,F5.1," (K)",","," P = ",F6.1, " (hPa)", &
                   &   " I =",I3," iindex=",I4)') &
                   &     TRIM (obs%location%id), TRIM (obs%location%name), &
                   &     t (i), p (i)/100., i, iindex(i)

                         WRITE (iunit, '(A)') message

                     ENDIF
                     iindex ( i ) = 0

               ELSE

                  iindex (i) = 0

               END IF

            END IF

         END IF

      END DO loop3

      loop4: DO i = 1, index

         IF (iindex (i) .EQ. 1) THEN

             IF (print_vert) THEN

                 WRITE (message , FMT = '(          &
           &    "Failed super-adiabatic check: ",   &
           &   " ID = ",A5,","," NAME = "  ,A8,",", & 
           &   " T = " ,F5.1," (K)",","," P = ",F6.1, " (hPa)", &
           &   " I =",I3," iindex=",I4)') &
           &     TRIM (obs%location%id), TRIM (obs%location%name), &
           &     t (i), p (i)/100., i, iindex(i)

                 WRITE (iunit,'(A)') message

            ENDIF

            tqc  (i) = t_fail_supa_inver
            tdqc (i) = t_fail_supa_inver
            rhqc (i) = t_fail_supa_inver
            qvqc (i) = t_fail_supa_inver

!           t  (i) = missing_r
!           td (i) = missing_r
!           rh (i) = missing_r
!           qv (i) = missing_r

            failed = .TRUE.

         END IF

      END DO loop4

      NULLIFY (current)

     !  Put the "Quality Flag" and changed data into "obs" data

      current => obs%surface

      loopthermal : DO WHILE (ASSOCIATED (current))

      DO i = 1, index

            IF (eps_equal (p (i), current%meas%pressure%data, 0.1)) THEN 

                current%meas%temperature%data = t  (i)
                current%meas%dew_point%data   = td (i)
                current%meas%rh%data          = rh (i)
                current%meas%qv%data          = qv (i)

               IF (current%meas%temperature%qc .NE. missing ) THEN
                   current%meas%temperature%qc = current%meas%temperature%qc &
                                               + tqc ( i )
               END IF

               IF (current%meas%dew_point%qc .NE. missing ) THEN
                   current%meas%dew_point%qc = current%meas%dew_point%qc &
                                             + tdqc ( i )
               END IF

               IF (current%meas%rh%qc .NE. missing ) THEN
                   current%meas%rh%qc = rhqc ( i ) &
                 + current%meas%rh%qc
               END IF

               IF (current%meas%qv%qc .NE. missing ) THEN
                   current%meas%qv%qc = qvqc (i ) &
                 + current%meas%qv%qc
               END IF

            ELSE

               ! Do nothing
            END IF

         END DO

         current => current%next

      END DO loopthermal

      DEALLOCATE ( p )
      DEALLOCATE ( t )
      DEALLOCATE ( td )
      DEALLOCATE ( rh )
      DEALLOCATE ( qv )
      DEALLOCATE ( tqc )
      DEALLOCATE ( tdqc )
      DEALLOCATE ( rhqc )
      DEALLOCATE ( qvqc )
      DEALLOCATE ( iindex )
      NULLIFY    ( current )

   END IF

   !  End checking thermal data

   !  Check for spikes in hodograph and mark them in wind data,
   !  if wind speed is too strong or wind direction is wrong. 
   
   !  Find out the vertical level number, then ALLOCATE arrays

   index = 1

   current => obs%surface

   DO WHILE (ASSOCIATED ( current ))

      IF   ((.NOT. eps_equal (current%meas%pressure%data, missing_r, 1.)) .AND.&
            (.NOT. eps_equal (psfc,                       missing_r, 1.)) .AND.&
                             (current%meas%pressure%data .LE. psfc )      .AND.&
            (.NOT. eps_equal (current%meas%speed%data,    missing_r, 1.)) .AND.&
            (.NOT. eps_equal (current%meas%direction%data,missing_r, 1.))) THEN

             index = index + 1

      ELSE

         ! Do nothing, missing or bad data

      END IF

      current => current%next

   END DO

   index = index - 1

   IF ((index .LE. 3) .OR. (eps_equal (psfc , missing_r, 1.))) THEN

      !    The vertical levels are less than 3, do nothing

   ELSE !  Check wind data

      ALLOCATE ( p     ( index ) )
      ALLOCATE ( u     ( index ) )
      ALLOCATE ( v     ( index ) )
      ALLOCATE ( ws    ( index ) )
      ALLOCATE ( wd    ( index ) )
      ALLOCATE ( wsqc  ( index ) )
      ALLOCATE ( wdqc  ( index ) )
      ALLOCATE ( uqc   ( index ) )
      ALLOCATE ( vqc   ( index ) )
      ALLOCATE ( work1 ( index ) )
      ALLOCATE ( work2 ( index ) )
      NULLIFY  ( current )

      !  Read data set into array and perform quality check

      index = 1
      current => obs%surface

      DO WHILE (ASSOCIATED (current))
         IF ((.NOT. eps_equal (current%meas%pressure%data, missing_r, 1.)).AND.&
                              (current%meas%pressure%data  .LE. psfc)     .AND.&
             (.NOT. eps_equal (current%meas%speed%data,    missing_r, 1.)).AND.&
             (.NOT. eps_equal (current%meas%direction%data,missing_r, 1.))) THEN

              p    (index) = current%meas%pressure%data
              ws   (index) = current%meas%speed%data
              wd   (index) = current%meas%direction%data
              u    (index) = current%meas%u%data
              v    (index) = current%meas%v%data
              wsqc (index) = 0
              wdqc (index) = 0
              uqc  (index) = 0
              vqc  (index) = 0
              index = index + 1

         ELSE

            ! Do nothing, missing or bad data

         END IF

         current => current%next

      END DO

      index = index - 1

      NULLIFY (current)

      loop5: DO i = 2, index - 1

             d1 = wd (i - 1)
             d2 = wd (i + 1)

             IF (( d2 - d1) .GT.  180.) d1 = d1 + 360.
             IF (( d2 - d1) .LT. -180.) d2 = d2 + 360.

             work1 (i) = wd (i) - 0.5 * (d1 + d2) + 0.0001

             IF (work1 (i) .LT. -180. ) work1 (i) = work1 (i) + 360.
             IF (work1 (i) .GT.  180. ) work1 (i) = work1 (i) - 360.

             work2 (i) = ws (i) - 0.5 * (ws (i - 1) + ws (i + 1)) + 0.0001

      END DO loop5

      loop6: DO i = 3, index - 2

! The wind speed at this level is reasonable compared to up and down levels;
! The wind direction at the current level is reasonable compared to up 
! and down levels.

      IF (((ABS (work2 (i)) .LT. 50.) .OR. & 
                (work2 (i - 1) * work2 (i + 1)  .LE.   0.0) .OR.               &
           (MAX (work2 (i + 1) / work2 (i - 1),                                &
                 work2 (i - 1) / work2 (i + 1)) .GT.   3.)) .AND.              &
              (((wd  (i)                        .LE. 360.)  .AND.              &
                (ws (i) * ABS (work1 (i)) .LT. 1600.))      .OR.               &
               ((work1 (i - 1) * work1 (i + 1)) .LE. 0.0 )  .OR.               &
           (MAX (work1 (i + 1) / work1 (i - 1),                                &
                 work1 (i - 1) / work1 (i + 1)) .GT. 3.)))  THEN

            ! wind data is good, do nothing

         ELSE

            IF (print_vert) THEN

                WRITE (message, FMT = '(&
           &   "Spikes in wind detected: ID = ",A5,", NAME="  ,A8, &
           &  " Speed = ",F6.1," (m/s), Direction = ",F5.1," (degrees)")')&
           &    obs%location%id , obs%location%name , ws(i) , wd(i)

                WRITE (iunit,'(A)') message

            ENDIF

            wsqc (i) = wrong_wind_data
            wdqc (i) = wrong_wind_data
            uqc  (i) = wrong_wind_data
            vqc  (i) = wrong_wind_data
!           ws   (i) = missing_r
!           wd   (i) = missing_r
!           u    (i) = missing_r
!           v    (i) = missing_r

         END IF

      END DO loop6

      !  Put the "Quality Flag" into "obs" data

      current => obs%surface

      loopwind : DO WHILE (ASSOCIATED (current))

         DO i = 1, index

            IF (eps_equal (p (i), current%meas%pressure%data, 0.1)) THEN 

                current%meas%speed%data     = ws (i)
                current%meas%direction%data = wd (i)
                current%meas%u%data = u (i)
                current%meas%v%data = v (i)

               IF (current%meas%speed%qc .NE. missing) THEN
                   current%meas%speed%qc = wsqc (i) &
                 + current%meas%speed%qc
               END IF

               IF (current%meas%direction%qc .NE. missing) THEN
                   current%meas%direction%qc = wdqc (i) &
                 + current%meas%direction%qc
               END IF

               IF ((current%meas%u%qc .NE. missing) .AND. &
                   (current%meas%v%qc .NE. missing)) THEN
                    current%meas%u%qc = uqc (i) &
                  + current%meas%u%qc
                    current%meas%v%qc = vqc (i) &
                  + current%meas%v%qc
               END IF

            ELSE

               ! Do nothing

            END IF

         END DO

         current => current%next

      END DO loopwind

      DEALLOCATE (p      )
      DEALLOCATE (u      )
      DEALLOCATE (v      )
      DEALLOCATE (ws     )
      DEALLOCATE (wd     )
      DEALLOCATE (wsqc   )
      DEALLOCATE (wdqc   )
      DEALLOCATE (uqc    )
      DEALLOCATE (vqc    )
      DEALLOCATE (work1  )
      DEALLOCATE (work2  )
      NULLIFY    (current)

   END IF

   !  End checking wind data

END SUBROUTINE vert_cons_check
!-------------------------------------------------------------------------------

 SUBROUTINE proc_qc2 (max_number_of_obs, obs , number_of_obs, &
                      qc_test_above_lid, print_height_qc)
!------------------------------------------------------------------------------

   USE module_type
   USE module_func
   USE module_mm5

   IMPLICIT NONE

   INTEGER,       INTENT (in)                   :: max_number_of_obs
   TYPE (report), DIMENSION (max_number_of_obs) :: obs
   INTEGER ,      INTENT (in)                   :: number_of_obs
   LOGICAL,       INTENT (in)                   :: qc_test_above_lid
   LOGICAL,       INTENT (in)                   :: print_height_qc

   REAL                                         :: hlid
   TYPE (measurement), POINTER                  :: current
   INTEGER                                      :: i
   LOGICAL                                      :: connected
   LOGICAL                                      :: found = .FALSE.
   INTEGER                                      :: iunit, io_error
   CHARACTER (LEN = 32)                         :: proc_name = 'proc_qc2: '
   CHARACTER (LEN = 80)                         :: title, fmt_id, fmt_level
   CHARACTER (LEN = 80)                         :: filename

   INCLUDE 'missing.inc'
!-------------------------------------------------------------------------------


     IF (.NOT. qc_test_above_lid) RETURN


! 1. OPEN DIAGNOSTIC FILE
! =======================

      WRITE (0,'(A)')  &
'------------------------------------------------------------------------------'
      WRITE (UNIT = 0, FMT = '(A,/)') 'CHECK OBS HEIGHT...:'


      IF (print_height_qc) THEN

      filename = 'obs_qc2.diag'
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

      fmt_level = '(/,A,2(F10.2,2X),A,I8)'

      WRITE (UNIT = iunit , FMT = '(A67)', ADVANCE = 'no') filename

      ENDIF


! 1.  CHECK IF OBSERVATIONS ARE WITHIN THE MODEL VERTICAL DOMAIN
! ==============================================================
 
! 1.0 Model Lid height
!     -----------------

      hlid = ref_height (ptop)

loop_all_obs: &
      DO i = 1, number_of_obs

! 1.1 Check only valid obs and not gpspw
!     ----------------------------------

valid_obs:&
         IF ((.NOT.  obs (i) % info % discard) .AND. &
        (ASSOCIATED (obs (i) % surface)))     THEN

! 1.1 Print obs id
!     ------------

         IF (print_height_qc) THEN

             IF (.NOT. found) THEN
                 fmt_id = '(TL67,A20,A5,1X,A23,2F9.3)'
             ELSE
                 fmt_id = '(//,A20,A5,1X,A23,2F9.3)'
             ENDIF

             WRITE (UNIT = iunit , FMT = TRIM (fmt_id), ADVANCE = 'no') &
            "Found Name and ID = " ,                                       &
             TRIM  (obs (i) % location % id ) ,                   &
             TRIM  (obs (i) % location % name),                   &
                    obs (i) % location % latitude,                &
                    obs (i) % location % longitude

!SDH010706             found = .FALSE.
             found = .TRUE.

         ENDIF


! 2.  Loop over upper levels
! ==========================

         current => obs (i) % surface

all_levels:&
         DO WHILE (ASSOCIATED (current))

! 2.1 Upper bound
!     -----------

! Use Ptop check instead of hlid check (Y.-R. Guo  07/13/2004):
            IF (current % meas % pressure % data < ptop) THEN

                current % meas % pressure % qc    = above_model_lid

                if ( print_height_qc ) then
                WRITE (UNIT = iunit , FMT = TRIM (fmt_level), ADVANCE = 'no') &
               "Model ptop & obs pressure = ", ptop,  &
                                            current % meas % pressure % data, &
                                 "  qc = ", current % meas % pressure % qc
                end if !print

                found = .TRUE.

            ENDIF

!            IF (current % meas % height % data >= hlid) THEN

!                current % meas % height % qc    = above_model_lid

!                WRITE (UNIT = iunit , FMT = TRIM (fmt_level), ADVANCE = 'no') &
!               "Model lid & obs height = ", hlid,  &
!                                            current % meas % height % data, &
!                                 "  qc = ", current % meas % height % qc

!                found = .TRUE.

!            ENDIF

! 2.2 Next level
!     ----------

            current => current % next

          ENDDO all_levels
         
        ENDIF valid_obs

      ENDDO loop_all_obs

      IF (print_height_qc) WRITE (0,'(A)') ""
      IF (print_height_qc) CLOSE (iunit)

 END SUBROUTINE proc_qc2
!-------------------------------------------------------------------------------
END MODULE module_qc
