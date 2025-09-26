!
MODULE Zeeman_Utility
  ! Description:
  !   containing routines to load geomagnetic field Lookup table, compute
  !   geomagnetic field and its angles relative to the wave propagation
  !   direction.
  !
  ! History:
  !    ----     -------
  ! 11/07/2007  created by Y. Han, NOAA/JCSDA
  ! 11/24/2009  modified for CRTM
  !
  
  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,         ONLY: fp                                     
  USE Message_Handler,    ONLY: SUCCESS, FAILURE, Display_Message  
  USE File_Utility,       ONLY: Get_Lun, File_Exists
       
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: load_bfield_lut   ! function to load Geomagnetic field LUT
  PUBLIC :: compute_bfield    ! subroutine to calculate the Geomagnetic field 
                              ! using a LUT and two cosine angles of the field
                              ! relative to the wave propagation direction k.
  PUBLIC :: compute_kb_angles ! subroutine to compute two cosine angles of the 
                              ! geomagnetic field relative to the wave 
                              ! propagation direction k.


  INTERFACE compute_bfield
    MODULE PROCEDURE Compute_bfield_F1
    MODULE PROCEDURE compute_bfield_F2
    MODULE PROCEDURE compute_bfield_F3
  END INTERFACE compute_bfield
  
  ! Array for Earth's magnetic field
  INTEGER,      PARAMETER   :: n_Lat = 91, n_Lon = 181               
  INTEGER, SAVE             :: BField(3, n_lat, n_lon)               

  Real(fp), parameter :: DEGREES_TO_RADIANS  = 3.141592653589793238462643_fp/180.0_fp

CONTAINS

  !--------------------------------------------------------------------------------
  !
  ! NAME:
  !       load_bfield_lut
  !
  ! PURPOSE:
  !       Function to the geomagnetic filed LUT
  !
  ! CALLING SEQUENCE:
  !   Error_Status = load_bfield_lut(filename_LUT)
  !
  ! INPUT ARGUMENT:
  ! 
  !   filename_LUT:       file name for the file containing the LUT of
  !                       the Earth magnetic field.
  !                       UNITS:      N/A
  !                       TYPE:       character string
  !                       DIMENSION:  Scale
  !                       ATTRIBUTES: INTENT(IN)
  !----------------------------------------------------------------------------
  Function load_bfield_lut(filename_LUT) Result(Error_Status)
    Character(*), Intent(in)  :: filename_LUT

    Integer             :: Error_Status

    ! local
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'load_bfield_lut'
    Integer            :: file_id, io_status
    Character (len=80) :: Message
    Integer            :: i, j, iskip

    Error_Status = SUCCESS

    IF ( .NOT. File_Exists( TRIM(filename_LUT) ) )THEN
      Error_Status = FAILURE
      Message = 'File '//TRIM(Filename_LUT)//' not found.'
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status)
    END IF

    ! Find a free logical unit number for file access
    file_id = Get_Lun()
    
    OPEN( file_id, FILE   = filename_LUT, &
          & STATUS = 'OLD', &
          & IOSTAT = io_status ) 

    IF ( io_status /= 0 ) THEN
       Error_Status = FAILURE
       WRITE( Message, '( "Error opening ", a, ". IOSTAT = ", i5 )' ) &
             & TRIM(filename_LUT), io_status 
       CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status)
       RETURN
    END IF

    READ(file_id, *, iostat = io_status)iskip, iskip, &
                      ((BField(:, i, j), j=1,n_Lon), i=1,n_Lat)  

    If(io_status /= 0) Then                                             
       Error_Status = FAILURE
       Write( Message, '( "Error reading data from ", a, ". IOSTAT = ", i5 )' ) &
             & TRIM(filename_LUT), io_status 
       CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status)
       Return                                                           
    Endif                                                               
              
    Close( unit = file_id )

  End Function load_bfield_lut

  !--------------------------------------------------------------------------------
  !
  ! NAME:
  !       compute_bfield
  !
  ! PURPOSE:
  !       Subroutine to calculate the Geomagnetic field using a LUT and 
  !       two cosine angles of the field relative to the wave propagation
  !       direction k. 
  !
  ! CALLING SEQUENCE:
  !
  !        CALL compute_bfield(latitude, longitude, & ! Inputs 
  !                            Bx, By, Bz, Be)        ! Outputs
  !
  !   OR
  !
  !        CALL compute_bfield(latitude,          &   ! input
  !                            longitude,         &   ! input   
  !                            sensor_zenang,     &   ! input   
  !                            sensor_aziang,     &   ! input     
  !                            Be,                &   ! output  
  !                            cos_bkang,         &   ! output  
  !                            cos_baziang)           ! output   
  !  
  !   OR       
  !
  !        CALL compute_bfield(latitude,               &   ! input
  !                            longitude,              &   ! input   
  !                            sensor_zenang,          &   ! input   
  !                            sensor_relative_aziang, &   ! input   
  !                            Julian_day,             &   ! input   
  !                            utc_time,               &   ! input   
  !                            Be,                     &   ! output  
  !                            cos_bkang,              &   ! output  
  !                            cos_baziang)                ! output  
  !
  !
  ! INPUT ARGUMENTS:
  !
  !
  !      latitude :       Latitude, 0 - 180 (0 North Pole; 180 - South Pole 
  !                       UNITS:      Degree
  !                       TYPE:       real(fp)
  !                       DIMENSION:  Scale
  !                       ATTRIBUTES: INTENT(IN)
  !
  !      longitude:       longitude, 0 - 360 East
  !                       UNITS:      Degree
  !                       TYPE:       real(fp)
  !                       DIMENSION:  Scale
  !                       ATTRIBUTES: INTENT(IN)
  !
  !   sensor_zenang:      sensor zenith angle
  !                       UNITS:      Degree
  !                       TYPE:       real(fp)
  !                       DIMENSION:  Scale
  !                       ATTRIBUTES: INTENT(IN)
  !
  !   sensor_aziang:      sensor azimuth angle (starts from East, 
  !                                             positive counterclockwise)
  !                       UNITS:      Degree
  !                       TYPE:       real(fp)
  !                       DIMENSION:  Scale
  !                       ATTRIBUTES: INTENT(IN)
  !
  ! sensor_relative_aziang: sensor azimuth angle relative to the sun azimuth
  !                         angle.  
  !    Sun_azimuth_angle - from north, East positive                                     
  !    sensor_relative_aziang = 90 - (sun_azimuth_angle + Sensor_aziang)          
  !                       UNITS:      degree
  !                       TYPE:       real(fp)
  !                       DIMENSION:  Scale
  !                       ATTRIBUTES: INTENT(IN)
  !
  !        Julian_day:    Julian_Day 1=Jan 1, 365=Dec31 (366 leap year) 
  !                       UNITS:      day
  !                       TYPE:       integer(JPIM)
  !                       DIMENSION:  Scale
  !                       ATTRIBUTES: INTENT(IN)
  !
  !          utc_time:    Universal_Time 0.00-23.999,(GMT,Z time) 
  !                       UNITS:      hour
  !                       TYPE:       real(fp)
  !                       DIMENSION:  Scale
  !                       ATTRIBUTES: INTENT(IN)
  !
  ! OUTPUT ARGUMENTS:
  !       Bx:             Magetic field East component 
  !                       UNITS:      Gauss
  !                       TYPE:       real(fp)
  !                       DIMENSION:  Scalar
  !                       ATTRIBUTES: INTENT(OUT)
  !
  !       By:             Magetic field North component 
  !                       UNITS:      Gauss
  !                       TYPE:       real(fp)
  !                       DIMENSION:  Scalar
  !                       ATTRIBUTES: INTENT(OUT)
  !
  !       Bz:             Magetic field zenith component (positive upward) 
  !                       UNITS:      Gauss
  !                       TYPE:       real(fp)
  !                       DIMENSION:  Scalar
  !                       ATTRIBUTES: INTENT(OUT)
  !
  !       Be:             Magetic field strength (sqrt(BxBx + ByBy + BzBz)) 
  !                       UNITS:      Gauss
  !                       TYPE:       real(fp)
  !                       DIMENSION:  Scalar
  !                       ATTRIBUTES: INTENT(OUT)
  !
  !     cos_bkang:        cosine of the angle between the magnetic field Be 
  !                       vector and the wave propagation direction k.
  !                       UNITS:      N/A
  !                       TYPE:       real(fp)
  !                       DIMENSION:  Scalar
  !                       ATTRIBUTES: INTENT(OUT)
  !
  !   cos_baziang:        cosine of the azimuth angle of the Be vector in the
  !                       (v, h, k) coordinates system, where v, h and k comprise
  !                       a right-hand orthogonal system, similar to the (x, y, z)
  !                       Catesian coordinates. The h vector is normal to the
  !                       plane containing the k and z vectors, where k points
  !                       to the wave propagation direction and z points 
  !                       to the zenith. h = (z cross k)/|z cross k|. The
  !                       azimuth angle is the angle on the (v, h) plane
  !                       from the positive v axis to the projected line of the
  !                       Be vector on this plane, positive counterclockwise.
  !                       UNITS:      N/A
  !                       TYPE:       real(fp)
  !                       DIMENSION:  Scalar
  !                       ATTRIBUTES: INTENT(OUT)
  !
  !--------------------------------------------------------------------------------

  SUBROUTINE compute_bfield_F1(latitude, longitude, & ! Inputs 
                               Bx, By, Bz, Be)        ! Outputs
    REAL(fp), INTENT(IN)  :: latitude, longitude
    REAL(fp), INTENT(OUT) :: Bx, By, Bz, Be

    ! Local
    REAL(fp)                :: x2, w1_lat, w1_lon
    INTEGER                 :: idx_lat, idx_lon
    REAL(fp), PARAMETER     :: dlat = 2.0_fp, dlon = 2.0_fp  

    idx_lat = INT(latitude/dlat)+1
    IF(idx_lat >= n_Lat)idx_lat = n_lat-1
    idx_lon = INT(longitude/dlat)+1
    IF(idx_lon >= n_Lon)idx_lon = n_lon-1

    x2 = REAL(idx_lat, fp)*dlat
    w1_lat = (x2 - latitude)/dlat
  
    x2 = REAL(idx_lon, fp)*dlat
    w1_lon = (x2 - longitude)/dlat

    Bx = BField_Component(1, Bfield, w1_lat, w1_lon, idx_lat, idx_lon)
    By = BField_Component(2, Bfield, w1_lat, w1_lon, idx_lat, idx_lon)
    Bz = BField_Component(3, Bfield, w1_lat, w1_lon, idx_lat, idx_lon)
                    
    Be = SQRT(Bx*Bx+By*By+Bz*Bz)

  CONTAINS

    FUNCTION BField_Component(comp, Bfield, w1_lat, w1_lon, &                    
                              idx_lat, idx_lon) Result(B) 

     INTEGER,      INTENT(IN)  :: comp
     INTEGER,      INTENT(IN)  :: BField(:,:,:)
     REAL(fp),     INTENT(IN)  :: w1_lat, w1_lon
     INTEGER,      INTENT(IN)  :: idx_lat, idx_lon

     REAL(fp) :: B                      

     REAL(fp), PARAMETER :: Scale = 0.001_fp
     REAL(fp)            :: w2_lat, w2_lon              

      w2_lat = 1.0_fp - w1_lat                                                      
      w2_lon = 1.0_fp - w1_lon                                                      
      B = (w1_lon*(w1_lat*REAL(BField(comp,idx_lat,   idx_lon),  fp) + &     
                   w2_lat*REAL(BField(comp,idx_lat+1, idx_lon),  fp))  &     
         + w2_lon*(w1_lat*REAL(BField(comp,idx_lat,   idx_lon+1),fp) + &     
                   w2_lat*REAL(BField(comp,idx_lat+1, idx_lon+1),fp)))*Scale 

     END FUNCTION BField_Component
        
  END SUBROUTINE compute_bfield_F1

  Subroutine compute_bfield_F2(latitude, longitude, sensor_zenang, sensor_aziang, & 
                               Be, cos_bkang, cos_baziang)

    !subroutine arguments:
    Real(fp), Intent(in)             :: latitude
    Real(fp), Intent(in)             :: longitude
    Real(fp), Intent(in)             :: sensor_zenang
    Real(fp), Intent(in)             :: sensor_aziang
    Real(fp), Intent(out)            :: Be
    Real(fp), Intent(out)            :: cos_bkang
    Real(fp), Intent(out)            :: cos_baziang

    ! local variable
    Real(fp)    :: Bx, By, Bz
  
    ! get Earth magnetic filed from LUT
    Call compute_bfield_F1(latitude, longitude,  & ! inputs
                           Bx, By, Bz, Be)         ! outputs

    ! compute the cosines of the angle between the magnetic field Be and
    ! propagation direction and Be's azimuth angle
    Call Compute_kb_Angles(Bx, By, Bz, sensor_zenang, sensor_aziang,  &  ! Input
                           cos_bkang, cos_baziang)                       ! Output

  End Subroutine compute_bfield_F2

  Subroutine compute_bfield_F3(latitude, longitude, sensor_zenang, &
                               sensor_relative_aziang, julian_day, utc_time, &
                               Be, cos_bkang, cos_baziang)
    !subroutine arguments:
    Real(fp), Intent(in)             :: latitude
    Real(fp), Intent(in)             :: longitude
    Real(fp), Intent(in)             :: sensor_zenang
    Real(fp), Intent(in)             :: sensor_relative_aziang
    Integer,  Intent(in)             :: julian_day
    Real(fp), Intent(in)             :: utc_time
    Real(fp), Intent(out)            :: Be
    Real(fp), Intent(out)            :: cos_bkang
    Real(fp), Intent(out)            :: cos_baziang

    ! Local     
    Real(fp)    :: Bx, By, Bz, Solar_ZA, Solar_AZ, lat, lon, sensor_aziang

    ! compute the cosines of the angle between the magnetic field Be and
    ! propagation direction and Be's azimuth angle
    Call compute_bfield_F1(latitude, longitude,  & ! inputs
                            Bx, By, Bz, Be)         ! outputs

    lat = 90.0_fp - latitude
    lon = Longitude
    If(lon > 180.0_fp)lon = lon - 360.0_fp
    ! Compute Solar azimuth angle
    Call Solar_ZA_AZ(lat,lon, Real(julian_day, fp), utc_time, &                     
                     Solar_ZA, Solar_AZ)
    ! Compute satellite azimuth angle (starts from East, positive counterclockwise)
    sensor_aziang = sensor_relative_aziang + solar_AZ
    sensor_aziang = 90.0_fp - sensor_aziang

    ! compute for cos_bkangle, cos_baziangle
    Call Compute_kb_Angles(Bx, By, Bz, sensor_zenang, sensor_aziang,  &  ! Input
                           cos_bkang, cos_baziang)                       ! Output


  End Subroutine compute_bfield_F3


!--------------------------------------------------------------------------------
!
! NAME:
!       Compute_kb_Angles
!
! PURPOSE:
!       Subroutine to calculate the cosine of the angle between the Geomagnetic 
!       field (Be) and wave propagation direction (k) and the cosine of the 
!       azimuth angle of the Be vector in the (v, h, k) coordinates system (see
!       more detailed description below)
!         
!
! CALLING SEQUENCE:
!      CALL Compute_kb_Angles(Bx, By, Bz,   &                       ! Input
!                             sensor_zenang, sensor_aziang, &       ! Input   
!                             cos_bk_Angle, cos_baziang)            ! Output  
! INPUT ARGUMENTS:
!
!       Bx:             Magetic field East component 
!                       UNITS:      Gauss
!                       TYPE:       Real(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       By:             Magetic field North component 
!                       UNITS:      Gauss
!                       TYPE:       Real(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Bz:             Magetic field zenith component (positive upward) 
!                       UNITS:      Gauss
!                       TYPE:       Real(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!   sensor_zenang :     sensor zenith angle
!                       UNITS:      Degree
!                       TYPE:       Real(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!    
!   sensor_aziang :     sensor zenith angle defined as the
!                       angle from the East towards North.
!                       UNITS:      Degree
!                       TYPE:       Real(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!     cos_bkang:        cosine of the angle between the magnetic field Be   
!                       vector and the wave propagation direction k.       
!                       UNITS:      N/A                                    
!                       TYPE:       real(fp)                             
!                       DIMENSION:  Scalar                                 
!                       ATTRIBUTES: INTENT(OUT)                            
!
!   cos_baziang:        cosine of the azimuth angle of the Be vector in the       
!                       (v, h, k) coordinates system, where v, h and k comprise   
!                       a right-hand orthogonal system, similar to the (x, y, z)  
!                       Catesian coordinates. The h vector is normal to the       
!                       plane containing the k and z vectors, where k points      
!                       to the wave propagation direction and z points            
!                       to the zenith. h = (z cross k)/|z cross k|. The           
!                       azimuth angle is the angle on the (v, h) plane            
!                       from the positive v axis to the projected line of the     
!                       Be vector on this plane, positive counterclockwise.       
!                       UNITS:      N/A
!                       TYPE:       Real(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE Compute_kb_Angles(Bx, By, Bz,   &                       ! Input
                               sensor_zenang, sensor_aziang, &       ! Input
                               cos_bkang, cos_baziang)   ! Output
    REAL(fp), INTENT(IN)  :: Bx, By, Bz
    REAL(fp), INTENT(IN)  :: sensor_zenang, sensor_aziang
    REAL(fp), INTENT(OUT) :: cos_bkang, cos_baziang

    ! Local
    REAL(fp) :: B, B_v, B_h, B_p, kx, ky, kz, &
                  SIN_SenZA, COS_SenAZ, SIN_SenAZ, COS_SenZA 

    SIN_SenZA = SIN(sensor_zenang*DEGREES_TO_RADIANS)
    COS_SenZA = COS(sensor_zenang*DEGREES_TO_RADIANS)
    SIN_SenAZ = SIN(sensor_aziang*DEGREES_TO_RADIANS)
    COS_SenAZ = COS(sensor_aziang*DEGREES_TO_RADIANS)

    ! compute k directional vector from satellite's zenith and azimuth angles 
    kx = SIN_SenZA*COS_SenAZ
    ky = SIN_SenZA*SIN_SenAZ
    kz = COS_SenZA

    ! compute consine of the angle between the magnetic field B and k
    B = SQRT(bx*bx+by*by+bz*bz)
    cos_bkang = (kx*bx + ky*by + kz*bz)/B
             
    ! Project the B vector on the V and H plane: B_v - B component on the V
    ! axis; B_h - B component on the H axis. 
    B_v = bx*kx*kz + by*ky*kz - bz*(ky*ky + kx*kx) ;  
    B_h = -bx*ky + by*kx                            

    ! compute the cosine of the azimuth angle
    B_p = SQRT(B_v*B_v + B_h*B_h)
    If(B_p /= 0.0_fp)Then
      cos_baziang = B_v / B_p
    Else
      cos_baziang = 0.0   ! not defined (take an arbitrary value)
    Endif  

  END SUBROUTINE Compute_kb_Angles
 
  Subroutine Solar_ZA_Az(latitude,        &      
                         longitude,       &      
                         julian_day,      &      
                         universal_time,  &      
                         ZA,              &      
                         Az)                     
!
!************************************************************************
!*                                                                      *
!*	Module Name:	Solar_Az_Za	                                *
!*                                                                      *
!*	Language:	Fortran 	   Library:	                *
!*	Version.Rev:	1.0  22 Feb 91	Programmer:	Kleespies       *
!*			1.1  28 Feb 91			Kleespies       *
!*			     Put equation of time into hour angle.      *
!*
!*      updated to f95, 4, September 2007, Y. Han                       *
!*                                                                      *
!*	Calling Seq:		Call Solar_Az_ZA(                       *
!*     &				latitude,                       *
!*     &				longitude,                      *
!*     &				julian_day,                     *
!*     &				universal_time,	                *
!*     &				ZA,	                        *
!*     &				Az)	                        *
!*                                                                      *
!*                                                                      *
!*	Description:	Computes solar azimuth and zenith angle for a   *
!*		given place and time.  Solar azimuth is the angle       *
!*		positive east of north form a point to the center of    *
!*		the solar disc. Solar zenith angle is the angle         *
!*		in degrees from zenith to the center of the solar disk. *
!*		The algorithms are taken from "Introduction to Solar    *
!*		Radiation" by Muhamad Iqbal, Academic Press, 1983.      *
!*		Note that lat=0,lon=0, 21Mar 12z will not give sun      *
!*		overhead, because that is not the way things work.      *
!*                                                                      *
!*	Input Args:	R*4  Latitude, +NH, -SH degrees	                *
!*			R*4  Longitude,-W, +E                           *
!*			R*4  Julian_Day 1=Jan 1, 365=Dec31 (366 leap yr)*
!*			R*4  Universal_Time 0.00-23.99,(GMT,Z time)     *
!*                                                                      *
!*	Output Args:	R*4  ZA	Solar Zenith Angle                      *
!*			R*4  AZ	Solar Azmuth Angle                      *
!*                                                                      *
!*	Common Blks:	none                                            *
!*	Include:	none                                            *
!*	Externals:	none                                            *
!*	Data Files:	none                                            *
!*                                                                      *
!*	Restrictions:	Accurate to within .1 deg.                      *
!*			No checking made to the validity of input       *
!*			parameters.                                     *
!*			Since solar zenith angle is a conic angle,      *
!*			it has no sign.                                 *
!*			No correction made for refraction.              *
!*                                                                      *
!*	Error Codes:	none                                            *
!*                                                                      *
!*	Error Messages:	                                                *
!*                                                                      *
!************************************************************************
!
        implicit none

        real(fp), intent(in)  :: latitude,longitude,julian_day,universal_time
        real(fp), intent(out) :: ZA, Az

        real(fp) :: local_sun_time,solar_elevation,equation_of_time
        real(fp) :: cosza,cosaz
        real(fp) :: hour_angle,day_angle
        real(fp) :: solar_declination
        real(fp) :: rlatitude, rlongitude
        real(fp), parameter :: DEGREES_TO_RADIANS  = 3.141592653589793238462643_fp/180.0_fp
        real(fp), parameter :: one_eighty_over_pi  = 1.0/DEGREES_TO_RADIANS
        real(fp), parameter :: threesixty_over_24 = 15.0
        real(fp), parameter ::  threesixty_over_365 = 0.98630137
        real(fp), parameter ::  min_declination = -23.433
        real(fp), parameter ::  day_offset = 10.0  !  original equation had this nine

!*	Compute day angle

        day_angle = threesixty_over_365*(julian_day-1.0)*DEGREES_TO_RADIANS

        rlatitude  = latitude  * DEGREES_TO_RADIANS
        rlongitude = longitude * DEGREES_TO_RADIANS

!*	Compute equation of Time

        Equation_of_Time =  &           
               ( 0.000075   &
               + 0.001868*Cos(Day_Angle)  &
               - 0.032077*Sin(Day_Angle)  &
               - 0.014615*Cos(2.*Day_Angle) &
               - 0.040890*Sin(2.*Day_Angle) )*229.18/60 ! in hours

!*	Compute local sun time
        local_sun_time  = universal_time   &
                       + Equation_of_Time &
                       + longitude/threesixty_over_24

!*	Compute solar declination

        solar_declination = &
         (       0.006918  &
               - 0.399912 * cos(day_angle)    &
               + 0.070257 * sin(day_angle)    &
               - 0.006758 * cos(2*day_angle)  &
               + 0.000907 * sin(2*day_angle)  &
               - 0.002697 * cos(3*day_angle)  &
               + 0.001480 * sin(3*day_angle) ) 

!*	Compute hour angle
        hour_angle = threesixty_over_24*mod(local_sun_time+12.0_fp , 24.0_fp)

!*	Compute solar zenith angle
        cosza = sin(rlatitude)*sin(solar_declination)  &
              + cos(rlatitude)*cos(solar_declination)*cos(hour_angle*DEGREES_TO_RADIANS)

        ZA = acos(cosza)*one_eighty_over_pi

!*	Compute solar azimuth angle
        solar_elevation = 90.0 - ZA

        If(Solar_Elevation .eq. 90.0) Then
          Az = 180.0    ! handle arbitrary case
        Else
          cosaz = (sin(solar_elevation*DEGREES_TO_RADIANS)*sin(rlatitude) - &
                  sin(solar_declination)) / &
                 (cos(solar_elevation*DEGREES_TO_RADIANS)*cos(rlatitude))

          If(cosaz .lt. -1.0) cosaz = -1.0
          If(cosaz .gt.  1.0) cosaz =  1.0

          Az = acos(cosaz)*one_eighty_over_pi

!	  The above formula produces azimuth positive east, zero south.
!	  We want positive east, zero north.

!	
          If (Az .ge. 0.0) Then
              Az = 180.0 - Az
          Else
              Az = -180.0 + Az
          EndIf

          If(hour_angle .lt. 180.0) Az = - Az
          If(Az .lt. 0) Az = 360.0 + Az

        EndIf

   end Subroutine Solar_ZA_Az

End MODULE Zeeman_Utility
