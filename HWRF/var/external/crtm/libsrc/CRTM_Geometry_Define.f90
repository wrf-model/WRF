!
! CRTM_Geometry_Define
!
! Module defining the CRTM Geometry data structure.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 18-Nov-2009
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_Geometry_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds            , ONLY: fp
  USE Message_Handler       , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Date_Utility          , ONLY: Days_in_Month
  USE Compare_Float_Numbers , ONLY: OPERATOR(.EqualTo.)
  USE CRTM_Parameters       , ONLY: ZERO, &
                                    MIN_SURFACE_ALTITUDE    , &
                                    MAX_SURFACE_ALTITUDE    , &
                                    MAX_SENSOR_SCAN_ANGLE   , &
                                    MAX_SENSOR_ZENITH_ANGLE , &
                                    MAX_SENSOR_AZIMUTH_ANGLE, &
                                    MAX_SOURCE_ZENITH_ANGLE , &
                                    MAX_SOURCE_AZIMUTH_ANGLE, &
                                    MAX_FLUX_ZENITH_ANGLE   , &
                                    DIFFUSIVITY_ANGLE       
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Geometry enitities
  ! ...Structures
  PUBLIC :: CRTM_Geometry_type
  ! ...Procedures
  PUBLIC :: CRTM_Geometry_Destroy
  PUBLIC :: CRTM_Geometry_SetValue
  PUBLIC :: CRTM_Geometry_GetValue
  PUBLIC :: CRTM_Geometry_IsValid
  PUBLIC :: CRTM_Geometry_Inspect
  PUBLIC :: CRTM_Geometry_DefineVersion


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CRTM_Geometry_Equal
  END INTERFACE OPERATOR(==)

  
  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*),  PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_Geometry_Define.f90 6881 2010-03-05 23:33:01Z paul.vandelst@noaa.gov $'
  ! Maximum message length
  INTEGER, PARAMETER :: ML=256
  ! Invalid date values
  INTEGER, PARAMETER :: MIN_YEAR = 1960  ! Vanguard 2, was launched on February 17, 1959
  
  
  ! ---------------------------------
  ! Geometry data type definition
  ! ---------------------------------
  !:tdoc+:
  TYPE :: CRTM_Geometry_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .TRUE.  ! Placeholder for future expansion
    ! Field of view index (1-nFOV)
    INTEGER  :: iFOV = 0
    ! Earth location
    REAL(fp) :: Longitude        = ZERO
    REAL(fp) :: Latitude         = ZERO
    REAL(fp) :: Surface_Altitude = ZERO
    ! Sensor angle information
    REAL(fp) :: Sensor_Scan_Angle    = ZERO
    REAL(fp) :: Sensor_Zenith_Angle  = ZERO
    REAL(fp) :: Sensor_Azimuth_Angle = ZERO 
    ! Source angle information
    REAL(fp) :: Source_Zenith_Angle  = 100.0_fp  ! Below horizon
    REAL(fp) :: Source_Azimuth_Angle = ZERO
    ! Flux angle information
    REAL(fp) :: Flux_Zenith_Angle = DIFFUSIVITY_ANGLE
    ! Date for geometry calculations
    INTEGER :: Year  = 2001
    INTEGER :: Month = 1
    INTEGER :: Day   = 1
  END TYPE CRTM_Geometry_type
  !:tdoc-:


CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                           ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Geometry_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize CRTM Geometry objects.
!
! CALLING SEQUENCE:
!       CALL CRTM_Geometry_Destroy( geo )
!
! OBJECTS:
!       geo:          Re-initialized Geometry structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Geometry_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_Geometry_Destroy( geo )
    TYPE(CRTM_Geometry_type), INTENT(OUT) :: geo
    geo%Is_Allocated = .TRUE.  ! Placeholder for future expansion
  END SUBROUTINE CRTM_Geometry_Destroy
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Geometry_SetValue
! 
! PURPOSE:
!       Elemental subroutine to set the values of CRTM Geometry
!       object components.
!
! CALLING SEQUENCE:
!       CALL CRTM_Geometry_SetValue( geo, &
!                                    iFOV                 = iFOV                , &
!                                    Longitude            = Longitude           , &
!                                    Latitude             = Latitude            , &
!                                    Surface_Altitude     = Surface_Altitude    , &
!                                    Sensor_Scan_Angle    = Sensor_Scan_Angle   , &
!                                    Sensor_Zenith_Angle  = Sensor_Zenith_Angle , &
!                                    Sensor_Azimuth_Angle = Sensor_Azimuth_Angle, &
!                                    Source_Zenith_Angle  = Source_Zenith_Angle , &
!                                    Source_Azimuth_Angle = Source_Azimuth_Angle, &
!                                    Flux_Zenith_Angle    = Flux_Zenith_Angle   , &
!                                    Year                 = Year                , &
!                                    Month                = Month               , &
!                                    Day                  = Day                   )
!
! OBJECTS:
!       geo:                  Geometry object for which component values
!                             are to be set.
!                             UNITS:      N/A
!                             TYPE:       CRTM_Geometry_type
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       iFOV:                 Sensor field-of-view index.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Longitude:            Earth longitude
!                             UNITS:      degrees East (0->360) 
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Latitude:             Earth latitude.
!                             UNITS:      degrees North (-90->+90)
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Surface_Altitude:     Altitude of the Earth's surface at the specifed
!                             lon/lat location.
!                             UNITS:      metres (m)
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Sensor_Scan_Angle:    The sensor scan angle from nadir.
!                             UNITS:      degrees
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Sensor_Zenith_Angle:  The zenith angle from the field-of-view
!                             to the sensor.
!                             UNITS:      degrees
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Sensor_Azimuth_Angle: The azimuth angle subtended by the horizontal
!                             projection of a direct line from the satellite
!                             to the FOV and the North-South axis measured
!                             clockwise from North.
!                             UNITS:      degrees from North (0->360) 
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Source_Zenith_Angle:  The zenith angle from the field-of-view
!                             to a source (sun or moon).
!                             UNITS:      degrees
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Source_Azimuth_Angle: The azimuth angle subtended by the horizontal
!                             projection of a direct line from the source
!                             to the FOV and the North-South axis measured
!                             clockwise from North.
!                             UNITS:      degrees from North (0->360) 
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Flux_Zenith_Angle:    The zenith angle used to approximate downwelling
!                             flux transmissivity
!                             UNITS:      degrees
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Year:                 The year in 4-digit format, e.g. 1997.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Month:                The month of the year (1-12).
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Day:                  The day of the month (1-28/29/30/31).
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_Geometry_SetValue( &
    geo                 , &  ! In/Output
    iFOV                , &  ! Optional input
    Longitude           , &  ! Optional input
    Latitude            , &  ! Optional input
    Surface_Altitude    , &  ! Optional input
    Sensor_Scan_Angle   , &  ! Optional input
    Sensor_Zenith_Angle , &  ! Optional input
    Sensor_Azimuth_Angle, &  ! Optional input
    Source_Zenith_Angle , &  ! Optional input
    Source_Azimuth_Angle, &  ! Optional input
    Flux_Zenith_Angle   , &  ! Optional input
    Year                , &  ! Optional input
    Month               , &  ! Optional input
    Day                   )  ! Optional input
    ! Arguments
    TYPE(CRTM_Geometry_type), INTENT(IN OUT) :: geo
    INTEGER ,       OPTIONAL, INTENT(IN)     :: iFOV
    REAL(fp),       OPTIONAL, INTENT(IN)     :: Longitude
    REAL(fp),       OPTIONAL, INTENT(IN)     :: Latitude
    REAL(fp),       OPTIONAL, INTENT(IN)     :: Surface_Altitude
    REAL(fp),       OPTIONAL, INTENT(IN)     :: Sensor_Scan_Angle
    REAL(fp),       OPTIONAL, INTENT(IN)     :: Sensor_Zenith_Angle
    REAL(fp),       OPTIONAL, INTENT(IN)     :: Sensor_Azimuth_Angle
    REAL(fp),       OPTIONAL, INTENT(IN)     :: Source_Zenith_Angle
    REAL(fp),       OPTIONAL, INTENT(IN)     :: Source_Azimuth_Angle
    REAL(fp),       OPTIONAL, INTENT(IN)     :: Flux_Zenith_Angle
    INTEGER,        OPTIONAL, INTENT(IN)     :: Year 
    INTEGER,        OPTIONAL, INTENT(IN)     :: Month
    INTEGER,        OPTIONAL, INTENT(IN)     :: Day  
    
    ! Set values
    IF ( PRESENT(iFOV                ) ) geo%iFOV = iFOV
    IF ( PRESENT(Longitude           ) ) geo%Longitude            = Longitude
    IF ( PRESENT(Latitude            ) ) geo%Latitude             = Latitude
    IF ( PRESENT(Surface_Altitude    ) ) geo%Surface_Altitude     = Surface_Altitude
    IF ( PRESENT(Sensor_Scan_Angle   ) ) geo%Sensor_Scan_Angle    = Sensor_Scan_Angle
    IF ( PRESENT(Sensor_Zenith_Angle ) ) geo%Sensor_Zenith_Angle  = Sensor_Zenith_Angle
    IF ( PRESENT(Sensor_Azimuth_Angle) ) geo%Sensor_Azimuth_Angle = Sensor_Azimuth_Angle
    IF ( PRESENT(Source_Zenith_Angle ) ) geo%Source_Zenith_Angle  = Source_Zenith_Angle
    IF ( PRESENT(Source_Azimuth_Angle) ) geo%Source_Azimuth_Angle = Source_Azimuth_Angle
    IF ( PRESENT(Flux_Zenith_Angle   ) ) geo%Flux_Zenith_Angle    = Flux_Zenith_Angle
    IF ( PRESENT(Year                ) ) geo%Year                 = Year 
    IF ( PRESENT(Month               ) ) geo%Month                = Month
    IF ( PRESENT(Day                 ) ) geo%Day                  = Day  
  
  END SUBROUTINE CRTM_Geometry_SetValue


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Geometry_GetValue
! 
! PURPOSE:
!       Elemental subroutine to get the values of CRTM Geometry
!       object components.
!
! CALLING SEQUENCE:
!       CALL CRTM_Geometry_GetValue( geo, &
!                                    iFOV                 = iFOV                , &
!                                    Longitude            = Longitude           , &
!                                    Latitude             = Latitude            , &
!                                    Surface_Altitude     = Surface_Altitude    , &
!                                    Sensor_Scan_Angle    = Sensor_Scan_Angle   , &
!                                    Sensor_Zenith_Angle  = Sensor_Zenith_Angle , &
!                                    Sensor_Azimuth_Angle = Sensor_Azimuth_Angle, &
!                                    Source_Zenith_Angle  = Source_Zenith_Angle , &
!                                    Source_Azimuth_Angle = Source_Azimuth_Angle, &
!                                    Flux_Zenith_Angle    = Flux_Zenith_Angle   , &
!                                    Year                 = Year                , &
!                                    Month                = Month               , &
!                                    Day                  = Day                   )
!
! OBJECTS:
!       geo:                  Geometry object from which component values
!                             are to be retrieved.
!                             UNITS:      N/A
!                             TYPE:       CRTM_Geometry_type
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL OUTPUTS:
!       iFOV:                 Sensor field-of-view index.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Longitude:            Earth longitude
!                             UNITS:      degrees East (0->360) 
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Latitude:             Earth latitude.
!                             UNITS:      degrees North (-90->+90)
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Surface_Altitude:     Altitude of the Earth's surface at the specifed
!                             lon/lat location.
!                             UNITS:      metres (m)
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Sensor_Scan_Angle:    The sensor scan angle from nadir.
!                             UNITS:      degrees
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Sensor_Zenith_Angle:  The zenith angle from the field-of-view
!                             to the sensor.
!                             UNITS:      degrees
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Sensor_Azimuth_Angle: The azimuth angle subtended by the horizontal
!                             projection of a direct line from the satellite
!                             to the FOV and the North-South axis measured
!                             clockwise from North.
!                             UNITS:      degrees from North (0->360) 
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Source_Zenith_Angle:  The zenith angle from the field-of-view
!                             to a source (sun or moon).
!                             UNITS:      degrees
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Source_Azimuth_Angle: The azimuth angle subtended by the horizontal
!                             projection of a direct line from the source
!                             to the FOV and the North-South axis measured
!                             clockwise from North.
!                             UNITS:      degrees from North (0->360) 
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Flux_Zenith_Angle:    The zenith angle used to approximate downwelling
!                             flux transmissivity
!                             UNITS:      degrees
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Year:                 The year in 4-digit format, e.g. 1997.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Month:                The month of the year (1-12).
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Day:                  The day of the month (1-28/29/30/31).
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar or same as geo input
!                             ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_Geometry_GetValue( &
    geo                 , &  ! Input
    iFOV                , &  ! Optional output
    Longitude           , &  ! Optional output
    Latitude            , &  ! Optional output
    Surface_Altitude    , &  ! Optional output
    Sensor_Scan_Angle   , &  ! Optional output
    Sensor_Zenith_Angle , &  ! Optional output
    Sensor_Azimuth_Angle, &  ! Optional output
    Source_Zenith_Angle , &  ! Optional output
    Source_Azimuth_Angle, &  ! Optional output
    Flux_Zenith_Angle   , &  ! Optional output
    Year                , &  ! Optional output
    Month               , &  ! Optional output
    Day                   )  ! Optional output
    ! Arguments
    TYPE(CRTM_Geometry_type), INTENT(IN)  :: geo
    INTEGER ,       OPTIONAL, INTENT(OUT) :: iFOV
    REAL(fp),       OPTIONAL, INTENT(OUT) :: Longitude
    REAL(fp),       OPTIONAL, INTENT(OUT) :: Latitude
    REAL(fp),       OPTIONAL, INTENT(OUT) :: Surface_Altitude
    REAL(fp),       OPTIONAL, INTENT(OUT) :: Sensor_Scan_Angle
    REAL(fp),       OPTIONAL, INTENT(OUT) :: Sensor_Zenith_Angle
    REAL(fp),       OPTIONAL, INTENT(OUT) :: Sensor_Azimuth_Angle
    REAL(fp),       OPTIONAL, INTENT(OUT) :: Source_Zenith_Angle
    REAL(fp),       OPTIONAL, INTENT(OUT) :: Source_Azimuth_Angle
    REAL(fp),       OPTIONAL, INTENT(OUT) :: Flux_Zenith_Angle
    INTEGER,        OPTIONAL, INTENT(OUT) :: Year 
    INTEGER,        OPTIONAL, INTENT(OUT) :: Month
    INTEGER,        OPTIONAL, INTENT(OUT) :: Day  
    
    ! Get values
    IF ( PRESENT(iFOV                ) ) iFOV                 = geo%iFOV
    IF ( PRESENT(Longitude           ) ) Longitude            = geo%Longitude
    IF ( PRESENT(Latitude            ) ) Latitude             = geo%Latitude
    IF ( PRESENT(Surface_Altitude    ) ) Surface_Altitude     = geo%Surface_Altitude
    IF ( PRESENT(Sensor_Scan_Angle   ) ) Sensor_Scan_Angle    = geo%Sensor_Scan_Angle
    IF ( PRESENT(Sensor_Zenith_Angle ) ) Sensor_Zenith_Angle  = geo%Sensor_Zenith_Angle
    IF ( PRESENT(Sensor_Azimuth_Angle) ) Sensor_Azimuth_Angle = geo%Sensor_Azimuth_Angle
    IF ( PRESENT(Source_Zenith_Angle ) ) Source_Zenith_Angle  = geo%Source_Zenith_Angle
    IF ( PRESENT(Source_Azimuth_Angle) ) Source_Azimuth_Angle = geo%Source_Azimuth_Angle
    IF ( PRESENT(Flux_Zenith_Angle   ) ) Flux_Zenith_Angle    = geo%Flux_Zenith_Angle
    IF ( PRESENT(Year                ) ) Year                 = geo%Year 
    IF ( PRESENT(Month               ) ) Month                = geo%Month
    IF ( PRESENT(Day                 ) ) Day                  = geo%Day  
  
  END SUBROUTINE CRTM_Geometry_GetValue


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Geometry_IsValid
!
! PURPOSE:
!       Non-pure function to perform some simple validity checks on a
!       CRTM Geometry object. 
!
!       If invalid data is found, a message is printed to stdout.
!
! CALLING SEQUENCE:
!       result = CRTM_Geometry_IsValid( geo )
!
!         or
!
!       IF ( CRTM_Geometry_IsValid( geo ) ) THEN....
!
! OBJECTS:
!       geo:       CRTM Geometry object which is to have its
!                  contents checked.
!                  UNITS:      N/A
!                  TYPE:       CRTM_Geometry_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:    Logical variable indicating whether or not the input
!                  passed the check.
!                  If == .FALSE., Geometry object is unused or contains
!                                 invalid data.
!                     == .TRUE.,  Geometry object can be used in CRTM.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Geometry_IsValid( geo ) RESULT( IsValid )
    TYPE(CRTM_Geometry_type), INTENT(IN) :: geo
    LOGICAL :: IsValid
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Geometry_IsValid'
    CHARACTER(ML) :: msg
    
    ! Setup
    IsValid = .TRUE.
    
    ! Field of view index (1-nFOV)
    IF ( geo%iFOV < 0 ) THEN
      msg = 'Invalid FOV index. Must be > 0.'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF

    ! Earth location
    IF ( geo%Longitude < ZERO .OR. geo%Longitude > 360.0_fp ) THEN
      msg = 'Invalid longitude. Must be degrees East (0->360)'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF
    IF ( geo%Latitude < -90.0_fp .OR. geo%Latitude > 90.0_fp ) THEN
      msg = 'Invalid latitude. Must be degrees North (-90->+90)'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF
    IF ( geo%Surface_Altitude < MIN_SURFACE_ALTITUDE .OR. &
         geo%Surface_Altitude > MAX_SURFACE_ALTITUDE      ) THEN
      WRITE(msg,'("Invalid surface altitude. Must be metres (",f6.1,"->+",f6.1,")")') &
            MIN_SURFACE_ALTITUDE, MAX_SURFACE_ALTITUDE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF
    
    ! Sensor angles
    IF ( ABS(geo%Sensor_Scan_Angle) > MAX_SENSOR_SCAN_ANGLE ) THEN
      WRITE(msg,'("Invalid sensor scan angle. Must be |thetas(i)|<=",f4.1)') &
            MAX_SENSOR_SCAN_ANGLE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF
    IF ( ABS(geo%Sensor_Zenith_Angle) > MAX_SENSOR_ZENITH_ANGLE ) THEN
      WRITE(msg,'("Invalid sensor zenith angle. Must be |thetaz(i)|<=",f4.1)') &
            MAX_SENSOR_ZENITH_ANGLE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF
    IF ( geo%Sensor_Azimuth_Angle < ZERO .OR. &
         geo%Sensor_Azimuth_Angle > MAX_SENSOR_AZIMUTH_ANGLE ) THEN
      WRITE(msg,'("Invalid sensor azimuth angle. Must be 0<=phi(i)<=",f5.1)') &
            MAX_SENSOR_AZIMUTH_ANGLE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF
    
    ! Source angle information
    IF ( ABS(geo%Source_Zenith_Angle) > 180.0_fp ) THEN
      msg = 'Invalid source zenith angle. Must be |thetaz(s)|<=180.0'
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF
    IF ( geo%Source_Azimuth_Angle < ZERO .OR. &
         geo%Source_Azimuth_Angle > MAX_SOURCE_AZIMUTH_ANGLE ) THEN
      WRITE(msg,'("Invalid source azimuth angle. Must be 0<=phi(s)<=",f5.1)') &
            MAX_SOURCE_AZIMUTH_ANGLE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF

    ! Flux angle information
    IF ( ABS(geo%Flux_Zenith_Angle) > MAX_FLUX_ZENITH_ANGLE ) THEN
      WRITE(msg,'("Invalid flux zenith angle. Must be |thetaz(f)|<=",f4.1)') &
            MAX_FLUX_ZENITH_ANGLE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF

    ! Date information
    IF ( geo%Year < MIN_YEAR ) THEN
      WRITE(msg,'("Invalid year. Must be > ",i0)') MIN_YEAR
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
      IsValid = .FALSE.
    END IF
    IF ( geo%Month < 1 .OR. geo%Month > 12 ) THEN
      CALL Display_Message( ROUTINE_NAME, 'Invalid month-of-year.', INFORMATION )
      IsValid = .FALSE.
    END IF
    ! ...Only test Day value if Month and Year are valid
    IF ( IsValid ) THEN
      IF ( geo%Day < 1 .OR. geo%Day > Days_in_Month(geo%Month,geo%Year) ) THEN
        CALL Display_Message( ROUTINE_NAME, 'Invalid day-of-month.', INFORMATION )
        IsValid = .FALSE.
      END IF
    END IF

  END FUNCTION CRTM_Geometry_IsValid


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Geometry_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a CRTM Geometry object to stdout.
!
! CALLING SEQUENCE:
!       CALL CRTM_Geometry_Inspect( geo )
!
! INPUTS:
!       geo:  CRTM Geometry object to display.
!             UNITS:      N/A
!             TYPE:       CRTM_Geometry_type
!             DIMENSION:  Scalar
!             ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Geometry_Inspect( geo )
    TYPE(CRTM_Geometry_type), INTENT(IN) :: geo
    CHARACTER(*), PARAMETER :: RFMT = 'es13.6'
    
    WRITE(*, '(1x,"Geometry OBJECT")')
    ! Field of view index
    WRITE(*, '(3x,"FOV index           :",1x,i0)') geo%iFOV
    ! Earth location
    WRITE(*, '(3x,"Longitude           :",1x,'//RFMT//')') geo%Longitude       
    WRITE(*, '(3x,"Latitude            :",1x,'//RFMT//')') geo%Latitude        
    WRITE(*, '(3x,"Surface altitude    :",1x,'//RFMT//')') geo%Surface_Altitude
    ! Sensor angle information
    WRITE(*, '(3x,"Sensor scan angle   :",1x,'//RFMT//')') geo%Sensor_Scan_Angle   
    WRITE(*, '(3x,"Sensor zenith angle :",1x,'//RFMT//')') geo%Sensor_Zenith_Angle 
    WRITE(*, '(3x,"Sensor azimuth angle:",1x,'//RFMT//')') geo%Sensor_Azimuth_Angle
    ! Source angle information
    WRITE(*, '(3x,"Source zenith angle :",1x,'//RFMT//')') geo%Source_Zenith_Angle 
    WRITE(*, '(3x,"Source azimuth angle:",1x,'//RFMT//')') geo%Source_Azimuth_Angle
    ! Flux angle information
    WRITE(*, '(3x,"Flux zenith angle   :",1x,'//RFMT//')') geo%Flux_Zenith_Angle
    ! Date information
    WRITE(*, '(3x,"Year                :",1x,i4)') geo%Year 
    WRITE(*, '(3x,"Month               :",1x,i4)') geo%Month
    WRITE(*, '(3x,"Day                 :",1x,i4)') geo%Day  

  END SUBROUTINE CRTM_Geometry_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Geometry_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_Geometry_DefineVersion( Id )
!
! OUTPUT ARGUMENTS:
!       Id:            Character string containing the version Id information
!                      for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Geometry_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_Geometry_DefineVersion



!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Geometry_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two CRTM_Geometry objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = CRTM_Geometry_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two CRTM Geometry objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Geometry_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       is_equal:      Logical value indicating whether the inputs are equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_Geometry_Equal( x, y ) RESULT( is_equal )
    TYPE(CRTM_Geometry_type) , INTENT(IN)  :: x, y
    LOGICAL :: is_equal

    is_equal = ( (x%iFOV                    ==     y%iFOV                ) .AND. &
                 (x%Longitude            .EqualTo. y%Longitude           ) .AND. &
                 (x%Latitude             .EqualTo. y%Latitude            ) .AND. &
                 (x%Surface_Altitude     .EqualTo. y%Surface_Altitude    ) .AND. &
                 (x%Sensor_Scan_Angle    .EqualTo. y%Sensor_Scan_Angle   ) .AND. &
                 (x%Sensor_Zenith_Angle  .EqualTo. y%Sensor_Zenith_Angle ) .AND. &
                 (x%Sensor_Azimuth_Angle .EqualTo. y%Sensor_Azimuth_Angle) .AND. &
                 (x%Source_Zenith_Angle  .EqualTo. y%Source_Zenith_Angle ) .AND. &
                 (x%Source_Azimuth_Angle .EqualTo. y%Source_Azimuth_Angle) .AND. &
                 (x%Flux_Zenith_Angle    .EqualTo. y%Flux_Zenith_Angle   ) .AND. &
                 (x%Year  == y%Year ) .AND. &
                 (x%Month == y%Month) .AND. &
                 (x%Day   == y%Day  )       )
    
  END FUNCTION CRTM_Geometry_Equal

END MODULE CRTM_Geometry_Define
