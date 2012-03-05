!
! CRTM_GeometryInfo_Define
!
! Module defining the CRTM GeometryInfo container object.
!
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 19-May-2004
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_GeometryInfo_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,            ONLY: fp
  USE Message_Handler,       ONLY: SUCCESS, WARNING, FAILURE, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  USE CRTM_Parameters,       ONLY: ZERO, ONE, SET          , &
                                   EARTH_RADIUS            , &
                                   SATELLITE_HEIGHT        , &
                                   DIFFUSIVITY_RADIAN      , &
                                   SECANT_DIFFUSIVITY
  USE CRTM_Geometry_Define,  ONLY: CRTM_Geometry_type, &
                                   OPERATOR(==), &
                                   CRTM_Geometry_SetValue, &
                                   CRTM_Geometry_GetValue, &
                                   CRTM_Geometry_IsValid, &
                                   CRTM_Geometry_Inspect
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Geometry entities
  ! ...Structures
  PUBLIC :: CRTM_Geometry_type
  ! GeometryInfo enitities
  ! ...Structures
  PUBLIC :: CRTM_GeometryInfo_type
  ! ...Procedures
  PUBLIC :: CRTM_GeometryInfo_Destroy
  PUBLIC :: CRTM_GeometryInfo_SetValue
  PUBLIC :: CRTM_GeometryInfo_GetValue
  PUBLIC :: CRTM_GeometryInfo_IsValid
  PUBLIC :: CRTM_GeometryInfo_Inspect
  PUBLIC :: CRTM_GeometryInfo_DefineVersion


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CRTM_GeometryInfo_Equal
  END INTERFACE OPERATOR(==)

  
  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*),  PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_GeometryInfo_Define.f90 6787 2010-02-26 20:23:06Z yong.han@noaa.gov $'
  ! Maximum message length
  INTEGER, PARAMETER :: ML=256


  ! ---------------------------------
  ! GeometryInfo data type definition
  ! ---------------------------------
  !:tdoc+:
  TYPE :: CRTM_GeometryInfo_type
    ! Structure for user Input
    TYPE(CRTM_Geometry_type) :: user
    ! Derived from User Input
    ! ...Default distance ratio
    REAL(fp) :: Distance_Ratio = EARTH_RADIUS/(EARTH_RADIUS + SATELLITE_HEIGHT)
    ! ...Sensor angle information
    REAL(fp) :: Sensor_Scan_Radian    = ZERO
    REAL(fp) :: Sensor_Zenith_Radian  = ZERO
    REAL(fp) :: Sensor_Azimuth_Radian = ZERO
    REAL(fp) :: Secant_Sensor_Zenith  = ZERO
    ! .... Zenith angle used in the transmittance algorithms 
    REAL(fp) :: Trans_Zenith_Radian  = ZERO
    REAL(fp) :: Secant_Trans_Zenith  = ZERO
    
    ! ...Source angle information
    REAL(fp) :: Source_Zenith_Radian  = ZERO
    REAL(fp) :: Source_Azimuth_Radian = ZERO
    REAL(fp) :: Secant_Source_Zenith  = ZERO
    ! ...Flux angle information
    REAL(fp) :: Flux_Zenith_Radian = DIFFUSIVITY_RADIAN
    REAL(fp) :: Secant_Flux_Zenith = SECANT_DIFFUSIVITY
    ! ...Square of ratio between mean and actual sun-earth (AU) distances
    REAL(fp) :: AU_ratio2 = ONE
  END TYPE CRTM_GeometryInfo_type
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
!       CRTM_GeometryInfo_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize a CRTM GeometryInfo objects.
!
! CALLING SEQUENCE:
!       CALL CRTM_GeometryInfo_Destroy( gInfo )
!
! OBJECTS:
!       gInfo:        Re-initialized GeometryInfo structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_GeometryInfo_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_GeometryInfo_Destroy( gInfo )
    TYPE(CRTM_GeometryInfo_type), INTENT(OUT) :: gInfo
  END SUBROUTINE CRTM_GeometryInfo_Destroy
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_GeometryInfo_SetValue
! 
! PURPOSE:
!       Elemental subroutine to set the values of CRTM GeometryInfo
!       object components.
!
! CALLING SEQUENCE:
!       CALL CRTM_GeometryInfo_SetValue( gInfo, &
!                                        Geometry              = Geometry             , &
!                                        iFOV                  = iFOV                 , &
!                                        Longitude             = Longitude            , &
!                                        Latitude              = Latitude             , &
!                                        Surface_Altitude      = Surface_Altitude     , &
!                                        Sensor_Scan_Angle     = Sensor_Scan_Angle    , &
!                                        Sensor_Zenith_Angle   = Sensor_Zenith_Angle  , &
!                                        Sensor_Azimuth_Angle  = Sensor_Azimuth_Angle , &
!                                        Source_Zenith_Angle   = Source_Zenith_Angle  , &
!                                        Source_Azimuth_Angle  = Source_Azimuth_Angle , &
!                                        Flux_Zenith_Angle     = Flux_Zenith_Angle    , &
!                                        Year                  = Year                 , &
!                                        Month                 = Month                , &
!                                        Day                   = Day                  , &
!                                        Distance_Ratio        = Distance_Ratio       , &
!                                        Sensor_Scan_Radian    = Sensor_Scan_Radian   , &
!                                        Sensor_Zenith_Radian  = Sensor_Zenith_Radian , &
!                                        Sensor_Azimuth_Radian = Sensor_Azimuth_Radian, &
!                                        Secant_Sensor_Zenith  = Secant_Sensor_Zenith , &
!                                        Source_Zenith_Radian  = Source_Zenith_Radian , &
!                                        Source_Azimuth_Radian = Source_Azimuth_Radian, &
!                                        Secant_Source_Zenith  = Secant_Source_Zenith , &
!                                        Flux_Zenith_Radian    = Flux_Zenith_Radian   , &
!                                        Secant_Flux_Zenith    = Secant_Flux_Zenith   , &
!                                        Trans_Zenith_Radian   = Trans_Zenith_Radian  , &
!                                        Secant_Trans_Zenith   = Secant_Trans_Zenith  , &
!                                        AU_ratio2             = AU_ratio2              )
!
! OBJECTS:
!       gInfo:                GeometryInfo object from which component values
!                             are to be retrieved.
!                             UNITS:      N/A
!                             TYPE:       CRTM_Geometry_type
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Geometry:             Geometry object.
!                             UNITS:      N/A
!                             TYPE:       CRTM_Geometry_type
!                             DIMENSION:  Scalar or same as gInfo input
!                             ATTRIBUTES: INTENT(IN)
!
!       All other gInfo components as listed in the calling sequence.
!       NOTE: If the Geometry argument as well as any of the arguments iFOV to
!             Flux_Zenith_Angle are specified, the latter values override any
!             contained in the passed Geometry object.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_GeometryInfo_SetValue( &
    gInfo                , &  ! Input
    Geometry             , &  ! Optional input
    iFOV                 , &  ! Optional input
    Longitude            , &  ! Optional input
    Latitude             , &  ! Optional input
    Surface_Altitude     , &  ! Optional input
    Sensor_Scan_Angle    , &  ! Optional input
    Sensor_Zenith_Angle  , &  ! Optional input
    Sensor_Azimuth_Angle , &  ! Optional input
    Source_Zenith_Angle  , &  ! Optional input
    Source_Azimuth_Angle , &  ! Optional input
    Flux_Zenith_Angle    , &  ! Optional input
    Year                 , &  ! Optional input
    Month                , &  ! Optional input
    Day                  , &  ! Optional input
    Distance_Ratio       , &  ! Optional input
    Sensor_Scan_Radian   , &  ! Optional input
    Sensor_Zenith_Radian , &  ! Optional input
    Sensor_Azimuth_Radian, &  ! Optional input
    Secant_Sensor_Zenith , &  ! Optional input
    Source_Zenith_Radian , &  ! Optional input
    Source_Azimuth_Radian, &  ! Optional input
    Secant_Source_Zenith , &  ! Optional input
    Flux_Zenith_Radian   , &  ! Optional input
    Secant_Flux_Zenith   , &  ! Optional input
    Trans_Zenith_Radian  , &  ! Optional input
    Secant_Trans_Zenith  , &  ! Optional input
    AU_ratio2              )  ! Optional input
    ! Arguments
    TYPE(CRTM_GeometryInfo_type),       INTENT(IN OUT) :: gInfo
    TYPE(CRTM_Geometry_type), OPTIONAL, INTENT(IN)     :: Geometry
    INTEGER ,                 OPTIONAL, INTENT(IN)     :: iFOV
    REAL(fp),                 OPTIONAL, INTENT(IN)     :: Longitude
    REAL(fp),                 OPTIONAL, INTENT(IN)     :: Latitude
    REAL(fp),                 OPTIONAL, INTENT(IN)     :: Surface_Altitude
    REAL(fp),                 OPTIONAL, INTENT(IN)     :: Sensor_Scan_Angle
    REAL(fp),                 OPTIONAL, INTENT(IN)     :: Sensor_Zenith_Angle
    REAL(fp),                 OPTIONAL, INTENT(IN)     :: Sensor_Azimuth_Angle
    REAL(fp),                 OPTIONAL, INTENT(IN)     :: Source_Zenith_Angle
    REAL(fp),                 OPTIONAL, INTENT(IN)     :: Source_Azimuth_Angle
    REAL(fp),                 OPTIONAL, INTENT(IN)     :: Flux_Zenith_Angle
    INTEGER,                  OPTIONAL, INTENT(IN)     :: Year 
    INTEGER,                  OPTIONAL, INTENT(IN)     :: Month
    INTEGER,                  OPTIONAL, INTENT(IN)     :: Day  
    REAL(fp),                 OPTIONAL, INTENT(IN)     :: Distance_Ratio       
    REAL(fp),                 OPTIONAL, INTENT(IN)     :: Sensor_Scan_Radian   
    REAL(fp),                 OPTIONAL, INTENT(IN)     :: Sensor_Zenith_Radian 
    REAL(fp),                 OPTIONAL, INTENT(IN)     :: Sensor_Azimuth_Radian
    REAL(fp),                 OPTIONAL, INTENT(IN)     :: Secant_Sensor_Zenith 
    REAL(fp),                 OPTIONAL, INTENT(IN)     :: Source_Zenith_Radian 
    REAL(fp),                 OPTIONAL, INTENT(IN)     :: Source_Azimuth_Radian
    REAL(fp),                 OPTIONAL, INTENT(IN)     :: Secant_Source_Zenith 
    REAL(fp),                 OPTIONAL, INTENT(IN)     :: Flux_Zenith_Radian   
    REAL(fp),                 OPTIONAL, INTENT(IN)     :: Secant_Flux_Zenith   
    REAL(fp),                 OPTIONAL, INTENT(IN)     :: Trans_Zenith_Radian   
    REAL(fp),                 OPTIONAL, INTENT(IN)     :: Secant_Trans_Zenith   
    REAL(fp),                 OPTIONAL, INTENT(IN)     :: AU_ratio2   
    
    ! Get values
    IF ( PRESENT(Geometry) ) gInfo%user = Geometry
    CALL CRTM_Geometry_SetValue( gInfo%user, &
                                 iFOV                 = iFOV                , &
                                 Longitude            = Longitude           , &
                                 Latitude             = Latitude            , &
                                 Surface_Altitude     = Surface_Altitude    , &
                                 Sensor_Scan_Angle    = Sensor_Scan_Angle   , &
                                 Sensor_Zenith_Angle  = Sensor_Zenith_Angle , &
                                 Sensor_Azimuth_Angle = Sensor_Azimuth_Angle, &
                                 Source_Zenith_Angle  = Source_Zenith_Angle , &
                                 Source_Azimuth_Angle = Source_Azimuth_Angle, &
                                 Flux_Zenith_Angle    = Flux_Zenith_Angle   , &
                                 Year                 = Year                , &
                                 Month                = Month               , &
                                 Day                  = Day                   )
                                 
    IF ( PRESENT(Distance_Ratio       ) ) gInfo%Distance_Ratio        = Distance_Ratio       
    IF ( PRESENT(Sensor_Scan_Radian   ) ) gInfo%Sensor_Scan_Radian    = Sensor_Scan_Radian   
    IF ( PRESENT(Sensor_Zenith_Radian ) ) gInfo%Sensor_Zenith_Radian  = Sensor_Zenith_Radian 
    IF ( PRESENT(Sensor_Azimuth_Radian) ) gInfo%Sensor_Azimuth_Radian = Sensor_Azimuth_Radian
    IF ( PRESENT(Secant_Sensor_Zenith ) ) gInfo%Secant_Sensor_Zenith  = Secant_Sensor_Zenith 
    IF ( PRESENT(Source_Zenith_Radian ) ) gInfo%Source_Zenith_Radian  = Source_Zenith_Radian 
    IF ( PRESENT(Source_Azimuth_Radian) ) gInfo%Source_Azimuth_Radian = Source_Azimuth_Radian
    IF ( PRESENT(Secant_Source_Zenith ) ) gInfo%Secant_Source_Zenith  = Secant_Source_Zenith 
    IF ( PRESENT(Flux_Zenith_Radian   ) ) gInfo%Flux_Zenith_Radian    = Flux_Zenith_Radian   
    IF ( PRESENT(Secant_Flux_Zenith   ) ) gInfo%Secant_Flux_Zenith    = Secant_Flux_Zenith   
    IF ( PRESENT(Trans_Zenith_Radian  ) ) gInfo%Trans_Zenith_Radian   = Trans_Zenith_Radian   
    IF ( PRESENT(Secant_Trans_Zenith  ) ) gInfo%Secant_Trans_Zenith   = Secant_Trans_Zenith   
    IF ( PRESENT(AU_ratio2            ) ) gInfo%AU_ratio2             = AU_ratio2   
  
  END SUBROUTINE CRTM_GeometryInfo_SetValue


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_GeometryInfo_GetValue
! 
! PURPOSE:
!       Elemental subroutine to get the values of CRTM GeometryInfo
!       object components.
!
! CALLING SEQUENCE:
!       CALL CRTM_GeometryInfo_GetValue( gInfo, &
!                                        Geometry              = Geometry             , &
!                                        iFOV                  = iFOV                 , &
!                                        Longitude             = Longitude            , &
!                                        Latitude              = Latitude             , &
!                                        Surface_Altitude      = Surface_Altitude     , &
!                                        Sensor_Scan_Angle     = Sensor_Scan_Angle    , &
!                                        Sensor_Zenith_Angle   = Sensor_Zenith_Angle  , &
!                                        Sensor_Azimuth_Angle  = Sensor_Azimuth_Angle , &
!                                        Source_Zenith_Angle   = Source_Zenith_Angle  , &
!                                        Source_Azimuth_Angle  = Source_Azimuth_Angle , &
!                                        Flux_Zenith_Angle     = Flux_Zenith_Angle    , &
!                                        Year                  = Year                 , &
!                                        Month                 = Month                , &
!                                        Day                   = Day                  , &
!                                        Distance_Ratio        = Distance_Ratio       , &
!                                        Sensor_Scan_Radian    = Sensor_Scan_Radian   , &
!                                        Sensor_Zenith_Radian  = Sensor_Zenith_Radian , &
!                                        Sensor_Azimuth_Radian = Sensor_Azimuth_Radian, &
!                                        Secant_Sensor_Zenith  = Secant_Sensor_Zenith , &
!                                        Source_Zenith_Radian  = Source_Zenith_Radian , &
!                                        Source_Azimuth_Radian = Source_Azimuth_Radian, &
!                                        Secant_Source_Zenith  = Secant_Source_Zenith , &
!                                        Flux_Zenith_Radian    = Flux_Zenith_Radian   , &
!                                        Secant_Flux_Zenith    = Secant_Flux_Zenith   , &
!                                        Trans_Zenith_Radian   = Trans_Zenith_Radian  , &
!                                        Secant_Trans_Zenith   = Secant_Trans_Zenith  , &
!                                        AU_ratio2             = AU_ratio2              )
! OBJECTS:
!       gInfo:                Geometry object from which component values
!                             are to be retrieved.
!                             UNITS:      N/A
!                             TYPE:       CRTM_Geometry_type
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL OUTPUTS:
!       Geometry:             Geometry object.
!                             UNITS:      N/A
!                             TYPE:       CRTM_Geometry_type
!                             DIMENSION:  Scalar or same as gInfo input
!                             ATTRIBUTES: INTENT(OUT)
!
!       All other gInfo components as listed in the calling sequence.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_GeometryInfo_GetValue( &
    gInfo                , &  ! Input
    Geometry             , &  ! Optional output
    iFOV                 , &  ! Optional output
    Longitude            , &  ! Optional output
    Latitude             , &  ! Optional output
    Surface_Altitude     , &  ! Optional output
    Sensor_Scan_Angle    , &  ! Optional output
    Sensor_Zenith_Angle  , &  ! Optional output
    Sensor_Azimuth_Angle , &  ! Optional output
    Source_Zenith_Angle  , &  ! Optional output
    Source_Azimuth_Angle , &  ! Optional output
    Flux_Zenith_Angle    , &  ! Optional output
    Year                 , &  ! Optional output
    Month                , &  ! Optional output
    Day                  , &  ! Optional output
    Distance_Ratio       , &  ! Optional output
    Sensor_Scan_Radian   , &  ! Optional output
    Sensor_Zenith_Radian , &  ! Optional output
    Sensor_Azimuth_Radian, &  ! Optional output
    Secant_Sensor_Zenith , &  ! Optional output
    Source_Zenith_Radian , &  ! Optional output
    Source_Azimuth_Radian, &  ! Optional output
    Secant_Source_Zenith , &  ! Optional output
    Flux_Zenith_Radian   , &  ! Optional output
    Secant_Flux_Zenith   , &  ! Optional output
    Trans_Zenith_Radian  , &  ! Optional output
    Secant_Trans_Zenith  , &  ! Optional output
    AU_ratio2              )  ! Optional output
    ! Arguments
    TYPE(CRTM_GeometryInfo_type),       INTENT(IN)  :: gInfo
    TYPE(CRTM_Geometry_type), OPTIONAL, INTENT(OUT) :: Geometry
    INTEGER ,                 OPTIONAL, INTENT(OUT) :: iFOV
    REAL(fp),                 OPTIONAL, INTENT(OUT) :: Longitude
    REAL(fp),                 OPTIONAL, INTENT(OUT) :: Latitude
    REAL(fp),                 OPTIONAL, INTENT(OUT) :: Surface_Altitude
    REAL(fp),                 OPTIONAL, INTENT(OUT) :: Sensor_Scan_Angle
    REAL(fp),                 OPTIONAL, INTENT(OUT) :: Sensor_Zenith_Angle
    REAL(fp),                 OPTIONAL, INTENT(OUT) :: Sensor_Azimuth_Angle
    REAL(fp),                 OPTIONAL, INTENT(OUT) :: Source_Zenith_Angle
    REAL(fp),                 OPTIONAL, INTENT(OUT) :: Source_Azimuth_Angle
    REAL(fp),                 OPTIONAL, INTENT(OUT) :: Flux_Zenith_Angle
    INTEGER,                  OPTIONAL, INTENT(OUT) :: Year 
    INTEGER,                  OPTIONAL, INTENT(OUT) :: Month
    INTEGER,                  OPTIONAL, INTENT(OUT) :: Day  
    REAL(fp),                 OPTIONAL, INTENT(OUT) :: Distance_Ratio       
    REAL(fp),                 OPTIONAL, INTENT(OUT) :: Sensor_Scan_Radian   
    REAL(fp),                 OPTIONAL, INTENT(OUT) :: Sensor_Zenith_Radian 
    REAL(fp),                 OPTIONAL, INTENT(OUT) :: Sensor_Azimuth_Radian
    REAL(fp),                 OPTIONAL, INTENT(OUT) :: Secant_Sensor_Zenith 
    REAL(fp),                 OPTIONAL, INTENT(OUT) :: Source_Zenith_Radian 
    REAL(fp),                 OPTIONAL, INTENT(OUT) :: Source_Azimuth_Radian
    REAL(fp),                 OPTIONAL, INTENT(OUT) :: Secant_Source_Zenith 
    REAL(fp),                 OPTIONAL, INTENT(OUT) :: Flux_Zenith_Radian   
    REAL(fp),                 OPTIONAL, INTENT(OUT) :: Secant_Flux_Zenith   
    REAL(fp),                 OPTIONAL, INTENT(OUT) :: Trans_Zenith_Radian   
    REAL(fp),                 OPTIONAL, INTENT(OUT) :: Secant_Trans_Zenith   
    REAL(fp),                 OPTIONAL, INTENT(OUT) :: AU_ratio2   
    
    ! Get values
    IF ( PRESENT(Geometry) ) Geometry = gInfo%user
    CALL CRTM_Geometry_GetValue( gInfo%user, &
                                 iFOV                 = iFOV                , &
                                 Longitude            = Longitude           , &
                                 Latitude             = Latitude            , &
                                 Surface_Altitude     = Surface_Altitude    , &
                                 Sensor_Scan_Angle    = Sensor_Scan_Angle   , &
                                 Sensor_Zenith_Angle  = Sensor_Zenith_Angle , &
                                 Sensor_Azimuth_Angle = Sensor_Azimuth_Angle, &
                                 Source_Zenith_Angle  = Source_Zenith_Angle , &
                                 Source_Azimuth_Angle = Source_Azimuth_Angle, &
                                 Flux_Zenith_Angle    = Flux_Zenith_Angle   , &
                                 Year                 = Year                , &
                                 Month                = Month               , &
                                 Day                  = Day                   )

    IF ( PRESENT(Distance_Ratio       ) ) Distance_Ratio        = gInfo%Distance_Ratio       
    IF ( PRESENT(Sensor_Scan_Radian   ) ) Sensor_Scan_Radian    = gInfo%Sensor_Scan_Radian   
    IF ( PRESENT(Sensor_Zenith_Radian ) ) Sensor_Zenith_Radian  = gInfo%Sensor_Zenith_Radian 
    IF ( PRESENT(Sensor_Azimuth_Radian) ) Sensor_Azimuth_Radian = gInfo%Sensor_Azimuth_Radian
    IF ( PRESENT(Secant_Sensor_Zenith ) ) Secant_Sensor_Zenith  = gInfo%Secant_Sensor_Zenith 
    IF ( PRESENT(Source_Zenith_Radian ) ) Source_Zenith_Radian  = gInfo%Source_Zenith_Radian 
    IF ( PRESENT(Source_Azimuth_Radian) ) Source_Azimuth_Radian = gInfo%Source_Azimuth_Radian
    IF ( PRESENT(Secant_Source_Zenith ) ) Secant_Source_Zenith  = gInfo%Secant_Source_Zenith 
    IF ( PRESENT(Flux_Zenith_Radian   ) ) Flux_Zenith_Radian    = gInfo%Flux_Zenith_Radian   
    IF ( PRESENT(Secant_Flux_Zenith   ) ) Secant_Flux_Zenith    = gInfo%Secant_Flux_Zenith   
    IF ( PRESENT(Trans_Zenith_Radian  ) ) Trans_Zenith_Radian   = gInfo%Trans_Zenith_Radian   
    IF ( PRESENT(Secant_Trans_Zenith  ) ) Secant_Trans_Zenith   = gInfo%Secant_Trans_Zenith   
    IF ( PRESENT(AU_ratio2            ) ) AU_ratio2             = gInfo%AU_ratio2   
  
  END SUBROUTINE CRTM_GeometryInfo_GetValue


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_GeometryInfo_IsValid
!
! PURPOSE:
!       Non-pure function to perform some simple validity checks on a
!       CRTM GeometryInfo container object. 
!
!       If invalid data is found, a message is printed to stdout.
!
! CALLING SEQUENCE:
!       result = CRTM_GeometryInfo_IsValid( gInfo )
!
!         or
!
!       IF ( CRTM_GeometryInfo_IsValid( gInfo ) ) THEN....
!
! OBJECTS:
!       gInfo:     CRTM GeometryInfo object which is to have its
!                  contents checked.
!                  UNITS:      N/A
!                  TYPE:       CRTM_GeometryInfo_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:    Logical variable indicating whether or not the input
!                  passed the check.
!                  If == .FALSE., GeometryInfo object is unused or contains
!                                 invalid data.
!                     == .TRUE.,  GeometryInfo object can be used in CRTM.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_GeometryInfo_IsValid( gInfo ) RESULT( IsValid )
    TYPE(CRTM_GeometryInfo_type), INTENT(IN) :: gInfo
    LOGICAL :: IsValid
    
    IsValid = CRTM_Geometry_IsValid( gInfo%user )

  END FUNCTION CRTM_GeometryInfo_IsValid


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_GeometryInfo_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a CRTM GeometryInfo container object
!       to stdout.
!
! CALLING SEQUENCE:
!       CALL CRTM_GeometryInfo_Inspect( gInfo )
!
! INPUTS:
!       gInfo:  CRTM GeometryInfo object to display.
!               UNITS:      N/A
!               TYPE:       CRTM_GeometryInfo_type
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_GeometryInfo_Inspect( gInfo )
    TYPE(CRTM_GeometryInfo_type), INTENT(IN) :: gInfo
    CHARACTER(*), PARAMETER :: RFMT = 'es13.6'

    WRITE(*, '(1x,"GeometryInfo OBJECT")')
    WRITE(*, '(3x,"Distance ratio        :",1x,'//RFMT//')') gInfo%Distance_Ratio     
    ! ...Sensor angle information
    WRITE(*, '(3x,"Sensor scan radian    :",1x,'//RFMT//')') gInfo%Sensor_Scan_Radian   
    WRITE(*, '(3x,"Sensor zenith radian  :",1x,'//RFMT//')') gInfo%Sensor_Zenith_Radian 
    WRITE(*, '(3x,"Sensor azimuth radian :",1x,'//RFMT//')') gInfo%Sensor_Azimuth_Radian
    WRITE(*, '(3x,"Secant sensor zenith  :",1x,'//RFMT//')') gInfo%Secant_Sensor_Zenith 
    ! ...Source angle information
    WRITE(*, '(3x,"Source zenith radian  :",1x,'//RFMT//')') gInfo%Source_Zenith_Radian 
    WRITE(*, '(3x,"Source azimuth radian :",1x,'//RFMT//')') gInfo%Source_Azimuth_Radian
    WRITE(*, '(3x,"Secant source zenith  :",1x,'//RFMT//')') gInfo%Secant_Source_Zenith 
    ! ...Flux angle information
    WRITE(*, '(3x,"Flux zenith radian    :",1x,'//RFMT//')') gInfo%Flux_Zenith_Radian
    WRITE(*, '(3x,"Secant flux zenith    :",1x,'//RFMT//')') gInfo%Secant_Flux_Zenith
    ! ...AU ratio information
    WRITE(*, '(3x,"AU ratio^2            :",1x,'//RFMT//')') gInfo%AU_ratio2

    ! The contained object
    CALL CRTM_Geometry_Inspect(gInfo%user)
    
  END SUBROUTINE CRTM_GeometryInfo_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_GeometryInfo_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_GeometryInfo_DefineVersion( Id )
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

  SUBROUTINE CRTM_GeometryInfo_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_GeometryInfo_DefineVersion


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
!       CRTM_GeometryInfo_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two CRTM_GeometryInfo objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = CRTM_GeometryInfo_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two CRTM GeometryInfo objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       CRTM_GeometryInfo_type
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

  ELEMENTAL FUNCTION CRTM_GeometryInfo_Equal( x, y ) RESULT( is_equal )
    TYPE(CRTM_GeometryInfo_type) , INTENT(IN)  :: x, y
    LOGICAL :: is_equal

    is_equal = ( (x%user == y%user ) .AND. &
                 (x%Distance_Ratio        .EqualTo. y%Distance_Ratio       ) .AND. &
                 (x%Sensor_Scan_Radian    .EqualTo. y%Sensor_Scan_Radian   ) .AND. &
                 (x%Sensor_Zenith_Radian  .EqualTo. y%Sensor_Zenith_Radian ) .AND. &
                 (x%Sensor_Azimuth_Radian .EqualTo. y%Sensor_Azimuth_Radian) .AND. &
                 (x%Secant_Sensor_Zenith  .EqualTo. y%Secant_Sensor_Zenith ) .AND. &
                 (x%Source_Zenith_Radian  .EqualTo. y%Source_Zenith_Radian ) .AND. &
                 (x%Source_Azimuth_Radian .EqualTo. y%Source_Azimuth_Radian) .AND. &
                 (x%Secant_Source_Zenith  .EqualTo. y%Secant_Source_Zenith ) .AND. &
                 (x%Flux_Zenith_Radian    .EqualTo. y%Flux_Zenith_Radian   ) .AND. &
                 (x%Secant_Flux_Zenith    .EqualTo. y%Secant_Flux_Zenith   ) .AND. &
                 (x%AU_ratio2             .EqualTo. y%AU_ratio2            )       )

  END FUNCTION CRTM_GeometryInfo_Equal

END MODULE CRTM_GeometryInfo_Define
