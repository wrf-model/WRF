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
  ! Intrinsic modules
  USE ISO_Fortran_Env      , ONLY: OUTPUT_UNIT
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: DEFAULT_N_SIGFIG, &
                                   OPERATOR(.EqualTo.), &
                                   Compares_Within_Tolerance
  USE File_Utility         , ONLY: File_Open, File_Exists
  USE Binary_File_Utility  , ONLY: Open_Binary_File      , &
                                   WriteGAtts_Binary_File, &
                                   ReadGAtts_Binary_File
  USE CRTM_Parameters      , ONLY: EARTH_RADIUS      , &
                                   SATELLITE_HEIGHT  , &
                                   DIFFUSIVITY_RADIAN, &
                                   SECANT_DIFFUSIVITY
  USE CRTM_Geometry_Define , ONLY: CRTM_Geometry_type, &
                                   OPERATOR(==), &
                                   OPERATOR(-) , &
                                   CRTM_Geometry_Destroy    , &
                                   CRTM_Geometry_SetValue   , &
                                   CRTM_Geometry_GetValue   , &
                                   CRTM_Geometry_IsValid    , &
                                   CRTM_Geometry_Inspect    , &
                                   CRTM_Geometry_ReadRecord , &
                                   CRTM_Geometry_WriteRecord
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Operators
  PUBLIC :: OPERATOR(==)
  PUBLIC :: OPERATOR(-)
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
  PUBLIC :: CRTM_GeometryInfo_InquireFile
  PUBLIC :: CRTM_GeometryInfo_ReadFile
  PUBLIC :: CRTM_GeometryInfo_WriteFile


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CRTM_GeometryInfo_Equal
  END INTERFACE OPERATOR(==)

  INTERFACE OPERATOR(-)
    MODULE PROCEDURE CRTM_GeometryInfo_Subtract
  END INTERFACE OPERATOR(-)
  
  
  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*),  PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_GeometryInfo_Define.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'
  ! Literal constants 
  REAL(fp), PARAMETER :: ZERO = 0.0_fp 
  REAL(fp), PARAMETER :: ONE  = 1.0_fp 
  ! Message string length 
  INTEGER, PARAMETER :: ML = 256
  ! File status on close after write error
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'


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
    REAL(fp) :: Cosine_Sensor_Zenith  = ZERO
    ! ...Zenith angle used in the transmittance algorithms  
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
    CALL CRTM_Geometry_Destroy(gInfo%user)
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
!                                        Cosine_Sensor_Zenith  = Cosine_Sensor_Zenith , &
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
    Cosine_Sensor_Zenith , &  ! Optional input
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
    REAL(fp),                 OPTIONAL, INTENT(IN)     :: Cosine_Sensor_Zenith
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
    IF ( PRESENT(Cosine_Sensor_Zenith ) ) gInfo%Cosine_Sensor_Zenith  = Cosine_Sensor_Zenith
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
!                                        Cosine_Sensor_Zenith  = Cosine_Sensor_Zenith , &
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
    Cosine_Sensor_Zenith , &  ! Optional output
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
    REAL(fp),                 OPTIONAL, INTENT(OUT) :: Cosine_Sensor_Zenith 
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
    IF ( PRESENT(Cosine_Sensor_Zenith ) ) Cosine_Sensor_Zenith  = gInfo%Cosine_Sensor_Zenith
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
!       CALL CRTM_GeometryInfo_Inspect( gInfo, Unit=unit )
!
! INPUTS:
!       gInfo:  CRTM GeometryInfo object to display.
!               UNITS:      N/A
!               TYPE:       CRTM_GeometryInfo_type
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Unit:   Unit number for an already open file to which the output
!               will be written.
!               If the argument is specified and the file unit is not
!               connected, the output goes to stdout.
!               UNITS:      N/A
!               TYPE:       INTEGER
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_GeometryInfo_Inspect( gInfo, Unit )
    ! Arguments
    TYPE(CRTM_GeometryInfo_type), INTENT(IN) :: gInfo
    INTEGER,            OPTIONAL, INTENT(IN) :: Unit
    ! Local parameters
    CHARACTER(*), PARAMETER :: RFMT = 'es13.6'
    ! Local variables
    INTEGER :: fid

    ! Setup
    fid = OUTPUT_UNIT
    IF ( PRESENT(Unit) ) THEN
      IF ( File_Open(Unit) ) fid = Unit
    END IF


    WRITE(fid, '(1x,"GeometryInfo OBJECT")')
    WRITE(fid, '(3x,"Distance ratio        :",1x,'//RFMT//')') gInfo%Distance_Ratio     
    ! ...Sensor angle information
    WRITE(fid, '(3x,"Sensor scan radian    :",1x,'//RFMT//')') gInfo%Sensor_Scan_Radian   
    WRITE(fid, '(3x,"Sensor zenith radian  :",1x,'//RFMT//')') gInfo%Sensor_Zenith_Radian 
    WRITE(fid, '(3x,"Sensor azimuth radian :",1x,'//RFMT//')') gInfo%Sensor_Azimuth_Radian
    WRITE(fid, '(3x,"Secant sensor zenith  :",1x,'//RFMT//')') gInfo%Secant_Sensor_Zenith 
    WRITE(fid, '(3x,"Cosine sensor zenith  :",1x,'//RFMT//')') gInfo%Cosine_Sensor_Zenith
    ! ...Transmittance algorithm sensor angle information
    WRITE(fid, '(3x,"Trans zenith radian   :",1x,'//RFMT//')') gInfo%Trans_Zenith_Radian 
    WRITE(fid, '(3x,"Secant trans zenith   :",1x,'//RFMT//')') gInfo%Secant_Trans_Zenith
    ! ...Source angle information
    WRITE(fid, '(3x,"Source zenith radian  :",1x,'//RFMT//')') gInfo%Source_Zenith_Radian 
    WRITE(fid, '(3x,"Source azimuth radian :",1x,'//RFMT//')') gInfo%Source_Azimuth_Radian
    WRITE(fid, '(3x,"Secant source zenith  :",1x,'//RFMT//')') gInfo%Secant_Source_Zenith 
    ! ...Flux angle information
    WRITE(fid, '(3x,"Flux zenith radian    :",1x,'//RFMT//')') gInfo%Flux_Zenith_Radian
    WRITE(fid, '(3x,"Secant flux zenith    :",1x,'//RFMT//')') gInfo%Secant_Flux_Zenith
    ! ...AU ratio information
    WRITE(fid, '(3x,"AU ratio^2            :",1x,'//RFMT//')') gInfo%AU_ratio2

    ! The contained object
    CALL CRTM_Geometry_Inspect(gInfo%user, Unit=Unit)
    
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


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_GeometryInfo_InquireFile
!
! PURPOSE:
!       Function to inquire CRTM GeometryInfo object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_GeometryInfo_InquireFile( &
!                        Filename               , &
!                        n_Profiles = n_Profiles  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       CRTM GeometryInfo data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Profiles:     The number of profiles for which there is geometry 
!                       information in the data file.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file inquire was successful
!                          == FAILURE, an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_GeometryInfo_InquireFile( &
    Filename  , &  ! Input
    n_Profiles) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Profiles
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_GeometryInfo_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: m
 
    ! Set up
    err_stat = SUCCESS
    ! ...Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the number of profiles
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) m
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimensions from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Close the file
    CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Set the return arguments
    IF ( PRESENT(n_Profiles) ) n_Profiles = m

  CONTAINS
  
    SUBROUTINE Inquire_CleanUp()
      IF ( File_Open(fid) ) THEN
        CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= SUCCESS ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION CRTM_GeometryInfo_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_GeometryInfo_ReadFile
!
! PURPOSE:
!       Function to read CRTM GeometryInfo object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_GeometryInfo_ReadFile( &
!                        Filename               , &
!                        GeometryInfo           , &
!                        Quiet      = Quiet     , &
!                        n_Profiles = n_Profiles  )
!
! INPUTS:
!       Filename:     Character string specifying the name of an
!                     a GeometryInfo data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       GeometryInfo: CRTM GeometryInfo object array containing the
!                     data read from file.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Geometry_type
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT(OUT), ALLOCATABLE
!
! OPTIONAL INPUTS:
!       Quiet:        Set this logical argument to suppress INFORMATION
!                     messages being printed to stdout
!                     If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                        == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                     If not specified, default is .FALSE.
!                     UNITS:      N/A
!                     TYPE:       LOGICAL
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       n_Profiles:   The number of profiles for which data was read.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS, the file read was successful
!                        == FAILURE, an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_GeometryInfo_ReadFile( &
    Filename    , &  ! Input
    GeometryInfo, &  ! Output
    Quiet       , &  ! Optional input
    n_Profiles  , &  ! Optional output
    Debug       ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),                              INTENT(IN)  :: Filename
    TYPE(CRTM_GeometryInfo_type), ALLOCATABLE, INTENT(OUT) :: GeometryInfo(:)
    LOGICAL,            OPTIONAL,              INTENT(IN)  :: Quiet
    INTEGER,            OPTIONAL,              INTENT(OUT) :: n_Profiles
    LOGICAL,            OPTIONAL,              INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Geometry_ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    CHARACTER(ML) :: alloc_msg
    INTEGER :: io_stat
    INTEGER :: alloc_stat
    LOGICAL :: noisy
    INTEGER :: fid
    INTEGER :: m, n_input_profiles
 

    ! Set up
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) noisy = Debug


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the dimensions     
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) n_input_profiles
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimension from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Allocate the return structure array
   !ALLOCATE(GeometryInfo(n_input_profiles), STAT=alloc_stat, ERRMSG=alloc_msg)
    ALLOCATE(GeometryInfo(n_input_profiles), STAT=alloc_stat)
    IF ( alloc_stat /= 0 ) THEN
      msg = 'Error allocating GeometryInfo array - '//TRIM(alloc_msg)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Loop over all the profiles
    GeometryInfo_Loop: DO m = 1, n_input_profiles
      err_stat = Read_Record( fid, GeometryInfo(m) )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error reading GeometryInfo element #",i0," from ",a)' ) m, TRIM(Filename)
        CALL Read_Cleanup(); RETURN
      END IF
    END DO GeometryInfo_Loop


    ! Close the file
    CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Set the return values
    IF ( PRESENT(n_Profiles) ) n_Profiles = n_input_profiles
    

    ! Output an info message
    IF ( noisy ) THEN
      WRITE( msg,'("Number of profiles read from ",a,": ",i0)' ) &
             TRIM(Filename), n_input_profiles
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp()
      IF ( File_Open(fid) ) THEN
        CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      IF ( ALLOCATED(GeometryInfo) ) THEN 
       !DEALLOCATE(GeometryInfo, STAT=alloc_stat, ERRMSG=alloc_msg)
        DEALLOCATE(GeometryInfo, STAT=alloc_stat)
        IF ( alloc_stat /= 0 ) &
          msg = TRIM(msg)//'; Error deallocating GeometryInfo array during error cleanup - '//&
                TRIM(alloc_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_CleanUp
  
  END FUNCTION CRTM_GeometryInfo_ReadFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_GeometryInfo_WriteFile
!
! PURPOSE:
!       Function to write CRTM GeometryInfo object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_GeometryInfo_WriteFile( &
!                        Filename     , &
!                        Geometry     , &
!                        Quiet = Quiet  )
!
! INPUTS:
!       Filename:     Character string specifying the name of the
!                     GeometryInfo format data file to write.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       GeometryInfo: CRTM GeometryInfo object array containing the
!                     data to write.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Geometry_type
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Quiet:        Set this logical argument to suppress INFORMATION
!                     messages being printed to stdout
!                     If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                        == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                     If not specified, default is .FALSE.
!                     UNITS:      N/A
!                     TYPE:       LOGICAL
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS, the file write was successful
!                        == FAILURE, an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs during *writing*, the output file is deleted before
!         returning to the calling routine.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_GeometryInfo_WriteFile( &
    Filename    , &  ! Input
    GeometryInfo, &  ! Input
    Quiet       , &  ! Optional input
    Debug       ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),                 INTENT(IN) :: Filename
    TYPE(CRTM_GeometryInfo_type), INTENT(IN) :: GeometryInfo(:)
    LOGICAL,            OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,            OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_GeometryInfo_WriteFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: m, n_profiles
 
    ! Set up
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) noisy = Debug


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid, For_Output = .TRUE. )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    n_profiles = SIZE(GeometryInfo)
    WRITE( fid, IOSTAT=io_stat ) n_profiles
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing data dimension to '//TRIM(Filename)//'- '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF

    
    ! Write the data
    GeometryInfo_Loop: DO m = 1, n_profiles
      err_stat = Write_Record( fid, GeometryInfo(m) )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error writing GeometryInfo element #",i0," to ",a)' ) m, TRIM(Filename)
        CALL Write_Cleanup(); RETURN
      END IF
    END DO GeometryInfo_Loop


    ! Close the file (if error, no delete)
    CLOSE( fid,STATUS='KEEP',IOSTAT=io_stat,IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//'- '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      WRITE( msg,'("Number of profiles written to ",a,": ",i0)' ) TRIM(Filename), n_profiles
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
    END IF

  CONTAINS
  
    SUBROUTINE Write_CleanUp()
      IF ( File_Open(fid) ) THEN
        CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error deleting output file during error cleanup - '//TRIM(io_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION CRTM_GeometryInfo_WriteFile


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
                 (x%Trans_Zenith_Radian   .EqualTo. y%Trans_Zenith_Radian  ) .AND. &
                 (x%Secant_Trans_Zenith   .EqualTo. y%Secant_Trans_Zenith  ) .AND. &                 
                 (x%Cosine_Sensor_Zenith  .EqualTo. y%Cosine_Sensor_Zenith ) .AND. &
                 (x%Source_Zenith_Radian  .EqualTo. y%Source_Zenith_Radian ) .AND. &
                 (x%Source_Azimuth_Radian .EqualTo. y%Source_Azimuth_Radian) .AND. &
                 (x%Secant_Source_Zenith  .EqualTo. y%Secant_Source_Zenith ) .AND. &
                 (x%Flux_Zenith_Radian    .EqualTo. y%Flux_Zenith_Radian   ) .AND. &
                 (x%Secant_Flux_Zenith    .EqualTo. y%Secant_Flux_Zenith   ) .AND. &
                 (x%AU_ratio2             .EqualTo. y%AU_ratio2            )       )

  END FUNCTION CRTM_GeometryInfo_Equal

!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_GeometryInfo_Subtract
!
! PURPOSE:
!       Pure function to subtract two CRTM GeometryInfo objects.
!       Used in OPERATOR(-) interface block.
!
! CALLING SEQUENCE:
!       gidiff = CRTM_GeometryInfo_Subtract( gi1, gi2 )
!
!         or
!
!       gidiff = gi1 - gi2
!
!
! INPUTS:
!       gi1, gi2:   The GeometryInfo objects to difference.
!                   UNITS:      N/A
!                   TYPE:       CRTM_GeometryInfo_type
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN OUT)
!
! RESULT:
!       gidiff:     GeometryInfo object containing the differenced components.
!                   UNITS:      N/A
!                   TYPE:       CRTM_GeometryInfo_type
!                   DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_GeometryInfo_Subtract( gi1, gi2 ) RESULT( gidiff )
    TYPE(CRTM_GeometryInfo_type), INTENT(IN) :: gi1, gi2
    TYPE(CRTM_GeometryInfo_type) :: gidiff

    ! Copy the first structure
    gidiff = gi1

    ! And subtract the second one's components from it
    ! ...Contained objects
    gidiff%user = gidiff%user - gi2%user
    ! ...Individual components
    gidiff%Distance_Ratio        = gidiff%Distance_Ratio        - gi2%Distance_Ratio 
    gidiff%Sensor_Scan_Radian    = gidiff%Sensor_Scan_Radian    - gi2%Sensor_Scan_Radian
    gidiff%Sensor_Zenith_Radian  = gidiff%Sensor_Zenith_Radian  - gi2%Sensor_Zenith_Radian
    gidiff%Sensor_Azimuth_Radian = gidiff%Sensor_Azimuth_Radian - gi2%Sensor_Azimuth_Radian
    gidiff%Secant_Sensor_Zenith  = gidiff%Secant_Sensor_Zenith  - gi2%Secant_Sensor_Zenith
    gidiff%Cosine_Sensor_Zenith  = gidiff%Cosine_Sensor_Zenith  - gi2%Cosine_Sensor_Zenith
    gidiff%Trans_Zenith_Radian   = gidiff%Trans_Zenith_Radian   - gi2%Trans_Zenith_Radian
    gidiff%Secant_Trans_Zenith   = gidiff%Secant_Trans_Zenith   - gi2%Secant_Trans_Zenith
    gidiff%Source_Zenith_Radian  = gidiff%Source_Zenith_Radian  - gi2%Source_Zenith_Radian
    gidiff%Source_Azimuth_Radian = gidiff%Source_Azimuth_Radian - gi2%Source_Azimuth_Radian
    gidiff%Secant_Source_Zenith  = gidiff%Secant_Source_Zenith  - gi2%Secant_Source_Zenith
    gidiff%Flux_Zenith_Radian    = gidiff%Flux_Zenith_Radian    - gi2%Flux_Zenith_Radian
    gidiff%Secant_Flux_Zenith    = gidiff%Secant_Flux_Zenith    - gi2%Secant_Flux_Zenith
    gidiff%AU_ratio2             = gidiff%AU_ratio2             - gi2%AU_ratio2

  END FUNCTION CRTM_GeometryInfo_Subtract


!----------------------------------------------------------------------------------
!
! NAME:
!       Read_Record
!
! PURPOSE:
!       Utility function to read a single GeometryInfo data record
!
! CALLING SEQUENCE:
!       Error_Status = Read_Record( FileID, GeometryInfo )
!
! INPUTS:
!       FileID:       Logical unit number from which to read data.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       GeometryInfo: CRTM GeometryInfo object containing the data read in.
!                     UNITS:      N/A
!                     TYPE:       CRTM_GeometryInfo_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS, the read was successful
!                        == FAILURE, an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION Read_Record( fid, ginfo ) RESULT( err_stat )
    ! Arguments
    INTEGER,                      INTENT(IN)  :: fid
    TYPE(CRTM_GeometryInfo_type), INTENT(OUT) :: ginfo
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_GeometryInfo_ReadFile(Record)'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat

    ! Set up
    err_stat = SUCCESS


    ! Read the embedded Geometry structure
    err_stat = CRTM_Geometry_ReadRecord( fid, ginfo%user )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading embedded Geometry data'
      CALL Read_Record_Cleanup(); RETURN
    END IF
    
    
    ! Read the data record
    READ( fid, IOSTAT=io_stat,IOMSG=io_msg ) &
      ginfo%Distance_Ratio       , &
      ginfo%Sensor_Scan_Radian   , &
      ginfo%Sensor_Zenith_Radian , &
      ginfo%Sensor_Azimuth_Radian, &
      ginfo%Secant_Sensor_Zenith , &
      ginfo%Cosine_Sensor_Zenith , &
      ginfo%Trans_Zenith_Radian  , &
      ginfo%Secant_Trans_Zenith  , &
      ginfo%Source_Zenith_Radian , &
      ginfo%Source_Azimuth_Radian, &
      ginfo%Secant_Source_Zenith , &
      ginfo%Flux_Zenith_Radian   , &
      ginfo%Secant_Flux_Zenith   , &
      ginfo%AU_ratio2            
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading GeometryInfo data - '//TRIM(io_msg)
      CALL Read_Record_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Read_Record_Cleanup()
      CALL CRTM_GeometryInfo_Destroy( ginfo )
      CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
      IF ( io_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error closing file during error cleanup - '//TRIM(io_msg)
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_Record_Cleanup

  END FUNCTION Read_Record


!----------------------------------------------------------------------------------
!
! NAME:
!       Write_Record
!
! PURPOSE:
!       Function to write a single GeometryInfo data record
!
! CALLING SEQUENCE:
!       Error_Status = Write_Record( FileID, GeometryInfo )
!
! INPUTS:
!       FileID:       Logical unit number to which data is written
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       GeometryInfo: CRTM GeometryInfo object containing the data to write.
!                     UNITS:      N/A
!                     TYPE:       CRTM_GeometryInfo_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the record write was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION Write_Record( fid, ginfo ) RESULT( err_stat )
    ! Arguments
    INTEGER,                      INTENT(IN)  :: fid
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)  :: ginfo
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_GeometryInfo_WriteFile(Record)'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
 
    ! Set up
    err_stat = SUCCESS


    ! Write the embedded Geometry structure
    err_stat = CRTM_Geometry_WriteRecord( fid, ginfo%user )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing embedded Geometry data'
      CALL Write_Record_Cleanup(); RETURN
    END IF
    
    
    ! Write the data record
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      ginfo%Distance_Ratio       , &
      ginfo%Sensor_Scan_Radian   , &
      ginfo%Sensor_Zenith_Radian , &
      ginfo%Sensor_Azimuth_Radian, &
      ginfo%Secant_Sensor_Zenith , &
      ginfo%Cosine_Sensor_Zenith , &
      ginfo%Trans_Zenith_Radian  , &
      ginfo%Secant_Trans_Zenith  , &
      ginfo%Source_Zenith_Radian , &
      ginfo%Source_Azimuth_Radian, &
      ginfo%Secant_Source_Zenith , &
      ginfo%Flux_Zenith_Radian   , &
      ginfo%Secant_Flux_Zenith   , &
      ginfo%AU_ratio2            
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing GeometryInfo data - '//TRIM(io_msg)
      CALL Write_Record_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Write_Record_Cleanup()
      CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat,IOMSG=io_msg )
      IF ( io_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error closing file during error cleanup'
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
    END SUBROUTINE Write_Record_Cleanup
    
  END FUNCTION Write_Record

END MODULE CRTM_GeometryInfo_Define
