!
! CRTM_GeometryInfo
!
! Application module for the GeometryInfo structure.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 19-May-2004
!                       paul.vandelst@noaa.gov
!
!                       Yong Chen, CSU/CIRA 10-May-2006
!                       Yong.Chen@noaa.gov
!

MODULE CRTM_GeometryInfo

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds     , ONLY: fp
  USE Message_Handler, ONLY: SUCCESS, WARNING, FAILURE, Display_Message
  USE Date_Utility   , ONLY: Day_Of_Year
  USE CRTM_Parameters, ONLY: ZERO, ONE, TWO  , &
                             TWOPI           , &
                             EARTH_RADIUS    , &
                             SATELLITE_HEIGHT, &
                             DEGREES_TO_RADIANS, &
                             MAX_TRANS_ZENITH_ANGLE
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_Geometry_type    , &
                                      CRTM_GeometryInfo_type, &
                                      CRTM_GeometryInfo_IsValid
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: CRTM_GeometryInfo_Compute
  PUBLIC :: SACONV, SACONV_TL, SACONV_AD 
  PUBLIC :: VACONV, VACONV_TL, VACONV_AD 
  PUBLIC :: CRTM_GeometryInfo_Version 
  
  
  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*),  PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_GeometryInfo.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  ! Metres->kilometres conversion factor
  REAL(fp), PARAMETER :: M_TO_KM = 1.0e-03_fp


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
!       CRTM_GeometryInfo_Compute
! 
! PURPOSE:
!       Elemental subroutine to compute the derived geometry from the user
!       specified components of the CRTM GeometryInfo structure.
!
! CALLING SEQUENCE:
!       CALL CRTM_GeometryInfo_Compute( GeometryInfo )
!
! INPUTS:
!       GeometryInfo:  The GeometryInfo object containing the user
!                      defined inputs
!                      UNITS:      N/A
!                      TYPE:       CRTM_GeometryInfo_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       GeometryInfo:  The GeometryInfo structure with the non-user
!                      components filled.
!                      UNITS:      N/A
!                      TYPE:       CRTM_GeometryInfo_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! SIDE EFFECTS:
!       This function changes the values of the non-user components of the
!       GeometryInfo object.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_GeometryInfo_Compute( gInfo )
    TYPE(CRTM_GeometryInfo_type), INTENT(IN OUT) :: gInfo
    
    REAL(fp) :: cosv

    ! Compute the derived values
    ! ...Derived sensor angles
    gInfo%Sensor_Scan_Radian    = DEGREES_TO_RADIANS * gInfo%user%Sensor_Scan_Angle
    gInfo%Sensor_Zenith_Radian  = DEGREES_TO_RADIANS * gInfo%user%Sensor_Zenith_Angle
    gInfo%Sensor_Azimuth_Radian = DEGREES_TO_RADIANS * gInfo%user%Sensor_Azimuth_Angle
    gInfo%Cosine_Sensor_Zenith  = COS(gInfo%Sensor_Zenith_Radian)
    gInfo%Secant_Sensor_Zenith  = ONE / gInfo%Cosine_Sensor_Zenith
    ! ...Check user zenith angle. If it is larger than the transmittance algorithms' limit
    !    then use the limiting value in the algorithms.  The optical depths will be scaled 
    !    back to the those at the user zenith angle.
    IF( gInfo%user%Sensor_Zenith_Angle > MAX_TRANS_ZENITH_ANGLE )THEN
      gInfo%Trans_Zenith_Radian = DEGREES_TO_RADIANS * MAX_TRANS_ZENITH_ANGLE
      gInfo%Secant_Trans_Zenith = ONE / COS(gInfo%Trans_Zenith_Radian)
    ELSE
      gInfo%Trans_Zenith_Radian = gInfo%Sensor_Zenith_Radian
      gInfo%Secant_Trans_Zenith = gInfo%Secant_Sensor_Zenith
    END IF
    ! ...Distance ratio, but only if zenith angle is large enough
    IF ( ABS(gInfo%user%Sensor_Zenith_Angle) > ONE ) THEN
      gInfo%Distance_Ratio = ABS(SIN(gInfo%Sensor_Scan_Radian)/SIN(gInfo%Sensor_Zenith_Radian))
    END IF
    ! ...Derived source angles
    gInfo%Source_Zenith_Radian  = DEGREES_TO_RADIANS * gInfo%user%Source_Zenith_Angle
    gInfo%Source_Azimuth_Radian = DEGREES_TO_RADIANS * gInfo%user%Source_Azimuth_Angle
    cosv = COS(gInfo%Source_Zenith_Radian)
    IF( cosv /= ZERO )THEN
      gInfo%Secant_Source_Zenith = ONE / cosv
    ELSE
      ! ...Set to a large number with the sign of the original value
      gInfo%Secant_Source_Zenith = HUGE(cosv) * cosv/ABS(cosv)  ! SIGN(HUGE(cosv),cosv) ?
    ENDIF
      
    ! ...Derived flux angles
    gInfo%Flux_Zenith_Radian = DEGREES_TO_RADIANS * gInfo%user%Flux_Zenith_Angle
    gInfo%Secant_Flux_Zenith = ONE / COS(gInfo%Flux_Zenith_Radian)
    ! ...AU ratio term
    gInfo%AU_ratio2 = Compute_AU_ratio2( gInfo%user%Year, gInfo%user%Month, gInfo%user%Day )
                    
  END SUBROUTINE CRTM_GeometryInfo_Compute


!----------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!      SACONV  
!
! PURPOSE:
!      Elemental subroutine to compute the sensor zenith angle at a given altitude
!      for a given surface sensor zenith angle.
!
! CALLING SEQUENCE:
!        CALL SACONV( Sensor_Zenith_Radian, &  ! Input
!                     Altitude            , &  ! Input
!                     Local_Zenith_Radian   )  ! Output
!
! INPUTS:
!       Sensor_Zenith_Radian: Sensor zenith angle at the Earth's surface
!                             UNITS:      radians
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN) 
!
!       Altitude:             The altitude at which the local sensor zenith angle
!                             is required 
!                             UNITS:      metres
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN)
!  
! OUTPUTS:
!       Local_Zenith_Radian:  The sensor zenith angle at the supplied altitude.
!                             UNITS:      radians
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input
!                             ATTRIBUTES: INTENT(OUT)
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 10-May-2006
!                       Yong.Chen@noaa.gov
!
!                       Paul van Delst, 18-Nov-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SACONV( &
    Sensor_Zenith_Radian, &  ! Input
    Altitude            , &  ! Input
    Local_Zenith_Radian   )  ! Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: Sensor_Zenith_Radian
    REAL(fp), INTENT(IN)  :: Altitude 
    REAL(fp), INTENT(OUT) :: Local_Zenith_Radian
    ! Local variables
    REAL(fp) :: ra

    ! Compute the radius, in km, of the point at which to calc the angle
    ra = EARTH_RADIUS + (M_TO_KM * Altitude)

    ! Compute the angle
    Local_Zenith_Radian = ASIN((EARTH_RADIUS / ra) * SIN(Sensor_Zenith_Radian))
  
  END SUBROUTINE SACONV


!----------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!      SACONV_TL  
!
! PURPOSE:
!      Tangent-linear form of elemental subroutine to compute the sensor zenith
!      angle at a given altitude for a given surface sensor zenith angle.
!
! CALLING SEQUENCE:
!        CALL SACONV_TL( Sensor_Zenith_Radian , &  ! FWD Input
!                        Altitude             , &  ! FWD Input
!                        Altitude_TL          , &  ! TL  Input
!                        Local_Zenith_Radian_TL )  ! TL  Output
!
! INPUTS:
!       Sensor_Zenith_Radian:    Sensor zenith angle at the Earth's surface
!                                UNITS:      radians
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar or any rank
!                                ATTRIBUTES: INTENT(IN) 
!
!       Altitude:                The altitude at which the local sensor zenith
!                                angle is required 
!                                UNITS:      metres
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar or any rank
!                                ATTRIBUTES: INTENT(IN)
!  
!       Altitude_TL:             Tangent-linear altitude. 
!                                UNITS:      metres
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar or any rank
!                                ATTRIBUTES: INTENT(IN)
!  
! OUTPUTS:
!       Local_Zenith_Radian_TL:  The tangent-linear sensor zenith angle at the
!                                supplied altitude.
!                                UNITS:      radians
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Same as input
!                                ATTRIBUTES: INTENT(OUT)
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 10-May-2006
!                       Yong.Chen@noaa.gov
!
!                       Paul van Delst, 20-Nov-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SACONV_TL( &
    Sensor_Zenith_Radian,  &  ! FWD Input
    Altitude,              &  ! FWD Input
    Altitude_TL,           &  ! TL  Input
    Local_Zenith_Radian_TL )  ! TL  Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: Sensor_Zenith_Radian 
    REAL(fp), INTENT(IN)  :: Altitude 
    REAL(fp), INTENT(IN)  :: Altitude_TL 
    REAL(fp), INTENT(OUT) :: Local_Zenith_Radian_TL
    ! Local variables
    REAL(fp) :: ra, ra_TL   
    REAL(fp) :: Local_Zenith_Radian

    ! Compute the radius, in km, of the point at which to calc the angle
    ra    = EARTH_RADIUS + (M_TO_KM * Altitude)
    ra_TL = M_TO_KM * Altitude_TL

    ! Compute the tangent-linear angle
    CALL SACONV( Sensor_Zenith_Radian, Altitude, Local_Zenith_Radian )
    Local_Zenith_Radian_TL = -TAN(Local_Zenith_Radian) * ra_TL / ra
  
  END SUBROUTINE SACONV_TL


!----------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!      SACONV_AD  
!
! PURPOSE:
!      Adjoint form of elemental subroutine to compute the sensor zenith
!      angle at a given altitude for a given surface sensor zenith angle.
!
! CALLING SEQUENCE:
!        CALL SACONV_AD( Sensor_Zenith_Radian  , &  ! FWD Input
!                        Altitude              , &  ! FWD Input
!                        Local_Zenith_Radian_AD, &  ! AD  Input
!                        Altitude_AD             )  ! AD  Output
!
! INPUTS:
!       Sensor_Zenith_Radian:    Sensor zenith angle at the Earth's surface
!                                UNITS:      radians
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar or any rank
!                                ATTRIBUTES: INTENT(IN) 
!
!       Altitude:                The altitude at which the local sensor zenith
!                                angle is required 
!                                UNITS:      metres
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar or any rank
!                                ATTRIBUTES: INTENT(IN)
!  
!       Local_Zenith_Radian_AD:  The adjoint sensor zenith angle at the
!                                supplied altitude.
!                                *** SET TO ZERO ON EXIT ***
!                                UNITS:      radians
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar or any rank
!                                ATTRIBUTES: INTENT(IN OUT)
!  
! OUTPUTS:
!       Altitude_AD:             Adjoint altitude. 
!                                *** MUST HAVE VALUE ON ENTRY ***
!                                UNITS:      metres
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Same as input
!                                ATTRIBUTES: INTENT(IN OUT)
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 10-May-2006
!                       Yong.Chen@noaa.gov
!
!                       Paul van Delst, 23-Nov-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE SACONV_AD( &
    Sensor_Zenith_Radian,   &  ! FWD Input
    Altitude,               &  ! FWD Input
    Local_Zenith_Radian_AD, &  ! AD  Input
    Altitude_AD             )  ! AD  Output
    ! Arguments
    REAL(fp), INTENT(IN)     :: Sensor_Zenith_Radian 
    REAL(fp), INTENT(IN)     :: Altitude 
    REAL(fp), INTENT(IN OUT) :: Local_Zenith_Radian_AD  
    REAL(fp), INTENT(IN OUT) :: Altitude_AD
    ! Local variables
    REAL(fp) :: Local_Zenith_Radian  
    REAL(fp) :: ra, ra_AD   

    ! Forward model calculations
    ra = EARTH_RADIUS + (M_TO_KM * Altitude)
    CALL SACONV( Sensor_Zenith_Radian, Altitude, Local_Zenith_Radian )
           
    ! Compute the angle adjoint
    ra_AD = -TAN(Local_Zenith_Radian) * Local_Zenith_Radian_AD / ra
    Local_Zenith_Radian_AD = ZERO
    Altitude_AD = Altitude_AD + (M_TO_KM * ra_AD)
   
  END SUBROUTINE SACONV_AD

  
!----------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!      VACONV  
!
! PURPOSE:
!      Elemental subroutine to compute the sensor zenith angle at a given altitude
!      for a given sensor scan angle.
!
! CALLING SEQUENCE:
!        CALL VACONV( Sensor_Scan_Radian, & ! Input
!                     Satellite_Altitude, & ! Input
!                     Altitude,           & ! Input
!                     Local_Zenith_Radian ) ! Output
!
! INPUTS:
!       Sensor_Scan_Radian:   Sensor scan angle.
!                             UNITS:      radians
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN) 
!
!       Satellite_Altitude:   The satellite altitude 
!                             UNITS:      kilometres
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN)
!  
!       Altitude:             The altitude at which the local sensor zenith angle
!                             is required 
!                             UNITS:      metres
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Scalar or any rank
!                             ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Local_Zenith_Radian:  The sensor zenith angle at the supplied altitude.
!                             UNITS:      radians
!                             TYPE:       REAL(fp)
!                             DIMENSION:  Same as input
!                             ATTRIBUTES: INTENT(OUT)
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 10-May-2006
!                       Yong.Chen@noaa.gov
!
!                       Paul van Delst, 23-Nov-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE VACONV( &
    Sensor_Scan_Radian, & ! Input
    Satellite_Altitude, & ! Input
    Altitude,           & ! Input
    Local_Zenith_Radian ) ! Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: Sensor_Scan_Radian
    REAL(fp), INTENT(IN)  :: Satellite_Altitude 
    REAL(fp), INTENT(IN)  :: Altitude 
    REAL(fp), INTENT(OUT) :: Local_Zenith_Radian
    ! Local variables
    REAL(fp) :: ra
    REAL(fp) :: rs

    ! Radius calculations
    ! ...Compute the radius, in km, of the point at which to calc the angle
    ra = EARTH_RADIUS + (M_TO_KM * Altitude)
    ! ...The radius of the satellite orbit, in km.
    rs = EARTH_RADIUS + Satellite_Altitude

    ! Compute the angle
    Local_Zenith_Radian = ASIN((rs / ra) * SIN(Sensor_Scan_Radian))
  
  END SUBROUTINE VACONV


!----------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!      VACONV_TL
!
! PURPOSE:
!      Tangent-linear form of elemental subroutine to compute the sensor zenith
!      angle at a given altitude for a given sensor scan angle.
!
! CALLING SEQUENCE:
!        CALL VACONV_TL( Sensor_Scan_Radian,    &  ! FWD Input
!                        Satellite_Altitude,    &  ! FWD Input
!                        Altitude,              &  ! FWD Input
!                        Altitude_TL,           &  ! TL  Input
!                        Local_Zenith_Radian_TL )  ! TL  Output
!
! INPUTS:
!       Sensor_Scan_Radian:      Sensor scan angle.
!                                UNITS:      radians
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar or any rank
!                                ATTRIBUTES: INTENT(IN) 
!
!       Satellite_Altitude:      The satellite altitude 
!                                UNITS:      kilometres
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar or any rank
!                                ATTRIBUTES: INTENT(IN)
!  
!       Altitude:                The altitude at which the local sensor zenith
!                                angle is required 
!                                UNITS:      metres
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar or any rank
!                                ATTRIBUTES: INTENT(IN)
!
!       Altitude_TL:             Tangent-linear altitude. 
!                                UNITS:      metres
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar or any rank
!                                ATTRIBUTES: INTENT(IN)
!  
! OUTPUTS:
!       Local_Zenith_Radian_TL:  The tangent-linear sensor zenith angle at the
!                                supplied altitude.
!                                UNITS:      radians
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Same as input
!                                ATTRIBUTES: INTENT(OUT)
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 10-May-2006
!                       Yong.Chen@noaa.gov
!
!                       Paul van Delst, 23-Nov-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE VACONV_TL( &
    Sensor_Scan_Radian,    &  ! FWD Input
    Satellite_Altitude,    &  ! FWD Input
    Altitude,              &  ! FWD Input
    Altitude_TL,           &  ! TL  Input
    Local_Zenith_Radian_TL )  ! TL  Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: Sensor_Scan_Radian 
    REAL(fp), INTENT(IN)  :: Satellite_Altitude 
    REAL(fp), INTENT(IN)  :: Altitude 
    REAL(fp), INTENT(IN)  :: Altitude_TL 
    REAL(fp), INTENT(OUT) :: Local_Zenith_Radian_TL
    ! Local variables
    REAL(fp) :: ra, ra_TL   
    REAL(fp) :: rs   
    REAL(fp) :: Local_Zenith_Radian

    ! Radius calculations
    ! ...Compute the radius, in km, of the point at which to calc the angle
    ra    = EARTH_RADIUS + (M_TO_KM * Altitude)
    ra_TL = M_TO_KM * Altitude_TL
    ! ...The radius of the satellite orbit, in km.
    rs = EARTH_RADIUS + Satellite_Altitude

    ! Compute the tangent-linear angle
    CALL VACONV( Sensor_Scan_Radian, Satellite_Altitude, Altitude, Local_Zenith_Radian )
    Local_Zenith_Radian_TL = -TAN(Local_Zenith_Radian) * ra_TL / ra
  
  END SUBROUTINE VACONV_TL


!----------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!      VACONV_AD
!
! PURPOSE:
!      Adjoint form of elemental subroutine to compute the sensor zenith
!      angle at a given altitude for a given sensor scan angle.
!
! CALLING SEQUENCE:
!        CALL VACONV_AD( Sensor_Scan_Radian,    &  ! FWD Input
!                        Satellite_Altitude,    &  ! FWD Input
!                        Altitude,              &  ! FWD Input
!                        Local_Zenith_Radian_AD )  ! TL  Output
!                        Altitude_AD,           &  ! TL  Input
!
! INPUTS:
!       Sensor_Scan_Radian:      Sensor scan angle.
!                                UNITS:      radians
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar or any rank
!                                ATTRIBUTES: INTENT(IN) 
!
!       Satellite_Altitude:      The satellite altitude 
!                                UNITS:      kilometres
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar or any rank
!                                ATTRIBUTES: INTENT(IN)
!  
!       Altitude:                The altitude at which the local sensor zenith
!                                angle is required 
!                                UNITS:      metres
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar or any rank
!                                ATTRIBUTES: INTENT(IN)
!
!       Local_Zenith_Radian_AD:  The adjoint sensor zenith angle at the
!                                supplied altitude.
!                                *** SET TO ZERO ON EXIT ***
!                                UNITS:      radians
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar or any rank
!                                ATTRIBUTES: INTENT(IN OUT)
!  
! OUTPUTS:
!       Altitude_AD:             Adjoint altitude. 
!                                *** MUST HAVE VALUE ON ENTRY ***
!                                UNITS:      metres
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Same as input
!                                ATTRIBUTES: INTENT(IN OUT)
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 10-May-2006
!                       Yong.Chen@noaa.gov
!
!                       Paul van Delst, 23-Nov-2009
!                       paul.vandelst@noaa.gov
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE VACONV_AD( &
    Sensor_Scan_Radian,     &  ! FWD Input
    Satellite_Altitude,     &  ! FWD Input
    Altitude,               &  ! FWD Input
    Local_Zenith_Radian_AD, &  ! AD  Input
    Altitude_AD             )  ! AD  Output
    ! Arguments
    REAL(fp), INTENT(IN)     :: Sensor_Scan_Radian 
    REAL(fp), INTENT(IN)     :: Satellite_Altitude 
    REAL(fp), INTENT(IN)     :: Altitude 
    REAL(fp), INTENT(IN OUT) :: Local_Zenith_Radian_AD 
    REAL(fp), INTENT(IN OUT) :: Altitude_AD
    ! Local variables
    REAL(fp) :: ra, ra_AD   
    REAL(fp) :: rs   
    REAL(fp) :: Local_Zenith_Radian

    ! Forward model calcuations
    ra = EARTH_RADIUS + (M_TO_KM * Altitude)
    rs = EARTH_RADIUS + Satellite_Altitude
    CALL VACONV( Sensor_Scan_Radian, Satellite_Altitude, Altitude, Local_Zenith_Radian )

    ! Compute the angle adjoint
    ra_AD = -TAN(Local_Zenith_Radian) * Local_Zenith_Radian_AD / ra
    Local_Zenith_Radian_AD = ZERO
    Altitude_AD = Altitude_AD + (M_TO_KM * ra_AD)
  
  END SUBROUTINE VACONV_AD


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_GeometryInfo_Version
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_GeometryInfo_Version( Id )
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

  SUBROUTINE CRTM_GeometryInfo_Version( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_GeometryInfo_Version


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
!       Compute_AU_ratio2
!
! PURPOSE:
!       Elemental function to compute the square of the ratio between the
!       semi-major axis of the Earth's elliptical orbit about the Sun, and
!       the actual Earth-Sun distance.
!
! CALLING SEQUENCE:
!       AU_ratio2 = Compute_AU_ratio2( Year, Month, Day )
!
! INPUTS:
!       Year:          The year in 4-digit format, e.g. 1997.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar or same as geo input
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Month:         The month of the year (1-12).
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar or same as geo input
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Day:           The day of the month (1-28/29/30/31).
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar or same as geo input
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       AU_Ratio2:     The square of the ratio between the semi-major axis
!                      of the Earth's elliptical orbit about the Sun, and
!                      the actual Earth-Sun distance.
!                      UNITS:      N/A
!                      TYPE:       REAL(fp)
!                      DIMENSION:  Same as inputs.
!
! PROCEDURE:
!       Adapted from eqn. 2.2.9 in 
!
!       Liou, K.N., 2002, "An Introduction to Atmospheric Radiation",
!         2nd Ed., Academic Press, San Diego, California
!
!                  __ N 
!       ( a )^2   \
!       (---)   =  >  a(i).cos(i.t) + b(i).sin(i.t)
!       ( r )     /__
!                     i=0
!
!       where
!       
!         a = semi-major axis of Earth's orbit about the Sun
!         
!         r = Earth-Sun distance at time t
!         
!             2.PI.(DoY-1)
!         t = ------------
!               Max_DoY
!
!         DoY = day of the year
!         
!         Max_DoY = number of days in the year (365 or 366 for leap year)
!                        
!       and the coefficients are taken from table 2.2 in Liou(2002):
!       
!           i     a(i)       b(i)
!         ---------------------------
!           0   1.000110       0
!           1   0.034221   0.001280
!           2   0.000719   0.000077
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Compute_AU_ratio2( Year, Month, Day ) RESULT( AU_ratio2 )
    ! Arguments
    INTEGER, INTENT(IN) :: Year 
    INTEGER, INTENT(IN) :: Month
    INTEGER, INTENT(IN) :: Day  
    ! Function result
    REAL(fp) :: AU_ratio2
    ! Local parameters
    INTEGER , PARAMETER :: N = 2
    REAL(fp), PARAMETER :: A(0:N) = (/ 1.000110_fp, 0.034221_fp, 0.000719_fp /)
    REAL(fp), PARAMETER :: B(0:N) = (/ ZERO       , 0.001280_fp, 0.000077_fp /)
    ! Local variables
    INTEGER  :: i
    REAL(fp) :: DoY, Max_DoY, t, it

    ! Determine the days-of-year
    DoY     = REAL(Day_of_Year( Day, Month, Year ), fp)
    Max_DoY = REAL(Day_of_Year( 31, 12, Year ), fp)
    
    ! Location of Earth in orbit relative to perihelion
    t = TWOPI * (DoY-ONE)/Max_DoY
    
    ! Compute the ratio term
    AU_ratio2 = ZERO
    DO i = 0, N
      it = REAL(i,fp) * t
      AU_ratio2 = AU_ratio2 + (A(i) * COS(it)) + (B(i) * SIN(it))
    END DO
    
  END FUNCTION Compute_AU_ratio2

END MODULE CRTM_GeometryInfo
