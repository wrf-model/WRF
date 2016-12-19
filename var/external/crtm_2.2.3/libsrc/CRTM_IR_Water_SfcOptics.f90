!
! CRTM_IR_Water_SfcOptics
!
! Module to compute the surface optical properties for WATER surfaces at
! infrared frequencies required for determining the WATER surface
! contribution to the radiative transfer.
!
! This module is provided to allow developers to "wrap" their existing
! codes inside the provided functions to simplify integration into
! the main CRTM_SfcOptics module.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 25-Jun-2005
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_IR_Water_SfcOptics

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,               ONLY: fp
  USE Message_Handler,          ONLY: SUCCESS, Display_Message
  USE CRTM_Parameters,          ONLY: ZERO, ONE, TWO, FOUR, PI, DEGREES_TO_RADIANS
  USE CRTM_SpcCoeff,            ONLY: SC, SpcCoeff_IsSolar
  USE CRTM_Surface_Define,      ONLY: CRTM_Surface_type
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type
  USE CRTM_SfcOptics_Define,    ONLY: CRTM_SfcOptics_type
  USE CRTM_IRSSEM,              ONLY: IRSSEM_type=>iVar_type, &
                                      CRTM_Compute_IRSSEM, &
                                      CRTM_Compute_IRSSEM_TL, &
                                      CRTM_Compute_IRSSEM_AD
  USE CRTM_IRwaterCoeff,        ONLY: IRwaterC
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Data types
  PUBLIC :: iVar_type
  ! Science routines
  PUBLIC :: Compute_IR_Water_SfcOptics
  PUBLIC :: Compute_IR_Water_SfcOptics_TL
  PUBLIC :: Compute_IR_Water_SfcOptics_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_IR_Water_SfcOptics.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'
  ! Coefficients for Sigma**2 in the Cox & Munk slope probability density function
  REAL(fp), PARAMETER :: CM_1 = 0.003_fp, CM_2 = 5.12e-3_fp


  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: iVar_type
    PRIVATE
    ! Variables in routines rough sea BRDF
    REAL(fp) :: pdf            ! slope distribution function
    REAL(fp) :: W              ! BRDF = W*pdf
    REAL(fp) :: tan2_theta_f   ! tan(theta_f)**2
    ! IRSSEM data structure
    TYPE(IRSSEM_type) :: IRSSEM
  END TYPE iVar_type


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_IR_Water_SfcOptics
!
! PURPOSE:
!       Function to compute the surface emissivity and reflectivity at infrared
!       frequencies over a water surface.
!
!       This function is a wrapper for third party code.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_IR_Water_SfcOptics( &
!                        Surface     , &  ! Input
!                        GeometryInfo, &  ! Input
!                        SensorIndex , &  ! Input
!                        ChannelIndex, &  ! Output
!                        SfcOptics   , &  ! Output
!                        iVar          )  ! Internal variable output
!
! INPUTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Surface_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       GeometryInfo:    CRTM_GeometryInfo structure containing the
!                        view geometry information.
!                        UNITS:      N/A
!                        TYPE:       CRTM_GeometryInfo_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SensorIndex:     Sensor index id. This is a unique index associated
!                        with a (supported) sensor used to access the
!                        shared coefficient data for a particular sensor.
!                        See the ChannelIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:    Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data for a particular sensor's
!                        channel.
!                        See the SensorIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        optical properties required for the radiative
!                        transfer calculation. On input the Angle component
!                        is assumed to contain data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       iVar:            Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_IR_Water_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       iVar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SfcOptics argument is IN OUT rather
!       than just OUT as it is assumed to contain some data upon input.
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION Compute_IR_Water_SfcOptics( &
    Surface     , &  ! Input
    GeometryInfo, &  ! Input
    SensorIndex , &  ! Input
    ChannelIndex, &  ! Input
    SfcOptics   , &  ! Output
    iVar        ) &  ! Internal variable output
  RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics
    TYPE(iVar_type),              INTENT(IN OUT) :: iVar
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_IR_Water_SfcOptics'
    ! Local variables
    INTEGER  :: j, nZ, iZ
    REAL(fp) :: Frequency
    REAL(fp) :: Relative_Azimuth_Radian, brdf


    ! Set up
    Error_Status = SUCCESS
    ! ...Short name for angle dimensions
    nZ = SfcOptics%n_Angles
    iZ = SfcOptics%Index_Sat_Ang
    ! ...Retrieve data from structures
    Frequency = SC(SensorIndex)%Wavenumber(ChannelIndex)


    ! Compute IR sea surface emissivity
    Error_Status = CRTM_Compute_IRSSEM( &
                     IRwaterC                    , &  ! Input model coefficients
                     Surface%Wind_Speed          , &  ! Input
                     Frequency                   , &  ! Input
                     SfcOptics%Angle(1:nZ)       , &  ! Input
                     iVar%IRSSEM                 , &  ! Internal variable output
                     SfcOptics%Emissivity(1:nZ,1)  )  ! Output
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error computing IR sea surface emissivity', &
                            Error_Status )
      RETURN
    END IF


    ! Compute the solar direct BRDF
    IF ( SpcCoeff_IsSolar(SC(SensorIndex), ChannelIndex=ChannelIndex) ) THEN

      IF( GeometryInfo%Source_Zenith_Radian < PI/TWO ) THEN
        Relative_Azimuth_Radian = GeometryInfo%Sensor_Azimuth_Radian - &
                                  GeometryInfo%Source_Azimuth_Radian
        CALL BRDF_Rough_Sea(SC(SensorIndex)%Wavenumber(ChannelIndex), &
                            GeometryInfo%Source_Zenith_Radian,     &
                            Relative_Azimuth_Radian,               &
                            GeometryInfo%Sensor_Zenith_Radian,     &
                            Surface%Wind_Speed,                    &
                            brdf,                                  &
                            iVar)
        SfcOptics%Direct_Reflectivity(1:nZ,1) = brdf
      ELSE
        SfcOptics%Direct_Reflectivity(1:nZ,1) = ZERO
      END IF

    END IF

    ! Surface reflectance (currently assumed to be specular ALWAYS)
    DO j = 1, nZ
      SfcOptics%Reflectivity(j,1,j,1) = ONE-SfcOptics%Emissivity(j,1)
    END DO

  END FUNCTION Compute_IR_Water_SfcOptics


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_IR_Water_SfcOptics_TL
!
! PURPOSE:
!       Function to compute the tangent-linear surface emissivity and
!       reflectivity at infrared frequencies over a water surface.
!
!       This function is a wrapper for third party code.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_IR_Water_SfcOptics_TL( &
!                        Surface     , &
!                        SfcOptics   , &
!                        Surface_TL  , &
!                        GeometryInfo, &
!                        SensorIndex , &
!                        ChannelIndex, &
!                        SfcOptics_TL, &
!                        iVar          )
!
! INPUTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Surface_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Surface_TL:      CRTM_Surface structure containing the tangent-linear
!                        surface state data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Surface_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        optical properties required for the radiative
!                        transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       GeometryInfo:    CRTM_GeometryInfo structure containing the
!                        view geometry information.
!                        UNITS:      N/A
!                        TYPE:       CRTM_GeometryInfo_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SensorIndex:     Sensor index id. This is a unique index associated
!                        with a (supported) sensor used to access the
!                        shared coefficient data for a particular sensor.
!                        See the ChannelIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:    Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data for a particular sensor's
!                        channel.
!                        See the SensorIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       iVar:            Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_IR_Water_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       iVar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       SfcOptics_TL:    CRTM_SfcOptics structure containing the tangent-linear
!                        surface optical properties required for the tangent-
!                        linear radiative transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SfcOptics_TL argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input.
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION Compute_IR_Water_SfcOptics_TL( &
    Surface     , &  ! Input
    SfcOptics   , &  ! Input
    Surface_TL  , &  ! Input
    GeometryInfo, &  ! Input
    SensorIndex , &  ! Input
    ChannelIndex, &  ! Input
    SfcOptics_TL, &  ! Output
    iVar        ) &  ! Internal variable input
  RESULT ( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface_TL
    TYPE(CRTM_SfcOptics_type),    INTENT(IN)     :: SfcOptics
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics_TL
    TYPE(iVar_type),              INTENT(IN)     :: iVar
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_IR_Water_SfcOptics_TL'
    ! Local variables
    INTEGER  :: j, nZ, iZ
    REAL(fp) :: Relative_Azimuth_Radian, brdf_TL

    ! Set up
    Error_Status = SUCCESS
    ! ...Short name for angle dimensions
    nZ = SfcOptics%n_Angles
    iZ = SfcOptics%Index_Sat_Ang

    ! Compute tangent-linear IR sea surface emissivity
    Error_Status = CRTM_Compute_IRSSEM_TL( &
                     IRwaterC                       , &  ! Input model coefficients
                     Surface_TL%Wind_Speed          , &  ! Input
                     iVar%IRSSEM                    , &  ! Internal variable input
                     SfcOptics_TL%Emissivity(1:nZ,1)  )  ! Output
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error computing Tangent_linear IR sea surface emissivity', &
                            Error_Status  )
      RETURN
    END IF


    ! Compute the tangent-linear solar direct BRDF
    IF ( SpcCoeff_IsSolar(SC(SensorIndex), ChannelIndex=ChannelIndex) ) THEN

      IF( GeometryInfo%Source_Zenith_Radian < PI/TWO ) THEN
        Relative_Azimuth_Radian = GeometryInfo%Sensor_Azimuth_Radian - &
                                  GeometryInfo%Source_Azimuth_Radian
        CALL BRDF_Rough_Sea_TL(Surface%Wind_Speed,                    &
                               Surface_TL%Wind_Speed,                 &
                               brdf_TL,                               &
                               iVar)
        SfcOptics_TL%Direct_Reflectivity(1:nZ,1) = brdf_TL
      ELSE
        SfcOptics_TL%Direct_Reflectivity(1:nZ,1) = ZERO
      END IF

    END IF

    ! Surface reflectance (currently assumed to be specular ALWAYS)
    DO j = 1, nZ
      SfcOptics_TL%Reflectivity(j,1,j,1) = -SfcOptics_TL%Emissivity(j,1)
    END DO

  END FUNCTION Compute_IR_Water_SfcOptics_TL


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_IR_Water_SfcOptics_AD
!
! PURPOSE:
!       Function to compute the adjoint surface emissivity and
!       reflectivity at infrared frequencies over a water surface.
!
!       This function is a wrapper for third party code.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_IR_Water_SfcOptics_AD( &
!                        Surface     , &
!                        SfcOptics   , &
!                        SfcOptics_AD, &
!                        GeometryInfo, &
!                        SensorIndex , &
!                        ChannelIndex, &
!                        Surface_AD  , &
!                        iVar          )
!
! INPUTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Surface_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        optical properties required for the radiative
!                        transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SfcOptics_AD:    CRTM_SfcOptics structure containing the adjoint
!                        surface optical properties required for the adjoint
!                        radiative transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       GeometryInfo:    CRTM_GeometryInfo structure containing the
!                        view geometry information.
!                        UNITS:      N/A
!                        TYPE:       CRTM_GeometryInfo_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SensorIndex:     Sensor index id. This is a unique index associated
!                        with a (supported) sensor used to access the
!                        shared coefficient data for a particular sensor.
!                        See the ChannelIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:    Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data for a particular sensor's
!                        channel.
!                        See the SensorIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       iVar:            Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_IR_Water_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       iVar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Surface_AD:      CRTM_Surface structure containing the adjoint
!                        surface state data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Surface_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the input SfcOptics_AD argument is IN OUT rather
!       than just OUT. This is necessary because components of this argument
!       may need to be zeroed out upon output.
!
!       Note the INTENT on the output Surface_AD argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input.
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION Compute_IR_Water_SfcOptics_AD( &
    Surface     , &  ! Input
    SfcOptics   , &  ! Input
    SfcOptics_AD, &  ! Input
    GeometryInfo, &  ! Input
    SensorIndex , &  ! Input
    ChannelIndex, &  ! Input
    Surface_AD  , &  ! Output
    iVar        ) &  ! Internal variable input
  RESULT ( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_SfcOptics_type),    INTENT(IN)     :: SfcOptics
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics_AD
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_Surface_type),      INTENT(IN OUT) :: Surface_AD
    TYPE(iVar_type),              INTENT(IN)     :: iVar
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_IR_Water_SfcOptics_AD'
    ! Local variables
    INTEGER  :: j, nZ, iZ
    REAL(fp) :: Relative_Azimuth_Radian, brdf_AD

    ! Set up
    Error_Status = SUCCESS
    ! ...Short name for angle dimensions
    nZ = SfcOptics%n_Angles
    iZ = SfcOptics%Index_Sat_Ang

    ! Surface reflectance (currently assumed to be specular ALWAYS)
    DO j = nZ, 1, -1
      SfcOptics_AD%Emissivity(j,1) = SfcOptics_AD%Emissivity(j,1) - &
                                     SfcOptics_AD%Reflectivity(j,1,j,1)
      SfcOptics_AD%Reflectivity(j,1,j,1) = ZERO
    END DO

    ! Solar direct BRDF
    IF ( SpcCoeff_IsSolar(SC(SensorIndex), ChannelIndex=ChannelIndex) ) THEN

      IF( GeometryInfo%Source_Zenith_Radian < PI/TWO ) THEN

        Relative_Azimuth_Radian = GeometryInfo%Sensor_Azimuth_Radian -   &
                                  GeometryInfo%Source_Azimuth_Radian

        brdf_AD = SUM(SfcOptics_AD%Direct_Reflectivity(1:nZ,1))
        SfcOptics_AD%Direct_Reflectivity(1:nZ,1) = ZERO
        CALL BRDF_Rough_Sea_AD(Surface%Wind_Speed,                    &
                               brdf_AD,                               &
                               Surface_AD%Wind_Speed,                 &
                               iVar)
      END IF
      SfcOptics_AD%Direct_Reflectivity(1:nZ,1) = ZERO

    END IF

    ! Compute sdjoint IRSSEM sea surface emissivity
    Error_Status = CRTM_Compute_IRSSEM_AD( &
                     IRwaterC                       , &  ! Input model coefficients
                     SfcOptics_AD%Emissivity(1:nZ,1), &  ! Input
                     iVar%IRSSEM                    , &  ! Internal Variable Input
                     Surface_AD%Wind_Speed            )  ! Output
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error computing Adjoint IR sea surface emissivity', &
                            Error_Status  )
      RETURN
    END IF

  END FUNCTION Compute_IR_Water_SfcOptics_AD


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  !--------------------------------------------------------------------
  ! Compute rough sea Bi-directional Reflectance Distribution Function (BRDF)
  ! for IR solar reflection
  !   Inputs:
  !      Frequency - Frequency (cm-1)
  !      theta_s - sun zenith angle (Radian)
  !      dphi - relative sun azimuth agnle, relative to the senosr's azimuth angle (Radian)
  !      theta_r - senor zenith angle (Radian)
  !      Wind_Speed - wind speed (m/s)
  !
  !        note: dphi is such defined that if the observation direction and the sun direction
  !              ara in the same vertical plane, then  dphi = 180 degree
  !
  !                              sun   Zenith    sensor
  !                               \       |      /
  !                               theta_s | Theta_r
  !                                 \     |    /
  !                                  \    |   /
  !                                   \   |  /
  !                                    \  | /
  !                                     \ |/
  !                             -------------------------
  !
  !  output:
  !      brdf - value of the Bi-directional Reflectance Distribution Function at
  !             the given condition.
  !  In/out:
  !     iVar -  Structure containing internal variables required for
  !               subsequent tangent-linear or adjoint model calls.
  !
  !         Written by Y. Han, May 5, 2009
  !--------------------------------------------------------------------

  SUBROUTINE BRDF_Rough_Sea(Frequency, theta_s, dphi, theta_r, Wind_Speed, &
                            brdf, iVar)
    REAL(fp), INTENT(IN)  :: Frequency
    REAL(fp), INTENT(IN)  :: theta_s
    REAL(fp), INTENT(IN)  :: dphi
    REAL(fp), INTENT(IN)  :: theta_r
    REAL(fp), INTENT(IN)  :: Wind_Speed
    REAL(fp), INTENT(OUT) :: brdf
    TYPE(iVar_type), INTENT(IN OUT) :: iVar

    ! LOCAL
    REAL(fp), PARAMETER :: MIN_THETA = 1.0e-15_fp
    REAL(fp) :: sin_theta_s, cos_theta_s, sin_theta_r, cos_theta_r, sin2_theta_s, &
                sin2_theta_r, cos_dphi, CosSum2, sec4_theta_f, &
                cos2_alpha, cos_alpha, alpha, rho

    ! various intermidiate variables
    sin_theta_s     = SIN(theta_s)
    cos_theta_s     = MAX(COS(theta_s), MIN_THETA)   ! make sure COS(theta_s) > 0
    sin_theta_r     = SIN(theta_r)
    cos_theta_r     = MAX(COS(theta_r), MIN_THETA)   ! make sure COS(theta_r) > 0
    sin2_theta_s    = sin_theta_s*sin_theta_s
    sin2_theta_r    = sin_theta_r*sin_theta_r
    cos_dphi        = COS(dphi)
    CosSum2         = (cos_theta_s + cos_theta_r)**2

    ! Compute specular reflection angle alpha
    cos2_alpha = (ONE + sin_theta_s*sin_theta_r*cos_dphi + cos_theta_s*cos_theta_r)/TWO
    cos_alpha = SQRT( cos2_alpha )
    alpha     = ACOS(MIN(cos_alpha, ONE))

    ! Compute Fresnel_reflectance
    rho = Fresnel_Reflectance(Frequency, alpha)

    ! Compute Tan(theta_f)**2, where theta_f is the zenith angle of the normal
    ! of the facet at the point of reflection.
    iVar%tan2_theta_f = (sin2_theta_s + sin2_theta_r &
               + TWO*sin_theta_s*sin_theta_r*cos_dphi) &
               / CosSum2

    ! Compute splope probability density function
    CALL Slope_pdf(iVar%tan2_theta_f, Wind_Speed, iVar%pdf)

    ! Compute BRDF
    sec4_theta_f = (iVar%tan2_theta_f + ONE)**2
    iVar%W       = PI * rho * sec4_theta_f / (FOUR*cos_theta_r*cos_theta_s)
    brdf         = iVar%W * iVar%pdf

  END SUBROUTINE BRDF_Rough_Sea

  SUBROUTINE BRDF_Rough_Sea_TL(Wind_Speed, &
                               Wind_Speed_TL, brdf_TL, iVar)
    REAL(fp), INTENT(IN)  :: Wind_Speed
    REAL(fp), INTENT(IN)  :: Wind_Speed_TL
    REAL(fp), INTENT(OUT) :: brdf_TL
    TYPE(iVar_type), INTENT(IN) :: iVar

    ! LOCAL
    REAL(fp) :: pdf_TL

    CALL Slope_pdf_TL(iVar%tan2_theta_f, Wind_Speed, iVar%pdf, Wind_Speed_TL, pdf_TL)

    brdf_TL   = iVar%W * pdf_TL

  END SUBROUTINE BRDF_Rough_Sea_TL

  SUBROUTINE BRDF_Rough_Sea_AD(Wind_Speed, &
                               brdf_AD, Wind_Speed_AD, iVar)
    REAL(fp), INTENT(IN)     :: Wind_Speed
    REAL(fp), INTENT(INOUT)  :: brdf_AD
    REAL(fp), INTENT(INOUT)  :: Wind_Speed_AD
    TYPE(iVar_type), INTENT(IN) :: iVar

    ! LOCAL
    REAL(fp) :: pdf_AD

    pdf_AD   = iVar%W * brdf_AD
    brdf_AD  = ZERO

    CALL Slope_pdf_AD(iVar%tan2_theta_f, Wind_Speed, iVar%pdf, pdf_AD, Wind_Speed_AD)

  END SUBROUTINE BRDF_Rough_Sea_AD

  ! Compute facet slope distribution function (pdf)
  !   Inputs:  tan2_theta_f  - tan(theta_f)**2
  !            Wind_Speed (m/s)
  !   Outputs: pdf

  SUBROUTINE Slope_pdf(tan2_theta_f, Wind_Speed, pdf)
    REAL(fp), INTENT(IN)   :: tan2_theta_f
    REAL(fp), INTENT(IN)   :: Wind_Speed
    REAL(fp), INTENT(OUT)  :: pdf

    ! Local
    REAL(fp) :: Sigma2

    ! Cox & Munk slope probability density function
    Sigma2 = CM_1 + CM_2*Wind_Speed
    pdf = EXP(-tan2_theta_f / Sigma2) / (PI*Sigma2)

  END SUBROUTINE Slope_pdf

  SUBROUTINE Slope_pdf_TL(tan2_theta_f, Wind_Speed, pdf, Wind_Speed_TL, pdf_TL)
    REAL(fp), INTENT(IN)   :: tan2_theta_f
    REAL(fp), INTENT(IN)   :: Wind_Speed
    REAL(fp), INTENT(IN)   :: pdf
    REAL(fp), INTENT(IN)   :: Wind_Speed_TL
    REAL(fp), INTENT(OUT)  :: pdf_TL

    ! LOCAL
    REAL(fp) :: Sigma2, Sigma2_TL

    Sigma2    = CM_1 + CM_2*Wind_Speed
    Sigma2_TL = CM_2*Wind_Speed_TL
    pdf_TL    = ( pdf*(tan2_theta_f/Sigma2 - ONE)/Sigma2 )*Sigma2_TL

  END SUBROUTINE Slope_pdf_TL

  SUBROUTINE Slope_pdf_AD(tan2_theta_f, Wind_Speed, pdf, pdf_AD, Wind_Speed_AD)
    REAL(fp), INTENT(IN)      :: tan2_theta_f
    REAL(fp), INTENT(IN)      :: Wind_Speed
    REAL(fp), INTENT(IN)      :: pdf
    REAL(fp), INTENT(IN OUT)  :: pdf_AD
    REAL(fp), INTENT(IN OUT)  :: Wind_Speed_AD

    ! LOCAL
    REAL(fp) :: Sigma2, Sigma2_AD

    Sigma2        = CM_1 + CM_2*Wind_Speed
    Sigma2_AD     = ( pdf*(tan2_theta_f/Sigma2 - ONE)/Sigma2 )*pdf_AD
    pdf_AD        = ZERO
    Wind_Speed_AD = Wind_Speed_AD + 5.12e-3_fp*Sigma2_AD

  END SUBROUTINE Slope_pdf_AD

  !---------------------------------------------------------
  ! Compute Fresnel sea surface reflectivity
  !   Inputs:
  !      Frequency - Frequency cm-1
  !      Ang_i - incident angle (Radian)
  !  output (as a function return):
  !      r - Fresnel reflectivity
  !         Written by Y. Han, May 5, 2009
  !---------------------------------------------------------
  FUNCTION Fresnel_Reflectance(Frequency, Ang_i) RESULT( r )

    REAL(fp),    INTENT(IN) :: Frequency
    REAL(fp),    INTENT(IN) :: Ang_i

    REAL(fp) :: r

    ! LOCAL
    REAL(fp) :: rh, rv
    COMPLEX(fp) :: CCos_i, CCos_t, n, z

    ! call function to compute complex refractive index
    n = Ref_Index(Frequency)

    ! Fresnel reflectivity

    z = CMPLX(SIN(Ang_i), ZERO, fp)/n
    CCos_t = SQRT(CMPLX(ONE, ZERO, fp) - z*z)

    CCos_i = CMPLX(COS(Ang_i), ZERO, fp)

    rv = ( ABS( (n*CCos_i - CCos_t) / (n*CCos_i + CCos_t) ) )**2
    rh = ( ABS( (CCos_i - n*CCos_t) / (CCos_i + n*CCos_t) ) )**2

    r = (rv + rh) / TWO

  END FUNCTION Fresnel_Reflectance

  !------------------------------------------------------------
  ! Obtain IR refractive index
  !  Input: Frequency - wavenumber cm-1, valid range 500 - 3500 cm-1
  !  Return: complex refractive index
  !------------------------------------------------------------
  FUNCTION Ref_Index(Frequency) RESULT(ref)
    REAL(fp), INTENT(IN) :: Frequency

    COMPLEX(fp) :: ref

    !-------------------------------------------------------------------------
    ! Refractive index of water from Wieliczka (1989), added
    ! salinity and chlorinity CORRECTIONS from Friedman (1969). The resolution
    ! of the Wieliczka data set is reduced to 20 cm-1. The frequancy range
    ! of the Friedman starts at 666.67 cm-1. For frequency < 666.67 cm-1
    ! the value at 666.67 is used.
    !-------------------------------------------------------------------------
    INTEGER, PARAMETER  :: nf = 151
    ! array holding wavenumbers at which the refractive indexes are given
    REAL(fp), PARAMETER :: freq(nf) = (/ &
      500.0,  520.0,  540.0,  560.0,  580.0,  600.0,  620.0,  640.0,  660.0,  680.0,&
      700.0,  720.0,  740.0,  760.0,  780.0,  800.0,  820.0,  840.0,  860.0,  880.0,&
      900.0,  920.0,  940.0,  960.0,  980.0, 1000.0, 1020.0, 1040.0, 1060.0, 1080.0,&
     1100.0, 1120.0, 1140.0, 1160.0, 1180.0, 1200.0, 1220.0, 1240.0, 1260.0, 1280.0,&
     1300.0, 1320.0, 1340.0, 1360.0, 1380.0, 1400.0, 1420.0, 1440.0, 1460.0, 1480.0,&
     1500.0, 1520.0, 1540.0, 1560.0, 1580.0, 1600.0, 1620.0, 1640.0, 1660.0, 1680.0,&
     1700.0, 1720.0, 1740.0, 1760.0, 1780.0, 1800.0, 1820.0, 1840.0, 1860.0, 1880.0,&
     1900.0, 1920.0, 1940.0, 1960.0, 1980.0, 2000.0, 2020.0, 2040.0, 2060.0, 2080.0,&
     2100.0, 2120.0, 2140.0, 2160.0, 2180.0, 2200.0, 2220.0, 2240.0, 2260.0, 2280.0,&
     2300.0, 2320.0, 2340.0, 2360.0, 2380.0, 2400.0, 2420.0, 2440.0, 2460.0, 2480.0,&
     2500.0, 2520.0, 2540.0, 2560.0, 2580.0, 2600.0, 2620.0, 2640.0, 2660.0, 2680.0,&
     2700.0, 2720.0, 2740.0, 2760.0, 2780.0, 2800.0, 2820.0, 2840.0, 2860.0, 2880.0,&
     2900.0, 2920.0, 2940.0, 2960.0, 2980.0, 3000.0, 3020.0, 3040.0, 3060.0, 3080.0,&
     3100.0, 3120.0, 3140.0, 3160.0, 3180.0, 3200.0, 3220.0, 3240.0, 3260.0, 3280.0,&
     3300.0, 3320.0, 3340.0, 3360.0, 3380.0, 3400.0, 3420.0, 3440.0, 3460.0, 3480.0,&
     3500.0/)
     ! real part of the refractive index
    REAL(fp), PARAMETER :: nr(nf) = (/ &
     1.5300, 1.5050, 1.4752, 1.4427, 1.4111, 1.3787, 1.3468, 1.3167, 1.2887, 1.2620,&
     1.2348, 1.2077, 1.1823, 1.1596, 1.1417, 1.1268, 1.1152, 1.1143, 1.1229, 1.1327,&
     1.1520, 1.1630, 1.1770, 1.1920, 1.2050, 1.2170, 1.2280, 1.2371, 1.2450, 1.2523,&
     1.2580, 1.2636, 1.2680, 1.2740, 1.2780, 1.2820, 1.2850, 1.2880, 1.2910, 1.2940,&
     1.2970, 1.3000, 1.3020, 1.3040, 1.3070, 1.3080, 1.3100, 1.3120, 1.3150, 1.3170,&
     1.3200, 1.3230, 1.3280, 1.3350, 1.3490, 1.3530, 1.3430, 1.3080, 1.2630, 1.2460,&
     1.2410, 1.2540, 1.2670, 1.2760, 1.2810, 1.2900, 1.2950, 1.3000, 1.3040, 1.3070,&
     1.3100, 1.3130, 1.3160, 1.3180, 1.3200, 1.3210, 1.3220, 1.3240, 1.3240, 1.3250,&
     1.3260, 1.3250, 1.3250, 1.3250, 1.3250, 1.3250, 1.3260, 1.3270, 1.3280, 1.3280,&
     1.3290, 1.3300, 1.3310, 1.3330, 1.3350, 1.3360, 1.3380, 1.3400, 1.3410, 1.3430,&
     1.3450, 1.3480, 1.3500, 1.3520, 1.3550, 1.3580, 1.3600, 1.3610, 1.3640, 1.3660,&
     1.3680, 1.3710, 1.3740, 1.3770, 1.3810, 1.3850, 1.3890, 1.3940, 1.3980, 1.4020,&
     1.4070, 1.4120, 1.4180, 1.4240, 1.4310, 1.4370, 1.4440, 1.4510, 1.4590, 1.4660,&
     1.4720, 1.4780, 1.4840, 1.4830, 1.4820, 1.4750, 1.4690, 1.4580, 1.4390, 1.4230,&
     1.4130, 1.3960, 1.3810, 1.3560, 1.3290, 1.2970, 1.2600, 1.2170, 1.1860, 1.1590,&
     1.1410/)
     ! imaginary part of the refractive index
    REAL(fp), PARAMETER :: ni(nf) = (/ &
     0.3874, 0.4031, 0.4149, 0.4213, 0.4243, 0.4227, 0.4180, 0.4105, 0.3997, 0.3877,&
     0.3731, 0.3556, 0.3341, 0.3079, 0.2803, 0.2489, 0.2144, 0.1776, 0.1505, 0.1243,&
     0.0970, 0.0822, 0.0693, 0.0593, 0.0517, 0.0469, 0.0432, 0.0404, 0.0384, 0.0369,&
     0.0357, 0.0353, 0.0352, 0.0348, 0.0343, 0.0340, 0.0338, 0.0334, 0.0329, 0.0330,&
     0.0329, 0.0326, 0.0323, 0.0320, 0.0318, 0.0317, 0.0316, 0.0317, 0.0323, 0.0329,&
     0.0337, 0.0352, 0.0394, 0.0454, 0.0538, 0.0711, 0.1040, 0.1239, 0.1144, 0.0844,&
     0.0539, 0.0364, 0.0264, 0.0177, 0.0145, 0.0127, 0.0117, 0.0108, 0.0104, 0.0103,&
     0.0104, 0.0105, 0.0108, 0.0114, 0.0122, 0.0129, 0.0135, 0.0143, 0.0150, 0.0155,&
     0.0156, 0.0155, 0.0152, 0.0148, 0.0140, 0.0132, 0.0123, 0.0115, 0.0106, 0.0098,&
     0.0091, 0.0085, 0.0079, 0.0073, 0.0068, 0.0064, 0.0060, 0.0056, 0.0052, 0.0049,&
     0.0046, 0.0043, 0.0041, 0.0039, 0.0038, 0.0037, 0.0036, 0.0036, 0.0037, 0.0038,&
     0.0040, 0.0042, 0.0046, 0.0050, 0.0055, 0.0061, 0.0070, 0.0080, 0.0092, 0.0106,&
     0.0124, 0.0146, 0.0174, 0.0197, 0.0259, 0.0350, 0.0429, 0.0476, 0.0541, 0.0655,&
     0.0793, 0.0921, 0.1058, 0.1232, 0.1450, 0.1656, 0.1833, 0.1990, 0.2161, 0.2267,&
     0.2369, 0.2487, 0.2606, 0.2740, 0.2819, 0.2854, 0.2826, 0.2765, 0.2637, 0.2451,&
     0.2230/)
    REAL(fp), PARAMETER :: df = freq(2) - freq(1)
    INTEGER  :: idx
    REAL(fp) :: c

    IF(Frequency < freq(1))THEN
      ref = CMPLX( nr(1), ni(1), fp )
    ELSE IF( Frequency > freq(nf) )THEN
      ref = CMPLX( nr(nf), ni(nf), fp )
    ELSE
      ! Linear interpolation
      idx = INT((Frequency - freq(1))/df) + 1  ! find the starting index
      c = (Frequency - freq(idx))/(freq(idx+1) - freq(idx))
      ref = CMPLX( nr(idx) + c*(nr(idx+1) - nr(idx)), &
                   ni(idx) + c*(ni(idx+1) - ni(idx)), &
                   fp )
    END IF

  END FUNCTION Ref_Index

END MODULE CRTM_IR_Water_SfcOptics
