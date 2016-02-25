!
! CRTM_MW_Water_SfcOptics
!
! Module to compute the surface optical properties for WATER surfaces at
! microwave frequencies required for determining the WATER surface
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

MODULE CRTM_MW_Water_SfcOptics

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,               ONLY: fp
  USE Message_Handler,          ONLY: SUCCESS
  USE CRTM_Parameters,          ONLY: SET, NOT_SET, &
                                      ZERO, ONE, &
                                      MAX_N_ANGLES, &
                                      N_STOKES => MAX_N_STOKES
  USE CRTM_SpcCoeff,            ONLY: SC
  USE CRTM_Surface_Define,      ONLY: CRTM_Surface_type
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type, &
                                      CRTM_GeometryInfo_GetValue
  USE CRTM_SfcOptics_Define,    ONLY: CRTM_SfcOptics_type
  USE CRTM_LowFrequency_MWSSEM, ONLY: LF_MWSSEM_type => iVar_type, &
                                      LowFrequency_MWSSEM, &
                                      LowFrequency_MWSSEM_TL, &
                                      LowFrequency_MWSSEM_AD
  USE CRTM_Fastem1,             ONLY: Fastem1
  USE CRTM_FastemX,             ONLY: FastemX_type => iVar_type, &
                                      Compute_FastemX,   &
                                      Compute_FastemX_TL,&
                                      Compute_FastemX_AD
  USE CRTM_MWwaterCoeff       , ONLY: MWwaterC
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
  PUBLIC :: Compute_MW_Water_SfcOptics
  PUBLIC :: Compute_MW_Water_SfcOptics_TL
  PUBLIC :: Compute_MW_Water_SfcOptics_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_MW_Water_SfcOptics.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  ! Low frequency model threshold
  REAL(fp), PARAMETER :: LOW_F_THRESHOLD = 20.0_fp ! GHz


  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: iVar_type
    PRIVATE
    ! FastemX model internal variable structure
    TYPE(FastemX_type) :: FastemX_Var
    ! Low frequency model internal variable structure
    TYPE(LF_MWSSEM_type) :: LF_MWSSEM_Var
    ! Fastem outputs
    REAL(fp), DIMENSION(MAX_N_ANGLES) :: dEH_dTs        = ZERO
    REAL(fp), DIMENSION(MAX_N_ANGLES) :: dEH_dWindSpeed = ZERO
    REAL(fp), DIMENSION(MAX_N_ANGLES) :: dEV_dTs        = ZERO
    REAL(fp), DIMENSION(MAX_N_ANGLES) :: dEV_dWindSpeed = ZERO
  END TYPE iVar_type

CONTAINS



!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_MW_Water_SfcOptics
!
! PURPOSE:
!       Function to compute the surface emissivity and reflectivity at microwave
!       frequencies over a water surface.
!
!       This function is a wrapper for third party code.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_MW_Water_SfcOptics( Surface     , &  ! Input
!                                                  GeometryInfo, &  ! Input
!                                                  SensorIndex , &  ! Input
!                                                  ChannelIndex, &  ! Input
!                                                  SfcOptics   , &  ! Output
!                                                  iVar          )  ! Internal variable output
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
!                        outside of the CRTM_MW_Water_SfcOptics module.
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

  FUNCTION Compute_MW_Water_SfcOptics( &
    Surface     , &  ! Input
    GeometryInfo, &  ! Input
    SensorIndex , &  ! Input
    ChannelIndex, &  ! Input
    SfcOptics   , &  ! Output
    iVar        ) &  ! Internal variable output
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics
    TYPE(iVar_type),              INTENT(IN OUT) :: iVar
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_MW_Water_SfcOptics'
    ! Local variables
    INTEGER  :: i, j
    REAL(fp) :: Frequency
    REAL(fp) :: Source_Azimuth_Angle, Sensor_Azimuth_Angle
    REAL(fp) :: Reflectivity(N_STOKES)


    ! Set up
    err_stat = SUCCESS
    SfcOptics%Reflectivity = ZERO
    ! ...Retrieve data from structures
    Frequency = SC(SensorIndex)%Frequency(ChannelIndex)
    CALL CRTM_GeometryInfo_GetValue( &
           GeometryInfo, &
           Source_Azimuth_Angle = Source_Azimuth_Angle, &
           Sensor_Azimuth_Angle = Sensor_Azimuth_Angle  )


    ! Compute the surface optical parameters
    IF( SfcOptics%Use_New_MWSSEM ) THEN

      ! FastemX model
      SfcOptics%Azimuth_Angle = Surface%Wind_Direction - Sensor_Azimuth_Angle
      DO i = 1, SfcOptics%n_Angles
        CALL Compute_FastemX( &
               MWwaterC                               , &  ! Input model coefficients
               Frequency                              , &  ! Input
               SfcOptics%Angle(i)                     , &  ! Input
               Surface%Water_Temperature              , &  ! Input
               Surface%Salinity                       , &  ! Input
               Surface%Wind_Speed                     , &  ! Input
               iVar%FastemX_Var                       , &  ! Internal variable output
               SfcOptics%Emissivity(i,:)              , &  ! Output
               Reflectivity                           , &  ! Output
               Azimuth_Angle = SfcOptics%Azimuth_Angle, &  ! Optional input
               Transmittance = SfcOptics%Transmittance  )  ! Optional input
        DO j = 1, N_STOKES
          SfcOptics%Reflectivity(i,j,i,j) = Reflectivity(j)
        END DO
      END DO

    ELSE

      ! Low frequency model coupled with Fastem1
      IF( Frequency < LOW_F_THRESHOLD ) THEN
        ! Call the low frequency model
        DO i = 1, SfcOptics%n_Angles
          CALL LowFrequency_MWSSEM( &
                 Frequency                , &  ! Input
                 SfcOptics%Angle(i)       , &  ! Input
                 Surface%Water_Temperature, &  ! Input
                 Surface%Salinity         , &  ! Input
                 Surface%Wind_Speed       , &  ! Input
                 SfcOptics%Emissivity(i,:), &  ! Output
                 iVar%LF_MWSSEM_Var         )  ! Internal variable output
          SfcOptics%Reflectivity(i,1,i,1) = ONE-SfcOptics%Emissivity(i,1)
          SfcOptics%Reflectivity(i,2,i,2) = ONE-SfcOptics%Emissivity(i,2)
        END DO
      ELSE
        ! Call Fastem1
        DO i = 1, SfcOptics%n_Angles
          CALL Fastem1( Frequency                , & ! Input
                        SfcOptics%Angle(i)       , & ! Input
                        Surface%Water_Temperature, & ! Input
                        Surface%Wind_Speed       , & ! Input
                        SfcOptics%Emissivity(i,:), & ! Output
                        iVar%dEH_dWindSpeed(i)   , & ! Output
                        iVar%dEV_dWindSpeed(i)     ) ! Output
          SfcOptics%Reflectivity(i,1,i,1) = ONE-SfcOptics%Emissivity(i,1)
          SfcOptics%Reflectivity(i,2,i,2) = ONE-SfcOptics%Emissivity(i,2)
        END DO
      END IF

    END IF

  END FUNCTION Compute_MW_Water_SfcOptics


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_MW_Water_SfcOptics_TL
!
! PURPOSE:
!       Function to compute the tangent-linear surface emissivity and
!       reflectivity at microwave frequencies over a water surface.
!
!       This function is a wrapper for third party code.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_MW_Water_SfcOptics_TL( Surface     , &  ! Input
!                                                     SfcOptics   , &  ! Input
!                                                     Surface_TL  , &  ! Input
!                                                     GeometryInfo, &  ! Input
!                                                     SensorIndex , &  ! Input
!                                                     ChannelIndex, &  ! Output
!                                                     SfcOptics_TL, &  ! Output
!                                                     iVar          )  ! Internal variable input
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
!                        outside of the CRTM_MW_Water_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       iVar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       SfcOptics_TL:    CRTM_SfcOptics structure containing the tangent-linear
!                        surface optical properties required for the tangent-
!                        linear radiative transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_SfcOptics_type)
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
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION Compute_MW_Water_SfcOptics_TL( &
    SfcOptics   , &  ! Input
    Surface_TL  , &  ! Input
    GeometryInfo, &  ! Input
    SensorIndex , &  ! Input
    ChannelIndex, &  ! Input
    SfcOptics_TL, &  ! Output
    iVar        ) &  ! Internal variable input
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface_TL
    TYPE(CRTM_SfcOptics_type),    INTENT(IN)     :: SfcOptics
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics_TL
    TYPE(iVar_type),              INTENT(IN)     :: iVar
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_MW_Water_SfcOptics_TL'
    ! Local variables
    INTEGER :: i, j
    REAL(fp) :: Frequency
    REAL(fp) :: Source_Azimuth_Angle, Sensor_Azimuth_Angle
    REAL(fp) :: Reflectivity_TL(N_STOKES)


    ! Set up
    err_stat = SUCCESS
    SfcOptics_TL%Reflectivity = ZERO
    ! ...Retrieve data from structures
    Frequency = SC(SensorIndex)%Frequency(ChannelIndex)
    CALL CRTM_GeometryInfo_GetValue( &
           GeometryInfo, &
           Source_Azimuth_Angle = Source_Azimuth_Angle, &
           Sensor_Azimuth_Angle = Sensor_Azimuth_Angle  )


    ! Compute the tangent-linear surface optical parameters
    IF( SfcOptics%Use_New_MWSSEM ) THEN

      ! FastemX model
      DO i = 1, SfcOptics%n_Angles
        CALL Compute_FastemX_TL( &
               MWwaterC                                    , &  ! Input model coefficients
               Surface_TL%Water_Temperature                , &  ! TL Input
               Surface_TL%Salinity                         , &  ! TL Input
               Surface_TL%Wind_Speed                       , &  ! TL Input
               iVar%FastemX_Var                            , &  ! Internal variable input
               SfcOptics_TL%Emissivity(i,:)                , &  ! TL Output
               Reflectivity_TL                             , &  ! TL Output
               Azimuth_Angle_TL = Surface_TL%Wind_Direction, &  ! Optional TL input
               Transmittance_TL = SfcOptics_TL%Transmittance )  ! Optional TL input
        DO j = 1, N_STOKES
          SfcOptics_TL%Reflectivity(i,j,i,j) = Reflectivity_TL(j)
        END DO
      END DO

    ELSE

      ! Low frequency model coupled with Fastem1
      IF( Frequency < LOW_F_THRESHOLD ) THEN
        ! Call the low frequency model
        DO i = 1, SfcOptics%n_Angles
          CALL LowFrequency_MWSSEM_TL( &
                 Surface_TL%Water_Temperature, &  ! TL  Input
                 Surface_TL%Salinity         , &  ! TL  Input
                 Surface_TL%Wind_Speed       , &  ! TL  Input
                 SfcOptics_TL%Emissivity(i,:), &  ! TL  Output
                 iVar%LF_MWSSEM_Var            )  ! Internal variable input
          SfcOptics_TL%Reflectivity(i,1,i,1) = -SfcOptics_TL%Emissivity(i,1)
          SfcOptics_TL%Reflectivity(i,2,i,2) = -SfcOptics_TL%Emissivity(i,2)
        END DO
      ELSE
        ! Call Fastem1
        DO i = 1, SfcOptics%n_Angles
          SfcOptics_TL%Emissivity(i,2) = (iVar%dEH_dTs(i)*Surface_TL%Water_Temperature) + &
                                         (iVar%dEH_dWindSpeed(i)*Surface_TL%Wind_Speed)
          SfcOptics_TL%Emissivity(i,1) = (iVar%dEV_dTs(i)*Surface_TL%Water_Temperature) + &
                                         (iVar%dEV_dWindSpeed(i)*Surface_TL%Wind_Speed)
          SfcOptics_TL%Reflectivity(i,1,i,1) = -SfcOptics_TL%Emissivity(i,1)
          SfcOptics_TL%Reflectivity(i,2,i,2) = -SfcOptics_TL%Emissivity(i,2)
        END DO
      END IF
    END IF

  END FUNCTION Compute_MW_Water_SfcOptics_TL


!----------------------------------------------------------------------------------
!
! NAME:
!       Compute_MW_Water_SfcOptics_AD
!
! PURPOSE:
!       Function to compute the adjoint surface emissivity and
!       reflectivity at microwave frequencies over a water surface.
!
!       This function is a wrapper for third party code.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_MW_Water_SfcOptics_AD( Surface     , &  ! Input
!                                                     SfcOptics   , &  ! Input
!                                                     SfcOptics_AD, &  ! Input
!                                                     GeometryInfo, &  ! Input
!                                                     SensorIndex , &  ! Input
!                                                     ChannelIndex, &  ! Output
!                                                     Surface_AD  , &  ! Output
!                                                     iVar          )  ! Internal variable input
!
! INPUT ARGUMENTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Surface_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        optical properties required for the radiative
!                        transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_SfcOptics_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SfcOptics_AD:    CRTM_SfcOptics structure containing the adjoint
!                        surface optical properties required for the adjoint
!                        radiative transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_SfcOptics_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       GeometryInfo:    CRTM_GeometryInfo structure containing the
!                        view geometry information.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_GeometryInfo_type)
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
!                        outside of the CRTM_MW_Water_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       iVar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Surface_AD:      CRTM_Surface structure containing the adjoint
!                        surface state data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Surface_type)
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
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

  FUNCTION Compute_MW_Water_SfcOptics_AD( &
    SfcOptics   , &  ! Input
    SfcOptics_AD, &  ! Input
    GeometryInfo, &  ! Input
    SensorIndex , &  ! Input
    ChannelIndex, &  ! Input
    Surface_AD  , &  ! Output
    iVar        ) &  ! Internal variable input
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_SfcOptics_type),    INTENT(IN)     :: SfcOptics
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics_AD
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_Surface_type),      INTENT(IN OUT) :: Surface_AD
    TYPE(iVar_type),              INTENT(IN)     :: iVar
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_MW_Water_SfcOptics_AD'
    ! Local variables
    INTEGER :: i, j
    REAL(fp) :: Frequency
    REAL(fp) :: Source_Azimuth_Angle, Sensor_Azimuth_Angle
    REAL(fp) :: Reflectivity_AD(N_STOKES)
    REAL(fp) :: Azimuth_Angle_AD


    ! Set up
    err_stat = SUCCESS
    ! ...Retrieve data from structures
    Frequency = SC(SensorIndex)%Frequency(ChannelIndex)
    CALL CRTM_GeometryInfo_GetValue( &
           GeometryInfo, &
           Source_Azimuth_Angle = Source_Azimuth_Angle, &
           Sensor_Azimuth_Angle = Sensor_Azimuth_Angle  )


    ! Compute the adjoint surface optical parameters
    IF( SfcOptics%Use_New_MWSSEM ) THEN

      ! FastemX model
      Azimuth_Angle_AD = ZERO
      DO i = 1, SfcOptics%n_Angles
        DO j = 1, N_STOKES
          Reflectivity_AD(j) = SfcOptics_AD%Reflectivity(i,j,i,j)
        END DO
        CALL Compute_FastemX_AD( &
               MWwaterC                                     , &  ! Input model coefficients
               SfcOptics_AD%Emissivity(i,:)                 , &  ! AD Input
               Reflectivity_ad                              , &  ! AD Input
               iVar%FastemX_Var                             , &  ! Internal variable input
               Surface_AD%Water_Temperature                 , &  ! AD Output
               Surface_AD%Salinity                          , &  ! AD Output
               Surface_AD%Wind_Speed                        , &  ! AD Output
               Azimuth_Angle_AD = Azimuth_Angle_AD          , &  ! Optional AD Output
               Transmittance_AD = SfcOptics_AD%Transmittance  )  ! Optional AD Output
      END DO
      Surface_AD%Wind_Direction = Surface_AD%Wind_Direction + Azimuth_Angle_AD

    ELSE

      ! Low frequency model coupled with Fastem1
      IF( Frequency < LOW_F_THRESHOLD ) THEN
        ! Call the low frequency model
        DO i = 1, SfcOptics%n_Angles
          SfcOptics_AD%Emissivity(i,1) = SfcOptics_AD%Emissivity(i,1)-SfcOptics_AD%Reflectivity(i,1,i,1)
          SfcOptics_AD%Emissivity(i,2) = SfcOptics_AD%Emissivity(i,2)-SfcOptics_AD%Reflectivity(i,2,i,2)
          CALL LowFrequency_MWSSEM_AD( &
                 SfcOptics_AD%Emissivity(i,:), &  ! AD  Input
                 Surface_AD%Water_Temperature, &  ! AD  Output
                 Surface_AD%Salinity         , &  ! AD  Output
                 Surface_AD%Wind_Speed       , &  ! AD  Output
                 iVar%LF_MWSSEM_Var            )  ! Internal variable input
        END DO
      ELSE
        ! Call Fastem1
        DO i = SfcOptics%n_Angles, 1, -1
          DO j = 1, 2
            SfcOptics_AD%Emissivity(i,j) = SfcOptics_AD%Emissivity(i,j) - &
                                           SfcOptics_AD%Reflectivity(i,j,i,j)
            SfcOptics_AD%Reflectivity(i,j,i,j) = ZERO
          END DO
          ! Vertical polarisation component
          Surface_AD%Water_Temperature  = Surface_AD%Water_Temperature + &
                                          (iVar%dEV_dTs(i)*SfcOptics_AD%Emissivity(i,1))
          Surface_AD%Wind_Speed         = Surface_AD%Wind_Speed + &
                                          (iVar%dEV_dWindSpeed(i)*SfcOptics_AD%Emissivity(i,1))
          SfcOptics_AD%Emissivity(i,1)  = ZERO
          ! Horizontal polarization component
          Surface_AD%Water_Temperature  = Surface_AD%Water_Temperature + &
                                          (iVar%dEH_dTs(i)*SfcOptics_AD%Emissivity(i,2))
          Surface_AD%Wind_Speed         = Surface_AD%Wind_Speed + &
                                          (iVar%dEH_dWindSpeed(i)*SfcOptics_AD%Emissivity(i,2))
          SfcOptics_AD%Emissivity(i,2)  = ZERO
        END DO
      END IF
    END IF

    SfcOptics_AD%Reflectivity = ZERO

  END FUNCTION Compute_MW_Water_SfcOptics_AD

END MODULE CRTM_MW_Water_SfcOptics
