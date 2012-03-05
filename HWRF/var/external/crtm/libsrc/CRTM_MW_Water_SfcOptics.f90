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
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_MW_Water_SfcOptics

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,               ONLY: fp
  USE Message_Handler,          ONLY: SUCCESS
  USE CRTM_Parameters,          ONLY: ZERO, ONE, MAX_N_ANGLES
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
  USE CRTM_Fastem3,             ONLY: Fastem3, &
                                      Fastem3_TL, &
                                      Fastem3_AD
  USE NESDIS_OceanEM_Module,    ONLY: NESDIS_OceanEM
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Data types
  PUBLIC :: MWWSOVariables_type
  ! Science routines
  PUBLIC :: Compute_MW_Water_SfcOptics  
  PUBLIC :: Compute_MW_Water_SfcOptics_TL
  PUBLIC :: Compute_MW_Water_SfcOptics_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_MW_Water_SfcOptics.f90 6285 2010-01-12 20:29:40Z paul.vandelst@noaa.gov $'
  ! Low frequency model threshold
  REAL(fp), PARAMETER :: LOW_F_THRESHOLD = 20.0_fp ! GHz
  ! Fastem3 model defaults
  INTEGER , PARAMETER :: FASTEM_VERSION        = 3          ! Version number
  REAL(fp), PARAMETER :: INVALID_TRANSMITTANCE = -999.0_fp  ! Disable non-specular correction
  
  
  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: MWWSOVariables_type
    PRIVATE
    ! Low frequency model internal variable structure
    TYPE(LF_MWSSEM_type) :: LF_MWSSEM_Var
    ! Fastem outputs
    REAL(fp), DIMENSION(MAX_N_ANGLES) :: dEH_dTs        = ZERO
    REAL(fp), DIMENSION(MAX_N_ANGLES) :: dEH_dWindSpeed = ZERO
    REAL(fp), DIMENSION(MAX_N_ANGLES) :: dEV_dTs        = ZERO
    REAL(fp), DIMENSION(MAX_N_ANGLES) :: dEV_dWindSpeed = ZERO
  END TYPE MWWSOVariables_type

CONTAINS



!----------------------------------------------------------------------------------
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
!       Error_Status = Compute_MW_Water_SfcOptics( Surface               , &  ! Input
!                                                  GeometryInfo          , &  ! Input
!                                                  SensorIndex           , &  ! Input
!                                                  ChannelIndex          , &  ! Output     
!                                                  SfcOptics             , &  ! Output     
!                                                  MWWSOVariables        , &  ! Internal variable output
!                                                  Message_Log=Message_Log )  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Surface_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
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
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      None
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        optical properties required for the radiative
!                        transfer calculation. On input the Angle component
!                        is assumed to contain data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_SfcOptics_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       MWWSOVariables:  Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_MW_Water_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       MWWSOVariables_type
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
!
!----------------------------------------------------------------------------------

  FUNCTION Compute_MW_Water_SfcOptics( Surface     , &  ! Input
                                       GeometryInfo, &  ! Input
                                       SensorIndex , &  ! Input
                                       ChannelIndex, &  ! Input
                                       SfcOptics   , &  ! Output
                                       MWWSOV      , &  ! Internal variable output
                                       Message_Log ) &  ! Error messaging
                                     RESULT ( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics
    TYPE(MWWSOVariables_type),    INTENT(IN OUT) :: MWWSOV
    CHARACTER(*), OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_MW_Water_SfcOptics'
    ! Local variables
    INTEGER  :: i
    REAL(fp) :: Source_Azimuth_Angle
    REAL(fp) :: Emissivity(4), Reflectivity(4)


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    SfcOptics%Reflectivity = ZERO
    CALL CRTM_GeometryInfo_GetValue( GeometryInfo, Source_Azimuth_Angle = Source_Azimuth_Angle )


    ! --------------------------------------
    ! Compute the surface optical parameters
    ! --------------------------------------
    
    ! Low frequency model coupled with Fastem3
    ! ----------------------------------------
    IF( SC(SensorIndex)%Frequency(ChannelIndex) < LOW_F_THRESHOLD ) THEN
      ! Call the low frequency model
      DO i = 1, SfcOptics%n_Angles
        CALL LowFrequency_MWSSEM( SC(SensorIndex)%Frequency(ChannelIndex), &  ! Input
                                  SfcOptics%Angle(i)                     , &  ! Input
                                  Surface%Water_Temperature              , &  ! Input
                                  Surface%Salinity                       , &  ! Input
                                  Surface%Wind_Speed                     , &  ! Input
                                  SfcOptics%Emissivity(i,:)              , &  ! Output
                                  MWWSOV%LF_MWSSEM_Var                     )  ! Internal variable output
        SfcOptics%Reflectivity(i,1,i,1) = ONE-SfcOptics%Emissivity(i,1)
        SfcOptics%Reflectivity(i,2,i,2) = ONE-SfcOptics%Emissivity(i,2)
      END DO
    ELSE
      ! Call Fastem1
      DO i = 1, SfcOptics%n_Angles
        CALL Fastem1( SC(SensorIndex)%Frequency(ChannelIndex), & ! Input
                      SfcOptics%Angle(i)                     , & ! Input
                      Surface%Water_Temperature              , & ! Input
                      Surface%Wind_Speed                     , & ! Input
                      SfcOptics%Emissivity(i,:)              , & ! Output
                      MWWSOV%dEH_dWindSpeed(i)               , & ! Output
                      MWWSOV%dEV_dWindSpeed(i)                 ) ! Output
        SfcOptics%Reflectivity(i,1,i,1) = ONE-SfcOptics%Emissivity(i,1)
        SfcOptics%Reflectivity(i,2,i,2) = ONE-SfcOptics%Emissivity(i,2)
      END DO
    END IF


!    ! The Fastem3 model
!    ! -----------------
!    DO i = 1, SfcOptics%n_Angles
!      CALL Fastem3( SC(SensorIndex)%Frequency(ChannelIndex), & ! Input
!                    SfcOptics%Angle(i)                     , & ! Input
!                    Source_Azimuth_Angle                   , & ! Input
!                    Surface%Water_Temperature              , & ! Input
!                    Surface%Wind_Speed                     , & ! Input
!                    Surface%Wind_Direction                 , & ! Input
!                    INVALID_TRANSMITTANCE                  , & ! Input
!                    FASTEM_VERSION                         , & ! Input
!                    SfcOptics%Emissivity(i,:)              , & ! Output
!                    Reflectivity                             ) ! Output
!      SfcOptics%Reflectivity(i,1,i,1) = Reflectivity(1) 
!      SfcOptics%Reflectivity(i,2,i,2) = Reflectivity(2) 
!      SfcOptics%Reflectivity(i,3,i,3) = Reflectivity(3)
!      SfcOptics%Reflectivity(i,4,i,4) = Reflectivity(4)
!    END DO


!    ! The Fastem1 model
!    ! -----------------
!    DO i = 1, SfcOptics%n_Angles
!      CALL Fastem1( SC(SensorIndex)%Frequency(ChannelIndex), & ! Input
!                    SfcOptics%Angle(i)                     , & ! Input
!                    Surface%Water_Temperature              , & ! Input
!                    Surface%Wind_Speed                     , & ! Input
!                    SfcOptics%Emissivity(i,:)              , & ! Output
!                    MWWSOV%dEH_dWindSpeed(i)               , & ! Output
!                    MWWSOV%dEV_dWindSpeed(i)                 ) ! Output
!      SfcOptics%Reflectivity(i,1,i,1) = ONE-SfcOptics%Emissivity(i,1)
!      SfcOptics%Reflectivity(i,2,i,2) = ONE-SfcOptics%Emissivity(i,2)
!    END DO


!    ! The NESDIS OceanEM model
!    ! ------------------------
!    DO i = 1, SfcOptics%n_Angles
!      CALL NESDIS_OceanEM( SC(SensorIndex)%Frequency(ChannelIndex), & ! Input, GHz
!                           SfcOptics%Angle(i),          & ! Input, Degree
!                           Surface%Water_Temperature,   & ! Input, K
!                           Surface%Wind_Speed ,         & ! Input, m/s
!                           Surface%Salinity,            & ! Input, 1/Thousand
!                           SfcOptics%Emissivity(i,2),   & ! Output, H component
!                           SfcOptics%Emissivity(i,1),   & ! Output, V component
!                           MWWSOV%dEH_dTs(i),           & ! Output
!                           MWWSOV%dEH_dWindSpeed(i),    & ! Output
!                           MWWSOV%dEV_dTs(i),           & ! Output
!                           MWWSOV%dEV_dWindSpeed(i)     ) ! Output
!      SfcOptics%Reflectivity(i,1,i,1) = ONE-SfcOptics%Emissivity(i,1)
!      SfcOptics%Reflectivity(i,2,i,2) = ONE-SfcOptics%Emissivity(i,2)
!    END DO

  END FUNCTION Compute_MW_Water_SfcOptics


!----------------------------------------------------------------------------------
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
!       Error_Status = Compute_MW_Water_SfcOptics_TL( Surface               , &  ! Input
!                                                     SfcOptics             , &  ! Input
!                                                     Surface_TL            , &  ! Input
!                                                     GeometryInfo          , &  ! Input
!                                                     SensorIndex           , &  ! Input
!                                                     ChannelIndex          , &  ! Output     
!                                                     SfcOptics_TL          , &  ! Output
!                                                     MWWSOVariables        , &  ! Internal variable input
!                                                     Message_Log=Message_Log )  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Surface_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Surface_TL:      CRTM_Surface structure containing the tangent-linear 
!                        surface state data.
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
!       MWWSOVariables:  Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_MW_Water_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       MWWSOVariables_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      None
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
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
!----------------------------------------------------------------------------------

  FUNCTION Compute_MW_Water_SfcOptics_TL( Surface     , &  ! Input
                                          SfcOptics   , &  ! Input     
                                          Surface_TL  , &  ! Input
                                          GeometryInfo, &  ! Input
                                          SensorIndex , &  ! Input
                                          ChannelIndex, &  ! Input
                                          SfcOptics_TL, &  ! Output     
                                          MWWSOV      , &  ! Internal variable input
                                          Message_Log ) &  ! Error messaging 
                                        RESULT ( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface_TL
    TYPE(CRTM_SfcOptics_type),    INTENT(IN)     :: SfcOptics
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics_TL
    TYPE(MWWSOVariables_type),    INTENT(IN)     :: MWWSOV
    CHARACTER(*), OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_MW_Water_SfcOptics_TL'
    ! Local variables
    INTEGER :: i
    REAL(fp) :: Source_Azimuth_Angle
    REAL(fp) :: Reflectivity(4), Reflectivity_TL(4)
    REAL(fp) :: Transmittance_TL


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    SfcOptics_TL%Reflectivity = ZERO
    CALL CRTM_GeometryInfo_GetValue( GeometryInfo, Source_Azimuth_Angle = Source_Azimuth_Angle )


    ! -----------------------------------------------------
    ! Compute the tangent-linear surface optical parameters
    ! -----------------------------------------------------
    
    ! Low frequency model coupled with Fastem3
    ! ----------------------------------------
    IF( SC(SensorIndex)%Frequency(ChannelIndex) < LOW_F_THRESHOLD ) THEN
      ! Call the low frequency model
      DO i = 1, SfcOptics%n_Angles
        CALL LowFrequency_MWSSEM_TL( SC(SensorIndex)%Frequency(ChannelIndex), &  ! Input
                                     SfcOptics%Angle(i)                     , &  ! Input
                                     Surface%Water_Temperature              , &  ! FWD Input
                                     Surface%Salinity                       , &  ! FWD Input
                                     Surface%Wind_Speed                     , &  ! FWD Input
                                     Surface_TL%Water_Temperature           , &  ! TL  Input
                                     Surface_TL%Salinity                    , &  ! TL  Input
                                     Surface_TL%Wind_Speed                  , &  ! TL  Input
                                     SfcOptics_TL%Emissivity(i,:)           , &  ! TL  Output
                                     MWWSOV%LF_MWSSEM_Var                     )  ! Internal variable input
        SfcOptics_TL%Reflectivity(i,1,i,1) = -SfcOptics_TL%Emissivity(i,1)
        SfcOptics_TL%Reflectivity(i,2,i,2) = -SfcOptics_TL%Emissivity(i,2)
      END DO
    ELSE
      ! Call Fastem1
      DO i = 1, SfcOptics%n_Angles
        SfcOptics_TL%Emissivity(i,2) = (MWWSOV%dEH_dTs(i)*Surface_TL%Water_Temperature) + &
                                       (MWWSOV%dEH_dWindSpeed(i)*Surface_TL%Wind_Speed)
        SfcOptics_TL%Emissivity(i,1) = (MWWSOV%dEV_dTs(i)*Surface_TL%Water_Temperature) + &
                                       (MWWSOV%dEV_dWindSpeed(i)*Surface_TL%Wind_Speed)
        SfcOptics_TL%Reflectivity(i,1,i,1) = -SfcOptics_TL%Emissivity(i,1)
        SfcOptics_TL%Reflectivity(i,2,i,2) = -SfcOptics_TL%Emissivity(i,2)
      END DO
    END IF


!    ! The Fastem3 model
!    ! -----------------
!    DO i = 1, SfcOptics%n_Angles
!      CALL Fastem3_TL( SC(SensorIndex)%Frequency(ChannelIndex), & ! Input
!                       SfcOptics%Angle(i)                     , & ! Input
!                       Source_Azimuth_Angle                   , & ! Input
!                       Surface%Water_Temperature              , & ! Input
!                       Surface%Wind_Speed                     , & ! Input
!                       Surface%Wind_Direction                 , & ! Input
!                       INVALID_TRANSMITTANCE                  , & ! Input
!                       Surface_TL%Water_Temperature           , & ! Input
!                       Surface_TL%Wind_Speed                  , & ! Input
!                       Surface_TL%Wind_Direction              , & ! Input
!                       Transmittance_TL                       , & ! Input
!                       FASTEM_VERSION                         , & ! Input
!                       SfcOptics_TL%Emissivity(i,:)           , & ! Output
!                       Reflectivity_TL                          ) ! Output
!      SfcOptics_TL%Reflectivity(i,1,i,1) = Reflectivity_TL(1) 
!      SfcOptics_TL%Reflectivity(i,2,i,2) = Reflectivity_TL(2) 
!      SfcOptics_TL%Reflectivity(i,3,i,3) = Reflectivity_TL(3)
!      SfcOptics_TL%Reflectivity(i,4,i,4) = Reflectivity_TL(4)
!    END DO

    
!    ! The NESDIS OceanEM or Fastem1 model
!    ! -----------------------------------
!    DO i = 1, SfcOptics%n_Angles
!      SfcOptics_TL%Emissivity(i,2) = (MWWSOV%dEH_dTs(i)*Surface_TL%Water_Temperature) + &
!                                     (MWWSOV%dEH_dWindSpeed(i)*Surface_TL%Wind_Speed)
!      SfcOptics_TL%Emissivity(i,1) = (MWWSOV%dEV_dTs(i)*Surface_TL%Water_Temperature) + &
!                                     (MWWSOV%dEV_dWindSpeed(i)*Surface_TL%Wind_Speed)
!      SfcOptics_TL%Reflectivity(i,1,i,1) = -SfcOptics_TL%Emissivity(i,1)
!      SfcOptics_TL%Reflectivity(i,2,i,2) = -SfcOptics_TL%Emissivity(i,2)
!    END DO

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
!       Error_Status = Compute_MW_Water_SfcOptics_AD( Surface               , &  ! Input
!                                                     SfcOptics             , &  ! Input     
!                                                     SfcOptics_AD          , &  ! Input     
!                                                     GeometryInfo          , &  ! Input
!                                                     SensorIndex           , &  ! Input
!                                                     ChannelIndex          , &  ! Output     
!                                                     Surface_AD            , &  ! Output
!                                                     MWWSOVariables        , &  ! Internal variable input
!                                                     Message_Log=Message_Log )  ! Error messaging 
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
!       MWWSOVariables:  Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_MW_Water_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       MWWSOVariables_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      None
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
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

  FUNCTION Compute_MW_Water_SfcOptics_AD( Surface     , &  ! Input
                                          SfcOptics   , &  ! Input     
                                          SfcOptics_AD, &  ! Input
                                          GeometryInfo, &  ! Input
                                          SensorIndex , &  ! Input
                                          ChannelIndex, &  ! Input
                                          Surface_AD  , &  ! Output     
                                          MWWSOV      , &  ! Internal variable input
                                          Message_Log ) &  ! Error messaging 
                                        RESULT ( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_SfcOptics_type),    INTENT(IN)     :: SfcOptics
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics_AD
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_Surface_type),      INTENT(IN OUT) :: Surface_AD
    TYPE(MWWSOVariables_type),    INTENT(IN)     :: MWWSOV
    CHARACTER(*), OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_MW_Water_SfcOptics_AD'
    ! Local variables
    INTEGER :: i, j
    REAL(fp) :: Source_Azimuth_Angle
    REAL(fp) :: Emissivity(4), Reflectivity(4), Reflectivity_AD(4)
    REAL(fp) :: Transmittance_AD

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    Transmittance_AD = ZERO
    CALL CRTM_GeometryInfo_GetValue( GeometryInfo, Source_Azimuth_Angle = Source_Azimuth_Angle )


    ! ----------------------------------------------
    ! Compute the adjoint surface optical parameters
    ! ----------------------------------------------
    
    ! Low frequency model coupled with Fastem3
    ! ----------------------------------------
    IF( SC(SensorIndex)%Frequency(ChannelIndex) < LOW_F_THRESHOLD ) THEN
      ! Call the low frequency model
      DO i = 1, SfcOptics%n_Angles
        SfcOptics_AD%Emissivity(i,1) = SfcOptics_AD%Emissivity(i,1)-SfcOptics_AD%Reflectivity(i,1,i,1)
        SfcOptics_AD%Emissivity(i,2) = SfcOptics_AD%Emissivity(i,2)-SfcOptics_AD%Reflectivity(i,2,i,2)
        CALL LowFrequency_MWSSEM_AD( SC(SensorIndex)%Frequency(ChannelIndex), &  ! Input
                                     SfcOptics%Angle(i)                     , &  ! Input
                                     Surface%Water_Temperature              , &  ! FWD Input
                                     Surface%Salinity                       , &  ! FWD Input
                                     Surface%Wind_Speed                     , &  ! FWD Input
                                     SfcOptics_AD%Emissivity(i,:)           , &  ! AD  Input
                                     Surface_AD%Water_Temperature           , &  ! AD  Output
                                     Surface_AD%Salinity                    , &  ! AD  Output
                                     Surface_AD%Wind_Speed                  , &  ! AD  Output
                                     MWWSOV%LF_MWSSEM_Var                     )  ! Internal variable input
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
                                        (MWWSOV%dEV_dTs(i)*SfcOptics_AD%Emissivity(i,1))
        Surface_AD%Wind_Speed         = Surface_AD%Wind_Speed + &
                                        (MWWSOV%dEV_dWindSpeed(i)*SfcOptics_AD%Emissivity(i,1))
        SfcOptics_AD%Emissivity(i,1)  = ZERO
        ! Horizontal polarization component
        Surface_AD%Water_Temperature  = Surface_AD%Water_Temperature + &
                                        (MWWSOV%dEH_dTs(i)*SfcOptics_AD%Emissivity(i,2))
        Surface_AD%Wind_Speed         = Surface_AD%Wind_Speed + &
                                        (MWWSOV%dEH_dWindSpeed(i)*SfcOptics_AD%Emissivity(i,2))
        SfcOptics_AD%Emissivity(i,2)  = ZERO
      END DO
    END IF
    

!    ! The Fastem3 model
!    ! -----------------
!    DO i = 1, SfcOptics%n_Angles
!      DO j = 1, 4
!        Reflectivity_AD(j) = SfcOptics_AD%Reflectivity(i,j,i,j)
!      END DO
!      CALL Fastem3_AD( SC(SensorIndex)%Frequency(ChannelIndex), & ! Input
!                       SfcOptics%Angle(i),                     , & ! Input
!                       Source_Azimuth_Angle,                   , & ! Input
!                       Surface%Water_Temperature,              , & ! Input
!                       Surface%Wind_Speed ,                    , & ! Input
!                       Surface%Wind_Direction,                 , & ! Input
!                       INVALID_TRANSMITTANCE,                  , & ! Input
!                       Emissivity,                             , & ! Output
!                       Reflectivity,                           , & ! Output
!                       FASTEM_VERSION,                         , & ! Input
!                       SfcOptics_AD%Emissivity(i,:),           , & ! Input/Output
!                       Reflectivity_AD,                        , & ! Input/Output
!                       Surface_AD%Water_Temperature,           , & ! Output
!                       Surface_AD%Wind_Speed,                  , & ! Output
!                       Surface_AD%Wind_Direction,              , & ! Output
!                       Transmittance_AD                          ) ! Output
!    END DO
    
    
        
!    ! The NESDIS OceanEM or Fastem1 model
!    ! -----------------------------------
!    DO i = SfcOptics%n_Angles, 1, -1
!      DO j = 1, 2
!        SfcOptics_AD%Emissivity(i,j) = SfcOptics_AD%Emissivity(i,j) - &
!                                       SfcOptics_AD%Reflectivity(i,j,i,j)
!        SfcOptics_AD%Reflectivity(i,j,i,j) = ZERO
!      END DO
!      ! Vertical polarisation component
!      Surface_AD%Water_Temperature  = Surface_AD%Water_Temperature + &
!                                      (MWWSOV%dEV_dTs(i)*SfcOptics_AD%Emissivity(i,1))
!      Surface_AD%Wind_Speed         = Surface_AD%Wind_Speed + &
!                                      (MWWSOV%dEV_dWindSpeed(i)*SfcOptics_AD%Emissivity(i,1))
!      SfcOptics_AD%Emissivity(i,1)  = ZERO
!      ! Horizontal polarization component
!      Surface_AD%Water_Temperature  = Surface_AD%Water_Temperature + &
!                                      (MWWSOV%dEH_dTs(i)*SfcOptics_AD%Emissivity(i,2))
!      Surface_AD%Wind_Speed         = Surface_AD%Wind_Speed + &
!                                      (MWWSOV%dEH_dWindSpeed(i)*SfcOptics_AD%Emissivity(i,2))
!      SfcOptics_AD%Emissivity(i,2)  = ZERO
!    END DO

    SfcOptics_AD%Reflectivity = ZERO
    
  END FUNCTION Compute_MW_Water_SfcOptics_AD

END MODULE CRTM_MW_Water_SfcOptics
