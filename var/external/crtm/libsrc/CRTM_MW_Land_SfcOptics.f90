!
! CRTM_MW_Land_SfcOptics
!
! Module to compute the surface optical properties for LAND surfaces at
! microwave frequencies required for determining the LAND surface
! contribution to the radiative transfer.
!
! This module is provided to allow developers to "wrap" their existing
! codes inside the provided functions to simplify integration into
! the main CRTM_SfcOptics module.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 23-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_MW_Land_SfcOptics

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,               ONLY: fp
  USE Message_Handler,          ONLY: SUCCESS
  USE CRTM_Parameters,          ONLY: ZERO, ONE, MAX_N_ANGLES
  USE CRTM_SpcCoeff,            ONLY: SC
  USE CRTM_Surface_Define,      ONLY: CRTM_Surface_type
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type
  USE CRTM_SfcOptics_Define,    ONLY: CRTM_SfcOptics_type
  USE NESDIS_LandEM_Module,     ONLY: NESDIS_LandEM
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Data types
  PUBLIC :: MWLSOVariables_type
  ! Science routines
  PUBLIC :: Compute_MW_Land_SfcOptics  
  PUBLIC :: Compute_MW_Land_SfcOptics_TL
  PUBLIC :: Compute_MW_Land_SfcOptics_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_MW_Land_SfcOptics.f90 6689 2010-02-18 22:37:33Z quanhua.liu@noaa.gov $'


  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: MWLSOVariables_type
    PRIVATE
    INTEGER :: Dummy = 0
  END TYPE MWLSOVariables_type


CONTAINS



!----------------------------------------------------------------------------------
!
! NAME:
!       Compute_MW_Land_SfcOptics
!
! PURPOSE:
!       Function to compute the surface emissivity and reflectivity at microwave
!       frequencies over a land surface.
!
!       This function is a wrapper for third party code.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_MW_Land_SfcOptics( Surface               , &  ! Input
!                                                 GeometryInfo          , &  ! Input
!                                                 SensorIndex           , &  ! Input
!                                                 ChannelIndex          , &  ! Output     
!                                                 SfcOptics             , &  ! Output
!                                                 MWLSOVariables        , &  ! Internal variable output
!                                                 Message_Log=Message_Log )  ! Error messaging 
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
!       MWLSOVariables:  Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_MW_Land_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       MWLSOVariables_type
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
!----------------------------------------------------------------------------------

  FUNCTION Compute_MW_Land_SfcOptics( Surface     , &  ! Input
                                      GeometryInfo, &  ! Input
                                      SensorIndex , &  ! Input
                                      ChannelIndex, &  ! Input
                                      SfcOptics   , &  ! Output
                                      MWLSOV      , &  ! Internal variable output
                                      Message_Log ) &  ! Error messaging
                                    RESULT ( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics
    TYPE(MWLSOVariables_type),    INTENT(IN OUT) :: MWLSOV
    CHARACTER(*), OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_MW_Land_SfcOptics'
    REAL(fp),     PARAMETER :: FREQUENCY_CUTOFF   = 80.0_fp  ! GHz
    REAL(fp),     PARAMETER :: DEFAULT_EMISSIVITY = 0.95_fp
    ! Local variables
    INTEGER :: i


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! --------------------------------------
    ! Compute the surface optical parameters
    ! --------------------------------------
    IF ( SC(SensorIndex)%Frequency(ChannelIndex) < FREQUENCY_CUTOFF ) THEN
      ! Frequency is low enough for the model
      DO i = 1, SfcOptics%n_Angles
        CALL NESDIS_LandEM(SfcOptics%Angle(i),            & ! Input, Degree
                           SC(SensorIndex)%Frequency(ChannelIndex),   & ! Input, GHz
                           Surface%Soil_Moisture_Content, & ! Input, g.cm^-3
                           Surface%Vegetation_Fraction,   & ! Input
                           Surface%Soil_Temperature,      & ! Input, K
                           Surface%Land_Temperature,      & ! Input, K
                           ZERO,                          & ! Input, Snow depth, mm
                           SfcOptics%Emissivity(i,2),     & ! Output, H component
                           SfcOptics%Emissivity(i,1)      ) ! Output, V component
        ! Assume specular surface
        SfcOptics%Reflectivity(i,1,i,1) = ONE-SfcOptics%Emissivity(i,1)
        SfcOptics%Reflectivity(i,2,i,2) = ONE-SfcOptics%Emissivity(i,2)
      END DO
    ELSE
      ! Frequency is too high for model. Use default.
      DO i = 1, SfcOptics%n_Angles
        SfcOptics%Emissivity(i,1:2)           = DEFAULT_EMISSIVITY
        SfcOptics%Reflectivity(i,1:2,i,1:2) = ONE-DEFAULT_EMISSIVITY
      END DO
    END IF

  END FUNCTION Compute_MW_Land_SfcOptics



!----------------------------------------------------------------------------------
!
! NAME:
!       Compute_MW_Land_SfcOptics_TL
!
! PURPOSE:
!       Function to compute the tangent-linear surface emissivity and
!       reflectivity at microwave frequencies over a land surface.
!
!       This function is a wrapper for third party code.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_MW_Land_SfcOptics_TL( Surface               , &  ! Input
!                                                    SfcOptics             , &  ! Input     
!                                                    Surface_TL            , &  ! Input
!                                                    GeometryInfo          , &  ! Input
!                                                    SensorIndex           , &  ! Input
!                                                    ChannelIndex          , &  ! Output     
!                                                    SfcOptics_TL          , &  ! Output
!                                                    MWLSOVariables        , &  ! Internal variable input
!                                                    Message_Log=Message_Log )  ! Error messaging 
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
!       MWLSOVariables:  Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_MW_Land_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       MWLSOVariables_type
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

  FUNCTION Compute_MW_Land_SfcOptics_TL( Surface     , &  ! Input
                                         SfcOptics   , &  ! Input     
                                         Surface_TL  , &  ! Input
                                         GeometryInfo, &  ! Input
                                         SensorIndex , &  ! Input
                                         ChannelIndex, &  ! Input
                                         SfcOptics_TL, &  ! Output
                                         MWLSOV      , &  ! Internal variable input
                                         Message_Log )  &  ! Error messaging 
                                       RESULT ( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface_TL
    TYPE(CRTM_SfcOptics_type),    INTENT(IN)     :: SfcOptics
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics_TL
    TYPE(MWLSOVariables_type),    INTENT(IN)     :: MWLSOV
    CHARACTER(*), OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_MW_Land_SfcOptics_TL'
    ! Local variables


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! -----------------------------------------------------
    ! Compute the tangent-linear surface optical parameters
    !
    ! ***No TL models yet, so default TL output is zero***
    ! -----------------------------------------------------
    SfcOptics_TL%Reflectivity = ZERO
    SfcOptics_TL%Emissivity   = ZERO

  END FUNCTION Compute_MW_Land_SfcOptics_TL



!----------------------------------------------------------------------------------
!
! NAME:
!       Compute_MW_Land_SfcOptics_AD
!
! PURPOSE:
!       Function to compute the adjoint surface emissivity and
!       reflectivity at microwave frequencies over a land surface.
!
!       This function is a wrapper for third party code.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_MW_Land_SfcOptics_AD( Surface               , &  ! Input
!                                                    SfcOptics             , &  ! Input     
!                                                    SfcOptics_AD          , &  ! Input     
!                                                    GeometryInfo          , &  ! Input
!                                                    SensorIndex           , &  ! Input
!                                                    ChannelIndex          , &  ! Output     
!                                                    Surface_AD            , &  ! Output
!                                                    MWLSOVariables        , &  ! Internal variable input
!                                                    Message_Log=Message_Log )  ! Error messaging 
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

  FUNCTION Compute_MW_Land_SfcOptics_AD( Surface     , &  ! Input
                                         SfcOptics   , &  ! Input     
                                         SfcOptics_AD, &  ! Input
                                         GeometryInfo, &  ! Input
                                         SensorIndex , &  ! Input
                                         ChannelIndex, &  ! Input
                                         Surface_AD  , &  ! Output     
                                         MWLSOV      , &  ! Internal variable input
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
    TYPE(MWLSOVariables_type),    INTENT(IN)     :: MWLSOV
    CHARACTER(*), OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_MW_Land_SfcOptics_AD'
    ! Local variables


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! ----------------------------------------------
    ! Compute the adjoint surface optical parameters
    !
    ! ***No AD models yet, so there is no impact on AD result***
    ! ----------------------------------------------
    SfcOptics_AD%Reflectivity = ZERO
    SfcOptics_AD%Emissivity   = ZERO

  END FUNCTION Compute_MW_Land_SfcOptics_AD

END MODULE CRTM_MW_Land_SfcOptics
