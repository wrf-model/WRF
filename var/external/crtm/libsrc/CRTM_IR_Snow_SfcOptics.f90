!
! CRTM_IR_Snow_SfcOptics
!
! Module to compute the surface optical properties for SNOW surfaces at
! infrared frequencies required for determining the SNOW surface
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

MODULE CRTM_IR_Snow_SfcOptics

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,                 ONLY: fp
  USE Message_Handler,            ONLY: SUCCESS
  USE Spectral_Units_Conversion,  ONLY: Inverse_cm_to_Micron
  USE CRTM_Parameters,            ONLY: ZERO, ONE, MAX_N_ANGLES
  USE CRTM_SpcCoeff,              ONLY: SC, SOLAR_FLAG, IsFlagSet_SpcCoeff
  USE CRTM_Surface_Define,        ONLY: CRTM_Surface_type, N_VALID_SNOW_TYPES
  USE CRTM_GeometryInfo_Define,   ONLY: CRTM_GeometryInfo_type
  USE CRTM_SfcOptics_Define,      ONLY: CRTM_SfcOptics_type
  USE CRTM_Surface_IR_Emissivity, ONLY: Surface_IR_Emissivity
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Data types
  PUBLIC :: IRSSOVariables_type
  ! Science routines
  PUBLIC :: Compute_IR_Snow_SfcOptics  
  PUBLIC :: Compute_IR_Snow_SfcOptics_TL
  PUBLIC :: Compute_IR_Snow_SfcOptics_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_IR_Snow_SfcOptics.f90 8128 2010-05-28 18:48:07Z paul.vandelst@noaa.gov $'


  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: IRSSOVariables_type
    PRIVATE
    INTEGER :: Dummy = 0
  END TYPE IRSSOVariables_type


CONTAINS



!----------------------------------------------------------------------------------
!
! NAME:
!       Compute_IR_Snow_SfcOptics
!
! PURPOSE:
!       Function to compute the surface emissivity and reflectivity at infrared
!       frequencies over a snow surface.
!
!       This function is a wrapper for third party code.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_IR_Snow_SfcOptics( Surface               , &  ! Input
!                                                 GeometryInfo          , &  ! Input
!                                                 SensorIndex           , &  ! Input
!                                                 ChannelIndex          , &  ! Output     
!                                                 SfcOptics             , &  ! Output     
!                                                 IRSSOVariables        , &  ! Internal variable output
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
!                        UNITS:      N/A
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
!       IRSSOVariables:  Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_IR_Snow_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       IRSSOVariables_type
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

  FUNCTION Compute_IR_Snow_SfcOptics( Surface     , &  ! Input
                                      GeometryInfo, &  ! Input
                                      SensorIndex , &  ! Input
                                      ChannelIndex, &  ! Input
                                      SfcOptics   , &  ! Output
                                      IRSSOV      , &  ! Internal variable output
                                      Message_Log ) &  ! Error messaging
                                    RESULT ( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics
    TYPE(IRSSOVariables_type),    INTENT(IN OUT) :: IRSSOV
    CHARACTER(*), OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_IR_Snow_SfcOptics'
    INTEGER,      PARAMETER, DIMENSION(N_VALID_SNOW_TYPES) :: &
      TYPE_MAP = (/ 2, 2, 3, 3, & ! Map the snow surface types defined in the
                    2, 2, 2, 2, & ! CRTM_Surface_Define module to the two snow
                    3, 3, 3, 2, & ! surface types (2 - old snow; 3 - fresh snow)
                    2, 3, 2, 2 /) ! used in this model  
    ! Local variables
    INTEGER :: j
    REAL(fp) :: Wavelength, Emissivity


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    ! Wavelength in microns 
    Wavelength = Inverse_cm_to_Micron(SC(SensorIndex)%Wavenumber(ChannelIndex))


    ! -------------------------------------
    ! Compute Lambertian surface emissivity
    ! -------------------------------------
    CALL Surface_IR_Emissivity( Wavelength, &
                                Emissivity, &
                                TYPE_MAP(Surface%Snow_Type))


    ! ----------------------
    ! Solar direct component
    ! ----------------------
    IF ( IsFlagSet_SpcCoeff(SC(SensorIndex)%Channel_Flag(ChannelIndex),SOLAR_FLAG) ) THEN
      SfcOptics%Direct_Reflectivity(:,1) = ONE-Emissivity
    END IF


    ! --------------------------------------------------
    ! Fill the return emissivity and reflectivity arrays
    ! --------------------------------------------------
    SfcOptics%Emissivity(1:SfcOptics%n_Angles,1) = Emissivity
    DO j = 1, SfcOptics%n_Angles 
      SfcOptics%Reflectivity(j,1,j,1) = ONE-SfcOptics%Emissivity(j,1)
    END DO

  END FUNCTION Compute_IR_Snow_SfcOptics


!----------------------------------------------------------------------------------
!
! NAME:
!       Compute_IR_Snow_SfcOptics_TL
!
! PURPOSE:
!       Function to compute the tangent-linear surface emissivity and
!       reflectivity at infrared frequencies over a snow surface.
!
!       This function is a wrapper for third party code.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_IR_Snow_SfcOptics_TL( Surface               , &  ! Input
!                                                    SfcOptics             , &  ! Input     
!                                                    Surface_TL            , &  ! Input
!                                                    GeometryInfo          , &  ! Input
!                                                    SensorIndex           , &  ! Input
!                                                    ChannelIndex          , &  ! Output     
!                                                    SfcOptics_TL          , &  ! Output     
!                                                    IRSSOVariables        , &  ! Internal variable input
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
!       IRSSOVariables:  Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_IR_Snow_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       IRSSOVariables_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
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

  FUNCTION Compute_IR_Snow_SfcOptics_TL( Surface     , &  ! Input
                                         SfcOptics   , &  ! Input     
                                         Surface_TL  , &  ! Input
                                         GeometryInfo, &  ! Input
                                         SensorIndex , &  ! Input
                                         ChannelIndex, &  ! Input
                                         SfcOptics_TL, &  ! Output     
                                         IRSSOV      , &  ! Internal variable input
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
    TYPE(IRSSOVariables_type),    INTENT(IN)     :: IRSSOV
    CHARACTER(*), OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_IR_Snow_SfcOptics_TL'
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
    SfcOptics_TL%Direct_Reflectivity = ZERO

  END FUNCTION Compute_IR_Snow_SfcOptics_TL


!----------------------------------------------------------------------------------
!
! NAME:
!       Compute_IR_Snow_SfcOptics_AD
!
! PURPOSE:
!       Function to compute the adjoint surface emissivity and
!       reflectivity at infrared frequencies over a snow surface.
!
!       This function is a wrapper for third party code.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_IR_Snow_SfcOptics_AD( Surface               , &  ! Input
!                                                    SfcOptics             , &  ! Input     
!                                                    SfcOptics_AD          , &  ! Input     
!                                                    GeometryInfo          , &  ! Input
!                                                    SensorIndex           , &  ! Input
!                                                    ChannelIndex          , &  ! Output     
!                                                    Surface_AD            , &  ! Output
!                                                    IRSSOVariables        , &  ! Internal variable input
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
!       IRSSOVariables:  Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_IR_Snow_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       IRSSOVariables_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
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

  FUNCTION Compute_IR_Snow_SfcOptics_AD( Surface     , &  ! Input
                                         SfcOptics   , &  ! Input     
                                         SfcOptics_AD, &  ! Input
                                         GeometryInfo, &  ! Input
                                         SensorIndex , &  ! Input
                                         ChannelIndex, &  ! Input
                                         Surface_AD  , &  ! Output     
                                         IRSSOV      , &  ! Internal variable input
                                         Message_Log )  &  ! Error messaging 
                                       RESULT ( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_SfcOptics_type),    INTENT(IN)     :: SfcOptics
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics_AD
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_Surface_type),      INTENT(IN OUT) :: Surface_AD
    TYPE(IRSSOVariables_type),    INTENT(IN)     :: IRSSOV
    CHARACTER(*), OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_IR_Snow_SfcOptics_AD'
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
    SfcOptics_AD%Direct_Reflectivity = ZERO

  END FUNCTION Compute_IR_Snow_SfcOptics_AD

END MODULE CRTM_IR_Snow_SfcOptics
