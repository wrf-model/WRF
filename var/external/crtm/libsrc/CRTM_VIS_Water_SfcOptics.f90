!
! CRTM_IR_Land_SfcOptics
!
! Module to compute the surface optical properties for LAND surfaces at
! infrared frequencies required for determining the LAND surface
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

MODULE CRTM_VIS_Water_SfcOptics

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,                 ONLY: fp=>fp_kind
  USE Message_Handler,            ONLY: SUCCESS
  USE Spectral_Units_Conversion,  ONLY: Inverse_cm_to_Micron
  USE CRTM_Parameters,            ONLY: ZERO, ONE, MAX_N_ANGLES
  USE CRTM_SpcCoeff,              ONLY: SC
  USE CRTM_Surface_Define,        ONLY: CRTM_Surface_type
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
  ! Science routines
  PUBLIC :: Compute_VIS_Water_SfcOptics
!  PUBLIC :: Compute_IR_Land_SfcOptics_TL
!  PUBLIC :: Compute_IR_Land_SfcOptics_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_VIS_Water_SfcOptics.f90 6287 2010-01-12 20:37:43Z paul.vandelst@noaa.gov $'


CONTAINS



!----------------------------------------------------------------------------------
!
! NAME:
!       Compute_VIS_Water_SfcOptics
!
! PURPOSE:
!       Function to compute the surface emissivity and reflectivity at UV/visible
!       frequencies over a land surface.
!
!       This function is a wrapper for third party code.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_VIS_Water_SfcOptics( Surface,                &  ! Input
!                                                 GeometryInfo,           &  ! Input
!                                                 SensorIndex ,           &  ! Input
!                                                 Channel_Index,          &  ! Input, scalar
!                                                 SfcOptics,              &  ! Output     
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
!       SensorIndex:     Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Channel_Index:   Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
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
!       IRLSOVariables:  Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_IR_Land_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       IRLSOVariables_type
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

  FUNCTION Compute_VIS_Water_SfcOptics( Surface,     &  ! Input
                                      GeometryInfo,  &  ! Input
                                      SensorIndex ,  &  ! Input
                                      ChannelIndex,  &  ! Input
                                      SfcOptics,     &  ! Output
                                      Message_Log )  &  ! Error messaging
                                    RESULT ( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: ChannelIndex,SensorIndex
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics
    CHARACTER(*), OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_VIS_Water_SfcOptics'
    INTEGER,      PARAMETER :: OFFSET = 3
    ! Local variables
    INTEGER :: j
    REAL(fp) :: Wavelength, Emissivity


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    ! Wavelength in microns 
    Wavelength = Inverse_cm_to_Micron(SC(SensorIndex)%Wavenumber(ChannelIndex))


    ! -------------------------------------------------------
    ! Compute Lambertian surface emissivity
    !
    ! The call below is uses a set offset for the land surface
    ! type based on those defined in the CRTM_Surface_Define.
    ! -------------------------------------------------------
    CALL Surface_IR_Emissivity( Wavelength, &
                                Emissivity, &
                                1)                                   
    ! ----------------------
    ! Solar direct component
    ! ----------------------
      SfcOptics%Direct_Reflectivity(:,1) = ONE-Emissivity
    ! --------------------------------------------------
    ! Fill the return emissivity and reflectivity arrays
    ! --------------------------------------------------
    SfcOptics%Emissivity(1:SfcOptics%n_Angles,1) = Emissivity
    DO j = 1, SfcOptics%n_Angles 
      SfcOptics%Reflectivity(1:SfcOptics%n_Angles,1,j,1) = (ONE-Emissivity)*SfcOptics%Weight(j)
    END DO

  END FUNCTION Compute_VIS_Water_SfcOptics


!----------------------------------------------------------------------------------
!
! NAME:
!       Compute_VIS_Water_SfcOptics_TL
!
! PURPOSE:
!       Function to compute the tangent-linear surface emissivity and
!       reflectivity at UV/visible frequencies over a land surface.
!
!       This function is a wrapper for third party code.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_VIS_Water_SfcOptics_TL( Surface,                &  ! Input
!                                                    SfcOptics,              &  ! Input     
!                                                    Surface_TL,             &  ! Input
!                                                    GeometryInfo,           &  ! Input
!                                                    SensorIndex ,           &  ! Input
!                                                    Channel_Index,          &  ! Input, scalar
!                                                    SfcOptics_TL,           &  ! Output     
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
!
!       SensorIndex:     Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Channel_Index:   Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       IRLSOVariables:  Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_IR_Land_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       IRLSOVariables_type
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

  FUNCTION Compute_VIS_Water_SfcOptics_TL( Surface,       &  ! Input
                                         SfcOptics,     &  ! Input     
                                         Surface_TL,    &  ! Input
                                         GeometryInfo,  &  ! Input
                                         SensorIndex ,  &  ! Input
                                         Channel_Index, &  ! Input, scalar
                                         SfcOptics_TL,  &  ! Output     
                                         Message_Log )  &  ! Error messaging 
                                       RESULT ( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface_TL
    TYPE(CRTM_SfcOptics_type),    INTENT(IN)     :: SfcOptics
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: Channel_Index,SensorIndex
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics_TL
    CHARACTER(*), OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_VIS_Water_SfcOptics_TL'
    ! Local variables


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! -----------------------------------------------------
    ! Compute the tangent-linear surface optical parameters
    ! ***Default TL output***
    ! -----------------------------------------------------
    SfcOptics_TL%Reflectivity = ZERO
    SfcOptics_TL%Emissivity   = ZERO
    SfcOptics_TL%Direct_Reflectivity = ZERO
      
  END FUNCTION Compute_VIS_Water_SfcOptics_TL


!----------------------------------------------------------------------------------
!
! NAME:
!       Compute_VIS_Water_SfcOptics_AD
!
! PURPOSE:
!       Function to compute the adjoint surface emissivity and
!       reflectivity at UV/visible frequencies over a land surface.
!
!       This function is a wrapper for third party code.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_VIS_Water_SfcOptics_AD( Surface,              &  ! Input
!                                                    SfcOptics,              &  ! Input     
!                                                    SfcOptics_AD,           &  ! Input     
!                                                    GeometryInfo,           &  ! Input
!                                                    SensorIndex ,           &  ! Input
!                                                    Channel_Index,          &  ! Input, scalar
!                                                    Surface_AD,             &  ! Output
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
!       SensorIndex:     Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Channel_Index:   Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       IRLSOVariables:  Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_IR_Land_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       IRLSOVariables_type
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

  FUNCTION Compute_VIS_Water_SfcOptics_AD( Surface,       &  ! Input
                                         SfcOptics,     &  ! Input     
                                         SfcOptics_AD,  &  ! Input
                                         GeometryInfo,  &  ! Input
                                         SensorIndex ,  &  ! Input
                                         Channel_Index, &  ! Input, scalar
                                         Surface_AD,    &  ! Output     
                                         Message_Log )  &  ! Error messaging 
                                       RESULT ( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_SfcOptics_type),    INTENT(IN)     :: SfcOptics
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics_AD
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: Channel_Index,SensorIndex
    TYPE(CRTM_Surface_type),      INTENT(IN OUT) :: Surface_AD
    CHARACTER(*), OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_VIS_Water_SfcOptics_AD'
    ! Local variables


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    SfcOptics_AD%Reflectivity = ZERO
    SfcOptics_AD%Emissivity   = ZERO
    SfcOptics_AD%Direct_Reflectivity = ZERO
    
  END FUNCTION Compute_VIS_Water_SfcOptics_AD

END MODULE CRTM_VIS_Water_SfcOptics
