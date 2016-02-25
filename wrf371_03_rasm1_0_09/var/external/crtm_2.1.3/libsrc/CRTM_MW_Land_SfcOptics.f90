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
!       Written by:     Paul van Delst, 23-Jun-2005
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_MW_Land_SfcOptics

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,               ONLY: fp
  USE Message_Handler,          ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters,          ONLY: ZERO, ONE, MAX_N_ANGLES
  USE CRTM_SpcCoeff,            ONLY: SC
  USE CRTM_Surface_Define,      ONLY: CRTM_Surface_type
  USE CRTM_Land_Parameters,     ONLY: N_VALID_SOIL_TYPES, &
                                      N_VALID_VEGETATION_TYPES
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
  PUBLIC :: iVar_type
  ! Science routines
  PUBLIC :: Compute_MW_Land_SfcOptics
  PUBLIC :: Compute_MW_Land_SfcOptics_TL
  PUBLIC :: Compute_MW_Land_SfcOptics_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_MW_Land_SfcOptics.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  ! Message length
  INTEGER, PARAMETER :: ML = 256



  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: iVar_type
    PRIVATE
    INTEGER :: Dummy = 0
  END TYPE iVar_type


CONTAINS



!----------------------------------------------------------------------------------
!:sdoc+:
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
!       Error_Status = Compute_MW_Land_SfcOptics( &
!                        Surface     , &
!                        SensorIndex , &
!                        ChannelIndex, &
!                        SfcOptics     )
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

  FUNCTION Compute_MW_Land_SfcOptics( &
    Surface     , &  ! Input
    SensorIndex , &  ! Input
    ChannelIndex, &  ! Input
    SfcOptics   ) &  ! Output
  RESULT ( err_stat )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_MW_Land_SfcOptics'
    REAL(fp),     PARAMETER :: FREQUENCY_CUTOFF   = 80.0_fp  ! GHz
    REAL(fp),     PARAMETER :: DEFAULT_EMISSIVITY = 0.95_fp
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: i


    ! Set up
    err_stat = SUCCESS
    ! ...Check the soil type...
    IF ( Surface%Soil_Type < 1 .OR. &
         Surface%Soil_Type > N_VALID_SOIL_TYPES ) THEN
      SfcOptics%Emissivity   = ZERO
      SfcOptics%Reflectivity = ZERO
      err_stat = FAILURE
      msg = 'Invalid soil type index specified'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF
    ! ...and the vegetation type
    IF ( Surface%Vegetation_Type < 1 .OR. &
         Surface%Vegetation_Type > N_VALID_VEGETATION_TYPES ) THEN
      SfcOptics%Emissivity   = ZERO
      SfcOptics%Reflectivity = ZERO
      err_stat = FAILURE
      msg = 'Invalid vegetation type index specified'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF


    ! Compute the surface optical parameters
    IF ( SC(SensorIndex)%Frequency(ChannelIndex) < FREQUENCY_CUTOFF ) THEN
      ! Frequency is low enough for the model
      DO i = 1, SfcOptics%n_Angles
        CALL NESDIS_LandEM(SfcOptics%Angle(i),            & ! Input, Degree
                           SC(SensorIndex)%Frequency(ChannelIndex),   & ! Input, GHz
                           Surface%Soil_Moisture_Content, & ! Input, g.cm^-3
                           Surface%Vegetation_Fraction,   & ! Input
                           Surface%Soil_Temperature,      & ! Input, K
                           Surface%Land_Temperature,      & ! Input, K
                           Surface%Lai,                   & ! Input, Leaf Area Index
                           Surface%Soil_Type,             & ! Input, Soil Type (1 -  9)
                           Surface%Vegetation_Type,       & ! Input, Vegetation Type (1 - 13)
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
        SfcOptics%Emissivity(i,1:2)         = DEFAULT_EMISSIVITY
        SfcOptics%Reflectivity(i,1:2,i,1:2) = ONE-DEFAULT_EMISSIVITY
      END DO
    END IF

  END FUNCTION Compute_MW_Land_SfcOptics


!----------------------------------------------------------------------------------
!:sdoc+:
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
!       NB: CURRENTLY THIS IS A STUB FUNCTION AS THERE ARE NO TL
!           COMPONENTS IN THE MW LAND SFCOPTICS COMPUTATIONS.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_MW_Land_SfcOptics_TL( SfcOptics_TL )
!
! OUTPUTS:
!       SfcOptics_TL:    Structure containing the tangent-linear surface
!                        optical properties required for the tangent-
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

  FUNCTION Compute_MW_Land_SfcOptics_TL( &
    SfcOptics_TL) &  ! TL  Output
  RESULT ( err_stat )
    ! Arguments
    TYPE(CRTM_SfcOptics_type), INTENT(IN OUT) :: SfcOptics_TL
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_MW_Land_SfcOptics_TL'
    ! Local variables


    ! Set up
    err_stat = SUCCESS


    ! Compute the tangent-linear surface optical parameters
    ! ***No TL models yet, so default TL output is zero***
    SfcOptics_TL%Reflectivity = ZERO
    SfcOptics_TL%Emissivity   = ZERO

  END FUNCTION Compute_MW_Land_SfcOptics_TL



!----------------------------------------------------------------------------------
!:sdoc+:
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
!       NB: CURRENTLY THIS IS A STUB FUNCTION AS THERE ARE NO AD
!           COMPONENTS IN THE MW LAND SFCOPTICS COMPUTATIONS.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_MW_Land_SfcOptics_AD( SfcOptics_AD )
!
! INPUTS:
!       SfcOptics_AD:    Structure containing the adjoint surface optical
!                        properties required for the adjoint radiative
!                        transfer calculation.
!                        *** COMPONENTS MODIFIED UPON OUTPUT ***
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
!       Note the INTENT on the input adjoint arguments are IN OUT regardless
!       of their specification as "input" or "output". This is because these
!       arguments may contain information on input, or need to be zeroed on
!       output (or both).
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION Compute_MW_Land_SfcOptics_AD( &
    SfcOptics_AD) &  ! AD  Input
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics_AD
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_MW_Land_SfcOptics_AD'
    ! Local variables


    ! Set up
    err_stat = SUCCESS


    ! Compute the adjoint surface optical parameters
    ! ***No AD models yet, so there is no impact on AD result***
    SfcOptics_AD%Reflectivity = ZERO
    SfcOptics_AD%Emissivity   = ZERO

  END FUNCTION Compute_MW_Land_SfcOptics_AD

END MODULE CRTM_MW_Land_SfcOptics
