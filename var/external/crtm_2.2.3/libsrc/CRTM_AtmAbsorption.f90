!
! CRTM_AtmAbsorption
!
! Module containing routines to compute the optical depth profile
! due to gaseous absorption.
!
!
! CREATION HISTORY:
!       Modifed by:     Yong Han, NESDIS/STAR 25-June-2008
!                       yong.han@noaa.gov


MODULE CRTM_AtmAbsorption

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,                 ONLY: fp
  USE Message_Handler,            ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters,            ONLY: ZERO, &
                                        ODAS_ALGORITHM,  ODPS_ALGORITHM, ODSSU_ALGORITHM
  USE CRTM_Atmosphere_Define,     ONLY: CRTM_Atmosphere_type
  USE CRTM_TauCoeff,              ONLY: TC
  USE CRTM_AncillaryInput_Define, ONLY: CRTM_AncillaryInput_type
  USE CRTM_GeometryInfo_Define,   ONLY: CRTM_GeometryInfo_type, &
                                        CRTM_GeometryInfo_GetValue
  USE CRTM_AtmOptics_Define,      ONLY: CRTM_AtmOptics_type
  USE CRTM_Predictor_Define,      ONLY: CRTM_Predictor_type
  ! ODAS modules
  USE ODAS_AtmAbsorption,         ONLY: ODAS_AAVar_type => iVar_type , &
                                        ODAS_Compute_AtmAbsorption   , &
                                        ODAS_Compute_AtmAbsorption_TL, &
                                        ODAS_Compute_AtmAbsorption_AD
  ! ODPS modules
  USE ODPS_AtmAbsorption,         ONLY: ODPS_AAVar_type => iVar_type , &
                                        ODPS_Compute_AtmAbsorption   , &
                                        ODPS_Compute_AtmAbsorption_TL, &
                                        ODPS_Compute_AtmAbsorption_AD
  ! ODSSU modules
  USE ODSSU_AtmAbsorption,        ONLY: ODSSU_AAVar_type => iVar_type , &
                                        ODSSU_Compute_Weights         , &
                                        ODSSU_Compute_AtmAbsorption   , &
                                        ODSSU_Compute_AtmAbsorption_TL, &
                                        ODSSU_Compute_AtmAbsorption_AD
  ! ODZeeman modules
  USE ODZeeman_AtmAbsorption,     ONLY: Zeeman_Compute_AtmAbsorption,    &
                                        Zeeman_Compute_AtmAbsorption_TL, &
                                        Zeeman_Compute_AtmAbsorption_AD, &
                                        Is_Zeeman_Channel

  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: iVar_type
  ! Procedures
  PUBLIC :: CRTM_Compute_AtmAbsorption
  PUBLIC :: CRTM_Compute_AtmAbsorption_TL
  PUBLIC :: CRTM_Compute_AtmAbsorption_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_AtmAbsorption.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


  ! ---------------------------------------------
  ! Structure to hold AtmAbsorption forward model
  ! variables across FWD, TL, and AD calls
  ! ---------------------------------------------
  !:tdoc+:
  TYPE :: iVar_type
    PRIVATE
    TYPE(ODAS_AAVar_type)   :: ODAS
    TYPE(ODPS_AAVar_type)   :: ODPS
    TYPE(ODSSU_AAVar_type)  :: ODSSU
  END TYPE iVar_type
  !:tdoc-:


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Compute_AtmAbsorption
!
! PURPOSE:
!       Subroutine to calculate the layer optical depths due to gaseous
!       absorption for a given sensor and channel and atmospheric profile.
!       It is a wrapper which calls the algorithm-specific routine.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_AtmAbsorption( &
!              SensorIndex   , &  ! Input
!              ChannelIndex  , &  ! Input
!              AncillaryInput, &  ! Input
!              Predictor     , &  ! Input
!              AtmOptics     , &  ! Output
!              iVar            )  ! Internal variable output
!
! INPUTS:
!       SensorIndex:
!         Sensor index id. This is a unique index associated
!         with a (supported) sensor used to access the
!         shared coefficient data for a particular sensor.
!         See the ChannelIndex argument.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:
!         Channel index id. This is a unique index associated
!         with a (supported) sensor channel used to access the
!         shared coefficient data for a particular sensor's
!         channel.
!         See the SensorIndex argument.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       AncillaryInput:
!         Structure holding ancillary inputs
!         UNITS:      N/A
!         TYPE:       AncillaryInput_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       Predictor:
!         Structure containing the integrated absorber and
!         predictor profile data.
!         UNITS:      N/A
!         TYPE:       CRTM_Predictor_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       AtmOptics:
!         Structure containing computed optical depth profile data.
!         UNITS:      N/A
!         TYPE:       CRTM_AtmOptics_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN OUT)
!
!       iVar:
!         Structure containing internal variables required for
!         subsequent tangent-linear or adjoint model calls.
!         The contents of this structure are NOT accessible
!         outside of this module.
!         UNITS:      N/A
!         TYPE:       iVar_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_AtmAbsorption( &
    SensorIndex   , &  ! Input
    ChannelIndex  , &  ! Input
    AncillaryInput, &  ! Input
    Predictor     , &  ! Input
    AtmOptics     , &  ! Output
    iVar            )  ! Internal variable output
    ! Arguments
    INTEGER                       , INTENT(IN)     :: SensorIndex
    INTEGER                       , INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_AncillaryInput_type), INTENT(IN)     :: AncillaryInput
    TYPE(CRTM_Predictor_type)     , INTENT(IN OUT) :: Predictor
    TYPE(CRTM_AtmOptics_type)     , INTENT(IN OUT) :: AtmOptics
    TYPE(iVar_type)               , INTENT(IN OUT) :: iVar
    ! Local variables
    INTEGER :: idx

    ! Is this a Zeeman channel?
    idx = TC%ZSensor_LoIndex(SensorIndex)
    IF( idx > 0 )THEN
      IF( Is_Zeeman_Channel(TC%ODZeeman(idx), ChannelIndex) )THEN
        CALL Zeeman_Compute_AtmAbsorption( &
               TC%ODZeeman(idx)  , &  ! Input
               ChannelIndex      , &  ! Input
               Predictor%ODZeeman, &  ! Input
               AtmOptics           )  ! Output
        RETURN
      END IF
    END IF


    ! Call required model
    idx = TC%Sensor_LoIndex(SensorIndex)
    SELECT CASE( TC%Algorithm_ID(SensorIndex) )

      ! ODAS transmittance model
      CASE( ODAS_ALGORITHM )
        CALL ODAS_Compute_AtmAbsorption( &
               TC%ODAS(idx)  , &  ! Input
               ChannelIndex  , &  ! Input
               Predictor%ODAS, &  ! Input
               AtmOptics     , &  ! Output
               iVar%ODAS       )  ! Internal variable output

      ! ODPS transmittance model
      CASE( ODPS_ALGORITHM )
        CALL ODPS_Compute_AtmAbsorption( &
               TC%ODPS(idx)  , &  ! Input
               ChannelIndex  , &  ! Input
               Predictor%ODPS, &  ! Input
               AtmOptics       )  ! Output

      ! SSU instrument specific
      CASE( ODSSU_ALGORITHM )
        CALL ODSSU_Compute_Weights( &
               AncillaryInput%SSU, &  ! Input
               SensorIndex       , &  ! Input
               ChannelIndex      , &  ! Input
               iVar%ODSSU          )  ! Internal variable output

        ! ...Select particular transmittance algorithm for this instrument
        SELECT CASE( TC%ODSSU(idx)%subAlgorithm )
          CASE( ODAS_ALGORITHM )
            CALL ODSSU_Compute_AtmAbsorption( &
                   TC%Sensor_LoIndex(SensorIndex), &  ! Input
                   ChannelIndex                  , &  ! Input
                   Predictor%ODAS                , &  ! Input
                   AtmOptics                     , &  ! Output
                   iVar%ODSSU                      )  ! Internal variable output
          CASE( ODPS_ALGORITHM )
            CALL ODSSU_Compute_AtmAbsorption( &
                   TC%Sensor_LoIndex(SensorIndex), &  ! Input
                   ChannelIndex                  , &  ! Input
                   Predictor%ODPS                , &  ! Input
                   AtmOptics                     , &  ! Output
                   iVar%ODSSU                      )  ! Internal variable output
        END SELECT
    END SELECT

  END SUBROUTINE CRTM_Compute_AtmAbsorption


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Compute_AtmAbsorption_TL
!
! PURPOSE:
!       Subroutine to calculate the tangent-linear layer optical depths due
!       to gaseous absorption for a given sensor and channel and atmospheric
!       profile. It is a wrapper which calls the algorithm specific routine.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_AtmAbsorption_TL( &
!              SensorIndex   , &  ! Input
!              ChannelIndex  , &  ! Input
!              Predictor     , &  ! FWD Input
!              Predictor_TL  , &  ! TL Input
!              AtmOptics_TL  , &  ! TL Output
!              iVar            )  ! Internal variable input
!
! INPUTS:
!       SensorIndex:
!         Sensor index id. This is a unique index associated
!         with a (supported) sensor used to access the
!         shared coefficient data for a particular sensor.
!         See the ChannelIndex argument.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:
!         Channel index id. This is a unique index associated
!         with a (supported) sensor channel used to access the
!         shared coefficient data for a particular sensor's
!         channel.
!         See the SensorIndex argument.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       Predictor:
!         Structure containing the integrated absorber and
!         predictor profile data.
!         UNITS:      N/A
!         TYPE:       CRTM_Predictor_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       Predictor_TL:
!         Structure containing the tangent-linearintegrated absorber and
!         predictor profile data.
!         UNITS:      N/A
!         TYPE:       CRTM_Predictor_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       iVar:
!         Structure containing internal variables required for
!         subsequent tangent-linear or adjoint model calls.
!         The contents of this structure are NOT accessible
!         outside of this module.
!         UNITS:      N/A
!         TYPE:       iVar_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       AtmOptics_TL:
!         Structure containing the computed tangent-linear
!         optical depth profile data.
!         UNITS:      N/A
!         TYPE:       CRTM_AtmOptics_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN OUT)
!
! :sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_AtmAbsorption_TL( &
    SensorIndex   , &  ! Input
    ChannelIndex  , &  ! Input
    Predictor     , &  ! Input
    Predictor_TL  , &  ! Input
    AtmOptics_TL  , &  ! Output
    iVar            )  ! Internal variable input
    ! Arguments
    INTEGER                  , INTENT(IN)     :: SensorIndex
    INTEGER                  , INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_Predictor_type), INTENT(IN)     :: Predictor
    TYPE(CRTM_Predictor_type), INTENT(IN OUT) :: Predictor_TL
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: AtmOptics_TL
    TYPE(iVar_type)          , INTENT(IN)     :: iVar
    ! Local variables
    INTEGER :: idx

    ! Is this a Zeeman channel?
    idx = TC%ZSensor_LoIndex(SensorIndex)
    IF( idx > 0 )THEN
      IF( Is_Zeeman_Channel(TC%ODZeeman(idx), ChannelIndex) )THEN
        CALL Zeeman_Compute_AtmAbsorption_TL( &
               TC%ODZeeman(idx)     , &  ! Input
               ChannelIndex         , &  ! Input
               Predictor%ODZeeman   , &  ! Input
               Predictor_TL%ODZeeman, &  ! Input
               AtmOptics_TL           )  ! Output
        RETURN
      END IF
    END IF


    ! Call required model
    idx = TC%Sensor_LoIndex(SensorIndex)
    SELECT CASE( TC%Algorithm_ID(SensorIndex) )

      ! ODAS transmittance model
      CASE( ODAS_ALGORITHM )
        CALL ODAS_Compute_AtmAbsorption_TL( &
               TC%ODAS(idx)     , &  ! Input
               ChannelIndex     , &  ! Input
               Predictor%ODAS   , &  ! Input
               Predictor_TL%ODAS, &  ! Input
               AtmOptics_TL     , &  ! Output
               iVar%ODAS          )  ! Internal variable input

      ! ODPS transmittance model
      CASE( ODPS_ALGORITHM )
        CALL ODPS_Compute_AtmAbsorption_TL( &
               TC%ODPS(idx)     , &  ! Input
               ChannelIndex     , &  ! Input
               Predictor%ODPS   , &  ! Input
               Predictor_TL%ODPS, &  ! Input
               AtmOptics_TL       )  ! Output

      ! SSU instrument specific
      CASE( ODSSU_ALGORITHM )

        ! ...Select particular transmittance algorithm for this instrument
        SELECT CASE( TC%ODSSU(idx)%subAlgorithm )
          CASE( ODAS_ALGORITHM )
            CALL ODSSU_Compute_AtmAbsorption_TL( &
                   TC%Sensor_LoIndex(SensorIndex), &  ! Input
                   ChannelIndex                  , &  ! Input
                   Predictor%ODAS                , &  ! Input
                   Predictor_TL%ODAS             , &  ! Input
                   AtmOptics_TL                  , &  ! Output
                   iVar%ODSSU                      )  ! Internal variable input
          CASE( ODPS_ALGORITHM )
            CALL ODSSU_Compute_AtmAbsorption_TL( &
                   TC%Sensor_LoIndex(SensorIndex), &  ! Input
                   ChannelIndex                  , &  ! Input
                   Predictor%ODPS                , &  ! Input
                   Predictor_TL%ODPS             , &  ! Input
                   AtmOptics_TL                  , &  ! Output
                   iVar%ODSSU                      )  ! Internal variable input
        END SELECT
    END SELECT

  END SUBROUTINE CRTM_Compute_AtmAbsorption_TL


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_AtmAbsorption_AD
!
! PURPOSE:
!       Subroutine to calculate the layer optical depth adjoints due to
!       gaseous absorption for a given sensor and channel and atmospheric
!       profile. It is a wrapper which calls the algorithm specific routine.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_AtmAbsorption_AD( &
!              SensorIndex , &  ! Input
!              ChannelIndex, &  ! Input
!              Predictor   , &  ! FWD Input
!              AtmOptics_AD, &  ! AD  Input
!              Predictor_AD, &  ! AD  Output
!              iVar          )  ! Internal variable input
!
! INPUT ARGUMENTS:
!       SensorIndex:
!         Sensor index id. This is a unique index associated
!         with a (supported) sensor used to access the
!         shared coefficient data for a particular sensor.
!         See the ChannelIndex argument.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:
!         Channel index id. This is a unique index associated
!         with a (supported) sensor channel used to access the
!         shared coefficient data for a particular sensor's
!         channel.
!         See the SensorIndex argument.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       Predictor:
!         Structure containing the integrated absorber and
!         predictor profile data.
!         UNITS:      N/A
!         TYPE:       CRTM_Predictor_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       AtmOptics_AD:
!         Structure containing the computed adjoint optical depth profile data.
!         UNITS:      N/A
!         TYPE:       CRTM_AtmOptics_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN OUT)
!
!       iVar:
!         Structure containing internal variables required for
!         subsequent tangent-linear or adjoint model calls.
!         The contents of this structure are NOT accessible
!         outside of this module.
!         UNITS:      N/A
!         TYPE:       iVar_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor_AD:
!         Structure containing the adjoint integrated absorber and
!         predictor profile data.
!         UNITS:      N/A
!         TYPE:       CRTM_Predictor_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN OUT)
!
! SIDE EFFECTS:
!       Components of the AtmOptics_AD structure argument are modified
!       in this function.
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_AtmAbsorption_AD( &
    SensorIndex , &  ! Input
    ChannelIndex, &  ! Input
    Predictor   , &  ! FWD Input
    AtmOptics_AD, &  ! AD  Input
    Predictor_AD, &  ! AD  Output
    iVar          )  ! Internal variable input
    ! Arguments
    INTEGER                  , INTENT(IN)     :: SensorIndex
    INTEGER                  , INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_Predictor_type), INTENT(IN)     :: Predictor
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: AtmOptics_AD
    TYPE(CRTM_Predictor_type), INTENT(IN OUT) :: Predictor_AD
    TYPE(iVar_type)          , INTENT(IN)     :: iVar
    ! Local variables
    INTEGER :: idx

    ! Is this a Zeeman channel?
    idx = TC%ZSensor_LoIndex(SensorIndex)
    IF( idx > 0 )THEN
      IF( Is_Zeeman_Channel(TC%ODZeeman(idx), ChannelIndex) )THEN
        CALL Zeeman_Compute_AtmAbsorption_AD( &
               TC%ODZeeman(idx)     , &  ! Input
               ChannelIndex         , &  ! Input
               Predictor%ODZeeman   , &  ! Input
               AtmOptics_AD         , &  ! AD Input
               Predictor_AD%ODZeeman  )  ! AD Output
        RETURN
      END IF
    END IF

    ! Call required model
    idx = TC%Sensor_LoIndex(SensorIndex)
    SELECT CASE( TC%Algorithm_ID(SensorIndex) )

      ! ODAS transmittance model
      CASE( ODAS_ALGORITHM )
        CALL ODAS_Compute_AtmAbsorption_AD( &
               TC%ODAS(idx)     , &  ! Input
               ChannelIndex     , &  ! Input
               Predictor%ODAS   , &  ! FWD Input
               AtmOptics_AD     , &  ! AD Input
               Predictor_AD%ODAS, &  ! AD Output
               iVar%ODAS          )  ! Internal variable input

      ! ODPS transmittance model
      CASE( ODPS_ALGORITHM )
        CALL ODPS_Compute_AtmAbsorption_AD( &
               TC%ODPS(idx)     , &  ! Input
               ChannelIndex     , &  ! Input
               Predictor%ODPS   , &  ! FWD Input
               AtmOptics_AD     , &  ! AD Input
               Predictor_AD%ODPS  )  ! AD Output

      ! SSU instrument specific
      CASE( ODSSU_ALGORITHM )

        ! Select particular transmittance algorithm for this instrument
        SELECT CASE( TC%ODSSU(idx)%subAlgorithm )
          CASE( ODAS_ALGORITHM )
            CALL ODSSU_Compute_AtmAbsorption_AD( &
                   TC%Sensor_LoIndex(SensorIndex), &  ! Input
                   ChannelIndex                  , &  ! Input
                   Predictor%ODAS                , &  ! FWD Input
                   AtmOptics_AD                  , &  ! AD Input
                   Predictor_AD%ODAS             , &  ! AD Output
                   iVar%ODSSU                      )  ! Internal variable input
          CASE( ODPS_ALGORITHM )
            CALL ODSSU_Compute_AtmAbsorption_AD( &
                   TC%Sensor_LoIndex(SensorIndex), &  ! Input
                   ChannelIndex                  , &  ! Input
                   Predictor%ODPS                , &  ! FWD Input
                   AtmOptics_AD                  , &  ! AD Input
                   Predictor_AD%ODPS             , &  ! AD Output
                   iVar%ODSSU                      )  ! Internal variable input
        END SELECT
    END SELECT

  END SUBROUTINE CRTM_Compute_AtmAbsorption_AD

END MODULE CRTM_AtmAbsorption
