!
! CRTM_Predictor
!
! Module containing routines to compute the predictors for the regression
! model algorithms that compute the optical depth profile due to gaseous
! absorption.
!

MODULE CRTM_Predictor

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
  USE ODAS_Predictor_Define,      ONLY: ODAS_Predictor_type      , &
                                        ODAS_Predictor_Associated, &
                                        ODAS_Predictor_Create    , &
                                        ODAS_Predictor_Destroy
  USE ODAS_Predictor,             ONLY: ODAS_APVar_type => iVar_type, &
                                        ODAS_Assemble_Predictors    , &
                                        ODAS_Assemble_Predictors_TL , &
                                        ODAS_Assemble_Predictors_AD , &
                                        ODAS_MAX_N_PREDICTORS => MAX_N_PREDICTORS, &
                                        ODAS_MAX_N_ABSORBERS  => MAX_N_ABSORBERS , &
                                        ODAS_MAX_N_ORDERS     => MAX_N_ORDERS
  ! ODPS modules
  USE ODPS_Predictor_Define,      ONLY: ODPS_Predictor_type      , &
                                        ODPS_Predictor_Associated, &
                                        ODPS_Predictor_Destroy   , &
                                        ODPS_Predictor_Create    , &
                                        PAFV_Associated          , &
                                        PAFV_Destroy             , &
                                        PAFV_Create
  USE ODPS_Predictor,             ONLY: ODPS_APVar_type => iVar_type, &
                                        ODPS_Get_n_Components    , &
                                        ODPS_Get_max_n_Predictors, &
                                        ODPS_Get_n_Absorbers     , &
                                        ODPS_Get_SaveFWVFlag     , &
                                        ODPS_Assemble_Predictors   , &
                                        ODPS_Assemble_Predictors_TL, &
                                        ODPS_Assemble_Predictors_AD, &
                                        ALLOW_OPTRAN
  ! ODZeeman modules
  USE ODZeeman_AtmAbsorption,     ONLY: Zeeman_Compute_Predictors,     &
                                        Zeeman_Compute_Predictors_TL,  &
                                        Zeeman_Compute_Predictors_AD,  &
                                        Is_ODZeeman

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
  PUBLIC :: CRTM_Compute_Predictors
  PUBLIC :: CRTM_Compute_Predictors_TL
  PUBLIC :: CRTM_Compute_Predictors_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_Predictor.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


  ! -----------------------------------------
  ! Structure to hold Predictor forward model
  ! variables across FWD, TL, and AD calls
  ! -----------------------------------------
  TYPE :: iVar_type
    PRIVATE
    TYPE(ODAS_APVar_type) :: ODAS
    TYPE(ODPS_APVar_type) :: ODPS
  END TYPE iVar_type


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Compute_Predictors
!
! PURPOSE:
!       Subroutine to calculate the gas absorption model predictors.
!       It is a wrapper which calls the algorithm specific routine.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_Predictors( &
!              SensorIndex   , &  ! Input
!              Atmosphere    , &  ! Input
!              GeometryInfo  , &  ! Input
!              AncillaryInput, &  ! Input
!              Predictor     , &  ! Output
!              iVar            )  ! Internal variable output
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
!       Atmosphere:
!         Structure containing the atmospheric state data.
!         UNITS:      N/A
!         TYPE:       CRTM_Atmosphere_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       GeometryInfo:
!         Structure containing the view geometry information.
!         UNITS:      N/A
!         TYPE:       CRTM_GeometryInfo_type
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
! OUTPUT ARGUMENTS:
!       Predictor:
!         Structure containing the integrated absorber and predictor profiles.
!         UNITS:      N/A
!         TYPE:       CRTM_Predictor_type
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
!         ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_Predictors( &
    SensorIndex   , &  ! Input
    Atmosphere    , &  ! Input
    GeometryInfo  , &  ! Input
    AncillaryInput, &  ! Input
    Predictor     , &  ! Output
    iVar            )  ! Internal variable output
    ! Arguments
    INTEGER                       , INTENT(IN)     :: SensorIndex
    TYPE(CRTM_Atmosphere_type)    , INTENT(IN)     :: Atmosphere
    TYPE(CRTM_GeometryInfo_type)  , INTENT(IN)     :: GeometryInfo
    TYPE(CRTM_AncillaryInput_type), INTENT(IN)     :: AncillaryInput
    TYPE(CRTM_Predictor_type)     , INTENT(IN OUT) :: Predictor
    TYPE(iVar_type)               , INTENT(IN OUT) :: iVar
    ! Local variables
    INTEGER :: idx, n

    ! Call required model
    idx = TC%Sensor_LoIndex(SensorIndex)
    SELECT CASE( TC%Algorithm_ID(SensorIndex) )

      ! Predictors for ODAS transmittance model
      CASE( ODAS_ALGORITHM )
        CALL ODAS_Assemble_Predictors( &
               Atmosphere            , &  ! Input
               GeometryInfo          , &  ! Input
               TC%ODAS(idx)%Max_Order, &  ! Input
               TC%ODAS(idx)%Alpha    , &  ! Input
               Predictor%ODAS        , &  ! Output
               iVar%ODAS               )  ! Output

      ! Predictors for ODPS transmittance model
      CASE( ODPS_ALGORITHM )
         CALL ODPS_Assemble_Predictors( &
                TC%ODPS(idx)  , &  ! Input
                Atmosphere    , &  ! Input
                GeometryInfo  , &  ! Input
                Predictor%ODPS  )  ! Output

      ! Predictors for SSU instrument specific model
      CASE( ODSSU_ALGORITHM )

        ! ...Select particular transmittance algorithm for this instrument
        SELECT CASE( TC%ODSSU(idx)%subAlgorithm )
          CASE( ODAS_ALGORITHM )
            ! ...Assumes the same alphas for all TCs
            n = TC%ODSSU(idx)%n_Absorbers
            CALL ODAS_Assemble_Predictors( &
                   Atmosphere                               , &  ! Input
                   GeometryInfo                             , &  ! Input
                   SPREAD(ODAS_MAX_N_ORDERS,DIM=1,NCOPIES=n), &  ! Input
                   TC%ODSSU(idx)%ODAS(1)%Alpha              , &  ! Input
                   Predictor%ODAS                           , &  ! Output
                   iVar%ODAS                                  )  ! Output
          CASE( ODPS_ALGORITHM )
            CALL ODPS_Assemble_Predictors( &
                   TC%ODSSU(idx)%ODPS(1), &  ! Input
                   Atmosphere           , &  ! Input
                   GeometryInfo         , &  ! Input
                   Predictor%ODPS         )  ! Output
        END SELECT
    END SELECT

    ! Is this a Zeeman channel?
    idx = TC%ZSensor_LoIndex(SensorIndex)
    IF( idx > 0 )THEN
      IF( Is_ODZeeman(TC%ODZeeman(idx)) )THEN
        CALL Zeeman_Compute_Predictors( &
               AncillaryInput%Zeeman, &  ! Input
               TC%ODZeeman(idx)     , &  ! Input
               Atmosphere           , &  ! Input
               GeometryInfo         , &  ! Input
               Predictor%ODZeeman     )  ! Output
      END IF
    END IF

  END SUBROUTINE CRTM_Compute_Predictors


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Compute_Predictors_TL
!
! PURPOSE:
!       Subroutine to calculate the gas absorption model tangent-linear
!       predictors. It is a wrapper which calls the algorithm specific routine.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_Predictors_TL( &
!              SensorIndex   , &  ! Input
!              Atmosphere    , &  ! FWD Input
!              Predictor     , &  ! FWD Input
!              Atmosphere_TL , &  ! TL Input
!              AncillaryInput, &  ! Input
!              Predictor_TL  , &  ! TL Output
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
!       Atmosphere:
!         Structure containing the atmospheric state data.
!         UNITS:      N/A
!         TYPE:       CRTM_Atmosphere_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       Predictor:
!         Structure containing the integrated absorber and predictor profiles.
!         UNITS:      N/A
!         TYPE:       CRTM_Predictor_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       Atmosphere_TL:
!         Structure containing the tangent-linear atmospheric state data.
!         UNITS:      N/A
!         TYPE:       CRTM_Atmosphere_type
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
!
! OUTPUTS:
!       Predictor_TL:
!         Structure containing the tangent-linear integrated absorber and
!         predictor profiles.
!         UNITS:      N/A
!         TYPE:       CRTM_Predictor_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_Predictors_TL( &
    SensorIndex   , &  ! Input
    Atmosphere    , &  ! FWD Input
    Predictor     , &  ! FWD Input
    Atmosphere_TL , &  ! TL Input
    AncillaryInput, &  ! Input
    Predictor_TL  , &  ! TL Output
    iVar            )  ! Internal variable input
    ! Arguments
    INTEGER                       , INTENT(IN)     :: SensorIndex
    TYPE(CRTM_Atmosphere_type)    , INTENT(IN)     :: Atmosphere
    TYPE(CRTM_Predictor_type)     , INTENT(IN)     :: Predictor
    TYPE(CRTM_Atmosphere_type)    , INTENT(IN)     :: Atmosphere_TL
    TYPE(CRTM_AncillaryInput_type), INTENT(IN)     :: AncillaryInput
    TYPE(CRTM_Predictor_type)     , INTENT(IN OUT) :: Predictor_TL
    TYPE(iVar_type)               , INTENT(IN)     :: iVar
    ! Local variables
    INTEGER :: idx, n

    ! Call required model
    idx = TC%Sensor_LoIndex(SensorIndex)
    SELECT CASE( TC%Algorithm_ID(SensorIndex) )

      ! Predictors for ODAS transmittance model
      CASE( ODAS_ALGORITHM )
        CALL ODAS_Assemble_Predictors_TL( &
               Atmosphere            , &  ! FWD Input
               Predictor%ODAS        , &  ! FWD Input
               Atmosphere_TL         , &  ! TL Input
               TC%ODAS(idx)%Max_Order, &  ! Input
               TC%ODAS(idx)%Alpha    , &  ! Input
               Predictor_TL%ODAS     , &  ! TL Output
               iVar%ODAS               )  ! Internal variable input

      ! Predictors for ODPS transmittance model
      CASE( ODPS_ALGORITHM )
        CALL ODPS_Assemble_Predictors_TL( &
               TC%ODPS(idx)     , &  ! Input
               Predictor%ODPS   , &  ! FWD Input
               Atmosphere_TL    , &  ! TL Input
               Predictor_TL%ODPS  )  ! TL Output

      ! Predictors for SSU instrument specific model
      CASE( ODSSU_ALGORITHM )

        ! ...Select particular transmittance algorithm for this instrument
        SELECT CASE( TC%ODSSU(idx)%subAlgorithm )
          CASE( ODAS_ALGORITHM )
            ! ...Assumes the same alphas for all TCs
            n = TC%ODSSU(idx)%n_Absorbers
            CALL ODAS_Assemble_Predictors_TL( &
                   Atmosphere                               , &  ! FWD Input
                   Predictor%ODAS                           , &  ! FWD Input
                   Atmosphere_TL                            , &  ! TL Input
                   SPREAD(ODAS_MAX_N_ORDERS,DIM=1,NCOPIES=n), &  ! Input
                   TC%ODSSU(idx)%ODAS(1)%Alpha              , &  ! Input,
                   Predictor_TL%ODAS                        , &  ! TL Output
                   iVar%ODAS                                  )  ! Internal variable input
          CASE( ODPS_ALGORITHM )
            CALL ODPS_Assemble_Predictors_TL( &
                   TC%ODSSU(idx)%ODPS(1), &  ! Input
                   Predictor%ODPS       , &  ! FWD Input
                   Atmosphere_TL        , &  ! TL Input
                   Predictor_TL%ODPS      )  ! TL Output
        END SELECT
    END SELECT

    ! Is this a Zeeman channel?
    idx = TC%ZSensor_LoIndex(SensorIndex)
    IF( idx > 0 )THEN
      IF( Is_ODZeeman(TC%ODZeeman(idx)) )THEN
        CALL Zeeman_Compute_Predictors_TL( &
               AncillaryInput%Zeeman, &  ! Input
               TC%ODZeeman(idx)     , &  ! Input
               Predictor%ODZeeman   , &  ! FWD Input
               Atmosphere_TL        , &  ! TL Input
               Predictor_TL%ODZeeman  )  ! TL Output
      END IF
    END IF

  END SUBROUTINE CRTM_Compute_Predictors_TL


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_Predictors_AD
!
! PURPOSE:
!       Subroutine to calculate the adjoint gas absorption model predictors.
!       It is a wrapper which calls the algorithm specific routine.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_Predictors_AD ( &
!              SensorIndex   , &  ! Input
!              Atmosphere    , &  ! FWD Input
!              Predictor     , &  ! FWD Input
!              Predictor_AD  , &  ! AD Input
!              AncillaryInput, &  ! Input
!              Atmosphere_AD , &  ! AD Output
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
!       Atmosphere:
!         Structure containing the atmospheric state data.
!         UNITS:      N/A
!         TYPE:       CRTM_Atmosphere_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       Predictor:
!         Structure containing the integrated absorber and predictor profiles.
!         UNITS:      N/A
!         TYPE:       CRTM_Predictor_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       Predictor_AD:
!         Structure containing the adjoint integrated absorber and
!         predictor profiles.
!         UNITS:      N/A
!         TYPE:       CRTM_Predictor_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN OUT)
!
!       AncillaryInput:
!         Structure holding ancillary inputs
!         UNITS:      N/A
!         TYPE:       AncillaryInput_type
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
!       Atmosphere_AD:
!         Structure containing the adjoint atmospheric state data.
!         UNITS:      N/A
!         TYPE:       CRTM_Atmosphere_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN OUT)
!
! SIDE EFFECTS:
!       Components of the Predictor_AD structure argument are modified
!       in this function.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_Predictors_AD( &
    SensorIndex   , &  ! Input
    Atmosphere    , &  ! FWD Input
    Predictor     , &  ! FWD Input
    Predictor_AD  , &  ! AD Input
    AncillaryInput, &  ! Input
    Atmosphere_AD , &  ! AD Output
    iVar            )  ! Internal variable input
    ! Arguments
    INTEGER                       , INTENT(IN)     :: SensorIndex
    TYPE(CRTM_Atmosphere_type)    , INTENT(IN)     :: Atmosphere
    TYPE(CRTM_Predictor_type)     , INTENT(IN)     :: Predictor
    TYPE(CRTM_Predictor_type)     , INTENT(IN OUT) :: Predictor_AD
    TYPE(CRTM_AncillaryInput_type), INTENT(IN)     :: AncillaryInput
    TYPE(CRTM_Atmosphere_type)    , INTENT(IN OUT) :: Atmosphere_AD
    TYPE(iVar_type)               , INTENT(IN)     :: iVar
    ! Local variables
    INTEGER :: idx, n

    ! Is this a Zeeman channel?
    idx = TC%ZSensor_LoIndex(SensorIndex)
    IF( idx > 0 )THEN
      IF( Is_ODZeeman(TC%ODZeeman(idx)) )THEN
        CALL Zeeman_Compute_Predictors_AD( &
               AncillaryInput%Zeeman, &  ! Input
               TC%ODZeeman(idx)     , &  ! Input
               Predictor%ODZeeman   , &  ! FWD Input
               Predictor_AD%ODZeeman, &  ! AD Intput
               Atmosphere_AD          )  ! AD Output
      END IF
    END IF

    ! Call required model
    idx = TC%Sensor_LoIndex(SensorIndex)
    SELECT CASE( TC%Algorithm_ID(SensorIndex) )

      ! Predictors for ODAS transmittance model
      CASE( ODAS_ALGORITHM )
        CALL ODAS_Assemble_Predictors_AD( &
               Atmosphere            , &  ! FWD Input
               Predictor%ODAS        , &  ! FWD Input
               Predictor_AD%ODAS     , &  ! AD Intput
               TC%ODAS(idx)%Max_Order, &  ! Input
               TC%ODAS(idx)%Alpha    , &  ! Input
               Atmosphere_AD         , &  ! AD Output
               iVar%ODAS               )  ! Internal variable input

      ! Predictors for ODPS transmittance model
      CASE( ODPS_ALGORITHM )
        CALL ODPS_Assemble_Predictors_AD( &
               TC%ODPS(idx)     , &  ! Input
               Predictor%ODPS   , &  ! FWD Input
               Predictor_AD%ODPS, &  ! AD Intput
               Atmosphere_AD      )  ! AD Output

      ! Predictors for SSU instrument specific model
      CASE( ODSSU_ALGORITHM )

        ! ...Select particular transmittance algorithm for this instrument
        SELECT CASE( TC%ODSSU(idx)%subAlgorithm )
          CASE( ODAS_ALGORITHM )
            ! ...Assumes the same alphas for all TCs
            n = TC%ODSSU(idx)%n_Absorbers
            CALL ODAS_Assemble_Predictors_AD( &
                   Atmosphere                               , &  ! FWD Input
                   Predictor%ODAS                           , &  ! FWD Input
                   Predictor_AD%ODAS                        , &  ! AD Intput
                   SPREAD(ODAS_MAX_N_ORDERS,DIM=1,NCOPIES=n), &  ! Input
                   TC%ODSSU(idx)%ODAS(1)%Alpha              , &  ! Input,
                   Atmosphere_AD                            , &  ! AD Output
                   iVar%ODAS                                  )  ! Internal variable input
          CASE( ODPS_ALGORITHM )
            CALL ODPS_Assemble_Predictors_AD( &
                   TC%ODSSU(idx)%ODPS(1), &  ! Input
                   Predictor%ODPS       , &  ! FWD Input
                   Predictor_AD%ODPS    , &  ! AD Intput
                   Atmosphere_AD          )  ! AD Output
        END SELECT
    END SELECT

  END SUBROUTINE CRTM_Compute_Predictors_AD

END MODULE CRTM_Predictor
