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
! **** REPLACE
  USE CRTM_AtmScatter_Define,     ONLY: CRTM_AtmOptics_type => CRTM_AtmScatter_type
! **** WITH THE FOLLOWING
!  USE CRTM_AtmOptics_Define,      ONLY: CRTM_AtmOptics_type
! ****
  USE ODAS_AtmAbsorption,         ONLY: ODAS_Compute_AtmAbsorption    => Compute_AtmAbsorption,    &
                                        ODAS_Compute_AtmAbsorption_TL => Compute_AtmAbsorption_TL, &
                                        ODAS_Compute_AtmAbsorption_AD => Compute_AtmAbsorption_AD, &
                                        ODAS_AAVariables_type      => AAVariables_type                                      
  USE ODAS_Predictor,             ONLY: ODAS_Compute_Predictors    => Compute_Predictors,    &
                                        ODAS_Compute_Predictors_TL => Compute_Predictors_TL, &
                                        ODAS_Compute_Predictors_AD => Compute_Predictors_AD, &
                                        ODAS_Predictor_type        => Predictor_type,        &
                                        ODAS_Allocate_Predictor    => Allocate_Predictor,    &
                                        ODAS_APVariables_type      => APVariables_type,      &
                                        ODAS_Destroy_Predictor     => Destroy_Predictor,     &
                                        ODAS_MAX_N_PREDICTORS      => MAX_N_PREDICTORS,      &
                                        ODAS_MAX_N_ABSORBERS       => MAX_N_ABSORBERS,       &
                                        ODAS_MAX_N_ORDERS          => MAX_N_ORDERS

  USE ODPS_AtmAbsorption,         ONLY: ODPS_Compute_AtmAbsorption,    &
                                        ODPS_Compute_AtmAbsorption_TL, &
                                        ODPS_Compute_AtmAbsorption_AD, &
                                        ODPS_AAVariables_type,         &
                                        ODPS_Compute_Predictors,       &
                                        ODPS_Compute_Predictors_TL,    &
                                        ODPS_Compute_Predictors_AD,    &
                                        ALLOW_OPTRAN

  USE ODSSU_AtmAbsorption,        ONLY: ODSSU_Compute_AtmAbsorption    => Compute_AtmAbsorption,    &
                                        ODSSU_Compute_AtmAbsorption_TL => Compute_AtmAbsorption_TL, &
                                        ODSSU_Compute_AtmAbsorption_AD => Compute_AtmAbsorption_AD, &
                                        ODSSU_AAVariables_type         => AAVariables_type, &
                                        ODSSU_Compute_AAV                                       
                            
  USE ODPS_Predictor,             ONLY: ODPS_Predictor_type        => Predictor_type,        &
                                        ODPS_APVariables_type,                               &
                                        ODPS_Get_n_Components      => Get_n_Components ,     &
                                        ODPS_Get_max_n_Predicotrs  => Get_max_n_Predicotrs,  &
                                        ODPS_Get_n_Absorbers       => Get_n_Absorbers,       &
                                        ODPS_Destroy_Predictor     => Destroy_Predictor,     &
                                        ODPS_Allocate_Predictor    => Allocate_Predictor,    &
                                        ODPS_Destroy_PAFV          => Destroy_PAFV,          &
                                        ODPS_Allocate_PAFV         => Allocate_PAFV,         &
                                        ODPS_Get_SaveFWVFlag       => Get_SaveFWVFlag
                                         
  USE ODZeeman_AtmAbsorption,     ONLY: Zeeman_Compute_AtmAbsorption,    &
                                        Zeeman_Compute_AtmAbsorption_TL, &
                                        Zeeman_Compute_AtmAbsorption_AD, &
                                        Zeeman_Compute_Predictors,     &
                                        Zeeman_Compute_Predictors_TL,  &
                                        Zeeman_Compute_Predictors_AD,  &
                                        Get_NumOfZComponents, &      
                                        Get_NumOfZAbsorbers,  &      
                                        Get_NumOfZPredictors, &
                                        Is_Zeeman_Channel,    &
                                        Is_ODZeeman

  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! routines in this modules
  PUBLIC :: CRTM_Compute_AtmAbsorption
  PUBLIC :: CRTM_Compute_AtmAbsorption_TL
  PUBLIC :: CRTM_Compute_AtmAbsorption_AD
  PUBLIC :: CRTM_Compute_Predictors
  PUBLIC :: CRTM_Compute_Predictors_TL
  PUBLIC :: CRTM_Compute_Predictors_AD
  PUBLIC :: CRTM_Destroy_Predictor
  PUBLIC :: CRTM_Allocate_Predictor
  ! CRTM Predictor structure type
  PUBLIC :: CRTM_Predictor_type
  ! Internal variable structure
  PUBLIC :: CRTM_AAVariables_type
  PUBLIC :: CRTM_APVariables_type

  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_AtmAbsorption.f90 6125 2009-12-18 20:19:59Z paul.vandelst@noaa.gov $'

  ! Message string length
  INTEGER, PARAMETER :: ML = 256

  ! ---------------------
  ! Structure definitions
  ! ---------------------
  ! Predictor container structure definition
  TYPE :: CRTM_Predictor_type
    PRIVATE
    TYPE(ODAS_Predictor_type)   :: ODAS
    TYPE(ODPS_Predictor_type)   :: ODPS
    TYPE(ODPS_Predictor_type)   :: ODZeeman
    
  END TYPE CRTM_Predictor_type

  ! Structure to hold AtmAbsorption
  ! forward model variables across
  ! FWD, TL, and AD calls
  TYPE :: CRTM_AAVariables_type
    PRIVATE
    TYPE(ODAS_AAVariables_type)   :: ODAS
    TYPE(ODPS_AAVariables_type)   :: ODPS
    TYPE(ODSSU_AAVariables_type)  :: ODSSU
  END TYPE CRTM_AAVariables_type

  ! Structure to hold Predictor
  ! forward model variables across
  ! FWD, TL, and AD calls
  TYPE :: CRTM_APVariables_type
    PRIVATE
    TYPE(ODAS_APVariables_type)   :: ODAS
    TYPE(ODPS_APVariables_type)   :: ODPS
  END TYPE CRTM_APVariables_type

  
CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_AtmAbsorption
!
! PURPOSE:
!       Subroutine to calculate the layer optical depths due to gaseous
!       absorption for a given sensor and channel and atmospheric profile.
!       It is a wrapper which calls the algorithm specific routine.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_AtmAbsorption( SensorIndex   , &  ! Input
!                                        ChannelIndex  , &  ! Input
!                                        AncillaryInput, &  ! Input
!                                        Predictor     , &  ! Input
!                                        AtmOptics     , &  ! Output
!                                        AAV             )  ! Internal variable output
!
! INPUT ARGUMENTS:
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
!       AncillaryInput:  Structure holding ancillary inputs
!                        UNITS:      N/A
!                        TYPE:       AncillaryInput_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Structure containing the integrated absorber and
!                        predictor profile data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Predictor_type 
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!        AtmOptics:      Structure containing computed optical depth
!                        profile data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_AtmOptics_type 
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!        AAV:            Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of this module.
!                        UNITS:      N/A
!                        TYPE:       CRTM_AAVariables_type 
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
! COMMENTS:
!       Note the INTENT on the structure arguments are IN OUT rather
!       than just OUT. This is necessary because the argument is defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_AtmAbsorption( &
    SensorIndex   , &  ! Input
    ChannelIndex  , &  ! Input 
    AncillaryInput, &  ! Input
    Predictor     , &  ! Input                       
    AtmOptics     , &  ! Output                      
    AAV             )  ! Internal variable output    
    ! Arguments
    INTEGER                       , INTENT(IN)     :: SensorIndex
    INTEGER                       , INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_AncillaryInput_type), INTENT(IN)     :: AncillaryInput
    TYPE(CRTM_Predictor_type)     , INTENT(IN OUT) :: Predictor
    TYPE(CRTM_AtmOptics_type)     , INTENT(IN OUT) :: AtmOptics
    TYPE(CRTM_AAVariables_type)   , INTENT(IN OUT) :: AAV
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
               AAV%ODAS        )  ! Internal variable output

      ! ODPS transmittance model
      CASE( ODPS_ALGORITHM )
        CALL ODPS_Compute_AtmAbsorption( &
               TC%ODPS(idx)  , &  ! Input
               ChannelIndex  , &  ! Input
               Predictor%ODPS, &  ! Input
               AtmOptics       )  ! Output

      ! SSU instrument specific
      CASE( ODSSU_ALGORITHM )
        CALL ODSSU_Compute_AAV( &
               AncillaryInput%SSU, &  ! Input
               SensorIndex       , &  ! Input
               ChannelIndex      , &  ! Input
               AAV%ODSSU           )  ! Internal variable output     

        ! ...Select particular transmittance algorithm for this instrument
        SELECT CASE( TC%ODSSU(idx)%subAlgorithm )
          CASE( ODAS_ALGORITHM )
            CALL ODSSU_Compute_AtmAbsorption( &
                   TC%Sensor_LoIndex(SensorIndex), &  ! Input
                   ChannelIndex                  , &  ! Input
                   Predictor%ODAS                , &  ! Input
                   AtmOptics                     , &  ! Output
                   AAV%ODSSU                       )  ! Internal variable output
          CASE( ODPS_ALGORITHM )
            CALL ODSSU_Compute_AtmAbsorption( &
                   TC%Sensor_LoIndex(SensorIndex), &  ! Input
                   ChannelIndex                  , &  ! Input
                   Predictor%ODPS                , &  ! Input
                   AtmOptics                     , &  ! Output
                   AAV%ODSSU                       )  ! Internal variable output
        END SELECT
    END SELECT

  END SUBROUTINE CRTM_Compute_AtmAbsorption               


!------------------------------------------------------------------------------
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
!       CALL CRTM_Compute_AtmAbsorption_TL( SensorIndex   , &  ! Input
!                                           ChannelIndex  , &  ! Input
!                                           Predictor     , &  ! FWD Input
!                                           Predictor_TL  , &  ! TL Input
!                                           AtmOptics_TL  , &  ! TL Output
!                                           AAV             )  ! Internal variable input
!
! INPUT ARGUMENTS:
!       SensorIndex:        Sensor index id. This is a unique index associated
!                           with a (supported) sensor used to access the
!                           shared coefficient data for a particular sensor.
!                           See the ChannelIndex argument.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:       Channel index id. This is a unique index associated
!                           with a (supported) sensor channel used to access the
!                           shared coefficient data for a particular sensor's
!                           channel.
!                           See the SensorIndex argument.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Predictor:          Structure containing the integrated absorber and
!                           predictor profile data.
!                           UNITS:      N/A
!                           TYPE:       CRTM_Predictor_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Predictor_TL:       Structure containing the tangent-linear integrated
!                           absorber and predictor profile data.
!                           UNITS:      N/A
!                           TYPE:       CRTM_Predictor_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       AAV:                Structure containing internal variables required for
!                           subsequent tangent-linear or adjoint model calls.
!                           The contents of this structure are NOT accessible
!                           outside of this module.
!                           UNITS:      N/A
!                           TYPE:       CRTM_AAVariables_type 
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!        AtmOptics_TL:      Structure containing the computed tangent-linear
!                           optical depth profile data.
!                           UNITS:      N/A
!                           TYPE:       CRTM_AtmOptics_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on some structure arguments is IN OUT rather
!       than just OUT. This is necessary because the argument is defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_AtmAbsorption_TL( &
    SensorIndex   , &  ! Input
    ChannelIndex  , &  ! Input
    Predictor     , &  ! Input
    Predictor_TL  , &  ! Input
    AtmOptics_TL  , &  ! Output
    AAV             )  ! Internal variable input
    ! Arguments
    INTEGER                    , INTENT(IN)     :: SensorIndex
    INTEGER                    , INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_Predictor_type)  , INTENT(IN)     :: Predictor
    TYPE(CRTM_Predictor_type)  , INTENT(IN OUT) :: Predictor_TL
    TYPE(CRTM_AtmOptics_type)  , INTENT(IN OUT) :: AtmOptics_TL
    TYPE(CRTM_AAVariables_type), INTENT(IN)     :: AAV
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
               AAV%ODAS           )  ! Internal variable input

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
                   AAV%ODSSU                       )  ! Internal variable input
          CASE( ODPS_ALGORITHM )
            CALL ODSSU_Compute_AtmAbsorption_TL( &
                   TC%Sensor_LoIndex(SensorIndex), &  ! Input
                   ChannelIndex                  , &  ! Input
                   Predictor%ODPS                , &  ! Input
                   Predictor_TL%ODPS             , &  ! Input
                   AtmOptics_TL                  , &  ! Output
                   AAV%ODSSU                       )  ! Internal variable input
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
!       CALL CRTM_Compute_AtmAbsorption_AD( SensorIndex , &  ! Input
!                                           ChannelIndex, &  ! Input
!                                           Predictor   , &  ! FWD Input
!                                           AtmOptics_AD, &  ! AD  Input
!                                           Predictor_AD, &  ! AD  Output
!                                           AAV           )  ! Internal variable input
!
! INPUT ARGUMENTS:
!       SensorIndex:       Sensor index id. This is a unique index associated
!                          with a (supported) sensor used to access the
!                          shared coefficient data for a particular sensor.
!                          See the ChannelIndex argument.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:      Channel index id. This is a unique index associated
!                          with a (supported) sensor channel used to access the
!                          shared coefficient data for a particular sensor's
!                          channel.
!                          See the SensorIndex argument.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Predictor:         Structure containing the integrated absorber and
!                          predictor profile data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_Predictor_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       AtmOptics_AD:      Structure containing the computed adjoint
!                          optical depth profile data.
!                          Set to zero upon output.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmOptics_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
!       AAV:               Structure containing internal variables required for
!                          subsequent tangent-linear or adjoint model calls.
!                          The contents of this structure are NOT accessible
!                          outside of the CRTM_AtmAbsorption module.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AAVariables_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor_AD:      Structure containing the adjoint integrated
!                          absorber and predictor profile data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_Predictor_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
! SIDE EFFECTS:
!       Components of the AtmAbsorption_AD structure argument are modified
!       in this function.
!
!------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_AtmAbsorption_AD( &
    SensorIndex , &  ! Input
    ChannelIndex, &  ! Input
    Predictor   , &  ! FWD Input
    AtmOptics_AD, &  ! AD  Input
    Predictor_AD, &  ! AD  Output
    AAV           )  ! Internal variable input
    ! Arguments
    INTEGER                    , INTENT(IN)     :: SensorIndex
    INTEGER                    , INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_Predictor_type)  , INTENT(IN)     :: Predictor
    TYPE(CRTM_AtmOptics_type)  , INTENT(IN OUT) :: AtmOptics_AD
    TYPE(CRTM_Predictor_type)  , INTENT(IN OUT) :: Predictor_AD
    TYPE(CRTM_AAVariables_type), INTENT(IN)     :: AAV
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
               AAV%ODAS           )  ! Internal variable input

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
                   AAV%ODSSU                       )  ! Internal variable input
          CASE( ODPS_ALGORITHM )
            CALL ODSSU_Compute_AtmAbsorption_AD( &
                   TC%Sensor_LoIndex(SensorIndex), &  ! Input                     
                   ChannelIndex                  , &  ! Input                                  
                   Predictor%ODPS                , &  ! FWD Input
                   AtmOptics_AD                  , &  ! AD Input                                 
                   Predictor_AD%ODPS             , &  ! AD Output                 
                   AAV%ODSSU                       )  ! Internal variable input
        END SELECT
    END SELECT

  END SUBROUTINE CRTM_Compute_AtmAbsorption_AD              


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_Predictors
!
! PURPOSE:
!       Subroutine to calculate the gas absorption model predictors.
!       It is a wrapper which calls the algorithm specific routine.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_Predictors( SensorIndex   , &  ! Input
!                                     Atmosphere    , &  ! Input
!                                     GeometryInfo  , &  ! Input
!                                     AncillaryInput, &  ! Input
!                                     Predictor     , &  ! Output
!                                     APVariables     )  ! Internal variable output
!
! INPUT ARGUMENTS:
!       SensorIndex:     Sensor index id. This is a unique index associated
!                        with a (supported) sensor used to access the
!                        shared coefficient data for a particular sensor.
!                        See the ChannelIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Atmosphere:      CRTM Atmosphere structure containing the atmospheric
!                        state data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
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
!       AncillaryInput:  Structure holding ancillary inputs
!                        UNITS:      N/A
!                        TYPE:       AncillaryInput_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor:       CRTM Predictor structure containing the integrated absorber
!                        and predictor profiles.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       APVariables:     Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_Predictor module.
!                        UNITS:      N/A
!                        TYPE:       CRTM_APVariables_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_Predictors( &
    SensorIndex   , &  ! Input
    Atmosphere    , &  ! Input                                            
    GeometryInfo  , &  ! Input                                         
    AncillaryInput, &  ! Input
    Predictor     , &  ! Output 
    APV             )  ! Internal variable output
    ! Arguments
    INTEGER                       , INTENT(IN)     :: SensorIndex
    TYPE(CRTM_Atmosphere_type)    , INTENT(IN)     :: Atmosphere
    TYPE(CRTM_GeometryInfo_type)  , INTENT(IN)     :: GeometryInfo
    TYPE(CRTM_AncillaryInput_type), INTENT(IN)     :: AncillaryInput
    TYPE(CRTM_Predictor_type)     , INTENT(IN OUT) :: Predictor
    TYPE(CRTM_APVariables_type)   , INTENT(IN OUT) :: APV
    ! Local variables
    INTEGER :: idx, n

    ! Call required model
    idx = TC%Sensor_LoIndex(SensorIndex)  
    SELECT CASE( TC%Algorithm_ID(SensorIndex) )
    
      ! Predictors for ODAS transmittance model
      CASE( ODAS_ALGORITHM )
        CALL ODAS_Compute_Predictors( &
               Atmosphere            , &  ! Input
               GeometryInfo          , &  ! Input        
               TC%ODAS(idx)%Max_Order, &  ! Input
               TC%ODAS(idx)%Alpha    , &  ! Input
               Predictor%ODAS        , &  ! Output
               APV%ODAS                )  ! Output

      ! Predictors for ODPS transmittance model
      CASE( ODPS_ALGORITHM )
         CALL ODPS_Compute_Predictors( &
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
            CALL ODAS_Compute_Predictors( &
                   Atmosphere                               , &  ! Input
                   GeometryInfo                             , &  ! Input
                   SPREAD(ODAS_MAX_N_ORDERS,DIM=1,NCOPIES=n), &  ! Input
                   TC%ODSSU(idx)%ODAS(1)%Alpha              , &  ! Input 
                   Predictor%ODAS                           , &  ! Output
                   APV%ODAS                                   )  ! Output            
          CASE( ODPS_ALGORITHM )
            CALL ODPS_Compute_Predictors( &
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
!
! NAME:
!       CRTM_Compute_Predictors_TL
!
! PURPOSE:
!       Subroutine to calculate the gas absorption model tangent-linear
!       predictors. It is a wrapper which calls the algorithm specific routine.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_Predictors_TL( Atmosphere    , &  ! FWD Input
!                                        Predictor     , &  ! FWD Input
!                                        Atmosphere_TL , &  ! TL Input
!                                        GeometryInfo  , &  ! Input
!                                        AncillaryInput, &  ! Input
!                                        Predictor_TL  , &  ! TL Output
!                                        APVariables     )  ! Internal variable input
!
! INPUT ARGUMENTS:
!       SensorIndex:       Sensor index id. This is a unique index associated
!                          with a (supported) sensor used to access the
!                          shared coefficient data for a particular sensor.
!                          See the ChannelIndex argument.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Atmosphere:        CRTM Atmosphere structure containing the atmospheric
!                          state data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_Atmosphere_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Predictor:         CRTM Predictor structure containing the integrated absorber
!                          and predictor profiles.
!                          UNITS:      N/A
!                          TYPE:       CRTM_Predictor_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Atmosphere_TL:     CRTM Atmosphere structure containing the tangent-linear
!                          atmospheric state data, i.e. the perturbations.
!                          UNITS:      N/A
!                          TYPE:       CRTM_Atmosphere_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       GeometryInfo:      CRTM_GeometryInfo structure containing the
!                          view geometry information.
!                          UNITS:      N/A
!                          TYPE:       CRTM_GeometryInfo_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       AncillaryInput:    Structure holding ancillary inputs 
!                          UNITS:      N/A                    
!                          TYPE:       AncillaryInput_type    
!                          DIMENSION:  Scalar                 
!                          ATTRIBUTES: INTENT(IN)             
!
!       APVariables:       Structure containing internal variables required for
!                          subsequent tangent-linear or adjoint model calls.
!                          The contents of this structure are NOT accessible
!                          outside of the CRTM_Predictor module.
!                          UNITS:      N/A
!                          TYPE:       CRTM_APVariables_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor_TL:      CRTM Predictor structure containing the tangent-linear
!                          integrated absorber and predictor profiles.
!                          UNITS:      N/A
!                          TYPE:       CRTM_Predictor_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_Predictors_TL( &
    SensorIndex   , &  ! Input
    Atmosphere    , &  ! FWD Input
    Predictor     , &  ! FWD Input
    Atmosphere_TL , &  ! TL Input
    GeometryInfo  , &  ! Input
    AncillaryInput, &  ! Input
    Predictor_TL  , &  ! TL Output
    APV             )  ! Internal variable input
    ! Arguments
    INTEGER                       , INTENT(IN)     :: SensorIndex
    TYPE(CRTM_Atmosphere_type)    , INTENT(IN)     :: Atmosphere
    TYPE(CRTM_Predictor_type)     , INTENT(IN)     :: Predictor
    TYPE(CRTM_Atmosphere_type)    , INTENT(IN)     :: Atmosphere_TL
    TYPE(CRTM_GeometryInfo_type)  , INTENT(IN)     :: GeometryInfo
    TYPE(CRTM_AncillaryInput_type), INTENT(IN)     :: AncillaryInput
    TYPE(CRTM_Predictor_type)     , INTENT(IN OUT) :: Predictor_TL
    TYPE(CRTM_APVariables_type)   , INTENT(IN)     :: APV
    ! Local variables
    INTEGER :: idx, n

    ! Call required model
    idx = TC%Sensor_LoIndex(SensorIndex)  
    SELECT CASE( TC%Algorithm_ID(SensorIndex) )
    
      ! Predictors for ODAS transmittance model
      CASE( ODAS_ALGORITHM )
        CALL ODAS_Compute_Predictors_TL( &
               Atmosphere            , &  ! FWD Input
               Predictor%ODAS        , &  ! FWD Input
               Atmosphere_TL         , &  ! TL Input
               GeometryInfo          , &  ! Input    
               TC%ODAS(idx)%Max_Order, &  ! Input
               TC%ODAS(idx)%Alpha    , &  ! Input
               Predictor_TL%ODAS     , &  ! TL Output
               APV%ODAS                )  ! Internal variable input

      ! Predictors for ODPS transmittance model
      CASE( ODPS_ALGORITHM )
        CALL ODPS_Compute_Predictors_TL( &
               TC%ODPS(idx)     , &  ! Input
               Atmosphere       , &  ! FWD Input
               GeometryInfo     , &  ! Input
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
            CALL ODAS_Compute_Predictors_TL( &
                   Atmosphere                               , &  ! FWD Input
                   Predictor%ODAS                           , &  ! FWD Input
                   Atmosphere_TL                            , &  ! TL Input
                   GeometryInfo                             , &  ! Input    
                   SPREAD(ODAS_MAX_N_ORDERS,DIM=1,NCOPIES=n), &  ! Input
                   TC%ODSSU(idx)%ODAS(1)%Alpha              , &  ! Input, 
                   Predictor_TL%ODAS                        , &  ! TL Output
                   APV%ODAS                                   )  ! Internal variable input                 
          CASE( ODPS_ALGORITHM )
            CALL ODPS_Compute_Predictors_TL( &
                   TC%ODSSU(idx)%ODPS(1), &  ! Input
                   Atmosphere           , &  ! FWD Input
                   GeometryInfo         , &  ! Input
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
               Atmosphere           , &  ! FWD Input    
               GeometryInfo         , &  ! Input    
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
!       CALL CRTM_Compute_Predictors_AD ( SensorIndex   , &  ! Input
!                                         Atmosphere    , &  ! FWD Input
!                                         Predictor     , &  ! FWD Input
!                                         Predictor_AD  , &  ! AD Input
!                                         GeometryInfo  , &  ! Input
!                                         AncillaryInput, &  ! Input
!                                         Atmosphere_AD , &  ! AD Output
!                                         APVariables     )  ! Internal variable input
!
! INPUT ARGUMENTS:
!       SensorIndex:       Sensor index id. This is a unique index associated
!                          with a (supported) sensor used to access the
!                          shared coefficient data for a particular sensor.
!                          See the ChannelIndex argument.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Atmosphere:        CRTM Atmosphere structure containing the atmospheric
!                          state data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_Atmosphere_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Predictor:         CRTM Predictor structure containing the integrated absorber
!                          and predictor profiles.
!                          UNITS:      N/A
!                          TYPE:       CRTM_Predictor_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Predictor_AD:      CRTM Predictor structure containing the adjoint
!                          integrated absorber and predictor profiles.
!                          **NOTE: This structure is zeroed upon output
!                          UNITS:      N/A
!                          TYPE:       CRTM_Predictor_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
!       GeometryInfo:      CRTM_GeometryInfo structure containing the
!                          view geometry information.
!                          UNITS:      N/A
!                          TYPE:       CRTM_GeometryInfo_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       AncillaryInput:    Structure holding ancillary inputs
!                          UNITS:      N/A
!                          TYPE:       AncillaryInput_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       APVariables:       Structure containing internal variables required for
!                          subsequent tangent-linear or adjoint model calls.
!                          The contents of this structure are NOT accessible
!                          outside of the CRTM_Predictor module.
!                          UNITS:      N/A
!                          TYPE:       CRTM_APVariables_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT)
!
! OUTPUT ARGUMENTS:
!       Atmosphere_AD:     CRTM Atmosphere structure containing the adjoint
!                          atmospheric state data, i.e. the Jacobians
!                          UNITS:      N/A
!                          TYPE:       CRTM_Atmosphere_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
! SIDE EFFECTS:
!       Components of the Predictor_AD structure argument are modified
!       in this function.
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_Predictors_AD( &
    SensorIndex   , &  ! Input
    Atmosphere    , &  ! FWD Input
    Predictor     , &  ! FWD Input
    Predictor_AD  , &  ! AD Input
    GeometryInfo  , &  ! Input
    AncillaryInput, &  ! Input
    Atmosphere_AD , &  ! AD Output
    APV             )  ! Internal variable input
    ! Arguments
    INTEGER                       , INTENT(IN)     :: SensorIndex
    TYPE(CRTM_Atmosphere_type)    , INTENT(IN)     :: Atmosphere
    TYPE(CRTM_Predictor_type)     , INTENT(IN)     :: Predictor
    TYPE(CRTM_Predictor_type)     , INTENT(IN OUT) :: Predictor_AD
    TYPE(CRTM_GeometryInfo_type)  , INTENT(IN)     :: GeometryInfo
    TYPE(CRTM_AncillaryInput_type), INTENT(IN)     :: AncillaryInput
    TYPE(CRTM_Atmosphere_type)    , INTENT(IN OUT) :: Atmosphere_AD
    TYPE(CRTM_APVariables_type)   , INTENT(IN)     :: APV
    ! Local variables
    INTEGER :: idx, n

    ! Is this a Zeeman channel?
    idx = TC%ZSensor_LoIndex(SensorIndex)
    IF( idx > 0 )THEN
      IF( Is_ODZeeman(TC%ODZeeman(idx)) )THEN
        CALL Zeeman_Compute_Predictors_AD( &
               AncillaryInput%Zeeman, &  ! Input
               TC%ODZeeman(idx)     , &  ! Input
               Atmosphere           , &  ! FWD Input
               GeometryInfo         , &  ! Input    
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
        CALL ODAS_Compute_Predictors_AD( &
               Atmosphere            , &  ! FWD Input
               Predictor%ODAS        , &  ! FWD Input
               Predictor_AD%ODAS     , &  ! AD Intput
               GeometryInfo          , &  ! Input    
               TC%ODAS(idx)%Max_Order, &  ! Input
               TC%ODAS(idx)%Alpha    , &  ! Input
               Atmosphere_AD         , &  ! AD Output
               APV%ODAS                )  ! Internal variable input

      ! Predictors for ODPS transmittance model
      CASE( ODPS_ALGORITHM )
        CALL ODPS_Compute_Predictors_AD( &
               TC%ODPS(idx)     , &  ! Input
               Atmosphere       , &  ! FWD Input
               GeometryInfo     , &  ! Input     
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
            CALL ODAS_Compute_Predictors_AD( &
                   Atmosphere                               , &  ! FWD Input
                   Predictor%ODAS                           , &  ! FWD Input
                   Predictor_AD%ODAS                        , &  ! AD Intput
                   GeometryInfo                             , &  ! Input    
                   SPREAD(ODAS_MAX_N_ORDERS,DIM=1,NCOPIES=n), &  ! Input
                   TC%ODSSU(idx)%ODAS(1)%Alpha              , &  ! Input, 
                   Atmosphere_AD                            , &  ! AD Output
                   APV%ODAS                                   )  ! Internal variable input
          CASE( ODPS_ALGORITHM )
            CALL ODPS_Compute_Predictors_AD( &
                   TC%ODSSU(idx)%ODPS(1), &  ! Input
                   Atmosphere           , &  ! FWD Input
                   GeometryInfo         , &  ! Input     
                   Predictor%ODPS       , &  ! FWD Input
                   Predictor_AD%ODPS    , &  ! AD Intput
                   Atmosphere_AD          )  ! AD Output
        END SELECT
    END SELECT

  END SUBROUTINE CRTM_Compute_Predictors_AD


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Destroy_Predictor
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of
!       a CRTM_Predictor data structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_Predictor( SensorIndex, &  ! Input
!                                              Predictor    )  ! Output
!
! INPUT ARGUMENTS:
!       SensorIndex:     Sensor index id. This is a unique index associated
!                        with a (supported) sensor used to access the
!                        shared coefficient data for a particular sensor.
!                        See the ChannelIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor:      Re-initialized CRTM_Predictor structure.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Predictor_type
!                       DIMENSION:  Scalar OR Rank-1 array
!                       ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the structure re-initialisation was successful
!                          == FAILURE - an error occurred, or
!                                     - the structure internal allocation counter
!                                       is not equal to zero (0) upon exiting this
!                                       function. This value is incremented and
!                                       decremented for every structure allocation
!                                       and deallocation respectively.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output Predictor argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy_Predictor( &
    SensorIndex, &  ! Input            
    Predictor  , &  ! Output           
    No_Clear   ) &  ! Optional input   
  RESULT(Error_Status)                 
    ! Arguments
    INTEGER,                   INTENT(IN)     :: SensorIndex
    TYPE(CRTM_Predictor_type), INTENT(IN OUT) :: Predictor 
    INTEGER,      OPTIONAL   , INTENT(IN)     :: No_Clear
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Predictor'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: idx

    ! Set up
    Error_Status = SUCCESS

    ! Call the required procedure
    idx = TC%Sensor_LoIndex(SensorIndex)
    SELECT CASE( TC%Algorithm_ID(SensorIndex) )
    
      ! Predictors for ODAS transmittance model
      CASE ( ODAS_ALGORITHM )
        Error_Status = ODAS_Destroy_Predictor( Predictor%ODAS, No_Clear=No_Clear )          

      ! Predictors for ODPS transmittance model
      CASE ( ODPS_ALGORITHM )
        Error_Status = ODPS_Destroy_Predictor( Predictor%ODPS, No_Clear=No_Clear )
        ! *****FLAW*****
        ! WHAT IS THIS DOING HERE???
        IF ( Error_Status == SUCCESS ) &
          Error_Status = ODPS_Destroy_PAFV( Predictor%ODPS%PAFV )
        ! *****FLAW*****

      ! Predictors for SSU instrument specific model
      CASE ( ODSSU_ALGORITHM )
      
        ! ...Select particular predictor type for this instrument
        SELECT CASE( TC%ODSSU(idx)%subAlgorithm )
          CASE( ODAS_ALGORITHM )
            Error_Status = ODAS_Destroy_Predictor( Predictor%ODAS, No_Clear=No_Clear )
          CASE( ODPS_ALGORITHM )
            Error_Status = ODPS_Destroy_Predictor( Predictor%ODPS, No_Clear=No_Clear )
            ! *****FLAW*****
            ! AGAIN, WHAT IS THIS DOING HERE???
            IF ( Error_Status == SUCCESS ) &
              Error_Status = ODPS_Destroy_PAFV( Predictor%ODPS%PAFV )
            ! *****FLAW*****
        END SELECT
    END SELECT

    ! Report error, but continue
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error deallocating Predictor structure'
      CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
    END IF                                                                    

    ! Is this a Zeeman channel?
    idx = TC%ZSensor_LoIndex(SensorIndex)
    IF ( idx > 0 ) THEN
      Error_Status = ODPS_Destroy_Predictor( Predictor%ODZeeman, No_Clear=No_Clear )
      ! *****FLAW*****
      ! AND ONE MORE TIME, WHAT IS THIS DOING HERE???
      IF( Error_Status == SUCCESS ) &
        Error_Status = ODPS_Destroy_PAFV( Predictor%ODZeeman%PAFV )
      ! *****FLAW*****
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error deallocating Zeeman Predictor structure'
        CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
      END IF                                                                    
    END IF

  END FUNCTION CRTM_Destroy_Predictor


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Allocate_Predictor
! 
! PURPOSE:
!       Function to allocate the pointer members of the CRTM_Predictor
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_Predictor( SensorIndex, &  ! Input
!                                               n_Layers   , &  ! Input
!                                               Predictor    )  ! Output
!
! INPUT ARGUMENTS:
!       SensorIndex:         Sensor index id. This is a unique index associated
!                            with a (supported) sensor used to access the
!                            shared coefficient data for a particular sensor.
!                            See the ChannelIndex argument.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
!       n_Layers:            Number of atmospheric layers.
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor:           CRTM_Predictor structure with allocated pointer members
!                            UNITS:      N/A
!                            TYPE:       CRTM_Predictor_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       SaveFWV:             Flag indicating the predictor allocation is for FW calculation.
!                            This flag may be used for some algorithms for additional memory 
!                            allocation to save FW variables for TL and AD calculations.
!                            UNITS:      N/A
!                            TYPE:       Integer
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN), OPTIONAL
! 
!
! FUNCTION RESULT:
!       Error_Status:        The return value is an integer defining the error status.
!                            The error codes are defined in the Message_Handler module.
!                            If == SUCCESS the structure re-initialisation was successful
!                               == FAILURE an error occurred
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output Predictor argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Allocate_Predictor( &
    SensorIndex , &  ! Input
    n_Layers    , &  ! Input            
    Predictor   , &  ! Output 
    SaveFWV     ) &  ! Optional Input          
  RESULT( Error_Status )                 
    ! Arguments
    INTEGER                     , INTENT(IN)     :: SensorIndex    
    INTEGER                     , INTENT(IN)     :: n_Layers       
    TYPE(CRTM_Predictor_type)   , INTENT(IN OUT) :: Predictor
    INTEGER,           OPTIONAL , INTENT(IN)     :: SaveFWV
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Predictor'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status
    INTEGER :: i, idx
    
    ! Set up
    Error_Status=SUCCESS

    ! Call the required procedure
    idx = TC%Sensor_LoIndex(SensorIndex)
    SELECT CASE( TC%Algorithm_ID(SensorIndex) )

      ! Predictors for ODAS transmittance model
      CASE( ODAS_ALGORITHM )
         Allocate_Status = ODAS_Allocate_Predictor( &
                             n_Layers                      , &  ! Input
                             ODAS_MAX_N_PREDICTORS         , &  ! Input
                             ODAS_MAX_N_ABSORBERS          , &  ! Input
                             MAXVAL(TC%ODAS(idx)%Max_Order), &  ! Input
                             Predictor%ODAS                  )  ! Output
                             
      ! Predictors for ODPS transmittance model
      CASE( ODPS_ALGORITHM )
        i = TC%ODPS(idx)%Group_Index
        ! ...Set OPTRAN switch
        IF ( TC%ODPS(idx)%n_OCoeffs > 0 .AND. ALLOW_OPTRAN ) &
          Predictor%ODPS%OPTRAN = .TRUE.
        ! ...Allocate main structure
        Allocate_Status = ODPS_Allocate_Predictor( &
                            TC%ODPS(idx)%n_Layers         , &  ! Input - internal layers
                            ODPS_Get_n_Components(i)      , &  ! Input
                            ODPS_Get_max_n_Predicotrs(i)  , &  ! Input                              
                            Predictor%ODPS                , &  ! Output            
                            OPTRAN = Predictor%ODPS%OPTRAN, &  ! Optional Input
                            n_User_Layers = n_Layers        )  ! Optional Input
        ! ...Allocate memory for saved forward variables
        ! *****FLAW*****
        ! MUST CHECK FOR SaveFWV *VALUE* NOT JUST PRESCENCE!
        IF ( PRESENT(SaveFWV) .AND. ODPS_Get_SaveFWVFlag(i) ) THEN
        ! *****FLAW*****
          Allocate_Status = ODPS_Allocate_PAFV( &
                              TC%ODPS(idx)%n_Layers  , & ! Input
                              ODPS_Get_n_Absorbers(i), & ! Input
                              n_Layers               , & ! Input
                              Predictor%ODPS%OPTRAN  , & ! Input
                              Predictor%ODPS%PAFV      ) ! Output
        END IF

      ! Predictors for SSU instrument specific model
      CASE( ODSSU_ALGORITHM )
      
        ! ...Select particular procedure for this instrument
        SELECT CASE( TC%ODSSU(idx)%subAlgorithm )
          CASE( ODAS_ALGORITHM )
            Allocate_Status = ODAS_Allocate_Predictor( &
                                n_Layers             , &  ! Input
                                ODAS_MAX_N_PREDICTORS, &  ! Input
                                ODAS_MAX_N_ABSORBERS , &  ! Input
                                ODAS_MAX_N_ORDERS    , &  ! Input
                                Predictor%ODAS         )  ! Output
          CASE( ODPS_ALGORITHM )
            i = TC%ODSSU(idx)%ODPS(1)%Group_Index
            ! ...Set OPTRAN switch
            IF ( TC%ODSSU(idx)%ODPS(1)%n_OCoeffs > 0 .AND. ALLOW_OPTRAN ) &
              Predictor%ODPS%OPTRAN = .TRUE.
            ! ...Allocate main structure
            Allocate_Status = ODPS_Allocate_Predictor( &
                                TC%ODSSU(idx)%ODPS(1)%n_Layers, &  ! Input - internal layers
                                ODPS_Get_n_Components(i)      , &  ! Input
                                ODPS_Get_max_n_Predicotrs(i)  , &  ! Input                              
                                Predictor%ODPS                , &  ! Output            
                                OPTRAN = Predictor%ODPS%OPTRAN, &  ! Optional Input
                                n_User_Layers = n_Layers        )  ! Optional Input
            ! ...Allocate memory for saved forward variables
            ! *****FLAW*****
            ! MUST CHECK FOR SaveFWV *VALUE* NOT JUST PRESCENCE!
            IF ( PRESENT(SaveFWV) .AND. ODPS_Get_SaveFWVFlag(i) ) THEN
            ! *****FLAW*****
              Allocate_Status = ODPS_Allocate_PAFV( &
                                  TC%ODSSU(idx)%ODPS(1)%n_Layers, & ! Input
                                  ODPS_Get_n_Absorbers(i)       , & ! Input
                                  n_Layers                      , & ! Input
                                  Predictor%ODPS%OPTRAN         , & ! Input
                                  Predictor%ODPS%PAFV             ) ! Output
            END IF
        END SELECT
    END SELECT

    ! Check status
    IF ( Allocate_Status /= SUCCESS ) THEN                                             
      Error_Status = FAILURE                                                              
      WRITE( Message,'("Error allocating Predictor for the sensor index ",i0)' ) SensorIndex  
      CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
      RETURN                                                                            
    END IF                                                                              

    ! Is this a Zeeman channel?
    idx = TC%ZSensor_LoIndex(SensorIndex)
    Zeeman_Block: IF ( idx > 0 ) THEN
      i = TC%ODZeeman(idx)%Group_index
      ! ...Allocate main structure
      Allocate_Status = ODPS_Allocate_Predictor( &
                          TC%ODZeeman(idx)%n_Layers, & ! Input - internal layers
                          Get_NumOfZComponents(i)  , & ! Input
                          Get_NumOfZPredictors(i)  , & ! Input
                          Predictor%ODZeeman       , & ! Output
                          n_User_Layers = n_Layers   ) ! Optional Input
      IF ( Allocate_Status /= SUCCESS ) THEN
        Error_Status=FAILURE
        WRITE( Message,'("Error allocating Zeeman Predictor for the sensor index ",i0)' ) SensorIndex
        CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
        RETURN                                                                                            
      END IF                                                                                              
      ! ...Allocate memory for saved forward variables
      ! *****FLAW*****
      ! MUST CHECK FOR SaveFWV *VALUE* NOT JUST PRESCENCE!
      IF ( PRESENT(SaveFWV) ) THEN
      ! *****FLAW*****
        Allocate_Status = ODPS_Allocate_PAFV( &
                            TC%ODZeeman(idx)%n_Layers , & ! Input
                            Get_NumOfZAbsorbers(i)    , & ! Input
                            n_Layers                  , & ! Input
                            Predictor%ODZeeman%OPTRAN , & ! Input
                            Predictor%ODZeeman%PAFV     ) ! Output
        IF ( Allocate_Status /= SUCCESS ) THEN
          Error_Status=FAILURE
          WRITE( Message,'("Error allocating Zeeman PAFV for the sensor index ",i0)' ) SensorIndex
          CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
          RETURN                                                                                                    
        END IF                                                                                              
      END IF
    END IF Zeeman_Block

  END FUNCTION CRTM_Allocate_Predictor

END MODULE CRTM_AtmAbsorption
