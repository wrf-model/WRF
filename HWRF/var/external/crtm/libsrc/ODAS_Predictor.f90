!
! ODAS_Predictor
!
! Module continaing routines to compute the predictors for the
! Optical Depth Absorber Space (ODAS) gas absorption (AtmAbsorption) model.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 29-Aug-2006
!                       paul.vandelst@ssec.wisc.edu
!
!       Modifed by:     Yong Han, NESDIS/STAR 25-June-2008
!                       yong.han@noaa.gov

MODULE ODAS_Predictor

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds              , ONLY: fp
  USE Message_Handler         , ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters         , ONLY: ZERO                       , &
                                      POINT_25, POINT_5, POINT_75, &
                                      ONE, TWO, THREE, TEN       , &
                                      TOA_PRESSURE               , &
                                      RECIPROCAL_GRAVITY         , &
                                      MAX_N_LAYERS 
  USE CRTM_Atmosphere_Define  , ONLY: CRTM_Atmosphere_type, &
                                      CRTM_Get_AbsorberIdx, &
                                      H2O_ID, O3_ID
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type, &
                                      CRTM_GeometryInfo_GetValue
  USE ODAS_Predictor_Define   , ONLY: Predictor_type      , &
                                      Associated_Predictor, &
                                      Destroy_Predictor   , &
                                      Allocate_Predictor  , &
                                      Assign_Predictor    , &
                                      Zero_Predictor
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Predictor structure data type
  ! in the ODAS_Predictor_Define module
  PUBLIC :: Predictor_type
  ! ODAS_Predictor structure routines inherited
  ! from the ODAS_Predictor_Define module
  PUBLIC :: Associated_Predictor
  PUBLIC :: Destroy_Predictor
  PUBLIC :: Allocate_Predictor
  PUBLIC :: Assign_Predictor
  PUBLIC :: Zero_Predictor
  ! Science routines in this module
  PUBLIC :: Compute_Predictors
  PUBLIC :: Compute_Predictors_TL
  PUBLIC :: Compute_Predictors_AD
  ! Internal variable structure
  PUBLIC :: APVariables_type


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: ODAS_Predictor.f90 7751 2010-05-10 22:32:05Z paul.vandelst@noaa.gov $'

  ! Absorbers in the gas absorption model
  ! -------------------------------------
  ! The total number
  INTEGER, PUBLIC, PARAMETER :: MAX_N_ABSORBERS = 3
  ! The indexing order of the absorbers
  INTEGER, PUBLIC, PARAMETER :: WET_ABSORBER_INDEX = 1
  INTEGER, PUBLIC, PARAMETER :: DRY_ABSORBER_INDEX = 2
  INTEGER, PUBLIC, PARAMETER :: OZO_ABSORBER_INDEX = 3
  ! The absorber index and name arrays
  INTEGER, PUBLIC, PARAMETER, DIMENSION( MAX_N_ABSORBERS ) :: &
    ABSORBER_INDEX = (/ WET_ABSORBER_INDEX, &
                        DRY_ABSORBER_INDEX, &
                        OZO_ABSORBER_INDEX /)
  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( MAX_N_ABSORBERS ) :: &
    ABSORBER_NAME = (/ 'wet', &
                       'dry', &
                       'ozo' /)

  ! Predictors in the gas absorption model
  ! --------------------------------------
  ! Standard predictors are absorber independent
  INTEGER, PUBLIC, PARAMETER :: MAX_N_STANDARD_PREDICTORS   = 11
  ! Integrated predictors are defined for EACH absoreber
  INTEGER, PUBLIC, PARAMETER :: MAX_N_INTEGRATED_PREDICTORS = 6
  ! The total number of predictors
  INTEGER, PUBLIC, PARAMETER :: MAX_N_PREDICTORS = MAX_N_STANDARD_PREDICTORS + &
                                                   ( MAX_N_ABSORBERS * MAX_N_INTEGRATED_PREDICTORS )
  ! The number selected from the total to be
  ! used in the gas absorption algorithm
  INTEGER, PUBLIC, PARAMETER :: MAX_N_PREDICTORS_USED = 6

  ! Maximum number of polynomial orders for
  ! reconstructing the gas absorption coefficients
  ! ----------------------------------------------
  INTEGER, PUBLIC, PARAMETER :: MAX_N_ORDERS = 10

  ! The minimum absorber amount allowed based upon
  ! the smallest representable numbers.
  ! This value is equivalent to TINY(ONE)**0.25
  ! ----------------------------------------------
  REAL(fp), PARAMETER :: MINIMUM_ABSORBER_AMOUNT = TEN**(-RANGE(ONE)/4)


  ! -------------------------------------------------
  ! Structure definition to hold integrated predictor
  ! forward variables across FWD, TL, and AD calls
  ! -------------------------------------------------
  TYPE :: APVariables_type
    PRIVATE
    REAL(fp), DIMENSION(0:MAX_N_LAYERS,MAX_N_ABSORBERS) :: A_2 = ZERO
    REAL(fp), DIMENSION(MAX_N_LAYERS,MAX_N_ABSORBERS) :: Factor_1 = ZERO
    REAL(fp), DIMENSION(MAX_N_LAYERS,MAX_N_ABSORBERS) :: Factor_2 = ZERO
    REAL(fp), DIMENSION(MAX_N_INTEGRATED_PREDICTORS,0:MAX_N_LAYERS,MAX_N_ABSORBERS) :: s ! no need to initialized it to zero
    REAL(fp), DIMENSION(MAX_N_LAYERS,MAX_N_ABSORBERS) :: A_Level ! no need to initialized it to zero

  END TYPE APVariables_type


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                       ## PRIVATE MODULE ROUTINES ##                        ##
!##                                                                            ##
!################################################################################
!################################################################################

!================================================================================
!                  -- INTEGRATED ABSORBER COMPUTATION ROUTINES --
!================================================================================

!--------------------------------------------------------------------------------
!
! NAME:
!       Compute_IntAbsorber
!
! PURPOSE:
!       Subroutine to compute the integrated absorber profiles.
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Compute_IntAbsorber( Atmosphere, &  ! Input
!                                 Predictor   )  ! Output
!
! INPUT ARGUMENTS:
!       Atmosphere:   CRTM Atmosphere structure containing the atmospheric
!                     state data.
!                     UNITS:      N/A
!                     TYPE:       TYPE(CRTM_Atmosphere_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor:    Predictor structure containing the calculated
!                     integrated absorber profiles
!                     UNITS:      N/A
!                     TYPE:       TYPE(Predictor_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE Compute_IntAbsorber( Atm, &  ! Input
                                  Pred )  ! Output
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm
    TYPE(Predictor_type),       INTENT(IN OUT) :: Pred
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_IntAbsorber'
    ! Local variables
    INTEGER  :: k, j
    REAL(fp) :: dPonG
    INTEGER  :: H2O_Idx
    INTEGER  ::  O3_Idx

    ! Initialise 0'th level amounts
    Pred%A(0,WET_ABSORBER_INDEX) = ZERO
    Pred%A(0,DRY_ABSORBER_INDEX) = MIN(TOA_PRESSURE,Atm%Level_Pressure(0))
    Pred%A(0,OZO_ABSORBER_INDEX) = ZERO

    ! Get the atmosphere gaseous absorber indices
    H2O_Idx = CRTM_Get_AbsorberIdx(Atm,H2O_ID)
    O3_Idx  = CRTM_Get_AbsorberIdx(Atm, O3_ID)

    ! Loop over layers, TOA -> SFC
    DO k = 1, Atm%n_Layers

      ! Compute dP/g for the current layer
      dPonG = RECIPROCAL_GRAVITY * (Atm%Level_Pressure(k) - Atm%Level_Pressure(k-1))

      ! Compute and accumulate the sum for the
      ! layer absorber amounts for each absorber
      Pred%A( k, WET_ABSORBER_INDEX ) = Pred%A(k-1,WET_ABSORBER_INDEX) + &
                                        (dPonG * Atm%Absorber(k,H2O_Idx))
      Pred%A( k, DRY_ABSORBER_INDEX ) = Atm%Level_Pressure(k)

      Pred%A( k, OZO_ABSORBER_INDEX ) = Pred%A(k-1,OZO_ABSORBER_INDEX) + &
                                        (dPonG * Atm%Absorber(k,O3_Idx))

    END DO

    ! Modify absorber quantities by the angle secant
    Pred%A = Pred%Secant_Sensor_Zenith * Pred%A

    ! Compute the integrated absorber level
    ! differences and average layer amount
    DO j = 1, Pred%n_Absorbers
      DO k = 1, Pred%n_Layers
        Pred%dA(k,j)   = Pred%A(k,j) - Pred%A(k-1,j)
        Pred%aveA(k,j) = POINT_5 * (Pred%A(k,j) + Pred%A(k-1,j))
      END DO
    END DO

  END SUBROUTINE Compute_IntAbsorber


!--------------------------------------------------------------------------------
!
! NAME:
!       Compute_IntAbsorber_TL
!
! PURPOSE:
!       Subroutine to compute the tangent-linear integrated absorber profiles.
!
! CALLING SEQUENCE:
!       CALL Compute_IntAbsorber_TL( Atmosphere,    &  ! Input
!                                    Atmosphere_TL, &  ! Input      
!                                    Predictor_TL   )  ! Output     
!
! INPUT ARGUMENTS:
!       Atmosphere:     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Atmosphere_TL:  CRTM Atmosphere structure containing the tangent-linear
!                       atmospheric state data, i.e. the perturbations.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!
! OUTPUT ARGUMENTS:
!       Predictor_TL:   Predictor structure containing the calculated
!                       tangent-linear integrated absorber profiles
!                       UNITS:      N/A
!                       TYPE:       TYPE(Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE Compute_IntAbsorber_TL( Atm,    &  ! Input
                                     Atm_TL, &  ! Input      
                                     Pred_TL )  ! Output     
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm_TL
    TYPE(Predictor_type),       INTENT(IN OUT) :: Pred_TL
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_IntAbsorber_TL'
    ! Local variables
    INTEGER  :: k, j
    REAL(fp) :: dPonG
    REAL(fp) :: dPonG_TL
    INTEGER  :: H2O_Idx
    INTEGER  ::  O3_Idx

    ! Initalise 0'th level amounts
    Pred_TL%A(0,:) = ZERO

    ! Get the atmosphere gaseous absorber indices
    H2O_Idx = CRTM_Get_AbsorberIdx(Atm,H2O_ID)
    O3_Idx  = CRTM_Get_AbsorberIdx(Atm, O3_ID)

    ! Loop over layers, TOA -> SFC
    DO k = 1, Atm_TL%n_Layers

      ! Compute dP/g for the current layer
      dPonG    = RECIPROCAL_GRAVITY * (Atm%Level_Pressure(k)    - Atm%Level_Pressure(k-1))
      dPonG_TL = RECIPROCAL_GRAVITY * (Atm_TL%Level_Pressure(k) - Atm_TL%Level_Pressure(k-1))

      ! Compute and accumulate the sum for the
      ! layer absorber amounts for each absorber
      Pred_TL%A(k,WET_ABSORBER_INDEX) = Pred_TL%A(k-1,WET_ABSORBER_INDEX) + &
                                        (dPonG * Atm_TL%Absorber(k,H2O_Idx)) + &
                                        (dPonG_TL * Atm%Absorber(k,H2O_Idx))

      Pred_TL%A(k,DRY_ABSORBER_INDEX) = Atm_TL%Level_Pressure(k)

      Pred_TL%A(k,OZO_ABSORBER_INDEX) = Pred_TL%A(k-1,OZO_ABSORBER_INDEX) + &
                                        (dPonG * Atm_TL%Absorber(k,O3_Idx)) + &
                                        (dPonG_TL * Atm%Absorber(k,O3_Idx))

    END DO

    ! Modify absorber quantities by the angle secant
    Pred_TL%A = Pred_TL%Secant_Sensor_Zenith * Pred_TL%A

    ! Compute the tangent-linear integrated absorber level
    ! differences and average layer amount
    DO j = 1, Pred_TL%n_Absorbers
      DO k = 1, Pred_TL%n_Layers
        Pred_TL%dA(k,j)   = Pred_TL%A(k,j) - Pred_TL%A(k-1,j)
        Pred_TL%aveA(k,j) = POINT_5 * (Pred_TL%A(k,j) + Pred_TL%A(k-1,j))
      END DO
    END DO

  END SUBROUTINE Compute_IntAbsorber_TL


!--------------------------------------------------------------------------------
!
! NAME:
!       Compute_IntAbsorber_AD
!
! PURPOSE:
!       Subroutine to compute the adjoint of the integrated absorber profiles.
!
! CALLING SEQUENCE:
!       CALL Compute_IntAbsorber_AD( Atmosphere,   &  ! Input
!                                     Predictor_AD, &  ! Input     
!                                     Atmosphere_AD )  ! Output    
!
! INPUT ARGUMENTS:
!       Atmosphere:     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Predictor_AD:   Predictor structure that, on input, contains the
!                       calculated adjoint integrated absorber profiles.
!                       These values are set to zero on output.
!                       UNITS:      N/A
!                       TYPE:       TYPE(Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUT ARGUMENTS:
!       Atmosphere_AD:  CRTM Atmosphere structure containing the adjoint
!                       atmospheric state data, i.e. the Jacobians.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! SIDE EFFECTS:
!       Components of the input structure, Predictor_AD, are set to zero
!       on output.
!
!--------------------------------------------------------------------------------

  SUBROUTINE Compute_IntAbsorber_AD( Atm,     &  ! Input
                                     Pred_AD, &  ! Input      
                                     Atm_AD   )  ! Output     
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm
    TYPE(Predictor_type),       INTENT(IN OUT) :: Pred_AD
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atm_AD
    ! Local variables
    INTEGER  :: k, j
    REAL(fp) :: dPonG
    REAL(fp) :: dPonG_AD
    INTEGER  :: H2O_Idx
    INTEGER  ::  O3_Idx

    ! Get the atmosphere gaseous absorber indices
    H2O_Idx = CRTM_Get_AbsorberIdx(Atm,H2O_ID)
    O3_Idx  = CRTM_Get_AbsorberIdx(Atm, O3_ID)

    ! Compute the adjoint integrated absorber level
    ! differences and average layer amount
    DO j = 1, Pred_AD%n_Absorbers
      DO k = Pred_AD%n_Layers, 1, -1
        Pred_AD%A(k-1,j) = Pred_AD%A(k-1,j) + (POINT_5*Pred_AD%aveA(k,j))
        Pred_AD%A(k-1,j) = Pred_AD%A(k-1,j) - Pred_AD%dA(k,j)
        Pred_AD%A(k  ,j) = Pred_AD%A(k  ,j) + (POINT_5*Pred_AD%aveA(k,j))
        Pred_AD%A(k  ,j) = Pred_AD%A(k  ,j) + Pred_AD%dA(k,j)
        Pred_AD%dA(  k,j) = ZERO
        Pred_AD%aveA(k,j) = ZERO
      END DO
    END DO

    ! Modify absorber quantities by the angle secant
    Pred_AD%A = Pred_AD%Secant_Sensor_Zenith * Pred_AD%A

    ! Loop over layers, SFC -> TOA
    DO k = Atm_AD%n_Layers, 1, -1

      ! Compute dP/g for the current layer
      dPonG = RECIPROCAL_GRAVITY * (Atm%Level_Pressure(k) - Atm%Level_Pressure(k-1))

      ! Ozone amount adjoint
      Atm_AD%Absorber(k,O3_Idx) = Atm_AD%Absorber(k,O3_Idx) + &
                                  (dPonG * Pred_AD%A(k,OZO_ABSORBER_INDEX))

      ! Pressure adjoint
      Atm_AD%Level_Pressure(k) = Atm_AD%Level_Pressure(k) + Pred_AD%A(k,DRY_ABSORBER_INDEX)

      ! Water vapor amount adjoint
      Atm_AD%Absorber(k,H2O_Idx) = Atm_AD%Absorber(k,H2O_Idx) + &
                                     (dPonG * Pred_AD%A(k,WET_ABSORBER_INDEX))


      ! dP/g adjoint
      dPonG_AD = ( Atm%Absorber(k, O3_Idx) * Pred_AD%A(k,OZO_ABSORBER_INDEX)) + &
                 ( Atm%Absorber(k,H2O_Idx) * Pred_AD%A(k,WET_ABSORBER_INDEX))

      Atm_AD%Level_Pressure(k-1) = Atm_AD%Level_Pressure(k-1) - (RECIPROCAL_GRAVITY * dPonG_AD)
      Atm_AD%Level_Pressure( k ) = Atm_AD%Level_Pressure( k ) + (RECIPROCAL_GRAVITY * dPonG_AD)

      ! Previous layer absorber amounts
      Pred_AD%A(k-1,OZO_ABSORBER_INDEX) = Pred_AD%A(k-1,OZO_ABSORBER_INDEX) + &
                                          Pred_AD%A( k, OZO_ABSORBER_INDEX)
      Pred_AD%A( k, OZO_ABSORBER_INDEX) = ZERO

      Pred_AD%A( k, DRY_ABSORBER_INDEX) = ZERO

      Pred_AD%A(k-1,WET_ABSORBER_INDEX) = Pred_AD%A(k-1,WET_ABSORBER_INDEX) + &
                                          Pred_AD%A( k, WET_ABSORBER_INDEX)
      Pred_AD%A( k, WET_ABSORBER_INDEX) = ZERO

    END DO

  END SUBROUTINE Compute_IntAbsorber_AD




!================================================================================
!                      -- PREDICTOR COMPUTATION ROUTINES --
!================================================================================

!--------------------------------------------------------------------------------
!
! NAME:
!       Standard_Predictors
!
! PURPOSE:
!       Subroutine to compute the integrated absorber INDEPENDENT
!       predictors for the gas absorption model.
!
! CALLING SEQUENCE:
!       CALL Standard_Predictors( Atmosphere, &  ! Input
!                                 Predictor   )  ! Output
!
! INPUT ARGUMENTS:
!       Atmosphere:   CRTM Atmosphere structure containing the atmospheric
!                     state data.
!                     UNITS:      N/A
!                     TYPE:       TYPE(CRTM_Atmosphere_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor:    Predictor structure containing the calculated
!                     standard predictors.
!                     UNITS:      N/A
!                     TYPE:       TYPE(Predictor_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE Standard_Predictors( Atm, &  ! Input
                                  Pred )  ! Output, Istd x K
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm
    TYPE(Predictor_type),       INTENT(IN OUT) :: Pred
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Standard_Predictors'
    ! Local variables
    INTEGER  :: k
    REAL(fp) :: p2
    REAL(fp) :: t2
    INTEGER  :: H2O_Idx

    ! Get the H2O absorber index
    H2O_Idx = CRTM_Get_AbsorberIdx(Atm,H2O_ID)

    ! Compute the standard predictor set
    Layer_Loop: DO k = 1, Atm%n_Layers

      ! Precalculate the squared terms
      p2 = Atm%Pressure(k)    * Atm%Pressure(k)
      t2 = Atm%Temperature(k) * Atm%Temperature(k)

      ! Calculate the standard predictors
      Pred%X(k, 1) = Atm%Temperature(k)
      Pred%X(k, 2) = Atm%Pressure(k)
      Pred%X(k, 3) = t2
      Pred%X(k, 4) = p2
      Pred%X(k, 5) = Atm%Temperature(k) * Atm%Pressure(k)
      Pred%X(k, 6) = t2 * Atm%Pressure(k)
      Pred%X(k, 7) = Atm%Temperature(k) * p2
      Pred%X(k, 8) = t2 * p2
      Pred%X(k, 9) = Atm%Pressure(k)**POINT_25
      Pred%X(k,10) = Atm%Absorber(k,H2O_Idx)
      Pred%X(k,11) = Atm%Absorber(k,H2O_Idx) / t2

    END DO Layer_Loop

  END SUBROUTINE Standard_Predictors


!--------------------------------------------------------------------------------
!
! NAME:
!       Integrated_Predictors
!
! PURPOSE:
!       Subroutine to compute the integrated absorber DEPENDENT
!       predictors for the gas absorption model.
!
! CALLING SEQUENCE:
!       CALL Integrated_Predictors( Atmosphere,  &  ! Input
!                                   Predictor,   &  ! In/Output                    
!                                   APVariables  )  ! Internal variable output     
!
! INPUT ARGUMENTS:
!       Atmosphere:   CRTM Atmosphere structure containing the atmospheric
!                     state data.
!                     UNITS:      N/A
!                     TYPE:       TYPE(CRTM_Atmosphere_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor:    Predictor structure containing the calculated
!                     integrated predictors.
!                     UNITS:      N/A
!                     TYPE:       TYPE(Predictor_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
!       APVariables:  Structure containing internal variables required for
!                     subsequent tangent-linear or adjoint model calls.
!                     The contents of this structure are NOT accessible
!                     outside of the ODAS_Predictor module.
!                     UNITS:      N/A
!                     TYPE:       APVariables_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)

!--------------------------------------------------------------------------------

  SUBROUTINE Integrated_Predictors( Atm,  &  ! Input
                                    Pred, &  ! Input/output                 
                                    APV   )  ! Internal variable output     
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN)      :: Atm
    TYPE(Predictor_type),       INTENT(IN OUT)  :: Pred
    TYPE(APVariables_type),     INTENT(OUT)     :: APV
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Integrated_Predictors'
    ! Local variables
    INTEGER :: i, i1, j, k
    REAL(fp) :: Inverse_1
    REAL(fp) :: Inverse_2
    REAL(fp) :: Inverse_3
    ! LEVEL Predictor, Iint x 0:K
    REAL(fp), DIMENSION(MAX_N_INTEGRATED_PREDICTORS,0:Atm%n_Layers) :: xL

    ! Begin absorber loop
    Absorber_Loop: DO j = 1, Pred%n_Absorbers

      ! Determine being index of current absorber predictors
      i1 = MAX_N_STANDARD_PREDICTORS + ((j-1) * MAX_N_INTEGRATED_PREDICTORS) + 1

      ! Initialise values
      APV%A_2(0,j) = Pred%A(0,j) * Pred%A(0,j)
      APV%s(:,0,j) = ZERO
      xL(:,0) = ZERO

      ! Compute the integrated predictor set
      Layer_Loop: DO k = 1, Pred%n_Layers

        ! Calculate Absorber multiplicative Factors
        APV%A_2(k,j)      = Pred%A(k,j)*Pred%A(k,j)
        APV%Factor_1(k,j) = (Pred%A(k,j)  + Pred%A(k-1,j) ) * Pred%dA(k,j) ! For ** terms
        APV%Factor_2(k,j) = (APV%A_2(k,j) + APV%A_2(k-1,j)) * Pred%dA(k,j) ! For *** terms

        ! Calculate the intermediate sums
        APV%s(1,k,j) = APV%s(1,k-1,j) + ( Atm%Temperature(k) * Pred%dA(k,j) )      ! T*
        APV%s(2,k,j) = APV%s(2,k-1,j) + ( Atm%Pressure(k)    * Pred%dA(k,j) )      ! P*
        APV%s(3,k,j) = APV%s(3,k-1,j) + ( Atm%Temperature(k) * APV%Factor_1(k,j) ) ! T**
        APV%s(4,k,j) = APV%s(4,k-1,j) + ( Atm%Pressure(k)    * APV%Factor_1(k,j) ) ! P**
        APV%s(5,k,j) = APV%s(5,k-1,j) + ( Atm%Temperature(k) * APV%Factor_2(k,j) ) ! T***
        APV%s(6,k,j) = APV%s(6,k-1,j) + ( Atm%Pressure(k)    * APV%Factor_2(k,j) ) ! P***

        ! Calculate the normalising factors
        ! for the integrated predictors
        IF ( Pred%A(k,j) > MINIMUM_ABSORBER_AMOUNT ) THEN
          Inverse_1 = ONE / Pred%A(k,j)
        ELSE
          Inverse_1 = ZERO
        END IF
        Inverse_2 = Inverse_1 * Inverse_1
        Inverse_3 = Inverse_2 * Inverse_1

        ! Compute the LEVEL integrated predictors
        xL(1,k) = POINT_5  * APV%s(1,k,j) * Inverse_1  ! T*
        xL(2,k) = POINT_5  * APV%s(2,k,j) * Inverse_1  ! P*
        xL(3,k) = POINT_5  * APV%s(3,k,j) * Inverse_2  ! T**
        xL(4,k) = POINT_5  * APV%s(4,k,j) * Inverse_2  ! P**
        xL(5,k) = POINT_75 * APV%s(5,k,j) * Inverse_3  ! T***
        xL(6,k) = POINT_75 * APV%s(6,k,j) * Inverse_3  ! P***

        ! Sum predictors for current absorber across layers
        DO i = 1, MAX_N_INTEGRATED_PREDICTORS
          Pred%X(k,i1+i-1) = xL(i,k) + xL(i,k-1)
        END DO

      END DO Layer_Loop
    END DO Absorber_Loop

  END SUBROUTINE Integrated_Predictors


!--------------------------------------------------------------------------------
!
! NAME:
!       Standard_Predictors_TL
!
! PURPOSE:
!       Subroutine to compute the integrated absorber INDEPENDENT
!       tangent-linear predictors for the gas absorption model.
!
! CALLING SEQUENCE:
!       CALL Standard_Predictors_TL( Atmosphere,    &  ! Input
!                                    Atmosphere_TL, &  ! Input      
!                                    Predictor_TL   )  ! Output     
!
! INPUT ARGUMENTS:
!       Atmosphere:     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Atmosphere_TL:  CRTM Atmosphere structure containing the tangent-linear
!                       atmospheric state data, i.e. the perturbations.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor_TL:   Predictor structure containing the calculated
!                       tangent-linear standard predictors.
!                       UNITS:      N/A
!                       TYPE:       TYPE(Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE Standard_Predictors_TL( Atm,    &  ! Input
                                     Atm_TL, &  ! Input      
                                     Pred_TL )  ! Output     
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN)  :: Atm
    TYPE(CRTM_Atmosphere_type), INTENT(IN)  :: Atm_TL
    TYPE(Predictor_type),       INTENT(IN OUT)  :: Pred_TL
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Standard_Predictors_TL'
    ! Local variables
    INTEGER  :: k
    REAL(fp) :: p2, p2_TL
    REAL(fp) :: t2, t2_TL
    INTEGER  :: H2O_Idx

    ! Get the H2O absorber index
    H2O_Idx = CRTM_Get_AbsorberIdx(Atm,H2O_ID)

    ! Compute the tangent-linear standard predictor set
    Layer_loop: DO k = 1, Atm%n_Layers

      ! Precalculate the squared terms
      p2 = Atm%Pressure(k)    * Atm%Pressure(k)
      t2 = Atm%Temperature(k) * Atm%Temperature(k)

      ! Tangent-linear of squared terms
      p2_TL = TWO * Atm%Pressure(k)    * Atm_TL%Pressure(k)
      t2_TL = TWO * Atm%Temperature(k) * Atm_TL%Temperature(k)
      
      ! Calculate and assign the integrated absorber independent predictors
      Pred_TL%X(k, 1) = Atm_TL%Temperature(k)
      Pred_TL%X(k, 2) = Atm_TL%Pressure(k)
      Pred_TL%X(k, 3) = t2_TL
      Pred_TL%X(k, 4) = p2_TL
      Pred_TL%X(k, 5) = ( Atm%Temperature(k) * Atm_TL%Pressure(k)    ) + &
                      ( Atm%Pressure(k)    * Atm_TL%Temperature(k) )
      Pred_TL%X(k, 6) = ( Atm%Pressure(k) * t2_TL ) + &
                      ( t2 * Atm_TL%Pressure(k) )
      Pred_TL%X(k, 7) = ( Atm%Temperature(k) * p2_TL ) + &
                      ( p2 * Atm_TL%Temperature(k) )
      Pred_TL%X(k, 8) = ( t2 * p2_TL ) + &
                      ( p2 * t2_TL )
      Pred_TL%X(k, 9) = POINT_25 * (Atm%Pressure(k)**(-POINT_75)) * Atm_TL%Pressure(k)
      Pred_TL%X(k,10) = Atm_TL%Absorber(k,H2O_Idx)
      Pred_TL%X(k,11) = ( Atm_TL%Absorber(k,H2O_Idx) - &
                        ( Atm%Absorber(k,H2O_Idx) * t2_TL / t2 ) ) / t2

    END DO Layer_loop

  END SUBROUTINE Standard_Predictors_TL


!--------------------------------------------------------------------------------
!
! NAME:
!       Integrated_Predictors_TL
!
! PURPOSE:
!       Subroutine to compute the integrated absorber amount DEPENDENT
!       tangent-linear predictors for the gas absorption model.
!
! CALLING SEQUENCE:
!       CALL Integrated_Predictors_TL( Atmosphere,    &  ! Input
!                                      Predictor,     &  ! Input                       
!                                      Atmosphere_TL, &  ! Input                       
!                                      Predictor_TL,  &  ! In/Output                   
!                                      APVariables    )  ! Internal variable input     
!
! INPUT ARGUMENTS:
!       Atmosphere:      CRTM Atmosphere structure containing the atmospheric
!                        state data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Atmosphere_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Predictor structure containing the calculated
!                        integrated predictors.
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Atmosphere_TL:   CRTM Atmosphere structure containing the tangent-linear
!                        atmospheric state data, i.e. the perturbations.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Atmosphere_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       APVariables:     Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the Predictor module.
!                        UNITS:      N/A
!                        TYPE:       APVariables_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
! OUTPUT ARGUMENTS:
!       Predictor_TL:    Predictor structure containing the calculated
!                        tangent-linear integrated predictors.
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE Integrated_Predictors_TL( Atm,     &  ! Input
                                       Pred,    &  ! Input                       
                                       Atm_TL,  &  ! Input                       
                                       Pred_TL, &  ! Output                      
                                       APV      )  ! Internal variable input     
    ! Arguments
    TYPE(CRTM_Atmosphere_type),  INTENT(IN)      :: Atm
    TYPE(Predictor_type),        INTENT(IN)      :: Pred
    TYPE(CRTM_Atmosphere_type),  INTENT(IN)      :: Atm_TL
    TYPE(Predictor_type),        INTENT(IN OUT)  :: Pred_TL
    TYPE(APVariables_type),      INTENT(IN)      :: APV
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Integrated_Predictors_TL'
    ! Local variables
    INTEGER :: i, i1, j, k
    REAL(fp) :: Factor_1_TL
    REAL(fp) :: Factor_2_TL
    REAL(fp) :: Inverse_1
    REAL(fp) :: Inverse_2
    REAL(fp) :: Inverse_3
    REAL(fp) :: Inverse_4
    REAL(fp) :: Inverse_1_TL
    REAL(fp) :: Inverse_2_TL
    REAL(fp) :: Inverse_3_TL
    ! Square of the Absorber amount. 0:K
    REAL(fp), DIMENSION(0:Atm%n_Layers) :: A_2_TL
    ! Intermediate summation arrays. Iint
    REAL(fp), DIMENSION(MAX_N_INTEGRATED_PREDICTORS) :: s_TL
    ! LEVEL Predictor, Iint x 0:K
    REAL(fp), DIMENSION(MAX_N_INTEGRATED_PREDICTORS,0:Atm%n_Layers) :: xL_TL

    ! Begin absorber loop
    Absorber_Loop: DO j = 1, Pred_TL%n_Absorbers

      ! Determine being index of current absorber predictors
      i1 = MAX_N_STANDARD_PREDICTORS + ((j-1) * MAX_N_INTEGRATED_PREDICTORS) + 1

      ! Initialise values
      A_2_TL(0) = TWO * Pred%A(0,j) * Pred_TL%A(0,j)
      s_TL(:)    = ZERO
      xL_TL(:,0) = ZERO

      ! Compute the integrated predictor set
      Layer_loop: DO k = 1, Atm%n_Layers

        ! Calculate absorber multiplicative Factors
        A_2_TL(k) = TWO * Pred%A(k,j) * Pred_TL%A(k,j)
        ! For the ** terms
        Factor_1_TL = ( ( Pred%A(k,j)    + Pred%A(k-1,j)    ) * Pred_TL%dA(k,j) ) + &
                      ( ( Pred_TL%A(k,j) + Pred_TL%A(k-1,j) ) *    Pred%dA(k,j) )
        ! For the *** terms       
        Factor_2_TL = ( ( APV%A_2(k,j) + APV%A_2(k-1,j)) * Pred_TL%dA(k,j) ) + &
                      ( ( A_2_TL(k)    + A_2_TL(k-1)   ) *    Pred%dA(k,j) )

        ! Calculate the intermediate sums
        s_TL(1) = s_TL(1) + ( Atm_TL%Temperature(k) *    Pred%dA(k,j)) + &   ! T*
                            ( Atm%Temperature(k)    * Pred_TL%dA(k,j))
        s_TL(2) = s_TL(2) + ( Atm_TL%Pressure(k)    *    Pred%dA(k,j)) + &   ! P*
                            ( Atm%Pressure(k)       * Pred_TL%dA(k,j))
        s_TL(3) = s_TL(3) + ( Atm_TL%Temperature(k) * APV%Factor_1(k,j)) + & ! T**
                            ( Atm%Temperature(k)    * Factor_1_TL )
        s_TL(4) = s_TL(4) + ( Atm_TL%Pressure(k)    * APV%Factor_1(k,j)) + & ! P**
                            ( Atm%Pressure(k)       * Factor_1_TL )
        s_TL(5) = s_TL(5) + ( Atm_TL%Temperature(k) * APV%Factor_2(k,j)) + & ! T***
                            ( Atm%Temperature(k)    * Factor_2_TL )
        s_TL(6) = s_TL(6) + ( Atm_TL%Pressure(k)    * APV%Factor_2(k,j)) + & ! P***
                            ( Atm%Pressure(k)       * Factor_2_TL )

        ! Calculate the normalising factors
        ! for the integrated predictors
        IF ( Pred%A(k,j) > MINIMUM_ABSORBER_AMOUNT ) THEN
          Inverse_1 = ONE / Pred%A(k,j)
        ELSE
          Inverse_1 = ZERO
        END IF
        Inverse_2 = Inverse_1 * Inverse_1
        Inverse_3 = Inverse_2 * Inverse_1
        Inverse_4 = Inverse_3 * Inverse_1
        Inverse_1_TL = -Inverse_2 * Pred_TL%A(k,j)
        Inverse_2_TL = -Inverse_3 * Pred_TL%A(k,j) * TWO
        Inverse_3_TL = -Inverse_4 * Pred_TL%A(k,j) * THREE

        ! Compute the tangent-linear LEVEL integrated predictors
        xL_TL(1,k) = POINT_5  * ( ( s_TL(1)      * Inverse_1    ) + &  ! T*
                                  ( APV%s(1,k,j) * Inverse_1_TL ) )
        xL_TL(2,k) = POINT_5  * ( ( s_TL(2)      * Inverse_1    ) + &  ! P*
                                  ( APV%s(2,k,j) * Inverse_1_TL ) )
        xL_TL(3,k) = POINT_5  * ( ( s_TL(3)      * Inverse_2    ) + &  ! T**
                                  ( APV%s(3,k,j) * Inverse_2_TL ) )
        xL_TL(4,k) = POINT_5  * ( ( s_TL(4)      * Inverse_2    ) + &  ! P**
                                  ( APV%s(4,k,j) * Inverse_2_TL ) )
        xL_TL(5,k) = POINT_75 * ( ( s_TL(5)      * Inverse_3    ) + &  ! T***
                                  ( APV%s(5,k,j) * Inverse_3_TL ) )
        xL_TL(6,k) = POINT_75 * ( ( s_TL(6)      * Inverse_3    ) + &  ! P***
                                  ( APV%s(6,k,j) * Inverse_3_TL ) )

        ! Sum predictors across layers
        DO i = 1, MAX_N_INTEGRATED_PREDICTORS
          Pred_TL%X(k,i1+i-1) = xL_TL(i,k) + xL_TL(i,k-1)
        END DO

      END DO Layer_loop
    END DO Absorber_Loop

  END SUBROUTINE Integrated_Predictors_TL


!--------------------------------------------------------------------------------
!
! NAME:
!       Standard_Predictors_AD
!
! PURPOSE:
!       Subroutine to compute the integrated absorber amount INDEPENDENT
!       predictors for the adjoint gas absorption model.
!
! CALLING SEQUENCE:
!       CALL Standard_Predictors_AD( Atmosphere,   &  ! Input
!                                    Predictor_AD, &  ! Input      
!                                    Atmosphere_AD )  ! Output     
!                                         
!
! INPUT ARGUMENTS:
!       Atmosphere:     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Predictor_AD:   Predictor structure containing the calculated
!                       adjoint integrated predictors.
!                       UNITS:      N/A
!                       TYPE:       TYPE(Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUT ARGUMENTS:
!       Atmosphere_AD:  CRTM Atmosphere structure containing the adjoints of
!                       the standard predictors.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note that the output adjoint argument, Atmosphere_AD, has INTENT of
!       IN OUT. This is because the pressure, temperature, and absorber
!       components of the Atmosphere_AD structure are assumed to have some
!       initial value (which could simply be zero) that is added to when
!       contructing the pressure, temperature and absorber adjoints.
!
!--------------------------------------------------------------------------------

  SUBROUTINE Standard_Predictors_AD( Atm,     &  ! Input
                                     Pred_AD, &  ! Input      
                                     Atm_AD   )  ! Output     
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm
    TYPE(Predictor_type),       INTENT(IN OUT) :: Pred_AD
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atm_AD
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Standard_Predictors_AD'
    ! Local variables
    INTEGER  :: k
    REAL(fp) :: p2, p2_AD
    REAL(fp) :: t2, t2_AD
    REAL(fp) :: t4
    INTEGER  :: H2O_Idx

    ! Get the H2O absorber index
    H2O_Idx = CRTM_Get_AbsorberIdx(Atm,H2O_ID)

    ! Compute the standard predictor set
    ! Don't have to loop backwards here as this is a parallel loop
    Layer_loop: DO k = 1, Atm%n_Layers

      ! Precalculate the squared terms
      p2 = Atm%Pressure(k)    * Atm%Pressure(k)
      t2 = Atm%Temperature(k) * Atm%Temperature(k)
      t4 = t2 * t2

      ! Pressure squared adjoint
      p2_AD =                        Pred_AD%X(k,4)   + &   ! Predictor #4, P^2
              ( Atm%Temperature(k) * Pred_AD%X(k,7) ) + &   ! Predictor #7, T.P^2
              ( t2                 * Pred_AD%X(k,8) )       ! Predictor #8, T^2.P^2

      ! Temperature squared adjoint
      t2_AD =                             Pred_AD%X(k,3)     + &  ! Predictor #3, T^2
              ( Atm%Pressure(k)         * Pred_AD%X(k,6) )   + &  ! Predictor #6, T^2.P
              ( p2                      * Pred_AD%X(k,8) )   + &  ! Predictor #8, T^2.P^2
              (-Atm%Absorber(k,H2O_Idx) * Pred_AD%X(k,11) / t4 )  ! Predictor #11, W/T^2

      ! Water vapor adjoint
      Atm_AD%Absorber(k,H2O_Idx) = Atm_AD%Absorber(k,H2O_Idx) + &
          Pred_AD%X(k,10) + &     ! Predictor #10, W
        ( Pred_AD%X(k,11) / t2 )  ! Predictor #11, W/T^2

      ! Temperature adjoint
      Atm_AD%Temperature(k) = Atm_AD%Temperature(k) + &
        ( TWO * Atm%Temperature(k) * t2_AD ) + &  ! T^2 term
                            Pred_AD%X(k,1)   + &  ! Predictor #1, T
        ( Atm%Pressure(k) * Pred_AD%X(k,5) ) + &  ! Predictor #5, T.P
        ( p2              * Pred_AD%X(k,7) )      ! Predictor #7, T.P^2

      ! Pressure adjoint
      Atm_AD%Pressure(k) = Atm_AD%Pressure(k) + &
        ( TWO * Atm%Pressure(k) * p2_AD )       + &                     ! P^2 term
                               Pred_AD%X(k,2)   + &                     ! Predictor #2, P
        ( Atm%Temperature(k) * Pred_AD%X(k,5) ) + &                     ! Predictor #5, T.P
        ( t2                 * Pred_AD%X(k,6) ) + &                     ! Predictor #6, T^2.P
        ( POINT_25 * (Atm%Pressure(k)**(-POINT_75)) * Pred_AD%X(k,9) )  ! Predictor #9, P^1/4

    END DO Layer_loop

  END SUBROUTINE Standard_Predictors_AD


!--------------------------------------------------------------------------------
!
! NAME:
!       Integrated_Predictors_AD
!
! PURPOSE:
!       Subroutine to compute the integrated absorber amount DEPENDENT
!       predictors for the adjoint gas absorption model.
!
! CALLING SEQUENCE:
!       CALL Integrated_Predictors_AD( Atmosphere,    &  ! Input
!                                      Predictor,     &  ! Input                       
!                                      Predictor_AD,  &  ! In/Output                   
!                                      Atmosphere_AD, &  ! Output                      
!                                      APVariables    )  ! Internal variable input     
!
! INPUT ARGUMENTS:
!       Atmosphere:      CRTM Atmosphere structure containing the atmospheric
!                        state data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Atmosphere_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Predictor structure containing the calculated
!                        integrated predictors.
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor_AD:    Predictor structure that, on input, contains
!                        the adjoint integrated predictors.
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUT ARGUMENTS:
!       Predictor_AD:    Predictor structure that, on output, contains
!                        the adjoint integrated absorber amounts.
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       Atmosphere_AD:   CRTM Atmosphere structure containing the adjoints of
!                        the integrated predictors.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Atmosphere_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note that all the adjoint arguments have INTENTs of IN OUT. This is
!       because they are assumed to have some value upon entry even if they 
!       are labeled as output arguments.
!
!--------------------------------------------------------------------------------

  SUBROUTINE Integrated_Predictors_AD( Atm,     &  ! Input
                                       Pred,    &  ! Input                       
                                       Pred_AD, &  ! In/Output                   
                                       Atm_AD,  &  ! Output                      
                                       APV      )  ! Internal variable input     
    ! Arguments
    TYPE(CRTM_Atmosphere_type),  INTENT(IN)     :: Atm
    TYPE(Predictor_type),        INTENT(IN)     :: Pred
    TYPE(Predictor_type),        INTENT(IN OUT) :: Pred_AD
    TYPE(CRTM_Atmosphere_type),  INTENT(IN OUT) :: Atm_AD
    TYPE(APVariables_type),      INTENT(IN)     :: APV
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Integrated_Predictors_AD'
    ! Local variables
    INTEGER :: i, i1, j, k
    REAL(fp) :: d_A_AD
    REAL(fp) :: Factor_1_AD
    REAL(fp) :: Factor_2_AD
    REAL(fp) :: Inverse_1
    REAL(fp) :: Inverse_2
    REAL(fp) :: Inverse_3
    REAL(fp) :: Inverse_4
    REAL(fp) :: Inverse_1_AD
    REAL(fp) :: Inverse_2_AD
    REAL(fp) :: Inverse_3_AD
    REAL(fp) :: Multiplier
    REAL(fp) :: Add_Factor
    ! Square of the absorber amount. 0:K
    REAL(fp), DIMENSION(0:Atm%n_Layers) :: A_2_AD
    ! Intermediate summation array, Iint x 0:K and Iint
    REAL(fp), DIMENSION(MAX_N_INTEGRATED_PREDICTORS) :: s_AD
    ! LEVEL predictor, Iint x 0:K
    REAL(fp), DIMENSION(MAX_N_INTEGRATED_PREDICTORS,0:Atm%n_Layers ) :: xL_AD

    ! Begin absorber loop
    Absorber_Loop: DO j = 1, Pred_AD%n_Absorbers

      ! Determine being index of current absorber predictors
      i1 = MAX_N_STANDARD_PREDICTORS + ((j-1) * MAX_N_INTEGRATED_PREDICTORS) + 1

      ! Initialise values
      xL_AD(:,Atm%n_Layers) = ZERO
      s_AD(:)               = ZERO
      A_2_AD(Atm%n_Layers)  = ZERO

      ! Compute the integrated predictor set adjoints
      Layer_Loop: DO k = Atm%n_Layers, 1, -1

        ! Calculate the normalising factors
        ! for the integrated predictors
        IF ( Pred%A(k,j) > MINIMUM_ABSORBER_AMOUNT ) THEN
          Inverse_1 = ONE / Pred%A(k,j)
        ELSE
          Inverse_1 = ZERO
        END IF

        Inverse_2 = Inverse_1 * Inverse_1
        Inverse_3 = Inverse_2 * Inverse_1
        Inverse_4 = Inverse_3 * Inverse_1

        ! Adjoint of predictor summation across layers
        DO i = 1, MAX_N_INTEGRATED_PREDICTORS
          xL_AD(i,k)   = xL_AD(i,k) + Pred_AD%X(k,i1+i-1)
          xL_AD(i,k-1) = Pred_AD%X(k,i1+i-1)
        END DO

        ! Adjoint of the LEVEL integrated predictors intermediate sums
        !
        ! Note that the adjoint variables Inverse_X_AD are local to this
        ! loop iteration so they are simply assigned when they are first
        ! used.
        !
        ! P* and T*, Predictor indices #2 and 1
        ! Simply assign a value for Inverse_1_AD
        Multiplier   = POINT_5 * Inverse_1
        s_AD(1)    = s_AD(1) + ( Multiplier * xL_AD(1,k) )
        s_AD(2)    = s_AD(2) + ( Multiplier * xL_AD(2,k) )
        Inverse_1_AD = POINT_5 * ( ( APV%s(1,k,j) * xL_AD(1,k) ) + &
                                   ( APV%s(2,k,j) * xL_AD(2,k) ) )
        ! P** and T**, Predictor indices #4 and 3
        Multiplier   = POINT_5 * Inverse_2
        s_AD(3)    = s_AD(3) + ( Multiplier * xL_AD(3,k) )
        s_AD(4)    = s_AD(4) + ( Multiplier * xL_AD(4,k) )
        Inverse_2_AD = POINT_5 * ( ( APV%s(3,k,j) * xL_AD(3,k) ) + &
                                   ( APV%s(4,k,j) * xL_AD(4,k) ) )
        ! P*** and T***, Predictor indices #6 and 5
        Multiplier   = POINT_75 * Inverse_3
        s_AD(5)    = s_AD(5) + ( Multiplier * xL_AD(5,k) )
        s_AD(6)    = s_AD(6) + ( Multiplier * xL_AD(6,k) )
        Inverse_3_AD = POINT_75 * ( ( APV%s(5,k,j) * xL_AD(5,k) ) + &
                                    ( APV%s(6,k,j) * xL_AD(6,k) ) )

        ! Adjoint of Inverse terms. Note that the Inverse_X_AD
        ! terms are *not* zeroed out as they are re-assigned values
        ! each loop iteration above.
        Pred_AD%A(k,j) = Pred_AD%A(k,j) - (Inverse_2 * Inverse_1_AD ) - &
                                          (TWO   * Inverse_3 * Inverse_2_AD ) - &
                                          (THREE * Inverse_4 * Inverse_3_AD )

        ! Pressure adjoint
        Atm_AD%Pressure(k) = Atm_AD%Pressure(k) + &
                             ( Pred%dA(k,j)      * s_AD(2) ) + &  ! P*
                             ( APV%Factor_1(k,j) * s_AD(4) ) + &  ! P**
                             ( APV%Factor_2(k,j) * s_AD(6) )      ! P***


        ! Temperature adjoint
        Atm_AD%Temperature(k) = Atm_AD%Temperature(k) + &
                                ( Pred%dA(k,j)      * s_AD(1) ) + &  ! T*
                                ( APV%Factor_1(k,j) * s_AD(3) ) + &  ! T**
                                ( APV%Factor_2(k,j) * s_AD(5) )      ! T***

        ! Adjoint of the absorber amount
        !
        ! Note that the adjoint variables Factor_X_AD and
        ! d_A_AD are local to this loop iteration
        ! so they are simply assigned when they are first
        ! used (and thus not zeroed out at the end of each
        ! iteration)
        !
        ! Note there are no
        !   s_AD() = 0
        ! because all the tangent-linear forms are
        !   s_TL() = s_TL() + (...)
        ! summing from the previous Layer.
        !
        ! Multiplicative factors
        Factor_1_AD = ( Atm%Temperature(k) * s_AD(3) ) + &
                      ( Atm%Pressure(k)    * s_AD(4) )

        Factor_2_AD = ( Atm%Temperature(k) * s_AD(5) ) + &
                      ( Atm%Pressure(k)    * s_AD(6) )

        ! Adjoint of the square integrated absorber amount.
        !
        ! Note that A_2_AD() is a LOCAL adjoint variable,
        ! so the initialisation of A_2_AD(k-1) here for
        ! each "k-1" is o.k. rather than
        !   A_2_AD(k-1) = A_2_AD(k-1) + ( d_A(k) * Factor_2_AD )
        !   A_2_AD( k ) = A_2_AD( k ) + ( d_A(k) * Factor_2_AD )
        ! since only A_2_AD( n_Layers ) is initialised outside the
        ! current layer loop.
        A_2_AD(k-1) = Pred%dA(k,j) * Factor_2_AD
        A_2_AD( k ) = A_2_AD( k ) + A_2_AD(k-1)

        ! Adjoint of A(). Here, since Pred_AD%A() is NOT a local adjoint
        ! variable, we can't use the same form as for A_2_AD() above.
        d_A_AD = ( Atm%Temperature(k) * s_AD(1) ) + &
                 ( Atm%Pressure(k)    * s_AD(2) ) + &
                 ( ( Pred%A(k,j)  + Pred%A(k-1,j)  ) * Factor_1_AD ) + &
                 ( ( APV%A_2(k,j) + APV%A_2(k-1,j) ) * Factor_2_AD )

        Add_Factor = Pred%dA(k,j) * Factor_1_AD
        Pred_AD%A(k-1,j) = Pred_AD%A(k-1,j) + Add_Factor - d_A_AD
        Pred_AD%A( k ,j) = Pred_AD%A( k ,j) + Add_Factor + d_A_AD + &
                           ( TWO * Pred%A(k,j) * A_2_AD(k) )
        A_2_AD(k) = ZERO

      END DO Layer_Loop

      ! Adjoint of level 0 A
      Pred_AD%A(0,j)   = Pred_AD%A(0,j) + ( TWO * Pred%A(0,j) * A_2_AD(0) )
      A_2_AD(0) = ZERO

    END DO Absorber_Loop
    
  END SUBROUTINE Integrated_Predictors_AD


!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PUBLIC MODULE ROUTINES ##                        ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       Compute_Predictors
!
! PURPOSE:
!       Subroutine to calculate the gas absorption model predictors.
!
! CALLING SEQUENCE:
!       CALL Compute_Predictors ( Atmosphere,   &  ! Input
!                                 GeometryInfo, &  ! Input
!                                 POrder,       &  ! Input                        
!                                 Alpha,        &  ! Input                        
!                                 Predictor,    &  ! Output                       
!                                 APVariables   )  ! Internal variable output     
!
! INPUT ARGUMENTS:
!       Atmosphere:     CRTM Atmosphere structure containing the atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_Atmosphere_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       GeometryInfo:   CRTM_GeometryInfo structure containing the 
!                       view geometry information.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CRTM_GeometryInfo_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Max_Order:      The maximum order of the polynomial function for each absorber 
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  1D array (n_Absorbers)
!                       ATTRIBUTES: INTENT(IN)
!
!       Alpha:          The alpha coefficients for absorber level calculations 
!                       UNITS:      depends on the units of the absorber
!                       TYPE:       INTEGER
!                       DIMENSION:  2D array (n_Alphas x n_Absorbers)
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor:      Predictor structure containing the integrated absorber
!                       and predictor profiles.
!                       UNITS:      N/A
!                       TYPE:       TYPE(Predictor_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       APVariables:    Structure containing internal variables required for
!                       subsequent tangent-linear or adjoint model calls.
!                       The contents of this structure are NOT accessible
!                       outside of the ODAS_Predictor module.
!                       UNITS:      N/A
!                       TYPE:       TYPE(APVariables_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! COMMENTS:
!       The predictors used in the gas absorption model are organised in
!       the following manner:
!
! ------------------------------------------------------------------------------
! | 1 | 2 | 3 | ... | 9 | 10 | 11 | 12 |....| 17 | 18 |....| 23 | 24 |....| 29 |
! ------------------------------------------------------------------------------
!
! \                              /\             /\             /\             /
!  \                            /  \           /  \           /  \           /
!   ----------------------------    -----------    -----------    -----------
!                 |                      |              |              |
!                 v                      v              v              v
!
!             Standard               Integrated     Integrated     Integrated
!            Predictors              predictors     predictors     predictors
!                                       for            for            for
!                                    Absorber 1     Absorber 2     Absorber 3
!                                   (water vapor)   (dry gases)     (ozone)
!
!--------------------------------------------------------------------------------

  SUBROUTINE Compute_Predictors( Atmosphere,   &  ! Input
                                 GeometryInfo, &  ! Input 
                                 Max_Order,    &  ! Input
                                 Alpha,        &  ! Input                       
                                 Predictor,    &  ! Output                       
                                 APV           )  ! Internal variable output     
    ! Arguments
    TYPE(CRTM_Atmosphere_type),   INTENT(IN)     :: Atmosphere
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: Max_Order(:)
    REAL(fp),                     INTENT(IN)     :: Alpha(:,:)
    TYPE(Predictor_type),         INTENT(IN OUT) :: Predictor
    TYPE(APVariables_type),       INTENT(OUT)    :: APV
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_Predictors'
    INTEGER :: i,j,k,n_Layers
    REAL(fp) :: Secant_Sensor_Zenith

    ! Save the angle information
    CALL CRTM_GeometryInfo_GetValue( GeometryInfo, &
                                     Secant_Trans_Zenith = Secant_Sensor_Zenith )
    Predictor%Secant_Sensor_Zenith = Secant_Sensor_Zenith

    ! Compute the nadir integrated absorber profiles
    CALL Compute_IntAbsorber( Atmosphere, &  ! Input
                              Predictor   )  ! Output

    ! Compute the predictors
    !
    ! Standard predictors
    CALL Standard_Predictors( Atmosphere, &
                              Predictor   )
    ! Integrated predictors
    CALL Integrated_Predictors( Atmosphere, &
                                Predictor,  &     
                                APV         )     
        ! ----------------------------------------------------------
        ! Calculate absorber space level associated with the average
        ! absorber amount
        ! 
        ! Absorber level, k, to amount 
        ! 
        !     A(k) = C1.exp(Alpha * k) + C2
        ! 
        ! Absorber amount to level 
        ! 
        !           1      A - C2
        !     k = ----- LN ------
        !         Alpha      C1
        ! 
        !     AP(k, i) = A(k)**(i), i = 1, Max_Order(j)
        ! 
        !   Alpha : absorber amount-level coordinate constant
        !   C1,C2 : scaling factors for level in the range of 0 to 1
        ! ----------------------------------------------------------

    n_Layers = Atmosphere%n_Layers
    DO j = 1, Predictor%n_Absorbers                                                    

      IF( Max_Order(j) < 0 )CYCLE

      DO k = 1, n_Layers                                                       

        APV%A_Level(k,j) = LOG((Predictor%aveA(k,j) - Alpha(3, j)) / Alpha(2, j)) / &    
        !                 ------------------------------------------------------------    
                                           Alpha(1, j)                                    
      END DO

      Predictor%Ap(1:n_Layers, 1, j) = APV%A_Level(1:n_Layers,j)                                                    
      DO i = 2, Max_Order(j)                                               
        DO k = 1, n_Layers     
          Predictor%Ap(k, i, j) = Predictor%Ap(k, i-1, j) * APV%A_Level(k,j)                        
        END DO                                                                                       
      END DO                                                                           
    END DO                                                                             


  END SUBROUTINE Compute_Predictors


!--------------------------------------------------------------------------------
!
! NAME:
!       Compute_Predictors_TL
!
! PURPOSE:
!       Subroutine to calculate the gas absorption model tangent-linear
!       predictors.
!
! CALLING SEQUENCE:
!       CALL Compute_Predictors_TL ( Atmosphere,    &  ! FWD Input
!                                    Predictor,     &  ! FWD Input                   
!                                    Atmosphere_TL, &  ! TL Input                    
!                                    GeometryInfo,  &  ! Input                       
!                                    POrder,        &  ! Input                        
!                                    Alpha,         &  ! Input                        
!                                    Predictor_TL,  &  ! TL Output                   
!                                    APVariables    )  ! Internal variable input     
!
! INPUT ARGUMENTS:
!       Atmosphere:        CRTM Atmosphere structure containing the atmospheric
!                          state data.
!                          UNITS:      N/A
!                          TYPE:       TYPE(CRTM_Atmosphere_type)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Predictor:         Predictor structure containing the integrated absorber
!                          and predictor profiles.
!                          UNITS:      N/A
!                          TYPE:       TYPE(Predictor_type)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Atmosphere_TL:     CRTM Atmosphere structure containing the tangent-linear
!                          atmospheric state data, i.e. the perturbations.
!                          UNITS:      N/A
!                          TYPE:       TYPE(CRTM_Atmosphere_type)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       GeometryInfo:      CRTM_GeometryInfo structure containing the
!                          view geometry information.
!                          UNITS:      N/A
!                          TYPE:       TYPE(CRTM_GeometryInfo_type)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Max_Order:         The maximum order of the polynomial function for each absorber 
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  1D array (n_Absorbers)
!                          ATTRIBUTES: INTENT(IN)
!
!       Alpha:             The alpha coefficients for absorber level calculations 
!                          UNITS:      depends on the units of the absorber
!                          TYPE:       INTEGER
!                          DIMENSION:  2D array (n_Alphas x n_Absorbers)
!                          ATTRIBUTES: INTENT(IN)
!
!       APVariables:       Structure containing internal variables required for
!                          subsequent tangent-linear or adjoint model calls.
!                          The contents of this structure are NOT accessible
!                          outside of the ODAS_Predictor module.
!                          UNITS:      N/A
!                          TYPE:       APVariables_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT)
!
! OUTPUT ARGUMENTS:
!       Predictor_TL:      Predictor structure containing the tangent-linear
!                          integrated absorber and predictor profiles.
!                          UNITS:      N/A
!                          TYPE:       TYPE(Predictor_type)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       The predictors used in the gas absorption model are organised in
!       the following manner:
!
! ------------------------------------------------------------------------------
! | 1 | 2 | 3 | ... | 9 | 10 | 11 | 12 |....| 17 | 18 |....| 23 | 24 |....| 29 |
! ------------------------------------------------------------------------------
!
! \                              /\             /\             /\             /
!  \                            /  \           /  \           /  \           /
!   ----------------------------    -----------    -----------    -----------
!                 |                      |              |              |
!                 v                      v              v              v
!
!             Standard               Integrated     Integrated     Integrated
!            Predictors              predictors     predictors     predictors
!                                       for            for            for
!                                    Absorber 1     Absorber 2     Absorber 3
!                                   (water vapor)   (dry gases)     (ozone)
!
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Compute_Predictors_TL( Atmosphere,    &  ! FWD Input
                                    Predictor,     &  ! FWD Input                   
                                    Atmosphere_TL, &  ! TL Input                    
                                    GeometryInfo,  &  ! Input 
                                    Max_Order,     &  ! Input
                                    Alpha,         &  ! Input                       
                                    Predictor_TL,  &  ! TL Output                   
                                    APV            )  ! Internal variable input     
    ! Arguments
    TYPE(CRTM_Atmosphere_type),   INTENT(IN)     :: Atmosphere
    TYPE(Predictor_type),         INTENT(IN)     :: Predictor
    TYPE(CRTM_Atmosphere_type),   INTENT(IN)     :: Atmosphere_TL
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: Max_Order(:)
    REAL(fp),                     INTENT(IN)     :: Alpha(:,:)
    TYPE(Predictor_type),         INTENT(IN OUT) :: Predictor_TL
    TYPE(APVariables_type),       INTENT(IN)     :: APV
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_Predictors_TL'
    REAL(fp):: A_Level_TL(Atmosphere%n_Layers)
    INTEGER :: i,j,k,n_Layers
    REAL(fp) :: Secant_Sensor_Zenith

    ! Save the angle information
    CALL CRTM_GeometryInfo_GetValue( GeometryInfo, &
                                     Secant_Trans_Zenith = Secant_Sensor_Zenith )
    Predictor_TL%Secant_Sensor_Zenith = Secant_Sensor_Zenith

    ! Compute the tangent-linear nadir integrated absorber profiles
    CALL Compute_IntAbsorber_TL( Atmosphere   , &  ! Input
                                 Atmosphere_TL, &  ! Input     
                                 Predictor_TL   )  ! Outpu     t

    ! Compute the predictors
    !
    ! Standard predictors
    CALL Standard_Predictors_TL( Atmosphere   , &  ! Input
                                 Atmosphere_TL, &  ! Input      
                                 Predictor_TL   )  ! Output     
    ! Integrated predictors
    CALL Integrated_Predictors_TL( Atmosphere   , &  ! Input
                                   Predictor    , &  ! Input                       
                                   Atmosphere_TL, &  ! Input                       
                                   Predictor_TL , &  ! Output                      
                                   APV            )  ! Internal variable input     


    n_Layers = Atmosphere%n_Layers
    DO j = 1, Predictor%n_Absorbers 

      IF( Max_Order(j) < 0 )CYCLE
                                                       
      DO k = 1, n_Layers                                                       

        A_Level_TL(k) =                      Predictor_TL%aveA(k,j)  /&   
        !            ------------------------------------------------------------------    
                       ( Alpha(1, j) * (Predictor%aveA(k,j) - Alpha(3, j) ) )                                   
      END DO
      
      Predictor_TL%Ap(1:n_layers, 1, j) = A_Level_TL(1:n_layers)                                                    
      DO i = 2, Max_Order(j)                                               
        DO k = 1, n_Layers                                                       
          Predictor_TL%Ap(k, i, j) = Predictor_TL%Ap(k, i-1, j)*APV%A_Level(k,j) &
                                   + Predictor%Ap(k, i-1, j)*A_Level_TL(k)                     
        END DO                                                                         
      END DO                                                                           
    END DO                                                                             

  END SUBROUTINE Compute_Predictors_TL


!--------------------------------------------------------------------------------
!
! NAME:
!       Compute_Predictors_AD
!
! PURPOSE:
!       Subroutine to calculate the adjoint gas absorption model predictors.
!
! CALLING SEQUENCE:
!       CALL Compute_Predictors_AD ( Atmosphere,    &  ! FWD Input
!                                    Predictor,     &  ! FWD Input                   
!                                    Predictor_AD,  &  ! AD Input                    
!                                    GeometryInfo,  &  ! Input                       
!                                    POrder,        &  ! Input                        
!                                    Alpha,         &  ! Input                        
!                                    Atmosphere_AD, &  ! AD Output                   
!                                    APVariables    )  ! Internal variable input     
! INPUT ARGUMENTS:
!       Atmosphere:        CRTM Atmosphere structure containing the atmospheric
!                          state data.
!                          UNITS:      N/A
!                          TYPE:       TYPE(CRTM_Atmosphere_type)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Predictor:         Predictor structure containing the integrated absorber
!                          and predictor profiles.
!                          UNITS:      N/A
!                          TYPE:       TYPE(Predictor_type)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Predictor_AD:      Predictor structure containing the adjoint
!                          integrated absorber and predictor profiles.
!                          **NOTE: This structure is zeroed upon output
!                          UNITS:      N/A
!                          TYPE:       TYPE(Predictor_type)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
!       GeometryInfo:      CRTM_GeometryInfo structure containing the
!                          view geometry information.
!                          UNITS:      N/A
!                          TYPE:       TYPE(CRTM_GeometryInfo_type)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Max_Order:         The maximum order of the polynomial function for each absorber 
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  1D array (n_Absorbers)
!                          ATTRIBUTES: INTENT(IN)
!
!       Alpha:             The alpha coefficients for absorber level calculations 
!                          UNITS:      depends on the units of the absorber
!                          TYPE:       INTEGER
!                          DIMENSION:  2D array (n_Alphas x n_Absorbers)
!                          ATTRIBUTES: INTENT(IN)
!
!       APVariables:       Structure containing internal variables required for
!                          subsequent tangent-linear or adjoint model calls.
!                          The contents of this structure are NOT accessible
!                          outside of the ODAS_Predictor module.
!                          UNITS:      N/A
!                          TYPE:       APVariables_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT)
!
! OUTPUT ARGUMENTS:
!       Atmosphere_AD:     CRTM Atmosphere structure containing the adjoint
!                          atmospheric state data, i.e. the Jacobians
!                          UNITS:      N/A
!                          TYPE:       TYPE(CRTM_Atmosphere_type)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       The predictors used in the gas absorption model are organised in
!       the following manner:
!
! ------------------------------------------------------------------------------
! | 1 | 2 | 3 | ... | 9 | 10 | 11 | 12 |....| 17 | 18 |....| 23 | 24 |....| 29 |
! ------------------------------------------------------------------------------
!
! \                              /\             /\             /\             /
!  \                            /  \           /  \           /  \           /
!   ----------------------------    -----------    -----------    -----------
!                 |                      |              |              |
!                 v                      v              v              v
!
!             Standard               Integrated     Integrated     Integrated
!            Predictors              predictors     predictors     predictors
!                                       for            for            for
!                                    water vapor    dry gases        ozone
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Compute_Predictors_AD ( Atmosphere,    &  ! FWD Input
                                     Predictor,     &  ! FWD Input                   
                                     Predictor_AD,  &  ! AD Input                    
                                     GeometryInfo,  &  ! Input                       
                                     Max_Order,     &  ! Input
                                     Alpha,         &  ! Input                       
                                     Atmosphere_AD, &  ! AD Output                   
                                     APV            )  ! Internal variable input     
    ! Arguments
    TYPE(CRTM_Atmosphere_type)  , INTENT(IN)     :: Atmosphere
    TYPE(Predictor_type)        , INTENT(IN)     :: Predictor     
    TYPE(Predictor_type)        , INTENT(IN OUT) :: Predictor_AD  
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: Max_Order(:)
    REAL(fp),                     INTENT(IN)     :: Alpha(:,:)
    TYPE(CRTM_Atmosphere_type)  , INTENT(IN OUT) :: Atmosphere_AD
    TYPE(APVariables_type)      , INTENT(IN)     :: APV
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_Predictors_AD'
    REAL(fp):: A_Level_AD(Atmosphere%n_Layers)
    INTEGER :: i, j, k, n_Layers
    REAL(fp) :: Secant_Sensor_Zenith

    ! Save the angle information
    CALL CRTM_GeometryInfo_GetValue( GeometryInfo, &
                                     Secant_Trans_Zenith = Secant_Sensor_Zenith )
    Predictor_AD%Secant_Sensor_Zenith = Secant_Sensor_Zenith

    ! Compute aveA AD
    A_Level_AD = ZERO

    n_Layers = Atmosphere%n_Layers
    DO j = 1, Predictor%n_Absorbers                                                    

      IF( Max_Order(j) < 0 )CYCLE

      DO i = Max_Order(j), 2, -1 
 
        DO k = n_Layers, 1, -1                                                      

           Predictor_AD%Ap(k, i-1, j) = Predictor_AD%Ap(k, i-1, j) &
                                      + Predictor_AD%Ap(k, i, j) *APV%A_Level(k,j)
           A_LEVEL_AD(k) = A_LEVEL_AD(k) + Predictor%Ap(k, i-1, j)*Predictor_AD%Ap(k, i, j)
           Predictor_AD%Ap(k, i, j) = ZERO
        END DO
        
      END DO
      
      A_Level_AD(1:n_Layers) = A_Level_AD(1:n_Layers) + Predictor_AD%Ap(1:n_layers, 1, j)  
      Predictor_AD%Ap(1:n_Layers, 1, j) = ZERO  

      DO k = n_Layers, 1, -1  
        Predictor_AD%aveA(k,j) = Predictor_AD%aveA(k,j) + &                                
                                       A_Level_AD(k) / &                                   
        !          --------------------------------------------------------------          
                   ( Alpha(1, j) * (Predictor%aveA(k,j) - Alpha(3, j) ))                   
        A_Level_AD(k) = ZERO                                                               

      END DO  
                                                                                   
    END DO

    ! Calculate the predictor adjoints
    !
    ! Integrated predictors
    CALL Integrated_Predictors_AD( Atmosphere   , &  ! Input
                                   Predictor    , &  ! Input                       
                                   Predictor_AD , &  ! In/Output                   
                                   Atmosphere_AD, &  ! Output                      
                                   APV            )  ! Internal variable input     
    ! Standard predictors
    CALL Standard_Predictors_AD( Atmosphere   , &  ! Input
                                 Predictor_AD , &  ! Input      
                                 Atmosphere_AD  )  ! Output     

    ! Compute the adjoint nadir integrated absorber profiles
    CALL Compute_IntAbsorber_AD( Atmosphere   , &  ! Input
                                 Predictor_AD , &  ! Output     
                                 Atmosphere_AD  )  ! Input      

    ! Zero the adjoint predictor structure
    CALL Zero_Predictor( Predictor_AD )
    
  END SUBROUTINE Compute_Predictors_AD

END MODULE ODAS_Predictor
