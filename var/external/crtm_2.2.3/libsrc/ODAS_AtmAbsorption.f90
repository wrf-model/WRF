!
! ODAS_AtmAbsorption
!
! Module containing routines to compute the optical depth profile
! due to gaseous absorption for the Optical Depth Absorber Space (ODAS)
! model.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 13-May-2004
!                       paul.vandelst@noaa.gov
!
!       Modifed by:     Yong Han, 25-June-2008
!                       yong.han@noaa.gov
!

MODULE ODAS_AtmAbsorption

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,                ONLY: fp
  USE Message_Handler,           ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE CRTM_Parameters,           ONLY: ZERO,         &
                                       MAX_N_LAYERS, &
                                       LIMIT_EXP,    &
                                       LIMIT_LOG
  USE CRTM_Atmosphere_Define,    ONLY: CRTM_Atmosphere_type
  USE CRTM_GeometryInfo_Define,  ONLY: CRTM_GeometryInfo_type
  USE CRTM_AtmOptics_Define,     ONLY: CRTM_AtmOptics_type
  USE ODAS_Predictor_Define,     ONLY: ODAS_Predictor_type
  USE ODAS_Predictor,            ONLY: MAX_N_ABSORBERS,       &
                                       MAX_N_ORDERS,          &
                                       MAX_N_PREDICTORS_USED, &
                                       MAX_N_ORDERS
  USE ODAS_TauCoeff,             ONLY: TC, &
                                       ODAS_TauCoeff_type

  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Science routines in this modules
  PUBLIC :: ODAS_Compute_AtmAbsorption
  PUBLIC :: ODAS_Compute_AtmAbsorption_TL
  PUBLIC :: ODAS_Compute_AtmAbsorption_AD
  ! Internal variable structure
  PUBLIC :: iVar_type

  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: ODAS_AtmAbsorption.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'


  ! ------------------------------------------
  ! Structure definition to hold forward model
  ! variables across FWD, TL, and AD calls
  ! ------------------------------------------
  TYPE :: iVar_type
    PRIVATE
    REAL(fp), DIMENSION(MAX_N_LAYERS, &
                        0:MAX_N_PREDICTORS_USED, &
                        MAX_N_ABSORBERS)              :: b
    REAL(fp), DIMENSION(MAX_N_LAYERS,MAX_N_ABSORBERS) :: LN_Chi
    REAL(fp), DIMENSION(MAX_N_LAYERS,MAX_N_ABSORBERS) :: Chi
  END TYPE iVar_type

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
!       ODAS_Compute_AtmAbsorption
!
! PURPOSE:
!       Subroutine to calculate the layer optical depths due to gaseous
!       absorption for a given sensor and channel and atmospheric profile
!       using the Optical Depth in Absorber Space (ODAS) algorithm).
!
! CALLING SEQUENCE:
!       CALL ODAS_Compute_AtmAbsorption( TC          , &
!                                        ChannelIndex, &
!                                        Predictor   , &
!                                        AtmOptics   , &
!                                        iVar   )
!
! INPUTS:
!       TC:              Structure containing ODAS model coefficient data
!                        for a sensor.
!                        UNITS:      N/A
!                        TYPE:       ODAS_TauCoeff_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
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
!       Predictor:       Structure containing the ODAS model integrated
!                        absorber and predictor profile data.
!                        UNITS:      N/A
!                        TYPE:       ODAS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       AtmOptics:       Structure containing computed optical depth
!                        profile data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_AtmOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       iVar:            Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of this module.
!                        UNITS:      N/A
!                        TYPE:       iVar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output structure arguments are IN OUT to prevent
!       reinitialisation upon entry.
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE ODAS_Compute_AtmAbsorption( &
    TC          , &  ! Input
    ChannelIndex, &  ! Input
    Predictor   , &  ! Input
    AtmOptics   , &  ! Output
    iVar          )  ! Internal variable output
    ! Arguments
    TYPE(ODAS_TauCoeff_type) , INTENT(IN)     :: TC
    INTEGER                  , INTENT(IN)     :: ChannelIndex
    TYPE(ODAS_Predictor_type), INTENT(IN)     :: Predictor
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: AtmOptics
    TYPE(iVar_type)          , INTENT(IN OUT) :: iVar
    ! Local variables
    INTEGER  :: l        ! Channel index
    INTEGER  :: k        ! Layer index
    INTEGER  :: j        ! Absorber index
    INTEGER  :: i, ip    ! Predictor index
    INTEGER  :: np       ! # of predictors
    INTEGER  :: ps       ! starting position of the coefficient subset for given j and l
    INTEGER  :: n_Orders ! order of the polynomial function
    INTEGER  :: ic_0     ! the index of the first coefficient in the C coeff subset for deriving the B coeff.
    INTEGER  :: ic       ! the index of the coefficients
    REAL(fp) :: c        ! a coefficient
    INTEGER  :: n_Layers


    ! Set up
    ! ...Assign the indices to a short name
    l = ChannelIndex
    n_Layers = Predictor%n_Layers
    ! ...Initilise the optical depth
    AtmOptics%Optical_Depth = ZERO  !! *** DOES THIS MAKE ROUTINE ORDER DEPENDENT? ***


    ! Loop over each absorber for optical depth calculation
    Absorber_Loop: DO j = 1, Predictor%n_Absorbers


      ! Check if there is any absorption for this
      ! absorber/channel combination.
      np = TC%Pre_Index(0,j,l)
      IF ( np < 0 ) CYCLE Absorber_Loop


      ! Compute the coefficients for use with the atmospheric predictors
      !
      ! For every atmospheric predictor, Pred(i), the coefficient
      ! associated with it, b(i), at a particular absorber amount
      ! level, k, is given by an N'th order polynomial,
      !
      !           __ N
      !          \           np
      !   b(i) =  > c(np,i).k
      !          /__
      !             np=0
      !
      ps = TC%Pos_Index(j,l)  ! starting position of the coefficient subset for given j and l
      n_Orders = TC%Order(j,l)
      ! ...Compute b(0)
      ic_0 = ps
      iVar%b(1:n_Layers,0,j) = TC%C(ic_0)
      DO ic = 1, n_Orders
        c = TC%C(ic_0 + ic)
        DO k = 1, n_Layers
          iVar%b(k,0,j) = iVar%b(k,0,j) + (c * Predictor%Ap(k, ic, j))
        END DO
      END DO


      ! compute b(i) coefficients (i > 0)
      ! Compute the logarithm of the absorption coefficient
      !
      ! The logarithm of the absorption coefficient, LN(chi), is
      ! determined from the regression equation,
      !
      !                     __Iuse
      !                    \
      !   LN(chi) = b(0) +  > b(i).X(i)
      !                    /__
      !                       i=1
      !
      ! ...The b(0) contribution
      iVar%LN_Chi(1:n_Layers, j) = iVar%b(1:n_Layers,0,j)
      DO i = 1, np
        ! ...b(i) term, i > 0
        ic_0 = ps + i*(n_orders+1)
        iVar%b(1:n_Layers,i,j) = TC%C(ic_0)
        DO ic = 1, n_Orders
          c = TC%C(ic_0 + ic)
          DO k = 1, n_Layers
            iVar%b(k,i,j) = iVar%b(k,i,j) + (c * Predictor%Ap(k, ic, j))
          END DO
        END DO
        ! ...b(i) term contribution
        ip = TC%Pre_Index(i,j,l)
        DO k = 1, n_Layers
          iVar%LN_Chi(k,j) = iVar%LN_Chi(k,j) + (iVar%b(k, i, j) * Predictor%X(k,ip))
        END DO
      END DO


      ! Compute the optical depth profile
      DO k = 1, n_Layers
        ! ...Compute the absorption coefficient
        IF( iVar%LN_Chi(k,j) > LIMIT_EXP ) THEN
          iVar%Chi(k,j) = LIMIT_LOG
        ELSE IF( iVar%LN_Chi(k,j) < -LIMIT_EXP ) THEN
          iVar%Chi(k,j) = ZERO
        ELSE
          iVar%Chi(k,j) = EXP(iVar%LN_Chi(k,j))
        ENDIF

        AtmOptics%Optical_Depth(k) = AtmOptics%Optical_Depth(k) + (iVar%Chi(k,j) * Predictor%dA(k,j))
      END DO

    END DO Absorber_Loop


    ! Scale the optical depth to nadir
    AtmOptics%Optical_Depth = AtmOptics%Optical_Depth / Predictor%Secant_Sensor_Zenith

  END SUBROUTINE ODAS_Compute_AtmAbsorption


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ODAS_Compute_AtmAbsorption_TL
!
! PURPOSE:
!       Subroutine to calculate the tangent-linear layer optical depths due
!       to gaseous absorption for a given sensor and channel and atmospheric
!       profile using the Optical Depth in Absorber Space (ODAS) algorithm).
!
! CALLING SEQUENCE:
!       CALL ODAS_Compute_AtmAbsorption_TL( TC          , &
!                                           ChannelIndex, &
!                                           Predictor   , &
!                                           Predictor_TL, &
!                                           AtmOptics_TL, &
!                                           iVar   )
!
! INPUTS:
!       TC:              Structure containing ODAS model coefficient data
!                        for a sensor.
!                        UNITS:      N/A
!                        TYPE:       ODAS_TauCoeff_type
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
!       Predictor:       Structure containing the ODAS model integrated
!                        absorber and predictor profile data.
!                        UNITS:      N/A
!                        TYPE:       ODAS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor_TL:    Structure containing the ODAS model tangent-linear
!                        integrated absorber and predictor profile data.
!                        UNITS:      N/A
!                        TYPE:       ODAS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       iVar:            Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of this module.
!                        UNITS:      N/A
!                        TYPE:       iVar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!        AtmOptics_TL:   Structure containing the computed tangent-linear
!                        optical depth profile data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_AtmOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       Note the INTENT on the output structure arguments are IN OUT to prevent
!       reinitialisation upon entry.
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE ODAS_Compute_AtmAbsorption_TL( &
    TC          , &  ! Input
    ChannelIndex, &  ! Input
    Predictor   , &  ! FWD Input
    Predictor_TL, &  ! TL  Input
    AtmOptics_TL, &  ! TL  Output
    iVar          )  ! Internal variable input
    ! Arguments
    TYPE(ODAS_TauCoeff_type) , INTENT(IN)     :: TC
    INTEGER                  , INTENT(IN)     :: ChannelIndex
    TYPE(ODAS_Predictor_type), INTENT(IN)     :: Predictor
    TYPE(ODAS_Predictor_type), INTENT(IN)     :: Predictor_TL
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: AtmOptics_TL
    TYPE(iVar_type)          , INTENT(IN)     :: iVar
    ! Local variables
    INTEGER  :: l        ! Channel index
    INTEGER  :: k        ! Layer index
    INTEGER  :: j        ! Absorber index
    INTEGER  :: i, ip    ! Predictor index
    INTEGER  :: np       ! # of predictors
    INTEGER  :: ps       ! starting position of the coefficient subset for given j and l
    INTEGER  :: n_Orders ! order of the polynomial function
    INTEGER  :: ic_0     ! the index of the first coefficient in the b coeff subset for deriving the b coeff.
    INTEGER  :: ic       ! the index of the coefficients
    REAL(fp) :: c        ! a coefficient
    REAL(fp) :: b_TL(Predictor%n_layers)
    REAL(fp) :: LN_Chi_TL(Predictor%n_layers)
    REAL(fp) :: Chi_TL
    INTEGER  :: n_Layers

    ! Set up
    ! ...Assign the indices to a short name
    l = ChannelIndex
    n_Layers = Predictor%n_Layers
    ! ...Initilise the tangent-linear optical depth
    AtmOptics_TL%Optical_Depth = ZERO


    ! Loop over each absorber for optical depth calculation
    Absorber_Loop: DO j = 1, Predictor%n_Absorbers


      ! Check if there is any absorption for this
      ! absorber/channel combination.
      np = TC%Pre_Index(0,j,l)
      IF ( np < 0 ) CYCLE Absorber_Loop


      ! Compute the coefficients for use with the atmospheric predictors
      !
      ! For every atmospheric predictor, Pred(i), the coefficient
      ! associated with it, b(i), at a particular absorber amount
      ! level, k, is given by an N'th order polynomial,
      !
      !           __ N
      !          \          np
      !   b(i) =  > c(np,i).k
      !          /__
      !             np=0
      !
      ps = TC%Pos_Index(j,l)  ! starting position of the coefficient subset f  or given j and l
      n_orders = TC%Order(j,l)
      ! ...Compute b_TL(0)
      ic_0 = ps
      b_TL(1:n_Layers) = ZERO
      DO ic = 1, n_Orders
        c = TC%C(ic_0 + ic)
        DO k = 1, n_Layers
          b_TL(k) = b_TL(k) + (c * Predictor_TL%Ap(k, ic, j))
        END DO
      END DO


      ! Compute the logarithm of the absorption coefficient
      !
      ! The logarithm of the absorption coefficient, LN(chi), is
      ! determined from the regression equation,
      !
      !                     __Iuse
      !                    \
      !   LN(chi) = b(0) +  > b(i).X(i)
      !                    /__
      !                       i=1
      !
      ! ...b_TL(0) term contribution
      LN_Chi_TL(1:n_Layers) = b_TL(1:n_Layers)
      DO i = 1, np
        ! ...b_TL(i) term, i > 0
        ic_0 = ps + i*(n_orders+1)
        b_TL(1:n_Layers) = ZERO
        DO ic = 1, n_Orders
          c = TC%C(ic_0 + ic)
          DO k = 1, n_Layers
            b_TL(k) = b_TL(k) + (c * Predictor_TL%Ap(k, ic, j))
          END DO
        END DO
        ! b_TL(i) term contribution
        ip = TC%Pre_Index(i,j,l)
        DO k = 1, n_Layers
          LN_Chi_TL(k) = LN_Chi_TL(k) + (b_TL(k) * Predictor%X(k,ip)) &
                                      + (iVar%b(k,i,j) * Predictor_TL%X(k,ip))
        END DO
      END DO


      ! Compute the tangent-linear optical depth profile
      DO k = 1, n_Layers
        ! ...Compute the tangent-linear absorption coefficient
        IF( iVar%LN_Chi(k,j) > LIMIT_EXP ) THEN
          Chi_TL = ZERO
        ELSE IF( iVar%LN_Chi(k,j) < -LIMIT_EXP ) THEN
          Chi_TL = ZERO
        ELSE
          Chi_TL = iVar%Chi(k,j) * LN_Chi_TL(k)
        ENDIF

        AtmOptics_TL%Optical_Depth(k) = AtmOptics_TL%Optical_Depth(k) &
                                          + (Chi_TL * Predictor%dA(k,j)) &
                                          + (iVar%Chi(k,j) * Predictor_TL%dA(k,j))
      END DO

    END DO Absorber_Loop


    ! Scale the tangent-linear optical depth to nadir
    AtmOptics_TL%Optical_Depth = AtmOptics_TL%Optical_Depth / &
                                 Predictor%Secant_Sensor_Zenith

  END SUBROUTINE ODAS_Compute_AtmAbsorption_TL


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ODAS_Compute_AtmAbsorption_AD
!
! PURPOSE:
!       Subroutine to calculate the adjoint of the layer optical depths due
!       to gaseous absorption for a given sensor and channel and atmospheric
!       profile using the Optical Depth in Absorber Space (ODAS) algorithm).
!
! CALLING SEQUENCE:
!       CALL ODAS_Compute_AtmAbsorption_AD( TC          , &
!                                           ChannelIndex, &
!                                           Predictor   , &
!                                           AtmOptics_AD, &
!                                           Predictor_AD, &
!                                           iVar   )
!
! INPUTS:
!       TC:              Structure containing ODAS model coefficient data
!                        for a sensor.
!                        UNITS:      N/A
!                        TYPE:       ODAS_TauCoeff_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:    Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data for a particular sensor's
!                        channel.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Structure containing the ODAS model integrated
!                        absorber and predictor profile data.
!                        UNITS:      N/A
!                        TYPE:       ODAS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       AtmOptics_AD:    Structure containing the adjoint optical depth
!                        profile data.
!                        *** NOTE: Optical depth component set to zero on output. ***
!                        UNITS:      N/A
!                        TYPE:       CRTM_AtmOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       iVar:            Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of this module.
!                        UNITS:      N/A
!                        TYPE:       iVar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Predictor_AD:    Structure containing the ODAS model adjoint
!                        integrated absorber and predictor profile data.
!                        *** NOTE: Must contain a value upon input. ***
!                        UNITS:      N/A
!                        TYPE:       ODAS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! SIDE EFFECTS:
!       Components of the AtmOptics_AD structure argument are modified
!       in this function.
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE ODAS_Compute_AtmAbsorption_AD( &
    TC          , &  ! Input
    ChannelIndex, &  ! Input
    Predictor   , &  ! FWD Input
    AtmOptics_AD, &  ! AD  Input
    Predictor_AD, &  ! AD  Output
    iVar          )  ! Internal variable input
    ! Arguments
    TYPE(ODAS_TauCoeff_type) , INTENT(IN)     :: TC
    INTEGER                  , INTENT(IN)     :: ChannelIndex
    TYPE(ODAS_Predictor_type), INTENT(IN)     :: Predictor
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: AtmOptics_AD
    TYPE(ODAS_Predictor_type), INTENT(IN OUT) :: Predictor_AD
    TYPE(iVar_type)          , INTENT(IN)     :: iVar
    ! Local variables
    INTEGER  :: l        ! Channel index
    INTEGER  :: k        ! Layer index
    INTEGER  :: j        ! Absorber index
    INTEGER  :: i, ip    ! Predictor index
    INTEGER  :: np       ! # of predictors
    INTEGER  :: ps       ! starting position of the coefficient subset for given j and l
    INTEGER  :: n_Orders ! order of the polynomial function
    INTEGER  :: ic_0     ! the index of the first coefficient in the b coeff subset for deriving the b coeff.
    INTEGER  :: ic       ! the index of the coefficients
    REAL(fp) :: c        ! a coefficient
    REAL(fp) :: b_AD(Predictor%n_layers)
    REAL(fp) :: LN_Chi_AD(Predictor%n_layers)
    REAL(fp) :: Chi_AD
    INTEGER  :: n_Layers


    ! Set up
    ! ...Assign the indices to a short name
    l = ChannelIndex
    n_Layers = Predictor%n_Layers
    ! ...Initialise local adjoint variables
    b_AD      = ZERO
    LN_Chi_AD = ZERO
    Chi_AD    = ZERO


    ! Compute adjoint nadir optical depth profile
    AtmOptics_AD%Optical_Depth = AtmOptics_AD%Optical_Depth / &
                                  Predictor%Secant_Sensor_Zenith


    ! Loop over each absorber for optical depth calculation
    Absorber_loop: DO j = 1, Predictor%n_Absorbers


      ! Check if there is any absorption for this
      ! absorber/channel combination.
      np = TC%Pre_Index(0,j,l)
      IF ( np < 0 ) CYCLE Absorber_Loop

      ! Starting position of the coefficient subset for given absorber (j) and channels(l)
      ps = TC%Pos_Index(j,l)
      n_orders = TC%Order(j,l)


      ! Compute the adjoint of the optical depth profile
      DO k = n_Layers, 1, -1
        Predictor_AD%dA(k,j) = Predictor_AD%dA(k,j) + &
                               (iVar%Chi(k,j) * AtmOptics_AD%Optical_Depth(k))
        Chi_AD = Chi_AD + (Predictor%dA(k,j) * AtmOptics_AD%Optical_Depth(k))


        ! ...Compute the adjoint of the absorption coefficient
        IF( iVar%LN_Chi(k,j) > LIMIT_EXP ) THEN
          Chi_AD = ZERO
        ELSE IF( iVar%LN_Chi(k,j) < -LIMIT_EXP ) THEN
          Chi_AD = ZERO
        ELSE
          LN_Chi_AD(k) = LN_Chi_AD(k) + iVar%Chi(k,j) * Chi_AD
          Chi_AD       = ZERO
        ENDIF
      END DO


      ! Compute the adjoint of the logarithm of the absorption coefficient
      DO i = np, 1, -1
        ! ...b(i) term contribution
        ip = TC%Pre_Index(i,j,l)
        DO k = n_Layers, 1, -1
          b_AD(k) = b_AD(k) + (LN_Chi_AD(k) * Predictor%X(k,ip))
          Predictor_AD%X(k,ip) = Predictor_AD%X(k,ip) + (iVar%b(k,i,j) * LN_Chi_AD(k))
        END DO
        ! ...b(i) term, i > 0
        ic_0 = ps + i*(n_orders+1)
        DO ic = n_Orders, 1, -1
          c = TC%C(ic_0 + ic)
          DO k = n_Layers, 1, -1
            Predictor_AD%Ap(k, ic, j) = Predictor_AD%Ap(k, ic, j) + (c * b_AD(k))
          END DO
        END DO
        b_AD(1:n_Layers) = ZERO
      END DO
      b_AD(1:n_Layers) = b_AD(1:n_Layers) + LN_Chi_AD(1:n_Layers)
      LN_Chi_AD(1:n_Layers) = ZERO


      ! Compute the adjoint of the b(i) coefficients
      ic_0 = ps
      DO ic = n_Orders, 1, -1
        c = TC%C(ic_0 + ic)
        DO k = n_Layers, 1, -1
          Predictor_AD%Ap(k, ic, j) = Predictor_AD%Ap(k, ic, j) + (c * b_AD(k))
        END DO
      END DO
      b_AD(1:n_Layers) = ZERO

    END DO Absorber_Loop


    ! No more impact of optical depth on derivatives
    AtmOptics_AD%Optical_Depth = ZERO

  END SUBROUTINE ODAS_Compute_AtmAbsorption_AD

END MODULE ODAS_AtmAbsorption
