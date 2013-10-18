!
! ODPS_AtmAbsorption
!
! Module containing routines to compute the optical depth profile
! due to gaseous absorption using the Optical Depth Pressure Space
! (ODPS) algorithm
!
!
! CREATION HISTORY:
!       Written by:     Yong Han & Yong Chen, JCSDA, NOAA/NESDIS 20-Jun-2008
!            TL,AD:     Tong Zhu, CIRA/CSU@NOAA/NESDIS 06-Jan-2009
!

MODULE ODPS_AtmAbsorption

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,               ONLY: fp
  USE CRTM_Parameters,          ONLY: ZERO, LIMIT_EXP, LIMIT_LOG
  USE CRTM_Atmosphere_Define,   ONLY: CRTM_Atmosphere_type
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type, &
                                      CRTM_GeometryInfo_GetValue
  USE CRTM_AtmOptics_Define,    ONLY: CRTM_AtmOptics_type
  USE ODPS_Define,              ONLY: ODPS_type          , &
                                      SIGNIFICANCE_OPTRAN
  USE ODPS_Predictor_Define,    ONLY: ODPS_Predictor_type, &
                                      PAFV_Associated    , &
                                      MAX_OPTRAN_ORDER   , &
                                      MAX_OPTRAN_USED_PREDICTORS
  USE ODPS_CoordinateMapping,   ONLY: Interpolate_Profile      , &
                                      Interpolate_Profile_F1_TL, &
                                      Interpolate_Profile_F1_AD, &
                                      Compute_Interp_Index

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
  PUBLIC :: ODPS_Compute_AtmAbsorption
  PUBLIC :: ODPS_Compute_AtmAbsorption_TL
  PUBLIC :: ODPS_Compute_AtmAbsorption_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
  '$Id: $'
  ! Maximum allowed layer optical depth
  REAL(fp), PARAMETER :: MAX_OD = 20.0_fp


  ! ------------------------------------------
  ! Structure definition to hold forward model
  ! variables across FWD, TL, and AD calls
  ! ------------------------------------------
  TYPE :: iVar_type
    PRIVATE
    INTEGER :: dummy
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
!       ODPS_Compute_AtmAbsorption
!
! PURPOSE:
!       Subroutine to calculate the layer optical depths due to gaseous
!       absorption for a given sensor and channel and atmospheric profile.
!
! CALLING SEQUENCE:
!       CALL ODPS_Compute_AtmAbsorption( &
!              TC           , &
!              ChannelIndex , &
!              Predictor    , &
!              AtmAbsorption  )
!
! INPUTS:
!       TC:
!         ODPS structure holding tau coefficients
!         UNITS:      N/A
!         TYPE:       ODPS_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:
!         Channel index id. This is a unique index associated with a
!         supported sensor channel used to access the shared coefficient
!         data for a particular sensor's channel.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       Predictor: 
!         Structure containing the predictor profile data.
!         UNITS:      N/A
!         TYPE:       ODPS_Predictor_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       AtmAbsorption:
!         Structure containing computed optical depth profile data.
!         UNITS:      N/A
!         TYPE:       CRTM_AtmOptics_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE ODPS_Compute_AtmAbsorption( &
    TC           , &  ! Input
    ChannelIndex , &  ! Input
    Predictor    , &  ! Input
    AtmAbsorption  )  ! Output
    ! Arguments
    TYPE(ODPS_type)          , INTENT(IN)     :: TC
    INTEGER                  , INTENT(IN)     :: ChannelIndex
    TYPE(ODPS_Predictor_type), INTENT(IN OUT) :: Predictor
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: AtmAbsorption
    ! Local variables
    INTEGER  :: n_Layers, n_User_Layers
    INTEGER  :: i          ! coefficent index
    INTEGER  :: k          ! Layer index
    INTEGER  :: j          ! Component index
    INTEGER  :: np         ! number of predictors
    REAL(fp) :: OD(Predictor%n_Layers)
    REAL(fp) :: OD_Path(0:Predictor%n_Layers)
    REAL(fp) :: User_OD_Path(0:Predictor%n_User_Layers)
    INTEGER  :: ODPS2User_Idx(2, 0:Predictor%n_User_Layers)
    REAL(fp) :: OD_tmp
    LOGICAL  :: OPTRAN
    INTEGER  :: j0, js

    ! ------
    ! Set up
    ! ------
    ! Assign the indices to a short name
    n_Layers = Predictor%n_Layers
    n_User_Layers = Predictor%n_User_Layers

    !--------------------------------------------------------
    ! Compute optical path profile
    !--------------------------------------------------------
    ! Loop over each tau component for optical depth calculation
    OD = ZERO
    Component_Loop: DO j = 1, Predictor%n_Components

      ! number of predictors for the current component and channel
      ! Note, TC%n_Predictors(j, ChannelIndex) <= Predictor%n_CP(j).
      ! For example, if the upper m predictors have zero coefficients,
      ! then, only coefficients with indexed 1 to (Predictor%n_CP(j) - m)
      ! are stored and used used in the OD calculations.
      np = TC%n_Predictors(j, ChannelIndex)

      ! Check if there is any absorption for the component&channel combination.
      IF( np <= 0 ) CYCLE Component_Loop

      ! set flag for possible OPTRAN algorithm
      ! If this flag is set, this component is computed using OPTRAN algorithm.
      ! Otherwise, it is computed using ODPS algorithm.
      IF( Predictor%OPTRAN .AND. j == TC%OComponent_Index)THEN
         OPTRAN = TC%OSignificance(ChannelIndex) == SIGNIFICANCE_OPTRAN
      ELSE
         OPTRAN = .FALSE.
      END IF

      IF(OPTRAN)THEN
        CALL Add_OPTRAN_wloOD(TC,          &
                             ChannelIndex,  &
                             Predictor,     &
                             OD )
      ELSE

        ! ODPS algorithm
        j0 = TC%Pos_Index(j, ChannelIndex)
        DO i = 1, np
          js = j0+(i-1)*n_Layers-1
          DO k = 1, n_Layers
            OD(k) = OD(k) + TC%C(js+k)*Predictor%X(k, i, j)
          END DO
        END DO

      END IF

    END DO Component_Loop

    !------------------------------------------------------
    ! Compute optical path (level to space) profile
    !------------------------------------------------------
    OD_Path(0) = ZERO
    DO k = 1, n_layers
      OD_tmp = OD(k)
      IF(OD(k) < ZERO)THEN
        OD_tmp = ZERO
      ELSE IF(OD(k) > MAX_OD)THEN
        OD_tmp = MAX_OD
      END IF
      OD_Path(k) = OD_Path(k-1) + OD_tmp
    END DO

    ! Save forward variables
    ! Interpolate the path profile back on the user pressure grids,
    ! Compute layer optical depths (vertical direction)
    IF ( PAFV_Associated(Predictor%PAFV) ) THEN
      ! save forwad variables
      Predictor%PAFV%OD = OD
      Predictor%PAFV%OD_Path = OD_Path
      ! If interpolation indexes are known
      User_OD_Path(0) = ZERO
      CALL Interpolate_Profile(Predictor%PAFV%ODPS2User_Idx,    &
                               OD_Path,                         &
                               Predictor%Ref_Level_LnPressure,  &
                               Predictor%User_Level_LnPressure, &
                               User_OD_Path)
    ELSE
      ! interpolation indexes are not known
      CALL Compute_Interp_Index(Predictor%Ref_Level_LnPressure, &
                                Predictor%User_Level_LnPressure,&
                                ODPS2User_Idx)
      CALL Interpolate_Profile(ODPS2User_Idx,                   &
                               OD_Path,                         &
                               Predictor%Ref_Level_LnPressure,  &
                               Predictor%User_Level_LnPressure, &
                               User_OD_Path)
    END IF

    ! Optical depth profile scaled to zenith.  Note that the scaling
    ! factor is the surface secant zenith angle.
    AtmAbsorption%Optical_Depth = (User_OD_Path(1:n_User_Layers) - &
                                   User_OD_Path(0:n_User_Layers-1)) / &
                                   Predictor%Secant_Zenith_Surface

  END SUBROUTINE ODPS_Compute_AtmAbsorption

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ODPS_Compute_AtmAbsorption_TL
!
! PURPOSE:
!       Subroutine to calculate the tangent-linear layer optical depths due
!       to gaseous absorption for a given sensor and channel and atmospheric
!       profile.
!
! CALLING SEQUENCE:
!       CALL ODPS_Compute_AtmAbsorption_TL( &
!              TC           ,   &
!              ChannelIndex ,   &
!              Predictor    ,   &
!              Predictor_TL,    &
!              AtmAbsorption_TL )
!
! INPUTS:
!       TC:
!         ODPS structure holding tau coefficients
!         UNITS:      N/A
!         TYPE:       ODPS_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:
!         Channel index id. This is a unique index associated with a
!         supported sensor channel used to access the shared coefficient
!         data for a particular sensor's channel.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       Predictor: 
!         Structure containing the predictor profile data.
!         UNITS:      N/A
!         TYPE:       ODPS_Predictor_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       Predictor_TL: 
!         Structure containing the tangent-linear predictor profile data.
!         UNITS:      N/A
!         TYPE:       ODPS_Predictor_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       AtmAbsorption_TL:
!         Structure containing computed tangent-linear optical depth profile
!         data.
!         UNITS:      N/A
!         TYPE:       CRTM_AtmOptics_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE ODPS_Compute_AtmAbsorption_TL( &
    TC              , &  ! Input
    ChannelIndex    , &  ! Input
    Predictor       , &  ! Input
    Predictor_TL    , &  ! Input
    AtmAbsorption_TL  )  ! Output
    ! Arguments
    TYPE(ODPS_type)          , INTENT(IN)     :: TC
    INTEGER                  , INTENT(IN)     :: ChannelIndex
    TYPE(ODPS_Predictor_type), INTENT(IN)     :: Predictor
    TYPE(ODPS_Predictor_type), INTENT(IN OUT) :: Predictor_TL
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: AtmAbsorption_TL
    ! Local variables
    INTEGER  :: n_Layers, n_User_Layers
    INTEGER  :: i          ! coefficent index
    INTEGER  :: k          ! Layer index
    INTEGER  :: j          ! Component index
    INTEGER  :: np         ! number of predictors
    REAL(fp) :: OD_TL(Predictor%n_Layers)
    REAL(fp) :: OD_Path_TL(0:Predictor%n_Layers)
    REAL(fp) :: User_OD_Path_TL(0:Predictor%n_User_Layers)
    LOGICAL  :: OPTRAN
    INTEGER  :: j0, js

    ! ------
    ! Set up
    ! ------
    ! Assign the indices to a short name
    n_Layers = Predictor%n_Layers
    n_User_Layers = Predictor%n_User_Layers

    !-----------------------------
    ! Compute optical path profile
    !-----------------------------
    ! Loop over each tau component for optical depth calculation
    OD_TL = ZERO
    Component_Loop: DO j = 1, Predictor%n_Components

      ! number of predictors for the current component and channel
      ! Note, TC%n_Predictors(j, ChannelIndex) <= Predictor%n_CP(j).
      ! For example, if the upper m predictors have zero coefficients,
      ! then, only coefficients with indexed 1 to (Predictor%n_CP(j) - m)
      ! are stored and used used in the OD calculations.
      np = TC%n_Predictors(j, ChannelIndex)

      ! Check if there is any absorption for the component&channel combination.
      IF( np <= 0 ) CYCLE Component_Loop

      ! set flag for possible OPTRAN algorithm
      ! If this flag is set, this component is computed using OPTRAN algorithm.
      ! Otherwise, it is computed using ODPS algorithm.
      IF( Predictor%OPTRAN .AND. j == TC%OComponent_Index)THEN
         OPTRAN = TC%OSignificance(ChannelIndex) == SIGNIFICANCE_OPTRAN
      ELSE
         OPTRAN = .FALSE.
      END IF

      IF(OPTRAN)THEN
        CALL Add_OPTRAN_wloOD_TL(TC,       &
                             ChannelIndex,  &
                             Predictor,     &
                             Predictor_TL,  &
                             OD_TL)
      ELSE

        ! ODPS algorithm
        j0 = TC%Pos_Index(j, ChannelIndex)
        DO i = 1, np
          js = j0+(i-1)*n_Layers-1
          DO k = 1, n_Layers
            OD_TL(k) = OD_TL(k) + TC%C(js+k)*Predictor_TL%X(k, i, j)
          END DO
        END DO

      END IF

    END DO Component_Loop

    !------------------------------------------------------
    ! Compute optical path (level to space) profile
    !------------------------------------------------------
    OD_Path_TL(0) = ZERO
    DO k = 1, n_layers
      IF(Predictor%PAFV%OD(k) < ZERO)THEN
        OD_TL(k) = ZERO
      ELSE IF(Predictor%PAFV%OD(k) > MAX_OD)THEN
        OD_TL(k) = ZERO
      END IF
      OD_Path_TL(k) = OD_Path_TL(k-1) + OD_TL(k)
    END DO

    ! Interpolate the path profile back on the user pressure grids,
    ! Compute layer optical depths (vertical direction)
    CALL Interpolate_Profile_F1_TL(Predictor%PAFV%ODPS2User_Idx,    &
                                   Predictor%Ref_Level_LnPressure,  &
                                   Predictor%User_Level_LnPressure, &
                                   OD_Path_TL,                      &
                                   User_OD_Path_TL)

    AtmAbsorption_TL%Optical_Depth = (User_OD_Path_TL(1:n_User_Layers) - &
                                   User_OD_Path_TL(0:n_User_Layers-1)) / &
                                   Predictor%Secant_Zenith_Surface

  END SUBROUTINE ODPS_Compute_AtmAbsorption_TL

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ODPS_Compute_AtmAbsorption_AD
!
! PURPOSE:
!       Subroutine to calculate the layer optical depth adjoint due to gaseous
!       absorption for a given sensor and channel and atmospheric profile.
!
! CALLING SEQUENCE:
!       CALL ODPS_Compute_AtmAbsorption_AD( &
!              TC              , &
!              ChannelIndex    , &
!              Predictor       , &
!              AtmAbsorption_AD, &
!              Predictor_AD      )
!
! INPUTS:
!       TC:
!         ODPS structure holding tau coefficients
!         UNITS:      N/A
!         TYPE:       ODPS_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:
!         Channel index id. This is a unique index associated with a
!         supported sensor channel used to access the shared coefficient
!         data for a particular sensor's channel.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       Predictor: 
!         Structure containing the predictor profile data.
!         UNITS:      N/A
!         TYPE:       ODPS_Predictor_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       AtmAbsorption_AD:
!         Structure containing optical depth adjoint profile data.
!         **NOTE: Optical depth component Set to zero upon exit **
!         UNITS:      N/A
!         TYPE:       CRTM_AtmOptics_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Predictor_AD: 
!         Structure containing the adjoint predictor profile data.
!         **NOTE: Must contain data upon entry **
!         UNITS:      N/A
!         TYPE:       ODPS_Predictor_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE ODPS_Compute_AtmAbsorption_AD( &
    TC              , &  ! Input
    ChannelIndex    , &  ! Input
    Predictor       , &  ! Input
    AtmAbsorption_AD, &  ! Input
    Predictor_AD      )  ! Output
    ! Arguments
    TYPE(ODPS_type)          , INTENT(IN)     :: TC
    INTEGER                  , INTENT(IN)     :: ChannelIndex
    TYPE(ODPS_Predictor_type), INTENT(IN)     :: Predictor
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: AtmAbsorption_AD
    TYPE(ODPS_Predictor_type), INTENT(IN OUT) :: Predictor_AD
    ! Local variables
    INTEGER  :: n_Layers, n_User_Layers
    INTEGER  :: i          ! coefficent index
    INTEGER  :: k          ! Layer index
    INTEGER  :: j          ! Component index
    INTEGER  :: np         ! number of predictors
    REAL(fp) :: OD_AD(Predictor%n_Layers)
    REAL(fp) :: OD_Path_AD(0:Predictor%n_Layers)
    REAL(fp) :: User_OD_Path_AD(0:Predictor%n_User_Layers)
    LOGICAL  :: OPTRAN
    INTEGER  :: j0, js

    ! ------
    ! Set up
    ! ------
    ! Assign the indices to a short name
    n_Layers = Predictor%n_Layers
    n_User_Layers = Predictor%n_User_Layers

    !------- Adjoint part ---------

    ! Interpolate the path profile back on the user pressure grids,
    ! Compute layer optical depths (vertical direction)
    User_OD_Path_AD(n_User_Layers) = ZERO
    DO k = n_User_Layers, 1, -1
      User_OD_Path_AD(k) = User_OD_Path_AD(k) &
                           + AtmAbsorption_AD%Optical_Depth(k)/Predictor%Secant_Zenith_Surface
      ! combined with initilization
      User_OD_Path_AD(k-1) = -AtmAbsorption_AD%Optical_Depth(k)/Predictor%Secant_Zenith_Surface
    END DO
    AtmAbsorption_AD%Optical_Depth = ZERO

    OD_Path_AD = ZERO
    CALL Interpolate_Profile_F1_AD(Predictor%PAFV%ODPS2User_Idx,       &
                                   Predictor%Ref_Level_LnPressure,     &
                                   Predictor%User_Level_LnPressure,    &
                                   User_OD_Path_AD,                    &
                                   OD_Path_AD )


    User_OD_Path_AD(0) = ZERO

    !-----------------------------
    ! Compute optical path profile
    !-----------------------------

    DO k = n_layers, 1, -1
      OD_Path_AD(k-1) = OD_Path_AD(k-1) + OD_Path_AD(k)
      ! combined with initialization
      OD_AD(k) = OD_Path_AD(k)
      OD_Path_AD(k) = ZERO
      IF(Predictor%PAFV%OD(k) < ZERO)THEN
        OD_AD(k) = ZERO
      ELSE IF(Predictor%PAFV%OD(k) > MAX_OD)THEN
        OD_AD(k) = ZERO
      END IF
    END DO
    OD_Path_AD(0) = ZERO

    ! Loop over each tau component for optical depth calculation

    Component_Loop_AD: DO j = 1, Predictor%n_Components

      ! number of predictors for the current component and channel
      ! Note, TC%n_Predictors(j, ChannelIndex) <= Predictor%n_CP(j).
      ! For example, if the upper m predictors have zero coefficients,
      ! then, only coefficients with indexed 1 to (Predictor%n_CP(j) - m)
      ! are stored and used used in the OD calculations.
      np = TC%n_Predictors(j, ChannelIndex)

      ! Check if there is any absorption for the component&channel combination.
      IF( np <= 0 ) CYCLE Component_Loop_AD

      IF( Predictor%OPTRAN .AND. j == TC%OComponent_Index)THEN
         OPTRAN = TC%OSignificance(ChannelIndex) == SIGNIFICANCE_OPTRAN
      ELSE
         OPTRAN = .FALSE.
      END IF

     IF(OPTRAN)THEN

       CALL Add_OPTRAN_wloOD_AD(TC,          &
                                ChannelIndex, &
                                Predictor,    &
                                OD_AD,        &
                                Predictor_AD )
     ELSE

       ! ODPS algorithm
       j0 = TC%Pos_Index(j, ChannelIndex)
       DO i = 1, np
         js = j0+(i-1)*n_Layers-1
         DO k = n_Layers, 1, -1
           Predictor_AD%X(k, i, j) = Predictor_AD%X(k, i, j) + TC%C(js+k)*OD_AD(k)
         END DO
       END DO

     END IF

    END DO Component_Loop_AD

  END SUBROUTINE ODPS_Compute_AtmAbsorption_AD


!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       Add_OPTRAN_wloOD
!
! PURPOSE:
!       Subroutine to calculate and add the layer optical depths due to water
!       vapor line absorption via the ODAS algorithm.
!       Note the OD argument is an in/out argument, which may hold accumulated
!       optical depths from other absorbers.
!
! CALLING SEQUENCE:
!        CALL Add_OPTRAN_wloOD( &
!               TC          , &
!               ChannelIndex, &
!               Predictor   , &
!               OD            )
!
! INPUTS:
!       TC:
!         ODPS structure holding tau coefficients
!         UNITS:      N/A
!         TYPE:       ODPS_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:
!         Channel index id. This is a unique index associated with a
!         supported sensor channel used to access the shared coefficient
!         data for a particular sensor's channel.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       Predictor: 
!         Structure containing the predictor profile data.
!         UNITS:      N/A
!         TYPE:       ODPS_Predictor_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
! IN/OUTPUTS:
!        OD:
!          Slant path optical depth profile.
!          UNITS:      N/A
!          TYPE:       REAL(fp)
!          DIMENSION:  Rank-1 array (n_Layers)
!          ATTRIBUTES: INTENT(IN OUT)
!
!
!------------------------------------------------------------------------------

  SUBROUTINE Add_OPTRAN_wloOD( &
    TC          , &  ! Input
    ChannelIndex, &  ! Input
    Predictor   , &  ! Input
    OD            )  ! In/Output
    ! Arguments
    TYPE(ODPS_type),           INTENT(IN)     :: TC
    INTEGER,                   INTENT(IN)     :: ChannelIndex
    TYPE(ODPS_Predictor_type), INTENT(IN OUT) :: Predictor
    REAL(fp),                  INTENT(IN OUT) :: OD(:)
    ! Local variables
    REAL(fp) :: LN_Chi(TC%n_Layers), coeff(0:MAX_OPTRAN_ORDER)
    REAL(fp) :: b(TC%n_Layers, 0:MAX_OPTRAN_USED_PREDICTORS)
    REAL(fp) :: Chi(TC%n_Layers)
    INTEGER  :: np, n_Layers, n_orders, js, i, j, k, ii, jj

    ! -----------------------------------------
    ! Check if there is any absorption for this
    ! absorber/channel combination.
    ! -----------------------------------------
    np = TC%OP_Index(0,ChannelIndex)  ! number of predictors
    IF ( np <= 0 ) RETURN

    n_Layers = TC%n_Layers
    js = TC%OPos_Index(ChannelIndex)
    n_orders = TC%Order(ChannelIndex)

    DO i = 0, np
      jj = js + i*(n_orders+1)
      coeff(0:n_orders) = TC%OC(jj:jj+n_orders)
      DO k = 1, n_Layers
        b(k,i) = coeff(0)
        DO j = 1, n_orders
          b(k,i) = b(k,i) + coeff(j)*Predictor%Ap(k, j)
        END DO
      END DO
    END DO

    LN_Chi = b(:,0)
    DO i = 1, np
      ii = TC%OP_Index(i,ChannelIndex)
      DO k = 1, n_Layers
        LN_Chi(k) = LN_Chi(k) + b(k, i)* Predictor%OX(k, ii)
      END DO
    END DO

    DO k = 1, n_Layers
      IF( LN_Chi(k) > LIMIT_EXP ) THEN
        Chi(k) = LIMIT_LOG
      ELSE IF( LN_Chi(k) < -LIMIT_EXP ) THEN
        Chi(k) = ZERO
      ELSE
        Chi(k) = EXP(LN_Chi(k))
      ENDIF
      OD(k) = OD(k) + Chi(k)*Predictor%dA(k)
    END DO

    IF(Predictor%PAFV%OPTRAN)THEN
      Predictor%PAFV%b      = b
      Predictor%PAFV%LN_Chi = LN_Chi
      Predictor%PAFV%Chi    = Chi
    END IF

  END SUBROUTINE Add_OPTRAN_wloOD


!------------------------------------------------------------------------------
!
! NAME:
!       Add_OPTRAN_wloOD_TL
!
! PURPOSE:
!       Subroutine to calculate and add the tangent-linear layer optical depths
!       due to water vapor line absorption via the ODAS algorithm.
!       Note the OD_TL argument is an in/out argument, which may hold accumulated
!       tangent-linear optical depths from other absorbers.
!
! CALLING SEQUENCE:
!        CALL Add_OPTRAN_wloOD_TL( &
!               TC          , &
!               ChannelIndex, &
!               Predictor   , &
!               Predictor_TL, &
!               OD_TL         )
!
! INPUTS:
!       TC:
!         ODPS structure holding tau coefficients
!         UNITS:      N/A
!         TYPE:       ODPS_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:
!         Channel index id. This is a unique index associated with a
!         supported sensor channel used to access the shared coefficient
!         data for a particular sensor's channel.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       Predictor: 
!         Structure containing the predictor profile data.
!         UNITS:      N/A
!         TYPE:       ODPS_Predictor_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       Predictor_TL: 
!         Structure containing the tangent-linear predictor profile data.
!         UNITS:      N/A
!         TYPE:       ODPS_Predictor_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
! IN/OUTPUT ARGUMENTS:
!       OD_TL:
!         Slant path tangent-linear optical depth profile
!         UNITS:      N/A
!         TYPE:       REAL(fp)
!         DIMENSION:  Rank-1 array (n_Layers)
!         ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------

  SUBROUTINE Add_OPTRAN_wloOD_TL( &
    TC          , &
    ChannelIndex, &
    Predictor   , &
    Predictor_TL, &
    OD_TL         )
    ! Arguments
    TYPE(ODPS_type),           INTENT(IN)     :: TC
    INTEGER,                   INTENT(IN)     :: ChannelIndex
    TYPE(ODPS_Predictor_type), INTENT(IN)     :: Predictor
    TYPE(ODPS_Predictor_type), INTENT(IN)     :: Predictor_TL
    REAL(fp),                  INTENT(IN OUT) :: OD_TL(:)
    ! Local variables
    REAL(fp) :: coeff(0:MAX_OPTRAN_ORDER)
    REAL(fp) :: LN_Chi_TL(TC%n_Layers), b_TL(TC%n_Layers, 0:MAX_OPTRAN_USED_PREDICTORS)
    REAL(fp) :: chi_TL(TC%n_Layers)
    INTEGER  :: np, n_Layers, n_orders, js, i, j, k, ii, jj

    ! -----------------------------------------
    ! Check if there is any absorption for this
    ! absorber/channel combination.
    ! -----------------------------------------
    np = TC%OP_Index(0,ChannelIndex)  ! number of predictors
    IF ( np <= 0 ) RETURN

    n_Layers = TC%n_Layers
    js = TC%OPos_Index(ChannelIndex)
    n_orders = TC%Order(ChannelIndex)

    DO i = 0, np
      jj = js + i*(n_orders+1)
      coeff(0:n_orders) = TC%OC(jj:jj+n_orders)
      DO k = 1, n_Layers
        b_TL(k,i) = ZERO
        DO j = 1, n_orders
          b_TL(k,i) = b_TL(k,i) + coeff(j)*Predictor_TL%Ap(k, j)
        END DO
      END DO
    END DO

    LN_Chi_TL = b_TL(:,0)
    DO i = 1, np
      ii = TC%OP_Index(i,ChannelIndex)
      DO k = 1, n_Layers
        LN_Chi_TL(k) = LN_Chi_TL(k) + b_TL(k, i)* Predictor%OX(k, ii) + Predictor%PAFV%b(k, i)* Predictor_TL%OX(k, ii)
      END DO
    END DO

    DO k = 1, n_Layers
      IF( Predictor%PAFV%LN_Chi(k) > LIMIT_EXP ) THEN
        Chi_TL(k) = ZERO
      ELSE IF( Predictor%PAFV%LN_Chi(k) < -LIMIT_EXP ) THEN
        Chi_TL(k) = ZERO
      ELSE
        Chi_TL(k) = Predictor%PAFV%Chi(k) * LN_Chi_TL(k)
      ENDIF
      OD_TL(k) = OD_TL(k) + Chi_TL(k)*Predictor%dA(k) + Predictor%PAFV%Chi(k)*Predictor_TL%dA(k)
    END DO

  END SUBROUTINE Add_OPTRAN_wloOD_TL


!------------------------------------------------------------------------------
!
! NAME:
!       Add_OPTRAN_wloOD_AD
!
! PURPOSE:
!       Subroutine to calculate and add the layer optical depth adjoints due
!       to water vapor line absorption via the ODAS algorithm.
!
! CALLING SEQUENCE:
!        CALL Add_OPTRAN_wloOD_AD( &
!               TC          , &
!               ChannelIndex, &
!               Predictor   , &
!               OD_AD       , &
!               Predictor_AD  )
!
! INPUTS:
!       TC:
!         ODPS structure holding tau coefficients
!         UNITS:      N/A
!         TYPE:       ODPS_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:
!         Channel index id. This is a unique index associated with a
!         supported sensor channel used to access the shared coefficient
!         data for a particular sensor's channel.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       Predictor: 
!         Structure containing the predictor profile data.
!         UNITS:      N/A
!         TYPE:       ODPS_Predictor_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!       OD_AD:
!         Slant path adjoint optical depth profile
!         UNITS:      N/A
!         TYPE:       REAL(fp)
!         DIMENSION:  Rank-1 array (n_Layers)
!         ATTRIBUTES: INTENT(IN)
!
! IN/OUTPUTS:
!       Predictor_AD: 
!         Structure containing the adjoint predictor profile data.
!         **NOTE: Must contain data upon entry **
!         UNITS:      N/A
!         TYPE:       ODPS_Predictor_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------

  SUBROUTINE Add_OPTRAN_wloOD_AD( &
    TC          , &
    ChannelIndex, &
    Predictor   , &
    OD_AD       , &
    Predictor_AD  )
    ! Arguments
    TYPE(ODPS_type),           INTENT(IN)     :: TC
    INTEGER,                   INTENT(IN)     :: ChannelIndex
    TYPE(ODPS_Predictor_type), INTENT(IN)     :: Predictor
    REAL(fp),                  INTENT(IN OUT) :: OD_AD(:)
    TYPE(ODPS_Predictor_type), INTENT(IN OUT) :: Predictor_AD
    ! Local variables
    REAL(fp) :: coeff(0:MAX_OPTRAN_ORDER)
    REAL(fp) :: LN_Chi_AD(TC%n_Layers), b_AD(TC%n_Layers, 0:MAX_OPTRAN_USED_PREDICTORS)
    REAL(fp) :: Chi_AD(TC%n_Layers)
    INTEGER  :: np, n_Layers, n_orders, js, i, j, k, ii, jj

      !------ Forward part for LN_Chi, b -----------

      ! -----------------------------------------
      ! Check if there is any absorption for this
      ! absorber/channel combination.
      ! -----------------------------------------
      np = TC%OP_Index(0,ChannelIndex)  ! number of predictors
      IF ( np <= 0 ) RETURN

      n_Layers = TC%n_Layers
      js = TC%OPos_Index(ChannelIndex)
      n_orders = TC%Order(ChannelIndex)

      !------ Adjoint part ----------------------

      ! -----------------------------------------
      ! Check if there is any absorption for this
      ! absorber/channel combination.
      ! -----------------------------------------

      Chi_AD = ZERO
      LN_Chi_AD = ZERO
      DO k = n_Layers, 1, -1

        Chi_AD(k) = Chi_AD(k) + OD_AD(k) * Predictor%dA(k)
        Predictor_AD%dA(k) = Predictor_AD%dA(k) + OD_AD(k) * Predictor%PAFV%Chi(k)
        IF( Predictor%PAFV%LN_Chi(k) > LIMIT_EXP ) THEN
          Chi_AD(k) = ZERO
        ELSE IF( Predictor%PAFV%LN_Chi(k) < -LIMIT_EXP ) THEN
          Chi_AD(k) = ZERO
        ELSE
          LN_Chi_AD(k) = Predictor%PAFV%Chi(k) * Chi_AD(k)  ! combinded with initialization for LN_Chi_AD(k)
        ENDIF
      END DO

      DO i = 1, np
        ii = TC%OP_Index(i,ChannelIndex)
        DO k = n_Layers, 1, -1
          b_AD(k, i) = LN_Chi_AD(k) * Predictor%OX(k, ii)  ! Combinded with initialization for b_AD
          Predictor_AD%OX(k, ii) = Predictor_AD%OX(k, ii) + LN_Chi_AD(k)*Predictor%PAFV%b(k, i)
        END DO
      END DO
      b_AD(:,0) = LN_Chi_AD

      DO i = 0, np
        jj = js + i*(n_orders+1)
        coeff(0:n_orders) = TC%OC(jj:jj+n_orders)
        DO k = n_Layers, 1, -1
          DO j = 1, n_orders
            Predictor_AD%Ap(k, j) = Predictor_AD%Ap(k, j) + coeff(j)*b_AD(k,i)
          END DO
          b_AD(k,i) = ZERO
        END DO
      END DO

   END SUBROUTINE Add_OPTRAN_wloOD_AD

END MODULE ODPS_AtmAbsorption
