!
! ODPS_CoordinateMapping
!
! Module containing routines to perform data mapping from user pressure space to
! internal pressure space for the ODPS algorithm.
!
!
! CREATION HISTORY:
!       Written by:     Yong Han & Yong Chen, JCSDA, NOAA/NESDIS 10-NOV-2009


MODULE ODPS_CoordinateMapping

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,                ONLY: fp
  USE CRTM_Atmosphere_Define,    ONLY: CRTM_Atmosphere_type, H2O_ID
  USE CRTM_GeometryInfo_Define,  ONLY: CRTM_GeometryInfo_type, &
                                       CRTM_GeometryInfo_GetValue
  USE ODPS_Define,               ONLY: ODPS_type
  USE PAFV_Define,               ONLY: PAFV_type, &
                                       PAFV_Associated
  USE Profile_Utility_Parameters,ONLY: G0, EPS, R_DRYAIR
  USE CRTM_Parameters,           ONLY: ZERO, ONE, TWO, &
                                       EARTH_RADIUS,   &
                                       DEGREES_TO_RADIANS

  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! variables and routines from USE modules
  PUBLIC :: Map_Input
  PUBLIC :: Map_Input_TL
  PUBLIC :: Map_Input_AD
  PUBLIC :: Interpolate_Profile
  PUBLIC :: Interpolate_Profile_F1_TL
  PUBLIC :: Interpolate_Profile_F1_AD
  PUBLIC :: Compute_Interp_Index

  ! Parameters used in the geopotential height calculation routines
  ! a factor used in the virtual temperature Tv calculation
  REAL(fp), PARAMETER :: C = (ONE/EPS - ONE) / 1000.0_fp
  ! a factor used in the scale height calculation ( H = CC*Tv)
  REAL(fp), PARAMETER :: CC = 0.001_fp*R_DRYAIR/G0
  ! for using in SUBROUTINE LayerAvg
  REAL(fp), PARAMETER :: SMALLDIFF  = 1.0E-20_fp

  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: ODPS_CoordinateMapping.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'

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
!       Map_Input
!
! PURPOSE:
!       Subroutine for data mapping from user pressure space to
!       internal pressure space for the ODPS algorithm.
!
! CALLING SEQUENCE:
!       CALL Map_Input( Atm,                   & ! Input
!                       TC,                    & ! Input
!                       GeoInfo,               & ! Input
!                       Temperature,           & ! Output
!                       Absorber,              & ! Output
!                       User_Level_LnPressure, & ! Output
!                       Ref_Level_LnPressure,  & ! Output
!                       Secant_Zenith,         & ! Output
!                       PAFV )                   ! In/Output
!
! INPUT ARGUMENTS:
!
!        Atm       :     CRTM Atmosphere structure containing the atmospheric
!                        state data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!          TC:           ODPS structure holding tau coefficients
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       GeoInfo     :    CRTM_GeometryInfo structure containing the
!                        view geometry information.
!                        UNITS:      N/A
!                        TYPE:       CRTM_GeometryInfo_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!
!       Temperature :    Temperatures on internal pressure grids
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank 1 array
!                        ATTRIBUTES: INTENT(IN)
!
!         Absorber :     Layer absorber amount on internal pressure grids
!                        UNITS:      depend on absorber types
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank 2 array (n_layers x n_abosrbers)
!                        ATTRIBUTES: INTENT(IN)
!
! User_Level_LnPressure: User level pressure coordinate in Log scale
!                        UNITS:      N/A  (Pressure in hPa)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank 1 array (0:n_userLayers)
!                        ATTRIBUTES: INTENT(IN)
!
! Ref_Level_LnPressure:  internal level pressure coordinate in Log scale
!                        UNITS:      N/A  (Pressure in hPa)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank 1 array (0:n_refLayers)
!                        ATTRIBUTES: INTENT(IN)
!       Secant_Zenith :  Secant zenith angle array on internal pressure grids
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank 1 array (n_refLayers)
!                        ATTRIBUTES: INTENT(IN)
!
! IN/OUTPUT ARGUMENTS:
!                 PAFV:  Structure containing FW variables for TL and AD uses
!                        UNITS:      N/A
!                        TYPE:       PAFV_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!------------------------------------------------------------------------------
  SUBROUTINE Map_Input(Atm,                   & ! Input
                       TC,                    & ! Input
                       GeoInfo,               & ! Input
                       Temperature,           & ! Output
                       Absorber,              & ! Output
                       User_Level_LnPressure, & ! Output
                       Ref_Level_LnPressure,  & ! Output
                       Secant_Zenith,         & ! Output
                       H2O_idx,               & ! Output
                       PAFV)                    ! In/Output
    TYPE(CRTM_Atmosphere_type)   , INTENT(IN)     :: Atm
    TYPE(ODPS_type)              , INTENT(IN)     :: TC
    TYPE(CRTM_GeometryInfo_type) , INTENT(IN)     :: GeoInfo
    REAL(fp)                     , INTENT(OUT)    :: Temperature(:)
    REAL(fp)                     , INTENT(OUT)    :: Absorber(:,:)
    REAL(fp)                     , INTENT(OUT)    :: User_Level_LnPressure(0:)
    REAL(fp)                     , INTENT(OUT)    :: Ref_Level_LnPressure(0:)
    REAL(fp)                     , INTENT(OUT)    :: Secant_Zenith(:)
    INTEGER                      , INTENT(OUT)    :: H2O_idx
    TYPE(PAFV_type)              , INTENT(INOUT)  :: PAFV

    ! Local variables
    REAL(fp) :: tolerance
    REAL(fp) :: SineAng
    REAL(fp) :: ODPS_sfc_fraction, Z_Offset, s
    REAL(fp) :: Z(0:TC%n_Layers)  ! Heights of pressure levels
    REAL(fp) :: Acc_Weighting(Atm%n_Layers, TC%n_Layers)
    INTEGER  :: interp_index(2, TC%n_Layers)
    REAL(fp) :: Ref_LnPressure(TC%n_Layers)
    REAL(fp) :: User_LnPressure(Atm%n_layers)
    REAL(fp) :: Surface_Altitude, Sensor_Zenith_Radian
    ! absorber index mapping from ODPS to user
    INTEGER  :: Idx_map(TC%n_Absorbers)
    INTEGER  :: j, jj, k, n_ODPS_Layers, n_User_Layers, ODPS_sfc_idx

    ! Define numerical precision tolerance.
    tolerance = EPSILON(ONE)

    n_ODPS_Layers = TC%n_Layers
    n_User_Layers = Atm%n_layers
    ! Set pressure profiles for interpolations
    Ref_LnPressure  = LOG(TC%Ref_Pressure)
    User_LnPressure = LOG(Atm%Pressure(1:n_User_Layers))
    Ref_Level_LnPressure = LOG(TC%Ref_Level_Pressure)
    IF(Atm%Level_Pressure(0) <= ZERO)THEN
      ! In this bad case, the top pressure level is set to the half of the next-to-top pressure level
      User_Level_LnPressure(0) = LOG(Atm%Level_Pressure(1)/TWO)
    ELSE
      User_Level_LnPressure(0) = LOG(Atm%Level_Pressure(0))
    END IF
    User_Level_LnPressure(1:n_User_Layers) = LOG(Atm%Level_Pressure(1:n_User_Layers))

    ! Find the index at which the ODPS layer contains the user surface pressure level. Then
    ! compute the fraction of the portion between the lower boundary of the ODPS layer and
    ! the user surface pressure level
    ODPS_sfc_idx = n_ODPS_Layers
    ODPS_sfc_fraction = ZERO
    IF(TC%Ref_Level_Pressure(n_ODPS_Layers) > Atm%Level_Pressure(n_User_Layers))THEN
      DO k = n_ODPS_Layers, 0, -1
        IF(TC%Ref_Level_Pressure(k) < Atm%Level_Pressure(n_User_Layers))THEN
          ODPS_sfc_idx = k+1
          ODPS_sfc_fraction = (Ref_Level_LnPressure(ODPS_sfc_idx) - &
                               User_Level_LnPressure(n_User_Layers)) /&
                              (Ref_Level_LnPressure(ODPS_sfc_idx) - &
                               Ref_Level_LnPressure(k))
          EXIT
        END IF
      END DO
    END IF

    ! Extract the needed GeometryInfo information
    CALL CRTM_GeometryInfo_GetValue( GeoInfo, &
                                     Surface_Altitude = Surface_Altitude, &
                                     Trans_Zenith_Radian = Sensor_Zenith_Radian )


    !-----------------------------------------------------------
    ! Interpolate temperautre profile on internal pressure grids
    !-----------------------------------------------------------
    CALL LayerAvg( Ref_LnPressure   , &
                   User_LnPressure  , &
                   Acc_Weighting    , &
                   interp_index)

    DO k = 1, n_ODPS_Layers
       Temperature(k) = SUM(Acc_Weighting(interp_index(1,k):interp_index(2,k), k)  &
                           * Atm%Temperature(interp_index(1,k):interp_index(2,k)) )
    END DO

    !-----------------------------------------------------------
    ! Interpolate absorber profiles on internal pressure grids
    !-----------------------------------------------------------
    DO j = 1,TC%n_Absorbers
      Idx_map(j) = -1
      DO jj=1, Atm%n_Absorbers
       IF( Atm%Absorber_ID(jj) == TC%Absorber_ID(j) ) THEN
        Idx_map(j) = jj
        EXIT
       END IF
      END DO

      ! save index for water vapor absorption
      H2O_idx = 1  ! default
      IF(TC%Absorber_ID(j) == H2O_ID)THEN
        H2O_idx = j
      END IF
      IF(Idx_map(j) > 0)THEN

        DO k = 1, n_ODPS_Layers
            Absorber(k,j) = SUM(Acc_Weighting(interp_index(1,k):interp_index(2,k), k)  &
                               * Atm%Absorber(interp_index(1,k):interp_index(2,k), Idx_map(j)) )
        END DO

        DO k=1, n_ODPS_Layers

          IF (Absorber(k,j) <= tolerance ) Absorber(k,j) = tolerance
          IF (Absorber(k,j) < TC%Min_Absorber(k,j) ) Absorber(k,j) = TC%Min_Absorber(k,j)
          IF (Absorber(k,j) > TC%Max_Absorber(k,j) ) Absorber(k,j) = TC%Max_Absorber(k,j)

        END DO
      ELSE ! when the profile is missing, use the referece profile
        Absorber(:, j) = TC%Ref_Absorber(:, j)
      END IF

    END DO

    !-----------------------------------------------
    ! Compute height dependent secant zenith angles
    !-----------------------------------------------
    ! Compute geopotential height, which starts from ODPS surface pressure level
    CALL Geopotential_Height( TC%Ref_Level_Pressure,       &
                              TC%Ref_Temperature,          &
                              TC%Ref_Absorber(:, H2O_idx), &
                              ZERO,                        &
                              Z)
    ! Adjust ODPS surface height for the user surface height. The adjustment includes two parts:
    ! (1) the delta Z from the ODPS surface pressure to the user surface pressure
    ! (2) the user-given surface height
    IF(TC%Ref_Level_Pressure(n_ODPS_Layers) >= Atm%Level_Pressure(n_User_Layers))THEN
      Z_Offset = -(Z(ODPS_sfc_idx) + ODPS_sfc_fraction*(Z(ODPS_sfc_idx-1)-Z(ODPS_sfc_idx))) &
                 + Surface_Altitude
    ELSE
      ! For the case in which the user surface pressure is larger than the ODPS surface pressure,
      ! the ODPS surface is adjusted for a surface pressure 1013 mb, regardless the user supplied
      ! surface height.
      Z_Offset = CC*TC%Ref_Temperature(n_ODPS_Layers) &  ! scale height
                 *LOG(1013.0_fp / TC%Ref_Level_Pressure(n_ODPS_Layers))
    END IF
    Z = Z + Z_Offset

    s = (EARTH_RADIUS + Surface_Altitude)*SIN(Sensor_Zenith_Radian)

    DO k = 1, n_ODPS_Layers
      SineAng = s /(EARTH_RADIUS + Z(k))
      Secant_Zenith(k) = ONE / SQRT(ONE - SineAng*SineAng)
    END DO

    IF ( PAFV_Associated(PAFV) ) THEN
      PAFV%Temperature  = Temperature
      PAFV%Absorber     = Absorber
      PAFV%interp_index = interp_index
      PAFV%Acc_Weighting= Acc_Weighting
      PAFV%idx_map      = idx_map
      PAFV%H2O_idx      = H2O_idx
      PAFV%Ref_LnPressure = Ref_LnPressure
      PAFV%User_LnPressure = User_LnPressure
    END IF

  END SUBROUTINE Map_Input

!------------------------------------------------------------------------------
!
! NAME:
!       Map_Input_TL
!
! PURPOSE:
!       TL Subroutine for data mapping from user pressure space to
!       internal pressure space for the ODPS algorithm.
!
! CALLING SEQUENCE:
!       CALL Map_Input_TL( TC,                    & ! Input
!                          Atm_TL,                & ! Input
!                          Temperature_TL,        & ! Output
!                          Absorber_TL,           & ! Output
!                          PAFV )                   ! In/Output
!
! INPUT ARGUMENTS:
!          TC:           ODPS structure holding tau coefficients
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!        Atm_TL    :     TL CRTM Atmosphere structure containing the atmospheric
!                        state data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!                 PAFV:  Structure containing FW variables for TL and AD uses
!                        UNITS:      N/A
!                        TYPE:       PAFV_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
! OUTPUT ARGUMENTS:
!
!       Temperature_TL : TL Temperatures on internal pressure grids
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank 1 array
!                        ATTRIBUTES: INTENT(OUT)
!
!         Absorber_TL :  TL Layer absorber amount on internal pressure grids
!                        UNITS:      depend on absorber types
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank 2 array (n_layers x n_abosrbers)
!                        ATTRIBUTES: INTENT(OUT)
!
!
!------------------------------------------------------------------------------
  SUBROUTINE Map_Input_TL(TC,              & ! Input
                          Atm_TL,          & ! Input
                          Temperature_TL,  & ! Output
                          Absorber_TL,     & ! Output
                          PAFV)              ! Input
    TYPE(ODPS_type)              , INTENT(IN)     :: TC
    TYPE(CRTM_Atmosphere_type)   , INTENT(IN)     :: Atm_TL
    REAL(fp)                     , INTENT(OUT)    :: Temperature_TL(:)
    REAL(fp)                     , INTENT(OUT)    :: Absorber_TL(:,:)
    TYPE(PAFV_type)              , INTENT(IN)     :: PAFV

    ! Local variables
    REAL(fp) :: tolerance
    ! absorber index mapping from ODPS to user
    INTEGER  :: Idx_map(TC%n_Absorbers), H2O_idx
    INTEGER  :: j, k, n_ODPS_Layers

    ! Define numerical precision tolerance.
    tolerance = EPSILON(ONE)

    n_ODPS_Layers = TC%n_Layers

    !-----------------------------------------------------------
    ! Interpolate temperautre profile on internal pressure grids
    !-----------------------------------------------------------
    DO k = 1, n_ODPS_Layers
       Temperature_TL(k) = &
         SUM(PAFV%Acc_Weighting(PAFV%interp_index(1,k):PAFV%interp_index(2,k), k)  &
             * Atm_TL%Temperature(PAFV%interp_index(1,k):PAFV%interp_index(2,k)) )
    END DO

    !-----------------------------------------------------------
    ! Interpolate absorber profiles on internal pressure grids
    !-----------------------------------------------------------
    H2O_idx = PAFV%H2O_idx
    Idx_map = PAFV%Idx_map

    DO j = 1,TC%n_Absorbers
      IF(idx_map(j) > 0)THEN

        DO k = 1, n_ODPS_Layers
           Absorber_TL(k,j) = &
            SUM(PAFV%Acc_Weighting(PAFV%interp_index(1,k):PAFV%interp_index(2,k), k)  &
                * Atm_TL%Absorber(PAFV%interp_index(1,k):PAFV%interp_index(2,k), Idx_map(j)) )
        END DO

        DO k=1, n_ODPS_Layers
          IF(PAFV%Absorber(k,j) <= tolerance ) Absorber_TL(k,j) = ZERO

        END DO
      ELSE ! when the profile is missing, use the referece profile
        Absorber_TL(:, j) = ZERO
      END IF

    END DO

  END SUBROUTINE Map_Input_TL

!------------------------------------------------------------------------------
!
! NAME:
!       Map_Input_AD
!
! PURPOSE:
!       AD Subroutine for data mapping from user pressure space to
!       internal pressure space for the ODPS algorithm.
!
! CALLING SEQUENCE:
!       CALL Map_Input_AD( TC,                    & ! Input
!                          Temperature_AD,        & ! Input
!                          Absorber_AD,           & ! Input
!                          Atm_AD,                & ! In/Output
!                          PAFV )                   ! Input
!
! INPUT ARGUMENTS:
!          TC:           ODPS structure holding tau coefficients
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Temperature_AD : AD Temperatures on internal pressure grids
!                        UNITS:      K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank 1 array
!                        ATTRIBUTES: INTENT(IN)
!
!         Absorber_AD :  AD Layer absorber amount on internal pressure grids
!                        UNITS:      depend on absorber types
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank 2 array (n_layers x n_abosrbers)
!                        ATTRIBUTES: INTENT(IN)
!
!                 PAFV:  Structure containing FW variables for TL and AD uses
!                        UNITS:      N/A
!                        TYPE:       PAFV_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
! OUTPUT ARGUMENTS:
!
!        Atm_AD    :     AD CRTM Atmosphere structure containing the atmospheric
!                        state data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(INOUT)
!
!------------------------------------------------------------------------------
  SUBROUTINE Map_Input_AD(TC,              & ! Input
                          Temperature_AD,  & ! Input
                          Absorber_AD,     & !  Input
                          Atm_AD,          & ! Output
                          PAFV)              ! Input

    TYPE(ODPS_type)              , INTENT(IN)     :: TC
    REAL(fp)                     , INTENT(INOUT)  :: Temperature_AD(:)
    REAL(fp)                     , INTENT(INOUT)  :: Absorber_AD(:,:)
    TYPE(CRTM_Atmosphere_type)   , INTENT(INOUT)  :: Atm_AD
    TYPE(PAFV_type)              , INTENT(IN)     :: PAFV

    ! Local variables
    REAL(fp) :: tolerance
    ! absorber index mapping from ODPS to user
    INTEGER  :: Idx_map(TC%n_Absorbers), H2O_idx
    INTEGER  :: j, k, n_ODPS_Layers

    ! Define numerical precision tolerance.
    tolerance = EPSILON(ONE)

    !-----------------------------------------------------------
    ! Initialization of forward model part
    !-----------------------------------------------------------

    n_ODPS_Layers = TC%n_Layers

    H2O_idx = PAFV%H2O_idx
    Idx_map = PAFV%Idx_map

    !-----------------------------------------------------------
    ! Interpolate absorber profiles on internal pressure grids
    !-----------------------------------------------------------
    DO j = TC%n_Absorbers, 1, -1
      IF(idx_map(j) > 0)THEN

        DO k=1, n_ODPS_Layers
          IF(PAFV%Absorber(k,j) <= tolerance ) Absorber_AD(k,j) = ZERO
        END DO

        DO k = n_ODPS_Layers, 1, -1
           Atm_AD%Absorber(PAFV%interp_index(1,k):PAFV%interp_index(2,k), Idx_map(j)) = &
              Atm_AD%Absorber(PAFV%interp_index(1,k):PAFV%interp_index(2,k), Idx_map(j)) &
            + PAFV%Acc_Weighting(PAFV%interp_index(1,k):PAFV%interp_index(2,k), k) &
            * Absorber_AD(k,j)
        END DO

      ELSE ! when the profile is missing, use the referece profile
        Absorber_AD(:, j) = ZERO
      END IF
    END DO

    !-----------------------------------------------------------
    ! Interpolate temperautre profile on internal pressure grids
    !-----------------------------------------------------------

    DO k = n_ODPS_Layers, 1, -1
      Atm_AD%Temperature(PAFV%interp_index(1,k):PAFV%interp_index(2,k)) = &
         Atm_AD%Temperature(PAFV%interp_index(1,k):PAFV%interp_index(2,k))  &
       + PAFV%Acc_Weighting(PAFV%interp_index(1,k):PAFV%interp_index(2,k), k) &
       * Temperature_AD(k)

    END DO

    Temperature_AD = ZERO
    Absorber_AD    = ZERO

  END SUBROUTINE Map_Input_AD

!--------------------------------------------------------------------------------
!S+
! NAME:
!       Geopotential_Height
!
! PURPOSE:
!       Routine to calculate geopotential height using the hypsometric
!       equation.
!
! CALLING SEQUENCE:
!       CALL Geopotential_Height( Level_Pressure,                                    &  ! Input
!                                 Temperature,                                       &  ! Input
!                                 Water_Vapor,                                       &  ! Input
!                                 Surface_Height,                                    &  ! input
!                                 Level_Height)
!
! INPUT ARGUMENTS:
!       Level_Pressure:            Level Pressures, which are the boundaries of of the
!                                  atmospheric layers for the Temperature and Water_vapor arrays
!                                  UNITS:      hectoPascals, hPa
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Rank-1 (0:n_layers)
!                                  ATTRIBUTES: INTENT(IN)
!
!       Temperature:               Layer Temperature.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Rank-1 (n_layers)
!                                  ATTRIBUTES: INTENT(IN)
!
!       Surface_Height:            Height of Level_Pressure(n_layer)
!                                  input arrays.
!                                  UNITS:      km.
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT(IN)
!
!
!       Water_Vapor:               Layer water vapor mixing radio
!                                  UNITS:      g/kg
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as Temperature
!                                  ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Level_Height:              Geopotential Heights of the input Pressure levels.
!                                  UNITS:      metres, m
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Same as input Pressure
!                                  ATTRIBUTES: INTENT(OUT)
!
!
!
! CREATION HISTORY:
!       Written by:     Yong Han, NOAA/NESDIS 5-Feb-2008
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Geopotential_Height( Level_Pressure,           &  ! Input
                                  Temperature,              &  ! Input
                                  Water_Vapor,              &  ! Input
                                  Surface_Height,           &  ! input
                                  Level_Height)                ! Output

    ! Arguments
    REAL(fp),               INTENT(IN)  :: Level_Pressure(0:)
    REAL(fp),               INTENT(IN)  :: Temperature(:)
    REAL(fp),               INTENT(IN)  :: Water_Vapor(:)
    REAL(fp),               INTENT(IN)  :: Surface_Height
    REAL(fp),               INTENT(OUT) :: Level_Height(0:)

    ! Function result
    REAL(fp) :: Tv, H
    INTEGER  :: k, n_Layers

    n_Layers =  SIZE(Temperature)
    Level_Height(n_layers) = Surface_Height
    DO k = n_Layers, 1, -1
      ! virtual temperature computed using an approximation of the exact formula:
      !  Tv = T*(1+w/epsilon)/(1+w), where w is the water vapor mixing ratio
      Tv = Temperature(k)*(ONE + C*Water_Vapor(k))
      H  = CC*Tv
      Level_Height(k-1) = Level_Height(k) + H*LOG(Level_Pressure(k)/Level_Pressure(k-1))
    END DO

  END SUBROUTINE Geopotential_Height


!---------------------------------------------------------------------------------------------
! NAME: Interpolate_Profile
!
! PURPOSE:
!    Given x and u that are ascending arrays, it interpolates y with the abscissa x
!    on the abscissa u using the following algorithm:
!       y_int(i) = y(1)  if u(i) < x(1)
!       y_int(i) = y(nx) if u(i) > x(nx)
!       y_int(i) = y(ix1) + (y(ix2)-y(ix1))*(u(i) - x(ix1))/(x(ix2)-x(ix1))
!                        if x(ix1) <= u(i) <= x(ix2)
!
!    IThe index array interp_index contains the following content
!
!      interp_index(1, i) = 1 and interp_index(2, i) = 1, if u(i) < x(1)
!      interp_index(1, i) = nx and interp_index(2, i) = nx, if u(i) > x(nx),
!                                                          where nx = SIZE(x)
!      x(interp_index(1, i)) <= u(i) <= x(interp_index(2, i)) if x(1) <= u(i) <= x(nx)
!
! CALLING SEQUENCE:
!            CALL Interpolate_Profile(interp_index, y, x, u, y_int)
!
! INPUT ARGUMENTS:
!       y:            The data array to be interpolated.
!                     UNITS:      N/A
!                     TYPE:       fp
!                     DIMENSION:  rank-1
!                     ATTRIBUTES: INTENT(IN)
!
!       x:            The abscissa values for y and they must be monotonically
!                     ascending.
!                     UNITS:      N/A
!                     TYPE:       fp
!                     DIMENSION:  rank-1
!                     ATTRIBUTES: INTENT(IN)
!
!       u:            The abscissa values for the results
!                     and they must be monotonically ascending
!                     UNITS:      N/A
!                     TYPE:       fp
!                     DIMENSION:  rank-1
!                     ATTRIBUTES: INTENT(IN)
!
!    interp_index:    The index array of dimension (nu x 2), where nu = SIZE(u)
!                     UNITS:      N/A
!                     TYPE:       Integer
!                     DIMENSION:  rank-2
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       y_int:        The array contains the results
!                     UNITS:      N/A
!                     TYPE:       Integer
!                     DIMENSION:  rank-1
!                     ATTRIBUTES: INTENT(OUT)
!
! RESTRICTIONS:
!     To be efficient, this routine does not check that x and u are both
!     monotonically ascending and the index bounds.
!
! CREATION HISTORY:
!       Written by:     Yong Han, 10-Dec-2008
!-----------------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !  Interpolation routine with interperlation index array already calculated.
  !----------------------------------------------------------------------------
  SUBROUTINE Interpolate_Profile(interp_index, y, x, u, y_int)
    ! Arguments
    INTEGER,  DIMENSION(:,:), INTENT(IN)  :: interp_index
    REAL(fp), DIMENSION(:),   INTENT(IN)  :: y
    REAL(fp), DIMENSION(:),   INTENT(IN)  :: x
    REAL(fp), DIMENSION(:),   INTENT(IN)  :: u
    REAL(fp), DIMENSION(:),   INTENT(OUT) :: y_int
    ! Local variables
    INTEGER :: i, n, k1, k2

    n = SIZE(interp_index, DIM=2)
    DO i = 1, n
      k1 = interp_index(1, i)
      k2 = interp_index(2, i)
      IF( k1 == k2)THEN
        y_int(i) = y(k1)
      ELSE
        CALL Interp_linear(y(k1), x(k1), y(k2), x(k2), u(i), y_int(i))
      END IF
    END DO

  END SUBROUTINE Interpolate_Profile


!---------------------------------------------------------------------------------------------
! NAME: Interpolate_Profile_TL
!
! PURPOSE:
!     The Tangent_Linear routine of Interpolate_Profile
! CALLING SEQUENCE:
!           CALL Interpolate_Profile_F1_TL(interp_index, y, x, u, y_TL, y_int_TL)
!      OR   CALL Interpolate_Profile_F2_TL(interp_index, y, x, u, y_TL, u_TL, y_int_TL)
!      OR   CALL Interpolate_Profile_F3_TL(interp_index, y, x, u, y_TL, x_TL, y_int_TL)
!
! INPUT ARGUMENTS:
!       y:            The data array to be interpolated.
!                     UNITS:      N/A
!                     TYPE:       fp
!                     DIMENSION:  rank-1
!                     ATTRIBUTES: INTENT(IN)
!
!       x:            The abscissa values for y and they must be monotonically
!                     ascending.
!                     UNITS:      N/A
!                     TYPE:       fp
!                     DIMENSION:  rank-1
!                     ATTRIBUTES: INTENT(IN)
!
!       u:            The abscissa values for the results
!                     and they must be monotonically ascending
!                     UNITS:      N/A
!                     TYPE:       fp
!                     DIMENSION:  rank-1
!                     ATTRIBUTES: INTENT(IN)
!
!       y_TL:         The Tangent-linear data array of y
!                     UNITS:      N/A
!                     TYPE:       fp
!                     DIMENSION:  rank-1
!                     ATTRIBUTES: INTENT(IN)
!
!       u_TL:         The Tangent-linear data array of u
!                     UNITS:      N/A
!                     TYPE:       fp
!                     DIMENSION:  rank-1
!                     ATTRIBUTES: INTENT(IN)
!
!       x_TL:         The Tangent-linear data array of x
!                     UNITS:      N/A
!                     TYPE:       fp
!                     DIMENSION:  rank-1
!                     ATTRIBUTES: INTENT(IN)
!
!    interp_index:    The index array of dimension (nu x 2), where nu = SIZE(u)
!                     UNITS:      N/A
!                     TYPE:       Integer
!                     DIMENSION:  rank-2
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       y_int_TL:     The Tangent-linear array of y_int
!                     UNITS:      N/A
!                     TYPE:       Integer
!                     DIMENSION:  rank-1
!                     ATTRIBUTES: INTENT(OUT)
!
! RESTRICTIONS:
!     To be efficient, this routine does not check that x and u are both
!     monotonically ascending and the index bounds.
!
! CREATION HISTORY:
!       Written by:     Yong Han, 10-Dec-2008
!-----------------------------------------------------------------------------------

  SUBROUTINE Interpolate_Profile_F1_TL(interp_index, x, u, y_TL, y_int_TL)
    ! Arguments
    INTEGER,  DIMENSION(:,:), INTENT(IN)  :: interp_index
    REAL(fp), DIMENSION(:),   INTENT(IN)  :: x
    REAL(fp), DIMENSION(:),   INTENT(IN)  :: u
    REAL(fp), DIMENSION(:),   INTENT(IN)  :: y_TL
    REAL(fp), DIMENSION(:),   INTENT(OUT) :: y_int_TL
    ! Local variables
    INTEGER :: i, n, k1, k2

    n = SIZE(interp_index, DIM=2)
    DO i = 1, n
      k1 = interp_index(1, i)
      k2 = interp_index(2, i)
      IF( k1 == k2)THEN
        y_int_TL(i) = y_TL(k1)
      ELSE
        CALL Interp_linear_F1_TL(x(k1), x(k2), u(i), y_TL(k1), y_TL(k2), y_int_TL(i))
      END IF
    END DO

  END SUBROUTINE Interpolate_Profile_F1_TL


!---------------------------------------------------------------------------------------------
! NAME: Interpolate_Profile_AD
!
! PURPOSE:
!     The Adjoint routine of Interpolate_Profile
!
! CALLING SEQUENCE:
!            CALL Interpolate_Profile_F1_AD(interp_index, y, x, u, y_int_AD, y_AD)
!    OR      CALL Interpolate_Profile_F2_AD(interp_index, y, x, u, y_int_AD, y_AD, u_AD)
!    OR      CALL Interpolate_Profile_F3_AD(interp_index, y, x, u, y_int_AD, y_AD, x_AD)
!
! INPUT ARGUMENTS:
!       y:            The data array to be interpolated.
!                     UNITS:      N/A
!                     TYPE:       fp
!                     DIMENSION:  rank-1
!                     ATTRIBUTES: INTENT(IN)
!
!       x:            The abscissa values for y and they must be monotonically
!                     ascending.
!                     UNITS:      N/A
!                     TYPE:       fp
!                     DIMENSION:  rank-1
!                     ATTRIBUTES: INTENT(IN)
!
!       u:            The abscissa values for the results
!                     and they must be monotonically ascending
!                     UNITS:      N/A
!                     TYPE:       fp
!                     DIMENSION:  rank-1
!                     ATTRIBUTES: INTENT(IN)
!
!       y_int_AD:     The Adjoint array of y_int
!                     UNITS:      N/A
!                     TYPE:       Integer
!                     DIMENSION:  rank-1
!                     ATTRIBUTES: INTENT(IN)
!
!    interp_index:    The index array of dimension (nu x 2), where nu = SIZE(u)
!                     UNITS:      N/A
!                     TYPE:       Integer
!                     DIMENSION:  rank-2
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       y_AD:         The Adjoint data array of y
!                     UNITS:      N/A
!                     TYPE:       fp
!                     DIMENSION:  rank-1
!                     ATTRIBUTES: INTENT(IN)
!
!       u_AD:         The Adjoint data array of u
!                     UNITS:      N/A
!                     TYPE:       fp
!                     DIMENSION:  rank-1
!                     ATTRIBUTES: INTENT(IN)
!
!       x_AD:         The Adjoint data array of x
!                     UNITS:      N/A
!                     TYPE:       fp
!                     DIMENSION:  rank-1
!                     ATTRIBUTES: INTENT(IN)
!
! RESTRICTIONS:
!     To be efficient, this routine does not check that x and u are both
!     monotonically ascending and the index bounds.
!
! CREATION HISTORY:
!       Written by:     Yong Han, 10-Dec-2008
!-----------------------------------------------------------------------------------

  SUBROUTINE Interpolate_Profile_F1_AD(interp_index, x, u, y_int_AD, y_AD)
    ! Arguments
    INTEGER,  DIMENSION(:,:), INTENT(IN)      :: interp_index
    REAL(fp), DIMENSION(:),   INTENT(IN)      :: x
    REAL(fp), DIMENSION(:),   INTENT(IN)      :: u
    REAL(fp), DIMENSION(:),   INTENT(IN OUT)  :: y_int_AD
    REAL(fp), DIMENSION(:),   INTENT(IN OUT)  :: y_AD
    ! Local variables
    INTEGER :: i, n, k1, k2

    n = SIZE(interp_index, DIM=2)
    DO i = n, 1, -1
      k1 = interp_index(1, i)
      k2 = interp_index(2, i)
      IF( k1 == k2)THEN
        y_AD(k1) = y_AD(k1) + y_int_AD(i)
        y_int_AD(i) = ZERO
      ELSE
        CALL Interp_linear_F1_AD(x(k1), x(k2), u(i), y_int_AD(i), y_AD(k1), y_AD(k2))
      END IF
    END DO

  END SUBROUTINE Interpolate_Profile_F1_AD


!---------------------------------------------------------------------------------------------
! NAME: Compute_Interp_Index
!
! PURPOSE:
!    Given x and u that are ascending arrays, it computes an index array, interp_index,
!    such that
!
!      interp_index(i, 1) = 1 and interp_index(i, 2) = 1, if u(i) < x(1)
!      interp_index(i, 1) = nx and interp_index(i, 2) = nx, if u(i) > x(nx),
!                                                          where nx = SIZE(x)
!      x(interp_index(i, 1)) <= u(i) <= x(interp_index(i, 2)) if x(1) <= u(i) <= x(nx)
!
! CALLING SEQUENCE:
!            CALL Compute_Interp_Index(x, u, interp_index)
!
! INPUT ARGUMENTS:
!       x:            The abscissa values for the data to be interpolated and
!                     they must be monotonically ascending.
!                     UNITS:      N/A
!                     TYPE:       fp
!                     DIMENSION:  rank-1
!                     ATTRIBUTES: INTENT(IN)
!
!       u:            The abscissa values on which the data are interpolated
!                     they must be monotonically ascending
!                     the elements of array x.
!                     UNITS:      N/A
!                     TYPE:       fp
!                     DIMENSION:  rank-1
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       interp_index: The index array of dimension (nu x 2), where nu = SIZE(u)
!                     UNITS:      N/A
!                     TYPE:       Integer
!                     DIMENSION:  rank-2
!                     ATTRIBUTES: INTENT(OUT)
!
! RESTRICTIONS:
!     To be efficient, this routine does not check that x and u are both
!     monotonically ascending and the index bounds.
!
! CREATION HISTORY:
!       Written by:     Yong Han, 07-May-2004
!-----------------------------------------------------------------------------------

  SUBROUTINE Compute_Interp_Index(x, u, interp_index)
    ! Arguments
    REAL(fp), DIMENSION(:),   INTENT(IN)  :: x
    REAL(fp), DIMENSION(:),   INTENT(IN)  :: u
    INTEGER,  DIMENSION(:,:), INTENT(OUT) :: interp_index
    ! Local variables
    INTEGER :: nx, nu, ix, iu, j, k1, k2

    nx = SIZE(x)
    nu = SIZE(u)

    ! Set the indexes to 1 for the elements in u that are smaller than x(1)
    k1 = nu + 1
    LessThan_Loop: DO iu = 1, nu
      IF(u(iu) < x(1))THEN
        interp_index(1, iu) = 1
        interp_index(2, iu) = 1
      ELSE
        k1 = iu
        EXIT LessThan_Loop
      END IF
    END DO LessThan_Loop

    ! Set the indexes to nx for the elements in u that are larger than x(nx)
    k2 = 0
    GreaterThan_Loop: DO iu = nu, k1, -1
      IF(u(iu) > x(nx))THEN
        interp_index(1, iu) = nx
        interp_index(2, iu) = nx
      ELSE
        k2 = iu
        EXIT GreaterThan_Loop
      END IF
    END DO GreaterThan_Loop

    ! Set the indexes for the elements in u that are in the range
    ! between x1(1) and x(nx)
    j = 1
    Outer_Loop: DO iu = k1, k2
      Inner_Loop: DO ix = j, nx-1
        IF(u(iu) >= x(ix) .AND. u(iu) <= x(ix+1))THEN
          interp_index(1, iu) = ix
          interp_index(2, iu) = ix+1
          j = ix
          EXIT Inner_Loop
        ENDIF
      END DO Inner_Loop
    END DO Outer_Loop

  END SUBROUTINE Compute_Interp_Index


  !---------------------------------------------
  ! Function for two points linear interpolation
  !---------------------------------------------
  SUBROUTINE Interp_linear(y1, x1, y2, x2, x, y)
    REAL(fp), INTENT(IN)  :: y1, x1, y2, x2, x
    REAL(fp), INTENT(OUT) :: y
    y = y1 + (y2-y1)*(x - x1)/(x2 - x1)
  END SUBROUTINE Interp_linear


  SUBROUTINE Interp_linear_F1_TL(x1, x2, x, y1_TL, y2_TL, y_TL)
    REAL(fp), INTENT(IN)  :: x1, x2, x, y1_TL, y2_TL
    REAL(fp), INTENT(OUT) :: y_TL
    y_TL = y1_TL + (y2_TL-y1_TL)*(x - x1)/(x2 - x1)
  END SUBROUTINE Interp_linear_F1_TL


  SUBROUTINE Interp_linear_F1_AD(x1, x2, x, y_AD, y1_AD, y2_AD)
    REAL(fp), INTENT(IN)     :: x1, x2, x
    REAL(fp), INTENT(IN OUT) :: y_AD
    REAL(fp), INTENT(IN OUT) :: y1_AD, y2_AD
    ! Local variables
    REAL(fp) :: fac
    fac = (x - x1)/(x2 - x1)
    y1_AD = y1_AD + (ONE - fac)*y_AD
    y2_AD = y2_AD + fac*y_AD
    y_AD = ZERO
  END SUBROUTINE Interp_linear_F1_AD


!--------------------------------------------------------------------------------
!
! NAME:
!       LayerAvg
!
! PURPOSE:
!    Given px1 (output domain) and px2 (input domain) that are ascending arrays,
!    it computes the accumulated weighting factors for interpolation from input
!    to output domainsan, and the index array, interp_index, such that output
!    variable
!      to(jo) = sum(pz(interp_index(1,jo):interp_index(2,jo),jo) &
!                   *(ti(interp_index(1,jo):interp_index(2,jo))))

!
! CALLING SEQUENCE:
!       CALL LayerAvg(PX1,PX2,PZ,Interp_index)
!
! INPUT ARGUMENTS:
!       PX1:          The abscissa values for the target data (output domain) and
!                     they must be monotonically ascending (e.g. lnP; in increasing values).
!                     UNITS:      N/A
!                     TYPE:       fp
!                     DIMENSION:  rank-1 (KN1)
!                     ATTRIBUTES: INTENT(IN)
!       PX2:          The abscissa values for the source data (input domain) and
!                     they must be monotonically ascending (e.g. lnP; in increasing values).
!                     UNITS:      N/A
!                     TYPE:       fp
!                     DIMENSION:  rank-1 (KN2)
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       PZ:           Resultant accumulated weighting factors for
!                     interpolation from input to output domains.
!                     UNITS:      N/A
!                     TYPE:       fp
!                     DIMENSION:  rank-2 (KN2,KN1)
!                     ATTRIBUTES: INTENT(OUT)
!    interp_index:    The index array of dimension (2 x KN1) for
!                     Start index for relevant PZ row array segment and
!                     End index for relevant PZ row array segment, where KN1 = SIZE(PX1)
!                     UNITS:      N/A
!                     TYPE:       Integer
!                     DIMENSION:  rank-2
!                     ATTRIBUTES: INTENT(IN)
!
!     Comments:
!
!     1) PX1(i)<PX1(i+1) & PX2(i)<PX2(i+1)
!
! RESTRICTIONS:
!     To be efficient, this routine does not check that x and u are both
!     monotonically ascending and the index bounds.
!
!     - Journal reference:
!       Rochon, Y.J., L. Garand, D.S. Turner, and S. Polavarapu.
!       Jacobian mapping between vertical coordinate systems in data assimilation,
!       Q. J. of the Royal Met. Soc., 133, 1547-1558 (2007) DOI: 10.1002/qj.117
!
! CREATION HISTORY:
!       Written by:     Yong Chen, 08-May-2009
!-----------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
  SUBROUTINE LayerAvg( PX1,PX2,PZ,interp_index)
    REAL(fp),     DIMENSION(:),       INTENT(IN)     :: PX1(:)
    REAL(fp),     DIMENSION(:),       INTENT(IN)     :: PX2(:)
    REAL(fp),     DIMENSION(:,:),     INTENT(OUT)    :: PZ(:, :)
    INTEGER,      DIMENSION(:,:),     INTENT(OUT)    :: interp_index

    ! Local variables
    INTEGER  ::  KN1,KN2, ibot,itop,ii,istart,iend, J,IC,ISKIP,KI
    REAL(fp) ::  z1,z2,z3,zw1,zw2,zsum
    REAL(fp) ::  y1,y2,d,w10,w20,dz,dx,dy,dzd,dxd

    KN1 = SIZE(PX1)
    KN2 = SIZE(PX2)

    istart=1
    iend=kn1
    DO KI=1,KN1
      z2=px1(ki)
!
      if (ki == 1) then
         z1=2.0*z2-px1(ki+1)
      else
         z1=px1(ki-1)
      endif
!
      if (ki == kn1) then
         z3=2.0*z2-z1
      else
         z3=px1(ki+1)
      endif
      if (z3 > px2(kn2)) z3=px2(kn2)
!
      iskip=0
      if (z2 >= px2(kn2)) then
         z3=px2(kn2)
         z2=px2(kn2)
         iskip=1
      endif

! --- Determine forward interpolator
!
      pz(1:kn2,ki)=ZERO
      ic=0
      do j=istart,kn2-1
        if (px2(j) > z3) go to 1000
!
        if (px2(j) <= z2 .and. px2(j+1) > z1) then
          itop=0
          ibot=0
          if (z1 < z3) then
             y1=z1
             if (px2(j) > z1) then
                y1=px2(j)
                itop=1
             endif
             y2=z2
             if (px2(j+1) < z2) then
                y2=px2(j+1)
                ibot=1
             endif
          else
             y1=z2
             if (px2(j) > z2) then
                y1=px2(j)
                itop=1
             endif
             y2=z1
             if (px2(j+1) < z1) then
                y2=px2(j+1)
                ibot=1
             endif
          endif
!
! ---     Set weights for forward interpolator
!
          dy=y2-y1
          dz=z1-z2
          if (abs(dz) < SMALLDIFF) then
             write(6,*) 'SUBLAYER: ERROR: dz is <=0. dz = ',dz
             write(6,*) 'z1,z2,z3 = ',z1,z2,z3
             write(6,*) 'px2(j),px2(j+1)    = ',px2(j),px2(j+1)
             return
          else
             dzd=ONE/dz
          endif
          zw1=(z1-y1)*dzd*dy
          zw2=(z1-y2)*dzd*dy
          w10=zw1
          w20=zw2
          dx=(px2(j+1)-px2(j))
          if (abs(dx) < SMALLDIFF) then
             write(6,*) 'SUBLAYER: ERROR: dx is <=0. dx = ',dx
             write(6,*) 'z1,z2,z3 = ',z1,z2,z3
             write(6,*) 'px2(j),px2(j+1)    = ',px2(j),px2(j+1)
             return
          else
             dxd=ONE/dx
          endif
!
          d=(px2(j+1)-z2)*dxd
          if (z1 < z3 .and. ibot == 0) then
             zw1=zw1+zw2*d
             zw2=zw2*(ONE-d)
          else if (z1 > z3 .and. itop == 0) then
             zw2=zw2+zw1*(ONE-d)
             zw1=zw1*d
          end if
          pz(j,ki)=pz(j,ki)+zw1
          pz(j+1,ki)=pz(j+1,ki)+zw2
          ic=1
        endif
!
        if (px2(j) < z3 .and. px2(j+1) >= z2 .and. iskip == 0) then
          itop=0
          ibot=0
          if (z3 < z1) then
             y1=z3
             if (px2(j) > z3) then
                y1=px2(j)
                itop=1
             endif
             y2=z2
             if (px2(j+1) < z2) then
                y2=px2(j+1)
                ibot=1
             endif
          else
             y1=z2
             if (px2(j) > z2) then
                y1=px2(j)
                itop=1
             endif
             y2=z3
             if (px2(j+1) < z3) then
                y2=px2(j+1)
                ibot=1
             endif
          endif
!
! ---     Set weights for forward interpolator
!
          dy=y2-y1
          dz=z3-z2
          if (abs(dz) < SMALLDIFF) then
             write(6,*) 'SUBLAYER: ERROR: dz is <=0. dz = ',dz
             write(6,*) 'z3,z2,z1 = ',z3,z2,z1
             write(6,*) 'px2(j),px2(j+1)    = ',px2(j),px2(j+1)
             return
          else
             dzd=ONE/dz
          endif
          zw1=(z3-y1)*dzd*dy
          zw2=(z3-y2)*dzd*dy
          w10=zw1
          w20=zw2
          dx=(px2(j+1)-px2(j))
          if (abs(dx) < SMALLDIFF) then
             write(6,*) 'SUBLAYER: ERROR: dx is <=0. dx = ',dx
             write(6,*) 'z3,z2,z1 = ',z3,z2,z1
             write(6,*) 'px2(j),px2(j+1)    = ',px2(j),px2(j+1)
             return
          else
             dxd=ONE/dx
          endif
!
          d=(px2(j+1)-z2)*dxd
          if (z3 < z1 .and. ibot == 0) then
             zw1=zw1+zw2*d
             zw2=zw2*(ONE-d)
          else if (z3 > z1 .and. itop == 0) then
             zw2=zw2+zw1*(ONE-d)
             zw1=zw1*d
          end if
          pz(j,ki)=pz(j,ki)+zw1
          pz(j+1,ki)=pz(j+1,ki)+zw2
          ic=1
        endif
      enddo
      j=kn2
 1000 continue
      if (ic == 0) pz(j,ki)=ONE
!
!     Normalize sum to unity (instead of calculating and dividing by
!     weighting denominator)
!
      do ii=istart,kn2
        if(PZ(ii,ki) /= ZERO)then
          interp_index(1, ki)=ii
          exit
        endif
      enddo
      istart=interp_index(1, ki)

      interp_index(2,ki)=kn2

      do ii=interp_index(1, ki)+1,kn2
        if(PZ(ii,ki) == ZERO)then
          interp_index(2,ki)=ii-1
          exit
        endif
      enddo
      iend=interp_index(2,ki)

      zsum=sum(pz(interp_index(1, ki):interp_index(2,ki),ki))
      pz(interp_index(1,ki):interp_index(2,ki),ki)= &
                 pz(interp_index(1,ki):interp_index(2, ki),ki)/zsum
    ENDDO


  END SUBROUTINE LayerAvg

END MODULE ODPS_CoordinateMapping
