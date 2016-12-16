!
! ODZeeman_AtmAbsorption
!
! Module containing routines to compute the optical depth profile
! due to gaseous absorption affected by Zeeman spilitting in
! the Optical Depth Pressure Space (ODPS).
!
!
! CREATION HISTORY:
!       Written by:     Yong Han, 10-Nov-2009
!                       yong.han@noaa.gov
!

MODULE ODZeeman_AtmAbsorption

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,                ONLY: fp
  USE Message_Handler,           ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters,           ONLY: ZERO, ONE
  USE CRTM_Atmosphere_Define,    ONLY: CRTM_Atmosphere_type, H2O_ID
  USE CRTM_AtmOptics_Define,     ONLY: CRTM_AtmOptics_type
  USE CRTM_GeometryInfo_Define,  ONLY: CRTM_GeometryInfo_type, &
                                       CRTM_GeometryInfo_GetValue
  USE ODPS_Predictor_Define,     ONLY: ODPS_Predictor_type, &
                                       PAFV_Associated
  USE ODPS_Define,               ONLY: ODPS_type
  USE ODZeeman_Predictor,        ONLY: Compute_Predictors_zssmis,     &
                                       Compute_Predictors_zssmis_TL,  &
                                       Compute_Predictors_zssmis_AD,  &
                                       Compute_Predictors_zamsua,     &
                                       Compute_Predictors_zamsua_TL,  &
                                       Compute_Predictors_zamsua_AD,  &
                                       ZSSMIS_ChannelMap,             &
                                       ZAMSUA_ChannelMap,             &
                                       ODPS_gINDEX_ZSSMIS,            &
                                       ODPS_gINDEX_ZAMSUA,            &
                                       N_ZCOMPONENTS,                 &
                                       N_ZABSORBERS,                  &
                                       MAX_N_PREDICTORS_ZSSMIS,       &
                                       MAX_N_PREDICTORS_ZAMSUA
  USE Zeeman_Input_Define,       ONLY: Zeeman_Input_type, &
                                       Zeeman_Input_GetValue
  USE ODPS_CoordinateMapping,    ONLY: Map_Input, Map_Input_TL, Map_Input_AD, &
                                       Interpolate_Profile,    &
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
  ! Public routines
  PUBLIC :: Zeeman_Compute_Predictors
  PUBLIC :: Zeeman_Compute_Predictors_TL
  PUBLIC :: Zeeman_Compute_Predictors_AD
  PUBLIC :: Zeeman_Compute_AtmAbsorption
  PUBLIC :: Zeeman_Compute_AtmAbsorption_TL
  PUBLIC :: Zeeman_Compute_AtmAbsorption_AD
  PUBLIC :: Is_Zeeman_Channel
  PUBLIC :: Is_ODZeeman
  PUBLIC :: Get_NumOfZPredictors
  PUBLIC :: Get_NumOfZComponents
  PUBLIC :: Get_NumOfZAbsorbers


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: ODZeeman_AtmAbsorption.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'


CONTAINS


!------------------------------------------------------------------------------
!
! NAME:
!      Zeeman_Compute_AtmAbsorption
!
! PURPOSE:
!       Subroutine to compute slant path optical path for channels affected by
!       affected by Zeeman splitting
!
! CALLING SEQUENCE:
!
!    SUBROUTINE Zeeman_Compute_AtmAbsorption(TC,            &
!                                            ChannelIndex,  &
!                                            Predictor,     &
!                                            AtmOptics )
!
! INPUT ARGUMENTS:
!
!            TC:         ODPS structure holding coefficient data
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!      ChannelIndex:     Channel index (a sequential number for the channels in the structure TC)
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Predictor structure containing the predictors for estimating of optical depth
!                        UNITS:      N/A
!                        TYPE:       ODPS_Predictor_type
!                        DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!   OUTPUT ARGUMENTS:
!        AtmOptics:  Structure containing computed optical depth
!                        profile data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_AtmOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------
  SUBROUTINE Zeeman_Compute_AtmAbsorption(TC           , &  ! Input
                                          ChannelIndex , &  ! Input
                                          Predictor    , &  ! Input
                                          AtmOptics)    ! Output

    ! Arguments
    TYPE(ODPS_type)          , INTENT(IN)     :: TC
    INTEGER                  , INTENT(IN)     :: ChannelIndex
    TYPE(ODPS_Predictor_type), INTENT(IN OUT) :: Predictor
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: AtmOptics
    ! Local variables
    INTEGER  :: n_User_Layers
    REAL(fp) :: OD_Path(0:Predictor%n_Layers)
    REAL(fp) :: User_OD_Path(0:Predictor%n_User_Layers)
    INTEGER  :: ODPS2User_Idx(2, 0:Predictor%n_User_Layers)
    INTEGER  :: idx

    n_User_Layers = Predictor%n_User_Layers

    IF(TC%Group_Index == ODPS_gINDEX_ZSSMIS)THEN
      idx = ZSSMIS_ChannelMap(ChannelIndex)
      CALL Compute_ODPath_zssmis(idx,        &
                                 TC,         &
                                 Predictor,  &
                                 OD_Path)
    ELSE
      CALL Compute_ODPath_zamsua(TC,         &
                                 Predictor,  &
                                 OD_Path)
    END IF

    ! Interpolate the path profile back on the user pressure grids,
    ! Compute layer optical depths (vertical direction)

    IF ( PAFV_Associated(Predictor%PAFV) ) THEN
      ! save forwad variables
      Predictor%PAFV%OD_Path = OD_Path
      ! If interpolation indexes are known
      User_OD_Path(0) = ZERO
      CALL Interpolate_Profile(Predictor%PAFV%ODPS2User_Idx,    &
                               OD_Path,                         &
                               Predictor%Ref_Level_LnPressure,  &
                               Predictor%User_Level_LnPressure, &
                               User_OD_Path)
    ELSE ! interpolation indexes are not known

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
    AtmOptics%Optical_Depth = (User_OD_Path(1:n_User_Layers) - &
                                   User_OD_Path(0:n_User_Layers-1)) / &
                                   Predictor%Secant_Zenith_Surface

  END SUBROUTINE Zeeman_Compute_AtmAbsorption

!------------------------------------------------------------------------------
!
! NAME:
!      Zeeman_Compute_AtmAbsorption_TL
!
! PURPOSE:
!       Subroutine to compute TL slant path optical path for channels affected by
!       affected by Zeeman splitting
!
! CALLING SEQUENCE:
!
!        CALL Zeeman_Compute_AtmAbsorption_TL(TC,            &
!                                             ChannelIndex,  &
!                                             Predictor,     &
!                                             Predictor_TL,  &
!                                             AtmOptics_TL )
!
! INPUT ARGUMENTS:
!
!            TC:         ODPS structure holding coefficient data
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!      ChannelIndex:     Channel index (a sequential number for the channels in the structure TC)
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Predictor structure containing the predictors for estimating of optical depth
!                        UNITS:      N/A
!                        TYPE:       ODPS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor_TL:    Predictor structure containing the TL predictors
!                        UNITS:      N/A
!                        TYPE:       ODPS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(INOUT)
!
!   OUTPUT ARGUMENTS:
!      AtmOptics_TL: Structure containing computed TL optical depth
!                        profile data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_AtmOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------
  SUBROUTINE Zeeman_Compute_AtmAbsorption_TL(TC           ,    &  ! Input
                                             ChannelIndex ,    &  ! Input
                                             Predictor    ,    &  ! Input
                                             Predictor_TL,     &  ! Input
                                             AtmOptics_TL)    ! Output
    ! Arguments
    TYPE(ODPS_type)          , INTENT(IN)     :: TC
    INTEGER                  , INTENT(IN)     :: ChannelIndex
    TYPE(ODPS_Predictor_type), INTENT(IN)     :: Predictor
    TYPE(ODPS_Predictor_type), INTENT(INOUT)  :: Predictor_TL
    TYPE(CRTM_AtmOptics_type), INTENT(INOUT)  :: AtmOptics_TL
    ! Local variables
    INTEGER  :: n_User_Layers
    REAL(fp) :: OD_Path_TL(0:Predictor%n_Layers)
    REAL(fp) :: User_OD_Path_TL(0:Predictor%n_User_Layers)
    INTEGER  :: idx

    n_User_Layers = Predictor%n_User_Layers

    IF(TC%Group_Index == ODPS_gINDEX_ZSSMIS)THEN
      idx = ZSSMIS_ChannelMap(ChannelIndex)
      CALL Compute_ODPath_zssmis_TL(idx,             &
                                    TC,              &
                                    Predictor,       &
                                    Predictor_TL,    &
                                    OD_Path_TL )
    ELSE
      CALL Compute_ODPath_zamsua_TL(TC,              &
                                    Predictor,       &
                                    Predictor_TL,    &
                                    OD_Path_TL )
    END IF

    ! Interpolate the path profile back on the user pressure grids,
    ! Compute layer optical depths (vertical direction)
    CALL Interpolate_Profile_F1_TL(Predictor%PAFV%ODPS2User_Idx,    &
                                   Predictor%Ref_Level_LnPressure,  &
                                   Predictor%User_Level_LnPressure, &
                                   OD_Path_TL,                      &
                                   User_OD_Path_TL)

    AtmOptics_TL%Optical_Depth = (User_OD_Path_TL(1:n_User_Layers) - &
                                   User_OD_Path_TL(0:n_User_Layers-1)) / &
                                   Predictor%Secant_Zenith_Surface

  END SUBROUTINE Zeeman_Compute_AtmAbsorption_TL

!------------------------------------------------------------------------------
!
! NAME:
!     Zeeman_Compute_AtmAbsorption_AD
!
! PURPOSE:
!       Subroutine to compute AD slant path optical path for channels affected by
!       affected by Zeeman splitting
!
! CALLING SEQUENCE:
!
!        CALL Zeeman_Compute_AtmAbsorption_AD(TC,           &
!                                      ChannelIndex,        &
!                                      Predictor,           &
!                                      AtmOptics_AD,    &
!                                      Predictor_AD)
!
! INPUT ARGUMENTS:
!
!            TC:         ODPS structure holding coefficient data
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!      ChannelIndex:     Channel index (a sequential number for the channels in the structure TC)
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Predictor structure containing the predictors for estimating of optical depth
!                        UNITS:      N/A
!                        TYPE:       ODPS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!         OD_Path_AD:    AD Slant path optical path profile (from space down)
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1 (0:n_Layers)
!                        ATTRIBUTES: INTENT(INOUT)
!
!   OUTPUT ARGUMENTS:
!      AtmOptics_AD: Structure containing computed AD optical depth
!                        profile data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_AtmOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!------------------------------------------------------------------------------
  SUBROUTINE Zeeman_Compute_AtmAbsorption_AD( TC           ,    &  ! Input
                                              ChannelIndex ,    &  ! Input
                                              Predictor    ,    &  ! Input
                                              AtmOptics_AD, &  ! Input
                                              Predictor_AD)        ! Output
    ! Arguments
    TYPE(ODPS_type)          , INTENT(IN)     :: TC
    INTEGER                  , INTENT(IN)     :: ChannelIndex
    TYPE(ODPS_Predictor_type), INTENT(IN)     :: Predictor
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: AtmOptics_AD
    TYPE(ODPS_Predictor_type), INTENT(IN OUT) :: Predictor_AD
    ! Local variables
    INTEGER  :: n_User_Layers, k
    REAL(fp) :: OD_Path_AD(0:Predictor%n_Layers)
    REAL(fp) :: User_OD_Path_AD(0:Predictor%n_User_Layers)
    INTEGER  :: idx

    n_User_Layers = Predictor%n_User_Layers

     !------- Adjoint part ---------

    ! Interpolate the path profile back on the user pressure grids,
    ! Compute layer optical depths (vertical direction)
    User_OD_Path_AD(n_User_Layers) = ZERO
    DO k = n_User_Layers, 1, -1
      User_OD_Path_AD(k) = User_OD_Path_AD(k) &
                           + AtmOptics_AD%Optical_Depth(k)/Predictor%Secant_Zenith_Surface
      ! combined with initilization
      User_OD_Path_AD(k-1) = -AtmOptics_AD%Optical_Depth(k)/Predictor%Secant_Zenith_Surface
    END DO
    AtmOptics_AD%Optical_Depth = ZERO

    OD_Path_AD = ZERO
    CALL Interpolate_Profile_F1_AD(Predictor%PAFV%ODPS2User_Idx,       &
                                   Predictor%Ref_Level_LnPressure,     &
                                   Predictor%User_Level_LnPressure,    &
                                   User_OD_Path_AD,                    &
                                   OD_Path_AD )


    User_OD_Path_AD(0) = ZERO

    IF(TC%Group_Index == ODPS_gINDEX_ZSSMIS)THEN
      idx = ZSSMIS_ChannelMap(ChannelIndex)
      CALL Compute_ODPath_zssmis_AD(idx,          &
                                    TC,           &
                                    Predictor,    &
                                    OD_Path_AD,   &
                                    Predictor_AD )
    ELSE
      CALL Compute_ODPath_zamsua_AD(TC,           &
                                    Predictor,    &
                                    OD_Path_AD,   &
                                    Predictor_AD )
    END IF

  END SUBROUTINE Zeeman_Compute_AtmAbsorption_AD

!------------------------------------------------------------------------------
!
! NAME:
!      Compute_ODPath_zssmis
!
! PURPOSE:
!       Subroutine to compute slant path optical path for ZSSMIS
!       (a virtual sensor created for the SSMIS 4 Zeeman channels:
!       ch 19, 20, 21, 22)
!
! CALLING SEQUENCE:
!
!    SUBROUTINE Compute_ODPath_zssmis(ChannelIndex,  &
!                                     TC,            &
!                                     Predictor,     &
!                                     OD_Path )
!
! INPUT ARGUMENTS:
!
!      ChannelIndex:     Channel index (a sequential number for the channels in the structure TC)
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!            TC:         ODPS structure holding coefficient data
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Predictor structure containing the predictors for estimating of optical depth
!                        UNITS:      N/A
!                        TYPE:       ODPS_Predictor_type
!                        DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!   OUTPUT ARGUMENTS:
!         OD_Path:      Slant path optical path profile (from space down)
!                       UNITS:      N/A
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1 (0:n_Layers)
!                       ATTRIBUTES: INTENT(OUT)
!
!------------------------------------------------------------------------------

  SUBROUTINE Compute_ODPath_zssmis(ChannelIndex,  &
                                   TC,            &
                                   Predictor,     &
                                   OD_Path )
    INTEGER,                   INTENT( IN )     :: ChannelIndex
    TYPE(ODPS_type),           INTENT( IN )     :: TC
    TYPE(ODPS_Predictor_type), INTENT( INOUT )  :: Predictor
    REAL(fp),                  INTENT( OUT)     :: OD_Path(0:)

    ! Local
    REAL(fp), DIMENSION(Predictor%n_Layers) :: OD1, OD2, OD
    REAL(fp) :: OD_tmp
    REAL(fp) :: w1, w2, Doppler_shift
    INTEGER  :: i, j, j1, j2, js1, js2, k, inode, n_nodes, n_Layers, nc, np

    OD_Path = ZERO
    np = TC%n_Predictors(1, ChannelIndex)

    ! Check if there is any absorption for the component&channel combination.
    IF( np > 0 ) THEN

      !----------------------------------------------------------------------------------
      ! (1) Find the nodes of the Doppler shift frequencies, which bracket the user
      ! Doppler frequency; (2) compute weights for interpolation.
      !----------------------------------------------------------------------------------
      Doppler_Shift = Predictor%u
      j = TC%Pos_Index(1, ChannelIndex)

      n_nodes = INT(TC%C(j))

      n_Layers = Predictor%n_Layers
      j = j + 1
      IF(Doppler_Shift < TC%C(j))THEN
        j1 = j
        w1 = ONE
        w2 = ZERO
        inode = 1
      ELSE IF(Doppler_Shift > TC%C(j+n_nodes-1))THEN
        j1 = j+n_nodes-2
        w1 = ZERO
        w2 = ONE
        inode = n_nodes-2
      ELSE
        DO i = 1, n_nodes-1
          j1 = j + i - 1
          j2 = j1 + 1
          IF(Doppler_Shift >= TC%C(j1) .AND. Doppler_Shift <= TC%C(j2))THEN
            w1 = (TC%C(j2) - Doppler_Shift)/(TC%C(j2) - TC%C(j1))
            w2 = ONE - w1
            inode = i
            EXIT
          END IF
        END DO
      END IF

      !--------------------------------------------
      ! Compute optical depths at the two nodes
      !--------------------------------------------
      OD1 = ZERO
      OD2 = ZERO
      nc = np * n_Layers
      j1 = j + n_nodes + (inode-1)*nc
      j2 = j1 + nc
      DO i = 1, np
        js1 = j1+(i-1)*n_Layers-1
        js2 = j2+(i-1)*n_Layers-1
        DO k = 1, n_Layers
          OD1(k) = OD1(k) + TC%C(js1+k)*Predictor%X(k, i, 1)
          OD2(k) = OD2(k) + TC%C(js2+k)*Predictor%X(k, i, 1)
        END DO
      END DO
      !-------------------------------------------------------
      ! (1) Interpolate on the user requested Doppler frequency
      ! (2) Compute the Slant path optical depth profile.
      !-------------------------------------------------------
      DO k = 1, n_Layers
        IF(ChannelIndex == 2)THEN
          OD(k) = w1*EXP(OD1(k)) + w2*EXP(OD2(k))
          OD_tmp = OD(k)
        ELSE
          OD(k) = w1*OD1(k) + w2*OD2(k)
          OD_tmp = OD(k)
          IF(OD(k) < ZERO)OD_tmp = ZERO
        END IF
        OD_Path(k) = OD_Path(k-1) + OD_tmp*Predictor%Secant_Zenith(k)
      END DO

      ! Save FW variables
      IF ( PAFV_Associated(Predictor%PAFV) ) THEN
        Predictor%PAFV%OD = OD
        Predictor%PAFV%w1 = w1
        Predictor%PAFV%w2 = w2
        Predictor%PAFV%inode = inode
      END IF

    END IF

  END SUBROUTINE Compute_ODPath_zssmis

!------------------------------------------------------------------------------
!
! NAME:
!      Compute_ODPath_zssmis_TL
!
! PURPOSE:
!       Subroutine to compute TL slant path optical path for ZSSMIS
!       (a virtual sensor created for the SSMIS 4 Zeeman channels:
!       ch 19, 20, 21, 22)
!
! CALLING SEQUENCE:
!
!        CALL Compute_ODPath_zssmis_TL(ChannelIndex,  &
!                                      TC,            &
!                                      Predictor,     &
!                                      Predictor_TL,  &
!                                      OD_Path_TL )
!
! INPUT ARGUMENTS:
!
!      ChannelIndex:     Channel index (a sequential number for the channels in the structure TC)
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!            TC:         ODPS structure holding coefficient data
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Predictor structure containing the predictors for estimating of optical depth
!                        UNITS:      N/A
!                        TYPE:       ODPS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor_TL:    Predictor structure containing the TL predictors
!                        UNITS:      N/A
!                        TYPE:       ODPS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(INOUT)
!
!   OUTPUT ARGUMENTS:
!         OD_Path_TL:   TL Slant path optical path profile (from space down)
!                       UNITS:      N/A
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1 (0:n_Layers)
!                       ATTRIBUTES: INTENT(OUT)
!
!------------------------------------------------------------------------------
  SUBROUTINE Compute_ODPath_zssmis_TL(ChannelIndex,  &
                                      TC,            &
                                      Predictor,     &
                                      Predictor_TL,  &
                                      OD_Path_TL )
    INTEGER,                   INTENT( IN )     :: ChannelIndex
    TYPE(ODPS_type),           INTENT( IN )     :: TC
    TYPE(ODPS_Predictor_type), INTENT( IN )     :: Predictor
    TYPE(ODPS_Predictor_type), INTENT( INOUT )  :: Predictor_TL
    REAL(fp),                  INTENT(OUT)      :: OD_Path_TL(0:)

    ! Local
    REAL(fp), DIMENSION(Predictor%n_Layers) :: OD1, OD2, OD1_TL, OD2_TL
    REAL(fp) :: O1, O2
    REAL(fp) :: OD_TL
    INTEGER  :: i, j, j1, j2, js1, js2, k, n_nodes, n_Layers, nc, np

    OD_Path_TL = ZERO
    np = TC%n_Predictors(1, ChannelIndex)

    ! Check if there is any absorption for the component&channel combination.
    IF( np > 0 ) THEN

      !----------------------------------------------------------------------------------
      ! (1) Find the nodes of the Doppler shift frequencies, which bracket the user
      ! Doppler frequency; (2) compute weights for interpolation.
      !----------------------------------------------------------------------------------
      j = TC%Pos_Index(1, ChannelIndex)
      n_nodes = INT(TC%C(j))
      n_Layers = Predictor%n_Layers
      j = j + 1

      !--------------------------------------------
      ! Compute optical depths at the two nodes
      !--------------------------------------------

      OD1 = ZERO
      OD2 = ZERO
      OD1_TL = ZERO
      OD2_TL = ZERO
      nc = np * n_Layers
      j1 = j + n_nodes + (Predictor%PAFV%inode-1)*nc
      j2 = j1 + nc
      DO i = 1, np
        js1 = j1+(i-1)*n_Layers-1
        js2 = j2+(i-1)*n_Layers-1
        DO k = 1, n_Layers
          OD1(k) = OD1(k) + TC%C(js1+k)*Predictor%X(k, i, 1)
          OD2(k) = OD2(k) + TC%C(js2+k)*Predictor%X(k, i, 1)
          OD1_TL(k) = OD1_TL(k) + TC%C(js1+k)*Predictor_TL%X(k, i, 1)
          OD2_TL(k) = OD2_TL(k) + TC%C(js2+k)*Predictor_TL%X(k, i, 1)
        END DO
      END DO
      !-------------------------------------------------------
      ! (1) Interpolate on the user requested Doppler frequency
      ! (2) Compute the Slant path optical depth profile.
      !-------------------------------------------------------
      DO k = 1, n_Layers
        IF(ChannelIndex == 2)THEN
          O1 = EXP(OD1(k))
          O2 = EXP(OD2(k))
          OD_TL = Predictor%PAFV%w1*O1*OD1_TL(k) + Predictor%PAFV%w2*O2*OD2_TL(k)
        ELSE
          OD_TL = Predictor%PAFV%w1*OD1_TL(k) + Predictor%PAFV%w2*OD2_TL(k)
          IF(Predictor%PAFV%OD(k) < ZERO)OD_TL = ZERO
        END IF
        OD_Path_TL(k) = OD_Path_TL(k-1) + OD_TL*Predictor%Secant_Zenith(k)
      END DO
    END IF
  END SUBROUTINE Compute_ODPath_zssmis_TL

!------------------------------------------------------------------------------
!
! NAME:
!      Compute_ODPath_zssmis_AD
!
! PURPOSE:
!       Subroutine to compute AD slant path optical path for ZSSMIS
!       (a virtual sensor created for the SSMIS 4 Zeeman channels:
!       ch 19, 20, 21, 22)
!
! CALLING SEQUENCE:
!
!        CALL Compute_ODPath_zssmis_AD(ChannelIndex,  &
!                                      TC,            &
!                                      Predictor,     &
!                                      OD_Path_AD,    &
!                                      Predictor_AD)
!
! INPUT ARGUMENTS:
!
!      ChannelIndex:     Channel index (a sequential number for the channels in the structure TC)
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!            TC:         ODPS structure holding coefficient data
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Predictor structure containing the predictors for estimating of optical depth
!                        UNITS:      N/A
!                        TYPE:       ODPS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!         OD_Path_AD:    AD Slant path optical path profile (from space down)
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1 (0:n_Layers)
!                        ATTRIBUTES: INTENT(INOUT)
!
!   OUTPUT ARGUMENTS:
!       Predictor_AD:    Predictor structure containing the AD predictors
!                        UNITS:      N/A
!                        TYPE:       ODPS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(INOUT)
!
!------------------------------------------------------------------------------
  SUBROUTINE Compute_ODPath_zssmis_AD(ChannelIndex,  &
                                      TC,            &
                                      Predictor,     &
                                      OD_Path_AD,    &
                                      Predictor_AD)
    INTEGER,                   INTENT( IN )     :: ChannelIndex
    TYPE(ODPS_type),           INTENT( IN )     :: TC
    TYPE(ODPS_Predictor_type), INTENT( IN )     :: Predictor
    REAL(fp),                  INTENT( INOUT )  :: OD_Path_AD(0:)
    TYPE(ODPS_Predictor_type), INTENT( INOUT )  :: Predictor_AD

    ! Local
    REAL(fp), DIMENSION(Predictor%n_Layers) :: OD1, OD2, OD1_AD, OD2_AD
    REAL(fp) :: O1, O2
    REAL(fp) :: OD_AD
    INTEGER  :: i, j, j1, j2, js1, js2, k, n_nodes, n_Layers, nc, np

    np = TC%n_Predictors(1, ChannelIndex)

    !------------------------
    ! Forward calculation
    !------------------------
    ! Check if there is any absorption for the component&channel combination.
    IF( np > 0 ) THEN
      j = TC%Pos_Index(1, ChannelIndex)
      n_nodes = INT(TC%C(j))
      n_Layers = Predictor%n_Layers
      j = j + 1

      OD1 = ZERO
      OD2 = ZERO
      nc = np * n_Layers
      j1 = j + n_nodes + (Predictor%PAFV%inode-1)*nc
      j2 = j1 + nc
      DO i = 1, np
        js1 = j1+(i-1)*n_Layers-1
        js2 = j2+(i-1)*n_Layers-1
        DO k = 1, n_Layers
          OD1(k) = OD1(k) + TC%C(js1+k)*Predictor%X(k, i, 1)
          OD2(k) = OD2(k) + TC%C(js2+k)*Predictor%X(k, i, 1)
        END DO
      END DO

     !-------------------------------
     ! AD calculation
     !-------------------------------
      DO k = n_Layers, 1, -1
        OD_AD = Predictor%Secant_Zenith(k)*OD_Path_AD(k) ! OD_AD does not cumulate
        OD_Path_AD(k-1) = OD_Path_AD(k-1) + OD_Path_AD(k)
        IF(ChannelIndex == 2)THEN
          O1 = EXP(OD1(k))
          O2 = EXP(OD2(k))

          OD1_AD(k) = Predictor%PAFV%w1*O1*OD_AD  ! OD1_AD(k) and OD2_AD(k) do not cumulate
          OD2_AD(k) = Predictor%PAFV%w2*O2*OD_AD
        ELSE
          IF(Predictor%PAFV%OD(k) < ZERO)OD_AD = ZERO
          OD1_AD(k) = Predictor%PAFV%w1*OD_AD
          OD2_AD(k) = Predictor%PAFV%w2*OD_AD
        END IF
      END DO

      DO i = 1, np
        js1 = j1+(i-1)*n_Layers-1
        js2 = j2+(i-1)*n_Layers-1
        DO k = n_Layers, 1, -1
          Predictor_AD%X(k, i, 1) = Predictor_AD%X(k, i, 1) + &
                                    OD1_AD(k)*TC%C(js1+k)
          Predictor_AD%X(k, i, 1) = Predictor_AD%X(k, i, 1) + &
                                    OD2_AD(k)*TC%C(js2+k)
        END DO
      END DO

      OD_Path_AD = ZERO

    END IF

  END SUBROUTINE Compute_ODPath_zssmis_AD

!------------------------------------------------------------------------------
!
! NAME:
!      Compute_ODPath_zamsua
!
! PURPOSE:
!       Subroutine to compute slant path optical path for AMSUA channel 14
!       affected by Zeeman splitting.
!
! CALLING SEQUENCE:
!
!    SUBROUTINE Compute_ODPath_zamsua(TC,            &
!                                     Predictor,     &
!                                     OD_Path )
!
! INPUT ARGUMENTS:
!
!            TC:         ODPS structure holding coefficient data
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Predictor structure containing the predictors for estimating of optical depth
!                        UNITS:      N/A
!                        TYPE:       ODPS_Predictor_type
!                        DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!   OUTPUT ARGUMENTS:
!         OD_Path:      Slant path optical path profile (from space down)
!                       UNITS:      N/A
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1 (0:n_Layers)
!                       ATTRIBUTES: INTENT(OUT)
!
!------------------------------------------------------------------------------

  SUBROUTINE Compute_ODPath_zamsua(TC,               &
                                   Predictor,        &
                                   OD_Path )
    TYPE(ODPS_type),           INTENT( IN )     :: TC
    TYPE(ODPS_Predictor_type), INTENT( INOUT )  :: Predictor
    REAL(fp),                  INTENT( OUT)     :: OD_Path(0:)

    ! Local
    REAL(fp), DIMENSION(Predictor%n_Layers)   :: ODv, ODh
    REAL(fp), DIMENSION(0:Predictor%n_Layers) :: ODv_Path, ODh_Path
    REAL(fp) :: tauv, tauh, tau
    REAL(fp) :: Wv, Wh
    INTEGER  :: i, j1, j2, k1, k2, k, m1, m2, n_Layers, np

    OD_Path = ZERO
    np = TC%n_Predictors(1, 1)

    ! Check if there is any absorption for the component&channel combination.
    IF( np > 0 ) THEN

      !----------------------------------------------------------------------------------
      ! Compute optical depths at specified polarizations
      !----------------------------------------------------------------------------------
      n_Layers = Predictor%n_Layers
      j1 = TC%Pos_Index(1, 1)    ! starting index for vertical polarization
      j2 = j1 + (np+1)*n_Layers  ! starting index for horizontal polarization

      ! offset coefficients
      ODv = TC%C(j1:(j1+n_Layers-1))
      ODh = TC%C(j2:(j2+n_Layers-1))
      ! Predictor contributions
      DO i = 1, np
        m1 = j1+i*n_Layers
        m2 = j2+i*n_Layers
        DO k = 1, n_Layers
          k1 = m1+(k-1)
          k2 = m2+(k-1)
          ODv(k) = ODv(k) + TC%C(k1)*Predictor%X(k, i, 1)
          ODh(k) = ODh(k) + TC%C(k2)*Predictor%X(k, i, 1)
        END DO
      END DO

      !------------------------------------------------------
      ! Compute transmittances and then combine them
      !------------------------------------------------------
      Wv = Predictor%w
      Wh = ONE - Wv
      ODv_Path(0) = ZERO
      ODh_Path(0) = ZERO
      DO k = 1, n_Layers
        IF(ODv(k) < ZERO)ODv(k) = ZERO
        ODv_Path(k) = ODv_Path(k-1) + ODv(k)
        tauv = EXP(-ODv_Path(k))
        IF(ODh(k) < ZERO)ODh(k) = ZERO
        ODh_Path(k) = ODh_Path(k-1) + ODh(k)
        tauh = EXP(-ODh_Path(k))

        tau = Wv*tauv + Wh*tauh
        OD_Path(k) = -LOG(tau)
      END DO

    END IF

  END SUBROUTINE Compute_ODPath_zamsua

!------------------------------------------------------------------------------
!
! NAME:
!      Compute_ODPath_zamsua_TL
!
! PURPOSE:
!       Subroutine to compute TL slant path optical path for AMSUA channel 14
!       affected by Zeeman splitting.
!
! CALLING SEQUENCE:
!
!        CALL Compute_ODPath_zamsua_TL(TC,            &
!                                      Predictor,     &
!                                      Predictor_TL,  &
!                                      OD_Path_TL )
!
! INPUT ARGUMENTS:
!
!            TC:         ODPS structure holding coefficient data
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Predictor structure containing the predictors for estimating of optical depth
!                        UNITS:      N/A
!                        TYPE:       ODPS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor_TL:    Predictor structure containing the TL predictors
!                        UNITS:      N/A
!                        TYPE:       ODPS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(INOUT)
!
!   OUTPUT ARGUMENTS:
!         OD_Path_TL:   TL Slant path optical path profile (from space down)
!                       UNITS:      N/A
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1 (0:n_Layers)
!                       ATTRIBUTES: INTENT(OUT)
!
!------------------------------------------------------------------------------
  SUBROUTINE Compute_ODPath_zamsua_TL(TC,               &
                                      Predictor,        &
                                      Predictor_TL,     &
                                      OD_Path_TL )
    TYPE(ODPS_type),           INTENT( IN )     :: TC
    TYPE(ODPS_Predictor_type), INTENT( IN )     :: Predictor
    TYPE(ODPS_Predictor_type), INTENT( IN )     :: Predictor_TL
    REAL(fp),                  INTENT( OUT)     :: OD_Path_TL(0:)

    ! Local
    REAL(fp), DIMENSION(Predictor%n_Layers)   :: ODv, ODh, ODv_TL, ODh_TL
    REAL(fp), DIMENSION(0:Predictor%n_Layers) :: ODv_Path, ODh_Path, ODv_Path_Tl, ODh_Path_TL
    REAL(fp) :: tauv, tauh, tau, tauv_TL, tauh_TL, tau_TL
    REAL(fp) :: Wv, Wh
    INTEGER  :: i, j1, j2, k1, k2, k, m1, m2, n_Layers, np

    OD_Path_TL = ZERO
    np = TC%n_Predictors(1, 1)

    ! Check if there is any absorption for the component&channel combination.
    IF( np > 0 ) THEN

      !----------------------------------------------------------------------------------
      ! Compute optical depths at specified polarizations
      !----------------------------------------------------------------------------------
      n_Layers = Predictor%n_Layers
      j1 = TC%Pos_Index(1, 1)    ! starting index for vertical polarization
      j2 = j1 + (np+1)*n_Layers  ! starting index for horizontal polarization

      ODv = TC%C(j1:(j1+n_Layers-1))
      ODh = TC%C(j2:(j2+n_Layers-1))
      ODv_TL = ZERO
      ODh_TL = ZERO
      DO i = 1, np
        m1 = j1+i*n_Layers
        m2 = j2+i*n_Layers
        DO k = 1, n_Layers
          k1 = m1+(k-1)
          k2 = m2+(k-1)
          ODv(k) = ODv(k) + TC%C(k1)*Predictor%X(k, i, 1)
          ODh(k) = ODh(k) + TC%C(k2)*Predictor%X(k, i, 1)
          ODv_TL(k) = ODv_TL(k) + TC%C(k1)*Predictor_TL%X(k, i, 1)
          ODh_TL(k) = ODh_TL(k) + TC%C(k2)*Predictor_TL%X(k, i, 1)
        END DO
      END DO

      !------------------------------------------------------
      ! Compute transmittances and then combine them
      !------------------------------------------------------
      Wv = Predictor%w
      Wh = ONE - Wv
      ODv_Path(0) = ZERO
      ODh_Path(0) = ZERO
      ODv_Path_TL(0) = ZERO
      ODh_Path_TL(0) = ZERO
      DO k = 1, n_Layers
        IF(ODv(k) < ZERO)THEN
          ODv(k)    = ZERO
          ODv_TL(k) = ZERO
        END IF
        ODv_Path(k) = ODv_Path(k-1) + ODv(k)
        tauv = EXP(-ODv_Path(k))
        ODv_Path_TL(k) = ODv_Path_TL(k-1) + ODv_TL(k)
        tauv_TL = -tauv*ODv_Path_TL(k)

        IF(ODh(k) < ZERO)THEN
          ODh(k)    = ZERO
          ODh_TL(k) = ZERO
        END IF
        ODh_Path(k) = ODh_Path(k-1) + ODh(k)
        tauh = EXP(-ODh_Path(k))
        ODh_Path_TL(k) = ODh_Path_TL(k-1) + ODh_TL(k)
        tauh_TL = -tauh*ODh_Path_TL(k)

        tau = Wv*tauv + Wh*tauh
        tau_TL = Wv*tauv_TL + Wh*tauh_TL
        OD_Path_TL(k) = -(ONE/tau)*tau_TL

      END DO

    END IF

  END SUBROUTINE Compute_ODPath_zamsua_TL

!------------------------------------------------------------------------------
!
! NAME:
!      Compute_ODPath_zamsua_AD
!
! PURPOSE:
!       Subroutine to compute AD slant path optical path for AMSUA channel 14
!       affected by Zeeman splitting.
!
! CALLING SEQUENCE:
!
!        CALL Compute_ODPath_zamsua_AD(TC,            &
!                                      Predictor,     &
!                                      OD_Path_AD,    &
!                                      Predictor_AD)
!
! INPUT ARGUMENTS:
!
!            TC:         ODPS structure holding coefficient data
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Predictor structure containing the predictors for estimating of optical depth
!                        UNITS:      N/A
!                        TYPE:       ODPS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!         OD_Path_AD:    AD Slant path optical path profile (from space down)
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1 (0:n_Layers)
!                        ATTRIBUTES: INTENT(INOUT)
!
!   OUTPUT ARGUMENTS:
!       Predictor_AD:    Predictor structure containing the AD predictors
!                        UNITS:      N/A
!                        TYPE:       ODPS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(INOUT)
!
!------------------------------------------------------------------------------
  SUBROUTINE Compute_ODPath_zamsua_AD(TC,               &
                                      Predictor,        &
                                      OD_Path_AD,       &
                                      Predictor_AD)
    TYPE(ODPS_type),           INTENT( IN )     :: TC
    TYPE(ODPS_Predictor_type), INTENT( IN )     :: Predictor
    REAL(fp),                  INTENT( INOUT)   :: OD_Path_AD(0:)
    TYPE(ODPS_Predictor_type), INTENT( INOUT )  :: Predictor_AD

    ! Local
    REAL(fp), DIMENSION(Predictor%n_Layers)   :: ODv, ODh, ODv_AD, ODh_AD
    REAL(fp), DIMENSION(0:Predictor%n_Layers) :: ODv_Path, ODh_Path, ODv_Path_AD, ODh_Path_AD
    REAL(fp) :: tauv, tauh, tau, tauv_AD, tauh_AD, tau_AD
    REAL(fp) :: Wv, Wh, OD_tmp
    INTEGER  :: i, j1, j2, k1, k2, m1, m2, k, n_Layers, np

    np = TC%n_Predictors(1, 1)

    ! Check if there is any absorption for the component&channel combination.
    IF( np > 0 ) THEN

      ODv_AD      = ZERO
      ODh_AD      = ZERO
      ODv_Path_AD = ZERO
      ODh_Path_AD = ZERO
      tauv_AD     = ZERO
      tauh_AD     = ZERO
      tau_AD      = ZERO

      ! ***************
      ! Forward part
      ! ***************

      !----------------------------------------------------------------------------------
      ! Compute optical depths at specified polarizations
      !----------------------------------------------------------------------------------
      n_Layers = Predictor%n_Layers
      j1 = TC%Pos_Index(1, 1)    ! starting index for vertical polarization
      j2 = j1 + (np+1)*n_Layers  ! starting index for horizontal polarization

      ODv = TC%C(j1:(j1+n_Layers-1))
      ODh = TC%C(j2:(j2+n_Layers-1))
      DO i = 1, np
        m1 = j1+i*n_Layers
        m2 = j2+i*n_Layers
        DO k = 1, n_Layers
          k1 = m1+(k-1)
          k2 = m2+(k-1)
          ODv(k) = ODv(k) + TC%C(k1)*Predictor%X(k, i, 1)
          ODh(k) = ODh(k) + TC%C(k2)*Predictor%X(k, i, 1)
        END DO
      END DO

      !------------------------------------------------------
      ! Compute transmittances and then combine them
      !------------------------------------------------------
      Wv = Predictor%w
      Wh = ONE - Wv
      ODv_Path(0) = ZERO
      ODh_Path(0) = ZERO
      DO k = 1, n_Layers
        OD_tmp = ODv(k)
        IF(ODv(k) < ZERO)OD_tmp = ZERO
        ODv_Path(k) = ODv_Path(k-1) + OD_tmp
        OD_tmp = ODh(k)
        IF(ODh(k) < ZERO)OD_tmp = ZERO
        ODh_Path(k) = ODh_Path(k-1) + OD_tmp
      END DO

      ! ***************
      ! Adjoint part
      ! ***************

      DO k = n_Layers, 1, -1
        tauv = EXP(-ODv_Path(k))
        tauh = EXP(-ODh_Path(k))
        tau = Wv*tauv + Wh*tauh

        tau_AD = tau_AD - (ONE/tau)*OD_Path_AD(k)
        OD_Path_AD(k) = ZERO
        tauv_AD = tauv_AD + Wv*tau_AD
        tauh_AD = tauh_AD + Wh*tau_AD
        tau_AD = ZERO

        ODh_PATH_AD(k) = ODh_PATH_AD(k) - tauh*tauh_AD
        tauh_AD = ZERO
        ODh_Path_AD(k-1) = ODh_Path_AD(k-1) + ODh_Path_AD(k)
        ODh_AD(k)        = ODh_AD(k)        + ODh_Path_AD(k)
        ODh_Path_AD(k) = ZERO
        IF(ODh(k) < ZERO)ODh_AD(k) = ZERO

        ODv_PATH_AD(k) = ODv_PATH_AD(k) - tauv*tauv_AD
        tauv_AD = ZERO
        ODv_Path_AD(k-1) = ODv_Path_AD(k-1) + ODv_Path_AD(k)
        ODv_AD(k)        = ODv_AD(k)        + ODv_Path_AD(k)
        ODv_Path_AD(k) = ZERO
        IF(ODv(k) < ZERO)ODv_AD(k) = ZERO
      END DO
!      ODv_Path_AD(0) = ZERO
!      ODh_Path_AD(0) = ZERO

      DO i = np, 1, -1
        m1 = j1+i*n_Layers
        m2 = j2+i*n_Layers
        DO k = n_Layers, 1, -1
          k1 = m1+(k-1)
          k2 = m2+(k-1)
          Predictor_AD%X(k, i, 1) = Predictor_AD%X(k, i, 1) + TC%C(k1)*ODv_AD(k)
          Predictor_AD%X(k, i, 1) = Predictor_AD%X(k, i, 1) + TC%C(k2)*ODh_AD(k)
        END DO
      END DO
!      OD1_AD(:) = ZERO
!      OD2_AD(:) = ZERO

    END IF

    OD_Path_AD = ZERO

  END SUBROUTINE Compute_ODPath_zamsua_AD

!--------------------------------------------------------------------------------
!
! NAME:
!       Zeeman_Compute_Predictors
!
! PURPOSE:
!       Subroutine to calculate the gas absorption model predictors. It first
!       Interpolates the user temperature and absorber profiles on the
!       internal pressure grids and then call the predictor computation
!       routine to compute the predictors
!
! CALLING SEQUENCE:
!       CALL Zeeman_Compute_Predictors( Zeeman   , &  ! Input
!                                       TC       , &  ! Input
!                                       Atm      , &  ! Input
!                                       GeoInfo  , &  ! Input
!                                       Predictor  )  ! Output
!
! INPUT ARGUMENTS:
!       Zeeman:          Structure holding Zeeman-specific user inputs
!                        UNITS:      N/A
!                        TYPE:       Zeeman_Input_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       TC:              ODPS structure holding tau coefficients
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Atm:             CRTM Atmosphere structure containing the atmospheric
!                        state data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       GeoInfo:         CRTM_GeometryInfo structure containing the
!                        view geometry information.
!                        UNITS:      N/A
!                        TYPE:       CRTM_GeometryInfo_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor:       Predictor structure containing the integrated absorber
!                        and predictor profiles.
!                        UNITS:      N/A
!                        TYPE:       ODPS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE Zeeman_Compute_Predictors( &
    Zeeman   ,  &
    TC       ,  &
    Atm      ,  &
    GeoInfo  ,  &
    Predictor   )
    ! Arguments
    TYPE(Zeeman_Input_type)     , INTENT(IN)     :: Zeeman
    TYPE(ODPS_type)             , INTENT(IN)     :: TC
    TYPE(CRTM_Atmosphere_type)  , INTENT(IN)     :: Atm
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeoInfo
    TYPE(ODPS_Predictor_type)   , INTENT(IN OUT) :: Predictor
    ! Local variables
    REAL(fp) :: Temperature(Predictor%n_Layers)
    REAL(fp) :: Absorber(Predictor%n_Layers, TC%n_Absorbers)
    INTEGER  :: H2O_idx
    REAL(fp) :: Sensor_Scan_Radian, Secant_Sensor_Zenith
    REAL(fp) :: Be, COS_ThetaB, COS_PhiB, Doppler_Shift
    REAL(fp) :: COS2_ScanA, COS2_PhiB

    ! Retrieve required geometry values
    CALL CRTM_GeometryInfo_GetValue( &
           GeoInfo                                    , &  ! Input
           Sensor_Scan_Radian   = Sensor_Scan_Radian  , &  ! Output
           Secant_Sensor_Zenith = Secant_Sensor_Zenith  )  ! Output
    ! ...Store the surface secant zenith angle
    Predictor%Secant_Zenith_Surface = Secant_Sensor_Zenith

    ! Mapping data from user to internal fixed pressure layers/levels.
    CALL Map_Input( &
           Atm                            , &  ! Input
           TC                             , &  ! Input
           GeoInfo                        , &  ! Input
           Temperature                    , &  ! Output
           Absorber                       , &  ! output
           Predictor%User_Level_LnPressure, &  ! Output, non variable
           Predictor%Ref_Level_LnPressure , &  ! Output, non variable
           Predictor%Secant_Zenith        , &  ! Output, non variable
           H2O_idx                        , &
           Predictor%PAFV                   )  ! structure holding FW parameters

    ! Compute predictor for specific instruments
    SELECT CASE ( TC%Group_Index )
      CASE ( ODPS_gINDEX_ZSSMIS )
        CALL Zeeman_Input_GetValue( &
               Zeeman                        , &  ! Input
               Field_Strength = Be           , &  ! Output
               COS_ThetaB     = COS_ThetaB   , &  ! Output
               Doppler_Shift  = Doppler_Shift  )  ! Output
        CALL Compute_Predictors_zssmis( &
               Temperature            , &
               Be                     , &
               COS_ThetaB             , &
               Doppler_Shift          , &
               Predictor%Secant_Zenith, &
               Predictor                )

      CASE ( ODPS_gINDEX_ZAMSUA )
        CALL Zeeman_Input_GetValue( &
               Zeeman                     , &  ! Input
               Field_Strength = Be        , &  ! Output
               COS_ThetaB     = COS_ThetaB, &  ! Output
               COS_PhiB       = COS_PhiB    )  ! Output
        CALL Compute_Predictors_zamsua( &
               Temperature            , &
               TC%Ref_Temperature     , &
               Be                     , &
               COS_ThetaB             , &
               Predictor%Secant_Zenith, &
               Predictor                )
        ! Weights for combining transmittances at the two special polarizations
        COS2_ScanA = COS(Sensor_Scan_Radian)**2
        COS2_PhiB  = COS_PhiB**2
        Predictor%w = (ONE-COS2_ScanA)*COS2_PhiB + COS2_ScanA*(ONE-COS2_PhiB)

      CASE DEFAULT
        ! This is a NOOP - does checking need to
        ! be done upon entry to this routine?
    END SELECT

    IF ( PAFV_Associated(Predictor%PAFV) ) THEN
      ! Set and save the interpolation index array for absorption
      ! calculations. Since the indexes do not depend on channel but
      ! the absorption calculations do, put the index calculation here
      ! can improve efficency.
      CALL Compute_Interp_Index( &
             Predictor%Ref_Level_LnPressure , &
             Predictor%User_Level_LnPressure, &
             Predictor%PAFV%ODPS2User_Idx     )
    END IF

  END SUBROUTINE Zeeman_Compute_Predictors

!--------------------------------------------------------------------------------
!
! NAME:
!       Zeeman_Compute_Predictors_TL
!
! PURPOSE:
!       Subroutine to calculate the TL gas absorption model predictors. It first
!       Interpolates the user temperature and absorber profiles on the
!       internal pressure grids and then call the predictor computation
!       routine to compute the predictors
!
! CALLING SEQUENCE:
!       CALL Zeeman_Compute_Predictors_TL( Zeeman      , &  ! Input
!                                          TC          , &  ! Input
!                                          Predictor   , &  ! FWD Input
!                                          Atm_TL      , &  ! TL  Input
!                                          Predictor_TL  )  ! TL  Output
!
! INPUT ARGUMENTS:
!       Zeeman:          Structure holding Zeeman-specific user inputs
!                        UNITS:      N/A
!                        TYPE:       Zeeman_Input_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       TC:              ODPS structure holding tau coefficients
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Predictor structure containing the integrated absorber
!                        and predictor profiles.
!                        UNITS:      N/A
!                        TYPE:       ODPS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Atm_TL:          CRTM Atmosphere structure containing the tangent-linear
!                        atmospheric state data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Predictor_TL:    Predictor structure containing the tangent-linear
!                        integrated absorber and predictor profiles.
!                        UNITS:      N/A
!                        TYPE:       ODPS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE Zeeman_Compute_Predictors_TL( &
    Zeeman      , &
    TC          , &
    Predictor   , &
    Atm_TL      , &
    Predictor_TL)
    ! Arguments
    TYPE(Zeeman_Input_type)   , INTENT(IN)     :: Zeeman
    TYPE(ODPS_type)           , INTENT(IN)     :: TC
    TYPE(ODPS_Predictor_type) , INTENT(IN)     :: Predictor
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm_TL
    TYPE(ODPS_Predictor_type) , INTENT(IN OUT) :: Predictor_TL
    ! Local variables
    REAL(fp) :: Absorber_TL(Predictor%n_Layers, TC%n_Absorbers)
    REAL(fp) :: Temperature_TL(Predictor%n_Layers)
    REAL(fp) :: Be, COS_ThetaB, Doppler_Shift

    ! Mapping data from user to internal fixed pressure layers/levels.
    CALL Map_Input_TL( &
           TC            , &  ! Input
           Atm_TL        , &  ! Input
           Temperature_TL, &  ! Output
           Absorber_TL   , &  ! Output
           Predictor%PAFV  )  ! Input

    ! Compute predictor for specific instruments
    SELECT CASE ( TC%Group_Index )
      CASE ( ODPS_gINDEX_ZSSMIS )
        CALL Zeeman_Input_GetValue( &
               Zeeman                        , &  ! Input
               Field_Strength = Be           , &  ! Output
               COS_ThetaB     = COS_ThetaB   , &  ! Output
               Doppler_Shift  = Doppler_Shift  )  ! Output
        CALL Compute_Predictors_zssmis_TL( &
               Predictor%PAFV%Temperature, &
               Be                        , &
               COS_ThetaB                , &
               Temperature_TL            , &
               Predictor_TL                )

      CASE ( ODPS_gINDEX_ZAMSUA )
        CALL Zeeman_Input_GetValue( &
               Zeeman                     , &  ! Input
               Field_Strength = Be        , &  ! Output
               COS_ThetaB     = COS_ThetaB  )  ! Output
        CALL Compute_Predictors_zamsua_TL( &
               Predictor%PAFV%Temperature, &
               TC%Ref_Temperature        , &
               Temperature_TL            , &
               Predictor_TL                )

      CASE DEFAULT
        ! This is a NOOP - does checking need to
        ! be done upon entry to this routine?
    END SELECT

  END SUBROUTINE Zeeman_Compute_Predictors_TL


!--------------------------------------------------------------------------------
!
! NAME:
!       Zeeman_Compute_Predictors_AD
!
! PURPOSE:
!       Subroutine to calculate the AD gas absorption model predictors. It first
!       Interpolates the user temperature and absorber profiles on the
!       internal pressure grids and then call the predictor computation
!       routine to compute the predictors
!
! CALLING SEQUENCE:
!       CALL Zeeman_Compute_Predictors_AD( Zeeman      , &  ! Input
!                                          TC,         , &  ! Input
!                                          Predictor,  , &  ! FWD Input
!                                          Predictor_AD, &  ! AD  Input
!                                          Atm_AD        )  ! AD  Output
!
! INPUT ARGUMENTS:
!       Zeeman:          Structure holding Zeeman-specific user inputs
!                        UNITS:      N/A
!                        TYPE:       Zeeman_Input_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       TC:              ODPS structure holding tau coefficients
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Predictor structure containing the integrated absorber
!                        and predictor profiles.
!                        UNITS:      N/A
!                        TYPE:       ODPS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor_AD:    Predictor structure containing the adjoint integrated
!                        absorber and predictor profiles.
!                        UNITS:      N/A
!                        TYPE:       ODPS_Predictor_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!
!       Atm_AD:          CRTM Atmosphere structure containing the adjoint
!                        atmospheric state data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE Zeeman_Compute_Predictors_AD( &
    Zeeman      , &
    TC          , &
    Predictor   , &
    Predictor_AD, &
    Atm_AD        )
    ! Arguments
    TYPE(Zeeman_Input_type)   , INTENT(IN)     :: Zeeman
    TYPE(ODPS_type)           , INTENT(IN)     :: TC
    TYPE(ODPS_Predictor_type) , INTENT(IN)     :: Predictor
    TYPE(ODPS_Predictor_type) , INTENT(IN OUT) :: predictor_AD
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atm_AD
    ! Local variables
    REAL(fp) :: Absorber_AD(Predictor%n_Layers, TC%n_Absorbers)
    REAL(fp) :: Temperature_AD(Predictor%n_Layers)
    REAL(fp) :: Be, COS_ThetaB, Doppler_Shift

    ! Local adjoint variable initialization
    Temperature_AD = ZERO
    Absorber_AD    = ZERO

    ! Compute predictor for specific instruments
    SELECT CASE ( TC%Group_Index )
      CASE ( ODPS_gINDEX_ZSSMIS )
        CALL Zeeman_Input_GetValue( &
               Zeeman                        , &  ! Input
               Field_Strength = Be           , &  ! Output
               COS_ThetaB     = COS_ThetaB   , &  ! Output
               Doppler_Shift  = Doppler_Shift  )  ! Output
        CALL Compute_Predictors_zssmis_AD( &
               Predictor%PAFV%Temperature, &
               Be                        , &
               COS_ThetaB                , &
               Predictor_AD              , &
               Temperature_AD              )

      CASE ( ODPS_gINDEX_ZAMSUA )
        CALL Zeeman_Input_GetValue( &
               Zeeman                     , &  ! Input
               Field_Strength = Be        , &  ! Output
               COS_ThetaB     = COS_ThetaB  )  ! Output
        CALL Compute_Predictors_zamsua_AD( &
               Predictor%PAFV%Temperature, &
               TC%Ref_Temperature        , &
               Predictor_AD              , &
               Temperature_AD              )

      CASE DEFAULT
        ! This is a NOOP - does checking need to
        ! be done upon entry to this routine?
    END SELECT

    ! Mapping data from user to internal fixed pressure layers/levels.
    CALL Map_Input_AD( &
           TC            , & ! Input
           Temperature_AD, & ! Input
           Absorber_AD   , & ! Input
           Atm_AD        , & ! output
           Predictor%PAFV  ) ! Input

  END SUBROUTINE Zeeman_Compute_Predictors_AD


  !-------------------------------------------------
  ! Check if the given channel is a Zeeman channel
  !  Inputs:
  !    TC  - Taucoeff structure
  !    ChannelIndex - the sensor's channel index
  !  Return
  !    .TRUE.  - this is a Zeeman channel
  !    .FALSE. - not a Zeeman channel
  !-------------------------------------------------
  FUNCTION Is_Zeeman_Channel(TC, ChannelIndex) RESULT( ZChannel )
    TYPE(ODPS_type), INTENT(IN) :: TC
    INTEGER,         INTENT(IN) :: ChannelIndex
    LOGICAL :: ZChannel

    SELECT CASE ( TC%Group_Index )
      CASE ( ODPS_gINDEX_ZSSMIS )
        ZChannel = ZSSMIS_ChannelMap(ChannelIndex) > 0
      CASE ( ODPS_gINDEX_ZAMSUA )
        ZChannel = ZAMSUA_ChannelMap(ChannelIndex) > 0
      CASE DEFAULT
        ZChannel = .FALSE.
    END SELECT

  END FUNCTION Is_Zeeman_Channel


  !---------------------------------------------------------
  ! Check if the given TC is associated with Zeeman algorithm
  !  Inputs:
  !    TC  - Taucoeff structure
  !  Return
  !    .TRUE.  - associated with the Zeeman algorithm
  !    .FALSE. - not associated with the Zeeman algorithm
  !-------------------------------------------------
  PURE FUNCTION Is_ODZeeman( TC ) RESULT( ODZeeman )
    TYPE(ODPS_type), INTENT(IN) :: TC
    LOGICAL :: ODZeeman
    ODZeeman = ( TC%Group_Index == ODPS_gINDEX_ZSSMIS .OR. &
                 TC%Group_Index == ODPS_gINDEX_ZAMSUA      )
  END FUNCTION Is_ODZeeman


  !----------------------------------------------------------
  ! Obtain number of predictors, given an ODPS group index
  !----------------------------------------------------------
  PURE FUNCTION Get_NumOfZPredictors( gIndex ) RESULT( n_Predictors )
    INTEGER, INTENT(IN) :: gIndex
    INTEGER :: n_Predictors

    SELECT CASE ( gIndex )
      CASE ( ODPS_gINDEX_ZSSMIS )
        n_Predictors = MAX_N_PREDICTORS_ZSSMIS
      CASE ( ODPS_gINDEX_ZAMSUA )
        n_Predictors = MAX_N_PREDICTORS_ZAMSUA
      CASE DEFAULT
        n_Predictors = 0
    END SELECT

  END FUNCTION Get_NumOfZPredictors


  !----------------------------------------------------------
  ! Obtain number of compoents
  !----------------------------------------------------------
  PURE FUNCTION Get_NumOfZComponents() RESULT( n_Components )
    INTEGER :: n_Components
    n_Components = N_ZCOMPONENTS
  END FUNCTION Get_NumOfZComponents


  ! Obtain number of compoents
  !----------------------------------------------------------
  PURE FUNCTION Get_NumOfZAbsorbers() RESULT( n_Absorbers )
    INTEGER :: n_Absorbers
    n_Absorbers = N_ZABSORBERS
  END FUNCTION Get_NumOfZAbsorbers

END MODULE ODZeeman_AtmAbsorption
