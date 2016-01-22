!
! Large_Scale_Correction_Module
!
! Module containing the large-scale correction procedures for the
! CRTM implementations of FASTEM4 and FASTEM5
!
! Equations (A5a) and (A5b) of
!
!   Liu, Q. et al. (2011) An Improved Fast Microwave Water
!     Emissivity Model, TGRSS, 49, pp1238-1250
!
! describes the fitting of the large-scale correction formulation.
! No explicit description of the data that was fitted is given.
!
!
! CREATION HISTORY:
!       Written by:     Original FASTEM authors
!
!       Refactored by:  Paul van Delst, November 2011
!                       paul.vandelst@noaa.gov
!

MODULE Large_Scale_Correction_Module

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds, &
        ONLY: fp
  USE FitCoeff_Define, &
        ONLY: FitCoeff_3D_type
  USE CRTM_Interpolation, &
        ONLY: NPTS        , &
              LPoly_type  , &
              find_index  , &
              interp_4D   , &
              interp_4D_TL, &
              interp_4D_AD, &
              Clear_LPoly , &
              LPoly       , &
              LPoly_TL    , &
              LPoly_AD
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Data types
  PUBLIC :: iVar_type
  ! Science routines
  PUBLIC :: Large_Scale_Correction
  PUBLIC :: Large_Scale_Correction_TL
  PUBLIC :: Large_Scale_Correction_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: Large_Scale_Correction_Module.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'

  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  REAL(fp), PARAMETER :: TWO  = 2.0_fp
  ! Number of "final" coefficients per polarisation
  ! Corresponds with middle dimension of LSCCOEFF data
  INTEGER, PARAMETER :: N_ZCOEFFS = 6
  ! Number of look-up table dimensions
  INTEGER, PARAMETER :: N_LUTDIMS = 4


  ! --------------------------------------
  ! Structure definition to hold internal
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  ! The interpolation routine structure
  TYPE :: iInterp_type
    TYPE(LPoly_type) :: lp        ! The interpolating polynomial
    INTEGER          :: i1, i2    ! The LUT interpolation indices
    LOGICAL          :: outbound  ! The LUT interpolation boundary check
    REAL(fp)         :: xint      ! The interpolation point
    REAL(fp)         :: x(NPTS)   ! The data to be interpolated
  END TYPE iInterp_type


  TYPE :: iVar_type
    PRIVATE
    ! Direct inputs
    REAL(fp) :: wind_speed = ZERO
    ! Coefficient validity flag
    LOGICAL :: zcoeff_invalid = .TRUE.
    ! Intermediate variables
    REAL(fp) :: sec_z  = ZERO
    REAL(fp) :: zcoeff_v(N_ZCOEFFS) = ZERO
    REAL(fp) :: zcoeff_h(N_ZCOEFFS) = ZERO
    ! Look-up table interpolation data
    TYPE(iInterp_type) :: lsci(N_LUTDIMS)
  END TYPE iVar_type


CONTAINS


  ! =============================================================
  ! Procedures to compute the reflectivity large scale correction
  ! =============================================================
  ! Forward model
  SUBROUTINE Large_Scale_Correction( &
    LSCCoeff  , &  ! Input
    Frequency , &  ! Input
    cos_Z     , &  ! Input
    Wind_Speed, &  ! Input
    Rv_Large  , &  ! Output
    Rh_Large  , &  ! Output
    iVar        )  ! Internal variable output
    ! Arguments
    TYPE(FitCoeff_3D_type), INTENT(IN)     :: LSCCoeff
    REAL(fp),               INTENT(IN)     :: Frequency
    REAL(fp),               INTENT(IN)     :: cos_Z
    REAL(fp),               INTENT(IN)     :: Wind_Speed
    REAL(fp),               INTENT(OUT)    :: Rv_Large
    REAL(fp),               INTENT(OUT)    :: Rh_Large
    TYPE(iVar_type),        INTENT(IN OUT) :: iVar

    ! Setup
    iVar%zcoeff_invalid = .TRUE.
    iVar%wind_speed     = Wind_Speed
    iVar%sec_z          = ONE/cos_Z

    ! Compute the frequency polynomial coefficients
    CALL Compute_ZCoeff(LSCCoeff%C(:,:,1), Frequency, iVar%zcoeff_v)
    CALL Compute_ZCoeff(LSCCoeff%C(:,:,2), Frequency, iVar%zcoeff_h)
    iVar%zcoeff_invalid = .FALSE.

    ! Compute the reflectivity corrections
    Rv_Large = iVar%zcoeff_v(1)                              + &
               iVar%zcoeff_v(2) * iVar%sec_z                 + &
               iVar%zcoeff_v(3) * iVar%sec_z**2              + &
               iVar%zcoeff_v(4) * iVar%wind_speed            + &
               iVar%zcoeff_v(5) * iVar%wind_speed**2         + &
               iVar%zcoeff_v(6) * iVar%wind_speed*iVar%sec_z

    Rh_Large = iVar%zcoeff_h(1)                              + &
               iVar%zcoeff_h(2) * iVar%sec_z                 + &
               iVar%zcoeff_h(3) * iVar%sec_z**2              + &
               iVar%zcoeff_h(4) * iVar%wind_speed            + &
               iVar%zcoeff_h(5) * iVar%wind_speed**2         + &
               iVar%zcoeff_h(6) * iVar%wind_speed*iVar%sec_z

  CONTAINS

    SUBROUTINE Compute_ZCoeff(coeff,frequency,zcoeff)
      REAL(fp), INTENT(IN)  :: coeff(:,:)
      REAL(fp), INTENT(IN)  :: frequency
      REAL(fp), INTENT(OUT) :: zcoeff(:)
      INTEGER :: i
      DO i = 1, SIZE(zcoeff)
        zcoeff(i) = coeff(1,i) + frequency*(coeff(2,i) + frequency*coeff(3,i))
      END DO
    END SUBROUTINE Compute_ZCoeff

  END SUBROUTINE Large_Scale_Correction


  ! Tangent-linear model
  SUBROUTINE Large_Scale_Correction_TL( &
    Wind_Speed_TL, &  ! Input
    Rv_Large_TL  , &  ! Output
    Rh_Large_TL  , &  ! Output
    iVar           )  ! Internal variable output
    ! Arguments
    REAL(fp),               INTENT(IN)  :: Wind_Speed_TL
    REAL(fp),               INTENT(OUT) :: Rv_Large_TL
    REAL(fp),               INTENT(OUT) :: Rh_Large_TL
    TYPE(iVar_type),        INTENT(IN)  :: iVar

    ! Setup
    IF ( iVar%zcoeff_invalid ) THEN
      Rv_Large_TL = ZERO
      Rh_Large_TL = ZERO
      RETURN
    END IF

    ! Compute the tangent-linear reflectivity corrections
    Rv_Large_TL = (       iVar%zcoeff_v(4)                    + &
                   (TWO * iVar%zcoeff_v(5) * iVar%wind_speed) + &
                   (      iVar%zcoeff_v(6) * iVar%sec_z     )   ) * Wind_Speed_TL

    Rh_Large_TL = (       iVar%zcoeff_h(4)                    + &
                   (TWO * iVar%zcoeff_h(5) * iVar%wind_speed) + &
                   (      iVar%zcoeff_h(6) * iVar%sec_z     )   ) * Wind_Speed_TL

  END SUBROUTINE Large_Scale_Correction_TL


  ! Adjoint model
  SUBROUTINE Large_Scale_Correction_AD( &
    Rv_Large_AD  , &  ! Input
    Rh_Large_AD  , &  ! Input
    Wind_Speed_AD, &  ! Output
    iVar           )  ! Internal variable output
    ! Arguments
    REAL(fp),               INTENT(IN OUT) :: Rv_Large_AD
    REAL(fp),               INTENT(IN OUT) :: Rh_Large_AD
    REAL(fp),               INTENT(IN OUT) :: Wind_Speed_AD
    TYPE(iVar_type),        INTENT(IN)     :: iVar

    ! Setup
    IF ( iVar%zcoeff_invalid ) THEN
      Rv_Large_AD = ZERO
      Rh_Large_AD = ZERO
      RETURN
    END IF

    ! Compute the adjoint reflectivity corrections
    ! ...Horizontal polarisation
    Wind_Speed_AD = Wind_Speed_AD + &
                    (       iVar%zcoeff_h(4)                    + &
                     (TWO * iVar%zcoeff_h(5) * iVar%wind_speed) + &
                     (      iVar%zcoeff_h(6) * iVar%sec_z     )   ) * Rh_Large_AD
    Rh_Large_AD = ZERO
    ! ...Vertical polarisation
    Wind_Speed_AD = Wind_Speed_AD + &
                    (       iVar%zcoeff_v(4)                    + &
                     (TWO * iVar%zcoeff_v(5) * iVar%wind_speed) + &
                     (      iVar%zcoeff_v(6) * iVar%sec_z     )   ) * Rv_Large_AD
    Rv_Large_AD = ZERO

  END SUBROUTINE Large_Scale_Correction_AD

END MODULE Large_Scale_Correction_Module
