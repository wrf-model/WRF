!
! Small_Scale_Correction_Module
!
! Module containing the small-scale correction procedures for the 
! CRTM implementations of FASTEM4 and FASTEM5
!
! Equation (A4) of
!
!   Liu, Q. et al. (2011) An Improved Fast Microwave Water
!     Emissivity Model, TGRSS, 49, pp1238-1250
!
! describes the fitting of the small-scale correction formulation
! given in equation (17a,b) of
!
!   Liu, Q. et al. (1998) Monte Carlo simulations of the microwave
!     emissivity of the sea surface, JGR, 103, pp24983-24989
!
! and originally in equation (30) of
!
!   Guissard,A. and P.Sobieski (1987) An approximate model
!     for the microwave brightness temperature of the sea,
!     Int.J.Rem.Sens., 8, pp1607-1627.
!
!
! CREATION HISTORY:
!       Written by:     Original FASTEM authors
!
!       Refactored by:  Paul van Delst, November 2011
!                       paul.vandelst@noaa.gov
!

MODULE Small_Scale_Correction_Module

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds     , ONLY: fp
  USE FitCoeff_Define, ONLY: FitCoeff_1D_type
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Data types
  PUBLIC :: iVar_type
  ! Science routines
  PUBLIC :: Small_Scale_Correction
  PUBLIC :: Small_Scale_Correction_TL
  PUBLIC :: Small_Scale_Correction_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: Small_Scale_Correction_Module.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'

  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  REAL(fp), PARAMETER :: TWO  = 2.0_fp
  ! Minimum and maximum frequency
  REAL(fp), PARAMETER :: MIN_FREQUENCY = 1.4_fp
  REAL(fp), PARAMETER :: MAX_FREQUENCY = 200.0_fp
  ! Minimum and maximum wind speed
  REAL(fp), PARAMETER :: MIN_WIND_SPEED = 0.3_fp
  REAL(fp), PARAMETER :: MAX_WIND_SPEED = 35.0_fp


  ! --------------------------------------
  ! Structure definition to hold internal
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: iVar_type
    PRIVATE
    ! Direct inputs
    REAL(fp) :: wind_speed = ZERO
    REAL(fp) :: frequency  = ZERO
    ! Flag to indicate if wind spped outside limits
    LOGICAL  :: wind_speed_limited = .FALSE.
    ! Intermediate variables
    REAL(fp) :: f2     = ZERO
    REAL(fp) :: y      = ZERO
    REAL(fp) :: cos2_z = ZERO
    ! Final result (since equation is an exponential)
    REAL(fp) :: correction = ZERO
  END TYPE iVar_type


CONTAINS


  ! =============================================================
  ! Procedures to compute the reflectivity small scale correction
  ! =============================================================
  ! Forward model
  SUBROUTINE Small_Scale_Correction( &
    SSCCoeff  , &  ! Input
    Frequency , &  ! Input
    cos_Z     , &  ! Input
    Wind_Speed, &  ! Input
    Correction, &  ! Output
    iVar        )  ! Internal variable output
    ! Arguments
    TYPE(FitCoeff_1D_type), INTENT(IN)     :: SSCCoeff
    REAL(fp),               INTENT(IN)     :: Frequency
    REAL(fp),               INTENT(IN)     :: cos_Z
    REAL(fp),               INTENT(IN)     :: Wind_Speed
    REAL(fp),               INTENT(OUT)    :: Correction
    TYPE(iVar_type),        INTENT(IN OUT) :: iVar
    ! Local variables
    REAL(fp) :: w2
    
    ! Check input
    iVar%frequency = Frequency
    IF ( iVar%frequency < MIN_FREQUENCY ) iVar%frequency = MIN_FREQUENCY
    IF ( iVar%frequency > MAX_FREQUENCY ) iVar%frequency = MAX_FREQUENCY
    iVar%wind_speed         = Wind_Speed
    iVar%wind_speed_limited = .FALSE.
    IF ( iVar%wind_speed < MIN_WIND_SPEED ) THEN
      iVar%wind_speed         = MIN_WIND_SPEED
      iVar%wind_speed_limited = .TRUE.
    END IF
    IF ( iVar%wind_speed > MAX_WIND_SPEED ) THEN
      iVar%wind_speed         = MAX_WIND_SPEED
      iVar%wind_speed_limited = .TRUE.
    END IF

    ! Compute correction
    iVar%f2 = iVar%frequency**2
    w2      = iVar%wind_speed**2
    ! ...Intermediate term regression equation   
    iVar%y = &
      (SSCCoeff%C(1) * iVar%wind_speed    * iVar%frequency) + &
      (SSCCoeff%C(2) * iVar%wind_speed    * iVar%f2       ) + &
      (SSCCoeff%C(3) * w2                 * iVar%frequency) + &
      (SSCCoeff%C(4) * w2                 * iVar%f2       ) + &
      (SSCCoeff%C(5) * w2                 / iVar%frequency) + &
      (SSCCoeff%C(6) * w2                 / iVar%f2       ) + &
      (SSCCoeff%C(7) * iVar%wind_speed                    ) + &
      (SSCCoeff%C(8) * w2                                 )
    ! ... The correction
    iVar%cos2_z     = cos_Z**2  
    iVar%correction = EXP(-iVar%y*iVar%cos2_z)
    Correction = iVar%correction

  END SUBROUTINE Small_Scale_Correction


  ! Tangent-linear model
  SUBROUTINE Small_Scale_Correction_TL( &
    SSCCoeff     , &  ! Input
    Wind_Speed_TL, &  ! TL input
    Correction_TL, &  ! TL output
    iVar           )  ! Internal variable input
    ! Arguments
    TYPE(FitCoeff_1D_type), INTENT(IN)  :: SSCCoeff
    REAL(fp),               INTENT(IN)  :: Wind_Speed_TL
    REAL(fp),               INTENT(OUT) :: Correction_TL
    TYPE(iVar_type),        INTENT(IN)  :: iVar
    ! Local variables
    REAL(fp) :: y_TL
    REAL(fp) :: two_w
    
    ! Check input
    IF ( iVar%wind_speed_limited ) THEN
      Correction_TL = ZERO
      RETURN
    END IF

    ! Compute tangent-linear intermediate term
    two_w = TWO * iVar%wind_speed
    y_TL = &
      ( (SSCCoeff%C(1) * iVar%frequency        ) + &
        (SSCCoeff%C(2) * iVar%f2               ) + &
        (SSCCoeff%C(3) * two_w * iVar%frequency) + &
        (SSCCoeff%C(4) * two_w * iVar%f2       ) + &
        (SSCCoeff%C(5) * two_w / iVar%frequency) + &
        (SSCCoeff%C(6) * two_w / iVar%f2       ) + &
         SSCCoeff%C(7)                           + &
        (SSCCoeff%C(8) * two_w                 )   ) * Wind_Speed_TL

    ! Compute the tangent-linear correction
    Correction_TL = -iVar%cos2_z * iVar%correction * y_TL

  END SUBROUTINE Small_Scale_Correction_TL
  

  ! Adjoint model
  SUBROUTINE Small_Scale_Correction_AD( &
    SSCCoeff     , &  ! Input
    Correction_AD, &  ! AD input
    Wind_Speed_AD, &  ! AD output
    iVar           )  ! Internal variable input
    TYPE(FitCoeff_1D_type), INTENT(IN)     :: SSCCoeff
    REAL(fp),               INTENT(IN OUT) :: Correction_AD
    REAL(fp),               INTENT(IN OUT) :: Wind_Speed_AD
    TYPE(iVar_type),        INTENT(IN)     :: iVar
    ! Local variables
    REAL(fp) :: y_AD
    REAL(fp) :: two_w

    ! Check input
    IF ( iVar%wind_speed_limited ) THEN
      Correction_AD = ZERO
      RETURN
    END IF
        
    ! Adjoint of correction
    y_AD = -iVar%cos2_z * iVar%correction * Correction_AD
    Correction_AD = ZERO
    
    ! Adjoint of intermediate term
    two_w = TWO * iVar%wind_speed
    Wind_Speed_AD = Wind_Speed_AD + & 
                    ( (SSCCoeff%C(1) * iVar%frequency        ) + &
                      (SSCCoeff%C(2) * iVar%f2               ) + &
                      (SSCCoeff%C(3) * two_w * iVar%frequency) + &
                      (SSCCoeff%C(4) * two_w * iVar%f2       ) + &
                      (SSCCoeff%C(5) * two_w / iVar%frequency) + &
                      (SSCCoeff%C(6) * two_w / iVar%f2       ) + &
                       SSCCoeff%C(7)                           + &
                      (SSCCoeff%C(8) * two_w                 )   ) * y_AD

  END SUBROUTINE Small_Scale_Correction_AD

END MODULE Small_Scale_Correction_Module
