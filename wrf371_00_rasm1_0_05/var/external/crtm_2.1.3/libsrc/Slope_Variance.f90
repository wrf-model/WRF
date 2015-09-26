!
! Helper module containing the slope variance routines for the
! CRTM implementation of FASTEM4
!
!
! CREATION HISTORY:
!       Written by:     Original FASTEM1/2/3 authors
!
!       Modified by:    Quanhua Liu, Quanhua.Liu@noaa.gov
!                       Stephen English, Stephen.English@metoffice.gov.uk
!                       July, 2009
!
!       Refactored by:  Paul van Delst, October 2010
!                       paul.vandelst@noaa.gov
!

MODULE Slope_Variance

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds     , ONLY: fp
  USE Hyperbolic_Step, ONLY: Step, Step_TL, Step_AD
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Data types
  PUBLIC :: iVar_type
  ! Science routines
  PUBLIC :: Compute_Slope_Variance
  PUBLIC :: Compute_Slope_Variance_TL
  PUBLIC :: Compute_Slope_Variance_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: Slope_Variance.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'

  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  REAL(fp), PARAMETER :: TWO  = 2.0_fp

  ! Wave slope variance parameters
  REAL(fp), PARAMETER :: VAR_COEFFS(2) = (/0.0030_fp, 0.00512_fp/)  ! Cox-Munk coeffs
  REAL(fp), PARAMETER :: F_COEFFS(3)   = (/1.0000_fp, 0.02000_fp, 0.30000_fp/)

  ! Scale factor for x-input to hyperbolic step function
  REAL(fp), PARAMETER :: XSCALE = 10000.0_fp


  ! --------------------------------------
  ! Structure definition to hold internal
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: iVar_type
    PRIVATE
    REAL(fp) :: vara  = ZERO
    REAL(fp) :: varb  = ZERO
    REAL(fp) :: Fterm = ZERO
    REAL(fp) :: x1 = ZERO, g1 = ZERO, var1 = ZERO
    REAL(fp) :: x2 = ZERO, g2 = ZERO
    REAL(fp) :: Variance = ZERO
  END TYPE iVar_type


CONTAINS


  ! =============================================
  ! Procedures to compute the wave slope variance
  ! =============================================
  ! Forward model
  SUBROUTINE Compute_Slope_Variance( &
    Frequency , &  ! Input
    Wind_Speed, &  ! Input
    iVar      , &  ! Internal variable output
    Variance    )  ! Output
    ! Arguments
    REAL(fp)       , INTENT(IN)  :: Frequency
    REAL(fp)       , INTENT(IN)  :: Wind_Speed
    TYPE(iVar_type), INTENT(OUT) :: iVar
    REAL(fp)       , INTENT(OUT) :: Variance
    ! Local variables
    REAL(fp) :: c
    ! Compute the frequency term
    iVar%Fterm   = (F_COEFFS(2)*Frequency + F_COEFFS(3))
    ! Compute the slope variances
    c = (VAR_COEFFS(1) + VAR_COEFFS(2)*Wind_Speed) * F_COEFFS(1)
    iVar%vara = c
    iVar%varb = c * iVar%Fterm
    ! Return required value
    ! ...First IF statement
    ! ...IF ( iVar%varb >= iVar%vara ) THEN
    iVar%x1 = XSCALE*(iVar%varb - iVar%vara)
    CALL Step(iVar%x1,iVar%g1)
    iVar%var1 = iVar%vara*iVar%g1 + iVar%varb*(ONE-iVar%g1)
    ! ...Second IF statement
    ! ...IF ( iVar%varb <= ZERO ) THEN
    iVar%x2 = XSCALE*iVar%varb
    CALL Step(iVar%x2,iVar%g2)
    Variance = iVar%var1*iVar%g2
  END SUBROUTINE Compute_Slope_Variance


  ! Tangent-linear model
  SUBROUTINE Compute_Slope_Variance_TL( &
    Wind_Speed_TL, &  ! Input
    iVar         , &  ! Internal variable input
    Variance_TL    )  ! Output
    ! Arguments
    REAL(fp)       , INTENT(IN)  :: Wind_Speed_TL
    TYPE(iVar_type), INTENT(IN)  :: iVar
    REAL(fp)       , INTENT(OUT) :: Variance_TL
    ! Local variables
    REAL(fp) :: c
    REAL(fp) :: vara_TL, varb_TL
    REAL(fp) :: x1_TL, g1_TL, var1_TL
    REAL(fp) :: x2_TL, g2_TL
    ! Compute slope variances
    c = VAR_COEFFS(2) * F_COEFFS(1)
    vara_TL = c * Wind_Speed_TL
    varb_TL = c * iVar%Fterm * Wind_Speed_TL
    ! Return required value
    ! ...First IF statement
    ! ...IF ( iVar%varb >= iVar%vara ) THEN
    x1_TL = XSCALE*(varb_TL - vara_TL)
    CALL Step_TL(iVar%x1,x1_TL,g1_TL)
    var1_TL = (iVar%vara - iVar%varb)*g1_TL + &
              iVar%g1*vara_TL + &
              (ONE-iVar%g1)*varb_TL
    ! ...Second IF statement
    ! ...IF ( iVar%varb <= ZERO ) THEN
    x2_TL = XSCALE*varb_TL
    CALL Step_TL(iVar%x2,x2_TL,g2_TL)
    Variance_TL = iVar%var1*g2_TL + iVar%g2*var1_TL
  END SUBROUTINE Compute_Slope_Variance_TL


  ! Adjoint model
  SUBROUTINE Compute_Slope_Variance_AD( &
    Variance_AD  , &  ! Input
    iVar         , &  ! Internal variable input
    Wind_Speed_AD  )  ! Output
    ! Arguments
    REAL(fp)       , INTENT(IN OUT) :: Variance_AD
    TYPE(iVar_type), INTENT(IN)     :: iVar
    REAL(fp)       , INTENT(IN OUT) :: Wind_Speed_AD
    ! Local variables
    REAL(fp) :: c
    REAL(fp) :: vara_AD, varb_AD
    REAL(fp) :: x1_AD, g1_AD, var1_AD
    REAL(fp) :: x2_AD, g2_AD
    ! Second IF statement
    ! ...IF ( iVar%varb <= ZERO ) THEN
    var1_AD = iVar%g2   * Variance_AD
    g2_AD   = iVar%var1 * Variance_AD
    Variance_AD = ZERO
    
    x2_AD = ZERO
    CALL Step_AD(iVar%x2, g2_AD, x2_AD)
    varb_AD = XSCALE * x2_AD

    ! First IF statement
    ! ...IF ( iVar%varb >= iVar%vara ) THEN
    varb_AD = varb_AD + (ONE-iVar%g1) * var1_AD
    vara_AD =                iVar%g1  * var1_AD
    g1_AD   = (iVar%vara - iVar%varb) * var1_AD
    
    x1_AD = ZERO
    CALL Step_AD(iVar%x1, g1_AD, x1_AD)
    
    vara_AD = vara_AD - XSCALE * x1_AD
    varb_AD = varb_AD + XSCALE * x1_AD
    
    ! Compute the adjoint of the slope variances
    c = VAR_COEFFS(2) * F_COEFFS(1)
    Wind_Speed_AD = Wind_Speed_AD + c * iVar%Fterm * varb_AD
    Wind_Speed_AD = Wind_Speed_AD + c * vara_AD
  END SUBROUTINE Compute_Slope_Variance_AD

END MODULE Slope_Variance
