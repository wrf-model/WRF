!
! Helper module containing the foam-related utility routines for the
! CRTM implementation of FASTEM4 and FASTEM5
!
!
! CREATION HISTORY:
!       Written by:     Original FASTEM1-5 authors
!
!       Refactored by:  Paul van Delst, November 2011
!                       paul.vandelst@noaa.gov
!

MODULE Foam_Utility_Module

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
  PUBLIC :: Foam_Coverage
  PUBLIC :: Foam_Coverage_TL
  PUBLIC :: Foam_Coverage_AD
  PUBLIC :: Foam_Reflectivity


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: Foam_Utility_Module.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'

  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  REAL(fp), PARAMETER :: TWO  = 2.0_fp
  

CONTAINS


  ! ===================================================================
  ! Foam coverage.
  !
  !   Monahan, E.C., and O'Muircheartaigh, I.G., (1986)
  !     Whitecaps and the passive remote sensing of the ocean surface,
  !     International Journal of Remote Sensing, 7, pp627-642.
  !
  ! The neutral stability condition is used here (i.e. the difference
  ! between the skin and air temperature is assumed to be zero) so
  ! that the form of the foam coverage equation is the same as in
  ! Tang (1974) and Liu et al. (1998)..
  !
  !   Liu, Q. et al. (1998) Monte Carlo simulations of the
  !     microwave emissivity of the sea surface.
  !     JGR, 103(C11), pp24983-24989
  !
  !   Tang, C. (1974) The effect of droplets in the air-sea
  !     transition zone on the sea brightness temperature.
  !     J. Phys. Oceanography, 4, pp579-593.
  !
  ! ===================================================================
  ! Forward model
  SUBROUTINE Foam_Coverage(FCCoeff, wind_speed, coverage)
    TYPE(FitCoeff_1D_type), INTENT(IN)  :: FCCoeff
    REAL(fp)              , INTENT(IN)  :: wind_speed
    REAL(fp)              , INTENT(OUT) :: coverage
    IF ( wind_speed < ZERO ) THEN
      coverage = ZERO
      RETURN
    END IF
    coverage = FCCoeff%C(1) * (wind_speed**FCCoeff%C(2))
  END SUBROUTINE Foam_Coverage
  
  ! Tangent-linear model
  SUBROUTINE Foam_Coverage_TL(FCCoeff, wind_speed, wind_speed_TL, coverage_TL)
    TYPE(FitCoeff_1D_type), INTENT(IN)  :: FCCoeff
    REAL(fp)              , INTENT(IN)  :: wind_speed
    REAL(fp)              , INTENT(IN)  :: wind_speed_TL
    REAL(fp)              , INTENT(OUT) :: coverage_TL
    IF ( wind_speed < ZERO ) THEN
      coverage_TL = ZERO
      RETURN
    END IF
    coverage_TL = FCCoeff%C(1)*FCCoeff%C(2) * (wind_speed**(FCCoeff%C(2)-ONE)) * wind_speed_TL
  END SUBROUTINE Foam_Coverage_TL

  ! Adjoint model
  SUBROUTINE Foam_Coverage_AD(FCCoeff, wind_speed, coverage_AD, wind_speed_AD)
    TYPE(FitCoeff_1D_type), INTENT(IN)     :: FCCoeff
    REAL(fp)              , INTENT(IN)     :: wind_speed     ! Input
    REAL(fp)              , INTENT(IN OUT) :: coverage_AD    ! Input
    REAL(fp)              , INTENT(IN OUT) :: wind_speed_AD  ! Output
    IF ( wind_speed < ZERO ) THEN
      coverage_AD = ZERO
      RETURN
    END IF
    wind_speed_AD = wind_speed_AD + &
                    FCCoeff%C(1)*FCCoeff%C(2) * (wind_speed**(FCCoeff%C(2)-ONE)) * coverage_AD
    coverage_AD = ZERO
  END SUBROUTINE Foam_Coverage_AD


  ! =============================================================
  ! Foam reflectivity
  !
  ! See section d in
  !
  !   Kazumori, M. et al. (2008) Impact Study of AMSR-E Radiances
  !     in the NCEP Global Data Assimilation System,
  !     Monthly Weather Review, 136, pp541-559
  !
  ! Function dependence is on zenith angle only so no TL
  ! or AD routine.
  ! =============================================================
  SUBROUTINE Foam_Reflectivity( &
    FRCoeff     , &
    Zenith_Angle, &
    Frequency   , &
    Rv          , &
    Rh            )
    ! Arguments
    TYPE(FitCoeff_1D_type), INTENT(IN)  :: FRCoeff
    REAL(fp)              , INTENT(IN)  :: Zenith_Angle
    REAL(fp)              , INTENT(IN)  :: Frequency
    REAL(fp)              , INTENT(OUT) :: Rv, Rh
    ! Local variables
    REAL(fp) :: factor
    
    ! The vertical component is a fixed value
    Rv = ONE - FRCoeff%C(1)  ! Fixed nadir emissivity
    
    ! The horizontal component uses a regression equation
    ! to compute a factor modifying the nadir emissivity
    factor = ONE + Zenith_Angle*(FRCoeff%C(2) + &
                     Zenith_Angle*(FRCoeff%C(3) + &
                       Zenith_Angle*FRCoeff%C(4)  )  )
    Rh = ONE - factor*FRCoeff%C(1)
    
    ! Frequency correction
    factor = FRCoeff%C(5) * EXP(FRCoeff%C(6)*Frequency)
    Rv = Rv * factor
    Rh = Rh * factor

  END SUBROUTINE Foam_Reflectivity  

END MODULE Foam_Utility_Module
