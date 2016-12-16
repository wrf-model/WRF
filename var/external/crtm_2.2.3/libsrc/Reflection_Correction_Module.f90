!
! Helper module conmtaining the reflection correction routines for the
! CRTM implementation of FASTEM4 and FASTEM5
!
!
! CREATION HISTORY:
!       Written by:     Original FASTEM1/2/3 authors
!
!       Modified by:    Quanhua Liu, Quanhua.Liu@noaa.gov
!                       Stephen English, Stephen.English@metoffice.gov.uk
!                       July, 2009
!
!       Modified by:    Quanhua Liu, Quanhua.Liu@noaa.gov
!                       Stephen English, Stephen.English@metoffice.gov.uk
!                       August 16, 2011
!
!       Refactored by:  Paul van Delst, November 2011
!                       paul.vandelst@noaa.gov
!

MODULE Reflection_Correction_Module

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds     , ONLY: fp
  USE Slope_Variance , ONLY: svVar_type => iVar_type, &
                             Compute_Slope_Variance   , &
                             Compute_Slope_Variance_TL, &
                             Compute_Slope_Variance_AD 
  USE FitCoeff_Define, ONLY: FitCoeff_3D_type
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Data types
  PUBLIC :: iVar_type
  ! Science routines
  PUBLIC :: Reflection_Correction
  PUBLIC :: Reflection_Correction_TL
  PUBLIC :: Reflection_Correction_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: Reflection_Correction_Module.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'

  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  REAL(fp), PARAMETER :: TWO  = 2.0_fp
    
  ! Rx_Rough regression equation parameters
  ! ...Number of predictors
  INTEGER, PARAMETER :: N_PREDICTORS = 7
  ! ...Number of summation loops and equation terms
  INTEGER, PARAMETER :: N_TERMS = 3


  ! --------------------------------------
  ! Structure definition to hold internal
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: iVar_type
    PRIVATE
    ! Forward model input values
    REAL(fp) :: Transmittance = ZERO
    REAL(fp) :: Wind_Speed    = ZERO
    REAL(fp) :: cos_z         = ZERO
    REAL(fp) :: Frequency     = ZERO
    ! Optical depth
    REAL(fp) :: od = ZERO
    ! Predictors
    REAL(fp) :: odx(N_TERMS-1)   = ZERO
    REAL(fp) :: zx(N_PREDICTORS) = ZERO
    ! Reflectance variables
    REAL(fp) :: Rv_Rough = ZERO
    REAL(fp) :: Rh_Rough = ZERO
    REAL(fp) :: Rv_Mod   = ZERO
    REAL(fp) :: Rh_Mod   = ZERO
    ! Components
    TYPE(svVar_type) :: svVar
  END TYPE iVar_type

  
CONTAINS


  ! ===============================================================
  ! Use the transmittance to compute anisotropic downward radiation 
  ! effect through a corrected surface reflection.
  ! ===============================================================
  
  ! Forward model
  SUBROUTINE Reflection_Correction( &
    RCCoeff      , &  ! Input
    Frequency    , &  ! Input
    cos_z        , &  ! Input
    Wind_Speed   , &  ! Input
    Transmittance, &  ! Input
    Rv_Mod       , &  ! Output
    Rh_Mod       , &  ! Output
    iVar           )  ! Internal variable output
    ! Arguments
    TYPE(FitCoeff_3D_type), INTENT(IN)     :: RCCoeff
    REAL(fp)              , INTENT(IN)     :: Frequency
    REAL(fp)              , INTENT(IN)     :: cos_Z
    REAL(fp)              , INTENT(IN)     :: Wind_Speed
    REAL(fp)              , INTENT(IN)     :: Transmittance
    REAL(fp)              , INTENT(OUT)    :: Rv_Mod
    REAL(fp)              , INTENT(OUT)    :: Rh_Mod
    TYPE(iVar_type)       , INTENT(IN OUT) :: iVar
    ! Local variables
    REAL(fp) :: variance
    INTEGER :: i


    ! Save forward input variables for TL and AD calculations
    iVar%Transmittance = Transmittance 
    iVar%Wind_Speed    = Wind_Speed    
    iVar%cos_z         = cos_z         
    iVar%Frequency     = Frequency    


    ! Compute the wave slope variance
    CALL Compute_Slope_Variance( Frequency, Wind_Speed, iVar%svVar, Variance )


    ! Compute surface to space optical depth predictors
    iVar%od     = -LOG(Transmittance) * cos_z
    iVar%odx(1) = LOG(iVar%od)
    iVar%odx(2) = iVar%odx(1)**2


    ! Compute effective angle predictors
    iVar%zx(1) = ONE
    iVar%zx(2) = variance
    iVar%zx(4) = ONE / cos_z
    iVar%zx(3) = iVar%zx(2) * iVar%zx(4)
    iVar%zx(5) = iVar%zx(3) * iVar%zx(3)
    iVar%zx(6) = iVar%zx(4) * iVar%zx(4)
    iVar%zx(7) = iVar%zx(2) * iVar%zx(2)

    
    ! Compute the rough surface reflectivity
    iVar%Rv_Rough = ONE
    iVar%Rh_Rough = ONE
    DO i = 1, N_PREDICTORS
      iVar%Rv_Rough = iVar%Rv_Rough + iVar%zx(i) * (             RCCoeff%C(1,i,1) + &
                                                     iVar%odx(1)*RCCoeff%C(2,i,1) + &
                                                     iVar%odx(2)*RCCoeff%C(3,i,1)   )

      iVar%Rh_Rough = iVar%Rh_Rough + iVar%zx(i) * (             RCCoeff%C(1,i,2) + &
                                                     iVar%odx(1)*RCCoeff%C(2,i,2) + &
                                                     iVar%odx(2)*RCCoeff%C(3,i,2)   )
    END DO

    
    ! Compute the reflectivity modifier
    Rv_Mod = (ONE - Transmittance**iVar%Rv_Rough) / (ONE - Transmittance)
    Rh_Mod = (ONE - Transmittance**iVar%Rh_Rough) / (ONE - Transmittance) 
    ! ...and save it
    iVar%Rv_Mod = Rv_Mod
    iVar%Rh_Mod = Rh_Mod 

  END SUBROUTINE Reflection_Correction


  ! Tangent-linear model
  SUBROUTINE Reflection_Correction_TL( &
    RCCoeff         , &  ! Input
    Wind_Speed_TL   , &  ! Input
    Transmittance_TL, &  ! Input
    Rv_Mod_TL       , &  ! Output
    Rh_Mod_TL       , &  ! Output
    iVar              )  ! Internal variable input
    ! Arguments
    TYPE(FitCoeff_3D_type), INTENT(IN)  :: RCCoeff
    REAL(fp)              , INTENT(IN)  :: Wind_Speed_TL
    REAL(fp)              , INTENT(IN)  :: Transmittance_TL
    REAL(fp)              , INTENT(OUT) :: Rv_Mod_TL
    REAL(fp)              , INTENT(OUT) :: Rh_Mod_TL
    TYPE(iVar_type)       , INTENT(IN)  :: iVar
    ! Local variables
    REAL(fp) :: variance_TL, od_TL
    REAL(fp) :: odx_TL(N_TERMS-1), zx_TL(N_PREDICTORS)
    REAL(fp) :: Rv_Rough_TL, Rh_Rough_TL
    INTEGER :: i

    
    ! Compute the wave slope variance
    CALL Compute_Slope_Variance_TL( Wind_Speed_TL, iVar%svVar, Variance_TL )


    ! Compute surface to space optical depth predictors
    od_TL     = -Transmittance_TL * iVar%cos_z / iVar%Transmittance
    odx_TL(1) = od_TL / iVar%od
    odx_TL(2) = TWO * odx_TL(1) * iVar%odx(1)


    ! Compute effective angle predictors
    zx_TL    = ZERO
    zx_TL(1) = ZERO
    zx_TL(2) = variance_TL
    zx_TL(4) = ZERO
    zx_TL(3) = zx_TL(2) * iVar%zx(4) 
    zx_TL(5) = TWO * zx_TL(3) * iVar%zx(3)
    zx_TL(6) = TWO * zx_TL(4) * iVar%zx(4)
    zx_TL(7) = TWO * zx_TL(2) * iVar%zx(2)


    ! Compute the rough surface reflectivity
    Rv_Rough_TL = ZERO
    Rh_Rough_TL = ZERO
    DO i = 1, N_PREDICTORS
      Rv_Rough_TL = Rv_Rough_TL + zx_TL(i)   * (            RCCoeff%C(1,i,1) + &
                                                iVar%odx(1)*RCCoeff%C(2,i,1) + &
                                                iVar%odx(2)*RCCoeff%C(3,i,1)   ) + &
                                  iVar%zx(i) * (odx_TL(1)*RCCoeff%C(2,i,1) + &
                                                odx_TL(2)*RCCoeff%C(3,i,1)   )

      Rh_Rough_TL = Rh_Rough_TL + zx_TL(i)   * (            RCCoeff%C(1,i,2) + &
                                                iVar%odx(1)*RCCoeff%C(2,i,2) + &
                                                iVar%odx(2)*RCCoeff%C(3,i,2)   ) + &
                                  iVar%zx(i) * (odx_TL(1)*RCCoeff%C(2,i,2) + &
                                                odx_TL(2)*RCCoeff%C(3,i,2)   )
    END DO


    ! Compute the reflectivity modifier
    Rv_Mod_TL = ((iVar%Rv_Mod - iVar%Rv_Rough*(iVar%Transmittance**(iVar%Rv_Rough-ONE)))*Transmittance_TL - &
                 (LOG(iVar%Transmittance)*(iVar%Transmittance**iVar%Rv_Rough))*Rv_Rough_TL) / &
                (ONE - iVar%Transmittance)
    Rh_Mod_TL = ((iVar%Rh_Mod - iVar%Rh_Rough*(iVar%Transmittance**(iVar%Rh_Rough-ONE)))*Transmittance_TL - &
                 (LOG(iVar%Transmittance)*(iVar%Transmittance**iVar%Rh_Rough))*Rh_Rough_TL) / &
                (ONE - iVar%Transmittance)
    
  END SUBROUTINE Reflection_Correction_TL


  ! Adjoint model
  SUBROUTINE Reflection_Correction_AD( &
    RCCoeff         , &  ! Input
    Rv_Mod_AD       , &  ! Input
    Rh_Mod_AD       , &  ! Input
    Wind_Speed_AD   , &  ! Output
    Transmittance_AD, &  ! Output
    iVar              )  ! Internal variable input
    ! Arguments
    TYPE(FitCoeff_3D_type), INTENT(IN)     :: RCCoeff
    REAL(fp)              , INTENT(IN OUT) :: Rv_Mod_AD
    REAL(fp)              , INTENT(IN OUT) :: Rh_Mod_AD
    REAL(fp)              , INTENT(IN OUT) :: Transmittance_AD
    REAL(fp)              , INTENT(IN OUT) :: Wind_Speed_AD
    TYPE(iVar_type)       , INTENT(IN)     :: iVar
    ! Local variables
    INTEGER :: i
    REAL(fp) :: Rv_Rough_AD, Rh_Rough_AD
    REAL(fp) :: odx_AD(N_TERMS-1), zx_AD(N_PREDICTORS)
    REAL(fp) :: od_AD
    REAL(fp) :: variance_AD


    ! Compute the reflectivity modifier
    Rv_Rough_AD = ZERO
    Rh_Rough_AD = ZERO
    ! ...for Rh_Mod
    Transmittance_AD = Transmittance_AD + &
                       (iVar%Rh_Mod - iVar%Rh_Rough * iVar%Transmittance**(iVar%Rh_Rough-ONE)) * Rh_Mod_AD / &
                       (ONE - iVar%Transmittance)
    Rh_Rough_AD = Rh_Rough_AD - &
                  (iVar%Transmittance**iVar%Rh_Rough * LOG(iVar%Transmittance)) * Rh_Mod_AD / &
                  (ONE - iVar%Transmittance)
    Rh_Mod_AD = ZERO
    ! ...for Rv_Mod
    Transmittance_AD = Transmittance_AD + &
                       (iVar%Rv_Mod - iVar%Rv_Rough * iVar%Transmittance**(iVar%Rv_Rough-ONE)) * Rv_Mod_AD / &
                       (ONE - iVar%Transmittance)
    Rv_Rough_AD = Rv_Rough_AD - &
                  (iVar%Transmittance**iVar%Rv_Rough * LOG(iVar%Transmittance)) * Rv_Mod_AD / &
                  (ONE - iVar%Transmittance)
    Rv_Mod_AD = ZERO


    ! Compute the rough surface reflectivity
    odx_AD = ZERO
    zx_AD  = ZERO
    DO i = 1, N_PREDICTORS
      ! ...Rh_Rough components
      odx_AD(2) = odx_AD(2) + iVar%zx(i)*RCCoeff%C(3,i,2)*Rh_Rough_AD
      odx_AD(1) = odx_AD(1) + iVar%zx(i)*RCCoeff%C(2,i,2)*Rh_Rough_AD
      zx_AD(i) = zx_AD(i) + (            RCCoeff%C(1,i,2) + &
                             iVar%odx(1)*RCCoeff%C(2,i,2) + &
                             iVar%odx(2)*RCCoeff%C(3,i,2)   )*Rh_Rough_AD
      ! ...Rv_Rough components
      odx_AD(2) = odx_AD(2) + iVar%zx(i)*RCCoeff%C(3,i,1)*Rv_Rough_AD
      odx_AD(1) = odx_AD(1) + iVar%zx(i)*RCCoeff%C(2,i,1)*Rv_Rough_AD
      zx_AD(i) = zx_AD(i) + (            RCCoeff%C(1,i,1) + &
                             iVar%odx(1)*RCCoeff%C(2,i,1) + &
                             iVar%odx(2)*RCCoeff%C(3,i,1)   )*Rv_Rough_AD
    END DO
    Rv_Rough_AD = ZERO
    Rh_Rough_AD = ZERO


    ! Compute adjoint of effective angle predictors
    ! ...(7)
    zx_AD(2) = zx_AD(2) + TWO*iVar%zx(2)*zx_AD(7)
    zx_AD(7) = ZERO
    ! ...(6)
    zx_AD(4) = zx_AD(4) + TWO*iVar%zx(4)*zx_AD(6)
    zx_AD(6) = ZERO    
    ! ...(5)
    zx_AD(3) = zx_AD(3) + TWO*iVar%zx(3)*zx_AD(5)
    zx_AD(5) = ZERO    
    ! ...(3)
    zx_AD(2) = zx_AD(2) + iVar%zx(4)*zx_AD(3)
    zx_AD(3) = ZERO    
    ! ...(4)
    zx_AD(4) = ZERO    
    ! ...(2)
    variance_AD = zx_AD(2)
    zx_AD(2) = ZERO    
    ! ...(1)
    zx_AD(1) = ZERO


    ! Compute adjoint of surface to space optical depth predictors
    odx_AD(1) = odx_AD(1) + TWO*iVar%odx(1)*odx_AD(2)
    odx_AD(2) = ZERO
    od_AD = odx_AD(1) / iVar%od
    odx_AD(1) = ZERO
    Transmittance_AD = Transmittance_AD - od_AD*iVar%cos_z/iVar%Transmittance
    

    ! Compute the wave slope variance
    CALL Compute_Slope_Variance_AD( variance_AD, iVar%svVar, Wind_Speed_AD )

  END SUBROUTINE Reflection_Correction_AD

END MODULE Reflection_Correction_Module
