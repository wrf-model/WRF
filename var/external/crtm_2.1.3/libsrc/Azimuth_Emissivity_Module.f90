!
! Helper module containing the azimuth emissivity routines for the
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
!       Refactored by:  Paul van Delst, December 2011
!                       paul.vandelst@noaa.gov
!

MODULE Azimuth_Emissivity_Module

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds     , ONLY: fp
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
  PUBLIC :: Azimuth_Emissivity
  PUBLIC :: Azimuth_Emissivity_TL
  PUBLIC :: Azimuth_Emissivity_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: Azimuth_Emissivity_Module.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'

  REAL(fp), PARAMETER :: ZERO  = 0.0_fp
  REAL(fp), PARAMETER :: ONE   = 1.0_fp
  REAL(fp), PARAMETER :: TWO   = 2.0_fp
  REAL(fp), PARAMETER :: THREE = 3.0_fp
  REAL(fp), PARAMETER :: PI = 3.141592653589793238462643383279_fp
  REAL(fp), PARAMETER :: DEGREES_TO_RADIANS = PI / 180.0_fp

  ! Dimensions
  ! ...Number of component predictors for harmonic coefficients
  INTEGER, PARAMETER :: N_PREDICTORS = 10
  ! ...Number of Stokes parameters
  INTEGER, PARAMETER :: N_STOKES = 4
  ! ...The number of harmonics considered in the trignometric parameterisation
  INTEGER, PARAMETER :: N_HARMONICS = 3
  
  
  ! --------------------------------------
  ! Structure definition to hold internal
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: iVar_type
    PRIVATE
    ! Direct inputs
    REAL(fp) :: wind_speed = ZERO
    REAL(fp) :: frequency  = ZERO
    ! Derived inputs
    REAL(fp) :: sec_z = ZERO
    ! Cosine and sine of the harmonic angle, m*phi
    REAL(fp) :: cos_angle(N_HARMONICS) = ZERO
    REAL(fp) :: sin_angle(N_HARMONICS) = ZERO
    ! Intermediate variables
    REAL(fp) :: trig_coeff(N_STOKES, N_HARMONICS) = ZERO
   END TYPE iVar_type

  
CONTAINS


  ! ===========================================================
  ! Compute emissivity as a function of relative azimuth angle.
  ! ===========================================================
  
  ! Forward model
  SUBROUTINE Azimuth_Emissivity( &
    AZCoeff      , &  ! Input
    Wind_Speed   , &  ! Input
    Azimuth_Angle, &  ! Input
    Frequency    , &  ! Input
    cos_z        , &  ! Input
    e_Azimuth    , &  ! Output
    iVar           )  ! Internal variable output
    ! Arguments
    TYPE(FitCoeff_3D_type), INTENT(IN)     :: AZCoeff
    REAL(fp)              , INTENT(IN)     :: Wind_Speed   
    REAL(fp)              , INTENT(IN)     :: Azimuth_Angle
    REAL(fp)              , INTENT(IN)     :: Frequency    
    REAL(fp)              , INTENT(IN)     :: cos_z        
    REAL(fp)              , INTENT(OUT)    :: e_Azimuth(:)
    TYPE(iVar_type)       , INTENT(IN OUT) :: iVar
    ! Local variables
    INTEGER :: i, m
    REAL(fp) :: phi, angle
    REAL(fp) :: predictor(N_PREDICTORS)

    ! Initialise output
    e_Azimuth = ZERO

    ! Save inputs for TL and AD calls
    iVar%wind_speed = Wind_Speed
    iVar%frequency  = Frequency
    iVar%sec_z      = ONE/cos_z
    
    ! Convert angle
    phi = Azimuth_Angle * DEGREES_TO_RADIANS

    ! Compute the azimuth emissivity component predictors
    CALL Compute_Predictors( Wind_Speed, Frequency, iVar%sec_z, Predictor )

    ! Compute the azimuth emissivity vector
    Harmonic_Loop: DO m = 1, N_HARMONICS

      ! Compute the angles
      angle = REAL(m,fp) * phi
      iVar%cos_angle(m) = COS(angle)
      iVar%sin_angle(m) = SIN(angle)

      ! Compute the coefficients
      DO i = 1, N_STOKES
        CALL Compute_Coefficient( &
               AZCoeff%C(:,i,m), &
               Predictor, &
               iVar%trig_coeff(i,m) )
      END DO

      ! Compute the emissivities
      e_Azimuth(1) = e_Azimuth(1) + iVar%trig_coeff(1,m)*iVar%cos_angle(m) ! Vertical
      e_Azimuth(2) = e_Azimuth(2) + iVar%trig_coeff(2,m)*iVar%cos_angle(m) ! Horizontal
      e_Azimuth(3) = e_Azimuth(3) + iVar%trig_coeff(3,m)*iVar%sin_angle(m) ! +/- 45deg.
      e_Azimuth(4) = e_Azimuth(4) + iVar%trig_coeff(4,m)*iVar%sin_angle(m) ! Circular

    END DO Harmonic_Loop

    ! Apply frequency correction 
    e_Azimuth = e_Azimuth * Azimuth_Freq_Correction(Frequency)

  END SUBROUTINE Azimuth_Emissivity


  ! Tangent-linear model
  SUBROUTINE Azimuth_Emissivity_TL( &
    AZCoeff         , &  ! Input
    Wind_Speed_TL   , &  ! Input
    Azimuth_Angle_TL, &  ! Input
    e_Azimuth_TL    , &  ! Output
    iVar              )  ! Internal variable input
    ! Arguments
    TYPE(FitCoeff_3D_type), INTENT(IN)  :: AZCoeff
    REAL(fp)              , INTENT(IN)  :: Wind_Speed_TL
    REAL(fp)              , INTENT(IN)  :: Azimuth_Angle_TL
    REAL(fp)              , INTENT(OUT) :: e_Azimuth_TL(:)
    TYPE(iVar_type)       , INTENT(IN)  :: iVar
    ! Local variables
    INTEGER :: i, m
    REAL(fp) :: phi_TL, angle_TL
    REAL(fp) :: predictor_TL(N_PREDICTORS)
    REAL(fp) :: trig_coeff_TL(N_STOKES)
    
    ! Initialise output
    e_Azimuth_TL = ZERO

    ! Compute angle perturbation in radians    
    phi_TL = Azimuth_Angle_TL * DEGREES_TO_RADIANS
    
    ! Compute the azimuth emissivity component tangent-linear predictors
    CALL Compute_Predictors_TL( iVar%wind_speed, iVar%frequency, iVar%sec_z, Wind_Speed_TL, predictor_TL )

    ! Compute the tangent-linear azimuth emissivity vector
    Harmonic_Loop: DO m = 1, N_HARMONICS
      
      ! Compute the angles
      angle_TL = REAL(m,fp) * phi_TL

      ! Compute the coefficients
      DO i = 1, N_STOKES
        CALL Compute_Coefficient_TL( &
               AZCoeff%C(:,i,m), &
               predictor_TL, &
               trig_coeff_TL(i) )
      END DO
      
      ! Compute the tangent-linear emissivities
      ! ...Vertical polarisation
      e_Azimuth_TL(1) = e_Azimuth_TL(1) + iVar%cos_angle(m)*trig_coeff_TL(1) - &
                                          iVar%trig_coeff(1,m)*iVar%sin_angle(m)*angle_TL
      ! ...Horizontal polarisation
      e_Azimuth_TL(2) = e_Azimuth_TL(2) + iVar%cos_angle(m)*trig_coeff_TL(2) - &
                                          iVar%trig_coeff(2,m)*iVar%sin_angle(m)*angle_TL
      ! ...+/- 45deg. polarisation
      e_Azimuth_TL(3) = e_Azimuth_TL(3) + iVar%sin_angle(m)*trig_coeff_TL(3) + &
                                          iVar%trig_coeff(3,m)*iVar%cos_angle(m)*angle_TL 
      ! ...Circular polarisation
      e_Azimuth_TL(4) = e_Azimuth_TL(4) + iVar%sin_angle(m)*trig_coeff_TL(4) + &
                                          iVar%trig_coeff(4,m)*iVar%cos_angle(m)*angle_TL
    END DO Harmonic_Loop

    ! Apply tangent-linear frequency correction
    e_Azimuth_TL = e_Azimuth_TL * Azimuth_Freq_Correction(iVar%frequency)

  END SUBROUTINE Azimuth_Emissivity_TL


  ! Adjoint model
  SUBROUTINE Azimuth_Emissivity_AD( &
    AZCoeff         , &  ! Input
    e_Azimuth_AD    , &  ! AD Input
    Wind_Speed_AD   , &  ! AD Output
    Azimuth_Angle_AD, &  ! AD Output
    iVar              )  ! Internal variable input
    ! Arguments
    TYPE(FitCoeff_3D_type), INTENT(IN)     :: AZCoeff
    REAL(fp)              , INTENT(IN OUT) :: e_Azimuth_AD(:)
    REAL(fp)              , INTENT(IN OUT) :: Wind_Speed_AD
    REAL(fp)              , INTENT(IN OUT) :: Azimuth_Angle_AD
    TYPE(iVar_type)       , INTENT(IN)     :: iVar
    ! Local variables
    INTEGER :: i, m
    REAL(fp) :: phi_AD, angle_AD
    REAL(fp) :: predictor_AD(N_PREDICTORS)
    REAL(fp) :: trig_coeff_AD(N_STOKES)

    ! Initialise local adjoints
    phi_AD       = ZERO
    predictor_AD = ZERO
    
    ! Adjoint of frequency correction
    e_Azimuth_AD = e_Azimuth_AD * Azimuth_Freq_Correction(iVar%frequency)
    
    ! Compute the azimuth emissivity vector adjoint
    Harmonic_Loop: DO m = 1, N_HARMONICS
      
      ! Compute the adjoint of the emissivities
      ! ...Circular polarisation
      angle_AD         = iVar%trig_coeff(4,m)*iVar%cos_angle(m)*e_Azimuth_AD(4)
      trig_coeff_AD(4) = iVar%sin_angle(m)*e_Azimuth_AD(4)
      ! ...+/- 45deg. polarisation
      angle_AD         = angle_AD + iVar%trig_coeff(3,m)*iVar%cos_angle(m)*e_Azimuth_AD(3)
      trig_coeff_AD(3) = iVar%sin_angle(m)*e_Azimuth_AD(3)
      ! ...Horizontal polarisation
      angle_AD         = angle_AD - iVar%trig_coeff(2,m)*iVar%sin_angle(m)*e_Azimuth_AD(2)
      trig_coeff_AD(2) = iVar%cos_angle(m)*e_Azimuth_AD(2)
      ! ...Vertical polarisation
      angle_AD         = angle_AD - iVar%trig_coeff(1,m)*iVar%sin_angle(m)*e_Azimuth_AD(1)
      trig_coeff_AD(1) = iVar%cos_angle(m)*e_Azimuth_AD(1)

      ! Compute the adjoint of the coefficients
      DO i = N_STOKES, 1, -1
        CALL Compute_Coefficient_AD( &
               AZCoeff%C(:,i,m), &
               trig_coeff_AD(i), &
               predictor_AD      )
      END DO

      ! Compute the angle adjoint
      phi_AD = phi_AD + REAL(m,fp)*angle_AD
      
    END DO Harmonic_Loop
    
    ! Compute the azimuth emissivity component predictor adjoint
    CALL Compute_Predictors_AD( iVar%wind_speed, iVar%frequency, iVar%sec_z, predictor_AD, Wind_Speed_AD )

    ! Adjoint of the angle perturbation in radians
    Azimuth_Angle_AD = Azimuth_Angle_AD + phi_AD*DEGREES_TO_RADIANS
    
  END SUBROUTINE Azimuth_Emissivity_AD


!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  ! =============================================
  ! Compute predictors for the azimuth components
  ! =============================================
  
  ! Forward model
  SUBROUTINE Compute_Predictors( &
    Wind_Speed, &  ! Input
    Frequency , &  ! Input
    sec_z     , &  ! Input
    Predictor   )  ! Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: Wind_Speed
    REAL(fp), INTENT(IN)  :: Frequency 
    REAL(fp), INTENT(IN)  :: sec_z     
    REAL(fp), INTENT(OUT) :: Predictor(N_PREDICTORS) 
    ! Compute the predictors.
    Predictor( 1) = ONE
    Predictor( 2) = Frequency
    Predictor( 3) = sec_z
    Predictor( 4) = sec_z * Frequency
    Predictor( 5) = Wind_Speed
    Predictor( 6) = Wind_Speed * Frequency
    Predictor( 7) = Wind_Speed**2
    Predictor( 8) = Frequency * Wind_Speed**2
    Predictor( 9) = Wind_Speed * sec_z
    Predictor(10) = Wind_Speed * sec_z * Frequency
  END SUBROUTINE Compute_Predictors
    
  
  ! Tangent-linear model  
  SUBROUTINE Compute_Predictors_TL(  &
    Wind_Speed   , &  ! FWD Input
    Frequency    , &  ! FWD Input
    sec_z        , &  ! FWD Input
    Wind_Speed_TL, &  ! TL  Input
    Predictor_TL   )  ! TL  Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: Wind_Speed
    REAL(fp), INTENT(IN)  :: Frequency 
    REAL(fp), INTENT(IN)  :: sec_z     
    REAL(fp), INTENT(IN)  :: Wind_Speed_TL
    REAL(fp), INTENT(OUT) :: Predictor_TL(N_PREDICTORS)
    ! Compute the tangent-linear predictors.
    Predictor_TL( 1) = ZERO
    Predictor_TL( 2) = ZERO
    Predictor_TL( 3) = ZERO
    Predictor_TL( 4) = ZERO
    Predictor_TL( 5) = Wind_Speed_TL
    Predictor_TL( 6) = Frequency * Wind_Speed_TL
    Predictor_TL( 7) = TWO * Wind_Speed * Wind_Speed_TL
    Predictor_TL( 8) = TWO * Frequency * Wind_Speed * Wind_Speed_TL
    Predictor_TL( 9) = sec_z * Wind_Speed_TL
    Predictor_TL(10) = sec_z * Frequency * Wind_Speed_TL
  END SUBROUTINE Compute_Predictors_TL
    
  
  ! Adjoint model
  SUBROUTINE Compute_Predictors_AD(  &
    Wind_Speed   , &  ! FWD Input
    Frequency    , &  ! FWD Input
    sec_z        , &  ! FWD Input
    Predictor_AD , &  ! AD  Input
    Wind_Speed_AD  )  ! AD  Output
    ! Arguments
    REAL(fp), INTENT(IN)     :: Wind_Speed
    REAL(fp), INTENT(IN)     :: Frequency 
    REAL(fp), INTENT(IN)     :: sec_z     
    REAL(fp), INTENT(IN OUT) :: Predictor_AD(N_PREDICTORS)
    REAL(fp), INTENT(IN OUT) :: Wind_Speed_AD
    ! Compute the predictor adjoints
    Wind_Speed_AD = Wind_Speed_AD + &
                    sec_z * Frequency            * Predictor_AD(10) + &
                    sec_z                        * Predictor_AD( 9) + &
                    TWO * Frequency * Wind_Speed * Predictor_AD( 8) + &
                    TWO             * Wind_Speed * Predictor_AD( 7) + &
                          Frequency              * Predictor_AD( 6) + &
                                                   Predictor_AD( 5)
    Predictor_AD = ZERO
  END SUBROUTINE Compute_Predictors_AD
    
    
  ! ==============================================================
  ! Compute the component coefficient from the regression equation
  ! ==============================================================
  
  ! Forward model
  SUBROUTINE Compute_Coefficient( &
    c           , &  ! Input
    X           , &  ! Input
    Coefficient   )  ! Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: c(:)  ! regression coefficient
    REAL(fp), INTENT(IN)  :: X(:)  ! predictor
    REAL(fp), INTENT(OUT) :: Coefficient
    ! Local variables
    INTEGER :: i
    ! Compute component coefficient
    Coefficient = ZERO
    DO i = 1, N_PREDICTORS
      Coefficient = Coefficient + c(i)*X(i)
    END DO
  END SUBROUTINE Compute_Coefficient


  ! Tangent-linear model
  SUBROUTINE Compute_Coefficient_TL( &
    c             , &  ! Input
    X_TL          , &  ! Input
    Coefficient_TL  )  ! Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: c(:)     ! regression coefficient
    REAL(fp), INTENT(IN)  :: X_TL(:)  ! predictor
    REAL(fp), INTENT(OUT) :: Coefficient_TL
    ! Local variables
    INTEGER :: i
    ! Compute tangent-linear component coefficient
    Coefficient_TL = ZERO
    DO i = 1, N_PREDICTORS
      Coefficient_TL = Coefficient_TL + c(i)*X_TL(i)
    END DO
  END SUBROUTINE Compute_Coefficient_TL


  ! Adjoint model
  SUBROUTINE Compute_Coefficient_AD( &
    c             , &  ! Input
    Coefficient_AD, &  ! Input
    X_AD            )  ! Output
    ! Arguments
    REAL(fp), INTENT(IN)     :: c(:)     ! regression coefficient
    REAL(fp), INTENT(IN OUT) :: Coefficient_AD
    REAL(fp), INTENT(IN OUT) :: X_AD(:)  ! predictor
    ! Local variables
    INTEGER :: i
    ! Compute adjoint of the component coefficient
    DO i = 1, N_PREDICTORS
      X_AD(i) = X_AD(i) + c(i)*Coefficient_AD
    END DO
    Coefficient_AD = ZERO
  END SUBROUTINE Compute_Coefficient_AD


  PURE FUNCTION  Azimuth_Freq_Correction( Frequency ) RESULT( Fre_C )
    IMPLICIT NONE
    REAL( fp ), INTENT(IN) :: Frequency
    REAL( fp ) :: Fre_C
    INTEGER :: i
      ! Data for the frequency correction
    REAL(fp), PARAMETER :: x(9) = (/ 0.0_fp, 1.4_fp, 6.8_fp, 10.7_fp, 19.35_fp, &
                                   37._fp, 89._fp, 150._fp, 200._fp/)
    REAL(fp), PARAMETER :: y(9) = (/ 0.0_fp, 0.1_fp, 0.6_fp, 0.9_fp, 1._fp, &
                                   1.0_fp, 0.4_fp, 0.2_fp, 0.0_fp/)
    ! 
    IF( Frequency <= ZERO .or. Frequency >= 200.0_fp ) THEN
      Fre_C = ZERO
      RETURN
    ELSE
      DO i = 1, 8
        IF( Frequency >= x(i) .and. Frequency <= x(i+1) ) THEN
          Fre_C = y(i) + (y(i+1)-y(i))/(x(i+1)-x(i))*(Frequency-x(i))
          RETURN
        END IF
      END DO
    END IF
    Fre_C = ZERO
    
  END FUNCTION  Azimuth_Freq_Correction

END MODULE Azimuth_Emissivity_Module
