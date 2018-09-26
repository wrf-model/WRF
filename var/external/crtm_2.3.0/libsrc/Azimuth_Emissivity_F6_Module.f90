!
! Helper module containing the azimuth emissivity routines for the
! CRTM implementation of FASTEM6
!
!
! CREATION HISTORY:
!       Written by:     Original FASTEM1-5 authors, and Masahiro Kazumori
!                       for the new azimuthal emissivity model.
!
!       Refactored by:  Paul van Delst, January 2015
!                       paul.vandelst@noaa.gov
!

MODULE Azimuth_Emissivity_F6_Module

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds     , ONLY: fp
  USE FitCoeff_Define, ONLY: FitCoeff_3D_type
  USE Search_Utility , ONLY: Bisection_Search
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Data types
  PUBLIC :: iVar_type
  ! Science routines
  PUBLIC :: Azimuth_Emissivity_F6
  PUBLIC :: Azimuth_Emissivity_F6_TL
  PUBLIC :: Azimuth_Emissivity_F6_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: Azimuth_Emissivity_F6_Module.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'

  REAL(fp), PARAMETER :: ZERO   = 0.0_fp
  REAL(fp), PARAMETER :: POINT5 = 0.5_fp
  REAL(fp), PARAMETER :: ONE    = 1.0_fp
  REAL(fp), PARAMETER :: TWO    = 2.0_fp
  REAL(fp), PARAMETER :: THREE  = 3.0_fp
  REAL(fp), PARAMETER :: FOUR   = 4.0_fp
  REAL(fp), PARAMETER :: PI = 3.141592653589793238462643383279_fp
  REAL(fp), PARAMETER :: DEGREES_TO_RADIANS = PI / 180.0_fp

  ! Dimensions
  ! ...Number of Stokes parameters handled
  INTEGER, PARAMETER :: N_STOKES = 2
  INTEGER, PARAMETER :: IVPOL = 1  ! Index for vertical polarisation
  INTEGER, PARAMETER :: IHPOL = 2  ! Index for horizontal polarisation
  ! ...The number of fitting frequencies
  INTEGER, PARAMETER :: N_FREQUENCIES = 6

  ! Parameters used in the model
  ! ...The fitting frequencies
  REAL(fp), PARAMETER :: FIT_FREQUENCY(N_FREQUENCIES) = &
  [ 6.925_fp, 10.65_fp, 18.7_fp, 23.8_fp, 36.5_fp, 89.0_fp ]
  ! ...Wind speed limits
  REAL(fp), PARAMETER :: WIND_SPEED_MAX18 = 18.0_fp
  REAL(fp), PARAMETER :: WIND_SPEED_MAX15 = 15.0_fp
  ! ...Frequency limits
  REAL(fp), PARAMETER :: FREQUENCY_MAX37 = 37.0_fp
  ! ...Exponents for the AxSy_theta terms
  REAL(fp), PARAMETER :: XS11 = TWO
  REAL(fp), PARAMETER :: XS12 = TWO
  REAL(fp), PARAMETER :: XS21 = ONE
  REAL(fp), PARAMETER :: XS22 = FOUR
  ! ...Reference zenith angle
  REAL(fp), PARAMETER :: THETA_REF = 55.2_fp


  ! --------------------------------------
  ! Structure definition to hold internal
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: iVar_type
    PRIVATE
    ! Direct inputs
    REAL(fp) :: wind_speed   = ZERO
    REAL(fp) :: frequency    = ZERO
    REAL(fp) :: zenith_angle = ZERO
    ! Derived inputs
    REAL(fp) :: phi  = ZERO    ! Azimuth angle in radians
    LOGICAL  :: lw18 = .FALSE. ! Logical to flag wind speed > 18m/s
    REAL(fp) :: w18  = ZERO    ! Wind speed with 18m/s maximum
    LOGICAL  :: lw15 = .FALSE. ! Logical to flag wind speed > 15m/s
    REAL(fp) :: w15  = ZERO    ! Wind speed with 15m/s maximum
    REAL(fp) :: f37  = ZERO    ! Frequency with 37GHz maximum
    ! Intermediate variables
    REAL(fp), DIMENSION(N_FREQUENCIES) :: A1v , A2v , A1h , A2h
    REAL(fp), DIMENSION(N_FREQUENCIES) :: A1s1, A1s2, A2s1, A2s2
    REAL(fp), DIMENSION(N_FREQUENCIES) :: A2s2_theta0
    REAL(fp), DIMENSION(N_FREQUENCIES) :: A1s1_theta, A1s2_theta, A2s1_theta, A2s2_theta
    REAL(fp), DIMENSION(N_FREQUENCIES) :: A1v_theta , A1h_theta , A2v_theta , A2h_theta
    REAL(fp) :: azimuth_component(N_FREQUENCIES, N_STOKES)
    ! Interpolation variables
    INTEGER :: i1 = 0
    INTEGER :: i2 = 0
    REAL(fp) :: lpoly = ZERO
   END TYPE iVar_type


CONTAINS


  ! ===========================================================
  ! Compute emissivity as a function of relative azimuth angle.
  ! ===========================================================

  ! Forward model
  SUBROUTINE Azimuth_Emissivity_F6( &
    AZCoeff      , &  ! Input
    Wind_Speed   , &  ! Input
    Azimuth_Angle, &  ! Input
    Frequency    , &  ! Input
    Zenith_Angle , &  ! Input
    e_Azimuth    , &  ! Output
    iVar           )  ! Internal variable output
    ! Arguments
    TYPE(FitCoeff_3D_type), INTENT(IN)     :: AZCoeff
    REAL(fp)              , INTENT(IN)     :: Wind_Speed
    REAL(fp)              , INTENT(IN)     :: Azimuth_Angle
    REAL(fp)              , INTENT(IN)     :: Frequency
    REAL(fp)              , INTENT(IN)     :: Zenith_Angle
    REAL(fp)              , INTENT(OUT)    :: e_Azimuth(:)
    TYPE(iVar_type)       , INTENT(IN OUT) :: iVar
    ! Local variables
    INTEGER :: j

    ! Initialise output
    e_Azimuth = ZERO

    ! Save inputs for TL and AD calls
    iVar%wind_speed   = Wind_Speed
    iVar%frequency    = Frequency
    iVar%zenith_angle = Zenith_Angle

    ! Convert inputs
    iVar%phi  = Azimuth_Angle * DEGREES_TO_RADIANS
    iVar%lw18 = Wind_Speed > WIND_SPEED_MAX18
    iVar%w18  = MIN(Wind_Speed, WIND_SPEED_MAX18)
    iVar%lw15 = Wind_Speed > WIND_SPEED_MAX15
    iVar%w15  = MIN(Wind_Speed, WIND_SPEED_MAX15)
    iVar%f37  = MIN(Frequency, FREQUENCY_MAX37)


    ! Loop over frequencies to compute the intermediate terms
    Frequency_Loop: DO j = 1, N_FREQUENCIES

      iVar%A1v(j) = AZCoeff%C(1,j,IVPOL) * ( EXP(-AZCoeff%C(5,j,IVPOL) * iVar%w18**2 ) - ONE ) * &
                    ( AZCoeff%C(2,j,IVPOL) * iVar%w18    + &
                      AZCoeff%C(3,j,IVPOL) * iVar%w18**2 + &
                      AZCoeff%C(4,j,IVPOL) * iVar%w18**3   )
      iVar%A2v(j) = AZCoeff%C(6,j,IVPOL) * iVar%w18

      iVar%A1h(j) = AZCoeff%C(1,j,IHPOL) * iVar%w18
      iVar%A2h(j) = AZCoeff%C(2,j,IHPOL) * ( EXP(-AZCoeff%C(6,j,IHPOL) * iVar%w18**2 ) - ONE ) * &
                    ( AZCoeff%C(3,j,IHPOL) * iVar%w18    + &
                      AZCoeff%C(4,j,IHPOL) * iVar%w18**2 + &
                      AZCoeff%C(5,j,IHPOL) * iVar%w18**3   )

      iVar%A1s1(j) = (iVar%A1v(j) + iVar%A1h(j))/TWO
      iVar%A1s2(j) =  iVar%A1v(j) - iVar%A1h(j)
      iVar%A2s1(j) = (iVar%A2v(j) + iVar%A2h(j))/TWO
      iVar%A2s2(j) =  iVar%A2v(j) - iVar%A2h(j)

      iVar%A2s2_theta0(j) = (iVar%w15**2 - (iVar%w15**3)/22.5_fp)/55.5556_fp * &
                            (TWO/290.0_fp) * &
                            (ONE - LOG10(30.0_fp/iVar%f37) )

      iVar%A1s1_theta(j) = iVar%A1s1(j)*((iVar%zenith_angle/THETA_REF)**XS11)
      iVar%A2s1_theta(j) = iVar%A2s1(j)*((iVar%zenith_angle/THETA_REF)**XS12)
      iVar%A1s2_theta(j) = iVar%A1s2(j)*((iVar%zenith_angle/THETA_REF)**XS21)
      iVar%A2s2_theta(j) = iVar%A2s2_theta0(j) + &
                           (iVar%A2s2(j) - iVar%A2s2_theta0(j))*((iVar%zenith_angle/THETA_REF)**XS22)

      iVar%A1v_theta(j) = POINT5*(TWO*iVar%A1s1_theta(j) + iVar%A1s2_theta(j))
      iVar%A1h_theta(j) = POINT5*(TWO*iVar%A1s1_theta(j) - iVar%A1s2_theta(j))
      iVar%A2v_theta(j) = POINT5*(TWO*iVar%A2s1_theta(j) + iVar%A2s2_theta(j))
      iVar%A2h_theta(j) = POINT5*(TWO*iVar%A2s1_theta(j) - iVar%A2s2_theta(j))

      iVar%azimuth_component(j,IVPOL) = (iVar%A1v_theta(j) * COS(iVar%phi)) + (iVar%A2v_theta(j) * COS(TWO*iVar%phi))
      iVar%azimuth_component(j,IHPOL) = (iVar%A1h_theta(j) * COS(iVar%phi)) + (iVar%A2h_theta(j) * COS(TWO*iVar%phi))

    END DO Frequency_Loop


    ! Interpolate to input frequency for result. Only V and H polarisation.
    ! ...Check for lower out of bounds frequency
    IF ( Frequency < FIT_FREQUENCY(1) ) THEN
      e_Azimuth(IVPOL) = iVar%azimuth_component(1,IVPOL)
      e_Azimuth(IHPOL) = iVar%azimuth_component(1,IHPOL)
      RETURN
    END IF
    ! ...Check for upper out of bounds frequency
    IF ( Frequency > FIT_FREQUENCY(N_FREQUENCIES) ) THEN
      e_Azimuth(IVPOL) = iVar%azimuth_component(N_FREQUENCIES,IVPOL)
      e_Azimuth(IHPOL) = iVar%azimuth_component(N_FREQUENCIES,IHPOL)
      RETURN
    END IF
    ! ...In bounds, so interpolate
    iVar%i1 = Bisection_Search(FIT_FREQUENCY, Frequency)
    iVar%i2 = iVar%i1 + 1
    iVar%lpoly = (Frequency - FIT_FREQUENCY(iVar%i1))/(FIT_FREQUENCY(iVar%i2)-FIT_FREQUENCY(iVar%i1))

    e_Azimuth(IVPOL) = (   iVar%lpoly      * iVar%azimuth_component(iVar%i2,IVPOL)) + &
                       ((ONE - iVar%lpoly) * iVar%azimuth_component(iVar%i1,IVPOL))
    e_Azimuth(IHPOL) = (   iVar%lpoly      * iVar%azimuth_component(iVar%i2,IHPOL)) + &
                       ((ONE - iVar%lpoly) * iVar%azimuth_component(iVar%i1,IHPOL))

  END SUBROUTINE Azimuth_Emissivity_F6



  ! Tangent-linear model
  SUBROUTINE Azimuth_Emissivity_F6_TL( &
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
    INTEGER  :: j
    REAL(fp) :: phi_TL
    REAL(fp), DIMENSION(N_FREQUENCIES) :: A1v_TL , A2v_TL , A1h_TL , A2h_TL
    REAL(fp), DIMENSION(N_FREQUENCIES) :: A1s1_TL, A1s2_TL, A2s1_TL, A2s2_TL
    REAL(fp), DIMENSION(N_FREQUENCIES) :: A2s2_theta0_TL
    REAL(fp), DIMENSION(N_FREQUENCIES) :: A1s1_theta_TL, A1s2_theta_TL, A2s1_theta_TL, A2s2_theta_TL
    REAL(fp), DIMENSION(N_FREQUENCIES) :: A1v_theta_TL , A1h_theta_TL , A2v_theta_TL , A2h_theta_TL
    REAL(fp) :: azimuth_component_TL(N_FREQUENCIES, N_STOKES)

    ! Initialise output
    e_Azimuth_TL = ZERO

    ! Convert inputs
    phi_TL = Azimuth_Angle_TL * DEGREES_TO_RADIANS

    ! Loop over frequencies to compute the intermediate terms
    Frequency_Loop: DO j = 1, N_FREQUENCIES

      ! Only compute TL values if wind speed is not maxed out
      IF ( iVar%lw18 ) THEN
        A1v_TL(j) = ZERO
        A2v_TL(j) = ZERO
        A1h_TL(j) = ZERO
        A2h_TL(j) = ZERO
      ELSE
        A1v_TL(j) = ( AZCoeff%C(1,j,IVPOL) * ( EXP(-AZCoeff%C(5,j,IVPOL) * iVar%w18**2 ) - ONE ) * &
                      (         AZCoeff%C(2,j,IVPOL)               + &
                        TWO   * AZCoeff%C(3,j,IVPOL) * iVar%w18    + &
                        THREE * AZCoeff%C(4,j,IVPOL) * iVar%w18**2   ) - &
                      TWO * AZCoeff%C(1,j,IVPOL) * AZCoeff%C(5,j,IVPOL) * iVar%w18 * &
                      EXP(-AZCoeff%C(5,j,IVPOL) * iVar%w18**2 ) * &
                      ( AZCoeff%C(2,j,IVPOL) * iVar%w18    + &
                        AZCoeff%C(3,j,IVPOL) * iVar%w18**2 + &
                        AZCoeff%C(4,j,IVPOL) * iVar%w18**3   ) ) * Wind_Speed_TL
        A2v_TL(j) = AZCoeff%C(6,j,IVPOL) * Wind_Speed_TL

        A1h_TL(j) = AZCoeff%C(1,j,IHPOL) * Wind_Speed_TL

        A2h_TL(j) = ( AZCoeff%C(2,j,IHPOL) * ( EXP(-AZCoeff%C(6,j,IHPOL) * iVar%w18**2 ) - ONE ) * &
                      (         AZCoeff%C(3,j,IHPOL)               + &
                        TWO   * AZCoeff%C(4,j,IHPOL) * iVar%w18    + &
                        THREE * AZCoeff%C(5,j,IHPOL) * iVar%w18**2   ) - &
                      TWO * AZCoeff%C(2,j,IHPOL) * AZCoeff%C(6,j,IHPOL) * iVar%w18 * &
                      EXP(-AZCoeff%C(6,j,IHPOL) * iVar%w18**2 ) * &
                      ( AZCoeff%C(3,j,IHPOL) * iVar%w18    + &
                        AZCoeff%C(4,j,IHPOL) * iVar%w18**2 + &
                        AZCoeff%C(5,j,IHPOL) * iVar%w18**3   ) ) * Wind_Speed_TL

      END IF

      A1s1_TL(j) = (A1v_TL(j) + A1h_TL(j))/TWO
      A1s2_TL(j) =  A1v_TL(j) - A1h_TL(j)
      A2s1_TL(j) = (A2v_TL(j) + A2h_TL(j))/TWO
      A2s2_TL(j) =  A2v_TL(j) - A2h_TL(j)


      ! Only compute TL value if wind speed is not maxed out
      IF ( iVar%lw15 ) THEN
        A2s2_theta0_TL(j) = ZERO
      ELSE
        A2s2_theta0_TL(j) = (TWO*iVar%w15 - (THREE*iVar%w15**2)/22.5_fp)/55.5556_fp * &
                            (TWO/290.0_fp) * &
                            (ONE - LOG10(30.0_fp/iVar%f37) ) * Wind_Speed_TL
      END IF


      A1s1_theta_TL(j) = A1s1_TL(j)*((iVar%zenith_angle/THETA_REF)**XS11)
      A2s1_theta_TL(j) = A2s1_TL(j)*((iVar%zenith_angle/THETA_REF)**XS12)
      A1s2_theta_TL(j) = A1s2_TL(j)*((iVar%zenith_angle/THETA_REF)**XS21)
      A2s2_theta_TL(j) = A2s2_theta0_TL(j) + &
                         (A2s2_TL(j) - A2s2_theta0_TL(j))*((iVar%zenith_angle/THETA_REF)**XS22)


      A1v_theta_TL(j) = POINT5*(TWO*A1s1_theta_TL(j) + A1s2_theta_TL(j))
      A1h_theta_TL(j) = POINT5*(TWO*A1s1_theta_TL(j) - A1s2_theta_TL(j))
      A2v_theta_TL(j) = POINT5*(TWO*A2s1_theta_TL(j) + A2s2_theta_TL(j))
      A2h_theta_TL(j) = POINT5*(TWO*A2s1_theta_TL(j) - A2s2_theta_TL(j))


      azimuth_component_TL(j,IVPOL) = (COS(  iVar%phi  ) * A1v_theta_TL(j)) + &
                                      (COS(TWO*iVar%phi) * A2v_theta_TL(j)) - &
                                      ( (      iVar%A1v_theta(j) * SIN(  iVar%phi  )) + &
                                        (TWO * iVar%A2v_theta(j) * SIN(TWO*iVar%phi))   ) * phi_TL
      azimuth_component_TL(j,IHPOL) = (COS(  iVar%phi  ) * A1h_theta_TL(j)) + &
                                      (COS(TWO*iVar%phi) * A2h_theta_TL(j)) - &
                                      ( (      iVar%A1h_theta(j) * SIN(  iVar%phi  )) + &
                                        (TWO * iVar%A2h_theta(j) * SIN(TWO*iVar%phi))   ) * phi_TL

    END DO Frequency_Loop


    ! Interpolate to input frequency for result. Only V and H polarisation.
    ! ...Check for lower out of bounds frequency
    IF ( iVar%frequency < FIT_FREQUENCY(1) ) THEN
      e_Azimuth_TL(IVPOL) = azimuth_component_TL(1,IVPOL)
      e_Azimuth_TL(IHPOL) = azimuth_component_TL(1,IHPOL)
      RETURN
    END IF
    ! ...Check for upper out of bounds frequency
    IF ( iVar%frequency > FIT_FREQUENCY(N_FREQUENCIES) ) THEN
      e_Azimuth_TL(IVPOL) = azimuth_component_TL(N_FREQUENCIES,IVPOL)
      e_Azimuth_TL(IHPOL) = azimuth_component_TL(N_FREQUENCIES,IHPOL)
      RETURN
    END IF
    ! ...In bounds, so interpolate
    e_Azimuth_TL(IVPOL) = (   iVar%lpoly      * azimuth_component_TL(iVar%i2,IVPOL)) + &
                          ((ONE - iVar%lpoly) * azimuth_component_TL(iVar%i1,IVPOL))
    e_Azimuth_TL(IHPOL) = (   iVar%lpoly      * azimuth_component_TL(iVar%i2,IHPOL)) + &
                          ((ONE - iVar%lpoly) * azimuth_component_TL(iVar%i1,IHPOL))

  END SUBROUTINE Azimuth_Emissivity_F6_TL


  ! Adjoint model
  SUBROUTINE Azimuth_Emissivity_F6_AD( &
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
    INTEGER  :: j
    REAL(fp) :: phi_AD
    REAL(fp), DIMENSION(N_FREQUENCIES) :: A1v_AD , A2v_AD , A1h_AD , A2h_AD
    REAL(fp), DIMENSION(N_FREQUENCIES) :: A1s1_AD, A1s2_AD, A2s1_AD, A2s2_AD
    REAL(fp), DIMENSION(N_FREQUENCIES) :: A2s2_theta0_AD
    REAL(fp), DIMENSION(N_FREQUENCIES) :: A1s1_theta_AD, A1s2_theta_AD, A2s1_theta_AD, A2s2_theta_AD
    REAL(fp), DIMENSION(N_FREQUENCIES) :: A1v_theta_AD , A1h_theta_AD , A2v_theta_AD , A2h_theta_AD
    REAL(fp) :: azimuth_component_AD(N_FREQUENCIES, N_STOKES)

    ! Initialise local adjoint variables
    phi_AD = ZERO
    A1v_AD  = ZERO;  A2v_AD  = ZERO;  A1h_AD  = ZERO;  A2h_AD  = ZERO
    A1s1_AD = ZERO;  A1s2_AD = ZERO;  A2s1_AD = ZERO;  A2s2_AD = ZERO
    A2s2_theta0_AD = ZERO
    A1s1_theta_AD = ZERO; A1s2_theta_AD = ZERO; A2s1_theta_AD = ZERO; A2s2_theta_AD = ZERO
    A1v_theta_AD  = ZERO; A1h_theta_AD  = ZERO; A2v_theta_AD  = ZERO; A2h_theta_AD  = ZERO
    azimuth_component_AD = ZERO


    ! Adjoint of frequency interpolation. Only V and H polarisation.
    IF ( iVar%frequency < FIT_FREQUENCY(1) ) THEN
      ! ...Lower out of bounds frequency
      azimuth_component_AD(1,IHPOL) = azimuth_component_AD(1,IHPOL) + e_Azimuth_AD(IHPOL)
      azimuth_component_AD(1,IVPOL) = azimuth_component_AD(1,IVPOL) + e_Azimuth_AD(IVPOL)
    ELSE IF ( iVar%frequency > FIT_FREQUENCY(N_FREQUENCIES) ) THEN
      ! ...Upper out of bounds frequency
      azimuth_component_AD(N_FREQUENCIES,IHPOL) = azimuth_component_AD(N_FREQUENCIES,IHPOL) + e_Azimuth_AD(IHPOL)
      azimuth_component_AD(N_FREQUENCIES,IVPOL) = azimuth_component_AD(N_FREQUENCIES,IVPOL) + e_Azimuth_AD(IVPOL)
    ELSE
      ! ...In bounds, so interpolate
      azimuth_component_AD(iVar%i1,IHPOL) = ((ONE - iVar%lpoly) * e_Azimuth_AD(IHPOL)) + azimuth_component_AD(iVar%i1,IHPOL)
      azimuth_component_AD(iVar%i2,IHPOL) = (   iVar%lpoly      * e_Azimuth_AD(IHPOL)) + azimuth_component_AD(iVar%i2,IHPOL)
      azimuth_component_AD(iVar%i1,IVPOL) = ((ONE - iVar%lpoly) * e_Azimuth_AD(IVPOL)) + azimuth_component_AD(iVar%i1,IVPOL)
      azimuth_component_AD(iVar%i2,IVPOL) = (   iVar%lpoly      * e_Azimuth_AD(IVPOL)) + azimuth_component_AD(iVar%i2,IVPOL)
    END IF
    e_Azimuth_AD(IHPOL) = ZERO
    e_Azimuth_AD(IVPOL) = ZERO


    ! Loop over frequencies to compute the intermediate term adjoints
    Frequency_Loop: DO j = 1, N_FREQUENCIES

      phi_AD = phi_AD - &
               ( (      iVar%A1h_theta(j) * SIN(  iVar%phi  )) + &
                 (TWO * iVar%A2h_theta(j) * SIN(TWO*iVar%phi))   ) * azimuth_component_AD(j,IHPOL)
      A2h_theta_AD(j) = (COS(TWO*iVar%phi) * azimuth_component_AD(j,IHPOL)) + A2h_theta_AD(j)
      A1h_theta_AD(j) = (COS(  iVar%phi  ) * azimuth_component_AD(j,IHPOL)) + A1h_theta_AD(j)
      azimuth_component_AD(j,IHPOL) = ZERO

      phi_AD = phi_AD - &
               ( (      iVar%A1v_theta(j) * SIN(  iVar%phi  )) + &
                 (TWO * iVar%A2v_theta(j) * SIN(TWO*iVar%phi))   ) * azimuth_component_AD(j,IVPOL)
      A2v_theta_AD(j) = (COS(TWO*iVar%phi) * azimuth_component_AD(j,IVPOL)) + A2v_theta_AD(j)
      A1v_theta_AD(j) = (COS(  iVar%phi  ) * azimuth_component_AD(j,IVPOL)) + A1v_theta_AD(j)
      azimuth_component_AD(j,IVPOL) = ZERO


      A2s2_theta_AD(j) = A2s2_theta_AD(j) - POINT5*A2h_theta_AD(j)
      A2s1_theta_AD(j) = A2s1_theta_AD(j) +        A2h_theta_AD(j)
      A2h_theta_AD(j)  = ZERO

      A2s2_theta_AD(j) = A2s2_theta_AD(j) + POINT5*A2v_theta_AD(j)
      A2s1_theta_AD(j) = A2s1_theta_AD(j) +        A2v_theta_AD(j)
      A2v_theta_AD(j)  = ZERO

      A1s2_theta_AD(j) = A1s2_theta_AD(j) - POINT5*A1h_theta_AD(j)
      A1s1_theta_AD(j) = A1s1_theta_AD(j) +        A1h_theta_AD(j)
      A1h_theta_AD(j)  = ZERO

      A1s2_theta_AD(j) = A1s2_theta_AD(j) + POINT5*A1v_theta_AD(j)
      A1s1_theta_AD(j) = A1s1_theta_AD(j) +        A1v_theta_AD(j)
      A1v_theta_AD(j)  = ZERO


      A2s2_AD(j)        = A2s2_AD(j)        + A2s2_theta_AD(j)*((iVar%zenith_angle/THETA_REF)**XS22)
      A2s2_theta0_AD(j) = A2s2_theta0_AD(j) + A2s2_theta_AD(j)*(ONE - (iVar%zenith_angle/THETA_REF)**XS22)
      A2s2_theta_AD(j)  = ZERO

      A1s2_AD(j)       = A1s2_AD(j) + A1s2_theta_AD(j)*((iVar%zenith_angle/THETA_REF)**XS21)
      A1s2_theta_AD(j) = ZERO

      A2s1_AD(j)       = A2s1_AD(j) + A2s1_theta_AD(j)*((iVar%zenith_angle/THETA_REF)**XS12)
      A2s1_theta_AD(j) = ZERO

      A1s1_AD(j)       = A1s1_AD(j) + A1s1_theta_AD(j)*((iVar%zenith_angle/THETA_REF)**XS11)
      A1s1_theta_AD(j) = ZERO


      ! Only compute AD value if wind speed is not maxed out
      IF ( iVar%lw15 ) THEN
        A2s2_theta0_AD(j) = ZERO
      ELSE
        Wind_Speed_AD = Wind_Speed_AD + &
                        ((TWO*iVar%w15 - (THREE*iVar%w15**2)/22.5_fp)/55.5556_fp * &
                         (TWO/290.0_fp) * &
                         (ONE - LOG10(30.0_fp/iVar%f37)))*A2s2_theta0_AD(j)
        A2s2_theta0_AD(j) = ZERO
      END IF


      A2h_AD(j)  = A2h_AD(j) - A2s2_AD(j)
      A2v_AD(j)  = A2v_AD(j) + A2s2_AD(j)
      A2s2_AD(j) = ZERO

      A2h_AD(j)  = A2h_AD(j) + POINT5*A2s1_AD(j)
      A2v_AD(j)  = A2v_AD(j) + POINT5*A2s1_AD(j)
      A2s1_AD(j) = ZERO

      A1h_AD(j)  = A1h_AD(j) - A1s2_AD(j)
      A1v_AD(j)  = A1v_AD(j) + A1s2_AD(j)
      A1s2_AD(j) = ZERO

      A1h_AD(j)  = A1h_AD(j) + POINT5*A1s1_AD(j)
      A1v_AD(j)  = A1v_AD(j) + POINT5*A1s1_AD(j)
      A1s1_AD(j) = ZERO
      
      
      ! Only compute AD values if wind speed is not maxed out
      IF ( iVar%lw18 ) THEN
        A1v_AD(j) = ZERO
        A2v_AD(j) = ZERO
        A1h_AD(j) = ZERO
        A2h_AD(j) = ZERO
      ELSE
        Wind_Speed_AD = Wind_Speed_AD + &
                        ( AZCoeff%C(2,j,IHPOL) * ( EXP(-AZCoeff%C(6,j,IHPOL) * iVar%w18**2 ) - ONE ) * &
                          (         AZCoeff%C(3,j,IHPOL)               + &
                            TWO   * AZCoeff%C(4,j,IHPOL) * iVar%w18    + &
                            THREE * AZCoeff%C(5,j,IHPOL) * iVar%w18**2   ) - &
                          TWO * AZCoeff%C(2,j,IHPOL) * AZCoeff%C(6,j,IHPOL) * iVar%w18 * &
                          EXP(-AZCoeff%C(6,j,IHPOL) * iVar%w18**2 ) * &
                          ( AZCoeff%C(3,j,IHPOL) * iVar%w18    + &
                            AZCoeff%C(4,j,IHPOL) * iVar%w18**2 + &
                            AZCoeff%C(5,j,IHPOL) * iVar%w18**3   ) ) * A2h_AD(j)
        A2h_AD(j) = ZERO
        
        Wind_Speed_AD = Wind_Speed_AD + AZCoeff%C(1,j,IHPOL)*A1h_AD(j) 
        A1h_AD(j) = ZERO
      
        Wind_Speed_AD = Wind_Speed_AD + AZCoeff%C(6,j,IVPOL)*A2v_AD(j) 
        A2v_AD(j) = ZERO
      
        Wind_Speed_AD = Wind_Speed_AD + &
                        ( AZCoeff%C(1,j,IVPOL) * ( EXP(-AZCoeff%C(5,j,IVPOL) * iVar%w18**2 ) - ONE ) * &
                          (         AZCoeff%C(2,j,IVPOL)               + &
                            TWO   * AZCoeff%C(3,j,IVPOL) * iVar%w18    + &
                            THREE * AZCoeff%C(4,j,IVPOL) * iVar%w18**2   ) - &
                          TWO * AZCoeff%C(1,j,IVPOL) * AZCoeff%C(5,j,IVPOL) * iVar%w18 * &
                          EXP(-AZCoeff%C(5,j,IVPOL) * iVar%w18**2 ) * &
                          ( AZCoeff%C(2,j,IVPOL) * iVar%w18    + &
                            AZCoeff%C(3,j,IVPOL) * iVar%w18**2 + &
                            AZCoeff%C(4,j,IVPOL) * iVar%w18**3   ) ) * A1v_AD(j)
        A1v_AD(j) = ZERO
      END IF

    END DO Frequency_Loop

    
    ! Adjoint of the angle perturbation in radians
    Azimuth_Angle_AD = Azimuth_Angle_AD + DEGREES_TO_RADIANS*phi_AD
    
  END SUBROUTINE Azimuth_Emissivity_F6_AD


!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################


END MODULE Azimuth_Emissivity_F6_Module
