!
! Liu Ocean Permittivity module.
!
! Module containing routines to compute the complex permittivities for
! sea water based on
!
!   Liu, Q. et al. (2010) An improved fast microwave water emissivity model.
!      IEEE Trans. Geosci. Remote Sensing, accepted June 25, 2010
!
!
! CREATION HISTORY:
!       Written by:     Quanhua (Mark) Liu, JCSDA 30-Jul-2009
!                       Quanhua.Liu@noaa.gov

MODULE Liu

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds, ONLY: fp
  USE Fundamental_Constants, ONLY: PI, &
                                   E0 => PERMITTIVITY, &  ! Permittivity of vacuum (F/m)
                                   K_TO_C => STANDARD_TEMPERATURE ! Temperature units conversion
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! ... Datatypes
  PUBLIC :: iVar_type
  ! ... Procedures
  PUBLIC :: Liu_Ocean_Permittivity
  PUBLIC :: Liu_Ocean_Permittivity_TL
  PUBLIC :: Liu_Ocean_Permittivity_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: Liu.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'

  ! Literal constants
  ! -----------------
  REAL(fp), PARAMETER :: ZERO   = 0.0_fp
  REAL(fp), PARAMETER :: ONE    = 1.0_fp
  REAL(fp), PARAMETER :: TWO    = 2.0_fp
  REAL(fp), PARAMETER :: THREE  = 3.0_fp
  REAL(fp), PARAMETER :: FOUR   = 4.0_fp

  ! Scaling factors
  ! ---------------
  REAL(fp), PARAMETER :: GHZ_TO_HZ = 1.0e+09_fp ! Gigahertz   -> Hertz

  ! Fixed value for ionic conductivity denominator term, scaled
  ! for frequency. See the last term of eqn.(3) in reference.
  ! -----------------------------------------------------------
  REAL(fp), PARAMETER :: TAU0 = TWO*PI*E0*GHZ_TO_HZ
  
  ! Parameters for the Liu et al (2010) permittivity model
  ! ------------------------------------------------------
  ! The coefficients for the high-frequency permittivity temperature
  ! polynomial. Eqn.(4a) in reference.
  REAL(fp), PARAMETER :: EINF_COEFF(0:1) = (/ 3.8_fp, &
                                              2.48033e-02_fp /)
  
  ! The coefficients for the static permittivity temperature
  ! and salinity polynomials. Eqn.(4b) in reference.
  REAL(fp), PARAMETER :: ES_T_COEFF(0:3) = (/ 87.9181727_fp      , &
                                              -4.031592248e-01_fp, &
                                               9.493088010e-04_fp, &
                                              -1.930858348E-06_fp /)
  REAL(fp), PARAMETER :: ES_S_COEFF(0:2) = (/-2.697e-03_fp, &
                                             -7.3E-06_fp  , &
                                             -8.9E-06_fp   /)

  ! The coefficients for the intermediate frequency permittivity
  ! temperature and salinity polynomials. Eqn.(4c) in reference.
  REAL(fp), PARAMETER :: E1_T_COEFF(0:2) = (/ 5.723_fp     , &
                                              2.2379e-02_fp, &
                                             -7.1237e-04_fp /)
  REAL(fp), PARAMETER :: E1_S_COEFF(0:2) = (/-6.28908E-03_fp, &
                                              1.76032E-04_fp, &
                                             -9.22144E-05_fp /)

  ! The coefficients for the relaxation time temperature and
  ! salinity polynomials. Eqns.(4e) and (4f) in reference.
  REAL(fp), PARAMETER :: TAU1_T_COEFF(0:3) = (/ 1.124465e-01_fp , &
                                               -3.9815727e-03_fp, &
                                                8.113381e-05_fp , &
                                               -7.1824242e-07_fp /)
  REAL(fp), PARAMETER :: TAU1_S_COEFF(0:2) = (/-2.39357E-03_fp, &  
                                                3.1353E-05_fp , &  
                                               -2.52477E-07_fp /)  

  REAL(fp), PARAMETER :: TAU2_T_COEFF(0:3) = (/ 3.049979018e-03_fp, &
                                               -3.010041629E-05_fp, &
                                                4.811910733E-06_fp, &
                                               -4.259775841E-08_fp /)
  REAL(fp), PARAMETER :: TAU2_S_COEFF(0:2) = (/ 1.49e-01_fp, &
                                               -8.8E-04_fp , &
                                               -1.05E-04_fp /)

  ! The coefficients for the ionic conductivity exponential.
  ! Eqn.(4g) in reference.
  REAL(fp), PARAMETER :: ALPHA_COEFF = -4.259775841E-08_fp

  ! The coefficients for the ionic conductivity exponent term
  ! polynomial. Eqn.(4i) in reference.
  REAL(fp), PARAMETER :: BETA_COEFF(0:5) = (/ 2.033E-02_fp, &
                                              1.266E-04_fp, &
                                              2.464E-06_fp, &
                                             -1.849E-05_fp, &
                                              2.551E-07_fp, &
                                             -2.551E-08_fp /)
     
  ! The coefficients for the ionic conductivity at 25C polynomial.
  ! Eqn.(4j) in reference.
  REAL(fp), PARAMETER :: ALPHA25_COEFF(0:3) = (/ 1.82521e-01_fp, &
                                                -1.46192E-03_fp, &
                                                 2.09324E-05_fp, &
                                                -1.28205E-07_fp /)


  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: iVar_type
    PRIVATE
    REAL(fp) :: t=ZERO                             ! Temperature in deg.C
    REAL(fp) :: s=ZERO                             ! Salinity
    REAL(fp) :: delta=ZERO, beta=ZERO              ! Ionic conductivity components
    REAL(fp) :: alpha25=ZERO, alpha=ZERO           ! Ionic conductivity terms
    REAL(fp) :: es_t=ZERO, es_s=ZERO               ! The temperature and salinity es terms
    REAL(fp) :: e1_t=ZERO, e1_s=ZERO               ! The temperature and salinity e1 terms
    REAL(fp) :: tau1_t=ZERO, tau1_s=ZERO           ! The temperature and salinity tau1 terms
    REAL(fp) :: tau2_t=ZERO, tau2_s=ZERO           ! The temperature and salinity tau2 terms
    REAL(fp) :: f1=ZERO, f2=ZERO                   ! The relaxation compound terms, f.tau
    REAL(fp) :: del1=ZERO, del2=ZERO               ! The permittivity differences
  END TYPE iVar_type


CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Liu_Ocean_Permittivity
!
! PURPOSE:
!       Subroutine to compute ocean permittivity according to the reference,
!         Liu, Q. et al. (2010) An improved fast microwave water emissivity model.
!            IEEE Trans. Geosci. Remote Sensing, accepted June 25, 2010
!
! CALLING SEQUENCE:
!       CALL Liu_Ocean_Permittivity( Temperature , & ! Input
!                                    Salinity    , & ! Input
!                                    Frequency   , & ! Input
!                                    Permittivity, & ! Output
!                                    iVar          ) ! Internal variable output
!
! INPUTS:
!       Temperature:   Sea surface temperature
!                      UNITS:      Kelvin (K)
!                      TYPE:       REAL(fp)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!       Salinity:      Water salinity
!                      UNITS:      ppt (parts per thousand)
!                      TYPE:       REAL(fp)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!       Frequency:     Frequency
!                      UNITS:      GHz
!                      TYPE:       REAL(fp)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Permittivity:  Ocean permittivity
!                      UNITS:      N/A
!                      TYPE:       COMPLEX(fp)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!       iVar:          Structure containing internal variables required for
!                      subsequent tangent-linear or adjoint model calls.
!                      The contents of this structure are NOT accessible
!                      outside of this module.
!                      UNITS:      N/A
!                      TYPE:       TYPE(iVar_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Liu_Ocean_Permittivity( &
    Temperature , & ! Input
    Salinity    , & ! Input
    Frequency   , & ! Input
    Permittivity, & ! Output
    iVar          ) ! Internal variable output                   
    ! Arguments
    REAL(fp),        INTENT(IN)     :: Temperature
    REAL(fp),        INTENT(IN)     :: Salinity
    REAL(fp),        INTENT(IN)     :: Frequency
    COMPLEX(fp),     INTENT(OUT)    :: Permittivity
    TYPE(iVar_type), INTENT(IN OUT) :: iVar
    ! Local variables
    REAL(fp) :: einf
    REAL(fp) :: tau1, tau2, es, e1
    REAL(fp) :: re, ie

    ! Setup
    ! -----
    ! ...Initialise imaginary component of result
    ie = ZERO
    ! ...Save the inputs
    iVar%t = Temperature - K_TO_C
    iVar%s = Salinity


    ! Compute the TEMPERATURE polynomial parameterisations
    ! ----------------------------------------------------
    ! ...The high-frequency permittivity temperature polynomial (eqn.4a)
    einf = EINF_COEFF(0) + iVar%t*EINF_COEFF(1)
    ! ...The static permittivity temperature polynomial (eqn.4b)
    es = ES_T_COEFF(0) + iVar%t*(ES_T_COEFF(1) + &
                           iVar%t*(ES_T_COEFF(2) + &
                             iVar%t*ES_T_COEFF(3)))
    iVar%es_t = es  ! Save it
    ! ...The intermediate frequency permittivity temperature polynomial (eqn.4c)
    e1 = E1_T_COEFF(0) + iVar%t*(E1_T_COEFF(1) + &
                           iVar%t*E1_T_COEFF(2))
    iVar%e1_t = e1  ! Save it
    ! ...The Debye relaxation time constants temperature polynomials (eqns.4e & 4f)
    ! ...Units of tau: nanoseconds (for use with GHz frequencies)
    tau1 = TAU1_T_COEFF(0) + iVar%t*(TAU1_T_COEFF(1) + &
                               iVar%t*(TAU1_T_COEFF(2) + &
                                 iVar%t*TAU1_T_COEFF(3)))
    iVar%tau1_t = tau1  ! Save it
    tau2 = TAU2_T_COEFF(0) + iVar%t*(TAU2_T_COEFF(1) + &
                               iVar%t*(TAU2_T_COEFF(2) + &
                                 iVar%t*TAU2_T_COEFF(3)))
    iVar%tau2_t = tau2  ! Save it 


    ! Compute the SALINITY polynomial parameterisations
    ! -------------------------------------------------
    IF ( iVar%s > ZERO ) THEN
      ! ...The temperature difference from 25C (eqn.4h) used to compute ionic conductivity.
      iVar%delta = 25.0_fp - iVar%t
      ! ...The beta term (eqn.4i) used to compute ionic conductivity
      iVar%beta = BETA_COEFF(0) + iVar%delta*(BETA_COEFF(1) + &
                                    iVar%delta*BETA_COEFF(2)) + &
                  (BETA_COEFF(3) + iVar%delta*(BETA_COEFF(4) + &
                                     iVar%delta*BETA_COEFF(5)))*iVar%S
      ! ...The ionic conductivity at 25C (eqn.4j)
      iVar%alpha25 = iVar%s*(ALPHA25_COEFF(0) + &
                       iVar%s*(ALPHA25_COEFF(1) + &
                         iVar%s*(ALPHA25_COEFF(2) + &
                           iVar%s*ALPHA25_COEFF(3))))
      ! ...The ionic conductivity (eqn.4g)
      iVar%alpha = iVar%alpha25*EXP(-iVar%delta*iVar%beta)
      !  ...The imaginary component dependent on ionic conductivity (eqn.3)
      ie = -iVar%alpha/(Frequency*TAU0)


      ! ...The static permittivity salinity polynomial (eqn.4b)
      iVar%es_s = ONE + iVar%s*(ES_S_COEFF(0) + iVar%s*ES_S_COEFF(1) + iVar%t*ES_S_COEFF(2))
      es = es * iVar%es_s 


      ! ...The intermediate frequency permittivity salinity polynomial (eqn.4c)
      iVar%e1_s = ONE + iVar%s*(E1_S_COEFF(0) + iVar%s*E1_S_COEFF(1) + iVar%t*E1_S_COEFF(2))
      e1 = e1 * iVar%e1_s 


      ! ...The Debye relaxation time constants salinity polynomials (eqns.4e & 4f)
      ! ...Units of tau: nanoseconds (for use with GHz frequencies)
      iVar%tau1_s = ONE + iVar%s*(TAU1_S_COEFF(0) + iVar%t*(TAU1_S_COEFF(1) + &
                                                      iVar%t*TAU1_S_COEFF(2)))
      tau1 = tau1 * iVar%tau1_s
      iVar%tau2_s = ONE + iVar%s*(TAU2_S_COEFF(0) + iVar%t*TAU2_S_COEFF(1) + &
                                                    (iVar%s**2)*TAU2_S_COEFF(2))
      tau2 = tau2 * iVar%tau2_s

    END IF
    
    
    ! Compute the complex permittivity
    ! --------------------------------
    ! ...The compound terms
    iVar%f1 = Frequency*tau1  ! Note there is no GHz->Hz conversion.
    iVar%f2 = Frequency*tau2  ! That is embedded in the tau values.
    iVar%del1 = es - e1
    iVar%del2 = e1 - einf
    ! ...The real part
    re = einf + iVar%del1/(ONE + iVar%f1**2) + &
                iVar%del2/(ONE + iVar%f2**2)
    ! ...The imaginary part
    ie = -ie + iVar%del1*iVar%f1/(ONE + iVar%f1**2) + &
               iVar%del2*iVar%f2/(ONE + iVar%f2**2)
    ! ...Combine them (e = re - j.ie)
    Permittivity = CMPLX(re,-ie,fp)                                  

  END SUBROUTINE Liu_Ocean_Permittivity


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Liu_Ocean_Permittivity_TL
!
! PURPOSE:
!       Subroutine to compute the tangent-linear ocean permittivity according
!       to the model in the reference,
!         Liu, Q. et al. (2010) An improved fast microwave water emissivity model.
!            IEEE Trans. Geosci. Remote Sensing, accepted June 25, 2010
!
! CALLING SEQUENCE:
!       CALL Liu_Ocean_Permittivity_TL( Temperature_TL , &  ! TL Input
!                                       Salinity_TL    , &  ! TL Input
!                                       Frequency      , &  ! Invariant Input
!                                       Permittivity_TL, &  ! TL Output
!                                       iVar             )  ! Internal variable input
!
! INPUTS:
!       Temperature_TL:  Tangent-linear sea surface temperature.
!                        UNITS:      Kelvin (K)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Salinity_TL:     Tangent-linear water salinity.
!                        UNITS:      ppt (parts per thousand)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Frequency:       Frequency
!                        UNITS:      GHz
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       iVar:            Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of this module.
!                        UNITS:      N/A
!                        TYPE:       TYPE(iVar_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Permittivity_TL: Tangent-linear permittivity
!                        UNITS:      N/A
!                        TYPE:       COMPLEX(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Liu_Ocean_Permittivity_TL( &
    Temperature_TL , &  ! TL Input
    Salinity_TL    , &  ! TL Input                             
    Frequency      , &  ! Invariant input
    Permittivity_TL, &  ! TL Output
    iVar             )  ! Internal variable input
    ! Arguments
    REAL(fp),        INTENT(IN)  :: Temperature_TL
    REAL(fp),        INTENT(IN)  :: Salinity_TL
    REAL(fp),        INTENT(IN)  :: Frequency
    COMPLEX(fp),     INTENT(OUT) :: Permittivity_TL
    TYPE(iVar_type), INTENT(IN)  :: iVar
 
    ! Local variables
    REAL(fp) :: einf_TL
    REAL(fp) :: es_s_TL, es_TL
    REAL(fp) :: e1_s_TL, e1_TL
    REAL(fp) :: tau1_s_TL, tau1_TL
    REAL(fp) :: tau2_s_TL, tau2_TL
    REAL(fp) :: delta_TL, beta_TL
    REAL(fp) :: alpha25_TL, alpha_TL
    REAL(fp) :: f1_TL, f2_TL, del1_TL, del2_TL
    REAL(fp) :: x1, x2
    REAL(fp) :: re_TL, ie_TL


    ! Setup
    ! -----
    ! ...Initialise imaginary component of result
    ie_TL = ZERO
    

    ! Compute the tangent-linear TEMPERATURE polynomial parameterisations
    ! -------------------------------------------------------------------
    ! ...The high-frequency permittivity temperature polynomial (eqn.4a)
    einf_TL = EINF_COEFF(1)*Temperature_TL
    ! ...The static permittivity temperature polynomial (eqn.4b)
    es_TL = (ES_T_COEFF(1) + iVar%t*(TWO*ES_T_COEFF(2) + &
                               iVar%t*THREE*ES_T_COEFF(3))) * Temperature_TL
    ! ...The intermediate frequency permittivity temperature polynomial (eqn.4c)
    e1_TL = (E1_T_COEFF(1) + iVar%t*TWO*E1_T_COEFF(2)) * Temperature_TL
    ! ...The Debye relaxation time constants temperature polynomials (eqns.4e & 4f)
    ! ...Units of tau: nanoseconds (for use with GHz frequencies)
    tau1_TL = (TAU1_T_COEFF(1) + iVar%t*(TWO*TAU1_T_COEFF(2) + &
                                   iVar%t*THREE*TAU1_T_COEFF(3))) * Temperature_TL
    tau2_TL = (TAU2_T_COEFF(1) + iVar%t*(TWO*TAU2_T_COEFF(2) + &
                                   iVar%t*THREE*TAU2_T_COEFF(3))) * Temperature_TL


    ! Compute the SALINITY polynomial parameterisations
    ! -------------------------------------------------
    IF ( iVar%s > ZERO ) THEN
      ! ...The temperature difference from 25C (eqn.4h) used to compute ionic conductivity.
      delta_TL = -Temperature_TL
      ! ...The beta term (eqn.4i) used to compute ionic conductivity
      beta_TL = (BETA_COEFF(1) + iVar%delta*TWO*BETA_COEFF(2) + &
                 iVar%s*(BETA_COEFF(4) + iVar%delta*TWO*BETA_COEFF(5)))*delta_TL + &
                (BETA_COEFF(3) + iVar%delta*(BETA_COEFF(4) + &
                                   iVar%delta*BETA_COEFF(5)))*Salinity_TL
      ! ...The ionic conductivity at 25C (eqn.4j)
      alpha25_TL = (ALPHA25_COEFF(0) + &
                     iVar%s*(TWO*ALPHA25_COEFF(1) + &
                       iVar%s*(THREE*ALPHA25_COEFF(2) + &
                         iVar%s*FOUR*ALPHA25_COEFF(3))))*Salinity_TL
      ! ...The ionic conductivity (eqn.4g)
      alpha_TL = (alpha25_TL/iVar%alpha25 - iVar%delta*beta_TL - iVar%beta*delta_TL)*iVar%alpha
      !  ...The imaginary component dependent on ionic conductivity (eqn.3)
      ie_TL = -alpha_TL/(Frequency*TAU0)


      ! ...The static permittivity salinity polynomial (eqn.4b)
      es_s_TL = (ES_S_COEFF(0) + iVar%s*TWO*ES_S_COEFF(1) + iVar%t*ES_S_COEFF(2))*Salinity_TL + &
                iVar%s*ES_S_COEFF(2)*Temperature_TL
      es_TL = iVar%es_t*es_s_TL + iVar%es_s*es_TL


      ! ...The intermediate frequency permittivity salinity polynomial (eqn.4c)
      e1_s_TL = (E1_S_COEFF(0) + iVar%s*TWO*E1_S_COEFF(1) + iVar%t*E1_S_COEFF(2))*Salinity_TL + &
                iVar%s*E1_S_COEFF(2)*Temperature_TL
      e1_TL = iVar%e1_t*e1_s_TL + iVar%e1_s*e1_TL


      ! ...The Debye relaxation time constants salinity polynomials (eqns.4e & 4f)
      ! ...Units of tau: nanoseconds (for use with GHz frequencies)
      tau1_s_TL = (TAU1_S_COEFF(0) + iVar%t*(TAU1_S_COEFF(1) + &
                                       iVar%t*TAU1_S_COEFF(2)))*Salinity_TL + &
                  iVar%s*(TAU1_S_COEFF(1) + iVar%t*TWO*TAU1_S_COEFF(2))*Temperature_TL
      tau1_TL = iVar%tau1_t*tau1_s_TL + iVar%tau1_s*tau1_TL

      tau2_s_TL = (TAU2_S_COEFF(0) + iVar%t*TAU2_S_COEFF(1) + &
                                     (iVar%s**2)*THREE*TAU2_S_COEFF(2))*Salinity_TL + &
                  iVar%s*TAU2_S_COEFF(1)*Temperature_TL
      tau2_TL = iVar%tau2_t*tau2_s_TL + iVar%tau2_s*tau2_TL

    END IF


    ! Compute the complex permittivity
    ! --------------------------------
    ! ...The compound terms
    f1_TL = Frequency*tau1_TL  ! Note there is no GHz->Hz conversion.
    f2_TL = Frequency*tau2_TL  ! That is embedded in the tau values.
    del1_TL = es_TL - e1_TL
    del2_TL = e1_TL - einf_TL
    ! ...Calculate the denominator terms
    x1 = ONE + iVar%f1**2
    x2 = ONE + iVar%f2**2
    ! ...The real part
    re_TL = einf_TL + &
            (x1*del1_TL - iVar%del1*iVar%f1*TWO*f1_TL)/x1**2 + &
            (x2*del2_TL - iVar%del2*iVar%f2*TWO*f2_TL)/x2**2
    ! ...The imaginary part
    ie_TL = iVar%f1*del1_TL/x1 + &
            iVar%del1*(ONE - iVar%f1**2)*f1_TL/x1**2 + &
            iVar%f2*del2_TL/x2 + &
            iVar%del2*(ONE - iVar%f2**2)*f2_TL/x2**2 - &
            ie_TL
    ! ...Combine them (e = re - j.ie)
    Permittivity_TL = CMPLX(re_TL,-ie_TL,fp)                                     
 
  END SUBROUTINE Liu_Ocean_Permittivity_TL


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Liu_Ocean_Permittivity_AD
!
! PURPOSE:
!       Subroutine to compute the ocean permittivity adjoint according to the
!       model in the reference,
!         Liu, Q. et al. (2010) An improved fast microwave water emissivity model.
!            IEEE Trans. Geosci. Remote Sensing, accepted June 25, 2010
!
! CALLING SEQUENCE:
!       CALL Liu_Ocean_Permittivity_AD( Permittivity_AD, &  ! AD Input
!                                       Frequency      , &  ! Invariant Input
!                                       Temperature_AD , &  ! AD Output
!                                       Salinity_AD    , &  ! AD Output
!                                       iVar             )  ! Internal variable input
!
! INPUTS:
!       Permittivity_AD:  Adjoint ocean permittivity
!                         *** SET TO ZERO UPON EXIT ***
!                         UNITS:      N/A
!                         TYPE:       COMPLEX(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN OUT)
!
!       Frequency:        Frequency
!                         UNITS:      GHz
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       iVar:             Structure containing internal variables required for
!                         subsequent tangent-linear or adjoint model calls.
!                         The contents of this structure are NOT accessible
!                         outside of this module.
!                         UNITS:      N/A
!                         TYPE:       TYPE(iVar_type)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Temperature_AD:   Adjoint sea surface temperature, de/dT.
!                         *** MUST HAVE VALUE UPON ENTRY ***
!                         UNITS:      per Kelvin (K^-1)
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN OUT)
!
!       Salinity_AD:      Adjoint water salinity, de/dS
!                         *** MUST HAVE VALUE UPON ENTRY ***
!                         UNITS:      per ppt (parts-per-thousand^-1)
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN OUT)
!
! SIDE EFFECTS:
!       The input adjoint variable, Permittivity_AD, is set to zero upon
!       exiting this routine.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Liu_Ocean_Permittivity_AD( &
    Permittivity_AD, & ! AD Input
    Frequency      , & ! Invariant Input
    Temperature_AD , & ! AD Output
    Salinity_AD    , & ! AD Output 
    iVar             ) ! Internal variable input  
    IMPLICIT NONE                                                           
    ! Arguments
    COMPLEX(fp),     INTENT(IN OUT) :: Permittivity_AD
    REAL(fp),        INTENT(IN)     :: Frequency
    REAL(fp),        INTENT(IN OUT) :: Temperature_AD
    REAL(fp),        INTENT(IN OUT) :: Salinity_AD
    TYPE(iVar_type), INTENT(IN)     :: iVar
    ! Local variables
    REAL(fp) :: re_AD, ie_AD
    REAL(fp) :: x1, x2
    REAL(fp) :: f1_AD, f2_AD, del1_AD, del2_AD
    REAL(fp) :: delta_AD, beta_AD
    REAL(fp) :: alpha25_AD, alpha_AD
    REAL(fp) :: tau1_s_AD, tau1_AD
    REAL(fp) :: tau2_s_AD, tau2_AD
    REAL(fp) :: einf_AD
    REAL(fp) :: es_s_AD, es_AD
    REAL(fp) :: e1_s_AD, e1_AD


    ! Complex permittivity
    ! --------------------
    ! ...Separate the real and imaginary parts
    ie_AD = -AIMAG(Permittivity_AD)
    re_AD = REAL(Permittivity_AD, fp)
    Permittivity_AD = ZERO
    ! ...Calculate the denominator terms
    x1 = ONE + iVar%f1**2
    x2 = ONE + iVar%f2**2
    ! ...The imaginary part
    f2_AD = iVar%del2*(ONE - iVar%f2**2)*ie_AD/x2**2
    del2_AD = iVar%f2*ie_AD/x2
    f1_AD = iVar%del1*(ONE - iVar%f1**2)*ie_AD/x1**2
    del1_AD = iVar%f1*ie_AD/x1
    ie_AD = -ie_AD
    ! ...The real part
    f2_AD = f2_AD - (iVar%del2*iVar%f2*TWO*re_AD)/x2**2
    del2_AD = del2_AD + re_AD/x2
    f1_AD = f1_AD - (iVar%del1*iVar%f1*TWO*re_AD)/x1**2
    del1_AD = del1_AD + re_AD/x1
    einf_AD = re_AD
    ! ...The compound terms
    einf_AD = einf_AD - del2_AD
    e1_AD = del2_AD
    e1_AD = e1_AD - del1_AD
    es_AD = del1_AD
    tau2_AD = Frequency*f2_AD
    tau1_AD = Frequency*f1_AD


    ! Compute the SALINITY polynomial parameterisations
    ! -------------------------------------------------
    IF ( iVar%s > ZERO ) THEN
      ! ...The Debye relaxation time constants salinity polynomials (eqns.4e & 4f)
      ! ...Units of tau: nanoseconds (for use with GHz frequencies)
      tau2_s_AD = iVar%tau2_t*tau2_AD
      tau2_AD   = iVar%tau2_s*tau2_AD
      Salinity_AD = Salinity_AD + &
                    (TAU2_S_COEFF(0) + iVar%t*TAU2_S_COEFF(1) + &
                                       (iVar%s**2)*THREE*TAU2_S_COEFF(2))*tau2_s_AD
      Temperature_AD = Temperature_AD + &
                       iVar%s*TAU2_S_COEFF(1)*tau2_s_AD
      
      tau1_s_AD = iVar%tau1_t*tau1_AD
      tau1_AD   = iVar%tau1_s*tau1_AD
      Salinity_AD = Salinity_AD + &
                    (TAU1_S_COEFF(0) + iVar%t*(TAU1_S_COEFF(1) + &
                                       iVar%t*TAU1_S_COEFF(2)))*tau1_s_AD
      Temperature_AD = Temperature_AD + &
                       iVar%s*(TAU1_S_COEFF(1) + iVar%t*TWO*TAU1_S_COEFF(2))*tau1_s_AD


      ! ...The intermediate frequency permittivity salinity polynomial (eqn.4c)
      e1_s_AD = iVar%e1_t*e1_AD
      e1_AD   = iVar%e1_s*e1_AD
      Salinity_AD = Salinity_AD + &
                    (E1_S_COEFF(0) + iVar%s*TWO*E1_S_COEFF(1) + iVar%t*E1_S_COEFF(2))*e1_s_AD
      Temperature_AD = Temperature_AD + &
                       iVar%s*E1_S_COEFF(2)*e1_s_AD


      ! ...The static permittivity salinity polynomial (eqn.4b)
      es_s_AD = iVar%es_t*es_AD
      es_AD   = iVar%es_s*es_AD
      Salinity_AD = Salinity_AD + &
                    (ES_S_COEFF(0) + iVar%s*TWO*ES_S_COEFF(1) + iVar%t*ES_S_COEFF(2))*es_s_AD
      Temperature_AD = Temperature_AD + &
                       iVar%s*ES_S_COEFF(2)*es_s_AD
            

      !  ...The imaginary component dependent on ionic conductivity (eqn.3)
      alpha_AD = -ie_AD/(Frequency*TAU0)
      ! ...The ionic conductivity (eqn.4g)
      alpha25_AD = iVar%alpha*alpha_AD/iVar%alpha25
      beta_AD  = -iVar%delta*iVar%alpha*alpha_AD
      delta_AD = -iVar%beta*iVar%alpha*alpha_AD
      ! ...The ionic conductivity at 25C (eqn.4j)
      Salinity_AD = Salinity_AD + &
                    (ALPHA25_COEFF(0) + &
                     iVar%s*(TWO*ALPHA25_COEFF(1) + &
                       iVar%s*(THREE*ALPHA25_COEFF(2) + &
                         iVar%s*FOUR*ALPHA25_COEFF(3))))*alpha25_AD
      ! ...The beta term (eqn.4i) used to compute ionic conductivity
      Salinity_AD = Salinity_AD + &
                    (BETA_COEFF(3) + iVar%delta*(BETA_COEFF(4) + &
                                       iVar%delta*BETA_COEFF(5)))*beta_AD
      delta_AD = delta_AD + &
                 (BETA_COEFF(1) + iVar%delta*TWO*BETA_COEFF(2) + &
                 (BETA_COEFF(4) + iVar%delta*TWO*BETA_COEFF(5))*iVar%s)*beta_AD
      ! ...The temperature difference from 25C (eqn.4h) used to compute ionic conductivity.
      Temperature_AD = Temperature_AD - delta_AD
    END IF


    ! ...The Debye relaxation time constants temperature polynomials (eqns.4e & 4f)
    ! ...Units of tau: nanoseconds (for use with GHz frequencies)
    Temperature_AD = Temperature_AD + &
                     (TAU2_T_COEFF(1) + iVar%t*(TWO*TAU2_T_COEFF(2) + &
                                          iVar%t*THREE*TAU2_T_COEFF(3)))*tau2_AD
    Temperature_AD = Temperature_AD + &
                     (TAU1_T_COEFF(1) + iVar%t*(TWO*TAU1_T_COEFF(2) + &
                                          iVar%t*THREE*TAU1_T_COEFF(3)))*tau1_AD
    ! ...The intermediate frequency permittivity temperature polynomial (eqn.4c)
    Temperature_AD = Temperature_AD + &
                     (E1_T_COEFF(1) + iVar%t*TWO*E1_T_COEFF(2))*e1_AD
    ! ...The static permittivity salinity polynomial (eqn.4b)
    Temperature_AD = Temperature_AD + &
                     (ES_T_COEFF(1) + iVar%t*(TWO*ES_T_COEFF(2) + &
                                        iVar%t*THREE*ES_T_COEFF(3)))*es_AD
    ! ...The high-frequency permittivity temperature polynomial (eqn.4a)
    Temperature_AD = Temperature_AD + EINF_COEFF(1)*einf_AD

  END SUBROUTINE Liu_Ocean_Permittivity_AD

END MODULE Liu
