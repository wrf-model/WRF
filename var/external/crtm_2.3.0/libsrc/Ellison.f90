!
! Ellison Ocean Permittivity module.
!
! Module containing routines to compute the complex permittivities for
! sea water based on
!
!   Ellison, W.J. et al. (2003) A comparison of ocean emissivity models
!     using the Advanced Microwave Sounding Unit, the Special Sensor
!     Microwave Imager, the TRMM Microwave Imager, and airborne radiometer
!     observations. Journal of Geophysical Research, v108, D21, Pages ACL 1,1-14
!     doi:10.1029/2002JD0032132
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 11-Apr-2007
!                       paul.vandelst@noaa.gov
!

MODULE Ellison

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
  PUBLIC :: Ellison_Ocean_Permittivity
  PUBLIC :: Ellison_Ocean_Permittivity_TL
  PUBLIC :: Ellison_Ocean_Permittivity_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: Ellison.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'
  REAL(fp), PARAMETER :: ZERO   = 0.0_fp
  REAL(fp), PARAMETER :: POINT5 = 0.5_fp
  REAL(fp), PARAMETER :: ONE    = 1.0_fp
  REAL(fp), PARAMETER :: TWO    = 2.0_fp
  REAL(fp), PARAMETER :: THREE  = 3.0_fp
  REAL(fp), PARAMETER :: FOUR   = 4.0_fp
  REAL(fp), PARAMETER :: FIVE   = 5.0_fp
  REAL(fp), PARAMETER :: TWOPI  = TWO*PI


  ! Scaling factors (used here for documenting the conversion
  ! to SI units of the double Debye model denominator)
  ! ---------------------------------------------------------
  REAL(fp), PARAMETER :: PS_TO_S   = 1.0e-12_fp ! Picoseconds -> Seconds
  REAL(fp), PARAMETER :: GHZ_TO_HZ = 1.0e+09_fp ! Gigahertz   -> Hertz
  REAL(fp), PARAMETER :: SCALE_FACTOR = PS_TO_S * GHZ_TO_HZ


  ! Parameters for the Ellison et al (2003) permittivity model
  ! ----------------------------------------------------------
  ! The coefficients used to fit the Double Debye model
  REAL(fp), PARAMETER :: TAU1_COEFF(0:2)   = (/    17.535_fp, &
                                                 -0.61767_fp, &
                                                0.0089481_fp /)
  REAL(fp), PARAMETER :: TAU2_COEFF(0:3)   = (/    3.1842_fp, &
                                                 0.019189_fp, &
                                                -0.010873_fp, &
                                               0.00025818_fp /)
  REAL(fp), PARAMETER :: DELTA1_COEFF(0:3) = (/    68.396_fp, &
                                                 -0.40643_fp, &
                                                 0.022832_fp, &
                                              -0.00053061_fp /)
  REAL(fp), PARAMETER :: DELTA2_COEFF(0:3) = (/    4.7629_fp, &
                                                   0.1541_fp, &
                                                -0.033717_fp, &
                                               0.00084428_fp /)
  REAL(fp), PARAMETER :: EINF_COEFF(0:1)   = (/   5.31250_fp, &
                                               -0.0114770_fp /)
  REAL(fp), PARAMETER :: SIGMA_COEFF(0:1) = (/      2.906_fp, &
                                                  0.09437_fp /)


  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: iVar_type
    PRIVATE
    REAL(fp) :: t=ZERO                    ! Temperature in degC
    REAL(fp) :: f=ZERO, f2=ZERO, f0=ZERO  ! Frequency terms
    REAL(fp) :: tau1=ZERO  , tau2=ZERO    ! Relaxation frequencies
    REAL(fp) :: delta1=ZERO, delta2=ZERO  ! Delta terms
    REAL(fp) :: d1=ZERO    , d2=ZERO      ! Denominator terms
  END TYPE iVar_type


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Ellison_Ocean_Permittivity
!
! PURPOSE:
!       Subroutine to compute ocean permittivity according to the reference,
!         Ellison, W.J. et al. (2003) A comparison of ocean emissivity models
!           using the Advanced Microwave Sounding Unit, the Special Sensor
!           Microwave Imager, the TRMM Microwave Imager, and airborne radiometer
!           observations. Journal of Geophysical Research, v108, D21, ACL 1,1-14
!           doi:10.1029/2002JD0032132
!
! CALLING SEQUENCE:
!       CALL Ellison_Ocean_Permittivity( Temperature , & ! Input
!                                        Frequency   , & ! Input
!                                        Permittivity, & ! Output
!                                        iVar          ) ! Internal variable output
!
! INPUTS:
!       Temperature:   Sea surface temperature
!                      UNITS:      Kelvin (K)
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
!
! COMMENTS:
!       There is currently no salinity dependence.
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Ellison_Ocean_Permittivity( &
    Temperature , & ! Input
    Frequency   , & ! Input
    Permittivity, & ! Output
    iVar          ) ! Internal variable output
    ! Arguments
    REAL(fp),        INTENT(IN)     :: Temperature
    REAL(fp),        INTENT(IN)     :: Frequency
    COMPLEX(fp),     INTENT(OUT)    :: Permittivity
    TYPE(iVar_type), INTENT(IN OUT) :: iVar
    ! Local variables
    REAL(fp) :: einf
    REAL(fp) :: re1, re2
    REAL(fp) :: ie1, ie2
    REAL(fp) :: sigma, iesigma
    REAL(fp) :: re, ie


    ! Compute the various polynomial components of the double Debye model
    ! -------------------------------------------------------------------
    ! Compute temperature value for polynomials
    iVar%t = Temperature - K_TO_C

    ! Compute the Debye model relaxation frequencies
    ! (eqn on pg ACL 1-4 of Ellison et al. 2003)
    iVar%tau1 = TAU1_COEFF(0) + iVar%t*(TAU1_COEFF(1) + &
                                  iVar%t*TAU1_COEFF(2))
    iVar%tau2 = TAU2_COEFF(0) + iVar%t*(TAU2_COEFF(1)   + &
                                  iVar%t*(TAU2_COEFF(2) + &
                                    iVar%t*TAU2_COEFF(3)))

    ! Compute the delta terms
    ! (eqn on pg ACL 1-4 of Ellison et al. 2003)
    iVar%delta1 = DELTA1_COEFF(0) + iVar%t*(DELTA1_COEFF(1) + &
                                      iVar%t*(DELTA1_COEFF(2) + &
                                        iVar%t*DELTA1_COEFF(3)))
    iVar%delta2 = DELTA2_COEFF(0) + iVar%t*(DELTA2_COEFF(1) + &
                                      iVar%t*(DELTA2_COEFF(2) + &
                                        iVar%t*DELTA2_COEFF(3)))

    ! Compute the "infinite" permittivity term
    ! (No coeffs provided in ref. Taken from existing code)
    einf = EINF_COEFF(0) + iVar%t*EINF_COEFF(1)


    ! Compute the permittivities using the double Debye model
    ! (eqn on pg ACL 1-3 of Ellison et al. 2003)
    ! -------------------------------------------------------
    ! The common frequency terms
    iVar%f  = TWOPI * Frequency * SCALE_FACTOR
    iVar%f2 = iVar%f**2
    iVar%f0 = TWOPI * Frequency * GHZ_TO_HZ * E0

    ! The denominators of the double Debye model
    iVar%d1 = ONE + iVar%f2*iVar%tau1**2
    iVar%d2 = ONE + iVar%f2*iVar%tau2**2

    ! The real parts of the "delta" terms
    re1 = iVar%delta1 / iVar%d1
    re2 = iVar%delta2 / iVar%d2

    ! The imaginary parts of the "delta" terms
    ie1 = iVar%delta1 * iVar%f * iVar%tau1 / iVar%d1
    ie2 = iVar%delta2 * iVar%f * iVar%tau2 / iVar%d2

    ! The conductivity term
    sigma   = SIGMA_COEFF(0) + SIGMA_COEFF(1)*iVar%t
    iesigma = sigma / iVar%f0

    ! Construct the complex permittivity, e = e' - j.e"
    re = re1 + re2 + einf
    ie = ie1 + ie2 + iesigma
    Permittivity = CMPLX(re, -ie, fp)

  END SUBROUTINE Ellison_Ocean_Permittivity


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Ellison_Ocean_Permittivity_TL
!
! PURPOSE:
!       Subroutine to compute the tangent-linear ocean permittivity according
!       to the reference,
!         Ellison, W.J. et al. (2003) A comparison of ocean emissivity models
!           using the Advanced Microwave Sounding Unit, the Special Sensor
!           Microwave Imager, the TRMM Microwave Imager, and airborne radiometer
!           observations. Journal of Geophysical Research, v108, D21, ACL 1,1-14
!           doi:10.1029/2002JD0032132
!
! CALLING SEQUENCE:
!       CALL Ellison_Ocean_Permittivity_TL( Temperature_TL , & ! Input
!                                           Permittivity_TL, & ! Output
!                                           iVar             ) ! Internal variable input
!
! INPUTS:
!       Temperature_TL:   Tangent-linear sea surface temperature
!                         UNITS:      Kelvin (K)
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
!       Permittivity_TL:  Tangent-linear ocean permittivity
!                         UNITS:      N/A
!                         TYPE:       COMPLEX(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT)
!
! COMMENTS:
!       There is currently no salinity dependence.
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Ellison_Ocean_Permittivity_TL( &
    Temperature_TL , & ! Input
    Permittivity_TL, & ! Output
    iVar             ) ! Internal variable input
    ! Arguments
    REAL(fp),        INTENT(IN)  :: Temperature_TL
    COMPLEX(fp),     INTENT(OUT) :: Permittivity_TL
    TYPE(iVar_type), INTENT(IN)  :: iVar
    ! Local variables
    REAL(fp) :: t_TL
    REAL(fp) :: tau1_TL, tau2_TL
    REAL(fp) :: delta1_TL, delta2_TL, einf_TL
    REAL(fp) :: d1_TL, d2_TL
    REAL(fp) :: d12, d22
    REAL(fp) :: re1_TL, re2_TL
    REAL(fp) :: ie1_TL, ie2_TL
    REAL(fp) :: sigma_TL, iesigma_TL
    REAL(fp) :: re_TL, ie_TL


    ! Compute the tangent-linear for of the various
    ! polynomial components of the double Debye model
    ! -----------------------------------------------
    ! Compute temperature value for polynomials
    t_TL = Temperature_TL

    ! Compute the tangent-linear Debye model relaxation frequencies
    ! (eqn on pg ACL 1-4 of Ellison et al. 2003)
    tau1_TL = (TAU1_COEFF(1) + iVar%t*TWO*TAU1_COEFF(2)) * t_TL
    tau2_TL = (TAU2_COEFF(1) + iVar%t*(TWO*TAU2_COEFF(2) + iVar%t*THREE*TAU2_COEFF(3))) * t_TL

    ! Compute the tangent-linear delta terms
    ! (eqn on pg ACL 1-4 of Ellison et al. 2003)
    delta1_TL = (DELTA1_COEFF(1) + iVar%t*(TWO*DELTA1_COEFF(2) + iVar%t*THREE*DELTA1_COEFF(3))) * t_TL
    delta2_TL = (DELTA2_COEFF(1) + iVar%t*(TWO*DELTA2_COEFF(2) + iVar%t*THREE*DELTA2_COEFF(3))) * t_TL

    ! Compute the tangent-liner "infinite" permittivity term
    ! (No coeffs provided in ref. Taken from existing code)
    einf_TL = EINF_COEFF(1) * t_TL


    ! Compute the tangent-linear permittivities
    ! using the double Debye model
    ! (eqn on pg ACL 1-3 of Ellison et al. 2003)
    ! ------------------------------------------
    ! The tangent-linear denominators of the double Debye model
    d1_TL = TWO*iVar%f2*iVar%tau1*tau1_TL
    d2_TL = TWO*iVar%f2*iVar%tau2*tau2_TL

    ! The tangent-linear real parts of the "delta" terms
    d12 = iVar%d1**2
    d22 = iVar%d2**2
    re1_TL = (iVar%d1*delta1_TL - iVar%delta1*d1_TL) / d12
    re2_TL = (iVar%d2*delta2_TL - iVar%delta2*d2_TL) / d22

    ! The tangent-linear imaginary parts of the "delta" terms
    ie1_TL = iVar%f * (delta1_TL*iVar%tau1*iVar%d1 + iVar%delta1*tau1_TL*iVar%d1 - iVar%delta1*iVar%tau1*d1_TL) / d12
    ie2_TL = iVar%f * (delta2_TL*iVar%tau2*iVar%d2 + iVar%delta2*tau2_TL*iVar%d2 - iVar%delta2*iVar%tau2*d2_TL) / d22

    ! The conductivity term
    sigma_TL   = SIGMA_COEFF(1)*t_TL
    iesigma_TL = sigma_TL / iVar%f0

    ! Construct the complex permittivity, de = de' - j.de"
    re_TL = re1_TL + re2_TL + einf_TL
    ie_TL = ie1_TL + ie2_TL + iesigma_TL
    Permittivity_TL = CMPLX(re_TL, -ie_TL, fp)

  END SUBROUTINE Ellison_Ocean_Permittivity_TL


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Ellison_Ocean_Permittivity_AD
!
! PURPOSE:
!       Subroutine to compute the adjoint ocean permittivity according
!       to the reference,
!         Ellison, W.J. et al. (2003) A comparison of ocean emissivity models
!           using the Advanced Microwave Sounding Unit, the Special Sensor
!           Microwave Imager, the TRMM Microwave Imager, and airborne radiometer
!           observations. Journal of Geophysical Research, v108, D21, ACL 1,1-14
!           doi:10.1029/2002JD0032132
!
! CALLING SEQUENCE:
!       CALL Ellison_Ocean_Permittivity_AD( Permittivity_AD, & ! Input
!                                           Temperature_AD , & ! Output
!                                           iVar             ) ! Internal variable input
!
! INPUTS:
!       Permittivity_AD:  Adjoint ocean permittivity
!                         UNITS:      N/A
!                         TYPE:       COMPLEX(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN OUT)
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
!                         UNITS:      per Kelvin (K^-1)
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN OUT)
!
! SIDE EFFECTS:
!       The input adjoint variable, Permittivity_AD, is set to zero upon
!       exiting this routine.
!
! COMMENTS:
!       There is currently no salinity dependence.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Ellison_Ocean_Permittivity_AD( &
    Permittivity_AD, & ! Input
    Temperature_AD , & ! Output
    iVar             ) ! Internal variable input
    ! Arguments
    COMPLEX(fp),     INTENT(IN OUT) :: Permittivity_AD
    REAL(fp),        INTENT(IN OUT) :: Temperature_AD
    TYPE(iVar_type), INTENT(IN)     :: iVar
    ! Local variables
    REAL(fp) :: t_AD
    REAL(fp) :: tau1_AD, tau2_AD
    REAL(fp) :: delta1_AD, delta2_AD, einf_AD
    REAL(fp) :: d1_AD, d2_AD
    REAL(fp) :: d12, d22
    REAL(fp) :: m1 , m2
    REAL(fp) :: re1_AD, re2_AD
    REAL(fp) :: ie1_AD, ie2_AD
    REAL(fp) :: sigma_AD, iesigma_AD
    REAL(fp) :: re_AD, ie_AD

    ! Compute the adjoint of the permittivities
    ! using the double Debye model
    ! (eqn on pg ACL 1-3 of Ellison et al. 2003)
    ! ------------------------------------------
    ! The complex permittivity
    ie_AD = -AIMAG(Permittivity_AD)
    re_AD = REAL(Permittivity_AD,fp)
    Permittivity_AD = ZERO

    ! Initialise all the local adjoint variables
    iesigma_AD = ie_AD; ie2_AD = ie_AD; ie1_AD = ie_AD
    einf_AD    = re_AD; re2_AD = re_AD; re1_AD = re_AD

    ! The adjoint of the conductivity term
    sigma_AD = iesigma_AD / iVar%f0
    t_AD = SIGMA_COEFF(1)*sigma_AD

    ! The adjoints of the imaginary parts of the "delta" terms
    d22 = iVar%d2**2; m2 = iVar%f / d22
    d12 = iVar%d1**2; m1 = iVar%f / d12

    d2_AD     = -(iVar%delta2 * iVar%tau2 * m2 * ie2_AD )
    tau2_AD   =   iVar%delta2 * iVar%d2   * m2 * ie2_AD
    delta2_AD =   iVar%tau2   * iVar%d2   * m2 * ie2_AD

    d1_AD     = -(iVar%delta1 * iVar%tau1 * m1 * ie1_AD )
    tau1_AD   =   iVar%delta1 * iVar%d1   * m1 * ie1_AD
    delta1_AD =   iVar%tau1   * iVar%d1   * m1 * ie1_AD

    ! The adjoints of the real parts of the "delta" terms
    d2_AD     = d2_AD     - (re2_AD * iVar%delta2 / d22)
    delta2_AD = delta2_AD + (re2_AD / iVar%d2)

    d1_AD     = d1_AD     - (re1_AD * iVar%delta1 / d12)
    delta1_AD = delta1_AD + (re1_AD / iVar%d1)

    ! The adjoints of the denominators of the double Debye model
    tau2_AD = tau2_AD + (TWO * iVar%f2 * iVar%tau2 * d2_AD)
    tau1_AD = tau1_AD + (TWO * iVar%f2 * iVar%tau1 * d1_AD)


    ! Compute the adjoints of the various
    ! polynomial components of the double Debye model
    ! -----------------------------------------------
    ! Compute the adjoint of the "infinite" permittivity term
    ! (No coeffs provided in ref. Taken from existing code)
    t_AD = t_AD + EINF_COEFF(1) * einf_AD

    ! Compute the adjoint of the delta terms
    ! (eqn on pg ACL 1-4 of Ellison et al. 2003)
    t_AD = t_AD + (DELTA2_COEFF(1) + iVar%t*(TWO*DELTA2_COEFF(2) + &
                                       iVar%t*THREE*DELTA2_COEFF(3)) ) * delta2_AD
    t_AD = t_AD + (DELTA1_COEFF(1) + iVar%t*(TWO*DELTA1_COEFF(2) + &
                                       iVar%t*THREE*DELTA1_COEFF(3)) ) * delta1_AD

    ! Compute the adjoint of the  Debye model relaxation frequencies
    ! (eqn on pg ACL 1-4 of Ellison et al. 2003)
    t_AD = t_AD + (TAU2_COEFF(1) + iVar%t*(TWO*TAU2_COEFF(2) + &
                                     iVar%t*THREE*TAU2_COEFF(3)) ) * tau2_AD
    t_AD = t_AD + (TAU1_COEFF(1) + iVar%t*TWO*TAU1_COEFF(2)) * tau1_AD

    ! The return value
    Temperature_AD = Temperature_AD + t_AD

  END SUBROUTINE Ellison_Ocean_Permittivity_AD

END MODULE Ellison
