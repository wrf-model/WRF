!
! Guillou Ocean Permittivity module.
!
! Module containing routines to compute the complex permittivities for
! sea water based on
!
!   Guillou, C. et al. (1998) Impact of new permittivity measurements
!      on sea surface emissivity modeling in microwaves.
!      Radio Science, Volume 33, Number 3, Pages 649-667
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 11-Apr-2007
!                       paul.vandelst@noaa.gov
!

MODULE Guillou

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
  ! ...Datatypes
  PUBLIC :: iVar_type
  ! ...Procedures
  PUBLIC :: Guillou_Ocean_Permittivity
  PUBLIC :: Guillou_Ocean_Permittivity_TL
  PUBLIC :: Guillou_Ocean_Permittivity_AD

  
  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: Guillou.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'
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


  ! Parameters for the Guillou et al (1998) permittivity model
  ! ----------------------------------------------------------
  ! The coefficients for the sea water conductivity temperature
  ! polynomials. Eqn.(1) in reference. Note that these values
  ! have more precision than is reported in the ref.
  REAL(fp), PARAMETER :: D1_COEFF(0:2) = (/  0.086374_fp, &
                                             0.030606_fp, &
                                           -0.0004121_fp /)
  REAL(fp), PARAMETER :: D2_COEFF(0:2) = (/ 0.077454_fp, &
                                            0.001687_fp, &
                                          0.00001937_fp /)
  
  ! The coefficients for the static permittivity temperature
  ! polynomials. Eqn.(3) in reference. Note that these values
  ! have more precision than is reported in the ref.
  REAL(fp), PARAMETER :: A1_COEFF(0:5) = (/      81.820_fp, &
                                            -6.0503E-02_fp, &
                                            -3.1661E-02_fp, &
                                             3.1097E-03_fp, &
                                            -1.1791E-04_fp, &
                                             1.4838E-06_fp /)
  REAL(fp), PARAMETER :: A2_COEFF(0:5) = (/     0.12544_fp, &
                                             9.4037E-03_fp, &
                                            -9.5551E-04_fp, &
                                             9.0888E-05_fp, &
                                            -3.6011E-06_fp, &
                                             4.7130E-08_fp /)
  
  ! The coefficients for the high-frequency permittivity temperature
  ! polynomial. Eqn.(4) in reference. Note that these values
  ! have more precision than is reported in the ref.
  REAL(fp), PARAMETER :: B1_COEFF(0:5) = (/  6.4587_fp    , &
                                            -0.04203_fp   , &
                                            -0.0065881_fp , &
                                             0.00064924_fp, &
                                            -1.2328E-05_fp, &
                                             5.0433E-08_fp /)
                                            
  ! The coefficients for the relaxation time temperature
  ! polynomial. Eqn.(5) in reference. Note that these values
  ! have more precision than is reported in the ref.
  REAL(fp), PARAMETER :: C1_COEFF(0:5) = (/ 17.303_fp    , &
                                           -0.66651_fp   , &
                                            5.1482E-03_fp, &
                                            1.2145E-03_fp, &
                                           -5.0325E-05_fp, &
                                            5.8272E-07_fp /)
  REAL(fp), PARAMETER :: C2_COEFF(0:5) = (/-6.272E-03_fp , &
                                            2.357E-04_fp , &
                                            5.075E-04_fp , &
                                           -6.3983E-05_fp, &
                                            2.463E-06_fp , &
                                           -3.0676E-08_fp /)


  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: iVar_type
    PRIVATE
    REAL(fp) :: t=ZERO, s=ZERO                       ! Temperature in degC; salinity
    REAL(fp) :: f=ZERO, f2=ZERO, f0=ZERO, f2po=ZERO  ! Frequency terms
    REAL(fp) :: a1=ZERO, a2=ZERO, es=ZERO            ! Static permittivity temperature polynomials
    REAL(fp) :: einf=ZERO                            ! High-frequency permittivity temperature polynomial
    REAL(fp) :: c1=ZERO, c2=ZERO                     ! Relaxation time temperature polynomial
    REAL(fp) :: d1=ZERO, d2=ZERO                     ! Conductivity temperature polynomials 
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
!       Guillou_Ocean_Permittivity
!
! PURPOSE:
!       Subroutine to compute ocean permittivity according to the reference,
!         Guillou, C. et al. (1998) Impact of new permittivity measurements
!            on sea surface emissivity modeling in microwaves.
!            Radio Science, Volume 33, Number 3, Pages 649-667
!
! CALLING SEQUENCE:
!       CALL Guillou_Ocean_Permittivity( Temperature , & ! Input
!                                        Salinity    , & ! Input
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

  SUBROUTINE Guillou_Ocean_Permittivity( &
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
    REAL(fp) :: sigma
    REAL(fp) :: tau
    REAL(fp) :: re, ie


    ! Save the inputs
    ! ---------------
    iVar%t = Temperature - K_TO_C
    iVar%s = Salinity
  
  
    ! Conductivity
    ! ------------
    ! Compute the conductivity temperature polynomials
    ! Eqn.(1) in reference
    iVar%d1 = D1_COEFF(0) + iVar%t*(D1_COEFF(1) + iVar%t*D1_COEFF(2))
    iVar%d2 = D2_COEFF(0) + iVar%t*(D2_COEFF(1) + iVar%t*D2_COEFF(2))

    ! Compute the salinity dependent conductivity
    sigma = iVar%d1 + iVar%s*iVar%d2


    ! Static permittivity
    ! -------------------
    ! Compute the static permittivity temperature polynomials.
    ! Eqn.(3) in reference.
    iVar%a1 = A1_COEFF(0) + iVar%t*(A1_COEFF(1) + &
                              iVar%t*(A1_COEFF(2) + &
                                iVar%t*(A1_COEFF(3) + &
                                  iVar%t*(A1_COEFF(4) + &
                                    iVar%t*A1_COEFF(5) ))))
    iVar%a2 = A2_COEFF(0) + iVar%t*(A2_COEFF(1) + &
                              iVar%t*(A2_COEFF(2) + &
                                iVar%t*(A2_COEFF(3) + &
                                  iVar%t*(A2_COEFF(4) + &
                                    iVar%t*A2_COEFF(5) ))))
  
    ! Compute the salinity dependent static permittivity
    iVar%es = iVar%a1 - iVar%s*iVar%a2


    ! High frequency permittivity
    ! ---------------------------
    ! Compute the high-frequency permittivity temperature polynomial
    ! Eqn.(4) in reference
    iVar%einf = B1_COEFF(0) + iVar%t*(B1_COEFF(1) + &
                                iVar%t*(B1_COEFF(2) + &
                                  iVar%t*(B1_COEFF(3) + &
                                    iVar%t*(B1_COEFF(4) + &
                                      iVar%t*B1_COEFF(5) ))))
  

    ! Relaxation time
    ! ---------------
    ! Compute the Debye relaxation time temperature polynomials
    ! Eqn.(5) in reference
    iVar%c1 = C1_COEFF(0) + iVar%t*(C1_COEFF(1) + &
                              iVar%t*(C1_COEFF(2) + &
                                iVar%t*(C1_COEFF(3) + &
                                  iVar%t*(C1_COEFF(4) + &
                                    iVar%t*C1_COEFF(5) ))))
    iVar%c2 = C2_COEFF(0) + iVar%t*(C2_COEFF(1) + &
                              iVar%t*(C2_COEFF(2) + &
                                iVar%t*(C2_COEFF(3) + &
                                  iVar%t*(C2_COEFF(4) + &
                                    iVar%t*C2_COEFF(5) ))))
    
    ! Compute the salinity dependent relaxation time in picoseconds
    tau = iVar%c1 + iVar%s*iVar%c2


    ! Compute the complex permittivity
    ! --------------------------------
    ! The various frequency terms
    iVar%f  = TWOPI * Frequency * tau * SCALE_FACTOR
    iVar%f2 = iVar%f**2
    iVar%f0 = TWOPI * Frequency * GHZ_TO_HZ * E0
    
    iVar%f2po = ONE+iVar%f2
    
    ! The real part
    re = (iVar%es + iVar%einf*iVar%f2)/iVar%f2po

    ! The imaginary part
    ie = iVar%f*(iVar%es - iVar%einf)/iVar%f2po + sigma/iVar%f0
    
    ! Combine them
    Permittivity = CMPLX(re,-ie,fp)
    
  END SUBROUTINE Guillou_Ocean_Permittivity


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Guillou_Ocean_Permittivity_TL
!
! PURPOSE:
!       Subroutine to compute the tangent-linear ocean permittivity according
!       to the reference,
!         Guillou, C. et al. (1998) Impact of new permittivity measurements
!            on sea surface emissivity modeling in microwaves.
!            Radio Science, Volume 33, Number 3, Pages 649-667
!
! CALLING SEQUENCE:
!       CALL Guillou_Ocean_Permittivity_TL( Temperature_TL , & ! Input
!                                           Salinity_TL    , & ! Input
!                                           Frequency      , & ! Input
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
!       Salinity_TL:      Tangent-linear water salinity
!                         UNITS:      ppt (parts per thousand)
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
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
!                         ATTRIBUTES: INTENT(OUT)
!
! OUTPUTS:
!       Permittivity_TL:  Tangent-linear ocean permittivity
!                         UNITS:      N/A
!                         TYPE:       COMPLEX(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT)
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Guillou_Ocean_Permittivity_TL( &
    Temperature_TL , & ! Input
    Salinity_TL    , & ! Input
    Frequency      , & ! Input
    Permittivity_TL, & ! Output
    iVar             ) ! Internal variable input
    ! Arguments
    REAL(fp),        INTENT(IN)  :: Temperature_TL
    REAL(fp),        INTENT(IN)  :: Salinity_TL
    REAL(fp),        INTENT(IN)  :: Frequency
    COMPLEX(fp),     INTENT(OUT) :: Permittivity_TL
    TYPE(iVar_type), INTENT(IN)  :: iVar
    ! Local variables
    REAL(fp) :: d1_TL, d2_TL, sigma_TL
    REAL(fp) :: a1_TL, a2_TL, es_TL
    REAL(fp) :: einf_TL
    REAL(fp) :: c1_TL, c2_TL, tau_TL
    REAL(fp) :: f_TL, f2_TL, f2po_TL
    REAL(fp) :: inv_f2po
    REAL(fp) :: re_TL, ie_TL


    ! Conductivity
    ! ------------
    ! Compute the tangent-linear conductivity
    ! temperature polynomials. Eqn.(1) in reference
    d1_TL = (D1_COEFF(1) + iVar%t*TWO*D1_COEFF(2)) * Temperature_TL
    d2_TL = (D2_COEFF(1) + iVar%t*TWO*D2_COEFF(2)) * Temperature_TL

    ! Compute the tangent-linear salinity
    ! dependent conductivity
    sigma_TL = d1_TL + iVar%s*d2_TL + Salinity_TL*iVar%d2


    ! Static permittivity
    ! -------------------
    ! Compute the tangent-linear static permittivity
    ! temperature polynomials. Eqn.(3) in reference.
    a1_TL = (A1_COEFF(1) + iVar%t*(TWO*A1_COEFF(2)      + &
                             iVar%t*(THREE*A1_COEFF(3)  + &
                               iVar%t*(FOUR*A1_COEFF(4) + &
                                 iVar%t*FIVE*A1_COEFF(5)))) ) * Temperature_TL
    a2_TL = (A2_COEFF(1) + iVar%t*(TWO*A2_COEFF(2)      + &
                             iVar%t*(THREE*A2_COEFF(3)  + &
                               iVar%t*(FOUR*A2_COEFF(4) + &
                                 iVar%t*FIVE*A2_COEFF(5)))) ) * Temperature_TL
  
    ! Compute the tangent-linear salinity
    ! dependent static permittivity
    es_TL = a1_TL - iVar%s*a2_TL - Salinity_TL*iVar%a2


    ! High frequency permittivity
    ! ---------------------------
    ! Compute the tangent-linear high-frequency permittivity
    ! temperature polynomial. Eqn.(4) in reference
    einf_TL = (B1_COEFF(1) + iVar%t*(TWO*B1_COEFF(2)      + &
                               iVar%t*(THREE*B1_COEFF(3)  + &
                                 iVar%t*(FOUR*B1_COEFF(4) + &
                                   iVar%t*FIVE*B1_COEFF(5)))) ) * Temperature_TL


    ! Relaxation time
    ! ---------------
    ! Compute the tangent-linear Debye relaxation time
    ! temperature polynomials. Eqn.(5) in reference
    c1_TL = (C1_COEFF(1) + iVar%t*(TWO*C1_COEFF(2)      + &
                             iVar%t*(THREE*C1_COEFF(3)  + &
                               iVar%t*(FOUR*C1_COEFF(4) + &
                                 iVar%t*FIVE*C1_COEFF(5)))) ) * Temperature_TL
    c2_TL = (C2_COEFF(1) + iVar%t*(TWO*C2_COEFF(2)      + &
                             iVar%t*(THREE*C2_COEFF(3)  + &
                               iVar%t*(FOUR*C2_COEFF(4) + &
                                 iVar%t*FIVE*C2_COEFF(5)))) ) * Temperature_TL
    
    ! Compute the tangent-linear salinity
    ! dependent relaxation time in picoseconds
    tau_TL = c1_TL + iVar%s*c2_TL + Salinity_TL*iVar%c2


    ! Compute the complex permittivity
    ! --------------------------------
    ! The tangent-linear of various frequency terms
    f_TL  = TWOPI * Frequency * tau_TL * SCALE_FACTOR
    f2_TL = TWO * iVar%f * f_TL
    
    f2po_TL  = f2_TL
    inv_f2po = ONE/iVar%f2po
    
    ! The real part
    re_TL = inv_f2po*(iVar%f2*einf_TL + &
                      es_TL - &
                      inv_f2po*(iVar%es-iVar%einf)*f2po_TL)

    ! The imaginary part
    ie_TL = inv_f2po*(iVar%f*es_TL - &
                      iVar%f*einf_TL +&
                      inv_f2po*(iVar%es-iVar%einf)*(ONE-iVar%f2)*f_TL) + &
            sigma_TL/iVar%f0
    
    ! Combine them
    Permittivity_tl = CMPLX(re_TL, -ie_TL, fp)

  END SUBROUTINE Guillou_Ocean_Permittivity_TL


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Guillou_Ocean_Permittivity_AD
!
! PURPOSE:
!       Subroutine to compute the adjoint of the ocean permittivity according
!       to the reference,
!         Guillou, C. et al. (1998) Impact of new permittivity measurements
!            on sea surface emissivity modeling in microwaves.
!            Radio Science, Volume 33, Number 3, Pages 649-667
!
! CALLING SEQUENCE:
!       CALL Guillou_Ocean_Permittivity_AD( Permittivity_AD, & ! Input
!                                           Frequency      , & ! Input
!                                           Temperature_AD , & ! Output
!                                           Salinity_AD    , & ! Output
!                                           iVar             ) ! Internal variable input
!
! INPUTS:
!       Permittivity_AD:  Adjoint ocean permittivity
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
!                         ATTRIBUTES: INTENT(OUT)
!
! OUTPUTS:
!       Temperature_AD:   Adjoint sea surface temperature, de/dT.
!                         UNITS:      per Kelvin (K^-1)
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN OUT)
!
!       Salinity_AD:      Adjoint water salinity, de/dS
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

  SUBROUTINE Guillou_Ocean_Permittivity_AD( &
    Permittivity_AD, & ! Input
    Frequency      , & ! Input
    Temperature_AD , & ! Output
    Salinity_AD    , & ! Output
    iVar             ) ! Internal variable input
    ! Arguments
    COMPLEX(fp),     INTENT(IN OUT) :: Permittivity_AD
    REAL(fp),        INTENT(IN)     :: Frequency
    REAL(fp),        INTENT(IN OUT) :: Temperature_AD
    REAL(fp),        INTENT(IN OUT) :: Salinity_AD
    TYPE(iVar_type), INTENT(IN)     :: iVar
    ! Local variables
    REAL(fp) :: ie_AD, re_AD
    REAL(fp) :: f2po2
    REAL(fp) :: a1_AD, a2_AD, es_AD
    REAL(fp) :: einf_AD
    REAL(fp) :: c1_AD, c2_AD, tau_AD
    REAL(fp) :: d1_AD, d2_AD, sigma_AD
    REAL(fp) :: f2_AD, f_AD
        

    ! Complex permittivity
    ! --------------------
    ! Separate the real and imaginary parts
    ie_AD = -AIMAG(Permittivity_AD)
    re_AD = REAL(Permittivity_AD, fp)
    Permittivity_AD = ZERO
    
    ! Adjoint of the imaginary part
    f2po2 = iVar%f2po**2
    sigma_AD =  ie_AD/iVar%f0
    f2_AD    = -ie_AD*iVar%f*(iVar%es-iVar%einf)/f2po2
    einf_AD  = -ie_AD*iVar%f/iVar%f2po
    es_AD    =  ie_AD*iVar%f/iVar%f2po
    f_AD     =  ie_AD*(iVar%es-iVar%einf)/iVar%f2po 
    ie_AD = ZERO

    ! Adjoint of the real part
    f2_AD   = f2_AD   - re_AD*(iVar%es-iVar%einf)/f2po2
    einf_AD = einf_AD + re_AD*iVar%f2/iVar%f2po
    es_AD   = es_AD   + re_AD/iVar%f2po
    re_AD = ZERO

    ! Adjoint of the frequency terms
    f_AD  = f_AD + TWO*iVar%f*f2_AD
    f2_AD = ZERO
    tau_AD = TWOPI * Frequency * SCALE_FACTOR * f_AD
    f_AD   = ZERO


    ! Relaxation time
    ! ---------------
    ! Compute the adjoint of the salinity
    ! dependent relaxation time in picoseconds
    Salinity_AD = Salinity_AD + iVar%c2*tau_AD
    c2_AD       = iVar%s * tau_AD
    c1_AD       = tau_AD
    tau_AD      = ZERO

    ! Compute the adjoint of the Debye relaxation time
    ! temperature polynomials. Eqn.(5) in reference
    Temperature_AD = Temperature_AD + (C2_COEFF(1) + iVar%t*(TWO*C2_COEFF(2)      + &
                                                       iVar%t*(THREE*C2_COEFF(3)  + &
                                                         iVar%t*(FOUR*C2_COEFF(4) + &
                                                           iVar%t*FIVE*C2_COEFF(5)))) ) * c2_AD
    Temperature_AD = Temperature_AD + (C1_COEFF(1) + iVar%t*(TWO*C1_COEFF(2)      + &
                                                       iVar%t*(THREE*C1_COEFF(3)  + &
                                                         iVar%t*(FOUR*C1_COEFF(4) + &
                                                           iVar%t*FIVE*C1_COEFF(5)))) ) * c1_AD
    
    
    ! High frequency permittivity
    ! ---------------------------
    ! Compute the adjoint of the high-frequency permittivity
    ! temperature polynomial. Eqn.(4) in reference
    Temperature_AD = Temperature_AD + (B1_COEFF(1) + iVar%t*(TWO*B1_COEFF(2)      + &
                                                       iVar%t*(THREE*B1_COEFF(3)  + &
                                                         iVar%t*(FOUR*B1_COEFF(4) + &
                                                           iVar%t*FIVE*B1_COEFF(5)))) ) * einf_AD


    ! Static permittivity
    ! -------------------
    ! Compute the adjoint of the salinity
    ! dependent static permittivity
    Salinity_AD = Salinity_AD - iVar%a2*es_AD
    a2_AD = -iVar%s * es_AD
    a1_AD = es_AD
    es_AD = ZERO

    ! Compute the adjoint of the static permittivity
    ! temperature polynomials. Eqn.(3) in reference.
    Temperature_AD = Temperature_AD + (A2_COEFF(1) + iVar%t*(TWO*A2_COEFF(2)      + &
                                                       iVar%t*(THREE*A2_COEFF(3)  + &
                                                         iVar%t*(FOUR*A2_COEFF(4) + &
                                                           iVar%t*FIVE*A2_COEFF(5)))) ) * a2_AD
    Temperature_AD = Temperature_AD + (A1_COEFF(1) + iVar%t*(TWO*A1_COEFF(2)      + &
                                                       iVar%t*(THREE*A1_COEFF(3)  + &
                                                         iVar%t*(FOUR*A1_COEFF(4) + &
                                                           iVar%t*FIVE*A1_COEFF(5)))) ) * a1_AD

    ! Conductivity
    ! ------------
    ! Compute the adjoint of the salinity
    ! dependent conductivity
    Salinity_AD = Salinity_AD + iVar%d2*sigma_AD
    d2_AD = iVar%s * sigma_AD
    d1_AD = sigma_AD
    sigma_AD = ZERO    

    ! Compute the adjoint of the conductivity
    ! temperature polynomials. Eqn.(1) in reference
    Temperature_AD = Temperature_AD + (D2_COEFF(1) + iVar%t*TWO*D2_COEFF(2)) * d2_AD
    Temperature_AD = Temperature_AD + (D1_COEFF(1) + iVar%t*TWO*D1_COEFF(2)) * d1_AD

  END SUBROUTINE Guillou_Ocean_Permittivity_AD

END MODULE Guillou
