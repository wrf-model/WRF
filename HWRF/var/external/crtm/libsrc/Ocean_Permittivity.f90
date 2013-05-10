!
! Ocean_Permittivity
!
! Module containing routines to compute complex permittivities for
! sea water.
!
! Two models are included in this module; that of
!
!   Guillou, C. et al. (1998) Impact of new permittivity measurements
!      on sea surface emissivity modeling in microwaves.
!      Radio Science, Volume 33, Number 3, Pages 649-667
!
! and of
!
!   Ellison, W.J. et al. (2003) A comparison of ocean emissivity models
!     using the Advanced Microwave Sounding Unit, the Special Sensor
!     Microwave Imager, the TRMM Microwave Imager, and airborne radiometer
!     observations. Journal of Geophysical Research, v108, D21, Pages ACL 1,1-14
!     doi:10.1029/2002JD0032132
!
! The former model was initially provided by Masahiro Kazumori (JMA visiting
! scientist to JCSDA) and that code was used to test this implementation of
! that model.
!
! The latter model is implemented in FASTEM-3 and that code was used to
! test this implementation of that model.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 11-Apr-2007
!                       paul.vandelst@ssec.wisc.edu
!

MODULE Ocean_Permittivity

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
  ! For Guillou model
  PUBLIC :: GuillouVariables_type
  PUBLIC :: Guillou_Ocean_Permittivity
  PUBLIC :: Guillou_Ocean_Permittivity_TL
  PUBLIC :: Guillou_Ocean_Permittivity_AD
  ! For Ellison model
  PUBLIC :: EllisonVariables_type
  PUBLIC :: Ellison_Ocean_Permittivity
  PUBLIC :: Ellison_Ocean_Permittivity_TL
  PUBLIC :: Ellison_Ocean_Permittivity_AD

  
  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: Ocean_Permittivity.f90 2272 2008-07-29 21:03:44Z paul.vandelst@noaa.gov $'
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
  ! For the Guillou et al (1998) permittivity model
  ! -----------------------------------------------
  TYPE :: GuillouVariables_type
    PRIVATE
    REAL(fp) :: t=ZERO, s=ZERO                       ! Temperature in degC; salinity
    REAL(fp) :: f=ZERO, f2=ZERO, f0=ZERO, f2po=ZERO  ! Frequency terms
    REAL(fp) :: a1=ZERO, a2=ZERO, es=ZERO            ! Static permittivity temperature polynomials
    REAL(fp) :: einf=ZERO                            ! High-frequency permittivity temperature polynomial
    REAL(fp) :: c1=ZERO, c2=ZERO                     ! Relaxation time temperature polynomial
    REAL(fp) :: d1=ZERO, d2=ZERO                     ! Conductivity temperature polynomials 
  END TYPE GuillouVariables_type
  
  ! For the Ellison et al (2003) permittivity model
  ! -----------------------------------------------
  TYPE :: EllisonVariables_type
    PRIVATE
    REAL(fp) :: t=ZERO                    ! Temperature in degC
    REAL(fp) :: f=ZERO, f2=ZERO, f0=ZERO  ! Frequency terms
    REAL(fp) :: tau1=ZERO  , tau2=ZERO    ! Relaxation frequencies
    REAL(fp) :: delta1=ZERO, delta2=ZERO  ! Delta terms
    REAL(fp) :: d1=ZERO    , d2=ZERO      ! Denominator terms
  END TYPE EllisonVariables_type


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
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
! INPUT ARGUMENTS:
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
! OUTPUT ARGUMENTS:
!       Permittivity:  Ocean permittivity
!                      UNITS:      N/A
!                      TYPE:       COMPLEX(fp)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!       iVar:          Structure containing internal variables required for
!                      subsequent tangent-linear or adjoint model calls.
!                      The contents of this structure are NOT accessible
!                      outside of the Ocean_Permittivity module.
!                      UNITS:      N/A
!                      TYPE:       TYPE(GuillouVariables_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE Guillou_Ocean_Permittivity( Temperature , & ! Input
                                         Salinity    , & ! Input
                                         Frequency   , & ! Input
                                         Permittivity, & ! Output
                                         iVar          ) ! Internal variable output
    ! Arguments
    REAL(fp),                    INTENT(IN)     :: Temperature
    REAL(fp),                    INTENT(IN)     :: Salinity
    REAL(fp),                    INTENT(IN)     :: Frequency
    COMPLEX(fp),                 INTENT(OUT)    :: Permittivity
    TYPE(GuillouVariables_type), INTENT(IN OUT) :: iVar
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
! INPUT ARGUMENTS:
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
!                         outside of the Ocean_Permittivity module.
!                         UNITS:      N/A
!                         TYPE:       TYPE(GuillouVariables_type)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT)
!
! OUTPUT ARGUMENTS:
!       Permittivity_TL:  Tangent-linear ocean permittivity
!                         UNITS:      N/A
!                         TYPE:       COMPLEX(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE Guillou_Ocean_Permittivity_TL( Temperature_TL , & ! Input
                                            Salinity_TL    , & ! Input
                                            Frequency      , & ! Input
                                            Permittivity_TL, & ! Output
                                            iVar             ) ! Internal variable input
    ! Arguments
    REAL(fp),                    INTENT(IN)  :: Temperature_TL
    REAL(fp),                    INTENT(IN)  :: Salinity_TL
    REAL(fp),                    INTENT(IN)  :: Frequency
    COMPLEX(fp),                 INTENT(OUT) :: Permittivity_TL
    TYPE(GuillouVariables_type), INTENT(IN)  :: iVar
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
! INPUT ARGUMENTS:
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
!                         outside of the Ocean_Permittivity module.
!                         UNITS:      N/A
!                         TYPE:       TYPE(GuillouVariables_type)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT)
!
! OUTPUT ARGUMENTS:
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
!--------------------------------------------------------------------------------

  SUBROUTINE Guillou_Ocean_Permittivity_AD( Permittivity_AD, & ! Input
                                            Frequency      , & ! Input
                                            Temperature_AD , & ! Output
                                            Salinity_AD    , & ! Output
                                            iVar             ) ! Internal variable input
    ! Arguments
    COMPLEX(fp),                 INTENT(IN OUT) :: Permittivity_AD
    REAL(fp),                    INTENT(IN)     :: Frequency
    REAL(fp),                    INTENT(IN OUT) :: Temperature_AD
    REAL(fp),                    INTENT(IN OUT) :: Salinity_AD
    TYPE(GuillouVariables_type), INTENT(IN)     :: iVar
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


!--------------------------------------------------------------------------------
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
!                                        Salinity    , & ! Input
!                                        Frequency   , & ! Input
!                                        Permittivity, & ! Output
!                                        iVar          ) ! Internal variable output
!
! INPUT ARGUMENTS:
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
! OUTPUT ARGUMENTS:
!       Permittivity:  Ocean permittivity
!                      UNITS:      N/A
!                      TYPE:       COMPLEX(fp)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!       iVar:          Structure containing internal variables required for
!                      subsequent tangent-linear or adjoint model calls.
!                      The contents of this structure are NOT accessible
!                      outside of the Ocean_Permittivity module.
!                      UNITS:      N/A
!                      TYPE:       TYPE(EllisonVariables_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
! COMMENTS:
!       There is currently no salinity dependence.
!
!--------------------------------------------------------------------------------

  SUBROUTINE Ellison_Ocean_Permittivity( Temperature , & ! Input
                                         Salinity    , & ! Input
                                         Frequency   , & ! Input
                                         Permittivity, & ! Output
                                         iVar          ) ! Internal variable output
    ! Arguments
    REAL(fp),                    INTENT(IN)     :: Temperature
    REAL(fp),                    INTENT(IN)     :: Salinity
    REAL(fp),                    INTENT(IN)     :: Frequency
    COMPLEX(fp),                 INTENT(OUT)    :: Permittivity
    TYPE(EllisonVariables_type), INTENT(IN OUT) :: iVar
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
!                                           Salinity_TL    , & ! Input
!                                           Frequency      , & ! Input
!                                           Permittivity_TL, & ! Output
!                                           iVar             ) ! Internal variable input
!
! INPUT ARGUMENTS:
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
!                         outside of the Ocean_Permittivity module.
!                         UNITS:      N/A
!                         TYPE:       TYPE(EllisonVariables_type)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Permittivity_TL:  Tangent-linear ocean permittivity
!                         UNITS:      N/A
!                         TYPE:       COMPLEX(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT)
!
! COMMENTS:
!       There is currently no salinity dependence.
!
!--------------------------------------------------------------------------------

  SUBROUTINE Ellison_Ocean_Permittivity_TL( Temperature_TL , & ! Input
                                            Salinity_TL    , & ! Input
                                            Frequency      , & ! Input
                                            Permittivity_TL, & ! Output
                                            iVar             ) ! Internal variable input
    ! Arguments
    REAL(fp),                    INTENT(IN)  :: Temperature_TL
    REAL(fp),                    INTENT(IN)  :: Salinity_TL
    REAL(fp),                    INTENT(IN)  :: Frequency
    COMPLEX(fp),                 INTENT(OUT) :: Permittivity_TL
    TYPE(EllisonVariables_type), INTENT(IN)  :: iVar
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
!                                           Frequency      , & ! Input
!                                           Temperature_AD , & ! Output
!                                           Salinity_AD    , & ! Output
!                                           iVar             ) ! Internal variable input
!
! INPUT ARGUMENTS:
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
!                         outside of the Ocean_Permittivity module.
!                         UNITS:      N/A
!                         TYPE:       TYPE(EllisonVariables_type)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
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
! COMMENTS:
!       There is currently no salinity dependence.
!
!--------------------------------------------------------------------------------

  SUBROUTINE Ellison_Ocean_Permittivity_AD( Permittivity_AD, & ! Input
                                            Frequency      , & ! Input
                                            Temperature_AD , & ! Output
                                            Salinity_AD    , & ! Output
                                            iVar             ) ! Internal variable input
    ! Arguments
    COMPLEX(fp),                 INTENT(IN OUT) :: Permittivity_AD
    REAL(fp),                    INTENT(IN)     :: Frequency
    REAL(fp),                    INTENT(IN OUT) :: Temperature_AD
    REAL(fp),                    INTENT(IN OUT) :: Salinity_AD
    TYPE(EllisonVariables_type), INTENT(IN)     :: iVar
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
    Salinity_AD    = ZERO  ! Fixed for now
    
  END SUBROUTINE Ellison_Ocean_Permittivity_AD

END MODULE Ocean_Permittivity
