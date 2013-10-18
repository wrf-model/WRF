!
! Fresnel
!
! Module containing routines to compute Fresnel reflectivities.
!
!
! CREATION HISTORY:
!       Written by:     Masahiro Kazumori, JCSDA
!                       Masahiro.Kazumori@noaa.gov
!       Modified by:    Paul van Delst, CIMSS/SSEC 11-Apr-2007
!                       paul.vandelst@noaa.gov

MODULE Fresnel

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds, ONLY: fp
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: iVar_type
  PUBLIC :: Fresnel_Reflectivity
  PUBLIC :: Fresnel_Reflectivity_TL
  PUBLIC :: Fresnel_Reflectivity_AD
  
  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: Fresnel.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  REAL(fp), PARAMETER :: ZERO   = 0.0_fp
  REAL(fp), PARAMETER :: POINT5 = 0.5_fp
  REAL(fp), PARAMETER :: ONE    = 1.0_fp
  REAL(fp), PARAMETER :: TWO    = 2.0_fp


  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: iVar_type
    PRIVATE
    ! The intermediate terms
    COMPLEX(fp) :: z1, z2
    ! The real and imaginary components
    REAL(fp)    :: rzRv,izRv  ! Vertical
    REAL(fp)    :: rzRh,izRh  ! Horizontal
  END TYPE iVar_type


CONTAINS


!--------------------------------------------------------------------------------
!
! NAME:
!       Fresnel_Reflectivity
!
! PURPOSE:
!       Subroutine to compute Fresnel reflectivities
!
! CALLING SEQUENCE:
!       CALL Fresnel_Reflectivity( permittivity, &  ! Input
!                                  cos_i       , &  ! Input
!                                  Rv          , &  ! Output
!                                  Rh          , &  ! Output
!                                  iVar          )  ! Internal variable output
!
! INPUT ARGUMENTS:
!       permittivity:  Permittivity of medium
!                      UNITS:      Farads per metre (F.m^-1)
!                      TYPE:       COMPLEX(fp)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!       cos_i:         Cosine of incidence angle
!                      UNITS:      N/A
!                      TYPE:       REAL(fp)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Rv:            Reflectivity for polarisation parallel to the 
!                      plane of incidence (i.e. vertical polarization)
!                      UNITS:      N/A
!                      TYPE:       REAL(fp)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!       Rh:            Reflectivity for polarisation perpendicular to the
!                      plane of incidence (i.e. horizontal polarization)
!                      UNITS:      N/A
!                      TYPE:       REAL(fp)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!       iVar:          Structure containing internal variables required for
!                      subsequent tangent-linear or adjoint model calls.
!                      The contents of this structure are NOT accessible
!                      outside of the Fresnel module.
!                      UNITS:      N/A
!                      TYPE:       TYPE(iVar_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
! CREATION HISTORY:
!       Written by:     Masahiro Kazumori, JCSDA
!                       Masahiro.Kazumori@noaa.gov
!       Modified by:    Paul van Delst, CIMSS/SSEC 11-Apr-2007
!                       paul.vandelst@noaa.gov
!
!--------------------------------------------------------------------------------

  SUBROUTINE Fresnel_Reflectivity( permittivity, &  ! Input
                                   cos_i       , &  ! Input
                                   Rv          , &  ! Output
                                   Rh          , &  ! Output
                                   iVar          )  ! Internal variable output
    ! Arguments
    COMPLEX(fp),     INTENT(IN)     :: permittivity
    REAL(fp),        INTENT(IN)     :: cos_i
    REAL(fp),        INTENT(OUT)    :: Rv
    REAL(fp),        INTENT(OUT)    :: Rh
    TYPE(iVar_type), INTENT(IN OUT) :: iVar
    ! Local variables
    COMPLEX(fp) :: zRv ! Vertical
    COMPLEX(fp) :: zRh ! Horizontal

    ! Compute the complex reflectivity components
    iVar%z1 = SQRT(permittivity - ONE + (cos_i*cos_i))
    iVar%z2 = permittivity * cos_i
    zRh = (cos_i  -iVar%z1) / (cos_i  +iVar%z1)
    zRv = (iVar%z2-iVar%z1) / (iVar%z2+iVar%z1)

    ! The square of the vertical abs value
    iVar%rzRv = REAL(zRv,fp)
    iVar%izRv = AIMAG(zRv)
    Rv = iVar%rzRv**2 + iVar%izRv**2

    ! The square of the horizontal abs value
    iVar%rzRh = REAL(zRh,fp)
    iVar%izRh = AIMAG(zRh)
    Rh = iVar%rzRh**2 + iVar%izRh**2

  END SUBROUTINE Fresnel_Reflectivity


!--------------------------------------------------------------------------------
!
! NAME:
!       Fresnel_Reflectivity_TL
!
! PURPOSE:
!       Subroutine to compute tangent-linear Fresnel reflectivities
!
! CALLING SEQUENCE:
!       CALL Fresnel_Reflectivity_TL( permittivity_TL, &  ! Input
!                                     cos_i          , &  ! Input
!                                     Rv_TL          , &  ! Output
!                                     Rh_TL          , &  ! Output
!                                     iVar             )  ! Internal variable input
!
! INPUT ARGUMENTS:
!       permittivity_TL:  Tangent-linear permittivity of medium
!                         UNITS:      Farads per metre (F.m^-1)
!                         TYPE:       COMPLEX(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       cos_i             Cosine of incidence angle
!                         UNITS:      N/A
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       iVar:             Structure containing internal variables required for
!                         subsequent tangent-linear or adjoint model calls.
!                         The contents of this structure are NOT accessible
!                         outside of the Fresnel module.
!                         UNITS:      N/A
!                         TYPE:       TYPE(iVar_type)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Rv_TL             Tangent-linear reflectivity for polarisation parallel
!                         to the plane of incidence (i.e. vertical polarization)
!                         UNITS:      N/A
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT)
!
!       Rh_TL             Tangent-linear reflectivity for polarisation
!                         perpendicular to the plane of incidence
!                         (i.e. horizontal polarization)
!                         UNITS:      N/A
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT)
!
! CREATION HISTORY:
!       Written by:     Masahiro Kazumori, JCSDA
!                       Masahiro.Kazumori@noaa.gov
!       Modified by:    Paul van Delst, CIMSS/SSEC 11-Apr-2007
!                       paul.vandelst@noaa.gov
!
!--------------------------------------------------------------------------------

  SUBROUTINE Fresnel_Reflectivity_TL( permittivity_TL, &  ! Input
                                      cos_i          , &  ! Input
                                      Rv_TL          , &  ! Output
                                      Rh_TL          , &  ! Output
                                      iVar             )  ! Internal variable input
    ! Arguments
    COMPLEX(fp),     INTENT(IN)  :: permittivity_TL
    REAL(fp),        INTENT(IN)  :: cos_i
    REAL(fp),        INTENT(OUT) :: Rv_TL
    REAL(fp),        INTENT(OUT) :: Rh_TL
    TYPE(iVar_type), INTENT(IN)  :: iVar
    ! Local variables
    COMPLEX(fp) :: z1_TL, z2_TL
    COMPLEX(fp) :: zRv_TL           ! Vertical
    COMPLEX(fp) :: zRh_TL           ! Horizontal
    REAL(fp)    :: rzRv_TL,izRv_TL  ! Vertical
    REAL(fp)    :: rzRh_TL,izRh_TL  ! Horizontal

    ! Compute the tangent-linear complex reflectivity components
    z1_TL = POINT5 * permittivity_TL / iVar%z1
    z2_TL = cos_i * permittivity_TL
    zRh_TL = -TWO * cos_i * z1_TL / (cos_i+iVar%z1)**2
    zRv_TL =  TWO * (iVar%z1*z2_TL - iVar%z2*z1_TL) / (iVar%z2+iVar%z1)**2

    ! The square of the tangent-linear vertical abs value
    rzRv_TL = REAL(zRv_TL,fp)
    izRv_TL = AIMAG(zRv_TL)
    Rv_TL = TWO * (iVar%rzRv*rzRv_TL + iVar%izRv*izRv_TL)

    ! The square of the tangent-linear horizontal abs value
    rzRh_TL = REAL(zRh_TL,fp)
    izRh_TL = AIMAG(zRh_TL)
    Rh_TL = TWO * (iVar%rzRh*rzRh_TL + iVar%izRh*izRh_TL)

  END SUBROUTINE Fresnel_Reflectivity_TL


!--------------------------------------------------------------------------------
!
! NAME:
!       Fresnel_Reflectivity_AD
!
! PURPOSE:
!       Subroutine to compute Fresnel reflectivity adjoints
!
! CALLING SEQUENCE:
!       CALL Fresnel_Reflectivity_AD( Rv_AD          , &  ! Input
!                                     Rh_AD          , &  ! Input
!                                     cos_i          , &  ! Input
!                                     permittivity_AD, &  ! Output
!                                     iVar             )  ! Internal variable input
!
! INPUT ARGUMENTS:
!       Rv_AD             Adjoint reflectivity for polarisation parallel
!                         to the plane of incidence (i.e. vertical polarization)
!                         ** Set to ZERO on exit **
!                         UNITS:      N/A
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN OUT)
!
!       Rh_AD             Adjoint reflectivity for polarisation
!                         perpendicular to the plane of incidence
!                         (i.e. horizontal polarization)
!                         ** Set to ZERO on exit **
!                         UNITS:      N/A
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN OUT)
!
!       cos_i             Cosine of incidence angle
!                         UNITS:      N/A
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       iVar:             Structure containing internal variables required for
!                         subsequent tangent-linear or adjoint model calls.
!                         The contents of this structure are NOT accessible
!                         outside of the Fresnel module.
!                         UNITS:      N/A
!                         TYPE:       TYPE(iVar_type)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       permittivity_AD:  Adjoint permittivity of medium, dR/de,
!                         where R == reflectivity
!                               e == permittivity
!                         UNITS:      inverse Farads per metre (F.m^-1)^-1
!                         TYPE:       COMPLEX(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN OUT)
!
! CREATION HISTORY:
!       Written by:     Masahiro Kazumori, JCSDA
!                       Masahiro.Kazumori@noaa.gov
!       Modified by:    Paul van Delst, CIMSS/SSEC 11-Apr-2007
!                       paul.vandelst@noaa.gov
!
!--------------------------------------------------------------------------------

  SUBROUTINE Fresnel_Reflectivity_AD( Rv_AD          , &  ! Input
                                      Rh_AD          , &  ! Input
                                      cos_i          , &  ! Input
                                      permittivity_AD, &  ! Output
                                      iVar             )  ! Internal variable input
    ! Arguments
    REAL(fp),        INTENT(IN OUT) :: Rv_AD
    REAL(fp),        INTENT(IN OUT) :: Rh_AD
    REAL(fp),        INTENT(IN)     :: cos_i
    COMPLEX(fp),     INTENT(IN OUT) :: permittivity_AD
    TYPE(iVar_type), INTENT(IN)     :: iVar
    ! Local variables
    COMPLEX(fp) :: z1_AD, z2_AD
    COMPLEX(fp) :: zRv_AD           ! Vertical
    COMPLEX(fp) :: zRh_AD           ! Horizontal
    REAL(fp)    :: rzRv_AD,izRv_AD  ! Vertical
    REAL(fp)    :: rzRh_AD,izRh_AD  ! Horizontal
    COMPLEX(fp) :: denom

    ! The adjoint of the horizontal reflectivity
    izRh_AD = TWO*iVar%izRh*Rh_AD
    rzRh_AD = TWO*iVar%rzRh*Rh_AD
    Rh_AD  = ZERO
    zRh_AD = CMPLX(rzRh_AD, -izRh_AD, fp)  ! complex conjugate

    ! The adjoint of the vertical reflectivity
    izRv_AD = TWO*iVar%izRv*Rv_AD 
    rzRv_AD = TWO*iVar%rzRv*Rv_AD
    Rv_AD  = ZERO
    zRv_AD = CMPLX(rzRv_AD, -izRv_AD, fp)  ! complex conjugate

    ! The adjoint of the complex vertical polarised component
    denom = (iVar%z2+iVar%z1)**2
    z1_AD = -TWO*iVar%z2*zRv_AD / denom
    z2_AD =  TWO*iVar%z1*zRv_AD / denom
    
    ! The adjoint of the complex horizontal polarised component
    z1_AD = z1_AD - ( TWO*cos_i*zRh_AD / (cos_i+iVar%z1)**2 )
    
    ! The adjoint of the preserved variables
    permittivity_AD = permittivity_AD + CONJG(cos_i*z2_AD)
    permittivity_AD = permittivity_AD + CONJG(POINT5*z1_AD/iVar%z1)

  END SUBROUTINE Fresnel_Reflectivity_AD
  
END MODULE Fresnel
