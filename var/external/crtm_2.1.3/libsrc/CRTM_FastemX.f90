!
! CRTM_FastemX
!
! Module containing the Fastem4/5 procedures. The difference between the Fastem4
! and Fastem5 models is realised purely through the coefficients read during CRTM
! initialisation.
!
! CREATION HISTORY:
!       Written by:     Quanhua (Mark) Liu, Quanhua.Liu@noaa.gov
!                       Stephen English, Stephen.English@metoffice.gov.uk
!                       Fuzhong Weng, Fuzhong.Weng@noaa.gov
!                       July, 2009
!
!       Refactored by:  Paul van Delst, paul.vandelst@noaa.gov
!                       October, 2010
!
!       Modified by:    Quanhua Liu, Quanhua.Liu@noaa.gov
!                       Stephen English, Stephen.English@metoffice.gov.uk
!                       August, 2011
!
!       Refactored by:  Paul van Delst, paul.vandelst@noaa.gov
!                       December, 2011
!
!       Renamed:        Separate Fastem4 and Fastem5 modules replaced with
!                       single module named "FastemX"
!                       Paul van Delst, paul.vandelst@noaa.gov
!                       March, 2012
!

MODULE CRTM_FastemX

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds         , ONLY: fp
  USE MWwaterCoeff_Define, ONLY: MWwaterCoeff_type

  USE CRTM_Parameters, &
    ONLY: PI, DEGREES_TO_RADIANS, &
          ZERO, ONE, TWO, THREE, POINT_5

  USE Fresnel, &
    ONLY: fVar_type => iVar_type , &
          Fresnel_Reflectivity   , &
          Fresnel_Reflectivity_TL, &
          Fresnel_Reflectivity_AD

  USE Liu, &
    ONLY: pVar_type => iVar_type, &
          Ocean_Permittivity    => Liu_Ocean_Permittivity   , &
          Ocean_Permittivity_TL => Liu_Ocean_Permittivity_TL, &
          Ocean_Permittivity_AD => Liu_Ocean_Permittivity_AD

  USE Foam_Utility_Module, &
    ONLY: Foam_Coverage, &
          Foam_Coverage_TL, &
          Foam_Coverage_AD, &
          Foam_Reflectivity

  USE Small_Scale_Correction_Module, &
    ONLY: sscVar_type => iVar_type, &
          Small_Scale_Correction, &
          Small_Scale_Correction_TL, &
          Small_Scale_Correction_AD

  USE Large_Scale_Correction_Module, &
    ONLY: lscVar_type => iVar_type, &
          Large_Scale_Correction, &
          Large_Scale_Correction_TL, &
          Large_Scale_Correction_AD

  USE Reflection_Correction_Module, &
    ONLY: rcVar_type => iVar_type, &
          Reflection_Correction   , &
          Reflection_Correction_TL, &
          Reflection_Correction_AD

  USE Azimuth_Emissivity_Module, &
    ONLY: aeVar_type => iVar_type, &
          Azimuth_Emissivity   , &
          Azimuth_Emissivity_TL, &
          Azimuth_Emissivity_AD

  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Data types
  PUBLIC :: iVar_type
  ! Science routines
  PUBLIC :: Compute_FastemX
  PUBLIC :: Compute_FastemX_TL
  PUBLIC :: Compute_FastemX_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_FastemX.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'

  ! Stokes component information
  ! ...The number of Stokes components
  INTEGER, PARAMETER :: N_STOKES = 4
  ! ...The vector indices
  INTEGER, PARAMETER :: Iv_IDX = 1 ! Describes vertical polarization
  INTEGER, PARAMETER :: Ih_IDX = 2 ! Describes horizontal polarization
  INTEGER, PARAMETER :: U_IDX  = 3 ! Describes plane of polarization
  INTEGER, PARAMETER :: V_IDX  = 4 ! Describes ellipticity of polarization

  ! Switch for using LUT for Fastem5 large scale correction
!  LOGICAL, PUBLIC, PARAMETER :: FASTEM5_LUT = .FALSE.

  ! Invalid indicators
  REAL(fp), PARAMETER :: INVALID_AZIMUTH_ANGLE = -999.0_fp
  REAL(fp), PARAMETER :: INVALID_TRANSMITTANCE = -999.0_fp  ! Disable non-specular correction


  ! --------------------------------------
  ! Structure definition to hold internal
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: iVar_type
    PRIVATE
    ! Validity indicator
    LOGICAL :: Is_Valid = .FALSE.
    ! Forward model input values
    REAL(fp) :: Frequency    = ZERO
    REAL(fp) :: Zenith_Angle = ZERO
    REAL(fp) :: Temperature  = ZERO
    REAL(fp) :: Salinity     = ZERO
    REAL(fp) :: Wind_Speed   = ZERO
    ! ...Optional
    LOGICAL  :: Azimuth_Angle_Valid = .FALSE.
    REAL(fp) :: Azimuth_Angle       = ZERO
    LOGICAL  :: Transmittance_Valid = .FALSE.
    REAL(fp) :: Transmittance       = ZERO
    ! The zenith angle term
    REAL(fp) :: cos_z = ONE
    ! The permittivity term
    COMPLEX(fp) :: Permittivity = ZERO
    ! The Fresnel reflectivity terms
    REAL(fp) :: Rv_Fresnel = ZERO
    REAL(fp) :: Rh_Fresnel = ZERO
    ! Foam Terms
    REAL(fp) :: Rv_Foam = ZERO
    REAL(fp) :: Rh_Foam = ZERO
    REAL(fp) :: Foam_Cover = ZERO
    ! Large scale correction reflectivities
    REAL(fp) :: Rv_Large = ZERO
    REAL(fp) :: Rh_Large = ZERO
    ! Small scale correction factor
    REAL(fp) :: F_Small = ZERO
    ! Final reflectivities
    REAL(fp) :: Rv = ZERO
    REAL(fp) :: Rh = ZERO
    ! Azimuthal emissivity
    REAL(fp) :: e_Azimuth(N_STOKES) = ZERO
    ! Anisotropic downward radiation correction
    REAL(fp) :: Rv_Mod = ZERO
    REAL(fp) :: Rh_Mod = ZERO
    ! The final emissivity
    REAL(fp) :: e(N_STOKES) = ZERO
    ! Internal variables for subcomponents
    TYPE(pVar_type)   :: pVar
    TYPE(fVar_type)   :: fVar
    TYPE(sscVar_type) :: sscVar
    TYPE(lscVar_type) :: lscVar
    TYPE(aeVar_type)  :: aeVar
    TYPE(rcVar_type)  :: rcVar
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
!       Compute_FastemX
!
! PURPOSE:
!       Subroutine to compute the Fastem4 or Fastem5 microwave sea surface
!       emissivity and reflectivity.
!
! CALLING SEQUENCE:
!       CALL Compute_FastemX( &
!              MWwaterCoeff , &  ! Input
!              Frequency    , &  ! Input
!              Zenith_Angle , &  ! Input
!              Temperature  , &  ! Input
!              Salinity     , &  ! Input
!              Wind_Speed   , &  ! Input
!              iVar         , &  ! Internal variable output
!              Emissivity   , &  ! Output
!              Reflectivity , &  ! Output
!              Azimuth_Angle, &  ! Optional input
!              Transmittance  )  ! Optional input
!
!
! INPUTS:
!       MWwaterCoeff:   Microwave water emissivity model coefficient object.
!                       Load the object with the coefficients for the emissivity
!                       model to use (Fastem4 or Fastem5)
!                       UNITS:      N/A
!                       TYPE:       MWwaterCoeff_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Frequency:      Microwave frequency.
!                       UNITS:      GHz
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Zenith_Angle:   Sensor zenith angle at the sea surface
!                       UNITS:      Degrees
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Temperature:    Sea surface temperature
!                       UNITS:      Kelvin, K
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Salinity:       Water salinity
!                       UNITS:      ppt (parts per thousand)
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Wind_Speed:     Sea surface wind speed
!                       UNITS:      m/s
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       iVar:           Structure containing internal variables required for
!                       subsequent tangent-linear or adjoint model calls.
!                       The contents of this structure are NOT accessible
!                       outside of this module.
!                       UNITS:      N/A
!                       TYPE:       iVar_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
!       Emissivity:     The surface emissivity
!                       UNITS:      N/A
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1, 4-elements (n_Stokes)
!                       ATTRIBUTES: INTENT(OUT)
!
!       Reflectivity:   The surface reflectivity.
!                       UNITS:      N/A
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1, 4-elements (n_Stokes)
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Azimuth_Angle:  Relative azimuth angle (wind direction - sensor azimuth)
!                       UNITS:      Degrees
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Transmittance:  Total atmospheric transmittance
!                       UNITS:      N/A
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Compute_FastemX( &
    MWwaterCoeff , &  ! Input
    Frequency    , &  ! Input
    Zenith_Angle , &  ! Input
    Temperature  , &  ! Input
    Salinity     , &  ! Input
    Wind_Speed   , &  ! Input
    iVar         , &  ! Internal variable output
    Emissivity   , &  ! Output
    Reflectivity , &  ! Output
    Azimuth_Angle, &  ! Optional Input
    Transmittance  )  ! Optional Input
    ! Arguments
    TYPE(MWwaterCoeff_type), INTENT(IN)  :: MWwaterCoeff
    REAL(fp),                INTENT(IN)  :: Frequency
    REAL(fp),                INTENT(IN)  :: Zenith_Angle
    REAL(fp),                INTENT(IN)  :: Temperature
    REAL(fp),                INTENT(IN)  :: Salinity
    REAL(fp),                INTENT(IN)  :: Wind_Speed
    TYPE(iVar_type),         INTENT(OUT) :: iVar
    REAL(fp),                INTENT(OUT) :: Emissivity(:)
    REAL(fp),                INTENT(OUT) :: Reflectivity(:)
    REAL(fp),      OPTIONAL, INTENT(IN)  :: Azimuth_Angle
    REAL(fp),      OPTIONAL, INTENT(IN)  :: Transmittance

    ! Setup
    ! ...Save forward input variables for TL and AD calculations
    iVar%Frequency    = Frequency
    iVar%Zenith_Angle = Zenith_Angle
    iVar%Temperature  = Temperature
    iVar%Salinity     = Salinity
    iVar%Wind_Speed   = Wind_Speed
    ! ...Save derived variables
    iVar%cos_z = COS(Zenith_Angle*DEGREES_TO_RADIANS)

    ! Permittivity calculation
    CALL Ocean_Permittivity( Temperature, Salinity, Frequency, &
                             iVar%Permittivity, &
                             iVar%pVar )


    ! Fresnel reflectivity calculation
    CALL Fresnel_Reflectivity( iVar%Permittivity, iVar%cos_z, &
                               iVar%Rv_Fresnel, iVar%Rh_Fresnel, &
                               iVar%fVar )


    ! Foam reflectivity calculation
    CALL Foam_Reflectivity( &
           MWwaterCoeff%FRCoeff, &
           Zenith_Angle, &
           Frequency   , &
           iVar%Rv_Foam, &
           iVar%Rh_Foam  )


    ! Foam coverage calculation
    CALL Foam_Coverage( &
           MWwaterCoeff%FCCoeff, &
           Wind_Speed, &
           iVar%Foam_Cover )


    ! Large scale correction calculation
!    IF( FASTEM5_LUT ) THEN
!      CALL Large_Scale_Correction_LUT( Wind_Speed, Temperature, Zenith_Angle, Frequency, &
!                                 iVar%Rv_Large, iVar%Rh_Large )
!    ELSE
      CALL Large_Scale_Correction( &
             MWwaterCoeff%LSCCoeff, &
             iVar%Frequency , &
             iVar%cos_z     , &
             iVar%Wind_Speed, &
             iVar%Rv_Large  , &
             iVar%Rh_Large  , &
             iVar%lscVar      )
!    END IF

    ! Small scale correction calculation, Var%small_corr
    CALL Small_Scale_Correction( &
           MWwaterCoeff%SSCCoeff, &
           iVar%Frequency , &
           iVar%cos_z     , &
           iVar%Wind_Speed, &
           iVar%F_Small   , &
           iVar%sscVar      )

    ! Compute the first two Stokes components of the emissivity
    iVar%Rv = iVar%Rv_Fresnel*iVar%F_Small -iVar%Rv_Large
    iVar%Rh = iVar%Rh_Fresnel*iVar%F_Small -iVar%Rh_Large
    Emissivity(Iv_IDX) = ONE - (ONE-iVar%Foam_Cover)*iVar%Rv - iVar%Foam_Cover*iVar%Rv_Foam
    Emissivity(Ih_IDX) = ONE - (ONE-iVar%Foam_Cover)*iVar%Rh - iVar%Foam_Cover*iVar%Rh_Foam

    ! Azimuthal component calculation
    iVar%Azimuth_Angle_Valid = .FALSE.
    iVar%e_Azimuth = ZERO
    IF ( PRESENT(Azimuth_Angle) ) THEN
      IF ( ABS(Azimuth_Angle) <= 360.0_fp ) THEN
        CALL Azimuth_Emissivity( &
               MWwaterCoeff%AZCoeff, &
               iVar%Wind_Speed, &
               Azimuth_Angle  , &
               iVar%Frequency , &
               iVar%cos_z     , &
               iVar%e_Azimuth , &
               iVar%aeVar       )
        iVar%Azimuth_Angle_Valid = .TRUE.
        iVar%Azimuth_Angle       = Azimuth_Angle
      END IF
    END IF

    ! Anisotropic downward radiation correction calculation
    iVar%Transmittance_Valid = .FALSE.
    iVar%Rv_Mod = ONE
    iVar%Rh_Mod = ONE
    IF ( PRESENT(Transmittance) ) THEN
      IF ( Transmittance > ZERO .AND. Transmittance < ONE ) THEN
        CALL Reflection_Correction( &
               MWwaterCoeff%RCCoeff, &
               iVar%Frequency , &
               iVar%cos_z     , &
               iVar%Wind_Speed, &
               Transmittance  , &
               iVar%Rv_Mod    , &
               iVar%Rh_Mod    , &
               iVar%rcVar       )
        iVar%Transmittance_Valid = .TRUE.
        iVar%Transmittance       = Transmittance
      END IF
    END IF

    ! Assemble the return...
    ! ...emissivities
    Emissivity(Iv_IDX) = Emissivity(Iv_IDX) + iVar%e_Azimuth(Iv_IDX)
    Emissivity(Ih_IDX) = Emissivity(Ih_IDX) + iVar%e_Azimuth(Ih_IDX)
    Emissivity(U_IDX)  = iVar%e_Azimuth(U_IDX)
    Emissivity(V_IDX)  = iVar%e_Azimuth(V_IDX)
    ! ...reflectivties

    Reflectivity(Iv_IDX)      = iVar%Rv_Mod * (ONE-Emissivity(Iv_IDX))
    Reflectivity(Ih_IDX)      = iVar%Rh_Mod * (ONE-Emissivity(Ih_IDX))
    Reflectivity(U_IDX:V_IDX) = ZERO   ! 3rd, 4th Stokes from atmosphere are not included.
    ! ...save the emissivity for TL and AD reflectivity calculations
    iVar%e = Emissivity


    ! Flag the internal variable structure as valid
    iVar%Is_Valid = .TRUE.

  END SUBROUTINE Compute_FastemX


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_FastemX_TL
!
! PURPOSE:
!       Subroutine to compute the tangent-linear Fastem4 or Fastem5 microwave
!       sea surface emissivity and reflectivity.
!
!       NOTE: The forward model must be called first to fill the internal
!             variable argument with the intermediate forward calculations.
!
! CALLING SEQUENCE:
!       CALL Compute_FastemX_TL( &
!              MWwaterCoeff    , &
!              Temperature_TL  , &
!              Salinity_TL     , &
!              Wind_Speed_TL   , &
!              iVar            , &
!              Emissivity_TL   , &
!              Reflectivity_TL , &
!              Azimuth_Angle_TL, &
!              Transmittance_TL  )
!
!
! INPUTS:
!       MWwaterCoeff:      Microwave water emissivity model coefficient object.
!                          Load the object with the coefficients for the emissivity
!                          model to use (Fastem4 or Fastem5)
!                          UNITS:      N/A
!                          TYPE:       MWwaterCoeff_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Temperature_TL:    Tangent-linear sea surface temperature
!                          UNITS:      Kelvin, K
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Salinity_TL:       Tangent-linear water salinity
!                          UNITS:      ppt (parts per thousand)
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Wind_Speed_TL:     Tangent-linear sea surface wind speed
!                          UNITS:      m/s
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       iVar:              Structure containing internal variables computed
!                          in a previous forward model call.
!                          The contents of this structure are NOT accessible
!                          outside of this module.
!                          UNITS:      N/A
!                          TYPE:       iVar_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Emissivity_TL:     Tangent-linear surface emissivity
!                          UNITS:      N/A
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Rank-1, 4-elements (n_Stokes)
!                          ATTRIBUTES: INTENT(OUT)
!
!       Reflectivity_TL:   Tangent-linear surface reflectivity.
!                          UNITS:      N/A
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Rank-1, 4-elements (n_Stokes)
!                          ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Azimuth_Angle_TL:  Tangent-linear relative azimuth angle.
!                          This argument is ignored if a valid relative azimuth
!                          angle was not passed into the forward model call.
!                          UNITS:      Degrees
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Transmittance_TL:  Tangent-linear total atmospheric transmittance
!                          This argument is ignored if a valid transmittance
!                          was not passed into the forward model call.
!                          UNITS:      N/A
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Compute_FastemX_TL( &
    MWwaterCoeff    , &  ! Input
    Temperature_TL  , &  ! TL  Input
    Salinity_TL     , &  ! TL  Input
    Wind_Speed_TL   , &  ! TL  Input
    iVar            , &  ! Internal variable input
    Emissivity_TL   , &  ! TL Output
    Reflectivity_TL , &  ! TL Output
    Azimuth_Angle_TL, &  ! Optional TL  input
    Transmittance_TL  )  ! Optional TL  input
    ! Arguments
    TYPE(MWwaterCoeff_type), INTENT(IN)  :: MWwaterCoeff
    REAL(fp),                INTENT(IN)  :: Temperature_TL
    REAL(fp),                INTENT(IN)  :: Salinity_TL
    REAL(fp),                INTENT(IN)  :: Wind_Speed_TL
    TYPE(iVar_type),         INTENT(IN)  :: iVar
    REAL(fp),                INTENT(OUT) :: Emissivity_TL(:)
    REAL(fp),                INTENT(OUT) :: Reflectivity_TL(:)
    REAL(fp),      OPTIONAL, INTENT(IN)  :: Azimuth_Angle_TL
    REAL(fp),      OPTIONAL, INTENT(IN)  :: Transmittance_TL
    ! Local variables
    REAL(fp) :: Rv_Fresnel_TL, Rh_Fresnel_TL
    REAL(fp) :: Rv_Foam_TL   , Rh_Foam_TL
    REAL(fp) :: Rv_Large_TL  , Rh_Large_TL
    REAL(fp) :: Rv_TL        , Rh_TL
    REAL(fp) :: Rv_Mod_TL    , Rh_Mod_TL
    REAL(fp) :: Foam_Cover_TL
    REAL(fp) :: e_Azimuth_TL(N_STOKES)
    COMPLEX(fp) :: Permittivity_TL
    REAL(fp) :: f_small_TL

    ! Check internal structure
    IF ( .NOT. iVar%Is_Valid ) THEN
      Emissivity_TL   = ZERO
      Reflectivity_TL = ZERO
      RETURN
    END IF


    ! Permittivity calculation
    CALL Ocean_Permittivity_TL( Temperature_TL, Salinity_TL, iVar%Frequency, &
                                Permittivity_TL, &
                                iVar%pVar)


    ! Fresnel reflectivity calculation
    CALL Fresnel_Reflectivity_TL( permittivity_TL, iVar%cos_z, &
                                  Rv_Fresnel_TL, Rh_Fresnel_TL, &
                                  iVar%fVar )


    ! Foam reflectivity "calculation"
    Rv_Foam_TL = ZERO
    Rh_Foam_TL = ZERO


    ! Foam coverage calculation
    CALL Foam_Coverage_TL( &
           MWwaterCoeff%FCCoeff, &
           iVar%Wind_Speed, &
           Wind_Speed_TL, &
           Foam_Cover_TL )


    ! Large Scale Correction Calculation
!    IF( FASTEM5_LUT ) THEN
!      CALL Large_Scale_Correction_LUT_TL( iVar%Wind_Speed, iVar%Temperature, iVar%Zenith_Angle,  &
!           iVar%Frequency, Wind_Speed_TL, Temperature_TL, Rv_Large_TL, Rh_Large_TL   )
!    ELSE
      CALL Large_Scale_Correction_TL( &
             Wind_Speed_TL, &
             Rv_Large_TL  , &
             Rh_Large_TL  , &
             iVar%lscVar    )
!    END IF

    ! Small Scale Correction Calculation
    CALL Small_Scale_Correction_TL( &
           MWwaterCoeff%SSCCoeff, &
           Wind_Speed_TL, &
           f_small_TL   , &
           iVar%sscVar    )

    ! Compute the first two Stokes components of the tangent-linear emissivity
    Rv_TL = Rv_Fresnel_TL*iVar%F_Small + iVar%Rv_Fresnel*f_small_TL - Rv_Large_TL
    Emissivity_TL(Iv_IDX) = (iVar%Foam_Cover-ONE)*Rv_TL + &
                            (iVar%Rv-iVar%Rv_Foam)*Foam_Cover_TL - &
                            iVar%Foam_Cover*Rv_Foam_TL
    Rh_TL = Rh_Fresnel_TL*iVar%F_Small + iVar%Rh_Fresnel*f_small_TL - Rh_Large_TL
    Emissivity_TL(Ih_IDX) = (iVar%Foam_Cover-ONE)*Rh_TL + &
                            (iVar%Rh-iVar%Rh_Foam)*Foam_Cover_TL - &
                            iVar%Foam_Cover*Rh_Foam_TL

    ! Azimuthal component calculation
    IF ( PRESENT(Azimuth_Angle_TL) .AND. iVar%Azimuth_Angle_Valid ) THEN
      CALL Azimuth_Emissivity_TL( &
             MWwaterCoeff%AZCoeff, &
             Wind_Speed_TL   , &
             Azimuth_Angle_TL, &
             e_Azimuth_TL    , &
             iVar%aeVar        )
    ELSE
      e_Azimuth_TL = ZERO
    END IF


    ! Anisotropic downward radiation correction calculation
    IF ( PRESENT(Transmittance_TL) .AND. iVar%Transmittance_Valid ) THEN
      CALL Reflection_Correction_TL( &
             MWwaterCoeff%RCCoeff, &
             Wind_Speed_TL   , &
             Transmittance_TL, &
             Rv_Mod_TL       , &
             Rh_Mod_TL       , &
             iVar%rcVar        )
    ELSE
      Rv_Mod_TL = ZERO
      Rh_Mod_TL = ZERO
    END IF


    ! Compute the tangent-linear...
    ! ...emissivities
    Emissivity_TL(Iv_IDX) = Emissivity_TL(Iv_IDX) + e_Azimuth_TL(Iv_IDX)
    Emissivity_TL(Ih_IDX) = Emissivity_TL(Ih_IDX) + e_Azimuth_TL(Ih_IDX)
    Emissivity_TL(U_IDX)  = e_Azimuth_TL(U_IDX)
    Emissivity_TL(V_IDX)  = e_Azimuth_TL(V_IDX)
    ! ...reflectivities
    Reflectivity_TL(Iv_IDX)      = (ONE-iVar%e(Iv_IDX))*Rv_Mod_TL - iVar%Rv_Mod*Emissivity_TL(Iv_IDX)
    Reflectivity_TL(Ih_IDX)      = (ONE-iVar%e(Ih_IDX))*Rh_Mod_TL - iVar%Rh_Mod*Emissivity_TL(Ih_IDX)
    Reflectivity_TL(U_IDX:V_IDX) = ZERO  ! 3rd, 4th Stokes from atmosphere are not included.

  END SUBROUTINE Compute_FastemX_TL


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_FastemX_AD
!
! PURPOSE:
!       Subroutine to compute the adjoint Fastem4 or Fastem5 microwave
!       sea surface emissivity and reflectivity.
!
!       NOTE: The forward model must be called first to fill the internal
!             variable argument with the intermediate forward calculations.
!
! CALLING SEQUENCE:
!       CALL Compute_FastemX_AD( &
!              MWwaterCoeff    , &
!              Emissivity_AD   , &
!              Reflectivity_AD , &
!              iVar            , &
!              Temperature_AD  , &
!              Salinity_AD     , &
!              Wind_Speed_AD   , &
!              Azimuth_Angle_AD, &
!              Transmittance_AD  )
!
!
! INPUTS:
!       MWwaterCoeff:      Microwave water emissivity model coefficient object.
!                          Load the object with the coefficients for the emissivity
!                          model to use (Fastem4 or Fastem5)
!                          UNITS:      N/A
!                          TYPE:       MWwaterCoeff_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Emissivity_AD:     Adjoint surface emissivity
!                          ***SET TO ZERO UPON EXIT***
!                          UNITS:      N/A
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Rank-1, 4-elements (n_Stokes)
!                          ATTRIBUTES: INTENT(IN OUT)
!
!       Reflectivity_AD:   Adjoint surface reflectivity.
!                          ***SET TO ZERO UPON EXIT***
!                          UNITS:      N/A
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Rank-1, 4-elements (n_Stokes)
!                          ATTRIBUTES: INTENT(IN OUT)
!
!       iVar:              Structure containing internal variables computed
!                          in a previous forward model call.
!                          The contents of this structure are NOT accessible
!                          outside of this module.
!                          UNITS:      N/A
!                          TYPE:       iVar_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Temperature_AD:    Adjoint sea surface temperature
!                          ***MUST CONTAIN VALUE UPON ENTRY***
!                          UNITS:      K^-1
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
!       Salinity_AD:       Adjoint water salinity
!                          ***MUST CONTAIN VALUE UPON ENTRY***
!                          UNITS:      ppt^-1 (parts per thousand)
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
!       Wind_Speed_AD:     Adjoint sea surface wind speed
!                          ***MUST CONTAIN VALUE UPON ENTRY***
!                          UNITS:      (m/s)^-1
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       Azimuth_Angle_AD:  Adjoint relative azimuth angle.
!                          This argument is ignored if a valid relative azimuth
!                          angle was not passed into the forward model call.
!                          UNITS:      Degrees^-1
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Transmittance_AD:  Adjoint total atmospheric transmittance
!                          This argument is ignored if a valid transmittance
!                          was not passed into the forward model call.
!                          UNITS:      N/A
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Compute_FastemX_AD( &
    MWwaterCoeff    , &  ! Input
    Emissivity_AD   , &  ! AD Input
    Reflectivity_AD , &  ! AD Input
    iVar            , &  ! Internal variable input
    Temperature_AD  , &  ! AD Output
    Salinity_AD     , &  ! AD Output
    Wind_Speed_AD   , &  ! AD Output
    Azimuth_Angle_AD, &  ! Optional AD Output
    Transmittance_AD )   ! Optional AD Output
    ! Arguments
    TYPE(MWwaterCoeff_type), INTENT(IN)  :: MWwaterCoeff
    REAL(fp),                INTENT(IN OUT) :: Emissivity_AD(:)
    REAL(fp),                INTENT(IN OUT) :: Reflectivity_AD(:)
    TYPE(iVar_type),         INTENT(IN)     :: iVar
    REAL(fp),                INTENT(IN OUT) :: Temperature_AD
    REAL(fp),                INTENT(IN OUT) :: Salinity_AD
    REAL(fp),                INTENT(IN OUT) :: Wind_Speed_AD
    REAL(fp),      OPTIONAL, INTENT(IN OUT) :: Azimuth_Angle_AD
    REAL(fp),      OPTIONAL, INTENT(IN OUT) :: Transmittance_AD
    ! Local variables
    REAL(fp) :: Rv_Fresnel_AD, Rh_Fresnel_AD
    REAL(fp) :: Rv_Foam_AD   , Rh_Foam_AD
    REAL(fp) :: Rv_Large_AD  , Rh_Large_AD
    REAL(fp) :: Rv_AD        , Rh_AD
    REAL(fp) :: Rv_Mod_AD    , Rh_Mod_AD
    REAL(fp) :: Foam_Cover_AD
    REAL(fp) :: e_Azimuth_AD(N_STOKES)
    COMPLEX(fp) :: Permittivity_AD
    REAL(fp) :: f_small_AD

    ! Check internal structure
    IF ( .NOT. iVar%Is_Valid ) THEN
      Temperature_AD = ZERO
      Salinity_AD    = ZERO
      Wind_Speed_AD  = ZERO
      IF ( PRESENT(Azimuth_Angle_AD) ) Azimuth_Angle_AD = ZERO
      IF ( PRESENT(Transmittance_AD) ) Transmittance_AD = ZERO
      RETURN
    END IF


    ! Compute the adjoint of the...
    ! ...reflectivities
    Reflectivity_AD(U_IDX:V_IDX) = ZERO  ! 3rd, 4th Stokes from atmosphere are not included.

    Emissivity_AD(Ih_IDX)   = Emissivity_AD(Ih_IDX) - iVar%Rh_Mod*Reflectivity_AD(Ih_IDX)
    Rh_Mod_AD               = (ONE-iVar%e(Ih_IDX))*Reflectivity_AD(Ih_IDX)
    Reflectivity_AD(Ih_IDX) = ZERO

    Emissivity_AD(Iv_IDX)   = Emissivity_AD(Iv_IDX) - iVar%Rv_Mod*Reflectivity_AD(Iv_IDX)
    Rv_Mod_AD               = (ONE-iVar%e(Iv_IDX))*Reflectivity_AD(Iv_IDX)
    Reflectivity_AD(Iv_IDX) = ZERO

    ! ...emissivities
    e_Azimuth_AD(V_IDX)  = Emissivity_AD(V_IDX); Emissivity_AD(V_IDX) = ZERO
    e_Azimuth_AD(U_IDX)  = Emissivity_AD(U_IDX); Emissivity_AD(U_IDX) = ZERO
    e_Azimuth_AD(Ih_IDX) = Emissivity_AD(Ih_IDX) ! No zero of Emissivity_AD(Ih_IDX)
    e_Azimuth_AD(Iv_IDX) = Emissivity_AD(Iv_IDX) ! No zero of Emissivity_AD(Ih_IDX)


    ! Anisotropic downward radiation correction calculation
    IF ( PRESENT(Transmittance_AD) .AND. iVar%Transmittance_Valid ) THEN
      CALL Reflection_Correction_AD( &
             MWwaterCoeff%RCCoeff, &
             Rv_Mod_AD       , &
             Rh_Mod_AD       , &
             Wind_Speed_AD   , &
             Transmittance_AD, &
             iVar%rcVar        )
    ELSE
      Rv_Mod_AD = ZERO
      Rh_Mod_AD = ZERO
    END IF


    ! Azimuthal component calculation
    IF ( PRESENT(Azimuth_Angle_AD) .AND. iVar%Azimuth_Angle_Valid ) THEN
      CALL Azimuth_Emissivity_AD( &
             MWwaterCoeff%AZCoeff, &
             e_Azimuth_AD    , &
             Wind_Speed_AD   , &
             Azimuth_Angle_AD, &
             iVar%aeVar        )
    ELSE
      e_Azimuth_AD = ZERO
    END IF

    ! Compute the adjoint of the first two Stokes components of the emissivity
    Rh_Foam_AD    = -iVar%Foam_Cover      *Emissivity_AD(Ih_IDX)
    Foam_Cover_AD = (iVar%Rh-iVar%Rh_Foam)*Emissivity_AD(Ih_IDX)
    Rh_AD         = (iVar%Foam_Cover-ONE) *Emissivity_AD(Ih_IDX)
    Emissivity_AD(Ih_IDX) = ZERO
    Rh_Large_AD   = -Rh_AD
    f_small_AD    = iVar%Rh_Fresnel*Rh_AD
    Rh_Fresnel_AD =  Rh_AD*iVar%F_Small
    Rh_AD = ZERO

    Rv_Foam_AD    = -iVar%Foam_Cover      *Emissivity_AD(Iv_IDX)
    Foam_Cover_AD = (iVar%Rv-iVar%Rv_Foam)*Emissivity_AD(Iv_IDX) + Foam_Cover_AD
    Rv_AD         = (iVar%Foam_Cover-ONE) *Emissivity_AD(Iv_IDX)
    Emissivity_AD(Iv_IDX) = ZERO
    Rv_Large_AD   = -Rv_AD
    f_small_AD    = f_small_AD + iVar%Rv_Fresnel*Rv_AD
    Rv_Fresnel_AD =  Rv_AD*iVar%F_Small
    Rv_AD = ZERO


    ! Small scale Correction Calculation AD
    CALL Small_Scale_Correction_AD( &
           MWwaterCoeff%SSCCoeff, &
           f_small_AD   , &
           Wind_Speed_AD, &
           iVar%sscVar    )


    ! Large Scale Correction Calculation
!    IF( FASTEM5_LUT ) THEN
!      CALL Large_Scale_Correction_LUT_AD( iVar%Wind_Speed, iVar%Temperature, iVar%Zenith_Angle,  &
!           iVar%Frequency, Rv_Large_AD, Rh_Large_AD, Wind_Speed_AD, Temperature_AD )
!    ELSE
      CALL Large_Scale_Correction_AD( &
             Rv_Large_AD  , &
             Rh_Large_AD  , &
             Wind_Speed_AD, &
             iVar%lscVar    )
!    END IF


    ! Foam coverage calculation
    CALL Foam_Coverage_AD( &
           MWwaterCoeff%FCCoeff, &
           iVar%Wind_Speed, &
           Foam_Cover_AD, &
           Wind_Speed_AD )


    ! Foam reflectivity "calculation"
    Rv_Foam_AD = ZERO
    Rh_Foam_AD = ZERO


    ! Fresnel reflectivity calculation
    Permittivity_AD = ZERO
    CALL Fresnel_Reflectivity_AD( Rv_Fresnel_AD, Rh_Fresnel_AD, iVar%cos_z, &
                                  Permittivity_AD, &
                                  iVar%fVar )

    ! Permittivity calculation
    CALL Ocean_Permittivity_AD( Permittivity_AD, iVar%Frequency, &
                                Temperature_AD, Salinity_AD, &
                                iVar%pVar )

  END SUBROUTINE Compute_FastemX_AD


!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################



!  ! ============================================
!  ! Reflectivity large scale correction routines
!  ! ============================================
!  !
!  SUBROUTINE Large_Scale_Correction_LUT( Wind_Speed, Temperature, Zenith_Angle, f, Rv, Rh )
!    ! Arguments
!    REAL(fp), INTENT(IN)  :: Wind_Speed   ! Wind speed
!    REAL(fp), INTENT(IN)  :: Temperature  ! SST
!    REAL(fp), INTENT(IN)  :: Zenith_Angle ! zenith angle
!    REAL(fp), INTENT(IN)  :: f       ! Frequency
!    REAL(fp), INTENT(OUT) :: Rv, Rh  ! Reflectivity correction
!    ! Local variables
!    REAL(fp) :: av1,av2,av3,av4,ah1,ah2,ah3,ah4,df,dw,dT,dz
!
!    INTEGER :: i, if1,if2,iw1,iw2,is1,is2,iz1,iz2
!
!!  print *,Wind_Speed, Temperature, Zenith_Angle, f
!
!  IF( f <= MW_W_LS%Fre(1) ) THEN
!    if1 = 1
!    if2 = 1
!    df = ZERO
!  ELSE IF( f >= MW_W_LS%Fre(MW_W_LS%N_Frequency) ) THEN
!    if1 = MW_W_LS%N_Frequency
!    if2 = MW_W_LS%N_Frequency
!    df = ZERO
!  ELSE
!    DO i = 1, MW_W_LS%N_Frequency -1
!      IF( f > MW_W_LS%Fre(i) .and. f <= MW_W_LS%Fre(i+1) ) THEN
!        if1 = i
!        if2 = if1 + 1
!        df = (f - MW_W_LS%Fre(i))/(MW_W_LS%Fre(i+1)-MW_W_LS%Fre(i))
!        GO TO 101
!      END IF
!    END DO
!  END IF
! 101 CONTINUE
!
!  IF( Wind_Speed < MW_W_LS%WindSp(1) ) THEN
!    iw1 = 1
!    iw2 = 1
!    dw = ZERO
!  ELSE IF( Wind_Speed >= MW_W_LS%WindSp(MW_W_LS%N_Wind_S) ) THEN
!    iw1 = MW_W_LS%N_Wind_S
!    iw2 = MW_W_LS%N_Wind_S
!    dw = ZERO
!  ELSE
!    iw1 = INT( (Wind_Speed-MW_W_LS%WindSp(1))/(MW_W_LS%WindSp(2)-MW_W_LS%WindSp(1)) ) + 1
!    iw2 = iw1 + 1
!    dw = (Wind_Speed-MW_W_LS%WindSp(iw1))/(MW_W_LS%WindSp(2)-MW_W_LS%WindSp(1))
!  END IF
!
!  IF( Temperature < MW_W_LS%SST(1) ) THEN
!    is1 = 1
!    is2 = 1
!    dT = ZERO
!  ELSE IF( Temperature >= MW_W_LS%SST(MW_W_LS%N_SST) ) THEN
!    is1 = MW_W_LS%N_SST
!    is2 = MW_W_LS%N_SST
!    dT = ZERO
!  ELSE
!    is1 = INT( (Temperature - MW_W_LS%SST(1))/(MW_W_LS%SST(2)-MW_W_LS%SST(1)) ) + 1
!    is2 = is1 + 1
!    dT = (Temperature - MW_W_LS%SST(is1))/(MW_W_LS%SST(2)-MW_W_LS%SST(1))
!  END IF
!
!  iz1 = INT( abs(Zenith_Angle)/(MW_W_LS%Zangle(2)-MW_W_LS%Zangle(1) ) ) + 1
!  IF( iz1 >= MW_W_LS%N_Zenith ) THEN
!    iz1 = MW_W_LS%N_Zenith
!    iz2 = MW_W_LS%N_Zenith
!    dz = ZERO
!  ELSE
!    iz2 = iz1 + 1
!    dz = ( abs(Zenith_Angle)-MW_W_LS%Zangle(iz1) )/(MW_W_LS%Zangle(2)-MW_W_LS%Zangle(1) )
!  END IF
!
!  av1 = (ONE-dz)*(ONE-df)*MW_W_LS%ev(iw1,is1,if1,iz1) + dz*(ONE-df)*MW_W_LS%ev(iw1,is1,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%ev(iw1,is1,if2,iz1) + dz*df*MW_W_LS%ev(iw1,is1,if2,iz2)
!
!  av2 = (ONE-dz)*(ONE-df)*MW_W_LS%ev(iw1,is2,if1,iz1) + dz*(ONE-df)*MW_W_LS%ev(iw1,is2,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%ev(iw1,is2,if2,iz1) + dz*df*MW_W_LS%ev(iw1,is2,if2,iz2)
!
!  av3 = (ONE-dz)*(ONE-df)*MW_W_LS%ev(iw2,is1,if1,iz1) + dz*(ONE-df)*MW_W_LS%ev(iw2,is1,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%ev(iw2,is1,if2,iz1) + dz*df*MW_W_LS%ev(iw2,is1,if2,iz2)
!
!  av4 = (ONE-dz)*(ONE-df)*MW_W_LS%ev(iw2,is2,if1,iz1) + dz*(ONE-df)*MW_W_LS%ev(iw2,is2,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%ev(iw2,is2,if2,iz1) + dz*df*MW_W_LS%ev(iw2,is2,if2,iz2)
!
!  Rv = (ONE-dw)*(ONE-dT)*av1 + (ONE-dw)*dT*av2 &
!                + dw*(ONE-dT)*av3 + dw*dT*av4
!
!  ah1 = (ONE-dz)*(ONE-df)*MW_W_LS%eh(iw1,is1,if1,iz1) + dz*(ONE-df)*MW_W_LS%eh(iw1,is1,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%eh(iw1,is1,if2,iz1) + dz*df*MW_W_LS%eh(iw1,is1,if2,iz2)
!
!  ah2 = (ONE-dz)*(ONE-df)*MW_W_LS%eh(iw1,is2,if1,iz1) + dz*(ONE-df)*MW_W_LS%eh(iw1,is2,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%eh(iw1,is2,if2,iz1) + dz*df*MW_W_LS%eh(iw1,is2,if2,iz2)
!
!  ah3 = (ONE-dz)*(ONE-df)*MW_W_LS%eh(iw2,is1,if1,iz1) + dz*(ONE-df)*MW_W_LS%eh(iw2,is1,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%eh(iw2,is1,if2,iz1) + dz*df*MW_W_LS%eh(iw2,is1,if2,iz2)
!
!  ah4 = (ONE-dz)*(ONE-df)*MW_W_LS%eh(iw2,is2,if1,iz1) + dz*(ONE-df)*MW_W_LS%eh(iw2,is2,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%eh(iw2,is2,if2,iz1) + dz*df*MW_W_LS%eh(iw2,is2,if2,iz2)
!
!  Rh = (ONE-dw)*(ONE-dT)*ah1 + (ONE-dw)*dT*ah2 &
!                + dw*(ONE-dT)*ah3 + dw*dT*ah4
!
!    RETURN
!  END SUBROUTINE Large_Scale_Correction_LUT
!!
!  !
!  SUBROUTINE Large_Scale_Correction_LUT_TL( Wind_Speed, Temperature, Zenith_Angle, f,  &
!     Wind_Speed_TL, Temperature_TL, Rv_TL, Rh_TL   )
!    ! Arguments
!    REAL(fp), INTENT(IN)  :: Wind_Speed   ! Wind speed
!    REAL(fp), INTENT(IN)  :: Temperature  ! SST
!    REAL(fp), INTENT(IN)  :: Zenith_Angle ! zenith angle
!    REAL(fp), INTENT(IN)  :: f       ! Frequency
!    REAL(fp), INTENT(IN)  :: Wind_Speed_TL
!    REAL(fp), INTENT(IN)  :: Temperature_TL
!    REAL(fp), INTENT(OUT) :: Rv_TL, Rh_TL  ! Reflectivity correction
!    ! Local variables
!    REAL(fp) :: Rv, Rh,av1,av2,av3,av4,ah1,ah2,ah3,ah4,df,dw,dT,dz,dw_TL,dT_TL
!
!    INTEGER :: i, if1,if2,iw1,iw2,is1,is2,iz1,iz2
!
!!  print *,Wind_Speed, Temperature, Zenith_Angle, f
!
!  IF( f <= MW_W_LS%Fre(1) ) THEN
!    if1 = 1
!    if2 = 1
!    df = ZERO
!  ELSE IF( f >= MW_W_LS%Fre(MW_W_LS%N_Frequency) ) THEN
!    if1 = MW_W_LS%N_Frequency
!    if2 = MW_W_LS%N_Frequency
!    df = ZERO
!  ELSE
!    DO i = 1, MW_W_LS%N_Frequency -1
!      IF( f > MW_W_LS%Fre(i) .and. f <= MW_W_LS%Fre(i+1) ) THEN
!        if1 = i
!        if2 = if1 + 1
!        df = (f - MW_W_LS%Fre(i))/(MW_W_LS%Fre(i+1)-MW_W_LS%Fre(i))
!        GO TO 101
!      END IF
!    END DO
!  END IF
! 101 CONTINUE
!
!  IF( Wind_Speed < MW_W_LS%WindSp(1) ) THEN
!    iw1 = 1
!    iw2 = 1
!    dw = ZERO
!  ELSE IF( Wind_Speed >= MW_W_LS%WindSp(MW_W_LS%N_Wind_S) ) THEN
!    iw1 = MW_W_LS%N_Wind_S
!    iw2 = MW_W_LS%N_Wind_S
!    dw = ZERO
!  ELSE
!    iw1 = INT( (Wind_Speed-MW_W_LS%WindSp(1))/(MW_W_LS%WindSp(2)-MW_W_LS%WindSp(1)) ) + 1
!    iw2 = iw1 + 1
!    dw = (Wind_Speed-MW_W_LS%WindSp(iw1))/(MW_W_LS%WindSp(2)-MW_W_LS%WindSp(1))
!  END IF
!
!  IF( Temperature < MW_W_LS%SST(1) ) THEN
!    is1 = 1
!    is2 = 1
!    dT = ZERO
!  ELSE IF( Temperature >= MW_W_LS%SST(MW_W_LS%N_SST) ) THEN
!    is1 = MW_W_LS%N_SST
!    is2 = MW_W_LS%N_SST
!    dT = ZERO
!  ELSE
!    is1 = INT( (Temperature - MW_W_LS%SST(1))/(MW_W_LS%SST(2)-MW_W_LS%SST(1)) ) + 1
!    is2 = is1 + 1
!    dT = (Temperature - MW_W_LS%SST(is1))/(MW_W_LS%SST(2)-MW_W_LS%SST(1))
!  END IF
!
!  iz1 = INT( abs(Zenith_Angle)/(MW_W_LS%Zangle(2)-MW_W_LS%Zangle(1) ) ) + 1
!  IF( iz1 >= MW_W_LS%N_Zenith ) THEN
!    iz1 = MW_W_LS%N_Zenith
!    iz2 = MW_W_LS%N_Zenith
!    dz = ZERO
!  ELSE
!    iz2 = iz1 + 1
!    dz = ( abs(Zenith_Angle)-MW_W_LS%Zangle(iz1) )/(MW_W_LS%Zangle(2)-MW_W_LS%Zangle(1) )
!  END IF
!
!  av1 = (ONE-dz)*(ONE-df)*MW_W_LS%ev(iw1,is1,if1,iz1) + dz*(ONE-df)*MW_W_LS%ev(iw1,is1,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%ev(iw1,is1,if2,iz1) + dz*df*MW_W_LS%ev(iw1,is1,if2,iz2)
!
!  av2 = (ONE-dz)*(ONE-df)*MW_W_LS%ev(iw1,is2,if1,iz1) + dz*(ONE-df)*MW_W_LS%ev(iw1,is2,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%ev(iw1,is2,if2,iz1) + dz*df*MW_W_LS%ev(iw1,is2,if2,iz2)
!
!  av3 = (ONE-dz)*(ONE-df)*MW_W_LS%ev(iw2,is1,if1,iz1) + dz*(ONE-df)*MW_W_LS%ev(iw2,is1,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%ev(iw2,is1,if2,iz1) + dz*df*MW_W_LS%ev(iw2,is1,if2,iz2)
!
!  av4 = (ONE-dz)*(ONE-df)*MW_W_LS%ev(iw2,is2,if1,iz1) + dz*(ONE-df)*MW_W_LS%ev(iw2,is2,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%ev(iw2,is2,if2,iz1) + dz*df*MW_W_LS%ev(iw2,is2,if2,iz2)
!
!  Rv = (ONE-dw)*(ONE-dT)*av1 + (ONE-dw)*dT*av2 &
!                + dw*(ONE-dT)*av3 + dw*dT*av4
!
!  ah1 = (ONE-dz)*(ONE-df)*MW_W_LS%eh(iw1,is1,if1,iz1) + dz*(ONE-df)*MW_W_LS%eh(iw1,is1,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%eh(iw1,is1,if2,iz1) + dz*df*MW_W_LS%eh(iw1,is1,if2,iz2)
!
!  ah2 = (ONE-dz)*(ONE-df)*MW_W_LS%eh(iw1,is2,if1,iz1) + dz*(ONE-df)*MW_W_LS%eh(iw1,is2,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%eh(iw1,is2,if2,iz1) + dz*df*MW_W_LS%eh(iw1,is2,if2,iz2)
!
!  ah3 = (ONE-dz)*(ONE-df)*MW_W_LS%eh(iw2,is1,if1,iz1) + dz*(ONE-df)*MW_W_LS%eh(iw2,is1,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%eh(iw2,is1,if2,iz1) + dz*df*MW_W_LS%eh(iw2,is1,if2,iz2)
!
!  ah4 = (ONE-dz)*(ONE-df)*MW_W_LS%eh(iw2,is2,if1,iz1) + dz*(ONE-df)*MW_W_LS%eh(iw2,is2,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%eh(iw2,is2,if2,iz1) + dz*df*MW_W_LS%eh(iw2,is2,if2,iz2)
!
!  Rh = (ONE-dw)*(ONE-dT)*ah1 + (ONE-dw)*dT*ah2 &
!                + dw*(ONE-dT)*ah3 + dw*dT*ah4
!
!!  ================  TL  ===============================================
!
!
!  IF( Wind_Speed < MW_W_LS%WindSp(1) ) THEN
!    dw_TL = ZERO
!  ELSE IF( Wind_Speed >= MW_W_LS%WindSp(MW_W_LS%N_Wind_S) ) THEN
!    dw_TL = ZERO
!  ELSE
!    dw_TL = Wind_Speed_TL/(MW_W_LS%WindSp(2)-MW_W_LS%WindSp(1))
!  END IF
!
!  IF( Temperature < MW_W_LS%SST(1) ) THEN
!    dT_TL = ZERO
!  ELSE IF( Temperature >= MW_W_LS%SST(MW_W_LS%N_SST) ) THEN
!    dT_TL = ZERO
!  ELSE
!    dT_TL = Temperature_TL/(MW_W_LS%SST(2)-MW_W_LS%SST(1))
!  END IF
!
!  Rv_TL = ( -dw_TL*(ONE-dT) -(ONE-dw)*dT_TL )*av1 + ( -dw_TL*dT+(ONE-dw)*dT_TL )*av2 &
!      + ( dw_TL*(ONE-dT) -dw*dT_TL )*av3 + ( dw_TL*dT+dw*dT_TL )*av4
!
!
!  Rh_TL = ( -dw_TL*(ONE-dT) -(ONE-dw)*dT_TL )*ah1 + ( -dw_TL*dT+(ONE-dw)*dT_TL )*ah2 &
!      + ( dw_TL*(ONE-dT) -dw*dT_TL )*ah3 + ( dw_TL*dT+dw*dT_TL )*ah4
!
!    RETURN
!  END SUBROUTINE Large_Scale_Correction_LUT_TL
!!
!
!  !
!  SUBROUTINE Large_Scale_Correction_LUT_AD( Wind_Speed, Temperature, Zenith_Angle, f,  &
!     Rv_AD, Rh_AD, Wind_Speed_AD, Temperature_AD   )
!    ! Arguments
!    REAL(fp), INTENT(IN)  :: Wind_Speed   ! Wind speed
!    REAL(fp), INTENT(IN)  :: Temperature  ! SST
!    REAL(fp), INTENT(IN)  :: Zenith_Angle ! zenith angle
!    REAL(fp), INTENT(IN)  :: f       ! Frequency
!    REAL(fp), INTENT(INOUT)  :: Wind_Speed_AD
!    REAL(fp), INTENT(INOUT)  :: Temperature_AD
!    REAL(fp), INTENT(INOUT) :: Rv_AD, Rh_AD  ! Reflectivity correction
!    ! Local variables
!    REAL(fp) :: Rv, Rh,av1,av2,av3,av4,ah1,ah2,ah3,ah4,df,dw,dT,dz,dw_AD,dT_AD
!
!    INTEGER :: i, if1,if2,iw1,iw2,is1,is2,iz1,iz2
!
!!  print *,Wind_Speed, Temperature, Zenith_Angle, f
!
!  IF( f <= MW_W_LS%Fre(1) ) THEN
!    if1 = 1
!    if2 = 1
!    df = ZERO
!  ELSE IF( f >= MW_W_LS%Fre(MW_W_LS%N_Frequency) ) THEN
!    if1 = MW_W_LS%N_Frequency
!    if2 = MW_W_LS%N_Frequency
!    df = ZERO
!  ELSE
!    DO i = 1, MW_W_LS%N_Frequency -1
!      IF( f > MW_W_LS%Fre(i) .and. f <= MW_W_LS%Fre(i+1) ) THEN
!        if1 = i
!        if2 = if1 + 1
!        df = (f - MW_W_LS%Fre(i))/(MW_W_LS%Fre(i+1)-MW_W_LS%Fre(i))
!        GO TO 101
!      END IF
!    END DO
!  END IF
! 101 CONTINUE
!
!  IF( Wind_Speed < MW_W_LS%WindSp(1) ) THEN
!    iw1 = 1
!    iw2 = 1
!    dw = ZERO
!  ELSE IF( Wind_Speed >= MW_W_LS%WindSp(MW_W_LS%N_Wind_S) ) THEN
!    iw1 = MW_W_LS%N_Wind_S
!    iw2 = MW_W_LS%N_Wind_S
!    dw = ZERO
!  ELSE
!    iw1 = INT( (Wind_Speed-MW_W_LS%WindSp(1))/(MW_W_LS%WindSp(2)-MW_W_LS%WindSp(1)) ) + 1
!    iw2 = iw1 + 1
!    dw = (Wind_Speed-MW_W_LS%WindSp(iw1))/(MW_W_LS%WindSp(2)-MW_W_LS%WindSp(1))
!  END IF
!
!  IF( Temperature < MW_W_LS%SST(1) ) THEN
!    is1 = 1
!    is2 = 1
!    dT = ZERO
!  ELSE IF( Temperature >= MW_W_LS%SST(MW_W_LS%N_SST) ) THEN
!    is1 = MW_W_LS%N_SST
!    is2 = MW_W_LS%N_SST
!    dT = ZERO
!  ELSE
!    is1 = INT( (Temperature - MW_W_LS%SST(1))/(MW_W_LS%SST(2)-MW_W_LS%SST(1)) ) + 1
!    is2 = is1 + 1
!    dT = (Temperature - MW_W_LS%SST(is1))/(MW_W_LS%SST(2)-MW_W_LS%SST(1))
!  END IF
!
!  iz1 = INT( abs(Zenith_Angle)/(MW_W_LS%Zangle(2)-MW_W_LS%Zangle(1) ) ) + 1
!  IF( iz1 >= MW_W_LS%N_Zenith ) THEN
!    iz1 = MW_W_LS%N_Zenith
!    iz2 = MW_W_LS%N_Zenith
!    dz = ZERO
!  ELSE
!    iz2 = iz1 + 1
!    dz = ( abs(Zenith_Angle)-MW_W_LS%Zangle(iz1) )/(MW_W_LS%Zangle(2)-MW_W_LS%Zangle(1) )
!  END IF
!
!  av1 = (ONE-dz)*(ONE-df)*MW_W_LS%ev(iw1,is1,if1,iz1) + dz*(ONE-df)*MW_W_LS%ev(iw1,is1,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%ev(iw1,is1,if2,iz1) + dz*df*MW_W_LS%ev(iw1,is1,if2,iz2)
!
!  av2 = (ONE-dz)*(ONE-df)*MW_W_LS%ev(iw1,is2,if1,iz1) + dz*(ONE-df)*MW_W_LS%ev(iw1,is2,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%ev(iw1,is2,if2,iz1) + dz*df*MW_W_LS%ev(iw1,is2,if2,iz2)
!
!  av3 = (ONE-dz)*(ONE-df)*MW_W_LS%ev(iw2,is1,if1,iz1) + dz*(ONE-df)*MW_W_LS%ev(iw2,is1,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%ev(iw2,is1,if2,iz1) + dz*df*MW_W_LS%ev(iw2,is1,if2,iz2)
!
!  av4 = (ONE-dz)*(ONE-df)*MW_W_LS%ev(iw2,is2,if1,iz1) + dz*(ONE-df)*MW_W_LS%ev(iw2,is2,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%ev(iw2,is2,if2,iz1) + dz*df*MW_W_LS%ev(iw2,is2,if2,iz2)
!
!  Rv = (ONE-dw)*(ONE-dT)*av1 + (ONE-dw)*dT*av2 &
!                + dw*(ONE-dT)*av3 + dw*dT*av4
!
!  ah1 = (ONE-dz)*(ONE-df)*MW_W_LS%eh(iw1,is1,if1,iz1) + dz*(ONE-df)*MW_W_LS%eh(iw1,is1,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%eh(iw1,is1,if2,iz1) + dz*df*MW_W_LS%eh(iw1,is1,if2,iz2)
!
!  ah2 = (ONE-dz)*(ONE-df)*MW_W_LS%eh(iw1,is2,if1,iz1) + dz*(ONE-df)*MW_W_LS%eh(iw1,is2,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%eh(iw1,is2,if2,iz1) + dz*df*MW_W_LS%eh(iw1,is2,if2,iz2)
!
!  ah3 = (ONE-dz)*(ONE-df)*MW_W_LS%eh(iw2,is1,if1,iz1) + dz*(ONE-df)*MW_W_LS%eh(iw2,is1,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%eh(iw2,is1,if2,iz1) + dz*df*MW_W_LS%eh(iw2,is1,if2,iz2)
!
!  ah4 = (ONE-dz)*(ONE-df)*MW_W_LS%eh(iw2,is2,if1,iz1) + dz*(ONE-df)*MW_W_LS%eh(iw2,is2,if1,iz2) &
!      + (ONE-dz)*df*MW_W_LS%eh(iw2,is2,if2,iz1) + dz*df*MW_W_LS%eh(iw2,is2,if2,iz2)
!
!  Rh = (ONE-dw)*(ONE-dT)*ah1 + (ONE-dw)*dT*ah2 &
!                + dw*(ONE-dT)*ah3 + dw*dT*ah4
!
!!  ================  TL  ===============================================
!
!  dw_AD = ( -ah1*(ONE-dT) -ah2*dT +ah3*(ONE-dT) + ah4*dT ) * Rh_AD
!  dT_AD = ( -(ONE-dw)*ah1 +(ONE-dw)*ah2 -dw*ah3 + dw*ah4 ) * Rh_AD
!
!  dw_AD = dw_AD + ( -av1*(ONE-dT) -av2*dT +av3*(ONE-dT) + av4*dT ) * Rv_AD
!  dT_AD = dT_AD + ( -(ONE-dw)*av1 +(ONE-dw)*av2 -dw*av3 + dw*av4 ) * Rv_AD
!
!  IF( Temperature < MW_W_LS%SST(1) ) THEN
!    dT_AD = ZERO
!  ELSE IF( Temperature >= MW_W_LS%SST(MW_W_LS%N_SST) ) THEN
!    dT_AD = ZERO
!  ELSE
!    Temperature_AD = Temperature_AD + dT_AD/(MW_W_LS%SST(2)-MW_W_LS%SST(1))
!  END IF
!
!  IF( Wind_Speed < MW_W_LS%WindSp(1) ) THEN
!    dw_AD = ZERO
!  ELSE IF( Wind_Speed >= MW_W_LS%WindSp(MW_W_LS%N_Wind_S) ) THEN
!    dw_AD = ZERO
!  ELSE
!    Wind_Speed_AD = Wind_Speed_AD + dw_AD/(MW_W_LS%WindSp(2)-MW_W_LS%WindSp(1))
!  END IF
!
!
!    RETURN
!  END SUBROUTINE Large_Scale_Correction_LUT_AD
!


!
END MODULE CRTM_FastemX
!
