!
! Profile_Utility_Parameters
!
! Module containing parameters used in the profile utility modules.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 30-Aug-2002
!                       paul.vandelst@noaa.gov
!

MODULE Profile_Utility_Parameters

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Modules used
  USE Type_Kinds, ONLY: fp
  USE Fundamental_Constants, ONLY : NA => AVOGADRO_CONSTANT,    &
                                    R0 => MOLAR_GAS_CONSTANT,   &
                                    L0 => LOSCHMIDT_CONSTANT,   &
                                    P0 => STANDARD_ATMOSPHERE,  &
                                    T0 => STANDARD_TEMPERATURE, &
                                    G0 => STANDARD_GRAVITY,     &
                                    PI
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything is default private
  PRIVATE
  ! Make renamed fundamental constants public
  PUBLIC :: NA
  PUBLIC :: R0
  PUBLIC :: L0
  PUBLIC :: P0
  PUBLIC :: T0
  PUBLIC :: G0
  PUBLIC :: PI


  ! ---------------------
  ! Parameter definitions
  ! ---------------------

  ! Literal constants
  REAL(fp), PUBLIC, PARAMETER :: ZERO      = 0.0_fp
  REAL(fp), PUBLIC, PARAMETER :: ONE       = 1.0_fp
  REAL(fp), PUBLIC, PARAMETER :: TWO       = 2.0_fp
  REAL(fp), PUBLIC, PARAMETER :: HUNDRED   = 100.0_fp

  ! Precision... ~EPSILON(ONE) for double precision.
  REAL(fp), PUBLIC, PARAMETER :: TOLERANCE = 2.0e-16_fp

  ! Conversion factors
  REAL(fp), PUBLIC, PARAMETER :: CELSIUS_TO_KELVIN = T0
  REAL(fp), PUBLIC, PARAMETER :: G_TO_KG           = 1.0e-03_fp
  REAL(fp), PUBLIC, PARAMETER :: KG_TO_G           = 1.0e+03_fp
  REAL(fp), PUBLIC, PARAMETER :: PA_TO_HPA         = 1.0e-02_fp
  REAL(fp), PUBLIC, PARAMETER :: HPA_TO_PA         = 1.0e+02_fp
  REAL(fp), PUBLIC, PARAMETER :: PPMV_TO_PPV       = 1.0e-06_fp
  REAL(fp), PUBLIC, PARAMETER :: PPV_TO_PPMV       = 1.0e+06_fp
  REAL(fp), PUBLIC, PARAMETER :: CM_TO_M           = 1.0e-02_fp
  REAL(fp), PUBLIC, PARAMETER :: M_TO_CM           = 1.0e+02_fp
  REAL(fp), PUBLIC, PARAMETER :: FROM_PERCENT      = 1.0e-02_fp
  REAL(fp), PUBLIC, PARAMETER :: TO_PERCENT        = 1.0e+02_fp

  ! Number of units specifiers. Same as for LBLRTM
  INTEGER, PUBLIC, PARAMETER :: N_ABSORBER_UNITS = 8

  ! Units specifiers. Same as for LBLRTM
  INTEGER, PUBLIC, PARAMETER :: INVALID_UNITS = 0  ! Invalid
  INTEGER, PUBLIC, PARAMETER :: PPMV_UNITS    = 1  ! Volume mixing ratio (ppmv)
  INTEGER, PUBLIC, PARAMETER :: ND_UNITS      = 2  ! Number density      (cm^-3)
  INTEGER, PUBLIC, PARAMETER :: MR_UNITS      = 3  ! Mass mixing ratio   (g/kg)
  INTEGER, PUBLIC, PARAMETER :: MD_UNITS      = 4  ! Mass density        (g.m^-3)
  INTEGER, PUBLIC, PARAMETER :: PP_UNITS      = 5  ! Partial pressure    (hPa)
  INTEGER, PUBLIC, PARAMETER :: DPK_UNITS     = 6  ! Dew point           (Kelvin)  [H2O only]
  INTEGER, PUBLIC, PARAMETER :: DPC_UNITS     = 7  ! Dew point           (Celsius) [H2O only]
  INTEGER, PUBLIC, PARAMETER :: RH_UNITS      = 8  ! Relative humidity   (%)       [H2O only]

  INTEGER, PUBLIC, PARAMETER :: ABSORBER_UNITS_ID(0:N_ABSORBER_UNITS) = &
    (/ INVALID_UNITS, &
       PPMV_UNITS   , &
       ND_UNITS     , &
       MR_UNITS     , &
       MD_UNITS     , &
       PP_UNITS     , &
       DPK_UNITS    , &  ! [H2O only]
       DPC_UNITS    , &  ! [H2O only]
       RH_UNITS      /)  ! [H2O only]
  CHARACTER(*), PUBLIC, PARAMETER :: ABSORBER_UNITS_NAME(0:N_ABSORBER_UNITS) = &
    (/ 'Invalid', &
       'ppmv   ', &
       'cm^-3  ', &
       'g/kg   ', &
       'g.m^-3 ', &
       'hPa    ', &
       'DP, K  ', &  ! [H2O only]
       'DP, C  ', &  ! [H2O only]
       'RH, %  ' /)  ! [H2O only]
  CHARACTER(*), PUBLIC, PARAMETER :: ABSORBER_UNITS_CHAR(0:N_ABSORBER_UNITS) = &
    (/ '-', &
       'A', &
       'B', &
       'C', &
       'D', &
       'E', &
       'F', &  ! [H2O only]
       'G', &  ! [H2O only]
       'H' /)  ! [H2O only]

  ! Maximum number of molecular species (as for HITRAN)
  INTEGER, PUBLIC, PARAMETER :: MAX_N_MOLECULAR_SPECIES = 32

  ! HITRAN molecular IDs and symbols
  INTEGER, PUBLIC, PARAMETER :: ID_H2O   = 1
  INTEGER, PUBLIC, PARAMETER :: ID_CO2   = 2
  INTEGER, PUBLIC, PARAMETER :: ID_O3    = 3
  INTEGER, PUBLIC, PARAMETER :: ID_N2O   = 4
  INTEGER, PUBLIC, PARAMETER :: ID_CO    = 5
  INTEGER, PUBLIC, PARAMETER :: ID_CH4   = 6
  INTEGER, PUBLIC, PARAMETER :: ID_O2    = 7

  INTEGER, PUBLIC, PARAMETER :: ID_NO    = 8
  INTEGER, PUBLIC, PARAMETER :: ID_SO2   = 9
  INTEGER, PUBLIC, PARAMETER :: ID_NO2   = 10
  INTEGER, PUBLIC, PARAMETER :: ID_NH3   = 11
  INTEGER, PUBLIC, PARAMETER :: ID_HNO3  = 12
  INTEGER, PUBLIC, PARAMETER :: ID_OH    = 13
  INTEGER, PUBLIC, PARAMETER :: ID_HF    = 14

  INTEGER, PUBLIC, PARAMETER :: ID_HCL   = 15
  INTEGER, PUBLIC, PARAMETER :: ID_HBR   = 16
  INTEGER, PUBLIC, PARAMETER :: ID_HI    = 17
  INTEGER, PUBLIC, PARAMETER :: ID_CLO   = 18
  INTEGER, PUBLIC, PARAMETER :: ID_OCS   = 19
  INTEGER, PUBLIC, PARAMETER :: ID_H2CO  = 20
  INTEGER, PUBLIC, PARAMETER :: ID_HOCL  = 21

  INTEGER, PUBLIC, PARAMETER :: ID_N2    = 22
  INTEGER, PUBLIC, PARAMETER :: ID_HCN   = 23
  INTEGER, PUBLIC, PARAMETER :: ID_CH3CL = 24
  INTEGER, PUBLIC, PARAMETER :: ID_H2O2  = 25
  INTEGER, PUBLIC, PARAMETER :: ID_C2H2  = 26
  INTEGER, PUBLIC, PARAMETER :: ID_C2H6  = 27
  INTEGER, PUBLIC, PARAMETER :: ID_PH3   = 28

  INTEGER, PUBLIC, PARAMETER :: ID_COF2  = 29
  INTEGER, PUBLIC, PARAMETER :: ID_SF6   = 30
  INTEGER, PUBLIC, PARAMETER :: ID_H2S   = 31
  INTEGER, PUBLIC, PARAMETER :: ID_HCOOH = 32

  CHARACTER(*), PUBLIC, PARAMETER :: MOLECULAR_SYMBOL(MAX_N_MOLECULAR_SPECIES) = &
      (/ 'H2O  ','CO2  ','O3   ','N2O  ', &
         'CO   ','CH4  ','O2   ','NO   ', &
         'SO2  ','NO2  ','NH3  ','HNO3 ', &
         'OH   ','HF   ','HCL  ','HBR  ', &
         'HI   ','CLO  ','OCS  ','H2CO ', &
         'HOCL ','N2   ','HCN  ','CH3CL', &
         'H2O2 ','C2H2 ','C2H6 ','PH3  ', &
         'COF2 ','SF6  ','H2S  ','HCOOH' /)


  ! Molecular weights of first seven HITRAN molecular species
  REAL(fp), PUBLIC, PARAMETER :: MW_H2O = 18.01528_fp
  REAL(fp), PUBLIC, PARAMETER :: MW_CO2 = 44.00950_fp
  REAL(fp), PUBLIC, PARAMETER :: MW_O3  = 47.99820_fp
  REAL(fp), PUBLIC, PARAMETER :: MW_N2O = 44.01288_fp
  REAL(fp), PUBLIC, PARAMETER :: MW_CO  = 28.01010_fp
  REAL(fp), PUBLIC, PARAMETER :: MW_CH4 = 16.04246_fp
  REAL(fp), PUBLIC, PARAMETER :: MW_O2  = 31.99880_fp
  REAL(fp), PUBLIC, PARAMETER :: MW_N2  = 28.01348_fp

  ! Weights of all 32 HITRAN molecular species
  REAL(fp), PUBLIC, PARAMETER :: MOLECULAR_WEIGHT(MAX_N_MOLECULAR_SPECIES) = &
    (/       MW_H2O,        MW_CO2,         MW_O3,        MW_N2O, &
             MW_CO ,        MW_CH4,         MW_O2,   30.00614_fp, &
        64.06480_fp,   46.00554_fp,   17.03056_fp,   63.01288_fp, &
        17.00734_fp,   20.00634_fp,   36.46064_fp,   80.91194_fp, &
       127.91241_fp,   51.45210_fp,   60.07610_fp,   30.02598_fp, &
        52.46004_fp,        MW_N2 ,   27.02538_fp,   50.48722_fp, &
        34.01468_fp,   26.03728_fp,   30.06904_fp,   33.99758_fp, &
        66.00690_fp,  146.05643_fp,   34.08188_fp,   46.02538_fp /)

  ! Average molecular weight of dry air
  REAL(fp), PUBLIC, PARAMETER :: MW_DRYAIR = 28.9648_fp

  ! Ratio of water vapor and dry air weights for conversion routines
  REAL(fp), PUBLIC, PARAMETER :: EPS       = MW_H2O / MW_DRYAIR

  ! Gas constant for dry air. Units are J.K-1.kg-1
  REAL(fp), PUBLIC, PARAMETER :: R_DRYAIR  = R0 / ( MW_DRYAIR * G_TO_KG )

  ! Specific heat of dry air. Units are J.K-1.kg-1
  REAL(fp), PUBLIC, PARAMETER :: Cp_DRYAIR = ( 7.0_fp/TWO ) * R_DRYAIR

END MODULE Profile_Utility_Parameters
