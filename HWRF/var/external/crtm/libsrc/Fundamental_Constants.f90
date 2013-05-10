!
! Fundamental_Constants
!
! Module containing various fundamental mathematical and physical constants.
!
! The fundamental constants and equations used are taken from the NIST
! Reference on Constants, Units, and Uncertainty website:
!
!   http://physics.nist.gov/cuu/Constants/
!
! See also:
!
!   Mohr, P.J. and B.N. Taylor, "CODATA recommended values of the
!     fundamental physical constants: 1998", Reviews of Modern Physics, 
!     Vol.72, No.2, 2000.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-May-2000
!                       paul.vandelst@ssec.wisc.edu
!

MODULE Fundamental_Constants

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Modules used
  USE Type_Kinds, ONLY: fp
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------------
  ! Default visibility
  ! ------------------
  PRIVATE

 

  !#----------------------------------------------------------------------------#
  !#                       -- LOCAL LITERAL CONSTANTS --                        #
  !#----------------------------------------------------------------------------#
  REAL(fp), PARAMETER, PRIVATE :: ONE = 1.0_fp
  REAL(fp), PARAMETER, PRIVATE :: TWO = 2.0_fp



  !#----------------------------------------------------------------------------#
  !#                -- IRRATIONAL NUMBERS AND ASSOCIATED BITS --                #
  !#----------------------------------------------------------------------------#

  ! PI
  REAL(fp), PARAMETER, PUBLIC :: PI             = 3.141592653589793238462643_fp
  REAL(fp), PARAMETER, PUBLIC :: PI_RECIPROCAL  = 0.318309886183790671537767_fp
  REAL(fp), PARAMETER, PUBLIC :: PI_SQUARED     = 9.869604401089358618834491_fp
  REAL(fp), PARAMETER, PUBLIC :: PI_SQUARE_ROOT = 1.772453850905516027298167_fp
  REAL(fp), PARAMETER, PUBLIC :: PI_LN          = 1.144729885849400174143427_fp
  REAL(fp), PARAMETER, PUBLIC :: PI_LOG10       = 0.497149872694133854351268_fp

  ! E
  REAL(fp), PARAMETER, PUBLIC :: E              = 2.718281828459045235360287_fp
  REAL(fp), PARAMETER, PUBLIC :: E_RECIPROCAL   = 0.367879441171442321595523_fp
  REAL(fp), PARAMETER, PUBLIC :: E_SQUARED      = 7.389056098930650227230427_fp
  REAL(fp), PARAMETER, PUBLIC :: E_LOG10        = 0.434294481903251827651129_fp

  ! Other transcendentals
  REAL(fp), PARAMETER, PUBLIC :: LN2            = 0.693147180559945309417232_fp


  !#----------------------------------------------------------------------------#
  !#                            -- UNIVERAL CONSTANTS --                        #
  !#----------------------------------------------------------------------------#

  ! ----------------------------------------------
  ! Speed of light
  ! Symbol:c,  Units:m/s,  Rel.Uncert.(ppm): exact
  ! ----------------------------------------------
  REAL(fp), PARAMETER, PUBLIC :: SPEED_OF_LIGHT = 2.99792458e+08_fp

  ! --------------------------------------------------
  ! Permeability of vacuum
  ! Symbol:mu0,  Units:N/A^2,  Rel.Uncert.(ppm): exact
  ! --------------------------------------------------
  REAL(fp), PARAMETER, PUBLIC :: PERMEABILITY = PI * 4.0e-07_fp

  ! -----------------------------------------------------
  ! Permittivity of vacuum
  ! Symbol:epsilon0,  Units:F/m,  Rel.Uncert.(ppm): exact
  ! -----------------------------------------------------
  REAL(fp), PARAMETER, PUBLIC :: PERMITTIVITY =                ONE                  / &
  !                                             ------------------------------------
                                                ( PERMEABILITY * SPEED_OF_LIGHT**2 )

  ! ---------------------------------------------
  ! Planck constant
  ! Symbol:h,  Units:Js,  Rel.Uncert.(ppm): 0.078
  ! ---------------------------------------------
  REAL(fp), PARAMETER, PUBLIC :: PLANCK_CONSTANT = 6.62606876e-34_fp

  ! ----------------------------------------------------
  ! Gravitational constant
  ! Symbol:G,  Units:m^3/kg/s^2,  Rel.Uncert.(ppm): 1500
  ! ----------------------------------------------------
  REAL(fp), PARAMETER, PUBLIC :: GRAVITATIONAL_CONSTANT = 6.673e-11_fp



  !#----------------------------------------------------------------------------#
  !#                          -- CONVERSION FACTORS --                          #
  !#----------------------------------------------------------------------------#

  ! ---------------------------------------------
  ! Electron volt
  ! Symbol:eV,  Units:J,  Rel.Uncert.(ppm): 0.039
  ! ---------------------------------------------
  REAL(fp), PARAMETER, PUBLIC :: ELECTRON_VOLT = 1.602176462e-19_fp

  ! ---------------------------------------------
  ! Unified atomic mass unit
  ! Symbol:u,  Units:kg,  Rel.Uncert.(ppm): 0.079
  ! ---------------------------------------------
  REAL(fp), PARAMETER, PUBLIC :: UNIFIED_ATOMIC_MASS_UNIT = 1.66053873e-27_fp

  ! ----------------------------------------------
  ! Standard atmosphere
  ! Symbol:P0,  Units:Pa,  Rel.Uncert.(ppm): exact
  ! ----------------------------------------------
  REAL(fp), PARAMETER, PUBLIC :: STANDARD_ATMOSPHERE = 101325.0_fp

  ! ----------------------------------------------------------------------
  ! Standard temperature
  ! Symbol:T0,  Units:Kelvin,  Rel.Uncert.(ppm): exact
  !
  ! Note that the unit of thermodynamic temperature, the Kelvin, is the
  ! fraction 1/273.16 of the thermodynamic temperature of the triple point
  ! of water. The standard temperature is the ice point of water, NOT the
  ! triple point, hence the 0.01K difference.
  ! ----------------------------------------------------------------------
  REAL(fp), PARAMETER, PUBLIC :: STANDARD_TEMPERATURE = 273.15_fp

  ! ------------------------------------------------
  ! Standard gravity
  ! Symbol:g,  Units:m/s^2,  Rel.Uncert.(ppm): exact
  ! ------------------------------------------------
  REAL(fp), PARAMETER, PUBLIC :: STANDARD_GRAVITY = 9.80665_fp



  !#----------------------------------------------------------------------------#
  !#                        -- PHYSICOCHEMICAL CONSTANTS --                     #
  !#----------------------------------------------------------------------------#

  ! -----------------------------------------------------
  ! Avogadro constant
  ! Symbol:N(A),  Units:mole^-1,  Rel.Uncert.(ppm): 0.079
  ! -----------------------------------------------------
  REAL(fp), PARAMETER, PUBLIC :: AVOGADRO_CONSTANT = 6.02214199e+23_fp


  ! -------------------------------------------------
  ! Molar gas constant
  ! Symbol:R,  Units:J/mole/K,  Rel.Uncert.(ppm): 1.7
  ! -------------------------------------------------
  REAL(fp), PARAMETER, PUBLIC :: MOLAR_GAS_CONSTANT = 8.314472_fp

  ! --------------------------------------------
  ! Boltzmann constant
  ! Symbol:k,  Units:J/K,  Rel.Uncert.(ppm): 1.7
  !
  !         R
  !   k = ------
  !        N(A)
  !
  !     = 1.3806503(24)e-23
  !
  ! --------------------------------------------
  REAL(fp), PARAMETER, PUBLIC :: BOLTZMANN_CONSTANT = MOLAR_GAS_CONSTANT / &
  !                                                   ------------------
                                                       AVOGADRO_CONSTANT

  ! ------------------------------------------------------
  ! Stefan-Boltzmann constant
  ! Symbol:sigma,  Units:W/m^2/K^4,  Rel.Uncert.(ppm): 7.0
  !
  !             PI^2
  !             ----.k^4
  !              60                     h
  !   sigma = ------------   ( hbar = ----- )
  !            hbar^3.c^2              2PI
  !
  !         = 5.670400(40)e-08
  !
  ! I just placed the value here due to the mathematical
  ! gymnastics required to calculate it directly.
  ! ------------------------------------------------------
  REAL(fp), PARAMETER, PUBLIC :: STEFAN_BOLTZMANN_CONSTANT = 5.670400e-08_fp

  ! -------------------------------------------------------
  ! First Planck function constant
  ! Symbol:c1,  Units:W.m^2.sr^-1,  Rel.Uncert.(ppm): 0.078
  !
  !   c1 = 2.h.c^2
  !
  !      = 1.191042722(93)e-16
  !
  ! -------------------------------------------------------
  REAL(fp), PARAMETER, PUBLIC :: C_1 = TWO * PLANCK_CONSTANT * SPEED_OF_LIGHT**2

  ! ---------------------------------------------
  ! Second Planck function constant
  ! Symbol:c2,  Units:K.m,  Rel.Uncert.(ppm): 1.7
  !
  !         h.c
  !   c2 = -----
  !          k
  !
  !      = 1.4387752(25)e-02
  !
  ! ---------------------------------------------
  REAL(fp), PARAMETER, PUBLIC :: C_2 = PLANCK_CONSTANT * SPEED_OF_LIGHT / &
  !                                    ----------------------------------
                                               BOLTZMANN_CONSTANT

  ! -----------------------------------------------------------------
  ! Molar volume of an ideal gas at standard temperature and pressure
  ! Symbol:Vm,  Units:m^3/mol,  Rel.Uncert.(ppm): 1.7
  !
  !         R.T0
  !   Vm = ------
  !          P0
  !
  !      = 2.2413996(39)e-02
  !
  ! -----------------------------------------------------------------
  REAL(fp), PARAMETER, PUBLIC :: STP_MOLAR_VOLUME = ( MOLAR_GAS_CONSTANT * STANDARD_TEMPERATURE ) / &
  !                                                 ---------------------------------------------
                                                                 STANDARD_ATMOSPHERE

  ! ------------------------------------------------------------------
  ! Loschmidt constant: The number density of one mole of an ideal gas
  ! at standard temperature and pressure
  ! Symbol:n0,  Units:m^-3,  Rel.Uncert.(ppm): 1.7
  !
  !         N(A).P0
  !   n0 = ---------
  !          R.T0
  !
  !         N(A)
  !      = ------     .....(1)
  !          Vm
  !
  !      = 2.6867775(47)e+25
  !
  ! Alternatively, using the ideal gas law directly, we know,
  !
  !   P.V = n.k.T     .....(2)
  !
  ! For V = 1m^3 (unit volume), and P = P0, T = T0, then eqn.(2)
  ! becomes,
  !
  !   P0 = n0.k.T0
  !
  ! which rearranges to
  !
  !          P0  
  !   n0 = ------     .....(3)
  !         k.T0 
  !
  ! Equation (1) rather than eqn(3) is used here.
  ! ------------------------------------------------------------------
  REAL(fp), PARAMETER, PUBLIC :: LOSCHMIDT_CONSTANT = AVOGADRO_CONSTANT / &
  !                                                   -----------------
                                                      STP_MOLAR_VOLUME

END MODULE Fundamental_Constants
