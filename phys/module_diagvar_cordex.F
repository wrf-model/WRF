MODULE module_diagvar_cordex
! Module with the specific subroutines and functions to compute the CORDEX required variables
! L. Fita, CIMA. December 2017
!
  USE module_model_constants

!! These are the definition of parameters used in the calculations
!   They might be placed on the general WRF's share/module_model_constants.F ... ?
  ! Current version of the module
  CHARACTER(LEN=50)                                      :: cdxwrfversion = '2.0'

  INTEGER, PARAMETER                                     :: r_k = KIND(1.d0)

  REAL(r_k), PARAMETER                                   :: ZEPSEC=1.0D-12
! Low limit pressure for medium clouds [Pa]
  REAL(r_k), PARAMETER                                   :: prmhc = 44000.d0
! Low limit pressure for High clouds [Pa]
  REAL(r_k), PARAMETER                                   :: prmlc = 68000.d0
! Real values for cloud intervals
  REAL, PARAMETER                                        :: prmhcR = prmhc*1.
  REAL, PARAMETER                                        :: prmlcR = prmlc*1.
! August-Roche-Magnus approximation
  REAL, PARAMETER                                        :: ARM1 = 6.1094
  REAL, PARAMETER                                        :: ARM2 = 17.625
  REAL, PARAMETER                                        :: ARM3 = 243.04

! Ratio between molecular weights of water and dry air
  REAL, PARAMETER                                        :: mol_watdry = 0.622
! Standard atmospheric lapse-rate [Km-1]
  REAL, PARAMETER                                        :: gammav=6.5e-3

  REAL(r_k), PARAMETER                                   :: zeroRK=0.d0
  REAL(r_K), PARAMETER                                   :: halfRK=0.5d0
  REAL(r_K), PARAMETER                                   :: oneRK=1.d0
  REAL(r_k), PARAMETER                                   :: twoRK=2.d0
! FillValue
  REAL, PARAMETER                                        :: fillValueR=1.e20

  CHARACTER(LEN=50)                                      :: sfname, IS, RS
  CHARACTER (LEN=256)                                    :: message
  CHARACTER (LEN=1000)                                   :: Lmessage

! Zero
!  REAL, PARAMETER                                        :: zero=10.e-6 

! null value
  REAL(r_k)                                              :: nullv = 1.e-7
  !REAL(r_k)                                              :: nullv = 1.d-15

  CONTAINS

!!!
! Variables
!!!
! Cdrag_0: first order generic approximation of the drag coefficient
! Theta: potential temperature
! Thetae: equivalent potential temperature
! The2T: temperature at any pressure level along a saturation adiabat
! VirtualTemperature: virtual temperature
! SaturationMixingRatio: saturation mixing ratio
! TLCL: Temperature of a parcel of air would have dry adiabatically lifted to it's lcl
! VirtPotTemp: Function to compute virtual potential temperature

!!!!!!! diagnosted variables
! var_bulk_Ri: Subroutine to compute the bulk Richardson number at all the space
! var_cape_afwa: CAPE following AFWA methodology
! var_cllmh: low, medium, high-cloud [0,100]
! var_clt: total cloudiness [0,100]
! var_fog_K84: Computation of fog and visibility following Kunkel, (1984)
! var_fog_RUC: Computation of fog and visibility following RUC method Smirnova, (2000)
! var_fog_FRAML50: fog and visibility following Gultepe and Milbrandt, (2010)
! var_hur: relative humidity using August-Roche-Magnus approximation [1]
! var_hurs: relative humidity at 2m using August-Roche-Magnus approximation [1]
! var_hus: specific humidity [1]
! var_huss: 2m specific humidity [1]
! var_potevap_bulk: potential evapotranspiration following simple bulk formulation
! var_potevap_Milly92: potential bulk evapotranspiration with Milly 1992 correction
! var_potevap_bulkgen: generic potential evapotranspiration following simple bulk formulation
! var_potevap_Milly92gen: genreic potential bulk evapotranspiration with Milly 1992 correction
! var_press: air pressure [Pa]
! var_psl_ecmwf: sea level pressure using ECMWF method
! var_psl_ptarget: mean sea level pressure using a target pressure
! var_psl_shuell: mean sea level pressure using the hydrostatic equation with the Shuell correction
! var_massvertint: Subroutine to vertically integrate a 1D variable in eta vertical coordinates
! var_vertint: Subroutine to vertically integrate a 1D variable in any vertical coordinates
! var_residencetime_2var2D: Subroutine to compute the time of residence of two 2D variables for a 
!   single time-step
! var_ta: air temperature [K]
! var_tav: Function for variable virtual temperature including solid water species
! var_tds: dew point temperature at 2m using August-Roche-Magnus approximation [K]
! var_tws_S11: Subroutine to compute wet bulb temperature using equation after Stull, R. (2011) [K]
! var_uava: earth-rotated wind components [ms-1]
! var_uasvas: 10m earth-rotated wind components [ms-1]
! var_zg: geopotential height [m]
! var_zmla_generic: pbl-height following a generic method [m]
! var_zmla_RIgen: Subroutine to compute the pbl height using bulk Richardson number
! var_ztaqv_MOtheor: Subroutine of temperature and water vapour extrapolation following Moin-Obukhov 
!   Similarity theory
! var_zwind: extrapolate the wind at a given height following the 'power law' methodology
! var_zwind_log: extrapolate the wind at a given height following the 'logarithmic law' methodology
! var_zwind_MOtheor: wind extrapolation following Moin-Obukhov theory
! water_budget: Subroutine to compute accumulated water budget
! gustwind_Brasseur01: Wind gust following Brasseur (2001), MWR. 
! gustwind_afwa: Wind gust following heavy precip methodology
! stabfunc_scalar_businger: Subroutine of the stability function after Businger et al. (1971), JAS, 
!   28(2), 181-189
! univ_func_scal_Businger71: Subroutine of the universal functions for scalars according to Businger
!   1971
! zero_isotherm1D: Function to compute 1D 0-isotherm

!!!
! Operations
!!!
! interval_agg: Subroutine to compute aggregation by intervals along the full axis of of mass-values
! massconserv_vinterp1D: Subrotine to mass-conservative vertically interpolate a variable 1D
! moist_group: Subroutine to group 1D values from each water-species to a single variable
! moist_group2D: Subroutine to group 2D values from each water-species to a single variable
! moist_redistribute: Subroutine to re-distribute a multi water-species 1D value to each specie
! moist_redistribute2D: Subroutine to re-distribute a multi water-species 2D value to each specie
! NumIntegration: Subroutine to compute numerical integrations according to the trapezoidal methodology
! partial_agg: Subroutine to compute a partial aggregation of mass-values between an interval
! presbars_to_hgtbars: Subroutine to transform from desired bars defined by their central pressures to 
!   bars in height coordinates using model's geopotential to define its heights

!!!
! ORCHIDEE related diagnostics (version 2 inclusion)
!!!
! coefcdrag1D: 1D copmutation of the drag coefficients for momentum, sensible and latent heat fluxs, 
!   neutral and the Richardson number between the surface and the reference level and the pressure at 
!   the refernce level
! SFCDIAGS_ORCHIDEE1D: 1D Surface diagnostics of t2 and q2 from the ORCHIDEE land model
! screenc1D: 1D correction to the wind, potential temperature and specific humidity anomalies at the 
!   reference level zref, and relatively to the first level (for wind) o to the surface (for the 
!   potential temperature and humidity
! screenp1D: 1D forecaster of the wind, potential temperature and specific humidity anomalies at the 
!   reference level zref, and relatively to the first level (for wind) o to the surface (for the 
!   potential temperature and humidity from Dyer-Businger relations

  
!!!
! Variables
!!!

  SUBROUTINE Cdrag_0(ust,uas,vas,Cd)
! Fuction to compute a first order generic approximation of the drag coefficient as
!   CD = (ust/wss)**2
!  after, Garratt, J.R., 1992.: The Atmospheric Boundary Layer. Cambridge Univ. Press, 
!    Cambridge, U.K., 316 pp
! Ackonwledgement: M. A. Jimenez, UIB
!
    IMPLICIT NONE

    REAL, INTENT(in)                                     :: ust, uas, vas
    REAL, INTENT(out)                                    :: Cd

!!!!!!! Variables
! ust: u* in similarity theory [ms-1]
! uas, vas: x/y-components of wind at 10 m
! Cd: Drag coefficient [-]

    sfname = 'Cdrag_0'
    IF (uas**2 + vas**2 /= 0.) THEN 
      Cd = ust**2/(uas**2+vas**2)
    ELSE
      Cd = 0.
    END IF

  END SUBROUTINE Cdrag_0

!
! L. Fita, CIMA. Jan 2018
! ---- BEGIN modified from module_diag_afwa.F / module_diag_functions.F ---- !

  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~ 
  !~ Name:
  !~    calc_rh
  !~
  !~ Description:
  !~    This function calculates relative humidity given pressure, 
  !~    temperature, and water vapor mixing ratio.
  !~ 
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION calc_rh ( p, t, qv ) result ( rh )
    
    IMPLICIT NONE
 
    REAL, INTENT(IN) :: p, t, qv
    REAL :: rh

    ! Local
    ! -----
    REAL, PARAMETER :: pq0=379.90516
    REAL, PARAMETER :: a2=17.2693882
    REAL, PARAMETER :: a3=273.16
    REAL, PARAMETER :: a4=35.86
    REAL, PARAMETER :: rhmin=1.
    REAL :: q, qs
    INTEGER :: i,j,k
  
    ! Following algorithms adapted from WRFPOST
    ! May want to substitute with another later
    ! -----------------------------------------
      q=qv/(1.0+qv)
      qs=pq0/p*exp(a2*(t-a3)/(t-a4))
      rh=100.*q/qs
      IF (rh .gt. 100.) THEN
        rh=100.
      ELSE IF (rh .lt. rhmin) THEN
        rh=rhmin
      ENDIF

  END FUNCTION calc_rh

  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~
  !~ Name:
  !~    Theta
  !~
  !~ Description:
  !~    This function calculates potential temperature as defined by
  !~    Poisson's equation, given temperature and pressure ( hPa ).
  !~
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION Theta ( t, p )
  IMPLICIT NONE

     !~ Variable declaration
     !  --------------------
     REAL, INTENT ( IN ) :: t
     REAL, INTENT ( IN ) :: p
     REAL                :: theta

     ! Using WRF values
     !REAL :: Rd ! Dry gas constant
     !REAL :: Cp ! Specific heat of dry air at constant pressure
     !REAL :: p00 ! Standard pressure ( 1000 hPa )
     REAL                                                :: Rd, p00
  
     !Rd =  287.04
     !Cp = 1004.67
     !p00 = 1000.00

     Rd = r_d
     p00 = p1000mb/100.

     !~ Poisson's equation
     !  ------------------
     theta = t * ( (p00/p)**(Rd/Cp) )
  
  END FUNCTION Theta



  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~
  !~ Name:
  !~    Thetae
  !~
  !~ Description:
  !~    This function returns equivalent potential temperature using the 
  !~    method described in Bolton 1980, Monthly Weather Review, equation 43.
  !~
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION Thetae ( tK, p, rh, mixr )
  IMPLICIT NONE

     !~ Variable Declarations
     !  ---------------------
     REAL :: tK        ! Temperature ( K )
     REAL :: p         ! Pressure ( hPa )
     REAL :: rh        ! Relative humidity
     REAL :: mixr      ! Mixing Ratio ( kg kg^-1)
     REAL :: te        ! Equivalent temperature ( K )
     REAL :: thetae    ! Equivalent potential temperature
  
     ! Using WRF values
     !REAL, PARAMETER :: R  = 287.04         ! Universal gas constant (J/deg kg)
     !REAL, PARAMETER :: P0 = 1000.0         ! Standard pressure at surface (hPa)
     REAL                                                :: R, p00, Lv
     !REAL, PARAMETER :: lv = 2.54*(10**6)   ! Latent heat of vaporization
                                            ! (J kg^-1)
     !REAL, PARAMETER :: cp = 1004.67        ! Specific heat of dry air constant
                                            ! at pressure (J/deg kg)
     REAL :: tlc                            ! LCL temperature
  
     R = r_d
     p00 = p1000mb/100.
     lv = XLV

     !~ Calculate the temperature of the LCL
     !  ------------------------------------
     tlc = TLCL ( tK, rh )
  
     !~ Calculate theta-e
     !  -----------------
     thetae = (tK * (p00/p)**( (R/Cp)*(1.- ( (.28E-3)*mixr*1000.) ) ) )* &
                 exp( (((3.376/tlc)-.00254))*&
                    (mixr*1000.*(1.+(.81E-3)*mixr*1000.)) )
  
  END FUNCTION Thetae



  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~
  !~ Name:
  !~    The2T.f90
  !~
  !~ Description:
  !~    This function returns the temperature at any pressure level along a
  !~    saturation adiabat by iteratively solving for it from the parcel
  !~    thetae.
  !~
  !~ Dependencies:
  !~    function thetae.f90
  !~
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION The2T ( thetaeK, pres, flag ) result ( tparcel )
  IMPLICIT NONE
  
     !~ Variable Declaration
     !  --------------------
     REAL,    INTENT     ( IN ) :: thetaeK
     REAL,    INTENT     ( IN ) :: pres
     LOGICAL, INTENT ( INOUT )  :: flag
     REAL                       :: tparcel
  
     REAL :: thetaK
     REAL :: tovtheta
     REAL :: tcheck
     REAL :: svpr, svpr2
     REAL :: smixr, smixr2
     REAL :: thetae_check, thetae_check2
     REAL :: tguess_2, correction
  
     LOGICAL :: found
     INTEGER :: iter
  
     ! Using WRF values
     !REAL :: R     ! Dry gas constant
     !REAL :: Cp    ! Specific heat for dry air
     !REAL :: kappa ! Rd / Cp
     !REAL :: Lv    ! Latent heat of vaporization at 0 deg. C
     REAL                                                :: R, kappa, Lv

     R = r_d
     Lv = XLV
     !R     = 287.04
     !Cp    = 1004.67
     Kappa = R/Cp
     !Lv    = 2.500E+6

     !~ Make initial guess for temperature of the parcel
     !  ------------------------------------------------
     tovtheta = (pres/100000.0)**(r/cp)
     tparcel  = thetaeK/exp(lv*.012/(cp*295.))*tovtheta

     iter = 1
     found = .false.
     flag = .false.

     DO
        IF ( iter > 105 ) EXIT

        tguess_2 = tparcel + REAL ( 1 )

        svpr   = 6.122 * exp ( (17.67*(tparcel-273.15)) / (tparcel-29.66) )
        smixr  = ( 0.622*svpr ) / ( (pres/100.0)-svpr )
        svpr2  = 6.122 * exp ( (17.67*(tguess_2-273.15)) / (tguess_2-29.66) )
        smixr2 = ( 0.622*svpr2 ) / ( (pres/100.0)-svpr2 )

        !  ------------------------------------------------------------------ ~!
        !~ When this function was orinially written, the final parcel         ~!
        !~ temperature check was based off of the parcel temperature and      ~!
        !~ not the theta-e it produced.  As there are multiple temperature-   ~!
        !~ mixing ratio combinations that can produce a single theta-e value, ~!
        !~ we change the check to be based off of the resultant theta-e       ~!
        !~ value.  This seems to be the most accurate way of backing out      ~!
        !~ temperature from theta-e.                                          ~!
        !~                                                                    ~!
        !~ Rentschler, April 2010                                             ~!
        !  ------------------------------------------------------------------  !

        !~ Old way...
        !thetaK = thetaeK / EXP (lv * smixr  /(cp*tparcel) )
        !tcheck = thetaK * tovtheta

        !~ New way
        thetae_check  = Thetae ( tparcel,  pres/100., 100., smixr  )
        thetae_check2 = Thetae ( tguess_2, pres/100., 100., smixr2 )

        !~ Whew doggies - that there is some accuracy...
        !IF ( ABS (tparcel-tcheck) < .05) THEN
        IF ( ABS (thetaeK-thetae_check) < .001) THEN
           found = .true.
           flag  = .true.
           EXIT
        END IF

        !~ Old
        !tparcel = tparcel + (tcheck - tparcel)*.3

        !~ New
        correction = ( thetaeK-thetae_check ) / ( thetae_check2-thetae_check )
        tparcel = tparcel + correction

        iter = iter + 1
     END DO

     !IF ( .not. found ) THEN
     !   print*, "Warning! Thetae to temperature calculation did not converge!"
     !   print*, "Thetae ", thetaeK, "Pressure ", pres
     !END IF

  END FUNCTION The2T



  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~
  !~ Name:
  !~    VirtualTemperature
  !~
  !~ Description:
  !~    This function returns virtual temperature given temperature ( K )
  !~    and mixing ratio.
  !~
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION VirtualTemperature ( tK, w ) result ( Tv )
  IMPLICIT NONE

     !~ Variable declaration
     real, intent ( in ) :: tK !~ Temperature
     real, intent ( in ) :: w  !~ Mixing ratio ( kg kg^-1 )
     real                :: Tv !~ Virtual temperature

     Tv = tK * ( 1.0 + (w/0.622) ) / ( 1.0 + w )

  END FUNCTION VirtualTemperature

  REAL FUNCTION var_tav(ta, qv, qc, qs)
  ! Function for variable virtual temperature including solid water species
  !      FROM: https://glossary.ametsoc.org/wiki/Virtual_temperature
  !        tav = ta(1 + qv/epsilon)/(1 + qv + qc + qs)

    IMPLICIT NONE

    REAL, INTENT(IN)                                     :: ta, qv, qc, qs
  
    ! Local
    CHARACTER(len=50)                                    :: fname

!!!!!!! Variables
! ta: air temperature [K]
! qv: water vapor mixing ratio [kgkg-1]
! qc: liquid water (cloud) mixing ratio [kgkg-1]
! qs: solid water (ice+snow+whail+graupel) mixing ratio [kgkg-1]
!      epsilon = Rd/Rv = 0.622

    fname = 'var_tav'
    
    var_tav = -99999.

    var_tav = ta*(1.+qv/EP_2)/(1. + qv + qc + qs)

    RETURN
    
  END FUNCTION var_tav

  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~
  !~ Name:
  !~    SaturationMixingRatio
  !~
  !~ Description:
  !~    This function calculates saturation mixing ratio given the
  !~    temperature ( K ) and the ambient pressure ( Pa ).  Uses 
  !~    approximation of saturation vapor pressure.
  !~
  !~ References:
  !~    Bolton (1980), Monthly Weather Review, pg. 1047, Eq. 10
  !~
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION SaturationMixingRatio ( tK, p ) result ( ws )

    IMPLICIT NONE

    REAL, INTENT ( IN ) :: tK
    REAL, INTENT ( IN ) :: p
    REAL                :: ws

    REAL :: es

    es = 6.122 * exp ( (17.67*(tK-273.15))/ (tK-29.66) )
    ws = ( 0.622*es ) / ( (p/100.0)-es )

  END FUNCTION SaturationMixingRatio

  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~                                                                     
  !~ Name:                                                                
  !~    tlcl                                                               
  !~                                                                        
  !~ Description:                                                            
  !~    This function calculates the temperature of a parcel of air would have
  !~    if lifed dry adiabatically to it's lifting condensation level (lcl).  
  !~                                                                          
  !~ References:                                                              
  !~    Bolton (1980), Monthly Weather Review, pg. 1048, Eq. 22
  !~                                                                          
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION TLCL ( tk, rh )
    
    IMPLICIT NONE
 
    REAL, INTENT ( IN ) :: tK   !~ Temperature ( K )
    REAL, INTENT ( IN ) :: rh   !~ Relative Humidity ( % )
    REAL                :: tlcl
    
    REAL :: denom, term1, term2

    term1 = 1.0 / ( tK - 55.0 )
    IF ( rh > REAL (0) ) THEN
      term2 = ( LOG (rh/100.0)  / 2840.0 )
    ELSE
      term2 = ( LOG (0.001/1.0) / 2840.0 )
    END IF
    denom = term1 - term2
    tlcl = ( 1.0 / denom ) + REAL ( 55 ) 

  END FUNCTION TLCL

! Changing to a subroutine
!  FUNCTION var_cape_afwa(nz, tk, rhv, p, hgt, sfc, cape, cin, zlfc, plfc, lidx, parcel) RESULT (ostat)
  SUBROUTINE var_cape_afwa(nz, tk, rh, p, hgt, sfc, cape, cin, zlfc, plfc, lidx, parcel)
! Function to compute cape on a 1D column following implementation in phys/module_diag_afwa.F (version from WRF4.3.1)
  
!!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~                                                                          ~!
  !~ Name:                                                                    ~!
  !~    Buoyancy                                                              ~!
  !~                                                                          ~!
  !~ Description:                                                             ~!
  !~    This function computes Convective Available Potential Energy (CAPE)   ~!
  !~    with inhibition as a result of water loading given the data required  ~!
  !~    to run up a sounding.                                                 ~!
  !~                                                                          ~!
  !~    Additionally, since we are running up a sounding anyways, this        ~!
  !~    function returns the height of the Level of Free Convection (LFC) and ~!
  !~    the pressure at the LFC.  That-a-ways, we don't have to run up a      ~!
  !~    sounding later, saving a relatively computationally expensive         ~!
  !~    routine.                                                              ~!
  !~                                                                          ~!
  !~ Usage:                                                                   ~!
  !~    ostat = Buoyancy ( tK, rh, p, hgt, sfc, CAPE, ZLFC, PLFC, parcel )    ~!
  !~                                                                          ~!
  !~ Where:                                                                   ~!
  !~                                                                          ~!
  !~    IN                                                                    ~!
  !~    --                                                                    ~!
  !~    tK   = Temperature ( K )                                              ~!
  !~    rh   = Relative Humidity ( % )                                        ~!
  !~    p    = Pressure ( Pa )                                                ~!
  !~    hgt  = Geopotential heights ( m )                                     ~!
  !~    sfc  = integer rank within submitted arrays that represents the       ~!
  !~           surface                                                        ~!
  !~                                                                          ~!
  !~    OUT                                                                   ~!
  !~    ---                                                                   ~!
  !~    ostat         INTEGER return status. Nonzero is bad.                  ~!
  !~    CAPE ( J/kg ) Convective Available Potential Energy                   ~!
  !~    ZLFC ( gpm )  Height at the LFC                                       ~!
  !~    PLFC ( Pa )   Pressure at the LFC                                     ~!
  !~                                                                          ~!
  !~    tK, rh, p, and hgt are all REAL arrays, arranged from lower levels    ~!
  !~    to higher levels.                                                     ~!
  !~                                                                          ~!
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!

    IMPLICIT NONE
  
    INTEGER, INTENT ( IN )  :: nz          !~ Number of vertical levels
    INTEGER, INTENT ( IN )  :: sfc         !~ Surface level in the profile
    REAL,    INTENT ( IN )  :: tk   ( nz ) !~ Temperature profile ( K )
    REAL,    INTENT ( IN )  :: rh   ( nz ) !~ Relative Humidity profile ( % )
    REAL,    INTENT ( IN )  :: p    ( nz ) !~ Pressure profile ( Pa )
    REAL,    INTENT ( IN )  :: hgt  ( nz ) !~ Height profile ( gpm )
    REAL,    INTENT ( OUT ) :: cape        !~ CAPE ( J kg^-1 )
    REAL,    INTENT ( OUT ) :: cin         !~ CIN  ( J kg^-1 )
    REAL,    INTENT ( OUT ) :: zlfc        !~ LFC Height ( gpm )
    REAL,    INTENT ( OUT ) :: plfc        !~ LFC Pressure ( Pa )
    REAL,    INTENT ( OUT ) :: lidx        !~ Lifted index
    INTEGER                 :: ostat       !~ Function return status
                                           !~ Nonzero is bad.

    INTEGER, INTENT ( IN  ) :: parcel      !~ Most Unstable = 1 (default)
                                           !~ Mean layer    = 2
                                           !~ Surface based = 3
  
    !~ Derived profile variables
    !  -------------------------
    REAL                    :: ws   ( nz ) !~ Saturation mixing ratio
    REAL                    :: w    ( nz ) !~ Mixing ratio
    REAL                    :: etheta( nz )!~ Equivalent potential temperature. Modified by Zhixiao.
    REAL                    :: dTvK ( nz ) !~ Parcel / ambient Tv difference
    REAL                    :: buoy ( nz ) !~ Buoyancy
    REAL                    :: tlclK       !~ LCL temperature ( K )
    REAL                    :: plcl        !~ LCL pressure ( Pa )
    REAL                    :: pel         !~ Equilibrium pressure ( Pa ). Modified by Zhixiao.
    REAL                    :: nbuoy       !~ Negative buoyancy
    REAL                    :: pbuoy       !~ Positive buoyancy

    !~ Source parcel information
    !  -------------------------
    REAL                    :: srctK       !~ Source parcel temperature ( K )
    REAL                    :: srcrh       !~ Source parcel rh ( % )
    REAL                    :: srcws       !~ Source parcel sat. mixing ratio
    REAL                    :: srcw        !~ Source parcel mixing ratio
    REAL                    :: srcp        !~ Source parcel pressure ( Pa )
    REAL                    :: srctheta    !~ Source parcel theta ( K )
    REAL                    :: srcthetaeK  !~ Source parcel theta-e ( K )
    INTEGER                 :: srclev      !~ Level of the source parcel
    REAL                    :: spdiff      !~ Pressure difference
    REAL                    :: srce        !~ Equivalent potential temperature ( K ). Modified by Zhixiao.
   
    !~ Parcel variables
    !  ----------------
    REAL                    :: ptK        !~ Parcel temperature ( K )
    REAL                    :: ptvK       !~ Parcel virtual temperature ( K )
    REAL                    :: tvK        !~ Ambient virtual temperature ( K )
    REAL                    :: pw         !~ Parcel mixing ratio
  
    !~ Other utility variables
    !  -----------------------
    INTEGER                 :: i, j, k    !~ Dummy iterator
    INTEGER                 :: lfclev     !~ Level of LFC
    INTEGER                 :: ellev      !~ Level of EL. Modified by Zhixiao.
    INTEGER                 :: lcllev     !~ Level of LCL. Modified by Zhixiao.
    INTEGER                 :: prcl       !~ Internal parcel type indicator
    INTEGER                 :: mlev       !~ Level for ML calculation
    INTEGER                 :: lyrcnt     !~ Number of layers in mean layer
    LOGICAL                 :: flag       !~ Dummy flag
    LOGICAL                 :: wflag      !~ Saturation flag
    REAL                    :: freeze     !~ Water loading multiplier
    REAL                    :: pdiff      !~ Pressure difference between levs 
    REAL                    :: pm, pu, pd !~ Middle, upper, lower pressures
    REAL                    :: lidxu      !~ Lifted index at upper level
    REAL                    :: lidxd      !~ Lifted index at lower level
  
    !~ Thermo / dynamical constants
    !  ----------------------------
    REAL                    :: Rd         !~ Dry gas constant
       PARAMETER ( Rd = 287.058 )         !~ J deg^-1 kg^-1
    REAL                    :: Cp         !~ Specific heat constant pressure
       PARAMETER ( Cp = 1004.67 )         !~ J deg^-1 kg^-1
    REAL                    :: g          !~ Acceleration due to gravity
       PARAMETER ( g  = 9.80665 )         !~ m s^-2
    REAL                    :: RUNDEF
       PARAMETER ( RUNDEF = -9.999E30 )


!!!!!!! Variables
! nz: Number of vertical levels
! sfc: Surface level in the profile
! tk: Temperature profile [K]
! rhv: Relative Humidity profile [1]
! rh: Relative Humidity profile [%]
! p: Pressure profile [Pa]
! hgt: Height profile [gpm]
! cape: CAPE [Jkg-1]
! cin: CIN [Jkg-1]
! zlfc: LFC Height [gpm]
! plfc: LFC Pressure [Pa]
! lidx: Lifted index
!   FROM: https://en.wikipedia.org/wiki/Lifted_index
!     lidx >= 6: Very Stable Conditions
!     6 > lidx > 1: Stable Conditions, Thunderstorms Not Likely
!     0 > lidx > -2: Slightly Unstable, Thunderstorms Possible, With Lifting Mechanism (i.e., cold front, daytime heating, ...)
!     -2 > lidx > -6: Unstable, Thunderstorms Likely, Some Severe With Lifting Mechanism
!     -6 > lidx: Very Unstable, Severe Thunderstorms Likely With Lifting Mechanism
! ostat: Function return status (Nonzero is bad)
! parcel:
!   Most Unstable = 1 (default)
!   Mean layer = 2
!   Surface based = 3
!~ Derived profile variables
!  -------------------------
! ws: Saturation mixing ratio
! w: Mixing ratio
! dTvK: Parcel / ambient Tv difference
! buoy: Buoyancy
! tlclK: LCL temperature [K]
! plcl: LCL pressure [Pa]
! nbuoy: Negative buoyancy
! pbuoy: Positive buoyancy
  
!~ Source parcel information
!  -------------------------
! srctK: Source parcel temperature [K]
! srcrh: Source parcel rh [%]
! srcws: Source parcel sat. mixing ratio
! srcw: Source parcel mixing ratio
! srcp: Source parcel pressure [Pa]
! srctheta: Source parcel theta [K]
! srcthetaeK: Source parcel theta-e [K]
! srclev: Level of the source parcel
! spdiff: Pressure difference
   
!~ Parcel variables
!  ----------------
! ptK: Parcel temperature [K]
! ptvK: Parcel virtual temperature [K]
! tvK: Ambient virtual temperature [K]
! pw: Parcel mixing ratio
  
!~ Other utility variables
!  -----------------------
! lfclev: Level of LFC
! prcl: Internal parcel type indicator
! mlev: Level for ML calculation
! lyrcnt: Number of layers in mean layer
! flag: Dummy flag
! wflag: Saturation flag
! freeze: Water loading multiplier
! pdiff: Pressure difference between levs 
! pm, pu, pd: Middle, upper, lower pressures
! lidxu: Lifted index at upper level
! lidxd: Lifted index at lower level

    sfname = 'var_cape_afwa'  

    !~ Initialize variables
    !  --------------------
    ostat  = 0
    CAPE   = REAL ( 0 )
    CIN    = RUNDEF                       !Change CIN filling values from 0 to default filling. 
                                          !  CIN should not initially be filled by 0, because 0 means
                                          !  no inhibition energy. Modified by Zhixiao
    ZLFC   = RUNDEF
    PLFC   = RUNDEF
  
    !~ Look for submitted parcel definition
    !~ 1 = Most unstable
    !~ 2 = Mean layer
    !~ 3 = Surface based
    !  -------------------------------------
    IF ( parcel > 3 .or. parcel < 1 ) THEN
       prcl = 1
    ELSE
       prcl =  parcel
    END IF
  
    !~ Initalize our parcel to be (sort of) surface based.  Because of
    !~ issues we've been observing in the WRF model, specifically with
    !~ excessive surface moisture values at the surface, using a true
    !~ surface based parcel is resulting a more unstable environment
    !~ than is actually occuring.  To address this, our surface parcel
    !~ is now going to be defined as the parcel between 25-50 hPa
    !~ above the surface. UPDATE - now that this routine is in WRF,
    !~ going to trust surface info. GAC 20140415
    !  ----------------------------------------------------------------
  
    !~ Compute mixing ratio values for the layer
    !  -----------------------------------------
    DO k = sfc, nz
      ws  ( k )   = SaturationMixingRatio ( tK(k), p(k) )
      w   ( k )   = ( rh(k)/100.0 ) * ws ( k )
      !Removed by Zhixiao. Water vapor mixing ratio (w) is not conserved during parcel lifting 
      !  processes. We should not use w to define MU layer.
      !thetav(k)   = Theta ( VirtualTemperature ( tK (k), w (k) ), p(k)/100.0 )
      !Added by Zhixiao. Critical modification: We use the model level with maximum equivalent 
      !  potential temperature (etheta) below 500mb to define the MU layer
      !Because equivalent potential temperature is conserved in dry and moist adiabatic processes.
      etheta(k)   = Thetae( tK(k), p(k)/100.0, rh(k), w(k) )
    END DO
  
    srclev      = sfc
    srctK       = tK    ( sfc )
    srcrh       = rh    ( sfc )
    srcp        = p     ( sfc )
    srcws       = ws    ( sfc )
    srcw        = w     ( sfc )
    srctheta    = Theta ( tK(sfc), p(sfc)/100.0 )
    srce        = etheta (sfc) !Modified by Zhixiao
   
    !~ Compute the profile mixing ratio.  If the parcel is the MU parcel,
    !~ define our parcel to be the most unstable parcel within the lowest
    !~ 180 mb.
    !  -------------------------------------------------------------------
    mlev = sfc + 1
    !Change initial searching level from the second to first model level. Because we did not compute 
    !  pdiff, and p(k-1) properties is unnecessary.
    !Modified by Zhixiao.
    DO k = sfc, nz
   
      !~ Identify the last layer within 100 hPa of the surface
      !  -----------------------------------------------------
      pdiff = ( p (sfc) - p (k) ) / REAL ( 100 )
      IF ( pdiff <= REAL (100) ) mlev = k

      !~ If we've made it past the lowest 500 hPa, exit the loop. MU layer is assumed below 500 hPa. Modified by Zhixiao.
      !  -------------------------------------------------------
      IF ( p(k) <= REAL (50000) ) EXIT

      IF ( prcl == 1 ) THEN
        ! Removed by Zhixiao, w can not used for defining MU layer
        !IF ( (p(k) > 70000.0) .and. (w(k) > srcw) ) THEN
        ! Modified by Zhixiao, MU layer is featured by the max etheta
        IF (etheta(k) > srce)  THEN !Modified by Zhixiao.
          srctheta = Theta ( tK(k), p(k)/100.0 )
          srcw = w ( k )
          srclev  = k
          srctK   = tK ( k )
          srcrh   = rh ( k )
          srcp    = p  ( k )
          srce = etheta(k) !Modified by Zhixiao
        END IF
      END IF
   
    END DO
   
    !~ If we want the mean layer parcel, compute the mean values in the
    !~ lowest 100 hPa.
    !  ----------------------------------------------------------------
    lyrcnt =  mlev - sfc + 1
    IF ( prcl == 2 ) THEN
   
      srclev   = sfc
      srctK    = SUM ( tK (sfc:mlev) ) / REAL ( lyrcnt )
      srcw     = SUM ( w  (sfc:mlev) ) / REAL ( lyrcnt )
      srcrh    = SUM ( rh (sfc:mlev) ) / REAL ( lyrcnt )
      srcp     = SUM ( p  (sfc:mlev) ) / REAL ( lyrcnt )
      srctheta = Theta ( srctK, srcp/100. )
   
    END IF
   
    srcthetaeK = Thetae ( srctK, srcp/100.0, srcrh, srcw )
    !~ Calculate temperature and pressure of the LCL
    !  ---------------------------------------------
    tlclK = TLCL ( tK(srclev), rh(srclev) )
    plcl  = p(srclev) * ( (tlclK/tK(srclev))**(Cp/Rd) )
      
    !Add caculation for LCL. Modified by Zhixiao
    lcllev = -1 !Modified by Zhixiao
    flag=.false. !Modified by Zhixiao
    DO k=sfc,nz !zhixiao search the layer of LCL
      IF (p (k) <= plcl) THEN !Modified by Zhixiao
        lcllev=k !Modified by Zhixiao
        flag=.true. !Modified by Zhixiao
      END IF !Modified by Zhixiao
      IF (flag) EXIT !Modified by Zhixiao
    END DO !Modified by Zhixiao  

    flag=.false. !Modified by Zhixiao
    !~ Now lift the parcel
    !  -------------------
   
    buoy  = REAL ( 0 )
    pw    = srcw
    wflag = .false.
    DO k  = srclev, nz
      IF ( p (k) <= plcl ) THEN
   
        !~ The first level after we pass the LCL, we're still going to
        !~ lift the parcel dry adiabatically, as we haven't added the
        !~ the required code to switch between the dry adiabatic and moist
        !~ adiabatic cooling.  Since the dry version results in a greater
        !~ temperature loss, doing that for the first step so we don't over
        !~ guesstimate the instability.
        !  ----------------------------------------------------------------
   
        IF ( wflag ) THEN
          flag  = .false.
   
          !~ Above the LCL, our parcel is now undergoing moist adiabatic
          !~ cooling.  Because of the latent heating being undergone as
          !~ the parcel rises above the LFC, must iterative solve for the
          !~ parcel temperature using equivalant potential temperature,
          !~ which is conserved during both dry adiabatic and
          !~ pseudoadiabatic displacements.
          !  --------------------------------------------------------------
          ptK   = The2T ( srcthetaeK, p(k), flag )
   
          !~ Calculate the parcel mixing ratio, which is now changing
          !~ as we condense moisture out of the parcel, and is equivalent
          !~ to the saturation mixing ratio, since we are, in theory, at
          !~ saturation.
          !  ------------------------------------------------------------
          pw = SaturationMixingRatio ( ptK, p(k) )
   
          !~ Now we can calculate the virtual temperature of the parcel
          !~ and the surrounding environment to assess the buoyancy.
          !  ----------------------------------------------------------
          ptvK  = VirtualTemperature ( ptK, pw )
          tvK   = VirtualTemperature ( tK (k), w (k) )
   
          !~ Modification to account for water loading
          !  -----------------------------------------
          freeze = 0.033 * ( 263.15 - pTvK )
          IF ( freeze > 1.0 ) freeze = 1.0
          IF ( freeze < 0.0 ) freeze = 0.0
   
          !~ Approximate how much of the water vapor has condensed out
          !~ of the parcel at this level
          !  ---------------------------------------------------------
          freeze = freeze * 333700.0 * ( srcw - pw ) / 1005.7
   
          pTvK = pTvK - pTvK * ( srcw - pw ) + freeze
          dTvK ( k ) = ptvK - tvK
          buoy ( k ) = g * ( dTvK ( k ) / tvK )
   
        ELSE
   
          !~ Since the theta remains constant whilst undergoing dry
          !~ adiabatic processes, can back out the parcel temperature
          !~ from potential temperature below the LCL
          !  --------------------------------------------------------
          ptK   = srctheta / ( 100000.0/p(k) )**(Rd/Cp)
   
          !~ Grab the parcel virtual temperture, can use the source
          !~ mixing ratio since we are undergoing dry adiabatic cooling
          !  ----------------------------------------------------------
          ptvK  = VirtualTemperature ( ptK, srcw )
   
          !~ Virtual temperature of the environment
          !  --------------------------------------
          tvK   = VirtualTemperature ( tK (k), w (k) )
   
          !~ Buoyancy at this level
          !  ----------------------
          dTvK ( k ) = ptvK - tvK
          buoy ( k ) = g * ( dtvK ( k ) / tvK )
   
          wflag = .true.
   
        END IF
   
      ELSE
   
        !~ Since the theta remains constant whilst undergoing dry
        !~ adiabatic processes, can back out the parcel temperature
        !~ from potential temperature below the LCL
        !  --------------------------------------------------------
        ptK   = srctheta / ( 100000.0/p(k) )**(Rd/Cp)
   
        !~ Grab the parcel virtual temperture, can use the source
        !~ mixing ratio since we are undergoing dry adiabatic cooling
        !  ----------------------------------------------------------
        ptvK  = VirtualTemperature ( ptK, srcw )
   
        !~ Virtual temperature of the environment
        !  --------------------------------------
        tvK   = VirtualTemperature ( tK (k), w (k) )
   
        !~ Buoyancy at this level
        !  ---------------------
        dTvK ( k ) = ptvK - tvK
        buoy ( k ) = g * ( dtvK ( k ) / tvK )
   
      END IF

      !~ Chirp
      !  -----
  !   WRITE ( *,'(I15,6F15.3)' )k,p(k)/100.,ptK,pw*1000.,ptvK,tvK,buoy(k)
   
    END DO
   
    !~ Add up the buoyancies, find the LFC
    !  -----------------------------------
    flag   = .false.
    lfclev = -1
    ellev = -1 !Modified by Zhixiao
    DO k = sfc, nz !Modified by Zhixiao
      !~ LFC is defiend as the highest level when negative buyancy turns postive.
      ! Let us ignore the lower LFCs, and keep the highest LFC as the final result
      !  -----------------------------------
      IF ( .not. flag .and. buoy (k) > REAL (0) .and. p (k) <= plcl ) THEN !Modified by Zhixiao
        flag = .true.
        lfclev = k
      END IF
      !~ Take the Highest EL as final result. Modified by Zhixiao
      !  ----------------------------------------------------------------
      IF (k >= 2) THEN !Modified by Zhixiao
        IF ( flag .and. buoy (k) < REAL (0) .and. buoy (k-1) >= REAL (0)) THEN !Modified by Zhixiao
          ellev = k !Modified by Zhixiao
        END IF
      END IF
    END DO
    IF (ellev >= 0) THEN !Modified by Zhixiao
      pel = p(ellev) !Modified by Zhixiao
      CIN=REAL ( 0 ) !Modified by Zhixiao
      DO k = sfc+1, nz !Modified by Zhixiao
        ! CAPE and CIN is defined as integrated positive and negative buoyant energy between LCL and EL, respectively. Modified by Zhixiao
        IF ( p (k) <= plcl .and. p (k) >= pel) THEN !Modified by Zhixiao
          CAPE = CAPE + MAX ( buoy (k), 0.0 ) * ( hgt (k) - hgt (k-1) ) !Modified by Zhixiao
          CIN  = CIN  + MIN ( buoy (k), 0.0 ) * ( hgt (k) - hgt (k-1) ) !Modified by Zhixiao
        END IF !Modified by Zhixiao         
      END DO !Modified by Zhixiao
    END IF !Modified by Zhixiao
    !~ Calculate lifted index by interpolating difference between
    !~ parcel and ambient Tv to 500mb.
    !  ----------------------------------------------------------
    DO k = sfc + 1, nz

      pm = 50000.
      pu = p ( k )
      pd = p ( k - 1 )

      !~ If we're already above 500mb just set lifted index to 0.
      !~ --------------------------------------------------------
      IF ( pd .le. pm ) THEN
        lidx = 0.
        EXIT

      ELSEIF ( pu .le. pm .and. pd .gt. pm) THEN

        !~ Found trapping pressure: up, middle, down.
        !~ We are doing first order interpolation.  
        !  ------------------------------------------
        lidxu = -dTvK ( k ) * ( pu / 100000. ) ** (Rd/Cp)
        lidxd = -dTvK ( k-1 ) * ( pd / 100000. ) ** (Rd/Cp)
        lidx = ( lidxu * (pm-pd) + lidxd * (pu-pm) ) / (pu-pd)
        EXIT

      ENDIF

    END DO
    !~ Assuming the the LFC is at a pressure level for now
    !  ---------------------------------------------------
    IF ( lfclev > 0 ) THEN
      PLFC = p   ( lfclev )
      ZLFC = hgt ( lfclev )
    END IF

    IF ( PLFC /= PLFC .OR. PLFC < REAL (0) ) THEN
      PLFC = REAL ( -1 )
      ZLFC = REAL ( -1 )
    END IF
   
    IF ( CAPE /= CAPE ) cape = REAL ( 0 )

    IF ( CIN  /= CIN  ) cin  = RUNDEF

    !~ Chirp
    !  -----
  !  WRITE ( *,* ) ' CAPE: ', cape, ' CIN:  ', cin
  !  WRITE ( *,* ) ' LFC:  ', ZLFC, ' PLFC: ', PLFC
  !  WRITE ( *,* ) ''
  !  WRITE ( *,* ) ' Exiting buoyancy.'
  !  WRITE ( *,* ) ' ==================================== '
  !  WRITE ( *,* ) ''
   
  END SUBROUTINE var_cape_afwa

! ---- END modified from module_diag_afwa.F ---- !

!  FUNCTION var_cllmh(clfra, p, dz) [L. Fita Not working as function ?!]
  SUBROUTINE var_cllmh(clfra, p, dz, cllmh)
! Function to compute cllmh on a 1D column 1: low-cloud; 2: medium-cloud; 3: high-cloud [1]

    USE module_wrf_error
    USE module_model_constants

    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: dz
    REAL, DIMENSION(dz), INTENT(in)                      :: clfra, p
    REAL, DIMENSION(3), INTENT(out)                      :: cllmh

! _ocal
    INTEGER                                              :: iz
    REAL(r_k)                                            :: zclearl, zcloudl, zclearm, zcloudm,       &
      zclearh, zcloudh
    REAL(r_k), DIMENSION(3)                              :: Dcllmh

!!!!!!! Variables
! clfra: cloudfraction as 1D verical-column [1]
! p: pressure values of the column
    sfname = 'var_cllmh'

    message = "CORDEX computing '" // TRIM(sfname) // "' "
    CALL wrf_debug(1000,message)

    zclearl = oneRK
    zcloudl = zeroRK
    zclearm = oneRK
    zcloudm = zeroRK
    zclearh = oneRK
    zcloudh = zeroRK

    Dcllmh = oneRK

    DO iz=1, dz
      IF (p(iz) < prmhc) THEN
        Dcllmh(3) = Dcllmh(3)*(oneRK-MAX(clfra(iz),zcloudh))/(oneRK-MIN(zcloudh,oneRK-ZEPSEC))
        zcloudh = clfra(iz)
      ELSE IF ( (p(iz) >= prmhc) .AND. (p(iz) < prmlc) ) THEN
        Dcllmh(2) = Dcllmh(2)*(oneRK-MAX(clfra(iz),zcloudm))/(oneRK-MIN(zcloudm,oneRK-ZEPSEC))
        zcloudm = clfra(iz)
      ELSE IF (p(iz) >= prmlc) THEN
        Dcllmh(1) = Dcllmh(1)*(oneRK-MAX(clfra(iz),zcloudl))/(oneRK-MIN(zcloudl,oneRK-ZEPSEC))
        zcloudl = clfra(iz)
      ELSE
        PRINT *,'  ' // TRIM(sfname) // ': This is weird, pressure:', p(iz), ' Pa fails out!!'
        PRINT *,'    from high, low cloud pressures:', prmhc, ' ,', prmlc,' Pa at z-level:', iz
        PRINT *,'    p_high > p:', prmhc,'> ',p(iz),' Pa'
        PRINT *,'    p_low > p >= p_high:', prmlc,'> ',p(iz),' >=', prmhc,' Pa'
        PRINT *,'    p_low >= p:', prmlc,'>= ',p(iz),' Pa'
        message = "program wrf: error on computing 'cllmh' for CORDEX"
        CALL WRF_ERROR_FATAL ( message )
        !STOP
      END IF
    END DO

    cllmh = REAL(oneRK - Dcllmh)*100.

    RETURN 

!  END FUNCTION var_cllmh
  END SUBROUTINE var_cllmh

!  REAL FUNCTION var_clt(clfra, dz)
  SUBROUTINE var_clt(clfra, dz, clt)
! Function to compute the total cloud following 'newmicro.F90' from LMDZ using 1D vertical 
!   column values

    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: dz
    REAL, DIMENSION(dz), INTENT(in)                      :: clfra
    REAL, INTENT(out)                                    :: clt


! Local
    INTEGER                                              :: iz
    REAL(r_k)                                            :: zclear, zcloud, Dclt

!!!!!!! Variables
! cfra: 1-column cloud fraction values

    sfname = 'var_clt'

    message = "CORDEX computing '" // TRIM(sfname) // "' "
    CALL wrf_debug(1000,message)

    zclear = oneRK
    zcloud = zeroRK

    DO iz=1,dz
      zclear = zclear*(oneRK-MAX(clfra(iz),zcloud))/(oneRK-MIN(zcloud,1.-ZEPSEC))
      Dclt = oneRK - zclear
      zcloud = clfra(iz)
    END DO

   clt = REAL(Dclt)*100.

    RETURN

!  END FUNCTION var_clt
  END SUBROUTINE var_clt

  SUBROUTINE var_hur(t, press, q, dz, hur)
! Subroutine to compute relative humidity using August-Roche-Magnus approximation [1]

    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: dz
    REAL, DIMENSION(dz), INTENT(in)                      :: t, press, q
    REAL, DIMENSION(dz), INTENT(out)                     :: hur

! Local
    INTEGER                                              :: k
    REAL                                                 :: tC, es, ws

!!!!!!! Variables
! t: temperature [K]
! press: pressure [Pa]
! q: mixing ratio [kgkg-1]
! dz: vertical extension
! hur: relative humidity [1]

    sfname = 'var_hur'

    ! August - Roche - Magnus formula (Avoiding overflow on last level)
    DO k=1,dz-1
      tC = t(k) - SVPT0
    
      es = ARM1 * exp(ARM2*tC/(tC+ARM3))
      ! Saturated mixing ratio
      ws = mol_watdry*es/(0.01*press(k)-es)

      ! Relative humidity
      hur(k) = q(k) / ws
    END DO

    RETURN

  END SUBROUTINE var_hur

  SUBROUTINE var_hurs(tas, ps, qas, hurs)
! Subroutine to compute relative humidity at 2m using August-Roche-Magnus approximation [1]

    IMPLICIT NONE

    REAL, INTENT(in)                                     :: tas, ps, qas
    REAL, INTENT(out)                                    :: hurs

! Local
    REAL                                                 :: es, ws

!!!!!!! Variables
! tas: 2m temperature [K]
! ps: surface pressure [Pa]
! qas: 2m mixing ratio [kgkg-1]
! hurs: 2m relative humidity [1]

    sfname = 'var_hurs'

    ! August - Roche - Magnus formula
    es = ARM1 * exp(ARM2*(tas-SVPT0)/((tas-SVPT0)+ARM3))
    ! Saturated mixing ratio (assuming ps similar press_2m)
    ws = mol_watdry*es/(0.01*ps-es)

    ! Relative humidity
    hurs = qas / ws

    RETURN

  END SUBROUTINE var_hurs

  SUBROUTINE var_hus(q, dz, hus)
! Subroutine to compute specific humidity [1]

    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: dz
    REAL, DIMENSION(dz), INTENT(in)                      :: q
    REAL, DIMENSION(dz), INTENT(out)                     :: hus

!!!!!!! Variables
! q: mixing ratio [kgkg-1]
! dz: vertical extension
! hus: specific humidity [1]

    sfname = 'var_hus'

    ! Specifc humidity
    hus = q / (1. + q)

    RETURN

  END SUBROUTINE var_hus

  SUBROUTINE var_huss(qas, huss)
! Subroutine to compute 2m specific humidity [1]

    IMPLICIT NONE

    REAL, INTENT(in)                                     :: qas
    REAL, INTENT(out)                                    :: huss

!!!!!!! Variables
! qas: 2m mixing ratio [kgkg-1]
! huss: 2m specific humidity [1]

    sfname = 'var_huss'

    ! Specifc humidity
    huss = qas / (1. + qas)

    RETURN

  END SUBROUTINE var_huss

  SUBROUTINE var_massvertint(var, mutot, dz, deta, integral)
    ! Subroutine to vertically integrate a 1D variable in eta vertical coordinates

    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: dz
    REAL, INTENT(in)                                     :: mutot
    REAL, DIMENSION(dz), INTENT(in)                      :: var, deta
    REAL, INTENT(out)                                    :: integral

! Local
    INTEGER                                              :: k

!!!!!!! Variables
! var: vertical variable to integrate (assuming kgkg-1)
! mutot: total dry-air mass in column
! dz: vertical dimension
! deta: eta-levels difference between full eta-layers

    sfname = 'var_massvertint'

!    integral=0.
!    DO k=1,dz
!      integral = integral + var(k)*deta(k)
!    END DO
     integral = SUM(var*deta)

    integral=integral*mutot/g

    RETURN

  END SUBROUTINE var_massvertint

  SUBROUTINE var_vertint(var, dz, deta, zweight, integral)
    ! Subroutine to vertically integrate a 1D variable in any vertical coordinates

    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: dz
    REAL, DIMENSION(dz), INTENT(in)                      :: var, deta, zweight
    REAL, INTENT(out)                                    :: integral

!!!!!!! Variables
! var: vertical variable to integrate
! dz: vertical dimension
! deta: eta-levels difference between layers
! zweight: weight for each level to be applied (=1. for no effect)

    sfname = 'var_vertint'

    integral = SUM(var*deta*zweight)

    RETURN

  END SUBROUTINE var_vertint

  SUBROUTINE var_psl_ecmwf(PRPRESS, hgt, PTB, PRESBH, PRESBF, psl)
    ! Subroutine to compute sea level pressure using ECMWF method following Mats Hamrud and Philippe Courtier
    !   method found in LMDZ in phylmd/pppmer.F90 in combination with phylmd/ctsar.F90

!        IMPLICIT ARGUMENTS :  CONSTANTS FROM YOMCST,YOMGEM,YOMSTA.
!        --------------------

    IMPLICIT NONE

    REAL, INTENT(in)                                     :: PRPRESS, hgt, PTB, PRESBH, PRESBF 
    REAL, INTENT(out)                                    :: psl

! Local
    REAL                                                 :: ghgt, PTSTAR, PT0, ZTSTAR
    REAL                                                 :: ZALPHA, POROG
    REAL                                                 :: ZDTDZSG, ZOROG, ZT0, ZTX, ZTY, ZX, ZY, ZY2
    REAL, PARAMETER                                      :: RDTDZ1 = -gammav

!!!!!!! Variables
! PRPRESS: Surface pressure [Pa]
! hgt: Terrain height [m]
! PTB: Temperature first half-level [K]
! PRESBH: Pressure first half-level [Pa]
! PRESBF: Pressure second full-level [Pa]
! psl: sea-level pressure

    sfname = 'var_psl_ecmwf'
    
    ! Height by gravity
    POROG = hgt*g

    !* 1. COMPUTES SURFACE TEMPERATURE
    !*   THEN STANDARD SURFACE TEMPERATURE.

    ZDTDZSG=-RDTDZ1/g
    ZALPHA=ZDTDZSG*r_d

    PTSTAR=PTB*(1.0+ZALPHA*(PRESBH/PRESBF-1.0))
    PT0=PTSTAR+ZDTDZSG*POROG

    !* 2.    POST-PROCESS MSL PRESSURE.
    !  --------------------------

    !* 2.1   COMPUTATION OF MODIFIED ALPHA AND TSTAR.

    ZTX=290.5
    ZTY=255.0

    IF (PTSTAR < ZTY) THEN
      ZTSTAR=0.5*(ZTY+PTSTAR)
    ELSEIF (PTSTAR < ZTX) THEN
      ZTSTAR=PTSTAR
    ELSE
      ZTSTAR=0.5*(ZTX+PTSTAR)
    ENDIF

    ZT0=ZTSTAR+ZDTDZSG*POROG
    IF (ZTX > ZTSTAR .AND. ZT0 > ZTX) THEN
      ZT0=ZTX
    ELSEIF (ZTX <= ZTSTAR .AND. ZT0 > ZTSTAR) THEN
      ZT0=ZTSTAR
    ELSE
      ZT0=PT0
    ENDIF

    ZOROG=SIGN(MAX(1.0,ABS(POROG)),POROG)
    ZALPHA=r_d*(ZT0-ZTSTAR)/ZOROG

    !* 2.2   COMPUTATION OF MSL PRESSURE.

    IF (ABS(POROG) >= 0.001) THEN
      ZX=POROG/(r_d*ZTSTAR)
      ZY=ZALPHA*ZX
      ZY2=ZY*ZY

      psl=PRPRESS*EXP(ZX*(1.0-0.5*ZY+1.0/3.*ZY2))
    ELSE
      psl=PRPRESS
    ENDIF

    RETURN

  END SUBROUTINE var_psl_ecmwf

  SUBROUTINE var_psl_ptarget(press, ps, hgt, ta, qv, dz, ptarget, psl)
    ! Subroutine to compute sea level pressure using a target pressure. Similar to the Benjamin 
    !   and Miller (1990). Method found in p_interp.F90

     INTEGER, INTENT(in)                                           :: dz
     REAL, DIMENSION(dz), INTENT(in)                               :: press, ta, qv
     REAL, INTENT(in)                                              :: ps, hgt, ptarget
     REAL, INTENT(out)                                             :: psl

! Local
     INTEGER                                                       :: kin
     INTEGER                                                       :: kupper
     REAL                                                          :: dpmin, dp, tbotextrap,   &
       tvbotextrap, virtual
     ! Exponential related to standard atmosphere lapse rate r_d*gammav/g
     REAL, PARAMETER                                               :: expon=r_d*gammav/g

!!!!!!! Variables
! press: Atmospheric pressure [Pa]
! ps: surface pressure [Pa]
! hgt: surface height
! ta: temperature [K]
! qv: water vapor mixing ratio
! dz: number of vertical levels
! psl: sea-level pressure

     sfname = 'var_psl_ptarget'

     IF (hgt /= 0.) THEN

       ! target pressure to be used for the extrapolation [Pa] (defined in namelist.input)
       !   ptarget = 70000. default value

       ! Minimal distance between pressures [Pa]
       dpmin=1.e6

       psl=0.

!      We are below both the ground and the lowest data level.

!      First, find the model level that is closest to a "target" pressure
!        level, where the "target" pressure is delta-p less that the local
!        value of a horizontally smoothed surface pressure field.  We use
!        delta-p = 150 hPa here. A standard lapse rate temperature profile
!        passing through the temperature at this model level will be used
!        to define the temperature profile below ground.  This is similar
!        to the Benjamin and Miller (1990) method, using  
!        700 hPa everywhere for the "target" pressure.

      kupper = 0
      loop_kIN: DO kin=dz,1,-1
        kupper = kin
        dp=abs( press(kin) - ptarget )
        IF (dp .GT. dpmin) EXIT loop_kIN
        dpmin=min(dpmin,dp)
      ENDDO loop_kIN

      tbotextrap=ta(kupper)*(ps/ptarget)**expon
      ! L. Fita. iCIMA, Janury 2018
      ! Using AFWA's one
      tvbotextrap=virtualTemperature(tbotextrap,qv(kupper))
      IF (tvbotextrap == 0.) THEN
        WRITE(message,*)'  ' // TRIM(sfname) //': wrong virtual temperature:', tvbotextrap, '!!'
        CALL wrf_debug(0,message)
        WRITE(message,*)'press:', press(kupper), ' ps:', ps, ' hgt:', hgt, ' ta:', ta(kupper), ' qv:',&
          ' ptarget:', ptarget, ' kupper:', kupper, ' dz:', dz 
        CALL wrf_debug(0,message)
        Lmessage = ''
        DO kin=1,dz-1
          WRITE(RS,'(F10.3)')press(kin) - ptarget
          WRITE(IS,'(I3)')kin
          WRITE(Lmessage,*) TRIM(Lmessage) //  TRIM(IS) // ' :' // TRIM(RS) // ', '
        END DO
        WRITE(RS,'(F10.3)')press(dz)
        WRITE(IS,'(I3)')dz
        WRITE(Lmessage,*) TRIM(Lmessage) //  TRIM(IS) // ' :' // TRIM(RS)
        CALL wrf_debug(0,Lmessage)
      END IF

      psl = ps*((tvbotextrap+gammav*hgt)/tvbotextrap)**(1/expon)
      IF (psl -1 == psl) THEN
        WRITE(message,*)'  ' // TRIM(sfname) //': wrong psl:', psl, '!!'
        CALL wrf_debug(0,message)
        WRITE(message,*)'press:', press(kupper), ' ps:', ps, ' hgt:', hgt, ' ta:', ta(kupper), ' qv:',&
          ' ptarget:', ptarget, ' kupper:', kupper, ' dz:', dz 
        CALL wrf_debug(0,message)
        WRITE(message,*)'expon', expon, ' tvbotextrap:', tvbotextrap, ' gammav:', gammav
        CALL wrf_debug(0,message)
      END IF
    ELSE
      psl = ps
    END IF

    RETURN

  END SUBROUTINE var_psl_ptarget

  SUBROUTINE var_psl_shuell(hgt, ps, zlev1, qlev1, tlev1, psl)
    ! Subroutine to compute sea level pressure using the hydrostatic equation with the Shuell 
    !   correction and existing code from 'phys/module_diag_afwa.F'

    IMPLICIT NONE

    REAL, INTENT(in)                                     :: hgt, ps, zlev1, qlev1, tlev1
    REAL, INTENT(out)                                    :: psl

! Local
    REAL                                                 :: tauvrt, tauvr_sfc, tau_sfc, tau_sl,       &
      tauvr_sl, tauavg, zsfc

    ! Inverse of gravity
    REAL, PARAMETER                                      :: gi=1./g
    ! Sea-level height
    REAL, PARAMETER                                      :: zsl=0.0
    ! control Tau
    REAL, PARAMETER                                      :: taucr=r_d*gi*290.66
    ! Constant
    REAL, PARAMETER                                      :: const=0.005*g/r_d


!!!!!!! Variables
! hgt: surface height [m]
! ps: surgface pressure [Pa]
! zlev1: height first level [m]
! qlev1: mixing ratio at first level [kgkg-1]
! tlev1: temperature at first level [K]
! psl: sea level pressure [Pa]

    sfname = 'var_psl_shuell'

    psl = ps
    zsfc = hgt

    IF (hgt /= 0.) THEN

!     Compute layer tau (virtual temp*rd/g)
      tauvrt = tlev1*(1.0+0.608*qlev1)

!     Compute tau at the ground (z=zsfc) and sea level (z=0) assuming a constant lapse rate of 
!       gammav=6.5deg/km
      tauvr_sfc = tauvrt + (zlev1 - zsfc)*gammav
      tau_sfc = tauvr_sfc*r_d*gi
      tauvr_sl  = tauvrt + (zlev1 - zsl)*gammav
      tau_sl  = tauvr_sl*r_d*gi
    
!     if need be apply Sheull correction
      IF ((tau_sl.GT.taucr) .AND. (tau_sfc.LE.taucr)) THEN
        tau_sl = taucr
      ELSEIF ((tau_sl.GT.taucr) .AND. (tau_sfc.GT.taucr)) THEN
        tau_sl = taucr-const*(tau_sfc-taucr)**2
      ENDIF

!     compute mean tau
      tauavg = 0.5*(tau_sl+tau_sfc)
   
!     compute sea level pressure
      psl = ps*EXP(hgt/tauavg)
    END IF

    RETURN

  END SUBROUTINE var_psl_shuell

  SUBROUTINE var_press(p, pb, dz, press)
! Subroutine to compute air pressure [Pa]

    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: dz
    REAL, DIMENSION(dz), INTENT(in)                      :: p, pb
    REAL, DIMENSION(dz), INTENT(out)                     :: press

!!!!!!! Variables
! p: WRF air-pressure perturbation [Pa]
! pb: WRF air-pressure base-state [Pa]
! dz: vertical extension (half-levels)
! press: air pressure (half-levels) [Pa]

    sfname = 'var_press'

    press = p + pb

    RETURN

  END SUBROUTINE var_press

  SUBROUTINE var_ta(tpot, press, dz, ta)
! Subroutine to compute air temperature [K]

    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: dz
    REAL, DIMENSION(dz), INTENT(in)                      :: tpot, press
    REAL, DIMENSION(dz), INTENT(out)                     :: ta

!!!!!!! Variables
! tpot: WRF potential air-temperature [K]
! press: air-pressure [Pa]
! dz: vertical extension (half-levels)
! ta: air temperature [K]

    sfname = 'var_ta'

    ta = (tpot + 300.)*(press/p0)**(rcp)

    RETURN

  END SUBROUTINE var_ta

  SUBROUTINE var_uava(u, v, sina, cosa, dz, ua, va)
! Subroutine to compute earth-rotated wind components [ms-1]

    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: dz
    REAL, DIMENSION(dz), INTENT(in)                      :: u, v
    REAL, INTENT(in)                                     :: sina, cosa
    REAL, DIMENSION(dz), INTENT(out)                     :: ua, va

! Local
    INTEGER                                              :: k

!!!!!!! Variables
! u, v: WRF unstaggered wind components [ms-1]
! sina, cosa: local sine and cosine of map rotation [1]
! dz: vertical extension
! ua, va: earth-rotated wind speed components [ms-1]
    sfname = 'var_uava'

    DO k=1, dz
      ua(k) = u(k)*cosa - v(k)*sina
      va(k) = u(k)*sina + v(k)*cosa
    END DO

    RETURN

  END SUBROUTINE var_uava

  SUBROUTINE var_uasvas(u10, v10, sina, cosa, uas, vas)
! Subroutine to compute 10m earth-rotated wind components [ms-1]

    IMPLICIT NONE

    REAL, INTENT(in)                                     :: u10, v10, sina, cosa
    REAL, INTENT(out)                                    :: uas, vas

!!!!!!! Variables
! u10, v10: WRF 10m wind components [ms-1]
! sina, cosa: local sine and cosine of map rotation [1]
! uas, vas: earth-rotated 2m wind speed components [ms-1]

    sfname = 'var_uasvas'

    uas = u10*cosa - v10*sina
    vas = u10*sina + v10*cosa

    RETURN

  END SUBROUTINE var_uasvas

  SUBROUTINE var_zg(ph, phb, dz1, dz, zg, unzg)
! Subroutine to compute geopotential height [m]

    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: dz, dz1
    REAL, DIMENSION(dz1), INTENT(in)                     :: ph, phb
    REAL, DIMENSION(dz1), INTENT(out)                    :: zg
    REAL, DIMENSION(dz), INTENT(out)                     :: unzg

! Local
    INTEGER                                              :: k

!!!!!!! Variables
! ph: WRF geopotential perturbation [m2s-2]
! phb: WRF geopotential base-state [m2s-2]
! dz1: vertical extension (full-levels)
! dz: vertical extension (half-levels)
! zg: geopotential height (full-levels) [m]
! unzg: geopotential height (half-levels) [m]

    sfname = 'var_zg'

    zg = (ph + phb) / g

    unzg(1:dz) = 0.5*(zg(1:dz) + zg(2:dz1))

    RETURN

  END SUBROUTINE var_zg

!!!!!!! Some more complex variables

  SUBROUTINE water_budget(dz, Nmoist, mutot, deta, dt, dtqvar, qvarhadv, qvarzadv, pwqvar, fqvar, zqvar)
! Subroutine to compute accumulated water budget
!! Code from: Fita and Flaounas, QJRMS, 2018
!! After Jian et al., 2008, Wea. Forecasting, 23, 44-61

    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: dz, Nmoist
    REAL, INTENT(in)                                     :: mutot
    REAL, DIMENSION(dz), INTENT(in)                      :: deta
    REAL, INTENT(in)                                     :: dt
    REAL, DIMENSION(dz,Nmoist), INTENT(in)               :: dtqvar, qvarhadv, qvarzadv
    REAL, DIMENSION(dz,Nmoist), INTENT(out)              :: pwqvar, fqvar, zqvar

! Local
    INTEGER                                              :: k, l
    REAL                                                 :: wrfhadvqvar, wrfzadvqvar
    
!!!!!!! Variables
! dz: vertical extent
! Nmoist: number of water species
! mutot: total dry-air pressure [Pa]
! deta: thickness of eta layers
! dt: time-step [s]
! dtqvar: tendenchy of all water species [kgkg-1s-1]
! qvarhadv: horizontal advection of all water species [kgkg-1s-1]
! qvarzadv: vertical advection of all water species [kgkg-1s-1]
! pwqvar: total tendency of all water species [mm]
! fqvar: horizontal advecton of all water species [mm]
! zqvar: vertical advecton of all water species [mm]

    sfname = 'water_budget'

    ! Initializing variables
    pwqvar = 0.
    fqvar = 0.
    zqvar = 0.

    ! Looping on the vertical
    vertlevels: DO k=1,dz

      ! Looping on species
      DO l=1, Nmoist

        ! total tendency
        pwqvar(k,l) = (1./g)*mutot*deta(k)*dtqvar(k,l)*dt

        ! horizontal advection
        fqvar(k,l) = deta(k)*qvarhadv(k,l)*mutot*dt/g   
        ! vertical advection
        ! original acz =  wrfzadvqv(i,k,j)/dt
        zqvar(k,l) = deta(k)*qvarzadv(k,l)*mutot*dt/g

      END DO

    END DO vertlevels

  END SUBROUTINE water_budget

  SUBROUTINE gustwind_afwa(dz, dt, u10, v10, pr, zagl, u_phy, v_phy, sa, ca, ublend, vblend, gustij)
! Subroutine to compute wind gust following heavy precip methodology found in phys/module_diag_afwa.F diagnostics

    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: dz
    REAL, INTENT(in)                                     :: dt, u10, v10, pr, sa, ca
    REAL, DIMENSION(dz), INTENT(in)                      :: zagl, u_phy, v_phy
    REAL, INTENT(out)                                    :: ublend, vblend
    INTEGER, INTENT(out)                                 :: gustij

! Local
    INTEGER                                              :: k
    REAL                                                 :: wind_vel, prate_mm_per_hr
    LOGICAL                                              :: is_target_level
    REAL                                                 :: factor, u1km, v1km
    REAL                                                 :: ublend0, vblend0

!!!!!!! Variables
! dz: number of vertical levels
! dt: time-step [s]
! u10, v10: 10-m horizontal wind components [ms-1]
! pr: time-step precipitation [kgm-2]
! zagl: height above ground [m]
! u_phy, v_phy: air wind speed [ms-1]
! sa, ca: : Map factors sinus and cosionus
! ublend, vblend: blended winds at 1 km AGL to be considered as wind gust components [ms-1]
! gustij: whether the grid point got gust winds (1) or not (0)

    sfname = 'gustwind_afwa'

    ! Calculate the max 10 m wind speed between output times
    ! ------------------------------------------------------
    ! UPDATE 20150112 - GAC
    ! Diagnose from model 10 m winds, and blend with 1 km AGL
    ! winds when precipitation rate is > 50 mm/hr to account
    ! for increased surface wind gust potential when precip
    ! is heavy and when winds aloft are strong.  Will use the
    ! higher of the surface and the blended winds. Blending
    ! is linear weighted between 50-150 mm/hr precip rates.
    ! -------------------------------------------------------
    ublend = 0.
    vblend = 0.

    wind_vel = SQRT(u10*u10 + v10*v10)
    prate_mm_per_hr = (pr/dt)*3600.

    ! Is this an area of heavy precip?  Calculate 1km winds to blend down
    ! -------------------------------------------------------------------
    IF ( prate_mm_per_hr .GT. 50. ) THEN
      gustij = 1
      is_target_level = .FALSE.
      DO k=1,dz
        IF ( (zagl(k) >= 1000.) .AND. (.NOT.is_target_level) .AND. (k .ne. 1)) THEN
          is_target_level = .TRUE.
          u1km = u_phy(k-1) + (1000.-zagl(k-1))*((u_phy(k)-u_phy(k-1))/(zagl(k)))
          v1km = v_phy(k-1) + (1000.-zagl(k-1))*((v_phy(k)-v_phy(k-1))/(zagl(k)))
          EXIT ! We've found our level, break the loop
        ENDIF
      ENDDO
          
      ! Compute blended wind
      ! --------------------
      factor = MAX( ((150.-prate_mm_per_hr)/100.), 0.)
      ublend0 = u10*factor + u1km*(1.-factor)
      vblend0 = v10*factor + v1km*(1.-factor)

      ! Rotating winds
      ublend = ublend0*ca-vblend0*sa
      vblend = vblend0*ca+ublend0*sa
    ELSE
      gustij = 0
      ublend = 0.
      vblend = 0.
    ENDIF

    RETURN

  END SUBROUTINE gustwind_afwa

! From clWRF heavy
  SUBROUTINE gustwind_Brasseur01(dz, geopoth, qr, lr, pt, tke, u, v, hpbl, topo, sina, cosa,          &
    ugustwind, vgustwind, gustij) 
! Subroutine to compute 1D wind gust following Brasseur (2001), MWR. 
! Downdraft effect from convection has not been applied

    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: dz
    REAL, DIMENSION(dz), INTENT(in)                      :: geopoth, qr, lr, pt, tke, u, v
    REAL, INTENT(in)                                     :: hpbl, topo, sina, cosa
    REAL, INTENT(out)                                    :: ugustwind, vgustwind
    INTEGER, INTENT(out)                                 :: gustij
! Not needed for climate runs
!    REAL, DIMENSION, INTENT(out)                         :: ugustlow, vgustlow, ugustup, vgustup
  
! Local
    INTEGER                                              :: i,j,k,l, Nlevs, km2, ilev
    INTEGER                                              :: Ngustlevels, Ngustlowlev, maxlev
    REAL, DIMENSION(dz)                                  :: vpt, heights, dheights, dvpt
    REAL, DIMENSION(dz,2)                                :: wge !, wge_low, wge_up
    INTEGER, DIMENSION(dz)                               :: gustlevels
    REAL                                                 :: preshpbl, tke_int, vpt_int
    REAL                                                 :: height2pres, virtualpottemp, pres2height
    REAL                                                 :: wgemax !, wgemaxlow, wgemaxup
    REAL                                                 :: ugust, vgust
!    REAL                                                 :: ugustl, vgustl, ugustu, vgustu
    CHARACTER(LEN=1024)                                  :: outstring

!!!!!!!!!! Variables
! geopoth: 3D geopotential height
! qr: 3D vapour water mixing ratio
! lr: 3D liquid water mixing ratio
! pt: 3D potential temperature
! tke: 3D turbulent kinetic energy
! u, v: 3D wind components
! hpbl: pbl height
! topo: topography
! sina, cosa: Map factors sinus and cosionus
! ugustwind, vgustwind: gust wind components
! ugustlow, vgustlow: gust wind components of lower bounder
! ugustup, vgustup: gust wind components of upper bounder
! Nlevs: number of layers within pbl
! Ngustlevels: number of pbl layers with a int_tke >= int_ept
! Ngustlowlev: number of pbl layers with a 2.5*tke(lay)/11. >= int_ept
! vpt: virtual potential temperature
! heights: vector of heights according hydrostatic conversion from pressure
! dheights: vector of differences between heights
! wge: estimated wind gusts 
! wge_low: low doundaries of estimated wind gust
! wge_up: up boundary of estimated wind gust
! gustlevels: levels of pbl with a possible deflected air parcel
! preshpbl: hydrostatic equivalent pbl height pressure
! tke_int: tke integral up to a given layer
! vpt_int: vpt integral up to a given layer
! wgemax: maximum wge
! wgemaxlow: lower wge boundary
! wgemaxup: upper wge boundary
! zero: zero values
! gustij: whether the grid point got gust winds (1) or not (0)

!!!!!!!!!! Functions
! virtualpottemp: calculates virtual potential temperature

    sfname = 'gustwind_Brasseur01'

    km2=INT(dz/2)

    ugustwind=0.
    vgustwind=0.
!! Not needed on climate runs
!    ugustlow=0.
!    vgustlow=0.
!    ugustup=0.
!    vgustup=0.

    maxlev=0
    heights=0.
    dheights=0.
    vpt=0.
    wge=0.
    !wge_low=0.
    !wge_up=0.

    k=1  
    IF ((geopoth(k+1)/g-topo) <= hpbl .AND. (k <= dz)) THEN
      !!outstring  = '  ' // TRIM(sfname) // ' _______'
      !!CALL wrf_debug(750, outstring)
      !!WRITE(outstring,"(4x,A4,1x,11(A10,1x))")'k','geoph','topo','geo_h','hpbl','pt', 'qr','lr',      &
      !!  'vpt', 'u', 'v', 'h'
      CALL wrf_debug(750, outstring)
    END IF
!!       'DO WHILE' does not work
!!        DO WHILE (( (geopoth(k+1)/g-topo) <= hpbl) .AND. (k+1 <= dz) )
    DO k=1,dz-1
      IF ( (geopoth(k+1)/g-topo) > hpbl ) THEN
        EXIT
      ELSE
!! Function does not work. Let's make it with subroutines
!!       vpt(k)=virtualpottemp(pt(k), qr(k), lr(k))
        CALL VirtPotTemp(pt(k), qr(k), lr(k), vpt(k))
        heights(k)=geopoth(k)/g-topo

        !!WRITE(outstring,*)'    ',k, geopoth(k), topo, geopoth(k)/g-topo, hpbl, pt(k), qr(k), lr(k),   &
        !!  vpt(k), u(k), v(k), heights(k)
        !!CALL wrf_debug(750, outstring)
      END IF 
    END DO
    Nlevs=k-1
!
!! Computation of wind gusts only if more than 1 levels behind 'hpbl' are found
      
    more1lev: IF (Nlevs > 1) THEN

      !wge_up(1:Nlevs,1)=u(1:Nlevs)
      !wge_up(1:Nlevs,2)=v(1:Nlevs)

      !!WRITE(outstring,"(3x,A14,1x)")'Integrating...'
      !!CALL wrf_debug ( 750, TRIM(outstring) )

      dheights(1:Nlevs-1)=heights(2:Nlevs)-heights(1:Nlevs-1)
        
      Ngustlevels=0
      Ngustlowlev=0
      !!WRITE(outstring,"(9(A10,1x))")'ilev','vpt','dvpt','h','dh','tke','tkeInt','dvptInt',  '25tke11'
      !!CALL wrf_debug(750, outstring)

      dvpt=0.
      dvpt(1)=g*(vpt(2)-vpt(1))/vpt(1)
      dvpt(2:Nlevs-1)=g*(vpt(3:Nlevs)-vpt(1:Nlevs-2))/vpt(2:Nlevs-1)
      dvpt(Nlevs)=g*(vpt(Nlevs)-vpt(Nlevs-1))/vpt(Nlevs)
      ilev=1
      !!WRITE(outstring,*)ilev, vpt(ilev), dvpt(ilev), heights(ilev), dheights(ilev), tke(ilev), 0.0,   &
      !!      0.0, 2.5*tke(ilev)/11.
      !!CALL wrf_debug(750, outstring)

      DO ilev=2,Nlevs
        tke_int = 0.
        vpt_int = 0.

! Trapezoidal integrations
!        CALL NumIntegration(tke,heights,ilev,Nlevs,tke_int)
!        CALL NumIntegration(dvpt,heights,ilev,Nlevs,vpt_int)
! Standard integrations
        DO k=1,ilev
          tke_int = tke_int + tke(k)*dheights(k)
          vpt_int = vpt_int + dvpt(k)
        END DO

        tke_int=tke_int/heights(ilev)
        !!WRITE(outstring,*)ilev, vpt(ilev), dvpt(ilev), heights(ilev),  dheights(ilev), tke(ilev),     &
        !!  tke_int, vpt_int, 2.5*tke(ilev)/11.
        !!CALL wrf_debug(750, outstring)

        IF (tke_int >= vpt_int) THEN
          Ngustlevels=Ngustlevels+1
          gustlevels(Ngustlevels)=ilev
          wge(Ngustlevels,1)=u(ilev)
          wge(Ngustlevels,2)=v(ilev)
        ELSE
          ! End of deflection
          EXIT
        END IF

!        IF (2.5*tke(ilev)/11. >= vpt_int) THEN
!          Ngustlowlev=Ngustlowlev+1
!          wge_low(Ngustlowlev,1)=u(ilev)
!          wge_low(Ngustlowlev,2)=v(ilev)
!        END IF
      END DO

      !!WRITE(outstring,*)'  ' // TRIM(sfname) // ': Wind gust estimation...'
      !!CALL wrf_debug( 750, outstring)

! gustwind
!!
      IF (Ngustlevels > 0 ) THEN
        gustij = 1
        wgemax=MAXVAL(SQRT(wge(:,1)*wge(:,1)+wge(:,2)*wge(:,2)))
        DO ilev=1,Ngustlevels
          IF (ABS(wgemax - SQRT(wge(ilev,1)*wge(ilev,1)+wge(ilev,2)*wge(ilev,2))) < nullv) THEN
            maxlev=ilev
            EXIT
          END IF
        END DO
        ugust=wge(maxlev,1)
        vgust=wge(maxlev,2)
      ELSE
        gustij = 0
        ugust=0.
        vgust=0.
      ENDIF

      !!WRITE(outstring,"(5x,A14,1x,I3,1x,3(A10,1x))")'N gust levels:',Ngustlevels,'gust lev', 'u wge', &
      !!  'v wge'
      !!CALL wrf_debug (750, outstring)
      !!DO l=1,Ngustlevels
        !!WRITE(outstring,"(22x,I10,1x,2(f10.4,1x))")gustlevels(l),wge(l,1:2)
        !!CALL wrf_debug(750,outstring)
      !!END DO

! gustwind lower bound
!!
!      IF (Ngustlowlev > 0) THEN
!        wgemaxlow=MAXVAL(SQRT(wge_low(:,1)*wge_low(:,1)+wge_low(:,2)*wge_low(:,2)))
!        DO ilev=1,Ngustlowlev
!          IF (ABS(wgemaxlow - SQRT(wge_low(ilev,1)*wge_low(ilev,1)+wge_low(ilev,2)*wge_low(ilev,2)))  &
!            < zero) THEN
!            maxlev=ilev
!            EXIT
!          END IF
!        END DO
!        ugustl=wge_low(maxlev,1)
!        vgustl=wge_low(maxlev,2)          
!      ELSE
!        ugustl=0.
!        vgustl=0.
!      END IF

! gustwind upperbound
!!
!      IF (Nlevs > 0) THEN
!        wgemaxup=MAXVAL(SQRT(wge_up(:,1)*wge_up(:,1)+wge_up(:,2)*wge_up(:,2)))
!        DO ilev=1,Nlevs
!          IF (ABS(wgemaxup - SQRT(wge_up(ilev,1)*wge_up(ilev,1)+wge_up(ilev,2)*wge_up(ilev,2))) <     &
!            zero ) THEN
!            maxlev=ilev
!            EXIT
!          END IF
!        END DO
!        ugustu=wge_up(maxlev,1)
!        vgustu=wge_up(maxlev,2)
!      ELSE
!        ugustu=0.
!        vgustu=0.
!      END IF
    ELSE
      gustij = 0
      ugust=0.
      vgust=0.
!      ugustl=0.
!      vgustl=0.
!      ugustu=0.
!      vgustu=0.
    END IF more1lev

    ! Rotation of winds to Earth surface
#if (EM_CORE == 1)
    ugustwind = ugust*cosa-vgust*sina
    vgustwind = vgust*cosa+ugust*sina
!    ugustlow = ugustl*cosa-vgustl*sina
!    vgustlow = vgustl*cosa+ugustl*sina
!    ugustup = ugustu*cosa-vgustu*sina
!    vgustup = vgustu*cosa+ugustu*sina
#else
    ugustwind = ugust
    vgustwind = vgust
!    ugustlow = ugustl
!    vgustlow = vgustl
!    ugustup = ugustu
!    vgustup = vgustu
#endif

    ! Some debugging printing
    !!outstring = '    ' // TRIM(sfname) // ' _______'
    !!CALL wrf_debug(750, outstring)
    !!WRITE(outstring,"(5x,6(A12,1x))")'name','u wind','v wind', 'rot. u wind', 'rot. v wind','wind'
    !!CALL wrf_debug(750, outstring)
    !!WRITE(outstring,"(11x,A12,1x,5(f12.5,1x))")'Gust wind', ugust, vgust, ugustwind, vgustwind,       &
    !!  sqrt(ugustwind*ugustwind+vgustwind*vgustwind)
!    CALL wrf_debug(750, outstring)
!    WRITE(outstring,"(11x,A12,1x,5(f12.5,1x))")'Low bound', ugustl, vgustl, ugustlow, vgustlow,       &
!      sqrt(ugustlow*ugustlow+vgustlow*vgustlow)
!    CALL wrf_debug(750, outstring)
!    WRITE(outstring,"(11x,A12,1x,5(f12.5,1x))")'Upper bound', ugustu, vgustu, ugustup, vgustup,       &
!      sqrt(ugustup*ugustup+vgustup*vgustup)
!    CALL wrf_debug(750, outstring)

    RETURN

  END SUBROUTINE gustwind_Brasseur01

  REAL FUNCTION virtualpottemp(pt_vpt, r_vpt, l_vpt)
! Function to compute virtual potential temperature

    IMPLICIT NONE

    REAL, INTENT(in)                                     :: pt_vpt, r_vpt, l_vpt 
! Local
    REAL, PARAMETER                                      :: vptA=0.61

    virtualpottemp=pt_vpt*(1+vptA*r_vpt-l_vpt)

    RETURN

  END FUNCTION virtualpottemp

  SUBROUTINE VirtPotTemp(pt_vpt, r_vpt, l_vpt, vpt)
! Subroutine to compute virtual potential temperature

    IMPLICIT NONE

    REAL, INTENT(in)                                     :: pt_vpt, r_vpt, l_vpt 
    REAL, INTENT(out)                                    :: vpt
! Local
    REAL, PARAMETER                                      :: vptA=0.61

    vpt=pt_vpt*(1.+vptA*r_vpt-l_vpt)

    RETURN

  END SUBROUTINE VirtPotTemp

  SUBROUTINE var_zmla_generic(dz, qv, tpot, z, topo, dqvar, refdt, zmla)
!  Subroutine to compute pbl-height following a generic method
!    from Nielsen-Gammon et al., 2008 J. Appl. Meteor. Clim.
!    applied also in Garcia-Diez et al., 2013, QJRMS
!   where 
!     "The technique identifies the ML height as a threshold increase of
!     potential temperature from 
!       its minimum value within the boundary layer."
!   here applied similarly to Garcia-Diez et al. where 
!      zmla = "...first level where potential temperature exceeds the minimum
!      potential temperature
!        reached in the mixed layer by more than 1.5 K"

    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: dz
    REAL, DIMENSION(dz), INTENT(in)                      :: qv, tpot, z
    REAL, INTENT(in)                                     :: topo, dqvar, refdt 
    REAL, INTENT(out)                                    :: zmla

! Local
    INTEGER                                              :: mldlev, bllev
    REAL                                                 :: tpotmin

!!!!!!! Variables
! qv: water vapour mixing ratio
! tpot: potential temperature [K]
! z: height above sea level [m]
! topo: topographic height [m]
! dqvar: Pecentage of difference of mixing ratio used to determine Mixed layer depth
! refdt: Change [K] in temperature to determine boundary layer height
! zmla: boundary layer height [m]

    sfname = 'var_zmla_generic'

    ! MLD = Mixed layer with no substantial variation of mixing ratio /\qv < 10%
    ! ?
    !PRINT *,'  Mixed layer mixing ratios qv[1] lev qv[lev] dqvar% _______'
    DO mldlev = 2, dz
      IF (ABS(qv(mldlev)-qv(1))/qv(1) > dqvar ) EXIT
    !  PRINT *,qv(1), mldlev, qv(mldlev), ABS(qv(mldlev)-qv(1))/qv(1)
    END DO

    ! Looking for the minimum potential temperature within the MLD [tpotmin =
    ! min(tpot)_0^MLD]
    tpotmin = MINVAL(tpot(1:mldlev))

    ! Determine the first level where tpot > tpotmin + 1.5 K
    !PRINT *,'  Mixed layer tpotmin lev tpotmin[lev] dtpot _______'
    DO bllev = 1, dz
      IF (ABS(tpot(bllev)-tpotmin) > refdt ) EXIT
    !  PRINT *,tpotmin, bllev, tpot(bllev), ABS(tpot(bllev)-tpotmin)
    END DO
    
    !PRINT *,'   height end MLD:', z(mldlev)
    !PRINT *,'   pbl height:', z(bllev)

    zmla = z(bllev) - topo

    RETURN

  END SUBROUTINE var_zmla_generic
  
  SUBROUTINE var_bulk_Ri(dz, pt, qv, qc, qs, z, ua, va, ta, ust, ri)
  ! Subroutine to compute the bulk Richardson number at all the space
  !      after: 
  !        https://glossary.ametsoc.org/wiki/Bulk_richardson_number
  !        Vogelezang and Holtslag, 1996, Bound.-Lay. Meteorol., 81, 245–269, 
  !          doi:10.1007/BF02430331
  
    IMPLICIT NONE
    
    INTEGER, INTENT(in)                                  :: dz
    REAL, DIMENSION(dz), INTENT(in)                      :: pt, qv, qc, qs, z, ua, va, ta
    REAL, INTENT(in)                                     :: ust
    REAL, DIMENSION(dz), INTENT(out)                     :: ri
    
    ! Local
    INTEGER                                              :: iz
    REAL                                                 :: tav, gtav, ri1, ri2, b, tahvb, tahvt
    CHARACTER(len=50)                                    :: fname

!!!!!!! Variables
! pt: potential temperature (K)
! qv: water vapour mixing ratio (kgkg-1)
! qc: cloud mixing ratio (kgkg-1)
! qs: solid species (qs+qi+qg+qh) mixing ratio (kgkg-1)
! z: height [m]
! ua: eastward air wind speed [ms-1]
! va: northward air wind speed [ms-1]
! ust: air friction velocity from the similarity theory [ms-1]

    fname = 'var_bulk_Ri'

    ri = -99999.

    b = 100.
    CALL VirtPotTemp(pt(1), qv(1), qc(1), tahvb)
    DO iz=1, dz-1
      tav = var_tav(ta(iz), qv(iz), qc(iz), qs(iz))
      gtav = g/tav
      CALL VirtPotTemp(pt(iz+1), qv(iz+1), qc(iz+1), tahvt)
      ri1 = gtav*(tahvt-tahvb)*(z(iz+1)-z(iz))
      ! Note, ust**2 term is included as a way to take into account turbulence due to surface friction 
      !   on neutral boundary layer (eq 3 in Vogelezang and Holstlag,  1996)
      ri2 = (ua(iz+1)-ua(iz))**2+(va(iz+1)-va(iz))**2+b*ust**2
      ! Let's get only positive values ?
      ri(iz) = ABS(ri1/ri2)
      tahvb = tahvt
    END DO

    RETURN

  END SUBROUTINE var_bulk_Ri
  
  SUBROUTINE var_zmla_RIgen(dz, ri, zv, topo, rithres, zmla)
  ! Subroutine to compute the pbl height using bulk Richardson number
  !        Vogelezang and Holtslag, 1996, Bound.-Lay. Meteorol., 81, 245–269, 
  !          doi:10.1007/BF02430331,
  !        Seidel et al., 2012: J. Geophys. Res.-Atmos., 117, 1–15, 
  !          doi:10.1029/2012JD018143
  !
  !       zmla = ri[z] <= rithres < ri[z+1] or ri[z] >= rithres > ri[z+1]
  
    IMPLICIT NONE
    
    INTEGER, INTENT(in)                                  :: dz
    REAL, INTENT(in)                                     :: topo, rithres
    REAL, DIMENSION(dz), INTENT(in)                      :: ri, zv
    REAL, INTENT(out)                                    :: zmla
    
    ! Local
    INTEGER                                              :: iz 
    CHARACTER(len=50)                                    :: fname
    
!!!!!!! Variables
! ri: 3D bulk Richardson number [-]
! zv: heights [m]
! topo: topography [m]
! rithres: threshold used to determine zmla (0.25 Vogelezang and Hollstag, 1996)

    fname = 'var_zmla_RIgen'

    zmla = -9.

    ! Looking for the height
    DO iz=1, dz-1
      IF ( (ri(iz)<=rithres.AND.ri(iz+1)>rithres) .OR. (ri(iz+1)<=rithres.AND.ri(iz)>rithres) ) THEN
        zmla = zv(iz) - topo
        EXIT
      END IF
    END DO

    RETURN
    
  END SUBROUTINE var_zmla_RIgen

  SUBROUTINE var_zwind(d1, u, v, z, u10, v10, sa, ca, newz, unewz, vnewz)
! Subroutine to extrapolate the wind at a given height following the 'power law' methodology
!    wss[newz] = wss[z1]*(newz/z1)**alpha
!    alpha = (ln(wss[z2])-ln(wss[z1]))/(ln(z2)-ln(z1))
! AFTER: Phd Thesis: 
!   Benedicte Jourdier. Ressource eolienne en France metropolitaine : methodes d’evaluation du 
!   potentiel, variabilite et tendances. Climatologie. Ecole Doctorale Polytechnique, 2015. French
!

    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: d1
    REAL, DIMENSION(d1), INTENT(in)                      :: u,v,z
    REAL, INTENT(in)                                     :: u10, v10, sa, ca, newz
    REAL, INTENT(out)                                    :: unewz, vnewz

! Local
    INTEGER                                              :: inear
    REAL                                                 :: zaground
    REAL, DIMENSION(2)                                   :: v1, v2, zz, alpha, uvnewz

!!!!!!! Variables
! u,v: vertical wind components [ms-1]
! z: height above surface [m]
! u10,v10: 10-m wind components [ms-1]
! topo: topographical height [m]
! sa, ca: local sine and cosine of map rotation [1.]
! newz: desired height above grpund of extrapolation
! unewz,vnewz: Wind compoonents at the given height [ms-1]

    sfname = 'var_zwind'

    !!WRITE(message,*)' ilev zaground newz z[ilev+1] z[ilev+2] _______'
    !!CALL wrf_debug(750,message)
    ! Looking for the level  below desired height
    IF (z(1) < newz ) THEN
      DO inear = 1,d1-2
        ! L. Fita, CIMA. Feb. 2018
        !! Choose between extra/inter-polate. Maybe better interpolate?
        ! Here we extrapolate from two closest lower levels
        !zaground = z(inear+2)
        ! Here we interpolate between levels
        zaground = z(inear+1)
        !!WRITE(message,*)inear, z(inear), newz, z(inear+1), z(inear+2)
        !!CALL wrf_debug(750,message)
        IF ( zaground >= newz) EXIT
      END DO
    ELSE
      !!WRITE(message,*)1, z(1), newz, z(2), z(3), ' z(1) > newz'
      !!CALL wrf_debug(750,message)
      inear = d1 - 2
    END IF

    IF (inear == d1-2) THEN
    ! No vertical pair of levels is below newz, using 10m wind as first value
    ! and the first level as the second
       v1(1) = u10
       v1(2) = v10
       v2(1) = u(1)
       v2(2) = v(1)
       zz(1) = 10.
       zz(2) = z(1)
    ELSE
       v1(1) = u(inear)
       v1(2) = v(inear)
       v2(1) = u(inear+1)
       v2(2) = v(inear+1)
       zz(1) = z(inear)
       zz(2) = z(inear+1)
    END IF

    ! Computing for each component
    alpha = (LOG(ABS(v2))-LOG(ABS(v1)))/(LOG(zz(2))-LOG(zz(1)))
    !!WRITE(message,*)' Computing with v1:', v1, ' ms-1 v2:', v2, ' ms-1'
    !!CALL wrf_debug(750,message)
    !!WRITE(message,*)' z1:', zz(1), 'm z2:', zz(2), ' m'
    !!CALL wrf_debug(750,message)
    !!WRITE(message,*)' alhpa u:', alpha(1), ' alpha 2:', alpha(2)
    !!CALL wrf_debug(750,message)

    uvnewz = v1*(newz/zz(1))**alpha
    ! Earth-rotation
    unewz = uvnewz(1)*ca - uvnewz(2)*sa
    vnewz = uvnewz(1)*sa + uvnewz(2)*ca

    !!WRITE(message,*)'  result vz:', uvnewz
    !!CALL wrf_debug(750,message)

    RETURN

  END SUBROUTINE var_zwind

  SUBROUTINE var_zwind_log(d1, u, v, z, u10, v10, sa, ca, newz, unewz, vnewz)
! Subroutine to extrapolate the wind at a given height following the 'logarithmic law' methodology
!    wsz = wss[z2]*(ln(newz)-ln(z0))(ln(z2)-ln(z0))
!    ln(z0) = (ws(z2)*ln(z1)-ws(z1)*ln(z2))/(ws(z2)-ws(z1))
! AFTER: Phd Thesis: 
!   Benedicte Jourdier. Ressource eolienne en France metropolitaine : methodes d’evaluation du 
!   potentiel, variabilite et tendances. Climatologie. Ecole Doctorale
!   Polytechnique, 2015. French
!
    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: d1
    REAL, DIMENSION(d1), INTENT(in)                      :: u,v,z
    REAL, INTENT(in)                                     :: u10, v10, sa, ca, newz
    REAL, INTENT(out)                                    :: unewz, vnewz

! Local
    INTEGER                                              :: inear
    REAL                                                 :: zaground
    REAL, DIMENSION(2)                                   :: v1, v2, zz, logz0, uvnewz

!!!!!!! Variables
! u,v: vertical wind components [ms-1]
! z: height above surface on half-mass levels [m]
! u10,v10: 10-m wind components [ms-1]
! sa, ca: local sine and cosine of map rotation [1.]
! newz: desired height above grpund of extrapolation
! unewz,vnewz: Wind compoonents at the given height [ms-1]

    sfname = 'var_zwind_log'

    IF (z(1) < newz ) THEN
      DO inear = 1,d1-2
        ! L. Fita, CIMA. Feb. 2018
        !! Choose between extra/inter-polate. Maybe better interpolate?
        ! Here we extrapolate from two closest lower levels
        !zaground = z(inear+2)
        zaground = z(inear+1)
        IF ( zaground >= newz) EXIT
      END DO
    ELSE
      inear = d1 - 2
    END IF

    IF (inear == d1-2) THEN
    ! No vertical pair of levels is below newz, using 10m wind as first value
    ! and the first level as the second
       v1(1) = u10
       v1(2) = v10
       v2(1) = u(1)
       v2(2) = v(1)
       zz(1) = 10.
       zz(2) = z(1)
    ELSE
       v1(1) = u(inear)
       v1(2) = v(inear)
       v2(1) = u(inear+1)
       v2(2) = v(inear+1)
       zz(1) = z(inear)
       zz(2) = z(inear+1)
    END IF

    ! Computing for each component
    logz0 = (v2*LOG(zz(1))-v1*LOG(zz(2)))/(v2-v1)

    uvnewz = v2*(LOG(newz)-logz0)/(LOG(zz(2))-logz0)
    ! Earth-rotation
    unewz = uvnewz(1)*ca - uvnewz(2)*sa
    vnewz = uvnewz(1)*sa + uvnewz(2)*ca

    RETURN

  END SUBROUTINE var_zwind_log

  SUBROUTINE univ_func_scal_Businger71(zL, univ_func)
  ! Subroutine of the universal functions for scalars according to Businger 1971
  
    IMPLICIT NONE
    
    REAL, INTENT(in)                                     :: zl
    REAL, INTENT(out)                                    :: univ_func
    
    ! Local
    REAL, PARAMETER                                      :: kappa = 0.35
    CHARACTER(len=50)                                    :: fname
    
!!!!!!! Variables
! zL: height/Obukhov length division [-]

    fname = 'univ_func_scal_Businger71'
    
    IF (zL > -2. .AND. zL < 0.) THEN
      ! Unstable 
      univ_func = 0.74*(1. - 9.*zL)**(-0.5)
    ELSE IF (zL == 0.) THEN
      ! Neutral
      !! From table 2.7 after T. Foken, 2008, 'Micrometeorology', ed. Springer, 
      !   ISBN: 978-3-540-74665-2, e-ISBN: 978-3-540-74666-9
      ! univ_func = 1.
      ! Although in Businger et al. 1971
      univ_func = 0.74
    ELSE IF (zL > 0. .AND. zL < 1.) THEN
      ! Stable (0 < zL < 1)
      univ_func = 0.74 + 4.7*zL
    ELSE
      ! Undefined (zL < -2. .OR. zL > 1.)
      !PRINT *,'  ' // TRIM(fname) // ': undefined regime zL < -2. .OR. zL > 1. !!, zL=', zL
      univ_func = 0.
    END IF
    
    RETURN
  
  END SUBROUTINE univ_func_scal_Businger71

  SUBROUTINE stabfunc_scalar_businger(z, L, stabfunc_busingerv)
  ! Subroutine of the stability function after Businger et al. (1971), JAS, 28(2), 181-189
  !   NOTE: In Businger's article is said in page 185 right column after equation 18 
  !    "... In the atmosphere, the temperature gradient usually vanishes at 100 m or so under
  !    unstable conditions, and there is a tendency for this height to decrease with increasing 
  !    instability... Therefore if Eq. (16) describes the correct height dependence for conditions
  !    approaching and incluing free convection, a new scale has to be introduced to replace u*."
  !  Therefore, ther is an inherent potential missbehaour of this approximation (L. Fita)

    IMPLICIT NONE

    REAL, INTENT(in)                                     :: z,L
    REAL, INTENT(out)                                    :: stabfunc_busingerv

! Local
    REAL                                                 :: zL, X, ufunc
    REAL, PARAMETER                                      :: kappa = 0.35
    CHARACTER(len=50)                                    :: fname

!!!!!!! Variables
! z: height [m]
! L: Obukhov length [-]

    fname = 'stabfunc_scalar_businger'

    IF (L /= 0.) THEN
      zL = z/L
    ELSE
      ! Neutral
      zL = 0.
    END IF
    
    CALL univ_func_scal_Businger71(zL, ufunc)
    !IF (ufunc == 0.) THEN
    !  PRINT *,'    ' // TRIM(fname) // ': Monin-Obukhov length L', L, ' z', z
    !END IF

    IF (zL >= 0.) THEN
      ! Stable case
      stabfunc_busingerv = 1.- ufunc
    ELSE IF (zL < 0.) THEN
      ! unstable
      X = 1./ufunc
      !stabfunc_busingerv = -2.*LOG((1.+X)/2.)-LOG((1.+X**2)/2.)+2.*ATAN(X)-piconst/2.
      stabfunc_busingerv = LOG( ((1.+X)/2.)**2. )
    END IF

    RETURN

  END SUBROUTINE stabfunc_scalar_businger

  SUBROUTINE var_ztaqv_MOtheor(dz, ta, qv, z, ust, znt, rmol, topo, newz, taznew, qvznew)
  ! Subroutine of temperature and water vapour extrapolation following Moin-Obukhov Similarity theory
  !   R. B. Stull, 1988, Springer (p376-383)
  ! NOTE only usefull for newz < 80. m (outside surface layer of the PBL)
  !
  ! Here, the profiles for scalars will be used from Businger 1971.
  ! Model does not provide t*, neither q*, therefore we need to introduce a 4-step methodology to 
  !   extrapolate the amounts (example for ta, the same for qv)
  ! 
  !   1. Compute the increment (e.g. /\ta' = ta(k+1)-ta(k)) of the variable respect a known heights
  !     (at vertical levels k and k+1) from the 3-dimensional atmospheric temperature field
  !   2. Infer ta∗
  !   3. Compute the increment between one of the 3-dimensional temperatures and the desired height 
  !     using ta∗ (e.g. /\ta'' = ta(k)-ta(zd)), to obtain ta0
  !   4. Obtain ta(zd)

    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: dz
    REAL, INTENT(in)                                     :: ust, znt, rmol, topo
    REAL, DIMENSION(dz), INTENT(in)                      :: ta, qv, z
    REAL, INTENT(in)                                     :: newz
    REAL, INTENT(out)                                    :: taznew, qvznew

! Local
    INTEGER                                              :: k, kclosest
    REAL, PARAMETER                                      :: kappa = 0.35
    REAL                                                 :: hgt, hgtp1
    REAL                                                 :: delta_ta, delta_ta2
    REAL                                                 :: delta_qv, delta_qv2
    REAL                                                 :: OL
    REAL                                                 :: stability, stabilityp1, stabilitynewz
    REAL                                                 :: tast, qvst
    REAL                                                 :: wsz, alpha
    REAL                                                 :: minta, minqv, maxta, maxqv
    REAL, DIMENSION(2)                                   :: uvnewz

!!!!!!! Variables
! dz: vertical levels
! ta: 3-dimensional air temperature [K] 
! qv: 3-dimensional water vapour [kgkg-1]
! z: 3-dimensional height [m]
! ust: u* in similarity theory [ms-1]
! z0: roughness length [m]
!!! L. Fita, CIMA. Feb. 2018
!! NOT SURE if it should be z0 instead?
! znt: thermal time-varying roughness length [m]
! rmol: inverse of Obukhov length [m-1]
! topo: topographical height [m]
! 
    sfname = 'var_ztaqv_MOtheor'

    ! 1.- /\ta' = ta(k+1) - ta(k)
    
    k = 1
    hgt = z(k)-topo
    hgtp1 = z(k+1)-topo
    
    ! We can only apply the technique within Monin-Obukhov range, therfore
    IF (hgt*rmol > -2. .AND. hgt*rmol < 1. .AND. hgtp1*rmol > -2. .AND. hgtp1*rmol < 1.) THEN

      delta_ta = ta(k+1)-ta(k)
      delta_qv = qv(k+1)-qv(k)
    
      ! 2.- infer ta*
      !  ta* = kappa*(delta_ta) / (ln(hgtp1/hgt) - Psi_scalar(hgt/OL) +  Psi_scalar(hgtp1/OL))
      !
      !  Psi_scalar: stability functions for scalars, Psi=func(phi(z,OL,stab.)), phi: universal function
      !     (Businger, 19671)
      !   OL: Obukhov length, stab.: stability

      ! Obukhov Length (using the Boussinesq approximation)
      OL = 1./rmol

      ! Stability function
      CALL stabfunc_scalar_businger(hgt, OL, stability)
      CALL stabfunc_scalar_businger(hgtp1, OL, stabilityp1)

      tast = kappa*(delta_ta)/(LOG(hgtp1/hgt) - stability + stabilityp1)
      qvst = kappa*(delta_qv)/(LOG(hgtp1/hgt) - stability + stabilityp1)
    
      ! 3.+4.- /\ta' = ta(k) - ta(kd)
      !   ta(kd) = ta(hgt) - ta*/kappa*(ln(hgt/newz) - Psi_scalar(hgt/OL) + Psi_scalar(newz/OL)) 
    
      CALL stabfunc_scalar_businger(newz, OL, stabilitynewz)

      IF (hgt > newz) THEN
        taznew = ta(k) - tast/kappa*( LOG(hgt/newz) - stabilitynewz + stability)
        qvznew = qv(k) - qvst/kappa*( LOG(hgt/newz) - stabilitynewz + stability)
      ELSE IF (hgt < newz) THEN
        taznew = tast/kappa*( LOG(hgt/newz) - stabilitynewz + stability) + ta(k)
        qvznew = qvst/kappa*( LOG(hgt/newz) - stabilitynewz + stability) + qv(k)
      ELSE
        taznew = ta(k)
        qvznew = qv(k)
      END IF

      ! Checking for consistency, limiting the extrapolated value to the range of values at k, k+1
      minta = MIN(ta(k), ta(k+1))
      minqv = MIN(qv(k), qv(k+1))
      maxta = MAX(ta(k), ta(k+1))
      maxqv = MAX(qv(k), qv(k+1))
      IF (ABS(hgt - newz) < ABS(hgtp1 - newz)) THEN
        kclosest = k
      ELSE
        kclosest = k+1    
      END IF
      IF (taznew < minta) taznew = ta(kclosest)
      IF (qvznew < minqv) qvznew = qv(kclosest)
      IF (taznew > maxta) taznew = ta(kclosest)
      IF (qvznew > maxta) qvznew = qv(kclosest)

    ELSE
      ! Linear interpolation. Something better?
      taznew = interpolate1D(dz, z-topo, ta, newz)
      qvznew = interpolate1D(dz, z-topo, qv, newz)
    END IF
        
    RETURN

  END SUBROUTINE var_ztaqv_MOtheor

  SUBROUTINE var_zwind_MOtheor(dz, ua, va, z, topo, ust, znt, rmol, u10, v10, sa, ca, newz, uznew,    &
    vznew)
  ! Subroutine of wind extrapolation following Moin-Obukhov theory R. B. Stull, 1988, 
  !   Springer (p376-383)
  ! NOTE only usefull for newz < 80. m and within turbelence driven layer
  !     newz/L > -2. & newz/L < 1.

    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: dz
    REAL, INTENT(in)                                     :: topo, ust, znt, rmol, u10, v10, sa, ca
    REAL, INTENT(in)                                     :: newz
    REAL, DIMENSION(dz), INTENT(in)                      :: ua, va, z
    REAL, INTENT(out)                                    :: uznew, vznew

! Local
    REAL                                                 :: OL
    REAL                                                 :: stability
    REAL                                                 :: wsz, alpha
    REAL, DIMENSION(2)                                   :: uvnewz

!!!!!!! Variables
! dz: amount of vertical layers
! ua,va: 3D wind speed [ms-1]
! z: height of the levels [m]
! topo: topography [m]
! ust: u* in similarity theory [ms-1]
! z0: roughness length [m]
!!! L. Fita, CIMA. Feb. 2018
!! NOT SURE if it should be z0 instead?
! znt: thermal time-varying roughness length [m]
! rmol: inverse of Obukhov length [m-1]
! u10: x-component 10-m wind speed [ms-1]
! v10: y-component 10-m wind speed [ms-1]
! sa, ca: local sine and cosine of map rotation [1.]
! 
    sfname = 'var_zwind_MOtheor'

        ! We can only apply the technique within Monin-Obukhov range, therfore
    IF (newz*rmol > -2. .AND. newz*rmol < 1.) THEN

      ! Obukhov Length (using the Boussinesq approximation giving Tv from t2)
      OL = 1/rmol

      ! Wind speed at desired height
      CALL stabfunc_businger(newz,OL,stability)
      wsz = ust/karman*( LOG(newz/znt) + stability)

      ! Without taking into account rotation of winds due to Ekman pumping, etc... redistributed by 
      !   components unsing 10-m wind as reference...
      alpha = ATAN2(v10,u10)
      uvnewz(1) = wsz*COS(alpha)
      uvnewz(2) = wsz*SIN(alpha)
      
    ELSE
      ! Linear interpolation. Something better?
      uvnewz(1) = interpolate1D(dz, z-topo, ua, newz)
      uvnewz(2) = interpolate1D(dz, z-topo, va, newz)
    
    END IF
    
    ! Earth-rotation
    uznew = uvnewz(1)*ca - uvnewz(2)*sa
    vznew = uvnewz(1)*sa + uvnewz(2)*ca

    RETURN

  END SUBROUTINE var_zwind_MOtheor

  ! L. Fita, CIMA. Feb. 2018
  ! WRF seems to have problems with my functions, let'suse subroutine instead
  !REAL FUNCTION stabfunc_businger(z,L)
  SUBROUTINE stabfunc_businger(z,L,stabfunc_busingerv)
  ! Fucntion of the stability function after Businger et al. (1971), JAS, 28(2), 181–189

    IMPLICIT NONE

    REAL, INTENT(in)                                     :: z,L
    REAL, INTENT(out)                                    :: stabfunc_busingerv

! Local
    REAL                                                 :: zL, X

!!!!!!! Variables
! z: height [m]
! L: Obukhov length [-]

    sfname = 'stabfunc_businger'

    IF (L /= 0.) THEN
      zL = z/L
    ELSE
      ! Neutral
      zL = 0.
    END IF

    IF (zL > 0.) THEN
    ! Stable case
      stabfunc_busingerv = 4.7*z/L
    ELSE IF (zL < 0.) THEN
    ! unstable
      X = (1. - 15.*z/L)**(0.25)
      !stabfunc_busingerv = -2.*LOG((1.+X)/2.)-LOG((1.+X**2)/2.)+2.*ATAN(X)-piconst/2.
      stabfunc_busingerv = LOG( ((1.+X**2)/2.)*((1.+X)/2.)**2)-2.*ATAN(X)+piconst/2.
    ELSE
      stabfunc_busingerv = 0.
    END IF

    RETURN

!  END FUNCTION stabfunc_businger
  END SUBROUTINE stabfunc_businger

  SUBROUTINE var_potevap_bulk(rho1, cd, uas, vas, ts, ps, qv1, potevap)
! Subroutine to compute the generic potential evapotranspiration following simple bulk formulation
!  Manabe, S., (1969): Climate and the ocean circulation, 1. the atmospheric circulation and the hydrology of 
!    the earth's surface, Mon. Weather Rev., 97, 739-774
!      potevap = dt*rho1*qc*(q2sat-qv1)

    IMPLICIT NONE

    REAL, INTENT(in)                                     :: rho1, cd, uas, vas, ts, ps, qv1
    REAL, INTENT(out)                                    :: potevap

! Local
    REAL                                                 :: qsol_sat, qc

!!!!!!! Variables
! rho1: atsmophere density at the first layer [kgm-3]
! cd: drag coefficient [-]
! uas, vas: x/y-components of 10-m wind [ms-1]
! ts: surface temperature [K]
! ps: surface pressure [Pa]
! qv1: 1st layer atmospheric mixing ratio [kgkg-1]
! potevap: potential evapo transpiration [kgm-2s-1]
  sfname = 'var_potevap_bulk'

  ! qsol_sat: Saturated air at ts
  qsol_sat = SaturationMixingRatio(ts, ps)

  ! qc: surface drag coefficient
  qc = SQRT(uas**2 + vas**2)*cd

  potevap = MAX(zeroRK, rho1*qc*(qsol_sat - qv1))

  END SUBROUTINE var_potevap_bulk

  SUBROUTINE var_potevap_Milly92(rho1, cd, uas, vas, tsk, ta1, ps, qv1, sfcevap, emiss, potevap)
! Subroutine to compute the potential bulk evapotranspiration with Milly 1992 correction
!   Milly, P. C. D. (1992): Potential evaporation and soil moisture in general circulation models, 
!     J. Climate, 5, 209–226
!      potevap = potevap_bulk*[1./(1.+correction_Milly)]
!        ETp(Ts) = potevap_bulk(Ts) = (dens/ra)*[qs(Ts) − qa]
!        ETp(Tv) = (dens/ra)*[qs(Tv) − qa]
!        correction_Milly = [ETp(Ts) - ETp(Tv)]/ETp(Tw)
!      see more details in:
!        A. Barella-Ortiz et al., (2013), Hydrol. Earth Syst. Sci., 17, 4625-4639
!
    IMPLICIT NONE

    REAL, INTENT(in)                                     :: rho1, cd, uas, vas, tsk, ta1, ps, qv1,    &
      sfcevap, emiss
    REAL, INTENT(out)                                    :: potevap

! Local
    REAL                                                 :: qsol_sat, qc
    REAL                                                 :: ta_05, ta05
    REAL                                                 :: potevapo_bulk, beta, derivT_qsol_sat
    REAL                                                 :: correction_Milly, corr_Milly1,            &
      corr_Milly2, corr_Milly

!!!!!!! Variables
! rho1: atsmophere density at the first layer [kgm-3]
! cd: drag coefficient [-]
! uas, vas: x/y-components of 10-m wind [ms-1]
! tsk: surface skin temperature
! ta1: 1st level air-temperature [K]
! ps: surface pressure [Pa]
! qv1: 1st layer atmospheric mixing ratio [kgkg-1]
! sfcevap: surface eavporation flux [Wm-2s-1]
! emiss: emissivity [1]
! potevap: potential evapo transpiration [kgm-2s-1]
  sfname = 'var_potevap_Milly92'

  ! qsol_sat: Saturated air by tsk
  qsol_sat = SaturationMixingRatio(tsk, ps)

  ! qc: surface drag coefficient
  qc = SQRT(uas**2 + vas**2)*cd

  ! Bulk potential evapotranspiration
  potevapo_bulk = rho1*qc*(qsol_sat-qv1)

  ! Moisture availability function (in a similar way is done in ORCHIDEE)
  !   beta = sfcevap / potevapo_bulk
  beta = sfcevap / potevapo_bulk

  ! derivT_qsol_sat: Derivative of Saturated air by ta1
  ! Using numerical 1st order approximation
  ! derivT_qsol_sat(T) = [qsol_sat(T+h) - qsol_sat(T-h)]/(2h)
  ta_05 = ta1-halfRK
  ta05 = ta1+halfRK
  derivT_qsol_sat = (SaturationMixingRatio(ta05, ps) - SaturationMixingRatio(ta_05, ps)) / (2.*halfRK)

  ! Milly's correction
  corr_Milly1 = XLV*rho1*qc*derivT_qsol_sat*(1.-beta)
  corr_Milly2 = 4.*emiss*STBOLT*ta1**3 + rho1*Cp*qc + XLV*rho1*qc*derivT_qsol_sat*beta
  correction_Milly = corr_Milly1 / corr_Milly2
  corr_Milly = 1./(1. + correction_Milly)

  potevap = MAX(zeroRK, potevapo_bulk*corr_Milly)

  END SUBROUTINE var_potevap_Milly92

  SUBROUTINE var_potevap_bulkgen(rho1, ust, uas, vas, ts, ps, qv1, potevap)
! Subroutine to compute the generic potential evapotranspiration following simple bulk formulation
!  Manabe, S., (1969): Climate and the ocean circulation, 1. the atmospheric circulation and the hydrology of 
!    the earth's surface, Mon. Weather Rev., 97, 739-774
!      potevap = dt*rho1*qc*(q2sat-qv1)

    IMPLICIT NONE

    REAL, INTENT(in)                                     :: rho1, ust, uas, vas, ts, ps, qv1
    REAL, INTENT(out)                                    :: potevap

! Local
    REAL                                                 :: qsol_sat, Cd, qc

!!!!!!! Variables
! rho1: atsmophere density at the first layer [kgm-3]
! ust: u* in similarity theory [ms-1]
! uas, vas: x/y-components of 10-m wind [ms-1]
! ts: surface temperature [K]
! ps: surface pressure [Pa]
! qv1: 1st layer atmospheric mixing ratio [kgkg-1]
! potevap: potential evapo transpiration [kgm-2s-1]
  sfname = 'var_potevap_bulkgen'

  ! qsol_sat: Saturated air at ts
  qsol_sat = SaturationMixingRatio(ts, ps)

  ! Cd: drag coeffiecient
  CALL Cdrag_0(ust, uas, vas, Cd)

  ! qc: surface drag coefficient
  qc = SQRT(uas**2 + vas**2)*Cd

  potevap = MAX(zeroRK, rho1*qc*(qsol_sat - qv1))

  END SUBROUTINE var_potevap_bulkgen

  SUBROUTINE var_potevap_Milly92gen(rho1, ust, uas, vas, tsk, ta1, ps, qv1, sfcevap, emiss, potevap)
! Subroutine to compute the genric potential bulk evapotranspiration with Milly 1992 correction
!   Milly, P. C. D. (1992): Potential evaporation and soil moisture in general circulation models, 
!     J. Climate, 5, 209–226
!      potevap = potevap_bulk*[1./(1.+correction_Milly)]
!        ETp(Ts) = potevap_bulk(Ts) = (dens/ra)*[qs(Ts) − qa]
!        ETp(Tv) = (dens/ra)*[qs(Tv) − qa]
!        correction_Milly = [ETp(Ts) - ETp(Tv)]/ETp(Tw)
!      see more details in:
!        A. Barella-Ortiz et al., (2013), Hydrol. Earth Syst. Sci., 17, 4625-4639
!
    IMPLICIT NONE

    REAL, INTENT(in)                                     :: rho1, ust, uas, vas, tsk, ta1, ps, qv1,   &
      sfcevap, emiss
    REAL, INTENT(out)                                    :: potevap

! Local
    REAL                                                 :: qsol_sat, Cd, qc
    REAL                                                 :: ta_05, ta05
    REAL                                                 :: potevapo_bulk, beta, derivT_qsol_sat
    REAL                                                 :: correction_Milly, corr_Milly1,            &
      corr_Milly2, corr_Milly

!!!!!!! Variables
! rho1: atsmophere density at the first layer [kgm-3]
! ust: u* in similarity theory [ms-1]
! uas, vas: x/y-components of 10-m wind [ms-1]
! tsk: surface skin temperature
! ta1: 1st level air-temperature [K]
! ps: surface pressure [Pa]
! qv1: 1st layer atmospheric mixing ratio [kgkg-1]
! sfcevap: surface eavporation flux [Wm-2s-1]
! emiss: emissivity [1]
! potevap: potential evapo transpiration [kgm-2s-1]
  sfname = 'var_potevap_Milly92gen'

  ! qsol_sat: Saturated air by tsk
  qsol_sat = SaturationMixingRatio(tsk, ps)

  ! Cd: drag coeffiecient
  CALL Cdrag_0(ust, uas, vas, Cd)

  ! qc: surface drag coefficient
  qc = SQRT(uas**2 + vas**2)*Cd

  ! Bulk potential evapotranspiration
  potevapo_bulk = rho1*qc*(qsol_sat-qv1)

  ! Moisture availability function (in a similar way is done in ORCHIDEE)
  !   beta = sfcevap / potevapo_bulk
  beta = sfcevap / potevapo_bulk

  ! derivT_qsol_sat: Derivative of Saturated air by ta1
  ! Using numerical 1st order approximation
  ! derivT_qsol_sat(T) = [qsol_sat(T+h) - qsol_sat(T-h)]/(2h)
  ta_05 = ta1-halfRK
  ta05 = ta1+halfRK
  derivT_qsol_sat = (SaturationMixingRatio(ta05, ps) - SaturationMixingRatio(ta_05, ps)) / (2.*halfRK)

  ! Milly's correction
  corr_Milly1 = XLV*rho1*qc*derivT_qsol_sat*(1.-beta)
  corr_Milly2 = 4.*emiss*STBOLT*ta1**3 + rho1*Cp*qc + XLV*rho1*qc*derivT_qsol_sat*beta
  correction_Milly = corr_Milly1 / corr_Milly2
  corr_Milly = 1./(1. + correction_Milly)

  potevap = MAX(zeroRK, potevapo_bulk*corr_Milly)

  END SUBROUTINE var_potevap_Milly92gen

  SUBROUTINE var_fog_K84(qc, qi, fog, vis)
  ! Computation of fog (vis < 1km) only computed where qcloud, qice /= 0.
  ! And visibility following Kunkel, B. A., (1984): Parameterization of droplet terminal velocity and 
  !   extinction coefficient in fog models. J. Climate Appl. Meteor., 23, 34–41.

  IMPLICIT NONE

  REAL, INTENT(in)                                       :: qc, qi
  INTEGER, INTENT(out)                                   :: fog
  REAL, INTENT(out)                                      :: vis

! Local
  REAL                                                   :: visc, visi

!!!!!!! Variables
! qc: cloud mixing ratio [kgkg-1]
! qi, ice mixing ratio [kgkg-1]
! fog: presence of fog (1: yes, 0: no)
! vis: visibility within fog [km]

  sfname = 'var_fog_K84'
  
  IF (qi > nullv .OR. qc > nullv) THEN
    visc = 100000.*oneRK
    visi = 100000.*oneRK
    ! From: Gultepe, 2006, JAM, 45, 1469-1480
    IF (qc > nullv) visc = 0.027*(qc*1000.)**(-0.88)
    IF (qi > nullv) visi = 0.024*(qi*1000.)**(-1.0)
    ! Getting the lowest visibility
    vis = MINVAL((/visc, visi/))
    IF (vis <= oneRK) THEN
      fog = 1
    ELSE
      fog = 0
      vis = -oneRK
    END IF
  ELSE
    fog = 0
    vis = -oneRK
  END IF

  END SUBROUTINE var_fog_K84

  SUBROUTINE var_fog_RUC(rhv, fog, vis)
  ! Computation of fog (vis < 1km) only computed where qcloud, qice /= 0.
  ! And visibility following RUC method Smirnova, T. G., S. G. Benjamin, and J. M. Brown, 2000: Case 
  !   study verification of RUC/MAPS fog and visibility forecasts. Preprints, 9 th Conference on 
  !   Aviation, Range, and Aerospace Meteorlogy, AMS, Orlando, FL, Sep. 2000. Paper#2.3, 6 pp.

  IMPLICIT NONE

  REAL, INTENT(in)                                       :: rhv
  INTEGER, INTENT(out)                                   :: fog
  REAL, INTENT(out)                                      :: vis

! Local
  REAL                                                   :: rh

!!!!!!! Variables
! rh: relative humidty [1]
! vis: visibility within fog [km]

  sfname = 'var_fog_RUC'

  ! Avoiding supersaturation 
  rh = MINVAL((/1.,rhv/))

  IF (rh > 0.3) THEN
    ! From: Gultepe, I., and G. Isaac, 2006: Visbility versus precipitation rate and relative 
    !   humidity. Preprints, 12th Cloud Physics Conf, Madison, WI, Amer. Meteor. Soc., P2.55. 
    !   [Available  online  at  http://ams.confex.com/ams/Madison2006/techprogram/paper_l13177.htm]
    vis = 60.*EXP(-2.5*(rh*100.-15.)/80.)
    IF (vis <= oneRK) THEN
      fog = 1
    ELSE
      fog = 0
      vis = -oneRK
    END IF
  ELSE
    fog = 0
    vis = -oneRK
  END IF

  END SUBROUTINE var_fog_RUC

  SUBROUTINE var_fog_FRAML50(rhv, fog, vis)
  ! Computation of fog (vis < 1km)
  ! And visibility following Gultepe, I. and J.A. Milbrandt, 2010: Probabilistic Parameterizations 
  !   of Visibility Using Observations of Rain Precipitation Rate, Relative Humidity, and Visibility. 
  !   J. Appl. Meteor. Climatol., 49, 36-46, https://doi.org/10.1175/2009JAMC1927.1
  ! Interest is focused on a 'general' fog/visibilty approach, thus the fit at 50 % of probability
  !   is chosen
  ! Effects from precipitation are not considered

  IMPLICIT NONE

  REAL, INTENT(in)                                       :: rhv
  INTEGER, INTENT(out)                                   :: fog
  REAL, INTENT(out)                                      :: vis

! Local
  REAL                                                   :: rh

!!!!!!! Variables
! rhv: relative humidty [1]
! fog: presence of fog (1: yes, 0: no)
! vis: visibility within fog [km]

  sfname = 'var_fog_FRAML50'

  ! Avoiding supersaturation 
  rh = MINVAL((/1.,rhv/))

  IF (rh > 0.3) THEN
    vis = -5.19*10.**(-10)*(rh*100.)**5.44+40.10
    ! Fog definition (vis <= 1. km)
    IF (vis <= oneRK) THEN
      fog = 1
    ELSE
      vis = -oneRK
      fog = 0
    END IF
  ELSE
    vis = -oneRK
    fog = 0
  END IF

  END SUBROUTINE var_fog_FRAML50

  SUBROUTINE var_tds(hurs, tas, tds)
! Subroutine to compute dew point temperature at 2m using August-Roche-Magnus approximation [k]

    IMPLICIT NONE

    REAL, INTENT(in)                                     :: hurs, tas
    REAL, INTENT(out)                                    :: tds

! Local
    REAL                                                 :: gammatarh

!!!!!!! Variables
! hurs: 2m relative humidity [1]
! tds: dew point temperature [k]

    sfname = 'var_tds'

    gammatarh = LOG(hurs) + ARM2*(tas-SVPT0)/((tas-SVPT0)+ARM3)
    tds = ARM3*gammatarh/(ARM2-gammatarh) + SVPT0

    RETURN

  END SUBROUTINE var_tds
  
  SUBROUTINE var_tws_S11(ta0, hur0, tws)
! Subroutine to compute wet bulb temperature using equation after:
!    Stull, R. (2011), J. Appl. Meteor. Climatol. 50(11):2267-2269. doi: 10.1175/JAMC-D-11-0143.1

    IMPLICIT NONE

    REAL, INTENT(in)                                     :: ta0, hur0
    REAL, INTENT(out)                                    :: tws

! Local
    REAL                                                 :: ta, hur
    CHARACTER(len=50)                                    :: fname

!!!!!!! Variables
! ta0: temperature [K] (does it only make sense if it is at 2 m?)
! hur0: relative humidity [1] (does it only make sense if it is at 2 m?)
! tws: wet bulb temperature [C]

    fname = 'var_tws_S11'

    ta = ta0 - SVPT0
    hur = hur0*100.*oneRK

    tws = ta * ATAN(0.151977*SQRT(hur+8.313659)) + ATAN(ta+hur) - ATAN(hur-1.676331) +                &
      0.00391838*(hur)**(1.5)*ATAN(0.023101*hur) - 4.686035
 
    RETURN

  END SUBROUTINE var_tws_S11
    
  SUBROUTINE var_residencetime_2var2D(vals1, vals2, Nrng1, Nrng2, rng1, rng2, ir1, ir2)
! Subroutine to compute the time of residence of two 2D variables for a single time-step
!  
!    Time of residence: accumulated time passed within a given range of variables
!
    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: Nrng1, Nrng2
    REAL, DIMENSION(Nrng1), INTENT(in)                   :: rng1
    REAL, DIMENSION(Nrng2), INTENT(in)                   :: rng2
    REAL, INTENT(in)                                     :: vals1, vals2
    INTEGER, INTENT(out)                                 :: ir1, ir2

    ! Local
    INTEGER                                              :: ir
    CHARACTER(len=50)                                    :: fname

!!!!!!! Variables
! vals[1/2]: values of the variable 1 and 2
! Nrng[1/2]: amount of intervals + 1 along the variable 1/2
! rng[1/2]: intervals along the variable 1/2
! dt: interval of time (in seconds)
! tresidence2D: 2D time of residence, the amount of time passed simultaneously within rangeN of var 1
!   and rangeM of var2

    fname = 'var_residencetime_2var2D'

    ! inicialization
    ir1 = -1
    ir2 = -1
    
    ! Looking for the varibale1's bin
    IF (vals1 < rng1(1)) THEN
      ir1 = 1    
    ELSE IF (vals1 >= rng1(Nrng1-1)) THEN
      ir1 = Nrng1 
    ELSE
      DO ir=1, Nrng1-2
        IF (vals1 >= rng1(ir) .AND. vals1 < rng1(ir+1)) THEN
          ir1 = ir+1
        END IF
      END DO
    END IF

    ! Looking for the varibale2's bin
    IF (vals2 < rng2(1)) THEN
      ir2 = 1
    ELSE IF (vals2 >= rng2(Nrng2-1)) THEN
      ir2 = Nrng2 
    ELSE
      DO ir=1, Nrng2-2
        IF (vals2 >= rng2(ir) .AND. vals2 < rng2(ir+1)) THEN
          ir2 = ir+1
        END IF
      END DO
    END IF
    
    ! Checking consistency
    ! L. Fita, January 2024. Removed for efficiency
    !   If you encounter problems with the variable, remove it and re-compile without clean -a
!    IF (ir1 == -1 .OR. ir2 == -1) THEN
!      PRINT *,'  ' // TRIM(fname) // ': Unsuccesfull allocation of var1:', vals1,' var2:', vals2
!      PRINT *,'    resultant ir1=', ir1, ' ir2=', ir2
!      PRINT 23,'var1 bins: ', rng1
!      PRINT 23,'var2 bins:', rng2
!    END IF

 23 FORMAT(4x,A10,50(F86.2))

    RETURN 

  END SUBROUTINE var_residencetime_2var2D
  
  REAL FUNCTION zero_isotherm1D(dz, ta, hgt)
  ! Function to compute 1D 0-isotherm
  
    IMPLICIT NONE
    
    INTEGER, INTENT(in)                                  :: dz
    REAL, DIMENSION(dz), INTENT(in)                      :: ta, hgt
    
    ! Local
    INTEGER                                              :: iz, i0ta
    REAL                                                 :: zeroK
    CHARACTER(len=50)                                    :: fname
    
!!!!!!! Variables
! dz: amount of vertical levels
! ta: vertical temperature [K]
! hgt: height above ground (zg/9.81 - topo) [m]
! interp: whether height should be interpolated or provide the closest level

    fname = 'zero_isotherm1D'
    
    zero_isotherm1D = 1.e20
    
    zeroK = 273.15
    
    zero_isotherm1D = interpolate1D(dz, ta, hgt, zeroK)
    
    RETURN
  
  END FUNCTION zero_isotherm1D
 
!!!
! Operations
!!!

  SUBROUTINE massconserv_vinterp1D(dorig, levorig, dorig1, btlevorig, valsorig, ddes, levdes,         &
    ddes1, btlevdes, valsdes)
  ! Subrotine to mass-conservative vertically interpolate a variable 1D
  
    IMPLICIT NONE
    
    INTEGER, INTENT(in)                                  :: dorig, ddes, dorig1, ddes1
    REAL, DIMENSION(dorig), INTENT(in)                   :: levorig, valsorig
    REAL, DIMENSION(dorig1), INTENT(in)                  :: btlevorig
    REAL, DIMENSION(ddes), INTENT(in)                    :: levdes
    REAL, DIMENSION(ddes1), INTENT(in)                   :: btlevdes
    REAL, DIMENSION(ddes), INTENT(out)                   :: valsdes
    
    ! Local
    INTEGER                                              :: ko, kd
    REAL                                                 :: deskm1, deskp1
    REAL                                                 :: orikm1, orikp1
    REAL                                                 :: Dlevorig, delta_levorig, Dlevdes
    REAL                                                 :: delta_desm1, delta_desp1
    REAL                                                 :: deltatop_levorig, deltabot_levorig
    CHARACTER(len=50)                                    :: fname

!!!!!!! Variables
! dorig: Original amount of vertical levels
! levorig: original values of the levels (centerd into the level) [m]
! valsorig: original values of the field at the original levels (as density or similar [kgm-3])
! dorig1: original amount of vertical levels + 1
! btlevorig: bottom, tops of the original cells [m]
!     btlev(k) < lev(k) < btlev(k+1)
! ddes: desired amount of vertical levels
! levdes: desired levels [m]
! ddes1: desired amount of vertical levels + 1
! btlevdes: bottom, tops of the desired cells [m]
! valsdest: mass-conservative interpolated values (as density or similar [kgm-3])
    
    fname = 'massconserv_vinterp1D'
    
    valsdes = zeroRK
    
    ! Let's be efficient
   
    ! Assuming surface -> top vertical layers and Xsfc < Xtop (as the case with height...)
   
    ! Here comes the proper integration, where the values of the original cells within the range
    !   of the desired cell, are added according to the percentage of the height of the original 
    !   cell that lays within the range of the desired cell
    !   
    !   valsdes(kd) = SUM_Nlev(delta_levorig(ko)*valsorig(ko)) + 
    !     deltatop_levorig(itopkin)*valsorig(itopkin) + deltabot_levorig(ibotkin)*valsorig(ibotkin)
    !
    !   valdes(kd) = valdes(kd) / Dlevdes
    !
    !     where:
    !        Dlevdes = deskp1 - deskm1
    !        Dlevorig = orikp1 - orikm1
    ! 
    !        delta_levorig(ko) = Dlevorig - delta_desm1 - delta_desp1
    !
    !        delta_desm1 = 
    !          deskm1 - orikm1;        deskm1 > orikm1
    !          0;                      deskm1 < orikm1   
    !        delta_desp1 = 
    !          orikp1 - deskp1;        deskp1 < orikp1
    !          0;                      deskp1 > orikp1
    !
    !        deltatop_levorig(itopkin) = orikp1 - deskm1
    !        deltabot_levorig(ibotkin) = deskp1 - orikm1
    !
    !     being:
    !       orikm1: value at the base of the original cell
    !       orikp1: value at the top of the original cell
    !       deskm1: value at the base of the desired cell
    !       deskp1: value at the top of the desired cell
    !       Nlev: amount of original levels within the height of the kd-desired cell
    !         Nlev = orilev(ko) <= deskm1 .and. orilev(ko) > deskp1
   
    DO kd=1, ddes

      ! Cell's bottom          
      deskm1 = btlevdes(kd)
      ! Cell0s top
      deskp1 = btlevdes(kd+1)
      
      Dlevdes = deskp1 - deskm1
  
      ! Looking for the original cells within the range of the desired cell
      DO ko=1, dorig
        ! original cell's top
        orikp1 = btlevorig(ko+1)

        ! original cell's bottom          
        orikm1 = btlevorig(ko)

        IF (levorig(ko) >= deskm1 .AND. levorig(ko) < deskp1) THEN
          ! Center of the columns
          
          Dlevorig = orikp1 - orikm1
          IF (deskm1 > orikm1) THEN
            delta_desm1 = deskm1 - orikm1
          ELSE
            delta_desm1 = zeroRK
          END IF
          IF (deskp1 < orikp1) THEN
            delta_desp1 = orikp1 - deskp1
          ELSE
            delta_desp1 = zeroRK
          END IF
        
          delta_levorig = Dlevorig - delta_desm1 - delta_desp1
        
          valsdes(kd) = valsdes(kd) + delta_levorig*valsorig(ko)

        ELSE IF (orikp1 > deskm1 .AND. levorig(ko) < deskm1) THEN
          ! We might have only top of the original columns wihtin the range
       
          deltatop_levorig = orikp1 - deskm1
        
          valsdes(kd) = valsdes(kd) + deltatop_levorig*valsorig(ko)      

        ELSE IF (orikm1 < deskp1 .AND. levorig(ko) > deskp1) THEN
          ! We might have only base of the original columns wihtin the range

          deltabot_levorig = deskp1 - orikm1
          
          valsdes(kd) = valsdes(kd) + deltabot_levorig*valsorig(ko)      
        
        ELSE IF (orikm1 > deskp1) THEN
          EXIT
          
        END IF

      END DO
      
      valsdes(kd) = valsdes(kd)/Dlevdes
      
    END DO
  
    RETURN
    
  END SUBROUTINE massconserv_vinterp1D
  
  SUBROUTINE presbars_to_hgtbars(topo, dzs, zg, dz, oripres, psfc, ptop, dpres, despressv,            &
    firstrightlev, Nrightlevs, rightdespressv, hgts, bthgts)
  ! Subroutine to transform from desired bars defined by their central pressures to bars in height 
  !   coordinates using model's geopotential to define its heights
  
    IMPLICIT NONE
    
    INTEGER, INTENT(in)                                  :: dpres, dz, dzs
    REAL, DIMENSION(dpres), INTENT(in)                   :: despressv
    REAL, INTENT(in)                                     :: topo, psfc, ptop
    REAL, DIMENSION(dzs), INTENT(in)                     :: zg
    REAL, DIMENSION(dz), INTENT(in)                      :: oripres
    INTEGER, INTENT(out)                                 :: firstrightlev, Nrightlevs
    REAL, DIMENSION(dpres), INTENT(out)                  :: hgts, rightdespressv
    REAL, DIMENSION(dpres,2), INTENT(out)                :: bthgts
    
    ! Local
    INTEGER                                              :: k, ko, dzstag
    REAL                                                 :: botpressv
    REAL                                                 :: a, b
    REAL, DIMENSION(dz)                                  :: orihgt
    REAL, DIMENSION(dzs)                                 :: oribthgt
    CHARACTER(len=50)                                    :: fname 

!!!!!!! Variables
! topo: topography [m]
! dzs: amount of geopotential heights
! zg: geopotential heights [m2s-2]
! dz: amount of original pressure
! oripres: original pressure values [Pa]
! psfc: pressure at the surfrace [Pa]
! ptop: pressure at the top [Pa]
! dpres: amount of desired pressure levels 
! despressv: desired pressure levels [Pa]
! Nrightlevs: amount of right pressure levels (it might hapeen to want 'below' ground or 'above' top 
!   values)
! rightdespressv: right desired pressure values to provide heights
! hgts: heights at the desired pressure levels [m]
! bthgts: bottoms and tops of the cells at the desired pressure levels [m]
  
    fname = 'presbars_to_hgtbars'
  
    rightdespressv = 0.
    hgts = 0.
    bthgts = 0.
  
    ! WRF has staggered zg
    dzstag = dzs
    oribthgt = zg / 9.81
    orihgt = 0.5*(oribthgt(2:dzstag)+oribthgt(1:dzstag-1))
  
    ! Get the conversion
    firstrightlev = -1
    Nrightlevs = 0
    hgts(1) = topo
    DO k=1, dpres
      DO ko = 1, dz
        ! Filtering to avoid 'below ground' and 'above atmos top' interpolation values
        IF (oripres(ko) >= despressv(k) .AND. oripres(ko+1) < despressv(k) .AND.                      &
            psfc > despressv(k) .AND. ptop < despressv(k)) THEN
  
          Nrightlevs = Nrightlevs + 1
          IF (firstrightlev == -1) firstrightlev = k
          
          a = (oripres(ko+1)*orihgt(ko)-oripres(ko)*orihgt(ko+1))/(oripres(ko+1)-oripres(ko))
          b = (orihgt(ko+1)-orihgt(ko))/(oripres(ko+1)-oripres(ko))
          rightdespressv(Nrightlevs) = despressv(k)
          hgts(Nrightlevs) = a + despressv(k)*b
  
          EXIT
        END IF
      END DO
    END DO
  
    ! Giving the bottom / tops of only the right desired pressure values. Interpolating from pressure
    !   values and using then the geopotential to get the right height, as a way to be more precise
    bthgts(1,1) = topo
    DO k=2, Nrightlevs
    
      ! p-height of the cell
      botpressv = 0.5*(rightdespressv(k) + rightdespressv(k-1))
      
      ! Interprolation
      bthgts(k,1) = interpolate1D(dz, oripres, orihgt, botpressv)
      bthgts(k-1,2) = bthgts(k,1)
    END DO
    bthgts(Nrightlevs,2) = oribthgt(dzstag)

    RETURN

  END SUBROUTINE presbars_to_hgtbars

  REAL FUNCTION interpolate1D(Nvals, xvals, yvals, xpos)
  ! Function to interpolate 1D value
  
    IMPLICIT NONE
    
    INTEGER, INTENT(in)                                  :: Nvals
    REAL, DIMENSION(Nvals), INTENT(in)                   :: xvals, yvals
    REAL, INTENT(in)                                     :: xpos
    
    ! Local
    INTEGER                                              :: i, ip, iclosest
    REAL(r_k)                                            :: mindist, avi, avip
    REAL                                                 :: vi, vip, dx, dy, a, b
    LOGICAL                                              :: found
    CHARACTER(len=50)                                    :: fname
    
!!!!!!! Variables
! Nvals: Amount of values to use for the interpolation
! xvals: x-axis positions
! yvals: y-axis values
! xpos: position at which we want to interpolate

    fname = 'interpolate1D'

    interpolate1D = 1.e20
  
    found = .FALSE.
  
    avi = ABS(xvals(1) - xpos)
    iclosest = 1    
    mindist = avi
    DO i=1, Nvals-1
      ip = i + 1
      vi = xvals(i) - xpos
      vip = xvals(ip) - xpos
      avip = ABS(vip)      
      IF (avip < mindist) THEN
        mindist = avip
        iclosest = ip
      END IF
      
      IF (vi*vip <= 0.) THEN
        dx = xvals(ip)-xvals(i)
        dy = yvals(ip)-yvals(i)
        a = (xvals(ip)*yvals(i)-xvals(i)*yvals(ip))/dx
        b = dy/dx
        interpolate1D = a + xpos*b
        found = .TRUE.
        EXIT
      END IF
    END DO
    
    ! Case where xpos does not lay within 2 consecutive xvals
    IF (.NOT.found) THEN
      IF (iclosest == Nvals) THEN
        i = Nvals-1
        ip = Nvals
      ELSE
        i = iclosest
        ip = iclosest + 1
      END IF
      dx = xvals(ip)-xvals(i)
      dy = yvals(ip)-yvals(i)
      a = (xvals(ip)*yvals(i)-xvals(i)*yvals(ip))/dx
      b = dy/dx
      interpolate1D = a + xpos*b
    END IF

    RETURN

  END FUNCTION interpolate1D


  SUBROUTINE NumIntegration(yvals, xvals, Nvalint, Ntotval, intval)
! Subroutine to compute numerical integrations according to the trapezoidal methodology

    IMPLICIT NONE
  
    REAL, DIMENSION(Ntotval), INTENT(in)                 :: yvals, xvals
    INTEGER, INTENT(in)                                  :: Nvalint, Ntotval
    REAL, INTENT(out)                                    :: intval
  
!!!!!!! Variables
! yvals: values to integrate
! xvals: equivalent positions on x-axis
! Nvalint: range of the integral
! Ntotval: total number of values
! intval: result of the integration

    intval=0.
    intval=SUM((xvals(2:Nvalint)-xvals(1:Nvalint-1))*(yvals(1:Nvalint-1)+yvals(2:Nvalint))/2.)

    RETURN
  
  END SUBROUTINE NumIntegration

  SUBROUTINE partial_agg(d1, vals, dvals, idvals, edvals, partagg)
! Subroutine to compute a partial aggregation of mass-values between an interval.

    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: d1
    REAL, DIMENSION(d1), INTENT(in)                      :: vals, dvals
    REAL, INTENT(in)                                     :: idvals, edvals
    REAL, INTENT(out)                                    :: partagg

! Local
    INTEGER                                              :: k

!!!!!!! Variables
! d1: length of the axis
! vals: values to aggregate
! dvals: values along the axis
! idvals, edvals: initial and final value of the interval
! partagg: partial aggregation

    sfname = 'partial_agg'

    partagg = 0.

    DO k=1,d1
      IF (dvals(k) >= idvals .AND. (dvals(k) <= edvals )) partagg = partagg + vals(k)
    END DO

    RETURN

  END SUBROUTINE partial_agg

  SUBROUTINE interval_agg(d1, Ninterv, Ninterv1, vals, dvals, interval, intagg)
! Subroutine to compute aggregation by intervals along the full axis of of mass-values

    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: d1, Ninterv, Ninterv1
    REAL, DIMENSION(d1), INTENT(in)                      :: vals, dvals
    REAL, DIMENSION(Ninterv), INTENT(in)                 :: interval
    REAL, DIMENSION(Ninterv1), INTENT(out)               :: intagg

! Local
    INTEGER                                              :: k, ijk
    REAL                                                 :: signd

!!!!!!! Variables
! d1: length of the axis
! Ninterv: Number of intervals
! Ninterv1: Number of aggregations (Ninterv+1)
! vals: values to aggregate
! dvals: values along the axis
! interval: values of the intervals
! intagg: aggregation along intervals

    sfname = 'interval_agg'

    intagg = 0.

    IF (dvals(1) > dvals(2)) THEN
      signd = -1
! Lest work fast ...
!      IF (interval(1) < interval(2)) THEN
!        WRITE(msg,*)'Axis values decrese, thus interval values must too'
!        CALL ErrMsg(msg, TRIM(sfname), -1)
!      END IF
    ELSE
      signd = 1
! Lest work fast ...
!      IF (interval(1) > interval(2)) THEN
!        WRITE(msg,*)'Axis values increase, thus interval values must too'
!        CALL ErrMsg(msg, TRIM(sfname), -1)
!      END IF
    END IF

    ijk = 1
    IF (signd == -1) THEN
      ! Axis in descending sense
      DO k=1, d1
        IF (dvals(k) < interval(ijk)) THEN
          IF (ijk < Ninterv) THEN
            ijk = ijk + 1
            intagg(ijk) = intagg(ijk) + vals(k)
          ELSE
            intagg(Ninterv1) = intagg(Ninterv1) + vals(k)
          END IF
        ELSE
          intagg(ijk) = intagg(ijk) + vals(k)
        END IF  
      END DO

    ELSE
      ! Axis in ascending sense
      DO k=1, d1
        IF (dvals(k) > interval(ijk)) THEN
          IF (ijk < Ninterv) THEN
            ijk = ijk + 1
            intagg(ijk) = intagg(ijk) + vals(k)
          ELSE
            intagg(Ninterv1) = intagg(Ninterv1) + vals(k)
          END IF
        ELSE
          intagg(ijk) = intagg(ijk) + vals(k)
        END IF  
      END DO

    END IF

    RETURN

  END SUBROUTINE interval_agg

  SUBROUTINE moist_group(Nmoist, i_qv, i_qc, i_qr, i_qs, i_qi, i_qg, i_qh, qvval, qcval, qrval, qsval,&
    qival, qgval, qhval, qvarvals)
! Subroutine to group 1D values from each water-species to a single variable

    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: Nmoist, i_qv, i_qc, i_qr, i_qs, i_qi,     &
      i_qg, i_qh
    REAL, INTENT(in)                                     :: qvval, qcval, qrval, qsval, qival, qgval, &
      qhval
    REAL, DIMENSION(Nmoist), INTENT(out)                 :: qvarvals

!!!!!!! Variables
! Nmoist: number of water species
! i_q[v/c/r/s/i/g/h]: index for water vapour, cloud, rain, snow, ice, graupel and hail
! q[v/c/r/s/i/g/h]val: individual values for water vapour, cloud, rain, snow, ice, graupel and hail
! qvarvals: 2D values of multiple water species

    sfname = 'moist_group'

    IF (MAXVAL((/ i_qv, i_qc, i_qr, i_qs, i_qi, i_qg, i_qh /)) > Nmoist) THEN
      WRITE(message,*) '  ' // TRIM(sfname) // ': Wrong number of water-species Nmoist:', Nmoist,     &
        ' for:', i_qv, i_qc, i_qr, i_qs, i_qi, i_qg, i_qh,' increase it !!'
      CALL wrf_error_fatal(message)
    END IF

    qvarvals(i_qv) = qvval
    qvarvals(i_qc) = qcval
    qvarvals(i_qr) = qrval
    qvarvals(i_qs) = qsval
    qvarvals(i_qi) = qival
    qvarvals(i_qg) = qgval
    qvarvals(i_qh) = qhval

    RETURN

  END SUBROUTINE moist_group

  SUBROUTINE moist_group2D(Nmoist, d1, i_qv, i_qc, i_qr, i_qs, i_qi, i_qg, i_qh, qvval, qcval, qrval, &
    qsval, qival, qgval, qhval, qvarvals)
! Subroutine to group 2D values from each water-species to a single variable

    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: Nmoist, d1, i_qv, i_qc, i_qr, i_qs, i_qi, &
      i_qg, i_qh
    REAL, DIMENSION(d1), INTENT(in)                      :: qvval, qcval, qrval, qsval, qival, qgval, &
      qhval
    REAL, DIMENSION(d1,Nmoist), INTENT(out)              :: qvarvals

!!!!!!! Variables
! Nmoist: number of water species
! i_q[v/c/r/s/i/g/h]: index for water vapour, cloud, rain, snow, ice, graupel and hail
! q[v/c/r/s/i/g/h]val: individual values for water vapour, cloud, rain, snow, ice, graupel and hail
! qvarvals: 2D values of multiple water species

    sfname = 'moist_group2D'

    IF (MAXVAL((/ i_qv, i_qc, i_qr, i_qs, i_qi, i_qg, i_qh /)) > Nmoist) THEN
      WRITE(message,*) '  ' // TRIM(sfname) // ': Wrong number of water-species Nmoist:', Nmoist,     &
        ' for:', i_qv, i_qc, i_qr, i_qs, i_qi, i_qg, i_qh,' increase it !!'
      CALL wrf_error_fatal(message)
    END IF

    qvarvals(:,i_qv) = qvval(:)
    qvarvals(:,i_qc) = qcval(:)
    qvarvals(:,i_qr) = qrval(:)
    qvarvals(:,i_qs) = qsval(:)
    qvarvals(:,i_qi) = qival(:)
    qvarvals(:,i_qg) = qgval(:)
    qvarvals(:,i_qh) = qhval(:)

    RETURN

  END SUBROUTINE moist_group2D

  SUBROUTINE moist_redistribute(Nmoist, qvarvals, i_qv, i_qc, i_qr, i_qs, i_qi, i_qg, i_qh, qvval,    &
    qcval, qrval, qsval, qival, qgval, qhval)
! Subroutine to re-distribute a multi water-species 1D value to each specie

    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: Nmoist, i_qv, i_qc, i_qr, i_qs, i_qi,     &
      i_qg, i_qh
    REAL, DIMENSION(Nmoist), INTENT(in)                  :: qvarvals
    REAL, INTENT(out)                                    :: qvval, qcval, qrval, qsval, qival, qgval, &
      qhval

!!!!!!! Variables
! Nmoist: number of water species
! qvarvals: 1D values of multiple water species
! i_q[v/c/r/s/i/g/h]: index for water vapour, cloud, rain, snow, ice, graupel and hail
! q[v/c/r/s/i/g/h]val: individual values for water vapour, cloud, rain, snow, ice, graupel and hail

    sfname = 'moist_redistribute'

    qvval = qvarvals(i_qv)
    qcval = qvarvals(i_qc)
    qrval = qvarvals(i_qr)
    qsval = qvarvals(i_qs)
    qival = qvarvals(i_qi)
    qgval = qvarvals(i_qg)
    qhval = qvarvals(i_qh)

    RETURN

  END SUBROUTINE moist_redistribute

  SUBROUTINE moist_redistribute2D(Nmoist, d1, qvarvals, i_qv, i_qc, i_qr, i_qs, i_qi, i_qg, i_qh,     &
    qvval, qcval, qrval, qsval, qival, qgval, qhval)
! Subroutine to re-distribute a multi water-species 2D value to each specie

    IMPLICIT NONE

    INTEGER, INTENT(in)                                  :: Nmoist, d1, i_qv, i_qc, i_qr, i_qs, i_qi, &
      i_qg, i_qh
    REAL, DIMENSION(d1,Nmoist), INTENT(in)               :: qvarvals
    REAL, DIMENSION(d1), INTENT(out)                     :: qvval, qcval, qrval, qsval, qival, qgval, &
      qhval

!!!!!!! Variables
! Nmoist: number of water species
! qvarvals: 2D values of multiple water species
! i_q[v/c/r/s/i/g/h]: index for water vapour, cloud, rain, snow, ice, graupel and hail
! q[v/c/r/s/i/g/h]val: individual values for water vapour, cloud, rain, snow, ice, graupel and hail

    sfname = 'moist_redistribute2D'

    qvval(:) = qvarvals(:,i_qv)
    qcval(:) = qvarvals(:,i_qc)
    qrval(:) = qvarvals(:,i_qr)
    qsval(:) = qvarvals(:,i_qs)
    qival(:) = qvarvals(:,i_qi)
    qgval(:) = qvarvals(:,i_qg)
    qhval(:) = qvarvals(:,i_qh)

    RETURN

  END SUBROUTINE moist_redistribute2D



!!!!! ORCHIDEE related diagnostics -------------------------------------------------------

   SUBROUTINE SFCDIAGS_ORCHIDEE1D( u1, v1, t1, q1, z1, ps1,                                           &
                           ts1, qsurf, rugos, psol,                                                   &
                           cdrah, cdram, ri1, ustar, lmon,                                            &
                           kms, kme, kts, kte,                                                        &
                           t_2m, q_2m)

     ! Surface diagnostics of t2 and q2 from the LMDZ GCM (old version)

     IMPLICIT NONE
      
     INTEGER,  INTENT(in)                                :: kms, kme, kts, kte


     REAL , DIMENSION(kms:kme) , INTENT(in)              :: u1, v1, t1, ps1
     REAL , INTENT(in)                                   :: q1, z1, ts1, qsurf, rugos, cdrah, cdram 
     REAL , INTENT(in)                                   :: ri1, psol, ustar, lmon
!
     REAL , INTENT(inout)                                :: t_2m, q_2m
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! niter : nombre iterations calcul "corrector"
!    INTEGER, PARAMETER                                  :: niter=6, ncon=niter-1
     INTEGER, PARAMETER                                  :: niter=2, ncon=niter-1
! Variables locales 
     INTEGER                                             :: i, j, n
     REAL                                                :: zref
     REAL                                                :: speed
     REAL                                                :: tasmin, qvsmin, tasmax, qvsmax
! tpot : temperature potentielle
     REAL                                                :: tpot
     REAL                                                :: zri1
     REAL                                                :: testar, qstar
     REAL                                                :: zdte, zdq   
     DOUBLE PRECISION, PARAMETER                         :: eps=1.0D-20
     REAL                                                :: delu, delte, delq
     REAL                                                :: u_zref, te_zref, q_zref  
     REAL                                                :: temp, pref
     LOGICAL                                             :: okri
     REAL                                                :: u_zref_p, te_zref_p, temp_p, q_zref_p
!convergence
     REAL                                                :: te_zref_con, q_zref_con
     REAL                                                :: u_zref_c, te_zref_c, temp_c, q_zref_c
     REAL                                                :: ok_pred, ok_corr
!conversion humidite specifique
     REAL                                                :: qs
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!! Variables
! u1: eastward wind speed at the first atmospheric layer [ms-1]
! v1: northward wind speed at the first atmospheric layer [ms-1]
! t1: air temperature at the first atmospheric layer [K]
! q1: water vapour mixing ratio at the first atmospheric layer [kgkg-1]
! z1: geopotential at the first atmospheric layer [m]
! ps1: air pressure at the first atmospheric layer [Pa]
! ts1: surface temperature [K]
! qsurf: surface water vapour [kgkg-1]
! rugos: rugossity [-]
! psol: surface pressure [Pa]
! cdrah: drag coefficien for heat [-]
! cdram: drag coefficient for momentum [-]
! ri1: Richardson number at the first atmospheric layer [-]
! ustar: U* in similarity theroy [ms-1]
! lmon: Monin-Obukhov length [-]

     speed=SQRT(u1(kts)**2+v1(kts)**2)
!
     okri=.TRUE.

     CALL coefcdrag1D(speed, t1(kts), q1, z1, psol, &
 &                   ts1, qsurf, rugos, okri, ri1, &         
 &                   zri1, pref)
 
!
!---------Star variables----------------------------------------------------
!

     tpot = t1(kts)* (psol/ps1(kts))**rcp
     zdte = tpot - ts1
     zdq = max(q1,0.0) - max(qsurf,0.0)
     zdte = sign(max(abs(zdte),1.e-10),zdte)
!
     testar = (cdrah * zdte * speed)/ustar
     qstar = (cdrah * zdq * speed)/ustar
!
!----------First aproximation of variables at zref --------------------------
     zref = 2.0
     CALL screenp1D(speed, tpot, q1, &
 &                 ts1, qsurf, rugos, lmon, &
 &                 ustar, testar, qstar, zref, &
 &                 delu, delte, delq)
    
!
     u_zref = delu
     q_zref = max(qsurf,0.0) + delq
     te_zref = ts1 + delte
     temp = te_zref * (psol/ps1(kts))**(-rcp)
     q_zref_p = q_zref
!     te_zref_p = te_zref
     temp_p = temp
!
! Iteration of the variables at the reference level zref : corrector calculation ; see Hess & McAvaney, 1995 
!
     DO n = 1, niter
!
       okri=.TRUE.
       CALL screenc1D(u_zref, temp, q_zref, zref, &
 &                   ts1, qsurf, rugos, psol, &           
 &                   ustar, testar, qstar, okri, ri1, &
 &                   pref, delu, delte, delq,          &
 &                   cdram, cdrah)
!
       u_zref = delu
       q_zref = delq + max(qsurf,0.0)
       te_zref = delte + ts1 
       !
       ! return to normal temperature
       !
       temp = te_zref * (psol/pref)**(-rcp)
       IF (n.EQ.ncon) THEN
         te_zref_con = te_zref
         q_zref_con = q_zref
       ENDIF 
!
     ENDDO 

     q_zref_c = q_zref
     temp_c = temp

     ok_pred=0.
     ok_corr=1.

     t_2m = temp_p * ok_pred + temp_c * ok_corr
     q_2m = q_zref_p * ok_pred + q_zref_c * ok_corr
     qs = pq0/psol*exp(a2*(t_2m-a3)/(t_2m-a4))         
     q_2m = MIN(qs, q_2m)
     
     ! Capping the diagnostics to prevent large values
     tasmin = MIN(t1(kts), ts1)
     qvsmin = MIN(q1, qsurf)
     tasmax = MAX(t1(kts), ts1)
     qvsmax = MAX(q1, qsurf)
     
     IF (t_2m < tasmin) t_2m = tasmin
     IF (q_2m < qvsmin) q_2m = qvsmin
     IF (t_2m > tasmax) t_2m = tasmin
     IF (q_2m > qvsmax) q_2m = qvsmax
     
     RETURN

   END subroutine SFCDIAGS_ORCHIDEE1D

!=======================================================================
!
!
!
    SUBROUTINE coefcdrag1D (speed, t, q, zgeop, psol,                                                 &
                            ts, qsurf, rugos, okri, ri1,                                              &
                            zri1, pref)

      IMPLICIT none
!-------------------------------------------------------------------------
! Objet : calcul des cdrags pour le moment (cdram) et les flux de chaleur 
!         sensible et latente (cdrah), du cdrag neutre (cdran), 
!         du nombre de Richardson entre la surface et le niveau de reference 
!         (zri1) et de la pression au niveau de reference (pref).    
!
! I. Musat, 01.07.2002
!-------------------------------------------------------------------------
!
! speed---input-R- module du vent au 1er niveau du modele
! t-------input-R- temperature de l'air au 1er niveau du modele
! q-------input-R- humidite de l'air au 1er niveau du modele
! zgeop---input-R- geopotentiel au 1er niveau du modele
! psol----input-R- pression au sol 
! ts------input-R- temperature de l'air a la surface
! qsurf---input-R- humidite de l'air a la surface
! rugos---input-R- rugosite
! okri----input-L- TRUE si on veut tester le nb. Richardson entre la sfce 
!                  et zref par rapport au Ri entre la sfce et la 1ere couche
! ri1-----input-R- nb. Richardson entre la surface et la 1ere couche
!
! zri1---output-R- nb. Richardson entre la surface et la couche zgeop/g
! pref---output-R- pression au niveau zgeop/g

      REAL, INTENT(in)                                   :: speed, t, q, zgeop, psol
      REAL, INTENT(in)                                   :: ts, qsurf, rugos, ri1 
      LOGICAL, INTENT(in)                                :: okri    
!
      REAL, INTENT(out)                                  :: zri1, pref
!-------------------------------------------------------------------------
!
!      include "YOMCST.h"
!      include "YOETHF.h"
!      INCLUDE "clesphys.h"
! Quelques constantes :
      REAL    , PARAMETER                                :: RVTMP2    = 4*rvovrd/3.5 -1
      REAL, parameter                                    :: CE=5.0, CC=5.0, CD=5.0, cepdu2=(0.1)**2
!
! Variables locales :
      INTEGER                                            :: i, j
      REAL                                               :: zdu2, zdphi, ztsolv, ztvd
      REAL                                               :: zscf, friv, frih, zucf, zcr
      REAL                                               :: zcfm1, zcfh1
      REAL                                               :: zcfm2, zcfh2
      REAL                                               :: trm0, trm1
 
!-------------------------------------------------------------------------

      zdphi = zgeop
      zdu2 = max(cepdu2,speed**2)
      pref = exp(log(psol) - zdphi/(r_d*t* &
        (1.+ p608 * max(q,0.0))))
      ztsolv = ts
!       ztvd = t * (psol/pref)**rcp
!       ztvd = (t+zdphi/cp/(1.+RVTMP2*q)) &
!          *(1.+p608*q)
      ztvd = (t+zdphi/cp/(1.+RVTMP2*q))
      trm0 = 1. + p608 * max(qsurf,0.0)
      trm1 = 1. + p608 * max(q,0.0)
      ztsolv = ztsolv * trm0
      ztvd = ztvd * trm1
      zri1 = zdphi*(ztvd-ztsolv)/(zdu2*ztvd)
!
! on teste zri1 par rapport au Richardson de la 1ere couche ri1 
!

!IM +++
      IF(1.EQ.0) THEN
      IF (okri) THEN
        IF (ri1.GE.0.0.AND.zri1.LT.0.0) THEN
          zri1 = ri1
        ELSE IF(ri1.LT.0.0.AND.zri1.GE.0.0) THEN
          zri1 = ri1
        ENDIF 
      ENDIF
      ENDIF
!IM ---
! 
      RETURN 

    END SUBROUTINE coefcdrag1D
!=======================================================================
    SUBROUTINE screenp1D(speed, tair, qair,                                                           &
     &                   ts, qsurf, rugos, lmon,                                                      &
     &                   ustar, testar, qstar, zref,                                                  &
     &                   delu, delte, delq)


      IMPLICIT none
!-------------------------------------------------------------------------
!
! Objet : calcul "predicteur" des anomalies du vent, de la temperature 
!         potentielle et de l'humidite specifique au niveau de reference zref et 
!         par rapport au 1er niveau (pour u) ou a la surface (pour theta et q) 
!         a partir des relations de Dyer-Businger.
!
! Reference : Hess, Colman et McAvaney (1995)
!
! I. Musat, 01.07.2002
!-------------------------------------------------------------------------
!
! speed---input-R- module du vent au 1er niveau du modele
! tair----input-R- temperature de l'air au 1er niveau du modele
! qair----input-R- humidite specifique au 1er niveau du modele
! ts------input-R- temperature de l'air a la surface
! qsurf---input-R- humidite specifique a la surface
! rugos---input-R- rugosite
! lmon----input-R- longueur de Monin-Obukov
! ustar---input-R- facteur d'echelle pour le vent
! testar--input-R- facteur d'echelle pour la temperature potentielle
! qstar---input-R- facteur d'echelle pour l'humidite 
! zref----input-R- altitude de reference
!
! delu----input-R- anomalie du vent par rapport au 1er niveau
! delte---input-R- anomalie de la temperature potentielle par rapport a la surface
! delq----input-R- anomalie de l'humidite par rapport a la surface

      REAL, INTENT(in)                                   :: speed, tair, qair
      REAL, INTENT(in)                                   :: ts, qsurf, rugos
      REAL, INTENT(in)                                   :: lmon
      REAL, INTENT(in)                                   :: ustar, testar, qstar
      REAL, INTENT(in)                                   :: zref
!
      REAL, INTENT(out)                                  :: delu, delte, delq
!
!-------------------------------------------------------------------------
! Variables locales et constantes :
      INTEGER                                            :: i, j
      REAL                                               :: xtmp, xtmp0
!-------------------------------------------------------------------------
!
      IF (lmon.GE.0.) THEN
!
! STABLE CASE
!
        IF (speed.GT.1.5.AND.lmon.LE.1.0 .AND. rugos.LE.1.0) THEN
          delu = (ustar/KARMAN)* &
            (log(zref/(rugos)+1.) + &
            min(5.d0, 5.0 *(zref - rugos)/lmon))
          delte = (testar/KARMAN)* &
            (log(zref/(rugos)+1.) + &
            min(5.d0, 5.0 * (zref - rugos)/lmon))
          delq = (qstar/KARMAN)* &
            (log(zref/(rugos)+1.) + &
            min(5.d0, 5.0 * (zref - rugos)/lmon))
        ELSE
          delu  = 0.1 * speed
          delte = 0.1 * (tair - ts )
          delq  = 0.1 * (max(qair,0.0) - max(qsurf,0.0))
        ENDIF
      ELSE  
!
! UNSTABLE CASE
!
        IF (speed.GT.5.0.AND.abs(lmon).LE.50.0) THEN
          xtmp = (1. - 16. * (zref/lmon))**(1./4.)
          xtmp0 = (1. - 16. * (rugos/lmon))**(1./4.)
          delu = (ustar/KARMAN)* &
            (log(zref/(rugos)+1.) & 
            - 2.*log(0.5*(1. + xtmp)) &
            + 2.*log(0.5*(1. + xtmp0)) &
            - log(0.5*(1. + xtmp*xtmp)) &
            + log(0.5*(1. + xtmp0*xtmp0)) &
            + 2.*atan(xtmp) - 2.*atan(xtmp0))
          delte = (testar/KARMAN)* &
            (log(zref/(rugos)+1.) &
            - 2.0 * log(0.5*(1. + xtmp*xtmp)) & 
            + 2.0 * log(0.5*(1. + xtmp0*xtmp0)))
          delq  = (qstar/KARMAN)* &
            (log(zref/(rugos)+1.) &
            - 2.0 * log(0.5*(1. + xtmp*xtmp)) & 
            + 2.0 * log(0.5*(1. + xtmp0*xtmp0)))
        ELSE
          delu  = 0.5 * speed
          delte = 0.5 * (tair - ts )
          delq  = 0.5 * (max(qair,0.0) - max(qsurf,0.0))
        ENDIF
      ENDIF
!
!
      RETURN

    END SUBROUTINE screenp1D
      
!=======================================================================
    SUBROUTINE screenc1D(speed, temp, q_zref, zref,                                                   &
                         ts, qsurf, rugos, psol,                                                      &
                         ustar, testar, qstar, okri, ri1,                                             &
                         pref, delu, delte, delq,                                                     &
                         cdram, cdrah)

      IMPLICIT NONE
!-----------------------------------------------------------------------
! 
! Objet : calcul "correcteur" des anomalies du vent, de la temperature 
!         potentielle et de l'humidite specifique au niveau de reference zref et 
!         par rapport au 1er niveau (pour u) ou a la surface (pour theta et q) 
!         a partir des equations de Louis.
!
! Reference : Hess, Colman et McAvaney (1995)
!
! I. Musat, 01.07.2002
!-----------------------------------------------------------------------
!
! speed---input-R- module du vent au 1er niveau du modele
! temp----input-R- temperature de l'air au 1er niveau du modele
! q_zref--input-R- humidite specifique au 1er niveau du modele
! zref----input-R- altitude de reference
! ts------input-R- temperature de l'air a la surface
! qsurf---input-R- humidite specifique a la surface
! rugos---input-R- rugosite
! psol----input-R- pression au sol
! ustar---input-R- facteur d'echelle pour le vent
! testar--input-R- facteur d'echelle pour la temperature potentielle
! qstar---input-R- facteur d'echelle pour l'humidite
! okri----input-L- TRUE si on veut tester le nb. Richardson entre la sfce 
!                  et zref par rapport au Ri entre la sfce et la 1ere couche
! ri1-----input-R- nb. Richardson entre la surface et la 1ere couche 
!
! pref----input-R- pression au niveau de reference
! delu----input-R- anomalie du vent par rapport au 1er niveau
! delte---input-R- anomalie de la temperature potentielle par rapport a la surface
! delq----input-R- anomalie de l'humidite par rapport a la surface

      LOGICAL, INTENT(in)                                :: okri 
      REAL, INTENT(in)                                   :: speed, temp, q_zref
      REAL, INTENT(in)                                   :: zref
      REAL, INTENT(in)                                   :: ts, qsurf, rugos, psol
      REAL, INTENT(in)                                   :: ustar, testar, qstar, ri1 
      REAL, INTENT(in)                                   :: cdram, cdrah 
!
      REAL, INTENT(out)                                  :: pref, delu, delte, delq 
!-----------------------------------------------------------------------
!      include "YOMCST.h"
!
! Variables locales  
      INTEGER                                            :: i, j 
      REAL                                               :: zri1, gref
!
!------------------------------------------------------------------------- 
      gref = zref*g
!
! Richardson at reference level 
!
      CALL coefcdrag1D(speed, temp, q_zref, gref, psol, &
 &                   ts, qsurf, rugos, okri, ri1,  &         
 &                   zri1, pref)
              !!!!!!!!!!!!!L'update des coefficients de trainee a ete enleve
!                      
      delu = ustar/sqrt(cdram)
      delte= (testar* sqrt(cdram))/ cdrah
      delq= (qstar* sqrt(cdram))/ cdrah
!
      RETURN 

    END SUBROUTINE screenc1D
!=======================================================================

END MODULE module_diagvar_cordex
