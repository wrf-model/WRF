! PsychroLib version 2.1.1 (https://github.com/psychrometrics/psychrolib)
! Copyright (c) 2018 D. Thevenard and D. Meyer for the current library implementation
! Copyright (c) 2017 ASHRAE Handbook — Fundamentals for ASHRAE equations and coefficients
! Licensed under the MIT License.

! Please cite paper as https://doi.org/10.21105/joss.01137.
! For version-specific citation see also https://doi.org/10.5281/zenodo.2537945.

module module_sf_psychrolib
  !+ Module overview
  !+  Contains functions for calculating thermodynamic properties of gas-vapor mixtures
  !+  and standard atmosphere suitable for most engineering, physical, and meteorological
  !+  applications.
  !+
  !+  Most of the functions are an implementation of the formulae found in the
  !+  2017 ASHRAE Handbook - Fundamentals, in both International System (SI),
  !+  and Imperial (IP) units. Please refer to the information included in
  !+  each function for their respective reference.
  !+
  !+ Example
  !+  use psychrolib, only: GetTDewPointFromRelHum, SetUnitSystem, SI
  !+  ! Set the unit system, for example to SI (can be either 'SI' or 'IP')
  !+  call SetUnitSystem(SI)
  !+  ! Calculate the dew point temperature for a dry bulb temperature of 25 C and a relative humidity of 80%
  !+  print *, GetTDewPointFromRelHum(25.0, 0.80)
  !+ 21.3094
  !+
  !+ Copyright
  !+  - For the current library implementation
  !+     Copyright (c) 2018 D. Thevenard and D. Meyer.
  !+  - For equations and coefficients published ASHRAE Handbook — Fundamentals, Chapter 1
  !+     Copyright (c) 2017 ASHRAE Handbook — Fundamentals (https://www.ashrae.org)
  !+
  !+ License
  !+  MIT (https://github.com/psychrometrics/psychrolib/LICENSE.txt)
  !+
  !+ Note from the Authors
  !+  We have made every effort to ensure that the code is adequate, however, we make no
  !+  representation with respect to its accuracy. Use at your own risk. Should you notice
  !+  an error, or if you have a suggestion, please notify us through GitHub at
  !+  https://github.com/psychrometrics/psychrolib/issues.


  implicit none

  private
  public :: IP
  public :: SI
  public :: SetUnitSystem
  public :: GetUnitSystem
  public :: isIP
  public :: GetTRankineFromTFahrenheit
  public :: GetTKelvinFromTCelsius
  public :: GetTWetBulbFromTDewPoint
  public :: GetTWetBulbFromRelHum
  public :: GetRelHumFromTDewPoint
  public :: GetRelHumFromTWetBulb
  public :: GetTDewPointFromRelHum
  public :: GetTDewPointFromTWetBulb
  public :: GetVapPresFromRelHum
  public :: GetRelHumFromVapPres
  public :: GetTDewPointFromVapPres
  public :: GetVapPresFromTDewPoint
  public :: GetTWetBulbFromHumRatio
  public :: GetHumRatioFromTWetBulb
  public :: GetHumRatioFromRelHum
  public :: GetRelHumFromHumRatio
  public :: GetHumRatioFromTDewPoint
  public :: GetTDewPointFromHumRatio
  public :: GetHumRatioFromVapPres
  public :: GetVapPresFromHumRatio
  public :: GetDryAirEnthalpy
  public :: GetDryAirDensity
  public :: GetDryAirVolume
  public :: GetTDryBulbFromEnthalpyAndHumRatio
  public :: GetHumRatioFromEnthalpyAndTDryBulb
  public :: GetSatVapPres
  public :: GetSatHumRatio
  public :: GetSatAirEnthalpy
  public :: GetVaporPressureDeficit
  public :: GetDegreeOfSaturation
  public :: GetMoistAirEnthalpy
  public :: GetMoistAirVolume
  public :: GetMoistAirDensity
  public :: GetStandardAtmPressure
  public :: GetStandardAtmTemperature
  public :: GetSeaLevelPressure
  public :: GetStationPressure
  public :: GetSpecificHumFromHumRatio
  public :: GetHumRatioFromSpecificHum
  public :: CalcPsychrometricsFromTWetBulb
  public :: CalcPsychrometricsFromTDewPoint
  public :: CalcPsychrometricsFromRelHum
  public :: dLnPws_


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Global constants
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  real, parameter ::  R_DA_IP = 53.350
    !+ Universal gas constant for dry air (IP version) in ft lb_Force lb_DryAir⁻¹ R⁻¹.
    !+ Reference:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1.

  real, parameter ::  R_DA_SI = 287.042
    !+ Universal gas constant for dry air (SI version) in J kg_DryAir⁻¹ K⁻¹.
    !+ Reference:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1.

  integer, parameter :: IP = 1
  integer, parameter :: SI = 2

  integer  :: PSYCHROLIB_UNITS = 0 ! 0 = undefined.
    !+ Unit system to use.

  real ::  PSYCHROLIB_TOLERANCE = 1.0
    !+ Tolerance of temperature calculations.

  integer, parameter  :: MAX_ITER_COUNT = 100
    !+ Maximum number of iterations before exiting while loops.

  real, parameter  :: MIN_HUM_RATIO = 1e-7
    !+ Minimum acceptable humidity ratio used/returned by any functions.
    !+ Any value above 0 or below the MIN_HUM_RATIO will be reset to this value.

  real, parameter  :: FREEZING_POINT_WATER_IP = 32.0
    !+ float: Freezing point of water in Fahrenheit.

  real, parameter  :: FREEZING_POINT_WATER_SI = 0.0
    !+ float: Freezing point of water in Celsius.

  real, parameter  :: TRIPLE_POINT_WATER_IP = 32.018
    !+ float: Triple point of water in Fahrenheit.

  real, parameter  :: TRIPLE_POINT_WATER_SI = 0.01
    !+ float: Triple point of water in Celsius.


  contains


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Helper functions
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetUnitSystem(UnitSystem)
  !+ Set the system of units to use (SI or IP).
  !+ Notes: this function *HAS TO BE CALLED* before the library can be used

    integer, intent(in)    :: UnitSystem
    !+ Units: string indicating the system of units chosen (SI or IP)

    if (.not. (UnitSystem == SI .or. UnitSystem == IP)) then
      error stop "The system of units has to be either SI or IP."
    end if

    PSYCHROLIB_UNITS = UnitSystem

    ! Define tolerance on temperature calculations
    ! The tolerance is the same in IP and SI
    if (UnitSystem == IP) then
      PSYCHROLIB_TOLERANCE = 0.001 * 9.0 / 5.0
    else
      PSYCHROLIB_TOLERANCE = 0.001
    end if
  end subroutine SetUnitSystem

  function GetUnitSystem() result(UnitSystem)
    !+ Return the system of units in use.
    integer :: UnitSystem
    UnitSystem = PSYCHROLIB_UNITS
  end function GetUnitSystem

  function isIP()
    !+ Check whether the system in use is IP or SI
    logical :: isIP
    if (PSYCHROLIB_UNITS == IP) then
      isIP = .true.
    else if (PSYCHROLIB_UNITS == SI) then
      isIP = .false.
    else
      error stop "The system of units has not been defined."
    end if
  end function isIP


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Conversion between temperature units
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function GetTRankineFromTFahrenheit(TFahrenheit) result(TRankine)
    real, intent(in)  :: TFahrenheit
      !+ Temperature in degree Fahrenheit
    real              :: TRankine
      !+ Temperature in degree Rankine
    real              :: ZERO_FAHRENHEIT_AS_RANKINE
      !+ Zero degree Fahrenheit (°F) expressed as degree Rankine (°R)

    ZERO_FAHRENHEIT_AS_RANKINE = 459.67

    TRankine = TFahrenheit + ZERO_FAHRENHEIT_AS_RANKINE
  end function GetTRankineFromTFahrenheit

  function GetTKelvinFromTCelsius(TCelsius) result(TKelvin)
    real, intent(in)  :: TCelsius
      !+ Temperature in degree Celsius
    real              :: TKelvin
      !+ Tempearatyre in Kelvin
    real              :: ZERO_CELSIUS_AS_KELVIN
      ! Zero degree Fahrenheit (°F) expressed as Kelvin (K)

    ZERO_CELSIUS_AS_KELVIN = 273.15

    TKelvin = TCelsius + ZERO_CELSIUS_AS_KELVIN
  end function GetTKelvinFromTCelsius


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Conversions between dew point, wet bulb, and relative humidity
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function GetTWetBulbFromTDewPoint(TDryBulb, TDewPoint, Pressure) result(TWetBulb)
    !+ Return wet-bulb temperature given dry-bulb temperature, dew-point temperature, and pressure.
    !+ References:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1

    real, intent(in)  :: TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  :: TDewPoint
      !+ Dew-point temperature in °F [IP] or °C [SI]
    real, intent(in)  :: Pressure
      !+ Atmospheric pressure in Psi [IP] or Pa [SI]
    real              :: TWetBulb
      !+ Wet-bulb temperature in °F [IP] or °C [SI]
    real              :: HumRatio
      !+ Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]

    if (TDewPoint > TDryBulb) then
      error stop "Error: dew point temperature is above dry bulb temperature"
    end if

    HumRatio = GetHumRatioFromTDewPoint(TDewPoint, Pressure)
    TWetBulb = GetTWetBulbFromHumRatio(TDryBulb, HumRatio, Pressure)
  end function GetTWetBulbFromTDewPoint

  function GetTWetBulbFromRelHum(TDryBulb, RelHum, Pressure) result(TWetBulb)
    !+ Return wet-bulb temperature given dry-bulb temperature, relative humidity, and pressure.
    !+ References:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  RelHum
      !+ Relative humidity in range [0, 1]
    real, intent(in)  ::  Pressure
      !+ Atmospheric pressure in Psi [IP] or Pa [SI]
    real              ::  TWetBulb
      !+ Wet-bulb temperature in °F [IP] or °C [SI]
    real              ::  HumRatio
      !+ Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]

    if (RelHum < 0.0 .or. RelHum > 1.0) then
      error stop "Error: relative humidity is outside range [0,1]"
    end if

    HumRatio = GetHumRatioFromRelHum(TDryBulb, RelHum, Pressure)
    TWetBulb = GetTWetBulbFromHumRatio(TDryBulb, HumRatio, Pressure)
  end function GetTWetBulbFromRelHum

  function GetRelHumFromTDewPoint(TDryBulb, TDewPoint) result(RelHum)
    !+ Return relative humidity given dry-bulb temperature and dew-point temperature.
    !+ References:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 22

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  TDewPoint
      !+ Dew-point temperature in °F [IP] or °C [SI]
    real              ::  RelHum
      !+ Relative humidity in range [0, 1]
    real              ::  VapPres
      !+ Partial pressure of water vapor in moist air in Psi [IP] or Pa [SI]
    real              ::  SatVapPres
      !+ Vapor pressure of saturated air in Psi [IP] or Pa [SI]

    if (TDewPoint > TDryBulb) then
      error stop "Error: dew point temperature is above dry bulb temperature"
    end if

    VapPres     = GetSatVapPres(TDewPoint)
    SatVapPres  = GetSatVapPres(TDryBulb)
    RelHum      = VapPres / SatVapPres
  end function GetRelHumFromTDewPoint

  function GetRelHumFromTWetBulb(TDryBulb, TWetBulb, Pressure) result(RelHum)
    !+ Return relative humidity given dry-bulb temperature, wet bulb temperature and pressure.
    !+ References:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  TWetBulb
      !+ Wet-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  Pressure
      !+ Atmospheric pressure in Psi [IP] or Pa [SI]
    real              ::  RelHum
      !+ Relative humidity in range [0, 1]
    real              ::  HumRatio
      !+ Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]

    if (TWetBulb > TDryBulb) then
      error stop "Error: wet bulb temperature is above dry bulb temperature"
    end if

    HumRatio = GetHumRatioFromTWetBulb(TDryBulb, TWetBulb, Pressure)
    RelHum   = GetRelHumFromHumRatio(TDryBulb, HumRatio, Pressure)
  end function GetRelHumFromTWetBulb

  function GetTDewPointFromRelHum(TDryBulb, RelHum) result(TDewPoint)
    !+ Return dew-point temperature given dry-bulb temperature and relative humidity.
    !+ References:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  RelHum
      !+ Relative humidity in range [0, 1]
    real              ::  TDewPoint
      !+ Dew-point temperature in °F [IP] or °C [SI]
    real              ::  VapPres
      !+ Partial pressure of water vapor in moist air in Psi [IP] or Pa [SI]

    if (RelHum < 0.0 .or. RelHum > 1.0) then
      error stop "Error: relative humidity is outside range [0,1]"
    end if

    VapPres   = GetVapPresFromRelHum(TDryBulb, RelHum)
    TDewPoint = GetTDewPointFromVapPres(TDryBulb, VapPres)
  end function GetTDewPointFromRelHum

  function GetTDewPointFromTWetBulb(TDryBulb, TWetBulb, Pressure) result(TDewPoint)
    !+ Return dew-point temperature given dry-bulb temperature, wet-bulb temperature, and pressure.
    !+ References:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  TWetBulb
      !+ Wet-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  Pressure
      !+ Atmospheric pressure in Psi [IP] or Pa [SI]
    real              ::  TDewPoint
      !+ Dew-point temperature in °F [IP] or °C [SI]
    real              ::  HumRatio
      !+ Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]

    if (TWetBulb > TDryBulb) then
      error stop "Error: wet bulb temperature is above dry bulb temperature"
    end if

    HumRatio  = GetHumRatioFromTWetBulb(TDryBulb, TWetBulb, Pressure)
    TDewPoint = GetTDewPointFromHumRatio(TDryBulb, HumRatio, Pressure)
  end function GetTDewPointFromTWetBulb


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Conversions between dew point, or relative humidity and vapor pressure
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function GetVapPresFromRelHum(TDryBulb, RelHum) result(VapPres)
    !+ Return partial pressure of water vapor as a function of relative humidity and temperature.
    !+ References:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 12, 22

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  RelHum
      !+ Relative humidity in range [0, 1]
    real              ::  VapPres
      !+ Partial pressure of water vapor in moist air in Psi [IP] or Pa [SI]

    if (RelHum < 0.0 .or. RelHum > 1.0) then
      error stop "Error: relative humidity is outside range [0,1]"
    end if

    VapPres = RelHum * GetSatVapPres(TDryBulb)
  end function GetVapPresFromRelHum

  function GetRelHumFromVapPres(TDryBulb, VapPres) result(RelHum)
    !+ Return relative humidity given dry-bulb temperature and vapor pressure.
    !+ References:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 12, 22

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  VapPres
      !+ Partial pressure of water vapor in moist air in Psi [IP] or Pa [SI]
    real              ::  RelHum
      !+ Relative humidity in range [0, 1]

    if (VapPres < 0.0) then
      error stop "Error: partial pressure of water vapor in moist air cannot be negative"
    end if

    RelHum = VapPres / GetSatVapPres(TDryBulb)
  end function GetRelHumFromVapPres

  function dLnPws_(TDryBulb) result(dLnPws)
    !+ Helper function returning the derivative of the natural log of the saturation vapor pressure
    !+ as a function of dry-bulb temperature.
    !+ Reference:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1  eqn 5

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real              ::  dLnPws
      !+ Derivative of natural log of vapor pressure of saturated air in Psi [IP] or Pa [SI]
    real              ::  T
      !+ Dry bulb temperature in R [IP] or K [SI]

    if (isIP()) then

      T = GetTRankineFromTFahrenheit(TDryBulb)

      if (TDryBulb <= TRIPLE_POINT_WATER_IP) then
        dLnPws = 1.0214165E+04 / T**2 - 5.3765794E-03 + 2 * 1.9202377E-07 * T &
                 + 2 * 3.5575832E-10 * T**2 - 4 * 9.0344688E-14 * T**3 + 4.1635019 / T
      else
        dLnPws = 1.0440397E+04 / T**2 - 2.7022355E-02 + 2 * 1.2890360E-05 * T &
                 - 3 * 2.4780681E-09 * T**2 + 6.5459673 / T
      end if

    else

      T = GetTKelvinFromTCelsius(TDryBulb)

      if (TDryBulb <= TRIPLE_POINT_WATER_SI) then
        dLnPws = 5.6745359E+03 / T**2 - 9.677843E-03 + 2 * 6.2215701E-07 * T &
                 + 3 * 2.0747825E-09 * T**2 - 4 * 9.484024E-13 * T**3 + 4.1635019 / T
      else
        dLnPws = 5.8002206E+03 / T**2 - 4.8640239E-02 + 2 * 4.1764768E-05 * T &
                 - 3 * 1.4452093E-08 * T**2 + 6.5459673 / T
      end if
    end if
  end function dLnPws_

  function GetTDewPointFromVapPres(TDryBulb, VapPres) result(TDewPoint)
    !+ Return dew-point temperature given dry-bulb temperature and vapor pressure.
    !+ References:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn. 5 and 6
    !+ Notes:
    !+ The dew point temperature is solved by inverting the equation giving water vapor pressure
    !+ at saturation from temperature rather than using the regressions provided
    !+ by ASHRAE (eqn. 37 and 38) which are much less accurate and have a
    !+ narrower range of validity.
    !+ The Newton-Raphson (NR) method is used on the logarithm of water vapour
    !+ pressure as a function of temperature, which is a very smooth function
    !+ Convergence is usually achieved in 3 to 5 iterations.
    !+ TDryBulb is not really needed here, just used for convenience.

    real, intent(in)    ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)    ::  VapPres
      !+ Partial pressure of water vapor in moist air in Psi [IP] or Pa [SI]
    real                ::  TDewPoint
      !+ Dew-point temperature in °F [IP] or °C [SI]
    real                ::  lnVP
      !+ Natural logarithm of partial pressure of water vapor pressure in moist air
    real                ::  d_lnVP
      !+ Derivative of function, calculated numerically
    real                ::  lnVP_iter
      !+ Value of log of vapor water pressure used in NR calculation
    real                ::  TDewPoint_iter
      !+ Value of TDewPoint used in NR calculation
    real, dimension(2)  ::  BOUNDS
      !+ Valid temperature range in °F [IP] or °C [SI]
    integer             :: index
      !+ Index used in the calculation

    ! Bounds and step size as a function of the system of units
    if (isIP()) then
        BOUNDS(1) = -148.0
        BOUNDS(2) =  392.0
    else
        BOUNDS(1) = -100.0
        BOUNDS(2) =  200.0
    end if

    ! Validity check -- bounds outside which a solution cannot be found
    if (VapPres < GetSatVapPres(BOUNDS(1)) .or. VapPres > GetSatVapPres(BOUNDS(2))) then
      error stop "Error: partial pressure of water vapor is outside range of validity of equations"
    end if

    ! We use NR to approximate the solution.
    TDewPoint = TDryBulb
    lnVP = log(VapPres)
    index = 1

    do while (.true.)
      TDewPoint_iter = TDewPoint ! TDewPoint_iter used in NR calculation
      lnVP_iter = log(GetSatVapPres(TDewPoint_iter))

      ! Derivative of function, calculated analytically
      d_lnVP = dLnPws_(TDewPoint_iter)

      ! New estimate, bounded by the search domain defined above
      TDewPoint = TDewPoint_iter - (lnVP_iter - lnVP) / d_lnVP
      TDewPoint = max(TDewPoint, BOUNDS(1))
      TDewPoint = min(TDewPoint, BOUNDS(2))

      if (abs(TDewPoint - TDewPoint_iter) <= PSYCHROLIB_TOLERANCE) then
        exit
      end if

      if (index > MAX_ITER_COUNT) then
        error stop "Convergence not reached in GetTDewPointFromVapPres. Stopping."
      end if

      index = index + 1
    end do

  TDewPoint = min(TDewPoint, TDryBulb)
  end function GetTDewPointFromVapPres

  function GetVapPresFromTDewPoint(TDewPoint) result(VapPres)
    !+ Return vapor pressure given dew point temperature.
    !+ References:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 36

    real, intent(in)  ::  TDewPoint
      !+ Dew-point temperature in °F [IP] or °C [SI]
    real              ::  VapPres
      !+ Partial pressure of water vapor in moist air in Psi [IP] or Pa [SI]

    VapPres = GetSatVapPres(TDewPoint)
  end function GetVapPresFromTDewPoint


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Conversions from wet-bulb temperature, dew-point temperature, or relative humidity to humidity ratio
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function GetTWetBulbFromHumRatio(TDryBulb, HumRatio, Pressure) result(TWetBulb)
    !+ Return wet-bulb temperature given dry-bulb temperature, humidity ratio, and pressure.
    !+ References:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 33 and 35 solved for Tstar

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  HumRatio
      !+ Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
    real, intent(in)  ::  Pressure
      !+ Atmospheric pressure in Psi [IP] or Pa [SI]
    real              ::  TWetBulb
      !+ Wet-bulb temperature in °F [IP] or °C [SI]
    real              ::  TDewPoint
      !+ TDewPoint : Dew-point temperature in °F [IP] or °C [SI]
    real              ::  TWetBulbSup
      !+ Upper value of wet bulb temperature in bissection method (initial guess is from dry bulb temperature) in °F [IP] or °C [SI]
    real              ::  TWetBulbInf
      !+ Lower value of wet bulb temperature in bissection method (initial guess is from dew point temperature) in °F [IP] or °C [SI]
    real              ::  Wstar
      !+ Humidity ratio at temperature Tstar in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
    real              ::  BoundedHumRatio
      !+ Humidity ratio bounded to MIN_HUM_RATIO
    integer           ::  index
      !+ index used in iteration

    if (HumRatio < 0.0) then
      error stop "Error: humidity ratio cannot be negative"
    end if
    BoundedHumRatio = max(HumRatio, MIN_HUM_RATIO)

    TDewPoint = GetTDewPointFromHumRatio(TDryBulb, BoundedHumRatio, Pressure)

    ! Initial guesses
    TWetBulbSup = TDryBulb
    TWetBulbInf = TDewPoint
    TWetBulb = (TWetBulbInf + TWetBulbSup) / 2.0

    index = 1
    ! Bisection loop
    do while ((TWetBulbSup - TWetBulbInf) > PSYCHROLIB_TOLERANCE)

    ! Compute humidity ratio at temperature Tstar
    Wstar = GetHumRatioFromTWetBulb(TDryBulb, TWetBulb, Pressure)

    ! Get new bounds
    if (Wstar > BoundedHumRatio) then
      TWetBulbSup = TWetBulb
    else
      TWetBulbInf = TWetBulb
    end if

    ! New guess of wet bulb temperature
    TWetBulb = (TWetBulbSup + TWetBulbInf) / 2.0

      if (index > MAX_ITER_COUNT) then
        error stop "Convergence not reached in GetTWetBulbFromHumRatio. Stopping."
      end if

    index = index + 1
    end do
  end function GetTWetBulbFromHumRatio

  function GetHumRatioFromTWetBulb(TDryBulb, TWetBulb, Pressure) result(HumRatio)
    !+ Return humidity ratio given dry-bulb temperature, wet-bulb temperature, and pressure.
    !+ References:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 33 and 35

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  TWetBulb
      !+ Wet-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  Pressure
      !+ Atmospheric pressure in Psi [IP] or Pa [SI]
    real              ::  HumRatio
      !+ Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
    real              ::  Wsstar
      !+ Humidity ratio at temperature Tstar in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]

    if (TWetBulb > TDryBulb) then
      error stop "Error: wet bulb temperature is above dry bulb temperature"
    end if

    Wsstar = GetSatHumRatio(TWetBulb, Pressure)

    if (isIP()) then
      if (TWetBulb >= FREEZING_POINT_WATER_IP) then
        HumRatio = ((1093.0 - 0.556 * TWetBulb) * Wsstar - 0.240 * (TDryBulb - TWetBulb))   &
                   / (1093.0 + 0.444 * TDryBulb - TWetBulb)
      else
        HumRatio = ((1220.0 - 0.04 * TWetBulb) * Wsstar - 0.240 * (TDryBulb - TWetBulb))    &
                   / (1220.0 + 0.444 * TDryBulb - 0.48 * TWetBulb)
      end if
    else
      if (TWetBulb >= FREEZING_POINT_WATER_SI) then
        HumRatio = ((2501.0 - 2.326 * TWetBulb) * Wsstar - 1.006 * (TDryBulb - TWetBulb))   &
                   / (2501.0 + 1.86 * TDryBulb - 4.186 * TWetBulb)
      else
           HumRatio = ((2830.0 - 0.24 * TWetBulb) * Wsstar - 1.006 * (TDryBulb - TWetBulb)) &
                      / (2830.0 + 1.86 * TDryBulb - 2.1 * TWetBulb)
      end if
    end if

    ! Validity check.
    HumRatio = max(HumRatio, MIN_HUM_RATIO)
  end function GetHumRatioFromTWetBulb

  function GetHumRatioFromRelHum(TDryBulb, RelHum, Pressure) result(HumRatio)
    !+ Return humidity ratio given dry-bulb temperature, relative humidity, and pressure.
    !+ Reference:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  RelHum
      !+ Relative humidity in range [0, 1]
    real, intent(in)  ::  Pressure
      !+ Atmospheric pressure in Psi [IP] or Pa [SI]
    real              ::  HumRatio
      !+ Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
    real              ::  VapPres
      !+ Partial pressure of water vapor in moist air in Psi [IP] or Pa [SI]

    if (RelHum < 0.0 .or. RelHum > 1.0) then
      error stop "Error: relative humidity is outside range [0,1]"
    end if

    VapPres   = GetVapPresFromRelHum(TDryBulb, RelHum)
    HumRatio  = GetHumRatioFromVapPres(VapPres, Pressure)
  end function GetHumRatioFromRelHum

  function GetRelHumFromHumRatio(TDryBulb, HumRatio, Pressure) result(RelHum)
  !+ Return relative humidity given dry-bulb temperature, humidity ratio, and pressure.
  !+ Reference:
  !+ ASHRAE Handbook - Fundamentals (2017) ch. 1

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  HumRatio
      !+ Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
    real, intent(in)  ::  Pressure
      !+ Atmospheric pressure in Psi [IP] or Pa [SI]
    real              ::  RelHum
      !+ Relative humidity in range [0, 1]
    real              ::  VapPres
      !+ Partial pressure of water vapor in moist air in Psi [IP] or Pa [SI]

    if (HumRatio < 0.0) then
      error stop "Error: humidity ratio cannot be negative"
    end if

    VapPres = GetVapPresFromHumRatio(HumRatio, Pressure)
    RelHum  = GetRelHumFromVapPres(TDryBulb, VapPres)
  end function GetRelHumFromHumRatio

  function GetHumRatioFromTDewPoint(TDewPoint, Pressure) result(HumRatio)
    !+ Return humidity ratio given dew-point temperature and pressure.
    !+ Reference:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1

    real, intent(in)  ::  TDewPoint
      !+ Dew-point temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  Pressure
      !+ Atmospheric pressure in Psi [IP] or Pa [SI]
    real              ::  HumRatio
      !+ Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
    real              ::  VapPres
      !+ Partial pressure of water vapor in moist air in Psi [IP] or Pa [SI]

    VapPres   = GetSatVapPres(TDewPoint)
    HumRatio  = GetHumRatioFromVapPres(VapPres, Pressure)
  end function GetHumRatioFromTDewPoint

  function GetTDewPointFromHumRatio(TDryBulb, HumRatio, Pressure) result(TDewPoint)
    !+ Return dew-point temperature given dry-bulb temperature, humidity ratio, and pressure.
    !+ Reference:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  HumRatio
      !+ Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
    real, intent(in)  ::  Pressure
      !+ Atmospheric pressure in Psi [IP] or Pa [SI]
    real              ::  TDewPoint
      !+ Dew-point temperature in °F [IP] or °C [SI]
    real              ::  VapPres
      !+ Partial pressure of water vapor in moist air in Psi [IP] or Pa [SI]

    if (HumRatio < 0.0) then
      error stop "Error: humidity ratio cannot be negative"
    end if

    VapPres   = GetVapPresFromHumRatio(HumRatio, Pressure)
    TDewPoint = GetTDewPointFromVapPres(TDryBulb, VapPres)
  end function GetTDewPointFromHumRatio


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Conversions between humidity ratio and vapor pressure
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function GetHumRatioFromVapPres(VapPres, Pressure) result(HumRatio)
    !+ Return humidity ratio given water vapor pressure and atmospheric pressure.
    !+ Reference:
    !+ ASHRAE Fundamentals (2005) ch. 6 eqn. 22;
    !+ ASHRAE Fundamentals (2009) ch. 1 eqn. 22.

    real, intent(in)  ::  VapPres
      !+ Partial pressure of water vapor in moist air in Psi [IP] or Pa [SI]
    real, intent(in)  ::  Pressure
      !+ Atmospheric pressure in Psi [IP] or Pa [SI]
    real              ::  HumRatio
      !+ Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]

    if (VapPres < 0.0) then
      error stop "Error: partial pressure of water vapor in moist air cannot be negative"
    end if

    HumRatio = 0.621945 * VapPres / (Pressure-VapPres)

    ! Validity check.
    HumRatio = max(HumRatio, MIN_HUM_RATIO)
  end function GetHumRatioFromVapPres

  function GetVapPresFromHumRatio(HumRatio, Pressure) result(VapPres)
    !+ Return vapor pressure given humidity ratio and pressure.
    !+ Reference:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 20 solved for pw

    real, intent(in)  ::  HumRatio
      !+ Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
    real, intent(in)  ::  Pressure
      !+ Atmospheric pressure in Psi [IP] or Pa [SI]
    real              ::  VapPres
      !+ Partial pressure of water vapor in moist air in Psi [IP] or Pa [SI]
    real              ::  BoundedHumRatio
      !+ Humidity ratio bounded to MIN_HUM_RATIO

    if (HumRatio < 0.0) then
      error stop "Error: humidity ratio is negative"
    end if
    BoundedHumRatio = max(HumRatio, MIN_HUM_RATIO)

    VapPres = Pressure * BoundedHumRatio / (0.621945 + BoundedHumRatio)
  end function GetVapPresFromHumRatio


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Conversions between humidity ratio and specific humidity
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function GetSpecificHumFromHumRatio(HumRatio) result(SpecificHum)
    !+ Return the specific humidity from humidity ratio (aka mixing ratio).
    !+ Reference:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 9b

    real, intent(in) :: HumRatio
      !+ Humidity ratio in lb_H₂O lb_Dry_Air⁻¹ [IP] or kg_H₂O kg_Dry_Air⁻¹ [SI]
    real             :: SpecificHum
      !+ Specific humidity in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
    real              ::  BoundedHumRatio
      !+ Humidity ratio bounded to MIN_HUM_RATIO

    if (HumRatio < 0.0) then
      print *, "Error: humidity ratio cannot be negative"
    end if
    BoundedHumRatio = max(HumRatio, MIN_HUM_RATIO)

    SpecificHum = BoundedHumRatio / (1.0 + BoundedHumRatio)
  end function GetSpecificHumFromHumRatio

  function GetHumRatioFromSpecificHum(SpecificHum) result(HumRatio)
    !+ Return the humidity ratio (aka mixing ratio) from specific humidity.
    !+ Reference:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 9b (solved for humidity ratio)

    real, intent(in)  :: SpecificHum
      !+ Specific humidity in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
    real              :: HumRatio
      !+ Humidity ratio in lb_H₂O lb_Dry_Air⁻¹ [IP] or kg_H₂O kg_Dry_Air⁻¹ [SI]

    if (SpecificHum < 0.0 .or. SpecificHum >= 1.0) then
      print *, "Error: specific humidity is outside range [0, 1["
    end if

    HumRatio = SpecificHum / (1.0 - SpecificHum)

    ! Validity check.
    HumRatio = max(HumRatio, MIN_HUM_RATIO)
  end function GetHumRatioFromSpecificHum


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Dry Air Calculations
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function GetDryAirEnthalpy(TDryBulb) result(DryAirEnthalpy)
    !+ Return dry-air enthalpy given dry-bulb temperature.
    !+ Reference:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 28

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real              ::  DryAirEnthalpy
      !+ Dry air enthalpy in Btu lb⁻¹ [IP] or J kg⁻¹ [SI]

    if (isIP()) then
      DryAirEnthalpy = 0.240 * TDryBulb
    else
      DryAirEnthalpy = 1006 * TDryBulb
    end if
  end function GetDryAirEnthalpy

  function GetDryAirDensity(TDryBulb, Pressure) result(DryAirDensity)
    !+ Return dry-air density given dry-bulb temperature and pressure.
    !+ Reference:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1
    !+ Notes:
    !+ Eqn 14 for the perfect gas relationship for dry air.
    !+ Eqn 1 for the universal gas constant.
    !+ The factor 144 in IP is for the conversion of Psi = lb in⁻² to lb ft⁻².

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  Pressure
      !+ Atmospheric pressure in Psi [IP] or Pa [SI]
    real              ::  DryAirDensity
      !+ Dry air density in lb ft⁻³ [IP] or kg m⁻³ [SI]

    if (isIP()) then
      DryAirDensity = (144 * Pressure) / R_DA_IP / GetTRankineFromTFahrenheit(TDryBulb)
    else
      DryAirDensity = Pressure / R_DA_SI / GetTKelvinFromTCelsius(TDryBulb)
    end if
  end function GetDryAirDensity

  function GetDryAirVolume(TDryBulb, Pressure) result(DryAirVolume)
    !+ Return dry-air volume given dry-bulb temperature and pressure.
    !+ Reference:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1
    !+ Notes:
    !+ Eqn 14 for the perfect gas relationship for dry air.
    !+ Eqn 1 for the universal gas constant.
    !+ The factor 144 in IP is for the conversion of Psi = lb in⁻² to lb ft⁻².

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  Pressure
      !+ Atmospheric pressure in Psi [IP] or Pa [SI]
    real              ::  DryAirVolume
      !+ Dry air volume in ft³ lb⁻¹ [IP] or in m³ kg⁻¹ [SI]

    if (isIP()) then
      DryAirVolume = GetTRankineFromTFahrenheit(TDryBulb) * R_DA_IP / (144 * Pressure)
    else
      DryAirVolume = GetTKelvinFromTCelsius(TDryBulb) * R_DA_SI / Pressure
    end if
  end function GetDryAirVolume

  function GetTDryBulbFromEnthalpyAndHumRatio(MoistAirEnthalpy, HumRatio) result(TDryBulb)
    !+ Return dry bulb temperature from enthalpy and humidity ratio.
    !+ Reference:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 30
    !+ Notes:
    !+ Based on the `GetMoistAirEnthalpy` function, rearranged for temperature.

    real, intent(in)  ::  MoistAirEnthalpy
      !+ Moist air enthalpy in Btu lb⁻¹ [IP] or J kg⁻¹
    real, intent(in)  ::  HumRatio
      !+ Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
    real              ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real              ::  BoundedHumRatio
      !+ Humidity ratio bounded to MIN_HUM_RATIO

    if (HumRatio < 0.0) then
      error stop "Error: humidity ratio is negative"
    end if
    BoundedHumRatio = max(HumRatio, MIN_HUM_RATIO)

    if (isIP()) then
      TDryBulb  = (MoistAirEnthalpy - 1061.0 * BoundedHumRatio) / (0.240 + 0.444 * BoundedHumRatio)
    else
      TDryBulb  = (MoistAirEnthalpy / 1000.0 - 2501.0 * BoundedHumRatio) / (1.006 + 1.86 * BoundedHumRatio)
    end if
  end function GetTDryBulbFromEnthalpyAndHumRatio

  function GetHumRatioFromEnthalpyAndTDryBulb(MoistAirEnthalpy, TDryBulb) result(HumRatio)
    !+ Return humidity ratio from enthalpy and dry-bulb temperature.
    !+ Reference:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 30
    !+ Notes:
    !+ Based on the `GetMoistAirEnthalpy` function, rearranged for humidity ratio.

    real, intent(in)  ::  MoistAirEnthalpy
      !+ Moist air enthalpy in Btu lb⁻¹ [IP] or J kg⁻¹
    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real              ::  HumRatio
      !+ Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]

    if (isIP()) then
      HumRatio  = (MoistAirEnthalpy - 0.240 * TDryBulb) / (1061.0 + 0.444 * TDryBulb)
    else
      HumRatio  = (MoistAirEnthalpy / 1000.0 - 1.006 * TDryBulb) / (2501.0 + 1.86 * TDryBulb)
    end if

    ! Validity check.
    HumRatio = max(HumRatio, MIN_HUM_RATIO)
  end function GetHumRatioFromEnthalpyAndTDryBulb


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Saturated Air Calculations
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function GetSatVapPres(TDryBulb) result(SatVapPres)
    !+ Return saturation vapor pressure given dry-bulb temperature.
    !+ Reference:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1  eqn 5
    !+ Important note: the ASHRAE formulae are defined above and below the freezing point but have
    !+ a discontinuity at the freezing point. This is a small inaccuracy on ASHRAE's part: the formulae
    !+ should be defined above and below the triple point of water (not the feezing point) in which case 
    !+ the discontinuity vanishes. It is essential to use the triple point of water otherwise function
    !+ GetTDewPointFromVapPres, which inverts the present function, does not converge properly around
    !+ the freezing point.

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real              ::  SatVapPres
      !+ Vapor pressure of saturated air in Psi [IP] or Pa [SI]
    real              ::  LnPws
      !+ Log of Vapor Pressure of saturated air (dimensionless)
    real              ::  T
      !+ Dry bulb temperature in R [IP] or K [SI]

    if (isIP()) then
      if (TDryBulb < -148.0 .or. TDryBulb > 392.0) then
        error stop "Error: dry bulb temperature must be in range [-148, 392]°F"
      end if

      T = GetTRankineFromTFahrenheit(TDryBulb)

      if (TDryBulb <= TRIPLE_POINT_WATER_IP) then
        LnPws = (-1.0214165E+04 / T - 4.8932428 - 5.3765794E-03 * T + 1.9202377E-07 * T**2    &
                + 3.5575832E-10 * T**3 - 9.0344688E-14 * T**4 + 4.1635019 * log(T))
      else
        LnPws = -1.0440397E+04 / T - 1.1294650E+01 - 2.7022355E-02* T + 1.2890360E-05 * T**2  &
                - 2.4780681E-09 * T**3 + 6.5459673 * log(T)
      end if

      else
        if (TDryBulb < -100.0 .or. TDryBulb > 200.0) then
          error stop "Error: dry bulb temperature must be in range [-100, 200]°C"
        end if

        T = GetTKelvinFromTCelsius(TDryBulb)

        if (TDryBulb <= TRIPLE_POINT_WATER_SI) then
          LnPws = -5.6745359E+03 / T + 6.3925247 - 9.677843E-03 * T + 6.2215701E-07 * T**2    &
                  + 2.0747825E-09 * T**3 - 9.484024E-13 * T**4 + 4.1635019 * log(T)
        else
          LnPws = -5.8002206E+03 / T + 1.3914993 - 4.8640239E-02 * T + 4.1764768E-05 * T**2   &
                  - 1.4452093E-08 * T**3 + 6.5459673 * log(T)
        end if
      end if

    SatVapPres = exp(LnPws)
  end function GetSatVapPres

  function GetSatHumRatio(TDryBulb, Pressure) result(SatHumRatio)
    !+ Return humidity ratio of saturated air given dry-bulb temperature and pressure.
    !+ Reference:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 36, solved for W

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  Pressure
      !+ Atmospheric pressure in Psi [IP] or Pa [SI]
    real              ::  SatHumRatio
      !+ Humidity ratio of saturated air in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
    real              ::  SatVaporPres
      !+ Vapor pressure of saturated air in in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]

    SatVaporPres  = GetSatVapPres(TDryBulb)
    SatHumRatio   = 0.621945 * SatVaporPres / (Pressure-SatVaporPres)

    ! Validity check.
    SatHumRatio = max(SatHumRatio, MIN_HUM_RATIO)
  end function GetSatHumRatio

  function GetSatAirEnthalpy(TDryBulb, Pressure) result(SatAirEnthalpy)
    !+ Return saturated air enthalpy given dry-bulb temperature and pressure.
    !+ Reference:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  Pressure
      !+ Atmospheric pressure in Psi [IP] or Pa [SI]
    real              ::  SatAirEnthalpy
      !+ Saturated air enthalpy in Btu lb⁻¹ [IP] or J kg⁻¹ [SI]

    SatAirEnthalpy = GetMoistAirEnthalpy(TDryBulb, GetSatHumRatio(TDryBulb, Pressure))
  end function GetSatAirEnthalpy


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Moist Air Calculations
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function GetVaporPressureDeficit(TDryBulb, HumRatio, Pressure) result(VaporPressureDeficit)
    !+ Return Vapor pressure deficit given dry-bulb temperature, humidity ratio, and pressure.
    !+ Reference:
    !+ Oke (1987) eqn 2.13a

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  HumRatio
      !+ Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
    real, intent(in)  ::  Pressure
      !+ Atmospheric pressure in Psi [IP] or Pa [SI]
    real              ::  VaporPressureDeficit
      !+ Vapor pressure deficit in Psi [IP] or Pa [SI]
    real              ::  RelHum
      !+ Relative humidity in range [0, 1]

    if (HumRatio < 0.0) then
      error stop "Error: humidity ratio is negative"
    end if

    RelHum = GetRelHumFromHumRatio(TDryBulb, HumRatio, Pressure)
    VaporPressureDeficit = GetSatVapPres(TDryBulb) * (1.0 - RelHum)
  end function GetVaporPressureDeficit

  function GetDegreeOfSaturation(TDryBulb, HumRatio, Pressure) result(DegreeOfSaturation)
    !+ Return the degree of saturation (i.e humidity ratio of the air / humidity ratio of the air at saturation
    !+ at the same temperature and pressure) given dry-bulb temperature, humidity ratio, and atmospheric pressure.
    !+ Reference:
    !+ ASHRAE Handbook - Fundamentals (2009) ch. 1 eqn 12
    !+ Notes:
    !+ This definition is absent from the 2017 Handbook. Using 2009 version instead.

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  HumRatio
      !+ Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
    real, intent(in)  ::  Pressure
      !+ Atmospheric pressure in Psi [IP] or Pa [SI]
    real              ::  DegreeOfSaturation
      !+ Degree of saturation in arbitrary unit
    real              ::  BoundedHumRatio
      !+ Humidity ratio bounded to MIN_HUM_RATIO

    if (HumRatio < 0.0) then
      error stop "Error: humidity ratio is negative"
    end if
    BoundedHumRatio = max(HumRatio, MIN_HUM_RATIO)

    DegreeOfSaturation = BoundedHumRatio / GetSatHumRatio(TDryBulb, Pressure)
  end function GetDegreeOfSaturation

  function GetMoistAirEnthalpy(TDryBulb, HumRatio) result(MoistAirEnthalpy)
    !+ Return moist air enthalpy given dry-bulb temperature and humidity ratio.
    !+ Reference:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 30

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  HumRatio
      !+ Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
    real              ::  MoistAirEnthalpy
      !+ Moist air enthalpy in Btu lb⁻¹ [IP] or J kg⁻¹
    real              ::  BoundedHumRatio
      !+ Humidity ratio bounded to MIN_HUM_RATIO

    if (HumRatio < 0.0) then
      error stop "Error: humidity ratio is negative"
    end if
    BoundedHumRatio = max(HumRatio, MIN_HUM_RATIO)

    if (isIP()) then
        MoistAirEnthalpy = 0.240 * TDryBulb + BoundedHumRatio * (1061.0 + 0.444 * TDryBulb)
    else
        MoistAirEnthalpy = (1.006 * TDryBulb + BoundedHumRatio * (2501.0 + 1.86 * TDryBulb)) * 1000.0
    end if
  end function GetMoistAirEnthalpy

  function GetMoistAirVolume(TDryBulb, HumRatio, Pressure) result(MoistAirVolume)
    !+ Return moist air specific volume given dry-bulb temperature, humidity ratio, and pressure.
    !+ Reference:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 26
    !+ Notes:
    !+ In IP units, R_DA_IP / 144 equals 0.370486 which is the coefficient appearing in eqn 26
    !+ The factor 144 is for the conversion of Psi = lb in⁻² to lb ft⁻².

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  HumRatio
      !+ Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
    real, intent(in)  ::  Pressure
      !+ Atmospheric pressure in Psi [IP] or Pa [SI]
    real              ::  MoistAirVolume
      !+ Specific volume of moist air in ft³ lb⁻¹ of dry air [IP] or in m³ kg⁻¹ of dry air [SI]
    real              ::  BoundedHumRatio
      !+ Humidity ratio bounded to MIN_HUM_RATIO

    if (HumRatio < 0.0) then
      error stop "Error: humidity ratio is negative"
    end if
    BoundedHumRatio = max(HumRatio, MIN_HUM_RATIO)

    if (isIP()) then
        MoistAirVolume = R_DA_IP * GetTRankineFromTFahrenheit(TDryBulb) * (1.0 + 1.607858 * BoundedHumRatio) / (144.0 * Pressure)
    else
        MoistAirVolume = R_DA_SI * GetTKelvinFromTCelsius(TDryBulb) * (1.0 + 1.607858 * BoundedHumRatio) / Pressure
    end if
  end function GetMoistAirVolume

  function GetMoistAirDensity(TDryBulb, HumRatio, Pressure) result(MoistAirDensity)
    !+ Return moist air density given humidity ratio, dry bulb temperature, and pressure.
    !+ Reference:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 11

    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)  ::  HumRatio
      !+ Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
    real, intent(in)  ::  Pressure
      !+ Atmospheric pressure in Psi [IP] or Pa [SI]
    real              ::  MoistAirDensity
      !+ Moist air density in lb ft⁻³ [IP] or kg m⁻³ [SI]
    real              ::  BoundedHumRatio
      !+ Humidity ratio bounded to MIN_HUM_RATIO

    if (HumRatio < 0.0) then
      error stop "Error: humidity ratio is negative"
    end if
    BoundedHumRatio = max(HumRatio, MIN_HUM_RATIO)

    MoistAirDensity = (1.0 + BoundedHumRatio) / GetMoistAirVolume(TDryBulb, BoundedHumRatio, Pressure)
  end function GetMoistAirDensity


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Standard atmosphere
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function GetStandardAtmPressure(Altitude) result(StandardAtmPressure)
    !+ Return standard atmosphere barometric pressure, given the elevation (altitude).
    !+ Reference:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 3

    real, intent(in)  ::  Altitude
      !+ Altitude in ft [IP] or m [SI]
    real              ::  StandardAtmPressure
      !+ Standard atmosphere barometric pressure in Psi [IP] or Pa [SI]

    if (isIP()) then
        StandardAtmPressure = 14.696 * (1.0 - 6.8754e-06 * Altitude)**5.2559
    else
        StandardAtmPressure = 101325 * (1 - 2.25577e-05 * Altitude)**5.2559
    end if
  end function GetStandardAtmPressure

  function GetStandardAtmTemperature(Altitude) result(StandardAtmTemperature)
    !+ Return standard atmosphere temperature, given the elevation (altitude).
    !+ Reference:
    !+ ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 4

    real, intent(in)  ::  Altitude
      !+ Altitude in ft [IP] or m [SI]
    real              ::  StandardAtmTemperature
      !+ Standard atmosphere dry-bulb temperature in °F [IP] or °C [SI]

    if (isIP()) then
        StandardAtmTemperature = 59.0 - 0.00356620 * Altitude
    else
        StandardAtmTemperature = 15.0 - 0.0065 * Altitude
    end if
  end function GetStandardAtmTemperature

  function GetSeaLevelPressure(StnPressure, Altitude, TDryBulb) result(SeaLevelPressure)
    !+ Return sea level pressure given dry-bulb temperature, altitude above sea level and pressure.
    !+ Reference:
    !+ Hess SL, Introduction to theoretical meteorology, Holt Rinehart and Winston, NY 1959,
    !+ ch. 6.5; Stull RB, Meteorology for scientists and engineers, 2nd edition,
    !+ Brooks/Cole 2000, ch. 1.
    !+ Notes:
    !+ The standard procedure for the US is to use for TDryBulb the average
    !+ of the current station temperature and the station temperature from 12 hours ago.

    real, intent(in)  ::  StnPressure
      !+ Observed station pressure in Psi [IP] or Pa [SI]
    real, intent(in)  ::  Altitude
      !+ Altitude in ft [IP] or m [SI]
    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real              ::  SeaLevelPressure
      !+ Sea level barometric pressure in Psi [IP] or Pa [SI]
    real              ::  TColumn
      !+ Average temperature in column of air in R [IP] or K [SI]
    real              ::  H
      !+ scale height (dimensionless)

    if (isIP()) then
      ! Calculate average temperature in column of air, assuming a lapse rate
      ! of 3.6 °F/1000ft
      TColumn = TDryBulb + 0.0036 * Altitude / 2.0

      ! Determine the scale height
      H = 53.351 * GetTRankineFromTFahrenheit(TColumn)
    else
      ! Calculate average temperature in column of air, assuming a lapse rate
      ! of 6.5 °C/km
      TColumn = TDryBulb + 0.0065 * Altitude / 2.0

      ! Determine the scale height
      H = 287.055 * GetTKelvinFromTCelsius(TColumn) / 9.807
    end if

    ! Calculate the sea level pressure
    SeaLevelPressure = StnPressure * exp(Altitude / H)
  end function GetSeaLevelPressure

  function GetStationPressure(SeaLevelPressure, Altitude, TDryBulb) result(StationPressure)
    !+ Return station pressure from sea level pressure.
    !+ Reference:
    !+ See 'GetSeaLevelPressure'
    !+ Notes:
    !+ This function is just the inverse of 'GetSeaLevelPressure'.

    real, intent(in)  ::  SeaLevelPressure
      !+ Sea level barometric pressure in Psi [IP] or Pa [SI]
    real, intent(in)  ::  Altitude
      !+ Altitude in ft [IP] or m [SI]
    real, intent(in)  ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real              ::  StationPressure
      !+ Station pressure in Psi [IP] or Pa [SI]

    StationPressure = SeaLevelPressure / GetSeaLevelPressure(1.0, Altitude, TDryBulb)
  end function GetStationPressure


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Functions to set all psychrometric values
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcPsychrometricsFromTWetBulb(TDryBulb,           &
                                            TWetBulb,           &
                                            Pressure,           &
                                            HumRatio,           &
                                            TDewPoint,          &
                                            RelHum,             &
                                            VapPres,            &
                                            MoistAirEnthalpy,   &
                                            MoistAirVolume,     &
                                            DegreeOfSaturation)

    !+ Utility function to calculate humidity ratio, dew-point temperature, relative humidity,
    !+ vapour pressure, moist air enthalpy, moist air volume, and degree of saturation of air given
    !+ dry-bulb temperature, wet-bulb temperature, and pressure.

    real, intent(in)    ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)    ::  TWetBulb
      !+ Wet-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)    ::  Pressure
      !+ Atmospheric pressure in Psi [IP] or Pa [SI]
    real, intent(out)   ::  HumRatio
      !+ Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
    real, intent(out)   ::  TDewPoint
      !+ Dew-point temperature in °F [IP] or °C [SI]
    real, intent(out)   ::  RelHum
      !+ Relative humidity in range [0, 1]
    real, intent(out)   ::  VapPres
      !+ Partial pressure of water vapor in moist air in Psi [IP] or Pa [SI]
    real, intent(out)   ::  MoistAirEnthalpy
      !+ Moist air enthalpy in Btu lb⁻¹ [IP] or J kg⁻¹ [SI]
    real, intent(out)   ::  MoistAirVolume
      !+ Specific volume of moist air in ft³ lb⁻¹ [IP] or in m³ kg⁻¹ [SI]
    real, intent(out)   ::  DegreeOfSaturation
      !+ Degree of saturation [unitless]

    HumRatio            = GetHumRatioFromTWetBulb(TDryBulb, TWetBulb, Pressure)
    TDewPoint           = GetTDewPointFromHumRatio(TDryBulb, HumRatio, Pressure)
    RelHum              = GetRelHumFromHumRatio(TDryBulb, HumRatio, Pressure)
    VapPres             = GetVapPresFromHumRatio(HumRatio, Pressure)
    MoistAirEnthalpy    = GetMoistAirEnthalpy(TDryBulb, HumRatio)
    MoistAirVolume      = GetMoistAirVolume(TDryBulb, HumRatio, Pressure)
    DegreeOfSaturation  = GetDegreeOfSaturation(TDryBulb, HumRatio, Pressure)
  end subroutine CalcPsychrometricsFromTWetBulb

  subroutine CalcPsychrometricsFromTDewPoint(TDryBulb,           &
                                             TDewPoint,          &
                                             Pressure,           &
                                             HumRatio,           &
                                             TWetBulb,           &
                                             RelHum,             &
                                             VapPres,            &
                                             MoistAirEnthalpy,   &
                                             MoistAirVolume,     &
                                             DegreeOfSaturation)

    !+ Utility function to calculate humidity ratio, wet-bulb temperature, relative humidity,
    !+ vapour pressure, moist air enthalpy, moist air volume, and degree of saturation of air given
    !+ dry-bulb temperature, dew-point temperature, and pressure.

    real, intent(in)    ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)    ::  TDewPoint
      !+ Dew-point temperature in °F [IP] or °C [SI]
    real, intent(in)    ::  Pressure
      !+ Atmospheric pressure in Psi [IP] or Pa [SI]
    real, intent(out)   ::  HumRatio
      !+ Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
    real, intent(out)   ::  TWetBulb
      !+ Wet-bulb temperature in °F [IP] or °C [SI]
    real, intent(out)   ::  RelHum
      !+ Relative humidity in range [0, 1]
    real, intent(out)   ::  VapPres
      !+ Partial pressure of water vapor in moist air in Psi [IP] or Pa [SI]
    real, intent(out)   ::  MoistAirEnthalpy
      !+ Moist air enthalpy in Btu lb⁻¹ [IP] or J kg⁻¹ [SI]
    real, intent(out)   ::  MoistAirVolume
      !+ Specific volume of moist air in ft³ lb⁻¹ [IP] or in m³ kg⁻¹ [SI]
    real, intent(out)   ::  DegreeOfSaturation
      !+ Degree of saturation [unitless]

    HumRatio            = GetHumRatioFromTDewPoint(TDewPoint, Pressure)
    TWetBulb            = GetTWetBulbFromHumRatio(TDryBulb, HumRatio, Pressure)
    RelHum              = GetRelHumFromHumRatio(TDryBulb, HumRatio, Pressure)
    VapPres             = GetVapPresFromHumRatio(HumRatio, Pressure)
    MoistAirEnthalpy    = GetMoistAirEnthalpy(TDryBulb, HumRatio)
    MoistAirVolume      = GetMoistAirVolume(TDryBulb, HumRatio, Pressure)
    DegreeOfSaturation  = GetDegreeOfSaturation(TDryBulb, HumRatio, Pressure)
  end subroutine CalcPsychrometricsFromTDewPoint

  subroutine CalcPsychrometricsFromRelHum(TDryBulb,           &
                                          RelHum,             &
                                          Pressure,           &
                                          HumRatio,           &
                                          TWetBulb,           &
                                          TDewPoint,          &
                                          VapPres,            &
                                          MoistAirEnthalpy,   &
                                          MoistAirVolume,     &
                                          DegreeOfSaturation)

    !+ Utility function to calculate humidity ratio, wet-bulb temperature, dew-point temperature,
    !+ vapour pressure, moist air enthalpy, moist air volume, and degree of saturation of air given
    !+ dry-bulb temperature, relative humidity and pressure.

    real, intent(in)    ::  TDryBulb
      !+ Dry-bulb temperature in °F [IP] or °C [SI]
    real, intent(in)    ::  RelHum
      !+ Relative humidity in range [0, 1]
    real, intent(in)    ::  Pressure
      !+ Atmospheric pressure in Psi [IP] or Pa [SI]
    real, intent(out)   ::  HumRatio
      !+ Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
    real, intent(out)   ::  TWetBulb
      !+ Wet-bulb temperature in °F [IP] or °C [SI]
    real, intent(out)   ::  TDewPoint
      !+ Dew-point temperature in °F [IP] or °C [SI]
    real, intent(out)   ::  VapPres
      !+ Partial pressure of water vapor in moist air in Psi [IP] or Pa [SI]
    real, intent(out)   ::  MoistAirEnthalpy
      !+ Moist air enthalpy in Btu lb⁻¹ [IP] or J kg⁻¹ [SI]
    real, intent(out)   ::  MoistAirVolume
      !+ Specific volume of moist air in ft³ lb⁻¹ [IP] or in m³ kg⁻¹ [SI]
    real, intent(out)   ::  DegreeOfSaturation
      !+ Degree of saturation [unitless]

    HumRatio            = GetHumRatioFromRelHum(TDryBulb, RelHum, Pressure)
    TWetBulb            = GetTWetBulbFromHumRatio(TDryBulb, HumRatio, Pressure)
    TDewPoint           = GetTDewPointFromHumRatio(TDryBulb, HumRatio, Pressure)
    VapPres             = GetVapPresFromHumRatio(HumRatio, Pressure)
    MoistAirEnthalpy    = GetMoistAirEnthalpy(TDryBulb, HumRatio)
    MoistAirVolume      = GetMoistAirVolume(TDryBulb, HumRatio, Pressure)
    DegreeOfSaturation  = GetDegreeOfSaturation(TDryBulb, HumRatio, Pressure)
  end subroutine CalcPsychrometricsFromRelHum


end module module_sf_psychrolib