/***********************************************************************************/
/*  Copyright 2009 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
/***********************************************************************************/
/* This file is part of MeteoIO.
    MeteoIO is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MeteoIO is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with MeteoIO.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <assert.h>
#include <sstream>
#include <cmath>
#include <algorithm>
#include <meteoio/meteoLaws/Atmosphere.h>
#include <meteoio/meteoLaws/Meteoconst.h>
#include <meteoio/meteoLaws/Sun.h>
#include <meteoio/MathOptim.h>
#include <meteoio/IOUtils.h>

namespace mio {
/**
 * @brief Calculate the black body emissivity
 * @param lwr longwave radiation emitted by the body (W m-2)
 * @param T   surface temperature of the body (K)
 * @return black body emissivity (0-1)
 */
double Atmosphere::blkBody_Emissivity(const double& lwr, const double& T) {
	const double T2 = T*T;
	const double ea = lwr / (Cst::stefan_boltzmann * (T2*T2));
	return std::min(ea, 1.);
}

/**
 * @brief Calculates the black body long wave radiation knowing its emissivity
 * @param ea emissivity of the body (0-1)
 * @param T   surface temperature of the body (K)
 * @return black body radiation (W/m^2)
 */
double Atmosphere::blkBody_Radiation(const double& ea, const double& T) {
	const double T2 = T*T;
	return ( ea * (Cst::stefan_boltzmann * (T2*T2)) );
}

/**
* @brief Standard atmospheric pressure as a function of the altitude.
* This is based on the following formula (with h the altitude, P<sub>0</sub> 
* and T<sub>0</sub> the standard sea level pressure and temperature, L the 
* dry adiabatic lapse rate and R<sub>0</sub> the earth's radius):
* \f[
* P = P_0 \cdot {\left( 1 - \frac{L \cdot R_0 \cdot h}{T_0 ( R_0 + h )}  \right)}^{\frac{g}{L R}}
* \f]
* @param altitude altitude above sea level (m)
* @return standard pressure (Pa)
*/
double Atmosphere::stdAirPressure(const double& altitude) {
	static const double expo = Cst::gravity / (Cst::mean_adiabatique_lapse_rate * Cst::gaz_constant_dry_air);
	const double p = Cst::std_press * pow( 1. - ( (Cst::mean_adiabatique_lapse_rate * Cst::earth_R0 * altitude) / (Cst::std_temp * (Cst::earth_R0 + altitude)) ), expo );
	return p;
}

/**
 * @brief Atmospheric pressure reduced to sea level.
 * This calculates the following formula (with P the pressure at altitude h, L the adiabatic lapse rate,
 * and T<sub>0</sub> the standard sea level temperature, the air mol mass m, gravity g at height, and the
 * gas constant R):
 * \f[
 * P_0 = \frac{P}{(1-\frac{Lh}{T_0})^\frac{mg}{RL}}
 * \f]
 * With growing altitude the output is of little use to predict the pressure, but it can be used to
 * baseline plots.
 * @param pressure Measured pressure at an altitude (Pa)
 * @param altitude The station's altitude above sea level (m)
 * @param latitude The station's latitude (degrees)
 * @return Reduced atmospheric pressure (Pa)
 */
double Atmosphere::reducedAirPressure(const double& pressure, const double& altitude, const double& latitude) {
	const double pp = pressure / pow( 1. - Cst::mean_adiabatique_lapse_rate * altitude / Cst::std_temp,
	    (Cst::dry_air_mol_mass * Atmosphere::gravity( altitude, latitude )) /
	    (Cst::gaz_constant * Cst::mean_adiabatique_lapse_rate) );
	return pp;
}

/**
 * @brief Acceleration due to gravity
 * @param altitude altitude above sea level (m)
 * @param latitude latitude in degrees
 * @return acceleration due to gravity (m/s2)
 */
double Atmosphere::gravity(const double& altitude, const double& latitude) {
	const double lat = latitude*Cst::to_rad;
	const double g = 9.780356 * (1. + 0.0052885*Optim::pow2(sin(lat)) - 0.0000059*Optim::pow2(sin(2.*lat))) - 0.003086 * altitude*1e-3;
	return g;
}

/**
* @brief Standard dry air pressure
* @param altitude altitude above sea level (m)
* @param temperature air temperature (K)
* @return standard pressure (Pa)
*/
double Atmosphere::stdDryAirDensity(const double& altitude, const double& temperature) {
	return stdAirPressure(altitude)/(Cst::gaz_constant_dry_air*temperature);
}

/**
* @brief Calculates the water vapor density, for a given temperature and vapor pressure
* @param Temperature air temperature (K)
* @param VaporPressure water vapor pressure (Pa)
* @return water vapor density (kg/m^3)
*/
double Atmosphere::waterVaporDensity(const double& Temperature, const double& VaporPressure) {
	return (Cst::water_molecular_mass*VaporPressure)/(Cst::gaz_constant*Temperature);
}

/**
* @brief Standard atmosphere wet bulb temperature.
* This gives the lowest temperature that could be reached by water evaporation. It is therefore linked to
* relative humidity. This implementation assumes a standard atmosphere for pressure and saturation pressure.
* @param TA air temperature (K)
* @param RH relative humidity (between 0 and 1)
* @param altitude altitude above sea level (m)
* @return wet bulb temperature (K)
*/
double Atmosphere::wetBulbTemperature(const double& TA, const double& RH, const double& altitude)
{ //naive numerical approach by guessing T_wet in RH = f(TA, T_wet, P) until we are right
	const double PP = stdAirPressure(altitude);
	const double ed = vaporSaturationPressure(TA);
	double TW = TA; //can't be higher
	double hum = 1.;
	while(hum > RH) { //calculate RH from TA and a guess for wet bulb temperature
		TW = TW - 0.01; //new guess; TODO: bisection
		const double AA = 0.00066 * (1. + 0.00115 * (TW - Cst::t_water_freezing_pt));
		const double ew = vaporSaturationPressure(TW);
		hum = (ew - AA * PP * (TA - TW)) / ed; //humidity given TA and T_wet
	}
	return TW;

//	static const double L = Cst::l_water_vaporization; //latent heat of vaporisation
//	static const double mixing_ratio = Cst::gaz_constant_dry_air / Cst::gaz_constant_water_vapor;
//	const double p = stdAirPressure(altitude);
//	const double Vp = vaporSaturationPressure(T);
//	return ( T - (RH*Vp - Vp) * mixing_ratio * L / p / Cst::specific_heat_air );
//
} //cf. https://maxwellsci.com/print/rjaset/v6-2984-2987.pdf

/**
* @brief Black Globe Temperature.
* This is an estimation of the black globe temperature based on physical modeling as in
* V. E. Dimiceli, S. F. Piltz and S. A. Amburn, <i>"Estimation of Black Globe Temperature for Calculation of the 
* Wet Bulb Globe Temperature Index"</i> in World Congress on Engineering and Computer Science, <b>2</b>, 2011.
* @param TA air temperature (K)
* @param RH relative humidity (between 0 and 1)
* @param VW wind velocity (m/s)
* @param iswr_dir direct solar SW radiation (W/m^2)
* @param iswr_diff diffuse solar SW radiation (W/m^2)
* @param cos_Z cosinus of the solar zenith angle
* @return black globe temperature (K)
*/
double Atmosphere::blackGlobeTemperature(const double& TA, const double& RH, const double& VW, const double& iswr_dir, const double& iswr_diff, const double& cos_Z)
{
	const double S = iswr_dir + iswr_diff;
	//const double a=1, b=1, c=1; // get real values!
	//const double h = a * pow(S, b) * pow(cos_Z, c);
	static const double h = 0.315; //personnal communication from S. Amburn
	const double emissivity = 0.575 * pow(RH*vaporSaturationPressure(TA), 1./7.);
	const double B = S * (iswr_dir/(4.*Cst::stefan_boltzmann*cos_Z) + 1.2/Cst::stefan_boltzmann*iswr_diff) + emissivity*Optim::pow4(TA);
	const double C = h * pow(VW, 0.58) / 5.3865e-8;

	const double Tg = (B + C*TA + 7680000.) / (C + 256000.);
	return Tg;
}

/**
* @brief Wind log profile.
* This is used to compute the equivalent wind speed at a different height.
* @details It depends on the roughness length
* that can take a default value (0.03) or any other, for example:
* <center><table border="0">
* <tr><th>land class</th><th>roughness length</th></tr>
* <tr><td>sea</td><td>0.0002</td></tr>
* <tr><td>smooth</td><td>0.005</td></tr>
* <tr><td>open</td><td>0.03</td></tr>
* <tr><td>roughly open</td><td>0.10</td></tr>
* <tr><td>rough</td><td>0.25</td></tr>
* <tr><td>very rough</td><td>0.5</td></tr>
* <tr><td>closed</td><td>1.0</td></tr>
* <tr><td>chaotic</td><td>over 2.0</td></tr>
* </table></center>
*
* @param v_ref reference wind speed (m/s)
* @param z_ref height of the reference wind speed (m)
* @param z new height (m)
* @param z0 roughness length (m)
* @return wind speed at height z (m/s)
*/
double Atmosphere::windLogProfile(const double& v_ref, const double& z_ref, const double& z, const double& z0)
{
	return v_ref * ( log(z/z0) / log(z_ref/z0) );
}

/**
* @brief Wind chill temperature.
* This is an index aiming at expressing the human-percived feeling of air temperature on exposed skin due to wind.
* This is NOT a scientific measurement, only an index to express a subjective feeling.
* It is inapplicable above 10 Celsius and a few m/s wind (5 m/s used here), therefore returning air temperature.
* @param TA air temperature (K)
* @param VW wind velocity (m/s)
* @return wind chill temperature (K)
*/
double Atmosphere::windChill(const double& TA, const double& VW)
{
	if (TA>(Cst::t_water_freezing_pt+10.) || VW<5.) return TA; //not applicable in this case

	const double t = IOUtils::K_TO_C(TA); //in Celsius
	const double v = VW*3.6; //in km/h
	const double v016 = pow(v, 0.16);

	const double WCT = 13.12 + 0.6215*t - 11.37*v016 + 0.3965*t*v016;
	return IOUtils::C_TO_K(WCT);
}

/**
* @brief Heat index.
* This is an index aiming at expressing the human-perceived air temperature due to humidity.
* This is NOT a scientific measurement, only an index to express a subjective feeling.
* It is inapplicable below 26.7 Celsius and below 40% humidity, therefore returning air temperature.
* @param TA air temperature (K)
* @param RH relative humidity (between 0 and 1)
* @return Heat index (K)
*/
double Atmosphere::heatIndex(const double& TA, const double& RH)
{
	if (TA<(Cst::t_water_freezing_pt+26.7) || RH<0.4) return TA; //not applicable in this case

	const double t = IOUtils::K_TO_C(TA); //in Celsius
	const double t2 = t*t;
	const double rh = RH*100.; //in percent
	const double rh2 = rh*rh;

	static const double c1=-8.784695, c2=1.61139411, c3=2.338549 , c4=-0.14611605;
	static const double c5=-1.2308094e-2 , c6=-1.6424828e-2 , c7=2.211732e-3 , c8=7.2546e-4, c9=-3.582e-6;

	const double HI = c1 + c2*t + c3*rh + c4*t*rh + c5*t2 + c6*rh2 + c7*t2*rh + c8*t*rh2 + c9*t2*rh2;

	return IOUtils::C_TO_K(HI);
}

/**
* @brief Wet Bulb Globe Temperature index.
* This is an index aiming at expressing the human-perceived air temperature due to humidity, wind and radiation.
* This is the foundation of ISO 7243 and is widely used for physical activity safety evaluation (for example for physical training).
* @param TA air temperature (K)
* @param RH relative humidity (between 0 and 1)
* @param VW wind velocity (m/s)
* @param iswr_dir direct solar SW radiation (W/m^2)
* @param iswr_diff diffuse solar SW radiation (W/m^2)
* @param cos_Z cosinus of the solar zenith angle
* @param altitude altitude of the point where to perform the calculation
* @return Heat index (K)
*/
double Atmosphere::WBGT_index(const double& TA, const double& RH, const double& VW, const double& iswr_dir, const double& iswr_diff, const double& cos_Z, const double& altitude)
{
	const double NWB = wetBulbTemperature(TA, RH, altitude);
	const double GT = blackGlobeTemperature(TA, RH, VW, iswr_dir, iswr_diff, cos_Z);
	const double DB = TA;

	return 0.7*NWB + 0.2*GT + 0.1*DB;
}

/**
* @brief Standard water vapor saturation pressure.
* See Murray, F. W., <i>"On the computation of saturation vapor pressure"</i>, 1966, J. Appl. Meteor., <b>6</b>, 203–204,
* doi: 10.1175/1520-0450(1967)006<0203:OTCOSV>2.0.CO;2.
* @param T air temperature (K)
* @return standard water vapor saturation pressure (Pa)
*/
double Atmosphere::vaporSaturationPressure(const double& T) {
	double c2, c3; // varying constants

	if ( T < Cst::t_water_triple_pt ) { // for a flat ice surface
		c2 = 21.88;
		c3 = 7.66;
	} else { // for a flat water surface
		c2 = 17.27;
		c3 = 35.86;
	}
	const double exp_p_sat = c2 *  (T - Cst::t_water_triple_pt) / (T - c3); //exponent

	return( Cst::p_water_triple_pt * exp( exp_p_sat ) );
}


/**
* @brief Standard water vapor saturation pressure, assuming "over water" for the full temperature range.
* See Murray, F. W., <i>"On the computation of saturation vapor pressure"</i>, 1966, J. Appl. Meteor., <b>6</b>, 203–204,
* doi: 10.1175/1520-0450(1967)006<0203:OTCOSV>2.0.CO;2.
* @param T air temperature (K)
* @return standard water vapor saturation pressure, assuming water surface (Pa)
*/
double Atmosphere::vaporSaturationPressureWater(const double& T) {
	double c2, c3; // varying constants

	c2 = 17.27;
	c3 = 35.86;

	const double exp_p_sat = c2 *  (T - Cst::t_water_triple_pt) / (T - c3); //exponent

	return( Cst::p_water_triple_pt * exp( exp_p_sat ) );
}

/**
* @brief Virtual temperature multiplying factor.
* In order to get a virtual temperature, multiply the original temperature by this factor. Note:
* since e/p is actually used, both pressures only need to have the same units.
* @param e vapor pressure (Pa)
* @param p air pressure (Pa)
* @return virtual temperature multiplying coefficient
*/
double Atmosphere::virtualTemperatureFactor(const double& e, const double& p) {
	static const double epsilon = 0.622;
	return 1. / (1.-(1.-epsilon)*e/p);
}

/**
 * @brief Evaluate the atmosphere emissivity for clear sky.
 * This uses the formula from Brutsaert -- "On a Derivable
 * Formula for Long-Wave Radiation From Clear Skies", Journal of Water Resources
 * Research, <b>11</b>, No. 5, October 1975, pp 742-744.
 * Alternative: Satterlund (1979): Water Resources Research, 15, 1649-1650.
 * @param RH relative humidity (between 0 and 1)
 * @param TA Air temperature (K)
 * @return clear sky emissivity
 */
double Atmosphere::Brutsaert_emissivity(const double& RH, const double& TA) {
	const double e0 = RH * vaporSaturationPressure(TA); //water vapor pressure
	const double e0_mBar = 0.01 * e0;
	static const double exponent = 1./7.;
	const double ea = 1.24 * pow( (e0_mBar / TA), exponent);
	return std::min(ea, 1.);
}

/**
 * @brief Evaluate the long wave radiation for clear sky.
 * This uses the formula from Brutsaert -- "On a Derivable
 * Formula for Long-Wave Radiation From Clear Skies", Journal of Water Resources
 * Research, <b>11</b>, No. 5, October 1975, pp 742-744.
 * Alternative: Satterlund (1979): Water Resources Research, 15, 1649-1650.
 * @param RH relative humidity (between 0 and 1)
 * @param TA Air temperature (K)
 * @return long wave radiation (W/m^2)
*/
double Atmosphere::Brutsaert_ilwr(const double& RH, const double& TA) {
	const double ea = Brutsaert_emissivity(RH, TA);
	return blkBody_Radiation(ea, TA);
}

/**
 * @brief Evaluate the atmosphere emissivity for clear sky.
 * This uses the formula from Dilley and O'Brien -- "Estimating downward clear sky
 * long-wave irradiance at the surface from screen temperature and precipitable water",
 * Q. J. R. Meteorolo. Soc., <b>124</b>, 1998, pp 1391-1401. The long wave is computed
 * and the ratio of this long wave to a black body emission gives an emissivity.
 * @param RH relative humidity (between 0 and 1)
 * @param TA near surface air temperature (K)
 * @return clear sky emissivity
*/
double Atmosphere::Dilley_emissivity(const double& RH, const double& TA) {
	const double ilwr_dilley = Dilley_ilwr(RH, TA);
	const double ilwr_blkbody = blkBody_Radiation(1., TA);
	const double ea = ilwr_dilley/ilwr_blkbody;
	return std::min(ea, 1.);
}

/**
 * @brief Evaluate the long wave radiation for clear sky.
 * This uses the formula from Dilley and O'Brien -- "Estimating downward clear sky
 * long-wave irradiance at the surface from screen temperature and precipitable water",
 * Q. J. R. Meteorolo. Soc., <b>124</b>, 1998, pp 1391-1401.
 * @param RH relative humidity (between 0 and 1)
 * @param TA near surface air temperature (K)
 * @return long wave radiation (W/m^2)
*/
double Atmosphere::Dilley_ilwr(const double& RH, const double& TA) {
	const double e0 = RH * vaporSaturationPressure(TA) * 0.001; //water vapor pressure, kPa
	const double w = 4650.*e0/TA; //precipitable water, Prata 1996

	const double tmp = TA/Cst::t_water_triple_pt;
	const double pow_tmp6 = tmp*tmp*tmp*tmp*tmp*tmp;
	return 59.38 + 113.7*pow_tmp6 + 96.96*sqrt(w/25.);
}

/**
 * @brief Evaluate the atmosphere emissivity for clear sky.
 * This uses the formula from Prata -- "A new long-wave formula for estimating
 * downward clear-sky radiation at the surface", Q. J. R. Meteorolo. Soc., <b>122</b>, 1996, pp 1127-1151.
 * @param RH relative humidity (between 0 and 1)
 * @param TA near surface air temperature (K)
 * @return clear sky emissivity
*/
double Atmosphere::Prata_emissivity(const double& RH, const double& TA) {
	const double e0 = RH * vaporSaturationPressure(TA) * 0.001; //water vapor pressure, kPa
	const double w = 4650.*e0/TA; //precipitable water, Prata 1996
	const double ea = 1. - (1.+w)*exp( -sqrt(1.2+3.*w) );
	return std::min(ea, 1.);
}

/**
 * @brief Evaluate the long wave radiation for clear sky.
 * This uses the formula from Prata -- "A new long-wave formula for estimating
 * downward clear-sky radiation at the surface", Q. J. R. Meteorolo. Soc., <b>122</b>, 1996, pp 1127-1151.
 * @param RH relative humidity (between 0 and 1)
 * @param TA near surface air temperature (K)
 * @return long wave radiation (W/m^2)
*/
double Atmosphere::Prata_ilwr(const double& RH, const double& TA) {
	const double epsilon = Prata_emissivity(RH, TA);
	return blkBody_Radiation(epsilon, TA);
}

/**
 * @brief Evaluate the atmosphere emissivity for clear sky.
 * This uses the formula from Clark & Allen -- "The estimation of atmospheric radiation for clear and
 * cloudy skies", Proceedings of the second national passive solar conference, <b>2</b>, 1978, p 676.
 * @param RH relative humidity (between 0 and 1)
 * @param TA near surface air temperature (K)
 * @return clear sky emissivity
*/
double Atmosphere::Clark_emissivity(const double& RH, const double& TA) {
	const double Tdp = RhtoDewPoint(RH, TA, false);
	const double ea = 0.787 + 0.0028 * (Tdp-Cst::t_water_triple_pt);
	return ea;
}

/**
 * @brief Evaluate the long wave radiation for clear sky.
 * This uses the formula from Clark & Allen -- "The estimation of atmospheric radiation for clear and
 * cloudy skies", Proceedings of the second national passive solar conference, <b>2</b>, 1978, p 676.
 * @param RH relative humidity (between 0 and 1)
 * @param TA near surface air temperature (K)
 * @return long wave radiation (W/m^2)
*/
double Atmosphere::Clark_ilwr(const double& RH, const double& TA) {
	const double epsilon = Clark_emissivity(RH, TA);
	return blkBody_Radiation(epsilon, TA);
}

/**
 * @brief Evaluate the atmosphere emissivity for clear sky.
 * This uses the formula from Tang, Etzion and Meir -- "Estimates of clear night sky emissivity in the
 * Negev Highlands, Israel", Energy Conversion and Management, <b>45.11</b>, 2004, pp 1831-1843.
 * @param RH relative humidity (between 0 and 1)
 * @param TA near surface air temperature (K)
 * @return clear sky emissivity
*/
double Atmosphere::Tang_emissivity(const double& RH, const double& TA) {
	const double Tdp = RhtoDewPoint(RH, TA, false);
	const double ea = 0.754 + 0.0044 * (Tdp-Cst::t_water_triple_pt);
	return std::min(ea, 1.);
}

/**
 * @brief Evaluate the long wave radiation for clear sky.
 * This uses the formula from Tang, Etzion and Meir -- "Estimates of clear night sky emissivity in the
 * Negev Highlands, Israel", Energy Conversion and Management, <b>45.11</b>, 2004, pp 1831-1843.
 * @param RH relative humidity (between 0 and 1)
 * @param TA near surface air temperature (K)
 * @return long wave radiation (W/m^2)
*/
double Atmosphere::Tang_ilwr(const double& RH, const double& TA) {
	const double epsilon = Tang_emissivity(RH, TA);
	return blkBody_Radiation(epsilon, TA);
}

/**
 * @brief Evaluate the atmosphere emissivity for clear sky.
 * This uses the formula from Idso -- "A set of equations for full spectrum and 8 to 14 um and
 * 10.5 to 12.5 um thermal radiation from cloudless skies", Water Resources Research, <b>17</b>, 1981, pp 295-304.
 * @param RH relative humidity (between 0 and 1)
 * @param TA near surface air temperature (K)
 * @return clear sky emissivity
*/
double Atmosphere::Idso_emissivity(const double& RH, const double& TA) {
	const double e0 = RH * vaporSaturationPressure(TA) * 0.0001; //water vapor pressure, mbar
	const double ea = 0.70 + 5.95e-5 * e0 * exp(1500./TA);
	return std::min(ea, 1.);
}

/**
 * @brief Evaluate the long wave radiation for clear sky.
 * This uses the formula from Idso -- "A set of equations for full spectrum and 8 to 14 um and
 * 10.5 to 12.5 um thermal radiation from cloudless skies", Water Resources Research, <b>17</b>, 1981, pp 295-304.
 * @param RH relative humidity (between 0 and 1)
 * @param TA near surface air temperature (K)
 * @return long wave radiation (W/m^2)
*/
double Atmosphere::Idso_ilwr(const double& RH, const double& TA) {
	const double epsilon = Idso_emissivity(RH, TA);
	return blkBody_Radiation(epsilon, TA);
}

/**
* @brief Evaluate the atmosphere emissivity from the water vapor pressure and cloudiness.
* This is according to A. Omstedt, <i>"A coupled one-dimensional sea ice-ocean model applied to a semi-enclosed basin"</i>,
* Tellus, <b>42 A</b>, 568-582, 1990, DOI:10.1034/j.1600-0870.1990.t01-3-00007.x
* @param RH relative humidity (between 0 and 1)
* @param TA air temperature (K)
* @param cloudiness cloudiness (between 0 and 1, 0 being clear sky)
* @return emissivity (between 0 and 1)
*/
double Atmosphere::Omstedt_emissivity(const double& RH, const double& TA, const double& cloudiness) {
	const double e0 = RH * vaporSaturationPressure(TA); //water vapor pressure
	static const double eps_w = 0.97;
	static const double a1 = 0.68;
	static const double a2 = 0.0036;
	static const double a3 = 0.18;

	const double ea = (eps_w * (a1 + a2 * sqrt(e0)) * (1. + a3 * cloudiness * cloudiness)); //emissivity
	return std::min(ea, 1.);
}

/**
* @brief Evaluate the long wave radiation from RH, TA and cloudiness.
* This is according to A. Omstedt, <i>"A coupled one-dimensional sea ice-ocean model applied to a semi-enclosed basin"</i>,
* Tellus, <b>42 A</b>, 568-582, 1990, DOI:10.1034/j.1600-0870.1990.t01-3-00007.x
* @param RH relative humidity (between 0 and 1)
* @param TA air temperature (K)
* @param cloudiness cloudiness (between 0 and 1, 0 being clear sky)
* @return long wave radiation (W/m^2)
*/
double Atmosphere::Omstedt_ilwr(const double& RH, const double& TA, const double& cloudiness) {
	const double ea = Omstedt_emissivity(RH, TA, cloudiness);
	return blkBody_Radiation(ea, TA);
}

/**
* @brief Evaluate the atmosphere emissivity from the water vapor pressure and cloudiness.
* This is according to Konzelmann, Thomas, et al. <i>"Parameterization of global and longwave incoming radiation
* for the Greenland Ice Sheet."</i> Global and Planetary change <b>9.1</b> (1994): 143-164.
* @param RH relative humidity (between 0 and 1)
* @param TA air temperature (K)
* @param cloudiness cloudiness (between 0 and 1, 0 being clear sky)
* @return emissivity (between 0 and 1)
*/
double Atmosphere::Konzelmann_emissivity(const double& RH, const double& TA, const double& cloudiness) {
	const double ea = RH * vaporSaturationPressure(TA); //screen-level water vapor pressure
	static const double exponent = 1./8.;

	const double epsilon_cs = 0.23 + 0.484*pow( ea/TA, exponent ); //clear sky emissivity
	static const double epsilon_oc = 0.952; //fully overcast sky emissivity

	const double weight = Optim::pow4(cloudiness); //weight for the weighted average between clear sky and overcast

	const double epsilon_cloudy = (1. - weight)*epsilon_cs + weight*epsilon_oc;
	return epsilon_cloudy;
}

/**
* @brief Evaluate the long wave radiation from RH, TA and cloudiness.
* This is according to Konzelmann, Thomas, et al. <i>"Parameterization of global and longwave incoming radiation
* for the Greenland Ice Sheet."</i> Global and Planetary change <b>9.1</b> (1994): 143-164.
* @param RH relative humidity (between 0 and 1)
* @param TA air temperature (K)
* @param cloudiness cloudiness (between 0 and 1, 0 being clear sky)
* @return long wave radiation (W/m^2)
*/
double Atmosphere::Konzelmann_ilwr(const double& RH, const double& TA, const double& cloudiness) {
	const double ea = Konzelmann_emissivity(RH, TA, cloudiness);
	return blkBody_Radiation(ea, TA);
}

/**
* @brief Evaluate the atmosphere emissivity from RH, TA and cloudiness.
* This is according to Carmona, Rivas, and Caselles. <i>"Estimation of daytime downward 
* longwave radiation under clear and cloudy skies conditions over a sub-humid region."</i> Theoretical and applied climatology <b>115.1-2</b> (2014): 281-295.
* Here the second variant (MLRM-2) is implemented.
* @param RH relative humidity (between 0 and 1)
* @param TA air temperature (K)
* @param cloudiness 1 - ratio of measured ISWR over potential ISWR (between 0 and 1, 0 being clear sky)
* @return emissivity (between 0 and 1)
*/
double Atmosphere::Carmona_emissivity(const double& RH, const double& TA, const double& cloudiness) {
	static const double beta_0 = -0.34;
	static const double beta_1 = 3.36e-3;
	static const double beta_2 = 1.94e-3;
	static const double beta_3 = 0.213;
	
	const double epsilon_cloudy = beta_0 + beta_1 * TA + beta_2 * (RH*100.) + beta_3 * cloudiness;
	return epsilon_cloudy;
}

/**
* @brief Evaluate the long wave radiation from RH, TA and cloudiness.
* This is according to Carmona, Rivas, and Caselles. <i>"Estimation of daytime downward 
* longwave radiation under clear and cloudy skies conditions over a sub-humid region."</i> Theoretical and applied climatology <b>115.1-2</b> (2014): 281-295.
* Here the second variant (MLRM-2) is implemented.
* @param RH relative humidity (between 0 and 1)
* @param TA air temperature (K)
* @param cloudiness 1 - ratio of measured ISWR over potential ISWR (between 0 and 1, 0 being clear sky)
* @return long wave radiation (W/m^2)
*/
double Atmosphere::Carmona_ilwr(const double& RH, const double& TA, const double& cloudiness) {
	const double ea = Carmona_emissivity(RH, TA, cloudiness);
	return blkBody_Radiation(ea, TA);
}

/**
 * @brief Evaluate the solar clearness index for a given cloudiness. 
 * This uses the formula from Kasten and Czeplak -- <i>"Solar and terrestrial radiation
 * dependent on the amount and type of cloud"</i>, Sol. Energy, <b>24</b>, 1980, pp 177-189.
 * The solar index is defined as measured radiation / clear sky radiation, values
 * outside of [0;1] will be truncated to [0;1].
 * @param cloudiness in okta, between 0 and 1
 * @return solar clearness index
*/
double Atmosphere::Kasten_clearness(const double& cloudiness) {
	static const double b1 = 0.75, b2 = 3.4;
	if (cloudiness<0. || cloudiness>1.) {
		std::ostringstream ss;
		ss << "Invalid cloudiness value: " << cloudiness << " (it should be between 0 and 1)";
		throw InvalidArgumentException(ss.str(), AT);
	}
	const double clearness = 1. - b1*pow(cloudiness, b2);
	return clearness;
}

/**
 * @brief Evaluate the cloudiness from a given solar index.
 * This uses the formula from Kasten and Czeplak -- <i>"Solar and terrestrial radiation
 * dependent on the amount and type of cloud"</i>, Sol. Energy, <b>24</b>, 1980, pp 177-189.
 * The solar index is defined as measured radiation / clear sky radiation, values
 * outside of [0;1] will be truncated to [0;1].
 * @param solarIndex solar index
 * @return cloudiness (in okta, between 0 and 1)
*/
double Atmosphere::Kasten_cloudiness(const double& solarIndex) {
	static const double b1 = 0.75, b2 = 3.4;

	if (solarIndex>1.) return 0.;
	const double cloudiness = pow((1.-solarIndex)/b1, 1./b2);
	return std::min(cloudiness, 1.);
}

/**
 * @brief Evaluate the long wave radiation for clear or cloudy sky.
 * This uses the formula from Crawford and Duchon -- <i>"An Improved Parametrization
 * for Estimating Effective Atmospheric Emissivity for Use in Calculating Daytime
 * Downwelling Longwave Radiation"</i>, Journal of Applied Meteorology,
 * <b>38</b>, 1999, pp 474-480. If no cloud cover fraction is provided, a parametrization
 * using iswr_meas and iswr_clear_sky will be used. These parameters can therefore safely
 * be set to IOUtils::nodata if cloudiness is provided.
 * @param RH relative humidity (between 0 and 1)
 * @param TA Air temperature (K)
 * @param iswr_meas Measured Incoming Short Wave Radiation (W/m^2)
 * @param iswr_clear_sky Clear Sky Modelled Incoming Short Wave Radiation (W/m^2)
 * @param month current month (1-12, for a sinusoidal variation of the leading coefficients)
 * @param cloudiness Cloud cover fraction (between 0 and 1, optional)
 * @return long wave radiation (W/m^2) or IOUtils::nodata at night time
*/
double Atmosphere::Crawford_ilwr(const double& RH, const double& TA, const double& iswr_meas, const double& iswr_clear_sky, const unsigned char& month, const double& cloudiness)
{
	double clf;
	if (cloudiness==IOUtils::nodata) {
		if (iswr_meas<=0. || iswr_clear_sky<=0.)
			return IOUtils::nodata;
		clf = 1. - iswr_meas/iswr_clear_sky;  //cloud fraction estimate
		if (clf<0.) clf=0.;
	} else {
		if (cloudiness<0. || cloudiness>1.)
			return IOUtils::nodata;
		clf = cloudiness;
	}

	const double e = RH * vaporSaturationPressure(TA); //near surface water vapor pressure
	const double e_mBar = 0.01 * e;

	const double epsilon = clf + (1.-clf) * (1.22 + 0.06*sin((month+2.)*Cst::PI/6.) ) * pow( (e_mBar/TA), 1./7.);
	return blkBody_Radiation(epsilon, TA);
}

/**
 * @brief Evaluate the long wave radiation for clear or cloudy sky.
 * This uses the formula from Crawford and Duchon -- <i>"An Improved Parametrization
 * for Estimating Effective Atmospheric Emissivity for Use in Calculating Daytime
 * Downwelling Longwave Radiation"</i>, Journal of Applied Meteorology,
 * <b>38</b>, 1999, pp 474-480. If no cloud cover fraction is provided, a parametrization
 * using the current location (lat, lon, altitude) and ISWR will be used. These parameters can therefore safely
 * be set to IOUtils::nodata if cloudiness is provided.
 * @param lat latitude of the point of observation
 * @param lon longitude of the point of observation
 * @param altitude altitude of the point of observation
 * @param julian julian date at the point of observation
 * @param TZ time zone at the point of observation
 * @param RH relative humidity (between 0 and 1)
 * @param TA Air temperature (K)
 * @param ISWR Measured Incoming Short Wave Radiation (W/m^2)
 * @param cloudiness Cloud cover fraction (between 0 and 1, optional)
 * @return long wave radiation (W/m^2) or IOUtils::nodata at night time
 * Please note that this call might NOT be efficient for processing large amounts of points,
 * since it internally builds complex objects at every call. You might want to copy/paste
 * its code in order to process data in bulk.
*/
double Atmosphere::Crawford_ilwr(const double& lat, const double& lon, const double& altitude,
                                 const double& julian, const double& TZ,
                                 const double& RH, const double& TA, const double& ISWR, const double& cloudiness)
{
	if (TA==IOUtils::nodata || RH==IOUtils::nodata) {
		return IOUtils::nodata;
	}

	const Date date(julian, TZ);
	int year, month, day;
	date.getDate(year, month, day);

	if (cloudiness==IOUtils::nodata) {
		if (ISWR==IOUtils::nodata) return IOUtils::nodata;

		SunObject Sun(lat, lon, altitude, julian, TZ);
		Sun.calculateRadiation(TA, RH, 0.5); //we force a terrain albedo of 0.5...
		double toa, direct, diffuse;
		Sun.getHorizontalRadiation(toa, direct, diffuse);
		return Atmosphere::Crawford_ilwr(RH, TA, ISWR, direct+diffuse, static_cast<unsigned char>(month));
	} else {
		return Atmosphere::Crawford_ilwr(RH, TA, IOUtils::nodata, IOUtils::nodata, static_cast<unsigned char>(month), cloudiness);
	}
}

/**
 * @brief Evaluate the long wave radiation for clear or cloudy sky.
 * This uses the formula from Unsworth and Monteith -- <i>"Long-wave radiation at the ground"</i>,
 * Q. J. R. Meteorolo. Soc., Vol. 101, 1975, pp 13-24 coupled with a clear sky emissivity following Dilley, 1998.
 * If no cloud cover fraction is provided, a parametrization (solar index according to Kasten and Czeplak (1980))
 * using iswr_meas and iswr_clear_sky will be used. These parameters can therefore safely
 * be set to IOUtils::nodata if cloudiness is provided.
 * @param RH relative humidity (between 0 and 1)
 * @param TA Air temperature (K)
 * @param iswr_meas Measured Incoming Short Wave Radiation (W/m^2)
 * @param iswr_clear_sky Clear Sky Modelled Incoming Short Wave Radiation (W/m^2)
 * @param cloudiness Cloud cover fraction (between 0 and 1, optional)
 * @return long wave radiation (W/m^2) or IOUtils::nodata at night time
*/
double Atmosphere::Unsworth_ilwr(const double& RH, const double& TA, const double& iswr_meas, const double& iswr_clear_sky, const double& cloudiness)
{
	double c;
	if (cloudiness!=IOUtils::nodata) {
		if (cloudiness<0. || cloudiness>1.)
			return IOUtils::nodata;
		c = cloudiness;
	} else {
		if (iswr_meas<=0. || iswr_clear_sky<=0.)
			return IOUtils::nodata;
		c = Kasten_cloudiness(iswr_meas/iswr_clear_sky);
	}

	const double epsilon_clear = Dilley_emissivity(RH, TA);
	const double epsilon = (1.-.84*c)*epsilon_clear + .84*c;

	return blkBody_Radiation(epsilon, TA);
}

/**
 * @brief Evaluate the long wave radiation for clear or cloudy sky.
 * This uses the formula from Unsworth and Monteith -- <i>"Long-wave radiation at the ground"</i>,
 * Q. J. R. Meteorolo. Soc., Vol. 101, 1975, pp 13-24 coupled with a clear sky emissivity following Dilley, 1998.
 * If no cloud cover fraction is provided, a parametrization (according to Kasten and Czeplak (1980))
 * using the current location (lat, lon, altitude) and ISWR will be used. These parameters can therefore safely
 * be set to IOUtils::nodata if cloudiness is provided.
 * @param lat latitude of the point of observation
 * @param lon longitude of the point of observation
 * @param altitude altitude of the point of observation
 * @param julian julian date at the point of observation
 * @param TZ time zone at the point of observation
 * @param RH relative humidity (between 0 and 1)
 * @param TA Air temperature (K)
 * @param ISWR Measured Incoming Short Wave Radiation (W/m^2)
 * @param cloudiness Cloud cover fraction (between 0 and 1, optional)
 * @return long wave radiation (W/m^2) or IOUtils::nodata at night time
 * Please note that this call might NOT be efficient for processing large amounts of points,
 * since it internally builds complex objects at every call. You might want to copy/paste
 * its code in order to process data in bulk.
*/
double Atmosphere::Unsworth_ilwr(const double& lat, const double& lon, const double& altitude,
                                 const double& julian, const double& TZ,
                                 const double& RH, const double& TA, const double& ISWR, const double& cloudiness)
{
	if (TA==IOUtils::nodata || RH==IOUtils::nodata) {
		return IOUtils::nodata;
	}

	if (cloudiness==IOUtils::nodata) {
		if (ISWR==IOUtils::nodata) return IOUtils::nodata;

		SunObject Sun(lat, lon, altitude, julian, TZ);
		Sun.calculateRadiation(TA, RH, 0.5); //we force a terrain albedo of 0.5...
		double toa, direct, diffuse;
		Sun.getHorizontalRadiation(toa, direct, diffuse);
		return Atmosphere::Unsworth_ilwr(RH, TA, ISWR, direct+diffuse);
	} else {
		return Atmosphere::Unsworth_ilwr(RH, TA, IOUtils::nodata, IOUtils::nodata, cloudiness);
	}
}

/**
 * @brief Compute a parametrized Incoming Long Wave Radiation
 * This uses either Atmosphere::Crawford_ilwr or Atmosphere::Omstedt_ilwr or Atmosphere::Brutsaert_ilwr
 * depending on which parameters are available.
 *
 * @param lat latitude of the point of observation
 * @param lon longitude of the point of observation
 * @param altitude altitude of the point of observation
 * @param julian julian date at the point of observation
 * @param TZ time zone at the point of observation
 * @param RH relative humidity (between 0 and 1)
 * @param TA Air temperature (K)
 * @param ISWR Measured Incoming Short Wave Radiation (W/m^2)
 * @param cloudiness fractional cloud cover (between 0 and 1, optional. If provided, it will have the priority)
 * @return long wave radiation (W/m^2) or IOUtils::nodata
 */
double Atmosphere::ILWR_parametrized(const double& lat, const double& lon, const double& altitude,
                                     const double& julian, const double& TZ,
                                     const double& RH, const double& TA, const double& ISWR, const double& cloudiness)
{
	static const double iswr_thresh = 5.; //any iswr less than this is not considered as valid for Crawford
	const double ND=IOUtils::nodata; //since we will do lots of comparisons with it...

	if (RH!=ND && TA!=ND && cloudiness!=ND) {
		return Omstedt_ilwr(RH, TA, cloudiness);
	}
	if (lat!=ND && lon!=ND && altitude!=ND && julian!=ND && TZ!=ND && RH!=ND && TA!=ND && ISWR!=ND && ISWR>iswr_thresh) {
		const double ilwr_p = Unsworth_ilwr(lat, lon, altitude, julian, TZ, RH, TA, ISWR);
		if (ilwr_p!=ND) return ilwr_p; //it might have been that we could not compute (for low solar angles)
	}
	if (RH!=ND && TA!=ND) {
		return Brutsaert_ilwr(RH, TA);
	}

	return ND;
}

/**
* @brief Convert a relative humidity to a dew point temperature.
* @param RH relative humidity between 0 and 1
* @param TA air temperature (K)
* @param force_water if set to true, compute over water. Otherwise, a smooth transition between over ice and over water is computed.
* @return dew point temperature (K)
*/
double Atmosphere::RhtoDewPoint(double RH, double TA, const bool& force_water)
{
	TA = IOUtils::K_TO_C(TA);
	double Es, E, Tdw, Tdi; //saturation and current water vapor pressure
	static const double Aw = 611.21, Bw = 17.502, Cw = 240.97; //parameters for water
	static const double Ai = 611.15, Bi = 22.452, Ci = 272.55; //parameters for ice
	static const double Tfreeze = 0.;                          //freezing temperature
	static const double Tnucl = -16.0;                         //nucleation temperature

	//in order to avoid getting NaN if RH=0
	RH += 0.0001;
	assert(RH>0.);
	if (TA >= Tfreeze || force_water==true) {//above freezing point, water
		Es = Aw * exp( (Bw * TA) / (Cw + TA) );
		E = RH * Es;
		Tdw = ( Cw * log(E / Aw) ) / ( Bw - log(E / Aw) );
		return IOUtils::C_TO_K(Tdw);
	}
	if (TA < Tnucl) { //below nucleation, ice
		Es = Ai * exp( (Bi * TA) / (Ci + TA) );
		E = RH * Es;
		Tdi = ( Ci * log(E / Ai) ) / ( Bi - log(E / Ai) );
		return IOUtils::C_TO_K(Tdi);
	}

	//no clear state, we do a smooth interpolation between water and ice
	Es = Ai * exp( (Bi*TA) / (Ci + TA) );
	E = RH * Es;
	Tdi = ( Ci * log(E / Ai) ) / ( Bi - log(E / Ai) );

	Es = Aw * exp( (Bw * TA) / (Cw + TA) );
	E = RH * Es;
	Tdw = ( Cw * log(E / Aw) ) / ( Bw - log(E / Aw) );

	const double di = 1. / ((TA - Tnucl) * (TA - Tnucl) + 1e-6);     //distance to pure ice
	const double dw = 1. / ((Tfreeze - TA) * (Tfreeze - TA) + 1e-6); //distance to pure water
	return IOUtils::C_TO_K( (di / (di + dw) * Tdi + dw / (di + dw) * Tdw) );
}

/**
* @brief Convert a dew point temperature to a relative humidity.
* @param TD dew point temperature (K)
* @param TA air temperature (K)
* @param force_water if set to true, compute over water. Otherwise, a smooth transition between over ice and over water is computed.
* @return relative humidity between 0 and 1
*/
double Atmosphere::DewPointtoRh(double TD, double TA, const bool& force_water)
{
	//Convert a dew point temperature into a Relative Humidity
	//TA, TD are in Kelvins, RH is returned between 0 and 1
	TA = IOUtils::K_TO_C(TA);
	TD = IOUtils::K_TO_C(TD);
	static const double Aw = 611.21, Bw = 17.502, Cw = 240.97; //parameters for water
	static const double Ai = 611.15, Bi = 22.452, Ci = 272.55; //parameters for ice
	static const double Tfreeze = 0.;                          //freezing temperature
	static const double Tnucl = -16.0;                         //nucleation temperature
	double Es, E, Rhi, Rhw;                         //saturation and current water vapro pressure

	if (TA >= Tfreeze || force_water==true) {
		//above freezing point, water
		Es = Aw * exp( (Bw * TA) / (Cw + TA) );
		E  = Aw * exp( (Bw * TD) / (Cw + TD) );
		Rhw = (E / Es);
		return std::min(Rhw, 1.);
	}
	if (TA < Tnucl) {
		//below nucleation, ice
		Es = Ai * exp( (Bi * TA) / (Ci + TA) );
		E  = Ai * exp( (Bi * TD) / (Ci + TD) );
		Rhi = (E / Es);
		return std::min(Rhi, 1.);
	}

	//no clear state, we do a smooth interpolation between water and ice
	Es = Ai * exp( (Bi * TA) / (Ci + TA) );
	E  = Ai * exp( (Bi * TD) / (Ci + TD) );
	Rhi = E / Es;

	Es = Aw * exp( (Bw * TA) / (Cw + TA) );
	E  = Aw * exp( (Bw * TD) / (Cw + TD) );
	Rhw = E / Es;

	const double di = 1. / ((TA - Tnucl) * (TA - Tnucl) + 1e-6);     //distance to pure ice
	const double dw = 1. / ((Tfreeze - TA) * (Tfreeze - TA) + 1e-6); //distance to pure water
	const double Rh = (di / (di + dw) * Rhi + dw / (di + dw) * Rhw);
	return std::min(Rh, 1.);
}

/**
* @brief Calculate the relative Humidity (RH) from specific humidity.
* @param altitude altitude over sea level (m)
* @param TA air temperature (K)
* @param qi specific humidity between 0 and 1
* @return relative humidity between 0 and 1
*/
double Atmosphere::specToRelHumidity(const double& altitude, const double& TA, const double& qi)
{
	const double SatVaporDensity = waterVaporDensity(TA, vaporSaturationPressure(TA));
	const double dryAir_density = stdDryAirDensity(altitude, TA);
	const double RH = qi/(1.-qi) * dryAir_density/SatVaporDensity;

	return std::min(RH, 1.);
}

/**
* @brief Calculate the specific Humidity from relative humidity (RH).
* @param altitude altitude over sea level (m)
* @param TA air temperature (K)
* @param RH relative humidity (between 0 and 1)
* @return specific humidity
*/
double Atmosphere::relToSpecHumidity(const double& altitude, const double& TA, const double& RH)
{
	const double dryAir_density = stdDryAirDensity(altitude, TA);
	const double SatVaporDensity = waterVaporDensity(TA, vaporSaturationPressure(TA));
	const double qi_inv = dryAir_density/(RH*SatVaporDensity) + 1.;

	return 1./qi_inv;
}

} //namespace
