/*
 *  SNOWPACK stand-alone
 *
 *  Copyright WSL Institute for Snow and Avalanche Research SLF, DAVOS, SWITZERLAND
*/
/*  This file is part of Snowpack.
    Snowpack is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Snowpack is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Snowpack.  If not, see <http://www.gnu.org/licenses/>.
*/
/**
 * @file Laws_sn.cc
 * @version 11.06
 * @brief This module contains (ALL) the constitutive laws for the 1d snowpack model \n
 * @par SnLaws is currently a merely static class (all functions and all variables are static members).
 *   In order to initialize the static member variables they are defined below and if dependent
 *   on the VARIANT in use initialized in setStaticData(const& std::string variant). This
 *   function also initializes static containers (swa_k, swa_fb, swa_pc). When calling
 *   compAlbedo(...) or compSnowViscosity(...) setStaticData might be called in case that
 *   the VARIANT has changed, thus some static variables may be adapted at runtime to the
 *   VARIANT specified.
 *
 * This includes Michael Lehning's ALBEDO law as well as Bob Brown's VISCOSITY and HEAT CONDUCTION models.
 * It also contains the SENSIBLE and LATENT heat CONVECTION coefficients.  Moreover, this code
 * is used by ALL snowpack modules -- creep, temperature and water transport.  Many of the
 * laws, for example the ELASTICITY modulus were programmed up by Marc Christen and taken
 * directly from program HAEFELI, the 2d snowpack model.
 */

#include <snowpack/Laws_sn.h>
#include <snowpack/Utils.h>
#include <snowpack/Constants.h>
#include <snowpack/snowpackCore/Metamorphism.h>

using namespace std;
using namespace mio;

/************************************************************
 * static section                                           *
 ************************************************************/

std::string SnLaws::current_variant = "DEFAULT"; // The default variant for SnLaws

/// @brief Switch on or off wind pumping in snow
const bool SnLaws::wind_pump = true;
/// @brief Switch on or off wind pumping in soil
const bool SnLaws::wind_pump_soil = true;

/// @brief Snow extinction coefficients for absorbtion of SW radiation (swa)
//@{
size_t SnLaws::swa_nBands = 5;      ///< Number of bands used
std::vector<double> SnLaws::swa_k;  ///< mean extinction coefficient for pure ice per band
std::vector<double> SnLaws::swa_pc; ///< fraction of sun power spectrum per band
std::vector<double> SnLaws::swa_fb; ///< fudge_bohren
//@}

/**
 * @name SOIL PARAMETERS
 *
 * @brief Define Method and Coefficents for the computation of the influence of soil water
 * content on Evaporation from Bare Soil Layers:
 *  - Resistance Approach, see Laws_sn.c
 *  - Relative Humidity Approach, see Snowpack.cc
 *  - none, assume saturation pressure and no extra resistance
 */
//@{
//const SnLaws::soil_evap_model SnLaws::soil_evaporation = EVAP_RESISTANCE; //EVAP_RELATIVE_HUMIDITY

/// @brief Minimum soil surface resistance, 50 sm-1 (van den Hurk et al, 2000)
const double SnLaws::rsoilmin = 50.0;

/// @brief Minimum relative saturation of top soil layer
const double SnLaws::relsatmin = 0.05;

/// @brief Ratio of porosity to tortuosity for Soil
const double SnLaws::alpha_por_tor_soil = 0.05;

/// @brief Pore length for surface soil for ventilation (m)
const double SnLaws::pore_length_soil = 0.01;

/// @brief Field capacity of the soil (1). Above this levels, water begins to drain
const double SnLaws::field_capacity_soil = 0.15;
//@}

/**
 * @name THERMAL CONDUCTIVITY
 * @brief Defines the constants and parameters for computing snow and soil thermal conductivity
 */
//@{
/// @brief Factor controlling ice to ice conduction
const double SnLaws::montana_c_fudge = 0.13;

/// @brief Factor controlling increase in water vapor transport and thus energy transport in wet snow
const double SnLaws::montana_vapor_fudge = 2.5;

/// @brief Defines the formerly used MONTANA fudge factor for old wet snow settling (up to snowpack_r8.0).
const double SnLaws::montana_v_water_fudge = 20.;
//@}

/**
 * @name WIND PUMPING
 * @note soil parameters
 */
//@{
/// @brief Used to decribe advective flux attenuation
const double SnLaws::wind_ext_coef = 0.1;

/// @brief Integral of snow density corresponding to the maximal displacement depth d_pump (m)
const double SnLaws::displacement_coef = 0.7;

/// @brief Ratio of porosity to tortuosity
const double SnLaws::alpha_por_tor = 0.07;
//@}

/// @brief To use J. Hendrikx's parameterization for wind speeds > 2.9 m s-1
const bool SnLaws::jordy_new_snow = false;

/// @brief Defines the smallest allowable viscosity (Pa s) that a viscosity law will return \n
/// Value is DAMM SMALL -- smaller values than this are pretty unrealistic.
const double SnLaws::smallest_viscosity = 1.0e6;

/**
 * @name SNOW VISCOSITY
 * @brief Defines the constants and parameters for computing the snow viscosity.
 * @note {
 * @par
 * The Japanese variant uses a viscosity parametrization by Kojima
 *
 * @par
 * These static variables are only defined below, if you want to change them
 * for your purposes you need to do so in the function SnLaws::setStaticData
 * where these parameters are set according to the VARIANT used
 * }
 * - t_term           : Temperature dependence.
 * 	- _arrhenius_critical, _arrhenius, t_term_837, _stk
 * - visc_ice_fudge   : Empirical constant related to volumetric ice content.
 * - visc_sp_fudge    : Empirical constant related to sphericity of snow grains.
 * - visc_water_fudge : Empirical constant related to volumetric water content.
 * - visc_time_fudge  : Empirical constant related to age of snow (deprecated).
 * - visc_*           : viscosity version:
 * 	- _dflt, _cal, _ant, _897, _837, _stk
 * - setfix           : Quickfix to "harden" antarctic snow (use with CALIBRATION variant only)
 */
//@{
SnLaws::TempDependence SnLaws::t_term = SnLaws::t_term_arrhenius_critical;
SnLaws::ViscosityVersion SnLaws::visc = SnLaws::visc_cal;
double SnLaws::visc_ice_fudge = 1.;
double SnLaws::visc_sp_fudge = 1.;
double SnLaws::visc_water_fudge = 0.;
bool   SnLaws::setfix = false;
//@}

/**
 * @name Event driven density of new snow
 * @note {
 * @par
 * These static variables are only defined below, if you want to change them
 * for your purposes you need to do so in the function SnLaws::setStaticData
 * where these parameters are set according to the VARIANT used
 * }
 * - event : type of event, currently only event_wind implemented
 * - 	event_wind_lowlim : lower limit for event_wind
 * - 	event_wind_highlim : upper limit for event_wind
 */
//@{
SnLaws::EventType SnLaws::event = SnLaws::event_none;
double SnLaws::event_wind_lowlim = 0.0;
double SnLaws::event_wind_highlim = 0.0;
//@}
double SnLaws::min_hn_density = 30.;
double SnLaws::max_hn_density = 250.0;

const bool SnLaws::__init = SnLaws::setStaticData("DEFAULT", "BUCKET");

/**
 * @brief This function is used to give default values to a bunch of static members of SnLaws
 *        it is called when the helper variable __init is initialized (compile-time)
 *        and everytime the user changes the variant and invokes either compAlbedo(..) or
 *        compSnowViscosity(...)
 * @return always true
 */
bool SnLaws::setStaticData(const std::string& variant, const std::string& watertransport_model)
{
	current_variant = variant;

	if (current_variant == "ANTARCTICA" || current_variant == "POLAR") {
		t_term = t_term_arrhenius_critical;
		visc = visc_dflt;
		visc_ice_fudge = 9.45;
		visc_sp_fudge = 16.5;
		//visc_water_fudge is set to zero by default
		setfix = false;
		if (watertransport_model == "RICHARDSEQUATION" ) {
			// No sophisticated calibration was performed to find this value. The only issue is that a different water transport scheme leads to different settling behaviour.
			// This value is chosen such that the melt curves in spring more or less resemble those of simulations with BUCKET.
			visc_water_fudge = 45.;
		} else {
			visc_water_fudge = 33.;
		}
		if (current_variant == "ANTARCTICA") {
			event = event_wind;
			event_wind_lowlim = 4.0;
			event_wind_highlim = 7.0;
		} else {
			// For other variants, event_wind is used in conjunction with WIND_EROSION == REDEPOSIT, in which case there is no wind speed limit on redeposition.
			event = event_wind;
			event_wind_lowlim = 0.0;
			event_wind_highlim = 100.0;
		}
	} else if (current_variant == "CALIBRATION") {
		// actual calibration; see factors in Laws_sn.cc
		t_term = t_term_arrhenius_critical;
		visc = visc_cal; //visc_897; //visc_ant; //
		// Revision 837 & Steinkogler
// 		t_term = t_term_837; //_stk; // settings from r837 or WSteinkogler (stk)
// 		visc = visc_837; //visc_stk; //
		visc_ice_fudge = 1.;
		visc_sp_fudge = 1.; //see factors in Laws_sn.cc
		visc_water_fudge = 1.;
// 		visc_time_fudge = 11.;
// 		visc_ice_fudge = 0.5; //0.5 // 837=0.5, stk=0.51
// 		visc_sp_fudge = 0.3; //0.3 // 837=0.3, stk=1.19
// 		visc_water_fudge = 31.;
		setfix = false;
	} else {
		t_term = t_term_arrhenius_critical;
		visc = visc_dflt;
		visc_ice_fudge = 9.45;
		visc_sp_fudge = 16.5;
		if (watertransport_model == "RICHARDSEQUATION" ) {
			// No sophisticated calibration was performed to find this value. The only issue is that a different water transport scheme leads to different settling behaviour.
			// This value is chosen such that the melt curves in spring more or less resemble those of simulations with BUCKET.
			visc_water_fudge = 45.;
		} else {
			visc_water_fudge = 33.;
		}
		setfix = false;
	}

	// snow extinction coefficients; values in use since r140
	double k_init[5]  = {0.059, 0.180, 0.525, 4.75, 85.23};
	double fb_init[5] = {29., 15., 5., 9., 35.};
	double pc_init[5] = {16.3, 16.8, 15.4, 31.0, 20.5};
	swa_k.resize(swa_nBands);
	swa_pc.resize(swa_nBands);
	swa_fb.resize(swa_nBands);
	std::copy(k_init, k_init+swa_nBands, swa_k.begin());
	std::copy(pc_init, pc_init+swa_nBands, swa_pc.begin());
	std::copy(fb_init, fb_init+swa_nBands, swa_fb.begin());

	return true;
}

/**
 * @name THERMAL CONDUCTIVITY OF ICE
 * @brief Based on master thesis of Tobias Hipp, who used relationships by Ling & Yhang (2005).
 * @version 11.03
 * @param Temperature Temperature (K)
 * @return Thermal conductivity of ice
 */
double SnLaws::conductivity_ice(const double& Temperature)
{
	const double ki = 0.4685 + 488.19 / Temperature;
	return ki;
}

/**
 * @name THERMAL CONDUCTIVITY OF WATER
 * @brief Based on master thesis of Tobias Hipp, who used relationships by Ling & Yhang (2005).
 * @version 11.03
 * @param Temperature Temperature (K)
 * @return Thermal conductivity of water
 */
double SnLaws::conductivity_water(const double& Temperature)
{
	const double kw = 0.11455 + 1.6318E-3 * Temperature;
	return kw;
}

/**
 * @name Snow albedo
 * @brief Computes the density of new snow. The options for SNOW_ALBEDO are:
 * - PARAMETERIZED (default is LEHNING_2):
 * 	- LEHNING_[012] : Statistical models of surface snow albedo based on measurements
 *      from the Weissfluhjoch study plot (SWin and SWout, K&Z CM21).
 * 	- SCHMUCKI_* : Edgar Schmucki's statistical models (Jan 2014) based on SWin and SWout measurements at 4 stations:
 *      Weissfluhjoch study plot (WFJ, 2540 m asl; K&Z CM21), Davos (DAV, 1594 m asl; K&Z CM21), Napf (NAP, 1404 m asl; K&Z CM21),
 *      and Payerne (PAY, 490 m asl; K&Z CM21). The variant (*_GSZ) considers grain size as a parameter, while the *_OGS variant
 *      replaces grain size with optical equivalent grain size. The user can choose from two average values, ALL_DATA and CUSTOM,
 *      the former being the average of all albedo values obtained from all four stations, the latter being the mean of the albedo averages for
 *      each single station WFJ, DAV, and PAY; it seems better suited for stations lying below 1500 m asl in Switzerland.
 * 	- NIED : The Japanese version of LEHNING_2
 * - MEASURED: Use measured incoming and reflected shortwave radiation fluxes; limited quality checks will be performed.
 *             The chosen parameterization will be computed for comparison.
 * - FIXED: Use a fixed albedo by assigning ALBEDO-FIXEDVALUE a value between 0.05 and 0.95.
 * @param i_snow_albedo type of albedo computation ()
 * @param i_albedo_parameterization (see above)
 * @param i_albAverageSchmucki
 * @param i_albedo_fixed_value to use
 * @param Edata compute albedo for this element
 * @param Tss Snow surface temperature (K)
 * @param Mdata
 */
double SnLaws::parameterizedSnowAlbedo(const std::string& i_snow_albedo, const std::string& i_albedo_parameterization, const std::string& i_albAverageSchmucki, const double& i_albNIED_av,
                                       const double& i_albedo_fixedValue, const ElementData& Edata, const double& Tss, const CurrentMeteo& Mdata, const SnowStation& Xdata, const bool& ageAlbedo)
{


        const int loc_I = Xdata.meta.position.getGridI();
        const int loc_J = Xdata.meta.position.getGridJ();

	double Alb = Constants::min_albedo;
	const double Ta = Mdata.ta;
	double age = (ageAlbedo)? Mdata.date.getJulian() - Edata.depositionDate.getJulian() : 0.;
             

	if (i_snow_albedo == "FIXED") {
		Alb = i_albedo_fixedValue;
	} else if ((ageAlbedo && (age > 365.)) || (Edata.mk % 10 == 7)) {
		Alb = Constants::glacier_albedo;
	}
	else if (i_albedo_parameterization == "LEHNING_0") {
		static const double weight=0.1;
		static const double a = -0.2, b = 1.3, c = -0.012, d = -0.011, e = 0.0024, f = 0.018;
		static const double g = -7.8e-6, h = -3.1e-3, i = 3.5e-4, j = 1.6e-7, k = -2.4e-7;
		static const double l = -9.2e-6, m = 6.7e-5, n = -7.2e-5, o = 2.0e-4;

		const double sqrt_age = (age<0.001)? Constants::eps : sqrt(age);
		const double lwi = Constants::stefan_boltzmann*Mdata.ea*Optim::pow4(Ta);
		const double Alb1 = exp(a + b*sqrt_age + c*Tss + d*Mdata.vw + e*Mdata.rswr + f*Mdata.rh
		+ g*sqrt_age*Optim::pow2(Ta) + h*sqrt_age*Tss + i*sqrt_age*lwi
		+ j*Optim::pow2(Ta)*Tss + k*Optim::pow2(Ta)*lwi + l*Tss*Mdata.rswr
		+ m*Tss*lwi + n*Tss*Mdata.rh + o*Mdata.vw*Mdata.rh);
		Alb = weight * Edata.dd * Snowpack::new_snow_albedo + (1. - weight * Edata.dd) * Alb1;
	}
	else if (i_albedo_parameterization == "LEHNING_1") {
		double mf = 0.;
		static const double av = 0.77;
		static const double Cta = -0.0052, Cv = 0.0056, Clwc = -3.0, Crho = -0.0003, Cmf = -0.032;
		static const double Crb = 0.06, Cdd = 0.017, Csp = 0.021, Ctss = 0.0084, Cswout = -6.8e-5;
		static const double Cta_tss = -1.1e-5;

		if (Edata.mk%100 > 19) {
			mf = 1.;
			// av *= exp(-age/1700.);
		}
		const double Alb1 = Crho*Edata.Rho + Clwc*(Edata.theta[WATER]+Edata.theta[WATER_PREF]) + Cdd*Edata.dd + Csp*Edata.sp
		+ Cmf*mf + Crb*Edata.rb +  Cta*Ta + Ctss*Tss
		+ Cv*Mdata.vw+ Cswout*Mdata.rswr + Cta_tss*Ta*Tss;
		if (Alb1 >= -1.) Alb = av + log(1.0 + Alb1);
		Alb = std::max(Constants::min_albedo, Alb);
	}
	else if (i_albedo_parameterization == "LEHNING_2") {

		//TODO: this perfoms very badly (if not completly wrong) for (very?) wet snowpack
		//for example, February 2007 in Davos with very warm weather resulting in (measured?) albedos of 0.3 ...
		double av = 0.8042; // Value of original regression
		if (!ageAlbedo) { // NOTE clean antarctic snow
			av = 0.7542; // estimated from comparison with measurements at Dome C
		} else {
			age = std::min(30., age);
		}

		static const double inter = 1.442;
		static const double Cage = -0.000575, Cta = -0.006, Cv = 0.00762, Clwc = -0.2735;
		static const double Crho = -0.000056, Crh = 0.0333, Crb = -0.301, Crg = 0.175;
		static const double Cdd = 0.064, Csp = -0.0736, Ctss = 0.00459, Cswout = -0.000101;
		const double Alb1 = inter + Cage*age + Crho*Edata.Rho + Clwc*(Edata.theta[WATER]+Edata.theta[WATER_PREF])
		+ Cdd*Edata.dd + Csp*Edata.sp + Crg*Edata.rg + Crb*Edata.rb
		+ Cta*Ta + Ctss*Tss + Cv*Mdata.vw + //Cswout*Mdata.rswr
		+ Crh*Mdata.rh;

       //         std::cout << loc_I << "," << loc_J << "," <<  Alb1 << "," << inter <<  "," << age << "," << Edata.Rho << "," << 
       //                      Edata.theta[WATER] << "," << Edata.theta[WATER_PREF] << "," <<
       //                      Edata.dd << "," << Edata.sp << "," << Edata.rg << "," << Edata.rb << ","
       //                      << Ta << "," << Tss << "," << Mdata.vw << "," << Mdata.rh << std::endl;

		if (Alb1 > 0.) {
			Alb = std::max(Constants::min_albedo, std::min(Constants::max_albedo, av + log(Alb1)));
		} else {
			Alb = Constants::min_albedo;
			prn_msg(__FILE__, __LINE__, "wrn", Mdata.date, "Alb1=%lf set Alb to %lf", Alb1, Alb);
		}
	}
	else if (i_albedo_parameterization == "SCHMUCKI_GSZ") { //from SCHMUCKI_ALLX35 regression with classical grain size
		double Alb1, av;
		if (i_albAverageSchmucki == "ALL_DATA")
			av = 0.7832462; // Mean value for all data @ WFJ, DAV, NAP, and PAY
		else if (i_albAverageSchmucki == "CUSTOM")
			av = 0.74824; // mean of single averages @ WFJ, DAV, and PAY
		else
			throw UnknownValueException("Invalid average value chosen for the \'"+i_albedo_parameterization+"\' parametrization: \'"+i_albAverageSchmucki+"\'", AT);

		static const double inter = 1.178904;
		static const double Cms = -5.691804e-02, Cage = -2.840603e-04, Crg = -1.029158e-01, Crho = -5.030213e-04, Cswin = -6.780479e-5;
		const double moist_snow = ((Edata.theta[WATER]+Edata.theta[WATER_PREF]) > SnowStation::thresh_moist_snow)? 1. : 0.;
		Alb1 = inter + Cms*moist_snow + Cage*age + Crg*(Edata.rg) + Crho*Edata.Rho + Cswin*Mdata.iswr;

		if (Alb1 > 0.) {
			Alb = std::max(Constants::min_albedo, std::min(Constants::max_albedo, av + log(Alb1)));
		} else {
			Alb = Constants::min_albedo;
			prn_msg(__FILE__, __LINE__, "wrn", Mdata.date, "Alb1=%lf set Alb to %lf", Alb1, Alb);
		}
	}
	else if (i_albedo_parameterization == "SCHMUCKI_OGS") { //from SCHMUCKI_ALLX32 regression with optical grain size
		double Alb1, av;
		if (i_albAverageSchmucki == "ALL_DATA")
			av = 0.7832462; // Mean value of regression @ WFJ  only
		else if (i_albAverageSchmucki == "CUSTOM")
			av = 0.74824; // Mean of single regressions @ WFJ, DAV, PAY, and NAP
		else
			throw UnknownValueException("Invalid average value chosen for the \'"+i_albedo_parameterization+"\' parametrization: \'"+i_albAverageSchmucki+"\'", AT);

		static const double inter = 1.148088;
		static const double Cms = -4.412422e-02, Cage = -1.523871e-03, Cogs = -1.099020e-01, Crho = -3.638010e-04, Cswin = -7.140708e-05;
		const double moist_snow = ((Edata.theta[WATER]+Edata.theta[WATER_PREF]) > SnowStation::thresh_moist_snow)? 1. : 0.;
		Alb1 = inter + Cms*moist_snow + Cage*age + Cogs*(Edata.ogs/2.) + Crho*Edata.Rho + Cswin*Mdata.iswr;

		if (Alb1 > 0.) {
			Alb = std::max(Constants::min_albedo, std::min(Constants::max_albedo, av + log(Alb1)));
		} else {
			Alb = Constants::min_albedo;
			prn_msg(__FILE__, __LINE__, "wrn", Mdata.date, "Alb1=%lf set Alb to %lf", Alb1, Alb);
		}
	}
	else if (i_albedo_parameterization == "NIED") { // by H. Hirashima (NIED, Nagaoka, Japan)
		const double av = i_albNIED_av;
		const double inter = 1.005;
		const double Cage = -0.00016*10.0, Cta = -0.000249*2.0, Cv = 0.00578, Clwc = -2.15;
		const double Crho = -0.000047, Crh = 0.129, Crb = -0.306, Crg = 0.107;
		const double Cdd = 0.076, Csp = 0.00964, Ctss = -0.000166, Cswout = -1.8e-5;

		const double Alb1 = inter + Crho*Edata.Rho + Clwc*(Edata.theta[WATER]+Edata.theta[WATER_PREF]) + Cdd*Edata.dd + Csp*Edata.sp
		+ Crg*Edata.rg + Crb*Edata.rb + Cta*Ta + Ctss*Tss + Cv*Mdata.vw
		+ Cswout*Mdata.rswr + Crh*Mdata.rh + Cage*age;

		if (Alb1 > 0.) {
			Alb = std::max(Constants::min_albedo, std::min(Constants::max_albedo, av + log(Alb1)));
		} else {
			Alb = Constants::min_albedo;
			prn_msg(__FILE__, __LINE__, "wrn", Mdata.date, "Alb1=%lf set Alb to %lf", Alb1, Alb);
		}
	}
	else if (i_albedo_parameterization == "RACMO") { // Albedo parametrization for Kuipers albedo
		std::vector<double> albc_i;
		std::vector<double> albs_i;
		std::vector<double> zmwe_i;
		std::vector<double> z_i;

		// Get layer information
		const size_t nE = Xdata.getNumberOfElements();

		// Loop over layers to calculate layer characteristics
		const size_t nlayers = std::min(size_t(50), nE);
		double zmwe_tot = 0.;
		//double z_tot = 0.;
		for (size_t i = 1; i < 1 + nlayers; i++) {
			// Albedo parametrization clean snow
			double S = 3. / Xdata.Edata[nE-i].ogs * 2000. / 91.7;
			double as = 1.48 - pow(S, -.07);
			//double as = 1.48 - 1.27048 * pow(Xdata.Edata[nE-i].ogs / 2. / 1000., 0.07);
			// Albedo parametrization for dirty snow
			double ppm = Xdata.Edata[nE-i].soot_ppmv;
//			if (ppm < 0.2) {
//				ppm = 0.002;
//			}
			double dac = - pow(ppm, 0.55) / (.16 + .6 * pow(S, .5) + 1.8 * pow(ppm, .6) * pow(S, -.25));
			double ac = as + dac;

//			// Albedo parametrization for solar zenith
// 			double elv;
// 			if (Mdata.elev > 0.)
// 				elv = Mdata.elev;
// 			else
// 				elv = 1.;
// 			double u = cos((90.-elv) * mio::Cst::to_rad);
// 			double x = std::min(pow(Mdata.odc / 3. / u, .5), 1.);
// 			double uacc = 0.64 * x + (1. - x) * u;
// 			double daz = 0.53 * as * (1. - ac) * pow((1. - uacc), 1.2);
// 			// Albedo parameterization for cloudiness
// 			double dtau = 0.1 * Mdata.odc * pow(ac, 1.3) / pow((1. + 1.5 * Mdata.odc), as);
// 			// Albedo parameterization for pressure
// 			double dh = 0.;
// 			if (Mdata.p != 0. && Mdata.p != mio::IOUtils::nodata)
// 				dh = std::min(0.03247 * log(Mdata.p / 153880.), 0.);

			// Total albedo
			albc_i.push_back(ac);
			albs_i.push_back(as);

			// Length
			zmwe_tot += Xdata.Edata[nE-i].M / 1000.;
			zmwe_i.push_back(zmwe_tot);
			//z_tot += Xdata.Edata[nE-i].L;
			//z_i.push_back(zmwe_tot);

			// Print to screen
			//prn_msg(__FILE__, __LINE__, "wrn", Mdata.date, "Alb=%lf with as=%lf, dac=%lf, daz=%lf and dtau=%lf", at, as, dac, daz, dtau);
		}

		// Loop over layers to integrate albedo
		double albc_sub = 0.;
		double albs_sub = 0.;
		for (size_t i = 0; i < nlayers - 1; i++) {
			albc_sub += (albc_i[i+1] - albc_i[i]) * exp(-zmwe_i[i] / 0.01);
			albs_sub += (albs_i[i+1] - albs_i[i]) * exp(-zmwe_i[i] / 0.01);
		}
		// Calculate clean and dirty snow albedo
		double ac = albc_i[0] + albc_sub;
		double as = albs_i[0] + albs_sub;

		// Two layer model of Gardner
		//double ac_top = albc_i[0];
		//double ac_bot = albc_i[1];
		//double as_top = albs_i[0];
		//double as_bot = albs_i[1];
		//double ppm = 0.1;
		//double AA = std::min(1., 2.1 * pow(zmwe_i[0], 1.35*(1. - as_top) - 0.1 * ppm - 0.13));
		//double dac = (ac_bot - as_top) + AA * (ac_top - ac_bot);
		//double as = albs_i[0];
		//double ac = as + dac;

		// Albedo parametrization for solar zenith
		double elv;
		double u;
		double x;
		if (Mdata.elev > 0.01) {
			elv = Mdata.elev;
			u = cos((90. * mio::Cst::to_rad) - elv);
			x = std::min(pow(Mdata.odc / 3. / u, .5), 1.);
		} else {
			elv = 0.01;
			u = cos((90. * mio::Cst::to_rad) - elv);
			x = 0.;
		}
		double uacc = 0.64 * x + (1. - x) * u;
		//uacc = u;
		//prn_msg(__FILE__, __LINE__, "wrn", Mdata.date, "OD=%lf, u=%lf, x=%lf, uacc=%lf", Mdata.odc, u, x, uacc);
		double daz = 0.53 * as * (1. - ac) * pow((1. - uacc), 1.2);
		// Albedo parameterization for cloudiness
		double dtau = 0.1 * Mdata.odc * pow(ac, 1.3) / pow((1. + 1.5 * Mdata.odc), as);
		// Albedo parameterization for pressure
		double dh = 0.;
		if (Mdata.p != 0 && Mdata.p != mio::IOUtils::nodata)
			dh = std::min(0.03247 * log(Mdata.p / 153880.), 0.);

		//prn_msg(__FILE__, __LINE__, "wrn", Mdata.date, "Alb=%lf with as=%lf, ac=%lf, daz=%lf, elv=%lf", ac + daz, as, ac, daz, elv);
		Alb = ac + daz + dtau + dh;
	}
	else {
		prn_msg(__FILE__, __LINE__, "err", Date(), "Albedo parameterization %s not implemented yet!", i_albedo_parameterization.c_str());
		throw IOException("The required snow albedo model is not implemented yet!", AT);
	}

	return(Alb);
}

/**
 * @brief Helen LeVesconte's Solution to short wave absorption by the snowpack
 * NOTE on fudge_bohren (fb): Larger values increase extinction --> Energy stays on top.
 * @param i_sw_absorption_scheme use multi band or single band approach
 * @param Xdata
 * @param I0 net shortwave radiation (W m-2)
 */
void SnLaws::compShortWaveAbsorption(const std::string& i_sw_absorption_scheme, SnowStation& Xdata, const double& I0)
{
	ElementData *EMS = &Xdata.Edata[0];
	const size_t nE = Xdata.getNumberOfElements();
	if (nE==0) return;

	const size_t bottom_element = (Xdata.SoilNode > 0)? Xdata.SoilNode - 1 : Xdata.SoilNode;
	for (size_t e = bottom_element; e < nE; e++)
		EMS[e].sw_abs = 0.;

	// Compute absorbed radiation
	if (i_sw_absorption_scheme == "MULTI_BAND") {
		for (size_t ii = 0; ii < swa_nBands; ii++) {
			double I0_band = I0 * swa_pc[ii] / 100.;
			for (size_t e = nE-1; e > bottom_element; e--) {
				const double Ks = swa_fb[ii] * 0.84 * sqrt( 1000. * swa_k[ii] / (2. * EMS[e].rg) ) * EMS[e].Rho
				       / Constants::density_ice;
				// Radiation absorbed by element e in band i, transparent (=0) for water
				const double dI = (EMS[e].mk%10 != 9)? I0_band * (1. - exp(-Ks * EMS[e].L)) : 0.;
				EMS[e].sw_abs += dI;
				I0_band -= dI;
			}
			if (I0_band > 5.e-4)
				EMS[bottom_element].sw_abs += I0_band;
		}
	} else if (i_sw_absorption_scheme == "SINGLE_BAND") { // ad hoc "1-Band" model
		double I0_band = I0;
		for (size_t e = nE-1; e > bottom_element; e--) {
			// Radiation absorbed by element e, transparent (=0.) for water
			const double dI = std::max(I0_band, (EMS[e].mk%10 != 9) ? I0_band * (1. - exp(-EMS[e].extinction() * EMS[e].L)) : 0.);
			EMS[e].sw_abs += dI;
			I0_band -= dI;
		}
		EMS[bottom_element].sw_abs += I0_band;
	} else if (i_sw_absorption_scheme == "SURFACE") { // absorb all shortwave energy in topmost element
		EMS[nE-1].sw_abs += I0;
	} else {
		throw InvalidArgumentException("Unknown shortwave absorption model: " + i_sw_absorption_scheme, AT);
	}

	for (size_t e = bottom_element; e < nE; e++) {
		if (EMS[e].sw_abs < 0.) {
			prn_msg(__FILE__, __LINE__, "err", Date(), "NEGATIVE Shortwave Radiation %lf absorbed by element %d (nE=%d)",
				   EMS[e].sw_abs, e, nE);
			throw IOException("SnLaws::compShortWaveAbsorption did not complete successfully", AT);
		}
	}
}

/**
 * @brief Advective Heat Flux injection (mimicking heat advection by infiltrating water?)
 * @param Xdata The advective heat flux will be added to Xdata.sw_abs
 * @param advective_heat heat flux (positive or negative) to add (W m-3) //NO_HACK: input as W/m2 seems more logical but wrong! It is a source term!
 * @param depth_begin depth where to begin injecting the heat flux (in m from the soil surface)
 * @param depth_end depth where to stop injecting the heat flux (in m from the soil surface)
 */
void SnLaws::compAdvectiveHeat(SnowStation& Xdata, const double& advective_heat, const double& depth_begin, const double& depth_end)
{
	ElementData *EMS = &Xdata.Edata[0];
	NodeData   *NDS = &Xdata.Ndata[0];

	const size_t top_element = (Xdata.SoilNode > 0)? Xdata.SoilNode - 1 : 0;
	const double z1 = NDS[top_element].z - depth_end; //HACK depth from the surface is WRONG since this moves with the snow... specify from the bottom?
	const double z2 = NDS[top_element].z - depth_begin;
	const double z_end = (z1<z2)? z1 : z2; //min value
	const double z_begin = (z1>z2)? z1 : z2; //max value

	// Add advective energy flux to shortwave energy flux
	for (size_t e = 0; e < top_element; e++) {
		if ( (NDS[e+1].z < z_begin) && (NDS[e].z > z_end) ) {
			EMS[e].sw_abs += advective_heat*EMS[e].L;
		}
	}
}

/**
 * @brief Computes the displacement depth in case of ventilation
 * @version 10.01
 * @param Xdata
 * @return Displacement depth (m)
 */
double SnLaws::compWindPumpingDisplacement(const SnowStation& Xdata)
{
	size_t e = Xdata.getNumberOfElements();
	double d_pump=0.;      // Displacement depth (m)
	double hw = 0.0;       // Cumulated water equivalent (kg m-2)

	while ( e > 0 && hw < SnLaws::displacement_coef) {
		e--;
		if ((hw + Xdata.Edata[e].L * Xdata.Edata[e].Rho) < SnLaws::displacement_coef)
			d_pump += Xdata.Edata[e].L;
		else
			d_pump += (SnLaws::displacement_coef - hw) / Xdata.Edata[e].Rho;
		hw += Xdata.Edata[e].L * Xdata.Edata[e].Rho;
	}
	return (d_pump);
}

/**
 * @brief Computes the wind pumping velocity at the surface
 * @version 10.01
 * @param Mdata
 * @param d_pump Displacement depth (m)
 * @return Wind pumping velocity (m s-1)
 */
double SnLaws::compWindPumpingVelocity(const CurrentMeteo& Mdata, const double& d_pump)
{
	//TODO Limit v_pump?
	return (Mdata.ustar / 0.4 * log((Mdata.z0 + d_pump) / Mdata.z0));
}

/**
 * @brief Computes the wind gradient near the snow-cover surface
 * @param Edata
 * @param v_pump Wind velocity at element depth (m s-1)
 * @return Wind pumping velocity gradient (s-1)
 */
double SnLaws::compWindGradientSnow(const ElementData& Edata, double& v_pump)
{
	const double v_EXt = SnLaws::wind_ext_coef * (Edata.Rho + 2.e4 * (Edata.theta[WATER]+Edata.theta[WATER_PREF]));
	const double dv = v_pump * (1. - exp(-v_EXt * (Edata.L)));
	v_pump -= dv;

	return (dv / (Edata.L));
}

/**
 * @brief Heat conduction in soil.
 * The formulation is based on curve fitting, the frozen soil data from
 * Kersten in <i>"Geotechnical Engeneering for Cold Regions"</i> article by Harlan and Nixon,
 * the water influence deduced from deVries and Afgan in <i>"Heat and Mass Transfer in the Biosphere"</i>.
 * @version 11.03: thermal conductivity made temperature dependent.
 * @param Edata
 * @param dvdz Wind velocity gradient (s-1)
 * @return Soil thermal conductivity (W K-1 m-1)
 */
double SnLaws::compSoilThermalConductivity(const ElementData& Edata, const double& dvdz)
{
	double C_eff_soil;

	//0 means no soil, 10000 means rock
	if ((Edata.rg > 0.) && (Edata.rg < 10000.)) {
		static const double c_clay = 1.3, c_sand = 0.27;
		static const double beta1 = 6., beta2 = 4.978, c_mineral = 2.9;
		const double weight = (c_clay - Edata.soil[SOIL_K]) / (c_clay - c_sand);
		const double C_eff_soil_max = Edata.theta[SOIL] * c_mineral + (Edata.theta[WATER]+Edata.theta[WATER_PREF])
		                              * SnLaws::conductivity_water(Edata.Te) + Edata.theta[ICE]
		                              * SnLaws::conductivity_ice(Edata.Te);

		C_eff_soil = (beta1 + weight * beta2) * Edata.theta[ICE];
		if ((Edata.theta[WATER]+Edata.theta[WATER_PREF]) > SnowStation::thresh_moist_soil) {
			static const double alpha1 = 0.389, alpha2 = 0.3567, alpha3 = 61.61;
			C_eff_soil += std::max( 0.27, (alpha1 + alpha2 * weight) * log(alpha3 * (Edata.theta[WATER]+Edata.theta[WATER_PREF])) );
		} else {
			C_eff_soil += 0.27;
		}
		C_eff_soil = std::min(C_eff_soil_max, C_eff_soil);
	} else {
		C_eff_soil = Edata.soil[SOIL_K] + (Edata.theta[WATER]+Edata.theta[WATER_PREF]) * SnLaws::conductivity_water(Edata.Te)
                       + Edata.theta[ICE] * SnLaws::conductivity_ice(Edata.Te);
	}

	// Now check for possible ERRORS
	if (!(C_eff_soil >= 0.1*Edata.soil[SOIL_K] && C_eff_soil < 10.)) //HACK
		prn_msg(__FILE__, __LINE__, "wrn", Date(), "Thermal Conductivity of Soil: %lf", C_eff_soil);

	// In case of dry soil, simply use the given conductivity with a possible ventilation part
	// TODO: check whether the second condition should be activated!
	if (SnLaws::wind_pump_soil /* && Edata.theta[WATER] < 0.0001 */)
		C_eff_soil += SnLaws::alpha_por_tor_soil * Constants::specific_heat_air
		                * Edata.soil[SOIL_RHO] * SnLaws::pore_length_soil * SnLaws::pore_length_soil * dvdz;
	return(C_eff_soil);
}

/**
 * @brief Water vapor diffusion coefficient in soil.
 * The formulation is based on Saito et al., 2006 "Numerical analysis of coupled water vapor
 * and heat transport in the vadose zone", see eq. [14] and [15].
 * It is defined as the product of the tortuosity factor (-) as defined by Millington and Quirck (1961),
 * the air-filled porosity (m3 m-3) and the diffusivity of water vapor in air (m2 s-1).
 * @author Margaux Couttet
 * @param Edata element data
 * @return vapour diffusivity in soil (m2 s-1)
 */
double SnLaws::soilVaporDiffusivity(const ElementData& Edata)
{
    double tortuosity = (Edata.VG.theta_s > Constants::eps2)?(pow(Edata.theta[AIR], 7./3.)/pow(Edata.VG.theta_s, 2.)):(0.);

    return (tortuosity * Edata.theta[AIR] * Constants::diffusion_coefficient_in_air);
}


/**
 * @brief Computes the enhancement factor for water vapor transport in soil.
 * Derived from Cass et al., 1984 "Enhancement of thermal water vapor diffusion in soil", see eq. [19].
 * Describe the increase in thermal vapor flux as a result of liquid islands and increased
 * temperature gradients in the air phase.
 * @author Margaux Couttet
 * @param Edata element data
 * @return Enhancement factor (-)
 */
double SnLaws::compEnhanceWaterVaporTransportSoil(const ElementData& Edata, const double& clay_fraction)
{
	const double r = (Edata.theta[WATER]+Edata.theta[ICE]*Constants::density_ice/Constants::density_water)/Edata.VG.theta_s;
	return ((Edata.VG.theta_s > Constants::eps2)?(9.5 + 3.*(r) - 8.5*exp(-1.*pow((1.+2.6/sqrt(clay_fraction))*r,4.))):(0.));
}

/**
* @brief Computes the soil THERMAL vapor hydraulic conductivity.
* Requires the use of RE to determine pressure head (Edata.h) and saturated water content (theta_s).
* The THERMAL vapor hydraulic conductivy formulation is based on Saito et al., 2006
* "Numerical analysis of coupled water, vapor, and heat transport in the vadose zone", see eq. [13].
* It is used to determine the flux density of water vapor in soil due to THERMAL gradient: q_vT = -Kvapor_T*gradT.
* The enhancement factor is used to describe the increase in the thermal vapor flux as a result of liquid islands
* and increased temperature gradients in the air phase (Philip and de Vries, 1957)
* The relative humidity is calculated from the pressure head (h), using a thermodynamic relationship between liquid water
* and water vapour in soil pores (Philip and de Vries, 1957)
* @author Margaux Couttet
* @param Edata_bot element data
* @param Edata_top element data
* @param Te_bot lower element temperature (K)
* @param Te_top upper element temperature (K)
* @return thermal vapor hydraulic conductivity (m2 K-1 s-1)
*/
double SnLaws::compSoilThermalVaporConductivity(const ElementData& Edata_bot, const ElementData& Edata_top, const double& Te_bot, const double& Te_top, const double& clay_fraction)
{
	//Determine the nodal values by averaging between top and bottom elements
	const double nodal_diffusivity = .5 * (SnLaws::soilVaporDiffusivity(Edata_top) + SnLaws::soilVaporDiffusivity(Edata_bot)); //(m2 s-1)
	const double nodal_HR = .5 * (Edata_top.RelativeHumidity() + Edata_bot.RelativeHumidity()); //(-)
	const double nodal_enhancement = .5 * (SnLaws::compEnhanceWaterVaporTransportSoil(Edata_top,clay_fraction)
	+ SnLaws::compEnhanceWaterVaporTransportSoil(Edata_bot,clay_fraction)); // (-)

	double dRhovs_dT = 0.; // change of water vapor density due to temperature gradient (kg m-3 K-1)
	if (fabs(Te_top - Te_bot) > Constants::eps2) { // if no temperature difference between top and bottom nodes, the vapor density gradient remains zero
		dRhovs_dT = (Atmosphere::waterVaporDensity(Te_top, Atmosphere::vaporSaturationPressure(Te_top)) -
			Atmosphere::waterVaporDensity(Te_bot, Atmosphere::vaporSaturationPressure(Te_bot))) / (Te_top - Te_bot);
	}
	return (nodal_diffusivity/Constants::density_water * nodal_enhancement * nodal_HR * dRhovs_dT);
}

/**
 * @brief Computes the soil ISOTHERMAL vapor hydraulic conductivity.
 * The ISOTHERMAL vapor hydraulic conductivy formulation is based on Saito et al., 2006
 * "Numerical analysis of coupled water, vapor, and heat transport in the vadose zone", see eq. [12].
 * It is used to determine the flux density of water vapor in soil due to MOISTURE gradient: q_vh = -Kvapor_h*gradH.
 * @author Margaux Couttet
 * @param Edata_bot element data
 * @param Edata_top element data
 * @param Te_bot lower element temperature (K)
 * @param Te_top upper element temperature (K)
 * @param T_node nodal temperature (K)
 * @return isothermal vapor hydraulic conductivity (m s-1)
 */
double SnLaws::compSoilIsothermalVaporConductivity(const ElementData& Edata_bot, const ElementData& Edata_top, const double& Te_bot, const double& Te_top, const double& T_node)
{
	//Determine the nodal values by averaging between top and bottom elements
	const double nodal_diffusivity = .5*(SnLaws::soilVaporDiffusivity(Edata_top) + SnLaws::soilVaporDiffusivity(Edata_bot)); //(m2 s-1)
	const double nodal_vaporDensity = .5*(Atmosphere::waterVaporDensity(Te_top, Atmosphere::vaporSaturationPressure(Te_top))
	                                   + Atmosphere::waterVaporDensity(Te_bot, Atmosphere::vaporSaturationPressure(Te_bot))); //(kg m-3)
	const double nodal_HR = .5*(Edata_top.RelativeHumidity() + Edata_bot.RelativeHumidity()); //(-)

	return (nodal_diffusivity/Constants::density_water * nodal_vaporDensity * Constants::g/(Constants::gas_constant * T_node)) * nodal_HR;
}

/**
 * @brief Computes the enhancement factor for water vapor transport in wet snow
 * @version 9Y.mm
 * @param Xdata
 * @param i_e Element index to start from
 * @return Enhancement factor
 */
double SnLaws::compEnhanceWaterVaporTransportSnow(const SnowStation& Xdata, const size_t& i_e)
{
	const size_t max_depth = (i_e>7)? i_e-7 : 0; //NOTE Works only for snow
	double vapor_enhance = 1.;

	const size_t e_stop = std::max(Xdata.SoilNode, max_depth);
	size_t e;
	for(e = i_e; e --> e_stop; ) { //decrements from i_e down to e_stop (included) even if e_stop==0
		//TODO: check limit on theta_water and second condition (to be checked on i_e only??)
		if ((Xdata.Edata[e].theta[WATER] > SnowStation::thresh_moist_snow) && (Xdata.Edata[i_e].theta[ICE] < 0.7)) {
			vapor_enhance = SnLaws::montana_vapor_fudge;
			break;
		}
	}

	if (vapor_enhance > 1.0)
		vapor_enhance *= ((8. - static_cast<double>(i_e - e)) / 7.);

	return (vapor_enhance);
}

/**
 * @brief Heat conduction in snow
 * Actual version: k = C1*[C2 + C3 + C4 + C5] \n
 * Adams/Sato model. The following piece of code was programmed by Perry on a warm
 * June afternoon in Birmensdorf.  H.U. Gubler was viciously attacking Ammann, saying nasty
 * things about our beloved chef, (even if his hair is a bit funny), in the DAVOSER ZEITUNG.
 * Perry was dreaming about San Diego and .... and Michael was dreamy about rock climbing in
 * Italy with funky, spunky Italian girls, Borolo Bob was busy with his wife on his way to
 * Mallorca -- to watch sexy TOPLESS bikini-slip clad GERMAN secretaries on the BEACH.
 * As usual, if this code does not work it is his FAULT ....  (Hey, Bob, when are we going to
 * meet ED ADAMS?  And remember PLEDGE 7 of Metamorphism.c.  The theory behind this piece of
 * code can be found in Bob's ROUGH DRAFT on MICROSTRUCTURE.
 *
 * Much much later, Michael re-programmed this code and introduced the effect of liquid water.
 * He extended the Adams/Sato model and hoped that he would not loose contact with Keegan, even
 * he was not going to fulfill the explicit wishes of his Grandma.
 * The model was now being used from Finland to the US. We will also conquer the southern hemis-
 * sphere.
 * @version 8.10 (??)
 * @param Edata
 * @param dvdz Wind velocity gradient (s-1)
 * @param show_warnings If warnings are produced, show them (default=true)
 * @return Thermal conductivity of snow (W K-1 m-1)
 */
double SnLaws::compSnowThermalConductivity(const ElementData& Edata, const double& dvdz, const bool& show_warnings)
{
	static const double Lh = Constants::lh_sublimation;
	static const double P = 950.;

	const double rg = MM_TO_M(Edata.rg); //Grain radius (m)
	const double rb = MM_TO_M(Edata.rb); //Bond radius (m)
	const double Te = std::min(Edata.Te, Edata.meltfreeze_tk); //Element temperature (K)

	// Check for elements with no ice and assume they contain only water
	if (Edata.theta[ICE] < Snowpack::min_ice_content)
		return(Constants::conductivity_water);

	// Important are the conductivities and cross sectional areas.
	// The conductivity including latent heat transfer (W K-1 m-1)
	const double kap = Constants::conductivity_air + ((Lh*Lh* Constants::diffusion_coefficient_in_air * P
		 * Atmosphere::vaporSaturationPressure(Te))
		 / (Constants::gas_constant*Constants::gas_constant * Optim::pow3(Te) * (P - Atmosphere::vaporSaturationPressure(Te))));

	/*
	* Determine C1 = nca/ncl (nca:=number of grains per unit area; ncl:=number
	* of grains per unit length) = ncl since nca=ncl^2
	* Now we can also determine the series cross sectional areas
	*/
	double C1 =  (3.0 * Edata.Rho / Constants::density_ice) / ( 4.0 * Constants::pi * Optim::pow3(rg));
	C1 =  pow(C1, 0.3333333);

	/*
	* Determine C2 = a funny fraction containing PI and the coordination number
	* (I believe that this is the solid ice conduction from one grain through a
	* bond/neck into another ice grain.)
	*/
	const double C2 = Optim::pow2(Constants::pi) * rb * Constants::conductivity_ice * Edata.N3 / 32.0;

	// Compute cross-sectional areas of conduction paths (m2)
	const double Ap = Metamorphism::csPoreArea(Edata); // (mm2)
	const double Aiw = std::max(0., Edata.theta[WATER] * (1. / C1 - rg) / C1 * (Ap + Constants::pi * rg*rg));
	const double Aip = std::max(0., Constants::pi * (rg*rg - rb*rb) - Aiw);

	/*
	 * Determine C3 = a funny fraction still containing PI but a lot of very funny
	 * k coefficients. (I believe that this is the series conduction in which
	 * heat goes through a grain, then across a pore and into another grain.)
	*/
	const double C3 = (Constants::conductivity_ice * kap * Aip)
	                  / (rg * kap + (1. / C1 - rg) * Constants::conductivity_ice);

	/*
	 * Determine C4 = a funny fraction containing one PI and  a few (one) funny
	 * k coefficients. (I believe that this is the series conduction purely
	 * across the pore, the most believable part.)  Note that the fraction is
	 * divided by 1/C1, so I multiply the whole thing ...
	*/
	const double C4 = C1 * kap * Ap;

	/*
	 * Determine C5 = a funny fraction still containing PI but a lot of very funny
	 * k coefficients. (I believe that this is the series conduction in which
	 * heat goes through a grain, then across water and into another grain.)
	*/
	const double C5 = (Constants::conductivity_ice * Constants::conductivity_water * Aiw)
	                  / (rg * Constants::conductivity_water  + (1./C1 - rg) * Constants::conductivity_ice);

	double C_eff  = SnLaws::montana_c_fudge * C1 * (C2 + C3 + C4 + C5) * (2.0 - Edata.dd) * (1.0 + pow(Edata.theta[ICE], 1.7)) * (0.5 + Optim::pow2(Te/Edata.meltfreeze_tk) );

	if (!((C_eff < 5.*Constants::conductivity_ice) && (C_eff > 0.2*Constants::conductivity_air)) && show_warnings) {
		prn_msg(__FILE__, __LINE__, "wrn", Date(), "Conductivity out of range (0.2*Constants::conductivity_air=%.3lf, 5.*Constants::conductivity_ice=%.3lf):", 0.2 * Constants::conductivity_air, 5. * Constants::conductivity_ice);
		prn_msg(__FILE__, __LINE__, "msg-", Date(), "C_eff: %lf  C_1: %lf  C_2: %lf  C_3: %lf  C_4: %lf  C_5: %lf", C_eff, C1, C2, C3, C4, C5);
		prn_msg(__FILE__, __LINE__, "msg-", Date(), "C_Air: %lf  C_Water: %lf  C_Ice: %lf", Constants::conductivity_air, Constants::conductivity_water, Constants::conductivity_ice);
		prn_msg(__FILE__, __LINE__, "msg-", Date(), "Ap: %lf  kap: %lf  Thet_i: %lf  Thet_w: %lf  T: %lf", Ap, kap, Edata.theta[ICE], Edata.theta[WATER], Te );
		prn_msg(__FILE__, __LINE__, "msg-", Date(), "type: %3d  rg: %.3lf mm  rb: %.3lf mm N3: %lf  Lel: %.3lf mm", Edata.type, Edata.rg, Edata.rb, Edata.N3, M_TO_MM(Edata.L));
	}

	if (!(C_eff < Constants::conductivity_ice))
		C_eff = Constants::conductivity_ice;
	if (!(C_eff > Constants::conductivity_air))
		C_eff = Constants::conductivity_air;
	// Now introduce the effect of windpumping
	if (SnLaws::wind_pump /* && Edata.theta[WATER] < SnowStation::thresh_moist_snow */)
		C_eff += SnLaws::alpha_por_tor * Constants::specific_heat_air * Edata.Rho * dvdz / (C1*C1);
	return (C_eff);
}

/**
 * @brief SENSIBLE HEAT EXCHANGE COEFFICIENT (Surface Energy Exchange)
 * @version 9Y.mm
 * @param Mdata
 * @param Xdata
 * @param height_of_meteo_values Height at which meteo parameters are measured
 * @return Exchange coefficient for sensible heat (1)
*/
double SnLaws::compSensibleHeatCoefficient(const CurrentMeteo& Mdata, const SnowStation& Xdata, const double& height_of_meteo_values)
{
	double lrat;

	//const double z = std::max(0.5, height_of_meteo_values - Xdata.cH + Xdata.Ground);
        const double z = height_of_meteo_values;
        
const int I = Xdata.meta.position.getGridI();
const int J = Xdata.meta.position.getGridJ();
   

	if ((Xdata.cH - Xdata.Ground) > 0.03) {
		//assert(Mdata.z0>0.);
		lrat = log(z / Mdata.z0);
	} else {
		//assert(Xdata.BareSoil_z0>0.);
		lrat = log(z / Xdata.BareSoil_z0);
	}


//        if((I==37) & (J==99)){
//          std::cout << "comp sensible heat coeff:\t" << height_of_meteo_values << "," << Xdata.cH << "," << Xdata.Ground << "," << z << "," << Mdata.ustar << "," << lrat << std::endl;
//        }


	return Constants::karman * Mdata.ustar / std::max(0.7, lrat-Mdata.psi_s);
        //  return Constants::karman * Mdata.ustar / (lrat-Mdata.psi_s) ; 
 
}

/**
 * @brief Latent heat flux including consideration of soil (one active element)
 * This method uses the Relative Humidity approach:
 * ql = beta*(eA - eS) Latent heat transfer. eA and eS are the vapor
 * pressures of air and snow, respectively.
 * @version 9Y.mm
 * @param Mdata
 * @param Xdata
 * @param height_of_meteo_values Height at which meteo parameters are measured
 * @return Latent heat flux (W m-2)
 */
double SnLaws::compLatentHeat_Rh(const Snowpack::soil_evap_model soil_evaporation, const CurrentMeteo& Mdata, SnowStation& Xdata, const double& height_of_meteo_values)
{
	const size_t nElems = Xdata.getNumberOfElements();
	const double T_air = Mdata.ta;
	const double Tss = Xdata.Ndata[nElems].T;
	const double Tse = (nElems > 0) ? (Xdata.Edata[nElems-1].Te) : Constants::meltfreeze_tk;
	double eS;

	// Vapor Pressures
	// TODO The part below needs to be rewritten in a more consistent way !!!
	//      In particular, look closely at the condition within compLatentHeat()
	const double eA = Mdata.rh * Atmosphere::vaporSaturationPressure(T_air); // depending on air temperature, w.r.t to ice or water 
	const double Vp1 = Atmosphere::vaporSaturationPressure(Tss);
	const double Vp2 = Atmosphere::vaporSaturationPressure(Tse);

	// First, the case of no snow
	if (Xdata.getNumberOfNodes() == Xdata.SoilNode + 1 && nElems > 0) {
		if ( Tss < Xdata.Edata[nElems-1].meltfreeze_tk) {
			eS = Vp1 ;
		} else {
			/*
			 * Soil evaporation can now be computed using the Relative Humidity approach below,
			 * or a Resistance approach modifying the ql value instead of the eS. The latter
			 * function is defined in compLatentHeat, and the Switch SnLaws::soil_evaporation is found
			 * in Laws_sn.h
			*/
			if (soil_evaporation==Snowpack::EVAP_RELATIVE_HUMIDITY) {
				eS = Vp2 * Xdata.Edata[Xdata.SoilNode-1].RelativeHumidity();
			} else {
				eS = Vp2;
			}
		}
	} else {
		// for snow assume saturation
		const double meltfreeze_tk = (nElems > 0) ? Xdata.Edata[nElems-1].meltfreeze_tk : Constants::meltfreeze_tk;
		if (Tss < meltfreeze_tk)
			eS = Vp1;
		else
			eS = Vp2;
	}
	// Now the latent heat
	const double beta = SnLaws::compLatentHeat(soil_evaporation, Mdata, Xdata, height_of_meteo_values);

	return (beta * (eA - eS));
}

/**
 * @brief LATENT HEAT EXCHANGE (Surface Energy Exchange)
 * David Gustafsson (davidg@kth.se) has introduced a resistance approach for latent
 * heat exchange from bare soil as alternative to the relative hyumidity (RH) approach
 * implemented in Snowpack.c (line 473): \n
 * An additional resistance, dependent on the relative saturation of the top soil layer,
 * is used to reduce the heat exchange coefficient in the case of evaporation:
 * c = 1/(Ra + Rsoil), where Ra = 1/c as computed above, and
 * Rsoil = 50 [s/m] * field_capacity_soil / theta_soil. \n
 * A new switch SnLaws::soil_evaporation is defined in Constants.h to select method.
 * The resistance formulation originates from van den Hurk et al.(2000) "Offline validation
 * of the ERA40 surface scheme": ECMWF Tech.Memo 295. \n
 * A difference from the RH method is that the surface vapour pressure is always assumed
 * to be at saturation at the surface. Thus, some unrealistic effects of the RH method in
 * present form are avoided -> the RH approach tend to estimate negative vapour gradient
 * between the surface and the atmosphere, causing large condensation, even if the top soil
 * layer is saturated, and even if the soil surface is warmer than the atmosphere! If a RH
 * method should work in a discretized model, it is important to consider the difference
 * between vapour pressure at the surface and the average of the top soil layer. \n
 * The soil resistance is only used for bare soil layers, when TSS >= 0C and eSurf >= eAtm
 * @param[in] Mdata
 * @param[in] Xdata
 * @param[in] height_of_meteo_values Height at which meteo parameters are measured
 * @return Latent heat flux (W m-2)
 */
double SnLaws::compLatentHeat(const Snowpack::soil_evap_model soil_evaporation, const CurrentMeteo& Mdata, SnowStation& Xdata, const double& height_of_meteo_values)
{
	const size_t nElems = Xdata.getNumberOfElements();
	const bool SurfSoil = (nElems > 0) ? (Xdata.Edata[nElems-1].theta[SOIL] > 0.) : false;

	double c = compSensibleHeatCoefficient(Mdata, Xdata, height_of_meteo_values);

	if (SurfSoil && (Xdata.Ndata[nElems].T >= Xdata.Edata[nElems-1].meltfreeze_tk)
		    && (soil_evaporation == Snowpack::EVAP_RESISTANCE)) {
		const double Tse = (nElems > 0) ? (Xdata.Edata[nElems-1].Te) : Constants::meltfreeze_tk;
		const double eA = Mdata.rh * Atmosphere::vaporSaturationPressure( Mdata.ta );
		const double eS = Atmosphere::vaporSaturationPressure( Tse );
		if (eS >= eA) {
			c = 1. / c + SnLaws::rsoilmin / std::max(SnLaws::relsatmin, std::min(1.,
			                                    (Xdata.Edata[nElems-1].theta[WATER]+Xdata.Edata[nElems-1].theta[WATER_PREF])
			                                    / Xdata.Edata[Xdata.SoilNode-1].soilFieldCapacity()));
			c = 1. / c;
		}
	}
	return c * 0.622 * Constants::lh_sublimation / ( Constants::gas_constant_air * Mdata.ta);
}

/**
 * @brief LONGWAVE RADIATION COEFFICIENT
 * This routine might look a bit unusual:
 * Radiation is treated as a CONVECTIVE boundary condition, similar to the sensible and latent heat
 * exchanges.  The exchange coefficient, however, is not a constant, dependent on say the wind
 * velocity, rather it is dependent on the temperature.  This routine computes the "pseudo"
 * convective heat exchange coefficient for radiation.
 * @version 9Y.mm
 * @param t_snow Snow surface temperature (K)
 * @param t_atm Temperature of the atmosphere, i.e., air (K)
 * @param e_atm Emissivity of the atmosphere (1)
 * @return LW radiation coefficient (W m-2 K-1)
 */
double SnLaws::compLWRadCoefficient(const double& t_snow, const double& t_atm, const double& e_atm)
{
	return (Constants::emissivity_snow * Constants::stefan_boltzmann
	           * ((t_snow*t_snow) + (sqrt(e_atm) * t_atm*t_atm))
	               * (t_snow + (pow(e_atm, 0.25) * t_atm)));

}

double SnLaws::compLWRadCoefficient_special(const double& t_snow, const double& ilwr)
{
        const double a = pow((ilwr / (Constants::stefan_boltzmann)),0.25000);


	return (Constants::emissivity_snow * Constants::stefan_boltzmann
	           * ((t_snow*t_snow) + a*a)
	                * (t_snow + a ));

}

/**
 * @brief Event driven new-snow density
 * @param i_event:
 * - event_wind: rho = 250.3 kg m-3 @ 4 m s-1; rho = 338 kg m-3 @ 7 m s-1 Antarctica
 * @param Mdata  Meteorological input
 */
double SnLaws::newSnowDensityEvent(const std::string& variant, const SnLaws::EventType& i_event,
                                   const CurrentMeteo& Mdata)
{
	if (variant != SnLaws::current_variant)
		setStaticData(variant, "BUCKET");

	switch (i_event) {
		case event_wind: {
			// Groot Zwaaftink et al.: Event-driven deposition of snow on the Antarctic Plateau: analyzing field measurements with SNOWPACK
			// Cryosphere, 7, 333-347, https://doi.org/10.5194/tc-7-333-2013, 2013.
			const double z_ref_vw = 3.;				// See p. 336 in Groot Zwaaftink et al.
			const double vw_avg_ref = Meteo::windspeedProfile(Mdata, z_ref_vw, Mdata.vw_avg);
			if ((vw_avg_ref >= event_wind_lowlim) && (vw_avg_ref <= event_wind_highlim)) {
				static const double rho_0=361., rho_1=33.;
				return (vw_avg_ref == 0.) ? (rho_1) : (std::max(0., rho_0*log10(vw_avg_ref)) + rho_1);
			} else
				return Constants::undefined;
		}
		case event_none:
		default:
			prn_msg(__FILE__, __LINE__,"err", Date(),
				"No new snow density parameterization for event type %d", i_event);
			throw IOException("Event type not implemented yet!", AT);
	}
}

/**
 * @brief Parameterized new-snow density
 * @param TA  Air temperature (K)
 * @param TSS Snow surface temperature (K)
 * @param RH  Relative air humidity (1)
 * @param VW  Mean wind velocity (m s-1)
 * @param HH  Altitude a.s.l. (m)
 * @param model Parameterization to be used
 * @return New snow density (kg m-3)
 */
double SnLaws::newSnowDensityPara(const std::string& i_hn_model,
                                  double TA, double TSS, double RH, double VW, double HH)
{
	double rho_hn;

	TA  = IOUtils::K_TO_C(TA);
	TSS = IOUtils::K_TO_C(TSS);
	RH  *= 100.;
	HH  = floor(HH);

	if (i_hn_model == "LEHNING_OLD") {
		static const double alpha=70., beta=30., gamma=10., delta=0.4;
		static const double eta=30., phi=6.0, mu=-3.0, nu=-0.5;
		rho_hn = alpha + beta*TA + gamma*TSS +  delta*RH + eta*VW + phi*TA*TSS + mu*TA*VW + nu*RH*VW;
		if (jordy_new_snow && (VW > 2.9))
			rho_hn = newSnowDensityHendrikx(TA, TSS, RH, VW);

	} else if (i_hn_model == "LEHNING_NEW") {
		static const double alpha=90., beta=6.5, gamma=7.5, delta=0.26;
		static const double eta=13., phi=-4.5, mu=-0.65, nu=-0.17, om=0.06;
		rho_hn = alpha + beta*TA + gamma*TSS +  delta*RH + eta*VW + phi*TA*TSS + mu*TA*VW + nu*RH*VW + om*TA*TSS*RH;
		// Ad hoc temperature correction
		if (TA < -10.)
			rho_hn = std::min(rho_hn, alpha*(1. + 0.05*(TA + 10.)));
		// Increase snow density under snow transport conditions
		if ((!jordy_new_snow) && (VW > 5.)) {
			rho_hn = 90. + (rho_hn - 30.)*0.9;
		} else if (jordy_new_snow && (VW > 2.9)) {
			rho_hn = newSnowDensityHendrikx(TA, TSS, RH, VW);
		}

	} else if (i_hn_model == "BELLAIRE") {
		static const double alpha=3.946, beta=0.07703, zeta=0.0001701, eta=0.02222, mu=-0.05371;
		// Transformations based on natural logarithm!!!
		VW = std::max(1., VW);
		const double arg = alpha + beta*TA + zeta*HH + eta*log(VW) + mu*TA*log(VW);
		rho_hn = exp(arg);

	} else if (i_hn_model == "ZWART") {
		VW = std::max(2., VW);
		RH = 0.8; // ori: std::min(1., RH/100.); see asin(sqrt()) below
		static const double beta01=3.28, beta1=0.03, beta02=-0.36, beta2=-0.75, beta3=0.3;
		double arg = beta01 + beta1*TA + beta2*asin(sqrt(RH)) + beta3*log10(VW);
		if(TA>=-14.) arg += beta02; // += beta2*TA;
		rho_hn = pow(10., arg);

	} else if (i_hn_model == "PAHAUT") {
		rho_hn = 109. + 6.*(IOUtils::C_TO_K(TA) - Constants::meltfreeze_tk) + 26.*sqrt(VW);

	} else if (i_hn_model == "NIED") {
		rho_hn = 62. + 3.6 * VW - 0.2 * TA;

	} else {
		prn_msg(__FILE__, __LINE__, "err", Date(),
		        "New snow density parameterization '%s' not available",
		        i_hn_model.c_str());
		exit(EXIT_FAILURE);
	}

	return(std::min(max_hn_density, std::max(min_hn_density, rho_hn)));
}

/**
 * @brief Jordy Hendrikx' new snow density parameterization for strong winds (> 2.9 m s-1)
 * @note To be used with Lehning's models only!
 * @param ta  Air temperature (degC)
 * @param tss Snow surface temperature (degC)
 * @param rh  Relative air humidity (%)
 * @param vw  Mean wind velocity (m s-1)
 * @return New snow density
 */
double SnLaws::newSnowDensityHendrikx(const double ta, const double tss, const double rh, const double vw)
{
	static const double alpha=91., beta=-35., gamma=-1.1, delta=49., eta=32.,  phi=4.6;
	return (alpha + beta*ta + gamma*rh +  delta*vw + eta*tss + phi*ta*vw);
}

/**
 * @name New snow density
 * @brief Computes the density of new snow. The options for HN_DENSITY are:
 * - PARAMETERIZED (default is LEHNING_NEW):
 * 	- ZWART: Costijn Zwart's model (elaborated 2006; in use since 4 Dec 2007
 * 	- LEHNING_NEW: Improved model by M. Lehning, incl. ad-hoc wind & temperature effects (used until 06/07)
 * 	- LEHNING_OLD: First model by M. Lehning
 *       @note {models by M. Lehning can be augmented with a parameterization for winds > 2.9 m s-1
 *              worked out by J. Hendrikx => set jordy_new_snow in Laws_sn.cc}
 * 	- BELLAIRE: Sascha Bellaire's model (elaborated 2007; used summer/fall 2007)
 * 	- PAHAUT: Edmond Pahaut's model, introduced Sep 1995 in CROCUS by G. Giraud
 * - EVENT: Driven by event type, that is,
 * 	- event_wind: Implemented 2009 by Christine Groot Zwaaftink for Antarctic variant
 * - MEASURED: Use measured new snow density read from meteo input
 * 	-Note: Set HN_DENSITY_FIXEDVALUE to 1. to use surface snow density as a "measured" value in case of missing values
 * - FIXED: Use a fixed new snow density by assigning HN_DENSITY-FIXEDVALUE a value (default: 100 kg m-3, at least min_hn_density)
 * @param i_hn_density type of density computation
 * @param i_hn_density_parameterization to use
 * @param i_hn_density_fixedValue to use
 * @param Mdata Meteorological input
 * @param Xdata Snow cover data
 * @param tss Snow surface temperature (K)
 * @param variant which is currently used
 */
double SnLaws::compNewSnowDensity(const std::string& i_hn_density, const std::string& i_hn_density_parameterization, const double& i_hn_density_fixedValue,
                                  const CurrentMeteo& Mdata, const SnowStation& Xdata, const double& tss,
                                  const std::string& variant)
{
	double rho;

	const double z_ref_vw = 4.5;	//Assumed reference height for the wind speed used in the new snow density parameterizations
	const double vw_ref = Meteo::windspeedProfile(Mdata, z_ref_vw);
	if (i_hn_density == "PARAMETERIZED") {
		rho = newSnowDensityPara(i_hn_density_parameterization,
		                         Mdata.ta, tss, Mdata.rh, vw_ref,
		                         Xdata.meta.position.getAltitude());
	} else if (i_hn_density == "EVENT") {
		rho = newSnowDensityEvent(variant, event, Mdata);
	}/* else if (i_hn_density == "MEASURED") {
		if (Mdata.rho_hn != Constants::undefined) {
			rho = Mdata.rho_hn; // New snow density as read from input file
		} else if (Mdata.psum > 0. && (Mdata.psum_ph==IOUtils::nodata || Mdata.psum_ph<1.)) {
			if (i_hn_density_fixedValue > 0. && i_hn_density_fixedValue > min_hn_density) // use density of surface snowpack
				rho = Xdata.Edata[Xdata.getNumberOfElements()-1].Rho;
			else
				rho = newSnowDensityPara(i_hn_density_parameterization,
				                         Mdata.ta, tss, Mdata.rh, vw_ref,
				                         Xdata.meta.position.getAltitude());
		} else {
			rho = Constants::undefined;
		}
	}*/ else { // "FIXED"
		rho = (i_hn_density_fixedValue != Constants::undefined) ? i_hn_density_fixedValue : Xdata.Edata[Xdata.getNumberOfElements()-1].Rho;
		rho = std::max(min_hn_density, rho);
	}

	return rho;
}

/**
 * @brief NEW SNOW VISCOSITY (dendritic snow, i.e., dd > 0.) \n
 * Actual version : ml_lw_VS_Lehning from r7.7 \n
 * This is Michael's viscosity routine, which is not a function of micro-structure, but which
 * is nonetheless pretty important because it is numerically STABLE and does predict decent
 * settling rates, sometimes a bit too high. This routine was (is) used for NEW or WET snow.
 * @version 9Y.mm
 * @return Viscosity of new snow (Pa s)
 */
double SnLaws::NewSnowViscosityLehning(const ElementData& Edata)
{
	const double rho_hn = std::max(min_hn_density, Edata.Rho);

	if (rho_hn > 913.) //upper boundary
		return (1.e9 * smallest_viscosity);

	if (Edata.theta[WATER] > 0.001)
		//return (0.01*pow(rho_hn, 4.7));
		return(0.0001 * pow(rho_hn, 5.5));
	else
		return (0.007 * pow(rho_hn, (4.75 - IOUtils::K_TO_C(Edata.Te) / 40.)));
}

/**
 * @brief Computes the temperature term of viscosity
 * The modifications for POLAR variant are described in: Steger CR, Reijmer CH, van den Broeke MR, Wever N,
 * Forster RR, Koenig LS, Kuipers Munneke P, Lehning M, Lhermitte S, Ligtenberg SRM, Miège C and Noël BPY (2017)
 * Firn Meltwater Retention on the Greenland Ice Sheet: A Model Comparison. Front. Earth Sci. 5:3.
 * doi: 10.3389/feart.2017.00003: "To improve the agreement with observations, the tunable factors in the snow
 * viscosity scheme (Groot Zwaaftink et al., 2013) for the activation energy of snow Qs and the critical exponent
 * β are set to 16,080 J mol−1 and 0.3, respectively."
 * @version 11.06
 * @param Te Element temperature (K)
 * @return Temperature term of snow viscosity
 */
double SnLaws::snowViscosityTemperatureTerm(const double& Te)
{
	const double Q = (current_variant == "POLAR") ? (16080.) : (67000.); // Activation energy for defects in ice (J mol-1)

	switch (SnLaws::t_term) {
	case t_term_arrhenius_critical:
	{
		const double Q_fac = (current_variant == "POLAR") ? (0.24) : (0.39); // Adjust Q to snow; from Schweizer et al. (2004): 0.24
		const double criticalExp = (current_variant == "POLAR") ? (0.3) : (0.7); //0.5; //0.3; //
		const double T_r = 265.15; // Reference temperature (K), from Schweizer et al. (2004)
		return ((1. / SnLaws::ArrheniusLaw(Q_fac * Q, Te, T_r))
		             * (0.3 * pow((Constants::meltfreeze_tk - Te), criticalExp) + 0.4));
	}
	case t_term_arrhenius:
		return (1. / SnLaws::ArrheniusLaw(Q, Te, 263.));
	case t_term_stk: // Master thesis, September 2009
		return (0.35 * sqrt(274.15 - Te));
	case t_term_837: // as of revision 243, used up to revision 837 (deprecated)
		return (9. - 8.7 * exp(0.015 * (Te - Constants::meltfreeze_tk)));
	default:
		throw UnknownValueException("Unknown viscosity temperature dependency selected!", AT);
	}
}

/**
 * @brief Computes the additional stress due to loading rate
 * @version 11.06
 * @param i_viscosity_model see compSnowViscosity()
 * @param Edata
 * @param date current
 * @return Initial stress (Pa); note it is a negative value!
 */
double SnLaws::compLoadingRateStress(const std::string& i_viscosity_model, ElementData& Edata, const mio::Date& date)
{
	if (i_viscosity_model == "CALIBRATION")
		return loadingRateStressCALIBRATION(Edata, date);
	else
		return loadingRateStressDEFAULT(Edata, date);
}

double SnLaws::loadingRateStressDEFAULT(ElementData& Edata, const mio::Date& date)
{
	const double age = std::max(0., date.getJulian() - Edata.depositionDate.getJulian());

	double sigReac = 15.5 * Edata.CDot * exp(-age/101.);
	if (Edata.theta[WATER] > SnowStation::thresh_moist_snow)
		sigReac *= 0.37 * (1. + Edata.theta[WATER]);
	return sigReac;
}

double SnLaws::loadingRateStressCALIBRATION(ElementData& Edata, const mio::Date& date)
{
	static const double sigTension = 0.11;  // Ice surface tension (N m-2)

	Edata.Eps_Dot = 0.;
	switch (visc) {
		case visc_dflt: case visc_cal: case visc_ant:  { // new calibration
			const double age = std::max(0., date.getJulian() - Edata.depositionDate.getJulian());
			double sigReac = 15.5 * Edata.CDot * exp(-age/101.);
			if (Edata.theta[WATER] > SnowStation::thresh_moist_snow)
				sigReac *= 0.37 * (1. + Edata.theta[WATER]); // 0.2 ; 0.37
			Edata.Eps_Dot = sigReac;
			return sigReac;
		}
		case visc_897: { // r897
			double sigMetamo = 0.;
			const double age = std::max(0., date.getJulian() - Edata.depositionDate.getJulian());
			const double sigReac = 15.9 * Edata.CDot * exp(-age/101.); //tst2: 553. //tst1: 735. //
			Edata.Eps_Dot = sigReac;
			if (Edata.dd > Constants::eps /*((Edata->dd < 0.9) && (Edata->dd > 0.3))*/) {
				sigMetamo = 37.0e3 * Metamorphism::ddRate(Edata); // 2010-10-23
			}
			return (sigReac + sigMetamo);
		}
		case visc_837: case visc_stk: { // as of revision 837
			double sig0 = 0.;
			if ((Edata.dd < 0.9) && (Edata.dd > 0.3)) {
				double facIS = 3.; // default r712
				if (SnLaws::visc == SnLaws::visc_stk)
					facIS = -1.5; //-1.1; //-0.5; //
				sig0 = facIS * Metamorphism::ddRate(Edata) * sigTension / MM_TO_M(Edata.rg);
			}
			return sig0;
		}
		default:
			//this should not be reached...
			prn_msg(__FILE__, __LINE__, "err", Date(), "visc=%d not a valid choice for loadingRateStress!", visc);
			throw IOException("Choice not implemented yet!", AT);
	}


}

/**
 * @brief Determines the fudge factor for viscosity \n
 * This fudge factor takes into account bond-ice imperfections and the effect of liquid water
 * @version 11.06
 * @param Edata
 * @return Fudge factor for snow viscosity
 */
double SnLaws::snowViscosityFudgeDEFAULT(const ElementData& Edata)
{
	double ice_fudge = SnLaws::visc_ice_fudge / Edata.theta[ICE];
	ice_fudge *= (1. - logisticFunction(Edata.theta[ICE], 0.019, 0.15))
	                 * pow(Edata.theta[ICE], 0.77);

	double sp_fudge;
	if (Edata.mk%100 >= 20 && Edata.theta[WATER] < SnowStation::thresh_moist_snow)
		sp_fudge = 0.;
	else
		sp_fudge = SnLaws::visc_sp_fudge;
	sp_fudge *= (1. - logisticFunction(Edata.sp, 0.37, 0.21))
	                 * Edata.sp*Edata.sp*Edata.sp;

	double visc_fudge = ice_fudge * (1. + sp_fudge) + 0.1;
	visc_fudge *= (1. + SnLaws::visc_water_fudge * Edata.theta[WATER]
	                   * (1. - Edata.theta[ICE]));
	return visc_fudge;
}

/**
 * @brief To calibrate the fudge factor for viscosity
 * @note This is the fudge playground for calibrations used in SnowViscosityCALIBRATION()
 * @version 10.11
 * @param Edata
 * @param date current date
 * @return Fudge factor for snow viscosity
 */
double SnLaws::snowViscosityFudgeCALIBRATION(const ElementData& Edata, const mio::Date& date)
{
	double visc_fudge, sp_fudge;
	const double age = std::max(0., date.getJulian() - Edata.depositionDate.getJulian());
	double thresh_rho1 = 1., thresh_rho2 = 1.; // Thresholds for enhanced viscosity
	bool use_thresh = false;

	if (Edata.mk%100 >= 20 && Edata.theta[WATER] < SnowStation::thresh_moist_snow)
		sp_fudge = 0.;
	else
		sp_fudge = SnLaws::visc_sp_fudge;

	switch (visc) {
	case visc_cal: case visc_dflt: { // Calibration currently under test
		double ice_fudge = (0.67*14.1*SnLaws::visc_ice_fudge / Edata.theta[ICE]); // 0.67; 0.59; r897: 0.51
		ice_fudge *= (1. - logisticFunction(Edata.theta[ICE], 0.019, 0.15))
		                  * pow(Edata.theta[ICE], 0.77);

		sp_fudge *= 0.67*24.7;  // 0.67 ; 0.77 ; r897: 1.19 //
		sp_fudge *= (1. - logisticFunction(Edata.sp, 0.37, 0.21))
		                 * Edata.sp*Edata.sp*Edata.sp;

		visc_fudge = ice_fudge * (1. + sp_fudge) + 0.1;

		visc_fudge *= (1. + 33.*visc_water_fudge * Edata.theta[WATER]
		                   * (1. - Edata.theta[ICE])); // 31. ; 33. ; 35.
		break;
	}
	case visc_897: case visc_ant: { // Calibration fall 2010 & Antarctica
		double ice_fudge = 0.51*SnLaws::visc_ice_fudge / Edata.theta[ICE]; //
		if (SnLaws::visc != SnLaws::visc_ant) {
			ice_fudge *= 1. - logisticFunction(Edata.theta[ICE], 0.431, 0.067) * Edata.theta[ICE];
		} else {  // antarctic
			ice_fudge *= 1. - logisticFunction(Edata.theta[ICE], 0.351, 0.097) * Edata.theta[ICE]
			               * (1. - Edata.theta[ICE]);
		}

		sp_fudge *= 1.19; //
		sp_fudge *= (1. - logisticFunction(Edata.sp, 1.0, 0.077)) * sqrt(Edata.sp);

		visc_fudge = ice_fudge * (1. + sp_fudge) + 0.1;

		visc_fudge *= (1. + 31.*SnLaws::visc_water_fudge * Edata.theta[WATER]
		                   * (1. - Edata.theta[ICE]*Edata.theta[ICE]));
		break;
	}
	case visc_stk: { // Walter Steinkogler's playground; master thesis, September 2009
		static const double visc_time_fudge = 8.;
		visc_fudge = visc_time_fudge / exp(age / 35.);
		visc_fudge += (0.5*SnLaws::visc_ice_fudge / Edata.theta[ICE])
		                 + (0.3*sp_fudge * sqrt(Edata.sp))
		                   + (3. * Edata.theta[WATER] / Edata.theta[ICE] * 0.5
		                          * (1. - Edata.theta[ICE]));
		use_thresh=true;
		thresh_rho1 = 0.5; // rho_dry > 458.5 kg m-3
		thresh_rho2 = 0.7;
		break;
	}
	case visc_837: { // as of revision 712, used up to r837 (deprecated)
		static const double visc_time_fudge = 11.;
		visc_fudge = visc_time_fudge * (1. - sqrt(std::min(1., age / 77.)))
		                  * (1. + std::min(0.3, (263.15 - Edata.Te) / 17.));
		visc_fudge += (0.5*SnLaws::visc_ice_fudge / Edata.theta[ICE])
		                 + (0.3*sp_fudge * sqrt(Edata.sp))
		                   + (3. * Edata.theta[WATER] / Edata.theta[ICE]
		                          * 0.5 * (1. - Edata.theta[ICE]));
		use_thresh=true;
		thresh_rho1 = 0.5; // rho_dry > 458.5 kg m-3
		thresh_rho2 = 0.7;
		break;
	}
	default:
		prn_msg(__FILE__, __LINE__, "err", Date(),
		        "visc=%d not a valid choice for SnowViscosityFudge!", visc);
		throw IOException("Choice not implemented yet!", AT);
	}

	if (use_thresh) { // Default false
		if (Edata.theta[ICE] > thresh_rho1)
			visc_fudge *= (1. - Edata.theta[ICE]);
		if (Edata.theta[ICE] > thresh_rho2)
			visc_fudge *= (1. - Edata.theta[ICE]);
	}
	return visc_fudge;
}

/**
 * @name SNOW VISCOSITY
 * @brief Computes snow viscosity according to the following options:
 * - DEFAULT      : default model (corresponding to last calibration)
 * - KOJIMA       : according to parameterization by Kojima
 * - CALIBRATION  : used for calibration purposes
 * @param variant see VARIANT
 * @param i_viscosity_model to use
 * @param Edata
 * @param date current
 */
double SnLaws::compSnowViscosity(const std::string& variant, const std::string& i_viscosity_model, const std::string& i_watertransport_model,
                                 ElementData& Edata, const mio::Date& date)
{
	if (variant != SnLaws::current_variant)
		setStaticData(variant, i_watertransport_model);

	if (i_viscosity_model == "KOJIMA") {
		return snowViscosityKOJIMA(Edata);
	} else if (i_viscosity_model == "CALIBRATION") {
		return snowViscosityCALIBRATION(Edata, date);
	} else if (i_viscosity_model == "DEFAULT") {
		return snowViscosityDEFAULT(Edata);
	} else {
		throw InvalidArgumentException("Unknown viscosity model: "+i_viscosity_model, AT);
	}
}

/**
 * @brief SNOW VISCOSITY (all types of snow)
 * @todo revise description
 * @version 11.02
 * @param Edata
 * @return Snow viscosity (Pa s)
 */
double SnLaws::snowViscosityDEFAULT(ElementData& Edata)
{
	// Check needed while JAM set!
	if (Edata.theta[WATER] > 0.3)
		return (1.e9 * SnLaws::smallest_viscosity);
	// Check that you are not in a ice or/and water layer
	if (Edata.theta[ICE] + Edata.theta[WATER] > 0.99)
		return (1.e9 * SnLaws::smallest_viscosity);

	const double visc_fudge = SnLaws::snowViscosityFudgeDEFAULT(Edata); // Snow viscosity fudge factor

	static const double eps1Dot = 1.76e-7;    // Unit strain rate (at stress = 1 MPa) (s-1)
	static const double sig1 = 0.5e6;         // Unit stress from Sinha's formulation (Pa)
	const double visc_factor = 1./eps1Dot * Optim::pow3(sig1/visc_fudge);
	const double visc_macro = Edata.neck2VolumetricStrain(); // Macro-structure (layer) related factor
	const double Te = std::min(Edata.Te, Edata.meltfreeze_tk);
	double eta = (1. / visc_macro) * SnLaws::snowViscosityTemperatureTerm(Te) * visc_factor;

	static const double sigNeckYield = 0.4e6; // Yield stress for ice in neck (Pa)
	const double visc_micro = Edata.neckStressEnhancement(); // Micro-structure related factor
	const double sig = -Edata.C;       // Overburden stress, that is, absolute value of Cauchy stress (Pa)
	// HACK multiply sigNeckYield by 100. to avoid yielding on purpose
	if ((visc_micro * sig) <= 100. * sigNeckYield) // NOT YIELDING, LINEAR
		eta /= visc_micro * sigNeckYield*sigNeckYield;
	else // YIELDING, NON-LINEAR
		eta /= Optim::pow3(visc_micro) * Optim::pow2(sig);
	return eta; // Viscosity (Pa s)
}

/**
 * @brief SNOW VISCOSITY according to formulation by Kojima
 * @version 9.10
 * @param Edata
 * @return Snow viscosity (Pa s)
 */
double SnLaws::snowViscosityKOJIMA(const ElementData& Edata)
{
	return (8.64e6 * exp(0.021*Edata.Rho));
}

/**
 * @brief Calibrate snow viscosity
 * NOTE This is the test or playground version for calibrating settling
 * @version 10.11
 * @param Edata
 * @param date current
 * @return Snow viscosity (Pa s)
 */
double SnLaws::snowViscosityCALIBRATION(ElementData& Edata, const mio::Date& date)
{
	// TODO Check whether the two commented checks below are needed!
	// If the element length is SMALLER than the grain size then the thing aint settling ....
	//  if (Edata.L <= 2.*MM_TO_M(rg) )
	//    return(Constants::big);
	// Perry introduced this little check when the ice matrix is completely melted away -- in this case
	// set the viscosity to a high number to give the water in the element time to percolate away
	// NOTE MassTransport() is called before SnowCreep, thus this condition should never be met?
	//if (Edata.theta[ICE] < Constants::eps) {
	//	return(Constants::big);
	//}
	// Check needed while JAM set!
	if (true && (Edata.theta[WATER] > 0.3)) {
		return (1.e9 * SnLaws::smallest_viscosity);
	} else if (false && (Edata.theta[WATER] >= SnowStation::thresh_moist_snow)) {
		return SnLaws::snowViscosityKOJIMA(Edata);
	}
	// Check that you are not in a ice or/and water layer
	if (Edata.theta[ICE] + Edata.theta[WATER] > 0.99)
		return (1.e9 * SnLaws::smallest_viscosity);

	const double visc_fudge = SnLaws::snowViscosityFudgeCALIBRATION(Edata, date); // Snow viscosity fudge factor
	static const double eps1Dot = 1.76e-7;    // Unit strain rate (at stress = 1 MPa) (s-1)
	static const double sig1 = 0.5e6;         // Unit stress from Sinha's formulation (Pa)
	const double visc_factor = 1./eps1Dot * Optim::pow3(sig1/visc_fudge);
	const double visc_macro = Edata.neck2VolumetricStrain(); // Macro-structure (layer) related factor
	const double Te = std::min(Edata.Te, Edata.meltfreeze_tk);
	double eta = (1. / visc_macro) * SnLaws::snowViscosityTemperatureTerm(Te) * visc_factor;

	static const double sigNeckYield = 0.4e6; // Yield stress for ice in neck (Pa)
	const double visc_micro = Edata.neckStressEnhancement(); // Micro-structure related factor
	const double sig = -Edata.C;      // Overburden stress, that is, absolute value of Cauchy stress (Pa)
	// HACK multiply sigNeckYield by 100. to avoid yielding on purpose
	if ((visc_micro * sig) <= 100. * sigNeckYield) // NOT YIELDING, LINEAR
		eta /= visc_micro * sigNeckYield*sigNeckYield;
	else // YIELDING, NON-LINEAR
		eta /= Optim::pow3(visc_micro) * Optim::pow2(sig);
	//ANT Quickfix for Antarctica only
	if (SnLaws::setfix && ((date.getJulian() - Edata.depositionDate.getJulian()) > 60.))
		eta /= 0.06;
	return eta; // Viscosity (Pa s)
}

/**
 * @brief Computes an Arrhenius-type temperature dependency
 * @version 9.11
 * @param ActEnergy (J mol-1)
 * @param T snow temperature (K)
 * @param T_ref a reference temperature (K)
 */
double SnLaws::ArrheniusLaw(const double ActEnergy, const double T, const double T_ref)
{
	return exp((ActEnergy / Constants::gas_constant_mol) * (1. / T_ref - 1. / T));
}

/**
 * @brief Compute the air emissivity
 * It relies on mio::ILWR_parametrized to get a parametrized ILWR if no measured ILWR is available.
 * This in turn relies either on  Brutsaert (clear sky) or Omstedt (cloudiness) or Crawford(cloudiness from ISWR).
 * The air temperature and relative humidity should be available for meanigful results.
 * In any case, the returned air emissivity will be between min_air_emissivity and 1, min_air_emissivity depending
 * on the variant.
 * @param md meteorological conditions
 * @param variant variant to use for the minimum allowed air emissivity
 * @note observed minimum air emissivities:
 * 	- default: 0.55 (from 1993 data at Weissfluhjoch)
 * 	- Antarctica: 0.31 (from 2006/2007 data of Dome C)
 * @return air emissivity in range [MIN_AIR_EMISSIVITY,1.] (1)
 */
double SnLaws::AirEmissivity(mio::MeteoData& md, const std::string& variant)
{
	const double ILWR = (md(MeteoData::ILWR)>1.)? md(MeteoData::ILWR) : IOUtils::nodata;

	if (ILWR!=IOUtils::nodata){
//                std::cout << " I am here:\t" << ILWR << " , " << AirEmissivity(ILWR, md(MeteoData::TA), variant) << std::endl;                         
		return AirEmissivity(ILWR, md(MeteoData::TA), variant);
        }
	else {
		const double cloudiness = (md(MeteoData::ILWR)>0. && md(MeteoData::ILWR)<=1.)? md(MeteoData::ILWR) : IOUtils::nodata;
		const double ilwr_p = Atmosphere::ILWR_parametrized(md.meta.position.getLat(), md.meta.position.getLon(), md.meta.position.getAltitude(),
	                                        md.date.getJulian(), md.date.getTimeZone(),
	                                        md(MeteoData::RH), md(MeteoData::TA), md(MeteoData::ISWR), cloudiness);

		return AirEmissivity(ilwr_p, md(MeteoData::TA), variant);
	}
}

/**
 * @brief Compute the air emissivity
 * In any case, the returned air emissivity will be between min_air_emissivity and 1,
 * min_air_emissivity depending on the variant. It returns min_air_emissivity if ta==nodata
 * or ilwr==nodata
 * @param ilwr incoming long wave radiation (W/m^2)
 * @param ta air temperature (K)
 * @param variant variant to use for the minimum allowed air emissivity
 * @note observed minimum air emissivities:
 * 	- default: 0.55 (from 1993 data at Weissfluhjoch)
 * 	- Antarctica: 0.31 (from 2006/2007 data of Dome C)
 * @return air emissivity in range [MIN_AIR_EMISSIVITY,1.] (1)
 */
double SnLaws::AirEmissivity(const double& ilwr, const double& ta, const std::string& variant)
{
	const double min_emissivity = (variant != "ANTARCTICA" && variant != "POLAR") ? 0.55 : 0.31;

	if(ilwr==IOUtils::nodata || ta==IOUtils::nodata) return min_emissivity;

	const double ea = Atmosphere::blkBody_Emissivity(ilwr, ta); //return 1 if >1
	//if (ea<min_emissivity) return min_emissivity;
	return ea;
}

/**
 * @brief MONTANA snow viscosity (non-dendritic snow, i.e. if dd=0.); From bb_lw_VS_Montana() (Laws.c r7.7).
 * Bob Brown's MICRO-STRUCTURE law. Clearly the BEST law we have right now
 * but also the most UNSTABLE:  note that the viscosity is not only a function of the grain
 * dimensions, but also a function of the overburden stress.
 * A series of equations that collectively give the viscosity Vis = S/eDot.
 * The microstructure parameters rb and rg are obtained through Edata pointer and are in mm.
 * The secondary microstructure parameters  L and rc are also calculated in mm. This means that
 * the dimensions of rg, rb, L, &  rc are in mm since they show up in the following equations
 * as ratios to give dimensionless numbers.
 * NOTE: The theory is NOT valid for NEW SNOW or WET SNOW. However, we try it for wet snow, since it
 * makes sense for wet snow and is related to the wet snow bond growth (Pressure Sintering)
 * If the snow is new, however, then use LEHNING's law, or any other law ... No, do not use any other law, they don't work.
 * @author Guess!
 * @version Antediluvienne
 * @param *Edata
 * @return Snow viscosity Montana style
 */
double SnLaws::SnowViscosityMSU(const ElementData& Edata)
{
	const double theta_i = Edata.theta[ICE]; // Ice volume fraction
	const double theta_w = Edata.theta[WATER]; // Water volume fraction

	// Perry introduced this CUTOFF to avoid numerical instabilities with higher densities; might
	// not need it any more, since the problem turned out to be in the stress calculation.
	if ( theta_i + theta_w > 0.99 )
		return(Constants::big);
	if ( theta_w > 0.3 )
		return(1e9*SnLaws::smallest_viscosity);  // ONLY WITH JAM IN WaterTransport.c

	// Introduced this little check when the ice matrix is completely melted away -- in this case
	// set the viscosity to a high number to give the water in the element time to percolate away
	if ( theta_i <= 0.000001 )
		return(Constants::big);

	// If the element length is SMALLER than the grain size then things aint settling ....
	const double rg = Edata.rg; // grain radius (m)
	if ( Edata.L <= 2.*rg/1000. )
		return(Constants::big);

	double Vis;   // viscosity

	const double res_wat_cont = ElementData::snowResidualWaterContent(Edata.theta[ICE]);
	if ( Edata.dd <= 0. && Edata.theta[WATER] < res_wat_cont ) {
		//Dereference some parameters
		const double T = Edata.Te; // temperature (K)
		const double S   = Edata.C; // applied stress (Pa)
		const double N3 = Edata.N3; // 3-D coordination number (from Brown's density function)
		const double rb = Edata.rb; // bond radius (m)
		const double rc = Edata.concaveNeckRadius(); // concave radius of neck
		const double L = 2.*rg*rc/(rg + rc); // neck length

		//define some constants
		static const double epdot = 1.76e-7;   // unit strain rate (at stress = 1 MPa) (1/sec)
		static const double Q = 67000.;        // J/mol
		static const double R = 8.31;          // gas constant J/mol/K
		static const double Sig1 = 0.5e6;      // unit stress  Pa  from Sinha's formulation
		static const double Tref = 263.0;      // reference temperature in K
		static const double SneckYield = 0.4e6;// Yield stress for ice in neck (Pa)
		static const double th_i_f = 0.35, f_2 = 0.02; // Empirical constants to control dry snow viscosity fudge

		// First check to see if neck stress (Sneck) is >= SneckYield = 0.4 MPa.
		const double Sneck = (4.0/(N3*theta_i)) * Optim::pow2(rg/rb) * (-S);   // Work with absolute value of stress
		// Determine Bob's fudge factor that takes into account bond-ice imperfections and liquid water
		double fudge = 0.1/theta_i + 0.8*( exp((th_i_f - theta_i)/f_2) / (1. + exp((th_i_f - theta_i)/f_2)) );
		fudge = 4.0*fudge + SnLaws::montana_v_water_fudge*theta_w;
		// Determine the linear initial viscosity (Temperature dependent)
		const double Linvis0 = Optim::pow3(Sig1) / (Optim::pow2(SneckYield)*Optim::pow3(fudge)*epdot) * exp((-Q/R)*(1./Tref - 1./T));

		// Now branch to either non- linear or linear viscosity based on value of Sneck.
		if ( Sneck > SneckYield ) {  // YIELDING, non-linear
			// The units here are in m, s and Pa. Vis has dimensions of Pa s.
			// Note that the viscosity is a function of the square of the applied stress.
			// Density does not show up explicitly, but is buried implicitly in the equation
			// because N3, L, rg and rb all change with time as the material deforms.
			const double Vis1 =  ( (4.*fudge) / (N3*theta_i*Sig1) ) * Optim::pow2(rg/rb);
			Vis = (L/(2.*rg + L)) * epdot * exp( (Q/R)*(1./Tref - 1./T) );
			Vis = 1. / (Vis * Optim::pow2(S) * Optim::pow3(Vis1));
		} else { // NOT YIELDING, linear
			// This viscocity is not a function of stress and is therefore a linear viscosity.  Its value
			// depends on rb, rg, N3, theta_i and T. The expression  ((N3*theta_i)/(4.))*(rb/rg)^2
			// determines the neck stress relative to the snow stress. The expression   ((rg + L)/(3.*L))
			// relates the neck strains to the global volumetric strains. The term MONTANA_V_FUDGE is a
			// correction factor to account for stress concentrations in the neck.
			Vis = Linvis0 * ((2.*rg + L)/(L));
			Vis = Vis * ((N3*theta_i)/4.) * Optim::pow2(rb/rg);
		}
	} else { // NEW SNOW VICOSITY -- Use LEHNING rule for now.
		Vis = SnLaws::NewSnowViscosityLehning(Edata);
	}
	// Set lower limit in case S is very small or mass in layer approaches zero
	if (Vis <= SnLaws::smallest_viscosity)
		Vis = SnLaws::smallest_viscosity;

	return(Vis);
}
