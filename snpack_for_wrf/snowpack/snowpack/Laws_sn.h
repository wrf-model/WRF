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
 * @file Laws_sn.h
 * @version 10.02
 */

#ifndef LAWS_SN_H
#define LAWS_SN_H

#include <string>
#include <meteoio/MeteoIO.h>

#include <snowpack/DataClasses.h>
#include <snowpack/snowpackCore/Snowpack.h> //some constants are necessary


class SnLaws {

	public:
		//@{
		/// Types of events for computing new snow density
		enum EventType {
			event_none,
			event_wind   ///< Wind driven deposition of snow (Antarctica)
		};
		/// Defines temperature dependence of snow viscosity
		enum TempDependence {
			t_term_arrhenius_critical=11, ///< Arrhenius type multiplied by critical function near melting point
			t_term_arrhenius=911,         ///< pure Arrhenius type w/ excitation energy of ice
			t_term_837=977,               ///< as of revision 837 (from r243)
			t_term_stk=999                ///< calibration 2009 by Walter Steinkogler (MSc thesis)
		};
		/// Defines which snow viscosity version to use
		enum ViscosityVersion {
			visc_dflt=111,  ///< actual version: revision 961, June 2011 by Fierz
			visc_cal=333,   ///< version under test
			visc_ant=335,   ///< if Antarctica needs adaptation of _new
			visc_897=555,   ///< calibration fall 2010 by Fierz
			visc_837=977,   ///< as of revision 837 (deprecated)
			visc_stk=999    ///< calibration 2009 by Walter Steinkogler (MSc thesis)
		};
		//@}

		static double conductivity_ice(const double& Temperature);
		static double conductivity_water(const double& Temperature);
		static double conductivity_air(void);


		static double compWindPumpingDisplacement(const SnowStation& Xdata);
		static double compWindPumpingVelocity(const CurrentMeteo& Mdata, const double& d_pump);
		static double compWindGradientSnow(const ElementData& Edata, double& v_pump);

		static double compSensibleHeatCoefficient(const CurrentMeteo& Mdata, const SnowStation& Xdata,
		                                          const double& height_of_meteo_values);
		static double compLatentHeat_Rh(const Snowpack::soil_evap_model soil_evaporation, const CurrentMeteo& Mdata, SnowStation& Xdata,
		                                const double& height_of_meteo_values);
		static double compLatentHeat(const Snowpack::soil_evap_model soil_evaporation, const CurrentMeteo& Mdata, SnowStation& Xdata,
		                             const double& height_of_meteo_values);

		static double compSoilThermalConductivity(const ElementData& Edata, const double& dvdz);

		static double soilVaporDiffusivity(const ElementData& Edata);
		static double compEnhanceWaterVaporTransportSoil(const ElementData& Edata,const double& clay_fraction);
		static double compSoilThermalVaporConductivity(const ElementData& Edata_bot, const ElementData& Edata_top, const double& Te_bot, const double& Te_top,const double& clay_fraction);
		static double compSoilIsothermalVaporConductivity(const ElementData& Edata_bot, const ElementData& Edata_top, const double& Te_bot, const double& Te_top, const double& T_node);

		static double compSnowThermalConductivity(const ElementData& Edata, const double& dvdz, const bool& show_warnings=true);

		static double compEnhanceWaterVaporTransportSnow(const SnowStation& Xdata, const size_t& i_e);

		static double compLWRadCoefficient(const double& t_snow, const double& t_atm, const double& e_atm);

		static double compLWRadCoefficient_special(const double& t_snow, const double& e_atm);

		static double parameterizedSnowAlbedo(const std::string& i_albedo, const std::string& i_albedo_parameterization, const std::string& i_albAverageSchmucki, const double& i_albNIED_av,
		                                      const double& i_hn_albedo_fixedValue, const ElementData& Edata, const double& Tss, const CurrentMeteo& Mdata, const SnowStation& Xdata, const bool& ageAlbedo=true);
		static void compShortWaveAbsorption(const std::string& i_sw_absorption_scheme, SnowStation& Xdata, const double& I0);
		static void compAdvectiveHeat(SnowStation& Xdata, const double& advective_heat,
		                                                  const double& depth_begin, const double& depth_end);

		static double compNewSnowDensity(const std::string& i_hn_density, const std::string& i_hn_density_parameterization, const double& i_hn_density_fixedValue,
		                                 const CurrentMeteo& Mdata, const SnowStation& Xdata, const double& tss,
		                                 const std::string& variant);

		static double NewSnowViscosityLehning(const ElementData& Edata);
		static double SnowViscosityMSU(const ElementData& Edata);

		static double snowViscosityTemperatureTerm(const double& Te);
		static double compLoadingRateStress(const std::string& variant, ElementData& Edata,
		                                    const mio::Date& date);
		static double loadingRateStressDEFAULT(ElementData& Edata, const mio::Date& date);
		static double loadingRateStressCALIBRATION(ElementData& Edata, const mio::Date& date);
		static double snowViscosityFudgeDEFAULT(const ElementData& Edata);
		static double snowViscosityFudgeCALIBRATION(const ElementData& Edata, const mio::Date& date);
		static double compSnowViscosity(const std::string& variant, const std::string& i_viscosity_model, const std::string& i_watertransport_model,
		                                ElementData& Edata, const mio::Date& date);
		static double snowViscosityDEFAULT(ElementData& Edata);
		static double snowViscosityKOJIMA(const ElementData& Edata);
		static double snowViscosityCALIBRATION(ElementData& Edata, const mio::Date& date);
		// minimum observed air emissivity: default=0.55 (from 1993 data at Weissfluhjoch) - Antarctica=0.31 (from 2006/2007 data of Dome C)
		static double AirEmissivity(mio::MeteoData& md, const std::string& variant);
		static double AirEmissivity(const double& ilwr, const double& ta, const std::string& variant);
		static double ArrheniusLaw(const double ActEnergy, const double T, const double T_ref);

		static double min_hn_density, max_hn_density, event_wind_lowlim;
		static const double smallest_viscosity, field_capacity_soil;
		static const bool jordy_new_snow, wind_pump, wind_pump_soil;

		static bool setStaticData(const std::string& variant, const std::string& watertransportmodel);

		static double newSnowDensityPara(const std::string& i_hn_model,
		                                 double TA, double TSS, double RH, double VW, double HH);
		static double newSnowDensityEvent(const std::string& variant, const SnLaws::EventType& i_event,
                                          const CurrentMeteo& Mdata);
		static double newSnowDensityHendrikx(const double ta, const double tss, const double rh, const double vw);
		static double event_wind_highlim;
		static EventType event;

		static double sn_dt; //Calculation time step in seconds as derived from CALCULATION_STEP_LENGTH
		static const bool __init;
		static std::string current_variant;
		static TempDependence t_term;
		static ViscosityVersion visc;
		static double visc_ice_fudge, visc_sp_fudge, visc_water_fudge;
		static bool setfix;
		static size_t swa_nBands;
		static std::vector<double> swa_k, swa_pc, swa_fb;
		//static const soil_evap_model soil_evaporation;
		static const double rsoilmin, relsatmin, alpha_por_tor_soil, pore_length_soil;
		static const double montana_c_fudge, montana_vapor_fudge, montana_v_water_fudge;
		static const double wind_ext_coef, displacement_coef, alpha_por_tor;
};

#endif
