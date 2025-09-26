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

#include <snowpack/SnowpackConfig.h>

using namespace mio;
using namespace std;

/************************************************************
 * static section                                           *
 ************************************************************/
map<string,string> SnowpackConfig::snowpackConfig;
map<string,string> SnowpackConfig::advancedConfig;
map<string,string> SnowpackConfig::inputConfig;
map<string,string> SnowpackConfig::outputConfig;

const bool SnowpackConfig::__init = SnowpackConfig::initStaticData();

bool SnowpackConfig::initStaticData()
{
	//[Snowpack] section
	advancedConfig["SOIL_FLUX"] = "false";

	//[SnowpackAdvanced] section
	advancedConfig["ADVECTIVE_HEAT"] = "false";
	advancedConfig["ALLOW_ADAPTIVE_TIMESTEPPING"] = "false";
	advancedConfig["ALPINE3D"] = "false";
	advancedConfig["ALPINE3D_PTS"] = "false";
	advancedConfig["DETECT_GRASS"] = "false";
	advancedConfig["ALBEDO_FIXEDVALUE"] = "-999.";
	advancedConfig["ALBEDO_PARAMETERIZATION"] = "LEHNING_2";
	advancedConfig["ALBEDO_AVERAGE_SCHMUCKI"] = "ALL_DATA";
	advancedConfig["ALBEDO_AGING"] = "true";
	advancedConfig["SOOT_PPMV"] = "0.0";
	advancedConfig["ENABLE_VAPOUR_TRANSPORT"] = "false";
	advancedConfig["FIXED_POSITIONS"] = "";
	advancedConfig["FORCE_RH_WATER"] = "true";
	advancedConfig["FORCE_ADD_SNOWFALL"] = "false";
	advancedConfig["HARDNESS_PARAMETERIZATION"] = "MONTI";
	advancedConfig["HEIGHT_NEW_ELEM"] = "0.02";
	advancedConfig["HN_DENSITY"] = "PARAMETERIZED";
	advancedConfig["HN_DENSITY_FIXEDVALUE"] = "100.";
	advancedConfig["HN_DENSITY_PARAMETERIZATION"] = "LEHNING_NEW";
	advancedConfig["HOAR_DENSITY_BURIED"] = "125.";
	advancedConfig["HOAR_DENSITY_SURF"] = "100.";
	advancedConfig["HOAR_MIN_SIZE_BURIED"] = "2.";
	advancedConfig["HOAR_MIN_SIZE_SURF"] = "0.5";
	advancedConfig["HOAR_THRESH_TA"] = "1.2";
	advancedConfig["HOAR_THRESH_RH"] = "0.97";
	advancedConfig["HOAR_THRESH_VW"] = "3.5";
	advancedConfig["JAM"] = "false";
	advancedConfig["COMBINE_ELEMENTS"] = "true";
	advancedConfig["REDUCE_N_ELEMENTS"] = "0";
	advancedConfig["MASS_BALANCE"] = "false";
	advancedConfig["MAX_NUMBER_MEAS_TEMPERATURES"] = "5";
	advancedConfig["MAX_SIMULATED_HS"] = "-1";
	advancedConfig["MEAS_INCOMING_LONGWAVE"] = "false";
	advancedConfig["METAMORPHISM_MODEL"] = "DEFAULT";
	advancedConfig["MIN_DEPTH_SUBSURF"] = "0.07";
	advancedConfig["MINIMUM_L_ELEMENT"] = "0.0025";
	advancedConfig["NEW_SNOW_GRAIN_SIZE"] = "0.3";
	advancedConfig["NUMBER_FIXED_RATES"] = "0";
	advancedConfig["NUMBER_SLOPES"] = "1";
	advancedConfig["PERP_TO_SLOPE"] = "false";
	advancedConfig["PLASTIC"] = "false";
	advancedConfig["PREVAILING_WIND_DIR"] = "0.";
	advancedConfig["RESEARCH"] = "true";
	advancedConfig["SNOW_ALBEDO"] = "PARAMETERIZED";
	advancedConfig["SNOW_EROSION"] = "false";
	advancedConfig["SNOW_REDISTRIBUTION"] = "false";
	advancedConfig["SALTATION_MODEL"] = "SORENSEN";
	advancedConfig["STRENGTH_MODEL"] = "DEFAULT";
	advancedConfig["SW_ABSORPTION_SCHEME"] = "MULTI_BAND";
	advancedConfig["FORCE_SW_MODE"] = "false";
	advancedConfig["TEMP_INDEX_DEGREE_DAY"] = "0."; //to replace the EB computation during melt phases by a dday model
	advancedConfig["TEMP_INDEX_SWR_FACTOR"] = "0.";; //to replace the EB computation during melt phases by a dday model
	advancedConfig["THRESH_RAIN"] = "1.2";
	advancedConfig["THRESH_RH"] = "0.5";
	advancedConfig["THRESH_DTEMP_AIR_SNOW"] = "3.0";
	advancedConfig["T_CRAZY_MAX"] = "320.";
	advancedConfig["T_CRAZY_MIN"] = "150.";
	advancedConfig["VARIANT"] = "DEFAULT";
	advancedConfig["VISCOSITY_MODEL"] = "DEFAULT";
	advancedConfig["WATER_LAYER"] = "false";
	advancedConfig["WATERTRANSPORTMODEL_SNOW"]="BUCKET";
	advancedConfig["WATERTRANSPORTMODEL_SOIL"]="BUCKET";
	advancedConfig["LB_COND_WATERFLUX"]="FREEDRAINAGE";				// Only for use with RE.
	advancedConfig["AVG_METHOD_HYDRAULIC_CONDUCTIVITY"]="ARITHMETICMEAN";		// Only for use with RE.
	advancedConfig["PREF_FLOW" ] = "false";						// Only for use with RE.
	advancedConfig["PREF_FLOW_PARAM_TH"] = "0.1";					// Only for use with RE and preferential flow.
	advancedConfig["PREF_FLOW_PARAM_N"] = "0.0";					// Only for use with RE and preferential flow.
	advancedConfig["PREF_FLOW_PARAM_HETEROGENEITY_FACTOR"] = "1.0";			// Only for use with RE and preferential flow.
	advancedConfig["PREF_FLOW_RAIN_INPUT_DOMAIN" ] = "MATRIX";			// Only for use with RE.
	advancedConfig["ADJUST_HEIGHT_OF_METEO_VALUES"] = "true";
	advancedConfig["ADJUST_HEIGHT_OF_WIND_VALUE"] = "true";
	advancedConfig["WIND_SCALING_FACTOR"] = "1.0";
	advancedConfig["ADVECTIVE_HEAT"] = "false";
	advancedConfig["HEAT_BEGIN"] = "0.0";
	advancedConfig["HEAT_END"] = "0.0";
	advancedConfig["TWO_LAYER_CANOPY"] = "true";
	advancedConfig["CANOPY_HEAT_MASS"] = "true";
	advancedConfig["CANOPY_TRANSMISSION"] = "true";
	advancedConfig["FORESTFLOOR_ALB"] = "true";
	advancedConfig["SOIL_EVAP_MODEL"] = "RESISTANCE";

	//temporary keys for Stability until we decide for a permanent solution
	advancedConfig["MULTI_LAYER_SK38"] = "false";
	advancedConfig["SSI_IS_RTA"] = "false";
	advancedConfig["SNOW_PREPARATION"] = "false";

	//[Input] section
	inputConfig["METEOPATH"] = "./input";
	inputConfig["NUMBER_OF_SOLUTES"] = "0";
	inputConfig["SNOW"] = "SMET";
	inputConfig["SOLUTE_NAMES"] = "NITRATE";
	inputConfig["ISWR_IS_NET"] = "false";

	//[Output] section
	outputConfig["AGGREGATE_PRO"] = "false";
	outputConfig["AGGREGATE_PRF"] = "false";
	outputConfig["AVGSUM_TIME_SERIES"] = "true";
	outputConfig["BACKUP_DAYS_BETWEEN"] = "365.";
	outputConfig["CLASSIFY_PROFILE"] = "false";
	outputConfig["CUMSUM_MASS"] = "false";
	outputConfig["EXPERIMENT"] = "NO_EXP";
	outputConfig["FIRST_BACKUP"] = "400.";
	outputConfig["HARDNESS_IN_NEWTON"] = "false";
	outputConfig["METEO"] = "SMET";
	outputConfig["METEOPATH"] = "./output";
	outputConfig["OUT_CANOPY"] = "false";
	outputConfig["OUT_HAZ"] = "false";
	outputConfig["OUT_HEAT"] = "true";
	outputConfig["OUT_LOAD"] = "false";
	outputConfig["OUT_LW"] = "true";
	outputConfig["OUT_MASS"] = "true";
	outputConfig["OUT_METEO"] = "true";
	outputConfig["OUT_SOILEB"] = "false";
	outputConfig["OUT_STAB"] = "true";
	outputConfig["OUT_SW"] = "true";
	outputConfig["OUT_T"] = "true";
	outputConfig["PRECIP_RATES"] = "true";
	outputConfig["PROF_FORMAT"] = "PRO";
	outputConfig["PROF_DAYS_BETWEEN"] = "1";
	outputConfig["PROF_START"] = "0";
	outputConfig["SNOW_WRITE"] = "true";
	outputConfig["SNOW"] = "SMET";
	outputConfig["TS_FORMAT"] = "MET";
	outputConfig["TS_DAYS_BETWEEN"] = "1";
	outputConfig["TS_START"] = "0";
	outputConfig["WRITE_PROCESSED_METEO"] = "false";

	return true;
}

/************************************************************
 * non-static section                                       *
 ************************************************************/

SnowpackConfig::SnowpackConfig(const mio::Config& i_cfg) : Config(i_cfg),
                               enforce_measured_snow_heights(false)
{
	setDefaults();
}

SnowpackConfig::SnowpackConfig(const std::string& i_filename) : Config(i_filename),
                               enforce_measured_snow_heights(false)
{
	setDefaults();
}

void SnowpackConfig::setDefaults()
{ //BUG we have a problem here: we try to keep the user settings if present. But we can not anymore make the difference between
// default values and user set values... The whole "if xxx.empty()" does not work anymore!
	string variant; getValue("VARIANT", "SnowpackAdvanced", variant, IOUtils::nothrow);

	getValue("ENFORCE_MEASURED_SNOW_HEIGHTS", "Snowpack", enforce_measured_snow_heights);

	string albedo_model; getValue("ALBEDO_MODEL", "SnowpackAdvanced", albedo_model, IOUtils::nothrow);
	string hn_density; getValue("HN_DENSITY", "SnowpackAdvanced", hn_density, IOUtils::nothrow);
	string hn_density_parameterization; getValue("HN_DENSITY_PARAMETERIZATION", "SnowpackAdvanced", hn_density_parameterization, IOUtils::nothrow);
	string metamorphism_model; getValue("METAMORPHISM_MODEL", "SnowpackAdvanced", metamorphism_model, IOUtils::nothrow);
	string strength_model; getValue("STRENGTH_MODEL", "SnowpackAdvanced", strength_model, IOUtils::nothrow);
	string viscosity_model; getValue("VISCOSITY_MODEL", "SnowpackAdvanced", viscosity_model, IOUtils::nothrow);
	string watertransportmodel_snow; getValue("WATERTRANSPORTMODEL_SNOW", "SnowpackAdvanced", watertransportmodel_snow, IOUtils::nothrow);
	string watertransportmodel_soil; getValue("WATERTRANSPORTMODEL_SOIL", "SnowpackAdvanced", watertransportmodel_soil, IOUtils::nothrow);
	string lb_cond_waterflux; getValue("LB_COND_WATERFLUX", "SnowpackAdvanced", lb_cond_waterflux, IOUtils::nothrow);
	string s_minimum_l_element; getValue("MINIMUM_L_ELEMENT", "SnowpackAdvanced", s_minimum_l_element, IOUtils::nothrow);
	string s_height_new_elem; getValue("HEIGHT_NEW_ELEM", "SnowpackAdvanced", s_height_new_elem, IOUtils::nothrow);

	if (s_minimum_l_element.empty()) addKey("MINIMUM_L_ELEMENT", "SnowpackAdvanced", advancedConfig["MINIMUM_L_ELEMENT"]);
	double minimum_l_element = get("MINIMUM_L_ELEMENT", "SnowpackAdvanced");

	if (variant == "ANTARCTICA") {	// Defaults for Antarctica variant
		if (hn_density.empty()) addKey("HN_DENSITY", "SnowpackAdvanced", "EVENT");

		if (s_minimum_l_element.empty()) addKey("MINIMUM_L_ELEMENT", "SnowpackAdvanced", "0.0001"); //Minimum element length (m)
		minimum_l_element = get("MINIMUM_L_ELEMENT", "SnowpackAdvanced");

		if ( !enforce_measured_snow_heights && s_height_new_elem.empty() ) {
			stringstream ss;
			const double tmp = 1.1 * minimum_l_element;
			ss << "" << tmp;
			addKey("HEIGHT_NEW_ELEM", "SnowpackAdvanced", ss.str());
		}
	} else {
		if (enforce_measured_snow_heights) {
			if(s_height_new_elem.empty()) addKey("HEIGHT_NEW_ELEM", "SnowpackAdvanced", advancedConfig["HEIGHT_NEW_ELEM"]);
		} else {
			if(s_height_new_elem.empty()) {
				stringstream ss;
				const double tmp = 2. * minimum_l_element;
				ss << "" << tmp;
				addKey("HEIGHT_NEW_ELEM", "SnowpackAdvanced", ss.str());
			}
		}
	}

	if ((variant.empty()) || (variant == "DEFAULT")) {
		// Use default settings
	} else if (variant == "JAPAN") {
		if (albedo_model.empty()) addKey("ALBEDO_MODEL", "SnowpackAdvanced", "NIED");
		if (hn_density_parameterization.empty()) addKey("HN_DENSITY_PARAMETERIZATION", "SnowpackAdvanced", "NIED");
		if (metamorphism_model.empty()) addKey("METAMORPHISM_MODEL", "SnowpackAdvanced", "NIED");
		if (strength_model.empty()) addKey("STRENGTH_MODEL", "SnowpackAdvanced", "NIED");
		if (viscosity_model.empty()) addKey("VISCOSITY_MODEL", "SnowpackAdvanced", "KOJIMA");
		if (watertransportmodel_snow.empty()) addKey("WATERTRANSPORTMODEL_SNOW", "SnowpackAdvanced", "NIED");
		if (watertransportmodel_soil.empty()) addKey("WATERTRANSPORTMODEL_SOIL", "SnowpackAdvanced", "NIED");

	} else if (variant == "ANTARCTICA" || variant == "POLAR") {
		string hoar_density_buried; getValue("HOAR_DENSITY_BURIED", "SnowpackAdvanced", hoar_density_buried, IOUtils::nothrow);
		if (hoar_density_buried.empty()) addKey("HOAR_DENSITY_BURIED", "SnowpackAdvanced", "200.0");

		string force_rh_water; getValue("FORCE_RH_WATER", "SnowpackAdvanced", force_rh_water, IOUtils::nothrow);
		if (force_rh_water.empty()) addKey("FORCE_RH_WATER", "SnowpackAdvanced", "false");

		string thresh_rh; getValue("THRESH_RH", "SnowpackAdvanced", thresh_rh, IOUtils::nothrow);
		if (thresh_rh.empty()) addKey("THRESH_RH", "SnowpackAdvanced", "0.1");

		addKey("MIN_DEPTH_SUBSURF", "SnowpackAdvanced", "0.");
		string t_crazy_min; getValue("T_CRAZY_MIN", "SnowpackAdvanced", t_crazy_min, IOUtils::nothrow);
		string t_crazy_max; getValue("T_CRAZY_MAX", "SnowpackAdvanced", t_crazy_max, IOUtils::nothrow);
		// If not specified in the ini file, set "polar" limits on the crazy temperatures
		if (t_crazy_min.empty()) addKey("T_CRAZY_MIN", "SnowpackAdvanced", "165.");
		if (t_crazy_max.empty()) addKey("T_CRAZY_MAX", "SnowpackAdvanced", "300.");
		addKey("NEW_SNOW_GRAIN_SIZE", "SnowpackAdvanced", "0.2");

	} else if (variant == "CALIBRATION") {
		if (hn_density_parameterization.empty()) addKey("HN_DENSITY_PARAMETERIZATION", "SnowpackAdvanced", "ZWART");
		if (viscosity_model.empty()) addKey("VISCOSITY_MODEL", "SnowpackAdvanced", "CALIBRATION");

		string fixed_positions; getValue("FIXED_POSITIONS", "SnowpackAdvanced", fixed_positions, IOUtils::nothrow);
		if (fixed_positions.empty()) addKey("FIXED_POSITIONS", "SnowpackAdvanced", "5");
		string number_fixed_rates; getValue("NUMBER_FIXED_RATES", "SnowpackAdvanced", number_fixed_rates, IOUtils::nothrow);
		if (number_fixed_rates.empty()) addKey("NUMBER_FIXED_RATES", "SnowpackAdvanced", "0");
		string max_number_meas_temperatures;
		getValue("MAX_NUMBER_MEAS_TEMPERATURES", "SnowpackAdvanced", max_number_meas_temperatures, IOUtils::nothrow);
		if (max_number_meas_temperatures.empty()) addKey("MAX_NUMBER_MEAS_TEMPERATURES", "SnowpackAdvanced", "5");
		string min_depth_subsurf; getValue("MIN_DEPTH_SUBSURF", "SnowpackAdvanced", min_depth_subsurf, IOUtils::nothrow);
		if (min_depth_subsurf.empty()) addKey("MIN_DEPTH_SUBSURF", "SnowpackAdvanced", "0.0");
	} else if (variant == "SEAICE") {
		// Initializations for sea ice
		if (lb_cond_waterflux.empty()) addKey("LB_COND_WATERFLUX", "SnowpackAdvanced", "SEAICEFLOODING");
	} else {
		throw UnknownValueException("Unknown variant " + variant, AT);
	}

	/* For all parameters not set by the user or by the initialization above, the default values apply
	 * That is, loop through advancedConfig (then inputConfig & outputConfig) and check whether user has set
	 * the parameter in the corresponding section, if not add default value
	 */
	for(map<string,string>::const_iterator it = snowpackConfig.begin(); it != snowpackConfig.end(); ++it) {
		//[Snowpack] section
		string value; getValue(it->first, "Snowpack", value, IOUtils::nothrow);
		if (value.empty()) addKey(it->first, "Snowpack", it->second);
	}

	for(map<string,string>::const_iterator it = advancedConfig.begin(); it != advancedConfig.end(); ++it) {
		//[SnowpackAdvanced] section
		string value; getValue(it->first, "SnowpackAdvanced", value, IOUtils::nothrow);
		if (value.empty()) addKey(it->first, "SnowpackAdvanced", it->second);
	}

	for(map<string,string>::const_iterator it = inputConfig.begin(); it != inputConfig.end(); ++it) {
		//[Input] section
		string value; getValue(it->first, "Input", value, IOUtils::nothrow);
		if (value.empty()) addKey(it->first, "Input", it->second);
	}

	for(map<string,string>::const_iterator it = outputConfig.begin(); it != outputConfig.end(); ++it) {
		//[Output] section
		string value; getValue(it->first, "Output", value, IOUtils::nothrow);
		if (value.empty()) addKey(it->first, "Output", it->second);
	}

	/**
	 * @brief Defines how energy and mass balance are output \n
	 * - AVGSUM_TIME_SERIES == 1 \n
	 *   Energy and mass fluxes are averaged and cumulated over TS_DAYS_BETWEEN, respectively. \n
	 *   Otherwise, instantaneous energy fluxes and mass fluxes cumulated over the last computation
	 *   time step (CALCULATION_STEP_LENGTH) are dumped. \n
	 *   @note Precipitations and Erosion are always given in rates per TS_DAYS_BETWEEN interval (h-1)
	 * - CUMSUM_MASS == 1 \n
	 *   Mass fluxes are cumulated over whole run period.
	 * - WARNING: In operational mode and if NUMBER_SLOPES > 1, the above two values are always unset!
	 */
	const unsigned int nSlopes = get("NUMBER_SLOPES", "SnowpackAdvanced");
	if (nSlopes > 1) {
		addKey("AVGSUM_TIME_SERIES", "Output", "false");
		addKey("CUMSUM_MASS", "Output", "false");
	}

	/**
	 * @brief Hazard data interval in units of CALCULATION_STEP_LENGTH \n
	 * WARNING: In operational mode, this has to result in a 30 min interval!
	 * It is a matter of consitency. If you change this, a big mess will result!!!
	 */
	const double calculation_step_length = get("CALCULATION_STEP_LENGTH", "Snowpack");

	string hazard_steps_between;
	getValue("HAZARD_STEPS_BETWEEN", "Output", hazard_steps_between, IOUtils::nothrow);
	if (hazard_steps_between.empty()) {
		stringstream ss;
		const int tmp = (int)(30./calculation_step_length + 0.5);
		ss << "" << tmp;
		addKey("HAZARD_STEPS_BETWEEN", "Output", ss.str());
	}

	/**
	 * @brief Default lower boundary condition for Richards equation solver \n
	 */
	if (watertransportmodel_soil == "RICHARDSEQUATION" && lb_cond_waterflux.empty()) addKey("LB_COND_WATERFLUX", "SnowpackAdvanced", "FREEDRAINAGE");

	/**
	 * @brief Checking the settings for hydraulic conductivity \n
	 */
	string tmp_avg_method_K; getValue("AVG_METHOD_HYDRAULIC_CONDUCTIVITY_PREF_FLOW", "SnowpackAdvanced", tmp_avg_method_K, IOUtils::nothrow);
	if (tmp_avg_method_K.empty()) {
		// If not explicitly specified, take the default one (i.e., the one for matrix flow)
		getValue("AVG_METHOD_HYDRAULIC_CONDUCTIVITY", "SnowpackAdvanced", tmp_avg_method_K);
		addKey("AVG_METHOD_HYDRAULIC_CONDUCTIVITY_PREF_FLOW", "SnowpackAdvanced", tmp_avg_method_K);
	}
}
