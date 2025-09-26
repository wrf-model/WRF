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
#include <snowpack/libsnowpack.h>
#include <meteoio/MeteoIO.h>

#include <iostream>
#include <string>
#include <sstream>
#include <ctime>

#ifdef _MSC_VER
	/*
	This software contains code under BSD license (namely, getopt for Visual C++).
	Therefore, this product includes software developed by the University of
	California, Berkeley and its contributors when compiling with Visual C++.
	*/
	#include "getopt.h"
#else
	//#include <unistd.h> //for getopt
	#include <getopt.h> //for getopt_long
#endif

using namespace std;
using namespace mio;

#ifdef DEBUG_ARITHM
	#ifndef _GNU_SOURCE
		#define _GNU_SOURCE
	#endif
	#ifndef __USE_GNU
		#define __USE_GNU
	#endif
	#include <fenv.h>
#endif


/**
 * @class Slope a C. Fierz class ;-)
 */
class Slope {

	public:
		Slope(const mio::Config& cfg);

		double prevailing_wind_dir;
		unsigned int nSlopes;
		unsigned int mainStation;  ///< main station, flat field or slope
		unsigned int sector;       ///< main station (0) or current slope sector (1:nSlopes)
		unsigned int first;        ///< first virtual slope station in computing sequence
		unsigned int luv;
		unsigned int lee;
		bool north, south;
		std::string snow_erosion;
		bool mainStationDriftIndex;
		bool snow_redistribution, luvDriftIndex;

		unsigned int getSectorDir(const double& dir_or_expo) const;
		void setSlope(const unsigned int slope_sequence, vector<SnowStation>& vecXdata, double& wind_dir);

	private:
		double sector_width;       ///< width of slope sector: 360./std::max((unsigned)1, nSlopes-1) deg
};

/**
 * @class Cumsum a C. Fierz class ;-)
 * To cumulate various mass fluxes over either hazard or time series time steps
 */
class Cumsum {

	public:
		Cumsum(const unsigned int nSlopes);

		double precip;
		double drift, snow, runoff, rain;
		vector<double> erosion; // Cumulated eroded mass; dumped to file as rate
};

/************************************************************
 * static section                                           *
 ************************************************************/

//Global variables in this file:
static string cfgfile = "io.ini";
static string mode = "RESEARCH";
bool restart = false;
static mio::Date dateBegin, dateEnd;
static vector<string> vecStationIDs;

/// @brief Main control parameters
struct MainControl
{
	size_t nStep;        ///< Time step number
	size_t nAvg;         ///< Number of calculation time steps to average fluxes etc.
	size_t HzStep;       ///< Hazard step number (should be half of nStep in operational mode)
	bool   TsDump;       ///< Flag for time series dump
	bool   HzDump;       ///< Calculation of hazard information will be performed
	bool   PrDump;       ///< Flag for profile dump
	bool   XdataDump;    ///< Backup of Xdata will be performed
	bool   sdbDump;      ///< Dump to data base if required in operational mode
	bool   resFirstDump; ///< Flag to dump initial state of snowpack
};

/************************************************************
 * non-static section                                       *
 ************************************************************/

Slope::Slope(const mio::Config& cfg)
       : prevailing_wind_dir(0.), nSlopes(0), mainStation(0), sector(0),
         first(1), luv(0), lee(0),
         north(false), south(false),
         snow_erosion("NONE"), mainStationDriftIndex(false),
         snow_redistribution(false), luvDriftIndex(false),
         sector_width(0)
{
	cfg.getValue("NUMBER_SLOPES", "SnowpackAdvanced", nSlopes);
	cfg.getValue("SNOW_EROSION", "SnowpackAdvanced", snow_erosion);
	std::transform(snow_erosion.begin(), snow_erosion.end(), snow_erosion.begin(), ::toupper);	// Force upper case
	stringstream ss;
	ss << "" << nSlopes;
	cfg.getValue("SNOW_REDISTRIBUTION", "SnowpackAdvanced", snow_redistribution);
	if (snow_redistribution && !(nSlopes > 1 && nSlopes % 2 == 1))
		throw mio::IOException("Please set NUMBER_SLOPES to 3, 5, 7, or 9 with SNOW_REDISTRIBUTION set! (nSlopes="+ss.str()+")", AT);
	cfg.getValue("PREVAILING_WIND_DIR", "SnowpackAdvanced", prevailing_wind_dir, mio::IOUtils::nothrow);
	sector_width = 360. / static_cast<double>(std::max((unsigned)1, nSlopes-1));
}

/**
 * @brief Determine either direction of blowing wind or slope exposition.
 * NOTE that station slope.first always corresponds to the prevailing wind direction
 * @param dir_or_expo direction of wind or exposition
 **/
unsigned int Slope::getSectorDir(const double& dir_or_expo) const
{
	double dir = dir_or_expo;
	if (dir > 360.) dir -= 360.;
	else if (dir < 0.) dir += 360.;
	const unsigned int sectorDir = (unsigned int)((floor((dir + 0.5*sector_width)/sector_width)) + 1);
	if (sectorDir >= nSlopes) return 1;
	else return sectorDir;
}

/**
 * @brief Set slope variables
 * @param slope_sequence computation sequence for slopes
 * @param vecXdata
 * @param wind_dir direction of wind
 **/
void Slope::setSlope(const unsigned int slope_sequence, vector<SnowStation>& vecXdata, double& wind_dir)
{
	mainStationDriftIndex = false;
	luvDriftIndex = false;
	switch (slope_sequence) {
	case 0:
		for (size_t kk=0; kk<nSlopes; kk++) {
			vecXdata[kk].windward = false;
			vecXdata[kk].rho_hn   = 0.;
			vecXdata[kk].hn       = 0.;
		}
		if (nSlopes > 1) {
			luv = getSectorDir(wind_dir - prevailing_wind_dir);
			vecXdata[luv].windward = true;
			lee = (luv + nSlopes/2) % (nSlopes-1);
			if (lee == 0) lee = nSlopes - 1;
		} else {
			//requesting slope 0 of 0 expositions
			luv = lee = 0;
		}
		sector = mainStation;
		mainStationDriftIndex = ((nSlopes == 1) && (snow_erosion != "NONE"));
		break;
	case 1:
		sector = luv;
		luvDriftIndex = snow_redistribution;
		break;
	default:
		sector++;
		if (sector == nSlopes) sector = 1;
	}
	north = (vecXdata[sector].meta.getSlopeAngle() > 0. && vecXdata[sector].meta.getAzimuth() == 0.);
	south = (vecXdata[sector].meta.getSlopeAngle() > 0. && vecXdata[sector].meta.getAzimuth() == 180.);
}

Cumsum::Cumsum(const unsigned int nSlopes)
        : precip(0.),
          drift(0.), snow(0.), runoff(0.), rain(0.),
          erosion(nSlopes, 0.)
{}

inline void Version()
{
#ifdef _MSC_VER
	cout << "This version of Snowpack uses a BSD-licensed port of getopt for Visual C++. \n"
		<< "It therefore includes software developed by the University of "
		<< "California, Berkeley and its contributors." << endl;
#endif
	cout << "Snowpack version " << SN_VERSION << " compiled on " << __DATE__ << " " << __TIME__ << "\n"
		<< "\tLibsnowpack " << snowpack::getLibVersion() << "\n"
		<< "\tMeteoIO " << mio::getLibVersion() << endl;
}

inline void Usage(const string& programname)
{
	Version();

	cout << "Usage: " << programname << endl
		<< "\t[-b, --begindate=YYYY-MM-DDTHH:MM] (e.g.:2007-08-11T09:00)\n"
		<< "\t[-e, --enddate=YYYY-MM-DDTHH:MM] (e.g.:2008-08-11T09:00 or NOW)\n"
		<< "\t[-c, --config=<ini file>] (e.g. io.ini)\n"
		<< "\t[-m, --mode=<operational or research>] (default: research)\n"
		<< "\t[-r, --restart (skip first time step, only in research mode)\n"
		<< "\t[-s, --stations=<comma delimited stationnames>] (e.g. DAV2,WFJ2)\n"
		<< "\t[-v, --version] Print the version number\n"
		<< "\t[-h, --help] Print help message and version information\n\n";
	cout << "\tPlease note that the operational mode should only be used within SLF\n";

	cout << "Example: " << programname << " -c io.ini -e 1996-06-17T00:00\n\n";
}

inline void parseCmdLine(int argc, char **argv, string& begin_date_str, string& end_date_str)
{
	int longindex=0, opt=-1;
	bool setEnd = false;

	struct option long_options[] =
	{
		{"begindate", required_argument, 0, 'b'},
		{"enddate", required_argument, 0, 'e'},
		{"mode", required_argument, 0, 'm'},
		{"restart", no_argument, 0, 'r'},
		{"config", required_argument, 0, 'c'},
		{"stations", required_argument, 0, 's'},
		{"version", no_argument, 0, 'v'},
		{"help", no_argument, 0, 'h'},
		{0, 0, 0, 0}
	};

	if (argc==1) { //no arguments provided
		Usage(string(argv[0]));
		exit(1);
	}

	while ((opt=getopt_long( argc, argv, ":b:e:m:rc:s:v:h", long_options, &longindex)) != -1) {
		switch (opt) {
		case 0:
			break;
		case 'b': {
			begin_date_str=string(optarg); //we don't know yet the time zone, conversion will be done later
			break;
		}
		case 'e': {
			end_date_str=string(optarg); //we don't know yet the time zone, conversion will be done later
			setEnd = true;
			break;
		}
		case 'm':
			mode = string(optarg);
			mio::IOUtils::toUpper(mode);
			if (!(mode == "RESEARCH" || mode == "OPERATIONAL")) {
				cerr << endl << "[E] Command line option '-" << char(opt) << "' requires 'research' or 'operational' as operand\n";
				Usage(string(argv[0]));
				exit(1);
			}
			break;
		case 'r':
			restart = true;
			break;
		case 'c':
			cfgfile = string(optarg);
			break;
		case 's':
			mio::IOUtils::readLineToVec(string(optarg), vecStationIDs, ',');
			break;
		case ':': //operand missing
			cerr << endl << "[E] Command line option '-" << char(opt) << "' requires an operand\n";
			Usage(string(argv[0]));
			exit(1);
		case 'v':
			Version();
			exit(0);
		case 'h':
			Usage(string(argv[0]));
			exit(0);
		case '?':
			cerr << endl << "[E] Unknown argument detected\n";
			Usage(string(argv[0]));
			exit(1);
		default:
			cerr << endl << "[E] getopt returned character code " <<  opt << "\n";
			Usage(string(argv[0]));
			exit(1);
		}
	}

	if (!setEnd) {
		cerr << endl << "[E] You must specify an enddate for the simulation!\n";
		Usage(string(argv[0]));
		exit(1);
	}
}

inline void editMeteoData(mio::MeteoData& md, const string& variant, const double& thresh_rain)
{ //HACK: these should be handled by DataGenerators
	if (md(MeteoData::PSUM_PH)==IOUtils::nodata) {
		const double ta = md(MeteoData::TA);
		if (ta!=IOUtils::nodata)
			md(MeteoData::PSUM_PH) = (ta>=IOUtils::C_TO_K(thresh_rain))? 1. : 0.; //fallback: simple temp threshold
	}

	//Add the atmospheric emissivity as a parameter
	if (!md.param_exists("EA")) {
		md.addParameter("EA");
		md("EA") = SnLaws::AirEmissivity(md, variant);
	}
	// Snow stations without separate wind station use their own wind for local drifting and blowing snow
	if (!md.param_exists("VW_DRIFT")) md.addParameter("VW_DRIFT");
	if (md("VW_DRIFT") == mio::IOUtils::nodata)
		md("VW_DRIFT") = md(MeteoData::VW);
	if (!md.param_exists("DW_DRIFT")) md.addParameter("DW_DRIFT");
	if (md("DW_DRIFT") == mio::IOUtils::nodata)
		md("DW_DRIFT") = md(MeteoData::DW);
}

// Return true if snowpack can compute the next timestep, else false
inline bool validMeteoData(const mio::MeteoData& md, const string& StationName, const string& variant, const bool& enforce_snow_height, const bool& advective_heat, const bool& soil_flux, const unsigned int& nslopes)
{
	bool miss_ta=false, miss_tsg=false, miss_rh=false, miss_precip=false, miss_splitting=false, miss_hs=false;
	bool miss_rad=false, miss_ea=false, miss_wind=false, miss_drift=false, miss_adv=false;

	if (md(MeteoData::TA) == mio::IOUtils::nodata)
		miss_ta=true;
	if (soil_flux==false && md(MeteoData::TSG) == mio::IOUtils::nodata)
		miss_tsg=true;
	if (md(MeteoData::RH) == mio::IOUtils::nodata)
		miss_rh=true;
	if ((variant != "ANTARCTICA")
	        && ((md(MeteoData::ISWR) == mio::IOUtils::nodata) && (md(MeteoData::RSWR) == mio::IOUtils::nodata)))
		miss_rad=true;
	if (enforce_snow_height && (md(MeteoData::HS) == mio::IOUtils::nodata))
		miss_hs=true;
	if (!enforce_snow_height && (md(MeteoData::PSUM) == mio::IOUtils::nodata) )
		miss_precip=true;
	if (!enforce_snow_height && (md(MeteoData::PSUM_PH) == mio::IOUtils::nodata) )
		miss_splitting=true;
	if (md("EA") == mio::IOUtils::nodata)
		miss_ea=true;
	if (md(MeteoData::VW) ==mio::IOUtils::nodata)
		miss_wind=true;
	if (nslopes>1 && (md("DW_DRIFT")==mio::IOUtils::nodata || md("VW_DRIFT")==mio::IOUtils::nodata))
		miss_drift=true;
	if (advective_heat && md("ADV_HEAT")==mio::IOUtils::nodata)
		miss_adv=true;

	if (miss_ta || miss_tsg || miss_rh || miss_rad || miss_precip || miss_splitting || miss_hs || miss_ea || miss_wind || miss_drift || miss_adv) {
		mio::Date now;
		now.setFromSys();
		cerr << "[E] [" << now.toString(mio::Date::ISO) << "] ";
		cerr << StationName << " missing { ";
		if (miss_ta) cerr << "TA ";
		if (miss_tsg) cerr << "TSG ";
		if (miss_rh) cerr << "RH ";
		if (miss_rad) cerr << "sw_radiation ";
		if (miss_hs) cerr << "HS ";
		if (miss_precip) cerr << "precipitation ";
		if (miss_splitting) cerr << "precip_splitting ";
		if (miss_ea) cerr << "lw_radiation ";
		if (miss_wind) cerr << "VW ";
		if (miss_drift) cerr << "drift ";
		if (miss_adv) cerr << "adv_heat ";
		cerr << "} on " << md.date.toString(mio::Date::ISO) << "\n";
		return false;
	}
	return true;
}

inline void copyMeteoData(const mio::MeteoData& md, CurrentMeteo& Mdata,
                   const double prevailing_wind_dir, const double wind_scaling_factor)
{
	Mdata.date   = Date::rnd(md.date, 1);
	Mdata.ta     = md(MeteoData::TA);
	Mdata.rh     = md(MeteoData::RH);
	if (md.param_exists("RH_AVG"))
		Mdata.rh_avg = md("RH_AVG");
	Mdata.vw     = md(MeteoData::VW);
	Mdata.dw     = md(MeteoData::DW);
	Mdata.vw_max = md(MeteoData::VW_MAX);
	if (md.param_exists("VW_AVG"))
		Mdata.vw_avg = md("VW_AVG");

	Mdata.vw_drift = md("VW_DRIFT");
	if (Mdata.vw_drift != mio::IOUtils::nodata) Mdata.vw_drift *= wind_scaling_factor;
	Mdata.dw_drift = md("DW_DRIFT");
	if (Mdata.dw_drift == mio::IOUtils::nodata) Mdata.dw_drift = prevailing_wind_dir;

	Mdata.iswr   = md(MeteoData::ISWR);
	Mdata.rswr   = md(MeteoData::RSWR);

	Mdata.ea  = md("EA");
	Mdata.tss = md(MeteoData::TSS);
	if (md.param_exists("TSS_A12H") && (md("TSS_A12H") != mio::IOUtils::nodata))
		Mdata.tss_a12h = md("TSS_A12H");
	else
		Mdata.tss_a12h = Constants::undefined;
	if (md.param_exists("TSS_A24H") && (md("TSS_A24H") != mio::IOUtils::nodata))
		Mdata.tss_a24h = md("TSS_A24H");
	else
		Mdata.tss_a24h = Constants::undefined;
	Mdata.ts0 = md(MeteoData::TSG);

	Mdata.psum_ph = md(MeteoData::PSUM_PH);
	Mdata.psum = md(MeteoData::PSUM);
	if (md.param_exists("PSUM_TECH"))
		Mdata.psum_tech = md("PSUM_TECH");
	else
		Mdata.psum_tech = Constants::undefined;

	Mdata.hs = md(MeteoData::HS);
	if (md.param_exists("HS_A3H") && (md("HS_A3H") != mio::IOUtils::nodata))
		Mdata.hs_a3h = md("HS_A3H");
	else
		Mdata.hs_a3h = Constants::undefined;

	// Add measured new snow density if available
	if (md.param_exists("RHO_HN"))
		Mdata.rho_hn = md("RHO_HN");

	// Add geo_heat if available
	if(md.param_exists("GEO_HEAT"))
		Mdata.geo_heat = md("GEO_HEAT");
	else
		Mdata.geo_heat = mio::IOUtils::nodata;

	// Add advective heat (for permafrost) if available
	if (md.param_exists("ADV_HEAT"))
		Mdata.adv_heat = md("ADV_HEAT");

	// Add massbal parameters (surface snow melt, snow drift, sublimation), all mass fluxes in kg m-2 CALCULATION_STEP_LENGTH-1
	if(md.param_exists("SMELT"))
		Mdata.surf_melt = md("SMELT");
	else
		Mdata.surf_melt = mio::IOUtils::nodata;
	if(md.param_exists("SNOWD"))
		Mdata.snowdrift = md("SNOWD");
	else
		Mdata.snowdrift = mio::IOUtils::nodata;
	if(md.param_exists("SUBLI"))
		Mdata.sublim = md("SUBLI");
	else
		Mdata.sublim = mio::IOUtils::nodata;
	
	//Add atmospheric optical depth and atmospheric pressure parameters
	if(md.param_exists("ODC"))
		Mdata.odc = md("ODC");
	else
		Mdata.odc = mio::IOUtils::nodata;
	if(md.param_exists("P"))
		Mdata.p = md("P");
	else
		Mdata.p = mio::IOUtils::nodata;

}

inline double getHS_last3hours(mio::IOManager &io, const mio::Date& current_date)
{
	std::vector<mio::MeteoData> MyMeteol3h;

	try {
		io.getMeteoData(current_date - 3.0/24.0, MyMeteol3h);  // meteo data with 3 h (left) lag
	} catch (...) {
		cerr << "[E] failed to read meteo data with 3 hours (left) lag\n";
		throw;
	}

	if (MyMeteol3h[0].param_exists("HS_A3H") && (MyMeteol3h[0]("HS_A3H") != mio::IOUtils::nodata))
		return MyMeteol3h[0]("HS_A3H");
	else
		return Constants::undefined;
}

/**
 * @brief Make sure that both short wave fluxes get at least a "realistic" value but measured albedo only if both fluxes are measured
 * @note To be done only for flat field or single slope station
 * @param Mdata
 * @param Xdata
 * @param slope
 */
inline void setShortWave(CurrentMeteo& Mdata, const SnowStation& Xdata, const bool& iswr_is_net)
{
	if ((Mdata.iswr > 5.) && (Mdata.rswr > 3.) && !iswr_is_net)
		Mdata.mAlbedo = Mdata.rswr / Mdata.iswr;
	else
		Mdata.mAlbedo = Constants::undefined;

	const double cAlbedo = Xdata.Albedo;

	if (iswr_is_net) {
		const double netSW = Mdata.iswr;
		if(netSW==0.) { //this should only happen at night
			Mdata.iswr = 0.;
			Mdata.rswr = 0.;
			return;
		}
		Mdata.iswr = netSW / (1. - cAlbedo);
		Mdata.rswr = netSW / (1./cAlbedo - 1.);
		return;
	}

	if (Mdata.iswr == mio::IOUtils::nodata)
		Mdata.iswr = Mdata.rswr / Xdata.Albedo;
	if (Mdata.rswr == mio::IOUtils::nodata)
		Mdata.rswr = Mdata.iswr * Xdata.Albedo;
}

//for a given config (that can be altered) and original meteo data, prepare the snowpack data structures
//This means that all tweaking of config MUST be reflected in the config object
inline void dataForCurrentTimeStep(CurrentMeteo& Mdata, SurfaceFluxes& surfFluxes, vector<SnowStation>& vecXdata,
                            const Slope& slope, SnowpackConfig& cfg,
                            SunObject &sun,
                            double& precip, const double& lw_in, const double hs_a3hl6,
                            double& tot_mass_in,
                            const std::string& variant)
{
	SnowStation &currentSector = vecXdata[slope.sector]; //alias: the current station
	const bool isMainStation = (slope.sector == slope.mainStation);
	const bool useCanopyModel = cfg.get("CANOPY", "Snowpack");
	const bool perp_to_slope = cfg.get("PERP_TO_SLOPE", "SnowpackAdvanced");
	const bool iswr_is_net = cfg.get("ISWR_IS_NET", "Input");
	if (Mdata.tss == mio::IOUtils::nodata) {
		cfg.addKey("MEAS_TSS", "Snowpack", "false");
	}

	Meteo meteo(cfg);

	// Reset Surface and Canopy Data to zero if you seek current values
	const bool avgsum_time_series = cfg.get("AVGSUM_TIME_SERIES", "Output");
	if (!avgsum_time_series) {
		const bool cumsum_mass = cfg.get("CUMSUM_MASS", "Output");
		surfFluxes.reset(cumsum_mass);
		if (useCanopyModel)
			currentSector.Cdata.reset(cumsum_mass);

		const bool mass_balance = cfg.get("MASS_BALANCE", "SnowpackAdvanced");
		if (mass_balance) {
			// Do an initial mass balance check
			if (!massBalanceCheck(currentSector, surfFluxes, tot_mass_in))
				prn_msg(__FILE__, __LINE__, "msg+", Mdata.date, "Mass error during initial check!");
		}
	}
	// Reset surfFluxes.drift and surfFluxes.mass[MS_WIND] anyway since you use these to transport eroded snow
	surfFluxes.drift = 0.;
	surfFluxes.mass[SurfaceFluxes::MS_WIND] = 0.;

	if (isMainStation) {
		// Check for growing grass
		if (!meteo.compHSrate(Mdata, currentSector, hs_a3hl6))
			cfg.addKey("DETECT_GRASS", "SnowpackAdvanced", "false");

		// Set iswr/rswr and measured albedo
		setShortWave(Mdata, currentSector, iswr_is_net);
		meteo.compRadiation(currentSector, sun, cfg, Mdata);
	} else { // Virtual slope
		cfg.addKey("CHANGE_BC", "Snowpack", "false");
		cfg.addKey("MEAS_TSS", "Snowpack", "false");
		Mdata.tss = Constants::undefined;
		cfg.addKey("ENFORCE_MEASURED_SNOW_HEIGHTS", "Snowpack", "true");
		cfg.addKey("DETECT_GRASS", "SnowpackAdvanced", "false");
	}

	std::string sw_mode = cfg.get("SW_MODE", "Snowpack"); //it must be after calling compRadiation!

	// Project irradiance on slope; take care of measured snow depth and/or precipitations too
	if (!perp_to_slope) {
		meteo.radiationOnSlope(currentSector, sun, Mdata, surfFluxes);
		if ( ((sw_mode == "REFLECTED") || (sw_mode == "BOTH"))
			&& (currentSector.meta.getSlopeAngle() > Constants::min_slope_angle)) { // Do not trust blindly measured RSWR on slopes
			cfg.addKey("SW_MODE", "Snowpack", "INCOMING"); // as Mdata.iswr is the sum of dir_slope and diff
		}
		if (Mdata.psum != mio::IOUtils::nodata) {
			meteo.projectPrecipitations(currentSector.meta.getSlopeAngle(), Mdata.psum, Mdata.hs);
		}
	}

	// Find the Wind Profile Parameters, w/ or w/o canopy; take care of canopy
	meteo.compMeteo(Mdata, currentSector, true);

	if (isMainStation) {
		// Update precipitation memory of main station
		if (Mdata.psum != mio::IOUtils::nodata) {
			precip += Mdata.psum;
		}

		if (Mdata.hs != mio::IOUtils::nodata) {
			currentSector.mH = Mdata.hs + currentSector.Ground;
		}
	} else { // Virtual slope
		currentSector.mH = Constants::undefined;

		// A) Compute depth of snowfall (hn*) and new snow density (rho_hn*)
		double hn_slope = 0., rho_hn_slope = SnLaws::min_hn_density;
		if (vecXdata[slope.mainStation].hn > 0.) {
			// Assign new snow depth and density from station field (usually flat)
			hn_slope = vecXdata[slope.mainStation].hn * currentSector.cos_sl;
			rho_hn_slope = vecXdata[slope.mainStation].rho_hn;
		}
		/*
		 * Snow redistribution on slopes: Add windward eroded snow to lee slope
		 * These are very important lines: Note that deposition is treated here (lee)
		 * while erosion is treated in SnowDrift.c (windward).
		*/
		if (slope.snow_redistribution && (slope.sector == slope.lee)) {
			// If it is not snowing, use surface snow density on windward slope
			if (!(hn_slope > 0.)) {
				rho_hn_slope = vecXdata[slope.luv].rho_hn;
			}
			// Add eroded mass from windward slope
			if (rho_hn_slope != 0.) {
				hn_slope += vecXdata[slope.luv].ErosionMass / rho_hn_slope;
			}
			vecXdata[slope.luv].ErosionMass = 0.;
		}
		// Update depth of snowfall on slopes.
		// This may include contributions from drifting snow eroded on the windward (luv) slope.
		if ((hn_slope > 0.) && (vecXdata[slope.mainStation].cH > 0.01)) {
			currentSector.hn = hn_slope;
			currentSector.rho_hn = rho_hn_slope;
		}

		// B) Check whether to use incoming longwave as estimated from station field
		const bool meas_incoming_longwave = cfg.get("MEAS_INCOMING_LONGWAVE", "SnowpackAdvanced");
		if (!meas_incoming_longwave) {
			Mdata.ea = SnLaws::AirEmissivity(lw_in, Mdata.ta, variant);
		}
	}
}

/**
 * @brief determine which outputs need to be done for the current time step
 * @param mn_ctrl timestep control structure
 * @param step current time integration step
 * @param sno_step current step in the sno files (current sno profile)
 */

inline void getOutputControl(MainControl& mn_ctrl, const mio::Date& step, const mio::Date& sno_step,
                      const double& calculation_step_length,
                      const double& tsstart, const double& tsdaysbetween,
                      const double& profstart, const double& profdaysbetween,
                      const double& first_backup, const double& backup_days_between)
{
//HACK: put all tsstart, tsdaysbetween, etc in MainControl as well as current timestep
	const double Dstep = step.getJulian();
	const double Dsno_step = sno_step.getJulian();
	if (mn_ctrl.resFirstDump) {
		mn_ctrl.HzDump = false;
		mn_ctrl.TsDump = true;
		mn_ctrl.PrDump = true;
		mn_ctrl.resFirstDump = false;
	} else {
		// Hazard data, every half-hour
		mn_ctrl.HzDump = booleanTime(Dstep, 0.5/24., 0.0, calculation_step_length);
		// Time series (*.met)
		double bool_start = H_TO_D(tsstart);
		if ( bool_start > 0. )
			bool_start += Dsno_step;
		mn_ctrl.TsDump = booleanTime(Dstep, tsdaysbetween, bool_start, calculation_step_length);
		// Profile (*.pro)
		bool_start = H_TO_D(profstart);
		if ( bool_start > 0. )
			bool_start += Dsno_step;
		mn_ctrl.PrDump = booleanTime(Dstep, profdaysbetween, bool_start, calculation_step_length);

	}

	// Additional Xdata backup (*.<JulianDate>sno)
	const double bool_start = Dsno_step + first_backup;
	mn_ctrl.XdataDump = booleanTime(Dstep, backup_days_between, bool_start, calculation_step_length);
}

inline bool readSlopeMeta(mio::IOManager& io, SnowpackIO& snowpackio, SnowpackConfig& cfg, const size_t& i_stn,
                   Slope& slope, mio::Date &current_date, vector<SN_SNOWSOIL_DATA> &vecSSdata,
                   vector<SnowStation> &vecXdata, ZwischenData &sn_Zdata, CurrentMeteo& Mdata,
                   double &wind_scaling_factor, double &time_count_deltaHS)
{
	string snowfile;
	stringstream ss;
	ss << "SNOWFILE" << i_stn+1;
	cfg.getValue(ss.str(), "Input", snowfile, mio::IOUtils::nothrow);

	//Read SSdata for every "slope" referred to as sector where sector 0 corresponds to the main station
	for (size_t sector=slope.mainStation; sector<slope.nSlopes; sector++) {
		try {
			if (sector == slope.mainStation) {
				if (snowfile.empty()) {
					snowfile = vecStationIDs[i_stn];
				} else {
					const size_t pos_dot = snowfile.rfind(".");
					const size_t pos_slash = snowfile.rfind("/");
					if (((pos_dot != string::npos) && (pos_dot > pos_slash)) ||
						((pos_dot != string::npos) && (pos_slash == string::npos))) //so that the dot is not in a directory name
						snowfile.erase(pos_dot, snowfile.size()-pos_dot);
				}
				snowpackio.readSnowCover(snowfile, vecStationIDs[i_stn], vecSSdata[slope.mainStation], sn_Zdata, (vecXdata[sector].Seaice!=NULL));
				prn_msg(__FILE__, __LINE__, "msg-", mio::Date(), "Reading snow cover data for station %s",
				        vecStationIDs[i_stn].c_str());
				// NOTE (Is it a HACK?) Reading station meta data provided in meteo data and prebuffering those data
				vector<mio::MeteoData> vectmpmd;
				if (current_date.isUndef()) //either force the start date or take it from the sno file
					current_date = Date::rnd(vecSSdata[slope.mainStation].profileDate, 1);
				else
					vecSSdata[sector].profileDate = current_date;
				io.getMeteoData(current_date, vectmpmd);
				if (vectmpmd.empty())
					throw mio::IOException("No data found for station " + vecStationIDs[i_stn] + " on "
					                       + current_date.toString(mio::Date::ISO), AT);
				Mdata.setMeasTempParameters(vectmpmd[i_stn]);
				vecSSdata[slope.mainStation].meta = mio::StationData::merge(vectmpmd[i_stn].meta,
				                                vecSSdata[slope.mainStation].meta);
			} else {
				std::stringstream sec_snowfile;
				sec_snowfile << "" << snowfile << sector;
				ss.str("");
				ss << "" << vecSSdata[slope.mainStation].meta.getStationID() << sector;
				snowpackio.readSnowCover(sec_snowfile.str(), ss.str(), vecSSdata[sector], sn_Zdata, (vecXdata[sector].Seaice!=NULL));
				vecSSdata[sector].meta.position = vecSSdata[slope.mainStation].meta.getPosition();
				vecSSdata[sector].meta.stationName = vecSSdata[slope.mainStation].meta.getStationName();
				if (!current_date.isUndef()) vecSSdata[sector].profileDate = current_date; //this should have been set when processing the main station
			}
			vecXdata[sector].initialize(vecSSdata[sector], sector); // Generate the corresponding Xdata
		} catch (const exception& e) {
			if (sector == slope.first) {
				prn_msg(__FILE__, __LINE__, "msg-", mio::Date(),
				        "No virtual slopes! Computation for main station %s only!", vecStationIDs[i_stn].c_str());
				slope.nSlopes = 1;
				if ((mode == "OPERATIONAL")
					&& (vecSSdata[slope.mainStation].meta.getSlopeAngle() > Constants::min_slope_angle)) {
					cfg.addKey("PERP_TO_SLOPE", "SnowpackAdvanced", "true");
				}
				break;
			} else {
				cout << e.what();
				throw;
			}
		}

		// Operational mode ONLY: Pass ... wind_factor, snow depth discrepancy time counter
		if ((sector == slope.mainStation) && (mode == "OPERATIONAL")) {
			wind_scaling_factor = vecSSdata[slope.mainStation].WindScalingFactor;
			time_count_deltaHS = vecSSdata[slope.mainStation].TimeCountDeltaHS;
		}
	}
	prn_msg(__FILE__, __LINE__, "msg-", mio::Date(), "Finished initializing station %s", vecStationIDs[i_stn].c_str());

	//CHECK date inconsistencies between sno files
	bool dates_consistent(true);
	for (size_t sector=slope.first; sector<slope.nSlopes; sector++) {
		if (vecSSdata[sector].profileDate != vecSSdata[slope.mainStation].profileDate) {
			prn_msg(__FILE__, __LINE__, "err", mio::Date(),
				"%s : Date of profile on virtual slope %d inconsistent with flat field", vecStationIDs[i_stn].c_str(), sector);
			dates_consistent = false;

		}
	}
	if (!dates_consistent) return false; //go to next station

	// Do not go ahead if starting time is larger than maxtime!
	if (vecSSdata[slope.mainStation].profileDate > dateEnd) {
		prn_msg(__FILE__, __LINE__, "err", mio::Date(),
			"%s : Starting time (%.5lf) larger than end time(%.5lf)",
			vecStationIDs[i_stn].c_str(), vecSSdata[slope.mainStation].profileDate.getJulian(), dateEnd.getJulian());
		return false; //goto next station
	}

	return true;
}

inline void addSpecialKeys(SnowpackConfig &cfg)
{
	const std::string variant = cfg.get("VARIANT", "SnowpackAdvanced");

	// Add keys to perform running mean in Antarctic variant
	if (variant == "ANTARCTICA") {
		cfg.addKey("VW_AVG::COPY", "Input", "VW");
		cfg.addKey("RH_AVG::COPY", "Input", "RH");

		cfg.addKey("VW_AVG::filter1", "Filters", "AGGREGATE");
		cfg.addKey("VW_AVG::arg1::type", "Filters", "MEAN");
		cfg.addKey("VW_AVG::arg1::soft", "Filters", "true");
		cfg.addKey("VW_AVG::arg1::min_pts", "Filters", "101");
		cfg.addKey("VW_AVG::arg1::min_span", "Filters", "360000");
		cfg.addKey("RH_AVG::filter1", "Filters", "AGGREGATE");
		cfg.addKey("RH_AVG::arg1::type", "Filters", "MEAN");
		cfg.addKey("RH_AVG::arg1::soft", "Filters", "true");
		cfg.addKey("RH_AVG::arg1::min_pts", "Filters", "101");
		cfg.addKey("RH_AVG::arg1::min_span", "Filters", "360000");
	}

	const std::string tst_sw_mode = cfg.get("SW_MODE", "Snowpack"); // Test settings for SW_MODE
	if (tst_sw_mode == "BOTH") { //HACK: this is only for INP!
		// Make sure there is not only one of ISWR and RSWR available
		bool iswr_inp=true, rswr_inp = true;
		cfg.getValue("ISWR_INP","Input",iswr_inp,IOUtils::nothrow);
		cfg.getValue("RSWR_INP","Input",rswr_inp,IOUtils::nothrow);
		if (!(iswr_inp && rswr_inp)) {
			cerr << "[E] SW_MODE = " << tst_sw_mode << ": Please set both ISWR_INP and RSWR_INP to true in [Input]-section of io.ini!\n";
			exit(1);
		}
	}

	const bool useCanopyModel = cfg.get("CANOPY", "Snowpack");
	bool detect_grass = cfg.get("DETECT_GRASS", "SnowpackAdvanced");
	if (mode == "OPERATIONAL") {
		cfg.addKey("RESEARCH", "SnowpackAdvanced", "false");
		cfg.addKey("AVGSUM_TIME_SERIES", "Output", "false");
		if (useCanopyModel) {
			throw mio::IOException("Please don't set CANOPY to 1 in OPERATIONAL mode", AT);
		}
		if (!detect_grass){
			cfg.addKey("DETECT_GRASS", "SnowpackAdvanced", "true");
			detect_grass = true;
		}
	}

	if (detect_grass) {
		// we need various average values of tss and hs, all for "past" windows (left)
		// Require at least one value per 3 hours
		cfg.addKey("TSS_A24H::COPY", "Input", "TSS");
		cfg.addKey("TSS_A24H::filter1", "Filters", "AGGREGATE");
		cfg.addKey("TSS_A24H::arg1::type", "Filters", "MEAN");
		cfg.addKey("TSS_A24H::arg1::centering", "Filters", "left");
		cfg.addKey("TSS_A24H::arg1::min_pts", "Filters", "48"); //TODO change # data required to 4
		cfg.addKey("TSS_A24H::arg1::min_span", "Filters", "86340");

		cfg.addKey("TSS_A12H::COPY", "Input", "TSS");
		cfg.addKey("TSS_A12H::filter1", "Filters", "AGGREGATE");
		cfg.addKey("TSS_A12H::arg1::type", "Filters", "MEAN");
		cfg.addKey("TSS_A12H::arg1::centering", "Filters", "left");
		cfg.addKey("TSS_A12H::arg1::min_pts", "Filters", "24"); //TODO change # data required to 2
		cfg.addKey("TSS_A12H::arg1::min_span", "Filters", "43140");

		cfg.addKey("HS_A3H::COPY", "Input", "HS");
		cfg.addKey("HS_A3H::filter1", "Filters", "AGGREGATE");
		cfg.addKey("HS_A3H::arg1::type", "Filters", "MEAN");
		cfg.addKey("HS_A3H::arg1::centering", "Filters", "left");
		cfg.addKey("HS_A3H::arg1::min_pts", "Filters", "6"); //TODO change # data required to 1
		cfg.addKey("HS_A3H::arg1::min_span", "Filters", "10740");
	}
	
	//warn the user if the precipitation miss proper re-accumulation
	const bool HS_driven = cfg.get("ENFORCE_MEASURED_SNOW_HEIGHTS", "Snowpack");
	if (mode != "OPERATIONAL" && !HS_driven) {
		const bool psum_key_exists = cfg.keyExists("PSUM::resample", "Interpolations1D");
		const std::string psum_resampling = (psum_key_exists)? IOUtils::strToUpper( cfg.get("PSUM::resample", "Interpolations1D") ) : "LINEAR";
		if (psum_resampling!="ACCUMULATE") {
			std::cerr << "[W] The precipitation should be re-accumulated over CALCULATION_STEP_LENGTH, not doing it is most probably an error!\n";
		} else {
			const double psum_accumulate = cfg.get("PSUM::accumulate::period", "Interpolations1D");
			const double sn_step_length = cfg.get("CALCULATION_STEP_LENGTH", "Snowpack");
			if (sn_step_length*60. != psum_accumulate)
				std::cerr << "[W] The precipitation should be re-accumulated over CALCULATION_STEP_LENGTH (currently, over " <<  psum_accumulate << "s)\n";
		}
	}
}

inline void writeForcing(Date d1, const Date& d2, const double& Tstep, IOManager &io)
{
	std::vector< std::vector<MeteoData> > vecMeteo;
	prn_msg(__FILE__, __LINE__, "msg",  mio::Date(), "Reading and writing out forcing data...");

	const std::string experiment = io.getConfig().get("EXPERIMENT", "Output");
	std::map<std::string, size_t> mapIDs; //over a large time range, the number of stations might change... this is the way to make it work
	std::vector<MeteoData> Meteo; //we need some intermediate storage, for storing data sets for 1 timestep
	
	for(; d1<=d2; d1+=Tstep) { //time loop
		io.getMeteoData(d1, Meteo); //read 1 timestep at once, forcing resampling to the timestep
		for(size_t ii=0; ii<Meteo.size(); ii++) {
			const std::string stationID( Meteo[ii].meta.stationID );
			if (mapIDs.count( stationID )==0) { //if this is the first time we encounter this station, save where it should be inserted
				mapIDs[ stationID ] = ii;
				if (ii>=vecMeteo.size()) vecMeteo.push_back( std::vector<MeteoData>() ); //allocate a new station
			}
			Meteo[ii].meta.stationID = Meteo[ii].meta.stationID + "_" + experiment + "_forcing";
			vecMeteo[ mapIDs[stationID] ].push_back(Meteo[ii]); //fill the data manually into the vector of vectors
		}
	}
	io.writeMeteoData(vecMeteo);

	prn_msg(__FILE__, __LINE__, "msg",  mio::Date(), "Forcing data written out");
}

inline void printStartInfo(const SnowpackConfig& cfg, const std::string& name)
{
	const bool useSoilLayers = cfg.get("SNP_SOIL", "Snowpack");
	if (useSoilLayers) {
		bool soil_flux = false;
		cfg.getValue("SOIL_FLUX", "Snowpack", soil_flux);
		prn_msg(__FILE__, __LINE__, "msg",  mio::Date(), "Start SNOWPACK w/ soil layers in %s mode", mode.c_str());
	} else {
		prn_msg(__FILE__, __LINE__, "msg",  mio::Date(), "Start SNOWPACK in %s mode", mode.c_str());
	}

	const std::string variant = cfg.get("VARIANT", "SnowpackAdvanced");
	if (variant != "DEFAULT") {
		prn_msg(__FILE__, __LINE__, "msg",  mio::Date(), "Variant is '%s'", variant.c_str());
	}
	prn_msg(__FILE__, __LINE__, "msg-", mio::Date(),
	        "%s compiled on %s at %s", name.c_str(), __DATE__, __TIME__);

	if (mode != "OPERATIONAL") {
		const std::string experiment = cfg.get("EXPERIMENT", "Output");
		const std::string outpath = cfg.get("METEOPATH", "Output");
		prn_msg(__FILE__, __LINE__, "msg-", mio::Date(), "Experiment : %s", experiment.c_str());
		prn_msg(__FILE__, __LINE__, "msg-", mio::Date(), "Output dir : %s", outpath.c_str());
	}
}

// SNOWPACK MAIN **************************************************************
inline void real_main (int argc, char *argv[])
{
#ifdef DEBUG_ARITHM
	feenableexcept(FE_DIVBYZERO | FE_INVALID | FE_OVERFLOW ); //for halting the process at arithmetic exceptions, see also ReSolver1d
#endif
	//parse the command line arguments
	std::string begin_date_str, end_date_str;
	parseCmdLine(argc, argv, begin_date_str, end_date_str);

	const bool prn_check = false;
	mio::Timer meteoRead_timer;
	mio::Timer run_timer;
	run_timer.start();
	time_t nowSRT = time(NULL);
	MainControl mn_ctrl; //Time step control parameters

	SnowpackConfig cfg(cfgfile);
	addSpecialKeys(cfg);

	const double i_time_zone = cfg.get("TIME_ZONE", "Input"); //get user provided input time_zone
	if (!begin_date_str.empty()) {
		mio::IOUtils::convertString(dateBegin, begin_date_str, i_time_zone);
	}
	if (end_date_str == "NOW") { //interpret user provided end date
		dateEnd.setFromSys();
		dateEnd.setTimeZone(i_time_zone);
		dateEnd.rnd(1800, mio::Date::DOWN);
	} else {
		mio::IOUtils::convertString(dateEnd, end_date_str, i_time_zone);
	}

	const std::string variant = cfg.get("VARIANT", "SnowpackAdvanced");
	const std::string experiment = cfg.get("EXPERIMENT", "Output");
	const std::string outpath = cfg.get("METEOPATH", "Output");
	const bool useSoilLayers = cfg.get("SNP_SOIL", "Snowpack");
	const bool useCanopyModel = cfg.get("CANOPY", "Snowpack");
	const double calculation_step_length = cfg.get("CALCULATION_STEP_LENGTH", "Snowpack");
	const double sn_dt = M_TO_S(calculation_step_length); //Calculation time step in seconds

	int nSolutes = Constants::iundefined;
	cfg.getValue("NUMBER_OF_SOLUTES", "Input", nSolutes, mio::IOUtils::nothrow);
	if (nSolutes > 0) SnowStation::number_of_solutes = static_cast<short unsigned int>(nSolutes);

	//Interval between profile backups (*.sno\<JulianDate\>) (d)
	double backup_days_between = 400.;
	cfg.getValue("BACKUP_DAYS_BETWEEN", "Output", backup_days_between, mio::IOUtils::nothrow);
	//First additional profile backup (*.sno\<JulianDate\>) since start of simulation (d)
	double first_backup = 0.;
	cfg.getValue("FIRST_BACKUP", "Output", first_backup, mio::IOUtils::nothrow);

	const bool snowPrep = cfg.get("SNOW_PREPARATION", "SnowpackAdvanced");
	const bool classify_profile = cfg.get("CLASSIFY_PROFILE", "Output");
	const bool profwrite = cfg.get("PROF_WRITE", "Output");
	const double profstart = cfg.get("PROF_START", "Output");
	const double profdaysbetween = cfg.get("PROF_DAYS_BETWEEN", "Output");
	const bool tswrite = cfg.get("TS_WRITE", "Output");
	const double tsstart = cfg.get("TS_START", "Output");
	const double tsdaysbetween = cfg.get("TS_DAYS_BETWEEN", "Output");

	const bool precip_rates = cfg.get("PRECIP_RATES", "Output");
	const bool avgsum_time_series = cfg.get("AVGSUM_TIME_SERIES", "Output");
	const bool cumsum_mass = cfg.get("CUMSUM_MASS", "Output");
	const double thresh_rain = cfg.get("THRESH_RAIN", "SnowpackAdvanced"); //Rain only for air temperatures warmer than threshold (degC)
	const bool advective_heat = cfg.get("ADVECTIVE_HEAT", "SnowpackAdvanced");
	const bool soil_flux = (useSoilLayers || variant == "SEAICE") ? cfg.get("SOIL_FLUX", "Snowpack") : false;

	//If the user provides the stationIDs - operational use case
	if (!vecStationIDs.empty()) { //operational use case: stationIDs provided on the command line
		for (size_t i_stn=0; i_stn<vecStationIDs.size(); i_stn++) {
			stringstream ss;
			ss << "STATION" << i_stn+1;
			cfg.addKey(ss.str(), "Input", vecStationIDs[i_stn]);
		}
	}

	SnowpackIO snowpackio(cfg);
	mio::IOManager io(cfg);
	io.setMinBufferRequirements(IOUtils::nodata, 1.1); //we require the buffer to contain at least 1.1 day before the current point

	if (vecStationIDs.empty()) { //research use case: stationIDs provided by the available input files
		vector<StationData> accessible_stations;
		io.getStationData(dateEnd, accessible_stations); //we are retrieving meta information from MeteoIO
		for (size_t ii=0; ii<accessible_stations.size(); ii++) {
			vecStationIDs.push_back( accessible_stations[ii].getStationID() ); //HACK: accessible_stations should be directly used
		}
	}

	//now, let's start!
	printStartInfo(cfg, string(argv[0]));

	// START LOOP OVER ALL STATIONS
	bool write_forcing = cfg.get("WRITE_PROCESSED_METEO", "Output"); //it will be set to false once it has been done
	for (size_t i_stn=0; i_stn<vecStationIDs.size(); i_stn++) {
		cout << endl;
		prn_msg(__FILE__, __LINE__, "msg-", mio::Date(), "Run on meteo station %s", vecStationIDs[i_stn].c_str());
		run_timer.reset();
		meteoRead_timer.reset();

		Slope slope(cfg);
		Cumsum cumsum(slope.nSlopes);

		double lw_in = Constants::undefined;    // Storage for LWin from flat field energy balance

		// Used to scale wind for blowing and drifting snowpack (from statistical analysis)
		double wind_scaling_factor = cfg.get("WIND_SCALING_FACTOR", "SnowpackAdvanced");

		// Control of time window: used for adapting diverging snow depth in operational mode
		double time_count_deltaHS = 0.;

		// Snowpack data (input/output)
		ZwischenData sn_Zdata;   // "Memory"-data, required for every operational station
		vector<SN_SNOWSOIL_DATA> vecSSdata(slope.nSlopes, SN_SNOWSOIL_DATA(/*number_of_solutes*/));
		vector<SnowStation> vecXdata;
		for (size_t ii=0; ii<slope.nSlopes; ii++) //fill vecXdata with *different* SnowStation objects
			vecXdata.push_back( SnowStation(useCanopyModel, useSoilLayers, (variant=="SEAICE")/*, number_of_solutes*/) );

		// Create meteo data object to hold interpolated current time steps
		CurrentMeteo Mdata(cfg);
		// To collect surface exchange data for output
		SurfaceFluxes surfFluxes/*(number_of_solutes)*/;
		// Boundary condition (fluxes)
		BoundCond sn_Bdata;

		mio::Date current_date( dateBegin );
		meteoRead_timer.start();
		if (mode == "OPERATIONAL")
			cfg.addKey("PERP_TO_SLOPE", "SnowpackAdvanced", "false");
		const bool read_slope_status = readSlopeMeta(io, snowpackio, cfg, i_stn, slope, current_date, vecSSdata, vecXdata, sn_Zdata, Mdata, wind_scaling_factor, time_count_deltaHS);
		meteoRead_timer.stop();
		if (!read_slope_status) continue; //something went wrong, move to the next station

		memset(&mn_ctrl, 0, sizeof(MainControl));
		if (mode == "RESEARCH") {
			mn_ctrl.resFirstDump = true; //HACK to dump the initial state in research mode
			deleteOldOutputFiles(outpath, experiment, vecStationIDs[i_stn], slope.nSlopes, snowpackio.getExtensions());
			cfg.write(outpath + "/" + vecStationIDs[i_stn] + "_" + experiment + ".ini"); //output config
			if (!restart) current_date -= calculation_step_length/(24.*60.);
		} else {
			const std::string db_name = cfg.get("DBNAME", "Output", "");
			if (db_name == "sdbo" || db_name == "sdbt")
				mn_ctrl.sdbDump = true;
		}

		SunObject sun(vecSSdata[slope.mainStation].meta.position.getLat(), vecSSdata[slope.mainStation].meta.position.getLon(), vecSSdata[slope.mainStation].meta.position.getAltitude());
		sun.setElevationThresh(0.6);
		vector<ProcessDat> qr_Hdata;     //Hazard data for t=0...tn
		vector<ProcessInd> qr_Hdata_ind; //Hazard data Index for t=0...tn
		const double duration = (dateEnd.getJulian() - current_date.getJulian() + 0.5/24)*24*3600; //HACK: why is it computed this way?
		Hazard hazard(cfg, duration);
		hazard.initializeHazard(sn_Zdata.drift24, vecXdata.at(0).meta.getSlopeAngle(), qr_Hdata, qr_Hdata_ind);

		prn_msg(__FILE__, __LINE__, "msg", mio::Date(), "Start simulation for %s on %s",
			vecStationIDs[i_stn].c_str(), current_date.toString(mio::Date::ISO_TZ).c_str());
		prn_msg(__FILE__, __LINE__, "msg-", mio::Date(), "End date specified by user: %s",
		        dateEnd.toString(mio::Date::ISO_TZ).c_str());
		prn_msg(__FILE__, __LINE__, "msg-", mio::Date(), "Integration step length: %f min",
		        calculation_step_length);

		bool computed_one_timestep = false;
		double meteo_step_length = -1.;
		const bool enforce_snow_height = cfg.get("ENFORCE_MEASURED_SNOW_HEIGHTS", "Snowpack");

		//from current_date to dateEnd, if necessary write out meteo forcing
		if (write_forcing==true) {
			writeForcing(current_date, dateEnd, calculation_step_length/1440, io);
			write_forcing = false; //no need to call it again for the other stations
		}
		
		// START TIME INTEGRATION LOOP
		do {
			current_date += calculation_step_length/1440;
			mn_ctrl.nStep++;
			mn_ctrl.nAvg++;

			// Get meteo data
			vector<mio::MeteoData> vecMyMeteo;
			meteoRead_timer.start();
			io.getMeteoData(current_date, vecMyMeteo);
			if(meteo_step_length<0.) {
				std::stringstream ss2;
				meteo_step_length = io.getAvgSamplingRate();
				ss2 << "" << meteo_step_length;
				cfg.addKey("METEO_STEP_LENGTH", "Snowpack", ss2.str());
			}
			meteoRead_timer.stop();
			editMeteoData(vecMyMeteo[i_stn], variant, thresh_rain);
			if (!validMeteoData(vecMyMeteo[i_stn], vecStationIDs[i_stn], variant, enforce_snow_height, advective_heat, soil_flux, slope.nSlopes)) {
				prn_msg(__FILE__, __LINE__, "msg-", current_date, "No valid data for station %s on [%s]",
				        vecStationIDs[i_stn].c_str(), current_date.toString(mio::Date::ISO).c_str());
				current_date -= calculation_step_length/1440;
				break;
			}

			//determine which outputs will have to be done
			getOutputControl(mn_ctrl, current_date, vecSSdata[slope.mainStation].profileDate, calculation_step_length,
			                 tsstart, tsdaysbetween, profstart, profdaysbetween,
			                 first_backup, backup_days_between);
			//Radiation data
			sun.setDate(current_date.getJulian(), current_date.getTimeZone());
			const double hs_a3hl6 = getHS_last3hours(io, current_date);

			// START LOOP OVER ASPECTS
			for (unsigned int slope_sequence=0; slope_sequence<slope.nSlopes; slope_sequence++) {
				double tot_mass_in = 0.; // To check mass balance over one CALCULATION_STEP_LENGTH if MASS_BALANCE is set
				SnowpackConfig tmpcfg(cfg);

				//fill Snowpack internal structure with forcing data
				copyMeteoData(vecMyMeteo[i_stn], Mdata, slope.prevailing_wind_dir, wind_scaling_factor);
				Mdata.copySnowTemperatures(vecMyMeteo[i_stn], slope_sequence);
				Mdata.copySolutes(vecMyMeteo[i_stn], SnowStation::number_of_solutes);
				slope.setSlope(slope_sequence, vecXdata, Mdata.dw_drift);
				dataForCurrentTimeStep(Mdata, surfFluxes, vecXdata, slope, tmpcfg,
                                       sun, cumsum.precip, lw_in, hs_a3hl6,
                                       tot_mass_in, variant);

				// Notify user every fifteen days of date being processed
				const double notify_start = floor(vecSSdata[slope.mainStation].profileDate.getJulian()) + 15.5;
				if ((mode == "RESEARCH") && (slope.sector == slope.mainStation)
				        && booleanTime(current_date.getJulian(), 15., notify_start, calculation_step_length)) {
					prn_msg(__FILE__, __LINE__, "msg", current_date,
					            "Station %s (%d slope(s)): advanced to %s (%f) station time",
					                vecSSdata[slope.mainStation].meta.stationID.c_str(), slope.nSlopes,
					                    current_date.toString(mio::Date::DIN).c_str(), current_date.getJulian());
				}

				// SNOWPACK model (Temperature and Settlement computations)
				Snowpack snowpack(tmpcfg); //the snowpack model to use
				Stability stability(tmpcfg, classify_profile);
				snowpack.runSnowpackModel(Mdata, vecXdata[slope.sector], cumsum.precip, sn_Bdata, surfFluxes);
				if (TechSnow::prepare(snowPrep, current_date, vecXdata[slope.sector]))
					snowpack.snowPreparation( vecXdata[slope.sector] );
				
				stability.checkStability(Mdata, vecXdata[slope.sector]);

				/***** OUTPUT SECTION *****/
				surfFluxes.collectSurfaceFluxes(sn_Bdata, vecXdata[slope.sector], Mdata);
				if (slope.sector == slope.mainStation) { // main station only (usually flat field)
					// Calculate consistent lw_in for virtual slopes
					if ( vecXdata[slope.mainStation].getNumberOfElements() > 0 ) {
						double k_eff, gradT;
						k_eff =
						    vecXdata[slope.mainStation].Edata[vecXdata[slope.mainStation].getNumberOfElements()-1].k[TEMPERATURE];
						gradT =
						    vecXdata[slope.mainStation].Edata[vecXdata[slope.mainStation].getNumberOfElements()-1].gradT;
						lw_in = k_eff*gradT + sn_Bdata.lw_out - sn_Bdata.qs - sn_Bdata.ql - sn_Bdata.qr;
					} else {
						lw_in = Constants::undefined;
					}
					// Deal with new snow densities
					if (vecXdata[slope.mainStation].hn > 0.) {
						surfFluxes.cRho_hn = vecXdata[slope.mainStation].rho_hn;
						surfFluxes.mRho_hn = Mdata.rho_hn;
					}
					if (slope.snow_erosion != "NONE") {
						// Update drifting snow index (VI24),
						//   from erosion at the main station only if no virtual slopes are available
						if (slope.mainStationDriftIndex)
							cumulate(cumsum.drift, surfFluxes.drift);
						// Update erosion mass from main station
						// NOTE cumsum.erosion[] will be positive in case of real erosion at any time during the output time step
						if (vecXdata[slope.mainStation].ErosionMass > Constants::eps) {
							// Real erosion
							if (cumsum.erosion[slope.mainStation] > Constants::eps)
								cumsum.erosion[slope.mainStation] += vecXdata[slope.mainStation].ErosionMass;
							else
								cumsum.erosion[slope.mainStation] = vecXdata[slope.mainStation].ErosionMass;
						} else {
							// Potential erosion at main station only
							if (cumsum.erosion[slope.mainStation] < -Constants::eps)
								cumsum.erosion[slope.mainStation] -= surfFluxes.mass[SurfaceFluxes::MS_WIND];
							else if (!(cumsum.erosion[slope.mainStation] > Constants::eps))
								cumsum.erosion[slope.mainStation] = -surfFluxes.mass[SurfaceFluxes::MS_WIND];
						}
					}
					const size_t i_hz = mn_ctrl.HzStep;
					if (mode == "OPERATIONAL") {
						if (!cumsum_mass) { // Cumulate flat field runoff in operational mode
							qr_Hdata.at(i_hz).runoff += surfFluxes.mass[SurfaceFluxes::MS_SNOWPACK_RUNOFF];
							cumsum.runoff += surfFluxes.mass[SurfaceFluxes::MS_SNOWPACK_RUNOFF];
						}
						/*
						 * Snow depth and mass corrections (deflate-inflate):
						 *   Monitor snow depth discrepancy assumed to be due to ...
						 *   ... wrong settling, which in turn is assumed to be due to a wrong estimation ...
						 *   of fresh snow mass because Michi spent many painful days calibrating the settling ...
						 *   and therefore it can't be wrong, dixunt Michi and Charles.
						 */
						const double cH = vecXdata[slope.mainStation].cH - vecXdata[slope.mainStation].Ground;
						const double mH = vecXdata[slope.mainStation].mH - vecXdata[slope.mainStation].Ground;
						// Look for missed erosion or not strong enough settling ...
						// ... and nastily deep "dips" caused by buggy data ...
						if (time_count_deltaHS > -Constants::eps2) {
							if ((mH + 0.01) < cH) {
								time_count_deltaHS += S_TO_D(sn_dt);
							} else {
								time_count_deltaHS = 0.;
							}
						}
						// ... or too strong settling
						if (time_count_deltaHS < Constants::eps2) {
							if ((mH - 0.01) > cH) {
								time_count_deltaHS -= S_TO_D(sn_dt);
							} else {
								time_count_deltaHS = 0.;
							}
						}
						// If the error persisted for at least one day => apply correction
						if (enforce_snow_height && (fabs(time_count_deltaHS) > (1. - 0.05 * M_TO_D(calculation_step_length)))) {
							deflateInflate(Mdata, vecXdata[slope.mainStation],
							               qr_Hdata.at(i_hz).dhs_corr, qr_Hdata.at(i_hz).mass_corr);
							if (prn_check) {
								prn_msg(__FILE__, __LINE__, "msg+", Mdata.date,
								        "InflDefl (i_hz=%u): dhs=%f, dmass=%f, counter=%f",
								        i_hz, qr_Hdata.at(i_hz).dhs_corr, qr_Hdata.at(i_hz).mass_corr,
								        time_count_deltaHS);
							}
							time_count_deltaHS = 0.;
						}
					}
					if (mn_ctrl.HzDump) { // Save hazard data ...
						qr_Hdata.at(i_hz).stat_abbrev = vecStationIDs[i_stn];
						if (mode == "OPERATIONAL") {
							qr_Hdata.at(i_hz).loc_for_snow = (unsigned char)vecStationIDs[i_stn][vecStationIDs[i_stn].length()-1];
							//TODO: WHAT SHOULD WE SET HERE? wstat_abk (not existing yet in DB) and wstao_nr, of course;-)
							qr_Hdata_ind.at(i_hz).loc_for_wind = -1;
						} else {
							qr_Hdata.at(i_hz).loc_for_snow = 2;
							qr_Hdata.at(i_hz).loc_for_wind = 1;
						}
						hazard.getHazardDataMainStation(qr_Hdata.at(i_hz), qr_Hdata_ind.at(i_hz),
						                                sn_Zdata, cumsum.drift, slope.mainStationDriftIndex,
						                                vecXdata[slope.mainStation], Mdata, surfFluxes);
						if (slope.nSlopes==1) { //only one slope, so set lwi_N and lwi_S to the same value
							const double lwi = vecXdata[slope.mainStation].getLiquidWaterIndex();
							if ((lwi < -Constants::eps) || (lwi >= 10.))
								qr_Hdata_ind.at(i_hz).lwi_N = qr_Hdata_ind.at(i_hz).lwi_S = false;
							qr_Hdata.at(i_hz).lwi_N = lwi;
							qr_Hdata.at(i_hz).lwi_S = lwi;
						}
						mn_ctrl.HzStep++;
						if (slope.mainStationDriftIndex)
							cumsum.drift = 0.;
						surfFluxes.hoar = 0.;
					}
					// New snow water equivalent (kg m-2), rain was dealt with in Watertransport.cc
					surfFluxes.mass[SurfaceFluxes::MS_HNW] += vecXdata[slope.mainStation].hn
					                                              * vecXdata[slope.mainStation].rho_hn;
					if (!avgsum_time_series) { // Sum up precipitations
						cumsum.rain += surfFluxes.mass[SurfaceFluxes::MS_RAIN];
						cumsum.snow += surfFluxes.mass[SurfaceFluxes::MS_HNW];
					}
				} else {
					const size_t i_hz = (mn_ctrl.HzStep > 0) ? mn_ctrl.HzStep-1 : 0;
					if (slope.luvDriftIndex) {
						// Update drifting snow index (VI24),
						// considering only snow eroded from the windward slope
						cumulate(cumsum.drift, surfFluxes.drift);
					}
					if (mn_ctrl.HzDump) {
						// NOTE qr_Hdata was first saved at the end of the mainStation simulation, at which time the drift index could not be dumped!
						hazard.getHazardDataSlope(qr_Hdata.at(i_hz), qr_Hdata_ind.at(i_hz),
						                          sn_Zdata.drift24, cumsum.drift, vecXdata[slope.sector],
						                          slope.luvDriftIndex, slope.north, slope.south);
						if(slope.luvDriftIndex) cumsum.drift = 0.;
					}

					// Update erosion mass from windward virtual slope
					cumsum.erosion[slope.sector] += vecXdata[slope.sector].ErosionMass;
				}

				// TIME SERIES (*.met)
				if (tswrite && mn_ctrl.TsDump) {
					// Average fluxes
					if (avgsum_time_series) {
						averageFluxTimeSeries(mn_ctrl.nAvg, useCanopyModel, surfFluxes, vecXdata[slope.sector]);
					} else {
						surfFluxes.mass[SurfaceFluxes::MS_RAIN] = cumsum.rain;
						surfFluxes.mass[SurfaceFluxes::MS_HNW] = cumsum.snow;
						// Add eroded snow from luv to precipitations on lee slope
						if (slope.sector == slope.lee && cumsum.erosion[slope.luv] > Constants::eps)
							surfFluxes.mass[SurfaceFluxes::MS_HNW] += cumsum.erosion[slope.luv] / vecXdata[slope.luv].cos_sl;
					}

					if (precip_rates) { // Precip rates in kg m-2 h-1
						surfFluxes.mass[SurfaceFluxes::MS_RAIN] /= static_cast<double>(mn_ctrl.nAvg)*M_TO_H(calculation_step_length);
						surfFluxes.mass[SurfaceFluxes::MS_HNW] /= static_cast<double>(mn_ctrl.nAvg)*M_TO_H(calculation_step_length);
						if ((mode == "OPERATIONAL") && (!cumsum_mass)) {
							surfFluxes.mass[SurfaceFluxes::MS_SNOWPACK_RUNOFF] = cumsum.runoff;
							surfFluxes.mass[SurfaceFluxes::MS_SNOWPACK_RUNOFF] /= static_cast<double>(mn_ctrl.nAvg)*M_TO_H(calculation_step_length);
							cumsum.runoff = 0.;
						}
					}

					// Erosion mass rate in kg m-2 h-1
					surfFluxes.mass[SurfaceFluxes::MS_WIND] = cumsum.erosion[slope.sector];
					surfFluxes.mass[SurfaceFluxes::MS_WIND] /= static_cast<double>(mn_ctrl.nAvg)*M_TO_H(calculation_step_length);

					// Dump
					const size_t i_hz = (mn_ctrl.HzStep > 0) ? mn_ctrl.HzStep - 1 : 0;
					size_t i_hz0 = (mn_ctrl.HzStep > 1) ? mn_ctrl.HzStep - 2 : 0;
					if (slope.mainStationDriftIndex)
						i_hz0 = i_hz;
					const double wind_trans24 = (slope.sector == slope.mainStation) ? qr_Hdata.at(i_hz0).wind_trans24 : qr_Hdata.at(i_hz).wind_trans24;
					snowpackio.writeTimeSeries(vecXdata[slope.sector], surfFluxes, Mdata,
					                           qr_Hdata.at(i_hz), wind_trans24);

					if (avgsum_time_series) {
						surfFluxes.reset(cumsum_mass);
						if (useCanopyModel) vecXdata[slope.sector].Cdata.reset(cumsum_mass);
					}
					surfFluxes.cRho_hn = Constants::undefined;
					surfFluxes.mRho_hn = Constants::undefined;
					// reset cumulative variables
					if (slope_sequence == slope.nSlopes-1) {
						cumsum.erosion.assign(cumsum.erosion.size(), 0.);
						cumsum.rain = cumsum.snow = 0.;
						mn_ctrl.nAvg = 0;
					}
				}

				// SNOW PROFILES ...
				// ... for visualization (*.pro), etc. (*.prf)
				if (profwrite && mn_ctrl.PrDump)
					snowpackio.writeProfile(current_date, vecXdata[slope.sector]);

				// ... backup Xdata (*.sno<JulianDate>)
				if (mn_ctrl.XdataDump) {
					std::stringstream ss;
					ss << "" << vecStationIDs[i_stn];
					if (slope.sector != slope.mainStation) ss << "" << slope.sector;
					snowpackio.writeSnowCover(current_date, vecXdata[slope.sector], sn_Zdata, true);
					prn_msg(__FILE__, __LINE__, "msg", current_date,
					        "Backup Xdata dumped for station %s [%.2f days, step %d]", ss.str().c_str(),
					        (current_date.getJulian()
					            - (vecSSdata[slope.mainStation].profileDate.getJulian() + 0.5/24)),
					        mn_ctrl.nStep);
				}

				// check mass balance if AVGSUM_TIME_SERIES is not set (screen output only)
				if (!avgsum_time_series) {
					const bool mass_balance = cfg.get("MASS_BALANCE", "SnowpackAdvanced");
					if (mass_balance) {
						if (massBalanceCheck(vecXdata[slope.sector], surfFluxes, tot_mass_in) == false)
							prn_msg(__FILE__, __LINE__, "msg+", current_date, "Mass error at end of time step!");
					}
				}
			} //end loop on slopes
			computed_one_timestep = true;
		} while ((dateEnd.getJulian() - current_date.getJulian()) > calculation_step_length/(2.*1440));
		//end loop on timesteps

		// If the simulation run for at least one time step,
		//   dump the PROFILEs (Xdata) for every station referred to as sector where sector 0 corresponds to the main station
		if (computed_one_timestep) {
			for (size_t sector=slope.mainStation; sector<slope.nSlopes; sector++) {
				if ((mode == "OPERATIONAL") && (sector == slope.mainStation)) {
					// Operational mode ONLY: dump snow depth discrepancy time counter
					vecXdata[slope.mainStation].TimeCountDeltaHS = time_count_deltaHS;
				}
				snowpackio.writeSnowCover(current_date, vecXdata[sector], sn_Zdata);
				if (sector == slope.mainStation) {
					prn_msg(__FILE__, __LINE__, "msg", mio::Date(),
					        "Writing data to sno file(s) for %s (station %s) on %s",
					        vecSSdata[slope.mainStation].meta.getStationName().c_str(),
					        vecStationIDs[i_stn].c_str(), current_date.toString(mio::Date::ISO).c_str());
				}
			}
			// Dump time series to snowpack.ams_pmod@SDBx (hazard data)
			if (mn_ctrl.sdbDump) {
				mio::Timer sdbDump_timer;
				sdbDump_timer.reset();
				sdbDump_timer.start();
				if (snowpackio.writeHazardData(vecStationIDs[i_stn], qr_Hdata, qr_Hdata_ind, mn_ctrl.HzStep)) {
					sdbDump_timer.stop();
					prn_msg(__FILE__, __LINE__, "msg-", mio::Date(),
					        "Finished writing Hdata to SDB for station %s on %s (%lf s)",
					        vecStationIDs[i_stn].c_str(), current_date.toString(mio::Date::ISO).c_str(), sdbDump_timer.getElapsed());
				}
			}
		}
		prn_msg(__FILE__, __LINE__, "msg-", mio::Date(), "Total time to read meteo data : %lf s",
		        meteoRead_timer.getElapsed());
		prn_msg(__FILE__, __LINE__, "msg-", mio::Date(), "Runtime for station %s: %lf s",
		        vecStationIDs[i_stn].c_str(), run_timer.getElapsed());
	}

	time_t nowEND=time(NULL);
	cout << endl;
	cout << "[i] []                 STARTED  running SLF " << mode << " Snowpack Model on " << ctime(&nowSRT);
	if (mode == "OPERATIONAL"){
		cout << "                       ===========================================================================" << endl;
	} else {
		cout << "                       ========================================================================" << endl;
	}
	cout << "                       FINISHED running SLF " << mode << " Snowpack Model on " << ctime(&nowEND) << endl;
}

int main(int argc, char *argv[]) {
	try {
		real_main(argc, argv);
	} catch (const std::exception &e) {
		std::cerr << e.what() << endl;
		throw;
	}

	return EXIT_SUCCESS;
}
