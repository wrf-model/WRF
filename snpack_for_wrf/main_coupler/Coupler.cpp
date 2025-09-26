#include "Coupler.hpp"
#include <iostream>
#include <snowpack/libsnowpack.h>
#include <meteoio/MeteoIO.h>
#include <getopt.h>
#include <assert.h>

using namespace std;
using namespace mio;

Slope::Slope(const mio::Config& cfg)
       : prevailing_wind_dir(0.), nSlopes(0), mainStation(0), sector(0),
         first(1), luv(0), lee(0),
         north(false), south(false),
         snow_erosion(false), mainStationDriftIndex(false),
         snow_redistribution(false), luvDriftIndex(false),
         sector_width(0)
{
	cfg.getValue("NUMBER_SLOPES", "SnowpackAdvanced", nSlopes);
	cfg.getValue("SNOW_EROSION", "SnowpackAdvanced", snow_erosion);
	stringstream ss;
	ss << nSlopes;
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
void Slope::setSlope(const unsigned int slope_sequence, SnowStation& vecXdata, double& wind_dir)
{
	mainStationDriftIndex = false;
	luvDriftIndex = false;
	switch (slope_sequence) {
	case 0:
		for (size_t kk=0; kk<nSlopes; kk++) {
			vecXdata.windward = false;
			vecXdata.rho_hn   = 0.;
			vecXdata.hn       = 0.;
		}
		if (nSlopes > 1) {
			luv = getSectorDir(wind_dir - prevailing_wind_dir);
			vecXdata.windward = true;
			lee = (luv + nSlopes/2) % (nSlopes-1);
			if (lee == 0) lee = nSlopes - 1;
		} else {
			//requesting slope 0 of 0 expositions
			luv = lee = 0;
		}
		sector = mainStation;
		mainStationDriftIndex = ((nSlopes == 1) && snow_erosion);
		break;
	case 1:
		sector = luv;
		luvDriftIndex = snow_redistribution;
		break;
	default:
		sector++;
		if (sector == nSlopes) sector = 1;
	}
	north = (vecXdata.meta.getSlopeAngle() > 0. && vecXdata.meta.getAzimuth() == 0.);
	south = (vecXdata.meta.getSlopeAngle() > 0. && vecXdata.meta.getAzimuth() == 180.);
}

Cumsum::Cumsum(const unsigned int nSlopes)
        : precip(0.),
          drift(0.), snow(0.), runoff(0.), rain(0.),
          erosion(nSlopes, 0.)
{}

void editMeteoData(mio::MeteoData& md, const string& variant, const double& thresh_rain)
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
bool validMeteoData(const mio::MeteoData& md, const string& StationName, const string& variant, const bool& enforce_snow_height, const bool& advective_heat, const bool& soil_flux, const unsigned int& nslopes)
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

void copyMeteoData(const mio::MeteoData& md, CurrentMeteo& Mdata,
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

        Mdata.p = md(MeteoData::P); 
}

double getHS_last3hours(mio::IOManager &io, const mio::Date& current_date)
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
void setShortWave(CurrentMeteo& Mdata, const SnowStation& Xdata, const bool& iswr_is_net)
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
void dataForCurrentTimeStep(CurrentMeteo& Mdata, SurfaceFluxes& surfFluxes, SnowStation &vecXdata,
                            const Slope& slope, SnowpackConfig& cfg,
//                            SunObject &sun,
                            double& precip, const double& lw_in, const double hs_a3hl6,
                            double& tot_mass_in,
                            const std::string& variant)
{
	SnowStation &currentSector = vecXdata; //alias: the current station
	const bool isMainStation = (slope.sector == slope.mainStation);
	const bool useCanopyModel = cfg.get("CANOPY", "Snowpack");
	const bool perp_to_slope = cfg.get("PERP_TO_SLOPE", "SnowpackAdvanced");
	const bool iswr_is_net = cfg.get("ISWR_IS_NET", "Input");
	if (Mdata.tss == mio::IOUtils::nodata) {
		cfg.addKey("MEAS_TSS", "Snowpack", "false");
	}

	Meteo meteo(cfg);

	// Reset surfFluxes.drift and surfFluxes.mass[MS_WIND] anyway since you use these to transport eroded snow
	surfFluxes.drift = 0.;
	surfFluxes.mass[SurfaceFluxes::MS_WIND] = 0.;

	std::string sw_mode = cfg.get("SW_MODE", "Snowpack"); //it must be after calling compRadiation!

	// Find the Wind Profile Parameters, w/ or w/o canopy; take care of canopy
	meteo.compMeteo(Mdata, currentSector, false);
}

/**
 * @brief determine which outputs need to be done for the current time step
 * @param mn_ctrl timestep control structure
 * @param step current time integration step
 * @param sno_step current step in the sno files (current sno profile)
 */

void getOutputControl(MainControl& mn_ctrl, const mio::Date& step, const mio::Date& sno_step,
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
		mn_ctrl.TsDump = false;
		mn_ctrl.PrDump = false;
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
                   Slope& slope, mio::Date &current_date, SN_SNOWSOIL_DATA &vecSSdata,
                   SnowStation &vecXdata, ZwischenData &sn_Zdata, CurrentMeteo& Mdata,
                   double &wind_scaling_factor, double &time_count_deltaHS, double &Lat, double &Lon, double &Altitude, double &sn_tsk, int &proc_id, vector<string> vecStationIDs)
{
	string snowfile;
	stringstream ss;
	ss << "SNOWFILE" << i_stn+1;
	cfg.getValue(ss.str(), "Input", snowfile, mio::IOUtils::nothrow);

                size_t sector = 0;
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
 
				snowpackio.readSnowCover(snowfile, vecStationIDs[i_stn], vecSSdata, sn_Zdata, (vecXdata.Seaice!=NULL));
                                vecSSdata.meta.position.setLatLon((double) 40.5,(double) 40.5,(double) 4000.0);
                                 
				prn_msg(__FILE__, __LINE__, "msg-", mio::Date(), "Reading snow cover data for station %s",
				        vecStationIDs[i_stn].c_str());
				// NOTE (Is it a HACK?) Reading station meta data provided in meteo data and prebuffering those data
				vector<mio::MeteoData> vectmpmd;
				if (current_date.isUndef()) //either force the start date or take it from the sno file
                                  { current_date = Date::rnd(vecSSdata.profileDate, 1);
                                      }
				else
				{	
                                    }
				io.getMeteoData(Date::rnd(vecSSdata.profileDate,1), vectmpmd);
                                vecSSdata.profileDate = current_date;
				if (vectmpmd.empty())
					throw mio::IOException("No data found for station " + vecStationIDs[i_stn] + " on "
					                       + current_date.toString(mio::Date::ISO), AT);
				Mdata.setMeasTempParameters(vectmpmd[i_stn]);
				vecSSdata.meta = mio::StationData::merge(vectmpmd[i_stn].meta,
				                                vecSSdata.meta);
			} else {
				std::stringstream sec_snowfile;
				sec_snowfile << snowfile << sector;
				ss.str("");
				ss << vecSSdata.meta.getStationID() << sector;
				snowpackio.readSnowCover(sec_snowfile.str(), ss.str(), vecSSdata, sn_Zdata, (vecXdata.Seaice!=NULL));
				vecSSdata.meta.position = vecSSdata.meta.getPosition();
				vecSSdata.meta.stationName = vecSSdata.meta.getStationName();
				if (!current_date.isUndef()) vecSSdata.profileDate = current_date; //this should have been set when processing the main station
			}
                                vecSSdata.meta.position.setLatLon(Lat,Lon,Altitude);

			        vecXdata.initialize(vecSSdata,0); // Generate the corresponding Xdata
		} catch (const exception& e) {
			if (sector == slope.first) {
				//prn_msg(__FILE__, __LINE__, "msg-", mio::Date(),
				//        "No virtual slopes! Computation for main station %s only!", vecStationIDs[i_stn].c_str());
				slope.nSlopes = 1;
			} else {
				cout << e.what();
				throw;
			}
		}


	return true;
}

void addSpecialKeys(SnowpackConfig &cfg)
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

void writeForcing(Date d1, const Date& d2, const double& Tstep, IOManager &io)
{
	std::vector< std::vector<MeteoData> > vecMeteo;
	//prn_msg(__FILE__, __LINE__, "msg",  mio::Date(), "Reading and writing out forcing data...");

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

	//prn_msg(__FILE__, __LINE__, "msg",  mio::Date(), "Forcing data written out");
}

void printStartInfo(const SnowpackConfig& cfg, const std::string& name)
{
	const bool useSoilLayers = cfg.get("SNP_SOIL", "Snowpack");
	if (useSoilLayers) {
		bool soil_flux = false;
		cfg.getValue("SOIL_FLUX", "Snowpack", soil_flux);
		//prn_msg(__FILE__, __LINE__, "msg",  mio::Date(), "Start SNOWPACK w/ soil layers in %s mode", mode.c_str());
	} else {
		//prn_msg(__FILE__, __LINE__, "msg",  mio::Date(), "Start SNOWPACK in %s mode", mode.c_str());
	}

	const std::string variant = cfg.get("VARIANT", "SnowpackAdvanced");
	if (variant != "DEFAULT") {
		//prn_msg(__FILE__, __LINE__, "msg",  mio::Date(), "Variant is '%s'", variant.c_str());
	}
	//prn_msg(__FILE__, __LINE__, "msg-", mio::Date(),
	//        "%s compiled on %s at %s", name.c_str(), __DATE__, __TIME__);

	if (mode != "OPERATIONAL") {
		const std::string experiment = cfg.get("EXPERIMENT", "Output");
		const std::string outpath = cfg.get("METEOPATH", "Output");
		//prn_msg(__FILE__, __LINE__, "msg-", mio::Date(), "Experiment : %s", experiment.c_str());
		//prn_msg(__FILE__, __LINE__, "msg-", mio::Date(), "Output dir : %s", outpath.c_str());
	}
}

SnowpackInterface::SnowpackInterface()
                   : cfg(cfgfile),snowpackio(cfg),io(cfg), vecSSdata(), vecXdata(false,false,false), Mdata(cfg),
                     surfFluxes(),sn_Bdata(),slope(cfg),cumsum(slope.nSlopes),current_date(dateBegin)
{
   //snowpack = new Snowpack(cfg);
 }

int SnowpackInterface::init_sn(int snpack_layers_to_save,double Lat,double Lon,double Altitude,double sn_tsk,double in_calc_step_length, int f_counter, int grid_id, int I, int J,
                               int yr, int month, int day, int hour, int minute,int snpack_nlayers, double* arr_T,double* arr_thick,double* arr_volI,double* arr_volW, 
			       double* arr_volV,double* arr_rg,double* arr_rb,double* arr_dd,double* arr_sp,double* arr_cdot,double* arr_meta,
			       double* arr_depd,double* arr_graintype, double* arr_mk, bool start_from_file, double wrf_rho, double& SNOWH, 
                               double& SNOW, double& snpack_dt, double& snpack_write_dt) 
{

   //Constants::density_air = wrf_rho;

   compute_counter = int(snpack_dt       / (in_calc_step_length * double(60.0))) ; 
   write_counter   = int(snpack_write_dt / (in_calc_step_length * double(60.0))) ; 

   loc_snpack_lay_to_sav = snpack_layers_to_save ; 

   const bool prn_check = false;
   run_timer.start();
   time_t nowSRT = time(NULL);

   string loc_cfg;

   int proc_id = grid_id;

   stringstream kkk;
   kkk << proc_id;

   loc_cfg = cfgfile + kkk.str();

   cfg.deleteKey("CALCULATION_STEP_LENGTH","Snowpack");
   std::stringstream ss_h;
   ss_h << in_calc_step_length*double(compute_counter);

   std::cout << "CALC_LENGTH:\t" << in_calc_step_length * double(compute_counter) << std::endl;
   cfg.addKey("CALCULATION_STEP_LENGTH","Snowpack",ss_h.str());
 
   string loc_ss_station;
   stringstream loc_a;
   stringstream loc_b;
   stringstream loc_g;
   //loc_a << grid_id; //f_counter;
   //loc_b << f_counter;
   loc_g << grid_id;
   loc_a << I; 
   loc_b << J;
   loc_ss_station = "snpack_" + loc_g.str() + "_" + loc_a.str() + "_" + loc_b.str();

   cfg.deleteKey("STATION1","Input");
   std::stringstream ss_station;
   ss_station << loc_ss_station; 
   cfg.addKey("STATION1","Input",ss_station.str());

   cfg.deleteKey("SNOWFILE1","Input");
   cfg.addKey("SNOWFILE1","Input",ss_station.str());

   i_time_zone = cfg.get("TIME_ZONE", "Input"); //get user provided input time_zone

   stringstream start_data; 

   start_data << yr << "-" << month << "-" << day << "T" << hour << ":" << minute ;

   mio::IOUtils::convertString(dateBegin, start_data.str() , i_time_zone);
   mio::IOUtils::convertString(dateEnd  , "2100-02-15T00:00" , i_time_zone);

   current_date = dateBegin;

        const std::string l_variant            = cfg.get("VARIANT", "SnowpackAdvanced"); variant = l_variant;
        const std::string l_experiment         = cfg.get("EXPERIMENT", "Output"); experiment = l_experiment;
        const std::string l_outpath            = cfg.get("METEOPATH", "Output"); outpath = l_outpath;
        const bool l_useSoilLayers             = cfg.get("SNP_SOIL", "Snowpack"); useSoilLayers = l_useSoilLayers;
        const bool l_useCanopyModel            = cfg.get("CANOPY", "Snowpack"); useCanopyModel = l_useCanopyModel;
        const double l_calculation_step_length = cfg.get("CALCULATION_STEP_LENGTH", "Snowpack"); calculation_step_length = l_calculation_step_length;
        const double l_sn_dt                   = M_TO_S(calculation_step_length);

        nSolutes = Constants::iundefined;
        cfg.getValue("NUMBER_OF_SOLUTES", "Input", nSolutes, mio::IOUtils::nothrow);
        if (nSolutes > 0) SnowStation::number_of_solutes = static_cast<short unsigned int>(nSolutes);

        //Interval between profile backups (*.sno\<JulianDate\>) (d)
        backup_days_between = 400.;
        cfg.getValue("BACKUP_DAYS_BETWEEN", "Output", backup_days_between, mio::IOUtils::nothrow);
        //First additional profile backup (*.sno\<JulianDate\>) since start of simulation (d)
        first_backup = 0.;
        cfg.getValue("FIRST_BACKUP", "Output", first_backup, mio::IOUtils::nothrow);

        const bool l_snowPrep = cfg.get("SNOW_PREPARATION", "SnowpackAdvanced"); snowPrep = l_snowPrep;
        const bool l_classify_profile = cfg.get("CLASSIFY_PROFILE", "Output"); classify_profile = l_classify_profile;
        const bool l_profwrite = cfg.get("PROF_WRITE", "Output"); profwrite = l_profwrite;
        const double l_profstart = cfg.get("PROF_START", "Output"); profstart = l_profstart;
        const double l_profdaysbetween = cfg.get("PROF_DAYS_BETWEEN", "Output"); profdaysbetween = l_profdaysbetween;
        const bool l_tswrite = cfg.get("TS_WRITE", "Output"); tswrite = l_tswrite;
        const double l_tsstart = cfg.get("TS_START", "Output"); tsstart = l_tsstart;
        const double l_tsdaysbetween = cfg.get("TS_DAYS_BETWEEN", "Output"); tsdaysbetween = l_tsdaysbetween;

        const bool l_precip_rates = cfg.get("PRECIP_RATES", "Output"); precip_rates = l_precip_rates;
        const bool l_avgsum_time_series = cfg.get("AVGSUM_TIME_SERIES", "Output"); avgsum_time_series = l_avgsum_time_series;
        const bool l_cumsum_mass = cfg.get("CUMSUM_MASS", "Output"); cumsum_mass = l_cumsum_mass;
        const double l_thresh_rain = cfg.get("THRESH_RAIN", "SnowpackAdvanced"); thresh_rain = l_thresh_rain; 
        const bool l_advective_heat = cfg.get("ADVECTIVE_HEAT", "SnowpackAdvanced"); advective_heat = l_advective_heat;
        const bool l_soil_flux = (useSoilLayers || variant == "SEAICE") ? cfg.get("SOIL_FLUX", "Snowpack") : false;
                   soil_flux = l_soil_flux;

        io.setMinBufferRequirements(IOUtils::nodata, 50.0); //we require the buffer to contain at least 1.1 day before the current point

        if (vecStationIDs.empty()) { //research use case: stationIDs provided by the available input files
                vector<StationData> accessible_stations;
                io.getStationData(dateEnd, accessible_stations); //we are retrieving meta information from MeteoIO
                for (size_t ii=0; ii<accessible_stations.size(); ii++) {
                     vecStationIDs.push_back(ss_station.str());
                }
        }


 // START LOOP OVER ALL STATIONS
        bool write_forcing = cfg.get("WRITE_PROCESSED_METEO", "Output"); //it will be set to false once it has been done
            size_t i_stn = 0;

                run_timer.reset();
                meteoRead_timer.reset();

                double lw_in = Constants::undefined;    // Storage for LWin from flat field energy balance

                // Used to scale wind for blowing and drifting snowpack (from statistical analysis)
                wind_scaling_factor = cfg.get("WIND_SCALING_FACTOR", "SnowpackAdvanced");

                // Control of time window: used for adapting diverging snow depth in operational mode
                time_count_deltaHS = 0.;

                meteoRead_timer.start();


		if (start_from_file) {
                const bool read_slope_status = readSlopeMeta(io, snowpackio, cfg, i_stn, slope, current_date, vecSSdata, vecXdata, sn_Zdata, Mdata, wind_scaling_factor, time_count_deltaHS, Lat, Lon, Altitude, sn_tsk, proc_id,vecStationIDs);
		} else {
		  vecSSdata.profileDate = current_date;	
		  int nLayers = std::min(snpack_nlayers,loc_snpack_lay_to_sav);

		  /*Check if the first layer to be read is -999 (everywhere - but only checked for the arr_thick layer); if yes, check second level, etc. This is to avoid having layers filled with -999 when initializing a domain, which would not pass the tests performed in vecXdata.initialize(vecSSdata,0); - -999 filled layer can pop up randomly, reason not yet clear. This should be cleaned at some point, FG, November 2022.*/

		      for (int lay = nLayers-1; lay >= 0; lay--){
	                     if (arr_thick[lay] == -999){
        	               std::cout << "nlayers :" << nLayers << std::endl;
                	       std::cout << "arr_thick= -999 at " << lay << std::endl;
                               nLayers = lay;
                               std::cout << arr_thick[lay] << std::endl;
			     } else {
			       break;
			     } 
		      }

		  vecSSdata.nLayers = nLayers;	  
                  if(vecSSdata.nLayers > 0) vecSSdata.Ldata.resize(vecSSdata.nLayers, LayerData());
                  for(size_t ll=0; ll<vecSSdata.nLayers; ll++){
	              vecSSdata.Ldata[ll].depositionDate    = mio::Date(arr_depd[nLayers-1-ll],i_time_zone); 
                      vecSSdata.Ldata[ll].hl                = arr_thick[nLayers-1-ll]; 
                      vecSSdata.Ldata[ll].tl                = arr_T[nLayers-1-ll]; 
                      vecSSdata.Ldata[ll].phiIce            = arr_volI[nLayers-1-ll]; 
                      vecSSdata.Ldata[ll].phiWater          = arr_volW[nLayers-1-ll]; 
                      vecSSdata.Ldata[ll].phiWaterPref      = 0.;
		      vecSSdata.Ldata[ll].phiVoids = arr_volV[nLayers-1-ll]; 
                      vecSSdata.Ldata[ll].phiSoil  = 1.0 - (arr_volV[nLayers-1-ll] + arr_volI[nLayers-1-ll] + arr_volW[nLayers-1-ll]) ;
                      if(vecSSdata.Ldata[ll].phiSoil < 1.0e-3) vecSSdata.Ldata[ll].phiSoil = 0.0;   
                      vecSSdata.Ldata[ll].SoilRho  = 2400.00;
                      vecSSdata.Ldata[ll].SoilK    = 2.0; 
                      vecSSdata.Ldata[ll].SoilC    = 2000.00; 
                      vecSSdata.Ldata[ll].rg       = arr_rg[nLayers-1-ll]; 
                      vecSSdata.Ldata[ll].rb       = arr_rb[nLayers-1-ll]; 
                      vecSSdata.Ldata[ll].dd       = arr_dd[nLayers-1-ll]; 
                      vecSSdata.Ldata[ll].sp       = arr_sp[nLayers-1-ll]; 
                      vecSSdata.Ldata[ll].CDot     = arr_cdot[nLayers-1-ll]; 
                      vecSSdata.Ldata[ll].metamo   = arr_meta[nLayers-1-ll];
                      vecSSdata.Ldata[ll].mk       = (unsigned short int) arr_mk[nLayers-1-ll];
                      vecSSdata.Ldata[ll].ne       = 1;
		      //if( (I==1) && (J==31)) {
                      std::cout << "INIT SNPACK:\t" << I << "," << J << "," << vecSSdata.nLayers <<"," << vecSSdata.Ldata[ll].hl  << ","
                                << vecSSdata.Ldata[ll].tl     << ","
                                << vecSSdata.Ldata[ll].phiIce   << ","
                                << vecSSdata.Ldata[ll].phiVoids   << "," 
                                << vecSSdata.Ldata[ll].phiWater << ","
                                << vecSSdata.Ldata[ll].phiSoil << "," << vecSSdata.Ldata[ll].phiWaterPref << 
                               std::endl; //}

		  }// Loop over layers
                           vecSSdata.SoilAlb = 0.2;
                           vecSSdata.BareSoil_z0 = 0.2;
                           vecSSdata.Canopy_Height = 0.0;
                           vecSSdata.nN = 1;
                           vecSSdata.Height = 0.;
                           for (size_t ll = 0; ll < vecSSdata.nLayers; ll++) {
                                  vecSSdata.nN    += vecSSdata.Ldata[ll].ne;
                                  vecSSdata.Height += vecSSdata.Ldata[ll].hl; 
			              }
                  vecSSdata.meta.position.setLatLon(Lat,Lon,Altitude);
		  vecXdata.initialize(vecSSdata,0);
		} // if..else for if starting with file data (*.sno) or WRF data
		
		meteoRead_timer.stop();

                memset(&mn_ctrl, 0, sizeof(MainControl));
                if (mode == "RESEARCH") {
                        mn_ctrl.resFirstDump = true; //HACK to dump the initial state in research mode
                        current_date -= calculation_step_length/(24.*60.);
                }

                const double l_duration = (dateEnd.getJulian() - current_date.getJulian() + 0.5/24)*24*3600; //HACK: why is it computed this way?
                duration = l_duration ; 

                computed_one_timestep = false;
                meteo_step_length = l_sn_dt;

                std::stringstream ss_l;
                ss_l << meteo_step_length;
                cfg.addKey("METEO_STEP_LENGTH","Snowpack",ss_l.str());
                const double l_trial_meteo_length = cfg.get("METEO_STEP_LENGTH","Snowpack");
 

                const bool l_enforce_snow_height = cfg.get("ENFORCE_MEASURED_SNOW_HEIGHTS", "Snowpack");
                enforce_snow_height = l_enforce_snow_height;
                //from current_date to dateEnd, if necessary write out meteo forcing
                if (write_forcing==true) {
                        writeForcing(current_date, dateEnd, calculation_step_length/1440, io);
                        write_forcing = false; //no need to call it again for the other stations
                }

             vecXdata.meta.stationID = ss_station.str();

             counter = 0;

             vecXdata.meta.position.setGridIndex(I,J,(int)0);

std::cout << "I,J,H:\t" << I << " , " << J << " , " << write_counter << " , " << compute_counter << std::endl;
SNOWH = vecXdata.cH - vecXdata.Ground ;
SNOW = vecXdata.swe;

cumsum.precip = 0.0;
Mdata.liq_psum = 0.0;
Mdata.solid_psum = 0.0;

snowpack = new Snowpack(cfg);
return 0;
}
 
int SnowpackInterface::nextStep(int xxx,double h_of_met_vals, double l_TA, double l_RH, double l_VW, double l_VW_MAX, double& l_DW, double& in_drift,
                                double& in_conc, double& in_csalt, double& in_q_lb, double& in_N_lb,
                                double l_iswr, double l_ilwr, double l_psum, double& TSK, double& HFX, double& QFX, double& ALBEDO, 
                                double& SNOWC, double& SNOW, double& SNOWH, double& l_in_q_corr, double& l_in_N_corr, double log_z_z0, 
                                double in_QI,double in_QNI,double& ust_wrf,int f_counter, int grid_id, int I, int J, double PSFC, 
                                double& m_budg_precip,double& m_budg_erosion,double& m_budg_sublim,double& m_budg_deposit,double& m_budg_swe,
                                double& m_budg_melt,double& m_budg_refreeze,
                                double& e_budg_ilwr_in,double& e_budg_ilwr_out,double& e_budg_sw_in, double& e_budg_sw_out,double& e_budg_sensible, 
                                double& e_budg_latent, double& e_budg_lower_bc, double& e_budg_raine, double& e_budg_totale,double* arr_T,
			        double* arr_thick,double* arr_volI,double* arr_volW, double* arr_volV,double* arr_rg,double* arr_rb,double* arr_dd,
				double* arr_sp,double* arr_cdot,double* arr_meta,double* arr_depd,double* arr_graintype,double* arr_mk,
                                bool bs_bool, int& sn_nlayer, double wrf_rho, 
                                double& bs_bdg_total,double& qi_in,double& qni_in,double& bs_K,double& bs_mass_turb,double& bs_number_turb,
                                double& in_hsalt, double& psi_s, double loc_sza, double tau_qc, double tau_qi, 
                                double tau_qc_tot, double tau_qi_tot )
{

    vecXdata.meltMassTot = 0.0;
    vecXdata.refreezeMassTot = 0.0;

    double lat_erosion;
    double lat_deposition;

    lat_erosion = 0.0;
    lat_deposition = 0.0;
    double conc_down = 0.0;

    counter++;
    Mdata.sim_counter = counter;

    Mdata.density_air = wrf_rho ;
    vecXdata.density_air = wrf_rho;
       
    double delta_drift_psum = 0.0 ;

    surfFluxes.drift = 0.0 ; // drift;
    in_drift         = 0.0 ; // drift;
    in_csalt         = 0.0 ; // ustar_thresh;
    in_q_lb          = 0.0 ; //q_lb_cond;
    in_N_lb          = 0.0 ; // n_lb_cond;
    in_hsalt         = 0.0 ; // h_salt;

    cfg.deleteKey("HEIGHT_OF_WIND_VALUE","Snowpack");
    cfg.deleteKey("HEIGHT_OF_METEO_VALUES","Snowpack");
    std::stringstream ss_h;
    ss_h << h_of_met_vals;
    
    cfg.addKey("HEIGHT_OF_WIND_VALUE","Snowpack",ss_h.str());
    cfg.addKey("HEIGHT_OF_METEO_VALUES","Snowpack",ss_h.str());
    
    
    size_t i_stn = 0;
    current_date += (calculation_step_length/double(compute_counter)  )/1440;

    mn_ctrl.nStep++;
    mn_ctrl.nAvg++;
    
    mio::MeteoData vecMyMeteo(current_date,vecSSdata.meta);
    meteoRead_timer.start();

    TA_avg   = TA_avg   + l_TA    ;
    RH_avg   = RH_avg   + l_RH    ;
    ISWR_avg = ISWR_avg + l_iswr  ;
    ILWR_avg = ILWR_avg + l_ilwr  ;
    PSFC_avg = PSFC_avg + PSFC    ;
    PSUM_avg = PSUM_avg + l_psum  ;
    VW_avg   = VW_avg   + l_VW    ;

    double d_compute_counter = double(compute_counter);

    if( (counter % compute_counter) == 0) {

        TA_final   = TA_avg   / d_compute_counter ;
        RH_final   = RH_avg   / d_compute_counter ;
        ISWR_final = ISWR_avg / d_compute_counter ;
        ILWR_final = ILWR_avg / d_compute_counter ;
        PSFC_final = PSFC_avg / d_compute_counter ;
        PSUM_final = PSUM_avg / d_compute_counter ;
        VW_final   = VW_avg   / d_compute_counter ;

        TA_avg   = 0.0 ;
        RH_avg   = 0.0 ;
        ISWR_avg = 0.0 ;
        ILWR_avg = 0.0 ;
        PSFC_avg = 0.0 ;
        PSUM_avg = 0.0 ;
        VW_avg   = 0.0 ;

      }

    
    vecMyMeteo(MeteoData::TA)     = l_TA; //TA_final; //l_TA;
    vecMyMeteo(MeteoData::RH)     = l_RH; //RH_final; //l_RH;
    vecMyMeteo(MeteoData::VW)     = l_VW;
    vecMyMeteo(MeteoData::ISWR)   = l_iswr; //ISWR_final; //l_iswr;
    vecMyMeteo(MeteoData::ILWR)   = l_ilwr; //ILWR_final; //l_ilwr;
    vecMyMeteo(MeteoData::PSUM)   = l_psum; // PSUM_final;
    vecMyMeteo(MeteoData::VW_MAX) = l_VW_MAX;
    vecMyMeteo(MeteoData::DW)     = l_DW;
    vecMyMeteo(MeteoData::TSG)    = 400.00;
    vecMyMeteo(MeteoData::P)      = PSFC; // PSFC_final;
    Mdata.ilwr_v = l_ilwr ; 
    double lw_in = l_ilwr ; 
    
    const double l_met_length = cfg.get("METEO_STEP_LENGTH","Snowpack");
    
    meteoRead_timer.stop();
    editMeteoData(vecMyMeteo, variant, thresh_rain);
   
    if ( l_TA >= IOUtils::C_TO_K(thresh_rain) ) 
       { Mdata.liq_psum += l_psum ;
           }
    else 
       { Mdata.solid_psum += l_psum ;
        }
    
    const double hs_a3hl6 = 0.0 ; // getHS_last3hours(io, current_date);

    // START LOOP OVER ASPECTS
    unsigned int slope_sequence=0;
    double tot_mass_in = 0.; // To check mass balance over one CALCULATION_STEP_LENGTH if MASS_BALANCE is set

    //fill Snowpack internal structure with forcing data
    copyMeteoData(vecMyMeteo, Mdata, slope.prevailing_wind_dir, wind_scaling_factor);
    slope.setSlope(slope_sequence, vecXdata, Mdata.dw_drift);
    dataForCurrentTimeStep(Mdata, surfFluxes, vecXdata, slope, cfg,
           cumsum.precip, lw_in, hs_a3hl6,
           tot_mass_in, variant);


   
   // DOING THE SNOW EROSION / DEPOSITION HERE !

   SnowDrift snowdrift(cfg);

   size_t nE = vecXdata.getNumberOfElements();
   size_t nN_c = vecXdata.getNumberOfNodes();

   vector<NodeData>& NDS = vecXdata.Ndata;
   vector<ElementData>& EMS = vecXdata.Edata;
   const double loc_sn_dt = M_TO_S(calculation_step_length) ;

//const bool no_snow = ((nE < vecXdata.SoilNode+1) || (EMS[nE-1].theta[SOIL] > 0.));
   const bool no_snow = ( (nE < vecXdata.SoilNode+1) || (EMS[nE-1].theta[SOIL] > 0.) || (EMS[nE-1].theta[WATER] > 0.5) );
 
   if (no_snow) {
                  vecXdata.ErosionMass = 0.;
                  vecXdata.ErosionLevel = vecXdata.SoilNode;
                  surfFluxes.drift = 0.;
   }
   else {

          const double ustar_max = Mdata.ustar; // [ m s-1] 
          double drift = snowdrift.compMassFlux(EMS[nE-1],ustar_max,vecXdata.meta.getSlopeAngle(),vecXdata.tau_thresh,vecXdata.tau, Mdata.density_air); // [kg m-1 s-1]
          if(!bs_bool) drift = 0.0;
 
          if( (vecXdata.cH - vecXdata.Ground) < 0.5){
           drift = 0.0;
             }        

          if( NDS[nN_c-1].T > 272.15){
             drift = 0.0;
          }

          double ustar_thresh = sqrt(vecXdata.tau_thresh / Mdata.density_air ); // [m s-1]
          double upart = 2.8 * ustar_thresh ; // [ m s-1]
          double conc = drift/upart ; // [kg m-2]

          double lambda_csalt = 0.45; // [-]
          double h_salt = 0.08436 * pow(ustar_max,1.27); //[m]
          double saltation_const = 0.0;
          if (ustar_max > 0.0) {
           saltation_const = (lambda_csalt*9.81/(ustar_max * ustar_max)) ;
             }

          double c_salt = conc * saltation_const * exp( -1.0 * saltation_const * h_salt); // [kg m-3]
          double q_lb_cond = c_salt / Mdata.density_air ; // [kg kg-1] 
          double n_q_factor = (6.0/(3.14 * 0.0002 * 0.0002 * 0.0002 * 918.0))  ; //8.452852671806426e+7 ;
          double n_lb_cond = 0.45 * q_lb_cond * n_q_factor;
          double mass_to_adjust = bs_bdg_total ;

          if(bs_bdg_total > 0) {
                     lat_erosion = 0.0;
                     lat_deposition = bs_bdg_total;
          }
          else {
                     lat_erosion = -1.0*bs_bdg_total;
                     lat_deposition = 0.0 ;
          }

          surfFluxes.drift = drift;
          in_drift         = drift;
          in_csalt         = ustar_thresh;
          in_q_lb          = q_lb_cond;
          in_N_lb          = n_lb_cond;
          in_hsalt         = h_salt;

          double delta_conc;
          delta_conc = -1.0 * mass_to_adjust;

          if(delta_conc > 0.) {
             double massErode = delta_conc  ; 
             unsigned int nErode=0;
             while(massErode > Constants::eps) {
                  if ( (massErode >= 0.95 * EMS[nE-1].M)  ) {
                          nE--;
                          vecXdata.cH -= EMS[nE].L;
                          NDS[nE].hoar = 0.;
                          vecXdata.ErosionMass += EMS[nE].M;
                          vecXdata.ErosionLevel = std::min(nE-1, vecXdata.ErosionLevel);
                          nErode++;
                          massErode -= EMS[nE].M;
                  } else if ( (massErode > Constants::eps) && (massErode < 0.95 * EMS[nE-1].M) ) {
                         if (fabs(EMS[nE-1].L * EMS[nE-1].Rho - EMS[nE-1].M) > 0.001) {
                                  prn_msg(__FILE__, __LINE__, "wrn", Mdata.date, "Inconsistent Mass:%lf   L*Rho:%lf", EMS[nE-1].M,EMS[nE-1].L*EMS[nE-1].Rho);
                                  EMS[nE-1].M = EMS[nE-1].L * EMS[nE-1].Rho;
                                  assert(EMS[nE-1].M>=0.); //mass must be positive
                          }
                          const double dL = -massErode / (EMS[nE-1].Rho);
                          NDS[nE].z += dL;
                          EMS[nE-1].L0 = EMS[nE-1].L = EMS[nE-1].L + dL;
                          vecXdata.cH += dL;
                          NDS[nE].z += NDS[nE].u;
                          NDS[nE].u = 0.0;
                          NDS[nE].hoar = 0.;
                          EMS[nE-1].M -= massErode;
                          assert(EMS[nE-1].M>=0.); //mass must be positive
                          vecXdata.ErosionMass += massErode;
                          massErode = 0.0 ; 

                  } else {
                          vecXdata.ErosionMass = 0.;
                  }
              }
            if (nErode > 0) {
                   vecXdata.resize(nE);
            }
       surfFluxes.mass[SurfaceFluxes::MS_WIND] = -1.0 * delta_conc ;
       conc_down = delta_conc;

          } else {
              double massErode = -1.0 * delta_conc ;
              delta_drift_psum = massErode ; 
              Mdata.solid_psum    += delta_drift_psum;
              cumsum.precip += delta_drift_psum;
              surfFluxes.mass[SurfaceFluxes::MS_DEPOSITION] = -1.0 * delta_conc ;
            
              } 

in_conc  = conc;

         } // end of if statement for no snow


surfFluxes.mass[SurfaceFluxes::MS_HNW] = Mdata.solid_psum;

surfFluxes.reset(cumsum_mass);
cumsum.precip += l_psum;

// NEW EDITS to account 
Mdata.elev = (90.0 - loc_sza) * (3.141628 / 180.0);
Mdata.odc = tau_qc_tot + tau_qi_tot;

                             if( (counter % compute_counter) == 0) {
                                snowpack->runSnowpackModel(Mdata, vecXdata, cumsum.precip, sn_Bdata, surfFluxes);
                                }
                           
                                /***** OUTPUT SECTION *****/
                                surfFluxes.collectSurfaceFluxes(sn_Bdata, vecXdata, Mdata);

             
const vector<NodeData>& NDS_a = vecXdata.Ndata;
const size_t nN = vecXdata.getNumberOfNodes();

/*double final_cold_content = vecXdata.ColdContent;

double v_check_energy = Constants::emissivity_snow*l_ilwr
                        +  (surfFluxes.lw_out * -1.0) 
                        +  (surfFluxes.sw_in)
                        +  (surfFluxes.sw_out * -1.0)
                        +  (surfFluxes.qs)
                        +  (surfFluxes.ql)
                        +  (surfFluxes.qg) // ;
                        +  (surfFluxes.qr)
                        +  (((final_energy - init_energy)/(double)60.0) * -1.0) ;

double h_check_energy = Constants::emissivity_snow*l_ilwr
                        +  (surfFluxes.lw_out * -1.0) 
                        +  (surfFluxes.sw_in)
                        +  (surfFluxes.sw_out * -1.0)
                        +  (surfFluxes.qs)
                        +  (surfFluxes.ql)
                        +  (surfFluxes.qg) // ;
                        +  (surfFluxes.qr)
                        +  ((surfFluxes.dIntEnergy/(double)60.0) * -1.0) ;

double m_check_energy = Constants::emissivity_snow*l_ilwr
                        +  (surfFluxes.lw_out * -1.0) 
                        +  (surfFluxes.sw_in)
                        +  (surfFluxes.sw_out * -1.0)
                        +  (surfFluxes.qs)
                        +  (surfFluxes.ql)
                        +  (surfFluxes.qg) // ;
                        +  (surfFluxes.qr)
                        +  (((final_cold_content-init_cold_content)/(double)60.0) * -1.0) ;

double v_check_mass = init_mass + surfFluxes.mass[SurfaceFluxes::MS_HNW]   
                                + surfFluxes.mass[SurfaceFluxes::MS_WIND] 
                                + surfFluxes.mass[SurfaceFluxes::MS_SUBLIMATION]
                                + (-1.0 * surfFluxes.mass[SurfaceFluxes::MS_SWE]);
*/
   TSK    = NDS_a[nN-1].T;

   HFX    = -1.0*surfFluxes.qs;
   QFX    = -1.0*sn_Bdata.ql; //-1.0*surfFluxes.ql;

   ALBEDO = vecXdata.Albedo;
   SNOWC  = (double) 1.0; 
   SNOW   = vecXdata.swe;
   SNOWH  = vecXdata.cH - vecXdata.Ground;
   ust_wrf = Mdata.ustar;

   psi_s = Mdata.psi_s;
 
   m_budg_precip        = l_psum ; //surfFluxes.mass[SurfaceFluxes::MS_HNW]  ;
   m_budg_erosion       = lat_erosion;  // surfFluxes.mass[SurfaceFluxes::MS_WIND] ;
   m_budg_sublim        = surfFluxes.mass[SurfaceFluxes::MS_SUBLIMATION];
   m_budg_deposit       = lat_deposition; // surfFluxes.mass[SurfaceFluxes::MS_DEPOSITION] ; 
   m_budg_swe           = surfFluxes.mass[SurfaceFluxes::MS_SWE] ;
   m_budg_melt          = vecXdata.meltMassTot;
   m_budg_refreeze      = vecXdata.refreezeMassTot;

   e_budg_ilwr_in       = Constants::emissivity_snow*l_ilwr ;
   e_budg_ilwr_out      = surfFluxes.lw_out ;
   e_budg_sw_in         = surfFluxes.sw_in  ;
   e_budg_sw_out        = surfFluxes.sw_out ;
   e_budg_sensible      = surfFluxes.qs ;
   e_budg_latent        = surfFluxes.ql ;
   e_budg_lower_bc      = surfFluxes.qg ;
   e_budg_raine         = surfFluxes.qr ;
   e_budg_totale        = surfFluxes.dIntEnergy/(loc_sn_dt) ;


   const vector<ElementData>& elem_data = vecXdata.Edata;

   const int get_size = (int)vecXdata.getNumberOfElements();

   const int lim_size = std::max(0,get_size-loc_snpack_lay_to_sav);
  
  if ( (counter % write_counter) == 0) {
     for(int e=get_size-1, tmtm=0; e >=lim_size ; e-- , tmtm++){
//            std::cout << e << " , " << get_size << " , " << lim_size << " , " << tmtm << " , " << I << " , " << J << std::endl;  

	    arr_T[tmtm] = vecXdata.Ndata[e+1].T;

	    arr_thick[tmtm]  = (double) elem_data[e].L;
	    arr_volI[tmtm]   = (double) elem_data[e].theta[ICE] ;
	    arr_volW[tmtm]   = (double) elem_data[e].theta[WATER] ;
	    arr_volV[tmtm]   = (double) elem_data[e].theta[AIR] ;
	    arr_rg[tmtm]     = (double) elem_data[e].rg ; 

            arr_rb[tmtm]   = (double) elem_data[e].rb ;
            arr_dd[tmtm]   = (double) elem_data[e].dd ;
            arr_sp[tmtm]   = (double) elem_data[e].sp ;
            arr_cdot[tmtm] = (double) elem_data[e].CDot   ;
            arr_meta[tmtm] = (double) elem_data[e].metamo ;

	    arr_depd[tmtm] = (double) elem_data[e].depositionDate.getJulian();

            arr_graintype[tmtm] = (double) elem_data[e].type ;
            arr_mk[tmtm] = (double) elem_data[e].mk;


         }   
   }
   sn_nlayer = get_size;

/*   if( (I==108) && (J==177)) {
     std::cout<< "ttt,TSK:\t" << Mdata.sim_counter << "," << TSK << "," << SNOWH <<"," 
                             << l_psum << "," << Mdata.psum_ph << "," << Mdata.psum << "," << cumsum.precip << "," << HFX << "," << QFX << std::endl;

     std::cout << TA_avg << "," << RH_avg << "," <<ISWR_avg << "," << ILWR_avg << "," << PSFC_avg << "," << PSUM_avg << "," << VW_avg << std::endl;
     std::cout << TA_final << "," << RH_final << "," <<ISWR_final << "," << ILWR_final << "," << PSFC_final << "," << PSUM_final << "," << VW_final << std::endl;
     std::cout << l_TA << "," << l_RH << "," << l_iswr << "," << l_ilwr << "," << PSFC << "," << l_psum  << "," << l_VW << std::endl;

     for(int e=get_size-1, tmtm=0; e >=lim_size ; e-- , tmtm++){
//            std::cout << e << " , " << get_size << " , " << lim_size << " , " << tmtm << " , " << I << " , " << J << std::endl;  
            std::cout << e << "," << elem_data[e].L << "," << elem_data[e].theta[ICE] << "," << elem_data[e].theta[AIR] << "," 
                      << elem_data[e].rg << "," << elem_data[e].rb << "," << elem_data[e].theta[SOIL] << std::endl;

	    arr_T[tmtm] = vecXdata.Ndata[e+1].T;

	    arr_thick[tmtm]  = (double) elem_data[e].L;
	    arr_volI[tmtm]   = (double) elem_data[e].theta[ICE] ;
	    arr_volW[tmtm]   = (double) elem_data[e].theta[WATER] ;
	    arr_volV[tmtm]   = (double) elem_data[e].theta[AIR] ;
	    arr_rg[tmtm]     = (double) elem_data[e].rg ; 

            arr_rb[tmtm]   = (double) elem_data[e].rb ;
            arr_dd[tmtm]   = (double) elem_data[e].dd ;
            arr_sp[tmtm]   = (double) elem_data[e].sp ;
            arr_cdot[tmtm] = (double) elem_data[e].CDot   ;
            arr_meta[tmtm] = (double) elem_data[e].metamo ;

	    arr_depd[tmtm] = (double) elem_data[e].depositionDate.getJulian();

         }   


   } */
   return 0;
}

