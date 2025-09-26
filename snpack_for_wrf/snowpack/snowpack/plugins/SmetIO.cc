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

#include <snowpack/plugins/SmetIO.h>
#include <snowpack/Utils.h>
#include <snowpack/snowpackCore/Metamorphism.h>

#define OUTPUT_PRECISION_SNO_FILE 6

using namespace std;
using namespace mio;

/**
 * @page smet SMET
 * @section smet_format Format
 * This plugin reads the SMET files as specified in the
 * <a href="https://models.slf.ch/p/meteoio">MeteoIO</a> pre-processing library documentation (under
 * <i>"Available plugins and usage"</i>, then <i>"smet"</i>).
 *
 * @section fluxes_ts Fluxes timeseries
 * These files are very regular SMET files with a large number of fields.
 *
 * @section layers_data Layers data
 * The snow/soil layers file has the structure described below:
 * - the SMET signature (to identify the file as SMET as well as the format version)
 * - a Header section containing the metadata for the location
 * - a Data section containing description of the layers (if any). Please note that the layers are given from the bottom to the top.
 *
 * The following points are important to remember:
 * - the station_id field is often used to generate output file names
 * - you don't have to provide lat/lon, you can provide easiting/northing instead alongside an epsg code (see MeteoIO documentation)
 * - the ProfileDate will be used as starting date for the simulation. Therefore, make sure you have meteorological data from this point on!
 * - the number of soil and snow layers <b>must</b> be right!
 * - timestamps follow the ISO format, temperatures are given in Kelvin, thicknesses in m, fractional volumes are between 0 and 1 (and the total sum <b>must</b> be exactly one), densities are in kg/m<SUP>3</SUP> (see the definition of the fields in the table below)
 *
 * <center><table border="0">
 * <caption>initial snow profile fields description</caption>
 * <tr><td>
 * <table border="1">
 * <tr><th>Field</th><th>Description</th></tr>
 * <tr><th>timestamp</th><td>ISO formatted time</td></tr>
 * <tr><th>Layer_Thick</th><td>layer thickness [mm]</td></tr>
 * <tr><th>T</th><td>layer temperature [K]</td></tr>
 * <tr><th>Vol_Frac_I</th><td>fractional ice volume [0-1]</td></tr>
 * <tr><th>Vol_Frac_W</th><td>fractional water volume [0-1]</td></tr>
 * <tr><th>Vol_Frac_WP</th><td>fractional preferential water volume [0-1]</td></tr>
 * <tr><th>Vol_Frac_V</th><td>fractional voids volume [0-1]</td></tr>
 * <tr><th>Vol_Frac_S</th><td>fractional soil volume [0-1]</td></tr>
 * <tr><th>Rho_S</th><td>soil density [kg/m3]</td></tr>
 * <tr><th>Conduc_S</th><td>mineral phase soil thermal conductivity [w/(mK)]</td></tr>
 * <tr><th>HeatCapac_S</th><td>mineral phase soil thermal capacity [J/(kg*K)]</td></tr>
 * </table></td><td><table border="1">
 * <tr><th>Field</th><th>Description</th></tr>
 * <tr><th>rg</th><td>grain radius [mm]</td></tr>
 * <tr><th>rb</th><td>bond radius [mm]</td></tr>
 * <tr><th>dd</th><td>dendricity [0-1]</td></tr>
 * <tr><th>sp</th><td>spericity [0-1]</td></tr>
 * <tr><th>mk</th><td>marker, see Metamorphism.cc</td></tr>
 * <tr><th>mass_hoar</th><td>mass of surface hoar []</td></tr>
 * <tr><th>ne</th><td>number of elements</td></tr>
 * <tr><th>CDot</th><td>stress change rate (initialize with 0.)</td></tr>
 * <tr><th>metamo</th><td>currently unused</td></tr>
 * <tr><th> <br></th><td> </td></tr>
 * </table></td></tr>
 * </table></center>
 *
 * Usually, simulations are started at a point in time when no snow is on the ground, therefore not requiring the definition of snow layers. An example is given below with one snow layer (and some comments to explain the different keys in the header):
 * @code
 * SMET 1.1 ASCII
 * [HEADER]
 * station_id       = DAV2
 * station_name     = Davos:Baerentaelli
 * latitude         = 46.701
 * longitude        = 9.82
 * altitude         = 2560
 * nodata           = -999				;code indicating a missing value
 * tz               = 1				;time zone in hours
 * source           = WSL-SLF			;optional key
 * ProfileDate      = 2009-10-01T00:00		;when was the profile made, see explanations above
 * HS_Last          = 0.0000			;last measured snow height
 * SlopeAngle       = 38.0
 * SlopeAzi         = 0.0
 * nSoilLayerData   = 0				;number of soil layers
 * nSnowLayerData   = 1				;number of snow layers
 * SoilAlbedo       = 0.20				;albedo of the exposed soil
 * BareSoil_z0      = 0.020			;roughtness length of the exposed soil
 * CanopyHeight     = 0.00				;height (in m) of the canopy
 * CanopyLeafAreaIndex     = 0.00
 * CanopyDirectThroughfall = 1.00
 * WindScalingFactor       = 1.00			;some stations consistently measure a wind that is too low
 * ErosionLevel     = 0
 * TimeCountDeltaHS = 0.000000
 * fields           = timestamp Layer_Thick  T  Vol_Frac_I  Vol_Frac_W  Vol_Frac_V  Vol_Frac_S Rho_S Conduc_S HeatCapac_S  rg  rb  dd  sp  mk mass_hoar ne CDot metamo
 * [DATA]
 * 2009-09-19T02:30 0.003399 273.15 0.579671 0.068490 0.351839 0.000000 0.0 0.0 0.0 1.432384 1.028390 0.000000 1.000000 22 0.000000 1 0.000000 0.000000
 * @endcode
 *
 * @section hazard_data Hazard data
 * The hazards file contain the temporal history of various parameters that are relevant for avalanche warning (such as three hours new
 * snow fall, etc). If such file is not provided, the internal data structures for such data will be initialized to zero (which is
 * what you usually want when starting a simulation before the start of the snow season). The hazards file has the following structure:
 * @code
 * SMET 1.1 ASCII
 * [HEADER]
 * station_id       = DAV2
 * station_name     = Davos:Baerentaelli
 * latitude         = 46.701
 * longitude        = 9.82
 * altitude         = 2560
 * nodata           = -999
 * tz               = 1
 * ProfileDate      = 2012-06-11T17:30
 * fields           = timestamp SurfaceHoarIndex DriftIndex ThreeHourNewSnow TwentyFourHourNewSnow
 * [DATA]
 * 2010-06-08T18:00       -999       -999   0.000000   0.000000
 * 2010-06-08T18:30       -999       -999   0.000000   0.000000
 * 2010-06-08T19:00       -999       -999   0.000000   0.000000
 * 2010-06-08T19:30       -999       -999   0.000000   0.000000
 * 2010-06-08T20:00       -999       -999   0.000000   0.000000
 * 2010-06-08T20:30       -999       -999   0.000000   0.000000
 * 2010-06-08T21:00       -999       -999   0.000000   0.000000
 * ...
 * 2010-06-11T17:30       -999       -999   0.000000   0.000000
 * @endcode
 *
 * As can be seen in this example, the various indices as well as the snow statistics are given every half an hour in reverse chronological order until
 * the profile date.
 */
SmetIO::SmetIO(const SnowpackConfig& cfg, const RunInfo& run_info)
        : fixedPositions(), outpath(), o_snowpath(), experiment(), inpath(), i_snowpath(), sw_mode(),
          info(run_info), tsWriters(),
          in_dflt_TZ(0.), calculation_step_length(0.), ts_days_between(0.), min_depth_subsurf(0.),
          avgsum_time_series(false), useCanopyModel(false), useSoilLayers(false), research_mode(false), perp_to_slope(false), useReferenceLayer(false),
          out_heat(false), out_lw(false), out_sw(false), out_meteo(false), out_haz(false), out_mass(false), out_t(false),
          out_load(false), out_stab(false), out_canopy(false), out_soileb(false), enable_pref_flow(false)
{
	cfg.getValue("TIME_ZONE", "Input", in_dflt_TZ);
	cfg.getValue("CANOPY", "Snowpack", useCanopyModel);
	cfg.getValue("SNP_SOIL", "Snowpack", useSoilLayers);
	cfg.getValue("SW_MODE", "Snowpack", sw_mode);
	cfg.getValue("MIN_DEPTH_SUBSURF", "SnowpackAdvanced", min_depth_subsurf);
	cfg.getValue("PERP_TO_SLOPE", "SnowpackAdvanced", perp_to_slope);
	cfg.getValue("AVGSUM_TIME_SERIES", "Output", avgsum_time_series, IOUtils::nothrow);
	cfg.getValue("RESEARCH", "SnowpackAdvanced", research_mode);

	cfg.getValue("EXPERIMENT", "Output", experiment);
	cfg.getValue("METEOPATH", "Output", outpath, IOUtils::nothrow);
	cfg.getValue("SNOWPATH", "Output", o_snowpath, IOUtils::nothrow);
	if (o_snowpath.empty()) o_snowpath = outpath;

	cfg.getValue("METEOPATH", "Input", inpath, IOUtils::nothrow);
	cfg.getValue("SNOWPATH", "Input", i_snowpath, IOUtils::nothrow);
	if (i_snowpath.empty()) i_snowpath = inpath;

	cfg.getValue("OUT_CANOPY", "Output", out_canopy);
	cfg.getValue("OUT_HAZ", "Output", out_haz);
	cfg.getValue("OUT_HEAT", "Output", out_heat);
	cfg.getValue("OUT_LOAD", "Output", out_load);
	cfg.getValue("OUT_LW", "Output", out_lw);
	cfg.getValue("OUT_MASS", "Output", out_mass);
	cfg.getValue("OUT_METEO", "Output", out_meteo);
	cfg.getValue("OUT_SOILEB", "Output", out_soileb);
	cfg.getValue("OUT_STAB", "Output", out_stab);
	cfg.getValue("OUT_SW", "Output", out_sw);
	cfg.getValue("OUT_T", "Output", out_t);
	cfg.getValue("USEREFERENCELAYER", "Output", useReferenceLayer, IOUtils::nothrow);
	cfg.getValue("TS_DAYS_BETWEEN", "Output", ts_days_between);
	cfg.getValue("CALCULATION_STEP_LENGTH", "Snowpack", calculation_step_length);
	cfg.getValue("PREF_FLOW", "SnowpackAdvanced", enable_pref_flow);
}

SmetIO::~SmetIO()
{
	for (std::map<std::string, smet::SMETWriter*>::iterator it = tsWriters.begin(); it != tsWriters.end(); ++it){
		if (it->second!=NULL) {
			delete it->second;
			it->second = NULL;
		}
	}
}

SmetIO& SmetIO::operator=(const SmetIO& source) {
	if (this != &source) {
		fixedPositions = source.fixedPositions;
		outpath = source.outpath;
		o_snowpath = source.o_snowpath;
		experiment = source.experiment;
		inpath = source.inpath;
		i_snowpath = source.i_snowpath;
		sw_mode = source.sw_mode;
		//info = source.info;
		tsWriters = std::map<std::string, smet::SMETWriter*>(); //it will have to be re-allocated for thread safety

		in_dflt_TZ = source.in_dflt_TZ;
		calculation_step_length = source.calculation_step_length;
		ts_days_between = source.ts_days_between;
		min_depth_subsurf = source.min_depth_subsurf;
		avgsum_time_series = source.avgsum_time_series;
		useCanopyModel = source.useCanopyModel;
		useSoilLayers = source.useSoilLayers;
		research_mode = source.research_mode;
		perp_to_slope = source.perp_to_slope;
		out_heat = source.out_heat;
		out_lw = source.out_lw;
		out_sw = source.out_sw;
		out_meteo = source.out_meteo;
		out_haz = source.out_haz;
		out_mass = source.out_mass;
		out_t = source.out_t;
		out_load = source.out_load;
		out_stab = source.out_stab;
		out_canopy = source.out_canopy;
		out_soileb = source.out_soileb;
		enable_pref_flow = source.enable_pref_flow;
	}
	return *this;
}

/**
 * @brief This routine checks if the specified snow cover data exists
 * @param i_snowfile file containing the initial state of the snowpack
 * @param stationID
 * @return true if the file exists
 */
bool SmetIO::snowCoverExists(const std::string& i_snowfile, const std::string& /*stationID*/) const
{
	std::string snofilename( getFilenamePrefix(i_snowfile, i_snowpath, false) );

	if (snofilename.rfind(".sno") == string::npos) {
		snofilename += ".sno";
	}

	return FileUtils::fileExists(snofilename);
}

/**
 * @brief This routine reads the status of the snow cover at program start
 * @version 11.02
 * @param i_snowfile file containing the initial state of the snowpack
 * @param stationID
 * @param SSdata
 * @param Zdata
 * @param read_salinity
 */
void SmetIO::readSnowCover(const std::string& i_snowfile, const std::string& stationID,
                           SN_SNOWSOIL_DATA& SSdata, ZwischenData& Zdata, const bool& read_salinity)
{
	std::string snofilename( getFilenamePrefix(i_snowfile, i_snowpath, false) );
	std::string hazfilename(snofilename);

	if (snofilename.rfind(".sno") == string::npos) {
		snofilename += ".sno";
		hazfilename += ".haz";
	} else {
		hazfilename.replace(hazfilename.rfind(".sno"), 4, ".haz");
	}

	const Date sno_date( read_snosmet(snofilename, stationID, SSdata, read_salinity) );
	if (FileUtils::fileExists(hazfilename)) {
		const Date haz_date( read_hazsmet(hazfilename, Zdata) );
		if (haz_date != sno_date)
			throw IOException("Inconsistent ProfileDate in files: " + snofilename + " and " + hazfilename, AT);
	} else {
		//prn_msg(__FILE__, __LINE__, "wrn", Date(), "Hazard file %s does not exist. Initialize Zdata to zero.", hazfilename.c_str());
		Zdata.reset();
	}
}

mio::Date SmetIO::read_hazsmet(const std::string& hazfilename, ZwischenData& Zdata)
{
	/*
	 * Read HAZ SMET file, parse header and fill Zdata with values from the [DATA] section
	 * The header is not parsed apart from the ProfileDate which is needed for a consistency
	 * check with the corresponding SNO SMET file
	 */
	smet::SMETReader haz_reader(hazfilename);
	static const unsigned char nr_datalines = 144;

	Date profile_date;
	IOUtils::convertString(profile_date, haz_reader.get_header_value("ProfileDate"),  SmetIO::in_dflt_TZ);
	if (profile_date.isUndef())
		throw InvalidFormatException("Invalid ProfileDate in file \""+hazfilename+"\"", AT);

	std::vector<string> vec_timestamp;
	std::vector<double> vec_data;
	haz_reader.read(vec_timestamp, vec_data);

	if (vec_timestamp.size() != nr_datalines)
		throw InvalidFormatException("There need to be 144 data lines in " + haz_reader.get_filename(), AT);

	size_t current_index = 0;
	for (size_t ii=0; ii<nr_datalines; ii++) {
		const size_t index = nr_datalines - 1 - ii;
		if (ii>=96){
			Zdata.hoar24[index]  = vec_data[current_index++];
			Zdata.drift24[index] = vec_data[current_index++];
		} else {
			current_index += 2;
		}

		Zdata.hn3[index]  = vec_data[current_index++];
		Zdata.hn24[index] = vec_data[current_index++];
	}

	return profile_date;
}

//Read SNO SMET file, parse header and fill SSdata with values from the [DATA] section
mio::Date SmetIO::read_snosmet(const std::string& snofilename, const std::string& stationID, SN_SNOWSOIL_DATA& SSdata, const bool& read_salinity) const
{
	smet::SMETReader sno_reader(snofilename);
	Date profile_date( read_snosmet_header(sno_reader, stationID, SSdata) );
	if (profile_date.isUndef())
		throw InvalidFormatException("Invalid ProfileDate in file \""+snofilename+"\"", AT);
	profile_date.rnd(1.);

	//Read actual data
	vector<string> vec_timestamp;
	vector<double> vec_data;
	sno_reader.read(vec_timestamp, vec_data);

	if (SSdata.nLayers > 0)
		SSdata.Ldata.resize(SSdata.nLayers, LayerData());
	if (vec_timestamp.size() != SSdata.nLayers)
		throw InvalidFormatException("Xdata: Layers expected != layers read in " + sno_reader.get_filename(), AT);

	const size_t nr_of_fields = sno_reader.get_nr_of_fields();
	const size_t nr_of_solutes = (nr_of_fields - 18) / 4;

	if (SnowStation::number_of_solutes != nr_of_solutes)
		throw InvalidFormatException("Mismatch in number_of_solutes and fields in " + sno_reader.get_filename(), AT);

	//copy data to respective variables in SSdata
	size_t current_index = 0;
	Date prev_depositionDate( 0., in_dflt_TZ );
	for (size_t ll=0; ll<SSdata.nLayers; ll++) {
		//firstly deal with date
		IOUtils::convertString(SSdata.Ldata[ll].depositionDate, vec_timestamp[ll],  in_dflt_TZ);
		SSdata.Ldata[ll].depositionDate.rnd(1.);

		if (SSdata.Ldata[ll].depositionDate > SSdata.profileDate) {
			prn_msg(__FILE__, __LINE__, "err", Date(),
				   "Layer %d from bottom is younger (%s) than ProfileDate (%s) !!!",
				   ll+1, SSdata.Ldata[ll].depositionDate.toString(Date::ISO).c_str(), SSdata.profileDate.toString(Date::ISO).c_str());
			throw IOException("Cannot generate Xdata from file " + sno_reader.get_filename(), AT);
		}
		if (SSdata.Ldata[ll].depositionDate < prev_depositionDate && !read_salinity) {		// Note: in sea ice it is possible that younger layers are below
			prn_msg(__FILE__, __LINE__, "err", Date(),
				   "Layer %d is younger (%s) than layer above (%s) !!!",
				   ll, prev_depositionDate.toString(Date::ISO).c_str(), SSdata.Ldata[ll].depositionDate.toString(Date::ISO).c_str());
			throw IOException("Cannot generate Xdata from file " + sno_reader.get_filename(), AT);
		}
		prev_depositionDate = SSdata.Ldata[ll].depositionDate;

		//secondly with the actual data
		SSdata.Ldata[ll].hl = vec_data[current_index++];
		SSdata.Ldata[ll].tl = vec_data[current_index++];
		SSdata.Ldata[ll].phiIce = vec_data[current_index++];
		SSdata.Ldata[ll].phiWater = vec_data[current_index++];
		if (enable_pref_flow) {
			SSdata.Ldata[ll].phiWaterPref = vec_data[current_index++];
		} else {
			SSdata.Ldata[ll].phiWaterPref = 0.;
		}
		SSdata.Ldata[ll].phiVoids = vec_data[current_index++];
		SSdata.Ldata[ll].phiSoil = vec_data[current_index++];

		if (SSdata.Ldata[ll].tl < 100.) {
			SSdata.Ldata[ll].tl = IOUtils::C_TO_K(SSdata.Ldata[ll].tl);
		}
		SSdata.Ldata[ll].SoilRho = vec_data[current_index++];
		SSdata.Ldata[ll].SoilK = vec_data[current_index++];
		SSdata.Ldata[ll].SoilC = vec_data[current_index++];
		SSdata.Ldata[ll].rg = vec_data[current_index++];
		SSdata.Ldata[ll].rb = vec_data[current_index++];
		SSdata.Ldata[ll].dd = vec_data[current_index++];
		SSdata.Ldata[ll].sp = vec_data[current_index++];
		SSdata.Ldata[ll].mk = static_cast<unsigned short int>(vec_data[current_index++]+.5); //int
		SSdata.Ldata[ll].hr = vec_data[current_index++];
		SSdata.Ldata[ll].ne = static_cast<unsigned int>(vec_data[current_index++]+.5); //int

		if (SSdata.Ldata[ll].rg>0. && SSdata.Ldata[ll].rb >= SSdata.Ldata[ll].rg) {
			//HACK To avoid surprises in lwsn_ConcaveNeckRadius()
			SSdata.Ldata[ll].rb = Metamorphism::max_grain_bond_ratio * SSdata.Ldata[ll].rg;
			prn_msg(__FILE__, __LINE__, "wrn", Date(), "Layer %d from bottom: bond radius rb/rg larger than Metamorphism::max_grain_bond_ratio=%lf (rb=%lf mm, rg=%lf mm)! Reset to Metamorphism::max_grain_bond_ratio", ll+1, Metamorphism::max_grain_bond_ratio, SSdata.Ldata[ll].rb, SSdata.Ldata[ll].rg);
		}

		SSdata.Ldata[ll].CDot = vec_data[current_index++];
		SSdata.Ldata[ll].metamo = vec_data[current_index++];

		if (read_salinity) {
			SSdata.Ldata[ll].salinity = vec_data[current_index++];
			SSdata.Ldata[ll].h = vec_data[current_index++];
		}

		for (size_t ii=0; ii<SnowStation::number_of_solutes; ii++) {
			SSdata.Ldata[ll].cIce[ii] = vec_data[current_index++];
			SSdata.Ldata[ll].cWater[ii] = vec_data[current_index++];
			SSdata.Ldata[ll].cVoids[ii] = vec_data[current_index++];
			SSdata.Ldata[ll].cSoil[ii] = vec_data[current_index++];
		}
	} //for loop over layers

	SSdata.nN = 1;
	SSdata.Height = 0.;
	for (size_t ll = 0; ll < SSdata.nLayers; ll++) {
		SSdata.nN    += SSdata.Ldata[ll].ne;
		SSdata.Height += SSdata.Ldata[ll].hl;
	}

	return profile_date;
}

mio::Date SmetIO::read_snosmet_header(const smet::SMETReader& sno_reader, const std::string& stationID,
                                      SN_SNOWSOIL_DATA& SSdata) const
{
	/*
	 * Read values for certain header keys (integer and double values) and perform
	 * consistency checks upon them.
	 */
	const std::string station_name( sno_reader.get_header_value("station_name") );
	IOUtils::convertString(SSdata.profileDate, sno_reader.get_header_value("ProfileDate"),  in_dflt_TZ);

	SSdata.HS_last = get_doubleval(sno_reader, "HS_Last");

	mio::Coords tmppos;
	const double alt = get_doubleval(sno_reader, "altitude");
	if (keyExists(sno_reader, "latitude")) {
		const double lat = get_doubleval(sno_reader, "latitude");
		const double lon = get_doubleval(sno_reader, "longitude");
		tmppos.setLatLon(lat, lon, alt);
	} else {
		const double easting = get_doubleval(sno_reader, "easting");
		const double northing = get_doubleval(sno_reader, "northing");
		const int epsg = get_intval(sno_reader, "epsg");
		tmppos.setEPSG(epsg);
		tmppos.setXY(easting, northing, alt);
	}

	const double slope_angle = get_doubleval(sno_reader, "SlopeAngle");
	const double azi = get_doubleval(sno_reader, "SlopeAzi");
	SSdata.meta.setStationData(tmppos, stationID, station_name);
	SSdata.meta.setSlope(slope_angle, azi);

	// Check consistency with radiation switch
	if ((sw_mode == "BOTH") && perp_to_slope && (SSdata.meta.getSlopeAngle() > Constants::min_slope_angle)) {
		prn_msg(__FILE__, __LINE__, "wrn", Date(),
		        "You want to use measured albedo in a slope steeper than 3 deg  with PERP_TO_SLOPE set!");
		throw IOException("Do not generate Xdata from file " + sno_reader.get_filename(), AT);
	}

	const int nSoilLayerData = get_intval(sno_reader, "nSoilLayerData");
	if (nSoilLayerData < 0) {
		prn_msg(__FILE__, __LINE__, "err", Date(), "'nSoilLayerData' < 0 !!!");
		throw InvalidFormatException("Cannot generate Xdata from file " + sno_reader.get_filename(), AT);
	} else if (useSoilLayers && (nSoilLayerData < 1)) {
		prn_msg(__FILE__, __LINE__, "err", Date(), "useSoilLayers set but 'nSoilLayerData' < 1 !!!");
		throw InvalidFormatException("Cannot generate Xdata from file " + sno_reader.get_filename(), AT);
	} else if (!useSoilLayers && (nSoilLayerData > 0)) {
		prn_msg(__FILE__, __LINE__, "err", Date(), "useSoilLayers not set but 'nSoilLayerData' > 0 !!!");
		throw InvalidFormatException("Cannot generate Xdata from file " + sno_reader.get_filename(), AT);
	}
	SSdata.nLayers = (unsigned int) nSoilLayerData;

	const int nSnowLayerData = get_intval(sno_reader, "nSnowLayerData");
	if (nSnowLayerData < 0) {
		prn_msg(__FILE__, __LINE__, "err", Date(), "'nSnowLayerData' < 0  !!!");
		throw InvalidFormatException("Cannot generate Xdata from file " + sno_reader.get_filename(), AT);
	}
	SSdata.nLayers += (unsigned int)nSnowLayerData;

	SSdata.SoilAlb = get_doubleval(sno_reader, "SoilAlbedo");
	SSdata.BareSoil_z0 = get_doubleval(sno_reader, "BareSoil_z0");
	if (SSdata.BareSoil_z0 == 0.) {
		prn_msg(__FILE__, __LINE__, "wrn", Date(), "'BareSoil_z0'=0 from %s, reset to 0.02",
			   sno_reader.get_filename().c_str());
		SSdata.BareSoil_z0 = 0.02;
	}
	if (SSdata.HS_last > 0.05) {
		SSdata.Albedo = 0.9;
	} else {
		SSdata.Albedo = SSdata.SoilAlb;
	}

	SSdata.Canopy_Height = get_doubleval(sno_reader, "CanopyHeight");
	SSdata.Canopy_LAI = get_doubleval(sno_reader, "CanopyLeafAreaIndex");
	SSdata.Canopy_Direct_Throughfall = get_doubleval(sno_reader, "CanopyDirectThroughfall");
	SSdata.WindScalingFactor = get_doubleval(sno_reader, "WindScalingFactor");

	SSdata.ErosionLevel = get_intval(sno_reader, "ErosionLevel");
	SSdata.TimeCountDeltaHS = get_doubleval(sno_reader, "TimeCountDeltaHS");

	SSdata.Canopy_int_cap_snow = get_doubleval_no_error(sno_reader, "CanopySnowIntCapacity");
	SSdata.Canopy_alb_dry = get_doubleval_no_error(sno_reader, "CanopyAlbedoDry");
	SSdata.Canopy_alb_wet = get_doubleval_no_error(sno_reader, "CanopyAlbedoWet");
	SSdata.Canopy_alb_snow = get_doubleval_no_error(sno_reader, "CanopyAlbedoSnow");
	SSdata.Canopy_diameter = get_doubleval_no_error(sno_reader, "CanopyDiameter");
	SSdata.Canopy_lai_frac_top_default = get_doubleval_no_error(sno_reader, "CanopyFracLAIUpperLayer");
	SSdata.Canopy_BasalArea = get_doubleval_no_error(sno_reader, "CanopyBasalArea");
	SSdata.Emissivity_soil = get_doubleval_no_error(sno_reader, "SoilEmissivity");


	return SSdata.profileDate;
}

bool SmetIO::keyExists(const smet::SMETReader& reader, const std::string& key)
{
	const double nodata = reader.get_header_doublevalue("nodata");
	const double value = reader.get_header_doublevalue(key);

	return value!=nodata;
}

double SmetIO::get_doubleval(const smet::SMETReader& reader, const std::string& key)
{
	// Retrieve a double value from a SMETReader object header and make sure it exists.
	// If the header key does not exist or the value is not set throw an exception
	const double nodata = reader.get_header_doublevalue("nodata");
	const double value = reader.get_header_doublevalue(key);

	if (value == nodata){
		const std::string msg( "Missing key '" + key + "'" );
		prn_msg(__FILE__, __LINE__, "err", Date(), msg.c_str());
		throw InvalidFormatException("Cannot generate Xdata from file " + reader.get_filename(), AT);
	}

	return value;
}

double SmetIO::get_doubleval_no_error(const smet::SMETReader& reader, const std::string& key)
{
	// Retrieve a double value from a SMETReader object header and make sure it exist
	// If the header key does not exist or the value is not set, set it to meteoIO nodata
	const double nodata = reader.get_header_doublevalue("nodata");
	double value = reader.get_header_doublevalue(key);

	if (value == nodata){
	   value=mio::IOUtils::nodata;
	}
	return value;
}

int SmetIO::get_intval(const smet::SMETReader& reader, const std::string& key)
{
	// Retrieve an integer value from a SMETReader object header and make sure it exists.
	// If the header key does not exist or the value is not set throw an exception
	const double nodata = reader.get_header_doublevalue("nodata");
	const int inodata = static_cast<int>( floor(nodata + .1) );

	const int value = reader.get_header_intvalue(key);
	if (value == inodata){
		const std::string msg( "Missing key '" + key + "'" );
		prn_msg(__FILE__, __LINE__, "err", Date(), msg.c_str());
		throw InvalidFormatException("Cannot generate Xdata from file " + reader.get_filename(), AT);
	}

	return value;
}

/**
 * @brief This routine writes the status of the snow cover at program termination and at specified backup times
 * @version 11.02
 * @param date current
 * @param Xdata
 * @param Zdata
 * @param forbackup dump Xdata on the go
 */
void SmetIO::writeSnowCover(const mio::Date& date, const SnowStation& Xdata,
                            const ZwischenData& Zdata, const bool& forbackup)
{
	std::string snofilename( getFilenamePrefix(Xdata.meta.getStationID().c_str(), o_snowpath) + ".sno" );
	std::string hazfilename( getFilenamePrefix(Xdata.meta.getStationID().c_str(), o_snowpath) + ".haz" );

	if (forbackup){
		std::stringstream ss;
		ss << "" << (int)(date.getJulian() + 0.5);
		snofilename += ss.str();
		hazfilename += ss.str();
	}

	writeSnoFile(snofilename, date, Xdata, Zdata, enable_pref_flow);
	writeHazFile(hazfilename, date, Xdata, Zdata);
}

/*
* Create a SMETWriter object, sets its header and copies all required
* data and timestamps into vec_timestamp and vec_data (copied from Zdata).
* The SMETWriter object finally writes out the HAZ SMET file
*/
void SmetIO::writeHazFile(const std::string& hazfilename, const mio::Date& date, const SnowStation& Xdata,
                          const ZwischenData& Zdata)
{
	smet::SMETWriter haz_writer(hazfilename);
	setBasicHeader(Xdata, "timestamp SurfaceHoarIndex DriftIndex ThreeHourNewSnow TwentyFourHourNewSnow", haz_writer);
	haz_writer.set_header_value("ProfileDate", date.toString(Date::ISO));

	haz_writer.set_width( vector<int>(4,10) );
	haz_writer.set_precision( vector<int>(4,6) );

	Date hrs72( date - Date(3.0,0.0) );
	const Duration half_hour(.5/24., 0.0);
	std::vector<std::string> vec_timestamp;
	std::vector<double> vec_data;

	for (size_t ii=0; ii<144; ii++){
		//hn3 and hn24 have 144 fields, the newest values have index 0
		//hoar24 and drift 24 have 48 fields, the newest values have index 0
		hrs72 += half_hour;
		vec_timestamp.push_back( hrs72.toString(Date::ISO) );
		if (ii >= 96){
			const size_t index = 143-ii;
			vec_data.push_back( Zdata.hoar24[index] );  //Print out the hoar hazard data info
			vec_data.push_back( Zdata.drift24[index] ); //Print out the drift hazard data info
		} else {
			vec_data.push_back( IOUtils::nodata );
			vec_data.push_back( IOUtils::nodata );
		}

		vec_data.push_back( Zdata.hn3[143-ii] );  //Print out the 3 hour new snowfall hazard data info
		vec_data.push_back( Zdata.hn24[143-ii] ); //Print out the 24 hour new snowfall hazard data info
	}
	haz_writer.write(vec_timestamp, vec_data);
}

/*
* Create a SMETWriter object, sets its header and copies all required
* data and timestamps into vec_timestamp and vec_data (from Xdata).
* The SMETWriter object finally writes out the SNO SMET file
*/
void SmetIO::writeSnoFile(const std::string& snofilename, const mio::Date& date, const SnowStation& Xdata,
                          const ZwischenData& /*Zdata*/, const bool& write_pref_flow) const
{
	smet::SMETWriter sno_writer(snofilename);
	stringstream ss;
	if (write_pref_flow) {
		// Header in case preferential flow is used
		ss << "timestamp Layer_Thick  T  Vol_Frac_I  Vol_Frac_W  Vol_Frac_WP  Vol_Frac_V  Vol_Frac_S Rho_S"; //8
	} else {
		// Default header
		ss << "timestamp Layer_Thick  T  Vol_Frac_I  Vol_Frac_W  Vol_Frac_V  Vol_Frac_S Rho_S"; //8
	}
	ss << " Conduc_S HeatCapac_S  rg  rb  dd  sp  mk mass_hoar ne CDot metamo";
	if (Xdata.Seaice != NULL) ss << " Sal h";
	for (size_t ii = 0; ii < Xdata.number_of_solutes; ii++) {
		ss << " cIce cWater cAir  cSoil";
	}

	setBasicHeader(Xdata, ss.str(), sno_writer);
	setSnoSmetHeader(Xdata, date, sno_writer);

	vector<string> vec_timestamp;
	vector<double> vec_data;
	vector<int> vec_width, vec_precision;
	setFormatting(Xdata.number_of_solutes, vec_width, vec_precision, write_pref_flow, (Xdata.Seaice!=NULL));
	sno_writer.set_width(vec_width);
	sno_writer.set_precision(vec_precision);

	//Fill vec_data with values
	const vector<ElementData>& EMS = Xdata.Edata;
	for (size_t e = 0; e < Xdata.getNumberOfElements(); e++) {
		vec_timestamp.push_back(EMS[e].depositionDate.toString(Date::ISO));

		vec_data.push_back(EMS[e].L);
		vec_data.push_back(Xdata.Ndata[e+1].T);
		vec_data.push_back(EMS[e].theta[ICE]);
		vec_data.push_back(EMS[e].theta[WATER]);
		if (write_pref_flow) vec_data.push_back(EMS[e].theta[WATER_PREF]);
		vec_data.push_back(EMS[e].theta[AIR]);
		vec_data.push_back(EMS[e].theta[SOIL]);
		vec_data.push_back(EMS[e].soil[SOIL_RHO]);
		vec_data.push_back(EMS[e].soil[SOIL_K]);
		vec_data.push_back(EMS[e].soil[SOIL_C]);
		vec_data.push_back(EMS[e].rg);
		vec_data.push_back(EMS[e].rb);
		vec_data.push_back(EMS[e].dd);
		vec_data.push_back(EMS[e].sp);
		vec_data.push_back(static_cast<double>(EMS[e].mk));

		vec_data.push_back(Xdata.Ndata[e+1].hoar);
		vec_data.push_back(1.);
		vec_data.push_back(EMS[e].CDot);
		vec_data.push_back(EMS[e].metamo);

		if (Xdata.Seaice != NULL) {
			vec_data.push_back(EMS[e].salinity);
			vec_data.push_back(EMS[e].h);
		}

		for (size_t ii = 0; ii < Xdata.number_of_solutes; ii++) {
			vec_data.push_back(EMS[e].conc(ICE,ii));
			vec_data.push_back(EMS[e].conc(WATER,ii));
			vec_data.push_back(EMS[e].conc(AIR,ii));
			vec_data.push_back(EMS[e].conc(SOIL,ii));
		}
	}

	sno_writer.write(vec_timestamp, vec_data);
}

void SmetIO::setBasicHeader(const SnowStation& Xdata, const std::string& fields, smet::SMETWriter& smet_writer)
{
	// Set the basic, mandatory header key/value pairs for a SMET file
	smet_writer.set_header_value("station_id", Xdata.meta.getStationID());
	smet_writer.set_header_value("station_name", Xdata.meta.getStationName());
	smet_writer.set_header_value("nodata", IOUtils::nodata);
	smet_writer.set_header_value("fields", fields);

	// Latitude, Longitude, Altitude
	smet_writer.set_header_value("latitude", Xdata.meta.position.getLat());
	smet_writer.set_header_value("longitude", Xdata.meta.position.getLon());
	smet_writer.set_header_value("altitude", Xdata.meta.position.getAltitude());
	smet_writer.set_header_value("epsg", Xdata.meta.position.getEPSG());
	smet_writer.set_header_value("slope_angle", Xdata.meta.getSlopeAngle());
	smet_writer.set_header_value("slope_azi", Xdata.meta.getAzimuth());
}

void SmetIO::setSnoSmetHeader(const SnowStation& Xdata, const Date& date, smet::SMETWriter& smet_writer)
{
	/*
	 * The non-compulsory header key/value pairs for SNO files are set in this procedure
	 * The sequence in which they are handed to smet_writer will be preserved when
	 * the SMETWriter actually writes the header
	 */
	stringstream ss; //we use the stringstream to produce strings in desired format

	smet_writer.set_header_value("ProfileDate", date.toString(Date::ISO));
	smet_writer.set_header_value("tz", date.getTimeZone());

	// Last checked calculated snow depth used for albedo control of next run
	ss.str(""); ss << fixed << setprecision(6) << (Xdata.cH - Xdata.Ground);
	smet_writer.set_header_value("HS_Last", ss.str());

	// Slope metadata
	ss.str(""); ss << fixed << setprecision(2) << Xdata.meta.getSlopeAngle();
	smet_writer.set_header_value("SlopeAngle", ss.str());
	ss.str(""); ss << fixed << setprecision(2) << Xdata.meta.getAzimuth();
	smet_writer.set_header_value("SlopeAzi", ss.str());

	// Number of Soil Layer Data; in case of no soil used to store the erosion level
	smet_writer.set_header_value("nSoilLayerData", static_cast<double>(Xdata.SoilNode));
	// Number of Snow Layer Data
	smet_writer.set_header_value("nSnowLayerData", static_cast<double>(Xdata.getNumberOfElements() - Xdata.SoilNode));

	// Ground Characteristics (introduced June 2006)
	ss.str(""); ss << fixed << setprecision(2) << Xdata.SoilAlb;
	smet_writer.set_header_value("SoilAlbedo", ss.str());
	ss.str(""); ss << fixed << setprecision(3) << Xdata.BareSoil_z0;
	smet_writer.set_header_value("BareSoil_z0", ss.str());

	// Canopy Characteristics
	ss.str(""); ss << fixed << setprecision(2) << Xdata.Cdata.height;
	smet_writer.set_header_value("CanopyHeight", ss.str());
	ss.str(""); ss << fixed << setprecision(6) << Xdata.Cdata.lai;
	smet_writer.set_header_value("CanopyLeafAreaIndex", ss.str());
	ss.str(""); ss << fixed << setprecision(2) << Xdata.Cdata.direct_throughfall;
	smet_writer.set_header_value("CanopyDirectThroughfall", ss.str());
	ss.str(""); ss << fixed << setprecision(2) << Xdata.Cdata.int_cap_snow;
	smet_writer.set_header_value("CanopySnowIntCapacity", ss.str());
	ss.str(""); ss << fixed << setprecision(2) << Xdata.Cdata.can_alb_dry;
	smet_writer.set_header_value("CanopyAlbedoDry", ss.str());
	ss.str(""); ss << fixed << setprecision(2) << Xdata.Cdata.can_alb_wet;
	smet_writer.set_header_value("CanopyAlbedoWet", ss.str());
	ss.str(""); ss << fixed << setprecision(2) << Xdata.Cdata.can_alb_snow;
	smet_writer.set_header_value("CanopyAlbedoSnow", ss.str());
	ss.str(""); ss << fixed << setprecision(2) << Xdata.Cdata.can_diameter;
	smet_writer.set_header_value("CanopyDiameter", ss.str());
	ss.str(""); ss << fixed << setprecision(2) << Xdata.Cdata.lai_frac_top_default;
	smet_writer.set_header_value("CanopyFracLAIUpperLayer", ss.str());
	ss.str(""); ss << fixed << setprecision(2) << Xdata.Cdata.BasalArea;
	smet_writer.set_header_value("CanopyBasalArea", ss.str());
	ss.str(""); ss << fixed << setprecision(2) << Xdata.SoilEmissivity;
	smet_writer.set_header_value("SoilEmissivity", ss.str());

	// Additional parameters
	ss.str(""); ss << fixed << setprecision(2) << Xdata.WindScalingFactor;
	smet_writer.set_header_value("WindScalingFactor", ss.str());
	smet_writer.set_header_value("ErosionLevel", static_cast<double>(Xdata.ErosionLevel));
	ss.str(""); ss << fixed << setprecision(6) << Xdata.TimeCountDeltaHS;
	smet_writer.set_header_value("TimeCountDeltaHS", ss.str());
}

void SmetIO::setFormatting(const size_t& nr_solutes,
                           std::vector<int>& vec_width, std::vector<int>&  vec_precision, const bool& write_pref_flow, const bool& write_sea_ice)
{
	/*
	 * When writing a SNOW SMET file each written parameter may have a different
	 * column width and precision. This procedure sets the vectors vec_width
	 * and vec_precision which are subsequently handed to a SMETWriter object
	 * It is paramount that the number of fields (not counting the timestamp)
	 * in the SMET file corresponds to the number of elements in both
	 * vec_width and vec_precision
	 */
	vec_width.clear();
	vec_precision.clear();
	vec_width.push_back(12); vec_precision.push_back(OUTPUT_PRECISION_SNO_FILE); //EMS[e].L
	vec_width.push_back(12); vec_precision.push_back(OUTPUT_PRECISION_SNO_FILE); //Xdata.Ndata[e+1].T
	vec_width.push_back(12); vec_precision.push_back(OUTPUT_PRECISION_SNO_FILE); //EMS[e].theta[ICE]
	vec_width.push_back(12); vec_precision.push_back(OUTPUT_PRECISION_SNO_FILE); //EMS[e].theta[WATER]
	if (write_pref_flow) {
		vec_width.push_back(12); vec_precision.push_back(OUTPUT_PRECISION_SNO_FILE); //EMS[e].theta[WATER_PREF]
	}
	vec_width.push_back(12); vec_precision.push_back(OUTPUT_PRECISION_SNO_FILE); //EMS[e].theta[AIR]
	vec_width.push_back(12); vec_precision.push_back(OUTPUT_PRECISION_SNO_FILE); //EMS[e].theta[SOIL]
	vec_width.push_back(9); vec_precision.push_back(1);  //EMS[e].soil[SOIL_RHO]
	vec_width.push_back(9); vec_precision.push_back(3);  //EMS[e].soil[SOIL_K]
	vec_width.push_back(12); vec_precision.push_back(1);  //EMS[e].soil[SOIL_C]
	vec_width.push_back(11); vec_precision.push_back(OUTPUT_PRECISION_SNO_FILE);  //EMS[e].rg
	vec_width.push_back(10); vec_precision.push_back(OUTPUT_PRECISION_SNO_FILE);  //EMS[e].rb
	vec_width.push_back(10); vec_precision.push_back(OUTPUT_PRECISION_SNO_FILE);  //EMS[e].dd
	vec_width.push_back(10); vec_precision.push_back(OUTPUT_PRECISION_SNO_FILE);  //EMS[e].sp
	vec_width.push_back(6); vec_precision.push_back(0);  //EMS[e].mk

	vec_width.push_back(13); vec_precision.push_back(OUTPUT_PRECISION_SNO_FILE); //Xdata.Ndata[e+1].hoar
	vec_width.push_back(4); vec_precision.push_back(0);  //ne
	vec_width.push_back(15); vec_precision.push_back(OUTPUT_PRECISION_SNO_FILE); //EMS[e].CDot
	vec_width.push_back(15); vec_precision.push_back(OUTPUT_PRECISION_SNO_FILE); //EMS[e].metamo

	if (write_sea_ice) {
		vec_width.push_back(15); vec_precision.push_back(OUTPUT_PRECISION_SNO_FILE); //EMS[e].salinity
		vec_width.push_back(16); vec_precision.push_back(OUTPUT_PRECISION_SNO_FILE); //EMS[e].h
	}

	for (size_t ii = 0; ii < nr_solutes; ii++) {
		vec_width.push_back(17); vec_precision.push_back(6); //EMS[e].conc(ICE,ii)
		vec_width.push_back(18); vec_precision.push_back(7); //EMS[e].conc(WATER,ii)
		vec_width.push_back(18); vec_precision.push_back(7); //EMS[e].conc(AIR,ii)
		vec_width.push_back(18); vec_precision.push_back(7); //EMS[e].conc(SOIL,ii)
	}
}

/**
 * @brief Returns sensor position perpendicular to slope (m) \n
 * Negative vertical height indicates depth from either snow or ground surface \n
 * NOTE: Depth from snow surface cannot be used with SNP_SOIL set
 * @author Charles Fierz
 * @version 10.02
 * @param z_vert Vertical position of the sensor (m)
 * @param hs_ref Height of snow to refer to (m)
 * @param Ground Ground level (m)
 * @param slope_angle (deg)
 */
double SmetIO::compPerpPosition(const double& z_vert, const double& hs_ref, const double& ground, const double& cos_sl) const
{
	double pos=0.;
	if (z_vert == mio::IOUtils::nodata) {
		pos = Constants::undefined;
	} else if (!useSoilLayers && (z_vert < 0.)) {
		pos = hs_ref + z_vert * cos_sl;
		if (pos < 0.)
			pos = Constants::undefined;
	} else {
		pos = ground + z_vert * cos_sl;
		if (pos < -ground)
			pos = Constants::undefined;
	}
	return pos;
}

std::string SmetIO::getFieldsHeader(const SnowStation& Xdata) const
{
	std::ostringstream os;
	os << "timestamp ";

	if (out_heat)
		os << "Qs Ql Qg TSG Qg0 Qr" << " "; // 1-2: Turbulent fluxes (W m-2)
		// 14-17: Heat flux at lower boundary (W m-2), ground surface temperature (degC),
		//        Heat flux at gound surface (W m-2), rain energy (W m-2)
	if (out_lw)
		os << "OLWR ILWR LWR_net" << " "; // 3-5: Longwave radiation fluxes (W m-2)
	if (out_sw)
		os << "OSWR ISWR Qw pAlbedo mAlbedo ISWR_h ISWR_dir ISWR_diff" << " "; // 6-9: Shortwave radiation fluxes (W m-2) and computed albedo
	if (out_meteo)
		os << "TA TSS_mod TSS_meas T_bottom RH VW VW_drift DW MS_Snow HS_mod HS_meas" << " "; // 10-13: Air temperature, snow surface temperature (modeled and measured), temperature at bottom of snow/soil pack (degC)
	if (out_haz)
		os << "hoar_size wind_trans24 HN24 HN72_24" << " ";// 30-33: surface hoar size (mm), 24h drift index (cm), height of new snow HN (cm), 3d sum of daily new snow depths (cm)
	if (out_soileb)
		os << "dIntEnergySoil meltFreezeEnergySoil ColdContentSoil" << " ";
	if (out_mass)
		os << "SWE MS_Water MS_Wind MS_Rain MS_SN_Runoff MS_Soil_Runoff MS_Sublimation MS_Evap" << " "; // 34-39: SWE, eroded mass, rain rate, runoff at bottom of snowpack, sublimation and evaporation, all in kg m-2 except rain as rate: kg m-2 h-1; see also 52 & 93. LWC (kg m-2);
	if (out_load)
		os << "load "; // 50: Solute load at ground surface
	if (out_t && !fixedPositions.empty()) {
		// 40-49: Internal Temperature Time Series at fixed heights, modeled and measured, all in degC
		for (size_t ii = 0; ii < fixedPositions.size(); ii++)
			os << "TS" << ii << " ";
	}
	if (out_stab)
		os << "Sclass1 Sclass2 zSd Sd zSn Sn zSs Ss zS4 S4 zS5 S5" << " "; //S5 is liquidWaterIndex

	if (out_canopy)
	{
		os << "Interception_storage Canopy_surface_temperature Canopy_albedo Wet_fraction Interception_capacity Net_shortwave_radiation_absorbed_by_canopy" << " "; //6
		os << "Net_longwave_radiation_absorbed_by_canopy Net_radiation_to_canopy Sensible_heat_flux_to_canopy Latent_heat_flux_to_canopy" << " "; //4
		os << "Biomass_heat_storage_flux_towards_Canopy Transpiration_of_the_canopy Evaporation_and_sublimation_of_interception_(liquid_and_frozen)" << " "; //3
		os << "Interception_rate Throughfall Snow_unload Longwave_radiation_up_above_canopy Longwave_radiation_down_above_canopy" << " "; //5
		os << "Shortwave_radiation_up_above_canopy Shortwave_radiation_down_above_canopy Total_land_surface_albedo" << " "; //3
		os << "Total_net_radiation_to_the_surface_(ground_+_canopy) Surface_radiative_temperature_(ground_+_canopy)" << " "; //2
		os << "Forest_floor_albedo Snowfall_rate_Above_Canopy Rainfall_rate_Above_Canopy Evapotranspiration_of_the_total_surface_(ground_+_canopy)" << " "; //4
	}

	if (Xdata.Seaice != NULL)
		os << "Total_thickness Ice_thickness Snow_thickness Snow_thickness_wrt_reference Freeboard Sea_level Bulk_salinity Avg_bulk_salinity Avg_brine_salinity Bottom_salinity_flux Top_salinity_flux Total_Flooding_Bucket" << " ";

	return os.str();
}

void SmetIO::writeTimeSeriesHeader(const SnowStation& Xdata, const double& tz, smet::SMETWriter& smet_writer) const
{
	const std::string fields( getFieldsHeader(Xdata) );
	setBasicHeader(Xdata, fields, smet_writer);
	smet_writer.set_header_value("tz", tz);
	if (out_haz) { // HACK To avoid troubles in A3D
		ostringstream ss;
		ss << "Snowpack " << info.version << " run by \"" << info.user << "\"";
		smet_writer.set_header_value("creator", ss.str());
	}

	std::ostringstream units_offset, units_multiplier;
	units_offset << "0 "; units_multiplier << "1 ";

	std::ostringstream plot_units, plot_description, plot_color, plot_min, plot_max;
	plot_units << "- "; plot_description << "timestamp  "; plot_color << "0x000000 "; plot_min << IOUtils::nodata << " "; plot_max << IOUtils::nodata << " ";

	if (out_heat) {
		//"Qs Ql Qg TSG Qg0 Qr"
		plot_description << "sensible_heat  latent_heat  ground_heat  ground_temperature  ground_heat_at_soil_interface  rain_energy" << " ";
		plot_units << "W/m2 W/m2 W/m2 K W/m2 W/m2" << " ";
		units_offset << "0 0 0 273.15 0 0" << " ";
		units_multiplier << "1 1 1 1 1 1" << " ";
		plot_color << "0x669933 0x66CC99 0xCC6600 0xDE22E2 0xFFCC00 0x6600FF" << " ";
		plot_min << "" << " ";
		plot_max << "" << " ";
	}
	if (out_lw) {
		//"OLWR ILWR LWR_net"
		plot_description << "outgoing_long_wave_radiation  incoming_long_wave_radiation  net_long_wave_radiation" << " ";
		plot_units << "W/m2 W/m2 W/m2" << " ";
		units_offset << "0 0 0" << " ";
		units_multiplier << "1 1 1" << " ";
		plot_color << "0x8F6216 0xD99521 0xD9954E" << " ";
		plot_min << "" << " ";
		plot_max << "" << " ";
	}
	if (out_sw) {
		//"OSWR ISWR Qw pAlbedo mAlbedo ISWR_h ISWR_dir ISWR_diff"
		plot_description << "reflected_short_wave_radiation  incoming_short_wave_radiation  net_short_wave_radiation  parametrized_albedo  measured_albedo  incoming_short_wave_on_horizontal  direct_incoming_short_wave  diffuse_incoming_short_wave" << " ";
		plot_units << "W/m2 W/m2 W/m2 - - W/m2 W/m2 W/m2" << " ";
		units_offset << "0 0 0 0 0 0 0 0" << " ";
		units_multiplier << "1 1 1 1 1 1 1 1" << " ";
		plot_color << "0x7D643A 0xF9CA25 0xF9CA9D 0xCC9966 0x996633 0xF9CA25 0x90ca25 0x90CAB8" << " ";
		plot_min << "" << " ";
		plot_max << "" << " ";
	}
	if (out_meteo) {
		//"TA TSS_mod TSS_meas T_bottom RH VW VW_drift DW MS_Snow HS_mod HS_meas"
		plot_description << "air_temperature  surface_temperature(mod)  surface_temperature(meas)  bottom_temperature  relative_humidity  wind_velocity  wind_velocity_drift  wind_direction  solid_precipitation_rate  snow_height(mod)  snow_height(meas)" << " ";
		plot_units << "K K K K - m/s m/s ° kg/m2/h m m" << " ";
		units_offset << "273.15 273.15 273.15 273.15 0 0 0 0 0 0 0" << " ";
		units_multiplier << "1 1 1 1 0.01 1 1 1 1 0.01 0.01" << " ";
		plot_color << "0x8324A4 0xFAA6D0 0xFA72B7 0xDE22E2 0x50CBDB 0x297E24 0x297E24 0x64DD78 0x2431A4 0x818181 0x000000" << " ";
		plot_min << "" << " ";
		plot_max << "" << " ";
	}
	if (out_haz) {
		//"hoar_size wind_trans24 HN24 HN72_24"
		plot_description << "hoar_size  24h_wind_drift  24h_height_of_new_snow  3d_sum_of_daily_height_of_new_snow" << " ";
		plot_units << "m m m m" << " ";
		units_offset << "0 0 0 0" << " ";
		units_multiplier << "0.001 0.01 0.01 0.01" << " ";
		plot_color << "0x9933FF 0x99FFCC 0x006699 0x33CCCC" << " ";
		plot_min << "" << " ";
		plot_max << "" << " ";
	}
	if (out_soileb) {
		//"dIntEnergySoil meltFreezeEnergySoil ColdContentSoil"
		plot_description << "soil_internal_energy_change  soil_melt_freeze_energy  soil_cold_content" << " ";
		plot_units << "W/m2 W/m2 J/m2" << " ";
		units_offset << "0 0 0" << " ";
		units_multiplier << "1 1 1" << " ";
		plot_color << "0x663300 0x996666 0xCC9966" << " ";
		plot_min << "" << " ";
		plot_max << "" << " ";
	}
	if (out_mass) {
		//"SWE MS_Water MS_Wind MS_Rain MS_SN_Runoff MS_Soil_Runoff MS_Sublimation MS_Evap"
		plot_description << "snow_water_equivalent  total_amount_of_water  erosion_mass_loss  rain_rate  virtual_lysimeter  virtual_lysimeter_under_the_soil  sublimation_mass  evaporated_mass" << " ";
		plot_units << "kg/m2 kg/m2 kg/m2 kg/m2/h kg/m2/h kg/m2/h kg/m2 kg/m2" << " ";
		units_offset << "0 0 0 0 0 0 0 0" << " ";
		units_multiplier << "1 1 1 1 1 1 1 1" << " ";
		plot_color << "0x3300FF 0x0000FF 0x99CCCC 0x3333 0x0066CC 0x003366 0xCCFFFF 0xCCCCFF" << " ";
		plot_min << "" << " ";
		plot_max << "" << " ";
	}
	if (out_load) {
		//"load"
		plot_description << "solute_load" << " ";
		plot_units << "-" << " "; //HACK
		units_offset << "0" << " ";
		units_multiplier << "1" << " ";
		plot_color << "0xFF0000" << " ";
		plot_min << "" << " ";
		plot_max << "" << " ";
	}
	if (out_t && !fixedPositions.empty()) {
		// 40-49: Internal Temperature Time Series at fixed heights, modeled and measured, all in degC
		for (size_t ii = 0; ii < fixedPositions.size(); ii++) {
			plot_description << "temperature@" << fixedPositions[ii] << "m ";
			plot_units << "K ";
			units_offset << "273.15 ";
			units_multiplier << "1 ";
			plot_color << "0xFF0000" << " ";
			plot_min << "" << " ";
			plot_max << "" << " ";
		}
	}
	if (out_stab) {
		//"Sclass1 Sclass2 zSd Sd zSn Sn zSs Ss zS4 S4 zS5 S5"
		plot_description << "profile_type  stability_class  z_Sdef  deformation_rate_stability_index  z_Sn38  natural_stability_index  z_Sk38  Sk38_skier_stability_index  z_SSI  structural_stability_index  z_S5 stability_index_5" << " ";
		plot_units << "- - m - m - m - m - m -" << " ";
		units_offset << "0 0 0 0 0 0 0 0 0 0 0 0" << " ";
		units_multiplier << "1 1 1 1 1 1 1 1 1 1 1 1" << " ";
		plot_color << "0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000" << " ";
		plot_min << "" << " ";
		plot_max << "" << " ";
	}
	if (out_canopy) { //HACK
		plot_description << "Interception_storage Canopy_surface_temperature Canopy_albedo Wet_fraction Interception_capacity Net_shortwave_radiation_absorbed_by_canopy" << " "; //6
		plot_description << "Net_longwave_radiation_absorbed_by_canopy Net_radiation_to_canopy Sensible_heat_flux_to_canopy Latent_heat_flux_to_canopy" << " "; //4
		plot_description << "Biomass_heat_storage_flux_towards_Canopy Transpiration_of_the_canopy Evaporation_and_sublimation_of_interception_(liquid_and_frozen)" << " "; //3
		plot_description << "Interception_rate Throughfall Snow_unload Longwave_radiation_up_above_canopy Longwave_radiation_down_above_canopy" << " "; //5
		plot_description << "Shortwave_radiation_up_above_canopy Shortwave_radiation_down_above_canopy Total_land_surface_albedo" << " "; //3
		plot_description << "Total_net_radiation_to_the_surface_(ground_+_canopy) Surface_radiative_temperature_(ground_+_canopy)" << " "; //2
		plot_description << "Forest_floor_albedo Snowfall_rate_Above_Canopy Rainfall_rate_Above_Canopy Evapotranspiration_of_the_total_surface_(ground_+_canopy)" << " "; //4
		plot_units << "kg/m2 degC  - - kg/m2 W/m2 W/m2 W/m2 W/m2 W/m2 W/m2 kg/m2/timestep kg/m2/timestep kg/m2/timestep kg/m2/timestep kg/m2/timestep W/m2" << " "; //17
		plot_units << "W/m2 W/m2 W/m2 - W/m2 degC - kg/m2/timestep kg/m2/timestep kg/m2/timestep" << " "; //10
		units_offset << "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0" << " ";
		units_multiplier << "1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1" << " ";
		plot_color << "0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000" << " ";
		plot_color << "0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000" << " ";
		plot_color << "0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000" << " ";
		plot_min << "" << " ";
		plot_max << "" << " ";
	}
	if (Xdata.Seaice != NULL) {
		plot_description << "total_thickness  ice_thickness  snow_thickness  snow_thickness_wrt_ref  freeboard  sea_level  tot_salinity  avg_bulk_salinity  avg_brine_salinity  bottom_sal_flux  top_sal_flux  total_flooding_bucket_scheme" << " ";
		plot_units << "m m m m m m g/m2 g/kg g/kg g/m2 g/m2 kg/m2" << " ";
		units_offset << "0 0 0 0 0 0 0 0 0 0 0 0" << " ";
		units_multiplier << "1 1 1 1 1 1 1 1 1 1 1 1" << " ";
		plot_color << "0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000 0xFF0000" << " ";
		plot_min << "" << " ";
		plot_max << "" << " ";
	}

	smet_writer.set_header_value("units_offset", units_offset.str());
	smet_writer.set_header_value("units_multiplier", units_multiplier.str());
	smet_writer.set_header_value("plot_unit", plot_units.str());
	smet_writer.set_header_value("plot_description", plot_description.str());
	smet_writer.set_header_value("plot_color", plot_color.str());
	//smet_writer.set_header_value("plot_min", plot_min.str());
	//smet_writer.set_header_value("plot_max", plot_max.str());
}

void SmetIO::writeTimeSeriesData(const SnowStation& Xdata, const SurfaceFluxes& Sdata, const CurrentMeteo& Mdata, const ProcessDat& Hdata, const double &wind_trans24, smet::SMETWriter& smet_writer) const
{
	std::vector<std::string> timestamp( 1, Mdata.date.toString(mio::Date::ISO) );
	std::vector<double> data;

	const vector<NodeData>& NDS = Xdata.Ndata;
	const size_t nN = Xdata.getNumberOfNodes();
	const double cos_sl = Xdata.cos_sl;

	//data.push_back(  );
	if (out_heat) {
		data.push_back( Sdata.qs );
		data.push_back( Sdata.ql );
		data.push_back( Sdata.qg );
		data.push_back( IOUtils::K_TO_C(NDS[Xdata.SoilNode].T) );
		data.push_back( Sdata.qg0 );
		data.push_back( Sdata.qr );
	}

	if (out_lw) {
		data.push_back( Sdata.lw_out );
		data.push_back( Sdata.lw_in );
		data.push_back( Sdata.lw_net );
	}

	if (out_sw) {
		data.push_back( Sdata.sw_out );
		data.push_back( Sdata.sw_in );
		data.push_back( Sdata.qw );
		data.push_back( Sdata.pAlbedo );
		data.push_back( Sdata.mAlbedo );
		data.push_back( Sdata.sw_hor );
		data.push_back( Sdata.sw_dir );
		data.push_back( Sdata.sw_diff );
	}

	if (out_meteo) {
		data.push_back( IOUtils::K_TO_C(Mdata.ta) );
		data.push_back( IOUtils::K_TO_C(NDS[nN-1].T) );
		data.push_back( IOUtils::K_TO_C(Mdata.tss) );
		data.push_back( IOUtils::K_TO_C(NDS[0].T) );
		data.push_back( 100.*Mdata.rh );
		data.push_back( Mdata.vw );
		data.push_back( Mdata.vw_drift );
		data.push_back( Mdata.dw );
		data.push_back( Sdata.mass[SurfaceFluxes::MS_HNW] );
		data.push_back( M_TO_CM((Xdata.cH - Xdata.Ground)/cos_sl) );
		if (Xdata.mH!=Constants::undefined)
			data.push_back( M_TO_CM((Xdata.mH - Xdata.Ground)/cos_sl) );
		else
			data.push_back( IOUtils::nodata );
	}

	if (out_haz) {
		data.push_back( Hdata.hoar_size );
		data.push_back( wind_trans24 );
		data.push_back( (perp_to_slope? Hdata.hn24/cos_sl : Hdata.hn24) );
		data.push_back( (perp_to_slope? Hdata.hn72_24/cos_sl : Hdata.hn72_24) );
	}

	if (out_soileb) {
		const size_t nCalcSteps = static_cast<size_t>( ts_days_between / M_TO_D(calculation_step_length) + 0.5 );
		data.push_back( (Sdata.dIntEnergySoil * static_cast<double>(nCalcSteps)) / 1000. );
		data.push_back( (Sdata.meltFreezeEnergySoil * static_cast<double>(nCalcSteps)) / 1000. );
		data.push_back( Xdata.ColdContentSoil/1e6 );
	}

	if (out_mass) {
		data.push_back( Sdata.mass[SurfaceFluxes::MS_SWE]/cos_sl );
		data.push_back( Sdata.mass[SurfaceFluxes::MS_WATER]/cos_sl );
		data.push_back( Sdata.mass[SurfaceFluxes::MS_WIND]/cos_sl );
		data.push_back( Sdata.mass[SurfaceFluxes::MS_RAIN] );
		data.push_back( Sdata.mass[SurfaceFluxes::MS_SNOWPACK_RUNOFF]/cos_sl );
		data.push_back( (useSoilLayers? Sdata.mass[SurfaceFluxes::MS_SOIL_RUNOFF] / Xdata.cos_sl : IOUtils::nodata) );
		data.push_back( Sdata.mass[SurfaceFluxes::MS_SUBLIMATION]/cos_sl );
		data.push_back( Sdata.mass[SurfaceFluxes::MS_EVAPORATION]/cos_sl );
	}

	if (out_load) {
		if (!Sdata.load.empty())
			data.push_back( Sdata.load[0] );
		else
			data.push_back( IOUtils::nodata );
	}

	if (out_t && !fixedPositions.empty()) {
		for (size_t ii = 0; ii < fixedPositions.size(); ii++) {
			const double z = compPerpPosition(Mdata.zv_ts.at(ii), Xdata.cH, Xdata.Ground, Xdata.meta.getSlopeAngle());
			const double T = Mdata.ts.at(ii);
			const bool valid = (z!=Constants::undefined && (T != mio::IOUtils::nodata) && (z <= (Xdata.mH - min_depth_subsurf)));
			if (valid)
				data.push_back( IOUtils::K_TO_C(T) );
			else
				data.push_back( Constants::undefined );
		}
	}

	if (out_stab) {
		data.push_back( Xdata.S_class1 );
		data.push_back( Xdata.S_class2 );
		data.push_back( M_TO_CM(Xdata.z_S_d/cos_sl) );
		data.push_back( Xdata.S_d );
		data.push_back( M_TO_CM(Xdata.z_S_n/cos_sl) );
		data.push_back( Xdata.S_n );
		data.push_back( M_TO_CM(Xdata.z_S_s/cos_sl) );
		data.push_back( Xdata.S_s );
		data.push_back( M_TO_CM(Xdata.z_S_4/cos_sl) );
		data.push_back( Xdata.S_4 );
		data.push_back( M_TO_CM(Xdata.z_S_5/cos_sl) );
		data.push_back( Xdata.getLiquidWaterIndex() ); //Xdata.S_5 HACK
	}

	if (out_canopy) {
		// PRIMARY "STATE" VARIABLES
		data.push_back(  Xdata.Cdata.storage/cos_sl );        // intercepted water (mm or kg m-2)
		data.push_back( IOUtils::K_TO_C( Xdata.Cdata.temp) ); // temperature (degC)

		// SECONDARY "STATE" VARIABLES
		data.push_back(  Xdata.Cdata.canopyalb );             // albedo (1)
		data.push_back(  Xdata.Cdata.wetfraction );           // wet fraction
		data.push_back(  Xdata.Cdata.intcapacity/cos_sl );    // interception capacity (kg m-2)

		// RADIATIVE FLUXES (W m-2)
		data.push_back(  Xdata.Cdata.rsnet );                 // net shortwave radiation to canopy
		data.push_back(  Xdata.Cdata.rlnet );                 // net longwave radiation to canopy
		data.push_back(  Xdata.Cdata.rsnet+ Xdata.Cdata.rlnet );    // net radiation to canopy

		// HEAT FLUXES CANOPY (W m-2)
		data.push_back( - Xdata.Cdata.sensible );             // sensible heat flux to canopy (>0 towards canopy)
		data.push_back( - Xdata.Cdata.latentcorr );           // latent heat flux to canopy (>0 towards canopy)
		data.push_back(  Xdata.Cdata.CondFluxCanop );         // biomass heat storage flux towards Canopy

		// WATER FLUXES CANOPY (kg m-2)
		data.push_back(  Xdata.Cdata.transp/cos_sl );         // transpiration
		data.push_back(  Xdata.Cdata.intevap/cos_sl );        // interception evaporation
		data.push_back(  Xdata.Cdata.interception/cos_sl );   // interception
		data.push_back(  Xdata.Cdata.throughfall/cos_sl );    // throughfall
		data.push_back(  Xdata.Cdata.snowunload/cos_sl );     // unload of snow

		// TOTAL SURFACE FLUXES,EVAPORATION; ETC
		data.push_back(  Xdata.Cdata.rlwrac );                // upward longwave radiation ABOVE canopy
		data.push_back(  Xdata.Cdata.ilwrac );                // upward longwave radiation ABOVE canopy
		data.push_back(  Xdata.Cdata.rswrac );                // upward longwave radiation ABOVE canopy
		data.push_back(  Xdata.Cdata.iswrac );                // upward longwave radiation ABOVE canopy
		data.push_back(  Xdata.Cdata.totalalb );              // total albedo [-]
		data.push_back(  Xdata.Cdata.rlnet+Sdata.lw_net+ Xdata.Cdata.rsnet+Sdata.qw ); // net radiation to the total surface
		data.push_back( IOUtils::K_TO_C(pow( Xdata.Cdata.rlwrac/Constants::stefan_boltzmann, 0.25)) ); // surface (ground + canopy) temperature
		data.push_back(  Xdata.Cdata.forestfloor_alb );       // albedo of the forest floor [-]
		data.push_back(  Xdata.Cdata.snowfac/cos_sl );        // snowfall rate above canopy (mm per output timestep)
		data.push_back(  Xdata.Cdata.rainfac/cos_sl );        // rainfall rate above canopy (mm per output timestep)
		data.push_back(  (Xdata.Cdata.transp+ Xdata.Cdata.intevap-(Sdata.mass[SurfaceFluxes::MS_SUBLIMATION]+Sdata.mass[SurfaceFluxes::MS_EVAPORATION]))/cos_sl ); //evapotranspiration of total surface (mm h-1)
}

	if (Xdata.Seaice != NULL) {
		data.push_back( Xdata.cH - Xdata.Ground );
		data.push_back( Xdata.Ndata[Xdata.Seaice->IceSurfaceNode].z - Xdata.Ground );
		data.push_back( Xdata.Ndata[Xdata.getNumberOfNodes()-1].z - Xdata.Ndata[Xdata.Seaice->IceSurfaceNode].z );
		// Check reference level: either a marked reference level, or, if non existent, the sea level (if sea ice module is used), otherwise 0:
		const double ReferenceLevel = (  Xdata.findMarkedReferenceLayer()==IOUtils::nodata || !useReferenceLayer  )  ?  (  (Xdata.Seaice==NULL)?(0.):(Xdata.Seaice->SeaLevel)  )  :  (Xdata.findMarkedReferenceLayer() - Xdata.Ground);
		data.push_back( Xdata.Ndata[Xdata.getNumberOfNodes()-1].z - ReferenceLevel );
		data.push_back( Xdata.Seaice->FreeBoard );
		data.push_back( Xdata.Seaice->SeaLevel );
		data.push_back( Xdata.Seaice->getTotSalinity(Xdata) );
		data.push_back( Xdata.Seaice->getAvgBulkSalinity(Xdata) );
		data.push_back( Xdata.Seaice->getAvgBrineSalinity(Xdata) );
		data.push_back( Xdata.Seaice->BottomSalFlux );
		data.push_back( Xdata.Seaice->TopSalFlux );
		data.push_back( Sdata.mass[SurfaceFluxes::MS_FLOODING]/cos_sl );
	}

	smet_writer.write(timestamp, data);
}

void SmetIO::writeTimeSeries(const SnowStation& Xdata, const SurfaceFluxes& Sdata, const CurrentMeteo& Mdata,
                               const ProcessDat& Hdata, const double wind_trans24)
{
	const std::string filename( getFilenamePrefix(Xdata.meta.getStationID(), outpath) + ".smet" );
	std::map<std::string,smet::SMETWriter*>::iterator it = tsWriters.find(filename);

	if (it==tsWriters.end()) { //if it was not found, create it
		if (out_t)
			Mdata.getFixedPositions(fixedPositions);

		if (!FileUtils::validFileAndPath(filename)) //Check whether filename is valid
				throw InvalidNameException(filename, AT);

		if (FileUtils::fileExists(filename)) {
			tsWriters[filename] = new smet::SMETWriter(filename, getFieldsHeader(Xdata), IOUtils::nodata); //set to append mode
		} else {
			tsWriters[filename] = new smet::SMETWriter(filename);
			writeTimeSeriesHeader(Xdata, Mdata.date.getTimeZone(), *tsWriters[filename]);
		}
	}

	writeTimeSeriesData(Xdata, Sdata, Mdata, Hdata, wind_trans24, *tsWriters[filename]);
}

void SmetIO::writeProfile(const mio::Date& /*date*/, const SnowStation& /*Xdata*/)
{
	throw IOException("Nothing implemented here!", AT);
}

bool SmetIO::writeHazardData(const std::string& /*stationID*/, const std::vector<ProcessDat>& /*Hdata*/,
                             const std::vector<ProcessInd>& /*Hdata_ind*/, const size_t& /*num*/)
{
	throw IOException("Nothing implemented here!", AT);
}

// complete filename_prefix
std::string SmetIO::getFilenamePrefix(const std::string& fnam, const std::string& path, const bool addexp) const
{
	std::string filename_prefix( path + "/" + fnam );

	if (addexp && (experiment != "NO_EXP")) //NOTE usually, experiment == NO_EXP in operational mode
		filename_prefix += "_" + experiment;

	return filename_prefix;
}
