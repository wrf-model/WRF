/***********************************************************************************/
/*  Copyright 2019 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#include <meteoio/plugins/Goes.h>
#include <meteoio/FileUtils.h>

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <cstring>
#include <cerrno>
#include <cstdlib>

using namespace std;

namespace mio {
/**
 * @page goesio GoesIO
 * This plugin deals with data that has been transmitted through the
 * <A HREF="https://en.wikipedia.org/wiki/Geostationary_Operational_Environmental_Satellite">GOES</A> satellites
 * (see also https://www.goes-r.gov/resources/docs.html and https://www.rtl-sdr.com/tag/goes/). In order to reduce data
 * transfers, no headers are provided with the data and therefore all the metadata will have to be provided to the plugin.
 *
 * Currently, this plugin is only designed to \e read data in the GOES format.
 *
 * @section goes_format Format
 * This is a format where all values are encoded in Ascii on fixed-lenght lines. The first 35 characters are related to the station ID
 * and the characters starting at position 37 until the end of the line encode the data. An example is given below:
 * @code
 * 8030011818095204101G44+1NN067EXE00143 FOh@_bBNvD_PFLhFbqFB@I~I~I~OQDOTROX{I~ONqOvaLMQLSpLS[LSWLRfBN[BOGFgsFmYBYlBZ`CKkI~FJ}FPBFA[F@|LS\LR~LS~LSiFykGAUFFhFGELPlI~NErLRGLQcDUg
 * 8030011818095214101G44+1NN067EXE00143 FOh@_bBNvD`tFGdFTnFB^I~I~I~OQAOTMOX{I~ONjOveLMQLTXLTKLT@LS[BNFBODFseFzgBZBBZtCKkI~FJ}FJaFAYF@hLSnLS`LUbLUFGRXG[FLDFMHLRBI~NEFLSTLRwDUQ 
 * 8030011818095224101G44+0NN067EXE00143 FOh@_bBNvDbXFC[FITFBSI~I~I~OQBOTMOX{I~ONbOvoLMSLUkLU`LU]LTzBN_BORFc]FiaBWaBXBCKjF@@FJ{FEFFAHF@^LT{LTkLVLLVEFusF|BFD}FEJLS~I~NCKLUILTpDTz
 * @endcode
 * 
 * Since there are no headers, the decoded values need some user-provided metadata to be interpreted (see below in \ref goes_keywords).
 *
 * @section goes_keywords Keywords
 * This plugin uses the following keywords, all in the [Input] section:
 * - COORDSYS: coordinate system (see Coords);
 * - COORDPARAM: extra coordinates parameters (see Coords);
 * - TIMEZONE: timezone of the data;
 * - METEOPATH: directory where to read the data files from;
 * - METEOFILE#: a filename to read the data from for each key; If no FILE# keywords are provided, all files with the right extension in METEOPATH will
 * be read.
 *     - GOES_EXT: extension of Goes data files to use when no FILE# keyword has been provided;
 *     - METEOPATH_RECURSIVE: when no FILE# keyword has been defined, should all files under METEOPATH be searched recursively? (default: false)
 * - GOES_NODATA: value used to represent nodata (default: -8190);
 * - GOES_ONLYFROMPAST: if set to true, data points beyond the current date and time will be rejected as invalid and reading will continue (default: true);
 * - GOES_DEBUG: should extra (ie very verbose) information be displayed? (default: false)
 * - METAFILE: an ini file that contains all the metadata, for each station that has to be read;
 *
 * The METAFILE is structured like an ini file with one section per Goes ID (ie per station). An extra [Default] section
 * can be defined so multiple stations sharing almost the same configuration can share some keys (the Goes ID section
 * keys have priority over the keys defined in [Default]). The following keys are defined:
 *  - ID: the station id (default: "Goes::" followed by Goes ID);
 *  - NAME: the station name (default: "Goes::" followed by Goes ID);
 *  - POSITION: the station coordinates, see \link Coords::Coords(const std::string& in_coordinatesystem, const std::string& in_parameters, std::string coord_spec) Coords()\endlink for the syntax;
 *  - UNITS_MULTIPLIER: factor to apply to each field to bring the value back to SI units (default: 1 for each field);
 *  - UNITS_MULTIPLIER_NEG: factor to apply to each field to bring the value back to SI units, \b when the raw data is \b negative (default: same as UNITS_MULTIPLIER);
 *  - UNITS_OFFSET: offset to add to each field \b after applying the UNITS_MULTIPLIER, to bring the value back to SI units (default: 0 for each field);
 *  - FIELDS: the parameter name to use for each field (the given number of parameters defines which lines are valid or invalid);
 *
 * The FIELDS should take their names from MeteoData::meteoparam when possible, or be either of the following: "STATIONID", "YEAR", "JDN", "HOUR", "SKIP".
 * Any other name will be used as is but won't be automatically recognized within MeteoIO.
 */

//this is fixed by GOES
static const size_t dataStartPos = 37;

GoesIO::GoesIO(const std::string& configfile)
             : vecFilenames(), stations(), metaCfg(), meteopath(), coordin(), coordinparam(),
               in_TZ(0.), in_nodata(-8190.), debug(false), OnlyFromPast(true)
{
	parseInputOutputSection( Config(configfile) );
}

GoesIO::GoesIO(const Config& cfgreader)
             : vecFilenames(), stations(), metaCfg(), meteopath(), coordin(), coordinparam(),
               in_TZ(0.), in_nodata(-8190.), debug(false), OnlyFromPast(true)

{
	parseInputOutputSection( cfgreader );
}

void GoesIO::parseInputOutputSection(const Config& cfg)
{
	cfg.getValue("TIME_ZONE", "Input", in_TZ);
	IOUtils::getProjectionParameters(cfg, coordin, coordinparam);
	
	cfg.getValue("GOES_DEBUG", "Input", debug, IOUtils::nothrow);
	cfg.getValue("GOES_NODATA", "Input", in_nodata, IOUtils::nothrow);
	cfg.getValue("GOES_ONLYFROMPAST", "Input", OnlyFromPast, IOUtils::nothrow);
	const std::string metafile = cfg.get("METAFILE", "Input");
	metaCfg.addFile( metafile );
	cfg.getValue("METEOPATH", "Input", meteopath);

	cfg.getValues("METEOFILE", "Input", vecFilenames);
	if (vecFilenames.empty()) { //no stations provided, then scan METEOPATH
		const std::string dflt_extension = cfg.get("GOES_EXT", "Input", "raw");
		const bool is_recursive = cfg.get("METEOPATH_RECURSIVE", "Input", false);
		std::list<std::string> dirlist( FileUtils::readDirectory(meteopath, dflt_extension, is_recursive) );
		dirlist.sort();
		vecFilenames.reserve( dirlist.size() );
		std::copy(dirlist.begin(), dirlist.end(), std::back_inserter(vecFilenames));
	}
}

void GoesIO::readStationData(const Date& /*date*/, std::vector<StationData>& vecStation)
{
	vecStation.clear();
	
	//read all the stations' metadata
	const std::set<std::string> sections( metaCfg.getSections() );
	for (std::set<std::string>::const_iterator it=sections.begin(); it!=sections.end(); ++it) {
		if (*it=="DEFAULT") continue;
		addStation( *it );
	}
	
	//fill vecStations with the metadata
	for (std::map<std::string, GoesStation>::const_iterator it=stations.begin(); it!=stations.end(); ++it)
		vecStation.push_back( it->second.getStationData() );
}

void GoesIO::readMeteoData(const Date& dateStart, const Date& dateEnd,
                             std::vector< std::vector<MeteoData> >& vecMeteo)
{
	vecMeteo.clear();
	for (std::map<std::string, GoesStation>::iterator it = stations.begin(); it!=stations.end(); ++it)
		it->second.meteoIdx = IOUtils::npos; //reset positions in vecMeteo

	if (vecFilenames.size()==1) {
		readRaw( meteopath+"/"+vecFilenames[0], dateStart, dateEnd, vecMeteo );
		return;
	}

	for (size_t ii=0; ii<vecFilenames.size(); ii++) {
		std::vector< std::vector<MeteoData> > vecTmp;
		readRaw( meteopath+"/"+vecFilenames[ii], dateStart, dateEnd, vecTmp );

		//merge the tmp data into vecMeteo
		for (size_t st=0; st<vecTmp.size(); st++) {
			if (vecTmp[st].empty()) continue;
			const std::string fromID( IOUtils::strToUpper(vecTmp[st][0].meta.stationID) );

			bool found = false;
			for (size_t jj=0; jj<vecMeteo.size(); jj++) {
				if (vecMeteo[jj].empty()) continue; //This should not happen!
				const std::string curr_station( IOUtils::strToUpper(vecMeteo[jj][0].meta.stationID) );
				if (curr_station==fromID) {
					MeteoData::mergeTimeSeries(vecMeteo[jj], vecTmp[st], MeteoData::FULL_MERGE); //merge timeseries for the two stations
					found = true;
				}
			}

			if (!found)
				vecMeteo.push_back( vecTmp[st] );
		}
	}
}

void GoesIO::readRaw(const std::string& file_and_path, const Date& dateStart, const Date& dateEnd, std::vector< std::vector<MeteoData> >& vecMeteo)
{
	if (!FileUtils::validFileAndPath(file_and_path)) //Check whether filename is valid
		throw InvalidNameException(file_and_path, AT);

	errno = 0;
	std::ifstream fin(file_and_path.c_str(), ios::in);
	if (fin.fail()) {
		std::ostringstream ss;
		ss << "Error opening file \"" << file_and_path << "\" for reading, possible reason: " << std::strerror(errno);
		ss << " Please check file existence and permissions!";
		throw AccessException(ss.str(), AT);
	}

	//this is required for the OnlyFromPast option
	Date now;
	now.setFromSys();
	
	std::vector<float> raw_data;
	while (!fin.eof()){
		std::string line;
		getline(fin, line);
		if (line.length()<dataStartPos) continue;

		const std::string goesID( line.substr(0, 8) ); //only first 8 characters of the station substring
		if (goesID[0]!='8') continue; //invalid GOES ID, this must be an invalid line
		if (stations.count( goesID )==0) addStation(goesID); //create the station if necessary (ie pulling its user-defined metadata)
		if (!stations[ goesID ].isValid()) continue; //this station has not been configured by the user
		const size_t nElems = stations[ goesID ].getNElems(); //getting the expected number of elements
		
		const std::string data_section( line.substr(dataStartPos) );
		if (data_section.length()<=(3*nElems)) { //the line is invalid
			if (debug)
				std::cout << goesID << ", line is " << data_section.length() << " chars long, expecting > " << 3*nElems << " -> rejecting\n";
			continue;
		}
		raw_data.resize(nElems);
		
		for (size_t ii=1; ii<=nElems; ii++) {
			raw_data[ii-1] = 0;
			const int A = data_section[3*ii-2] & 15;
			const int B = data_section[3*ii-1] & 63;
			const int C = data_section[3*ii-0] & 63;

			if ((A*64+B) > 1008) {
				raw_data[ii-1] = static_cast<float>( (B-48)*64 + C + 9000 );
				continue;
			}

			float SF = ((A & 8) != 0)? -1 : 1;
			if ((A & 4) != 0) SF *= 0.01f;
			if ((A & 2) != 0) SF *= 0.1f;
			if ((A & 1) != 0) raw_data[ii-1] = 4096.;

			raw_data[ii-1] = (raw_data[ii-1] + static_cast<float>((B & 63)*64 + (C & 63))) * SF;
		}

		//parsing date
		const Date dt( stations[ goesID ].parseDate(raw_data) );
		if (dt.isUndef()) continue; //that was an invalid line
		if (OnlyFromPast && dt>now) continue; //this is also an invalid line
		if (dt<dateStart) continue;
		if (dt>dateEnd) {
			fin.close();
			return;
		}
		
		//get/refresh the current station's index
		size_t st_idx = stations[ goesID ].meteoIdx;
		if (st_idx==IOUtils::npos) { //there was a rebuffer, we need to refresh the index
			st_idx = vecMeteo.size();
			stations[ goesID ].meteoIdx = st_idx;
			vecMeteo.push_back( std::vector<MeteoData>() );
		}

		const MeteoData md( stations[ goesID ].parseDataLine(dt, raw_data) );
		if (vecMeteo[st_idx].size() > 0 && md.date<=vecMeteo[st_idx].back().date) continue;
		vecMeteo[ st_idx ].push_back( md );
		
		if (debug) {
			std::cout << goesID << " " << dt.toString(Date::ISO) << " -> ";
			for (size_t ii=0; ii<raw_data.size(); ii++) std::cout << "   " << raw_data[ii];
			std::cout << "\n";
		}
	}
	
	fin.close();
}

/**
 * @brief Create a new station in the caching maps
 * @details Extract and parse all metadata for the given station and fill the caches for it
 * @param[in] goesID The Goes ID of the station
 */
void GoesIO::addStation(const std::string& goesID)
{
	const bool hasStation = metaCfg.sectionExists( goesID );
	if (hasStation) {
		stations[ goesID ] = GoesStation(goesID, metaCfg, in_nodata, in_TZ, coordin, coordinparam);
	} else {
		stations[ goesID ] = GoesStation();
		std::cerr << "Station " << goesID << " not configured, skipping\n";
	}
}


GoesStation::GoesStation()
                    : meteoIdx(IOUtils::npos), fields_idx(), units_offset(), units_multiplier(), units_multiplier_neg(), md_template(), TZ(0.), nodata(0.),
                    stationID_idx(IOUtils::npos), year_idx(IOUtils::npos), hour_idx(IOUtils::npos), jdn_idx(IOUtils::npos), nElems(0), validStation(false)
{}

GoesStation::GoesStation(const std::string& goesID, const Config& metaCfg, const float& in_nodata, const double& in_TZ, const std::string& coordin, const std::string& coordinparam)
                    : meteoIdx(IOUtils::npos), fields_idx(), units_offset(), units_multiplier(), units_multiplier_neg(), md_template(), TZ(in_TZ), nodata(in_nodata),
                    stationID_idx(IOUtils::npos), year_idx(IOUtils::npos), hour_idx(IOUtils::npos), jdn_idx(IOUtils::npos), nElems(0), validStation(true)
{
	//construct the StationData for this station
	const std::string station_id = metaCfg.get("ID", goesID, "Goes::"+goesID);
	const std::string station_name = metaCfg.get("NAME", goesID, "Goes::"+goesID);
	const std::string section_position = (metaCfg.keyExists("position", goesID))? goesID : "default";
	const std::string position_spec = metaCfg.get("POSITION", section_position);
	const Coords loc(coordin, coordinparam, position_spec);
	const StationData sd(loc, station_id, station_name);

	//identify all fields for this station and build the MeteoData template
	const std::string section_fields = (metaCfg.keyExists("fields", goesID))? goesID : "default";
	const std::vector<std::string> fields_str = metaCfg.get("FIELDS", section_fields);
	nElems = fields_str.size();
	
	md_template.meta = sd;
	parseFieldsSpecs(fields_str, md_template, fields_idx);

	//construct the units_offset
	const std::string section_offsets = (metaCfg.keyExists("units_offset", goesID))? goesID : "default";
	if (metaCfg.keyExists("UNITS_OFFSET", section_offsets)) {
		metaCfg.getValue("UNITS_OFFSET", section_offsets, units_offset);
	} else {
		units_offset = std::vector<double>(fields_idx.size(), 0.);
	}

	//construct the section_multipliers
	const std::string section_multipliers = (metaCfg.keyExists("units_multiplier", goesID))? goesID : "default";
	if (metaCfg.keyExists("UNITS_MULTIPLIER", section_multipliers)) {
		metaCfg.getValue("UNITS_MULTIPLIER", section_multipliers, units_multiplier);
	} else {
		units_multiplier = std::vector<double>(fields_idx.size(), 1.);
	}
	
	//construct the multipliers_neg
	const std::string section_multipliers_neg = (metaCfg.keyExists("units_multiplier_neg", goesID))? goesID : "default";
	if (metaCfg.keyExists("UNITS_MULTIPLIER_NEG", section_multipliers_neg)) {
		metaCfg.getValue("UNITS_MULTIPLIER_NEG", section_multipliers_neg, units_multiplier_neg);
	} else {
		units_multiplier_neg = units_multiplier;
	}
}

void GoesStation::parseFieldsSpecs(const std::vector<std::string>& fieldsNames, MeteoData &meteo_template, std::vector<size_t> &idx)
{
	idx.resize(fieldsNames.size(), IOUtils::npos);

	for (size_t ii=0; ii<fieldsNames.size(); ii++) {
		const std::string parname( IOUtils::strToUpper(fieldsNames[ii]) );
		if (parname=="SKIP") continue;

		if (parname=="STATIONID") {
			stationID_idx=ii;
			continue;
		}
		if (parname=="YEAR") {
			year_idx=ii;
			continue;
		}
		if (parname=="JDN") {
			jdn_idx=ii;
			continue;
		}
		if (parname=="HOUR") {
			hour_idx=ii;
			continue;
		}

		const size_t curr_idx = meteo_template.getParameterIndex(parname);
		if (curr_idx!=IOUtils::npos) idx[ii] = curr_idx;
		else idx[ii] = meteo_template.addParameter(parname);
	}
}

Date GoesStation::parseDate(const std::vector<float>& raw_data) const
{
	const double jdn = static_cast<double>(raw_data[ jdn_idx ]) + static_cast<double>(raw_data[ hour_idx ])/24.;
	const int year = static_cast<int>(raw_data[ year_idx ]);
	return Date(year, jdn, TZ);
}

MeteoData GoesStation::parseDataLine(const Date& dt, const std::vector<float>& raw_data) const
{
	MeteoData md( md_template );
	md.date.setDate(dt);

	for (size_t ii=0; ii<raw_data.size(); ii++) {
		const size_t idx = fields_idx[ii];
		if (idx==IOUtils::npos) continue;
		if (raw_data[ii] == nodata) continue;

		if (raw_data[ii]>=0)
			md( idx ) = static_cast<double>(raw_data[ii]) * units_multiplier[ii] + units_offset[ii];
		else
			md( idx ) = static_cast<double>(raw_data[ii]) * units_multiplier_neg[ii] + units_offset[ii];
	}

	return md;
}

} //namespace
