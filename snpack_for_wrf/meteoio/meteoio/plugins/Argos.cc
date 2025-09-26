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
#include <meteoio/plugins/Argos.h>
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
 * @page argosio ArgosIO
 * \attention This plugin is not functional yet!
 * 
 * This plugin deals with data that has been transmitted through the
 * <A HREF="http://www.argos-system.org/wp-content/uploads/2016/08/r286_9_argos3_metop_en.pdf">ARGOS</A> satellites. 
 * In order to reduce data
 * transfers, no headers are provided with the data and therefore all the metadata will have to be provided to the plugin.
 *
 * Currently, this plugin is only designed to \e read data in the ARGOS format.
 *
 * @section argos_format Format
 * This is a format where all values are encoded in Ascii on fixed-lenght lines. It is made of messages (ie one single
 * satellite transmission) that contain multiple timestamps. Each message starts some kind of a metadata line (for us, 
 * the deploy ID is the station ID:
 * @code
 * {program ID} {deploy ID} {number of lines in the message} {number of data fields per message line} {satellite id?}
 * @endcode
 * 
 * The each timestamp is made of the date and time (as "yyyy-mm-dd hh24:min:ss") followed by an unknown integer and 
 * {number of data fields} fields encoded into an integrer for each field.
 * @code
 * 01463 107283   9 15 K
 *       2018-04-24 00:48:15  2      29576         2017        10642        30576
 *                                   25129        24942        25170        51367
 *                                   51352        51342        51320        23932
 *                                   24270        31863        32053
 *       2018-04-24 00:51:35  2      29576        10395        10436        11967
 *                                   26750        27037        25254        24648
 *                                   51335        51320        51405        51386
 *                                   17238        17262        17635
 * @endcode
 * 
 * Since there are no headers, the decoded values need some user-provided metadata to be interpreted (see below in \ref goes_keywords).
 *
 * @section argos_keywords Keywords
 * This plugin uses the following keywords, all in the [Input] section:
 * - COORDSYS: coordinate system (see Coords);
 * - COORDPARAM: extra coordinates parameters (see Coords);
 * - TIMEZONE: timezone of the data;
 * - METEOPATH: directory where to read the data files from;
 * - FILE#: a filename to read the data from for each key; If no FILE# keywords are provided, all files with the right extension in METEOPATH will
 * be read.
 *     - ARGOS_EXT: extension of Argos data files to use when no FILE# keyword has been provided;
 *     - METEOPATH_RECURSIVE: when no FILE# keyword has been defined, should all files under METEOPATH be searched recursively? (default: false)
 * - ARGOS_DEBUG: should extra (ie very verbose) information be displayed? (default: false)
 * - METAFILE: an ini file that contains all the metadata, for each station that has to be read;
 *
 * The METAFILE is structured like an ini file with one section per Argos ID (ie per station). An extra [Default] section
 * can be defined so multiple stations sharing almost the same configuration can share some keys (the Argos ID section
 * keys have priority over the keys defined in [Default]). The following keys are defined:
 *  - ID: the station id (default: "Argos::" followed by Argos ID);
 *  - NAME: the station name (default: "Argos::" followed by Argos ID);
 *  - POSITION: the station coordinates, see \link Coords::Coords(const std::string& in_coordinatesystem, const std::string& in_parameters, std::string coord_spec) Coords()\endlink for the syntax;
 *  - UNITS_MULTIPLIER: factor to apply to each field to bring the value back to SI units (default: 1 for each field);
 *  - UNITS_OFFSET: offset to add to each field \b after applying the UNITS_MULTIPLIER, to bring the value back to SI units (default: 0 for each field);
 *  - FIELDS: the parameter name to use for each field;
 *  - WSL_HACK: support reading multiple messages containing information for the same station (used by some WSL stations)? (default: false)
 *  - if WSL_HACK has been set to \e true, there are also FIELDS2, UNITS_MULTIPLIER2, UNITS_OFFSET2 to describe a second, interlaced message.
 *
 * The FIELDS should take their names from MeteoData::meteoparam when possible, or can be "SKIP" in order to skip the matching parameter in the output.
 * Any other name will be used as is but won't be automatically recognized within MeteoIO.
 * 
 * Example metafile:
 * @code 
 * [107282]
 * fields = Station_nr SKIP SKIP SKIP ISWR RSWR SWR_Net TA RH VW
 * units_offset = 0 0 0 0 0 0 0 273.15 0 0
 * units_multiplier = 1 1 1 1 1 1 1 0.01 1 1
 * wsl_hack = true
 * 
 * fields2 = Station_nr DW P HS ISWR_MAX RSWR_MAX TA VW_MAX Battery_Voltage
 * units_offset2 = 0 0 0 0 0 0 273.15 0 0
 * position = latlon (46.8, 9.80, 1700)
 * id = ARG1
 * name = Swiss Camp 2
 * @endcode
 */

ArgosIO::ArgosIO(const std::string& configfile)
             : vecFilenames(), stations(), metaCfg(), meteopath(), coordin(), coordinparam(),
               in_TZ(0.), in_nodata(-8190.), debug(false)
{
	parseInputOutputSection( Config(configfile) );
}

ArgosIO::ArgosIO(const Config& cfgreader)
             : vecFilenames(), stations(), metaCfg(), meteopath(), coordin(), coordinparam(),
               in_TZ(0.), in_nodata(-8190.), debug(false)

{
	parseInputOutputSection( cfgreader );
}

void ArgosIO::parseInputOutputSection(const Config& cfg)
{
	cfg.getValue("TIME_ZONE", "Input", in_TZ);
	IOUtils::getProjectionParameters(cfg, coordin, coordinparam);
	
	cfg.getValue("ARGOS_DEBUG", "Input", debug, IOUtils::nothrow);
	cfg.getValue("ARGOS_NODATA", "Input", in_nodata, IOUtils::nothrow);
	const std::string metafile = cfg.get("METAFILE", "Input");
	metaCfg.addFile( metafile );

	cfg.getValue("METEOPATH", "Input", meteopath);

	cfg.getValues("FILE", "Input", vecFilenames);
	if (vecFilenames.empty()) { //no stations provided, then scan METEOPATH
		const std::string dflt_extension = cfg.get("ARGOS_EXT", "Input", "raw");
		const bool is_recursive = cfg.get("METEOPATH_RECURSIVE", "Input", false);
		std::list<std::string> dirlist( FileUtils::readDirectory(meteopath, dflt_extension, is_recursive) );
		dirlist.sort();
		vecFilenames.reserve( dirlist.size() );
		std::copy(dirlist.begin(), dirlist.end(), std::back_inserter(vecFilenames));
	}
}

void ArgosIO::readStationData(const Date& /*date*/, std::vector<StationData>& vecStation)
{
	vecStation.clear();

	//read all the stations' metadata
	const std::set<std::string> sections( metaCfg.getSections() );
	for (std::set<std::string>::const_iterator it=sections.begin(); it!=sections.end(); ++it) {
		if (*it=="DEFAULT") continue;
		addStation( *it );
	}

	//fill vecStations with the metadata
	for (std::map<std::string, ArgosStation>::const_iterator it=stations.begin(); it!=stations.end(); ++it)
		vecStation.push_back( it->second.getStationData() );
}

void ArgosIO::readMeteoData(const Date& dateStart, const Date& dateEnd,
                             std::vector< std::vector<MeteoData> >& vecMeteo)
{
	vecMeteo.clear();
	
	readRaw( meteopath+"/"+vecFilenames[0], dateStart, dateEnd, vecMeteo );
}

///////////////////////////////// Private methods

/**
 * @brief Create a new station in the caching maps
 * @details Extract and parse all metadata for the given station and fill the caches for it
 * @param[in] argosID The Argos ID of the station
 */
void ArgosIO::addStation(const std::string& argosID)
{
	const bool hasStation = metaCfg.sectionExists( argosID );
	if (hasStation){
		stations[ argosID ] = ArgosStation(argosID, metaCfg, in_nodata, in_TZ, coordin, coordinparam, debug);
	} else {
		stations[ argosID ] = ArgosStation();
		std::cerr << "Station " << argosID << " not configured, skipping\n";
	}
}

/**
 * @brief Read one line (terminated by \n) with a friendly fallback at the end of file
 * @param[in] fin stream to read from
 * @param[out] linenr current line number (for error messages)
 * @return line that has been read or empty string
 */
std::string ArgosIO::readLine(std::ifstream &fin, size_t &linenr)
{
	std::string line;
	getline(fin, line);
	linenr++;
	if (fin.eof()) return "";
	
	return line;
}

/**
 * @brief Read one data line .
 * @details This is from the point of view of Argos: it starts with a timestamps, spans multiple lines
 * and end when all fields have been read (or it is not formatted as expected anymore)
 * @param[in] fin stream to read from
 * @param[out] linenr current line number (for error messages)
 * @param[in] nFields expected number of fields
 * @param[in] station argos station ID (for error messages)
 * @param[in] dateStart expected start date
 * @param[in] dateEnd expected end date
 * @param[out] md read meteo data
 * @return true if everything went alright and the read data is valid, false otherwise
 */
bool ArgosIO::readTimestamp(std::ifstream &fin, size_t &linenr, const unsigned int& nFields, const ArgosStation& station, const Date& dateStart, const Date& dateEnd, MeteoData &md) const
{
	static const size_t first_data_field = 3; //first data field on timestamp line is #4
	static const size_t max_fields_per_line = 4;
	const size_t msg_start_timestamp = linenr;
	
	//parse the message content
	const std::string header( readLine(fin, linenr) );
	if (header.empty()) return false;
	if (!station.isValidMessage(nFields)) return false;
	//parse the timestamp
	if (header.substr(0,6)!="      ") return false; //the timestamp must be preceeded by 6 spaces
	std::vector<std::string> vecTmp;
	if (IOUtils::readLineToVec(header, vecTmp) < first_data_field ) //can it at least contain a timestamp?
		return false;
	
	const Date dt( station.parseDate(vecTmp[0]+" "+vecTmp[1], linenr) );
	if (dt.isUndef() || dt<dateStart || dt>dateEnd) return false;

	//parse the data section on the header line
	if ((vecTmp.size()-first_data_field) > nFields) return false;
	if ((nFields>=max_fields_per_line) && (vecTmp.size()-first_data_field) < max_fields_per_line) return false;
	
	std::vector<unsigned int> raw(nFields);
	for (size_t ii=first_data_field; ii<vecTmp.size(); ii++) 
		if (!IOUtils::convertString( raw[ii-first_data_field], vecTmp[ii] )) return false;
	
	//read all other data lines
	const size_t nrTimestampLines = (nFields + max_fields_per_line) / max_fields_per_line - 1; //rounding up
	for (size_t jj=1; jj<=nrTimestampLines; jj++) { //the first data line has been read with the date
		const std::string data_line( readLine(fin, linenr) );
		if (data_line.empty()) return false;
		if (data_line.substr(0,6)!="      ") return false; //very primitive format validation
		if (IOUtils::readLineToVec(data_line, vecTmp) > max_fields_per_line ) return false; //too many fields
		
		for (size_t ii=0; ii<vecTmp.size(); ii++) 
			if (!IOUtils::convertString( raw[ii+max_fields_per_line*jj], vecTmp[ii] )) return false;
	}
	
	md = station.parseDataLine(dt, raw, msg_start_timestamp);
	return true;
}

//read one message, that starts with a header followed by several timestamps
void ArgosIO::readMessage(std::ifstream &fin, size_t &linenr, const Date& dateStart, const Date& dateEnd, std::vector< std::vector<MeteoData> >& vecMeteo)
{
	static const size_t nr_header_fields = 5;
	const size_t msg_start_linenr = linenr;
	
	//parse given header
	//expected structure: {id} {stationID} {#of lines in message} {#of fields per timestamp} {satellite ID?}
	std::string line;
	getline(fin, line);
	linenr++;

	if (line[0]==' ') return; //a new message starts with a station_id, not spaces
	std::vector<std::string> vecTmp;
 	if (IOUtils::readLineToVec(line, vecTmp) != nr_header_fields ) return;
	
	const std::string ArgosID( vecTmp[1] );
	unsigned int nMessageLines, nFields;
	if (!IOUtils::convertString(nMessageLines, vecTmp[2])) return;
	if (!IOUtils::convertString(nFields, vecTmp[3])) return;
	
	if (stations.count( ArgosID )==0) addStation( ArgosID ); //add station into vecMeteo
	if (!stations[ ArgosID ].isValid()) return;
	size_t meteoIdx = stations[ ArgosID ].meteoIdx;
	if (meteoIdx==IOUtils::npos) {
		meteoIdx = vecMeteo.size();
		stations[ ArgosID ].meteoIdx = meteoIdx;
		vecMeteo.push_back( std::vector<MeteoData>() );
	}
	
	while ((linenr-msg_start_linenr) < nMessageLines){
		MeteoData md;
		if ( !readTimestamp(fin, linenr, nFields, stations[ ArgosID ], dateStart, dateEnd, md) ) return;
		
		if (vecMeteo[ meteoIdx ].empty()) {
			vecMeteo[ meteoIdx ].push_back( md );
		} else {
			if (md.date > vecMeteo[ meteoIdx ].back().date) {
				vecMeteo[ meteoIdx ].push_back( md );
			} else {
				for (size_t ii=vecMeteo[ meteoIdx ].size(); ii --> 0 ;) {
					if (md.date==vecMeteo[ meteoIdx ][ii].date) break;

					if (md.date > vecMeteo[ meteoIdx ][ii].date) {
						vecMeteo[ meteoIdx ].insert( vecMeteo[ meteoIdx ].begin() + (ii+1), md);
						break;
					}
				}
			}
		}
	}
}

void ArgosIO::readRaw(const std::string& file_and_path, const Date& dateStart, const Date& dateEnd, std::vector< std::vector<MeteoData> >& vecMeteo)
{
	if (!FileUtils::validFileAndPath(file_and_path)) //Check whether filename is valid
		throw InvalidNameException(file_and_path, AT);

	//make sure vecMeteo is empty and all stations' positions within vecMeteo are npos
	vecMeteo.clear();
	for (std::map<std::string, ArgosStation>::iterator it = stations.begin(); it != stations.end(); ++it)
		it->second.meteoIdx = IOUtils::npos;
	
	errno = 0;
	std::ifstream fin(file_and_path.c_str(), ios::in);
	if (fin.fail()) {
		std::ostringstream ss;
		ss << "Error opening file \"" << file_and_path << "\" for reading, possible reason: " << std::strerror(errno);
		ss << " Please check file existence and permissions!";
		throw AccessException(ss.str(), AT);
	}

	size_t linenr = 0;
	while (!fin.eof()){
		readMessage( fin, linenr, dateStart, dateEnd, vecMeteo ); //if a message header is not found, it will retrun after reading 1 line
	}
	
	fin.close();
}


ArgosStation::ArgosStation()
                    : meteoIdx(IOUtils::npos), fields_idx(), units_offset(), units_multiplier(), 
                    fields_idx2(), units_offset2(), units_multiplier2(), md_template(), TZ(0.), nodata(0.),
                    validStation(false), debug(false), wsl_hack(false)
{}

ArgosStation::ArgosStation(const std::string& argosID, const Config& metaCfg, const float& in_nodata, const double& in_TZ, const std::string& coordin, const std::string& coordinparam, const bool& isDebug)
                    : meteoIdx(IOUtils::npos), fields_idx(), units_offset(), units_multiplier(), 
                    fields_idx2(), units_offset2(), units_multiplier2(), md_template(), TZ(in_TZ), nodata(in_nodata),
                    validStation(true), debug(isDebug), wsl_hack(false)
{
	//construct the StationData for this station
	const std::string station_id = metaCfg.get("ID", argosID, "Argos::"+argosID);
	const std::string station_name = metaCfg.get("NAME", argosID, "Argos::"+argosID);
	const std::string section_position = (metaCfg.keyExists("position", argosID))? argosID : "default";
	const std::string position_spec = metaCfg.get("POSITION", section_position);
	const Coords loc(coordin, coordinparam, position_spec);
	const StationData sd(loc, station_id, station_name);

	//identify all fields for this station and build the MeteoData template
	const std::string section_fields = (metaCfg.keyExists("fields", argosID))? argosID : "default";
	const std::vector<std::string> fields_str = metaCfg.get("FIELDS", section_fields);
	md_template.meta = sd;
	parseFieldsSpecs(fields_str, md_template, fields_idx);

	//construct the units_offset
	const std::string section_offsets = (metaCfg.keyExists("units_offset", argosID))? argosID : "default";
	if (metaCfg.keyExists("UNITS_OFFSET", section_offsets)) {
		metaCfg.getValue("UNITS_OFFSET", section_offsets, units_offset);
	} else {
		units_offset = std::vector<double>(fields_idx.size(), 0.);
	}

	//construct the section_multipliers
	const std::string section_multipliers = (metaCfg.keyExists("units_multiplier", argosID))? argosID : "default";
	if (metaCfg.keyExists("UNITS_MULTIPLIER", section_multipliers)) {
		metaCfg.getValue("UNITS_MULTIPLIER", section_multipliers, units_multiplier);
	} else {
		units_multiplier = std::vector<double>(fields_idx.size(), 1.);
	}
	
	wsl_hack = metaCfg.get("wsl_hack", argosID, false);
	if (wsl_hack) {
		const std::vector<std::string> fields_str2 = metaCfg.get("FIELDS2", section_fields);
		parseFieldsSpecs(fields_str2, md_template, fields_idx2);
		if (metaCfg.keyExists("UNITS_OFFSET2", section_offsets)) {
			metaCfg.getValue("UNITS_OFFSET2", section_offsets, units_offset2);
		} else {
			units_offset2 = std::vector<double>(fields_idx2.size(), 0.);
		}
		if (metaCfg.keyExists("UNITS_MULTIPLIER2", section_multipliers)) {
			metaCfg.getValue("UNITS_MULTIPLIER2", section_multipliers, units_multiplier2);
		} else {
			units_multiplier2 = std::vector<double>(fields_idx2.size(), 1.);
		}
	}
}

void ArgosStation::parseFieldsSpecs(const std::vector<std::string>& fieldsNames, MeteoData &meteo_template, std::vector<size_t> &idx)
{
	idx.resize(fieldsNames.size(), IOUtils::npos);

	for (size_t ii=0; ii<fieldsNames.size(); ii++) {
		const std::string parname( IOUtils::strToUpper(fieldsNames[ii]) );
		if (parname=="SKIP") continue;

		const size_t curr_idx = meteo_template.getParameterIndex(parname);
		if (curr_idx!=IOUtils::npos) idx[ii] = curr_idx;
		else idx[ii] = meteo_template.addParameter(parname);
	}
}

std::vector<float> ArgosStation::decodeData(const std::vector<unsigned int> &raw)
{
	const size_t nFields = raw.size();
	std::vector<int> In(nFields);
	std::vector<float> out(nFields, static_cast<float>(IOUtils::nodata));
	
	for (size_t ii=0; ii<nFields; ii++) {
		int binval = raw[ii];
		for (size_t kk=0; kk<nFields; kk++) {
			In[kk] = 0;
			const int powerOfTwo = (1 << (15-kk));
			if (binval >= powerOfTwo) {
				In[kk] = 1;
				binval -= powerOfTwo;
			}
		}
		
		int nval = 0;
		for (size_t kk=3; kk<nFields; kk++) {
			if (In[kk]==1) {
				const int powerOfTwo = (1 << (15-kk));
				nval += powerOfTwo;
			}
		}
		
		if (In[0]==1) nval *= -1;
		if (In[1]==0 && In[2]==0) out[ii] = static_cast<float>( nval );
		if (In[1]==0 && In[2]==1) out[ii] = static_cast<float>( nval ) * 0.1f;
		if (In[1]==1 && In[2]==0) out[ii] = static_cast<float>( nval ) * 0.01f;
		if (In[1]==1 && In[2]==1) out[ii] = static_cast<float>( nval ) * 0.001f;
	}
	
	return out;
}

Date ArgosStation::parseDate(const std::string& str, const size_t &linenr) const
{
	int year, month, day, hour, minute, second;
	if (sscanf(str.c_str(), "%d-%d-%d %d:%d:%d", &year, &month, &day, &hour, &minute, &second) != 6) return Date();
	try {
		const Date dt(year, month, day, hour, minute, second, TZ);
		return dt;
	} catch( const IOException&) {
		throw InvalidFormatException("Could not parse date '"+str+"' at line "+IOUtils::toString(linenr), AT);
	}
	
	return Date();
}

MeteoData ArgosStation::parseDataLine(const Date& dt, const std::vector<unsigned int>& raw_data, const size_t& linenr) const
{
	const size_t nFields = raw_data.size();
	if (nFields!=fields_idx.size()) {
		ostringstream ss;
		ss << "Number of user-declared fields (" << fields_idx.size() << ") don't match with ARGOS message ";
		ss << "number of fields (" << nFields << ") starting at line " << linenr;
		throw InvalidArgumentException(ss.str(), AT);
	}

	MeteoData md( md_template );
	md.date.setDate(dt);
	
	const bool wsl_second_row = (wsl_hack && (raw_data[1]!=(unsigned)(dt.getYear())));

	const std::vector<float> decoded( decodeData( raw_data ) );
	if (!wsl_second_row) {
		for (size_t ii=0; ii<nFields; ii++) {
			const size_t idx = fields_idx[ii];
			if (idx==IOUtils::npos) continue;
			if (decoded[ii] == nodata) continue;
			md( idx ) = static_cast<double>(decoded[ii]) * units_multiplier[ii] + units_offset[ii];
		}
	} else {
		for (size_t ii=0; ii<nFields; ii++) {
			const size_t idx = fields_idx2[ii];
			if (idx==IOUtils::npos) continue;
			if (decoded[ii] == nodata) continue;
			md( idx ) = static_cast<double>(decoded[ii]) * units_multiplier2[ii] + units_offset2[ii];
		}
	}
	
	if (debug) {
		if (wsl_hack) {
			if (!wsl_second_row)
				std::cout << "1st row decoded: " << dt.toString(Date::ISO);
			else 
				std::cout << "2nd row decoded: " << dt.toString(Date::ISO);
		} else {
			std::cout << "decoded: " << dt.toString(Date::ISO);
		}
		for (size_t ii=0; ii<nFields; ii++) std::cout << " " << decoded[ii];
		std::cout << "\n";
	}

	return md;
}

} //namespace
