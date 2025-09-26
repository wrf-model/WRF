/***********************************************************************************/
/*  Copyright 2017 SLF                                                                                                                                */
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
#include <meteoio/plugins/DBO.h>

#include <meteoio/dataClasses/Coords.h>
#include <meteoio/IOExceptions.h>
#include <meteoio/meteoLaws/Meteoconst.h>

#include <algorithm>
#include <sstream>
#include <iostream>
#include <cstring>

#include <curl/curl.h>
#include <meteoio/thirdParty/picojson.h>

using namespace std;

namespace mio {
/**
 * @page dbo DBO
 * @section dbo_format Format
 * This plugin reads meteorological data from DBO
 * via the RESTful web service. To compile the plugin you need to have the <a href="http://curl.haxx.se/">CURL library</a> with its headers present.
 *
 * @section dbo_keywords Keywords
 * This plugin uses the following keywords:
 * - DBO_URL: The URL of the RESTful web service e.g. http://developwis.wsl.ch:8730
 * - STATION#: station code for the given station, prefixed by the network it belongs ot (for example: IMIS::SLF2)
 * - DBO_TIMEOUT: timeout (in seconds) for the connection to the server (default: 60s)
 * - DBO_DEBUG: print the full requests/answers from the server when something does not work as expected (default: false)
 *
 * @code
 * METEO	= DBO
 * DBO_URL	= http://developwis.wsl.ch:8730
 * STATION1	= WFJ2
 * STATION2	= DAV3
 * @endcode
 *
 * @section dbo_dependencies Picojson
 * This plugin relies on <A HREF="https://github.com/kazuho/picojson/">picojson</A> for reading and parsing
 * <A HREF="https://en.wikipedia.org/wiki/JSON">JSON</A> data. Picojson is released under a
 * <A HREF="https://opensource.org/licenses/BSD-2-Clause">2-Clause BSD License</A>. Please find here below
 * the full license agreement for picojson:
 *
 * @code
 * Copyright 2009-2010 Cybozu Labs, Inc.
 * Copyright 2011-2014 Kazuho Oku
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 * @endcode
 */

//we keep these below as a simple functions in order to avoid exposing picojson stuff in the header
void printJSON(const picojson::value& v, const unsigned int& depth)
{
	if (v.is<picojson::null>()) {
		for (unsigned int jj=0; jj<depth; jj++) std::cout << "\t";
		std::cout << "NULL\n";
		return;
	}

	if (v.is<picojson::object>()) {
		const picojson::value::object& obj = v.get<picojson::object>();
		for (picojson::value::object::const_iterator ii = obj.begin(); ii != obj.end(); ++ii) {
			for (unsigned int jj=0; jj<depth; jj++) std::cout << "\t";
			std::cout << ii->first << "\n";
			printJSON(ii->second, depth+1);
		}
	} else if (v.is<std::string>()){
		for (unsigned int jj=0; jj<depth; jj++) std::cout << "\t";
		std::cout << v.get<std::string>() << "\n";
	} else if (v.is<double>()){
		for (unsigned int jj=0; jj<depth; jj++) std::cout << "\t";
		std::cout << v.get<double>() << "\n";
	} else if (v.is<bool>()){
		for (unsigned int jj=0; jj<depth; jj++) std::cout << "\t";
		std::cout << std::boolalpha << v.get<bool>() << "\n";
	} else if (v.is<picojson::array>()){ //ie vector<picojson::value>
		for (unsigned int jj=0; jj<depth; jj++) std::cout << "\t";
		const picojson::array& array = v.get<picojson::array>();
		std::cout << "array " << array.size() << "\n";
		for (size_t jj=0; jj<array.size(); jj++)
			printJSON(array[jj], depth+1);
	}
}

picojson::value goToJSONPath(const std::string& path, picojson::value& v)
{
	size_t start_pos = 0;
	if (path[0]=='$') start_pos++;
	if (path[1]=='.') start_pos++;

	const size_t end_pos = path.find(".", start_pos);
	const std::string local_path = (end_pos!=std::string::npos)? path.substr(start_pos, end_pos-start_pos) : path.substr(start_pos);
	const std::string remaining_path = (end_pos!=std::string::npos)? path.substr(end_pos+1) : "";

	if (v.is<picojson::object>()) {
		picojson::value::object& obj = v.get<picojson::object>();
		for (std::map<std::string,picojson::value>::iterator it = obj.begin(); it != obj.end(); ++it) {
			if (it->first==local_path) {
				if (!remaining_path.empty())
					goToJSONPath(remaining_path, it->second);
				else
					return it->second;
			}
		}
	}

	return picojson::value();
}

void JSONQuery(const std::string& path, picojson::value& v, std::vector<picojson::value>& results)
{
	if (v.is<picojson::null>()) return;

	size_t start_pos = 0;
	if (path[0]=='$') start_pos++;
	if (path[1]=='.') start_pos++;

	const size_t end_pos = path.find(".", start_pos);
	const std::string local_path = (end_pos!=std::string::npos)? path.substr(start_pos, end_pos-start_pos) : path.substr(start_pos);
	const std::string remaining_path = (end_pos!=std::string::npos)? path.substr(end_pos+1) : "";

	if (v.is<picojson::object>()) {
		picojson::value::object& obj = v.get<picojson::object>();
		for (std::map<std::string,picojson::value>::iterator it = obj.begin(); it != obj.end(); ++it) {
			if (it->first==local_path) {
				if (!remaining_path.empty()) {
					 if (it->second.is<picojson::array>()){ //ie vector<picojson::value>
						picojson::array& array = it->second.get<picojson::array>();
						for (size_t jj=0; jj<array.size(); jj++)
							JSONQuery(remaining_path, array[jj], results);
					} else
						JSONQuery(remaining_path, it->second, results);
				} else {
					results.push_back( it->second );
				}
			}
		}
	}
}

std::string getString(const std::string& path, picojson::value& v)
{
	std::vector<picojson::value> results;
	JSONQuery(path, v, results);
	if (!results.empty()) {
		if (! results.front().is<picojson::null>() &&  results.front().is<std::string>()) return  results.front().get<std::string>();
	}

	return std::string();
}

std::vector<std::string> getStrings(const std::string& path, picojson::value& v)
{
	std::vector<picojson::value> results;
	JSONQuery(path, v, results);

	std::vector<std::string> vecString;
	for (size_t ii=0; ii<results.size(); ii++) {
		 if (results[ii].is<picojson::array>()){ //ie vector<picojson::value>
			const picojson::array& array = results[ii].get<picojson::array>();
			for (size_t jj=0; jj<array.size(); jj++) {
				if (! array[jj].is<picojson::null>() &&  array[jj].is<std::string>()) vecString.push_back( array[jj].get<std::string>() );
			}
		} else
			if (! results[ii].is<picojson::null>() &&  results[ii].is<std::string>()) vecString.push_back( results[ii].get<std::string>() );
	}

	return vecString;
}

double getDouble(const std::string& path, picojson::value& v)
{
	std::vector<picojson::value> results;
	JSONQuery(path, v, results);
	if (!results.empty()) {
		if (! results.front().is<picojson::null>() &&  results.front().is<double>()) return  results.front().get<double>();
	}

	return IOUtils::nodata;
}

std::vector<double> getDoubles(const std::string& path, picojson::value& v)
{
	std::vector<picojson::value> results;
	JSONQuery(path, v, results);

	std::vector<double> vecDouble;
	for (size_t ii=0; ii<results.size(); ii++) {
		 if (results[ii].is<picojson::array>()){ //ie vector<picojson::value>
			const picojson::array& array = results[ii].get<picojson::array>();
			//results.reserve( results.size()+array.size() ); //most of the time, we will come here with an empty vector
			for (size_t jj=0; jj<array.size(); jj++) {
				if (! array[jj].is<picojson::null>() &&  array[jj].is<double>()) vecDouble.push_back( array[jj].get<double>() );
			}
		} else
			if (! results[ii].is<picojson::null>() &&  results[ii].is<double>()) vecDouble.push_back( results[ii].get<double>() );
	}

	return vecDouble;
}

bool parseTsPoint(const picojson::value& v, Date& datum, double& value)
{
	if (!v.is<picojson::array>()) return false;

	const picojson::array& array = v.get<picojson::array>();
	if (array.size()!=2) return false;

	if (array[0].is<std::string>())
		IOUtils::convertString(datum, array[0].get<std::string>(), 0.); //hard-coding GMT
	else
		return false;

	if (array[1].is<double>())
		value = array[1].get<double>();
	else {
		if (!array[1].is<picojson::null>()) return false;
		value = IOUtils::nodata;
		return true;
	}

	return true;
}

const std::vector<DBO::tsData> parseTimeSerie(const size_t& tsID, const double& factor, const double& offset, picojson::value& v)
{
	picojson::value ts( goToJSONPath("$.measurements", v) );
	if (!ts.is<picojson::array>()) {
		std::ostringstream ss; ss << "Could not parse timeseries " << tsID;
		throw InvalidFormatException(ss.str(), AT);
	}
	const picojson::array& vecRaw = ts.get<picojson::array>();

	if (vecRaw.empty()) return std::vector<DBO::tsData>();

	std::vector<DBO::tsData> vecData( vecRaw.size() );
	for (size_t ii=0; ii<vecRaw.size(); ii++) {
		double value;
		Date datum;
		if (!parseTsPoint(vecRaw[ii], datum, value))  {
			printJSON(vecRaw[ii], 0);
			std::ostringstream ss; ss << "Error parsing element " << ii << " of timeseries " << tsID;
			throw InvalidFormatException(ss.str(), AT);
		}

		if (value!=IOUtils::nodata) value = value * factor + offset;
		vecData[ii] = DBO::tsData(datum, value);
	}

	return vecData;
}

unsigned int parseInterval(const std::string& interval_str)
{
	unsigned int hour, minute, second, val;
	 if (sscanf(interval_str.c_str(), "%u:%u:%u", &hour, &minute, &second) == 3) {
		return (hour*3600 + minute*60 + second);
	}

	static const unsigned int len = 16;
	char rest[len] = "";
	if (sscanf(interval_str.c_str(), "%u%15s", &val, rest) == 2) {
		if (strncmp(rest, "MIN", len)==0) return (val*60);
		if (strncmp(rest, "HOUR", len)==0) return (val*3600);
		if (strncmp(rest, "DAY", len)==0) return (val*24*3600);
		if (strncmp(rest, "D_BEOB", len)==0) return (val*24*3600);

		throw ConversionFailedException("Could not read measure interval unit '" + std::string(rest) + "'", AT);
	} else
		throw ConversionFailedException("Could not read measure interval '" + interval_str + "'", AT);
}

std::vector<DBO::tsMeta> getTsProperties(picojson::value& v)
{
	std::vector<DBO::tsMeta> tsVec;
	std::vector<picojson::value> results;
	JSONQuery("$.properties.timeseries", v, results);

	for (size_t ii=0; ii<results.size(); ii++) {
		 if (results[ii].is<picojson::array>()){
			const picojson::array& array = results[ii].get<picojson::array>();
			for (size_t jj=0; jj<array.size(); jj++) {
				if (!array[jj].is<picojson::null>()) {
					std::string code, device_code, agg_type;
					double id = -1.;
					unsigned int interval = 0;
					Date since, until;

					const picojson::value::object& obj = array[jj].get<picojson::object>();
					for (picojson::value::object::const_iterator it = obj.begin(); it != obj.end(); ++it) {
						if (it->first=="measurandCode" && it->second.is<std::string>()) code = it->second.get<std::string>();
						if (it->first=="deviceCode" && it->second.is<std::string>()) device_code = it->second.get<std::string>();
						if (it->first=="id" && it->second.is<double>()) id = it->second.get<double>();
						if (it->first=="since" && it->second.is<std::string>()) IOUtils::convertString(since, it->second.get<std::string>(), 0.);
						if (it->first=="until" && it->second.is<std::string>()) IOUtils::convertString(until, it->second.get<std::string>(), 0.);
						if (it->first=="aggregationType" && it->second.is<std::string>()) agg_type = it->second.get<std::string>();
						if (it->first=="measureInterval" && it->second.is<std::string>()) interval = parseInterval(it->second.get<std::string>());
					}

					if (device_code=="BATTERY" || device_code=="LOGGER") break;
					if (agg_type=="SD") break; //we don't care about standard deviation anyway
					if (id==-1.) break; //no id was provided

					const std::string param_str( IOUtils::strToUpper( code.substr(0, code.find('_')) ) );
					tsVec.push_back( DBO::tsMeta(param_str, since, until, agg_type, static_cast<size_t>(id), interval) );
				}
			}
		}
	}

	return tsVec;
}

/*************************************************************************************************/
const int DBO::http_timeout_dflt = 60; // seconds until connect time out for libcurl
const std::string DBO::metadata_endpoint = "/data-api/data/stations/";
const std::string DBO::data_endpoint = "/data-api/data/timeseries/";
const std::string DBO::null_string = "null";

DBO::DBO(const std::string& configfile)
      : cfg(configfile), vecStationName(), vecMeta(), vecTsMeta(),
        coordin(), coordinparam(), coordout(), coordoutparam(),
        endpoint(),
        http_timeout(http_timeout_dflt), dbo_debug(false)
{
	initDBOConnection();
	IOUtils::getProjectionParameters(cfg, coordin, coordinparam, coordout, coordoutparam);
	cfg.getValues("STATION", "INPUT", vecStationName); //reads station names into vector<string> vecStationName
}

DBO::DBO(const Config& cfgreader)
      : cfg(cfgreader), vecStationName(), vecMeta(), vecTsMeta(),
        coordin(), coordinparam(), coordout(), coordoutparam(),
        endpoint(),
        http_timeout(http_timeout_dflt), dbo_debug(false)
{
	initDBOConnection();
	IOUtils::getProjectionParameters(cfg, coordin, coordinparam, coordout, coordoutparam);
	cfg.getValues("STATION", "INPUT", vecStationName); //reads station names into vector<string> vecStationName
}

void DBO::initDBOConnection()
{
	curl_global_init(CURL_GLOBAL_ALL);

	cfg.getValue("DBO_TIMEOUT", "Input", http_timeout, IOUtils::nothrow);
	cfg.getValue("DBO_URL", "Input", endpoint);
	if (*endpoint.rbegin() != '/') endpoint += "/";
	cerr << "[i] Using DBO URL: " << endpoint << endl;

	cfg.getValue("DBO_DEBUG", "INPUT", dbo_debug, IOUtils::nothrow);
}

void DBO::readStationData(const Date& /*date*/, std::vector<StationData>& vecStation)
{
	vecStation.clear();
	if (vecMeta.empty()) fillStationMeta();
	vecStation = vecMeta;
}

void DBO::readMeteoData(const Date& dateStart, const Date& dateEnd,
                          std::vector< std::vector<MeteoData> >& vecMeteo)
{
	vecMeteo.clear();
	if (vecMeta.empty()) fillStationMeta();

	vecMeteo.resize(vecMeta.size());
	for(size_t ii=0; ii<vecMeta.size(); ii++)
		readData(dateStart, dateEnd, vecMeteo[ii], ii);
}

/**
* @brief Read and cache the stations' metadata
*/
void DBO::fillStationMeta()
{
	vecMeta.clear();
	vecMeta.resize( vecStationName.size() );
	vecTsMeta.resize( vecStationName.size() );

	for(size_t ii=0; ii<vecStationName.size(); ii++) {
		const std::string user_string( IOUtils::strToUpper(vecStationName[ii]) );
		const size_t pos_marker = user_string.find("::");
		const std::string station_id = (pos_marker==std::string::npos)? user_string : user_string.substr(pos_marker+2);
		const std::string network =  (pos_marker==std::string::npos)? "IMIS" : user_string.substr(0, pos_marker);
		const std::string request( metadata_endpoint + network + "/" + station_id );

		std::stringstream ss;
		if (curl_read(request, ss)) {
			//handling possible errors
			if (ss.str().empty()) throw UnknownValueException("Station not found: '"+station_id+"'", AT);
			picojson::value v;
			const std::string err( picojson::parse(v, ss.str()) );
			if (!err.empty()) throw IOException("Error while parsing JSON: "+err, AT);
			const std::string type( getString("$.type", v) );
			if (type!="Feature") {
				const std::string error( getString("$.error", v) );
				throw UnknownValueException("Station '"+station_id+"' returned with error: "+error, AT);
			}

			//processing metadata
			const std::vector<double> coordinates( getDoubles("$.geometry.coordinates", v) );
			if (coordinates.size()!=3) throw InvalidFormatException("Wrong coordinates specification!", AT);

			Coords position(coordin, coordinparam);
			position.setLatLon(coordinates[1], coordinates[0], coordinates[2]);
			const StationData sd(position, getString("$.properties.name", v), getString("$.properties.locationName", v));
			vecMeta[ii] = sd;

			//parse and store the time series belonging to this station
			vecTsMeta[ii] = getTsProperties(v);
		} else {
			if (dbo_debug)
				std::cout << "****\nRequest: " << request << "\n****\n";
			throw IOException("Could not retrieve data for station " + station_id, AT);
		}
	}
}

/**
* @brief Identify the relevant MeteoData::Parameters from DBO provided information
* @param[in] param_str DBO string representation of the meteo parameter
* @param[in] agg_type DBO aggregation type
* @param[out] param MeteoData::Parameters standardized parameter
* @return true if the parameter was properly identified, false otherwise
*/
bool DBO::getParameter(const std::string& param_str, const std::string& agg_type, MeteoData::Parameters &param)
{
	if (param_str=="P") param = MeteoData::P;
	else if (param_str=="TA") param = MeteoData::TA;
	else if (param_str=="RH") param = MeteoData::RH;
	else if (param_str=="TS0") param = MeteoData::TSG;
	else if (param_str=="TSS") param = MeteoData::TSS;
	else if (param_str=="HS") param = MeteoData::HS;
	else if (param_str=="VW" && agg_type=="MAX") param = MeteoData::VW_MAX;
	else if (param_str=="VW") param = MeteoData::VW;
	else if (param_str=="DW") param = MeteoData::DW;
	else if (param_str=="RSWR") param = MeteoData::RSWR;
	else if (param_str=="ISWR") param = MeteoData::ISWR;
	else if (param_str=="ILWR") param = MeteoData::ILWR;
	else if (param_str=="RRI") param = MeteoData::PSUM;
	else return false;

	return true;
}

bool DBO::getExtraParameter(const std::string& param_str, std::string& param_extra)
{
	if (param_str=="TS25") param_extra = "TS1";
	else if (param_str=="TS50")  param_extra = "TS2";
	else if (param_str=="TS100")  param_extra = "TS3";
	else return false;

	return true;
}

/**
* @brief Provide the way to convert the DBO units into standardized units (SI).
* It is assume that we can first multiply by a factor, then add an offset.
* @param[in] ts DBO timeseries properties
* @param[in] is_std is it a standard parameter or an extra parameter?
* @param[out] factor factor to apply
* @param[out] offset offset to apply
*/
void DBO::getUnitsConversion(const DBO::tsMeta& ts, const bool& is_std, double &factor, double &offset)
{
	if (is_std) {
		const MeteoData::Parameters param = static_cast<MeteoData::Parameters>( ts.param );

		//compute the conversion parameters (C to K, cm to m, % to [0-1], PINT to PSUM
		switch (param) {
			case MeteoData::TA: case MeteoData::TSG: case MeteoData::TSS:
				factor = 1.;
				offset = Cst::t_water_freezing_pt;
				return;
			case MeteoData::RH: case MeteoData::HS:
				factor = 1./100.;
				offset = 0.;
				return;
			case MeteoData::PSUM:
				factor = 3600. / ts.interval;
				offset = 0.;
				return;
			default:
				return;
		}
	} else {
		if (ts.param_extra=="TS1" || ts.param_extra=="TS2" || ts.param_extra=="TS3") offset = Cst::t_water_freezing_pt;
	}
}

//to evaluate the best combination of TS to select: try all possible combinations (there are not so many), rate them and pick the best!
//example: if a given combination covers the whole period, +10. If it is consistent in terms of rate, +5. If precips have the highest rate, +20...

/**
* @brief Select the timeseries that have to be retrive to build the output dataset.
* Since a parameter might be provided by multiple timeseries (with different start/end, sampling rates, etc),
* we have to select which ones are relevant for the output dataset.
* @param[in] stat_id Station ID (only needed for debug output)
* @param[in] tsVec DBO timeseries properties
* @param[in] dateStart start date of the output dataset
* @param[out] dateEnd end date of the output dataset
*/
void DBO::selectTimeSeries(const std::string& stat_id, std::vector<DBO::tsMeta>& tsVec, const Date& dateStart, const Date& dateEnd) const
{
	//for each parameter, a vector of suitable indices within tsVec (for internal use only)
	std::map<MeteoData::Parameters, std::vector<size_t> > mapParams;

	//for the current station, loop over the timeseries that cover [Start, End]
	for (size_t ii=0; ii<tsVec.size(); ii++) {
		MeteoData::Parameters param;
		if (getParameter(tsVec[ii].param_str, tsVec[ii].agg_type, param)==false) { //unrecognized parameter
			if (getExtraParameter(tsVec[ii].param_str, tsVec[ii].param_extra)==false) continue;
			tsVec[ii].selected = true;
			continue;
		}
		tsVec[ii].param = param; //we identified the parameter

		const Date tsStart(tsVec[ii].since), tsEnd(tsVec[ii].until);
		if (!tsStart.isUndef() && tsStart>dateEnd) continue; //this TS does not contain our period of interest
		if (!tsEnd.isUndef() && tsEnd<dateStart) continue; //this TS does not contain our period of interest

		if (mapParams.count(param)==0) {
			mapParams[param].push_back( ii );
			tsVec[ii].selected = true;
		} else {
			const bool hasStart = !tsStart.isUndef() && tsStart<=dateStart;
			const bool hasEnd = !tsEnd.isUndef() && tsEnd>=dateEnd;
			const unsigned int meas_interval = tsVec[ii].interval;
			if (hasStart && hasEnd && (meas_interval==1800 || meas_interval==3600)) { //it has everything we want from IMIS, so we take it
				tsVec[ mapParams[param][0] ].selected = false;
				mapParams[param][0] = ii;
				tsVec[ii].selected = true;
			}
		}
	}

	if (dbo_debug) {
		std::cout << "<Station " << stat_id << ">\n";
		for (size_t ii=0; ii<tsVec.size(); ii++) std::cout << tsVec[ii].toString() << "\n";
		std::cout << "</Station " << stat_id << ">\n";
	}
}

//read all data for the given station
void DBO::readData(const Date& dateStart, const Date& dateEnd, std::vector<MeteoData>& vecMeteo, const size_t& stationindex)
{
	const std::string Start( dateStart.toString(Date::ISO_Z) );
	const std::string End( dateEnd.toString(Date::ISO_Z) );

	//tag each timeseries with a valid MeteoData::Parameter if it should be used
	selectTimeSeries(vecMeta[stationindex].getStationID(), vecTsMeta[stationindex], dateStart, dateEnd);

	//now get the data
	for (size_t ii=0; ii<vecTsMeta[stationindex].size(); ii++) {
		const DBO::tsMeta &ts = vecTsMeta[stationindex][ii];
		if ( !ts.selected ) continue; //this timeseries was not selected

		const StationData &sd = vecMeta[stationindex];
		std::ostringstream ss_ID; ss_ID << ts.id;
		const std::string request( data_endpoint + ss_ID.str() + "?from=" + Start + "&until=" + End );

		std::stringstream ss;
		if (curl_read(request, ss)) { //retrieve the page from the formed URL
			if (ss.str().empty()) throw UnknownValueException("Timeseries not found: '"+ss_ID.str()+"'", AT);
			picojson::value v;
			const std::string err( picojson::parse(v, ss.str()) );
			if (!err.empty()) throw IOException("Error while parsing JSON: "+ss.str(), AT);

			const bool is_std = (ts.param!=IOUtils::unodata);
			double factor = 1., offset = 0.;
			getUnitsConversion(ts, is_std, factor, offset);

			const std::vector<DBO::tsData> vecData( parseTimeSerie(ts.id, factor, offset, v) );
			if (vecData.empty()) return;

			MeteoData md_pattern = (vecMeteo.empty())? MeteoData(Date(), sd) : vecMeteo.front(); //This assumes that the station is not moving!
			if (is_std) { //this is a standard parameter
				mergeTimeSeries(md_pattern, ts.param, vecData, vecMeteo);
			} else { //this is an extra parameter
				const size_t param = md_pattern.addParameter( ts.param_extra );
				for (size_t jj=0; jj<vecMeteo.size(); jj++) vecMeteo[jj].addParameter( ts.param_extra );
				mergeTimeSeries(md_pattern, param, vecData, vecMeteo);
			}
		} else {
			if (dbo_debug)
				std::cout << "****\nRequest: " << request << "\n****\n";
			throw IOException("Could not retrieve data for timeseries " + ss_ID.str(), AT);
		}
	}
}

/**
* @brief Merge a newly read timeseries into vecMeteo
* @param[in] md_pattern pattern MeteoData to be used to insert new elements
* @param[in] param index of the current meteo parameter
* @param[in] vecData the raw (but parsed) data
* @param vecMeteo the vector that will receive the new values (as well as inserts if necessary)
*/
void DBO::mergeTimeSeries(const MeteoData& md_pattern, const size_t& param, const std::vector<DBO::tsData>& vecData, std::vector<MeteoData>& vecMeteo) const
{
	if (vecData.empty()) return;

	if (vecMeteo.empty()) { //easy case: the initial vector is empty
		vecMeteo.resize( vecData.size() );
		for (size_t ii=0; ii<vecData.size(); ii++) {
			MeteoData md( md_pattern );
			md.date = vecData[ii].date;
			md(param) = vecData[ii].val;
			vecMeteo[ii] = md;
		}
	} else {
		size_t vecM_start = 0; //the index in vecRaw that matches the original start of vecMeteo
		size_t vecM_end = 0; //the index in vecRaw that matches the original end of vecMeteo

		//filling data before vecMeteo
		if (vecData.front().date<vecMeteo.front().date) {
			const Date start_date( vecMeteo.front().date );
			vecM_start = vecData.size(); //if no overlap is found, take all vecData
			for(size_t ii=0; ii<vecData.size(); ii++) { //find the range of elements to add
				if (vecData[ii].date>=start_date) {
					vecM_start = ii;
					break;
				}
			}

			vecMeteo.insert(vecMeteo.begin(), vecM_start, md_pattern);
			for (size_t ii=0; ii<vecM_start; ii++) {
				vecMeteo[ii].date = vecData[ii].date;
				vecMeteo[ii](param) = vecData[ii].val;
			}
		}

		//general case: merge one timestamp at a time
		std::vector<MeteoData> tmp;
		tmp.reserve( vecMeteo.size() + (vecData.size() - vecM_start)); //"worst case" scenario: all elements will be added

		size_t idx2 = vecM_start; //all previous elements were handled before
		size_t last_vM = vecM_start; //last element from vecMeteo that will have to be invalidated
		for(size_t ii=vecM_start; ii<vecMeteo.size(); ii++) {
			const Date curr_date( vecMeteo[ii].date );
			while ((idx2<vecData.size()) && (curr_date>vecData[idx2].date)) {
				tmp.push_back( md_pattern );
				tmp.back().date = vecData[idx2].date;
				tmp.back()(param) = vecData[idx2].val;
				idx2++;
			}
			if (idx2==vecData.size())  break; //nothing left to merge

			if (curr_date==vecData[idx2].date) {
				vecMeteo[ii](param) = vecData[idx2].val;
				idx2++;
			}
			tmp.push_back( vecMeteo[ii] );
			last_vM = ii;
		}

		const size_t new_count = last_vM - vecM_start + 1;
		if (new_count<tmp.size())
			vecMeteo.insert( vecMeteo.begin() + vecM_start, tmp.size()-new_count, tmp.front()); //so room for the extra params is allocated

		for(size_t ii=0; ii<tmp.size(); ii++)
			vecMeteo[vecM_start+ii] = tmp[ii];

		vecM_end = idx2;

		//filling data after vecMeteo
		if (vecMeteo.back().date<vecData.back().date) {
			if (vecM_end!=vecData.size()) {
				for (size_t ii=vecM_end; ii<vecData.size(); ii++) {
					vecMeteo.push_back( md_pattern );
					vecMeteo.back().date = vecData[ii].date;
					vecMeteo.back()(param) = vecData[ii].val;
				}
			}
		}
	}
}

size_t DBO::data_write(void* buf, const size_t size, const size_t nmemb, void* userp)
{
	if (userp) {
		ostream& os = *static_cast<ostream*>(userp);
		const std::streamsize len = size * nmemb;

		if (os.write(static_cast<char*>(buf), len)) return len;
	}

	return 0;
}

bool DBO::curl_read(const std::string& url_query, std::ostream& os) const
{
	CURLcode code(CURLE_FAILED_INIT);
	CURL* curl = curl_easy_init();

	const std::string url( endpoint + url_query );

	if (curl) {
		if (CURLE_OK == (code = curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, &data_write))
		   && CURLE_OK == (code = curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 1L))
		   && CURLE_OK == (code = curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L))
		   && CURLE_OK == (code = curl_easy_setopt(curl, CURLOPT_FILE, &os))
		   && CURLE_OK == (code = curl_easy_setopt(curl, CURLOPT_TIMEOUT, DBO::http_timeout))
		   && CURLE_OK == (code = curl_easy_setopt(curl, CURLOPT_URL, url.c_str())))
		{
			code = curl_easy_perform(curl);
		}
		curl_easy_cleanup(curl);
	}

	if (code!=CURLE_OK) {
		if (dbo_debug)
			std::cout << "****\nRequest: " << url_query << "\n****\n";
		std::cout << "[E] " << curl_easy_strerror(code) << "\t";
	}

	return (code==CURLE_OK);
}

} //namespace
