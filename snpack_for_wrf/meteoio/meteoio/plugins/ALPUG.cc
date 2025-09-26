/***********************************************************************************/
/*  Copyright 2015 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#include <meteoio/plugins/ALPUG.h>
#include <meteoio/plugins/libsmet.h>
#include <meteoio/meteoLaws/Meteoconst.h>

#include <cerrno>
#include <cstring>
#include <string.h>
#include <sstream>
#include <fstream>
#include <algorithm>

using namespace std;

namespace mio {
/**
 * @page alpug Alpug
 * This plugin reads the ASCII data produced by the automatic weather stations by <A href="http://www.alpug.ch/">ALPUG</A>. The metadata
 * for the stations must be provided in an additional file as well as the "description" of the fields that must be provided through the configuration
 * key ALPUG_FIELDS.
 *
 * @section alpug_format Format
 * The files are named with the following schema: {YY}{ID}.met where {YY} represents the last two digits of the year and {ID} is the station ID.
 *
 * The files contain all the measurements but no metadata and no header. The following fields can be present:
 * @code
 * cod area,cod,id_AWS,date hour,Mean Wind,MaxWind,WD,AT (C),HR %, SWOR,HS (cm),empty,HTS0 (cm),empty,6697,empty,GST (C),empty,????,empty,TSS (C),ISWR,P(hpa)
 * @endcode
 *
 * The metadata are provided in a separate file. This comma delimited file can contain comments (same syntax as for the configuration files) and must contain
 * first the station ID (as used in the meteo data file name and in the configuration file), a full name, the decimal latitude, decimal longitude and the altitude.
 * @code
 * #ID,name,lat,lon,alt
 * DAV3,Davos::Baerentalli,46.078243,9.272790,2400
 * SLF5,Davos::SLF,46.,9.2728,1550 ;test station
 * @endcode
 *
 * @section alpug_units Units
 * Temperatures are in Celsius, relative humidity between 0 and 100%, snow heights in cm. The timestamp is formatted as <a href="https://de.wikipedia.org/wiki/DIN_1355-1">DIN 1355</A>,
 * that is as "DD.MM.YYYY HH:MIN".
 *
 * @section alpug_keywords Keywords
 * This plugin uses the following keywords:
 * - COORDSYS: coordinate system (see Coords); [Input] and [Output] section
 * - COORDPARAM: extra coordinates parameters (see Coords); [Input] and [Output] section
 * - METEOPATH: where to find/write the meteo data; [Input] and [Output] section
 * - STATION#: input stations ID to be found in METEOPATH. As many stations as needed may be specified
 * - ALPUG_FIELDS: comma delimited list of fields. The fields <b>MUST</b> use the \ref meteoparam "MeteoData" naming scheme. Unknown or ignored fields are replaced by "%".
 * - WRAP_MONTH: which month (numerical) triggers the start of a new file (belonging to the next year. Default: 10); [Input] section
 * - METAFILE: file within METEOPATH that contains the stations' metadata; [Input] section
 *
 * @code
 * [Input]
 * METEO        = ALPUG
 * METEOPATH    = ./Met_files
 * METAFILE     = meta.txt
 * STATION1     = CAND5
 * ALPUG_FIELDS = %,%,ID,timestamp,VW,VW_MAX,DW,TA,RH, RSWR,HS,%,%,%,%,%,TSG,%,%,%,TSS,ISWR,P
 * WRAP_MONTH   = 10
 * @endcode
 */

const char* ALPUG::dflt_extension = ".met";
const double ALPUG::plugin_nodata = -999.; //plugin specific nodata value. It can also be read by the plugin (depending on what is appropriate)
const size_t ALPUG::max_buffered_lines = 4; //how many lines to keep in buffer in order to detect and silently skip duplicates

ALPUG::ALPUG(const std::string& configfile)
             : cfg(configfile), vecMeta(), LinesBuffer(), vecIDs(), vecFields(),
               coordin(), coordinparam(), coordout(), coordoutparam(), inpath(), outpath(),
	       in_dflt_TZ(0.), out_dflt_TZ(0.), wrap_month(10)
{
	parseInputOutputSection();
}

ALPUG::ALPUG(const Config& cfgreader)
             : cfg(cfgreader), vecMeta(), LinesBuffer(), vecIDs(), vecFields(),
               coordin(), coordinparam(), coordout(), coordoutparam(), inpath(), outpath(),
	       in_dflt_TZ(0.), out_dflt_TZ(0.), wrap_month(10)
{
	parseInputOutputSection();
}

void ALPUG::parseInputOutputSection()
{
	//default timezones
	in_dflt_TZ = out_dflt_TZ = IOUtils::nodata;
	cfg.getValue("TIME_ZONE","Input",in_dflt_TZ,IOUtils::nothrow);
	cfg.getValue("TIME_ZONE","Output",out_dflt_TZ,IOUtils::nothrow);

	// Parse the [Input] and [Output] sections within Config object cfg
	IOUtils::getProjectionParameters(cfg, coordin, coordinparam, coordout, coordoutparam);

	//Parse input section: extract number of files to read and store filenames in vecFiles
	const std::string in_meteo = cfg.get("METEO", "Input", "");
	if (in_meteo == "ALPUG") { //keep it synchronized with IOHandler.cc for plugin mapping!!
		cfg.getValue("METEOPATH", "Input", inpath);
		vecIDs.clear();
		cfg.getValues("STATION", "Input", vecIDs);
		readMetaData();
		cfg.getValue("WRAP_MONTH", "Input", wrap_month, IOUtils::nothrow);

		const std::string fields = cfg.get("ALPUG_FIELDS", "Input");
		IOUtils::readLineToVec(fields, vecFields, ',');
		if (vecFields.empty())
			throw InvalidArgumentException("Please provide a comma delimited list of fields!", AT);
		for (size_t ii=0; ii<vecFields.size(); ++ii) {
			IOUtils::toUpper( vecFields[ii] );
			IOUtils::trim( vecFields[ii] );
		}
	}

	cfg.getValue("METEOPATH", "Output", outpath, IOUtils::nothrow);
}

void ALPUG::readMetaData()
{
	const size_t nr_ids = vecIDs.size();
	vecMeta.clear();
	vecMeta.resize( nr_ids );
	vector<bool> foundID(nr_ids, false);

	const std::string filename = cfg.get("METAFILE", "Input");
	const std::string metafile( inpath + "/" + filename );
	if (!FileUtils::fileExists(metafile)) throw AccessException(metafile, AT); //prevent invalid filenames
	errno = 0;
	std::ifstream fin(metafile.c_str(), std::ifstream::in);
	if (fin.fail()) {
		ostringstream ss;
		ss << "File \'" << metafile << "\' could not be opened. Possible reason: " << std::strerror(errno) << "\n";
		throw AccessException(ss.str(), AT);
	}

	try {
		const char eoln = FileUtils::getEoln(fin); //get the end of line character for the file
		size_t linenr = 0;
		vector<string> vecLine;

		while (!fin.eof()) {
			string line;
			linenr++;
			getline(fin, line, eoln); //read complete line of data

			//strip comments, white spaces and skip empty lines
			IOUtils::stripComments(line);
			IOUtils::trim(line);
			if (line.empty())
				continue;

			const size_t ncols = IOUtils::readLineToVec(line, vecLine, ',');
			if (ncols!=5) { //invalid line
				ostringstream ss;
				ss << "Error in file \'" << metafile << "\' at line" <<linenr << ": invalid number of columns";
				throw InvalidFormatException(ss.str(), AT);
			}

			const std::string line_id( vecLine[0] );

			for (size_t ii=0; ii<nr_ids; ++ii) {
				if (line_id==vecIDs[ii]) { //station ID found in the input list
					if (foundID[ii])
						throw InvalidFormatException("Error: station "+line_id+" appears multiple times in metafile \'"+metafile+"\'", AT);

					std::vector<double> tmpdata( vecLine.size() );
					for (size_t jj=2; jj<5; jj++) {
						if (!IOUtils::convertString(tmpdata[jj], vecLine[jj], std::dec))
							throw ConversionFailedException("While reading meta data for station " + vecLine[0], AT);
					}

					Coords stationcoord(coordin, coordinparam);
					stationcoord.setLatLon(tmpdata[2], tmpdata[3], tmpdata[4]);
					vecMeta[ii].setStationData(stationcoord, vecLine[0], vecLine[1]);
					foundID[ii] = true;
				}
			}
		}
		fin.close();
	} catch(const std::exception&){
		fin.close();
		throw;
	}

	std::string msg;
	for (size_t ii=0; ii<nr_ids; ++ii) {
		if (!foundID[ii]) {
			if (msg.empty())
				msg = "Station(s) " + vecIDs[ii];
			else
				msg.append( ","+vecIDs[ii] );
		}
	}
	if (!msg.empty())
		throw NoDataException(msg+" do(es) not have metadata in \'"+metafile+"\'", AT);
}

void ALPUG::readStationData(const Date&, std::vector<StationData>& vecStation)
{
	vecStation = vecMeta;
}

Date ALPUG::parseDINDate(const std::string& datum) const
{
	int year;
	int month, day, hour, minute;
	double second;
	char rest[32] = "";

	if (sscanf(datum.c_str(), "%d.%d.%d %d:%d:%lg%31s", &day, &month, &year, &hour, &minute, &second, rest) >= 6) {
		return Date(year, month, day, hour, minute, in_dflt_TZ);
	} else if (sscanf(datum.c_str(), "%d.%d.%d %d:%d%31s", &day, &month, &year, &hour, &minute, rest) >= 5) {
		return Date(year, month, day, hour, minute, in_dflt_TZ);
	}

	return Date();
}

//return TRUE if we should keep reading lines
//if isValid==false, don't store the MeteoData object
bool ALPUG::parseLine(const std::string& filename, const size_t& nr_of_data_fields, const Date& dateStart, const Date& dateEnd, const std::string& line, MeteoData &md, bool &isValid) const
{
	md.reset();

	isValid = false;
	std::vector<std::string> tmp_vec;
	if (IOUtils::readLineToVec(line, tmp_vec, ',') == nr_of_data_fields){
		for (size_t ii=0; ii<nr_of_data_fields; ++ii) {
			const std::string field( vecFields[ii] );
			if (field=="%" || field=="ID") continue;
			if (field=="TIMESTAMP") {
				Date date( parseDINDate(tmp_vec[ii]) );
				if (date.isUndef())
					throw InvalidFormatException("Invalid date \'"+tmp_vec[ii]+"\' in file \'"+filename+"\'", AT);

				if (date<dateStart) return true;
				if (date>dateEnd) return false;
				md.setDate(date);
				continue;
			}

			double val;
			IOUtils::convertString(val, tmp_vec[ii]);

			if (field=="TA" || field=="TSG" || field=="TSS")
				val += Cst::t_water_freezing_pt;
			else if (field=="RH" || field=="HS")
				val *= 0.01;
			else if (field=="P")
				val *= 100.;

			md(field) = val;
		}
		isValid = true;
	} else {
		std::ostringstream ss;
		ss << "File \'" << filename << "\' declares " << nr_of_data_fields << " columns ";
		ss << "but this does not match the following line:\n" << line << "\n";
		throw InvalidFormatException(ss.str(), AT);
	}
	return true;
}

//since ALPUG files seem to often contain duplicate lines, just skip them
bool ALPUG::isDuplicate(const std::string& line)
{
	for (size_t ii=0; ii<LinesBuffer.size(); ++ii) {
		if (line==LinesBuffer[ii]) return true;
	}

	if (LinesBuffer.size()>max_buffered_lines) LinesBuffer.pop_front();
	LinesBuffer.push_back( line );
	return false;
}

void ALPUG::readMeteoFile(const size_t& station_index, const Date& dateStart, const Date& dateEnd,
                                              std::vector<MeteoData>& vecM)
{
	vecM.clear();

	int start_year, start_month, start_day;
	int end_year, end_month, end_day;
	dateStart.getDate(start_year, start_month, start_day);
	dateEnd.getDate(end_year, end_month, end_day);
	if (start_month>=wrap_month) start_year++;
	if (end_month>=wrap_month) end_year++;

	const std::string station_id( vecIDs[station_index] );
	Date prev_date(0., 0.);
	const std::list<std::string> dirlist( FileUtils::readDirectory( inpath, station_id+dflt_extension ) );
	if (dirlist.empty()) {
		const std::string msg = "No data file found for station "+station_id+" in \'"+inpath+"\'"+". Files should be named as {YY}{station_id}"+dflt_extension+" with {YY} the last two digits of the year.";
		throw NoDataException(msg, AT);
	}

	for (int year=start_year; year<=end_year; ++year) {
		std::ostringstream ss;
		ss << year;
		const std::string filename( ss.str().substr(2,2) + station_id + dflt_extension );
		if (std::find(dirlist.begin(), dirlist.end(), filename) == dirlist.end()) //this file does not exist
			continue;

		const std::string file_and_path( inpath + "/" + filename );
		if (!FileUtils::fileExists(file_and_path)) throw AccessException(file_and_path, AT); //prevent invalid filenames
		errno = 0;
		std::ifstream fin(file_and_path.c_str(), ios::in|ios::binary); //ascii does end of line translation, which messes up the pointer code
		if (fin.fail())
			throw AccessException("Could not open \'" + file_and_path +"\'. Possible reason: " + std::strerror(errno) + "\n", AT);

		const char eoln = FileUtils::getEoln(fin); //get the end of line character for the file
		const size_t nr_of_data_fields = vecFields.size();
		unsigned int nr_line = 0;
		bool print_warning = true; //to only print 1 warning when a block of multiple lines is duplicated
		while (!fin.eof()) {
			string line;
			getline(fin, line, eoln);
			nr_line++;
			if (line.empty()) continue; //Pure comment lines and empty lines are ignored
			//if (isDuplicate(line)) continue;

			MeteoData md(Date(), vecMeta[station_index]);
			bool isValid;
			if (!parseLine(file_and_path, nr_of_data_fields, dateStart, dateEnd, line, md, isValid))
				break;
			if (isValid) {
				if (md.date<=prev_date) { //this happens when large blocks of data are duplicated
					if (print_warning)
						std::cerr << "[W] timstamps not in order in file \'" << file_and_path << "\' starting at line " << nr_line << "; please check your data!\n";
					print_warning = false;
					continue;
				}
				vecM.push_back( md );
				prev_date = md.date;
				print_warning = true;
			}
		}

		fin.close();
	}
}

void ALPUG::readMeteoData(const Date& dateStart, const Date& dateEnd,
                             std::vector< std::vector<MeteoData> >& vecMeteo)
{
	vecMeteo.clear();
	vecMeteo.resize( vecIDs.size() );
	for (size_t ii=0; ii<vecIDs.size(); ++ii) {
		std::vector<MeteoData> vecM;
		readMeteoFile(ii, dateStart, dateEnd, vecM);
		vecMeteo[ii] = vecM;
	}
}

} //namespace
