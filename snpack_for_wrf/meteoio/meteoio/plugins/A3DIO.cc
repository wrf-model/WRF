/***********************************************************************************/
/*  Copyright 2009 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#include <meteoio/plugins/A3DIO.h>
#include <meteoio/IOExceptions.h>
#include <meteoio/IOUtils.h>
#include <meteoio/FileUtils.h>

#include <fstream>

using namespace std;

namespace mio {
/**
 * @page a3d A3D
 * @section a3d_format Format
 * This plugin reads legacy Alpine3D meteorological input files. It reads the meteo1d.txt file that contains, as measured at one unique location, the following fields:
 * - air temperature in field ta
 * - incoming short wave radiation in field iswr
 * - wind velocity in field vw
 * - relative humidity in field rh
 * - incoming long wave radiation in field ea
 * - precipitations in field nswc
 *
 * and optionally a list of stations with their measurements (meteo2d files) for the following parameters (with YYYY being the 4-digits year):
 * - precipitations (file named precYYYY.txt)
 * - relative humidity (file named rhumYYYY.txt)
 * - air temperature (file named tairYYYY.txt)
 * - wind speed (file named wspdYYYY.txt)
 * - and optionnally wind direction (file named wdirYYYY.txt)
 *
 * @section a3d_units Units
 * The units are assumed to be the following:
 * - temperatures in celsius
 * - relative humidity in %
 * - wind speed in m/s
 * - precipitations in mm/h
 * - radiation in W/m²
 *
 * @section a3d_keywords Keywords
 * This plugin uses the following keywords:
 * - METEOPATH: string containing the path to the meteorological files (ie: where to find meteo1d.txt and meteo2d files)
 * - COORDSYS: input coordinate system (see Coords) specified in the [Input] section
 * - COORDPARAM: extra input coordinates parameters (see Coords) specified in the [Input] section
 * - COORDSYS: output coordinate system (see Coords) specified in the [Output] section
 * - COORDPARAM: extra output coordinates parameters (see Coords) specified in the [Output] section
 * - POIFILE: a path+file name to the a file containing grid coordinates of Points of Interest (for special outputs). Everything
 * after a comment marker will be ignored until the end of the line.
 *
 * @code
 * [Input]
 * METEO     = A3D
 * METEOPATH = ./input/meteo
 * @endcode
 */

const double A3DIO::plugin_nodata = -9999.0; //plugin specific nodata value

A3DIO::A3DIO(const std::string& configfile)
       : cfg(configfile),
         in_tz(0.), out_tz(0.), meteo1d(), coordin(), coordinparam(), coordout(), coordoutparam()
{
	IOUtils::getProjectionParameters(cfg, coordin, coordinparam, coordout, coordoutparam);
	in_tz = out_tz = 0.;
	IOUtils::getTimeZoneParameters(cfg, in_tz, out_tz);
	cfg.getValue("METEOPATH", "Input", meteo1d);
	meteo1d += "/meteo1d.txt";
}

A3DIO::A3DIO(const Config& in_cfg)
       : cfg(in_cfg),
         in_tz(0.), out_tz(0.), meteo1d(), coordin(), coordinparam(), coordout(), coordoutparam()
{
	IOUtils::getProjectionParameters(cfg, coordin, coordinparam, coordout, coordoutparam);
	in_tz = out_tz = 0.;
	IOUtils::getTimeZoneParameters(cfg, in_tz, out_tz);
	cfg.getValue("METEOPATH", "Input", meteo1d);
	meteo1d += "/meteo1d.txt";
}

void A3DIO::writeMeteoData(const std::vector< std::vector<MeteoData> >& vec_data, const std::string&)
{
	if (!vec_data.empty()) {
		//A3D format does not support stations changing position over time
		create1DFile(vec_data);
		write2DMeteo(vec_data);
	}
}

void A3DIO::readStationData(const Date& timestamp, std::vector<StationData>& vecStation)
{
	vecStation.clear();

	//read 1D station and add it to vecStation
	if (!FileUtils::fileExists(meteo1d)) {
		throw NotFoundException(meteo1d, AT);
	}
	StationData sd;
	read1DStation(sd);
	vecStation.push_back(sd);

	//read 2D stations and add them to vecStation
	std::vector<StationData> tmpvecS;
	read2DStations(timestamp, tmpvecS);
	for (size_t i=0; i<tmpvecS.size(); i++) {
		vecStation.push_back(tmpvecS[i]);
	}
}

void A3DIO::readMeteoData(const Date& dateStart, const Date& dateEnd,
                          std::vector< std::vector<MeteoData> >& vecMeteo)
{
	//if dateStart and dateEnd are the same: return exact match for date
	//if dateStart > dateEnd: return first data set with date > dateStart
	//read in all data starting with dateStart until dateEnd
	//if there is no data at all then the vector will be empty, no exception will be thrown
	vecMeteo.clear();

	//first from meteo1d.txt
	read1DMeteo(dateStart, dateEnd, vecMeteo);

	//then all corresponding data sets from the 2d meteo files
	//note: they have to be entirely corresponding (for every date)
	try {
		read2DMeteo(vecMeteo);
	} catch(const std::exception& e){
		std::cerr << "[E] No meteo2d data found or error while reading it, using only Meteo1D data: "
		          << std::endl << "\t" << e.what() << std::endl;
	}
}

void A3DIO::convertUnits(MeteoData& meteo)
{
	meteo.standardizeNodata(plugin_nodata);

	//converts C to Kelvin, converts RH to [0,1]
	double& ta = meteo(MeteoData::TA);
	ta = IOUtils::C_TO_K(ta);

	double& tsg = meteo(MeteoData::TSG);
	tsg = IOUtils::C_TO_K(tsg);

	double& rh = meteo(MeteoData::RH);
	if (rh != IOUtils::nodata)
		rh /= 100.;
}

void A3DIO::read1DStation(StationData& sd)
{
	double latitude=IOUtils::nodata, longitude=IOUtils::nodata;
	double xcoord=IOUtils::nodata, ycoord=IOUtils::nodata, altitude=IOUtils::nodata;

	if (!FileUtils::fileExists(meteo1d)) throw AccessException(meteo1d, AT); //prevent invalid filenames
	std::ifstream fin(meteo1d.c_str(), std::ifstream::in);
	if (fin.fail()) {
		throw AccessException(meteo1d, AT);
	}

	//read and parse the header
	try {
		//Read in station meta data
		const std::map<std::string, std::string> header( FileUtils::readKeyValueHeader(fin, 5, "=") ); //Read in 5 lines as header into a key/value map
		IOUtils::getValueForKey(header, "latitude", latitude);
		IOUtils::getValueForKey(header, "longitude", longitude);
		IOUtils::getValueForKey(header, "x_coord", xcoord);
		IOUtils::getValueForKey(header, "y_coord", ycoord);
		IOUtils::getValueForKey(header, "altitude", altitude);

		latitude = IOUtils::standardizeNodata(latitude, plugin_nodata);
		longitude = IOUtils::standardizeNodata(longitude, plugin_nodata);
		altitude = IOUtils::standardizeNodata(altitude, plugin_nodata);
		xcoord = IOUtils::standardizeNodata(xcoord, plugin_nodata);
		ycoord = IOUtils::standardizeNodata(ycoord, plugin_nodata);

		//compute/check WGS coordinates (considered as the true reference) according to the projection as defined in cfg
		Coords location(coordin, coordinparam);
		location.setXY(xcoord, ycoord, altitude, false);
		location.setLatLon(latitude, longitude, altitude, false);
		location.check("Inconsistent geographic coordinates in file \"" + meteo1d + "\": ");
		sd.setStationData(location, "meteo1d", "Meteo1D station");
	} catch(const std::exception& e) {
		fin.close();
		std::ostringstream msg;
		msg << "[E] Error while reading header of file \"" << meteo1d << "\": " << e.what();
		throw InvalidFormatException(msg.str(), AT);
	}

	fin.close();
}

void A3DIO::read1DMeteo(const Date& dateStart, const Date& dateEnd, std::vector< std::vector<MeteoData> >& vecMeteo)
{
	if (!FileUtils::fileExists(meteo1d)) {
		throw NotFoundException(meteo1d, AT);
	}

	if (!FileUtils::fileExists(meteo1d)) throw AccessException(meteo1d, AT); //prevent invalid filenames
	std::ifstream fin(meteo1d.c_str(), std::ifstream::in);
	if (fin.fail()) {
		throw AccessException(meteo1d,AT);
	}

	const char eoln = FileUtils::getEoln(fin); //get the end of line character for the file

	//get station metadata
	MeteoData tmpdata;
	read1DStation(tmpdata.meta);

	//Go through file, save key value pairs
	std::string line;
	try {
		//Read one line, construct Date object and see whether date is greater or equal than the date_in object
		bool eof_reached = false;
		FileUtils::skipLines(fin, 6, eoln); //skip header lines

		//Loop going through the data sequentially until dateStart is found
		do {
			getline(fin, line, eoln); //read complete line
			eof_reached = readMeteoDataLine(line, tmpdata, meteo1d);
			convertUnits(tmpdata);

		} while ((tmpdata.date < dateStart) && (!eof_reached));

		if ((dateEnd < dateStart) && (!eof_reached)){ //Special case
			vecMeteo.push_back( std::vector<MeteoData>() );
			vecMeteo.front().push_back(tmpdata);
		} else if ((tmpdata.date <= dateEnd)  && (!eof_reached)) {
			vecMeteo.push_back( std::vector<MeteoData>() );
		}

		while ((tmpdata.date <= dateEnd)  && (!eof_reached)) {
			//At this point tmpdata.date is >= dateStart
			vecMeteo.front().push_back(tmpdata);

			getline(fin, line, eoln); //read complete line
			eof_reached = readMeteoDataLine(line, tmpdata, meteo1d);
			convertUnits(tmpdata);
		}
	} catch(...) {
		fin.close();
		std::ostringstream msg;
		msg << "[E] Error processing data section of file " << meteo1d << " possibly at line \"" << line << "\"";
		throw InvalidFormatException(msg.str(), AT);
	}

	fin.close();
}

bool A3DIO::readMeteoDataLine(std::string& line, MeteoData& tmpdata, std::string filename)
{
	std::vector<std::string> tmpvec;
	if (IOUtils::readLineToVec(line, tmpvec) != 10) {
		return true;
	}

	int tmp_ymdh[4];
	for (size_t ii=0; ii<4; ii++) {
		if (!IOUtils::convertString(tmp_ymdh[ii], tmpvec.at(ii), std::dec))
			throw InvalidFormatException(filename + ": " + line, AT);
	}

	const Date tmp_date(tmp_ymdh[0],tmp_ymdh[1],tmp_ymdh[2],tmp_ymdh[3], 0, in_tz);

	//Read rest of line with values ta, iswr, vw, rh, ea, psum
	double tmp_values[6];
	for (size_t ii=0; ii<6; ii++) { //go through the columns
		if (!IOUtils::convertString(tmp_values[ii], tmpvec.at(ii+4), std::dec)) {
			throw InvalidFormatException(filename + ": " + line, AT);
		}
	}

	tmpdata.setDate(tmp_date);
	tmpdata(MeteoData::TA)   = tmp_values[0];
	tmpdata(MeteoData::ISWR) = tmp_values[1];
	tmpdata(MeteoData::VW)   = tmp_values[2];
	tmpdata(MeteoData::RH)   = tmp_values[3];
	tmpdata(MeteoData::ILWR) = tmp_values[4];
	tmpdata(MeteoData::PSUM)  = tmp_values[5];

	return false;
}


void A3DIO::read2DStations(const Date& timestamp, std::vector<StationData>& vecStation)
{
	std::vector<std::string> filenames;
	constructMeteo2DFilenames(timestamp, timestamp, filenames);

	std::map<std::string, size_t> hashStations;
	const size_t stations = getNrOfStations(filenames, hashStations);
	vecStation.resize(stations); //we received an empty vector, so we need to allocate the room it needs

	try {
		for (size_t ii=0; ii<filenames.size(); ii++){
			read2DMeteoHeader(filenames[ii], hashStations, vecStation);
		}
	} catch(...) {
		//clear all 2D meteo data if error occurs
		if (!vecStation.empty())
			vecStation.clear();
		throw;
	}
}

/*
  Preamble: Files are in METEOFILE directory. 4 types of files:
  prec????.txt == psum
  rh????.txt == rh
  ta????.txt == ta
  wspd????.txt == vw

  Remarks: The headers of the files may differ - for each unique
  StationData one MeteoData and one StationData object will be created
*/
void A3DIO::read2DMeteo(std::vector< std::vector<MeteoData> >& vecMeteo)
{
	//Requirement: meteo1D data must exist:
	if ((vecMeteo.empty()) || (vecMeteo[0].empty()))
		return;

	//1D and 2D data must correspond, that means that if there is 1D data
	//for a certain date (e.g. 1.1.2006) then 2D data must exist (prec2006.txt etc),
	//otherwise throw NotFoundException
	const Date startDate(vecMeteo[0].front().date.getJulian(), in_tz, false); //so that the correct filenames for the TZ will be constructed
	const Date endDate(vecMeteo[0].back().date.getJulian(), in_tz, false);

	std::vector<std::string> filenames;
	constructMeteo2DFilenames(startDate, endDate, filenames);//get all files for all years
	std::map<std::string, size_t> hashStations;
	const size_t nr_stations = getNrOfStations(filenames, hashStations);

	if (nr_stations < 1) {
		string tmp;
		cfg.getValue("METEOPATH", "Input", tmp);
		std::ostringstream ss;
		ss << "[E] No stations metadata found between between " << startDate.toString(Date::ISO) << " and " << endDate.toString(Date::ISO);
		ss << " in header of 2D meteo files in " << tmp;
		throw InvalidFormatException(ss.str(), AT);
	}

	constructMeteo2DFilenames(startDate, startDate, filenames);//get filenames for current year
	std::vector<StationData> tmpvecS = std::vector<StationData>(nr_stations); //stores unique stations

	try {
		for (size_t ii=0; ii<filenames.size(); ii++){
			read2DMeteoHeader(filenames[ii], hashStations, tmpvecS);
		}

		//init vecStation with proper StationData, vecMeteo with nodata
		for (size_t jj=0; jj<tmpvecS.size(); jj++){
			vecMeteo.push_back( std::vector<MeteoData>() );
			MeteoData tmd;          //create an empty data set
			tmd.meta = tmpvecS[jj]; //add meta data to that data set
			for (size_t ii=0; ii<vecMeteo.front().size(); ii++){
				//NOTE: there needs to be the same amount of 1D and 2D data
				vecMeteo[jj+1].push_back(tmd);
			}
		}

		size_t bufferindex=0;
		do {
			const size_t currentindex = bufferindex;
			read2DMeteoData(filenames[0], "nswc", hashStations, vecMeteo, bufferindex);
			bufferindex = currentindex;
			read2DMeteoData(filenames[1], "rh", hashStations, vecMeteo, bufferindex);
			bufferindex = currentindex;
			read2DMeteoData(filenames[2], "ta", hashStations, vecMeteo, bufferindex);
			bufferindex = currentindex;
			read2DMeteoData(filenames[3], "vw", hashStations, vecMeteo, bufferindex);

			if (filenames.size() == 5) { //for keeping dw optional
				bufferindex = currentindex;
				read2DMeteoData(filenames.at(4), "dw", hashStations, vecMeteo, bufferindex);
			}

			if (bufferindex < (vecMeteo[0].size())) { //number of 1D meteo data
				//construct new filenames for the continued buffering
				constructMeteo2DFilenames(vecMeteo[0][bufferindex].date, vecMeteo[0][bufferindex].date, filenames);
			}
		} while (bufferindex < (vecMeteo[0].size()));
	} catch(...) {
		//clear all 2D meteo data if error occurs
		if (!vecMeteo.empty())
			vecMeteo.clear();
		throw;
	}

	//clean data and convert the units
	for (size_t ii=1; ii<vecMeteo.size(); ii++) { //loop over all stations except 1D Meteo
		for (size_t jj=0; jj<vecMeteo[ii].size(); jj++){ //Meteo1D data already cleaned
			convertUnits(vecMeteo[ii][jj]);
		}
	}
}

void A3DIO::constructMeteo2DFilenames(const Date& startDate, const Date& endDate, std::vector<std::string>& filenames)
{
	filenames.clear();

	const int startyear = startDate.getYear();
	const int endyear = endDate.getYear();

	std::string tmp;
	cfg.getValue("METEOPATH", "Input", tmp);

	for (int yyyy = startyear; yyyy<=endyear; yyyy++){
		std::ostringstream ss;
		ss << yyyy;

		std::string precFilename = tmp + "/prec" + ss.str() + ".txt";
		std::string rhFilename = tmp + "/rhum" + ss.str() + ".txt";
		std::string taFilename = tmp + "/tair" + ss.str() + ".txt";
		std::string wspdFilename = tmp + "/wspd" + ss.str() + ".txt";
		std::string wdirFilename = tmp + "/wdir" + ss.str() + ".txt";

		filenames.push_back(precFilename);
		filenames.push_back(rhFilename);
		filenames.push_back(taFilename);
		filenames.push_back(wspdFilename);

		if (FileUtils::fileExists(wdirFilename)) //keeping wdir optional
			filenames.push_back(wdirFilename);
	}

	for (size_t ii=0; ii<filenames.size(); ii++) {
		if (!FileUtils::fileExists(filenames[ii])) {
			throw NotFoundException(filenames[ii], AT);
		}
	}
}


size_t A3DIO::getNrOfStations(std::vector<std::string>& filenames, std::map<std::string, size_t>& hashStations)
{
	std::vector<std::string> tmpvec;
	std::string line_in;

	for (size_t ii=0; ii<filenames.size(); ii++) {
		const std::string filename = filenames[ii];

		if (!FileUtils::fileExists(filename)) throw AccessException(filename, AT); //prevent invalid filenames
		std::ifstream fin(filename.c_str(), std::ifstream::in);
		if (fin.fail()) throw AccessException(filename, AT);

		const char eoln = FileUtils::getEoln(fin); //get the end of line character for the file

		FileUtils::skipLines(fin, 4, eoln);
		getline(fin, line_in, eoln); //5th line holds the names of the stations
		const size_t cols = IOUtils::readLineToVec(line_in, tmpvec);
		if ( cols > 4) { // if there are any stations
			//check each station name and whether it's already hashed, otherwise: hash!
			for (size_t jj=4; jj<cols; jj++) {
				const size_t has_count = hashStations.count(tmpvec.at(jj));
				if (has_count == 0) {
					hashStations[tmpvec.at(jj)] = hashStations.size();
				}
			}
		}
		fin.close();
	}

	return (hashStations.size());
}

void A3DIO::read2DMeteoData(const std::string& filename, const std::string& parameter,
                            std::map<std::string,size_t>& hashStations,
                            std::vector< std::vector<MeteoData> >& vecM, size_t& bufferindex)
{
	if (!FileUtils::fileExists(filename)) throw AccessException(filename, AT); //prevent invalid filenames
	std::ifstream fin(filename.c_str(), std::ifstream::in);
	if (fin.fail()) {
		throw AccessException(filename, AT);
	}

	const char eoln = FileUtils::getEoln(fin); //get the end of line character for the file

	FileUtils::skipLines(fin, 4, eoln); //skip first 4 lines
	std::string line_in;
	getline(fin, line_in, eoln); //line containing UNIQUE station names
	std::vector<std::string> vec_names;
	const size_t columns = IOUtils::readLineToVec(line_in, vec_names);
	if (columns < 4) {
		fin.close();
		throw InvalidFormatException("[E] Premature end of line in file \"" + filename + "\" (line does not even contain full timestamp)", AT);
	}

	const MeteoData& lastMeteoData = vecM[0].back(); //last time stamp in buffer of 1D meteo
	std::vector<std::string> tmpvec;
	int tmp_ymdh[4];
	Date curr_date;
	do {
		getline(fin, line_in, eoln);
		std::string tmpline = line_in;
		IOUtils::trim(tmpline);
		if (tmpline.empty()) break;

		const size_t cols_found = IOUtils::readLineToVec(line_in, tmpvec);
		if (cols_found!=columns) { //Every station has to have its own column
			fin.close();
			std::ostringstream ss;
			ss << "[E] Premature End of Line or no data for date " << vecM[0][bufferindex].date.toString(Date::FULL);
			ss << " in file \"" << filename << "\"";
			ss << " (expected data for " << columns << " stations, found " << cols_found << " data fields instead)";
			throw InvalidFormatException(ss.str(), AT);
		}

		for (size_t ii=0; ii<4; ii++) {
			if (!IOUtils::convertString(tmp_ymdh[ii], tmpvec[ii], std::dec)) {
				fin.close();
				throw InvalidFormatException("[E] Check date columns in \"" + filename + "\" at line " + line_in, AT);
			}
		}
		curr_date.setDate(tmp_ymdh[0],tmp_ymdh[1],tmp_ymdh[2],tmp_ymdh[3],0, in_tz);

		const MeteoData& Meteo1D = vecM[0][bufferindex]; //1D Element to synchronize date
		if (curr_date == Meteo1D.date) {
			//Read in data
			for (size_t ii=4; ii<columns; ii++) {
				const size_t stationnr = hashStations[vec_names.at(ii)];
				MeteoData& tmpmd = vecM[stationnr][bufferindex];
				tmpmd.date = curr_date;

				if (parameter == "nswc") {
					if (!IOUtils::convertString(tmpmd(MeteoData::PSUM), tmpvec[ii], std::dec)) {
						fin.close();
						throw ConversionFailedException("For psum value in " + filename + "  for date " + tmpmd.date.toString(Date::FULL), AT);
					}

				} else if (parameter == "rh") {
					if (!IOUtils::convertString(tmpmd(MeteoData::RH), tmpvec[ii], std::dec)) {
						fin.close();
						throw ConversionFailedException("For rh value in " + filename + "  for date " + tmpmd.date.toString(Date::FULL), AT);
					}

				} else if (parameter == "ta") {
					if (!IOUtils::convertString(tmpmd(MeteoData::TA), tmpvec[ii], std::dec)) {
						fin.close();
						throw ConversionFailedException("For ta value in " + filename + "  for date " + tmpmd.date.toString(Date::FULL), AT);
					}

				} else if (parameter == "vw") {
					if (!IOUtils::convertString(tmpmd(MeteoData::VW), tmpvec[ii], std::dec)) {
						fin.close();
						throw ConversionFailedException("For vw value in " + filename + "  for date " + tmpmd.date.toString(Date::FULL), AT);
					}
				} else if (parameter == "dw") {
					if (!IOUtils::convertString(tmpmd(MeteoData::DW), tmpvec[ii], std::dec)) {
						fin.close();
						throw ConversionFailedException("For dw value in " + filename + "  for date " + tmpmd.date.toString(Date::FULL), AT);
					}
				}
			}

			bufferindex++;
		}
	} while ((curr_date<lastMeteoData.date) && (!fin.eof()));

	fin.close();
}

void A3DIO::read2DMeteoHeader(const std::string& filename, std::map<std::string,size_t>& hashStations,
                              std::vector<StationData>& vecS)
{
	if (!FileUtils::fileExists(filename)) throw AccessException(filename, AT); //prevent invalid filenames
	std::ifstream fin(filename.c_str(), std::ifstream::in);
	if (fin.fail()) {
		throw AccessException(filename, AT);
	}

	const char eoln = FileUtils::getEoln(fin); //get the end of line character for the file

	FileUtils::skipLines(fin, 1, eoln);

	//Read all relevant lines in
	std::vector<std::string> vec_altitude, vec_xcoord, vec_ycoord, vec_names;
	std::string line_in;
	getline(fin, line_in, eoln); //Altitude
	const size_t columns = IOUtils::readLineToVec(line_in, vec_altitude);

	getline(fin, line_in, eoln); //xcoord
	if (IOUtils::readLineToVec(line_in, vec_xcoord) != columns) {
		fin.close();
		throw InvalidFormatException("Column count doesn't match from line to line in \"" + filename + "\" at line " + line_in, AT);
	}

	getline(fin, line_in, eoln); //ycoord
	if (IOUtils::readLineToVec(line_in, vec_ycoord) != columns) {
		fin.close();
		throw InvalidFormatException("Column count doesn't match from line to line in \"" + filename + "\" at line " + line_in, AT);
	}

	getline(fin, line_in, eoln); //names
	if (IOUtils::readLineToVec(line_in, vec_names) != columns) {
		fin.close();
		throw InvalidFormatException("Column count doesn't match from line to line in \"" + filename + "\" at line " + line_in, AT);
	}

	fin.close();

	//Check for duplicate station names within one file ... station names need to be unique!
	const size_t nr_stations = vec_names.size();
	for (size_t ii=0; ii<nr_stations; ii++) {
		for (size_t jj=ii+1; jj<nr_stations; jj++) {
			if (vec_names[jj] == vec_names[ii]) {
				throw IOException("Duplicate station names detected in file \"" + filename + "\": " + vec_names[ii], AT);
			}
		}
	}

	//Build Coords object to convert easting/northing values to lat/long in WGS84
	Coords coordinate(coordin, coordinparam);

	for (size_t ii=4; ii<columns; ii++) {
		const size_t stationnr = hashStations[vec_names.at(ii)];
		double altitude=0., easting=0., northing=0.; //to silence a warning
		std::string stationName;
		if ((!IOUtils::convertString(altitude, vec_altitude.at(ii), std::dec))
		    || (!IOUtils::convertString(easting, vec_xcoord.at(ii), std::dec))
		    || (!IOUtils::convertString(northing, vec_ycoord.at(ii), std::dec))
		    || (!IOUtils::convertString(stationName, vec_names.at(ii), std::dec))) {
			throw ConversionFailedException("Conversion of station description failed in file \"" + filename + "\"", AT);
		}
		coordinate.setXY(easting, northing, altitude);
		vecS[stationnr-1].setStationData(coordinate, stationName, stationName);
	}
}

void A3DIO::readPOI(std::vector<Coords>& pts)
{
	std::string filename;
	cfg.getValue("POIFILE", "Input", filename);

	if (!FileUtils::fileExists(filename)) throw AccessException(filename, AT); //prevent invalid filenames
	std::ifstream fin(filename.c_str(), std::ifstream::in);
	if (fin.fail()) throw AccessException(filename,AT);

	const char eoln = FileUtils::getEoln(fin); //get the end of line character for the file

	std::string line_in;
	std::vector<std::string> tmpvec;
	while (!fin.eof()) {
		getline(fin, line_in, eoln);

		IOUtils::stripComments(line_in);
		if (IOUtils::readLineToVec(line_in, tmpvec)==2) { //Try to convert
			int x, y;
			if (!IOUtils::convertString(x, tmpvec.at(0), std::dec)) {
				fin.close();
				throw ConversionFailedException("Conversion of a value failed in \"" + filename + "\" line: " + line_in, AT);
			}

			if (!IOUtils::convertString(y, tmpvec.at(1), std::dec)) {
				fin.close();
				throw ConversionFailedException("Conversion of a value failed in \"" + filename + "\" line: " + line_in, AT);
			}

			Coords tmp_pts;
			tmp_pts.setGridIndex(x, y, IOUtils::inodata, false);
			pts.push_back(tmp_pts);
		}
	}
	fin.close();
}

bool A3DIO::create1DFile(const std::vector< std::vector<MeteoData> >& data)
{
	std::string tmp_path;
	cfg.getValue("METEOPATH", "Output", tmp_path);
	const size_t sta_nr = data.size();
	if (sta_nr==0) return false;

	for (size_t ii=0; ii<sta_nr; ii++) {
		const size_t size = data[ii].size();
		if (size>0) {
			const std::string filename( tmp_path+"/meteo1D_"+data[ii][0].meta.getStationID()+".txt" );
			if (!FileUtils::validFileAndPath(filename)) throw InvalidNameException(filename,AT);
			std::ofstream file(filename.c_str(), std::ios::out | std::ios::trunc);
			if (!file) {
				throw AccessException("[E] Can not open file "+filename, AT);
			}

			file << "Name = " << data[ii][0].meta.getStationID() << endl;
			file << "Latitude = " << data[ii][0].meta.position.getLat() << endl;
			file << "Longitude = " << data[ii][0].meta.position.getLon() << endl;
			file << "X_Coord = " << data[ii][0].meta.position.getEasting() << endl;
			file << "Y_Coord = " << data[ii][0].meta.position.getNorthing() << endl;
			file << "Altitude = " << data[ii][0].meta.position.getAltitude() << endl;
			file << "YYYY MM DD HH ta iswr vw rh ea nswc" << endl;

			file.flags ( std::ios::fixed );
			for (size_t j=0; j<size; j++) {
				int yyyy, mm, dd, hh;
				const Date tmp_date(data[ii][j].date.getJulian(true), out_tz);
				tmp_date.getDate(yyyy, mm, dd, hh);
				file.fill('0');
				file << setw(4) << yyyy << " " << setw(2) << mm << " " << setw(2) << dd << " " << setw(2) << hh << " ";
				file.fill(' ');
				if (data[ii][j](MeteoData::TA) == IOUtils::nodata)
					file << setw(6) << setprecision(0) <<  IOUtils::nodata << " ";
				else
					file << setw(6) << setprecision(2) <<  IOUtils::K_TO_C(data[ii][j](MeteoData::TA)) << " ";
				if (data[ii][j](MeteoData::ISWR) == IOUtils::nodata)
					file << setw(6) << setprecision(0) << IOUtils::nodata << " ";
				else
					file << setw(6) << setprecision(2) << data[ii][j](MeteoData::ISWR) << " ";
				if (data[ii][j](MeteoData::VW) == IOUtils::nodata)
					file << setw(6) << setprecision(0) << IOUtils::nodata << " ";
				else
					file << setw(6) << setprecision(2) << data[ii][j](MeteoData::VW) << " ";
				if (data[ii][j](MeteoData::RH) == IOUtils::nodata)
					file << setw(6) << setprecision(0) << IOUtils::nodata << " ";
				else
					file << setw(6) << setprecision(2) << data[ii][j](MeteoData::RH) * 100. << " ";
				if (data[ii][j](MeteoData::ILWR) == IOUtils::nodata)
					file << setw(6) << setprecision(0) << IOUtils::nodata << " ";
				else
					file << setw(6) << setprecision(2) << data[ii][j](MeteoData::ILWR) << " ";
				if (data[ii][j](MeteoData::PSUM) == IOUtils::nodata)
					file << setw(6) << setprecision(0) << IOUtils::nodata << "\n";
				else
					file << setw(6) << setprecision(2) << data[ii][j](MeteoData::PSUM) << "\n";
			}
			file.close();
		}
	}
	return true;
}

bool A3DIO::writeHeader(std::ofstream &file, const std::vector< std::vector<MeteoData> >& data, const std::string& parameter_name)
{
	std::ostringstream str_altitudes;
	std::ostringstream str_eastings;
	std::ostringstream str_northings;
	const size_t sta_nr = data.size();
	if (sta_nr==0) return false;

	file << "X:\\filepath " << parameter_name <<endl;
	for (size_t ii=0; ii<sta_nr; ii++) {
		if (!data[ii].empty()) {
			str_altitudes << data[ii][0].meta.position.getAltitude() << " ";
			str_eastings  << data[ii][0].meta.position.getEasting() << " ";
			str_northings << data[ii][0].meta.position.getNorthing() << " ";
		}
	}
	file << "YY MM DD HH " << str_altitudes.str() << endl; //altitudes
	file << "YY MM DD HH " << str_eastings.str()  << endl; //easting
	file << "YY MM DD HH " << str_northings.str() << endl; //northing
	file << "YYYY MM DD HH";
	for (size_t ii=0; ii<sta_nr; ii++) {
		if (!data[ii].empty()) {
			file << " " << data[ii][0].meta.getStationID();
		}
	}
	file << std::endl;

	return true;
}

void A3DIO::open2DFile(const std::vector< std::vector<MeteoData> >& data,
                       const std::string& fileprefix, const std::string& label, const double& year,
                       std::ofstream& file)
{//creates a meteo2D file according to the specifications
//the header is also written
	std::ostringstream out;
	out << year;

	const std::string filename = fileprefix+out.str()+".txt";
	if (!FileUtils::validFileAndPath(filename)) throw InvalidNameException(filename,AT);
	file.open(filename.c_str(), ios::out | ios::trunc);
	if (!file) {
		throw AccessException("Can not create file "+filename, AT);
	}
	writeHeader(file, data, label);
}

bool A3DIO::write2DmeteoFile(const std::vector< std::vector<MeteoData> >& data,
                            const unsigned int& parindex, const std::string& fileprefix,
                            const std::string& label)
{
	const size_t sta_nr = data.size();
	if (sta_nr==0) return false;
	const size_t nb_timesteps = data[0].size();
	if (nb_timesteps==0) return false;

	std::ofstream file;
	int startyear, year, month, day, hour;
	Date tmp_date(data[0][0].date.getJulian(true), out_tz);
	tmp_date.getDate(startyear, month, day, hour);

	open2DFile(data, fileprefix, label, startyear, file);
	file.flags ( ios::fixed );

	for (size_t ii=0; ii<nb_timesteps; ii++) {
		tmp_date.setDate(data[0][ii].date.getJulian(true), out_tz);
		tmp_date.getDate(year, month, day, hour);
		if (year!=startyear) {
			//if the year has changed, we need to write to a new file
			file.close();
			startyear = year;
			open2DFile(data, fileprefix, label, year, file);
			file.flags ( ios::fixed );
		}

		file.fill('0');
		file << setw(4) << year << " " << setw(2) << month << " " << setw(2) << day << " " << setw(2) << hour;
		file.fill(' ');
		for (size_t j=0; j<sta_nr; j++) {
			double value = data[j][ii](parindex);
			if (value==IOUtils::nodata) {
				file << " " << setw(7) << setprecision(0) << IOUtils::nodata;
			} else {
				if (parindex==mio::MeteoData::TA) value = IOUtils::K_TO_C(value);
				if (parindex==mio::MeteoData::RH) value = value*100.;
				file << " " << setw(7) << setprecision(2) << value;
			}
		}
		file << "\n";
	}
	file.close();
	return true;
}

void A3DIO::write2DMeteo(const std::vector< std::vector<MeteoData> >& data)
{
	std::string tmp_path = cfg.get("METEOPATH", "Output");

	write2DmeteoFile(data, mio::MeteoData::TA, tmp_path+"/tair", "air temperature");
	write2DmeteoFile(data, mio::MeteoData::RH, tmp_path+"/rhum", "relative humidity");
	write2DmeteoFile(data, mio::MeteoData::VW, tmp_path+"/wspd", "wind velocity");
	write2DmeteoFile(data, mio::MeteoData::DW, tmp_path+"/wdir", "wind direction");
	write2DmeteoFile(data, mio::MeteoData::PSUM, tmp_path+"/prec", "precipitations");
}

} //namespace
