/***********************************************************************************/
/*  Copyright 2019 WSL Institute for Snow and Avalanche Research    PMOD-DAVOS      */
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
#include <meteoio/plugins/PmodIO.h>
#include <meteoio/FileUtils.h>
#include <meteoio/IOUtils.h>

#include <cerrno>
#include <string.h>
#include <fstream>

using namespace std;

namespace mio {
/**
 * @page pmodio PmodIO
 * This plugin reads raw data file from PMOD/WRC Institute.
 *
 * @section pmodio_format Format
 * This file format is basically an ascii file with a header section containing various metadata (such as the instruments used, etc)
 * followed by a data section. Each line of the data section is made of an ISO timestamp followed by several fileds. The
 * "Measurements" column identifies which parameter the "value" column belongs to. Therefore, the timestamps are not unique
 * in the file (but they are unique for each different parameter).
 *
 * For example, the file below contains two parameters, \e Vtseq and \e Vtpar
 * @code
 * Raw Data File:	DAQ2019-V.4.13
 * Instrument Name:	2917_diffuse
 * Instrument Class:    	Pyranometer
 * owner:	pmodwrc
 * factoryCalibrationFactor:	10.83
 * thermopileVoltageDaqAddress:	DMM8LEV1
 * model:	SR30_Diffuse
 *
 *
 * Timetable File:	T201802281204.xml
 * Timetable Start:	2018-02-28T07:55:30.000
 * Actual Start:	2018-04-06T08:16:30.000
 * Time of first measurement:	2018-04-06T08:21:00.000
 *
 *
 * Timestamp			Delay[ms]	Measurement 	Value
 *
 *
 * 2018-04-06T08:21:00.600		572		Vtseq		-3.852684E-4
 * 2018-04-06T08:21:00.600		572		Vtpar		-2.156243E-4
 * 2018-04-06T08:22:30.600		573		Vtseq		-3.829858E-4
 * 2018-04-06T08:22:30.600		573		Vtpar		-2.178132E-4
 * 2018-04-06T08:24:00.600		573		Vtseq		-3.855155E-4
 * 2018-04-06T08:24:00.600		573		Vtpar		-2.197622E-4
 * @endcode
 *
 * @section pmodio_units Units
 *
 *
 * @section pmodio_keywords Keywords
 * This plugin uses the following keywords, all in the [Input] section:
 * - COORDSYS: coordinate system (see Coords);
 * - COORDPARAM: extra coordinates parameters (see Coords);
 * - METEOPATH: the directory where the data files are located;
 * - STATION1: the filename that contains the data;
 * - POSITION1: the station coordinates, see \link Coords::Coords(const std::string& in_coordinatesystem, const std::string& in_parameters, std::string coord_spec) Coords()\endlink for the syntax;
 * - NAME1: the station name;
 * - ID1: the station ID.
 *
 * @note This plugin is still highly experimental and therefore currently only handles reading one file at a time. Whenever necessary, development would resume to make it a full featured,
 * robust plugin.
 */

const double PmodIO::plugin_nodata = -999.; //plugin specific nodata value. It can also be read by the plugin (depending on what is appropriate)

PmodIO::PmodIO(const std::string& configfile) : location(), name(), id(), filename(), inpath(), cfg(configfile), in_TZ(0.)
{
	parseInputOutputSection();
}

PmodIO::PmodIO(const Config& cfgreader) : location(), name(), id(), filename(), inpath(), cfg(cfgreader), in_TZ(0.)
{
	parseInputOutputSection();
}

void PmodIO::parseInputOutputSection()
{
	cfg.getValue("TIME_ZONE","Input", in_TZ);

	//Parse input section: extract number of files to read and store filenames in vecFiles
	const std::string in_meteo = cfg.get("METEO", "Input", "");
	if (in_meteo == "PMOD") { //keep it synchronized with IOHandler.cc for plugin mapping!!
		cfg.getValue("METEOPATH", "Input", inpath);
		cfg.getValue("STATION1", "Input", filename);

		std::string coordin, coordinparam; //projection parameters
		IOUtils::getProjectionParameters(cfg, coordin, coordinparam);

		std::string coords_specs;
		cfg.getValue("POSITION1", "INPUT", coords_specs);
		location = Coords(coordin, coordinparam, coords_specs);
		cfg.getValue("NAME1", "INPUT", name);
		cfg.getValue("ID1", "INPUT", id);
	}
}

void PmodIO::readStationData(const Date& /*date*/, std::vector<StationData>& vecStation)
{
	vecStation.clear();
	vecStation.push_back( StationData(location, id, name) );
}

//build MeteoData template, based on parameters available in the csv file
//this is not that appropriate for PMOD, it should instead read the first
MeteoData PmodIO::createTemplate(const std::vector<std::string>& fields) const
{
	const size_t nr_of_data_fields = fields.size();

	//build MeteoData template
	MeteoData template_md( Date(0., 0.),  StationData(location, id, name));
	for (size_t ii=0; ii<nr_of_data_fields; ii++)
		template_md.addParameter( fields[ii] );

	return template_md;
}

//read the first 10 lines of data in order to know which fields are present. We consider that the field names are always
//in field 3
MeteoData PmodIO::createTemplate(std::ifstream &fin, const char& eoln) const
{
	const streampos fpointer = fin.tellg();
	size_t linenr = 0;
	std::vector<std::string> tmp_vec;
	MeteoData template_md( Date(0., 0.),  StationData(location, id, name));

	while (!fin.eof() && linenr<10) {
		std::string line;
		getline(fin, line, eoln);
		IOUtils::trim(line);
		linenr++;
		if (line.empty()) continue; //Empty lines are ignored

		IOUtils::readLineToVec(line, tmp_vec);
		if (!template_md.param_exists( tmp_vec[2] )) template_md.addParameter( tmp_vec[2] );
	}

	fin.seekg(fpointer); //jump back to the begining of the data section
	return template_md;
}

void PmodIO::readMeteoData(const Date& dateStart, const Date& dateEnd,
                             std::vector< std::vector<MeteoData> >& vecvecMeteo)
{
	const std::string file_and_path( inpath + "/" + filename );
	if (!FileUtils::fileExists(file_and_path)) throw AccessException(file_and_path, AT); //prevent invalid filenames
	errno = 0;
	std::ifstream fin(file_and_path.c_str(), ios::in|ios::binary); //ascii does end of line translation, which messes up the pointer code
	if (fin.fail())
		throw AccessException("Could not open \'" + file_and_path +"\'. Possible reason: " + std::strerror(errno) + "\n", AT);

	const char eoln = FileUtils::getEoln(fin); //get the end of line character for the file
	size_t linenr=0;
	std::vector<std::string> tmp_vec;
	double calibration_factor=0.;
	std::vector<MeteoData> vecMeteo;

	//reading the headers
	while (!fin.eof()) {
		std::string line;
		getline(fin, line, eoln);
		IOUtils::trim(line);
		linenr++;
		if (line.empty()) continue; //Empty lines are ignored

		if (line.find("Timestamp", 0)!=std::string::npos) { //end of header reached
			break;
		} else if (line.find("factoryCalibrationFactor", 0)!=std::string::npos) {
			IOUtils::readLineToVec(line, tmp_vec);
			if (tmp_vec.size()!=2) {
				fin.close();
				throw InvalidArgumentException("Wrong number of arguments at line "+IOUtils::toString(linenr)+" in file '"+file_and_path+"'", AT);
			}
			if (IOUtils::convertString(calibration_factor, tmp_vec[1])==false) {
				fin.close();
				throw InvalidArgumentException("Can not convert calibration factor '"+tmp_vec[0]+" in file '"+file_and_path+"'", AT);
			}
		}
	}

	//create a MeteoData template
	const MeteoData template_md( createTemplate(fin, eoln) );

	//reading the data
	while (!fin.eof()) {
		std::string line;
		getline(fin, line, eoln);
		IOUtils::trim(line);
		linenr++;
		if (line.empty()) continue; //Empty lines are ignored

		IOUtils::readLineToVec(line, tmp_vec);

		Date dt;
		if (IOUtils::convertString(dt, tmp_vec[0], in_TZ)==false) {
			fin.close();
			throw InvalidArgumentException("Can not convert date '"+tmp_vec[0]+"' at line "+IOUtils::toString(linenr)+" in file '"+file_and_path+"'", AT);
		}
		if (dt<dateStart) continue;
		if (dt>dateEnd) break;
		MeteoData md(template_md);
		md.setDate(dt);

		if (tmp_vec.size()!=4) {
			fin.close();
			throw InvalidArgumentException("Wrong number of fields at line "+IOUtils::toString(linenr)+" in file '"+file_and_path+"'", AT);
		}

		double tmp;
		if (!IOUtils::convertString(tmp, tmp_vec[3])) {
			fin.close();
			const std::string err_msg( "Could not parse field '"+tmp_vec[3]+"' in file \'"+filename+"' at line "+IOUtils::toString(linenr) );
			throw InvalidArgumentException(err_msg, AT);
		}

		md(tmp_vec[2]) = tmp * calibration_factor;
		vecMeteo.push_back( md );
	}

	vecvecMeteo.push_back( vecMeteo );
}

} //namespace
