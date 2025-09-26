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
#include <meteoio/plugins/SNIO.h>
#include <meteoio/IOUtils.h>
#include <meteoio/FileUtils.h>
#include <meteoio/IOExceptions.h>
#include <meteoio/meteoLaws/Atmosphere.h>

#include <sstream>
#include <iostream>
#include <fstream>

using namespace std;

namespace mio {
/**
 * @page snowpack SNIO
 * @section snowpack_format Format
 * This is for reading meteo data in the SNOWPACK meteo format. It is mostly a meteo data format but a
 * metadata file can be provided alongside. These two formats are described below. Please note that when writing
 * to a file, the header line will only be created if the file does not already exist. If the file already
 * exists on the disk, new data will be appended without attempting to write a header line.
 *
 * @subsection snowpack_meteo_format Meteo data file format
 * The SNOWPACK meteo format is defined as such:
 * - a one line header that starts with "MTO" as marker, followed by a short name suitable to be used
 *   as station_id (between <>) and the number of data lines.
 * - multiple lines of data, starting with "M"
 * - a one line footer that consists of the "END" string followed by a new line character.
 *
 * A short example could therefore be:
 * @code
 * MTO <Example_station> 2
 * M 01.08.1958 05:00 21396.20834 9.6 0.674 0.3 356.1 0.0 0.0 276.6 1 -0.1 0 0 1 1 1 1 1 0.6
 * M 01.08.1958 06:00 21396.25000 9.6 0.675 0.8 343.0 27.1 5.3 288.7 1 -0.1 0 0 1 1 1 1 1 1.6
 * END
 * @endcode
 *
 * The data section is made of independent lines that all start with the "M" character, followed by
 * the date as DD.MM.YYYY and the time as HH:mm (24 hours time) and then the Julian day
 * relative to 01.01.1900 00:00 (please note that the Julian day as given by Excel is off by 2 days!).
 * All other fields in each lines are meteo parameters, in order given in the table below.
 *
 * <center><table>
 * <tr><th>Parameter</th><th>Units</th><th>Mandatory</th><th>Comments</th></tr>
 * <tr><td></td><td></td><td></td><td></td></tr>
 * <tr><td>Air temperature</td><td>K or &deg;C</td><td>yes</td><td></td></tr>
 * <tr><td>Relative air humidity</td><td>[0-1] or %</td><td>yes</td><td></td></tr>
 * <tr><td>Wind velocity</td><td>m s<sup>-1</sup></td><td>yes</td><td>at snow station</td></tr>
 * <tr><td>Wind direction</td><td>degrees</td><td>no</td><td>At either snow or wind station</td></tr>
 * <tr><td>Incoming SW radiation</td><td>W m<sup>-2</sup></td><td>no</td><td></td></tr>
 * <tr><td>Reflected SW radiation</td><td>W m<sup>-2</sup></td><td>no</td><td></td></tr>
 * <tr><td>Incoming LW radiation or Cloudiness</td><td>W m<sup>-2 or [0-1]</sup></td><td>no</td><td></td></tr>
 * <tr><td>Snow surface temperature</td><td>K or &deg;C</td><td>no</td><td></td></tr>
 * <tr><td>Bottom temperature</td><td>K or &deg;C</td><td>no</td><td></td></tr>
 * <tr><td>Precipitations per meteo step</td><td>kg m<sup>-2</sup> (mm H<sub>2</sub>O)</td><td>no</td><td></td></tr>
 * <tr><td>Snow depth</td><td>m</td><td>no</td><td></td></tr>
 * <tr><td>Measured temperatures in snow/soil</td><td>K or &deg;C</td><td>no</td><td></td></tr>
 * <tr><td>Wind velocity</td><td>m s<sup>-1</sup></td><td>no</td><td>at wind station</td></tr>
 * </table></center>
 *
 * The optional parameters are futher specified through some plugin options (see below, \ref snowpack_keywords).
 *
 * @subsection snowpack_metadata_format Metadata file format
 * The (optional) metadata is provided in a separate file that can contain multiple stations, one per line.
 * Each line has the following structure:
 * - ALI2 Allieres:Chenau 1767 6.993 46.489 1.22
 *
 * where the first field is the short name, followed by the fullname and the location, then the elevation,
 * the longitude, the latitude and a wind coefficient (unused by MeteoIO). The short name is used for
 * identifying the station (stationID) and matching it with the data file (name given in io.ini).
 * If no such metadata file is provided, the metadata will be left nodata. This only makes sense
 * if the metadata would be later filled by another way (like a merge).
 *
 * @section snowpack_units Units
 * - temperatures in degrees Celsius (input and output) or in kelvins (input only)
 * - relative humidity in % (input and output) or in [0;1] (input only)
 * - wind speed in m/s
 * - precipitations in mm w.e. (kg/m²) per meteo time step
 * - radiation in W/m²
 *
 * @section snowpack_keywords Keywords
 * This plugin uses the following keywords:
 * - COORDSYS: coordinate system (see Coords); [Input] and [Output] section
 * - COORDPARAM: extra coordinates parameters (see Coords); [Input] and [Output] section
 * - METEOPATH: path to the meteo files directory; [Input] and [Output] sections
 * - STATION#: input meteo data file, e.g. STATION1, STATION2; [Input] section
 * - METAFILE: filename of the meta data file (in METEOPATH); [Input] section (optional but recommended)
 * - optional:
 * 	- ISWR_INP or RSWR_INP: if one of these data is missing, set corresponding switch to false. The setting
 *                        will be valid for all stations.
 * 	- additional data must follow order given below but may be missing:
 * 		- NUMBER_MEAS_TEMPERATURES: integer, the number of measured snow temperatures provided; [Input] section \n
 * 		- NUMBER_OF_SOLUTES: integer, the number of solutes for which input data are provided; [Input] section
 * 		- VW_DRIFT: bool, a wind velocity to use for blowing and drifting snow is provided; [Input] section
 * 		- RHO_HN: bool, measured new snow density is provided; [Input] section
 *
 * @code
 * [Input]
 * METEO     = SNOWPACK
 * METEOPATH = input
 * METAFILE  = IMIS_Extracted_Info.txt ;metadata for all stations
 * STATION1  = MST96_RR.inp
 * @endcode
 *
 * @section snowpack_errors Errors
 * When writing in Snowpack format, potential errors in the data set are written out. The error count is split between the different
 * types of errors:
 *	- basic input data: each mandatory parameter that is missing at a timestep increments this counter;
 *	- Dirichlet boundary condition data: each TSS or TSG parameter that is missing at a timestep increments this counter;
 *	- optional data: each snow temperature, solutes, snow density, wind drift optional parameter that is missing (if it
 *	 was previously written out) increments this counter;
 * Overall, this means that the count of basic errors should be zero while Dirichlet errors might be tolerable as well as optional data errors.
 */

const int SNIO::sn_julian_offset = 2415021;
const double SNIO::plugin_nodata = -999.0; //plugin specific nodata value
const size_t SNIO::min_nr_meteoData = 15;
const size_t SNIO::streampos_every_n_lines = 2000; //save streampos every 2000 lines of data
const char* SNIO::dflt_extension = ".inp";

SNIO::SNIO(const std::string& configfile)
      : cfg(configfile),
        vecAllStations(), vecFilenames(), vecIndex(),
        coordin(), coordinparam(), coordout(), coordoutparam(),
        in_tz(0.), out_tz(0.), nr_meteoData(min_nr_meteoData),
        number_meas_temperatures(0), number_of_solutes (0), vw_drift(false), rho_hn(false),
        iswr_inp(true), rswr_inp(true)
{
	IOUtils::getProjectionParameters(cfg, coordin, coordinparam, coordout, coordoutparam);
	cfg.getValue("TIME_ZONE","Input",in_tz,IOUtils::nothrow);
	cfg.getValue("TIME_ZONE","Output",out_tz,IOUtils::nothrow);
	cfg.getValue("ISWR_INP","Input",iswr_inp,IOUtils::nothrow);
	cfg.getValue("RSWR_INP","Input",rswr_inp,IOUtils::nothrow);
	if (!iswr_inp || !rswr_inp) nr_meteoData = min_nr_meteoData - 1;
	cfg.getValue("NUMBER_MEAS_TEMPERATURES", "Input", number_meas_temperatures, IOUtils::nothrow);
	cfg.getValue("NUMBER_OF_SOLUTES", "Input", number_of_solutes, IOUtils::nothrow);
	cfg.getValue("VW_DRIFT", "Input", vw_drift, IOUtils::nothrow);
	cfg.getValue("RHO_HN", "Input", rho_hn, IOUtils::nothrow);
}

SNIO::SNIO(const Config& cfgreader)
      : cfg(cfgreader),
        vecAllStations(), vecFilenames(), vecIndex(),
        coordin(), coordinparam(), coordout(), coordoutparam(),
        in_tz(0.), out_tz(0.), nr_meteoData(min_nr_meteoData),
        number_meas_temperatures(0), number_of_solutes (0), vw_drift(false), rho_hn(false),
        iswr_inp(true), rswr_inp(true)
{
	IOUtils::getProjectionParameters(cfg, coordin, coordinparam, coordout, coordoutparam);
	cfg.getValue("TIME_ZONE","Input",in_tz,IOUtils::nothrow);
	cfg.getValue("TIME_ZONE","Output",out_tz,IOUtils::nothrow);
	cfg.getValue("ISWR_INP","Input",iswr_inp,IOUtils::nothrow);
	cfg.getValue("RSWR_INP","Input",rswr_inp,IOUtils::nothrow);
	if (!iswr_inp || !rswr_inp) nr_meteoData = min_nr_meteoData - 1;
	cfg.getValue("NUMBER_MEAS_TEMPERATURES", "Input", number_meas_temperatures, IOUtils::nothrow);
	cfg.getValue("NUMBER_OF_SOLUTES", "Input", number_of_solutes, IOUtils::nothrow);
	cfg.getValue("VW_DRIFT", "Input", vw_drift, IOUtils::nothrow);
	cfg.getValue("RHO_HN", "Input", rho_hn, IOUtils::nothrow);
}

std::string SNIO::file_pos(const std::string& filename, const size_t& linenr)
{
	ostringstream ss2;
	ss2 << filename << ":" <<linenr;
	return ss2.str();
}

void SNIO::readStationData(const Date&, std::vector<StationData>& vecStation)
{
	//the meta data cannot change for the stations in dependence of time
	if (vecAllStations.empty())
		readMetaData();

	vecStation = vecAllStations; //vecAllStations is a global vector that holds all meta data
}

bool SNIO::readStationMetaData(const std::string& metafile, const std::string& stationID, StationData& sd)
{
	if (!FileUtils::validFileAndPath(metafile)) throw InvalidNameException("\"" + metafile + "\" is not a valid file name. Please check your METAFILE key!", AT);
	if (!FileUtils::fileExists(metafile)) throw NotFoundException( "File \"" + metafile + "\" does not exist. Please check your METAFILE key!", AT);

	std::ifstream fin;
	fin.open (metafile.c_str(), std::ifstream::in);
	if (fin.fail())
		throw AccessException(metafile, AT);

	try {
		std::string line;
		const char eoln = FileUtils::getEoln(fin); //get the end of line character for the file

		size_t linenr = 0;
		std::vector<std::string> tmpvec;

		while (!fin.eof()) {
			linenr++;
			getline(fin, line, eoln); //read complete line of data

			const size_t ncols = IOUtils::readLineToVec(line, tmpvec); //split up line (whitespaces are delimiters)

			if (ncols==0) {
				//Ignore empty lines
			} else if (ncols == 6) { //valid line, e.g. "MST96 Weissfluhjoch:StudyPlot_MST 2540 9.81 46.831 1.00"
				if (tmpvec.at(0) == stationID) {
					parseMetaDataLine(tmpvec, sd);
					fin.close();
					return true;
				}
			} else {
				throw InvalidFormatException(file_pos(metafile, linenr) + " each line must have 6 columns", AT);
			}
		}
		fin.close();
		return false;
	} catch(const std::exception&){
		if (fin.is_open()) fin.close();
		throw;
	}
}

std::string SNIO::getStationID(const std::string& filename)
{
	/**
	 * This function will return the station name as retrieved from
	 * the first line of a SNIO formatted meteo file
	 */
	if ( !FileUtils::validFileAndPath(filename) ) throw InvalidNameException(filename, AT);
	if ( !FileUtils::fileExists(filename) ) throw NotFoundException(filename, AT);

	std::ifstream fin;
	fin.open (filename.c_str(), std::ifstream::in);

	if (fin.fail())
		throw AccessException(filename, AT);
	if (fin.eof())
		throw InvalidNameException(filename + ": Empty file", AT);

	const char eoln = FileUtils::getEoln(fin); //get the end of line character for the file

	std::string station_id;
	try {
		std::string line;
		std::vector<std::string> tmpvec;

		getline(fin, line, eoln);      //read complete line meta information, parse it
		const size_t ncols = IOUtils::readLineToVec(line, tmpvec); //split up line (whitespaces are delimiters)
		if ((ncols != 3) || (tmpvec.at(0) != "MTO") || (tmpvec.at(1).size() < 3) || (tmpvec.at(1)[0] != '<') || (tmpvec.at(1)[tmpvec.at(1).length()-1] != '>')) //in c++11: .front() and .back()
			throw InvalidFormatException(filename + ": first line in invalid format", AT);

		//Now get the 2nd column looking something like <{STATIONNAME}Data>
		station_id = tmpvec[1].substr(1, tmpvec[1].length()-1); //leaving away the
		const size_t pos = station_id.find("Data");
		if (pos != string::npos) {
			station_id = station_id.substr(0, pos);
		} else {
			throw InvalidFormatException(filename + ": first line in invalid format", AT);
		}
	} catch (...) {
		fin.close();
		throw;
	}

	fin.close();
	return station_id;
}

void SNIO::parseMetaDataLine(const std::vector<std::string>& vecLine, StationData& sd)
{
	if (vecLine.size() != 6)
		throw InvalidFormatException("While reading metadata: each line must have 6 columns", AT);

	//Extract all data as double values
	std::vector<double> tmpdata = vector<double>(vecLine.size());
	for (size_t ii=2; ii<5; ii++) {
		if (!IOUtils::convertString(tmpdata[ii], vecLine[ii], std::dec))
			throw ConversionFailedException("While reading meta data for station " + vecLine[0], AT);
	}

	Coords stationcoord(coordin, coordinparam);
	stationcoord.setLatLon(tmpdata[3], tmpdata[4], tmpdata[2]);
	sd.setStationData(stationcoord, vecLine[0], vecLine[1]);
}

void SNIO::readMetaData()
{
	/**
	 * Parse through the io.ini file and read the desired file names STATION#
	 */
	vecAllStations.clear();
	vecFilenames.clear();

	std::string metafile, inpath;
	cfg.getValue("METAFILE", "Input", metafile, IOUtils::nothrow);
	cfg.getValue("METEOPATH", "Input", inpath);
	cfg.getValues("STATION", "INPUT", vecFilenames);
	vecAllStations.resize(vecFilenames.size());

	for (size_t ii=0; ii<vecFilenames.size(); ii++) {
		const std::string filename( vecFilenames[ii] );
		const std::string extension( FileUtils::getExtension(filename) );
		const std::string file_and_path = (extension!="")? inpath+"/"+filename : inpath+"/"+filename+dflt_extension;
		const std::string station_id( getStationID(file_and_path) );

		if (!FileUtils::validFileAndPath(file_and_path)) //Check whether filename is valid
			throw InvalidNameException(file_and_path, AT);

		StationData sd(Coords(), station_id);
		if (!metafile.empty()) { //a metafile has been provided, so get metadata
			if (readStationMetaData(inpath+ "/" +metafile, station_id, sd) == false) {
				ostringstream msg;
				msg << "No metadata found for station " << station_id << " in " << metafile;
				throw NoDataException(msg.str(), AT);
			}
		}
		vecAllStations[ii] = sd;
		vecFilenames[ii] = file_and_path; //replace short name with usable file name
	}
}

void SNIO::readMeteoData(const Date& dateStart, const Date& dateEnd,
                         std::vector< std::vector<MeteoData> >& vecMeteo)
{
	/*
	 * Read the meteorological snowpack input file, formatted as follows:
	 * M Date Time Date(Julian) TA RH VW DW ISWR RSWR ILWR TSS TSG PSUM HS (TS1 TS2 ... TSN CONC0 CONC1 ... CONCM rho_hn)
	 * The first line may be a comment, it won't start with "M", but with "MTO"
	 * The meteo data is terminated by a singular "END" on a line of its own
	 */
	std::string inpath;
	cfg.getValue("METEOPATH", "Input", inpath);

	if (vecAllStations.empty() || vecFilenames.empty())
		readMetaData();

	vecMeteo.clear();
	vecMeteo.insert(vecMeteo.begin(), vecAllStations.size(), vector<MeteoData>());
	if (vecIndex.empty()) //the vecIndex save file pointers for certain dates
		vecIndex.resize(vecAllStations.size());

	std::vector<std::string> tmpvec;
	for (size_t ii=0; ii<vecAllStations.size(); ii++){
		const std::string file_with_path( vecFilenames[ii] );

		if ( !FileUtils::validFileAndPath(file_with_path) ) throw InvalidNameException(file_with_path, AT);
		if ( !FileUtils::fileExists(file_with_path) ) throw NotFoundException(file_with_path, AT);

		std::ifstream fin;
		fin.open (file_with_path.c_str(), std::ifstream::in);

		if (fin.fail())
			throw AccessException(file_with_path, AT);
		if (fin.eof())
			throw InvalidNameException(file_with_path + ": Empty file", AT);

		const char eoln = FileUtils::getEoln(fin); //get the end of line character for the file

		try {
			string line;
			getline(fin, line, eoln);      //read complete line meta information, ignore it
			if (line.length()>=3){
				if (line.substr(0,3) != "MTO") //if its not meta information rewind to the beginning
					fin.seekg (0, ios::beg);
			} else {
				throw InvalidFormatException(file_with_path + ": first line in invalid format", AT);
			}

			size_t linenr = 0;

			//The following lines are an optimization to jump to the correct position in the file
			streampos current_fpointer = vecIndex.at(ii).getIndex(dateStart);
			if (current_fpointer!=(streampos)-1) fin.seekg(current_fpointer);

			while (!fin.eof()) {
				const streampos tmp_fpointer = fin.tellg();
				getline(fin, line, eoln); //read complete line of data
				linenr++;

				const size_t ncols = IOUtils::readLineToVec(line, tmpvec); //split up line (whitespaces are delimiters)
				if (ncols >= nr_meteoData){
					MeteoData md;
					md.meta = vecAllStations[ii];

					if (parseMeteoLine(tmpvec, file_with_path, linenr, dateStart, dateEnd, md)){//the date is within the requested range -> we keep the data
						convertUnits(md);
						vecMeteo[ii].push_back(md);

					}
					current_fpointer = tmp_fpointer; //save file pointer
					if ( (linenr % streampos_every_n_lines)==0 && (current_fpointer != ((ifstream::pos_type)-1)))
						vecIndex.at(ii).setIndex(md.date, current_fpointer);

					if (md.date>dateEnd) break;
				} else if (ncols == 1){
					if (tmpvec.at(0) == "END") {
						break; //reached end of MeteoData
					} else {
						throw InvalidFormatException(file_pos(file_with_path, linenr) + " premature end of line", AT);
					}
				} else if (ncols == 0){
					//Ignore empty lines
				} else {
					throw InvalidFormatException(file_pos(file_with_path, linenr) + " premature end of line", AT);
				}
			}

			//save stream position and the corresponding end date
			if (current_fpointer != ((ifstream::pos_type)-1))
				vecIndex.at(ii).setIndex(dateEnd, current_fpointer);
		} catch (const std::exception&){
			fin.close();
			throw;
		}
		fin.close();
	}
}

//This function takes a meteo line, extracts the date (ignores Julian) and then converts
//all meteo parameters to doubles and finally copies them into the MeteoData object md
//if the date is outside of the requested range, returns false.
//if the date is as requested -> return true
bool SNIO::parseMeteoLine(const std::vector<std::string>& vecLine, const std::string& filename, const size_t& linenr,
                          const Date& dateStart, const Date& dateEnd, MeteoData& md)
{
	/*
	 * This function takes a meteo line, extracts the date (ignores Julian) and then converts
	 * all meteo parameters to doubles and finally copies them into the MeteoData object md
	 */
	if (vecLine.size() < nr_meteoData)
		throw InvalidFormatException("Reading station "+md.meta.stationID+", at "+file_pos(filename, linenr)+": line is too short", AT);

	if (vecLine[0] != "M")
		throw InvalidFormatException("Reading station "+md.meta.stationID+", at "+file_pos(filename, linenr)+": meteo input lines must start with 'M'", AT);

	//deal with the date
	if (vecLine[1].length() != 10)
		throw InvalidFormatException("Reading station "+md.meta.stationID+", at "+file_pos(filename, linenr)+": date format must be DD.MM.YYYY", AT);

	const std::string year( vecLine[1].substr(6,4) );
	const std::string month( vecLine[1].substr(3,2) );
	const std::string day( vecLine[1].substr(0,2) );

	if (!IOUtils::convertString(md.date, year+"-"+month+"-"+day+"T"+vecLine[2], in_tz, std::dec))
		throw InvalidFormatException("Reading station "+md.meta.stationID+", at "+file_pos(filename, linenr)+": invalid date format", AT);

	if ((md.date < dateStart) || (md.date > dateEnd)) //stop parsing data for dates out of the scope
		return false;

	//Extract all data as double values
	std::vector<double> tmpdata( vecLine.size() );
	for (size_t ii=4; ii<vecLine.size(); ii++) {
		if (!IOUtils::convertString(tmpdata[ii], vecLine[ii], std::dec))
			throw ConversionFailedException("Reading station "+md.meta.stationID+", at "+file_pos(filename, linenr)+": can not convert  '"+vecLine[ii]+"' to double", AT);
	}

	//Copy data into MeteoData object
	size_t ii = 4;
	md(MeteoData::TA) = tmpdata[ii++];
	md(MeteoData::RH) = tmpdata[ii++];
	md(MeteoData::VW) = tmpdata[ii++];
	md(MeteoData::DW) = tmpdata[ii++];
	if (iswr_inp)
		md(MeteoData::ISWR) = tmpdata[ii++];
	else
		md(MeteoData::ISWR) = IOUtils::nodata;
	if (rswr_inp)
		md(MeteoData::RSWR) = tmpdata[ii++];
	else
		md(MeteoData::RSWR) = IOUtils::nodata;

	double& ea = tmpdata[ii++];
	if ((ea <= 1) && (ea != plugin_nodata)){
		if ((md(MeteoData::TA) != plugin_nodata) && (md(MeteoData::RH) != plugin_nodata)) {
			if (ea==0.)
				ea = Atmosphere::Brutsaert_ilwr(md(MeteoData::RH)/100., IOUtils::C_TO_K(md(MeteoData::TA)));
			else
				ea = Atmosphere::Omstedt_ilwr(md(MeteoData::RH)/100., IOUtils::C_TO_K(md(MeteoData::TA)), ea); //calculate ILWR from cloudiness
		} else {
			ea = plugin_nodata;
		}
	}

	md(MeteoData::ILWR) = ea;
	md(MeteoData::TSS)  = tmpdata[ii++];
	md(MeteoData::TSG)  = tmpdata[ii++];
	md(MeteoData::PSUM)  = tmpdata[ii++];
	md(MeteoData::HS)   = tmpdata[ii++]; // nr_meteoData

	// Read optional values
	// TS[]: snow temperatures
	if (vecLine.size() < nr_meteoData + number_meas_temperatures)
		throw InvalidFormatException("Reading station "+md.meta.stationID+", at "+file_pos(filename, linenr)+": not enough measured temperatures data", AT);

	for (size_t jj = 1; jj <= number_meas_temperatures; jj++) {
		ostringstream ss;
		ss << "TS" << (jj);
		md.addParameter(ss.str());
		md(ss.str()) = tmpdata[ii++];
	}
	// CONC[]: solute concentrations
	if (vecLine.size() < nr_meteoData + number_meas_temperatures + number_of_solutes)
		throw InvalidFormatException("Reading station "+md.meta.stationID+", at "+file_pos(filename, linenr)+": not enough solute data", AT);

	for (size_t jj = 0 ; jj < number_of_solutes; jj++) {
		ostringstream ss;
		ss << "CONC" << jj;
		md.addParameter(ss.str());
		md(ss.str()) = tmpdata[ii++];
	}
	// VW_DRIFT: optional wind velocity for blowing and drifting snow
	if (vw_drift) {
		if (vecLine.size() < ii+1)
			throw InvalidFormatException("Reading station "+md.meta.stationID+", at "+file_pos(filename, linenr)+": no data for vw_drift", AT);
		md.addParameter("VW_DRIFT");
		md("VW_DRIFT") = tmpdata[ii++];
	}
	// RHO_HN: measured new snow density
	if (rho_hn) {
		if (vecLine.size() < ii+1)
			throw InvalidFormatException("Reading station "+md.meta.stationID+", at "+file_pos(filename, linenr)+": no data for rho_hn", AT);
		md.addParameter("RHO_HN");
		md("RHO_HN") = tmpdata[ii++];
	}
	if (vecLine.size() > ii) {
		std::ostringstream ss;
		ss << "Reading station " << md.meta.stationID << ", at " << file_pos(filename, linenr) << ": too many fields.\n";
		ss << "Looking for " << nr_meteoData << " standard fields + " << number_meas_temperatures << " snow temperatures + ";
		ss << number_of_solutes << " solutes";

		size_t nb_fields = nr_meteoData + number_meas_temperatures + number_of_solutes;
		if (vw_drift) {
			ss << " + 1 VW_DRIFT";
			nb_fields++;
		}
		if (rho_hn) {
			ss << " + 1 RHO_HN";
			nb_fields++;
		}
		ss << " = " << nb_fields << " fields, but found " << vecLine.size() << " fields";

		throw InvalidFormatException(ss.str(), AT);
	}

	return true;
}

void SNIO::writeMeteoData(const std::vector< std::vector<MeteoData> >& vecMeteo, const std::string&)
{
	std::string outpath;
	cfg.getValue("METEOPATH", "Output", outpath);

	for (size_t ii=0; ii<vecMeteo.size(); ii++) {
		if (!vecMeteo[ii].empty()) {
			std::string station_id( vecMeteo[ii].front().meta.getStationID() );
			if (station_id.empty()) station_id = "UNKNOWN";
			const std::string output_name( outpath + "/" + station_id + ".inp" );
			if (!FileUtils::validFileAndPath(output_name)) throw InvalidNameException(output_name,AT);
			std::ofstream fout;
			if ( !FileUtils::fileExists(output_name) ) {
				fout.open(output_name.c_str());
				fout << "MTO <" << station_id << "> " << vecMeteo[ii].size() << "\n";
			} else {
				fout.open(output_name.c_str());
			}
			writeStationMeteo(vecMeteo[ii], output_name, fout);
			fout.close();
		}
	}
}

void SNIO::writeStationMeteo(const std::vector<MeteoData>& vecmd, const std::string& file_name, std::ofstream& fout)
{ //write out the data for 1 station
	unsigned int failure_count = 0;
	unsigned int Dirichlet_failure_count = 0;
	unsigned int optional_failure_count = 0;

	for (size_t jj=0; jj<vecmd.size(); jj++) {
		int YYYY, MM, DD, HH, MI;
		Date tmp_date(vecmd[jj].date);
		tmp_date.setTimeZone(out_tz);
		tmp_date.getDate(YYYY, MM, DD, HH, MI);
		const double sn_julian = tmp_date.getJulian() - sn_julian_offset + 0.5;
		const double ta = vecmd[jj](MeteoData::TA);
		const double rh = vecmd[jj](MeteoData::RH);
		const double psum = vecmd[jj](MeteoData::PSUM);
		const double vw = vecmd[jj](MeteoData::VW);
		const double dw = vecmd[jj](MeteoData::DW);
		const double iswr = vecmd[jj](MeteoData::ISWR);
		const double rswr = vecmd[jj](MeteoData::RSWR);
		const double ilwr = vecmd[jj](MeteoData::ILWR);
		const double tss = vecmd[jj](MeteoData::TSS);
		const double tsg = vecmd[jj](MeteoData::TSG);
		const double hs = vecmd[jj](MeteoData::HS);

		fout.fill('0');
		fout << "M " << setw(2) << DD << "." << setw(2) << MM << "." << setw(4) << YYYY << " " << setw(2) << HH << ":" << setw(2) << MI << " ";
		fout.flags ( ios::fixed );
		fout << setprecision(6) << setw(12) << sn_julian << " ";

		//default formatting parameters for the measurements
		fout.flags(ios::fixed);
		fout.fill(' ');
		fout.width(6);

		//TA, RH, VW, DW
		if (ta==IOUtils::nodata) {
			failure_count++;
			fout << setw(6) << setprecision(0) << ta << " ";
		} else
			fout << setw(6) << setprecision(2) << IOUtils::K_TO_C(ta) << " ";
		if (rh==IOUtils::nodata) {
			failure_count++;
			fout << setw(5) << setprecision(0) << rh << " ";
		} else
			fout << setw(5) << setprecision(1) << rh * 100. << " ";
		if (vw==IOUtils::nodata) {
			failure_count++;
			fout << setw(4) << setprecision(0) << vw << " ";
		} else {
			fout << setw(4) << setprecision(1) << vw << " ";
		}
		if (dw==IOUtils::nodata)
			failure_count++;
		fout << setw(4) << setprecision(0) << dw << " ";

		//ISWR, RSWR
		if (iswr==IOUtils::nodata && rswr==IOUtils::nodata) {
			failure_count++;
			fout << setw(6) << setprecision(0) << iswr << " " << setprecision(0) << rswr << " ";
		} else {
			if (iswr==IOUtils::nodata)
				fout << setw(6) << setprecision(1) << "0.0" << " ";
			else
				fout << setw(6) << setprecision(1) << iswr << " ";
			if (rswr==IOUtils::nodata)
				fout << setw(6) << setprecision(1) << "0.0" << " ";
			else
				fout << setw(6) << setprecision(1) << rswr << " ";
		}

		//LWR
		if (ilwr==IOUtils::nodata) {
			if (tss==IOUtils::nodata) failure_count++; //if we have tss, we can compute the local ilwr
			fout << setw(5) << setprecision(1) << "0.0" << " ";
		} else {
			fout << setw(5) << setprecision(1) << ilwr << " ";
		}

		//TSS, TSG (only required for Dirichlet)
		if (tss==IOUtils::nodata) {
			Dirichlet_failure_count++;
			fout << setw(7) << setprecision(1) << "0.0" << " ";
		} else {
			fout << setw(7) << setprecision(2) << IOUtils::K_TO_C(tss) << " ";
		}
		if (tsg==IOUtils::nodata) {
			Dirichlet_failure_count++;
			fout << setw(6) << setprecision(1) << "0.0" << " ";
		} else {
			fout << setw(6) << setprecision(2) << IOUtils::K_TO_C(tsg) << " ";
		}

		//PSUM, HS
		if (psum==IOUtils::nodata && hs==IOUtils::nodata) {
			failure_count++;
			fout << setw(7) << setprecision(2) << psum << " " << setw(6) << setprecision(3) << hs << " ";
		} else {
			if (psum==IOUtils::nodata)
				fout << setw(7) << setprecision(1) << "0.0" << " ";
			else
				fout << setw(7) << setprecision(4) << psum << " ";
			if (hs==IOUtils::nodata)
				fout << setw(6) << setprecision(1) << "0.0";
			else
				fout << setw(6) << setprecision(3) << hs;
		}

		// Write optional values
		//TS[]: snow temperatures
		ostringstream ss;
		for (size_t kk=1; kk<100; kk++) {
			ss.str("");
			ss << "TS" << kk;
			if (vecmd[jj].param_exists(ss.str())){
				const double ts = vecmd[jj](ss.str());
				if (ts == IOUtils::nodata) {
					optional_failure_count++;
					fout << setw(7) << setprecision(0) << ts << " ";
				} else {
					fout << setw(7) << setprecision(2) << IOUtils::K_TO_C(ts) << " ";
				}
			} else {
				break;
			}
		}
		//CONC[]: solute concentrations
		for (size_t kk=0; kk<100; kk++) {
			ss.str("");
			ss << "CONC" << kk;
			if (vecmd[jj].param_exists(ss.str())) {
				const double conc = vecmd[jj](ss.str());
				if (conc == IOUtils::nodata) {
					optional_failure_count++;
					fout << setw(6) << setprecision(0) << conc << " ";
				} else {
					fout << setw(6) << setprecision(4) << conc << " ";
				}
			} else {
				break;
			}
		}
		// VW_DRIFT: optional wind velocity for blowing and drifting snow
		if (vecmd[jj].param_exists("VW_DRIFT")) {
			const double vw_drift_val = vecmd[jj]("VW_DRIFT");
			if (vw_drift_val == IOUtils::nodata) {
				optional_failure_count++;
				fout << setw(4) << setprecision(0) << vw_drift_val << " ";
			} else {
				fout << setw(4) << setprecision(1) << vw_drift_val << " ";
			}
		}
		// RHO_HN: measured new snow density
		if (vecmd[jj].param_exists("RHO_HN")) {
			const double rho_hn_val = vecmd[jj]("RHO_HN");
			if (rho_hn_val == IOUtils::nodata) {
				optional_failure_count++;
				fout << setw(6) << setprecision(0) << rho_hn_val << " ";
			} else {
				fout << setw(6) << setprecision(1) << rho_hn_val << " ";
			}
		}

		fout << "\n";
	}

	fout << "END" << endl;

	if ((failure_count > 0) || (Dirichlet_failure_count > 0) || (optional_failure_count > 0)) {
		cerr << "[W] " << failure_count << " basic input data, " << Dirichlet_failure_count <<
		        " Dirichlet boundary condition data, and " << optional_failure_count <<
		        " optional data found missing when writing " << file_name << "\n";
	}
}

void SNIO::convertUnits(MeteoData& meteo)
{
	//converts C to Kelvin, converts ilwr to ea, converts RH to [0,1]
	double& ta = meteo(MeteoData::TA);
	if (ta != IOUtils::nodata) {
		if (ta < 100)
			ta = IOUtils::C_TO_K(ta);
	}

	double& tsg = meteo(MeteoData::TSG);
	if ( tsg != IOUtils::nodata) {
		if (tsg < 100)
			tsg = IOUtils::C_TO_K(tsg);
	}

	double& tss = meteo(MeteoData::TSS);
	if (tss != IOUtils::nodata) {
		if (tss < 100)
			tss = IOUtils::C_TO_K(tss);
	}

	double& rh = meteo(MeteoData::RH);
	if (rh != IOUtils::nodata) {
		if (rh > 1.2)
			rh /= 100;
	}

	ostringstream ss;
	for (size_t ii=1; ii<50; ii++){
		ss.str("");
		ss << "TS" << ii;
		if (meteo.param_exists(ss.str())){
			double& value = meteo(ss.str());
			value = IOUtils::C_TO_K(value);
		} else {
			break;
		}
	}
}

} //namespace
