/***********************************************************************************/
/*  Copyright 2019 Avalanche Warning Service Tyrol                  LWD-TIROL      */
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

#include <fstream>
#include <cerrno> //for file open errors
#include <cstring> //for pretty file open errors

#include <meteoio/dataClasses/Date.h>
#include <meteoio/FileUtils.h>
#include <meteoio/plugins/ZRXPIO.h>

namespace mio {

/**
 * @page zrxpio ZRXP
 * This is a simple ASCII plugin handling output only. It outputs meteo data in a text format that can be
 * read by the WISKI database environment.
 *
 * A <a href="https://www.kisters.net/NA/products/wiski/">WISKI database</a> is set atop an Oracle database and built for meteo data and scalability.
 * Some very large collections of meteorological data are stored this way. Due to its intertwined setup it is quite complicated to
 * interact with directly resulting in more or less durable custom solutions. What always works however is
 * an ASCII import via its native ZRXP format ensuring that all necessary processing is initiated, and all
 * necessary recalculations within the database take place.
 *
 * The WISKI database has a rather extensive set of meteo-specific filters and processing elements itself, but the
 * versatility of MeteoIO is on a different level and for a big enough setup this comes with a performance penalty that might not be neglectable.
 * So, the following could also be a proper use case: Export CSV data from WISKI --> Use CSV plugin to read that into MeteoIO and
 * perform the processing --> Output as .zrxp --> Import back to WISKI.
 *
 * @section zrxp_format Format
 * The ZRXP format is a basic ASCII-text format where in contrast to table based formats different parameters
 * are written below each other, allowing for an arbitrary number of associated flags and annotations per value.
 * Each parameter is preceded by one or more header lines laying out the keywords that will be used.
 *
 * Currently, MeteoIO fills the following keywords:
 *  <table>
 *  <tr><th>Keyword</th><th>Meaning</th><th>Value</th></tr>
 *  <tr><td>#</td><td>Comment line 1</td><td>Station and parameter IDs, export timestamp</td></tr>
 *  <tr><td>#</td><td>Comment line 2</td><td>Station coordinates and altitude</td></tr>
 *  <tr><td>ZRXPVERSION</td><td>Used version of the file format</td><td>30 (meaning 3.0)</td></tr>
 *  <tr><td>ZRXPCREATOR</td><td>Origin of the ZRXP file</td><td>MeteoIO's library version</td></tr>
 *  <tr><td>REXCHANGE</td><td>Data exchange number</td><td>Per station and parameter: MIO_STATIONID_PARAMETER</td></tr>
 *  <tr><td>RINVAL</td><td>Code for invalid data</td><td>MeteoIO's nodata (-999), or the value set for `ZRXP_RINVAL`</td></tr>
 *  <tr><td>SANR</td><td>Station ID</td><td>Station ID</td></tr>
 *  <tr><td>CNR</td><td>[Optional] Parameter ID</td><td>MeteoIO's internal index for the parameter</td></tr>
 *  <tr><td>TZ</td><td>Time zone</td><td>Time zone of first output datapoint</td></tr>
 *  <tr><td>CNAME</td><td>Name of parameter</td><td>MeteoIO's short name for the parameter (if available)</td></tr>
 *  <tr><td>CUNIT</td><td>[Optional] Unit of parameter</td><td>MeteoIO's unit for the parameter (if available)</td></tr>
 *  <tr><td>LAYOUT</td><td>Parameter layout</td><td>(timestamp,value,status) and optionally remark</td></tr>
 * </table>
 * `REXCHANGE` denotes the exchange number/ID that is used to import the dataset into WISKI. Imports with the
 * same ID will be associated with one measurement parameter of a certain station. For example, the exchange ID
 * `MIO_SEEG2_TA` could be reserved for a MeteoIO-filtered temperature timeline of station `SEEG2`
 * and will then always contribute to this graph.
 *
 * @note `CUNIT` is automatically set by MeteoIO, but you can make it available as a comment only by setting ZRXP_WRITE_UNITS
 * (see below). This is because if a unit is present, then WISKI is picky about the exact choice (a new unit has to be created in
 * the database). If it is omitted however, it can be set in the WISKI timeline.
 * @note WISKI demands Latin-1 as character encoding. Running on Windows, you can set ZRXP_CONVERT_LATIN to `true`
 * to let the operating system encode the degrees symbol
 * (or convert after the fact with e. g. `iconv -f UTF-8 -t ISO-8859-1 "$file" -o "$file".out`).
 *
 * @section zrxp_keywords Keywords
 * This plugin uses the following keywords in the [Output] section (all of which are optional):
 * - ZRXP_FILE_EXTENSION: The file extension to use for the output files (default: `zrxp`);
 * - ZRXP_SEPARATOR: The delimiter used to separate the header fields (default: `|`);
 * - ZRXP_WRITE_UNITS: Controls whether MeteoIO's stored units for the parameters are written out.
 * Can be `KEYWORD` (default, respected by the WISKI import), `COMMENT` (not used in WISKI import), or `OFF`;
 * - ZRXP_CONVERT_LATIN: Encode non-ASCII characters on the operating system running the program (default: false);
 * - ZRXP_WRITE_CNR: Outputs MeteoIO's internal parameter index (default: true);
 * - ZRXP_RINVAL: The value given to WISKI to interpret as missing data (default: MeteoIO's nodata, usually -999);
 * - ZRXP_REMARK: A remark that is passed through to each output line (default: empty);
 * - ZRXP_NODATA_PARAMS: Parameters that are nodata throughout will be output if at least one nodata is the result of processing (default: true);
 * - ZRXP_STATUS_UNALTERED: Status to give for data that was checked and left unchanged (default: 41);
 * - ZRXP_STATUS_RESAMPLED: Status for temporally interpolated data (default: 42);
 * - ZRXP_STATUS_FILTERED: Status for filtered (changed) data (default: 43);
 * - ZRXP_STATUS_GENERATED: Status for data originating from a MeteoIO generator (default: 44);
 * - ZRXP_STATUS_NODATA: Status for `nodata` values (default: disabled, has priority over all others).
 * - ZRXP_STATUS_UNALTERED_NODATA: Status for when `nodata` is already present in the input file and left untouched (default: 255).
 *
 * The last five status parameters are used to transport data quality assurance flags to the database.
 * `ZRXP_STATUS_NODATA` will be set only if the *filtered* value is found to be `nodata` (e. g. -999).
 * If `ZRXP_STATUS_NODATA` is not given a value, then this check is omitted and timesteps filtered to nodata will get the flag "qa_filtered".
 * Furthermore, if there is a nodata value in the input file that is not altered at all by MeteoIO, then `ZRXP_STATUS_UNALTERED_NODATA`
 * is set as flag, and the value is set to `ZRXP_RINVAL`.
 *
 * @note In addition, you can separately set `ZRXP_RINVAL`. This is necessary for a use case where original and
 * potentially newer data is merged with filtered data. WISKI would fill data gaps with original data and
 * therefore WISKI's nodata value (`RINVAL`) must not match MeteoIO's. This way it's possible to create
 * persistent gaps on import according to the status flag `ZRXP_STATUS_NODATA`.
 *
 * For the file extension, sometimes `.rxp` is used. The separator should either be `|` or `;` which will
 * result in the header fields being separated by `|*|` or `;*;` respectively. The remark offers a
 * rudimentary way to transport any kind of information, albeit statically and not on a per-value basis.
 * This means that for example you can put `ZRXP_REMARK = mio` to give each individual timestamp-value-pair
 * this remark. Remarks are automatically enclosed by double quotes.
 *
 * @note The internal parameter index that is passed as `CNR` may change within MeteoIO for special parameters,
 * hence you can turn it off aswell and make sure WISKI does not use it.
 *
 * @section Example
 * Putting ZRXP as the METEO output plugin will produce .(z)rxp-files:
 * @code
 * [Output]
 * METEOPATH = ./output
 * METEO     = ZRXP
 * ZRXP_FILE_EXTENSION = rxp
 * ZRXP_WRITE_UNITS    = KEYWORD
 * ZRXP_REMARK         = processed by mio
 * @endcode
 *
 * @section Output
 * ZRXP output could look like this if a remark is set:
 * @code
 * ##Station: 101329, export at 2019-02-06T11:53:45 (TZ: 1)
 * ##Station coordinates (Lat/Lon): 47.5069, 11.5912, altitude: 946
 * ##Parameter: Air Temperature
 * #ZRXPVERSION30|*|ZRXPCREATORMeteoIO2.80|*|
 * #REXCHANGEMIO_101329_TA|*|RINVAL-999|*|
 * #SANR101329|*|
 * #TZUTC0|*|
 * #CNAMETA|*|CUNITK|*|
 * #LAYOUT(timestamp,value,status,remark)|*|
 * 20190130203000 269.81 41 "processed by mio"
 * 20190130204500 269.66 41 "processed by mio"
 * 20190130210000 269.63 41 "processed by mio"
 * 20190130211500 269.56 41 "processed by mio"
 * 20190130213000 269.49 41 "processed by mio"
 * 20190130214500   -999 44 "processed by mio"
 * 20190130220000 269.28 41 "processed by mio"
 * @endcode
 * An arbitrary number of such blocks may be present, so in this example after all values for `TA`
 * were printed the next header starting with \c \#\#Station... could follow below.
 * Here, more information about the <a href="https://www.tbbm.at/display/TBBMAT/ZRXP">ZRXP format specifications</a>
 * is available.
 */

/**
 * @brief Plugin constructor taking a configfile and doing nothing else.
 * @param configfile Your MeteoIO configuration file.
 */
ZRXPIO::ZRXPIO(const std::string& configfile) : cfg(configfile)
{
	//do nothing;
}

 /**
  * @brief Plugin constructor taking a cfgreader and doing nothing else.
  * @param cfgreader Your MeteoIO config reader object.
  */
ZRXPIO::ZRXPIO(const Config& cfgreader) : cfg(cfgreader)
{
	//do nothing;
}

/**
 * @brief Output-routine to ASCII on file system.
 * @param[in] vecMeteo MeteoData vector containing the dataset
 * @param[in] name This parameter is unused
 */
void ZRXPIO::writeMeteoData(const std::vector< std::vector<MeteoData> >& vecMeteo, const std::string& /*name*/)
{

	std::string outpath; //output folder
	cfg.getValue("METEOPATH", "Output", outpath);
	std::string sep("|");
	cfg.getValue("ZRXP_SEPARATOR", "Output", sep, IOUtils::nothrow);
	sep = sep + "*" + sep; //a WISKI separator looks like this: |*| or ;*;
	std::string zrxp_file_ext("zrxp");
	cfg.getValue("ZRXP_FILE_EXTENSION", "Output", zrxp_file_ext, IOUtils::nothrow);
	if (zrxp_file_ext.substr(0, 1) == ".") zrxp_file_ext.erase(0, 1); //allow "rxp" and ".rxp"

	std::string zrxp_write_units("KEYWORD"); //optional since WISKI can be picky if the unit is fixed
	cfg.getValue("ZRXP_WRITE_UNITS", "Output", zrxp_write_units, IOUtils::nothrow);
	IOUtils::toUpper(zrxp_write_units); //anything but "COMMENT" and "KEYWORD" means "OFF"
	bool zrxp_convert_latin(false);
	cfg.getValue("ZRXP_CONVERT_LATIN", "Output", zrxp_convert_latin, IOUtils::nothrow);

	bool zrxp_write_cnr(true); //output parameter index
	cfg.getValue("ZRXP_WRITE_CNR", "Output", zrxp_write_cnr, IOUtils::nothrow);

	//read which quality flags should be set for filtered, resampled, generated, and nodata:
	int qa_unaltered(41), qa_resampled(42), qa_generated(43), qa_filtered(44), qa_nodata, qa_unaltered_nodata(255);
	bool use_qa_nodata(false); //disabled by default
	cfg.getValue("ZRXP_STATUS_UNALTERED", "Output", qa_unaltered, IOUtils::nothrow);
	cfg.getValue("ZRXP_STATUS_RESAMPLED", "Output", qa_resampled, IOUtils::nothrow);
	cfg.getValue("ZRXP_STATUS_GENERATED", "Output", qa_generated, IOUtils::nothrow);
	cfg.getValue("ZRXP_STATUS_FILTERED", "Output", qa_filtered, IOUtils::nothrow);
	cfg.getValue("ZRXP_STATUS_UNALTERED_NODATA", "Output", qa_unaltered_nodata, IOUtils::nothrow);
	if (cfg.keyExists("ZRXP_STATUS_NODATA", "Output")) { //value is nodata before and/or after filtering
		cfg.getValue("ZRXP_STATUS_NODATA", "Output", qa_nodata);
		use_qa_nodata = true; //if not used, nodata is not treated differently
	}

	//normally, nodata is transportet to RINVAL, but we also allow something different:
	double zrxp_rinval = IOUtils::nodata;
	if (cfg.keyExists("ZRXP_RINVAL", "Output")) { //now, nodata and RINVAL could be different
		cfg.getValue("ZRXP_RINVAL", "Output", zrxp_rinval);
	}

	bool output_nodata_params(true);
	cfg.getValue("ZRXP_NODATA_PARAMS", "Output", output_nodata_params, IOUtils::nothrow);

	//read remark and then construct layout string:
	std::string zrxp_layout("(timestamp,value,status");
	std::string zrxp_remark("");
	if (cfg.keyExists("ZRXP_REMARK", "Output")) { //a remark is transferred
		cfg.getValue("ZRXP_REMARK", "Output", zrxp_remark);
		zrxp_remark = " \"" + zrxp_remark + "\""; //add quotes to allow spaces
		zrxp_layout = zrxp_layout + ",remark";
	}
	zrxp_layout = zrxp_layout + ")";

	Date now; //for an output comment
	now.setFromSys();

	for (size_t ii = 0; ii < vecMeteo.size(); ++ii) { //loop through stations

		if (vecMeteo[ii].empty()) continue; //instead of .at() for speed

		//assuming constant metadata per station (WISKI tracks such changes in timelines)
		StationData sd = vecMeteo[ii].front().meta;
		if (sd.stationID.empty()) {
			std::ostringstream ss;
			ss << "Station" << ii+1;
			sd.stationID = ss.str();
		}

		std::vector<bool> vecUsedParams; //will be filled with true or false for each param
		bool data_exists(false);
		const size_t nr_of_params = vecMeteo[ii].front().getNrOfParameters();
		checkForUsedParameters(vecMeteo[ii], vecUsedParams, nr_of_params, output_nodata_params, data_exists);

		if (data_exists) { //don't open the file if there is no data at all

			//Open output file:
			std::string filepath = FileUtils::cleanPath(outpath + "/" + sd.stationID + "." + zrxp_file_ext, false);
			if (!FileUtils::validFileAndPath(filepath))
				throw InvalidNameException(filepath, AT);

			std::ofstream outfile;
			outfile.open(filepath.c_str());

			if (outfile.fail()) {
				std::ostringstream ss;
				ss << "ZRXP plugin could not open file \"" << filepath << "\" for writing. Possible reason: "
				   << std::strerror(errno);
				throw AccessException(ss.str(), AT);
			}

			for (size_t pp = 0; pp < nr_of_params; ++pp) { //loop through parameters

				if (!vecUsedParams[pp]) continue; //don't output all nodata params

				const std::string param_name = vecMeteo[ii].front().getNameForParameter(pp);
				//only the MeteoGrids class offers units, so get it from there, if available:
				std::string param_unit("N/A"), param_description("N/A");
				if (param_name.substr(0, 3) == "TS.") { //there are no standard names for this; SNOWPACK expects TS1, TS2, ...
						param_unit = "K";
						param_description = "Snow temperature " + param_name.substr(3) + " cm above ground";
				} else {
					const size_t param_idx = MeteoGrids::getParameterIndex(param_name);
					if (param_idx != IOUtils::npos) {
						param_unit = MeteoGrids::getParameterUnits(MeteoGrids::getParameterIndex(param_name));
						param_description = MeteoGrids::getParameterDescription(MeteoGrids::getParameterIndex(param_name));
						if ( zrxp_convert_latin && (param_unit == "°") ) //WISKI needs ISO-8859-1 - this is the only troublesome standard unit
							param_unit = static_cast<char>(0xb0); //let the OS encode it
					}
				}

				//this should be a unique data exchange key
				const std::string exchange_no = "MIO_" + sd.stationID + "_" + param_name;

				//each parameter gets its own time series with a dedicated header:
				outfile << "##Station: " << sd.stationID << ", export at " << now.toString(Date::ISO, false)
						<< " (TZ: " << now.getTimeZone() << ")" << std::endl;
				outfile << "##Station coordinates (Lat/Lon): " << sd.position.getLat() << ", " << sd.position.getLon()
						<< ", altitude: " << sd.position.getAltitude() << std::endl;
				outfile << "##Parameter: " << param_description;
				if (zrxp_write_units == "COMMENT")
					outfile << " (" << param_unit << ")";
				outfile << std::endl;
				outfile << "#ZRXPVERSION30" << sep << "ZRXPCREATORMeteoIO" << getLibVersion(true) << sep << std::endl
						<< "#REXCHANGE" << exchange_no << sep << "RINVAL" << zrxp_rinval << sep << std::endl;
				outfile << "#SANR" << sd.stationID << sep;
				if (zrxp_write_cnr)
					outfile << "CNR" << pp << sep;
				outfile << std::endl;
				outfile << "#TZUTC" << vecMeteo[ii].front().date.getTimeZone() << sep << std::endl;
				outfile << "#CNAME" << param_name << sep;
				if ( (zrxp_write_units == "KEYWORD") && (param_unit != "N/A") ) //"N/A" is put in the description but not as valid unit
					outfile << "CUNIT" << param_unit << sep;
				outfile << std::endl;
				outfile << "#LAYOUT" << zrxp_layout << sep << std::endl;

				for (size_t jj = 0; jj < vecMeteo[ii].size(); ++jj) { //loop through datasets
					double out_value = vecMeteo[ii][jj](pp);
					//transport data qa flags (same for all parameters of a timestep, except qa_nodata):
					//qa_unaltered(41), qa_resampled(42), qa_filtered(43), qa_generated(44), qa_nodata=190, qa_unaltered_nodata(255);
					int qa_status(qa_unaltered);
					if (vecMeteo[ii][jj].isGenerated(pp)) {
						qa_status = qa_generated;
					} else if (vecMeteo[ii][jj].isResampledParam(pp)) {
						qa_status = qa_resampled;
					} else if (vecMeteo[ii][jj].isFiltered(pp)) {
						if ((vecMeteo[ii][jj](pp) == IOUtils::nodata) && use_qa_nodata)
							qa_status = qa_nodata;
						else
							qa_status = qa_filtered;
					} else if (vecMeteo[ii][jj](pp) == IOUtils::nodata) { //transport nodata without touching it
						qa_status = qa_unaltered_nodata;
						out_value = zrxp_rinval;
					}
					//print timestamp, value, status, and optional remark
					outfile << vecMeteo[ii][jj].date.toString(Date::NUM) << " " << out_value
								<< " " << qa_status << zrxp_remark << std::endl;
				} //endfor jj

			} //endfor params

			outfile.close();
		} //endif data_exists
	}
}

/**
 * @brief Check meteo set for used parameters.
 * @param vecMeteo Meteo data that is checked for available parameters.
 * @param[out] vecUsedParams Vector that is filled with true or false for parameter availability.
 * @param nr_of_params Number of parameters to check for
 */
void ZRXPIO::checkForUsedParameters(const std::vector<MeteoData>& vecMeteo, std::vector<bool>& vecUsedParams,
    const size_t& nr_of_params, const bool& output_all_filtered, bool& data_exists)
{
	vecUsedParams.clear();
	for (size_t pp = 0; pp < nr_of_params; ++pp) { //loop through parameters
		vecUsedParams.push_back(false);
		for (size_t ii = 0; ii < vecMeteo.size(); ++ii) {
			if ( (vecMeteo[ii](pp) != IOUtils::nodata) || (vecMeteo[ii].isFiltered(pp) && output_all_filtered) ) { //at least one datapoint of this param is available
				vecUsedParams[pp] = true;
				data_exists = true; //global check
				break;
			}
		}
	}
}


} //namespace
