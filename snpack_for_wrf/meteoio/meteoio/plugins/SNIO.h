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
#ifndef SNIO_H
#define SNIO_H

#include <meteoio/IOInterface.h>
#include <meteoio/FileUtils.h>

#include <string>

namespace mio {

/**
 * @class SNIO
 * @brief This class enables the access to meteo data stored in SNOWPACK format
 *
 * @ingroup plugins
 * @author Mathias Bavay
 * @date   2009-12-04
 */
class SNIO : public IOInterface {
	public:
		SNIO(const std::string& configfile);
		SNIO(const SNIO&);
		SNIO(const Config& cfgreader);

		virtual void readStationData(const Date& date, std::vector<StationData>& vecStation);
		virtual void readMeteoData(const Date& dateStart, const Date& dateEnd,
		                           std::vector< std::vector<MeteoData> >& vecMeteo);

		virtual void writeMeteoData(const std::vector< std::vector<MeteoData> >& vecMeteo,
		                            const std::string& name="");

	private:
		static std::string file_pos(const std::string& filename, const size_t& linenr);
		void writeStationMeteo(const std::vector<MeteoData>& Meteo, const std::string& file_name, std::ofstream& fout);
		void convertUnits(MeteoData& meteo);
		void convertUnitsBack(MeteoData& meteo);
		double cloudiness_to_ilwr (const double& RH, const double& TA, const double& cloudiness );
		bool parseMeteoLine(const std::vector<std::string>& vecLine, const std::string& filename,
		                    const size_t& linenr, const Date& dateStart, const Date& dateEnd, MeteoData& md);
		bool readStationMetaData(const std::string& metafile, const std::string& stationname, StationData& sd);
		void readMetaData();
		std::string getStationID(const std::string& filename);
		void parseMetaDataLine(const std::vector<std::string>& vecLine, StationData& sd);

		const Config cfg;
		std::vector<StationData> vecAllStations;
		std::vector<std::string> vecFilenames;
		std::vector< FileUtils::FileIndexer > vecIndex;
		std::string coordin, coordinparam, coordout, coordoutparam; //projection parameters
		double in_tz, out_tz;
		static const char* dflt_extension;
		static const double plugin_nodata; //plugin specific nodata value, e.g. -999
		static const int sn_julian_offset;
		static const size_t min_nr_meteoData; // minimal number of parameters on data input lines
		static const size_t streampos_every_n_lines; //save current stream pos every n lines of data
		size_t nr_meteoData; // number of parameters on data input lines, excluding optional ones
		size_t number_meas_temperatures, number_of_solutes;
		bool vw_drift, rho_hn;
		bool iswr_inp, rswr_inp;
};

} //namespace
#endif
