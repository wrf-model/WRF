/***********************************************************************************/
/*  Copyright 2014 Snow and Avalanche Study Establishment    SASE-CHANDIGARH       */
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
#ifndef SASEIO_H
#define SASEIO_H

#include <meteoio/IOInterface.h>

#include <string>

namespace mio {

/**
 * @class SASEIO
 * @brief This is the plugin required to get meteorological data from the SASE database.
 *
 * @ingroup plugins
 * @author Paramvir Singh Mathias Bavay
 * @date   2014-12-15
 */
class SASEIO : public IOInterface {
	public:
		SASEIO(const std::string& configfile);
		SASEIO(const SASEIO&);
		SASEIO(const Config& cfgreader);

		virtual void readStationData(const Date& date, std::vector<StationData>& vecStation);
		virtual void readMeteoData(const Date& dateStart, const Date& dateEnd,
		                           std::vector< std::vector<MeteoData> >& vecMeteo);

	private:
	    	void readConfig();
		void readStationIDs(std::vector<std::string>& vecStationID) const;
		static void parseStationID(const std::string& stationID, std::string& stnAbbrev, std::string& stnNumber);
		void getStationMetaData(const std::string& stat_abk, const std::string& stao_nr,const std::string& sqlQuery,
		                        std::vector<std::string>& vecMetaData);
		void readStationMetaData();
		void readData(const Date& dateStart, const Date& dateEnd, std::vector< std::vector<MeteoData> >& vecMeteo,
		              const size_t& stationindex, const std::vector<StationData>& vecMeta) const;
		static void convertUnits(MeteoData& meteo);
		void parseDataSet(const std::vector<std::string>& i_meteo, MeteoData& md) const;
		bool getStationData(const std::string& stat_abk, const std::string& stao_nr,const Date& dateS,
		                    const Date& dateE,const std::vector<std::string>& vecHTS1,
		                    std::vector< std::vector<std::string> >& vecMeteoData) const;

		const Config cfg;
		std::vector<std::string> vecStationIDs;
		std::vector<StationData> vecStationMetaData;
		std::string mysqlhost, mysqldb, mysqluser, mysqlpass;
		std::string coordin, coordinparam, coordout, coordoutparam; //projection parameters
		double in_dflt_TZ, out_dflt_TZ;

		static const double plugin_nodata; //plugin specific nodata value, e.g. -999
		static const std::string MySQLQueryStationMetaData;
		static const std::string MySQLQueryMeteoData;
};

} //namespace
#endif

