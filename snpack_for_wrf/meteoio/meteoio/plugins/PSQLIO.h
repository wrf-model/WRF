/***********************************************************************************/
/*  Copyright 2012 Mountain-eering Srl, Trento/Bolzano, Italy                      */
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
#ifndef PSQLIO_H
#define PSQLIO_H

#include <meteoio/IOInterface.h>

#include <libpq-fe.h>
#include <string>
#include <map>

namespace mio {

/**
 * @class PSQLIO
 * @brief This plugin connects to a generic PostgreSQL server to retrieve its meteorological data.
 *
 * This plugin was funded by <A HREF="http://www.mountain-eering.com">Mountain-eering</A>.
 * @ingroup plugins
 * @author Thomas Egger
 * @date   2014-01-28
 */
class PSQLIO : public IOInterface {
	public:
		PSQLIO(const std::string& configfile);
		PSQLIO(const PSQLIO&);
		PSQLIO(const Config& cfg);
		
		PSQLIO& operator=(const PSQLIO& in);

		virtual void readStationData(const Date& date, std::vector<StationData>& vecStation);
		virtual void readMeteoData(const Date& dateStart, const Date& dateEnd,
		                           std::vector< std::vector<MeteoData> >& vecMeteo);

		virtual void writeMeteoData(const std::vector< std::vector<MeteoData> >& vecMeteo,
		                            const std::string& name="");

	private:
		void getParameters(const Config& cfg);
		void open_connection(const bool& input=true);
		PGresult* sql_exec(const std::string& sqlcommand, const bool& input=true);
		static bool replace(std::string& str, const std::string& from, const std::string& to);
		void readData(const Date& dateStart, const Date& dateEnd, std::vector<MeteoData>& vecMeteo, const size_t& stationindex);
		void readMetaData(const std::string& query, std::vector<StationData>& vecStation, const bool& input=true);
		void add_meta_data(const unsigned int& index, const StationData& sd);
		static void map_parameters(const PGresult* result, MeteoData& md, std::vector<size_t>& index);
		void parse_row(const PGresult* result, const int& row, const int& cols,
		               MeteoData& md, const std::vector<size_t>& index, std::vector<mio::MeteoData>& vecMeteo) const;
		void close_connection(PGconn *conn);
		static bool checkConsistency(const std::vector<MeteoData>& vecMeteo, StationData& sd);
		static size_t checkExistence(const std::vector<StationData>& vec_stations, const StationData& sd);
		static void convertUnits(MeteoData& meteo);
		static void convertUnitsBack(MeteoData& meteo);
		static void checkForUsedParameters(const std::vector<MeteoData>& vecMeteo, std::vector<bool>& vecParamInUse, std::vector<std::string>& vecColumnName);
		void add_sensors(const unsigned int& index, const std::vector<std::string>& vecColumnName, std::map<size_t, std::string>& map_sensor_id);
		int get_sensor_index();
		int get_measurement_index();
		void get_sensors(const std::string& index, const std::vector<std::string>& vecColumnName, std::map<size_t, std::string>& map_sensor_id);

		std::string coordin, coordinparam, coordout, coordoutparam; //projection parameters
		std::string in_endpoint, in_port, in_dbname, in_userid, in_passwd; ///< Variables for endpoint configuration
		std::string out_endpoint, out_port, out_dbname, out_userid, out_passwd; ///< Variables for endpoint configuration
		bool input_configured, output_configured;

		PGconn *psql; ///<holds the current connection
		double default_timezone;
		std::vector<StationData> vecMeta;
		std::vector<std::string> vecFixedStationID, vecMobileStationID;
		std::string sql_meta, sql_data;

		static const std::string sqlInsertMetadata, sqlInsertSensor, sqlInsertMeasurement;
		static const double plugin_nodata; //plugin specific nodata value, e.g. -999
};

} //namespace
#endif
