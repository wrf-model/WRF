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
#include <meteoio/plugins/PSQLIO.h>

#include <set>
#include <algorithm>

using namespace std;

namespace mio {
/**
 * @page psqlio PSQLIO
 * @section psql_format Format
 * This plugin connects to a <i>generic</i> <A HREF="www.postgresql.org/">PostgreSQL</A> server to retrieve its meteorological data. The server
 * parameters must be provided as well as the queries to retrieve the stations' data and metadata. In order to compile this plugin,
 * the development package of libpq is required (this is the PostgreSQL c client library) and most probably also postgresql-server-dev-all.
 *
 * @subsection psql_meta_query Metadata query
 * This query is used to retrieve the stations' metadata. This SQL query string should retrieve the following columns as result set (in this very order):
 *
 *      id (int), name (string), x (easting as double), y (northing as double), altitude (height above sea level as double), epsg (int)
 *
 * The user is allowed to select stations with the STATIONS keyword (see below). That is why the SQL query has to end with a 'WHERE id_column_name IN' clause, for example:
 *
 *      SELECT id, station_name AS name, x_coord AS x, y_coord AS y, z AS altitude, epsg from all_stations WHERE id IN
 *
 * @subsection psql_data_query Data query
 * This query is used to retrieve the data for the user selected stations within a given time interval.
 * The SQL query may retrieve the following columns as result set (any order, only date is mandatory):
 *
 *      date (mandatory, as date), ta (double), rh (double), p (double), vw (double), dw (double), iprec (the PSUM value, double), iswr (double)
 *
 * The SQL query must retrieve the data for one station only, which has to be specified as \a STATIONID (this will be dynamically replaced by the plugin).
 * To set the upper and lower bounds for the date the SQL query has to contain \a DATE_START and \a DATE_END. These keywords will be dynamically replaced by
 * the plugin with the correct date. Furthermore the resultset should be ordered by date ascending. An example for a correct SQL data query string is therefore:
 *
 *      SELECT * FROM all_measurements WHERE id = ''STATIONID'' AND date>=''DATE_START'' AND date<=''DATE_END'' ORDER BY date
 *
 * @section psql_units Units
 * Units are assumed to be pure SI, except:
 *  - temperatures in &deg;C
 *  - relative humidity in %
 *  - snow height in cm
 *  - pressure in mbar
 *
 * @section psql_keywords Keywords
 * This plugin uses the following keywords:
 * - COORDSYS: coordinate system (see Coords); [Input] section
 * - COORDPARAM: extra coordinates parameters (see Coords); [Input] section
 * - database connection keywords; [Input] and [Output] sections:
 *      - PSQL_URL: The URL or IP of the database server
 *      - PSQL_PORT: the port to use to connect
 *      - PSQL_DB: The name of the database to access
 *      - PSQL_USER: The username to access the server
 *      - PSQL_PASS: The password to authenticate the PSQL_USER
 * - database structure keywords; [Input] section
 *      - SQL_META: SQL query to use to get the stations' metadata.
 *      - SQL_DATA: SQL query to use to get the stations' data.
 * - STATIONS: comma separated list of station ids that the user is interested in; [Input] section
 * 
 * @note Currently, the output structure is fixed with a hard-coded table name and hard-coded fields so it can not be considered usable by most users...
 *
 */

const double PSQLIO::plugin_nodata = -999.; //plugin specific nodata value. It can also be read by the plugin (depending on what is appropriate)

//Hard-coded output queries HACK some selects are also made in the code!!
const std::string PSQLIO::sqlInsertMetadata = "INSERT INTO FIXED_STATION (ID_FIXED_STATION,STATION_NAME,COORD_X,COORD_Y,ALTITUDE, EPSG) VALUES ";
const std::string PSQLIO::sqlInsertSensor = "INSERT INTO FIXED_SENSOR (ID_FIXED_SENSOR,FK_ID_FIXED_STATION,FK_ID_MEASUREMENT_TYPE,MEAS_HEIGHT) VALUES ";
const std::string PSQLIO::sqlInsertMeasurement = "INSERT INTO FIXED_MEASUREMENT (ID_FIXED_MEASUREMENT,FK_ID_FIXED_SENSOR,MEAS_DATE,MEAS_VALUE) VALUES ";

PSQLIO::PSQLIO(const std::string& configfile) : coordin(), coordinparam(), coordout(), coordoutparam(), in_endpoint(), in_port(),
                                                in_dbname(), in_userid(), in_passwd(), out_endpoint(), out_port(), out_dbname(),
                                                out_userid(), out_passwd(), input_configured(false), output_configured(false),
                                                psql(NULL), default_timezone(1.), vecMeta(), vecFixedStationID(),
                                                vecMobileStationID(), sql_meta(), sql_data()
{
	Config cfg(configfile);
	IOUtils::getProjectionParameters(cfg, coordin, coordinparam, coordout, coordoutparam);
	getParameters(cfg);
}

PSQLIO::PSQLIO(const Config& cfg) : coordin(), coordinparam(), coordout(), coordoutparam(), in_endpoint(), in_port(),
                                    in_dbname(), in_userid(), in_passwd(), out_endpoint(), out_port(), out_dbname(),
                                    out_userid(), out_passwd(), input_configured(false), output_configured(false),
                                    psql(NULL), default_timezone(1.), vecMeta(), vecFixedStationID(),
                                    vecMobileStationID(), sql_meta(), sql_data()
{
	IOUtils::getProjectionParameters(cfg, coordin, coordinparam, coordout, coordoutparam);
	getParameters(cfg);
}

PSQLIO::PSQLIO(const PSQLIO& in) : coordin(in.coordin), coordinparam(in.coordinparam), coordout(in.coordout),
                                   coordoutparam(in.coordoutparam), in_endpoint(in.in_endpoint), in_port(in.in_port),
                                   in_dbname(in.in_dbname), in_userid(in.in_userid), in_passwd(in.in_passwd),
                                   out_endpoint(in.out_endpoint), out_port(in.out_port), out_dbname(in.out_dbname),
                                   out_userid(in.out_userid), out_passwd(in.out_passwd), input_configured(false),
                                   output_configured(false), psql(NULL), default_timezone(1.), vecMeta(in.vecMeta),
                                   vecFixedStationID(in.vecFixedStationID), vecMobileStationID(in.vecMobileStationID),
                                   sql_meta(in.sql_meta), sql_data(in.sql_data) {}

PSQLIO& PSQLIO::operator=(const PSQLIO& in)
{
	PSQLIO tmp(in);

	std::swap(coordin, tmp.coordin);
	std::swap(coordinparam, tmp.coordinparam);
	std::swap(coordout, tmp.coordout);
	std::swap(coordoutparam, tmp.coordoutparam);
	std::swap(in_endpoint, tmp.in_endpoint);
	std::swap(in_port, tmp.in_port);
	std::swap(in_dbname, tmp.in_dbname);
	std::swap(in_userid, tmp.in_userid);
	std::swap(in_passwd, tmp.in_passwd);
	std::swap(out_endpoint, tmp.out_endpoint);
	std::swap(out_port, tmp.out_port);
	std::swap(out_dbname, tmp.out_dbname);
	std::swap(out_userid, tmp.out_userid);
	std::swap(out_passwd, tmp.out_passwd);
	std::swap(input_configured, tmp.input_configured);
	std::swap(output_configured, tmp.output_configured);
	std::swap(psql, tmp.psql);
	std::swap(default_timezone, tmp.default_timezone);
	std::swap(vecMeta, tmp.vecMeta);
	std::swap(vecFixedStationID, tmp.vecFixedStationID);
	std::swap(vecMobileStationID, tmp.vecMobileStationID);
	std::swap(sql_meta, tmp.sql_meta);
	std::swap(sql_data, tmp.sql_data);

	return *this;
}

void PSQLIO::getParameters(const Config& cfg)
{
	in_port = "5432"; //The default PostgreSQL port
	out_port = "5432"; //The default PostgreSQL port

	try {
		cfg.getValue("PSQL_URL", "Input", in_endpoint);
		cfg.getValue("PSQL_PORT", "Input", in_port, IOUtils::nothrow);
		cfg.getValue("PSQL_DB", "Input", in_dbname);
		cfg.getValue("PSQL_USER", "Input", in_userid);
		cfg.getValue("PSQL_PASS", "Input", in_passwd);

		cfg.getValue("SQL_META", "Input", sql_meta);
		cfg.getValue("SQL_DATA", "Input", sql_data);
		input_configured = true;
	} catch (...) {
		input_configured = false;
	}

	try {
		cfg.getValue("PSQL_URL", "Output", out_endpoint);
		cfg.getValue("PSQL_PORT", "Output", out_port, IOUtils::nothrow);
		cfg.getValue("PSQL_DB", "Output", out_dbname);
		cfg.getValue("PSQL_USER", "Output", out_userid);
		cfg.getValue("PSQL_PASS", "Output", out_passwd);
		output_configured = true;
	} catch (...) {
		output_configured = false;
	}

	std::string stations;
	cfg.getValue("STATIONS", "Input", stations, IOUtils::nothrow);
	IOUtils::readLineToVec(stations, vecFixedStationID, ',');

	cfg.getValue("TIME_ZONE", "Input", default_timezone, IOUtils::nothrow);
}

void PSQLIO::readMetaData(const std::string& query, std::vector<StationData>& vecStation, const bool& input)
{
	if (input && !input_configured) throw IOException("Please configure all necessary parameters in the [Input] section", AT);
	if (!input && !output_configured) throw IOException("Please configure all necessary parameters in the [Output] section", AT);

	PGresult *result( sql_exec(query, input) );
	if (result) {
		const int rows = PQntuples(result);

		const int col_id = PQfnumber(result, "id");
		const int col_name = PQfnumber(result, "name");
		const int col_x = PQfnumber(result, "x");
		const int col_y = PQfnumber(result, "y");
		const int col_alt = PQfnumber(result, "altitude");
		const int col_epsg = PQfnumber(result, "epsg");

		if ((col_id * col_name * col_x * col_y * col_alt * col_epsg) < 0) { //missing column
			throw IOException("Result set does not have all necessary columns", AT);
		}

		for (int ii=0; ii<rows; ii++) {
			int epsg;
			double easting, northing, altitude;

			IOUtils::convertString(epsg, PQgetvalue(result, ii, col_epsg));
			IOUtils::convertString(easting, PQgetvalue(result, ii, col_x));
			IOUtils::convertString(northing, PQgetvalue(result, ii, col_y));
			IOUtils::convertString(altitude, PQgetvalue(result, ii, col_alt));

			Coords point;
			point.setEPSG(epsg);
			point.setXY(easting, northing, altitude);

			StationData sd(point, PQgetvalue(result, ii, col_id), PQgetvalue(result, ii, col_name));
			vecStation.push_back(sd); //this is ordered ascending by id
		}

		PQclear(result);
	}
}

void PSQLIO::readStationData(const Date&, std::vector<StationData>& vecStation)
{
	if (!input_configured) throw IOException("Please configure all necessary parameters in the [Input] section", AT);

	if (!vecMeta.empty()) {
		vecStation = vecMeta;
		return;
	}

	vecStation.clear();
	std::string station_list;

	if (vecFixedStationID.empty() && vecMobileStationID.empty()) {
		return; //nothing to do
	} else {
		for (std::vector<std::string>::const_iterator it = vecFixedStationID.begin(); it != vecFixedStationID.end(); ++it) {
			if (it != vecFixedStationID.begin()) {
				station_list += ", ";
			}
			station_list += "'" + *it + "'";
		}
	}

	const std::string query( sql_meta + " (" + station_list + ") ORDER BY id;" );
	std::vector<StationData> tmp_station;
	readMetaData(query, tmp_station);

	for (std::vector<std::string>::const_iterator it = vecFixedStationID.begin(); it != vecFixedStationID.end(); ++it) {
		for (std::vector<StationData>::const_iterator station_it = tmp_station.begin(); station_it != tmp_station.end(); ++station_it) {
			if ((*station_it).stationID == *it) {
				vecStation.push_back(*station_it);
			}
		}
	}
}

void PSQLIO::readMeteoData(const Date& dateStart, const Date& dateEnd,
                           std::vector< std::vector<MeteoData> >& vecMeteo)
{
	if (!input_configured) throw IOException("Please configure all necessary parameters in the [Input] section", AT);

	if (vecMeta.empty()) readStationData(dateStart, vecMeta);
	if (vecMeta.empty()) return; //if there are no stations -> return

	vecMeteo.clear();
	vecMeteo.insert(vecMeteo.begin(), vecMeta.size(), vector<MeteoData>());

	for (size_t ii=0; ii<vecMeta.size(); ii++){ //loop through stations
		readData(dateStart, dateEnd, vecMeteo[ii], ii);
	}
}

bool PSQLIO::replace(std::string& str, const std::string& from, const std::string& to)
{
	const size_t start_pos = str.find(from);
	if (start_pos == std::string::npos)
		return false;
	str.replace(start_pos, from.length(), to);
	return true;
}

void PSQLIO::readData(const Date& dateStart, const Date& dateEnd, std::vector<MeteoData>& vecMeteo, const size_t& stationindex)
{
	std::string sql_query(sql_data);

	std::string date_start = dateStart.toString(Date::ISO);
	std::string date_end = dateEnd.toString(Date::ISO);
	std::replace(date_start.begin(), date_start.end(), 'T', ' ');
	std::replace(date_end.begin(), date_end.end(), 'T', ' ');

	replace(sql_query, "STATIONID", vecMeta.at(stationindex).stationID);
	replace(sql_query, "DATE_START", date_start);
	replace(sql_query, "DATE_END", date_end);

	PGresult *result( sql_exec(sql_query) );
	if (result) {
		const int rows = PQntuples(result);
		const int columns = PQnfields(result);

		std::vector<size_t> index;
		MeteoData tmpmeteo;
		tmpmeteo.meta = vecMeta.at(stationindex);

		map_parameters(result, tmpmeteo, index);

		for (int ii=0; ii<rows; ii++) {
			parse_row(result, ii, columns, tmpmeteo, index, vecMeteo);
		}

		PQclear(result);
	}
}

void PSQLIO::parse_row(const PGresult* result, const int& row, const int& cols, MeteoData& md, const std::vector<size_t>& index, std::vector<mio::MeteoData>& vecMeteo) const
{
	MeteoData tmp(md);
	IOUtils::convertString(tmp.date, PQgetvalue(result, row, 0), default_timezone);

	for (int ii=1; ii<cols; ii++) {
		if (index[ii] != IOUtils::npos) {
			const std::string val( PQgetvalue(result, row, ii) );
			if (!val.empty()) IOUtils::convertString(tmp(index[ii]), val);
		}
	}

	convertUnits(tmp);
	vecMeteo.push_back(tmp);
}

void PSQLIO::map_parameters(const PGresult* result, MeteoData& md, std::vector<size_t>& index)
{
	const int columns = PQnfields(result);
	
	for (int ii=0; ii<columns; ii++) {
		const std::string field_name( IOUtils::strToUpper(PQfname(result, ii)) );

		if (field_name == "RH") {
			index.push_back(MeteoData::RH);
		} else if (field_name == "TA") {
			index.push_back(MeteoData::TA);
		} else if (field_name == "DW") {
			index.push_back(MeteoData::DW);
		} else if (field_name == "VW") {
			index.push_back(MeteoData::VW);
		} else if (field_name == "ISWR") {
			index.push_back(MeteoData::ISWR);
		} else if (field_name == "RSWR") {
			index.push_back(MeteoData::RSWR);
		} else if (field_name == "HS") {
			index.push_back(MeteoData::HS);
		} else if (field_name == "IPREC") {
			index.push_back(MeteoData::PSUM);
		} else if (field_name == "TSS") {
			index.push_back(MeteoData::TSS);
		} else if (field_name == "TSG") {
			index.push_back(MeteoData::TSG);
		} else if (field_name == "P") {
			index.push_back(MeteoData::P);
		} else { //this is an extra parameter
			md.addParameter(field_name);
			const size_t parindex = md.getParameterIndex(field_name);
			index.push_back(parindex);
		}
	}
}

bool PSQLIO::checkConsistency(const std::vector<MeteoData>& vecMeteo, StationData& sd)
{
	/**
	 * This function checks whether all the MeteoData elements in vecMeteo are consistent
	 * regarding their meta data (position information, station name). If they are consistent
	 * true is returned, otherwise false
	 */

	if (!vecMeteo.empty()) // to get the station data even when in bug 87 conditions
		sd = vecMeteo[0].meta;

	for (size_t ii=1; ii<vecMeteo.size(); ii++){
		const Coords& p1 = vecMeteo[ii-1].meta.position;
		const Coords& p2 = vecMeteo[ii].meta.position;

		if (p1 != p2) {
			//we don't mind if p1==nodata or p2==nodata
			if (p1.isNodata()==false && p2.isNodata()==false) return false;
		}
	}

	return true;
}

void PSQLIO::checkForUsedParameters(const std::vector<MeteoData>& vecMeteo, std::vector<bool>& vecParamInUse, std::vector<std::string>& vecColumnName)
{
	if (vecMeteo.empty()) return;

	/**
	 * This procedure loops through all MeteoData objects present in vecMeteo and finds out which
	 * meteo parameters are actually in use, i. e. have at least one value that differs from IOUtils::nodata.
	 * If a parameter is in use, then vecParamInUse[index_of_parameter] is set to true and the column
	 * name is set in vecColumnName[index_of_parameter]
	 */
	const size_t nr_of_parameters = vecMeteo[0].getNrOfParameters();
	vecParamInUse.resize(nr_of_parameters, false);
	vecColumnName.resize(nr_of_parameters, "NULL");

	for (size_t ii=0; ii<vecMeteo.size(); ii++) {
		for (size_t jj=0; jj<nr_of_parameters; jj++) {
			if (!vecParamInUse[jj]) {
				if (vecMeteo[ii](jj) != IOUtils::nodata) {
					vecParamInUse[jj] = true;
					vecColumnName.at(jj) = vecMeteo[ii].getNameForParameter(jj);
				}
			}
		}
	}
}

size_t PSQLIO::checkExistence(const std::vector<StationData>& vec_stations, const StationData& sd)
{
	//This function checks whether the station is already present in the DB
	for (size_t ii=0; ii<vec_stations.size(); ii++) {
		if (sd == vec_stations[ii]) return ii;
	}

	return IOUtils::npos;
}

void PSQLIO::add_meta_data(const unsigned int& index, const StationData& sd)
{
	//Adding a new station to the table FIXED_STATION

	const std::string stationName = (sd.stationName != "" ? sd.stationName : sd.stationID);

	const short int epsg = sd.position.getEPSG();
	if (epsg==IOUtils::snodata)
		throw InvalidArgumentException("Station '"+stationName+"' does not have a EPSG code", AT);
	
	stringstream values;
	values << "(" << index << ","
		  << "'" << stationName << "'," << fixed
		  << setprecision(2) << sd.position.getEasting() << ","
		  << sd.position.getNorthing() << ","
		  << sd.position.getAltitude() << ","
		  << epsg << ")";

	const std::string query( sqlInsertMetadata + values.str() + ";" );
	sql_exec(query, false);
}

int PSQLIO::get_sensor_index()
{
	//Get first id of new sensors

	int sensor_index = 1;
	const std::string query( "SELECT max(id_fixed_sensor) from fixed_sensor;" );

	PGresult *result = sql_exec(query, false);
	if (result) {
		const int rows = PQntuples(result);
		const int columns = PQnfields(result);

		if (rows != 1 || columns != 1) {
			throw IOException("ERROR", AT);
		}

		const std::string val( PQgetvalue(result, 0, 0) );

		IOUtils::convertString(sensor_index, val);
		sensor_index++;

		PQclear(result);
	}

	return sensor_index;
}

void PSQLIO::add_sensors(const unsigned int& index, const std::vector<std::string>& vecColumnName, std::map<size_t, std::string>& map_sensor_id)
{
	//Adding new sensors for station with id index to the table FIXED_SENSOR

	std::string query( "SELECT id_measurement_type as id, meas_name from measurement_type order by id asc;" );
	int sensor_index = get_sensor_index();

	stringstream ss;
	ss << index;
	const std::string station_id( ss.str() );

	std::map<size_t, std::string> map_sensor_type;

	PGresult *result = sql_exec(query, false);
	if (result) {
		const int rows = PQntuples(result);
		//int columns = PQnfields(result);

		for (int ii=0; ii<rows; ii++) {
			const std::string id( PQgetvalue(result, ii, 0) );
			std::string type( PQgetvalue(result, ii, 1) );

			IOUtils::toUpper(type);
			IOUtils::trim(type);

			for (size_t jj=0; jj<vecColumnName.size(); jj++) {
				if (type == vecColumnName[jj]) map_sensor_type[jj] = id;
				if (type == "IPREC" && vecColumnName[jj] == "PSUM") map_sensor_type[jj] = id;
			}
		}

		PQclear(result);
	} else {
		throw;
	}

	// Now actually add all sensors that were identified
	for (map<size_t, string>::const_iterator it = map_sensor_type.begin(); it != map_sensor_type.end(); ++it) {
		ss.str("");
		ss << sensor_index;
		const std::string sensor_id( ss.str() );
		const std::string type( it->second );

		query = sqlInsertSensor + " (" + sensor_id + "," + station_id + "," + type  + ",0.0);";
		sql_exec(query, false);

		map_sensor_id[it->first] = sensor_id;
		sensor_index++;
	}
}

void PSQLIO::get_sensors(const std::string& index, const std::vector<std::string>& vecColumnName, std::map<size_t, std::string>& map_sensor_id)
{
	// Retrieve a mapping of all active meteo parameters and their respective sensor ids

	stringstream ss;
	ss << "SELECT id, station, meas_type, meas_name FROM "
	   << "(SELECT id_fixed_sensor as id, fk_id_fixed_station as station, fk_id_measurement_type as meas_type from fixed_sensor where fk_id_fixed_station=" << index << ") a "
	   << "INNER JOIN measurement_type ON a.meas_type=measurement_type.id_measurement_type;";

	const std::string query(  ss.str() );
	//cout << query << endl;

	PGresult *result = sql_exec(query, false);
	if (result) {
		const int rows = PQntuples(result);

		for (int ii=0; ii<rows; ii++) {
			const std::string id( PQgetvalue(result, ii, 0) );
			std::string type( PQgetvalue(result, ii, 3) );

			IOUtils::toUpper(type);
			IOUtils::trim(type);

			for (size_t jj=0; jj<vecColumnName.size(); jj++) {
				if (type == vecColumnName[jj]) map_sensor_id[jj] = id;
				if (type == "IPREC" && vecColumnName[jj] == "PSUM") map_sensor_id[jj] = id;
			}
		}

		PQclear(result);
	} else {
		throw;
	}

	/*for (map<size_t, string>::const_iterator it = map_sensor_id.begin(); it != map_sensor_id.end(); ++it) {
		cout << "Sensor for param: " << it->first << "  id: " << it->second << endl;
	}*/
}

int PSQLIO::get_measurement_index()
{
	//Get first id for new measurements to be added

	int index = 1;
	const std::string query( "SELECT MAX(ID_FIXED_MEASUREMENT) from fixed_measurement;" );

	PGresult *result = sql_exec(query, false);
	if (result) {
		const int rows = PQntuples(result);
		const int columns = PQnfields(result);

		if (rows != 1 || columns != 1) {
			throw IOException("ERROR", AT);
		}

		const std::string val( PQgetvalue(result, 0, 0) );

		IOUtils::convertString(index, val);
		index++;

		PQclear(result);
	}

	//cout << "Measurement index: " << index << endl;
	return index;
}

void PSQLIO::writeMeteoData(const std::vector< std::vector<MeteoData> >& vecMeteo, const std::string&)
{
	if (!output_configured) throw IOException("Please configure all necessary parameters in the [Output] section", AT);

	// Make sure we have an up to date set of all the stations already available in the DB
	vector<StationData> vecAllStations;
	readMetaData("select id_fixed_station as id, station_name as name, coord_x as x, coord_y as y, altitude, epsg from fixed_station ORDER BY id;", vecAllStations, false);

	unsigned int index = 1;

	if (vecAllStations.size()) {
		cout << "Found " << vecAllStations.size() << " stations overall, highest id: " << vecAllStations[vecAllStations.size()-1].stationID << endl;
		IOUtils::convertString(index, vecAllStations[vecAllStations.size()-1].stationID);
		index++;
	}

	for (size_t ii=0; ii<vecMeteo.size(); ii++){
		if (!vecMeteo[ii].size()) continue; // in case there is no data, we can't write anything to DB

		//1. check consistency of station data position -> write location in header or data section
		StationData sd;
		vector<bool> vecParamInUse;
		vector<string> vecColumnName;
		map<size_t, string> map_sensor_id;

		sd.position.setProj(coordout, coordoutparam);
		const bool isConsistent = checkConsistency(vecMeteo[ii], sd); // sd will hold valid meta info
		const size_t present_index = checkExistence(vecAllStations, sd);
		checkForUsedParameters(vecMeteo[ii], vecParamInUse, vecColumnName);

		if (isConsistent) { //static station
			if (present_index == IOUtils::npos) { //write into fixed_station
				cout << "Inserting data for station '" << sd.stationName << "' with the id_fixed_station " << index << endl;
				add_meta_data(index, sd);
				add_sensors(index, vecColumnName, map_sensor_id);
				index++;
			} else { // just get the sensor mappings
				cout << "Inserting data for station '" << sd.stationName << "' with the id_fixed_station " << vecAllStations[present_index].stationID << endl;
				get_sensors(vecAllStations[present_index].stationID, vecColumnName, map_sensor_id);
			}
		} else { //mobile station
			throw IOException("Mobile station writing not implemented", AT);
		}

		int currentid = get_measurement_index();
		stringstream ss;
		std::string query( sqlInsertMeasurement );
		bool comma = false;

		for (size_t jj=0; jj<vecMeteo[ii].size(); jj++) {
			MeteoData tmp(vecMeteo[ii][jj]);
			convertUnitsBack(tmp);

			string timestamp(vecMeteo[ii][jj].date.toString(Date::ISO));
			std::replace( timestamp.begin(), timestamp.end(), 'T', ' ');

			for (map<size_t, string>::const_iterator it = map_sensor_id.begin(); it != map_sensor_id.end(); ++it) {
				ss.str("");
				ss << currentid;
				const std::string id(ss.str());

				ss.str("");
				ss << tmp(it->first);
				const std::string value(ss.str());
				const std::string values( "(" + id + "," + it->second + ", TIMESTAMP '" + timestamp + "'," + value + ")" );

				if (!comma) {
					comma = true;
				} else {
					query += ",";
				}
				query += values;
				currentid++;
			}
		}

		query += ";";
		sql_exec(query, false);
	}
}

void PSQLIO::convertUnitsBack(MeteoData& meteo)
{
	//converts Kelvin to °C, converts RH to [0,100]
	double& ta = meteo(MeteoData::TA);
	ta = IOUtils::K_TO_C(ta);

	double& tsg = meteo(MeteoData::TSG);
	tsg = IOUtils::K_TO_C(tsg);

	double& tss = meteo(MeteoData::TSS);
	tss = IOUtils::K_TO_C(tss);

	double& rh = meteo(MeteoData::RH);
	if (rh != IOUtils::nodata)
		rh *= 100.;

	double& hs = meteo(MeteoData::HS);
	if (hs != IOUtils::nodata)
		hs *= 100.; //is in cm

	double& p = meteo(MeteoData::P);
	if (p != IOUtils::nodata)
		p /= 100.; //is in mbar
}

void PSQLIO::convertUnits(MeteoData& meteo)
{
	//converts °C to Kelvin, converts RH to [0,1]
	double& ta = meteo(MeteoData::TA);
	ta = IOUtils::C_TO_K(ta);

	double& tsg = meteo(MeteoData::TSG);
	tsg = IOUtils::C_TO_K(tsg);

	double& tss = meteo(MeteoData::TSS);
	tss = IOUtils::C_TO_K(tss);

	double& rh = meteo(MeteoData::RH);
	if (rh != IOUtils::nodata)
		rh /= 100.;

	double& hs = meteo(MeteoData::HS); //is in cm
	if (hs != IOUtils::nodata)
		hs /= 100.;

	double& p = meteo(MeteoData::P); //is in mbar
	if (p != IOUtils::nodata)
		p *= 100.;
}

void PSQLIO::open_connection(const bool& input)
{
	std::string connect;
	if (input) {
		connect = "hostaddr = '" + in_endpoint +
			"' port = '" + in_port +
			"' dbname = '" + in_dbname +
			"' user = '" + in_userid +
			"' password = '" + in_passwd +
			"' connect_timeout = '10'";
	} else {
		connect = "hostaddr = '" + out_endpoint +
			"' port = '" + out_port +
			"' dbname = '" + out_dbname +
			"' user = '" + out_userid +
			"' password = '" + out_passwd +
			"' connect_timeout = '10'";
	}
	psql = PQconnectdb(connect.c_str());

	if (!psql) {
		throw IOException("PSQLIO connection error: PQconnectdb returned NULL", AT);
	}
	if (PQstatus(psql) != CONNECTION_OK) {
		cerr << "ERROR" << PQstatus(psql) << endl;
		throw IOException("PSQLIO connection error: PQstatus(psql) != CONNECTION_OK", AT);
	}
}

PGresult *PSQLIO::sql_exec(const string& sql_command, const bool& input)
{
	open_connection(input);

	PGresult *result = PQexec(psql, sql_command.c_str());
	ExecStatusType status = PQresultStatus(result);
	if (status == PGRES_TUPLES_OK) { //Successful completion of a SELECT data request
		// cout << "Select executed normally... " << endl;

		// PQprintOpt        options = {0};
		// options.header    = 1;    /* Ask for column headers            */
		// options.align     = 1;    /* Pad short columns for alignment   */
		// options.fieldSep  = "|";  /* Use a pipe as the field separator */
		// PQprint(stdout, result, &options);
	} else if (status == PGRES_COMMAND_OK) {
		// other command like insert executed
		//cout << "Successful completion of a command returning no data." << endl;
	} else {
		cout << "ERROR while executing the following sql statement: " << sql_command << endl;
		//cout << "BAD SELECT: " << PQresStatus(status) << endl;
		PQclear(result);
		return NULL;
	}

	close_connection(psql);
	return result;
}

void PSQLIO::close_connection(PGconn *conn)
{
    PQfinish(conn);
}

} //namespace
