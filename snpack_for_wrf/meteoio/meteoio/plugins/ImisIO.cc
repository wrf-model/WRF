/***********************************************************************************/
/*  Copyright 2009, 2010 WSL Institute for Snow and Avalanche Research   SLF-DAVOS */
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
#include <meteoio/plugins/ImisIO.h>
#include <meteoio/IOUtils.h>
#include <meteoio/IOExceptions.h>
#include <meteoio/meteoLaws/Meteoconst.h>
#include <meteoio/meteoLaws/Atmosphere.h> //for p_sea to p_local conversion
#include <meteoio/MathOptim.h>

#include <sstream>
#include <set>
#include <iostream>
#include <ctime>

using namespace std;
using namespace oracle;
using namespace oracle::occi;
using namespace mio;

namespace mio {
/**
 * @page imis IMIS
 * @section imis_format Format
 * This plugin reads data directly from the IMIS network database (Oracle database).
 * It retrieves standard IMIS data as well as ENETZ and ANETZ data.
 *
 * @section imis_units Units
 * The units are assumed to be the following:
 * - temperatures in celsius
 * - relative humidity in %
 * - wind speed in m/s
 * - precipitations in mm/h
 * - radiation in W/m²
 *
 * @section imis_keywords Keywords
 * This plugin uses the following keywords:
 * - COORDSYS: input coordinate system (see Coords) specified in the [Input] section
 * - COORDPARAM: extra input coordinates parameters (see Coords) specified in the [Input] section
 * - COORDSYS: output coordinate system (see Coords) specified in the [Output] section
 * - COORDPARAM: extra output coordinates parameters (see Coords) specified in the [Output] section
 * - DBNAME: the database to connect to. There are two possibilities:
 *       - as a symbolic name as defined in the tnsnames.ora Network Configuration File provided by the Database administrator (exemple: sdbo);
 *       - as //hostname:port/service_name (exemple: //sdbo.wd.op:1521/sdbo.slf.ch).
 * - DBUSER: user name to use when connecting to the database
 * - DBPASS: password to use when connecting to the database
 * - STATION#: station code for the given number #
 * - USEANETZ: use ANETZ stations to provide precipitations for normal IMIS stations. Almost each IMIS station is associated with one or two ANETZ stations and does a weighted average to get what should be its local precipitations if no local precipitation has been found (either nodata or 0), default: false.
 * - USE_IMIS_PSUM: if set to false (default), all IMIS precipitation will be deleted (since IMIS stations don't have heated rain gauges, their precipitation measurements are not good in winter conditions). If set to true, the precipitation measurements will be accepted from IMIS stations. In this case, it is strongly advised to apply the filter FilterUnheatedPSUM to detect snow melting in the rain gauge. ANETZ stations are unaffected (their precipitation is always used), default: false.
 * - USE_SNOWPACK_PSUM: if set to true, the SNOWPACK simulated Snow Water Equivalent from the database will be used to compute PSUM. Data gaps greater than 3 hours on SWE will lead to unchanged psum while all data that can properly be computed will <b>overwrite</b> psum. (default=false)
 *
 * It is possible to use both USE_IMIS_PSUM and USE_SNOWPACK_PSUM to create composite PSUM (from SNOWPACK in the snow season and from IMIS otherwise).
 * In such a case, as soon as SNOWPACK SWE > 0, all previous PSUM data will be deleted (ie those potentially coming from IMIS_PSUM).
 * But if there is no SNOWPACK data, the IMIS measurements will be kept.
 *
 * @code
 * [Input]
 * METEO    = IMIS
 * USEANETZ = true
 * USE_IMIS_PSUM = true
 * DBNAME   = sdbo
 * DBUSER   = xxx
 * DBPASS   = xxx
 *
 * STATION1 = WFJ2
 * STATION2 = *SIO
 * STATION3 = *MVE
 * @endcode
 */

const double ImisIO::plugin_nodata = -999.; ///< plugin specific nodata value
const double ImisIO::in_tz = 1.; ///< All IMIS data is in gmt+1, that is UTC+1 (a quelques secondes près;-)

const std::string ImisIO::sqlQueryStationIDs = "SELECT station_name, drift_stat_abk, drift_stao_nr FROM station2.v_snow_drift_standort WHERE application_code='snowpack' AND station_code=:1"; ///< Wind drift station meta data

const std::string ImisIO::sqlQueryStationMetaData = "SELECT stao_name, stao_x, stao_y, stao_h FROM station2.v_station_standort WHERE stat_abk LIKE :1 AND stao_nr=:2"; ///< Snow station meta data

const std::string ImisIO::sqlQuerySensorDepths = "SELECT hts1_1, hts1_2, hts1_3 FROM station2.v_station_standort WHERE stat_abk LIKE :1 AND stao_nr=:2"; ///< Sensor depths at station

const std::string ImisIO::sqlQueryMeteoDataDrift = "SELECT TO_CHAR(a.datum, 'YYYY-MM-DD HH24:MI') AS thedate, a.ta, a.iswr, a.vw, a.dw, a.vw_max, a.rh, a.ilwr, a.hnw, a.tsg, a.tss, a.hs, a.rswr, a.ap, b.vw AS vw_drift, b.dw AS dw_drift, a.ts1, a.ts2, a.ts3 FROM (SELECT * FROM ams.v_ams_raw WHERE stat_abk=:1 AND stao_nr=:2 AND datum>=:3 AND datum<=:4) a LEFT OUTER JOIN (SELECT case when to_char(datum,'MI')=40 then trunc(datum,'HH24')+0.5/24 else datum end as datum, vw, dw FROM ams.v_ams_raw WHERE stat_abk=:5 AND stao_nr=:6 AND datum>=:3 AND datum<=:4) b ON a.datum=b.datum ORDER BY thedate"; ///< C. Marty's Data query with wind drift station; gets wind from enet stations for imis snow station too! [2010-02-24]

const std::string ImisIO::sqlQueryMeteoData = "SELECT TO_CHAR(datum, 'YYYY-MM-DD HH24:MI') AS thedate, ta, iswr, vw, dw, vw_max, rh, ilwr, hnw, tsg, tss, hs, rswr, ap, ts1, ts2, ts3 FROM ams.v_ams_raw WHERE stat_abk=:1 AND stao_nr=:2 AND datum>=:3 AND datum<=:4 ORDER BY thedate ASC"; ///< Data query without wind drift station

const std::string ImisIO::sqlQuerySWEData = "SELECT TO_CHAR(datum, 'YYYY-MM-DD HH24:MI') AS thedate, swe FROM snowpack.ams_pmod WHERE stat_abk=:1 AND stao_nr=:2 AND datum>=:3 AND datum<=:4 ORDER BY thedate ASC"; ///< Query SWE as calculated by SNOWPACK to feed into PSUM

const std::string ImisIO::coordin("CH1903"), ImisIO::coordinparam("");
std::map<std::string, AnetzData> ImisIO::mapAnetz;
std::map< std::string, std::pair<double, double> > ImisIO::mapSlopes;
const bool ImisIO::__init = ImisIO::initStaticData();

bool ImisIO::initStaticData()
{
	//Some stations are not on the flat but in the slope
	mapSlopes["FRA3"] = std::make_pair(43., 225.);
	mapSlopes["DAV5"] = std::make_pair(25., 45.);
	mapSlopes["STB2"] = std::make_pair(35., 45.);
	mapSlopes["WAN5"] = std::make_pair(20., 225.);
	mapSlopes["ROA4"] = std::make_pair(36., 115.);
	mapSlopes["CAM3"] = std::make_pair(40., 60.);
	
	//Associate string with AnetzData
	//map[station ID] = (#stations, STA1, STA2, STA3, #coeffs, coeff1, coeff2, coeff3)
	mapAnetz["ALI2"] = AnetzData(2, "*SIO", "*PUY", "", 3, 2.14, 1.72, -1.669);
	mapAnetz["AMD2"] = AnetzData(2, "*SAE", "*HOE", "", 3, 0.684, 1.871, -0.519);
	mapAnetz["ANV2"] = AnetzData(2, "*ZER", "*ABO", "", 3, 1.489, 1.225, -0.742);
	mapAnetz["ANV3"] = AnetzData(2, "*VIS", "*ZER", "", 3, 1.214, 1.294, -0.579);
	mapAnetz["ARO2"] = AnetzData(2, "*GSB", "*MVE", "", 3, 0.57, 1.518, -0.351);
	mapAnetz["ARO3"] = AnetzData(2, "*GSB", "*ZER", "", 3, 0.552, 1.802, -0.399);
	mapAnetz["BED2"] = AnetzData(2, "*GRH", "*ROE", "", 3, 0.852, 0.734, -0.233);
	mapAnetz["BED3"] = AnetzData(2, "*GRH", "*ROE", "", 3, 0.845, 0.733, -0.227);
	mapAnetz["BEL2"] = AnetzData(2, "*GRH", "*ULR", "", 3, 0.63, 1.12, -0.264);
	mapAnetz["BER2"] = AnetzData(2, "*COV", "*BUF", "", 3, 1.105, 1.47, -0.5);
	mapAnetz["BER3"] = AnetzData(2, "*COV", "*BUF", "", 3, 0.925, 1.298, -0.392);
	mapAnetz["BEV2"] = AnetzData(2, "*COV", "*WFJ", "", 3, 0.764, 1.172, -0.335);
	mapAnetz["BOG2"] = AnetzData(2, "*ROE", "*CIM", "", 3, 0.856, 0.838, -0.247);
	mapAnetz["BOR2"] = AnetzData(2, "*GRH", "*ROE", "", 3, 1.262, 0.634, -0.253);
	mapAnetz["BOV2"] = AnetzData(2, "*GSB", "*MVE", "", 3, 0.561, 1.356, -0.31);
	mapAnetz["CAM2"] = AnetzData(2, "*GRH", "*ROE", "", 3, 0.839, 0.637, -0.174);
	mapAnetz["CHA2"] = AnetzData(2, "*MVE", "*ABO", "", 3, 2.281, 1.638, -1.29);
	mapAnetz["CON2"] = AnetzData(2, "*EVO", "*MVE", "", 3, 2.229, 1.603, -1.299);
	mapAnetz["DAV2"] = AnetzData(2, "*COV", "*WFJ", "", 3, 0.976, 1.378, -0.495);
	mapAnetz["DAV3"] = AnetzData(2, "*WFJ", "*SAM", "", 3, 1.162, 1.72, -0.701);
	mapAnetz["DAV4"] = AnetzData(2, "*WFJ", "*SCU", "", 3, 1.333, 1.219, -0.605);
	mapAnetz["DAV5"] = AnetzData(2, "*WFJ", "*SAM", "", 3, 1.133, 5.609, -2.096);
	mapAnetz["DTR2"] = AnetzData(2, "*GUE", "*ROE", "", 3, 0.858, 0.631, -0.19);
	mapAnetz["ELA2"] = AnetzData(2, "*COV", "*WFJ", "", 3, 0.736, 1.213, -0.336);
	mapAnetz["ELM2"] = AnetzData(2, "*SAE", "*DIS", "", 3, 0.756, 1.311, -0.458);
	mapAnetz["ELS2"] = AnetzData(2, "*INT", "*PLF", "", 3, 1.591, 1.78, -1.081);
	mapAnetz["FAE2"] = AnetzData(2, "*ABO", "*INT", "", 3, 2.185, 1.93, -1.522);
	mapAnetz["FIR2"] = AnetzData(2, "*PIL", "*GRH", "", 3, 1.067, 0.897, -0.382);
	mapAnetz["FIS2"] = AnetzData(2, "*ABO", "*INT", "", 3, 2.04, 1.583, -1.18);
	mapAnetz["FLU2"] = AnetzData(2, "*WFJ", "*COV", "", 3, 1.811, 1.262, -0.811);
	mapAnetz["FNH2"] = AnetzData(2, "*GSB", "*SIO", "", 3, 0.481, 1.474, -0.276);
	mapAnetz["FOU2"] = AnetzData(2, "*GSB", "*SIO", "", 3, 0.587, 1.522, -0.293);
	mapAnetz["FRA2"] = AnetzData(2, "*GUE", "*ROE", "", 3, 0.876, 0.72, -0.217);
	mapAnetz["FRA3"] = AnetzData(2, "*GUE", "*ROE", "", 3, 2.876, 0.801, -0.989);
	mapAnetz["FUL2"] = AnetzData(2, "*GSB", "*AIG", "", 3, 0.848, 1.674, -0.533);
	mapAnetz["FUS2"] = AnetzData(2, "*GRH", "*ROE", "", 3, 0.788, 0.726, -0.224);
	mapAnetz["GAD2"] = AnetzData(2, "*PIL", "*GRH", "", 3, 1.105, 0.977, -0.375);
	mapAnetz["GAN2"] = AnetzData(2, "*MVE", "*INT", "", 3, 1.698, 1.534, -1.025);
	mapAnetz["GLA2"] = AnetzData(2, "*SAE", "*DIS", "", 3, 0.834, 1.664, -0.558);
	mapAnetz["GOM2"] = AnetzData(2, "*ROE", "*ULR", "", 3, 0.576, 1.143, -0.246);
	mapAnetz["GOM3"] = AnetzData(2, "*GRH", "*ULR", "", 3, 0.888, 1.037, -0.315);
	mapAnetz["GRA2"] = AnetzData(2, "*PUY", "*AIG", "", 3, 1.613, 1.238, -0.741);
	mapAnetz["GUT2"] = AnetzData(2, "*PIL", "*GRH", "", 3, 1.13, 1.129, -0.468);
	mapAnetz["HTR2"] = AnetzData(2, "*SBE", "*DIS", "", 3, 0.831, 0.914, -0.219);
	mapAnetz["HTR3"] = AnetzData(2, "*COV", "*AND", "", 3, 0.809, 0.786, -0.205);
	mapAnetz["ILI2"] = AnetzData(2, "*SIO", "*PUY", "", 3, 1.404, 1.549, -0.86);
	mapAnetz["JAU2"] = AnetzData(2, "*MVE", "*PLF", "", 3, 1.544, 2.101, -1.235);
	mapAnetz["JUL2"] = AnetzData(2, "*DAV", "*COV", "", 3, 1.354, 0.783, -0.373);
	mapAnetz["KES2"] = AnetzData(2, "*COV", "*WFJ", "", 3, 0.778, 1.244, -0.379);
	mapAnetz["KLO2"] = AnetzData(2, "*WFJ", "*SCU", "", 3, 1.332, 1.034, -0.511);
	mapAnetz["KLO3"] = AnetzData(2, "*WFJ", "*BUF", "", 3, 1.204, 1.456, -0.688);
	mapAnetz["LAG2"] = AnetzData(2, "*COV", "*ROB", "", 3, 0.832, 0.963, -0.309);
	mapAnetz["LAU2"] = AnetzData(2, "*ABO", "*PLF", "", 3, 1.524, 2.387, -1.417);
	mapAnetz["LUK2"] = AnetzData(2, "*GRH", "*ROE", "", 3, 0.823, 0.645, -0.213);
	mapAnetz["LUM2"] = AnetzData(2, "*ENG", "*SBE", "", 3, 1.56, 1.048, -0.459);
	mapAnetz["MEI2"] = AnetzData(2, "*PIL", "*GRH", "", 3, 0.999, 0.851, -0.334);
	mapAnetz["MES2"] = AnetzData(2, "*CIM", "*SBE", "", 3, 0.843, 0.91, -0.261);
	mapAnetz["MLB2"] = AnetzData(2, "*WFJ", "*SAE", "", 3, 1.505, 0.818, -0.465);
	mapAnetz["MUN2"] = AnetzData(2, "*ULR", "*ABO", "", 3, 1.158, 2.095, -0.688);
	mapAnetz["MUO2"] = AnetzData(2, "*PIL", "*ENG", "", 3, 1.178, 1.628, -0.703);
	mapAnetz["MUT2"] = AnetzData(2, "*CHU", "*GLA", "", 3, 1.706, 1.509, -0.947);
	mapAnetz["NAR2"] = AnetzData(2, "*GUE", "*ROE", "", 3, 0.799, 0.703, -0.193);
	mapAnetz["OBM2"] = AnetzData(2, "*MVE", "*WFJ", "", 3, 0.56, 0.003, -0.002);
	mapAnetz["OBW2"] = AnetzData(2, "*GRH", "*ULR", "", 3, 0.756, 1.207, -0.303);
	mapAnetz["OBW3"] = AnetzData(2, "*GRH", "*ROE", "", 3, 0.902, 0.707, -0.225);
	mapAnetz["OFE2"] = AnetzData(2, "*BUF", "*SCU", "", 3, 1.722, 2.129, -1.113);
	mapAnetz["ORT2"] = AnetzData(2, "*GLA", "*WAE", "", 3, 1.648, 1.542, -0.99);
	mapAnetz["OTT2"] = AnetzData(2, "*MVE", "*PLF", "", 3, 1.856, 1.599, -1.046);
	mapAnetz["PAR2"] = AnetzData(2, "*SCU", "*WFJ", "", 3, 1.467, 1.497, -0.752);
	mapAnetz["PUZ2"] = AnetzData(2, "*GUE", "*ROE", "", 3, 0.869, 0.724, -0.227);
	mapAnetz["ROA2"] = AnetzData(2, "*PIL", "*GRH", "", 3, 1.17, 0.845, -0.391);
	mapAnetz["ROA4"] = AnetzData(1, "*GRH", "", "", 3, 1.412, IOUtils::nodata, IOUtils::nodata);
	mapAnetz["ROT2"] = AnetzData(2, "*WFJ", "*SAM", "", 3, 1.161, 1.714, -0.813);
	mapAnetz["SAA2"] = AnetzData(2, "*ZER", "*MVE", "", 3, 1.165, 1.233, -0.514);
	mapAnetz["SAA3"] = AnetzData(2, "*EVO", "*VIS", "", 3, 1.922, 1.51, -1.051);
	mapAnetz["SAA4"] = AnetzData(2, "*ZER", "*MVE", "", 3, 1.255, 1.122, -0.429);
	mapAnetz["SCA2"] = AnetzData(2, "*PIL", "*GRH", "", 3, 1.081, 0.876, -0.321);
	mapAnetz["SCA3"] = AnetzData(2, "*ENG", "*GLA", "", 3, 1.55, 1.649, -0.776);
	mapAnetz["SCB2"] = AnetzData(2, "*PIL", "*GRH", "", 3, 1.009, 0.942, -0.383);
	mapAnetz["SCH2"] = AnetzData(2, "*ABO", "*VIS", "", 3, 1.557, 1.629, -0.852);
	mapAnetz["SHE2"] = AnetzData(2, "*NAP", "*INT", "", 3, 1.752, 1.628, -1.056);
	mapAnetz["SIM2"] = AnetzData(2, "*GUE", "*ROE", "", 3, 0.743, 0.69, -0.196);
	mapAnetz["SLF2"] = AnetzData(2, "*WFJ", "*BUF", "", 3, 1.391, 2.02, -1.061);
	mapAnetz["SMN2"] = AnetzData(2, "*BUF", "*SCU", "", 3, 1.554, 2.108, -1.129);
	mapAnetz["SPN2"] = AnetzData(2, "*ROE", "*VIS", "", 3, 0.772, 1.189, -0.327);
	mapAnetz["SPN3"] = AnetzData(2, "*ZER", "*VIS", "", 3, 1.849, 1.157, -0.677);
	mapAnetz["STH2"] = AnetzData(2, "*ABO", "*INT", "", 3, 1.821, 1.465, -1.047);
	mapAnetz["STN2"] = AnetzData(2, "*MVE", "*ZER", "", 3, 2.036, 1.641, -1.218);
	mapAnetz["TAM2"] = AnetzData(2, "*SAE", "*WFJ", "", 3, 1.101, 1.296, -0.396);
	mapAnetz["TAM3"] = AnetzData(2, "*WFJ", "*GLA", "", 3, 1.225, 1.369, -0.629);
	mapAnetz["TRU2"] = AnetzData(2, "*EVO", "*MVE", "", 3, 2.002, 1.52, -0.995);
	mapAnetz["TUJ2"] = AnetzData(2, "*GRH", "*DIS", "", 3, 0.714, 0.99, -0.234);
	mapAnetz["TUJ3"] = AnetzData(2, "*GRH", "*SBE", "", 3, 0.714, 0.662, -0.179);
	mapAnetz["TUM2"] = AnetzData(2, "*GUE", "*SBE", "", 3, 0.791, 0.711, -0.193);
	mapAnetz["URS2"] = AnetzData(2, "*GRH", "*ROE", "", 3, 0.842, 0.716, -0.205);
	mapAnetz["VAL2"] = AnetzData(2, "*GUE", "*GRH", "", 3, 1.023, 1, -0.339);
	mapAnetz["VIN2"] = AnetzData(2, "*BUF", "*SCU", "", 3, 1.744, 1.576, -0.963);
	mapAnetz["VLS2"] = AnetzData(2, "*SBE", "*DIS", "", 3, 0.81, 1.015, -0.257);
	mapAnetz["VST2"] = AnetzData(2, "*WFJ", "*VAD", "", 3, 1.254, 2.526, -1.169);
	mapAnetz["YBR2"] = AnetzData(2, "*HOE", "*ENG", "", 3, 1.513, 1.67, -0.978);
	mapAnetz["ZER2"] = AnetzData(2, "*ZER", "*MVE", "", 3, 1.239, 1.585, -0.766);
	mapAnetz["ZER4"] = AnetzData(2, "*ZER", "*MVE", "", 3, 1.15, 1.51, -0.684);
	mapAnetz["ZNZ2"] = AnetzData(2, "*COV", "*WFJ", "", 3, 1.047, 1.179, -0.457);
	mapAnetz["GOR2"] = AnetzData(2, "*EVO", "*ZER", "", 3, 1.981, 1.653, -0.974);
	mapAnetz["LHO2"] = AnetzData(2, "*GRH", "*ULR", "", 3, 0.964, 1.183, -0.31);
	mapAnetz["NAS2"] = AnetzData(2, "*BUF", "*SCU", "", 3, 1.947, 2.751, -1.748);
	mapAnetz["RNZ2"] = AnetzData(2, "*GUE", "*GRH", "", 3, 1.318, 1.191, -0.416);
	mapAnetz["SEN2"] = AnetzData(1, "*WFJ", "", "", 3, 1.822, IOUtils::nodata, IOUtils::nodata);
	mapAnetz["WFJ2"] = AnetzData(2, "*WFJ", "*CHU", "", 3, 1.852, 1.337, -0.813);

	return true;
}

void ImisIO::getDBParameters()
{
	cfg.getValue("DBNAME", "Input", oracleDBName_in);
	cfg.getValue("DBUSER", "Input", oracleUserName_in);
	cfg.getValue("DBPASS", "Input", oraclePassword_in);

	cfg.getValue("USEANETZ", "Input", useAnetz, IOUtils::nothrow);
	cfg.getValue("USE_IMIS_PSUM", "Input", use_imis_psum, IOUtils::nothrow);
	cfg.getValue("USE_SNOWPACK_PSUM", "Input", use_psum_snowpack, IOUtils::nothrow);
}

ImisIO::ImisIO(const std::string& configfile)
        : cfg(configfile), coordout(), coordoutparam(), vecStationMetaData(), mapDriftStation(),
          oracleUserName_in(), oraclePassword_in(), oracleDBName_in(), useAnetz(false), use_imis_psum(false), use_psum_snowpack(false)
{
	IOUtils::getProjectionParameters(cfg, coordout, coordoutparam);
	getDBParameters();
}

ImisIO::ImisIO(const Config& cfgreader)
        : cfg(cfgreader), coordout(), coordoutparam(), vecStationMetaData(), mapDriftStation(),
          oracleUserName_in(), oraclePassword_in(), oracleDBName_in(), useAnetz(false), use_imis_psum(false), use_psum_snowpack(false)
{
	IOUtils::getProjectionParameters(cfg, coordout, coordoutparam);
	getDBParameters();
}

void ImisIO::openDBConnection(oracle::occi::Environment*& env, oracle::occi::Connection*& conn) const
{
	env  = Environment::createEnvironment();// static OCCI function
	conn = env->createConnection(oracleUserName_in, oraclePassword_in, oracleDBName_in);
}

void ImisIO::closeDBConnection(oracle::occi::Environment*& env, oracle::occi::Connection*& conn) const
{
	try {
		if (conn != NULL)
			env->terminateConnection(conn);
		if (env != NULL)
			Environment::terminateEnvironment(env); // static OCCI function
		conn = NULL;
		env = NULL;
	} catch (const exception&){
		Environment::terminateEnvironment(env); // static OCCI function
	}
}

void ImisIO::readStationData(const Date&, std::vector<StationData>& vecStation)
{
	vecStation.clear();

	if (vecStationMetaData.empty()){//Imis station meta data cannot change between time steps
		Environment *env = NULL;
		Connection *conn = NULL;

		try {
			openDBConnection(env, conn);
			readStationMetaData(conn); //reads all the station meta data into the vecStationMetaData (member vector)
			closeDBConnection(env, conn);
		} catch (const exception& e){
			closeDBConnection(env, conn);
			throw IOException("Oracle Error when reading stations' metedata: " + string(e.what()), AT); //Translation of OCCI exception to IOException
		}
	}

	vecStation = vecStationMetaData; //vecStationMetaData is a global vector holding all meta data
}

/**
 * @brief A meta function that extracts all station names from the Config,
 *        parses them and retrieves all meta data from SDB
 */
void ImisIO::readStationMetaData(oracle::occi::Connection*& conn)
{
	const std::vector<std::string> vecStationID( readStationIDs() );

	Statement *stmt = conn->createStatement();
	for (size_t ii=0; ii<vecStationID.size(); ii++) {
		// Retrieve the station IDs - this only needs to be done once per instance
		std::string stat_abk, stao_nr, station_name;
		parseStationID(vecStationID[ii], stat_abk, stao_nr);
		
		//Retrieve the drift station - this only needs to be done once per instance
		const std::vector<std::string> stnDrift( getStationIDs(vecStationID[ii], sqlQueryStationIDs, stmt) );
		std::string drift_stat_abk, drift_stao_nr;
		IOUtils::convertString(station_name, stnDrift.at(0));
		IOUtils::convertString(drift_stat_abk, stnDrift.at(1));
		IOUtils::convertString(drift_stao_nr, stnDrift.at(2));
		const std::string drift_stationID( drift_stat_abk + drift_stao_nr );
		if (!drift_stationID.empty())
			mapDriftStation[vecStationID[ii]] = drift_stationID;
		else
			std::cerr << "[W] No drift station for station " << stat_abk << stao_nr << "\n";

		// Retrieve the station meta data - this only needs to be done once per instance
		std::vector<std::string> stationMetaData( getStationMetaData(stat_abk, stao_nr, sqlQueryStationMetaData, stmt) );
		std::string stao_name;
		double east, north, alt;
		IOUtils::convertString(stao_name, stationMetaData.at(0));
		IOUtils::convertString(east, stationMetaData.at(1), std::dec);
		IOUtils::convertString(north, stationMetaData.at(2), std::dec);
		IOUtils::convertString(alt, stationMetaData.at(3), std::dec);

		//obtain a valid station_name w/o spaces within
		if (station_name.empty()) {
			if (!stao_name.empty()) {
				station_name += vecStationID[ii] + ":" + stao_name;
			} else {
				station_name += vecStationID[ii];
			}
		} else {
			vector<string> tmpname;
			IOUtils::readLineToVec(station_name, tmpname, ' ');
			size_t jj=1;
			while (jj < tmpname.size()) {
				if (tmpname.at(jj) != "-") {
					tmpname.at(0) += "_" + tmpname.at(jj);
				} else {
					tmpname.at(0) += ":";
					if (jj < tmpname.size()-1)
						tmpname.at(0) += tmpname.at(++jj);
				}
				jj++;
			}
			station_name = tmpname.at(0);
		}
		Coords myCoord(coordin, coordinparam);
		myCoord.setXY(east, north, alt);
		StationData sd(myCoord, vecStationID[ii], station_name);
		if (mapSlopes.count(vecStationID[ii])==0) {
			sd.setSlope(0., 0.);
		} else {
			sd.setSlope(mapSlopes[ vecStationID[ii] ].first, mapSlopes[ vecStationID[ii] ].second);
		}
		
		vecStationMetaData.push_back( sd );
	}
	conn->terminateStatement(stmt);
}

/**
 * @brief This function breaks up the station name into two components (a string and a number e.g. KLO2 -> "KLO","2"). For 
 * Enet stations (such as *WFJ), the numeric part is asusmed to be 1 (ie: the automatic station).
 * @param stationID The full name of the station (e.g. "KLO2")
 * @param stName      The string part of the name  (e.g. "KLO")
 * @param stNumber    The integer part of the name (e.g. "2")
 */
void ImisIO::parseStationID(const std::string& stationID, std::string& stat_abk, std::string& stao_nr)
{
	stat_abk = stationID.substr(0, stationID.length()-1); //The station name: e.g. KLO
	stao_nr = stationID.substr(stationID.length()-1, 1); //The station number: e.g. 2
	if (!std::isdigit(stao_nr[0])) {
		//the station is one of these non-imis stations that don't contain a number...
		stat_abk = stationID;
		stao_nr = "1";
	}
}

/**
 * @brief This function extracts all info about the stations that are to be used from global Config object
 * @return a vector that will hold all relevant stations as std::strings
 */
std::vector<std::string> ImisIO::readStationIDs()
{
	std::vector<std::string> vecStationID;
	cfg.getValues("STATION", "INPUT", vecStationID);

	if (vecStationID.empty()) {
		cerr << "\tNo stations specified for IMISIO... is this what you want?\n";
	}
	return vecStationID;
}

void ImisIO::readMeteoData(const Date& dateStart, const Date& dateEnd,
                           std::vector< std::vector<MeteoData> >& vecMeteo)
{
	Environment *env = NULL;
	Connection *conn = NULL;
	Statement *stmt = NULL;

	try {
		if (vecStationMetaData.empty()) {
			openDBConnection(env, conn);
			readStationMetaData(conn); //reads all the station meta data into the vecStationMetaData (member vector)
		}

		if (vecStationMetaData.empty()) { //if there are no stations -> return
			closeDBConnection(env, conn);
			return;
		}

		vecMeteo.clear();
		vecMeteo.insert(vecMeteo.begin(), vecStationMetaData.size(), vector<MeteoData>());

		if ((env == NULL) || (conn == NULL))
			openDBConnection(env, conn);
		stmt = conn->createStatement();

		//read the "raw" station data from db (ie pure imis data)
		for (size_t ii=0; ii<vecStationMetaData.size(); ii++) { //loop through relevant stations
			readData(dateStart, dateEnd, vecMeteo, ii, vecStationMetaData, env, stmt);
		}

		if (useAnetz) { //Important: we don't care about the metadata for ANETZ stations
			std::vector<StationData> vecAnetzStation;       //holds the unique ANETZ stations that need to be read
			std::map<std::string, size_t> mapAnetzNames;   //associates an ANETZ station with an index within vecMeteoAnetz
			findAnetzStations(mapAnetzNames, vecAnetzStation);

			//read Anetz Data, convert it to PSUMs at XX:00 and XX:30
			std::vector< std::vector< std::pair<Date, double> > > vecPsum( vecAnetzStation.size() );
			std::vector< std::vector<MeteoData> > vecMeteoAnetz( vecAnetzStation.size() ); //holds the meteo data of the ANETZ stations
			const Date AnetzStart( dateStart-1./24. ); //to be sure that we can resample the ANETZ data
			const Date AnetzEnd( dateEnd+1./24. );
			for (size_t ii=0; ii<vecAnetzStation.size(); ii++) {
				readData(AnetzStart, AnetzEnd, vecMeteoAnetz, ii, vecAnetzStation, env, stmt);
				vecPsum[ii] = computeAnetzPSUM(vecMeteoAnetz[ii]);
			}

			for (size_t ii=0; ii<vecStationMetaData.size(); ii++){ //loop through relevant stations
				const map<string,AnetzData>::const_iterator it = mapAnetz.find( vecStationMetaData.at(ii).getStationID() );
				if (it != mapAnetz.end())
					assimilateAnetzData(it->second, mapAnetzNames, vecPsum, ii, vecMeteo);
			}
		}

		if (use_psum_snowpack) {
			for (size_t ii=0; ii<vecStationMetaData.size(); ii++) { //loop through relevant stations
				readSWE(dateStart, dateEnd, vecMeteo, ii, vecStationMetaData, env, stmt);
			}
		}

		conn->terminateStatement(stmt);
		closeDBConnection(env, conn);
	} catch (const exception& e){
		closeDBConnection(env, conn);
		throw IOException("Oracle Error when reading stations' data: " + std::string(e.what()), AT); //Translation of OCCI exception to IOException
	}
}

//using the XX:00 and XX:30 Anetz precipi sums (in vecPsum), compute the regression for each timestep in vecMeteo for a given station
void ImisIO::assimilateAnetzData(const AnetzData& ad,
                                 const std::map<std::string, size_t>& mapAnetzNames, const std::vector< std::vector< std::pair<Date, double> > > &vecPsum,
                                 const size_t& stationindex, std::vector< std::vector<MeteoData> >& vecMeteo)
{
	std::vector<size_t> vecAnetz( ad.nrOfAnetzStations ); //this will contain the index in vecMeteoAnetz
	for (size_t jj=0; jj<ad.nrOfAnetzStations; jj++) { //loop over all associated anetz stations
		const map<string, size_t>::const_iterator it = mapAnetzNames.find( ad.anetzstations[jj] );
		if (it==mapAnetzNames.end())
			throw NotFoundException("The ANETZ station '"+ad.anetzstations[jj]+"' could not be found", AT);
		vecAnetz[jj] = (it->second);
	}

	std::vector<size_t> last_found( vecAnetz.size(), 0 ); //index of last found timestep in vecPsum
	for (size_t ii=0; ii<vecMeteo[stationindex].size(); ii++){ //loop over all timesteps for the current station
		const Date current_date( vecMeteo[stationindex][ii].date );

		//collect the ANETZ psums for this station and timestep
		std::vector<double> psum( vecAnetz.size(), IOUtils::nodata );
		for (size_t jj=0; jj<vecAnetz.size(); jj++) { //loop over all contributing anetz stations
			//find the current timestep in vecPsum
			for (size_t kk=last_found[jj]; kk<vecPsum[vecAnetz[jj]].size(); kk++) {
				const Date &anetz_date = vecPsum[vecAnetz[jj]][kk].first;
				if (anetz_date >= current_date) { //time step was found
					if (anetz_date == current_date) {
						last_found[jj]=kk;
						psum[jj] = vecPsum[vecAnetz[jj]][kk].second;
					}
					break;
				}
			}
		}

		//compute regression for the current point using the contributing stations
		double sum = 0.;
		if (ad.nrOfAnetzStations == ad.nrOfCoefficients){ //1, 2, or 3 ANETZ stations without interaction
			for (size_t jj=0; jj<vecAnetz.size(); jj++){
				if (psum[jj]==IOUtils::nodata) {
					sum = IOUtils::nodata;
					break;
				}
				sum += ad.coeffs[jj] * psum[jj];
			}
		} else { // Exactly two ANETZ stations with one interaction term
			if (ad.nrOfCoefficients != 3)
				throw IOException("Misconfiguration in ANETZ data", AT);

			if (psum[0]==IOUtils::nodata || psum[1]==IOUtils::nodata)
				sum = IOUtils::nodata;
			else {
				sum =ad.coeffs[0] * psum[0] + ad.coeffs[1] * psum[1] + ad.coeffs[2] * psum[0] * psum[1];
			}
		}

		//replace by ANETZ psum if there is no own value measured
		double& md_psum = vecMeteo[stationindex][ii](MeteoData::PSUM);
		if ((md_psum == IOUtils::nodata) || (IOUtils::checkEpsilonEquality(md_psum, 0.0, 0.001) && sum!=IOUtils::nodata))
			md_psum = sum;
	}
}

//we compute PSUM at XX:30 and (XX+1):30 from the XX:40 sums from Anetz
std::vector< std::pair<Date, double> > ImisIO::computeAnetzPSUM(std::vector<MeteoData> &vecMeteo)
{
	const size_t nr_meteo = vecMeteo.size();
	std::vector< std::pair<Date, double> > vecPsum;
	vecPsum.reserve( nr_meteo*2 );
	for (size_t ii=0; ii<nr_meteo; ii++) {
		int hour, minute;
		vecMeteo[ii].date.getTime(hour, minute);
		if (minute==0) { //normal hourly sampling at XX:00
			const double prev_psum = vecMeteo[ii](MeteoData::PSUM);
			const Date datum_halfhour( Date::rnd(vecMeteo[ii].date-0.25/24., 1800, Date::DOWN) );
			if (prev_psum!=IOUtils::nodata) {
				vecPsum.push_back( std::make_pair(datum_halfhour, prev_psum*.5) );
				vecPsum.push_back( std::make_pair(vecMeteo[ii].date, prev_psum*.5) );
			} else {
				vecPsum.push_back( std::make_pair(datum_halfhour, IOUtils::nodata) );
				vecPsum.push_back( std::make_pair(vecMeteo[ii].date, IOUtils::nodata) );
			}
		} else { //hourly sampling at XX:40
			//generate sum at XX:30
			const double prev_psum = vecMeteo[ii](MeteoData::PSUM);
			const Date datum_halfhour( Date::rnd(vecMeteo[ii].date, 1800, Date::DOWN) );
			if (prev_psum!=IOUtils::nodata)
				vecPsum.push_back( std::make_pair(datum_halfhour, prev_psum*.5) ); //30 minute sum fully contained in the XX:40 sum
			else
				vecPsum.push_back( std::make_pair(datum_halfhour, IOUtils::nodata) );

			//generate sum at (XX+1):00
			const double next_psum = (ii<(nr_meteo-1))? vecMeteo[ii+1](MeteoData::PSUM) : IOUtils::nodata;
			const Date datum( Date::rnd(vecMeteo[ii].date, 3600, Date::UP) );
			if (prev_psum!=IOUtils::nodata && next_psum!=IOUtils::nodata)
				vecPsum.push_back( std::make_pair(datum, (prev_psum+next_psum*2.)/6.) );
			else
				vecPsum.push_back( std::make_pair(datum, IOUtils::nodata) );
		}
	}
	
	return vecPsum;
}

void ImisIO::findAnetzStations(std::map<std::string, size_t>& mapAnetzNames,
                               std::vector<StationData>& vecAnetzStation) const
{
	std::set<std::string> uniqueStations;

	for (size_t ii=0; ii<vecStationMetaData.size(); ii++){ //loop through stations
		const map<string, AnetzData>::const_iterator it = mapAnetz.find( vecStationMetaData.at(ii).getStationID() );
		if (it != mapAnetz.end()){
			for (size_t jj=0; jj<it->second.nrOfAnetzStations; jj++){
				uniqueStations.insert( it->second.anetzstations[jj] );
			}
		}
	}

	size_t pp = 0;
	for (set<string>::const_iterator ii=uniqueStations.begin(); ii!=uniqueStations.end(); ii++){
		mapAnetzNames[*ii] = pp;
		pp++;

		StationData sd;
		sd.stationID = *ii;
		vecAnetzStation.push_back( sd );
	}
}

/**
 * @brief A meta function to read meteo data for one specific station (specified by the stationindex)
 * @param dateStart     The beginning of the interval to retrieve data for
 * @param dateEnd       The end of the interval to retrieve data for
 * @param vecMeteo      The vector that will hold all MeteoData for each station
 * @param stationindex  The index of the station as specified in the Config
 * @param vecStationIDs Vector of station IDs
 * @param env           Create Oracle environnment
 * @param conn          Create connection to SDB
 */
void ImisIO::readData(const Date& dateStart, const Date& dateEnd, std::vector< std::vector<MeteoData> >& vecMeteo,
                      const size_t& stationindex, const std::vector<StationData>& vecStationIDs,
                      oracle::occi::Environment*& env, oracle::occi::Statement*& stmt)
{
	vecMeteo.at(stationindex).clear();

	std::string stat_abk, stao_nr;
	std::vector< std::vector<std::string> > vecResult;

	// Moving back to the IMIS timezone (UTC+1)
	Date dateS(dateStart), dateE(dateEnd);
	dateS.setTimeZone(in_tz);
	dateE.setTimeZone(in_tz);

	//get data for one specific station
	parseStationID(vecStationIDs[stationindex].getStationID(), stat_abk, stao_nr);
	const std::vector<std::string> vecHTS1( getSensorDepths(stat_abk, stao_nr, sqlQuerySensorDepths, stmt) );
	const bool fullStation = getStationData(stat_abk, stao_nr, dateS, dateE, vecHTS1, vecResult, env, stmt);

	MeteoData tmpmd;
	tmpmd.meta = vecStationIDs.at(stationindex);
	const bool reduce_pressure = (tmpmd.meta.stationID!="STB2")? true : false; //unfortunatelly, there is no metadata to know this...
	vecMeteo[stationindex].resize( vecResult.size() );
	for (size_t ii=0; ii<vecResult.size(); ii++) {
		parseDataSet(vecResult[ii], tmpmd, fullStation);
		convertUnits(tmpmd, reduce_pressure);

		//For IMIS stations the psum value is a rate (kg m-2 h-1), therefore we need to
		//divide it by two to conjure the accumulated value for the half hour
		if (tmpmd.meta.stationID.length() > 0) {
			if (tmpmd.meta.stationID[0] != '*') { //only consider IMIS stations (ie: not ANETZ which simply go through)
				if (use_imis_psum==false) {
					tmpmd(MeteoData::PSUM) = IOUtils::nodata;
				} else {
					double& psum = tmpmd(MeteoData::PSUM);
					if (psum!=IOUtils::nodata) {
						psum *= .5; //half hour accumulated value for IMIS stations only
					}
				}
			}
		}

		vecMeteo[stationindex][ii] = tmpmd; //Now insert tmpmd
	}
}

/**
 * @brief Read simulated SWE from the database, compute Delta(SWE) and use it as PSUM for one specific station (specified by the stationindex)
 * @param dateStart     The beginning of the interval to retrieve data for
 * @param dateEnd       The end of the interval to retrieve data for
 * @param vecMeteo      The vector that will hold all MeteoData for each station
 * @param stationindex  The index of the station as specified in the Config
 * @param vecStationIDs Vector of station IDs
 * @param env           Create Oracle environnment
 * @param conn          Create connection to SDB
 */
void ImisIO::readSWE(const Date& dateStart, const Date& dateEnd, std::vector< std::vector<MeteoData> >& vecMeteo,
                      const size_t& stationindex, const std::vector<StationData>& vecStationIDs,
                      oracle::occi::Environment*& env, oracle::occi::Statement*& stmt)
{
	const double max_interval = 3./24.; //max hours between two SWE values
	const double swe_threshold = 1.5; //precip less than this are delayed until its sum gets greater
	const double eps_swe = 0.1; //very small variations on SWE are simply ignored

	// Moving back to the IMIS timezone (UTC+1)
	Date dateS(dateStart), dateE(dateEnd);
	dateS.setTimeZone(in_tz);
	dateE.setTimeZone(in_tz);

	//build stat_abk and stao_nr from station name
	std::string stat_abk, stao_nr;
	parseStationID(vecStationIDs.at(stationindex).getStationID(), stat_abk, stao_nr);

	const unsigned int max_row = static_cast<unsigned int>( Optim::ceil( (dateE.getJulian()-dateS.getJulian())*24.*2. ) ); //for prefetching

	//query
	try {
		stmt->setSQL(sqlQuerySWEData);
		stmt->setPrefetchRowCount(max_row);

		// construct the oracle specific Date object: year, month, day, hour, minutes
		int year, month, day, hour, minutes, seconds;
		dateS.getDate(year, month, day, hour, minutes, seconds);
		const occi::Date begindate(env, year, month, day, hour, minutes, seconds);
		dateE.getDate(year, month, day, hour, minutes, seconds);
		const occi::Date enddate(env, year, month, day, hour, minutes, seconds);
		stmt->setString(1, stat_abk); // set 1st variable's value (station name)
		stmt->setString(2, stao_nr);  // set 2nd variable's value (station number)
		stmt->setDate(3, begindate);  // set 3rd variable's value (begin date)
		stmt->setDate(4, enddate);    // set 4th variable's value (end date)

		ResultSet *rs = stmt->executeQuery(); // execute the statement stmt
		const vector<MetaData> cols( rs->getColumnListMetaData() );

		double prev_swe = IOUtils::nodata;
		Date prev_date;
		size_t ii_serie = 0; //index in meteo time serie
		const size_t serie_len = vecMeteo[stationindex].size();
		double accumulator = 0.;

		while (rs->next() == true) { //loop over timesteps
			if (cols.size()!=2) {
				ostringstream ss;
				ss << "For station " << vecStationIDs.at(stationindex).getStationID() << ", ";
				ss << "snowpack SWE query returned " << cols.size() << " columns, while 2 were expected";
				throw UnknownValueException(ss.str(), AT);
			}
			Date curr_date;
			IOUtils::convertString(curr_date, rs->getString(1), 1.);
			double curr_swe;
			IOUtils::convertString(curr_swe, rs->getString(2));
			if (curr_swe==IOUtils::nodata || curr_swe<0.) continue;

			//looking for matching timestamp in the vecMeteo
			while (ii_serie<serie_len && vecMeteo[stationindex][ii_serie].date<curr_date) ii_serie++;
			if (ii_serie>=serie_len) return;


			if (prev_swe==IOUtils::nodata) {
				 //this looks like the first valid data point that we find
				prev_swe = curr_swe;
				prev_date = curr_date;
				continue;
			}

			const double measurement_interval = curr_date.getJulian() - prev_date.getJulian();
			if (measurement_interval<=max_interval && curr_swe>0.) { //keep previous PSUM if no snow on the ground
				vecMeteo[stationindex][ii_serie](MeteoData::PSUM) = 0.;
				//data not too far apart, so we accept it for Delta SWE
				if (vecMeteo[stationindex][ii_serie].date==curr_date) {
					//we found the matching timestamp -> writing Delta(SWE) as psum
					const double new_psum_sum = curr_swe - prev_swe;
					if (new_psum_sum>eps_swe) {
						accumulator += new_psum_sum;
						if (accumulator>=swe_threshold) {
							vecMeteo[stationindex][ii_serie](MeteoData::PSUM) = accumulator;
							accumulator = 0.;
						}
					}
				}
				prev_swe = curr_swe;
				prev_date = curr_date;
			} else {
				//data points in SWE too far apart, we could not use it for psum but we reset our prev_swe to this new point
				prev_swe = curr_swe;
				prev_date = curr_date;
			}
		}

		stmt->closeResultSet(rs);
	} catch (const exception& e){
		throw IOException("Oracle Error when SWE data: " + string(e.what()), AT); //Translation of OCCI exception to IOException
	}
}


/**
 * @brief Puts the data that has been retrieved from the database into a MeteoData object
 * @param i_meteo a row of meteo data from the database (NOTE order important, matches SQL query, see also MeteoData.[cch])
 * @param md     the object to copy the data to
 * @param fullstation
 * 	- true if it is a combined snow_drift station (station2.v_snow_drift_standort)
 * 	- false if it is a "conventional" station, for example an ANETZ-station (station2.standort)
 */
void ImisIO::parseDataSet(const std::vector<std::string>& i_meteo, MeteoData& md, const bool& fullStation)
{
	if (i_meteo.size()<14) throw InvalidFormatException("Wrong result vector received from the database", AT);
	
	IOUtils::convertString(md.date,               i_meteo[0], in_tz, dec);
	IOUtils::convertString(md(MeteoData::TA),     i_meteo[1],  std::dec);
	IOUtils::convertString(md(MeteoData::ISWR),   i_meteo[2],  std::dec);
	IOUtils::convertString(md(MeteoData::VW),     i_meteo[3],  std::dec);
	IOUtils::convertString(md(MeteoData::DW),     i_meteo[4],  std::dec);
	IOUtils::convertString(md(MeteoData::VW_MAX), i_meteo[5],  std::dec);
	IOUtils::convertString(md(MeteoData::RH),     i_meteo[6],  std::dec);
	IOUtils::convertString(md(MeteoData::ILWR),   i_meteo[7],  std::dec);
	IOUtils::convertString(md(MeteoData::PSUM),   i_meteo[8],  std::dec);
	IOUtils::convertString(md(MeteoData::TSG),    i_meteo[9],  std::dec);
	IOUtils::convertString(md(MeteoData::TSS),    i_meteo[10], std::dec);
	IOUtils::convertString(md(MeteoData::HS),     i_meteo[11], std::dec);
	IOUtils::convertString(md(MeteoData::RSWR),   i_meteo[12], std::dec);
	IOUtils::convertString(md(MeteoData::P),      i_meteo[13], std::dec);

	unsigned int ii = 14;
	if (fullStation) {
		if (!md.param_exists("VW_DRIFT")) md.addParameter("VW_DRIFT");
		IOUtils::convertString(md("VW_DRIFT"), i_meteo.at(ii++), std::dec);
		if (!md.param_exists("DW_DRIFT")) md.addParameter("DW_DRIFT");
		IOUtils::convertString(md("DW_DRIFT"), i_meteo.at(ii++), std::dec);
	}

	// additional snow station parameters
	if (!md.param_exists("TS1")) md.addParameter("TS1");
	IOUtils::convertString(md("TS1"), i_meteo.at(ii++), std::dec);
	if (!md.param_exists("TS2")) md.addParameter("TS2");
	IOUtils::convertString(md("TS2"), i_meteo.at(ii++), std::dec);
	if (!md.param_exists("TS3")) md.addParameter("TS3");
	IOUtils::convertString(md("TS3"), i_meteo.at(ii++), std::dec);
	if (fullStation) {
		if (!md.param_exists("HTS1")) md.addParameter("HTS1");
		IOUtils::convertString(md("HTS1"), i_meteo.at(ii++), std::dec);
		if (!md.param_exists("HTS2")) md.addParameter("HTS2");
		IOUtils::convertString(md("HTS2"), i_meteo.at(ii++), std::dec);
		if (!md.param_exists("HTS3")) md.addParameter("HTS3");
		IOUtils::convertString(md("HTS3"), i_meteo.at(ii++), std::dec);
	}
}

/**
 * @brief This function gets IDs from table station2.v_snow_drift_standort and fills vecStationIDs
 * @param station_code  a string key corresponding to stationID
 * @param conn          create connection to SDB
 * @param return the IDS in a vector<string>
 */
std::vector<std::string> ImisIO::getStationIDs(const std::string& station_code, const std::string& sqlQuery,
                                   oracle::occi::Statement*& stmt) const
{
	std::vector<std::string> vecStationIDs;

	try {
		stmt->setSQL( sqlQuery );
		stmt->setString(1, station_code); // set 1st variable's value

		ResultSet *rs = stmt->executeQuery();    // execute the statement stmt
		const std::vector<MetaData> cols( rs->getColumnListMetaData() );

		while (rs->next() == true) {
			for (unsigned int ii=1; ii<=static_cast<unsigned int>(cols.size()); ii++) {
				vecStationIDs.push_back( rs->getString(ii) );
			}
		}

		if (vecStationIDs.size() < 3) { //if the station has not been found
			std::string stat_abk, stao_nr;
			parseStationID(station_code, stat_abk, stao_nr);
			vecStationIDs.push_back(station_code);
			vecStationIDs.push_back(stat_abk);
			vecStationIDs.push_back(stao_nr);
		}

		stmt->closeResultSet(rs);
		return vecStationIDs;
	} catch (const exception& e){
		throw IOException("Oracle Error when reading stations' id: " + string(e.what()), AT); //Translation of OCCI exception to IOException
	}
}

/**
 * @brief This function gets IDs from table station2.v_snow_drift_standort and fills vecStationIDs
 * @param stat_abk a string key of table station2
 * @param stao_nr  a string key of table station2
 * @param conn     create connection to SDB
 * @param return vector of string with sensor depths
 */
std::vector<std::string> ImisIO::getSensorDepths(const std::string& stat_abk, const std::string& stao_nr,
                                     const std::string& sqlQuery,
                                     oracle::occi::Statement*& stmt) const
{
	std::vector<std::string> vecHTS1;

	try {
		stmt->setSQL(sqlQuery);
		stmt->setString(1, stat_abk); // set 1st variable's value
		stmt->setString(2, stao_nr);  // set 2nd variable's value

		ResultSet *rs = stmt->executeQuery();    // execute the statement stmt
		const std::vector<MetaData> cols( rs->getColumnListMetaData() );

		while (rs->next() == true) {
			for (unsigned int ii=1; ii<=static_cast<unsigned int>(cols.size()); ii++) {
				vecHTS1.push_back( rs->getString(ii) );
			}
		}

		stmt->closeResultSet(rs);
		return vecHTS1;
	} catch (const exception& e){
		throw IOException("Oracle Error when reading sensors' depths: " + string(e.what()), AT); //Translation of OCCI exception to IOException
	}
}

/**
 * @brief This function gets meta data from table station2.standort and fills vecStationMetaData.
 * This is also the moment to take the opportunity to check if the station really does exist.
 * @param stat_abk           a string key of table
 * @param stao_nr            a string key of table
 * @param sqlQuery           the query to execute
 * @param conn               create connection to SDB
 * @param return             vector<string> containing the metadata
 */
std::vector<std::string> ImisIO::getStationMetaData(const std::string& stat_abk, const std::string& stao_nr,
                                        const std::string& sqlQuery,
                                        oracle::occi::Statement*& stmt) const
{
	std::vector<std::string> vecMetaData;

	try {
		stmt->setSQL(sqlQuery);
		stmt->setString(1, stat_abk); // set 1st variable's value
		stmt->setString(2, stao_nr);  // set 2nd variable's value

		ResultSet *rs = stmt->executeQuery();    // execute the statement stmt
		const std::vector<MetaData> cols( rs->getColumnListMetaData() );

		while (rs->next() == true) {
			for (unsigned int ii=1; ii<=static_cast<unsigned int>(cols.size()); ii++) {
				vecMetaData.push_back( rs->getString(ii) );
			}
		}

		stmt->closeResultSet(rs);
	} catch (const exception& e){
		throw IOException("Oracle Error when reading stations' metadata: " + string(e.what()), AT); //Translation of OCCI exception to IOException
	}

	const size_t nr_metadata = vecMetaData.size();
	if (nr_metadata==0)
			throw NoDataException("Station " + stat_abk+stao_nr + " not found in the database", AT);
	if (nr_metadata<4)
			throw ConversionFailedException("Error while converting station meta data for station "+stat_abk+stao_nr, AT);
	return vecMetaData;
}

/**
 * @brief Gets data from ams.v_ams_raw which is a table of SDB and
 * retrieves the temperature sensor depths from station2.standort.
 * Each record returned are vector of strings which are pushed back in vecMeteoData.
 * @param stat_abk :     a string key of ams.v_ams_raw
 * @param stao_nr :      a string key of ams.v_ams_raw
 * @param dateS :        beginning of the recording date
 * @param dateE :        end of the recording date
 * @param vecMeteoData : a vector of vector of string in which data will be filled
 * @param return true if the station is a full station, false otherwise (ex: wind station)
 */
bool ImisIO::getStationData(const std::string& stat_abk, const std::string& stao_nr,
                            const Date& dateS, const Date& dateE,
                            const std::vector<std::string>& vecHTS1,
                            std::vector< std::vector<std::string> >& vecMeteoData,
                            oracle::occi::Environment*& env, oracle::occi::Statement*& stmt)
{
	vecMeteoData.clear();
	bool fullStation = true;
	const unsigned int max_row = static_cast<unsigned int>( Optim::ceil( (dateE.getJulian()-dateS.getJulian())*24.*2. ) ); //for prefetching
	try {
		const std::map<std::string, std::string>::const_iterator it = mapDriftStation.find(stat_abk+stao_nr);
		if (it != mapDriftStation.end()) {
			stmt->setSQL( sqlQueryMeteoDataDrift );
			std::string drift_stat_abk, drift_stao_nr;
			parseStationID(it->second, drift_stat_abk, drift_stao_nr);
			stmt->setString(5, drift_stat_abk);
			stmt->setString(6, drift_stao_nr);
		} else {
			stmt->setSQL( sqlQueryMeteoData );
			fullStation = false;
		}
		stmt->setPrefetchRowCount(max_row);

		// construct the oracle specific Date object: year, month, day, hour, minutes
		int year, month, day, hour, minutes, seconds;
		dateS.getDate(year, month, day, hour, minutes, seconds);
		const occi::Date begindate(env, year, month, day, hour, minutes, seconds);
		dateE.getDate(year, month, day, hour, minutes, seconds);
		const occi::Date enddate(env, year, month, day, hour, minutes, seconds);
		stmt->setString(1, stat_abk); // set 1st variable's value (station name)
		stmt->setString(2, stao_nr);  // set 2nd variable's value (station number)
		stmt->setDate(3, begindate);  // set 3rd variable's value (begin date)
		stmt->setDate(4, enddate);    // set 4th variable's value (end date)

		ResultSet *rs = stmt->executeQuery(); // execute the statement stmt
		const std::vector<MetaData> cols( rs->getColumnListMetaData() );

		std::vector<std::string> vecData;
		while (rs->next() == true) {
			vecData.clear();
			for (unsigned int ii=1; ii<=static_cast<unsigned int>(cols.size()); ii++) {
				vecData.push_back( rs->getString(ii) );
			}
			if (fullStation) {
				for (unsigned int ii=0; ii<static_cast<unsigned int>(vecHTS1.size()); ii++) {
					vecData.push_back( vecHTS1.at(ii) );
				}
			}
			vecMeteoData.push_back(vecData);
		}

		stmt->closeResultSet(rs);
		return fullStation;
	} catch (const exception& e){
		throw IOException("Oracle Error when reading stations' data: " + string(e.what()), AT); //Translation of OCCI exception to IOException
	}
}

void ImisIO::convertSnowTemperature(MeteoData& meteo, const std::string& parameter)
{
	if (meteo.param_exists(parameter)) {
		const size_t idx = meteo.getParameterIndex(parameter);
		if (meteo(idx)!=IOUtils::nodata)
			meteo(idx) += Cst::t_water_freezing_pt; //C_TO_K
	}
}

void ImisIO::convertSensorDepth(MeteoData& meteo, const std::string& parameter)
{
	if (meteo.param_exists(parameter)) {
		const size_t idx = meteo.getParameterIndex(parameter);
		if (meteo(idx)!=IOUtils::nodata)
			meteo(idx) /= 100.; // centimetre to metre
	}
}

void ImisIO::convertUnits(MeteoData& meteo, const bool& reduce_pressure)
{
	meteo.standardizeNodata(plugin_nodata);

	//converts C to Kelvin, converts RH to [0,1], HS to m and P to Pa
	double& ta = meteo(MeteoData::TA);
	ta = IOUtils::C_TO_K(ta);

	double& tsg = meteo(MeteoData::TSG);
	tsg = IOUtils::C_TO_K(tsg);

	double& tss = meteo(MeteoData::TSS);
	tss = IOUtils::C_TO_K(tss);

	double& rh = meteo(MeteoData::RH);
	if (rh != IOUtils::nodata)
		rh /= 100.;

	double& hs = meteo(MeteoData::HS);
	if (hs != IOUtils::nodata)
		hs /= 100.0;
	
	double& p = meteo(MeteoData::P);
	if (p != IOUtils::nodata) {
		if (reduce_pressure) 
			p *= 100. * Atmosphere::stdAirPressure(meteo.meta.position.getAltitude()) / Cst::std_press;
		else
			p *= 100.; //simply convert the units
	}

	//convert extra parameters (if present) //HACK TODO: find a dynamic way...
	convertSnowTemperature(meteo, "TS1");
	convertSnowTemperature(meteo, "TS2");
	convertSnowTemperature(meteo, "TS3");
	convertSensorDepth(meteo, "HTS1");
	convertSensorDepth(meteo, "HTS2");
	convertSensorDepth(meteo, "HTS3");
}

} //namespace
