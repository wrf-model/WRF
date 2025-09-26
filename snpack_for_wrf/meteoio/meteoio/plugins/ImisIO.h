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
#ifndef IMISIO_H
#define IMISIO_H

#include <meteoio/IOInterface.h>

#include <string>
#include <map>
#include <vector>
#include <occi.h>

namespace mio {

class AnetzData{
	public:
		AnetzData()
		: anetzstations(3, ""), coeffs(3, IOUtils::nodata), nrOfAnetzStations(3), nrOfCoefficients(3)  {}

		AnetzData(const size_t& nr_anetz,
		          const std::string& i_anetz1, const std::string& i_anetz2, const std::string& i_anetz3,
		          const size_t& nr_coeffs,
		          const double& coeff1, const double& coeff2, const double& coeff3)
		          : anetzstations(3), coeffs(3), nrOfAnetzStations(nr_anetz), nrOfCoefficients(nr_coeffs)
		{
			anetzstations[0] = i_anetz1;
			anetzstations[1] = i_anetz2;
			anetzstations[2] = i_anetz3;
			coeffs[0] = coeff1;
			coeffs[1] = coeff2;
			coeffs[2] = coeff3;
		}

		std::vector<std::string> anetzstations;
		std::vector<double> coeffs;
		size_t nrOfAnetzStations, nrOfCoefficients;
};

/**
 * @class ImisIO
 * @brief The class with-in the data from the database are treated. The MeteoData and the StationData will be set in.
 * This class also herited to IOInterface class which is abstract.
 *
 * @ingroup plugins
 * @author Thomas Egger, first version by Moustapha Mbengue
 * @date 2010-11-02
 */
class ImisIO : public IOInterface {
	public:
		ImisIO(const std::string& configfile);
		ImisIO(const ImisIO&);
		ImisIO(const Config&);

		virtual void readStationData(const Date& date, std::vector<StationData>& vecStation);
		virtual void readMeteoData(const Date& dateStart, const Date& dateEnd,
		                           std::vector< std::vector<MeteoData> >& vecMeteo);

	private:
		void openDBConnection(oracle::occi::Environment*& env, oracle::occi::Connection*& conn) const;
		void closeDBConnection(oracle::occi::Environment*& env, oracle::occi::Connection*& conn) const;
		void getDBParameters();

		std::vector<std::string> getStationIDs(const std::string& stat_code,
		                     const std::string& sqlQuery, oracle::occi::Statement*& stmt) const;
		std::vector<std::string> getStationMetaData(const std::string& stat_abk, const std::string& stao_nr,
		                          const std::string& sqlQuery, oracle::occi::Statement*& stmt) const;
		std::vector<std::string> getSensorDepths(const std::string& stat_abk, const std::string& stao_nr,
		                       const std::string& sqlQuery, oracle::occi::Statement*& stmt) const;
		bool getStationData(const std::string& stat_abk, const std::string& stao_nr,
		                    const Date& dateS, const Date& dateE,
		                    const std::vector<std::string>& i_vecHTS1,
		                    std::vector< std::vector<std::string> >& vecMeteoData,
		                    oracle::occi::Environment*& env, oracle::occi::Statement*& stmt);

		static void parseDataSet(const std::vector<std::string>& meteo_in, MeteoData& md, const bool& fullStation);
		void readData(const Date& dateStart, const Date& dateEnd, std::vector< std::vector<MeteoData> >& vecMeteo,
		              const size_t& stationindex, const std::vector<StationData>& vecStationID,
		              oracle::occi::Environment*& env, oracle::occi::Statement*& stmt);
		void readSWE(const Date& dateStart, const Date& dateEnd, std::vector< std::vector<MeteoData> >& vecMeteo,
		             const size_t& stationindex, const std::vector<StationData>& vecStationIDs,
		             oracle::occi::Environment*& env, oracle::occi::Statement*& stmt);
		std::vector<std::string> readStationIDs();
		static void parseStationID(const std::string& stationID, std::string& stnAbbrev, std::string& stnNumber);

		void readStationMetaData(oracle::occi::Connection*& conn);
		void convertSnowTemperature(MeteoData& meteo, const std::string& parameter);
		void convertSensorDepth(MeteoData& meteo, const std::string& parameter);
		void convertUnits(MeteoData& meteo, const bool& reduce_pressure);

		//helper functions for the Anetz coefficient mangling:
		void findAnetzStations(std::map<std::string, size_t>& mapAnetzNames, std::vector<StationData>& vecAnetzStation) const;
		void assimilateAnetzData(const AnetzData& ad,
                                 const std::map<std::string, size_t>& mapAnetzNames, const std::vector< std::vector< std::pair<Date, double> > > &vecPsum,
                                 const size_t& stationindex, std::vector< std::vector<MeteoData> >& vecMeteo);
		static std::vector< std::pair<Date, double> > computeAnetzPSUM(std::vector<MeteoData> &vecMeteo);

		static const double in_tz; //timezone
		const Config cfg;
		std::string coordout, coordoutparam; //projection parameters
		std::vector<StationData> vecStationMetaData;
		std::map<std::string, std::string> mapDriftStation;
		static const double plugin_nodata; //plugin specific nodata value, e.g. -999
		static const std::string sqlQueryStationIDs;
		static const std::string sqlQueryStationMetaData;
		static const std::string sqlQuerySensorDepths;
		static const std::string sqlQueryMeteoDataDrift; // combined snow_drift query from two stations (ams.v_ams_raw)
		static const std::string sqlQueryMeteoData; // single station query (ams.v_ams_raw)
		static const std::string sqlQuerySWEData; //SWE as computed by SNOWPACK
		static const std::string coordin, coordinparam; //these are fixed for IMIS
		std::string oracleUserName_in;
		std::string oraclePassword_in;
		std::string oracleDBName_in;
		bool useAnetz, use_imis_psum, use_psum_snowpack;

		static std::map<std::string, AnetzData> mapAnetz;
		static std::map< std::string, std::pair<double, double> > mapSlopes;
		static const bool __init;    ///<helper variable to enable the init of static collection data
		static bool initStaticData();///<initialize the static map meteoparamname
};

} //end namespace mio

#endif

