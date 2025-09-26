/*
 *  SNOWPACK stand-alone
 *
 *  Copyright WSL Institute for Snow and Avalanche Research SLF, DAVOS, SWITZERLAND
*/
/*  This file is part of Snowpack.
    Snowpack is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Snowpack is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Snowpack.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef IMISDBIO_H
#define IMISDBIO_H

#include <snowpack/plugins/SnowpackIOInterface.h>
#include <snowpack/DataClasses.h>
#include <meteoio/MeteoIO.h>
#include <occi.h>
#include <cctype>

class ImisDBIO : public SnowpackIOInterface{

	public:
		ImisDBIO(const SnowpackConfig& i_cfg, const RunInfo& run_info);
		ImisDBIO(const ImisDBIO& in);
		~ImisDBIO();

		virtual bool snowCoverExists(const std::string& i_snowfile, const std::string& stationID) const;

		virtual void readSnowCover(const std::string& i_snowfile, const std::string& stationID,
		                           SN_SNOWSOIL_DATA& SSdata, ZwischenData& Zdata);

		virtual void writeSnowCover(const mio::Date& date, const SnowStation& Xdata,
		                            const ZwischenData& Zdata, const bool& forbackup=false);

		virtual void writeTimeSeries(const SnowStation& Xdata, const SurfaceFluxes& Sdata, const CurrentMeteo& Mdata,
		                             const ProcessDat& Hdata, const double wind_trans24);

		virtual void writeProfile(const mio::Date& date, const SnowStation& Xdata);

		virtual bool writeHazardData(const std::string& stationID, const std::vector<ProcessDat>& Hdata,
		                             const std::vector<ProcessInd>& Hdata_ind, const size_t& num);

		ImisDBIO& operator=(const ImisDBIO& in);

	private:
		static void parseStationName(const std::string& stationName, std::string& stName, std::string& stNumber);
		void print_Hdata_query(const ProcessDat& Hdata, const ProcessInd& Hdata_ind) const;
		void print_Profile_query(const SnowProfileLayer& Pdata) const;

		void openDB();
		void closeDB();
		void deleteProfile(const std::string& stationName, const unsigned char& stationNumber,
		                 const mio::Date& dateStart, const mio::Date& dateEnd);
		void deleteHdata(const std::string& stationName, const std::string& stationNumber,
		                 const mio::Date& dateStart, const mio::Date& dateEnd);
		void insertProfile(const std::vector<SnowProfileLayer> &Pdata);
		void insertHdata(const std::string& stationName, const std::string& stationNumber,
		                 const std::vector<ProcessDat>& Hdata, const std::vector<ProcessInd>& Hdata_ind,
		                 const size_t& num);

		oracle::occi::Date OracleDate(mio::Date in_date) const;
		static std::string getKey(const SnowpackConfig& i_cfg, const std::string& key, const std::string& section);

		const RunInfo info;

		//Oracle OCCI variable so we don't reopen a new connection at each call
		oracle::occi::Environment *env;
		oracle::occi::Connection *conn;
		oracle::occi::Statement *stmt;

		const std::string oracleDB, oracleUser, oraclePassword;

		//double time_zone; ///< input data time zone
		static const double time_zone; //All IMIS data is in gmt+1
		static double hoar_density_surf, hoar_min_size_surf;

		static const std::string sqlDeleteHdata; //Delete statement for Hdata from snowpack.ams_pmod
		static const std::string sqlDeleteProfile; //Delete statement for profile from snowpack.ams_pmod_profile
		static const std::string sqlInsertHdata; //Insert statement for Hdata to snowpack.ams_pmod
		static const std::string sqlInsertProfile; //Insert statement for profile to snowpack.ams_pmod_profile
};

#endif
