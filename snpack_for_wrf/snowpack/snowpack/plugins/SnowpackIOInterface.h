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

#ifndef SNOWPACKIOINTERFACE_H
#define SNOWPACKIOINTERFACE_H

#include <snowpack/DataClasses.h>
#include <snowpack/Hazard.h>

class SnowpackIOInterface {

	public:
		virtual ~SnowpackIOInterface(){}

		virtual bool snowCoverExists(const std::string& i_snowfile, const std::string& stationID) const = 0;

		virtual void readSnowCover(const std::string& i_snowfile, const std::string& stationID,
		                           SN_SNOWSOIL_DATA& SSdata, ZwischenData& Zdata, const bool& read_salinity) = 0;

		virtual void writeSnowCover(const mio::Date& date, const SnowStation& Xdata,
                                    const ZwischenData& Zdata, const bool& forbackup=false) = 0;

		virtual void writeTimeSeries(const SnowStation& Xdata, const SurfaceFluxes& Sdata, const CurrentMeteo& Mdata,
		                             const ProcessDat& Hdata, const double wind_trans24) = 0;

		virtual void writeProfile(const mio::Date& date, const SnowStation& Xdata) = 0;

		virtual bool writeHazardData(const std::string& stationID, const std::vector<ProcessDat>& Hdata,
                                     const std::vector<ProcessInd>& Hdata_ind, const size_t& num) = 0;
};

#endif
