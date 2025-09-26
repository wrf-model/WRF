/***********************************************************************************/
/*  Copyright 2014 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef PROCPSUMDISTIBUTE_H
#define PROCPSUMDISTIBUTE_H

#include <meteoio/meteoFilters/ProcessingBlock.h>
#include <vector>
#include <string>

namespace mio {

/**
 * @class  ProcPSUMDistribute
 * @ingroup processing
 * @brief Distributes precipitation on the <b>preceeding timesteps</b> in a physically plausible way
 * @details
 * This assumes that the precipitation has been measured on intervals greater than the sampling interval
 * of the data file (for example, 24 hours accumulations written once per day in an hourly file, the
 * other timesteps receiving nodata). And the accumulation has to be written on the last timestep of the accumulation period.
 *
 * It supports the following arguments:
 *  - SOFT: if set to TRUE, missing accumulated values would be replaced by "0";
 *  - MEAS_PERIOD: measured accumulation period in seconds (mandatory).
 *
 * The precipitation is distributed on the preceeding timesteps by using criterias on relative humidity
 * and the difference between the air temperature and the surface temperature.
 * @code
 * PSUM::filter1           = PSUM_DISTRIBUTE
 * PSUM::arg1::meas_period = 86400
 * @endcode
 */

class ProcPSUMDistribute : public ProcessingBlock {
	public:
		ProcPSUMDistribute(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);

		static void SmartDistributePSUM(const double& precip, const size_t& start_idx, const size_t& end_idx, const size_t& paramindex, std::vector<MeteoData>& vecM);
		static void CstDistributePSUM(const double& precip, const size_t& start_idx, const size_t& end_idx, const size_t& paramindex, std::vector<MeteoData>& vecM);
	private:
		void parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs);
		static size_t findNextAccumulation(const unsigned int& param, const std::vector<MeteoData>& ivec, const Date& endDate, size_t ii);
		static void fillInterval(const unsigned int& param, std::vector<MeteoData>& ivec, const size_t& start, const size_t& end, const double value);

		static const double thresh_rh, thresh_Dt;
		double measured_period;
		bool is_soft;
};

} //end namespace

#endif
