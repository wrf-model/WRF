/***********************************************************************************/
/*  Copyright 2011 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef FILTERSTDDEV_H
#define FILTERSTDDEV_H

#include <meteoio/meteoFilters/WindowedFilter.h>
#include <vector>
#include <string>

namespace mio {

/**
 * @class FilterStdDev
 * @ingroup processing
 * @brief Standard deviation filter.
 * @details
 * Values outside of mean ± 2 std_dev are rejected.
 * It takes as arguments all the window parameters as defined in WindowedFilter::setWindowFParams().
 *
 * @code
 * TA::filter1         = std_dev
 * TA::arg1::soft      = TRUE
 * TA::arg1::centering = left
 * TA::arg1::MIN_PTS   = 1
 * TA::arg1::MIN_SPAN  = 1800 ;ie 1800 seconds time span for the left leaning window
 *
 * RH::filter1        = std_dev
 * RH::arg1::MIN_PTS  = 10
 * RH::arg1::MIN_SPAN = 600 ;strictly centered window spanning 600 seconds and at least 10 points
 * @endcode
 */

class FilterStdDev : public WindowedFilter {
	public:
		FilterStdDev(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);

	private:
		static void getStat(const std::vector<MeteoData>& ivec, const unsigned int& param,
		             const size_t& start, const size_t& end, double& stddev, double& mean);
		static const double sigma; ///<How many times the stddev allowed for valid points
};

} //end namespace

#endif
