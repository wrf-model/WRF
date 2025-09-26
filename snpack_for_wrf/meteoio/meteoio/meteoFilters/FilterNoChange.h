/***********************************************************************************/
/*  Copyright 2015 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef FILTERNOCHANGE_H
#define FILTERNOCHANGE_H

#include <meteoio/meteoFilters/WindowedFilter.h>

#include <vector>
#include <string>

namespace mio {

/**
 * @class FilterNoChange
 * @ingroup processing
 * @brief This filter removes periods showing insufficient changes. 
 * @details
 * It searches for time periods in which the value of the certain variable doesn't change by looking at the variance. 
 * It takes as arguments all the window parameters as defined in WindowedFilter::setWindowFParams().
 * Additionally, you can set the maximum variance that is allowed with MAX_VARIANCE. If not set, this value is 0.
 * 
 * For example:
 * @code
 * HS::filter1         = NO_CHANGE
 * HS::arg1::soft      = true
 * HS::arg1::centering = left
 * HS::arg1::min_pts   = 1
 * HS::arg1::min_span  = 1800 ;ie left centered window spanning 1800 seconds and at least 1 points
 * 
 * TA::filter1            = NO_CHANGE
 * TA::arg1::soft         = false
 * TA::arg1::min_pts      = 10
 * TA::arg1::min_span     = 600 ;ie strictly centered window spanning 600 seconds and at least 10 points
 * TA::arg1::max_variance = 0.001 ;filter everything with a variance of this or lower in data window
 * @endcode
 * 
 * @author Anna-Maria Tilg
 * @date   2015-12-04
 */

class FilterNoChange : public WindowedFilter {
	public:
		FilterNoChange(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);
	private:
		void parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs);
		double max_variance;
};

} //end namespace

#endif
