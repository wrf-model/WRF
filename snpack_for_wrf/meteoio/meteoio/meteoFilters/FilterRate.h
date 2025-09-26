/***********************************************************************************/
/*  Copyright 2009 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef FILTERRATE_H
#define FILTERRATE_H

#include <meteoio/meteoFilters/ProcessingBlock.h>
#include <vector>
#include <string>

namespace mio {

/**
 * @class FilterRate
 * @ingroup processing
 * @brief Rate of change filter.
 * @details
 * Calculate the change rate (ie: slope) between two points, if it is above a user given value, reject the point. It takes the following arguments:
 *  - MIN: minimum permissible rate of change (per seconds, optional);
 *  - MAX: either the absolute value of the maximum permissible rate of change (per seconds) if no other argument is provided, or maximum
 * permissible rate of change (per seconds) if a MIN was provided.
 *
 * So depending if MIN and MAX were provided or only MAX, every point where the local rate of change is outside <em>[ MIN , MAX]</em> or
 * every point outside  <em>[ -MAX , MAX]</em> is rejected.
 *
 * @code
 * TA::filter1   = rate
 * TA::arg1::MIN = -0.01
 * TA::arg1::MAX = 0.015
 * @endcode
 */
class FilterRate : public ProcessingBlock {
	public:
		FilterRate(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);

	private:
		void parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs);
		double min_rate_of_change, max_rate_of_change;
};

} //end namespace

#endif
