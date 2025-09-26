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
#ifndef FILTERMINMAX_H
#define FILTERMINMAX_H

//#define CALL_MEMBER_FN(object,ptrToMember)  ((object).*(ptrToMember))

#include <meteoio/meteoFilters/ProcessingBlock.h>
#include <vector>
#include <string>

namespace mio {

/**
 * @class FilterMinMax
 * @ingroup processing
 * @brief Min/Max range filter.
 * @details
 * Reject all values greater than the max or smaller than the min. Arguments:
 * - MIN: the minimum permissible value (in SI, mandatory);
 * - MAX: the maximum permissible value (in SI, mandatory);
 * - SOFT: if set to TRUE, all data smaller than the min / larger than the max, would be assigned
 * either the minimum / maximum permissible value or another value given as an extra argument (optional);
 * - MIN_RESET: if SOFT has been set to TRUE, this is the new value for otherwise rejected points (optional).
 * - MAX_RESET: if SOFT has been set to TRUE, this is the new value for otherwise rejected points (optional).
 * @code
 * TA::filter1   = min_max
 * TA::arg1::MIN = 230
 * TA::arg1::MAX = 330
 *
 * ISWR::filter1         = min_max
 * ISWR::arg1::SOFT      = TRUE
 * ISWR::arg1::MIN       = 8
 * ISWR::arg1::MIN_RESET = 0
 * ISWR::arg1::MAX       = 1400
 * ISWR::arg1::MAX_RESET = 1398
 * @endcode
 *
 */

class FilterMinMax : public ProcessingBlock {
	public:
		FilterMinMax(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);

	private:
		void parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs);

		double min_val, max_val;
		double min_soft, max_soft;
		bool is_soft;
};

} //end namespace

#endif
