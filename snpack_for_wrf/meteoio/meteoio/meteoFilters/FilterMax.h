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
#ifndef FILTERMAX_H
#define FILTERMAX_H

#include <meteoio/meteoFilters/ProcessingBlock.h>
#include <vector>
#include <string>

namespace mio {

/**
 * @class FilterMax
 * @ingroup processing
 * @brief Max range filter.
 * @details
 * Reject all values greater than the max. Remarks:
 * - MAX: the maximum permissible value (in SI, mandatory);
 * - SOFT: if set to TRUE, all data larger than the max would be assigned
 * either the maximum permissible value or another value given as an extra argument (optional);
 * - MAX_RESET: if SOFT has been set to TRUE, this is the new value for otherwise rejected points (optional).
 *
 * @code
 * TA::filter1         = MAX
 * TA::arg1::MAX       = 330
 * TA::arg1::SOFT      = TRUE
 * TA::arg1::MAX_RESET = 350
 * @endcode
 */

class FilterMax : public ProcessingBlock {
	public:
		FilterMax(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);

	private:
		void parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs);

		double max_val;
		double max_soft;
		bool is_soft;
};

} //end namespace

#endif
