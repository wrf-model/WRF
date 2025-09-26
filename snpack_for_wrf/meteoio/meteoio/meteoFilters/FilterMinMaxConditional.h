/***********************************************************************************/
/*  Copyright 2019 Avalanche Warning Service Tyrol                  LWD-TIROL      */
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

#ifndef FILTERMINMAXCONDITIONAL_H
#define FILTERMINMAXCONDITIONAL_H

#include <meteoio/meteoFilters/ProcessingBlock.h>
#include <string>
#include <vector>

namespace mio {

/**
 * @class FilterMinMaxConditional
 * @ingroup processing
 * @author Michael Reisecker
 * @date   2019-04-03
 * @brief Applies a min/max filter if a second parameter matches a comparison.
 * @details
 * This is a min/max filter that only takes action if a different parameter holds true to a condition.
 * The synopsis is the same as for the `MIN_MAX` filter, but you can (have to) provide a second parameter
 * with the `PARAMETER` keyword, the comparison operation with `OPERATOR` and the value to compare against
 * with `COMPARE`.
 *
 * Arguments:
 *  - `PARAMETER`: The parameter that has to hold true to a comparison
 *  - `OPERATOR`: Comparison operator (`LT`, `LE`, `GT`, `GE`, `EQ`, `NE`)
 *  - `COMPARE`: Value to compare against
 *  - `SOFT`: If true, values are not filtered to nodata but to the filter limits or the reset values
 *  - `MIN`: Minimum value for the min/max filter
 *  - `MAX`: Maximum value for the min/max filter
 *  - `MIN_RESET`: Vales lower than `MIN` are not filtered to `MIN` but to this
 *  - `MAX_RESET`: Values greater than `MAX` are not filtered to `MAX` but to this
 *  - `RESET`: Values filtered if neither `MIN` nor `MAX` are supplied are not filtered to nodata, but to this
 *  - `IGNORE_MISSING`: Skip over access errors if `PARAMETER` is an extra parameter and not present
 *
 * The `OPERATOR` can be `LT` (less than), `LE` (less than or equal), `GT` (greater than), `GE` (greater equal),
 * `EQ` (equal) or `NE` (not equal).
 *
 * For example, to filter snow surface temperatures above zero to zero if there is enough snow (30cm) you could write:
 * @code
 * TSS::FILTER1         = MIN_MAX_CONDITIONAL
 * TSS::ARG1::PARAMETER = HS
 * TSS::ARG1::OPERATOR  = GT
 * TSS::ARG1::COMPARE   = 0.3
 * TSS::ARG1::MAX       = 273.15
 * TSS::ARG1::SOFT      = TRUE
 * @endcode
 *
 * If neither `OPERATOR` nor `COMPARE` are supplied, it defaults to `EQ` and `nodata`. Furthermore, if
 * neither a `MIN` nor a `MAX` value are provided, it is always filtered to `nodata` (or the `RESET` value).
 * Thus, to filter all wind directions if no wind speed is available it's enough to write:
 * @code
 * DW::FILTER1 = MIN_MAX_CONDITIONAL
 * DW::FILTER1::PARAMETER = VW
 * @endcode
 */

class FilterMinMaxConditional : public ProcessingBlock {
	public:
		FilterMinMaxConditional(const std::vector< std::pair<std::string, std::string> >& vecArgs,
		    const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		    std::vector<MeteoData>& ovec);

	private:
		bool assert_condition(const double& condition_value);
		void parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs);

		double min_val, max_val;
		double min_soft, max_soft;
		double nodata_reset;
		bool is_soft;
		std::string condition_param, condition_operator;
		double condition_compare;
		bool ignore_missing_param;
};

} //namespace

#endif
