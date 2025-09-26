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
#ifndef FILTERPOTENTIALSW_H
#define FILTERPOTENTIALSW_H

#include <meteoio/meteoFilters/ProcessingBlock.h>
#include <vector>
#include <string>

namespace mio {

/**
 * @class FilterPotentialSW
 * @ingroup processing
 * @brief Checks for physically realistic incoming short wave radiation (ISWR) values.
 * @details
 * For each data point, the measured value must be:
 *     - more than the horizontal top of atmosphere potential radiation multiplied by the *min_coeff* coefficient;
 *     - less than the global horizontal potential radiation multiplied by the *max_coeff* coefficient.
 *
 * Alternatively, you can set the MODE keyword to GROUND, in which case *min_coeff* is also multiplied with
 * the global potential radiation for the minimum check.
 *
 * It takes the following arguments:
 *  - MIN_COEFF: minimum coefficient (default: 0.03);
 *  - MAX_COEFF: maximum coefficient (default: 1.1);
 *  - SOFT: if true, filtered values are set to the filter limits (coefficient * calculation) instead of nodata (optional);
 *  - MEAN_PERIOD: average the radiation over this given period (in seconds)
 *  - MODE: set to GROUND to use ground level global radiation when evaluating the minimum plausible radiation (optional).
 *
 * The default values come from Moradi, I., <i>"Quality control of global solar radiation using
 * sunshine duration hours"</i>, 2009, Energy 34, <b>no. 1</b>, 1-6.
 * @code
 * ISWR::filter1         = PotentialSW
 * ISWR::arg1::MIN_COEFF = 0.03
 * ISWR::arg1::MAX_COEFF = 1.1
 * @endcode
 *
 * If your data logger aggregates measurements this can lead to troubles at the steeper parts of the potential radiation
 * curve. For 10 minutes, the difference can be a couple of dozens W/m^2 which would be averaged and compared to the theoretical value
 * at the end of the aggregation period. To remedy this, you can set MEAN_PERIOD (in seconds).
 * @code
 * ISWR::filter1           = PotentialSW
 * ISWR::arg1::min_coeff   = 0
 * ISWR::arg1::max_coeff   = 1.05
 * ISWR::arg1::soft        = true
 * ISWR::arg1::mean_period = 600 ;ie 10 minutes
 * @endcode
 *
 * In the above example, if the filter meets a measurement at 10:15 it will compute the potential radiation for 10:15, 10:14, ...,
 * 10:05 (i. e. 11 values), take the arithmetic mean, and compare that to the value at 10:15 as usual. Note that influential
 * atmospheric parameters (TA, RH, P) are currently taken as constant from the measurement time step (at 10:15 in
 * the example). The *min_coeff* in this case is essentially a minimum filter to 0.
 */

class FilterPotentialSW : public ProcessingBlock {
	public:
		FilterPotentialSW(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);

	private:
		void parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs);

		double min_coeff, max_coeff;
		double mean_period; //in minutes
		bool is_soft;
		bool use_toa;
};

} //end namespace

#endif
