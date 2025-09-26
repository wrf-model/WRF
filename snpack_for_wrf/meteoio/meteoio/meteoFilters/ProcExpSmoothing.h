/***********************************************************************************/
/*  Copyright 2012 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef PROCEXPSMOOTHING_H
#define PROCEXPSMOOTHING_H

#include <meteoio/meteoFilters/WindowedFilter.h>
#include <vector>
#include <string>

namespace mio {

/**
 * @class ProcExpSmoothing
 * @ingroup processing
 * @brief Exponential smoothing.
 * @details
 * This implements an exponential moving average smoothing of parameter alpha (https://en.wikipedia.org/wiki/Exponential_smoothing)
 * such as (s being the filtered values and x the raw data):
 * - s_0 = x_0
 * - s_n = alpha*x_(t-1) + (1-alpha)*s_(t-1)
 *
 * Nodata values are excluded from the moving average calculation. It takes the following arguments:
 *  - all the window parameters as defined in WindowedFilter::setWindowFParams();
 *  - ALPHA: the alpha parameter defined above (it must be between 0 and 1, mandatory).
 *
 * The standard behavior for this filter is obtained by using a left window. If using a right window, it behaves as if time was reversed
 * (ie. predictions from the future). A centered window applies the standard algorithms on the <b>distance</b> between the center point and each points,
 * that is that points get averaged symetrically around the middle point before running the standard algorithm.
 *
 * @note This filter temporally shifts the signal back by window_width/2 if using left or right window
 * @note This would probably lead to slightly unexpected results if used on irregularly sampled data
 *
 * @code
 * TA::filter1         = exp_smoothing
 * TA::arg1::soft      = FALSE
 * TA::arg1::centering = right
 * TA::arg1::min_pts   = 1
 * TA::arg1::min_span  = 1800 ;ie 1800 seconds time span for the left leaning window
 * TA::arg1::alpha     = 0.8
 *
 * RH::filter1        = mean_avg
 * RH::arg1::min_pts  = 10
 * RH::arg1::min_span = 600 ;strictly centered window spanning 600 seconds and at least 10 points
 * RH::arg1::alpha    = 0.6
 * @endcode
 */

class ProcExpSmoothing : public WindowedFilter {
	public:
		ProcExpSmoothing(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);

	private:
		void parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs);
		static double calcExpSmoothing(const std::vector<MeteoData>& ivec, const unsigned int& param, const size_t& start, const size_t& end, const size_t& pos, const double& i_alpha);

		double alpha;
};

} //end namespace

#endif
