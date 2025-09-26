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
#ifndef PROCWMASMOOTHING_H
#define PROCWMASMOOTHING_H

#include <meteoio/meteoFilters/WindowedFilter.h>
#include <vector>
#include <string>

namespace mio {

/**
 * @class  ProcWMASmoothing
 * @ingroup processing
 * @brief Weighted moving average smoothing.
 * @details
 * This implements a weighted moving average smoothing (see http://en.wikipedia.org/wiki/Simple_moving_average#Weighted_moving_average) such as:
 * - WMA_unnormalized = 1*X1 + 2*X2 + ... + n*Xn (n being the current point)
 * - then WMA = WMA_unnormalized / sum_of_weights with sum_of_weights = 1+2+3+...+n = n(n+1)/2
 *
 *  Nodata values are excluded from the moving average calculation. It takes as arguments all the window parameters as defined in WindowedFilter::setWindowFParams();
 *
 * The standard behavior for this filter is obtained by using a left window. If using a right window, it behaves as if time was reversed
 * (ie. predictions from the future). A centered window applies the standard algorithms on the <b>distance</b> between the center point and each points,
 * that is that points get averaged symetrically around the middle point before running the standard algorithm.
 *
 * @note This filter temporally shifts the signal back by window_width/2 if using left or right window
 * @note This would probably lead to slightly unexpected results if used on irregularly sampled data
 *
 * @code
 * TA::filter1         = wma_smoothing
 * TA::arg1::soft      = FALSE
 * TA::arg1::centering = right
 * TA::arg1::min_pts   = 1
 * TA::arg1::min_span  = 1800 ;ie 1800 seconds time span for the left leaning window
 * @endcode
 */

class ProcWMASmoothing : public WindowedFilter {
	public:
		ProcWMASmoothing(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);

	private:
		static double calcWMASmoothing(const std::vector<MeteoData>& ivec, const unsigned int& param, const size_t& start, const size_t& end, const size_t& pos);
};

} //end namespace

#endif
