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
#ifndef PROCAGGREGATE_H
#define PROCAGGREGATE_H

#include <meteoio/meteoFilters/WindowedFilter.h>
#include <vector>
#include <string>

namespace mio {

/**
 * @class  ProcAggregate
 * @ingroup processing
 * @brief Data aggregation.
 * @details
 * This aggregates the input data over the defined window with the defined aggregation algorithm. It takes as arguments
 * all the window parameters as defined in WindowedFilter::setWindowFParams() as well as the following:
 *  - SOFT: the keyword "soft" maybe added (this is highly recommended), if the window position is allowed to be adjusted to the data present (boolean, optional);
 *  - TYPE: the type of filter to use, one of the following:
 *    + min: return the minimum value of the whole window;
 *    + max: return the maximum value of the whole window;
 *    + mean: return the mean of the whole window;
 *    + median: return the median of the whole window;
 *    + step_sum: return the sum over the last timestep (assuming that the value given at the end of the timestep is valid for the whole timestep);
 *    + wind_avg: Wind vector averaging. CURRENTLY, THIS FILTER DOES NOT WORK PROPERLY (the first parameter is correctly calculated but the second one uses the modified output of the first one and therefore is WRONG).
 *
 * Remarks: nodata values are excluded from the aggregation
 *
 * @code
 * VW::filter3    = AGGREGATE
 * VW::arg3::TYPE = MEAN
 * VW::arg3::soft = TRUE
 * VW::arg3::centering = left
 * VW::arg3::MIN_PTS = 4
 * VW::arg3::MIN_SPAN = 14400 ;ie 14400 seconds time span, at least 4 points, for a left leaning window
 * 
 * ;Reconstruct a PSUM signal from PINT
 * PINT::filter1 = AGGREGATE
 * PINT::arg1::type = step_sum
 * @endcode
 */

class ProcAggregate : public WindowedFilter {
	public:
		ProcAggregate(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);

	private:
		typedef enum AGGREGATE_TYPE {
			min_agg,
			max_agg,
			mean_agg,
			median_agg,
			step_sum,
			wind_avg_agg
		} aggregate_type;
		
		void parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs);
		static void sumOverLastStep(std::vector<MeteoData>& ovec, const unsigned int& param);
		static double calc_min(const std::vector<MeteoData>& ivec, const unsigned int& param, const size_t& start, const size_t& end);
		static double calc_max(const std::vector<MeteoData>& ivec, const unsigned int& param, const size_t& start, const size_t& end);
		static double calc_mean(const std::vector<MeteoData>& ivec, const unsigned int& param, const size_t& start, const size_t& end);
		static double calc_median(const std::vector<MeteoData>& ivec, const unsigned int& param, const size_t& start, const size_t& end);
		static double calc_wind_avg(const std::vector<MeteoData>& ivec, const unsigned int& param, const size_t& start, const size_t& end);
		
		aggregate_type type;
};

} //end namespace

#endif
