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
#ifndef FILTERMAD_H
#define FILTERMAD_H

#include <meteoio/meteoFilters/WindowedFilter.h>
#include <vector>
#include <string>

namespace mio {

/**
 * @class  FilterMAD
 * @ingroup processing
 * @brief Median Absolute Deviation.
 * @details
 * Values outside of median ± 3 σ_MAD are rejected. The σ_MAD is calculated as follows:\n
 * <center>\f$ \sigma_{MAD} = K \cdot \mathop{median_i} \left( \left| X_i - \mathop{median_j} ( X_j ) \right| \right) \f$ with \f$ K = \Phi^{-1}; \Phi = 0.6745 \f$ </center>\n
 * See http://en.wikipedia.org/wiki/Median_absolute_deviation for more information about the Mean Absolute Deviation.
 * It takes the following arguments:
 *  - MIN_SIGMA: to avoid rejecting all points after a period of constant signal (which brings sigma to zero),
 * you can set a minimum value for sigma (optional);
 *  - all the window parameters as defined in WindowedFilter::setWindowFParams().
 *
 * Examples for the io.ini file:
 * @code
 * TA::filter1         = mad
 * TA::arg1::soft      = TRUE
 * TA::arg1::centering = left
 * TA::arg1::MIN_PTS   = 1
 * TA::arg1::MIN_SPAN  = 1800 ;ie 1800 seconds time span for the left leaning window
 *
 * RH::filter1        = mad
 * RH::arg1::MIN_PTS  = 10
 * RH::arg1::MIN_SPAN = 600 ;strictly centered window spanning 600 seconds and at least 10 points
 * @endcode
 */

class FilterMAD : public WindowedFilter {
	public:
		FilterMAD(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);

	private:
		void MAD_filter_point(const std::vector<MeteoData>& ivec, const unsigned int& param, const size_t& start, const size_t& end, double &value) const;

		double min_sigma; //to avoid rejecting all points after a period of constant signal
};

} //end namespace

#endif
