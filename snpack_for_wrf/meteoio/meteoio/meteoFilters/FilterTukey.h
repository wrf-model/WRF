/***********************************************************************************/
/*  Copyright 2011 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef FILTERTUKEY_H
#define FILTERTUKEY_H

#include <meteoio/meteoFilters/WindowedFilter.h>
#include <vector>
#include <string>

namespace mio {

/**
 * @class  FilterTukey
 * @ingroup processing
 * @brief Tukey 53H method
 * @details
 * A smooth time sequence is generated from the median, substracted from the original signal and compared with the standard deviation.
 * see <i>"Despiking Acoustic Doppler Velocimeter Data"</i>, Derek G. Goring and Vladimir L. Nikora, Journal of Hydraulic Engineering, <b>128</b>, 1, 2002
 * The deviation factor coeffecient is currently hard-coded as k=1.5. It takes as arguments all the window parameters as defined in WindowedFilter::setWindowFParams().
 *
 * @code
 * TA::filter1         = Tukey
 * TA::arg1::soft      = TRUE
 * TA::arg1::centering = left
 * TA::arg1::MIN_PTS   = 1
 * TA::arg1::MIN_SPAN  = 1800 ;ie 1800 seconds time span for the left leaning window
 *
 * RH::filter1        = Tukey
 * RH::arg1::MIN_PTS  = 10
 * RH::arg1::MIN_SPAN = 600 ;strictly centered window spanning 600 seconds and at least 10 points
 * @endcode
 */
class FilterTukey : public WindowedFilter {
	public:
		FilterTukey(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);

	private:
		void parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs);
		static double getStdDev(const std::vector<MeteoData>& ivec, const unsigned int& param, const size_t& start, const size_t& end);
		static double getU3(const std::vector<MeteoData>& ivec, const size_t& i, const unsigned int& param);
		static const double k; ///<How many times the stddev allowed as deviation to the smooth signal for valid points
};

} //end namespace

#endif
