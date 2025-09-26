/***********************************************************************************/
/*  Copyright 2014 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef FilterTimeconsistency_H
#define FilterTimeconsistency_H

#include <meteoio/meteoFilters/WindowedFilter.h>

#include <vector>
#include <string>

namespace mio {

/**
 * @class FilterTimeconsistency
 * @ingroup processing
 * @brief Check that the time evolution of a given parameter is consistent with its past evolution
 * @details
 * Compare sum of differences between snow depth (HS) value and HS value before and HS value afterwards, respectively,
 *  with 4 times of the standard deviation of HS in a defined time period (something like MAD)
 * First, the standard deviation of HS is calculated for a certain time period. Afterwards, the difference between the value and 
 * the value before and the difference between the value and the following value are calculated. Then, the sum of the two 
 * differences is calculated and compared with 4 times of the standard deviation. Is the sum lower than the standard deviation, 
 * the HS value is accepted. Otherwise the HS value gets invalid. 
 * References/Literature:  Zahumensky, Igor, 2004: Guidelines on Quality Control Procedures for Data from Automatic Weather Stations, World Meteorological Organisation 
 * 
 * It takes as arguments the window parameters as defined in WindowedFilter::setWindowFParams().
 *
 * @code
 * Valid examples for the io.ini file:
 * HS::filter1         = time_consistency
 * HS::arg1::soft      = true
 * HS::arg1::centering = left
 * HS::arg1::min_pts   = 1
 * HS::arg1::min_span  = 1800 ;ie left centered window spanning 1800 seconds and at least 1 points
 *
 * TA::filter1         = time_consistency
 * TA::arg1::min_pts   = 10
 * TA::arg1::min_span  = 600 ;ie strictly centered window spanning 600 seconds and at least 10 points
 * @endcode
 * 
 * @author Anna-Maria Tilg
 * @date   2015-12-08
 */

class FilterTimeconsistency : public WindowedFilter {
	public:
		FilterTimeconsistency(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);
};

} //end namespace

#endif
