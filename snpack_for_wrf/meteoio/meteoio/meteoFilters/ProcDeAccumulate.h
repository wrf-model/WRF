/***********************************************************************************/
/*  Copyright 2018 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef PROCDEACCUMULATE_H
#define PROCDEACCUMULATE_H

#include <meteoio/meteoFilters/ProcessingBlock.h>
#include <vector>
#include <string>

namespace mio {

/**
 * @class  ProcDeAccumulate
 * @ingroup processing
 * @brief Convert accumulated values back to instantaneous values.
 * @details
 * Some radiation parameters are sometimes provided as accumulated values over a given period (such as ECMWF radiation fields). 
 * The value at any given timestep is either the instantaneous value (at the beginning of the accumulation period) or the sum of
 * the instantaneous value and the preceeding values.
 * This can be illustrated as:
 *  - at time t, the value is x1;
 *  - at time t+dt, the value is x1+dx1;
 *  - at time t+2dt, the value is x1+dx1+dx2;
 *  - at time t+3dt, the value is x2;
 *  - at time t+4dt, the value is x2+dx2;
 *  - ...
 *
 * This filter recomputes the instantaneous values by considering that once a value gets smaller than its preceding values, the
 * end of the accumulation period has been reached, so the value is kept as is and the process starts over again (thus this
 * assumes that they are no negative values). 
 * 
 * @code
 * VW::filter3    = DEACCUMULATE
 * @endcode
 */

class ProcDeAccumulate : public ProcessingBlock {
	public:
		ProcDeAccumulate(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);
};

} //end namespace

#endif
