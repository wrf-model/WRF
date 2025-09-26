/***********************************************************************************/
/*  Copyright 2019 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef PROCRHWATERTOICE_H
#define PROCRHWATERTOICE_H

#include <meteoio/meteoFilters/ProcessingBlock.h>
#include <vector>
#include <string>

namespace mio {

/**
 * @class  ProcRHWaterToIce
 * @ingroup processing
 * @brief Correct relative humidity over water to over ice in case temperature is below freezing.
 * @details
 * This implements a correction for relative humidity in case the provided relative humidity is relative to water for the full
 * temperature range. It recalculates the relative humidity to a value for above an ice surface in case air temperature is
 * below freezing.
 *
 * @code
 * RH::filter1    = RHWATERTOICE
 * @endcode
 */

class ProcRHWaterToIce : public ProcessingBlock {
	public:
		ProcRHWaterToIce(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);

	private:
};

} //end namespace

#endif
