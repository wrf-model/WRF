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
#ifndef PROCUNDERCATCH_HAMON_H
#define PROCUNDERCATCH_HAMON_H

#include <meteoio/meteoFilters/ProcessingBlock.h>
#include <vector>
#include <string>

namespace mio {

/**
 * @class  ProcUndercatch_Hamon
 * @ingroup processing
 * @brief Correct precipitation for undercatch in winter conditions.
 * @details
 * This implements the method for precipitation correction as described in
 * <i>"Computing actual precipitation"</i>, W. R. Hamon, in <i>Distribution of precipitation in mountaineous areas, Geilo symposium 1</i>, pp 159-174, World Meteorological Organization, Geneva, 1972. This correction depends on the usage of a shield around the gauge,
 * therefore the type of rain gauge must be specified with the TYPE argument as one of:
 * - sh - original rain gauge as used in the reference, shielded
 * - unsh - original rain gauge as used in the reference, unshielded
 * - Hellmannsh - shielded and fitted on Weissflujoch (Switzerland) data
 *
 * @code
 * PSUM::filter1    = undercatch_hamon
 * PSUM::arg1::type = sh
 * @endcode
 */

class ProcUndercatch_Hamon : public ProcessingBlock {
	public:
		ProcUndercatch_Hamon(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);

	private:
		typedef enum SENSOR_TYPE {
			sh,
			unsh,
			hellmannsh
		} sensor_type;

		void parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs);

		sensor_type type;
};

} //end namespace

#endif
