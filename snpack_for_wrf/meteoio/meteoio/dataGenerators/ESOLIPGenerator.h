/***********************************************************************************/
/*  Copyright 2013 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef ESOLIPGENERATOR_H
#define ESOLIPGENERATOR_H

#include <meteoio/dataGenerators/GeneratorAlgorithms.h>

namespace mio {

/**
 * @class ESOLIPGenerator
 * @ingroup parametrizations
 * @brief Generate precipitation from changes in snow height.
 * @details
 * This implements the approach laid out in
 * Mair et al., <i>"ESOLIP–estimate of solid and liquid precipitation at sub-daily time resolution by combining snow height
 * and rain gauge measurements"</i>, Hydrology and Earth System Sciences Discussions, <b>10(7)</b>, 8683-8714, 2013. or
 * Mair E., Leitinger G., Della Chiesa S., Niedrist G., Tappeiner U., Bertoldi G., <i>"A simple method to combine snow height and
 * meteorological observations to estimate winter precipitation at sub-daily resolution"</i>, Journal of Hydrological Sciences,
 * in revision, 2015.
 * The snow density relies on Zwart, <i>"Significance of new-snow properties for snowcover development"</i>,master's thesis,
 * Institute for Marine and Atmospheric Research, University of Utrecht, 78 pp, 2007.
 *
 * @note only identified precipitation events are written out, this means that it is recommended to run through a Cst=0 data generator afterward
 *
 * @code
 * [Generators]
 * PSUM::generators = ESOLIP
 * @endcode
 */
class ESOLIPGenerator : public GeneratorAlgorithm {
	public:
		ESOLIPGenerator(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo)
			: GeneratorAlgorithm(vecArgs, i_algo) { parse_args(vecArgs); }
		bool generate(const size_t& param, MeteoData& md);
		bool create(const size_t& param, std::vector<MeteoData>& vecMeteo);

	private:
		static double newSnowDensity(const MeteoData& md);
};

} //end namespace mio

#endif
