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
#ifndef STANDARDPRESSUREGENERATOR_H
#define STANDARDPRESSUREGENERATOR_H

#include <meteoio/dataGenerators/GeneratorAlgorithms.h>

namespace mio {

/**
 * @class StandardPressureGenerator
 * @ingroup parametrizations
 * @brief Standard atmospheric pressure generator.
 * @details
 * Generate a standard atmosphere's pressure, depending on the local elevation.
 * @code
 * [Generators]
 * P::generators = STD_PRESS
 * @endcode
 */
class StandardPressureGenerator : public GeneratorAlgorithm {
	public:
		StandardPressureGenerator(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo)
			: GeneratorAlgorithm(vecArgs, i_algo) { parse_args(vecArgs); }
		bool generate(const size_t& param, MeteoData& md);
		bool create(const size_t& param, std::vector<MeteoData>& vecMeteo);
};

} //end namespace mio

#endif
