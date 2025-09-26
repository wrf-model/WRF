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
#ifndef TSGENERATOR_H
#define TSGENERATOR_H

#include <meteoio/dataGenerators/GeneratorAlgorithms.h>

namespace mio {

/**
 * @class TsGenerator
 * @ingroup parametrizations
 * @brief Surface temperature generator.
 * @details
 * Generate the surface temperature from the outgoing long wave (OLWR).
 * @code
 * [Generators]
 * TSS::generators = TS_OLWR
 * @endcode
 */
class TsGenerator : public GeneratorAlgorithm {
	public:
		TsGenerator(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo)
			: GeneratorAlgorithm(vecArgs, i_algo) { parse_args(vecArgs); }
		bool generate(const size_t& param, MeteoData& md);
		bool create(const size_t& param, std::vector<MeteoData>& vecMeteo);
	private:
		static const double e_snow, e_soil;
};

} //end namespace mio

#endif
