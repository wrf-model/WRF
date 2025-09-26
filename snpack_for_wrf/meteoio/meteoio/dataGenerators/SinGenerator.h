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
#ifndef SINGENERATOR_H
#define SINGENERATOR_H

#include <meteoio/dataGenerators/GeneratorAlgorithms.h>

namespace mio {

/**
 * @class SinGenerator
 * @ingroup parametrizations
 * @brief Sinusoid generator.
 * @details
 * Generate a sinusoidal variation for this parameter, as provided in argument (please remember that it must be in SI units).
 * The arguments that must be provided are the following:
 *  - TYPE: the sinusoidal period, either
 *       - yearly
 *       - daily
 *  - MIN: the minimum value (SI units, mandatory);
 *  - MAX: the maximum value (SI units, mandatory);
 *  - PHASE: the time offset specifying when the minimum value should be reached, within one period (expressed as fraction of such period, defaults to zero);
 *
 * The example below generates a yearly sinusoidal variation for the air temperature, the minimum being 268.26 K and occuring at 1/12
 * of the period (which practically means, at the end of the first month).
 * @code
 * [Generators]
 * TA::generators = Sin
 * TA::Sin::type  = yearly
 * TA::Sin::min   = 268.26
 * TA::Sin::max   = 285.56
 * TA::Sin::phase = 0.0833
 * @endcode
 */
class SinGenerator : public GeneratorAlgorithm {
	public:
		SinGenerator(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo)
			: GeneratorAlgorithm(vecArgs, i_algo), amplitude(IOUtils::nodata), offset(IOUtils::nodata), phase(0.), type(' ') { parse_args(vecArgs); }
		bool generate(const size_t& param, MeteoData& md);
		bool create(const size_t& param, std::vector<MeteoData>& vecMeteo);
	private:
		void parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs);
		double amplitude, offset, phase;
		char type;
};

} //end namespace mio

#endif
