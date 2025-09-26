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
#ifndef ALLSKYSWGENERATOR_H
#define ALLSKYSWGENERATOR_H

#include <meteoio/dataGenerators/GeneratorAlgorithms.h>
#include <meteoio/meteoLaws/Sun.h>

namespace mio {

/**
 * @class AllSkySWGenerator
 * @ingroup parametrizations
 * @brief ISWR all sky parametrization
 * @details
 * Using air temperature (TA) and relative humidity (RH) and optionnally cloud transmissivity (TAU_CLD),
 * this computes the potential incoming solar radiation, based on the position of the sun in the sky
 * (as a function of the location and the date) on the horizontal at ground level.
 * If a cloud transmissivity (TAU_CLD) has been provided, or if an
 * incoming long wave measurement is available, it corrects the generated iswr for cloudiness
 * (basically doing like UnsworthGenerator in reverse), otherwise this assumes clear sky!
 * If no TA or RH is available, average values will be used (in order to get an average value
 * for the precipitable water vapor).
 * @code
 * [Generators]
 * ISWR::generators = allsky_SW
 * @endcode
 * @note This relies on SunObject to perform the heavy duty computation.
 */
class AllSkySWGenerator : public GeneratorAlgorithm {
	public:
		AllSkySWGenerator(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo)
			: GeneratorAlgorithm(vecArgs, i_algo), sun() { parse_args(vecArgs); }
		bool generate(const size_t& param, MeteoData& md);
		bool create(const size_t& param, std::vector<MeteoData>& vecMeteo);
	private:
		static double getSolarIndex(const double& ta, const double& rh, const double& ilwr);
		SunObject sun;
};

} //end namespace mio

#endif
