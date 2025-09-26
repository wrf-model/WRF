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
#ifndef ISWRALBEDOGENERATOR_H
#define ISWRALBEDOGENERATOR_H

#include <meteoio/dataGenerators/GeneratorAlgorithms.h>

namespace mio {

/**
 * @class IswrAlbedoGenerator
 * @ingroup parametrizations
 * @brief Incoming or reflected short wave generator.
 * @details
 * Generate the incoming short wave radiation from the reflected short wave radiation or the opposite. The albedo
 * is either a grassy soil albedo or a snow albedo depending on the snow height. It has the following optional arguments:
 *  - FORCE:  If no snow height is available, the generator will simply return unless the "FORCE" argument is set to TRUE
 *
 * @code
 * [Generators]
 * ISWR::generators = ISWR_ALBEDO
 * @endcode
 */
class IswrAlbedoGenerator : public GeneratorAlgorithm {
	public:
		IswrAlbedoGenerator(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo)
			: GeneratorAlgorithm(vecArgs, i_algo), force(false) { parse_args(vecArgs); }
		bool generate(const size_t& param, MeteoData& md);
		bool create(const size_t& param, std::vector<MeteoData>& vecMeteo);
	private:
		void parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs);
		bool force; ///< forces to convert radiation even when no HS is present
};

} //end namespace mio

#endif
