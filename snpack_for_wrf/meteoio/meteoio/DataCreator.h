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

#ifndef DATACREATOR_H
#define DATACREATOR_H

#include <meteoio/Config.h>
#include <meteoio/dataClasses/MeteoData.h>
#include <meteoio/dataGenerators/GeneratorAlgorithms.h>

#include <vector>
#include <map>
#include <set>

namespace mio {

/**
 * @class DataCreator
 * @brief A class to create new meteo data parameters from user-selected models or parametrizations.
 * This class sits in between the actual implementation of the various methods and the IOManager in
 * order to offer some high level interface. It basically reads the arguments and creates the objects for
 * the various data generators in its constructor and loop through the parameters and stations when called.
 *
 * @ingroup meteoLaws
 * @author Mathias Bavay
 */

class DataCreator {
	public:
		DataCreator(const Config& cfg);
		DataCreator(const DataCreator& c) : mapAlgorithms(c.mapAlgorithms) {}
		virtual ~DataCreator();

		void createParameters(std::vector<METEO_SET>& vecVecMeteo) const;

		DataCreator& operator=(const DataCreator& source);
		const std::string toString() const;

	protected:
		static std::set<std::string> getParameters(const Config& cfg, const std::string& key_pattern, const std::string& section);
		static std::vector<std::string> getAlgorithmsForParameter(const Config& cfg, const std::string& key_pattern, const std::string& section, const std::string& parname);
		static std::vector< std::pair<std::string, std::string> > getArgumentsForAlgorithm(const Config& cfg, const std::string& parname,
		                                const std::string& algorithm, const std::string& section);

		std::map< std::string, std::vector<GeneratorAlgorithm*> > mapAlgorithms; //per parameter data creators algorithms
};

} //end namespace

#endif
