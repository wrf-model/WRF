/*
 *  SNOWPACK stand-alone
 *
 *  Copyright WSL Institute for Snow and Avalanche Research SLF, DAVOS, SWITZERLAND
*/
/*  This file is part of Snowpack.
    Snowpack is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Snowpack is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Snowpack.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef SNOWPACKCONFIG_H
#define SNOWPACKCONFIG_H

#include <meteoio/MeteoIO.h>
#include <map>
#include <string>

class SnowpackConfig : public mio::Config {

	public:
		/**
		 * @brief Main constructor. The file is parsed and a key/value map object is internally created
		 *        furthermore all missing key value pairs needed for Snowpack operation are added
		 * @param[in] i_filename string representing the absolute filename of the key/value file
		 */
		SnowpackConfig(const std::string& i_filename);
		SnowpackConfig(const mio::Config& i_cfg);
		~SnowpackConfig() {}

	private:
		void setDefaults();
		bool enforce_measured_snow_heights;

		static const bool __init;     ///<helper variable to enable the init of static collection data
		static bool initStaticData(); ///<initialize the static containers
		static std::map<std::string, std::string> snowpackConfig, advancedConfig, inputConfig, outputConfig;
};

#endif
