/***********************************************************************************/
/*  Copyright 2019 Avalanche Warning Service Tyrol                  LWD-TIROL      */
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

#ifndef ZRXPIO_H
#define ZRXPIO_H

#include <string>

#include <meteoio/IOInterface.h>

namespace mio {

/**
 * @class ZRXPIO
 * @brief Outputs meteo data in a text format readable by a WISKI database.
 *
 * @ingroup plugins
 * @author Michael Reisecker
 * @date   2018-12
 */
class ZRXPIO : public IOInterface {
	public:
		ZRXPIO(const std::string& configfile);
		ZRXPIO(const Config& cfgreader);
		ZRXPIO(const ZRXPIO&);
		virtual void writeMeteoData(const std::vector< std::vector<MeteoData> >& vecMeteo,
		                            const std::string& name = "");

	private:
		const Config cfg;

		void checkForUsedParameters(const std::vector<MeteoData>& vecMeteo, std::vector<bool>& vecUsedParams,
		    const size_t& nr_of_params, const bool& output_nodata_params, bool& data_exists);
};

} //namespace

#endif
