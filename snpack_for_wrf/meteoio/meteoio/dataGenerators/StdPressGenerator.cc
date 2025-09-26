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

#include <meteoio/dataGenerators/StdPressGenerator.h>
#include <meteoio/meteoLaws/Atmosphere.h>

namespace mio {

bool StandardPressureGenerator::generate(const size_t& param, MeteoData& md)
{
	double &value = md(param);
	if (value == IOUtils::nodata) {
		const double altitude = md.meta.position.getAltitude();
		if (altitude==IOUtils::nodata) return false;
		value = Atmosphere::stdAirPressure(altitude);
	}

	return true; //all missing values could be filled
}

bool StandardPressureGenerator::create(const size_t& param, std::vector<MeteoData>& vecMeteo)
{
	if (vecMeteo.empty()) return true;

	const double altitude = vecMeteo.front().meta.position.getAltitude(); //if the stations move, this has to be in the loop
	if (altitude==IOUtils::nodata) return false;

	for (size_t ii=0; ii<vecMeteo.size(); ii++) {
		double &value = vecMeteo[ii](param);
		if (value == IOUtils::nodata)
			value = Atmosphere::stdAirPressure(altitude);
	}

	return true; //all missing values could be filled
}

} //namespace
