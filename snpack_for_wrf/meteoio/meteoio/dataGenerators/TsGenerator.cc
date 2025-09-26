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

#include <meteoio/dataGenerators/TsGenerator.h>
#include <meteoio/meteoLaws/Meteoconst.h>
#include <meteoio/MathOptim.h>

namespace mio {

const double TsGenerator::e_snow = .983; //snow emissivity (0.969 - 0.997)
const double TsGenerator::e_soil = .9805; //grass emissivity (0.975 - 0.986)

bool TsGenerator::generate(const size_t& param, MeteoData& md)
{
	double &value = md(param);
	if (value == IOUtils::nodata) {
		const double olwr = md("OLWR");
		if (olwr==IOUtils::nodata) //nothing else we can do here
			return false;

		const double hs = md(MeteoData::HS);
		const double ea = (hs==IOUtils::nodata)? .5*(e_snow+e_soil) : (hs>snow_thresh)? e_snow : e_soil;

		//value = pow( olwr / ( ea * Cst::stefan_boltzmann ), 0.25);
		value = Optim::invSqrt( Optim::invSqrt(olwr / ( ea * Cst::stefan_boltzmann )) );

		if (value==IOUtils::nodata) return false;
	}

	return true; //all missing values could be filled
}

bool TsGenerator::create(const size_t& param, std::vector<MeteoData>& vecMeteo)
{
	if (vecMeteo.empty()) return true;

	bool status = true;
	for (size_t ii=0; ii<vecMeteo.size(); ii++) {
		if (!generate(param, vecMeteo[ii]))
			status = false;
	}

	return status;
}

} //namespace
