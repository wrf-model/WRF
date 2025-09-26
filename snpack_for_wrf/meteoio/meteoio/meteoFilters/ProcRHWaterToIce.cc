/***********************************************************************************/
/*  Copyright 2019 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#include <meteoio/meteoFilters/ProcRHWaterToIce.h>
#include <meteoio/meteoLaws/Meteoconst.h>
#include <meteoio/meteoLaws/Atmosphere.h>
#include <cmath>
#include <stdio.h>

using namespace std;

namespace mio {

ProcRHWaterToIce::ProcRHWaterToIce(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name)
                       : ProcessingBlock(vecArgs, name)
{
	properties.stage = ProcessingProperties::first; //for the rest: default values
}

void ProcRHWaterToIce::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
                        std::vector<MeteoData>& ovec)
{
	if (param!=MeteoData::RH)
		throw InvalidArgumentException("Trying to use "+getName()+" filter on " + MeteoData::getParameterName(param) + " but it can only be applied to RH!!" + getName(), AT);
	ovec = ivec;

	for (size_t ii=0; ii<ovec.size(); ii++) {
		double& tmp = ovec[ii](param);
		const double TA = ovec[ii](MeteoData::TA);

		if (tmp == IOUtils::nodata || TA==IOUtils::nodata) {
			continue; //preserve nodata values and no precip
		}

		tmp *= Atmosphere::vaporSaturationPressureWater(TA) / Atmosphere::vaporSaturationPressure(TA);
	}
}

} //end namespace
