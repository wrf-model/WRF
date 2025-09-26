/***********************************************************************************/
/*  Copyright 2018 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#include <meteoio/meteoFilters/ProcDeAccumulate.h>

using namespace std;

namespace mio {

ProcDeAccumulate::ProcDeAccumulate(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name)
              : ProcessingBlock(vecArgs, name)
{
	properties.stage = ProcessingProperties::first;
}

void ProcDeAccumulate::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
                            std::vector<MeteoData>& ovec)
{
	ovec = ivec;
	
	double prec_value = IOUtils::nodata;
	for (size_t ii=0; ii<ovec.size(); ii++){
		double& value = ovec[ii](param);
		const double org_value = ovec[ii](param);
		if (value!=IOUtils::nodata && prec_value!=IOUtils::nodata && prec_value<=value)
			value -= prec_value;
		if (prec_value==IOUtils::nodata)
			value = IOUtils::nodata;
		
		prec_value = org_value;
	}
}

} //namespace
