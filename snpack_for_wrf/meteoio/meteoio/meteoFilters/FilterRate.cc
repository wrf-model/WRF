/***********************************************************************************/
/*  Copyright 2009 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#include <meteoio/meteoFilters/FilterRate.h>
#include <cmath>
#include <algorithm>

using namespace std;

namespace mio {

FilterRate::FilterRate(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name)
           : ProcessingBlock(vecArgs, name), min_rate_of_change(0.), max_rate_of_change(0.)
{
	parse_args(vecArgs);
	properties.stage = ProcessingProperties::both; //for the rest: default values
}

void FilterRate::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
                           std::vector<MeteoData>& ovec)
{
	ovec = ivec;
	size_t last_good = IOUtils::npos;

	//Find first point that is not IOUtils::nodata
	for (size_t ii=0; ii<ovec.size(); ii++){
		if (ovec[ii](param) != IOUtils::nodata){
			last_good = ii;
			break;
		}
	}

	if (last_good == IOUtils::npos) //can not find a good point to start
		return;

	for (size_t ii=(last_good+1); ii<ovec.size(); ii++) {
		double& curr_value       = ovec[ii](param);
		const double& prev_value = ovec[last_good](param);
		const double curr_time   = ovec[ii].date.getJulian();
		const double prev_time   = ovec[last_good].date.getJulian();

		if (curr_value == IOUtils::nodata)
			continue;

		const double local_rate = (curr_value-prev_value) / ((curr_time-prev_time+1e-12)*24.*3600.); //per seconds

		if ( local_rate>max_rate_of_change || local_rate<min_rate_of_change ) {
			curr_value = IOUtils::nodata;
		} else {
			last_good = ii;
		}
	}
}

void FilterRate::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	const std::string where( "Filters::"+block_name );
	bool has_max=false, has_min=false;

	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="MIN") {
			IOUtils::parseArg(vecArgs[ii], where, min_rate_of_change);
			has_min = true;
		} else if (vecArgs[ii].first=="MAX") {
			IOUtils::parseArg(vecArgs[ii], where, max_rate_of_change);
			has_max = true;
		}
	}

	if (!has_max) throw InvalidArgumentException("Please provide a MAX value for "+where, AT);
	if (has_max && !has_min) min_rate_of_change = -max_rate_of_change;
}

}
