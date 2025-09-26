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
#include <meteoio/meteoFilters/FilterMin.h>

using namespace std;

namespace mio {

FilterMin::FilterMin(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name)
          : ProcessingBlock(vecArgs, name), min_val(0.), min_soft(0.), is_soft(false)
{
	parse_args(vecArgs);
	properties.stage = ProcessingProperties::both; //for the rest: default values
}

void FilterMin::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
                        std::vector<MeteoData>& ovec)
{
	ovec = ivec;
	for (size_t ii=0; ii<ovec.size(); ii++){
		double& tmp = ovec[ii](param);
		if (tmp == IOUtils::nodata) continue; //preserve nodata values

		if (tmp < min_val){
			if (is_soft){
				tmp = min_soft;
			} else {
				tmp = IOUtils::nodata;
			}
		}
	}
}

void FilterMin::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	const std::string where( "Filters::"+block_name );
	bool has_min=false, has_min_reset=false;

	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="SOFT") {
			IOUtils::parseArg(vecArgs[ii], where, is_soft);
		} else if (vecArgs[ii].first=="MIN") {
			IOUtils::parseArg(vecArgs[ii], where, min_val);
			has_min = true;
		} else if (vecArgs[ii].first=="MIN_RESET") {
			IOUtils::parseArg(vecArgs[ii], where, min_soft);
			has_min_reset = true;
		}
	}

	if (!has_min) throw InvalidArgumentException("Please provide a MIN value for "+where, AT);
	if (is_soft && !has_min_reset) min_soft = min_val;
}

} //end namespace
