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

#include <meteoio/dataGenerators/RadiationComponents.h>
#include <meteoio/MathOptim.h>

namespace mio {

void RadiationComponents::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	if (!vecArgs.empty()) throw InvalidArgumentException("The "+where+" generator does not take any arguments", AT);
}

bool RadiationComponents::generate(const size_t& param, MeteoData& md)
{
	if (param!=MeteoData::ISWR)
		throw InvalidArgumentException("The "+where+" generator can only be applied to ISWR_DIR and ISWR_DIFF", AT);
	
	double &value = md(param);
	if (value == IOUtils::nodata) {
		std::string iswr_dir;
		if (md.param_exists("ISWR_DIR")) iswr_dir = "ISWR_DIR";
		
		std::string iswr_diff;
		if (md.param_exists("ISWR_DIFF")) iswr_diff = "ISWR_DIFF";
		
		if (iswr_dir.empty() || iswr_diff.empty())
			throw NotFoundException("No ISWR_DIR and/or ISWR_DIFF radiation components found", AT);
		
		const double dir = md(iswr_dir);
		const double diff = md(iswr_diff);
		if (dir==IOUtils::nodata || diff==IOUtils::nodata) return false;
		
		value = dir + diff;
	}

	return true;
}

bool RadiationComponents::create(const size_t& param, std::vector<MeteoData>& vecMeteo)
{
	if (param!=MeteoData::ISWR)
		throw InvalidArgumentException("The "+where+" generator can only be applied to ISWR_DIR and/or ISWR_DIFF", AT);
	
	if (vecMeteo.empty()) return true;

	std::string iswr_dir;
	if (vecMeteo[0].param_exists("ISWR_DIR")) iswr_dir = "ISWR_DIR";
	
	std::string iswr_diff;
	if (vecMeteo[0].param_exists("ISWR_DIFF")) iswr_diff = "ISWR_DIFF";
	
	if (iswr_dir.empty() || iswr_diff.empty())
		throw NotFoundException("No ISWR_DIR and/or ISWR_DIFF radiation components found", AT);
	
	bool all_filled = true;
	for (size_t ii=0; ii<vecMeteo.size(); ii++) {
		if (vecMeteo[ii](param)!=IOUtils::nodata) continue;
		
		const double dir = vecMeteo[ii](iswr_dir);
		const double diff = vecMeteo[ii](iswr_diff);
		if (dir==IOUtils::nodata || diff==IOUtils::nodata) {
			all_filled = false;
			continue;
		}
		
		vecMeteo[ii](param) = dir + diff;
	}
	
	return all_filled;
}

} //namespace
