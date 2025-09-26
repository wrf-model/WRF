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

#include <meteoio/dataGenerators/WindComponents.h>
#include <meteoio/MathOptim.h>

namespace mio {

void WindComponents::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	if (!vecArgs.empty()) throw InvalidArgumentException("The "+where+" generator does not take any arguments", AT);
}

std::string WindComponents::findUComponent(const MeteoData& md)
{
	if (md.param_exists("U")) return "U";
	else if (md.param_exists("VW_U")) return "VW_U";
	else if (md.param_exists("WIND_U")) return "WIND_U";
	
	return std::string();
}

std::string WindComponents::findVComponent(const MeteoData& md)
{
	if (md.param_exists("V")) return "V";
	else if (md.param_exists("VW_V")) return "VW_V";
	else if (md.param_exists("WIND_V")) return "WIND_V";
	
	return std::string();
}


bool WindComponents::generate(const size_t& param, MeteoData& md)
{
	if (param!=MeteoData::VW && param!=MeteoData::DW)
		throw InvalidArgumentException("The "+where+" generator can only be applied to VW and/or DW", AT);
	
	double &value = md(param);
	if (value == IOUtils::nodata) {
		const std::string U_param( findUComponent(md) );
		const std::string V_param( findVComponent(md) );
		
		if (U_param.empty() || V_param.empty())
			return false;
		
		const double u = md(U_param);
		const double v = md(V_param);
		if (u==IOUtils::nodata || v==IOUtils::nodata) return false;
		
		if (param==MeteoData::VW)
			value = Optim::fastSqrt_Q3( u*u + v*v );
		else
			value = fmod(atan2(u, v)*Cst::to_deg + 360., 360.);
	}

	return true;
}

bool WindComponents::create(const size_t& param, std::vector<MeteoData>& vecMeteo)
{
	if (param!=MeteoData::VW && param!=MeteoData::DW)
		throw InvalidArgumentException("The "+where+" generator can only be applied to VW and/or DW", AT);
	
	if (vecMeteo.empty()) return true;

	const std::string U_param( findUComponent(vecMeteo[0]) );
	const std::string V_param( findVComponent(vecMeteo[0]) );
	
	if (U_param.empty() || V_param.empty())
		return false;
	
	bool all_filled = true;
	for (size_t ii=0; ii<vecMeteo.size(); ii++) {
		if (vecMeteo[ii](param)!=IOUtils::nodata) continue;
		
		const double u = vecMeteo[ii](U_param);
		const double v = vecMeteo[ii](V_param);
		if (u==IOUtils::nodata || v==IOUtils::nodata) {
			all_filled = false;
			continue;
		}
		
		if (param==MeteoData::VW)
			vecMeteo[ii](param) = Optim::fastSqrt_Q3( u*u + v*v );
		else
			vecMeteo[ii](param) = 270. - fmod(atan2(v, u)*Cst::to_deg, 360.);
	}
	
	return all_filled;
}

} //namespace
