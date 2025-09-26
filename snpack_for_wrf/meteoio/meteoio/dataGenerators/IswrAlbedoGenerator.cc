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

#include <meteoio/dataGenerators/IswrAlbedoGenerator.h>
#include <meteoio/meteoLaws/Sun.h>

namespace mio {

void IswrAlbedoGenerator::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	const std::string where( "generators::"+algo );
	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="FORCE") {
			IOUtils::parseArg(vecArgs[ii], where, force);
		}
	}
}

bool IswrAlbedoGenerator::generate(const size_t& param, MeteoData& md)
{
	double &value = md(param);
	if (value == IOUtils::nodata) {
		const double HS=md(MeteoData::HS), RSWR=md(MeteoData::RSWR), ISWR=md(MeteoData::ISWR);

		if (param==MeteoData::ISWR && (RSWR!=IOUtils::nodata && RSWR<=SunObject::rad_threshold)) {
			value = RSWR;
			return true;
		}
		if (param==MeteoData::RSWR && (ISWR!=IOUtils::nodata && ISWR<=SunObject::rad_threshold)) {
			value = ISWR;
			return true;
		}

		double albedo = .5;
		if (HS!=IOUtils::nodata)
			albedo = (HS>=snow_thresh)? snow_albedo : soil_albedo;
		else if (!force) return false;

		if (param==MeteoData::ISWR && RSWR!=IOUtils::nodata) {
			value = RSWR / albedo;
			return true;
		}

		if (param==MeteoData::RSWR && ISWR!=IOUtils::nodata) {
			value = ISWR * albedo;
			return true;
		}

		return false;
	}

	return true; //all missing values could be filled
}

bool IswrAlbedoGenerator::create(const size_t& param, std::vector<MeteoData>& vecMeteo)
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
