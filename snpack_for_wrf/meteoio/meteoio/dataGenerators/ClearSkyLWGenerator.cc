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

#include <meteoio/dataGenerators/ClearSkyLWGenerator.h>
#include <meteoio/meteoLaws/Atmosphere.h>

namespace mio {

void ClearSkyLWGenerator::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	const std::string where( "generators::"+algo );
	bool has_type=false;

	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="TYPE") {
			const std::string user_algo( IOUtils::strToUpper(vecArgs[ii].second) );

			if (user_algo=="BRUTSAERT") model = BRUTSAERT;
			else if (user_algo=="DILLEY") model = DILLEY;
			else if (user_algo=="PRATA") model = PRATA;
			else if (user_algo=="CLARK") model = CLARK;
			else if (user_algo=="TANG") model = TANG;
			else if (user_algo=="IDSO") model = IDSO;
			else
				throw InvalidArgumentException("Unknown parametrization \""+user_algo+"\" supplied for "+where, AT);

			has_type = true;
		}
	}

	if (!has_type) throw InvalidArgumentException("Please provide a TYPE for "+where, AT);
}

bool ClearSkyLWGenerator::generate(const size_t& param, MeteoData& md)
{
	double &value = md(param);
	if (value==IOUtils::nodata) {
		const double TA=md(MeteoData::TA), RH=md(MeteoData::RH);
		if (TA==IOUtils::nodata || RH==IOUtils::nodata) return false;

		if (model==BRUTSAERT)
			value = Atmosphere::Brutsaert_ilwr(RH, TA);
		else if (model==DILLEY)
			value = Atmosphere::Dilley_ilwr(RH, TA);
		else if (model==PRATA)
			value = Atmosphere::Prata_ilwr(RH, TA);
		else if (model==CLARK)
			value = Atmosphere::Clark_ilwr(RH, TA);
		else if (model==TANG)
			value = Atmosphere::Tang_ilwr(RH, TA);
		else if (model==IDSO)
			value = Atmosphere::Idso_ilwr(RH, TA);
	}

	return true; //all missing values could be filled
}

bool ClearSkyLWGenerator::create(const size_t& param, std::vector<MeteoData>& vecMeteo)
{
	if (vecMeteo.empty()) return true;

	bool all_filled = true;
	for (size_t ii=0; ii<vecMeteo.size(); ii++) {
		const bool status = generate(param, vecMeteo[ii]);
		if (status==false) all_filled=false;
	}

	return all_filled;
}

} //namespace
