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

#include <meteoio/dataGenerators/AllSkyLWGenerator.h>
#include <meteoio/meteoLaws/Atmosphere.h>
#include <meteoio/meteoLaws/Sun.h>

#include <string>
#include <utility>

namespace mio {

void AllSkyLWGenerator::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	const std::string where( "generators::"+algo );
	bool has_type=false;

	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="TYPE") {
			const std::string user_algo( IOUtils::strToUpper(vecArgs[ii].second) );

			if (user_algo=="CARMONA") model = CARMONA;
			else if (user_algo=="OMSTEDT") model = OMSTEDT;
			else if (user_algo=="KONZELMANN") model = KONZELMANN;
			else if (user_algo=="UNSWORTH") model = UNSWORTH;
			else if (user_algo=="CRAWFORD") {
				model = CRAWFORD;
				clf_model = TauCLDGenerator::CLF_CRAWFORD;
			} else
				throw InvalidArgumentException("Unknown parametrization \""+user_algo+"\" supplied for "+where, AT);

			has_type = true;
		}
		if (vecArgs[ii].first=="USE_RSWR") {
			IOUtils::parseArg(vecArgs[ii], where, use_rswr);
		}
	}

	if (!has_type) throw InvalidArgumentException("Please provide a TYPE for "+where, AT);
}

bool AllSkyLWGenerator::generate(const size_t& param, MeteoData& md)
{
	double &value = md(param);
	if (value==IOUtils::nodata) {
		const double TA=md(MeteoData::TA), RH=md(MeteoData::RH), TAU_CLD=md(MeteoData::TAU_CLD);
		if (TA==IOUtils::nodata || RH==IOUtils::nodata) return false;
		double cloudiness = (TAU_CLD!=IOUtils::nodata)? Atmosphere::Kasten_cloudiness( TAU_CLD ) : IOUtils::nodata;

		const std::string station_hash( md.meta.stationID + ":" + md.meta.stationName );
		const double julian_gmt = md.date.getJulian(true);
		bool cloudiness_from_cache = false;

		//try to get a cloudiness value
		if (cloudiness==IOUtils::nodata) {
			const double lat = md.meta.position.getLat();
			const double lon = md.meta.position.getLon();
			const double alt = md.meta.position.getAltitude();
			sun.setLatLon(lat, lon, alt);
			sun.setDate(julian_gmt, 0.);

			bool is_night;
			cloudiness = TauCLDGenerator::getCloudiness(clf_model, md, use_rswr, sun, is_night);
			if (cloudiness==IOUtils::nodata && !is_night) return false;

			if (is_night) { //interpolate the cloudiness over the night
				const std::map< std::string, std::pair<double, double> >::const_iterator it = last_cloudiness.find(station_hash);
				if (it==last_cloudiness.end()) return false;

				cloudiness_from_cache = true;
				const double last_cloudiness_julian = it->second.first;
				const double last_cloudiness_value = it->second.second;
				if ((julian_gmt - last_cloudiness_julian) < 1.) cloudiness = last_cloudiness_value;
				else return false;
			}
		}

		//save the last valid cloudiness
		if (!cloudiness_from_cache)
			last_cloudiness[station_hash] = std::pair<double,double>( julian_gmt, cloudiness );

		//run the ILWR parametrization
		if (model==CARMONA)
			value = Atmosphere::Carmona_ilwr(RH, TA, cloudiness);
		else if (model==OMSTEDT)
			value = Atmosphere::Omstedt_ilwr(RH, TA, cloudiness);
		else if (model==KONZELMANN)
			value = Atmosphere::Konzelmann_ilwr(RH, TA, cloudiness);
		else if (model==UNSWORTH)
			value = Atmosphere::Unsworth_ilwr(RH, TA, IOUtils::nodata, IOUtils::nodata, cloudiness);
		else if (model==CRAWFORD) {
			int year, month, day;
			md.date.getDate(year, month, day);
			value = Atmosphere::Crawford_ilwr(RH, TA, IOUtils::nodata, IOUtils::nodata, static_cast<unsigned char>(month), cloudiness);
		}
	}

	return true; //all missing values could be filled
}

bool AllSkyLWGenerator::create(const size_t& param, std::vector<MeteoData>& vecMeteo)
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
