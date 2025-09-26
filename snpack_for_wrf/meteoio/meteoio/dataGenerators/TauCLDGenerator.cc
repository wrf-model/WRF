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

#include <meteoio/dataGenerators/TauCLDGenerator.h>
#include <meteoio/meteoLaws/Atmosphere.h>
#include <algorithm>

namespace mio {

TauCLDGenerator::TauCLDGenerator(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo)
                              : GeneratorAlgorithm(vecArgs, i_algo), last_cloudiness(), use_rswr(false)
{
	const std::string where( "generators::"+algo );
	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="USE_RSWR") {
			IOUtils::parseArg(vecArgs[ii], where, use_rswr);
		}
	}
}

/**
 * @brief Compute the atmospheric cloudiness from the available measurements
 * @details
 * The clearness index (ie the ratio of the incoming short wave radiation over the ground potential radiation, projected on the horizontal) is computed and
 * used to evaluate the cloudiness, based on the chosen parametrization.
 * @param[in] clf_model cloudiness parametrization
 * @param[in] md MeteoData
 * @param[in] i_use_rswr if set to true, in case of no iswr measurements, a ground albedo is assumed and used to compute iswr. Based on HS, this albedo can either
 * be a soil ro a snow albedo
 * @param sun For better efficiency, the SunObject for this location (so it can be cached)
 * @param[out] is_night set to TRUE if it is night time
 * @return cloudiness (between 0 and 1)
 */
double TauCLDGenerator::getCloudiness(const clf_parametrization& clf_model, const MeteoData& md, const bool& i_use_rswr, SunObject& sun, bool &is_night)
{
	static const double night_thresh = 5.; //threshold on iswr for night or day
	//we know that TA and RH are available, otherwise we would not get called
	const double TA=md(MeteoData::TA), RH=md(MeteoData::RH), HS=md(MeteoData::HS), RSWR=md(MeteoData::RSWR);
	double ISWR=md(MeteoData::ISWR);

	is_night = false;

	double albedo = .5;
	if (RSWR!=IOUtils::nodata && ISWR!=IOUtils::nodata) {
		if (ISWR<night_thresh) {
			is_night = true;
			return IOUtils::nodata;
		}
		albedo = std::min( 0.99, std::max(0.01, RSWR / ISWR) );
	} else { //so some measurements are missing
		if (HS!=IOUtils::nodata) //no big deal if we can not adapt the albedo
			albedo = (HS>=snow_thresh)? snow_albedo : soil_albedo;

		if (ISWR==IOUtils::nodata) { //ISWR is missing, trying to compute it
			if (RSWR!=IOUtils::nodata)
				is_night = (RSWR / albedo) < night_thresh; //in any case, we use RSWR, at least to know if it's night
			if (!i_use_rswr)
				return IOUtils::nodata;
			if (RSWR!=IOUtils::nodata && HS!=IOUtils::nodata)
				ISWR = RSWR / albedo;
			else
				return IOUtils::nodata; //no way to get ISWR, aborting
		}
	}

	if (ISWR<night_thresh) {
		is_night = true;
		return IOUtils::nodata;
	}

	sun.calculateRadiation(TA, RH, albedo);
	double toa, direct, diffuse;
	sun.getHorizontalRadiation(toa, direct, diffuse);
	const double iswr_clear_sky = direct+diffuse;

	//at sunrise or sunset, we might get clf<0 or clf>1 -> return nodata in order to use interpolation instead
	if (iswr_clear_sky<night_thresh || iswr_clear_sky<ISWR) {
		is_night = true;
		return IOUtils::nodata;
	}

	if (clf_model==KASTEN) {
		const double clf = Atmosphere::Kasten_cloudiness(ISWR/iswr_clear_sky);
		if (clf<0. || clf>1.) return IOUtils::nodata;
		return clf;
	} else if (clf_model==CLF_CRAWFORD) {
		const double clf = 1. - ISWR/iswr_clear_sky;
		if (clf<0. || clf>1.) return IOUtils::nodata;
		return clf;
	} else
		return IOUtils::nodata; //this should never happen
}

bool TauCLDGenerator::generate(const size_t& param, MeteoData& md)
{
	double &value = md(param);
	if (value == IOUtils::nodata) {
		double cld = (md.param_exists("CLD"))? md("CLD") : IOUtils::nodata;
		if (cld!=IOUtils::nodata) {
			if (cld>8. || cld<0.) throw InvalidArgumentException("Cloud cover CLD should be between 0 and 8!", AT);
			if (cld==9) cld=8.; //Synop sky obstructed from view -> fully cloudy
			value = Atmosphere::Kasten_clearness( cld/8. );
			return true;
		}

		const double TA=md(MeteoData::TA), RH=md(MeteoData::RH);
		if (TA==IOUtils::nodata || RH==IOUtils::nodata) return false;

		const std::string station_hash = md.meta.stationID + ":" + md.meta.stationName;
		const double julian_gmt = md.date.getJulian(true);
		bool cloudiness_from_cache = false;

		const double lat = md.meta.position.getLat();
		const double lon = md.meta.position.getLon();
		const double alt = md.meta.position.getAltitude();
		if (lat==IOUtils::nodata || lon==IOUtils::nodata || alt==IOUtils::nodata) return false;
		SunObject sun;
		sun.setLatLon(lat, lon, alt);
		sun.setDate(julian_gmt, 0.);

		bool is_night;
		double cloudiness = TauCLDGenerator::getCloudiness(KASTEN, md, use_rswr, sun, is_night);
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

		//save the last valid cloudiness
		if (!cloudiness_from_cache)
			last_cloudiness[station_hash] = std::pair<double,double>( julian_gmt, cloudiness );

		value = 1.- cloudiness;
	}

	return true; //all missing values could be filled
}

bool TauCLDGenerator::create(const size_t& param, std::vector<MeteoData>& vecMeteo)
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
