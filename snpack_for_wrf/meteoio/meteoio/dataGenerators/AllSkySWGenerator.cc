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

#include <meteoio/dataGenerators/AllSkySWGenerator.h>
#include <meteoio/meteoLaws/Atmosphere.h>
#include <algorithm>

namespace mio {

bool AllSkySWGenerator::generate(const size_t& param, MeteoData& md)
{
	double &value = md(param);
	if (value == IOUtils::nodata) {
		const double ISWR=md(MeteoData::ISWR), RSWR=md(MeteoData::RSWR), HS=md(MeteoData::HS), TAU_CLD=md(MeteoData::TAU_CLD);
		double TA=md(MeteoData::TA), RH=md(MeteoData::RH), ILWR=md(MeteoData::ILWR);

		const double lat = md.meta.position.getLat();
		const double lon = md.meta.position.getLon();
		const double alt = md.meta.position.getAltitude();
		if (lat==IOUtils::nodata || lon==IOUtils::nodata || alt==IOUtils::nodata) return false;

		double albedo = .5;
		if (RSWR==IOUtils::nodata || ISWR==IOUtils::nodata) {
			if (HS!=IOUtils::nodata) //no big deal if we can not adapt the albedo
				albedo = (HS>=snow_thresh)? snow_albedo : soil_albedo;
		} else if (ISWR>0. && RSWR>0.) { //this could happen if the user calls this generator for a copy parameter, etc
			albedo = std::max(0.01, std::min(0.99, RSWR / ISWR));
		}

		if (TA==IOUtils::nodata || RH==IOUtils::nodata) {
			//set TA & RH so the reduced precipitable water will get an average value
			TA=274.98;
			RH=0.666;
			ILWR=IOUtils::nodata; //skip solarIndex correction
		}

		sun.setLatLon(lat, lon, alt);
		sun.setDate(md.date.getJulian(true), 0.);
		const double solarIndex = (TAU_CLD!=IOUtils::nodata)? TAU_CLD : (ILWR!=IOUtils::nodata)? getSolarIndex(TA, RH, ILWR) : 1.;

		const double P=md(MeteoData::P);
		if (P==IOUtils::nodata)
			sun.calculateRadiation(TA, RH, albedo);
		else
			sun.calculateRadiation(TA, RH, P, albedo);

		double toa, direct, diffuse;
		sun.getHorizontalRadiation(toa, direct, diffuse);
		if (param!=MeteoData::RSWR)
			value = (direct+diffuse)*solarIndex; //ISWR
		else
			value = (direct+diffuse)*albedo*solarIndex; //RSWR
	}

	return true; //all missing values could be filled
}

bool AllSkySWGenerator::create(const size_t& param, std::vector<MeteoData>& vecMeteo)
{
	if (vecMeteo.empty()) return true;

	bool all_filled = true;
	for (size_t ii=0; ii<vecMeteo.size(); ii++) {
		const bool status = generate(param, vecMeteo[ii]);
		if (status==false) all_filled=false;
	}

	return all_filled;
}

double AllSkySWGenerator::getSolarIndex(const double& ta, const double& rh, const double& ilwr)
{// this is based on Karsten cloudiness, Dilley clear sky emissivity and Unsworth ILWR
//this means that this solar index is the ratio of iswr for clear sky on a horizontal
//surface and the measured iswr
	const double epsilon_clear = Atmosphere::Dilley_emissivity(rh, ta);
	const double ilwr_clear = Atmosphere::blkBody_Radiation(1., ta);

	double cloudiness = (ilwr/ilwr_clear - epsilon_clear) / (.84 * (1.-epsilon_clear));
	if (cloudiness>1.) cloudiness=1.;
	if (cloudiness<0.) cloudiness=0.;

	static const double b1 = 0.75, b2 = 3.4;
	const double karsten_Si = 1. - (b1 * pow(cloudiness, b2));
	return karsten_Si;
}

} //namespace
