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

#include <meteoio/meteoResampling/SolarResampling.h>
#include <meteoio/meteoLaws/Sun.h>
#include <meteoio/meteoLaws/Atmosphere.h>
#include <meteoio/IOUtils.h>
#include <meteoio/meteoStats/libinterpol1D.h>

#include <sstream>

namespace mio {

Solar::Solar(const std::string& i_algoname, const std::string& i_parname, const double& dflt_window_size, const std::vector< std::pair<std::string, std::string> >& vecArgs)
            : ResamplingAlgorithms(i_algoname, i_parname, dflt_window_size, vecArgs), cache_losses(), extrapolate(false)
{
	const std::string where( "Interpolations1D::"+i_parname+"::"+i_algoname );
	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="WINDOW_SIZE") {
			IOUtils::parseArg(vecArgs[ii], where, window_size);
			window_size /= 86400.; //user uses seconds, internally julian day is used
			if (window_size<=0.) {
				std::ostringstream ss;
				ss << "Invalid accumulation period (" << window_size << ") for \"" << where << "\"";
				throw InvalidArgumentException(ss.str(), AT);
			}
		} else if (vecArgs[ii].first=="EXTRAPOLATE") {
			IOUtils::parseArg(vecArgs[ii], where, extrapolate);
		}
	}
}

std::string Solar::toString() const
{
	std::ostringstream ss;
	ss << std::right << std::setw(10) << parname << "::"  << std::left << std::setw(15) << algo << "[ ]";
	return ss.str();
}

double Solar::getPotentialH(const MeteoData& md)
{
	const double lat = md.meta.position.getLat();
	const double lon = md.meta.position.getLon();
	const double alt = md.meta.position.getAltitude();
	if (lat==IOUtils::nodata || lon==IOUtils::nodata || alt==IOUtils::nodata) return IOUtils::nodata;
	SunObject sun(lat, lon, alt);

	const double P = (md(MeteoData::P)!=IOUtils::nodata)? md(MeteoData::P) : Atmosphere::stdAirPressure(alt);
	const double HS = md(MeteoData::HS);
	double albedo = 0.5;
	if (HS!=IOUtils::nodata) //no big deal if we can not adapt the albedo
		albedo = (HS>=snow_thresh)? snow_albedo : soil_albedo;
	//if we don't have TA and RH, set them so the reduced precipitable water will get an average value
	double TA = md(MeteoData::TA);
	double RH = md(MeteoData::RH);
	if (TA==IOUtils::nodata || RH==IOUtils::nodata) {
		TA = 274.98;
		RH = 0.666;
	}

	sun.setDate(md.date.getJulian(), md.date.getTimeZone());
	sun.calculateRadiation(TA, RH, P, albedo);
	double toa, direct, diffuse;
	sun.getHorizontalRadiation(toa, direct, diffuse);
	const double global_h = direct+diffuse;

	return global_h;
}

bool Solar::computeLossFactor(const std::string& stationHash, const size_t& index, const size_t& paramindex,
                           const std::vector<MeteoData>& vecM, const Date& resampling_date, Points &pts)
{
	size_t indexP1=IOUtils::npos, indexP2=IOUtils::npos;
	getNearestValidPts(stationHash, index, paramindex, vecM, resampling_date, window_size, indexP1, indexP2);
	const bool foundP1=(indexP1!=IOUtils::npos), foundP2=(indexP2!=IOUtils::npos);

	if (!extrapolate && (!foundP1 || !foundP2)) return false;

	const double jul1 = vecM[indexP1].date.getJulian();
	if (jul1==pts.jul2) { //if the new start point is the old end point
		pts.jul1 = pts.jul2;
		pts.loss1 = pts.loss2;
	} else {
		const double pot1 = (foundP1)? getPotentialH( vecM[indexP1] ) : IOUtils::nodata;
		pts.loss1 = (pot1!=IOUtils::nodata && pot1>SunObject::rad_threshold)? vecM[indexP1](paramindex) / pot1 : IOUtils::nodata;
		pts.jul1 = jul1;
	}

	const double pot2 = (foundP2)? getPotentialH( vecM[indexP2] ) : IOUtils::nodata;
	pts.loss2 = (pot2!=IOUtils::nodata && pot2>SunObject::rad_threshold)? vecM[indexP2](paramindex) / pot2 : IOUtils::nodata;
	pts.jul2 = vecM[indexP2].date.getJulian();
	return true;
}

double Solar::interpolateLossFactor(const double& resampling_jul, const Points &pts)
{
	if (pts.loss1!=IOUtils::nodata && pts.loss2!=IOUtils::nodata) {
		const double weight = (resampling_jul - pts.jul1) / (pts.jul2 - pts.jul1);
		return Interpol1D::weightedMean(pts.loss1, pts.loss2, weight);
	} else if (pts.loss1==IOUtils::nodata && pts.loss2!=IOUtils::nodata){
		return pts.loss2;
	} else if (pts.loss2==IOUtils::nodata && pts.loss1!=IOUtils::nodata) {
		return pts.loss1;
	}

	return 1.;
}

void Solar::resample(const std::string& stationHash, const size_t& index, const ResamplingPosition& /*position*/, const size_t& paramindex,
                           const std::vector<MeteoData>& vecM, MeteoData& md)
{
	if (index >= vecM.size())
		throw IOException("The index of the element to be resampled is out of bounds", AT);

	const double pot_pt = getPotentialH( md );
	if (pot_pt==IOUtils::nodata) return;

	const double resampling_jul = md.date.getJulian();
	Points pts( cache_losses[ stationHash ] );
	if (pts.jul1==0. || (resampling_jul<pts.jul1 || resampling_jul>pts.jul2)) {
		const bool status = computeLossFactor(stationHash, index, paramindex, vecM, md.date, pts);
		if (!status) return;
		cache_losses[ stationHash ] = pts;
	}

	const double loss = interpolateLossFactor(resampling_jul, pts);
	md(paramindex) = loss * pot_pt;
	md.setResampled(true);
}

} //namespace
