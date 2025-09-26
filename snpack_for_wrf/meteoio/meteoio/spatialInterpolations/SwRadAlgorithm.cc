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

#include <meteoio/spatialInterpolations/SwRadAlgorithm.h>
#include <meteoio/meteoStats/libinterpol2D.h>
#include <meteoio/dataClasses/DEMAlgorithms.h>

namespace mio {

const double SWRadInterpolation::soil_albedo = .23; //grass
const double SWRadInterpolation::snow_albedo = .85; //snow
const double SWRadInterpolation::snow_thresh = .1; //if snow height greater than this threshold -> snow albedo

SWRadInterpolation::SWRadInterpolation(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm,
                                                                       Meteo2DInterpolator& i_mi)
                                   : InterpolationAlgorithm(vecArgs, i_algo, i_param, i_tsm), mi(i_mi), Sun(), vecIdx(), scale(1e3), alpha(1.), shading(true), project_on_slope(false)
{
	const std::string where( "Interpolations2D::"+i_param+"::"+i_algo );
	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="SHADING") {
			IOUtils::parseArg(vecArgs[ii], where, shading);
		} else if (vecArgs[ii].first=="PROJECT_ON_SLOPE") {
			IOUtils::parseArg(vecArgs[ii], where, project_on_slope);
		} else if (vecArgs[ii].first=="SCALE") {
			IOUtils::parseArg(vecArgs[ii], where, scale);
		} else if (vecArgs[ii].first=="ALPHA") {
			IOUtils::parseArg(vecArgs[ii], where, alpha);
		}
	}
}

double SWRadInterpolation::getQualityRating(const Date& i_date)
{
	date = i_date;

	vecIdx.clear(); vecMeta.clear();
	tsmanager.getMeteoData(i_date, vecMeteo);

	//fill vecIdx with the indices of the stations that can be used and set the Sun coordinates to the middle of the stations
	double avg_lat = 0., avg_lon = 0., avg_alt = 0.;
	for(size_t ii=0; ii<vecMeteo.size(); ii++) {
		const Coords &location( vecMeteo[ii].meta.position );
		const bool has_meta = (location.getLat()!=IOUtils::nodata) && (location.getLon()!=IOUtils::nodata) && (location.getAltitude()!=IOUtils::nodata);
		const bool has_meteo = (vecMeteo[ii](MeteoData::ISWR)!=IOUtils::nodata)
		                                  && (vecMeteo[ii](MeteoData::TA)!=IOUtils::nodata)
		                                  && (vecMeteo[ii](MeteoData::RH)!=IOUtils::nodata);
		if (has_meta && has_meteo) {
			vecMeta.push_back( vecMeteo[ii].meta );
			avg_lat += location.getLat();
			avg_lon += location.getLon();
			avg_alt += location.getAltitude();
			vecIdx.push_back( ii );
		}
	}

	nrOfMeasurments = vecIdx.size();
	if (nrOfMeasurments == 0) return 0.0;

	avg_lat /= static_cast<double>(nrOfMeasurments);
	avg_lon /= static_cast<double>(nrOfMeasurments);
	avg_alt /= static_cast<double>(nrOfMeasurments);
	Sun.setLatLon(avg_lat, avg_lon, avg_alt); //set the sun for the average of the stations
	Sun.setDate(vecMeteo[ vecIdx[0] ].date.getJulian(true), vecMeteo[ vecIdx[0] ].date.getTimeZone()); //we have at least one station

	return 0.9;
}

void SWRadInterpolation::calculate(const DEMObject& dem, Grid2DObject& grid)
{
	info.clear(); info.str("");

	//compute the correction factors at every station
	std::vector<double> vecMd(nrOfMeasurments, IOUtils::nodata);
	std::vector<double> vecCorr(nrOfMeasurments, IOUtils::nodata);
	bool glob_day=true, glob_night=true;
	for (size_t ii=0; ii<nrOfMeasurments; ii++) {
		const size_t idx = vecIdx[ii];
		const double HS = vecMeteo[idx](MeteoData::HS);
		double albedo = 0.5;
		if (HS!=IOUtils::nodata) //no big deal if we can not adapt the albedo
			albedo = (HS>=snow_thresh)? snow_albedo : soil_albedo;
		Sun.calculateRadiation(vecMeteo[idx](mio::MeteoData::TA), vecMeteo[idx](mio::MeteoData::RH), vecMeteo[idx](mio::MeteoData::P), albedo);

		bool day, night;
		vecCorr[ii] = Sun.getCorrectionFactor(vecMeteo[idx](mio::MeteoData::ISWR), vecMd[ii], day, night);
		glob_day = glob_day && day;
		glob_night = glob_night && night;
	}

	if (glob_night) {
		Interpol2D::constant(0., dem, grid); //no iswr at night
		return;
	}

	//compute the distributed  splitting and correction coefficient fields
	Grid2DObject Md;
	Interpol2D::IDW(vecMd, vecMeta, dem, Md, scale, alpha);
	Grid2DObject Corr;
	Interpol2D::IDW(vecCorr, vecMeta, dem, Corr, scale, alpha);

	//get TA, RH and P interpolation from call back to Meteo2DInterpolator
	Grid2DObject ta;
	mi.interpolate(date, dem, MeteoData::TA, ta);
	Grid2DObject rh;
	mi.interpolate(date, dem, MeteoData::RH, rh);
	Grid2DObject p;
	mi.interpolate(date, dem, MeteoData::P, p);

	//fill the final results with the proper radiation (with shading)
	double solarAzimuth, solarElevation;
	Sun.position.getHorizontalCoordinates(solarAzimuth, solarElevation);
	const double tan_sun_elev = tan(solarElevation*Cst::to_rad);

	grid.set(dem, IOUtils::nodata);
	for (size_t jj=0; jj<dem.getNy(); jj++) {
		for (size_t ii=0; ii<dem.getNx(); ii++) {
			if (dem(ii,jj)==IOUtils::nodata) continue;

			Sun.resetAltitude( dem(ii,jj) );
			Sun.calculateRadiation(ta(ii,jj), rh(ii,jj), p(ii,jj), .5); //we don't have any albedo, so use .5
			double cell_toa, cell_direct, cell_diffuse;
			Sun.getHorizontalRadiation(cell_toa, cell_direct, cell_diffuse);

			if (glob_day && shading) { //at dawn/dusk, we consider it to be all diffuse, so no shading
				const double tan_horizon = DEMAlgorithms::getHorizon(dem, ii, jj, solarAzimuth);

				//redo the splitting using the distributed splitting coefficient
				const double global = cell_direct + cell_diffuse;
				cell_direct = global * (1. - Md(ii,jj));
				cell_diffuse = global * Md(ii,jj);

				if ( tan_sun_elev<tan_horizon ) cell_direct = 0.;//cell is shaded
			}
			if (project_on_slope && glob_day && cell_direct>0.) {
				cell_direct = SunTrajectory::projectHorizontalToSlope( solarAzimuth, solarElevation, dem.azi(ii,jj), dem.slope(ii,jj), cell_direct );
			}
			grid(ii,jj) = Corr(ii,jj) * (cell_direct+cell_diffuse);
		}
	}
}

} //namespace
