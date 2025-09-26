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

#include <meteoio/spatialInterpolations/PPhaseAlgorithm.h>

namespace mio {
PPHASEInterpolation::PPHASEInterpolation(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm,
                                                                           Meteo2DInterpolator& i_mi)
                                      : InterpolationAlgorithm(vecArgs, i_algo, i_param, i_tsm), mi(i_mi),
                                      model(THRESH), fixed_thresh(IOUtils::nodata), range_start(IOUtils::nodata), range_norm(IOUtils::nodata)
{
	const std::string where( "Interpolations2D::"+i_param+"::"+i_algo );
	bool has_type=false, has_snow=false, has_rain=false;
	double snow_thresh=273.15, rain_thresh=273.15; //to silence a warning

	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="TYPE") {
			const std::string user_algo( IOUtils::strToUpper(vecArgs[ii].second) );

			if (user_algo=="THRESH") model = THRESH;
			else if (user_algo=="RANGE") model = RANGE;
			else
				throw InvalidArgumentException("Unknown algorithm \""+user_algo+"\" supplied for "+where, AT);

			has_type = true;
		} else if(vecArgs[ii].first=="SNOW") {
			IOUtils::parseArg(vecArgs[ii], where, snow_thresh);
			has_snow = true;
		} else if(vecArgs[ii].first=="RAIN") {
			IOUtils::parseArg(vecArgs[ii], where, rain_thresh);
			has_rain = true;
		}
	}

	if (!has_type) throw InvalidArgumentException("Please provide a TYPE for "+where, AT);
	if (model == THRESH) {
		if (!has_snow) throw InvalidArgumentException("Please provide a snow/rain threshold for "+where, AT);
		fixed_thresh = snow_thresh;
	}
	if (model == RANGE) {
		if (!has_snow || !has_rain) throw InvalidArgumentException("Please provide a a snow and a rain threshold for "+where, AT);
		if (snow_thresh==rain_thresh) throw InvalidArgumentException(where+" : the two provided threshold must be different", AT);
		if (snow_thresh>rain_thresh) std::swap(snow_thresh, rain_thresh);
		range_start = snow_thresh;
		range_norm = 1. / (rain_thresh-snow_thresh);
	}
}

double PPHASEInterpolation::getQualityRating(const Date& i_date)
{
	date = i_date;
	return 0.1;
}

void PPHASEInterpolation::calculate(const DEMObject& dem, Grid2DObject& grid)
{
	info.clear(); info.str("");

	Grid2DObject ta;
	mi.interpolate(date, dem, MeteoData::TA, ta); //get TA interpolation from call back to Meteo2DInterpolator

	grid.set(dem, IOUtils::nodata);

	if (model==THRESH) {
		for (size_t ii=0; ii<dem.size(); ii++) {
			const double TA=ta(ii);
			if (TA==IOUtils::nodata) continue;
			grid(ii) = (TA>=fixed_thresh)? 1. : 0.;
		}
	} else if (model==RANGE) {
		for (size_t ii=0; ii<dem.size(); ii++) {
			const double TA=ta(ii);
			if (TA==IOUtils::nodata) continue;
			const double tmp_rainfraction = range_norm * (TA - range_start);
			grid(ii) = (tmp_rainfraction>1)? 1. : (tmp_rainfraction<0.)? 0. : tmp_rainfraction;
		}
	}
}

} //namespace
