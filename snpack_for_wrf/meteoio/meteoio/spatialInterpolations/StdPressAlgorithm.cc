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

#include <meteoio/spatialInterpolations/StdPressAlgorithm.h>
#include <meteoio/meteoStats/libinterpol2D.h>
#include <meteoio/meteoLaws/Atmosphere.h>

namespace mio {

StandardPressureAlgorithm::StandardPressureAlgorithm(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm)
                                                : InterpolationAlgorithm(vecArgs, i_algo, i_param, i_tsm), scale(1e3), alpha(1.), use_residuals(false)
{
	const std::string where( "Interpolations2D::"+i_param+"::"+i_algo );
	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="USE_RESIDUALS") {
			IOUtils::parseArg(vecArgs[ii], where, use_residuals);
		} else if (vecArgs[ii].first=="SCALE") {
			IOUtils::parseArg(vecArgs[ii], where, scale);
		} else if (vecArgs[ii].first=="ALPHA") {
			IOUtils::parseArg(vecArgs[ii], where, alpha);
		}
	}
}

double StandardPressureAlgorithm::getQualityRating(const Date& i_date)
{
	date = i_date;
	nrOfMeasurments = getData(date, param, vecData, vecMeta);

	if (nrOfMeasurments <=1 || use_residuals) return 1.0;
	return 0.1;
}

void StandardPressureAlgorithm::calculate(const DEMObject& dem, Grid2DObject& grid)
{
	Interpol2D::stdPressure(dem, grid);

	if (nrOfMeasurments==1 || !use_residuals) { //correct the locally measured offset to std pressure
		double offset = 0.;
		size_t count = 0;
		for (size_t ii=0; ii<nrOfMeasurments; ii++) {
			const double altitude = vecMeta[ii].position.getAltitude();
			if (altitude!=IOUtils::nodata) {
				offset += vecData[ii] - Atmosphere::stdAirPressure( altitude );
				count++;
			}
		}
		if (count>0) {
			offset /= static_cast<double>( count );
			grid += offset;
		}
	} else if (use_residuals && nrOfMeasurments>1) { //spatially distribute the residuals
		std::vector<double> residuals;
		for (size_t ii=0; ii<nrOfMeasurments; ii++) {
			const double altitude = vecMeta[ii].position.getAltitude();
			if (altitude!=IOUtils::nodata)
				residuals.push_back( vecData[ii] - Atmosphere::stdAirPressure( altitude ) );
		}
		if (residuals.empty())
			throw IOException("Not enough data for spatially interpolating parameter " + param, AT);

		Grid2DObject offset;
		Interpol2D::IDW(residuals, vecMeta, dem, offset, scale, alpha);
		grid += offset;
	}
}

} //namespace
