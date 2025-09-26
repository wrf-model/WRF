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

#include <meteoio/spatialInterpolations/IDWAlgorithm.h>
#include <meteoio/meteoStats/libinterpol2D.h>

namespace mio {

IDWAlgorithm::IDWAlgorithm(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm)
                         : InterpolationAlgorithm(vecArgs, i_algo, i_param, i_tsm), scale(1e3), alpha(1.)
{
	const std::string where( "Interpolations2D::"+i_param+"::"+i_algo );
	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="SCALE") {
			IOUtils::parseArg(vecArgs[ii], where, scale);
		} else if (vecArgs[ii].first=="ALPHA") {
			IOUtils::parseArg(vecArgs[ii], where, alpha);
		}
	}
}

double IDWAlgorithm::getQualityRating(const Date& i_date)
{
	date = i_date;
	nrOfMeasurments = getData(date, param, vecData, vecMeta);

	if (nrOfMeasurments == 0){
		return 0.0;
	} else if (nrOfMeasurments == 1){
		return 0.3;
	} else if (nrOfMeasurments > 1){
		return 0.5;
	}

	return 0.2;
}

void IDWAlgorithm::calculate(const DEMObject& dem, Grid2DObject& grid)
{
	Interpol2D::IDW(vecData, vecMeta, dem, grid, scale, alpha);
}

} //namespace
