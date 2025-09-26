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

#include <meteoio/spatialInterpolations/ODKrigLapseAlgorithm.h>
#include <meteoio/meteoStats/libinterpol2D.h>

namespace mio {

LapseOrdinaryKrigingAlgorithm::LapseOrdinaryKrigingAlgorithm(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm)
                                                      : OrdinaryKrigingAlgorithm(vecArgs, i_algo, i_param, i_tsm), trend(vecArgs, i_algo, i_param) {}

void LapseOrdinaryKrigingAlgorithm::calculate(const DEMObject& dem, Grid2DObject& grid)
{
	info.clear(); info.str("");
	//optimization: getRange (from variogram fit -> exclude stations that are at distances > range (-> smaller matrix)
	//or, get max range from io.ini, build variogram from this user defined max range
	trend.detrend(vecMeta, vecData);
	info << trend.getInfo();

	if (!computeVariogram(true)) //only refresh once a month, or once a week, etc
		throw IOException("The variogram for parameter " + param + " could not be computed!", AT);
	Interpol2D::ODKriging(vecData, vecMeta, dem, variogram, grid);

	trend.retrend(dem, grid);
}

} //namespace
