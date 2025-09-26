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

#include <meteoio/spatialInterpolations/NoneAlgorithm.h>

namespace mio {

double NoneAlgorithm::getQualityRating(const Date& /*i_date*/)
{
	return 1e-6;
}

void NoneAlgorithm::calculate(const DEMObject& dem, Grid2DObject& grid)
{
	grid.set(dem.getNx(), dem.getNy(), dem.cellsize, dem.llcorner, IOUtils::nodata);
}

} //namespace
