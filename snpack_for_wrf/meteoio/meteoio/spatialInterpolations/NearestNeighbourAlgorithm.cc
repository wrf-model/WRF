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

#include <meteoio/spatialInterpolations/NearestNeighbourAlgorithm.h>
#include <meteoio/meteoLaws/Meteoconst.h>

namespace mio {

double NearestNeighbourAlgorithm::getQualityRating(const Date& i_date)
{
	date = i_date;
	nrOfMeasurments = getData(date, param, vecData, vecMeta);

	if (nrOfMeasurments == 0) return 0.0;

	return 0.99;
}

void NearestNeighbourAlgorithm::calculate(const DEMObject& dem, Grid2DObject& grid)
{
	info.clear(); info.str("");
	grid.set(dem, IOUtils::nodata);

	for (size_t jj=0; jj<grid.getNy(); jj++) {
		for (size_t ii=0; ii<grid.getNx(); ii++) {
			if (dem(ii,jj)==IOUtils::nodata) continue;
			const double x = grid.llcorner.getEasting() + static_cast<double>(ii)*grid.cellsize;
			const double y = grid.llcorner.getNorthing() + static_cast<double>(jj)*grid.cellsize;
			grid(ii,jj) = vecData[ getNeighbors(x, y) ];
		}
	}
}

size_t NearestNeighbourAlgorithm::getNeighbors(const double& x, const double& y) const
{
	size_t closest = 0;
	double dist = Cst::dbl_max;

	for (size_t ii=0; ii<vecMeta.size(); ii++) {
		const double DX = x - vecMeta[ii].position.getEasting();
		const double DY = y - vecMeta[ii].position.getNorthing();
		const double d2 = (DX*DX + DY*DY);
		if (d2<=dist) {
			closest = ii;
			dist = d2;
		}
	}

	return closest;
}

} //namespace
