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

#include <meteoio/spatialInterpolations/AvgAlgorithm.h>
#include <meteoio/meteoStats/libinterpol1D.h>
#include <meteoio/meteoStats/libinterpol2D.h>

namespace mio {

double AvgAlgorithm::getQualityRating(const Date& i_date)
{
	date = i_date;
	nrOfMeasurments = getData(date, param, vecData, vecMeta);

	if (nrOfMeasurments == 1) {
		return 0.8;
	} else if (nrOfMeasurments > 1) {
		return 0.2;
	}

	return 0.0;
}

void AvgAlgorithm::calculate(const DEMObject& dem, Grid2DObject& grid)
{
	Interpol2D::constant(Interpol1D::arithmeticMean(vecData), dem, grid);
}

} //namespace
