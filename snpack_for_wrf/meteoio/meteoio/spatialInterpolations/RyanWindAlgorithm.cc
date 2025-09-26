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

#include <meteoio/spatialInterpolations/RyanWindAlgorithm.h>
#include <meteoio/meteoStats/libinterpol2D.h>

namespace mio {

double RyanAlgorithm::getQualityRating(const Date& i_date)
{
	date = i_date;
	vecMeta.clear();
	vecDataVW.clear(); vecDataDW.clear();

	tsmanager.getMeteoData(date, vecMeteo);
	if (!vecMeteo.empty()) param_idx = vecMeteo[0].getParameterIndex( param );
	for (size_t ii=0; ii<vecMeteo.size(); ii++){
		if ((vecMeteo[ii](MeteoData::VW) != IOUtils::nodata) && (vecMeteo[ii](MeteoData::DW) != IOUtils::nodata)){
			vecDataVW.push_back(vecMeteo[ii](MeteoData::VW));
			vecDataDW.push_back(vecMeteo[ii](MeteoData::DW));
			vecMeta.push_back(vecMeteo[ii].meta);
		}
	}

	nrOfMeasurments = vecMeta.size();

	if (nrOfMeasurments==0) return 0.0;

	if (Interpol2D::allZeroes(vecDataVW)) {
		inputIsAllZeroes = true;
		return 0.9;
	}

	if (nrOfMeasurments<2) return 0.6;

	return 0.9;
}

void RyanAlgorithm::calculate(const DEMObject& dem, Grid2DObject& grid)
{
	info.clear(); info.str("");

	//if all data points are zero, simply fill the grid with zeroes
	if (inputIsAllZeroes) {
		Interpol2D::constant(0., dem, grid);
		return;
	}

	if (param_idx==MeteoData::VW) {
		Grid2DObject DW;
		simpleWindInterpolate(dem, grid, DW);
		Interpol2D::RyanWind(dem, grid, DW);
	}
	if (param_idx==MeteoData::DW) {
		Grid2DObject VW;
		simpleWindInterpolate(dem, VW, grid);
		Interpol2D::RyanWind(dem, VW, grid);
	}
}

} //namespace
