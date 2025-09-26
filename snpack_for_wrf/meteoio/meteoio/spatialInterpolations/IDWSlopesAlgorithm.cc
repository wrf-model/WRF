/***********************************************************************************/
/*  Copyright 2018 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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

#include <meteoio/spatialInterpolations/IDWSlopesAlgorithm.h>
#include <meteoio/meteoStats/libinterpol2D.h>

namespace mio {
const double IDWSlopesAlgorithm::min_slope = 10.;
const double IDWSlopesAlgorithm::max_slope = 38.;
const size_t IDWSlopesAlgorithm::nrSlopes = 1+lastSlope; //ie flat + 4 expositions

IDWSlopesAlgorithm::IDWSlopesAlgorithm(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm)
                   : InterpolationAlgorithm(vecArgs, i_algo, i_param, i_tsm), 
                     vecDataCache(nrSlopes), vecMetaCache(nrSlopes),
                     trend(vecArgs, i_algo, i_param), scale(1e3), alpha(1.)
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

double IDWSlopesAlgorithm::getQualityRating(const Date& i_date)
{
	//delete the previous cached values
	for (size_t curr_slope=firstSlope; curr_slope<=lastSlope; curr_slope++) {
		vecDataCache[curr_slope].clear();
		vecMetaCache[curr_slope].clear();
	}
	
	date = i_date;
	tsmanager.getMeteoData(date, vecMeteo);
	
	for (size_t ii=0; ii<vecMeteo.size(); ii++){
		const double val = vecMeteo[ii](param);
		if (val==IOUtils::nodata) continue;

		const double slope = vecMeteo[ii].meta.getSlopeAngle();
		if (slope==IOUtils::nodata) continue;
		
		Slopes curr_slope = FLAT;
		if (slope>min_slope) {
			const double azimuth = vecMeteo[ii].meta.getAzimuth();
			if (azimuth==IOUtils::nodata) continue;
			
			if (azimuth<45. || azimuth>315.) {
				curr_slope = NORTH;
			} else if (azimuth<135) {
				curr_slope = EAST;
			} else if (azimuth<225) {
				curr_slope = SOUTH;
			} else {
				curr_slope = WEST;
			}
		}
		
		vecDataCache[curr_slope].push_back( val );
		vecMetaCache[curr_slope].push_back( vecMeteo[ii].meta );
	}

	nrOfMeasurments = vecDataCache[FLAT].size();
	static const size_t minNrStations = 3;
	if (vecDataCache[FLAT].size()<minNrStations || vecDataCache[NORTH].size()<minNrStations || vecDataCache[EAST].size()<minNrStations || vecDataCache[SOUTH].size()<minNrStations || vecDataCache[WEST].size()<minNrStations) return 0;

	return 0.8;
}

Grid2DObject IDWSlopesAlgorithm::computeAspect(const DEMObject& dem, const Slopes& curr_slope)
{
	Grid2DObject grid;
	trend.detrend(vecMetaCache[curr_slope], vecDataCache[curr_slope]);
	Interpol2D::IDW(vecDataCache[curr_slope], vecMetaCache[curr_slope], dem, grid, scale, alpha);
	trend.retrend(dem, grid);
	return grid;
}

void IDWSlopesAlgorithm::calculate(const DEMObject& dem, Grid2DObject& grid)
{
	info.clear(); info.str("");
	info << vecDataCache[NORTH].size() << " north, " << vecDataCache[EAST].size() << " east, " << vecDataCache[SOUTH].size() << " south, " << vecDataCache[WEST].size() << " west stations";

	grid = computeAspect(dem, FLAT);
	const Grid2DObject north( computeAspect(dem, NORTH) );
	const Grid2DObject east( computeAspect(dem, EAST) );
	const Grid2DObject south( computeAspect(dem, SOUTH) );
	const Grid2DObject west( computeAspect(dem, WEST) );

	//now we merge the grids together as a weighted average of the aspects and slope angle
	for (size_t ii=0; ii<dem.size(); ++ii) {
		if (dem(ii)==IOUtils::nodata) continue;

		const double slope = dem.slope(ii);
		const double azi = dem.azi(ii);
		if (slope==IOUtils::nodata || azi==IOUtils::nodata) continue;

		if (slope<=min_slope) continue; //we keep the flat value
		const double w_flat = (slope>=max_slope)? 0. : (max_slope - slope)/(max_slope - min_slope);
		
		if (azi<90) {
			const double w_azi = 1. - azi / 90.;
			grid(ii) = w_flat*grid(ii) + (1.-w_flat)*(w_azi*north(ii) + (1.-w_azi)*east(ii));
		} else if (azi<180) {
			const double w_azi = 1. - fabs(azi - 90.) / 90.;
			grid(ii) = w_flat*grid(ii) + (1.-w_flat)*(w_azi*east(ii) + (1.-w_azi)*south(ii));
		} else if (azi<270) {
			const double w_azi = 1. - fabs(azi - 180.) / 90.;
			grid(ii) = w_flat*grid(ii) + (1.-w_flat)*(w_azi*south(ii) + (1.-w_azi)*west(ii));
		} else {
			const double w_azi = 1. - fabs(azi - 270.) / 90.;
			grid(ii) = w_flat*grid(ii) + (1.-w_flat)*(w_azi*west(ii) + (1.-w_azi)*north(ii));
		}
		
	}

}

} //namespace
