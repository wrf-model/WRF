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

#include <meteoio/spatialInterpolations/SnowPsumAlgorithm.h>
#include <meteoio/meteoStats/libinterpol2D.h>

namespace mio {

SnowPSUMInterpolation::SnowPSUMInterpolation(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm,
                                                                                     GridsManager& i_gdm, Meteo2DInterpolator& i_mi)
                                           : InterpolationAlgorithm(vecArgs, i_algo, i_param, i_tsm), mi(i_mi), gdm(i_gdm), base_algo_user("IDW_LAPSE")
{
	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if(vecArgs[ii].first=="BASE") {
			base_algo_user = IOUtils::strToUpper( vecArgs[ii].second );
		}
	}
}

double SnowPSUMInterpolation::getQualityRating(const Date& i_date)
{
	date = i_date;
	nrOfMeasurments = getData(date, param, vecData, vecMeta);

	if (nrOfMeasurments == 0) return 0.0;

	return 0.9;
}

void SnowPSUMInterpolation::calculate(const DEMObject& dem, Grid2DObject& grid)
{
	info.clear(); info.str("");

	const std::vector< std::pair<std::string, std::string> > vecArgs( mi.getArgumentsForAlgorithm(param, base_algo_user, "Interpolations2D") );
	InterpolationAlgorithm* algorithm( AlgorithmFactory::getAlgorithm(base_algo_user, mi, vecArgs, tsmanager, gdm, param) );
	algorithm->getQualityRating(date);
	algorithm->calculate(dem, grid);
	info << algorithm->getInfo();
	delete algorithm;

	//get TA interpolation from call back to Meteo2DInterpolator
	Grid2DObject ta;
	mi.interpolate(date, dem, MeteoData::TA, ta);

	//slope/curvature correction for solid precipitation
	const double orig_mean = grid.grid2D.getMean();
	Interpol2D::PrecipSnow(dem, ta, grid);
	//HACK: correction for precipitation sum over the whole domain
	//this is a cheap/crappy way of compensating for the spatial redistribution of snow on the slopes
	const double new_mean = grid.grid2D.getMean();
	if (new_mean!=0.) grid *= orig_mean/new_mean;

	//Interpol2D::SteepSlopeRedistribution(dem, ta, grid);
	//Interpol2D::CurvatureCorrection(dem, ta, grid);
}


} //namespace
