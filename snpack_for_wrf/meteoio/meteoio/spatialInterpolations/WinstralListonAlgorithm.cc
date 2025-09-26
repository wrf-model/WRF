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

#include <meteoio/spatialInterpolations/WinstralListonAlgorithm.h>
#include <meteoio/spatialInterpolations/WinstralAlgorithm.h>
#include <meteoio/meteoStats/libinterpol2D.h>

namespace mio {

WinstralListonAlgorithm::WinstralListonAlgorithm(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm,
		                               GridsManager& i_gdm, Meteo2DInterpolator& i_mi)
                  : InterpolationAlgorithm(vecArgs, i_algo, i_param, i_tsm), mi(i_mi), gdm(i_gdm), base_algo_user("IDW_LAPSE"),
                    inputIsAllZeroes(false), dmax(300.)
{
	const std::string where( "Interpolations2D::"+i_param+"::"+i_algo );
	bool has_base=false;

	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if(vecArgs[ii].first=="BASE") {
			base_algo_user = IOUtils::strToUpper( vecArgs[ii].second );
			has_base = true;
		} else if (vecArgs[ii].first=="DMAX") {
			IOUtils::parseArg(vecArgs[ii], where, dmax);
		}
	}

	if (!has_base) throw InvalidArgumentException("Wrong number of arguments supplied for "+where, AT);
}

double WinstralListonAlgorithm::getQualityRating(const Date& i_date)
{
	date = i_date;
	nrOfMeasurments = getData(date, param, vecData, vecMeta);
	inputIsAllZeroes = Interpol2D::allZeroes(vecData);

	if (inputIsAllZeroes) return 0.99;

	if (nrOfMeasurments==0) return 0.0;

	return 0.99;
}

void WinstralListonAlgorithm::initGrid(const DEMObject& dem, Grid2DObject& grid)
{
	//initialize precipitation grid with user supplied algorithm (IDW_LAPSE by default)
	const std::string base_algo = (nrOfMeasurments==1)? "AVG" : base_algo_user; //if there is only one station, revert to a safe default

	const std::vector< std::pair<std::string, std::string> > vecArgs( mi.getArgumentsForAlgorithm(param, base_algo, "Interpolations2D") );
	InterpolationAlgorithm* algorithm( AlgorithmFactory::getAlgorithm(base_algo, mi, vecArgs, tsmanager, gdm, param) );
	algorithm->getQualityRating(date);
	algorithm->calculate(dem, grid);
	info << algorithm->getInfo();
	delete algorithm;
}


void WinstralListonAlgorithm::calculate(const DEMObject& dem, Grid2DObject& grid)
{
	info.clear(); info.str("");

	//if all data points are zero, simply fill the grid with zeroes
	if (inputIsAllZeroes) {
		Interpol2D::constant(0., dem, grid);
		return;
	}

	//get initial PSUM grid
	initGrid(dem, grid);

	//get meteo fields interpolation from call back to Meteo2DInterpolator
	Grid2DObject ta, dw, vw;
	mi.interpolate(date, dem, MeteoData::TA, ta);
	mi.interpolate(date, dem, MeteoData::DW, dw);
	mi.interpolate(date, dem, MeteoData::VW, vw);

	//alter the field with Winstral and the chosen wind direction
	Interpol2D::Winstral(dem, ta, dw, vw, dmax, grid);
}

} //namespace
