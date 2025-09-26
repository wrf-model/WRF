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

#include <meteoio/spatialInterpolations/template.h>

namespace mio {

TEMPLATE::TEMPLATE(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm)
                                     : InterpolationAlgorithm(vecArgs, i_algo, i_param, i_tsm)
{
	/*parse the arguments, for example:

	const std::string where( "Interpolations2D::"+i_param+"::"+i_algo );
	bool has_file=false; //this is used to perform basic syntax checks
	double thresh;

	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="FILE") {
			filename = vecArgs[ii].second;
			has_file = true;
		} else if(vecArgs[ii].first=="THRESH") {
			IOUtils::parseArg(vecArgs[ii], where, thresh);
		}
	}

	//basic syntax checks
	if (!has_file) throw InvalidArgumentException("Please provide a filename for the "+algo+" algorithm", AT);
	*/
}

double TEMPLATE::getQualityRating(const Date& i_date)
{
	date = i_date;
	nrOfMeasurments = getData(date, param, vecData, vecMeta); //this is important since the user will see how many stations could be used

	//depending on the arguments and the available data, return a quality rating.
	//Have a look at the other method in order to figure out which rating should be return to rank
	//where you want within the other algorithms
	return 0.1;
}

void TEMPLATE::calculate(const DEMObject& dem, Grid2DObject& grid)
{
	//this should contain all the information to forward to the user so he can understand how the interpolation went
	info.clear(); info.str("");

	//depending on how the interpolation is done, either reset the grid here or in a call to a method of  Interpol2D
	grid.set(dem, IOUtils::nodata);

	//now fill each cell of the grid
	/*for (size_t ii=0; ii<grid.size(); ii++) {
		grid(ii) = ...;
	}
	*/

}

} //namespace
