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

#include <meteoio/spatialInterpolations/UserAlgorithm.h>
#include <meteoio/FileUtils.h>

namespace mio {

USERInterpolation::USERInterpolation(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm,
                                                                 GridsManager& i_gdm)
                                : InterpolationAlgorithm(vecArgs, i_algo, i_param, i_tsm), gdm(i_gdm), filename(), grid2d_path(), subdir(), file_ext()
{
	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="SUBDIR") {
			subdir = vecArgs[ii].second;
		} else if (vecArgs[ii].first=="EXT") {
			file_ext = vecArgs[ii].second;
		}
	}

	if (!subdir.empty()) subdir += "/";
	if (file_ext.empty()) file_ext = ".dat";

	gdm.getConfig().getValue("GRID2DPATH", "Input", grid2d_path);
}

double USERInterpolation::getQualityRating(const Date& i_date)
{
	date = i_date;
	filename = subdir + date.toString(Date::NUM) + "_" + param + file_ext;

	if (!FileUtils::validFileAndPath(grid2d_path+"/"+filename)) {
		std::cerr << "[E] Invalid grid filename for "+algo+" interpolation algorithm: " << grid2d_path+"/"+filename << "\n";
		return 0.0;
	}

	return (FileUtils::fileExists(grid2d_path+"/"+filename))? 1. : 0.;
}

void USERInterpolation::calculate(const DEMObject& dem, Grid2DObject& grid)
{
	info.clear(); info.str("");
	gdm.read2DGrid(grid, filename);
	if (!grid.isSameGeolocalization(dem)) {
		throw InvalidArgumentException("[E] trying to load a grid(" + filename + ") that does not have the same georeferencing as the DEM!", AT);
	} else {
		info << FileUtils::getFilename(filename);
	}
}

} //namespace
