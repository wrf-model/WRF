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
#ifndef DEMALGORITHMS_H
#define DEMALGORITHMS_H

#include <meteoio/dataClasses/DEMObject.h>
#include <meteoio/dataClasses/Grid2DObject.h>

namespace mio {

/**
 * @class DEMAlgorithms
 * @brief A static class to compute various DEM-related parameters (such as shading, view factors, etc)
 */

class DEMAlgorithms {
	public:
		static Grid2DObject getHillshade(const DEMObject& dem, const double& elev, const double& azimuth);
		static double getHorizon(const DEMObject& dem, const size_t& ix1, const size_t& iy1, const double& bearing);
		static double getHorizon(const DEMObject& dem, const Coords& point, const double& bearing);
		static void getHorizon(const DEMObject& dem, const Coords& point, const double& increment, std::vector< std::pair<double,double> >& horizon);
        static double getCellSkyViewFactor(const DEMObject& dem, const size_t& ii, const size_t& jj);

	private:
		static double getSearchDistance(const DEMObject& dem);
        static double getTanMaxSlope(const DEMObject& dem, const double& dmax, const double& bearing, const size_t& i, const size_t& j);
};
} //end namespace

#endif
