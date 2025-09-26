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
#ifndef NEARESTNEIGHBOUR_ALGORITHM_H
#define NEARESTNEIGHBOUR_ALGORITHM_H

#include <meteoio/spatialInterpolations/InterpolationAlgorithms.h>

namespace mio {

/**
 * @class NearestNeighbourAlgorithm
 * @ingroup spatialization
 * @brief Fill each pixel with the measured data of its closests station.
 * @details
 * This can be specially useful for virtual stations, as it guarantee that the spatially interpolated data is measured data. On the other hand, this creates spatial discontinuities when
 * moving from one station to another one. Furthermore, please keep in mind that when this algorithm is enabled, it has the highest priority as soon as at least one station is available.
 * @code
 * TA::algorithms           = NEAREST
 * @endcode
 */
class NearestNeighbourAlgorithm : public InterpolationAlgorithm {
	public:
		NearestNeighbourAlgorithm(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm)
		: InterpolationAlgorithm(vecArgs, i_algo, i_param, i_tsm) {}
		virtual double getQualityRating(const Date& i_date);
		virtual void calculate(const DEMObject& dem, Grid2DObject& grid);
	private:
		size_t getNeighbors(const double& x, const double& y) const;
};

} //end namespace mio

#endif
