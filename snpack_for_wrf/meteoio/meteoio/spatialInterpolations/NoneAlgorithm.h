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
#ifndef NONEALGORITHM_H
#define NONEALGORITHM_H

#include <meteoio/spatialInterpolations/InterpolationAlgorithms.h>

namespace mio {

/**
 * @class NoneAlgorithm
 * @ingroup spatialization
 * @brief Returns a nodata filled grid
 * @details
 * This allows to tolerate missing data, which can be useful if an alternate strategy could
 * later be used to generate the data (ie. a parametrization). This algorithm will only run
 * after all others failed.
 *
 * @code
 * ILWR::algorithms = IDW_LAPSE AVG NONE
 * @endcode
 */
class NoneAlgorithm : public InterpolationAlgorithm {
	public:
		NoneAlgorithm(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm)
			: InterpolationAlgorithm(vecArgs, i_algo, i_param, i_tsm) {}
		virtual double getQualityRating(const Date& i_date);
		virtual void calculate(const DEMObject& dem, Grid2DObject& grid);
};

} //end namespace mio

#endif
