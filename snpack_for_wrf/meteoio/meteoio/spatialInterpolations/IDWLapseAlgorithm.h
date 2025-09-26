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
#ifndef IDWLAPSE_ALGORITHM_H
#define IDWLAPSE_ALGORITHM_H

#include <meteoio/spatialInterpolations/InterpolationAlgorithms.h>

namespace mio {

/**
 * @class IDWLapseAlgorithm
 * @ingroup spatialization
 * @brief Inverse Distance Weighting interpolation algorithm with elevation detrending/reprojection.
 * @details
 * The input data is detrended and the residuals are spatially interpolated using an Inverse Distance
 * Weighting interpolation algorithm (see IDWAlgorithm). Then, each value is reprojected to the real
 * elevation of the relative cell (re-trending).
 *
 * The lapse rate definition arguments as parsed by Trend::Trend() are supported as
 * well as the following arguments:
 *  - SCALE: this is a scaling parameter to smooth the IDW distribution. In effect, this is added to the distance in order
 * to move into the tail of the 1/d distribution (default: 1000m);
 *  - ALPHA: this is an exponent to the 1/d distribution (default: 1);
 *
 * @code
 * TA::algorithms      = IDW_LAPSE
 * TA::idw_lapse::soft = true
 * TA::idw_lapse::rate = -0.008
 * @endcode
 */
class IDWLapseAlgorithm : public InterpolationAlgorithm {
	public:
		IDWLapseAlgorithm(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm);
		virtual double getQualityRating(const Date& i_date);
		virtual void calculate(const DEMObject& dem, Grid2DObject& grid);
	private:
		Trend trend;
		double scale, alpha; ///<a scale parameter to smooth out the 1/dist and an exponent
};

} //end namespace mio

#endif
