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
#ifndef AVGLAPSERATEALGORITHM_H
#define AVGLAPSERATEALGORITHM_H

#include <meteoio/spatialInterpolations/InterpolationAlgorithms.h>

namespace mio {

/**
 * @class AvgLapseRateAlgorithm
 * @ingroup spatialization
 * @brief Average filling with elevation lapse rate interpolation algorithm.
 * @details
 * The grid is filled with the average of the detrended measured values and then re-trended. Or to put it
 * differently, the following operations are performed: detrending - averaging - re-trending. The
 * lapse rate definition arguments as parsed by Trend::Trend() are supported.
 *
 * @code
 * PSUM::algorithms      = AVG_LAPSE
 * PSUM::avg_lapse::soft = true
 * PSUM::avg_lapse::frac = true
 * PSUM::avg_lapse::rate = 0.05
 * @endcode
 */
class AvgLapseRateAlgorithm : public InterpolationAlgorithm {
	public:
		AvgLapseRateAlgorithm(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm)
		                                        : InterpolationAlgorithm(vecArgs, i_algo, i_param, i_tsm), trend(vecArgs, i_algo, i_param) {}
		virtual double getQualityRating(const Date& i_date);
		virtual void calculate(const DEMObject& dem, Grid2DObject& grid);
	private:
		Trend trend;
};

} //end namespace mio

#endif
