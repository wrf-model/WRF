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
#ifndef LAPSEORDINARYKRIGINGALGORITHM_H
#define LAPSEORDINARYKRIGINGALGORITHM_H

#include <meteoio/spatialInterpolations/InterpolationAlgorithms.h>
#include <meteoio/spatialInterpolations/ODKrigAlgorithm.h>

namespace mio {

/**
 * @class LapseOrdinaryKrigingAlgorithm
 * @ingroup spatialization
 * @brief Ordinary kriging with detrending.
 * @details
 * This is very similar to OrdinaryKrigingAlgorithm but performs detrending on the data and therefore also accepts
 * the lapse rate definition arguments as parsed by Trend::Trend().
 *
 * @code
 * TA::algorithms    = ODKRIG_LAPSE
 * TA::odkrig::vario = SPHERICVARIO linvario
 * TA::odkrig::soft  = true
 * TA::odkrig::rate  = -0.008
 * @endcode
 */
class LapseOrdinaryKrigingAlgorithm : public OrdinaryKrigingAlgorithm {
	public:
		LapseOrdinaryKrigingAlgorithm(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm);
		virtual void calculate(const DEMObject& dem, Grid2DObject& grid);
	private:
		Trend trend;
};

} //end namespace mio

#endif
