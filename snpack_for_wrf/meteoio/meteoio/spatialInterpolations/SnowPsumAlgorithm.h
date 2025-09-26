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
#ifndef SNOWPSUMINTERPOLATION_H
#define SNOWPSUMINTERPOLATION_H

#include <meteoio/spatialInterpolations/InterpolationAlgorithms.h>

namespace mio {

/**
 * @class SnowPSUMInterpolation
 * @ingroup spatialization
 * @brief Precipitation distribution according to the local slope and curvature.
 * @details
 * The precipitation distribution is initialized using a specified algorithm ("BASE" option, set to IDW_LAPSE by default, see IDWLapseAlgorithm).
 * An optional parameter can be given to specify which algorithm has to be used for initializing the grid.
 * Please do not forget to provide the arguments of the chosen algorithm itself if necessary!
 *
 * After filling the initial grid, this method modifies the solid precipitation distribution according to the local slope and curvature:
 * all pixels whose slope is greater than 60° will not receive any snow at all. All pixels whose slope is less than 40° will receive full snow
 * and any pixel between 40° and 60° sees a linear correction between 100% and 0% snow. After this step, a curvature
 * correction is applied: pixels having the minimum curvature see 50% snow more, pixels having the maximum curvature see
 * 50% snow less and pixels ate the middle of the curvature range are unaffected.
 *
 * For more, see <i>"Quantitative evaluation of different hydrological modelling approaches
 * in a partly glacierized Swiss watershed"</i>, Magnusson et Al., Hydrological Processes, <b>25</b>, 2071-2084, 2011 and
 * <i>"Modelling runoff from highly glacierized alpine catchments in a changing climate"</i>, Huss et All., Hydrological Processes, <b>22</b>, 3888-3902, 2008.
 *
 * An example using this algorithm, initializing the grid with a constant lapse rate fill using +0.05% precipitation increase per meter of elevation, is given below:
 * @code
 * PSUM::algorithms      = PSUM_SNOW
 * PSUM::psum_snow::base = avg_lapse
 * PSUM::avg_lapse::frac = true
 * PSUM::avg_lapse::rate = 0.0005
 * @endcode
 */
class SnowPSUMInterpolation : public InterpolationAlgorithm {
	public:
		SnowPSUMInterpolation(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm,
		                                          GridsManager& i_gdm, Meteo2DInterpolator& i_mi);
		virtual double getQualityRating(const Date& i_date);
		virtual void calculate(const DEMObject& dem, Grid2DObject& grid);
	private:
		Meteo2DInterpolator& mi;
		GridsManager& gdm;
		std::string base_algo_user;
};

} //end namespace mio

#endif
