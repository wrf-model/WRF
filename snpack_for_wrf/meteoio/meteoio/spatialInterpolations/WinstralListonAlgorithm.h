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
#ifndef WINSTRAL_LISTON_ALGORITHM_H
#define WINSTRAL_LISTON_ALGORITHM_H

#include <meteoio/spatialInterpolations/InterpolationAlgorithms.h>

namespace mio {

/**
 * @class WinstralListonAlgorithm
 * @ingroup spatialization
 * @brief DEM-based wind-exposure interpolation algorithm, for a spatially explicit varying DW field.
 * @details
 * This is an implementation of the method described in Winstral, Elder, & Davis,
 * <i>"Spatial snow modeling of wind-redistributed snow using terrain-based parameters"</i>, 2002,
 * Journal of Hydrometeorology, <b>3(5)</b>, 524-538.
 * The wind direction for determining wind-exposure is taken from a 2D wind direction field.
 * The DEM is used to compute wind exposure factors that are used to alter the precipitation fields.
 * It is usually a good idea to provide a DEM that also contain the accumulated snow height in order
 * to get a progressive softening of the terrain features.
 *
 * It takes the following arguments:
 *  - BASE:: provide the base algorithm to pre-fill the grid, since this method must first use another algorithm to
 * generate an initial precipitation field, and then modify it. By default, this base method is "idw_lapse" and switches to
 * "avg" if only one station can provide the precipitation at a given time step (for an easy fallback). Please do not forget
 * to provide any necessary arguments for this base method!
 *  - DMAX: maximum search distance or radius (default: 300m);
 *
 * @remarks
 *  - Only cells with an air temperature below freezing participate in the redistribution
 *  - Using the WinstralListonAlgorithm also requires the specification of an interpolation method for DW
 *
 * @code
 * PSUM::algorithms         = WINSTRAL++
 * PSUM::winstral++::base   = idw_lapse
 * PSUM::winstral++::dmax   = 300
 * DW::algorithms           = LISTON_WIND
 * DW::liston_wind::scale   = 300
 * @endcode
 */
class WinstralListonAlgorithm : public InterpolationAlgorithm {
	public:
		WinstralListonAlgorithm(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm,
		                               GridsManager& i_gdm, Meteo2DInterpolator& i_mi);
		virtual double getQualityRating(const Date& i_date);
		virtual void calculate(const DEMObject& dem, Grid2DObject& grid);
	private:
		void initGrid(const DEMObject& dem, Grid2DObject& grid);

		Meteo2DInterpolator& mi;
		GridsManager& gdm;
		std::string base_algo_user;
		bool inputIsAllZeroes;
		double dmax;
};

} //end namespace mio

#endif
