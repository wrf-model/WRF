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
#ifndef ALS_INTERPOLATION_H
#define ALS_INTERPOLATION_H

#include <meteoio/spatialInterpolations/InterpolationAlgorithms.h>

namespace mio {

/**
 * @class ALS_Interpolation
 * @ingroup spatialization
 * @brief Scale and distribute the precipitation according to Airborn Laser Scans (ALS) grids.
 * @details
 * This assumes that precipitation fields can be scaled according to
 * snow heights grids in order to get improved precipitation patterns (see <i>"Scaling Precipitation Input to Spatially Distributed Hydrological Models 
 * by Measured Snow Distribution"</i>, C. Vögeli, M. Lehning, N. Wever, M. Bavay, 2016, Front. Earth Sci. <b>4</b>: 108. 
 * <a href="https://doi.org/10.3389/feart.2016.00108">doi: 10.3389/feart.2016.00108.</a>). 
 * 
 * In a first step, the precipitation are spatially distributed with a <i>base</i> method and in a second step the resulting grid is modified to reflect the
 * spatial distribution of the provided snow height grid. Of course, these two steps happen automatically and transparently to the caller of this
 * algorithm.
 * 
 * It takes the following arguments:
 *  - BASE: the base method to fill the grid (for example, idw_lapse (default));
 *  - GRID: the name of the file (in GRID2DPATH) containing the gridded ALS data (relying on the GRID2D plugin). This must provide 
 * a snow height map that will be used to scale the precipitation patterns.
 *  - TA_THRESH: an optional air temperature threshold (in K) below which such redistribution occurs
 * (so liquid precipitation is not redistributed)
 *
 * @note If there are some time steps when only one station provides the necessary parameter, the base method will
 * automatically switch to "AVG".
 *
 * @code
 * PSUM::algorithms        = ALS_SCALING
 * PSUM::als_scaling::base = idw_lapse
 * PSUM::als_scaling::grid = als_20150213.asc
 * @endcode
 */
class ALS_Interpolation : public InterpolationAlgorithm {
	public:
		ALS_Interpolation(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm,
		                               GridsManager& i_gdm, Meteo2DInterpolator& i_mi);
		virtual double getQualityRating(const Date& i_date);
		virtual void calculate(const DEMObject& dem, Grid2DObject& grid);
	private:
		void initGrid(const DEMObject& dem, Grid2DObject& grid);

		Meteo2DInterpolator& mi;
		GridsManager& gdm;
		Grid2DObject ALS_scan;
		std::string filename, grid2d_path, base_algo, base_algo_user;
		double ta_thresh, als_mean; ///< the air temperature must be below a given threshold for the scaling to be applied
		bool inputIsAllZeroes;
};

} //end namespace mio

#endif
