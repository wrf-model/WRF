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
#ifndef WINSTRAL_ALGORITHM_H
#define WINSTRAL_ALGORITHM_H

#include <meteoio/spatialInterpolations/InterpolationAlgorithms.h>

namespace mio {

/**
 * @class WinstralAlgorithm
 * @ingroup spatialization
 * @brief DEM-based wind-exposure interpolation algorithm.
 * @details
 * This is an implementation of the method described in Winstral, Elder, & Davis,
 * <i>"Spatial snow modeling of wind-redistributed snow using terrain-based parameters"</i>, 2002,
 * Journal of Hydrometeorology, <b>3(5)</b>, 524-538.
 * The DEM is used to compute wind exposure factors that are used to alter the precipitation fields.
 * It is usually a good idea to provide a DEM that also contain the accumulated snow height in order
 * to get a progressive softening of the terrain features.
 *
 * It takes the following arguments:
 *  - BASE:: provide the base algorithm to pre-fill the grid, since this method must first use another algorithm to
 * generate an initial precipitation field, and then modify it. By default, this base method is "idw_lapse" and switches to
 * "avg" if only one station can provide the precipitation at a given time step (for an easy fallback). Please do not forget
 * to provide any necessary arguments for this base method!
 *  - TYPE: specify how the synoptic wind direction is derived. It is either of the following:
 *     - AUTO: automatic computation of the synoptic wind direction (see below);
 *     - FIXED: provide a fixed synoptic wind bearing that is used for all time steps. It then needs the following argument:
 *          - DW_SYNOP: fixed synoptic wind bearing that is used for all time steps;
 *     - REF_STATION: the wind direction at the provided station is assumed to be the synoptic wind direction. It then needs the following argument:
 *          - REF_STATION: the station ID providing the wind direction;
 *  - DMAX: maximum search distance or radius (default: 300m);
 *
 * If type=AUTO, the synoptic wind direction will be computed as follow:
 * the stations are located in the DEM and their wind shading (or exposure) is computed. If at least one station is found
 * that is not sheltered from the wind (in every direction), it provides the synoptic wind (in case of multiple stations, the
 * vector average is used). Please note that the stations that are not included in the DEM are considered to be sheltered.
 * If no such station is found, the vector average of all the available stations is used.
 *
 * @code
 * PSUM::algorithms         = WINSTRAL
 * PSUM::winstral::base     = idw_lapse
 * PSUM::winstral::type     = fixed
 * PSUM::winstral::dw_synop = 180
 * PSUM::winstral::dmax     = 300
 * @endcode
 * @remarks Only cells with an air temperature below freezing participate in the redistribution
 */
class WinstralAlgorithm : public InterpolationAlgorithm {
	public:
		WinstralAlgorithm(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm,
		                               GridsManager& i_gdm, Meteo2DInterpolator& i_mi);
		virtual double getQualityRating(const Date& i_date);
		virtual void calculate(const DEMObject& dem, Grid2DObject& grid);
	private:
		void initGrid(const DEMObject& dem, Grid2DObject& grid);
		static bool windIsAvailable(const std::vector<MeteoData>& i_vecMeteo, const std::string& i_ref_station);
		static bool isExposed(const DEMObject& dem, Coords location);
		static double getSynopticBearing(const std::vector<MeteoData>& i_vecMeteo, const std::string& i_ref_station);
		static double getSynopticBearing(const std::vector<MeteoData>& i_vecMeteo);
		static double getSynopticBearing(const DEMObject& dem, const std::vector<MeteoData>& i_vecMeteo);

		typedef enum TYPE {
				FIXED,
				REF_STATION,
				AUTO
			} synoptic_wind_type;
			
		Meteo2DInterpolator& mi;
		GridsManager& gdm;
		std::string base_algo_user, ref_station;
		double user_synoptic_bearing;
		bool inputIsAllZeroes;
		double dmax;
};

} //end namespace mio

#endif
