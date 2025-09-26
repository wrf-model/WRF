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
#ifndef SWRADINTERPOLATION_H
#define SWRADINTERPOLATION_H

#include <meteoio/spatialInterpolations/InterpolationAlgorithms.h>
#include <meteoio/meteoLaws/Sun.h>

namespace mio {

/**
 * @class SWRadInterpolation
 * @ingroup spatialization
 * @brief %Solar radiation interpolation with optional terrain shading.
 * @details
 * The splitting coefficients and an atmospheric losses factors are computed at each station that provides ISWR and spatially interpolated
 * with an Inverse Distance Weighting scheme. Then the potential radiation is computed at each pixel and scaled appropriately with the
 * atmospheric loss factor for this pixel. When applying topographic shading (default), the local splitting coefficient is used. The global, horizontal
 * (except when using the PROJECT_ON_SLOPE option) short wave radiation is then returned. It supports the following arguments:
 *  - SHADING: if set to FALSE, turns off topographic shading (default: TRUE);
 *  - PROJECT_ON_SLOPE: if set to TRUE, the computed radiation will be projected on the slopes (default: FALSE so it returns an horizontal radiation field);
 *  - SCALE: this is a scaling parameter to smooth the IDW distribution. In effect, this is added to the distance in order
 * to move into the tail of the 1/d distribution (default: 1000m);
 *  - ALPHA: this is an exponent to the 1/d distribution (default: 1);
 *
 * @code
 * ISWR::algorithms     = SWRad
 * ISWR::SWRad::shading = true
 * @endcode
 *
 * @note For this method to work, you also need to define spatial interpolations algorithms for TA, RH and P (a basic STD_PRESS algorithm
 * is usually enough)
 * @note This algorithm is quite time consuming (specially the topographic shading) and therefore not appropriate for very large domains.
 */
class SWRadInterpolation : public InterpolationAlgorithm {
	public:
		SWRadInterpolation(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm,
		                                   Meteo2DInterpolator& i_mi);
		virtual double getQualityRating(const Date& i_date);
		virtual void calculate(const DEMObject& dem, Grid2DObject& grid);
	private:
		Meteo2DInterpolator& mi;
		SunObject Sun;
		std::vector<size_t> vecIdx;
		double scale, alpha; ///<a scale parameter to smooth out the 1/dist and an exponent
		bool shading, project_on_slope; ///<sould we also compute the shading? should we project the computed fields on the slopes?
		static const double soil_albedo, snow_albedo, snow_thresh;
};

} //end namespace mio

#endif
