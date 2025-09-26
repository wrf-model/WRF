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
#ifndef STDPRESSALGORITHM_H
#define STDPRESSALGORITHM_H

#include <meteoio/spatialInterpolations/InterpolationAlgorithms.h>

namespace mio {

/**
 * @class StandardPressureAlgorithm
 * @ingroup spatialization
 * @brief Standard atmospheric pressure interpolation algorithm.
 * @details
 * This first fills the grid with the standard atmosphere's pressure, depending on the local elevation. Then, depending on the available data:
 *     - if there are no measured atmospheric pressure, nothing else happens;
 *     - if one station has measured local atmospheric pressure, its offset to the standard atmospheric pressure is computed and applied to
 *       the computed grid;
 *     - if multiple stations have measured local atmospheric pressure:
 *                - default: the average offset will be applied to the computed grid;
 *                - USE_RESIDUALS option set to TRUE: the residuals are computed at each station, spatially distributed (with IDW)
 * and applied to the computed grid. Therefore the following extra arguments are supported:
 *                       - SCALE: this is a scaling parameter to smooth the IDW distribution. In effect, this is added to the distance in order
 * to move into the tail of the 1/d distribution (default: 1000m);
 *                       - ALPHA: this is an exponent to the 1/d distribution (default: 1);
 *
 * @code
 * P::algorithms               = STD_PRESS
 * P::Std_Press::USE_RESIDUALS = true
 * @endcode
 */
class StandardPressureAlgorithm : public InterpolationAlgorithm {
	public:
		StandardPressureAlgorithm(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm);
		virtual double getQualityRating(const Date& i_date);
		virtual void calculate(const DEMObject& dem, Grid2DObject& grid);
	private:
		double scale, alpha; ///<a scale parameter to smooth out the 1/dist and an exponent
		bool use_residuals; ///< should we compute residuals ate each station and distribute them spatially?
};

} //end namespace mio

#endif
