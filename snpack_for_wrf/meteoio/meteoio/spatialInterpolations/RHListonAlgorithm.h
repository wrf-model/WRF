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
#ifndef RHLISTONALGORITHM_H
#define RHLISTONALGORITHM_H

#include <meteoio/spatialInterpolations/InterpolationAlgorithms.h>

namespace mio {

/**
 * @class RHListonAlgorithm
 * @ingroup spatialization
 * @brief Relative humidity interpolation algorithm.
 * @details
 * This is an implementation of the method described in (Liston & Elder, 2006): for each input point, the dew
 * point temperature is calculated. Then, the dew point temperatures are spatially interpolated using IDWLapseAlgorithm
 * (if only one station is available, no altitudinal trends will be applied).
 * Finally, each local dew point temperature is converted back to a local relative humidity. As a side effect, the user must
 * have defined algorithms to be used for air temperature (since this is needed for dew point to RH conversion).
 * It takes the following arguments:
 *  - SCALE: this is a scaling parameter to smooth the IDW distribution. In effect, this is added to the distance in order
 * to move into the tail of the 1/d distribution (default: 1000m);
 *  - ALPHA: this is an exponent to the 1/d distribution (default: 1);
 *  - all the trend-controlling arguments supported by Trend::Trend().
 *
 * @code
 * RH::algorithms = LISTON_RH
 * @endcode
 */
class RHListonAlgorithm : public InterpolationAlgorithm {
	public:
		RHListonAlgorithm(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm,
		                                 Meteo2DInterpolator& i_mi);
		virtual double getQualityRating(const Date& i_date);
		virtual void calculate(const DEMObject& dem, Grid2DObject& grid);
	private:
		Trend trend;
		Meteo2DInterpolator& mi;
		std::vector<double> vecDataTA, vecDataRH; ///<vectors of extracted TA and RH
		double scale, alpha; ///<a scale parameter to smooth out the 1/dist and an exponent
};


} //end namespace mio

#endif
