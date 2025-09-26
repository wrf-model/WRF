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
#ifndef ILWREPSALGORITHM_H
#define ILWREPSALGORITHM_H

#include <meteoio/spatialInterpolations/InterpolationAlgorithms.h>

namespace mio {

/**
 * @class ILWREpsAlgorithm
 * @ingroup spatialization
 * @brief Incoming Long Wave Radiation interpolation algorithm.
 * @details
 * Each ILWR is converted to an emissivity (using the local air temperature), interpolated using IDW_LAPSE and reconverted to ILWR. As
 * a side effect, the user must have defined algorithms to be used for air temperature (since this is needed for emissivity to ILWR conversion).
 * The lapse rate definition arguments as parsed by Trend::Trend() are supported (but keep in mind <b><i>these apply to the emissivity</i></b>, not to the ILWR!)
 * as well as the following arguments:
 *  - SCALE: this is a scaling parameter to smooth the IDW distribution. In effect, this is added to the distance in order
 * to move into the tail of the 1/d distribution (default: 1000m);
 *  - ALPHA: this is an exponent to the 1/d distribution (default: 1);
 *
 * @note the emissivity variation is not linear with elevation, but it is possible to derive some approximate lapse rate for the elevation on interest, for example from
 * Centeno, M. <i>"New formulae for the equivalent night sky emissivity"</i>, 1982, Solar Energy, <b>28(6)</b>, 489-498 (watch out, an exponent is wrong in quation 13 page 491).
 * A better approach would be to use a power function to fit the emissivity as a function of the elevation...
 *
 * @code
 * ILWR::algorithms = ILWR_EPS
 * ILWR::ilwr_eps::soft = true
 * ILWR::ilwr_eps::rate = -1.8e-5 ;around 2000m elevation
 * @endcode
 */
class ILWREpsAlgorithm : public InterpolationAlgorithm {
	public:
		ILWREpsAlgorithm(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm,
		                                 Meteo2DInterpolator& i_mi);
		virtual double getQualityRating(const Date& i_date);
		virtual void calculate(const DEMObject& dem, Grid2DObject& grid);
	private:
		Trend trend;
		Meteo2DInterpolator& mi;
		std::vector<double> vecDataEA; ///<vectors of extracted emissivities
		double scale, alpha; ///<a scale parameter to smooth out the 1/dist and an exponent
};

} //end namespace mio

#endif
