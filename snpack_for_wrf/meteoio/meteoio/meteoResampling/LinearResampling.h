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
#ifndef LINEARRESAMPLING_H
#define LINEARRESAMPLING_H

#include <meteoio/meteoResampling/ResamplingAlgorithms.h>

namespace mio {

/**
 * @brief Linear data resampling.
 * @details
 * If a point is requested that is in between two input data points,
 * the requested value is automatically calculated using a linear interpolation. Furthermore
 * if the argument extrapolate is provided there will be an attempt made to extrapolate the
 * point if the interpolation fails, by computing the line equation y = kx + d. It takes the following arguments:
 *  - WINDOW_SIZE: This represents how big a data gap can be and still be interpolated. This allows to overwrite the global WINDOW_SIZE (in ResamplingAlgorithms) for
 * this parameter and resampling algorithm (optional);
 *  - EXTRAPOLATE: If set to TRUE, points *outside* of available measurements will be interpolated (otherwise, there need to be values before and after a missing point for it to
 * be interpolated. Optional).
 *
 * @code
 * [Interpolations1D]
 * TA::resample            = linear
 * TA::linear::extrapolate = true
 * @endcode
 */
class LinearResampling : public ResamplingAlgorithms {
	public:
		LinearResampling(const std::string& i_algoname, const std::string& i_parname, const double& dflt_window_size, const std::vector< std::pair<std::string, std::string> >& vecArgs);

		void resample(const std::string& stationHash, const size_t& index, const ResamplingPosition& position, const size_t& paramindex,
		              const std::vector<MeteoData>& vecM, MeteoData& md);
		std::string toString() const;
	private:
		bool extrapolate;
};

} //end namespace mio

#endif
