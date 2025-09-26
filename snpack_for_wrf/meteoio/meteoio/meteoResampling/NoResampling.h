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
#ifndef NORESAMPLING_H
#define NORESAMPLING_H

#include <meteoio/meteoResampling/ResamplingAlgorithms.h>

namespace mio {

/**
 * @brief No resampling: do not resample parameter but keep original sampling rate
 * @details
 * It is enabled either with the "none" or "no" key.
 * @code
 * [Interpolations1D]
 * TA::resample = none
 * @endcode
 */
class NoResampling : public ResamplingAlgorithms {
	public:
		NoResampling(const std::string& i_algoname, const std::string& i_parname, const double& dflt_window_size, const std::vector< std::pair<std::string, std::string> >& vecArgs);

		void resample(const std::string& stationHash, const size_t& index, const ResamplingPosition& position, const size_t& paramindex,
		              const std::vector<MeteoData>& vecM, MeteoData& md);
		std::string toString() const;
};

} //end namespace mio

#endif
