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
#ifndef ACCUMULATE_H
#define ACCUMULATE_H

#include <meteoio/meteoResampling/ResamplingAlgorithms.h>

namespace mio {

/**
 * @brief Accumulation over a user given period.
 * @details
 * The input data is accumulated over a given time interval (given as filter argument, in seconds).
 * This is for example needed for converting rain gauges measurements read every 10 minutes to
 * hourly precipitation measurements. It takes the following arguments:
 * - PERIOD: the accumulation period in seconds (mandatory);
 * - STRICT: if setting the argument "strict" to "true", nodatas will propagate (ie. a single nodata in the input will force the re-accumulated value to be nodata). By default, all valid values are aggregated and only pure nodata intervals produce a nodata in the output.
 * @code
 * PSUM::resample   = accumulate
 * PSUM::accumulate::period = 3600
 * @endcode
 */
class Accumulate : public ResamplingAlgorithms {
	public:
		Accumulate(const std::string& i_algoname, const std::string& i_parname, const double& dflt_window_size, const std::vector< std::pair<std::string, std::string> >& vecArgs);

		void resample(const std::string& stationHash, const size_t& index, const ResamplingPosition& position, const size_t& paramindex,
		              const std::vector<MeteoData>& vecM, MeteoData& md);
		std::string toString() const;
	private:
		static size_t findStartOfPeriod(const std::vector<MeteoData>& vecM, const size_t& index, const Date& dateStart);
		double easySampling(const std::vector<MeteoData>& vecM, const size_t& paramindex, const size_t& /*index*/, const size_t& start_idx, const Date& dateStart, const Date& resampling_date) const;
		double complexSampling(const std::vector<MeteoData>& vecM, const size_t& paramindex, const size_t& index, const size_t& start_idx, const Date& dateStart, const Date& resampling_date) const;

		double accumulate_period; //internally, in julian days
		bool strict;
};

} //end namespace mio

#endif
