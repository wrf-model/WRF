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
#ifndef DAILY_AVERAGE_RESAMPLING_H
#define DAILY_AVERAGE_RESAMPLING_H

#include <meteoio/meteoResampling/ResamplingAlgorithms.h>

namespace mio {

/**
 * @brief Generate daily variations around a single daily average.
 * @details
 * The parameter to be interpolated is assumed to be a daily average and a sinusoidal variation of the
 * data will be generated around this average.
 *
 * Either one or two additional parameters bearing the same name followed by "_MIN" and/or "_MAX"
 * are provided or a fixed amplitude is provided as argument (it will also be used as a fallback if the _MIN and
 * _MAX parameters are missing at some timesteps). Iit is also possible to provide the "phase" or the
 * fraction of the day when the minimum is reached.
 *
 * @code
 * [Interpolations1D]
 * TA::resample         = daily_avg
 * TA::daily_avg::range = 5
 * TA::daily_avg::phase = .25                ;assume that TA varies +/- 5K around its average during the day and reaches its minimum at 6am
 * @endcode
 * @note If both the average (the parameter itself in the data set),
 * min and max are provided, an error message will be returned.
 */
class DailyAverage : public ResamplingAlgorithms {
	public:
		DailyAverage(const std::string& i_algoname, const std::string& i_parname, const double& dflt_window_size, const std::vector< std::pair<std::string, std::string> >& vecArgs);

		void resample(const std::string& stationHash, const size_t& index, const ResamplingPosition& position, const size_t& paramindex,
		              const std::vector<MeteoData>& vecM, MeteoData& md);
		std::string toString() const;
	private:
		double getValue(const std::vector<MeteoData>& vecM, const size_t& paramindex, const size_t& index, const Date& dayStart, const double& frac_day) const;
		double range, phase;
};

} //end namespace mio

#endif
