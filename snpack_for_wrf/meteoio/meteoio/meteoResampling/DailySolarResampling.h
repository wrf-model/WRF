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
#ifndef DAILY_SOLAR_RESAMPLING_H
#define DAILY_SOLAR_RESAMPLING_H

#include <meteoio/meteoResampling/ResamplingAlgorithms.h>

namespace mio {

/**
 * @brief Generate solar radiation out of daily sums.
 * @details
 * Daily sums of solar radiation (once, per day, any time during the day). Data provided at midnight is considered to belong to the day that just finished)
 * are compared to the potential radiation, leading to an atmospheric loss factor.
 * This loss factor is then applied to the potential solar radiation calculated at the requested time.
 * When using this algorithm for RSWR, an albedo is required. A default value of 0.5 is used. If the snow height is available and greater than a 10cm threshold,
 * a snow albedo is used. Below this threshold, a soil albedo is used.
 * @code
 * ISWR::resample   = daily_solar
 * @endcode
 */
class Daily_solar : public ResamplingAlgorithms {
	public:
		Daily_solar(const std::string& i_algoname, const std::string& i_parname, const double& dflt_window_size, const std::vector< std::pair<std::string, std::string> >& vecArgs);

		void resample(const std::string& stationHash, const size_t& index, const ResamplingPosition& position, const size_t& paramindex,
		              const std::vector<MeteoData>& vecM, MeteoData& md);
		std::string toString() const;
	private:
		double getSolarInterpol(const Date& resampling_date, const size_t& stat_idx) const;
		double compRadiation(const double& lat, const double& lon, const double& alt, const double& HS, const size_t& stat_idx);
		size_t getStationIndex(const std::string& key);

		std::vector< std::vector<double> > radiation;
		std::vector<std::string> station_index;
		std::vector<Date> dateStart, dateEnd;
		std::vector<double> loss_factor;

		static const size_t samples_per_day;
};

} //end namespace mio

#endif
