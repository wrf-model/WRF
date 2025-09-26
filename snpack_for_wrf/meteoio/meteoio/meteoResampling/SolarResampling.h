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
#ifndef SOLARRESAMPLING_H
#define SOLARRESAMPLING_H

#include <meteoio/meteoResampling/ResamplingAlgorithms.h>
#include <meteoio/IOUtils.h>

#include <map>

namespace mio {

/**
 * @brief Interpolate solar radiation.
 * @details
 * The available solar radiation data is compared to the potential radiation, leading to atmospheric loss
 * factors. At the point that has to be interpolated, the loss factor is linearly interpolated and
 * applied to the potential radiation. When extrapolating the data as well as at the start/end of the day (ie
 * when only one measured value is available), the available value is kept and applied (thus this behaves as
 * a nearest neighbour on the atmospheric loss factor).
 *
 * When using this algorithm for RSWR, an albedo is required. A default value of 0.5 is used. If the snow
 * height is available and greater than a 10cm threshold, a snow albedo is used. Below this threshold,
 * a soil albedo is used.
 *
 * It takes the following arguments:
 *  - WINDOW_SIZE: This represents how big a data gap can be and still be interpolated. This allows to overwrite the global WINDOW_SIZE (in ResamplingAlgorithms) for
 * this parameter and resampling algorithm (optional);
 *  - EXTRAPOLATE: If set to TRUE, points *outside* of available measurements will be interpolated (otherwise, there need to be values before and after a missing point for it to
 * be interpolated. Optional).
 * @code
 * ISWR::resample   = solar
 * @endcode
 */
class Solar : public ResamplingAlgorithms {
	public:
		Solar(const std::string& i_algoname, const std::string& i_parname, const double& dflt_window_size, const std::vector< std::pair<std::string, std::string> >& vecArgs);

		void resample(const std::string& stationHash, const size_t& index, const ResamplingPosition& position, const size_t& paramindex,
		              const std::vector<MeteoData>& vecM, MeteoData& md);
		std::string toString() const;
	private:
		typedef struct POINTS {
			POINTS(): jul1(0.), loss1(IOUtils::nodata), jul2(0), loss2(IOUtils::nodata) {}
			double jul1, loss1, jul2, loss2;
		} Points;

		static double getPotentialH(const MeteoData& md);
		bool computeLossFactor(const std::string& stationHash, const size_t& index, const size_t& paramindex,
		           const std::vector<MeteoData>& vecM, const Date& resampling_date, Points &pts);
		static double interpolateLossFactor(const double& resampling_jul, const Points &pts);

		std::map< std::string, Points > cache_losses;
		bool extrapolate;
};

} //end namespace mio

#endif
