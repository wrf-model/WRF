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
#ifndef ORDINARYKRIGINGALGORITHM_H
#define ORDINARYKRIGINGALGORITHM_H

#include <meteoio/spatialInterpolations/InterpolationAlgorithms.h>
#include <meteoio/meteoStats/libinterpol1D.h>

namespace mio {

/**
 * @class OrdinaryKrigingAlgorithm
 * @ingroup spatialization
 * @brief Ordinary kriging.
 * @details
 * This implements ordinary krigging (see https://secure.wikimedia.org/wikipedia/en/wiki/Kriging)
 * with user-selectable variogram model (see https://secure.wikimedia.org/wikipedia/en/wiki/Variogram).
 * More details about the specific computation steps of kriging are provided in Interpol2D::ODKriging.
 *
 * The variogram is currently computed with the current data (as ½*(X1-X2)²), which makes it quite
 * uninteresting... However, calculating the covariances (used to build the variogram) from time series
 * (thus reflecting the time-correlation between stations) has not brought any significant improvements, so it is currently
 * not used (although implemented).
 *
 * Please note that the variogram and krigging coefficients are re-computed fresh for each new grid (or time step).
 * The available variogram models are found in Fit1D::regression and given as optional VARIO argument
 * (by default, LINVARIO is used). Several models can be given, the first that can fit the data will be used
 * for the current timestep:
 * @code
 * TA::algorithms    = ODKRIG
 * TA::odkrig::vario = SPHERICVARIO linvario
 * @endcode
 */
class OrdinaryKrigingAlgorithm : public InterpolationAlgorithm {
	public:
		OrdinaryKrigingAlgorithm(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm);
		virtual double getQualityRating(const Date& i_date);
		virtual void calculate(const DEMObject& dem, Grid2DObject& grid);
	protected:
		std::vector< std::vector<double> > getTimeSeries(const bool& detrend_data) const;
		void getDataForEmpiricalVariogram(std::vector<double> &distData, std::vector<double> &variData) const;
		void getDataForVariogram(std::vector<double> &distData, std::vector<double> &variData, const bool& detrend_data=false) const;
		bool computeVariogram(const bool& detrend_data=false);
		Fit1D variogram;
		std::vector<std::string> vario_types;
};

} //end namespace mio

#endif
