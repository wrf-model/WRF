/*
 *  SNOWPACK stand-alone
 *
 *  Copyright WSL Institute for Snow and Avalanche Research SLF, DAVOS, SWITZERLAND
*/
/*  This file is part of Snowpack.
    Snowpack is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Snowpack is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Snowpack.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef AGGREGATE_H
#define AGGREGATE_H

#include <snowpack/DataClasses.h>
#include <vector>

#include <cstddef> //needed for size_t

/**
 * @class Aggregate
 * @version 7.03
 * @bug     -
 * @brief This module contains the routines to perform profile aggregation
 */
class Aggregate {

	public:
		static size_t aggregate(std::vector<SnowProfileLayer>& Pdata);
		static bool joinSimilarLayers(ElementData& Edata_upper, ElementData& Edata_lower);
		static bool mergeThinLayer(ElementData& Edata_upper, ElementData& Edata_lower);

	private:
		static void shift(const size_t& nL_ini, std::vector<SnowProfileLayer>& Pdata);
		static bool joinSimilarLayers(const size_t& e_upper, std::vector<SnowProfileLayer>& Pdata);
		static bool mergeThinLayer(const size_t& e_lower, std::vector<SnowProfileLayer>& Pdata);

		static const double limit_dry;     ///< Distinguishes between dry and wet snow layers (1)
		static const double diff_theta_w;  ///< Maximum water difference for aggregation (% by volume)
		static const double diff_jul;      ///< Maximum  age difference for aggregation (d)
		static const double diff_dg;       ///< Maximum  grain size difference for aggregation (mm)
		static const double diff_dg_rel;   ///< Maximum  relative grain size difference for aggregation (mm)
		static const double diff_sp;       ///< Maximum  sphericity difference for aggregation (1)
		static const double diff_dd;       ///< Maximum  dendricity difference for aggregation (1)
		static const double min_l_element; ///< Minimum length of element to be kept separate (cm)
};

#endif
