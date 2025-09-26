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
/**
 * @file TechnicalSnow.h
 * This header file defines the technical snow module
 */

#ifndef TECHNICALSNOW_H
#define TECHNICALSNOW_H

#include <snowpack/DataClasses.h>

class TechSnow {
	public:
		TechSnow(const SnowpackConfig& /*i_cfg*/) {}
		
		static bool prepare(const bool& snowPrep, const mio::Date& current_date, const SnowStation& Xdata);
		static void preparation(SnowStation& Xdata);
		static void productionPpt(const CurrentMeteo& Mdata, const double& cumu_precip, double &Tw, double &rho_hn, double &delta_cH, double &theta_w);
};
#endif
