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

#ifndef SNOWDRIFT_H
#define SNOWDRIFT_H

#include <meteoio/MeteoIO.h>

#include <snowpack/Saltation.h>
#include <snowpack/DataClasses.h>
#include <snowpack/SnowpackConfig.h>

class Saltation;

/**
 * @class SnowDrift
 * @brief This class contains the computation of local snow drift and the associated erosion
 * @ingroup postprocessing
 */
class SnowDrift {

	public:
		SnowDrift(const SnowpackConfig& i_cfg);

		double compSnowDrift(const CurrentMeteo& Mdata, SnowStation& Xdata, SurfaceFluxes& Sdata, double& cumu_psum) const;

		static const double schmidt_drift_fudge;

                double compMassFlux(const ElementData& Edata, const double& ustar, const double& slope_angle, double& l_tau_thresh, double& l_tau, const double& density_air) const; // ADDED BY VARUN

 	private:
//		double compMassFlux(const ElementData& Edata, const double& ustar, const double& slope_angle) const; // COMMENTED BY VARUN

		const Saltation saltation; // The saltation model used
		const bool enforce_measured_snow_heights, snow_redistribution; // Will be read from cfg object
		const std::string snow_erosion;
		const bool alpine3d; ///< triggers various tricks for Alpine3D (including reducing the number of warnings)
		const double sn_dt;        //Calculation time step in seconds as derived from CALCULATION_STEP_LENGTH
		static const bool msg_erosion;
		std::string forcing;
}; //End class SnowDrift

#endif

