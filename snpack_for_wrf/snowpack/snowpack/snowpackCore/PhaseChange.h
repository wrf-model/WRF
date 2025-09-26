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
#ifndef PHASE_CHANGE_H
#define PHASE_CHANGE_H

#include <snowpack/DataClasses.h>
#include <meteoio/MeteoIO.h>

/**
 * @class PhaseChange
 * @author Perry Bartelt \n Michael Lehning and others
 * @version 10.03
 * @brief This class contains the phase change routines for the 1d snowpack model
 *        It also updates the volumetric contents of each element.
 */
class PhaseChange {
 	public:
		PhaseChange(const SnowpackConfig& i_cfg);
		
		void reset();
		void initialize(SnowStation& Xdata);								//Call before first call to compPhaseChange in a time step
		void finalize(const SurfaceFluxes& Sdata, SnowStation& Xdata, const mio::Date& date_in);	//Call after last call to compPhaseChange in a time step
		double compPhaseChange(SnowStation& Xdata, const mio::Date& date_in, const bool& verbose=true, const double& surf_melt=0.);	//Call to do a phase change in a time step, returning the temperature of the top node (K)

		static const double RE_theta_r;		///< Residual Water Content for snow, when using water transport model "RICHARDSEQUATION"
		static const double RE_theta_threshold; ///< Threshold Water Content for snow, when using water transport model "RICHARDSEQUATION", to determine what is dry and wet snow
		static const double theta_r;    	///< Residual Water Content for snow and soil, when using water transport model "BUCKET" or "NIED"

	private:
		//To prevent string comparisons, we define an enumerated list:
		enum watertransportmodels{BUCKET, NIED, RICHARDSEQUATION};
		watertransportmodels iwatertransportmodel_snow, iwatertransportmodel_soil;

		std::string watertransportmodel_snow;
		std::string watertransportmodel_soil;
		std::string forcing;
		void compSubSurfaceMelt(ElementData& Edata, const unsigned int nSolutes, const double& dt,
		                        double& ql_Rest, const mio::Date& date_in, double& mass_melt);
		void compSubSurfaceFrze(ElementData& Edata, const unsigned int nSolutes, const double& dt,
		                        const mio::Date& date_in);

		const double sn_dt; ///< The calculation_step_length in seconds

		double cold_content_in;		///< cold content before first PhaseChange call (for checking energy balance)
		double cold_content_soil_in;	///< cold content before first PhaseChange call (for checking energy balance)
		double cold_content_out;	///< cold content after last PhaseChange call (for checking energy balance)
		double cold_content_soil_out;	///< cold content after last PhaseChange call (for checking energy balance)

		const bool alpine3d;			///< flag for alpine3d simulations
		double t_crazy_min, t_crazy_max;///< reasonable temperature bounds
		double max_theta_ice;		///< maximum ice content of a layer, above which the presence of liquid water is allowed with sub-freezing temperatures

		static const double theta_s;	///< Saturated Water Content, for now we say  1.0
};

#endif
