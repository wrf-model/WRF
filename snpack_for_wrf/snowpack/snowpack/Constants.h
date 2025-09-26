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

/**
 * @file Constants.h
 * @version 10.02
 * This module defines constants needed for the 1d snowpack model
 */

#ifndef CONSTANTS_H
#define CONSTANTS_H

#include <cstddef> //needed for size_t

/**
 * @brief _VERSION is given as a compilation flag to tell us what is the version number \n
 * Please only use SN_VERSION in the code
 */
#ifndef SN_VERSION
	//here below, the double-expansion stringification macro trick...
	#define STR1(x) #x
	#define STR2(x) STR1(x)
	#define SN_VERSION STR2( _VERSION )
#endif

/// @brief Initial value for stability parameter
#define INIT_STABILITY 999.

namespace Constants {
	const double undefined = -999.; ///<This is the snowpack undefined value
	const int iundefined = -999; ///<This is the snowpack undefined value
	const size_t stundefined = static_cast<size_t>(-999);
	const double min_percent_values = 0.9;

	const double pi = 3.14159265358979323846;
	const double g  = 9.80665;
	const double stefan_boltzmann = 5.67051e-8;

	const double solcon = 1366.1; ///< Total Solar Irradiance (W m-2) (Froehlich, 2006)

	const double gas_constant = 461.9; ///< (J kg-1 K-1)
	const double gas_constant_air = 287.0; ///< for air (J kg-1 K-1)
	const double gas_constant_mol = 8.31;  ///< (J mol-1 K-1)
	const double karman = 0.4; ///< Karman constant

	/// @name Albedo (1)
	//@{
	const double min_albedo = 0.3;
	const double max_albedo = 0.99;
	const double glacier_albedo = 0.3;
	//@}
	/// @name Emissivity (1)
	//@{
	const double emissivity_snow = 0.98;
  const double emissivity_soil = 0.95;

	//@}
	/// @name Density (kg m-3)
	//@{
	const double density_air = 1.1 ; ///< Approximation: use ideal gas law
	const double density_ice = 917.0; ///< At T = 0 degC
	const double density_water = 1000.0; ///<  At T = 0 degC
	const double density_vapor = 1.0; ///< Approximation: use ideal gas law

	///@name Specific heat (J kg-1 K-1)
	//@{
	const double specific_heat_ice = 2100.0; ///< at T = 0 degC
	const double specific_heat_water = 4190.0; ///< at T = 0 degC
	const double specific_heat_air = 1004.67; ///< see Stull "Meteorology for scientists and engineers" p44
	//@}

	///@name Thermal conductivity of snow components ice, water and air (W m-1 K-1)
	//@{
	const double conductivity_ice = 2.2; ///< (W m-1 K-1)
	const double conductivity_water = 0.598; ///< (W m-1 K-1)
	const double conductivity_air = 0.026; ///< (W m-1 K-1)
	//@}

	///@name Vapor Diffusion in Air and Snow (m2 s-1)
	//@{
	///@brief This value was taken from: Colbeck, S.C., 1993. The vapor diffusion coefficient for snow, Water Resources Research, 29(1)
	const double diffusion_coefficient_in_air = 22.0e-6;
	const double diffusion_coefficient_in_snow = 85.0e-6; ///< It is larger (see Colbeck) according to work prior to 2008!
	//@}

	///@name Phase change constants
	//@{
	const double meltfreeze_tk = 273.15; ///< (K)
	const double triple_point_t = 273.16; ///< (K)
	const double triple_point_p = 611.73; ///< (Pa)
	const double lh_sublimation = 2.838e6; ///< (J kg-1) (solid to vapor)
	const double lh_vaporization = 2.504e6; ///< (J kg-1) (liquid to vapor)
	const double lh_fusion = 3.34e5; ///< (J kg-1) (solid to liquid)
	//@}

	///@name Numerical Constants
	//@{
	const double min_slope_angle = 3.; ///< Smallest angle a flat field may show (deg)

	/// @brief Small numbers for comparisons and to avoid divisions by zero
	const double eps = 1.e-6;
	const double eps2 = eps*eps;
	const double big = 1.0e200; ///< used for Dirichlet boundary conditions
	//@}
}

#endif
