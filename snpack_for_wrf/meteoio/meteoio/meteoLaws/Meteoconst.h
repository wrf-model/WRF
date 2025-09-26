/***********************************************************************************/
/*  Copyright 2009 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef METEOCONST_H
#define METEOCONST_H

#include <cfloat>

/**
 * @brief _VERSION is given as a compilation flag to tell us what is the version number \n
 * Please only use MIO_VERSION in the code
 */
#ifndef MIO_VERSION
	//here below, the double-expansion stringification macro trick...
	#define STR1(x) #x
	#define STR2(x) STR1(x)
	#define MIO_VERSION STR2( _VERSION )
#endif

namespace mio {

namespace Cst {
	const double stefan_boltzmann = 5.670373e-8; // (W m-2 K-4)
	const double gravity = 9.80665; // (m s-2)
	const double std_press = 101325.; // (Pa) at sea level
	const double std_temp = 288.15; // (K) at sea level
	const double mean_adiabatique_lapse_rate = 0.0065; // (K/m)
	const double dry_adiabatique_lapse_rate = 0.0098; // (K/m)

	const double gaz_constant_dry_air = 287.058; // (J kg-1 K-1)
	const double gaz_constant_water_vapor = 461.9; // (J kg-1 K-1)
	const double gaz_constant = 8.31451; // (J mol-1 K-1)

	const double p_water_triple_pt = 611.73; // (Pa)
	const double t_water_freezing_pt = 273.15; // (K)
	const double t_water_triple_pt = 273.16; // (K)
	const double l_water_sublimation = 2.838e6; // (J kg-1)
	const double l_water_vaporization = 2.504e6; // (J kg-1)
	const double l_water_fusion = 3.34e5; // (J Kg-1)
	const double water_molecular_mass = 18.0153e-3; // (kg)
	const double dry_air_mol_mass = 0.0289644; // (kg/mol)

	const double specific_heat_ice = 2100.0; // (J K-1), at 0C
	const double specific_heat_water = 4190.0; // (J K-1) at 0C
	const double specific_heat_air = 1004.67; // (J K-1), see Stull "Meteorology for scientists and engineers" p44

	const double earth_R0 = 6356766.0; // (m)

	const double solcon = 1366.1; // (W/m^2)

	//Math constants
	const double e = 2.71828182845904523536; // e
	const double Log2e = 1.44269504088896340736; // log2(e)
	const double Log10e = 0.434294481903251827651; // log10(e)
	const double Ln2 = 0.693147180559945309417; // ln(2)
	const double Ln10 = 2.30258509299404568402; // ln(10)
	const double PI = 3.14159265358979323846; // pi
	const double PI2 = 1.57079632679489661923; // pi/2
	const double PI4 = 0.785398163397448309616; // pi/4
	const double InvPI = 0.318309886183790671538; // 1/pi
	const double TwoOverPI = 0.636619772367581343076; // 2/pi
	const double TwoOverSqrtPI = 1.12837916709551257390; // 2/sqrt(pi)
	const double Sqrt2 = 1.41421356237309504880; // sqrt(2)
	const double InvSqrt2 = 0.707106781186547524401; // 1/sqrt(2)
	const double to_rad = PI/180.; // conversion factor from deg to rad
	const double to_deg = 180./PI; // conversion factor from rad to deg

	const double dbl_max = DBL_MAX;
	const double dbl_min = -DBL_MAX;
} //end CST namespace

} //end namespace

#endif
