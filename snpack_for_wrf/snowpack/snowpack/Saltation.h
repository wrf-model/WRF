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
#ifndef SALTATION_H
#define SALTATION_H

#include <meteoio/MeteoIO.h>
#include <string.h>

#include <snowpack/SnowpackConfig.h>

/**
 * @brief This module contains the saltation model of Judith.
 * @ingroup postprocessing
 */
class Saltation {
	public:
		Saltation(const SnowpackConfig& i_cfg);

		bool compSaltation(const double& tauS, const double& tau_th, const double& slope_angle, const double& dg,
		                   double& massflux, double& c_salt, const double& density_air) const;

		static const double karman;
		static const double z0_salt;

	private:
		static double sa_vw(const double& z, const double& tauA, const double& tauS, const double& z0,
                     const double& u_start, const double& slope_angle);
		static double sa_vw2(const double& z, const double& tauA, const double& tauS, const double& z0,
                      const double& u_start, const double& slope_angle);

		static bool sa_Traject(const double& u0, const double& angle_e_rad, const double& slope_angle, const double& dg,
		                const double& tauA, const double& tauS, const double& z0,
		                double& ubar, double& u_i, double& angle_i_rad, double& t_i, double& z_max);

		static double sa_MassFlux(const double& z0, const double& tauS, const double& tauA, const double& slope_angle,
		                   const double& dg, const double& tau_th, double& z_max, double& ubar, double& cs);

		static double sa_AeroEntrain(const double& z0, const double& tauS, const double& slope_angle, const double& dg,
		                      const double& tau_th, double& flux, double& z_max, double& ubar, double& cs);

		static int sa_TestSaltation(const double& z0, const double& tauS, const double& tauA, const double& slope_angle,
		                     const double& dg, const double& tau_th, double& z_max, double& ubar);

		const std::string saltation_model;
		static const double hs_frac, elas, angle_ej, ratio_ve_ustar, salt_height;
		static const int strong, weak;
};

#endif

