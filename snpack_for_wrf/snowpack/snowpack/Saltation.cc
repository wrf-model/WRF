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

#include <meteoio/MeteoIO.h>

#include <snowpack/Saltation.h>
#include <snowpack/Constants.h>
#include <snowpack/Utils.h>
#include <cmath>

using namespace mio;
using namespace std;

/************************************************************
 * static section                                           *
 ************************************************************/
/**
 * Height relative to maximum jump height taken
 * as hs, i.e. height where the concentration
 * is determined as BC for suspension
 */
const double Saltation::hs_frac = 1.0;

const double Saltation::karman = 0.4; ///< Von Karman constant
const double Saltation::elas = 0.5;   ///< Coefficient of Elasticity 0.5

const double Saltation::angle_ej = 25.; ///< ejection angle (deg)
const double Saltation::ratio_ve_ustar = 3.1; ///< Original Value by Judith: 2.9

const int Saltation::strong = 1;
const int Saltation::weak = 0;

///Saltation Z0 Preliminary values will be changed later Judith original: 0.00098
const double Saltation::z0_salt = 0.0017;
const double Saltation::salt_height = 0.07;

/*
 * The follwing const variables are no longer needed, their values shall remain documented:
 * const double Saltation::c_red = 1.0; //Defines red. of saltation conc. in suspension sol.
 * const double Saltation::grain_size = 0.000680;
 * const double Saltation::tau_thresh = 0.094; //Original Value by Judith: 0.094
 * const double Saltation::z0 = 0.01; //Wind Field Z0 - includes larger surface features
 */

/************************************************************
 * non-static section                                       *
 ************************************************************/

static std::string get_model(const SnowpackConfig& cfg) 
{
	std::string model;
	cfg.getValue("SALTATION_MODEL", "SnowpackAdvanced", model);
	return model;
}

Saltation::Saltation(const SnowpackConfig& cfg) : saltation_model( get_model(cfg) ) {}

/**
 * @brief Returns the wind profile
 * @param z
 * @param tauA
 * @param tauS
 * @param z0
 * @param u_start
 * @param slope_angle (deg)
 * @return u
 */
double Saltation::sa_vw(const double& z, const double& tauA, const double& tauS, const double& z0,
                        const double& u_start, const double& slope_angle)
{
	//  hs = DSQR(0.6*u_start)/(2.*Constants::g * cos(slope_angle*mio::Cst::to_rad));
	const double hs = z0 / 100. + 0.6 * Optim::pow2(u_start) / (2. * Constants::g * cos(slope_angle*mio::Cst::to_rad));
	const double B = (tauS - tauA) / log(hs / z0);

	double u;
	if (B > 0.0) {
		u = (pow(tauA + B * log(z / z0), 1.5) - pow(tauA, 1.5))
			/ (1.5 * B * Saltation::karman * sqrt(Constants::density_air));
	} else {
		u = sqrt(tauS / Constants::density_air) / Saltation::karman * log(z / z0);
	}

	return (u);
}

/**
 * @brief Returns the wind profile (alternative)
 * @param z
 * @param tauA
 * @param tauS
 * @param z0
 * @param u_start
 * @param slope_angle (deg)
 * @return u
 */
double Saltation::sa_vw2(const double& z, const double& tauA, const double& tauS, const double& z0,
                         const double& u_start, const double& slope_angle)
{
	//  hs = z0 + DSQR(0.6*u_start)/(2.*Constants::g * cos(slope_angle*mio::Cst::to_rad));
	const double hs = Optim::pow2(0.6 * u_start) / (2. * Constants::g * cos(slope_angle*mio::Cst::to_rad));
	const double r = tauA / tauS;
	const double ustar = sqrt(tauS / Constants::density_air);

	double u = 0., z_act = z0;
	while (z_act < z) {
		static const double dz = 0.00002;
		z_act += dz;
		const double ustarz = ustar * (1. - (1. - sqrt(r)) *  exp(-z_act / hs));
		const double dudz = ustarz / Saltation::karman / z_act;
		u += dudz * dz;
	}

	return(u);
}

/**
 * @brief Computes a trajectory and returns the characteristics
 * @param u0
 * @param angle_e_rad (rad)
 * @param slope_angle (deg)
 * @param dg
 * @param tauA
 * @param tauS
 * @param z0
 * @param ubar
 * @param u_i
 * @param angle_i_rad (rad)
 * @param t_i
 * @param z_max
 * @return bool
 */
bool Saltation::sa_Traject(const double& u0, const double& angle_e_rad, const double& slope_angle, const double& dg,
                           const double& tauA, const double& tauS, const double& z0,
                           double& ubar, double& u_i, double& angle_i_rad, double& t_i, double& z_max)
{
	static const double DT = 0.0005; //time step in seconds
	static const double vis = 1.74e-5; //viscosity

	// Initialize velocities of particle and position
	double xdot = u0 * cos(angle_e_rad);
	double zdot = u0 * sin(angle_e_rad);
	double z = z0 + zdot * DT;

	ubar = xdot;
	z_max = z0;
	if (ubar == 0.0) return false;

	// Make a simple forward time integration
	int nt = 0;
	do {
		// Wind velocity
		const double u = sa_vw(z, tauA, tauS, z0, u0*sin(angle_e_rad), slope_angle);
		// Relative velocity
		const double Ur = sqrt(Optim::pow2((xdot - u)) + Optim::pow2(zdot));
		// Reynolds number and Drag coefficient
		const double Re = dg * Ur / vis;
		const double Cd = 24. / Re + 6. / (1. + sqrt(Re)) + 0.4;

		// Accelerations
		const double xdotdot = -0.75 * Constants::density_air / Constants::density_ice * Ur / dg * Cd * (xdot - u) - Constants::g * sin(slope_angle*mio::Cst::to_rad);
		const double zdotdot = -0.75 * Constants::density_air / Constants::density_ice * Ur / dg * Cd * zdot - Constants::g * cos(slope_angle*mio::Cst::to_rad);

		// Velocities & positions
		xdot += xdotdot * DT;
		zdot += zdotdot * DT;
		z += zdot * DT;

		ubar += xdot;
		if (z > z_max)
			z_max = z;

		nt++;
	} while (z > z0);

	if (!((nt > 1) && (xdot > 0.0)) ) {
		return false;
	} else {
		// Mean horizontal velocity
		ubar /= nt;
		// Impact velocity
		u_i = sqrt(Optim::pow2(xdot) + Optim::pow2(zdot));
		// Impact angle
		angle_i_rad = atan(-zdot / xdot);
		// Flight time
		t_i = nt * DT;

		return true;
	}
}

/**
 * @brief Computes the saltation flux for one bottom element
 * @param z0e
 * @param tauS
 * @param tauA
 * @param slope_angle (deg)
 * @param dg
 * @param tau_th
 * @param z_max
 * @param ubar
 * @param cs
 * @return mass flux
 */
double Saltation::sa_MassFlux(const double& z0, const double& tauS, const double& tauA, const double& slope_angle,
                              const double& dg, const double& tau_th, double& z_max, double& ubar, double& cs)
{
	// Initialize mass: Model Mass Flus is extremely sensitive to grain mass
	const double mass = Constants::density_ice * 4. / 3. * Constants::pi * Optim::pow3(.5*dg);

	// Compute trajectories until stationary
	const double angle_e_rad = Saltation::angle_ej*mio::Cst::to_rad;
	const double u0 = Saltation::ratio_ve_ustar * sqrt((tauS - tau_th) / Constants::density_air);

	// Iterate until stationary trajectory
	double angle_i_rad=0., t_i=0.;
	double u_i = u0;
	while (u_i == u0) {
		if (!sa_Traject(u0, angle_e_rad, slope_angle, dg, tauA, tauS, z0, ubar, u_i, angle_i_rad, t_i, z_max)) {
			cs = 0.;
			return 0.;
		}
	}

	// Shear stress from one grain
	const double tau1G_reb = mass * ( u_i * cos(angle_i_rad) - u0 * cos(angle_e_rad) ) / t_i;

	// Number of rebounding grains
	const double Nreb = (tauS - tauA) / tau1G_reb;

	// Now compute the concentration
	if ( (u0 > 0.0) && (ubar > 0.0) ) {
		const double hsalt = z0 + Saltation::hs_frac * Optim::pow2(u0 * cos(angle_e_rad)) / (4. * Constants::g);
		const double udisturb = sa_vw(hsalt, tauA, tauS, z0, u0 * sin(angle_e_rad), slope_angle);
		const double ulog = sqrt(tauS / Constants::density_air) / Saltation::karman * log(hsalt / z0);
		cs = std::min(0.02, Constants::density_air * Optim::pow2((ulog - udisturb)/ubar) );
	} else {
		cs = 0.0;
	}

	if ( !(mass*Nreb*ubar > 0. && mass*Nreb*ubar < 1e10) ) {
		prn_msg(__FILE__, __LINE__, "msg+", Date(),
			"Infinite Flux from Mass Flux  mass:%lf NrebG:%lf ubar:%lf", mass, Nreb, ubar);
	}

	return (mass*Nreb*ubar);
}

/**
 * @brief Computes the saltation flux for one bottom element
 * @param z0
 * @param tauS
 * @param slope_angle (deg)
 * @param dg
 * @param tau_th
 * @param flux
 * @param z_max
 * @param ubar
 * @param cs
 * @return surface shear stress
*/
double Saltation::sa_AeroEntrain(const double& z0, const double& tauS, const double& slope_angle, const double& dg,
                                 const double& tau_th, double& flux, double& z_max, double& ubar, double& cs)
{
	// Initialize mass, entrainment number and surface shear stress
	const double mass = Constants::density_ice * 4. / 3. * Constants::pi * Optim::pow3(.5*dg);
	const double angle_e_rad = Saltation::angle_ej*mio::Cst::to_rad;
	//  n_ae = 1./(1.09*mass*sqrt(tauS/DENSITY_AIR));
	//const double n_ae  = 0.5 / 8. / Constants::pi / dg / dg;
	const double n_ae = 1. / (2. * 8. * Constants::pi * Optim::pow2(dg));

	// Compute trajectories until stationary
	//  u0 = 0.63/sin(angle_e_rad)*sqrt(tauS/DENSITY_AIR);
	const double u0 = Saltation::ratio_ve_ustar * sqrt((tauS - tau_th) / Constants::density_air);
	//  u0 = 3.7*sqrt(tauS-tau_th);

	static const double eps = 0.001;
	static const int maxit = 40;
	int iter=0;
	double tauA_old, Nae;
	double tauA = .5 * (tauS + tau_th);
	do {
		double u_i, angle_i_rad, t_i;
		if (!sa_Traject(u0, angle_e_rad, slope_angle, dg, tauA, tauS, z0, ubar, u_i, angle_i_rad, t_i, z_max)) {
			flux = 0.;
			cs = 0.;
			return tauS;
		}
		tauA_old = tauA;
		Nae = n_ae * (tauA - tau_th);
		tauA = tauS - Nae *mass * (u_i * cos(angle_i_rad) - u0 * cos(angle_e_rad)) / t_i;
		//    tauA = tauS - Nae*mass*(u_i*cos(angle_i_rad) - u0*cos(angle_e_rad));
		if(tauA < tau_th) tauA = tau_th;
		tauA = (tauA+tauA_old)/2.;
		iter++;
	} while ( (fabs((tauA - tauA_old)/tauA) > eps) && (iter < maxit) );

	if (iter == maxit) {
		prn_msg(__FILE__, __LINE__, "wrn", Date(), "Airborne shear stress Iteration did not converge");
		tauA = 0.5 * tauS;
		Nae = n_ae * (tauA - tau_th);
	}
	if (tauA < tau_th) {
		prn_msg(__FILE__, __LINE__, "wrn", Date(), "Airborne shear stress smaller than threshold");
		Nae = 0.;
	}

	flux = mass * Nae * ubar;

	if ( !((flux > 0.) && (flux < 1e10)) ) {
		prn_msg(__FILE__, __LINE__, "msg+", Date(),
			"Infinite Flux from Aero Entrain  mass:%lf Nae:%lf ubar:%lf",mass,Nae,ubar);
	}

	// Now compute the concentration
	if ( (u0 > 0.0) && (ubar > 0.0) ){
		const double hsalt = z0 + Saltation::hs_frac * Optim::pow2(u0 * cos(angle_e_rad)) / (4. * Constants::g);
		const double udisturb = sa_vw(hsalt, tauA, tauS, z0, u0 * sin(angle_e_rad), slope_angle);
		const double ulog = sqrt(tauS / Constants::density_air) / Saltation::karman * log(hsalt / z0);
		cs = std::min(0.02, Constants::density_air * Optim::pow2((ulog - udisturb)/ubar) );
	} else {
		cs = 0.0;
	}

	return(tauA);

}

/**
 * @brief Computes the saltation flux
 * @param z0
 * @param tauS
 * @param tauA
 * @param slope_angle (deg)
 * @param dg
 * @param tau_th
 * @param z_max
 * @param ubar
 * @return saltation weak or strong
 */
int Saltation::sa_TestSaltation(const double& z0, const double& tauS, const double& tauA, const double& slope_angle,
                                const double& dg, const double& tau_th, double& z_max, double& ubar)
{
	// Compute trajectories until stationary
	const double angle_e_rad = Saltation::angle_ej*mio::Cst::to_rad;
	double u0 = Saltation::ratio_ve_ustar * sqrt((tauS - tau_th) / Constants::density_air);

	// Compute the first trajectory
	double u_i, angle_i_rad, t_i;
	sa_Traject(u0, angle_e_rad, slope_angle, dg, tauA, tauS, z0, ubar, u_i, angle_i_rad, t_i, z_max);
	const double hsalt1 = z_max;
	u0 = Saltation::elas * u_i;

	// Compute three trajectories to see whether they are growing
	double hsalt2=0.;
	for (unsigned int j = 0; j < 3; j++) {
		sa_Traject(u0, angle_e_rad, slope_angle, dg, tauA, tauS, z0, ubar, u_i, angle_i_rad, t_i, z_max);
		hsalt2 = z_max;
		u0 = Saltation::elas * u_i;
	}

	if (hsalt2 > hsalt1) {
		return (Saltation::strong);
	} else {
		return (Saltation::weak);
	}

}

/**
 * @brief Computes the saltation flux
 * @note  Sorensen's model is computationally more efficient than Judith Doorschot's
 * @param i_tauS
 * @param tau_th
 * @param slope_angle (deg)
 * @param dg
 * @param massflux
 * @param c_salt
 * @return bool
*/
bool Saltation::compSaltation(const double& i_tauS, const double& tau_th, const double& slope_angle, const double& dg,
                                  double& massflux, double& c_salt, const double& density_air) const
{
	if (saltation_model == "SORENSEN") { // Default model
		// Sorensen
		const double tauS = i_tauS;
		const double ustar = sqrt(tauS / density_air);
		const double ustar_thresh = sqrt(tau_th / density_air);
		if (ustar > ustar_thresh) {
			//Sorensen (2004), parameters from Vionnet et al. (2014)
			massflux = (density_air / Constants::g) * Optim::pow3(ustar) * (1. - Optim::pow2(ustar_thresh / ustar)) * (2.6 + 2.5 * Optim::pow2(ustar_thresh / ustar) + 2. * (ustar_thresh / ustar) );

			//Sorensen (1991) with the wrong units
			//massflux = 0.0014 * Constants::density_air * ustar * (ustar - ustar_thresh) * (ustar + 7.6*ustar_thresh + 205.);

			c_salt = massflux / ustar*0.001; // Arbitrary Scaling to match Doorschot concentration
		} else {
			massflux = 0.;
			c_salt = 0.;
		}
	}
	else if (saltation_model == "DOORSCHOT") { // Judith Doorschot's model
		int    k = 5;
		// Initialize Shear Stress Distribution
		const double taumean = i_tauS;
		const double taumax = 15.* i_tauS;
		const double taustep = (taumax - tau_th) / k;
		const double Cp = 1. / taumean;
		double tauA_middle=0.;
		double flux_mean = 0., cs_mean = 0.;

		for (int j = 0; j < k; j++) {
			const double tau_j = tau_th + ((double)(j) + 0.5) * taustep;
			const double tauS = tau_j;

			if(tauS > tau_th) {
				double ubar = 0., z_lower = 0.;
				double flux, cs; // What we finally want
				// First test for large rebound thresholds
				if (sa_TestSaltation(Saltation::z0_salt, tauS, tauS, slope_angle, dg, tau_th, z_lower, ubar) == Saltation::weak) {
					sa_AeroEntrain(Saltation::z0_salt, tauS, slope_angle, dg, tau_th, flux, z_lower, ubar, cs);
				} else {
					// Use an iterative method to determine the rebound threshold at the ground
					const double eps = 1e-5;
					double tauA_right = tauS, tauA_left = 0.;
					do {
						tauA_middle = .5 * (tauA_left + tauA_right);
						if (sa_TestSaltation(Saltation::z0_salt, tauS, tauA_middle, slope_angle,
									      dg, tau_th, z_lower, ubar) == Saltation::strong) {
							tauA_right = tauA_middle;
						} else {
							tauA_left = tauA_middle;
						}
					} while (tauA_right - tauA_left > eps);
					const double tau_r = tauA_middle;

					/*
					* Distinguish the different possibilities after Judith and compute
					* the flux; Start with computation of tau_e: The surface shear stress
					* for the hypothetical case of aerodynamic entrainment only given a
					* certain overall shear stress, tauS
					*/
					const double tau_e = sa_AeroEntrain(Saltation::z0_salt, tauS, slope_angle, dg, tau_th, flux, z_lower, ubar, cs);
					if (tau_e < tau_th) {
						const double tauA = tau_r;
						flux = sa_MassFlux(Saltation::z0_salt, tauS, tauA, slope_angle, dg, tau_th, z_lower, ubar, cs);
					} else if (tau_e < tau_r) {
						//const double tauA = tau_e;
					} else {
						const double tauA = tau_r; // Flux computation is wrong, must be redone
						flux = sa_MassFlux(Saltation::z0_salt, tauS, tauA, slope_angle, dg, tau_th, z_lower, ubar, cs);
					}
				} // else large rebound threshold
				const double Ptau_j = exp(-Cp * (tau_j - 0.5 * taustep)) - exp(-Cp * (tau_j+ 0.5 * taustep));
				cs_mean += cs * Ptau_j;
				flux_mean += flux * Ptau_j;
			} // if there is s.th. to do

		} // for all shear stress classes

		// Fill return Values
		massflux = flux_mean;
		c_salt = cs_mean;
	}
	else {
		prn_msg(__FILE__, __LINE__, "err", Date(), "Saltation model %s not implemented yet!", saltation_model.c_str());
		throw IOException("The required saltation model is not implemented yet!", AT);
	}

	return true;
}

