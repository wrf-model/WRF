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
 * @file SalinityTransport.h
 */

#ifndef SALINITYTRANSPORT_H
#define SALINITYTRANSPORT_H

#include <meteoio/MeteoIO.h>

/**
 * @class SalinityTransport
 * @author Nander Wever
 * @brief This module contains the solver for the diffusion-advection equation for the transport of salinity
 */
class SalinityTransport {

	public:
		SalinityTransport(size_t nE);		// Class constructor

		bool VerifyCFL(const double dt);
		bool VerifyImplicitDt(const double dt);
		bool SolveSalinityTransportEquationImplicit(const double dt, std::vector <double>& DeltaSal, const double f, const bool DonorCell = true);	// Donor cell or central differences?
		bool SolveSalinityTransportEquationExplicit(const double dt, std::vector <double>& DeltaSal);
		enum SalinityTransportSolvers{EXPLICIT, IMPLICIT, IMPLICIT2};

		std::vector<double> flux_up;		//Flux with element above (negative=upward, positive=downward)
		std::vector<double> flux_down;		//Flux with element below (negative=upward, positive=downward)
		std::vector<double> flux_up_2;		//Flux with element above (negative=upward, positive=downward)
		std::vector<double> flux_down_2;	//Flux with element below (negative=upward, positive=downward)
		std::vector<double> dz_;		//Grid cell size
		std::vector<double> dz_up;		//Grid cell distance above
		std::vector<double> dz_down;		//Grid cell distance below
		std::vector<double> theta1;		//Vol. liquid water content (m^3/m^3), at t=n
		std::vector<double> theta2;		//Vol. liquid water content (m^3/m^3), at t=n+1
		std::vector<double> BrineSal;		//Salinity in brine, in g/(m^3_water)
		std::vector<double> D;			//Diffusivity
		std::vector<double> sb;			//Source/sink term for brine salinity

		double BottomSalinity, TopSalinity;	//The boundary conditions bottom and top salinities.

		double BottomSalFlux, TopSalFlux;	//Bottom and top salt flux

	private:
		void SetDomainSize(size_t nE);
		size_t NumberOfElements;
};
#endif //End of SalinityTransport.h
