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
 * @file ReSolver1d.h
 * @version 10.02
 */

#ifndef RESOLVER1D_H
#define RESOLVER1D_H

#include <snowpack/snowpackCore/SalinityTransport.h>
#include <snowpack/DataClasses.h>

/**
 * @class ReSolver1d
 * @author Nander Wever
 * @brief This module contains the solver for the 1d Richards Equation for the 1d snowpack model
 */
class ReSolver1d {

	public:
		ReSolver1d(const SnowpackConfig& cfg, const bool& matrix_part);	// Class constructor
		void SolveRichardsEquation(SnowStation& Xdata, SurfaceFluxes& Sdata, double& ql);

		double surfacefluxrate;			// Surfacefluxrate for solving RE. It is either surface of snow, in case of snowpack and solving RE for snow, or surface of soil, when no snowpack and/or solving RE only for soil.
		double soilsurfacesourceflux;		// Soilsurfacesourceflux for solving RE. This is used when we use RE for snow AND there is a snowpack AND the lowest snow element is removed.

		const static double max_theta_ice;	// Maximum allowed theta[ICE]. RE always need some pore space, which is defined by this value.
		const static double REQUIRED_ACCURACY_THETA;

		// Solvers
		static int TDMASolver (size_t n, double *a, double *b, double *c, double *v, double *x);	// Thomas algorithm for tridiagonal matrices
		static int pinv(int m, int n, int lda, double *a);						// Full matrix inversion

	private:
		std::string variant;

		//To prevent string comparisons, we define an enumerated list:
		enum watertransportmodels{UNDEFINED, BUCKET, NIED, RICHARDSEQUATION};
		//K_Average types
		enum K_AverageTypes{ARITHMETICMEAN, GEOMETRICMEAN, HARMONICMEAN, MINIMUMVALUE, UPSTREAM};
		//Solvers
		enum SOLVERS{DGESVD, DGTSV, TDMA};
		//Boundary conditions
		enum BoundaryConditions{DIRICHLET, NEUMANN, LIMITEDFLUXEVAPORATION, LIMITEDFLUXINFILTRATION, LIMITEDFLUX, WATERTABLE, FREEDRAINAGE, GRAVITATIONALDRAINAGE, SEEPAGEBOUNDARY, SEAICE};
		//Salinity mixing models
		enum SalinityMixingModels{NONE, CAPILLARY_GRAVITY, DENSITY_DIFFERENCE, DENSITY_GRAVITY};

		watertransportmodels iwatertransportmodel_snow, iwatertransportmodel_soil;

		std::string watertransportmodel_snow;
		std::string watertransportmodel_soil;
		BoundaryConditions BottomBC;			//Bottom boundary condition (recommended choice either DIRICHLET with saturation (lower boundary in water table) or FREEDRAINAGE (lower boundary not in water table))
		K_AverageTypes K_AverageType;			//Implemented choices: ARITHMETICMEAN (recommended), HARMONICMEAN, GEOMETRICMEAN, MINIMUMVALUE, UPSTREAM
		bool enable_pref_flow;				//true: dual domain approach, false: classic Richards equation.
		double pref_flow_param_th;			//Tuning parameter: saturation threshold in preferential flow
		double pref_flow_param_N;			//Tuning parameter: number of preferential flow paths for heat exchange
		double pref_flow_param_heterogeneity_factor;	//Tuning parameter: heterogeneity factor for grain size

		double sn_dt;					//SNOWPACK time step
		bool allow_surface_ponding;			//boolean to switch on/off the formation of surface ponds in case prescribed infiltration flux exceeds matrix capacity
		bool lateral_flow;				//boolean if lateral flow should be calculated
		bool matrix;					//boolean to define if water transport is calculated for matrixflow or preferential flow
		SalinityTransport::SalinityTransportSolvers SalinityTransportSolver;	//How to solve salinity transport?

		// Grid info
		std::vector<double> dz;				//Layer height (in meters)
		std::vector<double> z;				//Height above the surface (so -1 is 1m below surface)
		std::vector<double> dz_up;			//Distance to upper node (in meters)
		std::vector<double> dz_down;			//Distance to lower node (in meters)
		std::vector<double> dz_;			//Layer distance for the finite differences, see Rathfelder (2004).

		// General functions
		void InitializeGrid(const std::vector<ElementData>& EMS, const size_t& lowernode, const size_t& uppernode);
		std::vector<double> AssembleRHS(const size_t& lowernode, const size_t& uppernode, const std::vector<double>& h_np1_m, const std::vector<double>& theta_n, const std::vector<double>& theta_np1_m, const std::vector<double>& theta_i_n, const std::vector<double>& theta_i_np1_m, const std::vector<double>& s, const double& dt, const std::vector<double>& rho, const std::vector<double>& k_np1_m_im12, const std::vector<double>& k_np1_m_ip12, const BoundaryConditions aTopBC, const double& TopFluxRate, const BoundaryConditions aBottomBC, const double& BottomFluxRate, const SnowStation& Xdata, SalinityTransport& Salinity, const SalinityMixingModels& SALINITY_MIXING);

		// Solver control variables
		const static double REQUIRED_ACCURACY_H, convergencecriterionthreshold, MAX_ALLOWED_DELTA_H;
		const static size_t INCR_ITER, DECR_ITER, MAX_ITER, BS_MAX_ITER;
		const static double MIN_VAL_TIMESTEP, MAX_VAL_TIMESTEP, MIN_DT_FOR_INFILTRATION;
		const static double SF_epsilon;
};
#endif //End of ReSolver1d.h
