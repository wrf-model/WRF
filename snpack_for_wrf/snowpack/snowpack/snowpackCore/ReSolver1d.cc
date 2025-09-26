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
#include <snowpack/snowpackCore/ReSolver1d.h>
#include <snowpack/vanGenuchten.h>
#include <snowpack/snowpackCore/SalinityTransport.h>
#include <snowpack/Utils.h>
#include <snowpack/snowpackCore/Snowpack.h>
#ifdef CLAPACK
	// Matching C data types with FORTRAN data types (taken from f2c.h):
	typedef long int integer;
	typedef double doublereal;

	// Declare the function interfaces with the LAPACK library (taken from clapack.h):
	extern "C" {
		/* Subroutine */ int dgesvd_(char *jobu, char *jobvt, integer *m, integer *n,
		doublereal *a, integer *lda, doublereal *s, doublereal *u, integer *
		ldu, doublereal *vt, integer *ldvt, doublereal *work, integer *lwork,
		integer *info);

		/* Subroutine */ int dgesdd_(char *jobz, integer *m, integer *n, doublereal *
		a, integer *lda, doublereal *s, doublereal *u, integer *ldu,
		doublereal *vt, integer *ldvt, doublereal *work, integer *lwork,
		integer *iwork, integer *info);

		/* Subroutine */ int dgtsv_(integer *n, integer *nrhs, doublereal *dl,
		doublereal *d__, doublereal *du, doublereal *b, integer *ldb, integer
		*info);
	}
#endif
#include <string.h>


using namespace std;
using namespace mio;

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#endif

const double ReSolver1d::max_theta_ice = 0.99;	//An ice pore space of around 5% is a reasonable value: K. M. Golden et al. The Percolation Phase Transition in Sea Ice, Science 282, 2238 (1998), doi: 10.1126/science.282.5397.2238

//Setting convergence criteria and numerical limits
const double ReSolver1d::REQUIRED_ACCURACY_H = 1E-6;		//Required accuracy for the Richard solver: this is for the delta h convergence criterion
const double ReSolver1d::REQUIRED_ACCURACY_THETA = 1E-5;	//Required accuracy for the Richard solver: this is for the delta theta convergence criterion. It is recommended to adjust PhaseChange::RE_theta_r in PhaseChanges.cc in case this value is changed.
								//Huang et al. (1996) proposes 0.0001 here (=1E-4). 1E-4 causes some mass balance problems. Therefore, it is set to 1E-5.
const double ReSolver1d::convergencecriterionthreshold = 0.8;	//Based on this value of theta_dim, either theta-based convergence is chosen, or h-based. Note we need to make this destinction, beacuse theta-based does not work close to saturation or with ponding.
const double ReSolver1d::MAX_ALLOWED_DELTA_H = 1E32;		//Set an upper threshold for the delta_h[i] that is allowed. The idea is that when delta_h for an iteration is too large, we have a too large time step and a rewind is necessary.
const size_t ReSolver1d::INCR_ITER = 5;				//Number of iterations for the Richard solver after which time step is increased.
const size_t ReSolver1d::DECR_ITER = 10;			//Number of iterations for the Richard solver after which time step is decreased.
const size_t ReSolver1d::MAX_ITER = 15;				//Maximum number of iterations for the Richard solver.
const double ReSolver1d::MIN_VAL_TIMESTEP = 1E-12;		//Minimum time step allowed in Richards solver. Don't set this too low (let's say 1E-40), becuase the calculations are then done at the limits of the floating point precision.
const double ReSolver1d::MAX_VAL_TIMESTEP = 900.;		//Maximum time step allowed in Richards solver.
const double ReSolver1d::MIN_DT_FOR_INFILTRATION=10.;		//If dt is above this value, do a rewind if the matrix cannot allow for all infiltrating water
const size_t ReSolver1d::BS_MAX_ITER = 5000;			//Maximum allowed number of iterations in the soil-freezing algorithm.
const double ReSolver1d::SF_epsilon = 1E-4;			//Required accuracy for the root finding algorithm when solving soil freezing/thawing.


ReSolver1d::ReSolver1d(const SnowpackConfig& cfg, const bool& matrix_part)
           : surfacefluxrate(0.), soilsurfacesourceflux(0.), variant(),
             iwatertransportmodel_snow(BUCKET), iwatertransportmodel_soil(BUCKET),
             watertransportmodel_snow("BUCKET"), watertransportmodel_soil("BUCKET"), BottomBC(FREEDRAINAGE), K_AverageType(ARITHMETICMEAN),
             enable_pref_flow(false), pref_flow_param_th(0.), pref_flow_param_N(0.), pref_flow_param_heterogeneity_factor(1.),
             sn_dt(IOUtils::nodata), allow_surface_ponding(false), lateral_flow(false), matrix(false), SalinityTransportSolver(SalinityTransport::IMPLICIT),
             dz(), z(), dz_up(), dz_down(), dz_()
{
	cfg.getValue("VARIANT", "SnowpackAdvanced", variant);

	//Allow for water ponding on the surface in case of high infilitration fluxes
	cfg.getValue("WATER_LAYER", "SnowpackAdvanced", allow_surface_ponding);

	//Calculation time step in seconds as derived from CALCULATION_STEP_LENGTH
	double calculation_step_length = cfg.get("CALCULATION_STEP_LENGTH", "Snowpack");
	sn_dt = M_TO_S(calculation_step_length);

	//Water transport model snow
	cfg.getValue("WATERTRANSPORTMODEL_SNOW", "SnowpackAdvanced", watertransportmodel_snow);
	iwatertransportmodel_snow=UNDEFINED;
	if (watertransportmodel_snow=="BUCKET") {
		iwatertransportmodel_snow=BUCKET;
	} else if (watertransportmodel_snow=="NIED") {
		iwatertransportmodel_snow=NIED;
	} else if (watertransportmodel_snow=="RICHARDSEQUATION") {
		iwatertransportmodel_snow=RICHARDSEQUATION;
	}

	//Water transport model soil
	cfg.getValue("WATERTRANSPORTMODEL_SOIL", "SnowpackAdvanced", watertransportmodel_soil);
	iwatertransportmodel_soil=UNDEFINED;
	if (watertransportmodel_soil=="BUCKET") {
		iwatertransportmodel_soil=BUCKET;
	} else if (watertransportmodel_soil=="NIED") {
		iwatertransportmodel_soil=NIED;
	} else if (watertransportmodel_soil=="RICHARDSEQUATION") {
		iwatertransportmodel_soil=RICHARDSEQUATION;
	}

	//Set lower boundary condition
	std::string tmp_lb_cond_waterflux;
	cfg.getValue("LB_COND_WATERFLUX", "SnowpackAdvanced", tmp_lb_cond_waterflux);
	if (tmp_lb_cond_waterflux=="DIRICHLET") {
		BottomBC=DIRICHLET;
	} else if (tmp_lb_cond_waterflux=="WATERTABLE") {
		BottomBC=WATERTABLE;
	} else if (tmp_lb_cond_waterflux=="FREEDRAINAGE") {
		BottomBC=FREEDRAINAGE;
	} else if (tmp_lb_cond_waterflux=="GRAVITATIONALDRAINAGE") {
		BottomBC=GRAVITATIONALDRAINAGE;
	} else if (tmp_lb_cond_waterflux=="SEEPAGE") {
		BottomBC=SEEPAGEBOUNDARY;
	} else if (tmp_lb_cond_waterflux=="SEAICE") {
		BottomBC=SEAICE;
	}

	//Check for preferential flow
	cfg.getValue("PREF_FLOW", "SnowpackAdvanced", enable_pref_flow);
	cfg.getValue("PREF_FLOW_PARAM_TH", "SnowpackAdvanced", pref_flow_param_th);
	cfg.getValue("PREF_FLOW_PARAM_N", "SnowpackAdvanced", pref_flow_param_N);
	cfg.getValue("PREF_FLOW_PARAM_HETEROGENEITY_FACTOR", "SnowpackAdvanced", pref_flow_param_heterogeneity_factor);

	//Set averaging method for hydraulic conductivity at the layer interfaces
	std::string tmp_avg_method_K;
	if(matrix_part) {
		// Setting the values for matrix domain
		cfg.getValue("AVG_METHOD_HYDRAULIC_CONDUCTIVITY", "SnowpackAdvanced", tmp_avg_method_K);
		if (tmp_avg_method_K=="ARITHMETICMEAN") {
			K_AverageType=ARITHMETICMEAN;
		} else if (tmp_avg_method_K=="GEOMETRICMEAN") {
			K_AverageType=GEOMETRICMEAN;
		} else if (tmp_avg_method_K=="HARMONICMEAN") {
			K_AverageType=HARMONICMEAN;
		} else if (tmp_avg_method_K=="MINIMUMVALUE") {
			K_AverageType=MINIMUMVALUE;
		} else if (tmp_avg_method_K=="UPSTREAM") {
			K_AverageType=UPSTREAM;
		} else {
			prn_msg( __FILE__, __LINE__, "err", Date(), "Unknown averaging method for hydraulic conductivity (key: AVG_METHOD_HYDRAULIC_CONDUCTIVITY).");
			throw;
		}
	} else {
		// Setting the values for preferential flow domain
		cfg.getValue("AVG_METHOD_HYDRAULIC_CONDUCTIVITY_PREF_FLOW", "SnowpackAdvanced", tmp_avg_method_K);
		if (tmp_avg_method_K=="ARITHMETICMEAN") {
			K_AverageType=ARITHMETICMEAN;
		} else if (tmp_avg_method_K=="GEOMETRICMEAN") {
			K_AverageType=GEOMETRICMEAN;
		} else if (tmp_avg_method_K=="HARMONICMEAN") {
			K_AverageType=HARMONICMEAN;
		} else if (tmp_avg_method_K=="MINIMUMVALUE") {
			K_AverageType=MINIMUMVALUE;
		} else if (tmp_avg_method_K=="UPSTREAM") {
			K_AverageType=UPSTREAM;
		} else {
			prn_msg( __FILE__, __LINE__, "err", Date(), "Unknown averaging method for hydraulic conductivity (key: AVG_METHOD_HYDRAULIC_CONDUCTIVITY_PREF_FLOW).");
			throw;
		}
	}

	std::string tmp_SalinityTransportSolver = "EXPLICIT";
	cfg.getValue("SALINITYTRANSPORT_SOLVER", "SnowpackSeaice", tmp_SalinityTransportSolver, IOUtils::nothrow);
	if (tmp_SalinityTransportSolver=="EXPLICIT") {
		SalinityTransportSolver=SalinityTransport::EXPLICIT;
	} else if (tmp_SalinityTransportSolver=="IMPLICIT") {
		SalinityTransportSolver=SalinityTransport::IMPLICIT;
	} else if (tmp_SalinityTransportSolver=="IMPLICIT2") {
		SalinityTransportSolver=SalinityTransport::IMPLICIT2;
	} else {
		prn_msg( __FILE__, __LINE__, "err", Date(), "Unknown solver method for SalinityTransport (key: SALINITYTRANSPORT_SOLVER).");
		throw;
	}

	//Check if lateral flow is considered
	lateral_flow = false;
	cfg.getValue("LATERAL_FLOW", "Alpine3D", lateral_flow, IOUtils::nothrow);

	//Assign if we solve matrix or prefential flow
	matrix=matrix_part;
}


/**
 * @brief Solving system of equations using Thomas Algorithm \n
 * The following function solves a tridiagonal system of equations using Thomas Algorithm \n
 * @author http://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm
 * @param n number of equations
 * @param a sub-diagonal (means it is the diagonal below the main diagonal) -- indexed from 0..n-2
 * @param b the main diagonal
 * @param c sup-diagonal (means it is the diagonal above the main diagonal) -- indexed from 0..n-2
 * @param v right part
 * @param x the solution
 */
int ReSolver1d::TDMASolver (size_t n, double *a, double *b, double *c, double *v, double *x)
{
	// See: http://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm
	// This solver is very rapid, but has the problem that when elements of the matrix get very small, relative to each other, precision problems propagate.
	// A better option is to use the DGTSV solver from LAPACK, as it does partial pivoting, although even that is not always enough.
	// See for explanation: http://en.wikipedia.org/wiki/Pivot_element#Partial_and_complete_pivoting
        /**
         * n - number of equations
         * a - sub-diagonal (means it is the diagonal below the main diagonal) -- indexed from 0..n-2
         * b - the main diagonal
         * c - sup-diagonal (means it is the diagonal above the main diagonal) -- indexed from 0..n-2
         * v - right part
         * x - the solution
         * Return value: 0 = succes, otherwise: error
         */
	if (b[n-1] == 0.) return -1;		// This will cause division by 0, so return with error code.

	size_t i;
	for (i = 1; i < n; i++) {
		if (b[i-1]==0.) return -1;	// This will cause division by 0, so return with error code.
		const double m = a[i-1] / b[i-1];
		b[i] = b[i] - m * c[i-1];
		v[i] = v[i] - m * v[i-1];
	}

	x[n-1] = v[n-1] / b[n-1];

	i = n - 1;
	while (i-- > 0) {
		x[i] = (v[i] - c[i] * x[i+1]) / b[i];
	}

	return 0;
}

/**
 * @brief Solving system of equations using matrix inversion \n
 * The following function solves a tridiagonal system of equations using \n
 * Moore-Penrose matrix inversion, using SVD, giving a pseudo-inverse of a. \n
 * @author Nander Wever
 * @param m number of rows in matrix
 * @param n number of columns in matrix
 * @param lda leading dimension of matrix
 * @param a pointer to top-left corner of matrix to inverse
 */
#ifdef CLAPACK
int ReSolver1d::pinv(int m, int n, int lda, double *a)
// NOTE: inverse matrix is returned in "a" as well. Make sure to send a copy to pinv if you want to keep the original matrix.
//
// Returned status by dgesvd_/dgesdd_:
//  INFO    (output) INTEGER
//          = 0:  successful exit.
//          < 0:  if INFO = -i, the i-th argument had an illegal value.
//          > 0:  For DGESVD: DBDSQR did not converge, INFO specifies how many. Superdiagonals of an intermediate bidiagonal form B did not converge to zero.
//                For DGESDD: DBDSDC did not converge, updating process failed. The algorithm failed to compute an singular value. The update process of divide and conquer failed.
{
	//Switch for dgesvd/dgesdd
	const bool useOptimezedSVD=true;	//True: dgesdd is used, which is faster, but requires more memory, compared to dgesvd. Note that when dgesdd failes, the function tries dgesvd. When that fails as well, the program is terminated.
						//I encountered some numerical issues with dgesdd, depending on settings in feenable (likely over- or underflow). So setting this switch to false gives a safe mode.
						//Note: there are some bugreports for dgesdd.

	//1D-Array for singular values:
	const int nSV = m < n ? m : n;	//nSV: number of singular values
	double *s = (double *)calloc(nSV * sizeof *s, sizeof(double));

	//2D-Arrays for matrices U and Vt:
	double *u = (double *)calloc((m*m) * sizeof(*u), sizeof(double));
	double *vt = (double *)calloc((n*n) * sizeof(*vt), sizeof(double));

	//Setup workspace
	double workspaceSize;
	double *work = &workspaceSize;	//From the documentation, it's not clear to me whether with lwork=-1, work can be considered as an array, or will always return 1 value. If it might return an array, not enough memory is allocated here.
	int lwork = -1;
	int *iwork = (int *)calloc(8*nSV, sizeof(double));
	int info = 0;
	char jobu='A';
	char jobvt='A';

	//Call dgesvd_/dgesdd_ with lwork = -1 to determine optimal workspace size:
	if (useOptimezedSVD) {
		dgesdd_(&jobu, (integer*) &m, (integer*) &n, a, (integer*) &lda, s, u, (integer*) &m, vt, (integer*) &n, work, (integer*) &lwork, (integer*) iwork, (integer*) &info);
	} else {
		dgesvd_(&jobu, &jobvt, (integer*) &m, (integer*) &n, a, (integer*) &lda, s, u, (integer*) &m, vt, (integer*) &n, work, (integer*) &lwork, (integer*) &info);
	}
	if (info!=0) {
		printf ("ERROR in ReSolver1d.cc: SVD [determining optimal workspace size failed]: info = %d\n", info);
		throw;
	}

	//Now, workspace size is returned in work.
	lwork = int(work[0]);

	//Then allocate work again for the determined workspace size
	work = (double *)calloc(lwork * sizeof(*work), sizeof(double));

	// Call dgesdd_ to do the actual computation:
	if (useOptimezedSVD) {
		dgesdd_(&jobu, (integer*) &m, (integer*) &n, a, (integer*) &lda, s, u, (integer*) &m, vt, (integer*) &n, work, (integer*) &lwork, (integer*) iwork, (integer*) &info);
		if (info>0) {		//if info>0, there is a convergence problem, probably because the matrix is ill-conditioned. We can try dgesvd before giving up.
			printf ("ERROR in ReSolver1d.cc: DGESDD failed [info = %d]. Falling back on DGESVD.\n", info);
			dgesvd_(&jobu, &jobvt, (integer*) &m, (integer*) &n, a, (integer*) &lda, s, u, (integer*) &m, vt, (integer*) &n, work, (integer*) &lwork, (integer*) &info);
			//Now, workspace size is returned in work.
			lwork = int(work[0]);

			//Then allocate work again for the determined workspace size
			free(work);								//First free the previously allocated work.
			work = (double *)calloc(lwork * sizeof(*work), sizeof(double));		//Reallocate work.

			dgesvd_(&jobu, &jobvt, (integer*) &m, (integer*) &n, a, (integer*) &lda, s, u, (integer*) &m, vt, (integer*) &n, work, (integer*) &lwork, (integer*) &info);
		}
	} else {
		dgesvd_(&jobu, &jobvt, (integer*) &m, (integer*) &n, a, (integer*) &lda, s, u, (integer*) &m, vt, (integer*) &n, work, (integer*) &lwork, (integer*) &info);
	}
	if (info!=0) {
		printf ("ERROR in ReSolver1d.cc: DGESVD failed [info = %d]. No solution found with currently used time step.\n", info);
		return -1;
	}

	//dgesvd/dgesdd gave us:
	//A=U.S.Vt
	//We now need:
	//A'=V.(S').Ut
	//Because S is a diagonal matrix, S' is just (1/S)
	//Be careful: comparing the results with for example MATLAB is not directly possible.
	//Singular value decomposition is not unique, e.g. sign can differ for U and Vt, but due to multiplication, this sign reversal cancels out.

	std::vector<double> dinv(m*n, 0.);
	int i, j, k;

	//Calculate pseudo-inverse: Step 1: U.S':
	for (i = m-1; i >= 0; i--) {
		for (j = n-1; j >= 0; j--) {
			//dinv[j*m+i]=0.;							//This line is not necessary anymore, as we reset array to zero where it is declared.
			for (k = n-1; k >= 0; k--) {						//Do matrix multiplications
				if (j==k) {							//j==k: this is because s is actually a diagonal matrix, and therefore, represented as a vector.
					if(s[k]!=0.) {						//NANDER TODO HACK: I'm not happy with this solution, but how to circumvent underflow? I just want 0. in that case.
						dinv[j*m+i]+=(vt[i*n+k]*(double(1.)/s[k]));	//Note: vt needs to be transposed AND is in FORTRAN matrix notation, therefore, the indices appear normal again.
					}
				}
			}
		}
	}
	//Step 2: (U.S')*VT
	for (i = m-1; i >= 0; i--) {
		for (j = n-1; j >= 0; j--) {
			a[j*m+i]=0;
			for (k = n-1; k >= 0; k--) {				//Do matrix multiplications
				a[j*m+i]+=(dinv[k*m+j]*u[k*n+i]);		//Note: u needs to be transposed AND is in FORTRAN matrix notation.
			}
		}
	}

	//Free memory
	free(work);
	free(iwork);
	free(s);
	free(u);
	free(vt);

	return 0;
}
#else
int ReSolver1d::pinv(int /*m*/, int /*n*/, int /*lda*/, double */*a*/) {
	//Nothing so far
	throw IOException("This private method is not implemented when the LAPACK and BLAS libraries are not installed", AT);
}
#endif


/**
 * @brief Initializing the finite differences grid for solving Richards Equation \n
 * The function fills vectors z, dz, dz_, dz_up and dz_down.
 * @author Nander Wever
 * @param EMS ElementData structure
 * @param lowernode The lower node of the domain for which Richards Equation is solved. The function assumes that lowernode is contained in EMS.
 * @param uppernode The upper node of the domain for which Richards Equation is solved. The function assumes that uppernode is contained in EMS.
 */
void ReSolver1d::InitializeGrid(const vector<ElementData>& EMS, const size_t& lowernode, const size_t& uppernode)
{
	// Give vectors correct size
	z.resize(uppernode+1);
	dz.resize(uppernode+1);
	dz_.resize(uppernode+1);
	dz_up.resize(uppernode+1);
	dz_down.resize(uppernode+1);

	// Initialize grid
	double totalheight=0.;				//tracking the total height of the column
	size_t i, j;					//layer indices
	for (i = lowernode; i <= uppernode; i++) {
		dz[i]=EMS[i].L;
		totalheight+=dz[i];
		if(i==0) {	//Lowest element
			z[i]=.5*dz[i];
		} else {
			z[i]=z[i-1]+(dz[i-1]/2.+(dz[i])/2.);
		}
	}

	//Additional domain initialization: determine grid cell sizes, and node distances.
	//See for additional details on finite differences scheme with varying grid cell size: Rathfelder (1994).
	double tmpheight1=0., tmpheight2=0.;
	for (j=lowernode; j<=uppernode; j++) {
		//Distance to lower node
		if(j!=lowernode) {
			dz_down[j]=z[j]-z[j-1];
		}
		tmpheight1+=dz_down[j];
		//Distance to upper node
		if(j!=uppernode) {
			dz_up[j]=z[j+1]-z[j];
		}
		tmpheight2+=dz_up[j];
		//Mean distance
		//dz_[j]=0.5*(dz_down[j]+dz_up[j]);	//This is the definition of dz_ by Rathfelder (2004). However, it does not work, results in mass balance errors.
		dz_[j]=dz[j];				//This works.
	}
	dz_down[lowernode]=totalheight-tmpheight1;
	dz_up[uppernode]=totalheight-tmpheight2;

	return;
}


/**
 * @brief Assemble the right hand side and assess the fluxes
 * @author Nander Wever
 * @param Takes many arguments, but in the future, many variables should become owned by the class.
 */
std::vector<double> ReSolver1d::AssembleRHS( const size_t& lowernode,
					     const size_t& uppernode,
					     const std::vector<double>& h_np1_m,
					     const std::vector<double>& theta_n,
					     const std::vector<double>& theta_np1_m,
					     const std::vector<double>& theta_i_n,
					     const std::vector<double>& theta_i_np1_m,
					     const std::vector<double>& s,
					     const double& dt,
					     const std:: vector<double>& rho,
					     const std::vector<double>& k_np1_m_im12,
					     const std::vector<double>& k_np1_m_ip12,
					     const BoundaryConditions aTopBC,
					     const double& TopFluxRate,
					     const BoundaryConditions aBottomBC,
					     const double& BottomFluxRate,
					     const SnowStation& Xdata,
					     SalinityTransport& Salinity,
					     const SalinityMixingModels& SALINITY_MIXING
					)
{
	size_t nE = (uppernode - lowernode) + 1;
	std::vector<double> term_up(nE, 0.);			//Variable to support construction of the R.H.S. (R_mpfd in Celia et al., 1990).
	std::vector<double> term_down(nE, 0.);			//Variable to support construction of the R.H.S. (R_mpfd in Celia et al., 1990).
	std::vector<double> term_up_crho(nE, 0.);		//Variable to support construction of the R.H.S. (R_mpfd in Celia et al., 1990), assuming constant density.
	std::vector<double> term_down_crho(nE, 0.);		//Variable to support construction of the R.H.S. (R_mpfd in Celia et al., 1990), assuming constant density.

	std::vector<double> r_mpfd(nE, 0.);			//Variable to support construction of the R.H.S. (R_mpfd in Celia et al., 1990).

	for (size_t i = lowernode; i <= uppernode; i++) {	//We loop over all Richards solver domain layers
		// Calculate density related variables
		double rho_up = 0;
		double rho_down = 0;
		double drho_up = 0;
		double drho_down = 0;
		if(i==uppernode) {
			rho_up = rho[i]; //0.5 * (rho[i] + Constants::density_water);
			drho_up = 0.; //(Constants::density_water - rho[i]) / dz_up[i];
		} else {
			rho_up = 0.5 * (rho[i] + rho[i+1]);
			drho_up = (rho[i+1] - rho[i]) / (dz_up[i]);
		}
		if(i==lowernode) {
			rho_down = 0.5 * (rho[i] + Constants::density_water + SeaIce::betaS * Xdata.Seaice->OceanSalinity);
			drho_down = (rho[i] - Constants::density_water + SeaIce::betaS * Xdata.Seaice->OceanSalinity) / dz_down[i];
		} else {
			rho_down = 0.5 * (rho[i] + rho[i-1]);
			drho_down = (rho[i] - rho[i-1]) / (dz_down[i]);
		}

		//Determine R:
		term_up[i]=0.;
		term_down[i]=0.;
		term_up_crho[i]=0.;
		term_down_crho[i]=0.;

		//Fill R.H.S. vector
		//Note: the gravity term is not explicitly in Celia et al (1990). It is just z[i], as pressure head should already be scaled by rho_water * g. Then it is taken outside the nabla, by using the chain rule.
		if(i==uppernode) {
			if(aTopBC==NEUMANN) {		//Neumann, following Equation 4 in McCord, WRR (1991).
				term_up[i]=(TopFluxRate)*dz_up[i] - Xdata.cos_sl*(dz_up[i]*k_np1_m_ip12[i]) - Xdata.cos_sl*(dz_up[i]*(k_np1_m_ip12[i]/rho_up)*(z[i]+0.5*(dz_up[i]))) * (drho_up);
				term_up_crho[i]=(TopFluxRate)*dz_up[i] - Xdata.cos_sl*(dz_up[i]*k_np1_m_ip12[i]);
			} else {			//Dirichlet
				term_up[i]=0.;
				term_up_crho[i]=0.;
				std::cout << "DIRICHLET NOT CORRECTLY IMPLEMENTED.\n";
				throw;
			}
		} else {
			term_up[i]=(k_np1_m_ip12[i]/rho_up)*(h_np1_m[i+1]*rho[i+1]-h_np1_m[i]*rho[i]);
			term_up_crho[i]=k_np1_m_ip12[i]*(h_np1_m[i+1]-h_np1_m[i]);
			//term_up_crho[i]=(k_np1_m_ip12[i]/rho_up)*(h_np1_m[i+1]*rho_up-h_np1_m[i]*rho_up);
		}
		if(i==lowernode) {
			if(aBottomBC == NEUMANN) {	//Neumann, following Equation 4 in McCord, WRR (1991).
				term_down[i]=(BottomFluxRate)*dz_down[i] - Xdata.cos_sl*(dz_down[i]*k_np1_m_im12[i]) - Xdata.cos_sl*(dz_down[i]*(k_np1_m_im12[i]/rho_down)*(z[i]-0.5*(dz_down[i]))) * (drho_down);
				term_down_crho[i]=term_down[i] + Xdata.cos_sl*(dz_down[i]*(k_np1_m_im12[i]/rho_down)*(z[i]-0.5*(dz_down[i]))) * (drho_down);
			} else {			//Dirichlet (r_mpfd[lowernode] should equal 0.)
				term_down[i]=(term_up[i]/dz_up[i])*dz_down[i];
				term_down[i]+=
					(+ Xdata.cos_sl*((k_np1_m_ip12[i]-k_np1_m_im12[i])/(dz_[i]))
					+ Xdata.cos_sl*(((k_np1_m_ip12[i]/rho_up)*(z[i]+0.5*(dz_up[i])) * (drho_up) - (k_np1_m_im12[i]/rho_down)*(z[i]-0.5*(dz_down[i])) * (drho_down)) / (dz_[i]))
					- (1./dt)*((theta_np1_m[i]-theta_n[i]) + (theta_i_np1_m[i]-theta_i_n[i])*(Constants::density_ice/Constants::density_water))
					+ s[i]) * dz_[i] * dz_down[i];
				term_down_crho[i]=term_down[i] + Xdata.cos_sl*(dz_down[i]*(k_np1_m_im12[i]/rho_down)*(z[i]-0.5*(dz_down[i]))) * (drho_down);
			}
		} else {
			term_down[i]=(k_np1_m_im12[i]/rho_down)*(h_np1_m[i]*rho[i] - h_np1_m[i-1]*rho[i-1]);
			term_down_crho[i]=k_np1_m_im12[i]*(h_np1_m[i] - h_np1_m[i-1]);
			//term_down_crho[i]=(k_np1_m_im12[i]/rho_down)*(h_np1_m[i]*rho_down - h_np1_m[i-1]*rho_down);
		}

		//RHS eq. 17 in Celia et al. (1990):
		r_mpfd[i]=(1./(dz_[i]))*((term_up[i]/dz_up[i])-(term_down[i]/dz_down[i]))
			+ Xdata.cos_sl*((k_np1_m_ip12[i]-k_np1_m_im12[i])/(dz_[i]))
			+ Xdata.cos_sl*(((k_np1_m_ip12[i]/rho_up)*(z[i]+0.5*(dz_up[i])) * (drho_up) - (k_np1_m_im12[i]/rho_down)*(z[i]-0.5*(dz_down[i])) * (drho_down)) / (dz_[i]))
			- (1./dt)*((theta_np1_m[i]-theta_n[i]) + (theta_i_np1_m[i]-theta_i_n[i])*(Constants::density_ice/Constants::density_water))
			+ s[i];

		//Determine individual fluxes (defined as: negative = upward, positive = downward):
		Salinity.dz_[i] = dz_[i];
		Salinity.dz_up[i] = dz_up[i];
		Salinity.dz_down[i] = dz_down[i];
		Salinity.theta1[i] = theta_n[i];
		Salinity.D[i] = 1E-10;
		switch (SALINITY_MIXING) {
			case NONE:
			{
				//No mixing: salt fluxes only based on net fluxes (1) and second term (2) is zero.
				Salinity.flux_up[i] = term_up[i]/dz_up[i]
					+ Xdata.cos_sl*(k_np1_m_ip12[i])
					+ Xdata.cos_sl*((k_np1_m_ip12[i]/rho_up)*(z[i]+0.5*(dz_up[i]))) * (drho_up);
				Salinity.flux_up_2[i] = 0.; //Salinity.flux_up[i] - term_up_crho[i]/dz_up[i] - Xdata.cos_sl*(k_np1_m_ip12[i]);

				Salinity.flux_down[i] = term_down[i]/dz_down[i]
					+ Xdata.cos_sl*(k_np1_m_im12[i])
					+ Xdata.cos_sl*((k_np1_m_im12[i]/rho_down)*(z[i]-0.5*(dz_down[i]))) * (drho_down);
				Salinity.flux_down_2[i] = 0.; //Salinity.flux_down[i] - term_down_crho[i]/dz_down[i] - Xdata.cos_sl*(k_np1_m_im12[i]);
				break;
			}
			case CAPILLARY_GRAVITY:
			{
				//Separate capillary (1) and gravity term (2)
				Salinity.flux_up[i] = term_up[i]/dz_up[i];				// Capillary term
				Salinity.flux_up_2[i] =							// Gravity term
					+ Xdata.cos_sl*(k_np1_m_ip12[i])
					+ Xdata.cos_sl*((k_np1_m_ip12[i]/rho_up)*(z[i]+0.5*(dz_up[i]))) * (drho_up);

				Salinity.flux_down[i] = term_down[i]/dz_down[i];			// Capillary term
				Salinity.flux_down_2[i] =						// Gravity term
					+ Xdata.cos_sl*(k_np1_m_im12[i])
					+ Xdata.cos_sl*((k_np1_m_im12[i]/rho_down)*(z[i]-0.5*(dz_down[i]))) * (drho_down);
				break;
			}
			case DENSITY_DIFFERENCE:
			{
				//Separate term dependent on density (1) and term constant density (2)
				Salinity.flux_up[i] = term_up_crho[i]/dz_up[i]
						+ Xdata.cos_sl*k_np1_m_ip12[i];
				Salinity.flux_up_2[i] = (term_up[i]/dz_up[i] - term_up_crho[i]/dz_up[i])
						+ Xdata.cos_sl*((k_np1_m_ip12[i]/rho_up)*(z[i]+0.5*(dz_up[i]))) * (drho_up);

				Salinity.flux_down[i] = term_down_crho[i]/dz_down[i]
						+ Xdata.cos_sl*k_np1_m_im12[i];
				Salinity.flux_down_2[i] = (term_down[i]/dz_down[i] - term_down_crho[i]/dz_down[i])
						+ Xdata.cos_sl*((k_np1_m_im12[i]/rho_down)*(z[i]-0.5*(dz_down[i]))) * (drho_down);
				break;
			}
			case DENSITY_GRAVITY:
			{
				//Separate term constant density (1) and term dependent on density (2)
				Salinity.flux_up[i] = term_up[i]/dz_up[i] + Xdata.cos_sl*(k_np1_m_ip12[i]*rho[i]);
				Salinity.flux_up_2[i] =
					+ Xdata.cos_sl*(k_np1_m_ip12[i]*(rho_up-rho[i]))
					+ Xdata.cos_sl*(k_np1_m_ip12[i]*(z[i]+0.5*(dz_[i]))-k_np1_m_im12[i]*(z[i]-0.5*(dz_[i]))) * (drho_up);

				Salinity.flux_down[i] = term_down[i]/dz_down[i] + Xdata.cos_sl*(k_np1_m_im12[i]*rho[i]);
				Salinity.flux_down_2[i] =
					+ Xdata.cos_sl*(k_np1_m_im12[i]*(rho_down-rho[i]))
					+ Xdata.cos_sl*(k_np1_m_ip12[i]*(z[i]+0.5*(dz_[i]))-k_np1_m_im12[i]*(z[i]-0.5*(dz_[i]))) * (drho_down);
				break;
			}
		}
		if(i==uppernode) {
			Salinity.flux_up[uppernode]+=Salinity.flux_up_2[uppernode];
			Salinity.flux_up_2[uppernode]=0.;
		}
		if(i==lowernode) {
			Salinity.flux_down[lowernode]+=Salinity.flux_down_2[lowernode];
			Salinity.flux_down_2[lowernode]=0.;
		}
	}


	// r_mpfd is an approximation of how far one is away from the solution. So in case of Dirichlet boundaries, we are *at* the solution:
	if(aTopBC==DIRICHLET) r_mpfd[uppernode]=0.;
	if(aBottomBC==DIRICHLET) r_mpfd[lowernode]=0.;


	// return the right hand side vector
	return r_mpfd;
}



/**
 * @brief Solve Richards Equation \n
 * Solve Richards Equation \n
 * @author Nander Wever
 * @param Xdata
 * @param Sdata
 */
void ReSolver1d::SolveRichardsEquation(SnowStation& Xdata, SurfaceFluxes& Sdata, double& ql)
{
// Main publications about the development of this code:
// - Wever, N., Fierz, C., Mitterer, C., Hirashima, H., and Lehning, M.: Solving Richards Equation for snow improves snowpack meltwater runoff estimations in detailed multi-layer snowpack model, The Cryosphere, 8, 257-274, doi:10.5194/tc-8-257-2014, 2014.
//   First publication describing the implementation of Richards equation in SNOWPACK.
// - Wever, N., Schmid, L., Heilig, A., Eisen, O., Fierz, C., and Lehning, M.: Verification of the multi-layer SNOWPACK model with different water transport schemes, The Cryosphere, 9, 2271-2293, doi:10.5194/tc-9-2271-2015, 2015.
//   In-depth model verification and description of the soil part with Richards equation (soil properties and soil phase changes).
// - Wever, N., Würzer, S., Fierz, C., and Lehning, M.: Simulating ice layer formation under the presence of preferential flow in layered snowpacks, The Cryosphere, 10, 2731-2744, doi:10.5194/tc-10-2731-2016, 2016.
//   Describes the preferential flow implementation via the dual-domain approach.
//
// Main references used to write this code, as a reference to understand the working of this code:
// - Celia, M, A., Bouloutas, E.T., Zabra, R.L. (1990) A general mass-conservative numerical solution for the unsaturated flow equation Water Resources Research (26:7), 1483-1496.
//   Describes the main part of the solver (Picard iteration of the mixed form of the Richards equation, see eq. 17 in that paper)
// - Bastos de Vasconcellos, C.A., Amorim, J.C.C. (2001) in Proceedings of COBEM 2001, Fluid Mechanics, Vol. 8, 139-148.
//   Is using Celia (1990), but I mention it here because it provides an excellent overview in matrix-notation of how to exactly implement the Picard-iteration and fill in the matrices and vectors, also in case fo Dirichlet or Neumann boundary conditions.
//   It will certainly help to understand why the matrices are filled the way they are (see Equations 18, 19, 20 and 21). Note: in Eq. 19, the coefficient for alpha is missing a minus-sign and in Eq. 20, ignore the alpha_1 coefficient, because that one does not exist.
// - Hari Prasad, K.S., Mohan Kumar, M.S and Sekhar, M. (2001) Modelling flow through unsaturated zones: Sensitivity to unsaturated soil properties. Sadhana vol. 26, no 6. 517–528. DOI: 10.1007/BF02703457
//   Provides an explicit description of how to implement Dirichlet boundary conditions.
// - Huang, K., Mohanty, B.P., van Genuchten, M.Th. (1996) A new convergence criterion for the modified Picard iteration method to solve variably saturated flow equation Journal of Hydrology (178), 69-91.
//   Describes a better convergence criterion, based on theta. It is implemented here, but it is not always behaving stable, likely when the convergence criterion is set too low.
// - McCord, J.T. (1991) Application of second-type boundaries in unsaturated flow modelling Water Resources Research (27:12), 3257-3260.
//   Describes the boundary conditions applied here (Neumann and Dirichlet).
// - Paniconi, C., Putti, M. (1994) A comparison of Picard and Newton iteration in the numerical solution of multidimensional variably saturated flow problems. Water Resources Research (30:12) 3357-3374.
//   Describes the time stepping mechanism implemented here.
// - Schaap, M.G. and van Genuchten, M.Th. (2006) A modified Mualem-van Genuchten formulation for improved description of the hydraulic conductivity near saturation. Vadoze Zone Journal (5) 27-34,
//   Describes the small changes made to the van Genuchten parameterization of soil, to better deal with saturated conditions (theta_m and h_s). This, I deactivated, as a better solution seems to be:
// - Ippisch, O., Vogel, H.-J. and Bastian, P. (2006) Validity limits for the van Genuchten-Mualem model and implications for parameter estimation and numerical simulation. Adv. in Water Res. (29), 1780-1789.
//   Describes inconsistenties in near saturation and suggest using an air entry pressure he.
// - Zhang, X., Bengough, A.G., Crawford, J.W. and Young, I.M. (2002) Efficient methods for solving water flow in variably saturated soils under prescribed flux infiltration. Journal of Hydrology (260) 75-87.
//   Describes an more efficient way of treating the gravity term, and describes a way of estimating the hydraulic conductivity at the nodes.
// - Li, C.W. (1993) A Simplified Newton Iteration Method With Linear Finite Elements for Transient Unsaturated Flow. Water Resources Research (29:4) 965-971.
//   Describes how the hydraulic conductivity at the interface nodes can be approximated by integration.
// - Yamaguchi, S., Katsushima, T., Sato, A. and Kumakura, T. (2010) Water retention curve of snow with different grain sizes. Cold Regions Science and Technology (64:2) 87-93.
//   Describes van Genuchten parameters (alpha and n) for snow, based on measurements. Experiments for around 550 kg/m^3 dense snow.
// - Yamaguchi, S., Watanabe, K., Katsushima, T., Sato, A., Kumakura, T. (2012) Dependence of the water retention curve of snow on snow characteristics. Annals of Glaciology 53(61). doi: 10.3189/2012AoG61A001
//   Update of the Yamaguchi (2010) paper: describes van Genuchten parameters (alpha and n) for snow, based on measurements. Experiments for a range of snow densities.
// - Hirashima, H., Yamaguchi, S., Sato, A., and Lehning, M. (2010) Numerical modeling of liquid water movement through layered snow based on new measurements of the water retention curve. Cold Regions Science and Technology (64:2), 94-103.
//   Describes van Genuchten parameters for snow, based on Yamaguchi (2010).
// - Rathfelder, K and Abriola, L. (1994) Mass conservative numerical solutions of the head-based Richards equation. Water Resources Research (30:9) 2579-2586.
//   Describes an implementation of variable grid spacing for solving Richards Equation in 1D.

// KNOWN ISSUES:
//	- When using Richars-Equation, the PhaseChange-scheme may cause snow temperatures to be above 273.15K. As long as they are transient, it should not considered
//        to be a problem. Future optimization here may be possible. It's likely related to the fact that when solving Richards Equation, basically every snow layer has some amount
//        of water in it, albeit very little. But this causes some difficulties in determining whether snow is wet or dry, so whether the nodes are at melting temperature.
//      - In case of floating point exceptions: ReSolver1d has some problems when (in CMake) DEBUG_ARITHM is set to ON. You can safely set it to OFF, as the code detects for
//        illegal operations itself and takes appropriate measures, like choosing another solver or reducing the time step.
//	- In case of non-convergence of the solver: Numerical problems were found when the SNOWPACK time step is larger than 15 minutes. For example caused by the settling routine,
//	  which is based on 15 minute time steps. So before digging further in the problem, make sure you run SNOWPACK with 15 minute time steps.
//
//      A lot of problems arise from non convergence in the solver and very small time steps. A common reason for this is filling of the model domain. Clear examples:
//      - Evaporation from soil in dry limit. This causes numerical troubles, but it is also not realistic to force a certain amount of evaporation from a near-dry soil (the water
//	  is just not there!). Set LIMITEDFLUXEVAPORATION or LIMITEDFLUX as top boundary condition to be safe.
//      - Infiltration in soil in wet limit. This can cause numerical trouble, but it is also not realistic. You cannot put more water in the domain then there is room for.
//        Typically this may occur with strong infiltration rates (high precipitation rates) or large values in the source/sink term (potentially from the canopy module).
//	  So for example: never use 10 cm of soil with DIRICHLET lower boundary condition and NEUMANN on top. The soil then may saturate very quickly. No melt water can infilitrate
//        the soil anymore, but starts ponding. In reality, such cases would lead to a water layer, or overland flow, which is not considered in SNOWPACK (yet).
//        Set LIMITEDFLUXINFILTRATION or LIMITEDFLUX as lower boundary condition to be safe.

// TODO IN FUTURE DEVELOPMENT
// -  Implement a strategy what to do with the rejected infiltrating water in case of LIMITEDFLUX and LIMITEDFLUXINFILTRATION. Either built-up a water layer (theta[WATER]==1) on top (real ponding),
//    or write it out in a kind of overland flow variable.

	// define if matrix or preferential flow
	const int WATERINDEX = (matrix == true) ? (WATER) : (WATER_PREF);

	//
	// BEGIN OF SETTINGS
	//
	const BoundaryConditions TopBC = LIMITEDFLUX;				//Top boundary condition (DIRICHLET, NEUMANN or LIMITEDFLUX). Recommended: LIMITEDFLUX, i.e. too much evaporation from dry soil or snow or too much infilitration in wet soil or snow is prohibited.
		//In case you select one of the LIMITEDFLUX options, specify whether these are only for soil, for snow or for both:
		const bool LIMITEDFLUXEVAPORATION_soil=true;
		const bool LIMITEDFLUXEVAPORATION_snow=true;
		const bool LIMITEDFLUXINFILTRATION_soil=true;
		const bool LIMITEDFLUXINFILTRATION_snow=true;
		const bool LIMITEDFLUXINFILTRATION_snowsoil=true;		//This switch allows to limit the infiltration flux from snow into soil, when the snowpack is solved with the Bucket or NIED water transport scheme.
	const bool LIMITEDFLUXSOURCESINKTERM=true;				//Check if the source/sink term can be absorbed by the matrix.
	const bool ApplyIceImpedance=false;					//Apply impedance on hydraulic conductivity in case of soil freezing. See: Zhao et al. (1997) and Hansson et al. (2004)  [Dall'Amicao, 2011].

	//Setting some program flow variables
	const bool SafeMode=true;			//Enable safemode only when necessary, for example in operational runs or Alpine3D simulations. It rescues simulations that do not converge, at the cost of violating mass balance.

	const bool WriteDebugOutputput=false;		//true: debugging output is printed


//Set the defaults based on whether CLAPACK is available
#ifdef CLAPACK
	SOLVERS PreferredSolver=DGTSV;			//Choose either DGESVD, DGTSV or TDMA.
	const bool AllowSwitchSolver=true;		//If true: solver DGESVD will be tried when DGTSV fails. There is a trade-off here between matrix inversion and smaller time steps. In case of many layers (>100), DGESVD can become very slow, so in case DGTSV does not find a solution, it may be more efficient to
	//take a smaller time step than to do full matrix inversion.
#else
	SOLVERS PreferredSolver=TDMA;			//Without CLAPACK, only TDMA is available.
#endif
							// DGTSV : (recommended) This function does matrix inversion giving the knowledge matrix A is a tridiagonal matrix (fast). Does partial pivoting to stabelize numerical solutions. But partial pivoting is not always enough.
							// DGESVD: (recommended only when stability issues are encounterd) This function does full matrix inversion (slow, but most reliable and maybe useful for finding 2D solutions one day).
							// TDMA  : (recommended only when libraries BLAS and LAPACK are not available) This function does matrix inversion giving the knowledge matrix A is a tridiagonal matrix (really fast). Does no (partial) pivoting at all, so big risks of numerical troubles.

	SOLVERS ActiveSolver=PreferredSolver;		//Set the ActiveSolver to the PreferredSolver. This is because the code tries to prevent "difficult" matrices to be solved by the DGTSV or TDMA algorithm, so we should be able to switch temporarily to another solver.


	//Set parameterization for hydraulic functions for snow
	const vanGenuchten::VanGenuchten_ModelTypesSnow VGModelTypeSnow=vanGenuchten::YAMAGUCHI2012;	//[Water retention curve]    Recommended: YAMAGUCHI2012   Set a VanGenuchten model for snow (relates pressure head to theta and vice versa)
	const vanGenuchten::K_Parameterizations K_PARAM=vanGenuchten::CALONNE;				//[Hydraulic conductivity]   Recommended: CALONNE         Implemented choices: SHIMIZU, CALONNE, based on Shimizu (1970) and Calonne (2012).
	const SalinityMixingModels SALINITY_MIXING = NONE;


	//
	// END OF SETTINGS
	// WARNING: Below this line, changes to initializations are likely to break the code!
	//


	//Check for water layer (presence of a pond) and add it to the surfacefluxrate:
	double backupWATERLAYER_Te = Constants::undefined;
	if(allow_surface_ponding == true && Xdata.getNumberOfElements() > Xdata.SoilNode) {
		if(Xdata.Edata[Xdata.getNumberOfElements()-1].theta[ICE] == 0. && Xdata.Edata[Xdata.getNumberOfElements()-1].theta[SOIL] == 0.) {
			surfacefluxrate += (Xdata.Edata[Xdata.getNumberOfElements()-1].theta[WATER] * Xdata.Edata[Xdata.getNumberOfElements()-1].L) / sn_dt;
			backupWATERLAYER_Te = Xdata.Edata[Xdata.getNumberOfElements()-1].Te;
			Xdata.reduceNumberOfElements(Xdata.getNumberOfElements()-1);
		} else {
			backupWATERLAYER_Te = Constants::undefined;
		}
	}


	//Initializing and defining Richards solver time domain
	double dt=10.;					//Set the initial time step for the Richard solver (in seconds). This time step should be smaller or equal to the SNOWPACK time step.
	bool boolFirstFunctionCall;			//true: first execution of this function, false: not the first execution of this function
	if (Xdata.ReSolver_dt>0.) {			//Retrieve last dt used in last performed time step. Note Xdata.ReSolver_dt<0 when initialized
		boolFirstFunctionCall=false;		//Subsequent call to ReSolver1d
		dt=Xdata.ReSolver_dt;			//Set time step to last used time step
 	} else {					//else it is the first time this function is called.
		boolFirstFunctionCall=true;		//Set this flag to true, so we know that no previous pressure head information is available, and we can only work with theta.
	}
	double TimeAdvance=0.;				//Time advance of the Richards solver


	//Initializing and defining Richards solver space domain
	const size_t nN=Xdata.getNumberOfNodes();	//Number of nodes
	const size_t nE=nN-1;				//Number of layers
	vector<ElementData>& EMS = Xdata.Edata;		//Create reference to SNOWPACK elements.
	vector<NodeData>& NDS = Xdata.Ndata;		//Create reference to SNOWPACK nodes.

	if ((nE == 0) || (iwatertransportmodel_snow != RICHARDSEQUATION && Xdata.SoilNode == 0)) return; //Nothing to do here!
	const size_t uppernode = (iwatertransportmodel_snow != RICHARDSEQUATION) ? (Xdata.SoilNode - 1) : (nE - 1);	//highest layer (top of snowpack, or top of soil in case of no soil)
	size_t lowernode=0;				//Lower node of Richards solver domain

	//Initializations of the convergence criteria
	double track_accuracy_h=0.;			//This variable tracks the accuracy of convergence for all h-convergence based layers.
	double track_accuracy_theta=0.;			//This variable tracks the accuracy of convergence for all theta-convergence based layers.
	double max_delta_h=0.;				//Tracks max_delta_h, to determine if our time step is too large. Note: this is different from checking the convergence criterion. This is just to check the time step. If a too large time step is used, big values of delta_h may arise, which cause pressure head to hit the singularities for dry and wet soil, and causes problems with the power functions in the Von Genuchten-Mualem model.
	bool boolConvergence=false;			//true: convergence is reached, false: convergence not reached
	double mass1=0, mass2=0, massbalanceerror=0.;	//Mass balance check variables.
	const double maxallowedmassbalanceerror=1E-6;	//This value is carefully chosen. It should be considered together with REQUIRED_ACCURACY_THETA and REQUIRED_ACCURACY_H
	double massbalanceerror_sum=0.;			//Sum of mass balance error over snowpack time step.


	//Initializations for summarizing statistics and some supporting variables, like indices, counters, etc.
	double accuracy=0.;				//Keeps track of reached accuracy.
	unsigned int niter=0;				//Counts iterations within one time step of the Richards solver
	unsigned int niter_snowpack_dt=0;		//Counts iterations within one time step of the SNOWPACK time domain
	unsigned int niter_nrewinds=0;			//Counts number of rewinds (i.e. a solution was not found and it is tried again with a smaller time step)
	unsigned int niter_seqrewinds=0;		//Counts number of sequential rewinds. We then decrease the time step more, when we encounter sequential rewinds.
	unsigned int seq_safemode=0;			//Counts the number of sequential SafeMode actions
	//Numerical performance statistics
	double stats_min_dt=MAX_VAL_TIMESTEP;		//Minimum RE time step in SNOWPACK time step, initialized in a way that the comparison will always work.
	double stats_max_dt=0.;				//Maximum RE time step in SNOWPACK time step, initialized in a way that the comparison will always work.
	int stats_nrewinds=0;				//Number of rewinds over the SNOWPACK time step.
	int stats_niters=0;				//Number of iterations over the SNOWPACK time step, excluding the ones before a rewind.
	int stats_nsteps=0;				//Number of time steps in the RE solver over the SNOWPACK time step.
	size_t bs_stats_totiter=0;			//Soil freezing/thawing solver: total number of iterations over all layers over the SNOWPACK time step,
	size_t bs_stats_maxiter=0;			//Soil freezing/thawing solver: maximum number of iterations in a certain layers over the SNOWPACK time step.
	//Counters, etc.
	size_t i, j, k;					//Counters for layers
	const size_t nmemstates=1;			//Number of memory states, used to store changes of delta_h between iterations. Currently not used, but possible use is to check if delta_h is blowing up.
	int memstate=0;					//Keeping track of the current memory index
	double h_d=0.;					//Lower limit for pressure head: definition of "completely dry". This value will be determined later.
	double min_theta=Constants::eps;


	//Initializing and declaring boundary conditions and flux variables
	BoundaryConditions aTopBC;			//Actual applied top boundary condition (can only be either Dirichlet or Neumann, as all the others can be translated in an application of either one of those two.)
	BoundaryConditions aBottomBC;			//Actual applied bottom boundary condition (can only be either Dirichlet or Neumann, as all the others can be translated in an application of either one of those two.)
	double htop=0., TopFluxRate=0.;			//Dirichlet (constant head) and Neumann (constant flux) upper boundary values respectively.
	double h_d_uppernode=0.;			//Used for LIMITEDFLUXEVAPORATION boundary condition.
	double hbottom=0., BottomFluxRate=0.;		//Dirichlet (constant head) and Neumann (constant flux) lower boundary values respectively.
	double actualtopflux=0;				//Stores the actual applied flux through top (positive is inflow).
	double refusedtopflux=0;			//Stores the difference in flux that was requested, but could not be applied
	double actualbottomflux=0;			//Stores the actual flux through the bottom (positive is outflow).
	double snowsoilinterfaceflux=0.;		//Stores the actual flux through the soil-snow interface (positive is flow into soil).
	double totalsourcetermflux=0.;			//Stores the total applied source term flux (it's a kind of boundary flux, but then in the middle of the domain).

	//Declare all numerical arrays and matrices:
	std::vector< std::vector<double> > delta_h(nmemstates, std::vector<double> (nE,0.));	//Change in pressure head per iteration
	std::vector<double> delta_h_dt(nE, 0.);		//Change in pressure head per time step.
	std::vector<double> delta_theta(nE, 0.);	//Change in volumetric water content per iteration
	std::vector<double> delta_theta_dt(nE, 0.);	//Change in volumetric water content per time step.
	std::vector<double> delta_theta_i(nE, 0.);	//Change in volumetric ice content per iteration
	std::vector<double> delta_theta_i_dt(nE, 0.);	//Change in volumetric ice content per time step.
	std::vector<double> delta_Te(nE, 0.);		//Change in element temperature per time step due to soil freezing/thawing.
	std::vector<double> delta_Te_i(nE, 0.);		//Change in element temperature per iteration time step due to soil freezing/thawing.
	std::vector<double> delta_Te_adv(nE, 0.);	//Change in element temperature per time step due to heat advection by the water flow.
	std::vector<double> delta_Te_adv_i(nE, 0.);	//Change in element temperature per iteration time step due to heat advection by the water flow.
	std::vector<double> rho(nE, 0.);		//Liquid density

	//std::vector<std::vector<double> > a(nE, std::vector<double> (nE, 0));	//Left hand side matrix. Note, we write immediately to ainv! But this is kept in to understand the original code.
	std::vector<double> ainv(nE*nE, 0.);			//Inverse of A, written down as a 1D array instead of a 2D array, with the translation: a[i][j]=ainv[i*nlayers+j]
	std::vector<double> ad(nE, 0.);				//The diagonal of matrix A, used for DGTSV
	std::vector<double> adu(nE, 0.);			//The upper second diagonal of matrix A, used for DGTSV
	std::vector<double> adl(nE, 0.);			//The lower second diagonal of matrix A, used for DGTSV

	std::vector<double> k_np1_m_ip12(nE, 0.);		//Hydraulic conductivity at the upper interface node
	std::vector<double> k_np1_m_im12(nE, 0.);		//Hydraulic conductivity at the lower interface node
	std::vector<double> h_np1_m(nE, 0.);			//Pressure head at beginning of an iteration.
	std::vector<double> h_n(nE, 0.);			//Pressure head at beginning of time step dt. Used to determine delta_h_dt, to better forecast value for next time step.
	std::vector<double> s(nE, 0.);				//Source/sink in terms of theta [m^3/m^3/s].
	std::vector<double> C(nE, 0.);				//Water capacity function. Specific moisture capacity (dtheta/dh), see Celia et al., (1990).
	std::vector<double> K(nE, 0.);				//Hydraulic conductivity function
	std::vector<double> impedance(nE, 0.);			//Impedance factor due to ice formation in matrix (see Dall'Amico, 2011);
	std::vector<double> Se(nE, 0.);				//Effective saturation, sometimes called dimensionless volumetric water content.
	std::vector<double> r_mpfd(nE, 0.);			//R_mpfd (see Celia et al, 1990).
	std::vector<double> r_mpfd2(nE, 0.);			//Copy of R_mpfd, used for DGTSV. Note: R_mpfd2 is overwritten by DGTSV, so we need a copy.
	std::vector<double> h_np1_mp1(nE, 0.);			//Pressure head for the solution time step in the next iteration
	std::vector<double> theta_np1_m(nE, 0.);		//Theta for the solution time step in the current iteration.
	std::vector<double> theta_np1_mp1(nE, 0.);		//Theta for the solution time step in the next iteration.
	std::vector<double> theta_n(nE, 0.);			//Theta at the current time step.
	std::vector<double> theta_d(nE, 0.);			//There is a singularity for dry soils, at theta=theta_r. There h -> OO. So we limit this. We define a pressure head that we consider "dry soil" (h_d) and then we calculate what theta belongs to this h_d.

	std::vector<double> theta_i_n(nE, 0.);			//Soil state, ice content at the beginning of the time step. Volumetric water content and NOT liquid water equivalent!
	std::vector<double> theta_i_np1_m(nE, 0.);		//Soil state, ice content at the beginning of the current iteration. Volumetric water content and NOT liquid water equivalent!
	std::vector<double> theta_i_np1_mp1(nE, 0.);		//Soil state, ice content at the next iteration. Volumetric water content and NOT liquid water equivalent!

	std::vector<double> dT(nE, 0.);				//Stores the energy needed to create theta_r from the ice matrix.
	std::vector<double> snowpackBACKUPTHETAICE(nE, 0.);	//Backup array for the initial SNOWPACK theta ice


	//Prevent buffering on the stdout when we write debugging output. In case of exceptions (program crashes), we don't loose any output which is still in the buffer and we can better track what went wrong.
	if(WriteDebugOutputput) setvbuf(stdout, NULL, _IONBF, 0);

#ifdef DEBUG_ARITHM
	if(boolFirstFunctionCall==true) {
		// Warn users for compilation with DEBUG_ARITHM = ON. The solver uses isnan and isinf to check if the time step is too large.
		prn_msg( __FILE__, __LINE__, "wrn", Date(), "SNOWPACK has been compiled with DEBUG_ARITHM set to ON. This will likely result in a \"Floating point exception\" when using Richards equation solver. It is strongly recommended to set DEBUG_ARITHM to OFF!");
	}
#endif

	if(boolFirstFunctionCall==true && matrix == true && enable_pref_flow == true && K_AverageType != GEOMETRICMEAN) {
		// Warn if enable_pref_flow == true and the K_AverageType != GEOMETRICMEAN.
		// Dual domain approach was designed using GEOMETRICMEAN and other combinations are unlikely to be made on purpose.
		prn_msg(__FILE__, __LINE__, "wrn", Date(), "PREF_FLOW = TRUE is expecting (for the matrix part) AVG_METHOD_HYDRAULIC_CONDUCTIVITY = GEOMETRICMEAN!");
	}

	if(WriteDebugOutputput) {
		if(matrix == true) {
			printf("RUNNING MATRIX_FLOW...\n");
		} else {
			printf("RUNNING PREF_FLOW...\n");
		}
	}

	//Backup SNOWPACK state
	for (i = lowernode; i <= uppernode; i++) {
		if (WriteDebugOutputput) std::cout << "RECEIVING at layer " << i << std::fixed << std::setprecision(10) <<  " air=" << EMS[i].theta[AIR] << " ice=" << EMS[i].theta[ICE] << " soil=" << EMS[i].theta[SOIL] << " water=" << EMS[i].theta[WATER] << " water_pref=" << EMS[i].theta[WATER_PREF] << " Te=" << EMS[i].Te << " L=" << EMS[i].L << " " << EMS[i].h << "\n" << std::setprecision(6) ;

		//Make backup of incoming values for theta[ICE]
		snowpackBACKUPTHETAICE[i]=EMS[i].theta[ICE];

		//Check min theta which is not 0.
		if (EMS[i].theta[WATERINDEX] > 0. && EMS[i].theta[WATERINDEX] < min_theta) min_theta = EMS[i].theta[WATERINDEX];
	}

	// Grid initialization (this needs to be done every time step, as snowpack layers will settle and thereby change height)
	InitializeGrid(EMS, lowernode, uppernode);

	//Now set hydraulic properties for each layer
	h_d=0.;							//Set definition of pressure head of completely dry to zero, we will determine it in the next loop.
	for (i = lowernode; i <= uppernode; i++) {
		if ( i >= Xdata.SoilNode ) {		//Snow
			if(EMS[i].theta[ICE]>max_theta_ice) {
				//Pure ice layers are a problem for Richards equation (of course...), so we limit the volumetric ice content to 99 %.
				const double tmp_excess_theta=(EMS[i].theta[ICE]-max_theta_ice)*(Constants::density_ice/Constants::density_water);
				//HACK: how to treat ice layers? The line below that is commented out makes the model spiral out of control...
				// Produce warning, but not when running for sea ice, because then ice layers are too common.
				//dT[i]+=tmp_excess_theta*(Constants::density_water/Constants::density_ice) / ((EMS[i].c[TEMPERATURE] * EMS[i].Rho) / ( Constants::density_ice * Constants::lh_fusion ));
				if(variant!="SEAICE") std::cout << "[W] ReSolver1d.cc: ICE LAYER --> WATER CREATED (" << tmp_excess_theta << "): i=" << i << " --- dT=" << dT[i] << " T=" << EMS[i].Te << " theta[WATER]=" << EMS[i].theta[WATER] << " theta[ICE]=" << EMS[i].theta[ICE] << "\n";
				EMS[i].theta[WATERINDEX]+=tmp_excess_theta;
				EMS[i].theta[ICE]-=tmp_excess_theta*(Constants::density_water/Constants::density_ice);
				EMS[i].theta[AIR]=1.-EMS[i].theta[ICE]-EMS[i].theta[WATER]-EMS[i].theta[WATER_PREF];
			}

			EMS[i].VG.SetVGParamsSnow(VGModelTypeSnow, K_PARAM, matrix, variant=="SEAICE");

			// Recheck pref flow area: some processes in SNOWPACK may change pore space and water contents, such that the pref flow area is not consistent to store all water.
			const double tmpPoreSpace = (1. - EMS[i].theta[ICE]) * (Constants::density_ice / Constants::density_water);
			EMS[i].PrefFlowArea = std::min(0.999*(1.-(EMS[i].theta[WATER]/tmpPoreSpace)), std::max(1.001*(EMS[i].theta[WATER_PREF]/tmpPoreSpace), EMS[i].PrefFlowArea));

			// Scale theta_s
			if(WATERINDEX==WATER_PREF) {
				EMS[i].VG.theta_s*=EMS[i].PrefFlowArea;
			} else {
				if(enable_pref_flow) {
					EMS[i].VG.theta_s*=(1.-EMS[i].PrefFlowArea);
				}
			}

			theta_i_n[i]=0.;						//This sounds strange for snow, but the idea is that ice in snow functions as soil in soil (being the matrix)
			EMS[i].meltfreeze_tk=Constants::meltfreeze_tk;	//For snow, we currently don't have anything with freezing point depression, as we have in soil.
		} else {				//Soil
			EMS[i].VG.SetVGParamsSoil();
			theta_i_n[i]=EMS[i].theta[ICE];
			//Get melting point that suffices partitioning pressure head into part for ice and part for water
			const double hw0=std::min(EMS[i].VG.h_e, EMS[i].VG.fromTHETAtoH(EMS[i].theta[WATER]+(EMS[i].theta[ICE]*(Constants::density_ice/Constants::density_water)), h_d));
			EMS[i].meltfreeze_tk=Constants::meltfreeze_tk+((Constants::g*Constants::meltfreeze_tk)/Constants::lh_fusion)*hw0;
		}

		//Determine what pressure head should be considered "dry".
		//Explanation: cold dry new snow layers are initialized with this value. We need to make sure that ALL the other layers have at least a higher pressure head when they contain at least a little bit of water. Else, various numerical troubles arise.
		//In case the value is too high, we get fluxes out of the completely dry snow layer, and a too low value causes many numerical difficulties as in that case, it represents a much stronger gradient in pressure head than necessary (many iterations and small time steps).
		//So we check for each particular layer what pressure head is associated with a theta[WATER] that is a smaller deviation from theta_r then the solver will resolve.
		const double tmp_head=EMS[i].VG.fromTHETAtoHforICE(EMS[i].VG.theta_r+(REQUIRED_ACCURACY_THETA/10.), h_d, theta_i_n[i]);
		if(h_d>tmp_head) h_d=tmp_head;
		if(i==uppernode) h_d_uppernode=tmp_head;	//We store this value in order to use it for the LIMITEDFLUXEVAPORATION
		if (WriteDebugOutputput)
			std::cout << "H_D at " << i << ": " << std::scientific << tmp_head << std::fixed << " [alpha: " << EMS[i].VG.alpha << "; m: " << EMS[i].VG.m << "; n: " << EMS[i].VG.n << "; Sc: " << EMS[i].VG.Sc << "]; h_e: " << EMS[i].VG.h_e << " min_theta: " << min_theta << "\n";
	}


	//Coupling of SNOWPACK domain to RE-solver domain. Makes sure the EMS.theta[XXX] are within the limits specified by the Van Genuchten parameterizations.
	for (i = lowernode; i <= uppernode; i++) {
		//Now calculate the theta that should be considered "dry soil".
		theta_d[i]=EMS[i].VG.fromHtoTHETAforICE(h_d, 0.);

		//Now check if this case is not too extreme
		const double fact=1000.;
		if(theta_d[i]<EMS[i].VG.theta_r+(REQUIRED_ACCURACY_THETA/fact)) {
			theta_d[i]=EMS[i].VG.theta_r+(REQUIRED_ACCURACY_THETA/fact);
		}

		//Now make sure that the water content in SNOWPACK's ElementData matches the soil settings (not too wet, not too dry):
		// 1) Not too wet
		if(EMS[i].theta[SOIL]>Constants::eps2) {		//For soil
			if(WATERINDEX==WATER){	//As for soil, we only use the matrix flow part, and we inhibit water flow in preferential flow, we should check this only for the matrix flow
				const double corr_factor = (boolFirstFunctionCall) ? (0.95) : (1.);
				if(EMS[i].theta[WATERINDEX]+(EMS[i].theta[ICE]*(Constants::density_ice/Constants::density_water)) > corr_factor*EMS[i].VG.theta_s) {
					EMS[i].theta[WATERINDEX]=corr_factor*EMS[i].VG.theta_s-(EMS[i].theta[ICE]*(Constants::density_ice/Constants::density_water));
				}
			}
		} else {						//For snow
			if(EMS[i].theta[WATERINDEX] > EMS[i].VG.theta_s) {
				EMS[i].theta[WATERINDEX]=EMS[i].VG.theta_s;
			}
		}

		// 2) Not too dry
		if(EMS[i].theta[SOIL]>Constants::eps2) {		//For soil
			if(matrix==true) {
				if ((EMS[i].theta[WATERINDEX]+(EMS[i].theta[ICE]*(Constants::density_ice/Constants::density_water))) < theta_d[i]) {
					EMS[i].theta[WATERINDEX]=theta_d[i]-(EMS[i].theta[ICE]*(Constants::density_ice/Constants::density_water));
				}
			} /* else {
				if (EMS[i].theta[WATER_PREF]<theta_d[i]) {
					EMS[i].theta[WATER_PREF]+=theta_d[i];
					EMS[i].theta[WATER]-=theta_d[i];
				}
				EMS[i].theta[AIR]=1.-EMS[i].theta[WATER]-EMS[i].theta[ICE]-EMS[i].theta[SOIL];
			} */
			// The part is blended out, as we suppress preferential flow in soil for the moment.
		} else {
			//For snow, we have to melt ice to create theta_r!!
			if(EMS[i].theta[WATERINDEX]<theta_d[i]) {
				const double tmp_missing_theta=(theta_d[i]-EMS[i].theta[WATERINDEX]);	//Not too dry (original)
				dT[i]+=tmp_missing_theta*(Constants::density_water/Constants::density_ice) / ((EMS[i].c[TEMPERATURE] * EMS[i].Rho) / ( Constants::density_ice * Constants::lh_fusion ));
				if (WriteDebugOutputput)
					std::cout << "WATER CREATED (" << tmp_missing_theta << "): i=" << i << " --- dT=" << dT[i] << " T=" << EMS[i].Te << "  theta[WATER]=" << EMS[i].theta[WATER] << " theta[ICE]=" << EMS[i].theta[ICE] << "\n";
				EMS[i].theta[WATERINDEX]+=tmp_missing_theta;
				EMS[i].theta[ICE]-=tmp_missing_theta*(Constants::density_water/Constants::density_ice);
				EMS[i].VG.SetVGParamsSnow(VGModelTypeSnow, K_PARAM, matrix, variant=="SEAICE");    // Update the van Genuchten parameters
			}
		}


		//Now copy the EMS water content into the working arrays to solve Richards-equation (so this is the important part were this function is coupled to the rest of SNOWPACK).
		if(EMS[i].theta[SOIL]<Constants::eps2) {		//For snow
			h_n[i]=EMS[i].VG.fromTHETAtoHforICE(EMS[i].theta[WATERINDEX], h_d, theta_i_n[i]);
			if(variant=="SEAICE" && ((NDS[i].z < Xdata.Seaice->SeaLevel && fabs(EMS[i].theta[WATERINDEX]-EMS[i].VG.theta_s) < Constants::eps2) || (h_n[i]>EMS[i].VG.h_e-Constants::eps2 && EMS[i].h>EMS[i].VG.h_e-Constants::eps2)) && i>0 && h_n[i-1]>EMS[i].VG.h_e-Constants::eps2) {
				h_n[i]=EMS[i].h;
			}
		} else {
			//For soil, we take the matrix pressure head as lower boundary for the snow preferential flow
			if(matrix == false) theta_i_n[i]=EMS[i].theta[ICE];	// Keep ice contents in sync, as we skip soil freezing with preferential flow (in turn because we suppress preferential flow in soil)
			h_n[i]=EMS[i].VG.fromTHETAtoHforICE(EMS[i].theta[WATER], h_d, theta_i_n[i]);
		}
		theta_n[i]=EMS[i].VG.fromHtoTHETAforICE(h_n[i], theta_i_n[i]);	//This is the current theta, which we determine from h_n[i].

		//Determine source/sink term
		s[i]=0.;							//Reset source/sink term

		//Now add soilsurfacesourceflux (in case RemoveElements removed the lowest snow element):
		if(soilsurfacesourceflux>0. && i==Xdata.SoilNode) {		//We assign source flux in the lowest snow element if the source flux is >0. This can only be the case when we use RE for snow, so we don't have to check for this.
			//Remember: soilsurfacesourceflux=[m^3/m^2/s]
			s[i]+=soilsurfacesourceflux/dz[i];			//Soilsurfacesourceflux>0. if we remove the first snow element above the soil AND there are more snow layers (else it is a surfaceflux) AND we use RE for snow.
		}

		//Add source/sink term from other parts of SNOWPACK (in particular Canopy.cc)
		s[i]+=EMS[i].lwc_source/sn_dt;
		EMS[i].lwc_source=0.;		// Now that we used the variable, reset it.
	}


	//Initialize upper boundary in case of Dirichlet
	if(TopBC==DIRICHLET) {
		aTopBC=DIRICHLET;
		htop=h_n[uppernode];
	}

	// Initialize upper boundary for evaporation
	surfacefluxrate += (ql/Constants::lh_vaporization)/Constants::density_water;
	Sdata.mass[SurfaceFluxes::MS_EVAPORATION] += ql*sn_dt/Constants::lh_vaporization;
	ql = 0.; //We dealt with ql, so set it to 0, only to be possibly modified at the end of the function.

	//Important: We have to be aware that the previous time step may be too large for the infiltration flux in the current time step. Then, too much of the infiltration flux may be rejected.
	//           Two mechanisms to prevent this are: provide a better estimate of the necessery time step (done here), and try trigger a rewind with smaller time step first, before limiting the infilitration flux (done later).
	if((TopBC == LIMITEDFLUXINFILTRATION || TopBC == LIMITEDFLUX) && (TopFluxRate>0.) && (
	      (LIMITEDFLUXINFILTRATION_soil==true && Xdata.SoilNode==nE)
	        || (LIMITEDFLUXINFILTRATION_snowsoil==true && Xdata.SoilNode<nE && (uppernode+1)==Xdata.SoilNode)
	            || (LIMITEDFLUXINFILTRATION_snow==true && Xdata.SoilNode<nE))) {
		// Improve estimate of required time step to accomodate for all infiltrating water
		dt=std::min(dt, std::max(MIN_DT_FOR_INFILTRATION, (dz[uppernode]*(EMS[uppernode].VG.theta_s - (theta_n[uppernode] + theta_i_n[uppernode]))/surfacefluxrate)));
	}

	//Initialize lower boundary in case of Dirichlet
	if(BottomBC==DIRICHLET) {
		hbottom=h_n[lowernode];
	}

	//Initialize lower boundary in case of WATERTABLE: saturated
	if(BottomBC==WATERTABLE) {
		aBottomBC=DIRICHLET;
		hbottom=EMS[lowernode].VG.h_e;
		h_n[lowernode]=hbottom;
		theta_n[lowernode]=EMS[lowernode].VG.fromHtoTHETAforICE(h_n[lowernode], theta_i_n[lowernode]);
	}

	//Initialize lower boundary in case of SEAICE: prescribe pressure of water column at lowest ice node.
	if(BottomBC==SEAICE) {
		if(Xdata.Seaice == NULL) {
			prn_msg( __FILE__, __LINE__, "err", Date(), "[Xdata.SeaIce==NULL] You can only use LB_COND_WATERFLUX = SEAICE for simulations with sea ice!");
			throw;
		} else {
			Xdata.Seaice->updateFreeboard(Xdata);
		}
		aBottomBC=DIRICHLET;
		hbottom=std::min((Xdata.Seaice->SeaLevel - NDS[lowernode].z - .5 * EMS[lowernode].L), NDS[uppernode+1].z); // Keep hbottom smaller than depth of simulation domain.
		h_n[lowernode]=hbottom;
		EMS[lowernode].salinity += SeaIce::OceanSalinity * (EMS[lowernode].VG.fromHtoTHETAforICE(h_n[lowernode], theta_i_n[lowernode]) - EMS[lowernode].theta[WATER]);
		EMS[lowernode].theta[WATER] = theta_n[lowernode] = EMS[lowernode].VG.fromHtoTHETAforICE(h_n[lowernode], theta_i_n[lowernode]);
		EMS[lowernode].updDensity();
	}

	SalinityTransport Salinity(nE);

	//Note: there are 2 iterations. First, the iteration starts to match the Richards solver time step to the SNOWPACK time step. Simple example: assume SNOWPACK time step is 15 minutes and
	//Richards solver time step is 1 minute, there should be 15 iterations to match the solution to the SNOWPACK time step.
	//Then, for each time step of the Richard solver, iterations are necessary to find the solution to the equation.
	int nsteps=0;			//Counts the number of time steps in the Richards solver.
	bool StopLoop=false;		//Will switch to true when the integrated time step matches the SNOWPACK time step.
	bool DoRewindFlag=false;	//Will switch to true when the time step has to be redone with a smaller time step.

	//Determine mass at beginning of snowpack time step.
	mass1=0.;
	for (i = lowernode; i <= uppernode; i++) {
		mass1+=(theta_n[i]+(theta_i_n[i]*(Constants::density_ice/Constants::density_water)))*dz[i];
	}

	do {
		if(DoRewindFlag==false) {		//Only if we are not doing a rewind, we should increase the number of steps (else it basically is the same time step).
			nsteps++;			//Increase the number of steps
			niter_nrewinds=0;		//Reset rewind counter
		}

		Xdata.ReSolver_dt=dt;			//Store the last used time step.
		if ((TimeAdvance+dt)>=sn_dt) {		//If our time step is so large that the integrated time step will exceed the SNOWPACK time step, we limit the dt for the current time step...
			dt=sn_dt-TimeAdvance;		//...so it matches exactly the SNOWPACK time step.
			StopLoop=true;			//And we set the switch to stop the Richards solver.
		}
		TimeAdvance+=dt;			//Update the total time in this time step. This variable is used to match SNOWPACK time steps.

		//Prepare for next time step:
		niter=0;				//reset iter counter
		accuracy=0.;				//reset accuracy.


		//Set Solver
		ActiveSolver=PreferredSolver;		//We set the active solver to the preferred solver


		//Initialize values for the first iteration (iteration m)
		i = uppernode + 1;
		while (i-- > lowernode) {
			// Note, it is not possible to do an educated guess. The guess should be mass-conservative, which is very difficult to achieve.
			h_np1_m[i]=h_n[i];
			theta_np1_m[i]=theta_n[i];
			theta_i_np1_m[i]=theta_i_n[i];
		}

		//Write out initial water content
		if (WriteDebugOutputput) {
			for (i = lowernode; i <= uppernode; i++) {
				std::cout << "ITER: " << niter << " i: " << i << std::setprecision(15) << "; h_n: " << h_n[i] << " (h_np1: " << h_np1_m[i] << ") theta: " << theta_n[i] << std::setprecision(6) << "(" << EMS[i].VG.theta_r << "-" << EMS[i].VG.theta_s << ") ice: " << EMS[i].theta[ICE] << "/" << theta_i_n[i] << " (vg_params: " << EMS[i].VG.alpha << " " << EMS[i].VG.m << " " << EMS[i].VG.n << ") s[i]: " << s[i] << "\n";
			}
		}

		DoRewindFlag=false;							//Reset DoRewindFlag. We do it now, just before starting the solver, so we can use the status of this flag to initialize the solver properly.
		boolConvergence=false;							//Default is no convergence, until proven otherwise

		while (boolConvergence==false && DoRewindFlag==false) {			//In theory, this can create an endless loop, but for this, I put a throw in the code when no convergence is achieved, because then the situation is hopeless anyway.
			niter++;
			niter_snowpack_dt++;
			memstate++;
			int solver_result=0;

			//Prepare matrices
			//Update state properties
			for (i = lowernode; i <= uppernode; i++) {
				//Calculate theta from h
				theta_np1_m[i]=EMS[i].VG.fromHtoTHETAforICE(h_np1_m[i], theta_i_np1_m[i]);

				//Calculate dimensionless saturation Se, see Ippisch (2006), Eq. 11
				Se[i] = ((theta_np1_m[i] + (theta_i_np1_m[i]*(Constants::density_ice/Constants::density_water)) - EMS[i].VG.theta_r)/(EMS[i].VG.theta_s - EMS[i].VG.theta_r));
				if(Se[i]<0.) {
					//The formulation of Se[i] as used here may lead to very small negative values for Se. These are corrected here.
					if(Se[i]<-1E-12) std::cout << "WARNING: Se[" << i << "]=" << std::scientific << Se[i] << std::fixed << ".\n";	//This points towards a more serious problem, so give a warning...
					Se[i]=0.;
				}

				//Determine hydraulic conductivity, using the Mualem model (see Ippisch, 2006), Eq. 11
				K[i]=EMS[i].VG.ksat*sqrt(Se[i])*pow((1.-(pow(1.-pow(Se[i]*EMS[i].VG.Sc,(1./EMS[i].VG.m)),EMS[i].VG.m)))/(1.-pow(1.-pow(EMS[i].VG.Sc,(1./EMS[i].VG.m)), EMS[i].VG.m)),2.);

				//Applying ice impedance on K
				if(ApplyIceImpedance==true) {
					const double omega=7.;		//See Zhao et al. (1997) and Hansson et al. (2004)  [Dall'Amicao, 2011].
					if (i < Xdata.SoilNode && theta_i_np1_m[i]>0. && K[i]>0. ) {	//Only for soil and when there is ice in the soil
						//q=theta_i_np1_m[i]/(EMS[i].VG.theta_s-EMS[i].VG.theta_r);					//This is how Dall'Amico presents it, but it is based on Hanssen (2004), who defines it as:
						const double q = (theta_i_np1_m[i]*(Constants::density_ice/Constants::density_water))/((theta_np1_m[i]+(theta_i_np1_m[i]*(Constants::density_ice/Constants::density_water)))-EMS[i].VG.theta_r);		//Hanssen (2004).
						impedance[i]=pow(10., -1.*omega*q);
					} else {
						impedance[i]=1.;
					}
					K[i]*=impedance[i];
				}

				//Calculate the specific moisture capacity (which is derivative d.theta/d.h)
				if(h_np1_m[i]>EMS[i].VG.h_e) {
					C[i]=0.;
				} else {
					C[i]=EMS[i].VG.dtheta_dh(std::min(h_np1_m[i], EMS[i].VG.h_e));
					if(isnan(C[i])) solver_result=-1;
				}

				//Update liquid density and brine salinity
				rho[i] = Constants::density_water + SeaIce::betaS * EMS[i].salinity;

				if(WriteDebugOutputput) std::cout << "HYDPROPS: i=" << i << std::scientific << " Se=" << Se[i] << " C=" << C[i] << " K=" << K[i] << " rho=" << rho[i] << " sal=" << EMS[i].salinity << ".\n" << std::fixed;
			}

			for (i = lowernode; i <= uppernode; i++) {
				//Determine K at interface nodes
				// 1) Determine k_np1_m_ip12
				if (i!=uppernode) {
					//For the rest of the domain, we might have heterogeneous soils, so we have to approximate the hydraulic conductivity at the interface nodes.

					switch (K_AverageType) {
						case ARITHMETICMEAN:
						{
							k_np1_m_ip12[i]=.5*(K[i]+K[i+1]);
							break;
						}

						case GEOMETRICMEAN:
						{
							k_np1_m_ip12[i]=sqrt(K[i]*K[i+1]);
							break;
						}

						case HARMONICMEAN:
						{
							if(K[i]>0. && K[i+1]>0.) {
								k_np1_m_ip12[i]=((dz[i]+dz[i+1])/(dz[i+1]*K[i]+dz[i]*K[i+1]))*K[i]*K[i+1];
							} else {
								k_np1_m_ip12[i]=0.;
							}
							break;
						}

						case MINIMUMVALUE:
						{
							if(K[i]>K[i+1]) {
								k_np1_m_ip12[i]=K[i+1];
							} else {
								k_np1_m_ip12[i]=K[i];
							}
							break;
						}

						case UPSTREAM:
						{
							if (((h_np1_m[i+1]-h_np1_m[i])/dz_down[i+1]) - Xdata.cos_sl > 0.) {
								k_np1_m_ip12[i]=K[i];
							} else {
								k_np1_m_ip12[i]=K[i+1];
							}
							break;
						}
						default:
							InvalidArgumentException("Unknown K_AverageType value provided", AT);

					}
					if(matrix==false) {
						// When solving preferential flow, we suppress liquid water flow in soil by setting hydraulic conductivity to 0.
						if(i<Xdata.SoilNode) {
							K[i]=0.;
							k_np1_m_ip12[i]=0.;
						}
					}
				} else {
					//For the boundaries, we neglect gradients in K. This corresponds to the specified fluid flux boundary condition (Equation 4 of McCord, WRR, 1991).
					k_np1_m_ip12[i]=K[i];
				}

				// 2) Determine k_np1_m_im12
				if (i!=lowernode) {
					// The following statement needs to be true, else you won't have mass balance in the solver!
					k_np1_m_im12[i]=k_np1_m_ip12[i-1];
				} else {
					//For the boundaries, we neglect gradients in K. This corresponds to the specified fluid flux boundary condition (Equation 4 of McCord, WRR, 1991).
					k_np1_m_im12[i]=K[i];
				}
			}

			//Determine which and how boundary conditions should be applied:
			if (TopBC==DIRICHLET) {
				aTopBC=DIRICHLET;				//Set Dirichlet BC
				TopFluxRate=0.;					//Dirichlet BC, so no flux
				theta_np1_m[uppernode]=theta_n[uppernode];
			} else if (TopBC==NEUMANN) {
				//Note: TopFluxRate is defined as gradient over pressure head. For influx, pressure head is increasing with increasing height, so TopFluxRate is positive.
				//Units: surfacefluxrate=[m^3/m^2/s]
				aTopBC=NEUMANN;					//Set Neumann BC
				TopFluxRate=surfacefluxrate;			//Flux for Neumann BC
			} else if (TopBC==LIMITEDFLUXEVAPORATION || TopBC==LIMITEDFLUXINFILTRATION || TopBC==LIMITEDFLUX) {
				//Now check if the topflux is not too big or small, giving positive pressure heads. For example: during heavy rain, the rain rate can be much more than handled by the soil. The upper layer will blow up the model in this case, as it cannot deal with all the incoming water. So the fluxes should not exceed dry or saturated conditions.
				aTopBC=NEUMANN;					// Limited flux is technically just Neumann, but with limited fluxes.
				if(niter==1) TopFluxRate=surfacefluxrate;	// Initial guess for Neumann BC, plus we also only allow reductions during a time step with constant forcing, so we set it only at the first iteration.
				// Now reduce flux when necessary:
				if((TopBC == LIMITEDFLUXINFILTRATION || TopBC == LIMITEDFLUX) && (TopFluxRate>0.) && (
				     (LIMITEDFLUXINFILTRATION_soil==true && Xdata.SoilNode==nE)
				        || (LIMITEDFLUXINFILTRATION_snowsoil==true && Xdata.SoilNode<nE && (uppernode+1)==Xdata.SoilNode)
				           || (LIMITEDFLUXINFILTRATION_snow==true && Xdata.SoilNode<nE))) {
					// Influx condition
					// Determine the limiting flux, which is the flux that would fill the upper element:
					const double flux_compare = (dz[uppernode]*(EMS[uppernode].VG.theta_s - (theta_np1_m[uppernode] + theta_i_np1_m[uppernode]))/dt);
					if((0.999*flux_compare) < TopFluxRate) {			//If prescribed flux is too large:
						if(dt>MIN_DT_FOR_INFILTRATION) {			//Trigger rewind when the top layer cannot accomodate for all infiltrating flux
							solver_result=-1.;
						} else {						//Limit flux. Note: we multiply flux_compare with 0.999 because flux_compare can be
							TopFluxRate=std::max(0., (0.999*flux_compare));	//regarded as the asymptotic case from which we want to stay away a little.
						}
					}
				}
				if((TopBC == LIMITEDFLUXEVAPORATION || TopBC == LIMITEDFLUX) && (TopFluxRate<0.) && ((LIMITEDFLUXEVAPORATION_soil==true && (Xdata.SoilNode==nE || uppernode+1==Xdata.SoilNode)) || (LIMITEDFLUXEVAPORATION_snow==true && Xdata.SoilNode<nE))) {
					// Outflux condition
					const double head_compare=h_d_uppernode;
					const double flux_compare=k_np1_m_ip12[uppernode]*(((head_compare-h_np1_m[uppernode])/dz_up[uppernode]) + Xdata.cos_sl);
					if(flux_compare > TopFluxRate) {
						TopFluxRate=std::min(0., flux_compare);
					}
				}
				if(h_np1_m[uppernode]>EMS[uppernode].VG.h_e) {
					TopFluxRate=std::min(0., TopFluxRate);
				}
			} else if (TopBC==WATERTABLE) {
				std::cout << "ERROR in ReSolver1d.cc: WATERTABLE cannot be applied as top boundary condition (doesn't make sense)!\n";
				throw;
			} else if (TopBC==FREEDRAINAGE) {
				std::cout << "ERROR in ReSolver1d.cc: FREEDRAINAGE cannot be applied as top boundary condition (doesn't make sense)!\n";
				throw;
			} else if (TopBC==SEEPAGEBOUNDARY) {
				std::cout << "ERROR in ReSolver1d.cc: SEEPAGEBOUNDARY cannot be applied as top boundary condition (doesn't make sense)!\n";
				throw;
			} else if (TopBC==GRAVITATIONALDRAINAGE) {
				std::cout << "ERROR in ReSolver1d.cc: GRAVITATIONALDRAINAGE cannot be applied as top boundary condition (doesn't make sense)!\n";
				throw;
			}


			if (BottomBC==DIRICHLET) {
				aBottomBC=DIRICHLET;		//Set Dirichlet BC.
				BottomFluxRate=0.;		//Dirichlet BC, so no prescribed flux.
				theta_np1_m[lowernode]=theta_n[lowernode];
			} else if (BottomBC==WATERTABLE) {
				// Default water table
				aBottomBC=DIRICHLET;		//Water table is a Dirichlet BC.
				h_n[lowernode]=EMS[lowernode].VG.h_e;
				hbottom=h_n[lowernode];
				theta_np1_m[lowernode]=theta_n[lowernode];
				BottomFluxRate=0.;		//Dirichlet BC, so no prescribed flux.
			} else if (BottomBC==NEUMANN) {
				aBottomBC=NEUMANN;		//Set Neumann BC.
				//Note: BottomFluxRate is defined as gradient over pressure head. For outflux (drainage), pressure head is increasing with increasing height, so BottomFluxRate is positive.
				BottomFluxRate=0.0000005;	//Flux for Neumann BC.
			} else if (BottomBC==FREEDRAINAGE) {
				aBottomBC=NEUMANN;
				if(uppernode>0) {
					//First calculate flux between lowest and lowest+1 element.
					const double tmpgrad=((h_np1_m[lowernode+1]-h_np1_m[lowernode])/dz_up[lowernode]);	//Note: flux would be (tmpgrad * K).
					if((tmpgrad+Xdata.cos_sl) < 0.) {
						//In this case, we would create influx at lower boundary, which does not work with FREEDRAINAGE.
						//Then set zero flux:
						BottomFluxRate=0.;
					} else {
						//Now, prescribe flux at lower boundary equivalent to tmpgrad
						BottomFluxRate=(tmpgrad+Xdata.cos_sl)*k_np1_m_im12[lowernode];
					}
				} else {
					//With one element only, fall back to GRAVITATIONALDRAINAGE
					BottomFluxRate=k_np1_m_im12[lowernode];
				}
			} else if (BottomBC==SEEPAGEBOUNDARY) {
				//Neumann with flux=0 in case of unsaturated
				//Dirichlet with h_bottom=h_e in case of saturated
				if(h_n[lowernode]<EMS[lowernode].VG.h_e) {
					aBottomBC=NEUMANN;
					BottomFluxRate=0.;
				} else {
					aBottomBC=DIRICHLET;
					h_n[lowernode]=EMS[lowernode].VG.h_e;
					hbottom=h_n[lowernode];
					BottomFluxRate=0.;
				}
			} else if (BottomBC==GRAVITATIONALDRAINAGE) {
				// See: Xubin Zeng and Mark Decker (2008). Improving the Numerical Solution of Soil Moisture–Based Richards Equation for Land Models with a Deep or Shallow Water Table
				// http://dx.doi.org/10.1175/2008JHM1011.1
				aBottomBC=NEUMANN;
				BottomFluxRate=k_np1_m_im12[lowernode];
			} else if (BottomBC==SEAICE) {
				aBottomBC=DIRICHLET;
				if(niter==1) {
					// For a new iteration, do a quick update of sea ice mass and the lower boundary
					for (i = lowernode; i <= uppernode; i++) {
						EMS[i].theta[WATERINDEX]=theta_np1_m[i];
						EMS[i].updDensity();
						EMS[i].M=EMS[i].L*EMS[i].Rho;
					}
					Xdata.Seaice->updateFreeboard(Xdata);
					hbottom=std::min((Xdata.Seaice->SeaLevel - NDS[lowernode].z - .5 * EMS[lowernode].L), NDS[uppernode+1].z); // Keep hbottom smaller than depth of simulation domain.
				}
				h_n[lowernode] = hbottom;
				BottomFluxRate=0.;
				theta_np1_m[lowernode]=theta_n[lowernode];
			} else if (BottomBC==LIMITEDFLUX) {
				//Probably also not necessary.
				std::cout << "ERROR in ReSolver1d.cc: No implementation for LIMITEDFLUX lower boundary condition. Either choose a saturated DIRICHLET (lower boundary in water table), or choose GRAVITATIONAL or FREEDRAINAGE (lower boundary not in water table).\n";
				throw;
			} else if (BottomBC==LIMITEDFLUXEVAPORATION) {
				std::cout << "ERROR in ReSolver1d.cc: LIMITEDFLUXEVAPORATION cannot be applied as bottom boundary condition (doesn't make sense)!\n";
				throw;
			} else if (BottomBC==LIMITEDFLUXINFILTRATION) {
				std::cout << "ERROR in ReSolver1d.cc: LIMITEDFLUXINFILTRATION cannot be applied as bottom boundary condition (doesn't make sense)!\n";
				throw;
			}


			if (WriteDebugOutputput)
				std::cout << "BOUNDARYTOPFLUX: [ BC: " << TopBC << "] " << std::scientific << TopFluxRate << " " << surfacefluxrate << " " << theta_n[lowernode] << " " << K[lowernode] << " " << ((h_np1_mp1[lowernode])+(((TopFluxRate/k_np1_m_im12[lowernode])-1.)*dz_down[lowernode])) << " " << h_np1_mp1[lowernode] << " " << k_np1_m_im12[lowernode] << " " << (TopFluxRate/k_np1_m_im12[lowernode]) << "\n" << std::fixed;

			// Verify source/sink term
			totalsourcetermflux=0.;
			for (i = lowernode; i <= uppernode; i++) {
				if(s[i] != 0.) {
					if(LIMITEDFLUXSOURCESINKTERM==true) {
						const double tmp = s[i];
						// Determine the limiting influx:
						const double flux_compare_max =														//The limiting flux is (positive is inflow):
							std::max(0., (dz[i]*(EMS[i].VG.theta_s - (theta_np1_m[i] + theta_i_np1_m[i]))/dt)						// net flux that would lead to saturation of the layer
								+ ((i>lowernode) ? k_np1_m_im12[i]*(((h_np1_m[i]-h_np1_m[i-1])/dz_down[i]) + Xdata.cos_sl) : BottomFluxRate)		// plus what could leave below
								- ((i<uppernode) ? k_np1_m_ip12[i]*(((h_np1_m[i+1]-h_np1_m[i])/dz_up[i]) + Xdata.cos_sl) : TopFluxRate));		// minus what comes from above
						// Determine the limiting outflux (note we take theta_d as reference "dry")
						const double flux_compare_min =														//The limiting flux is (positive is outflow):
							std::max(0., (dz[i]*((theta_np1_m[i] + theta_i_np1_m[i]) - theta_d[i])/dt)							// net flux that would lead to drying of the layer
								- ((i>lowernode) ? k_np1_m_im12[i]*(((h_np1_m[i]-h_np1_m[i-1])/dz_down[i]) + Xdata.cos_sl) : BottomFluxRate)		// minus what will leave below
								+ ((i<uppernode) ? k_np1_m_ip12[i]*(((h_np1_m[i+1]-h_np1_m[i])/dz_up[i]) + Xdata.cos_sl) : TopFluxRate));		// plus what comes from above
						s[i] = std::min(std::max(s[i], -0.999*flux_compare_min), 0.999*flux_compare_max);

						if(fabs(tmp - s[i])>Constants::eps2) {
							if(dt>MIN_DT_FOR_INFILTRATION) {			//Trigger rewind when the layer cannot accomodate for the source/sink term
								s[i]=tmp;					//Reset source/sink term to original value
								solver_result=-1.;				//Trigger rewind
							} else {
								std::cout << "[W] ReSolver1d.cc: for layer " << i << ", prescribed source/sink term could not be applied with dt=" << dt << ". Term reduced from " << tmp << " to " << s[i] << ".\n";
							}
						}
					}

					//Update now the flux of water in/out of the model domain due to the source/sink term.
					totalsourcetermflux+=s[i]*dz[i];
				}
			}

			//Solve equation
			std::fill(ainv.begin(), ainv.end(), 0.);	//This is very important: with inverting the matrix, it may become non-tridiagonal! So we have to explicitly set its elements to 0, because some of the for-loops only touch the tridiagonal part of the matrix.
			for (i = lowernode; i <= uppernode; i++) {
				j=i;	//As matrix A is tridiagonal, it can be filled very efficiently. The notation of i and j is kept for clarity of the structure of A. However, only evaluating when i==j is required.


				// Calculate density related variables
				double rho_up = 0;
				double rho_down = 0;
				if(i==uppernode) {
					rho_up = 0.5 * (rho[i] + Constants::density_water);
				} else {
					rho_up = 0.5 * (rho[i] + rho[i+1]);
				}
				if(i==lowernode) {
					rho_down = 0.5 * (rho[i] + Constants::density_water + SeaIce::betaS * Xdata.Seaice->OceanSalinity);
				} else {
					rho_down = 0.5 * (rho[i] + rho[i-1]);
				}



				//This part is for the DGESVD/DGESDD solver, which uses full matrix a (ainv). We always need them, because in case DGTSV fails, we should be able to fall back on DGESVD/DGESDD:
				if(i==j) {
					//Set up the matrix diagonal
					ainv[j*(uppernode+1)+i]=(1./dt)*(C[i]/rho[i]);

					//The following two lines assume Neumann boundary conditions (for upper and lowernode, one of the terms drop out). If Dirichlet is used, this will be corrected later.
					if(i!=lowernode) ainv[j*(uppernode+1)+i]+=(1./dz_[i])*(k_np1_m_im12[i]/rho_down/dz_down[i]);
					if(i!=uppernode) ainv[j*(uppernode+1)+i]+=(1./dz_[i])*(k_np1_m_ip12[i]/rho_up/dz_up[i]);

					//Correct diagonal in case of Dirichlet
					if(aTopBC==DIRICHLET && i==uppernode) {
						ainv[i*(uppernode+1)+i]=1.;
					}
					if(aBottomBC==DIRICHLET && i==lowernode) {
						ainv[i*(uppernode+1)+i]=1.;
					}

					//Set up the matrix upper and lower diagonals
					if(i!=lowernode) ainv[i*(uppernode+1)+(i-1)]=(-1./dz_[i])*(k_np1_m_im12[i]/rho_down/dz_down[i]);
					if(i!=uppernode) ainv[i*(uppernode+1)+(i+1)]=(-1./dz_[i])*(k_np1_m_ip12[i]/rho_up/dz_up[i]);

					if(uppernode>0) {
						//Correct upper and lower diagonals in case of Dirichlet
						//HACK/TODO: check if this piece of code is actually correct. Why is condition uppernode>0 necessary here, but not when using DGTSV or TDMA solvers?
						if(aTopBC==DIRICHLET && i==uppernode) {
							ainv[(i-1)*(uppernode+1)+i]=0.;
							ainv[i*(uppernode+1)+(i-1)]=0.;
						}
						if(aBottomBC==DIRICHLET && i==lowernode) {
							ainv[(i+1)*(uppernode+1)+i]=0.;
							ainv[i*(uppernode+1)+(i+1)]=0.;
						}
					}
				}

				//This part is for the DGTSV or TDMA solver, that uses the fact that A is a tridiagonal matrix, so we only have to specify the diagonals and subdiagonals.
				if(ActiveSolver==DGTSV || ActiveSolver==TDMA ) {
					if(i==j) {
						//Set up the matrix diagonal
						ad[i]=(1./dt)*(C[i]/rho[i]);

						//The following two lines assume Neumann boundary conditions (for upper and lowernode, one of the terms drop out). If Dirichlet is used, this will be corrected later.
						if(i!=lowernode) ad[i]+=(1./dz_[i])*(k_np1_m_im12[i]/rho_down/dz_down[i]);
						if(i!=uppernode) ad[i]+=(1./dz_[i])*(k_np1_m_ip12[i]/rho_up/dz_up[i]);

						//Correct diagonal in case of Dirichlet
						if(aTopBC==DIRICHLET && i==uppernode) {
							ad[i]=1.;
						}
						if(aBottomBC==DIRICHLET && i==lowernode) {
							ad[i]=1.;
						}

						//Set up the matrix upper and lower diagonals
						if(i!=lowernode) adl[i-1]=-(1./dz_[i])*(k_np1_m_im12[i]/rho_down/dz_down[i]);
						if(i!=uppernode) adu[i]=-(1./dz_[i])*(k_np1_m_ip12[i]/rho_up/dz_up[i]);

						//Correct diagonals in case of Dirichlet
						if(aTopBC==DIRICHLET && i==uppernode) {
							adu[i-1]=0.;
							adl[i-1]=0.;
						}
						if(aBottomBC==DIRICHLET && i==lowernode) {
							adu[i]=0.;
							adl[i]=0.;
						}
					}
				}

				//We copy here the matrix to the ainv, which is passed to the SVD-routine later on. This ainv is altered externally, that's why we need a copy.
				//ainv[j*(uppernode+1)+i]=a[i][j];
			}

			r_mpfd = AssembleRHS(lowernode, uppernode, h_np1_m, theta_n, theta_np1_m, theta_i_n, theta_i_np1_m, s, dt, rho, k_np1_m_im12, k_np1_m_ip12, aTopBC, TopFluxRate, aBottomBC, BottomFluxRate, Xdata, Salinity, SALINITY_MIXING);
			r_mpfd2 = r_mpfd;			// We make a copy for use with DGTSV and TDMA solvers.

			if(variant=="SEAICE" && SalinityTransportSolver==SalinityTransport::EXPLICIT && Salinity.VerifyCFL(dt)==false) {
				printf("CFL failed for dt=%.10f\n", dt);
				solver_result=-1;
			}
			if(variant=="SEAICE" && (SalinityTransportSolver==SalinityTransport::IMPLICIT || SalinityTransportSolver==SalinityTransport::IMPLICIT2) && Salinity.VerifyImplicitDt(dt)==false) {
				printf("ImplicitLimit failed for dt=%.10f\n", dt);
				solver_result=-1;
			}


			//Before solving the system of equations, reset convergence tracking variables:
			track_accuracy_h=0.;
			track_accuracy_theta=0.;
			accuracy=-1.;				//-1 is a flag. accuracy can only be positive, so when it is negative, we know that no layer did NOT converged yet.
			max_delta_h=0.;
			boolConvergence=true;			//We initialize it as true, and set it to false when necessary.
			mass2=0.;

			//Now call the designated solver.
			if(solver_result==0) {
				if (ActiveSolver==TDMA) {
					// Note: TDMA is very rapid, but has the problem that when elements in the matrix differ order of magnitudes, rounding errors can occur that destroy accuracy.
					// For this reason, it is better to use DGTSV solver, which does partial pivoting to prevent this. See: http://en.wikipedia.org/wiki/Pivot_element#Partial_and_complete_pivoting
					const size_t matrixdimensions=(uppernode-lowernode)+1;
					solver_result=TDMASolver(matrixdimensions, &adl[0], &ad[0], &adu[0], &r_mpfd[0], &r_mpfd2[0]);
				}

				if(ActiveSolver==DGTSV) {
#ifdef CLAPACK
					// Solver for Tridiagonal matrices, with partial pivoting.
					int info=0;
					const int matrixdimensions=int((uppernode-lowernode)+1);	// Cast from size_t to int is necessary, to interface correctly with LAPACK dgtsv_.
					const int vectordimensions=1;
					dgtsv_( (integer*) &matrixdimensions, (integer*) &vectordimensions, &adl[0], &ad[0], &adu[0], &r_mpfd2[0], (integer*) &matrixdimensions, (integer*) &info );

					if(info!=0) {
						//= 0: successful exit
						//< 0: if INFO = -i, the i-th argument had an illegal value
						//> 0: if INFO = i, U(i,i) is exactly zero, and the solution
						//    has not been computed.  The factorization has not been
						//    completed unless i = N.
						if(AllowSwitchSolver==true) {
							if(WriteDebugOutputput) std::cout << "ERROR in ReSolver1d.cc: DGTSV failed [info = " << info << "]. Trying DGESVD/DGESDD...\n";
							ActiveSolver=DGESVD;
						} else {
							if(WriteDebugOutputput) std::cout << "ERROR in ReSolver1d.cc: DGTSV failed [info = " << info << "]. Trying with smaller time step...\n";
							solver_result=-1;
						}
					}
#else
					throw InvalidArgumentException("you cannot use solver DGTSV when libraries BLAS and LAPACK are not installed. Either install these libraries, or choose solver TDMA", AT);
#endif
				}

				if(ActiveSolver==DGESVD) {
#ifdef CLAPACK
					//Do Moore-Penrose matrix inversion, using singular value decomposition (SVD), so we can write: H = A' * R
					solver_result=pinv(int((uppernode-lowernode)+1), int((uppernode-lowernode)+1), int((uppernode-lowernode)+1), &ainv[0]);
#else
					throw InvalidArgumentException("you cannot use solver DGESVD when libraries BLAS and LAPACK are not installed. Either install these libraries, or choose solver TDMA", AT);
#endif
				}


				//Apply new iteration solution

				//This is a little bit complicated. The problem started when we did soil freezing. If then suddenly an isnan is detected somewhere in the model domain, some part of the soil is already through the phasechange function, other parts not (maybe).
				//It is difficult to revert this soil freezing, so therefore, we need first to loop over i to determine the complete solution vector delta_h, and then an other loop over i to apply the new solution.
				//However, if a proper way to revert soil freezing is made, this extra loop can be removed.
				for (i = lowernode; i <= uppernode; i++) {
					//Determine delta h:
					if(ActiveSolver==DGESVD) {
						delta_h[memstate%nmemstates][i]=0.;
						//Note: after inverting, ainv is non tridiagonal, so we have to loop over all elements.
						for (k = lowernode; k <= uppernode; k++) {
						//for (k = std::min(uppernode, i+1); k >= std::max(lowernode, i-1); k--) {
							delta_h[memstate%nmemstates][i]+=ainv[i*(uppernode+1)+k]*r_mpfd[k];
						}
					} else {	//In case of DGTSV, solution is returned in r_mpfd2, overwriting original content.
						delta_h[memstate%nmemstates][i]=r_mpfd2[i];
					}
					if(isnan(delta_h[memstate%nmemstates][i])==true || isinf(delta_h[memstate%nmemstates][i])==true) {
						solver_result=-1;
					}
					delta_h[memstate%nmemstates][i]/=rho[i];
				}
			}


			//Apply Dirichlet BCs:
			if(aTopBC==DIRICHLET) {
				h_np1_mp1[uppernode]=htop;
				delta_h[memstate%nmemstates][uppernode]=0.;
				delta_theta[uppernode]=0.;
			}
			if(aBottomBC==DIRICHLET) {
				h_np1_mp1[lowernode]=hbottom;
				delta_h[memstate%nmemstates][lowernode]=0.;
				delta_theta[lowernode]=0.;
			}


			if (Xdata.Seaice != NULL && solver_result != -1) {
				AssembleRHS(lowernode, uppernode, h_np1_m, theta_n, theta_np1_m, theta_i_n, theta_i_np1_m, s, dt, rho, k_np1_m_im12, k_np1_m_ip12, aTopBC, TopFluxRate, aBottomBC, BottomFluxRate, Xdata, Salinity, SALINITY_MIXING);
				if(SalinityTransportSolver==SalinityTransport::EXPLICIT && Salinity.VerifyCFL(dt)==false) {
					printf("CFL failed for dt=%.10f @ second time\n", dt);
					solver_result=-1;
				}
				if((SalinityTransportSolver==SalinityTransport::IMPLICIT || SalinityTransportSolver==SalinityTransport::IMPLICIT2) && Salinity.VerifyImplicitDt(dt)==false) {
					printf("ImplicitLimit failed for dt=%.10f @ second time\n", dt);
					solver_result=-1;
				}
			}


			i = uppernode + 1;
			while (i-- > lowernode) {
				//Keep track of the maximum delta h, to detect possible model blow-ups.
				if(fabs(delta_h[memstate%nmemstates][i])>max_delta_h) {	// If change is too big and we are allowed to do a rewind, don't check for accuracy
					//delta_h[memstate%nmemstates][i]=0.;
					max_delta_h=fabs(delta_h[memstate%nmemstates][i]);
					h_np1_mp1[i]=h_np1_m[i];
					theta_np1_mp1[i]=theta_np1_m[i];
					delta_theta_i[i]=0.;
					delta_theta[i]=1E10;			//Set delta_theta[i] to any value, to make sure the convergence test will fail.
				}

				//if(not(max_delta_h>MAX_ALLOWED_DELTA_H) || niter>MAX_ITER+1) {	//If it is, there is a big chance the call to fromHtoTHETA will fail because of floating point exceptions (overflows). In this case, we will force a rewind later on, so the solution does not matter anymore.
													//The second clause means we cannot increase time step anymore, so we should just try the solution.
				if(solver_result!=-1) {
					//Apply solution
					h_np1_mp1[i]=h_np1_m[i]+delta_h[memstate%nmemstates][i];

					//Calculate theta
					theta_np1_mp1[i]=EMS[i].VG.fromHtoTHETAforICE(h_np1_mp1[i], theta_i_np1_m[i]);

					//Calculate temperature change of soil layers to reflect heat advected by the flowing water
					if(i<Xdata.SoilNode) {
						//Calculate the fluxes from above and below for this layer
						const double tmp_flux_above = (i<uppernode) ? (((((h_np1_m[i+1]+delta_h[memstate%nmemstates][i+1])-(h_np1_m[i]+delta_h[memstate%nmemstates][i]))/dz_up[i])+Xdata.cos_sl)*k_np1_m_ip12[i]*dt) : 0;			//Units: [m^3/m^2]
						const double tmp_flux_below = (i>lowernode) ? (((((h_np1_m[i]+delta_h[memstate%nmemstates][i])-(h_np1_m[i-1]+delta_h[memstate%nmemstates][i-1]))/dz_up[i-1])+Xdata.cos_sl)*k_np1_m_ip12[i-1]*dt) : 0;				//Units: [m^3/m^2]

						//Calculate intermediate state variables of this layer
						const double tmp_theta_air = 1. - theta_i_n[i] - (theta_np1_mp1[i] + (theta_i_np1_m[i]-theta_i_n[i])*(Constants::density_ice/Constants::density_water)) - EMS[i].theta[SOIL];					//Units: [m^3 m^-3]
						const double tmp_rho = (Constants::density_ice * theta_i_n[i] + Constants::density_water * (theta_np1_mp1[i] + (theta_i_np1_m[i]-theta_i_n[i])*(Constants::density_ice/Constants::density_water)) + EMS[i].soil[SOIL_RHO] * EMS[i].theta[SOIL]);	//Units: [kg m-3]
						const double tmp_c_p = (Xdata.density_air * tmp_theta_air * Constants::specific_heat_air							//Units: [J kg-1 K-1]
									+ Constants::density_ice * theta_i_n[i] * Constants::specific_heat_ice
									+ Constants::density_water * (theta_np1_mp1[i] + (theta_i_np1_m[i]-theta_i_n[i])*(Constants::density_ice/Constants::density_water)) * Constants::specific_heat_water
									+ EMS[i].soil[SOIL_RHO] * EMS[i].theta[SOIL] * EMS[i].soil[SOIL_C]
									) / tmp_rho;
						delta_Te_adv_i[i]=0.;
						if (tmp_flux_above>0.) {	//Positve flux from above (= influx in current layer)
							//Advected heat
							const double tmp_adv_heat = ((EMS[i+1].Te + delta_Te_adv[i+1] + delta_Te[i+1]) - (EMS[i].Te + delta_Te_adv[i] + delta_Te[i])) * Constants::density_water * tmp_flux_above * Constants::specific_heat_water;	//Units [J/m^2]
							delta_Te_adv_i[i] = (tmp_adv_heat) / (tmp_c_p * tmp_rho * EMS[i].L);
						}
						if (tmp_flux_below<0.) {	//Negative flux from below (=influx in current layer)
							//Advected heat
							const double tmp_adv_heat = ((EMS[i-1].Te + delta_Te_adv[i-1] + delta_Te[i-1]) - (EMS[i].Te + delta_Te_adv[i] + delta_Te[i])) * Constants::density_water * (-1.*tmp_flux_below) * Constants::specific_heat_water;	//Units [J/m^2]
							//In rare cases, we may have inflow from above AND below, so we add (+=) the temperature change due to heat advection
							delta_Te_adv_i[i] += (tmp_adv_heat) / (tmp_c_p * tmp_rho * EMS[i].L);
						}

						//Repartition ice/water based on new head
						if(matrix==true) {
							unsigned int BS_iter=0;			//Counting the number of iterations
							const double hw0=std::min(EMS[i].VG.h_e, h_np1_mp1[i]);
							EMS[i].meltfreeze_tk=Constants::meltfreeze_tk+((Constants::g*Constants::meltfreeze_tk)/Constants::lh_fusion)*hw0;
							// Bisection-Secant method, see wikipedia: http://en.wikipedia.org/wiki/False_position_method
							//   fromHtoTHETA(hw0+(Constants::lh_fusion/(Constants::g*EMS[i].meltfreeze_tk))*(EMS[i].Te-EMS[i].meltfreeze_tk))
							//      +
							//   (theta_i_np1_mp1[i]*(Constants::density_ice/Constants::density_water))
							//      -
							//   fromHtoTHETA(hw0);
							//      = 0.
							// Solving this equation for theta_i_np1_mp1[i] (which influences theta_np1_mp1 and Te)

							// So the new liquid water content basically is the same equation, but we have to adapt EMS[i].Te to the amount of ice we create (delta_i).
							if((theta_i_np1_m[i] > 0. && (EMS[i].Te + delta_Te_adv[i] + delta_Te_adv_i[i] + delta_Te[i]) > EMS[i].meltfreeze_tk) || (EMS[i].Te + delta_Te_adv[i] + delta_Te_adv_i[i] + delta_Te[i]) < EMS[i].meltfreeze_tk) {

								if(WriteDebugOutputput) {
									const double tmp_T = EMS[i].Te + delta_Te_adv[i] + delta_Te_adv_i[i] + delta_Te[i] + delta_Te_i[i];
									std::cout << "BEFORE [" << i << std::fixed << std::setprecision(15) << "]; theta_w: " << theta_np1_mp1[i] << " theta_i_np1_m: " << theta_i_np1_m[i] << " theta_s: " << EMS[i].VG.theta_s << std::setprecision(3) << "  T: " << tmp_T << std::setprecision(8) << "  rho: " << tmp_rho << "  cp: " << tmp_c_p << " ColdC: " << tmp_rho * tmp_c_p * tmp_T * EMS[i].L << "\n" << std::setprecision(6);
								}

								//Determine maximum possible change in ice content, which should be between 0, and theta_water > theta_d (all possible water freezes). Then maximum ice content is determined based on the temperature difference between element and EMS[i].meltfreeze_tk.
								//const double max_delta_ice=(std::min((theta_np1_mp1[i]-theta_d[i])*(Constants::density_ice/Constants::density_water), std::max(0., EMS[i].meltfreeze_tk-(EMS[i].Te/*+delta_Te*/)) * ((EMS[i].c[TEMPERATURE] * EMS[i].Rho) / ( Constants::density_ice * Constants::lh_fusion ))));
								double max_delta_ice;
								if((EMS[i].Te + delta_Te_adv[i] + delta_Te_adv_i[i] + delta_Te[i]) > EMS[i].meltfreeze_tk) {
									// Melt: either all ice will disappear, or a fraction based on available energy
									max_delta_ice=-1.*theta_i_n[i];
								} else {
									// Freeze: either all available water will freeze, or a fraction based on available energy.
									max_delta_ice=(theta_np1_mp1[i]-0.)*(Constants::density_water/Constants::density_ice);
								}

								bool BS_converged=false;
								double ak=0., bk=0., ck=0.;	//These are values for changes in ice content.
								double delta_Te_ak=0., delta_Te_bk=0., delta_Te_ck=0., delta_w_ak=0., delta_w_bk=0., delta_w_ck=0.;
								double ck1=0, delta_Te_ck1=0., delta_w_ck1=0.;
								if(max_delta_ice>0.) {
									ak=0.;
									bk=max_delta_ice;
								} else {
									ak=max_delta_ice;
									bk=0.;
								}
								// Deal with special cases:
								// 1) So much energy available that all ice will melt (note: this case will not be properly solved by Bisection-Secant method.)
								if((EMS[i].meltfreeze_tk-(EMS[i].Te + delta_Te_adv[i] + delta_Te_adv_i[i] + delta_Te[i])) * ((tmp_c_p * tmp_rho) / ( Constants::density_ice * Constants::lh_fusion )) < -1.*theta_i_n[i] && BS_converged==false) {
									ck=-1.*theta_i_np1_m[i];
									delta_w_ck=-1.*(ck*(Constants::density_ice/Constants::density_water));
									delta_Te_ck=((theta_i_np1_m[i] - theta_i_n[i]) + ck) / ((tmp_c_p * tmp_rho) / ( Constants::density_ice * Constants::lh_fusion ));	//Change in element temperature associated with change in ice content
									if(WriteDebugOutputput) {
										const double tmp_T = EMS[i].Te + delta_Te_adv[i] + delta_Te_adv_i[i] + delta_Te[i] + delta_Te_i[i] + delta_Te_ck;
										std::cout << "BS_ITER [" << BS_iter << std::scientific << "], case 2: a=" << ak << " b=" << bk << " c=" << ck << " (max: " << max_delta_ice << ") " << delta_w_ck << " " << tmp_T << " " << EMS[i].meltfreeze_tk << ": fa: " << (delta_w_ak + ak*(Constants::density_ice/Constants::density_water)) << " fb: " << (delta_w_bk + bk*(Constants::density_ice/Constants::density_water)) << " fc: " << (delta_w_ck + ck*(Constants::density_ice/Constants::density_water)) << "\n" << std::fixed;
									}
									BS_converged=true;
								}
								// 2) Very small temperature difference or very small possible change in ice content
								if(fabs(ak-bk)<SF_epsilon && BS_converged==false) {
									// In this case it is possible that we should melt some ice in order to prevent theta[WATER] to get negative (drainage case):
									ck=0.;
									if(theta_np1_mp1[i]<0.) {
										delta_w_ck=-1.*theta_np1_mp1[i];					//Make sure water gets 0.
										ck=theta_np1_mp1[i]*(Constants::density_water/Constants::density_ice);	//Necessary change in theta[ICE]
										delta_Te_ck=((theta_i_np1_m[i] - theta_i_n[i]) + ck) / ((tmp_c_p * tmp_rho) / ( Constants::density_ice * Constants::lh_fusion ));	//Change in element temperature associated with change in ice content
									}
									if(WriteDebugOutputput) {
										const double tmp_T = EMS[i].Te + delta_Te_adv[i] + delta_Te_adv_i[i] + delta_Te[i] + delta_Te_i[i] + delta_Te_ck;
										std::cout << "BS_ITER [" << BS_iter << std::scientific << "], case 1: a=" << ak << " b=" << bk << " c=" << ck << " (max: " << max_delta_ice << ") " << delta_w_ck << " " << tmp_T << " " << EMS[i].meltfreeze_tk << ": fa: " << (delta_w_ak + ak*(Constants::density_ice/Constants::density_water)) << " fb: " << (delta_w_bk + bk*(Constants::density_ice/Constants::density_water)) << " fc: " << (delta_w_ck + ck*(Constants::density_ice/Constants::density_water)) << "\n" << std::fixed;
									}
									BS_converged=true;
								}
								while (BS_converged==false && BS_iter < BS_MAX_ITER) {
									BS_iter++;
									delta_Te_ak=((theta_i_np1_m[i] - theta_i_n[i]) + ak) / ((tmp_c_p * tmp_rho) / ( Constants::density_ice * Constants::lh_fusion ));			//Change in element temperature associated with change in ice content
									delta_Te_bk=((theta_i_np1_m[i] - theta_i_n[i]) + bk) / ((tmp_c_p * tmp_rho) / ( Constants::density_ice * Constants::lh_fusion ));			//Change in element temperature associated with change in ice content
									delta_w_ak=(EMS[i].VG.fromHtoTHETA(hw0+(Constants::lh_fusion/(Constants::g*EMS[i].meltfreeze_tk))*std::min(0., (EMS[i].Te + delta_Te_adv[i] + delta_Te_adv_i[i] + delta_Te[i] + delta_Te_ak)-EMS[i].meltfreeze_tk))) - theta_np1_mp1[i];
									delta_w_bk=(EMS[i].VG.fromHtoTHETA(hw0+(Constants::lh_fusion/(Constants::g*EMS[i].meltfreeze_tk))*std::min(0., (EMS[i].Te + delta_Te_adv[i] + delta_Te_adv_i[i] + delta_Te[i] + delta_Te_bk)-EMS[i].meltfreeze_tk))) - theta_np1_mp1[i];
									//Now calculate bisect
									ck1=(ak+bk)/2.;
									delta_Te_ck1=((theta_i_np1_m[i] - theta_i_n[i]) + ck1) / ((tmp_c_p * tmp_rho) / ( Constants::density_ice * Constants::lh_fusion ));			//Change in element temperature associated with change in ice content
									delta_w_ck1=(EMS[i].VG.fromHtoTHETA(hw0+(Constants::lh_fusion/(Constants::g*EMS[i].meltfreeze_tk))*std::min(0., (EMS[i].Te + delta_Te_adv[i] + delta_Te_adv_i[i] + delta_Te[i] + delta_Te_ck1)-EMS[i].meltfreeze_tk))) - theta_np1_mp1[i];
									//Now check secant
									ck=((delta_w_bk + bk*(Constants::density_ice/Constants::density_water))*ak  -  (delta_w_ak + ak*(Constants::density_ice/Constants::density_water))*bk)  /  ((delta_w_bk + bk*(Constants::density_ice/Constants::density_water)) - (delta_w_ak + ak*(Constants::density_ice/Constants::density_water)));
									delta_Te_ck=((theta_i_np1_m[i] - theta_i_n[i]) + ck) / ((tmp_c_p * tmp_rho) / ( Constants::density_ice * Constants::lh_fusion ));			//Change in element temperature associated with change in ice content
									delta_w_ck=(EMS[i].VG.fromHtoTHETA(hw0+(Constants::lh_fusion/(Constants::g*EMS[i].meltfreeze_tk))*std::min(0., (EMS[i].Te + delta_Te_adv[i] + delta_Te_adv_i[i] + delta_Te[i] + delta_Te_ck)-EMS[i].meltfreeze_tk))) - theta_np1_mp1[i];
									//Now check if bisect or secant is a better approximation
									if(fabs(delta_w_ck + ck*(Constants::density_ice/Constants::density_water))>fabs(delta_w_ck1+ck1*(Constants::density_ice/Constants::density_water))) {
										ck=ck1;
										delta_Te_ck=delta_Te_ck1;
										delta_w_ck=delta_w_ck1;
									}
									if(WriteDebugOutputput) {
										const double tmp_T = EMS[i].Te + delta_Te_adv[i] + delta_Te_adv_i[i] + delta_Te[i] + delta_Te_i[i] + delta_Te_ck;
										std::cout << "BS_ITER [" << BS_iter << std::scientific << "]: a=" << ak << " b=" << bk << " c=" << ck << " (max: " << max_delta_ice << ") " << delta_w_ck << " " << tmp_T << " " << EMS[i].meltfreeze_tk << ": fa: " << (delta_w_ak + ak*(Constants::density_ice/Constants::density_water)) << " fb: " << (delta_w_bk + bk*(Constants::density_ice/Constants::density_water)) << " fc: " << (delta_w_ck + ck*(Constants::density_ice/Constants::density_water)) << "\n" << std::fixed;
									}
									//Now check if convergence is achieved
									if(fabs(delta_w_ck + ck*(Constants::density_ice/Constants::density_water)) < SF_epsilon) {
										delta_w_ck=-1.*(ck*(Constants::density_ice/Constants::density_water));	//Make delta in water equal to ice, so we keep mass-balance.
										BS_converged=true;
									} else if(fabs(delta_w_ak + ak*(Constants::density_ice/Constants::density_water)) < SF_epsilon) {
										ck=ak;
										delta_w_ck=-1.*(ck*(Constants::density_ice/Constants::density_water));	//Make delta in water equal to ice, so we keep mass-balance.
										delta_Te_ck=delta_Te_ak;
										BS_converged=true;
									} else if(fabs(delta_w_bk + bk*(Constants::density_ice/Constants::density_water)) < SF_epsilon) {
										ck=bk;
										delta_w_ck=-1.*(ck*(Constants::density_ice/Constants::density_water));	//Make delta in water equal to ice, so we keep mass-balance.
										delta_Te_ck=delta_Te_bk;
										BS_converged=true;
									} else {
										//And determine whether to update the left or right point
										if((delta_w_ck + ck*(Constants::density_ice/Constants::density_water)) * (delta_w_ak + ak*(Constants::density_ice/Constants::density_water)) > 0.) {	//Multiply to check if same sign
											ak=ck;
										} else {
											bk=ck;
										}
									}
								}
								if(BS_converged==false) {
									if(WriteDebugOutputput) std::cout << "[W] ReSolver1d.cc: Bisect-Secant method failed to converge in soil freezing with dt = " << dt << ".\n";
									if(WriteDebugOutputput) {
										const double tmp_T = EMS[i].Te + delta_Te_adv[i] + delta_Te_adv_i[i] + delta_Te[i] + delta_Te_i[i] + delta_Te_ck;
										std::cout << "  -- BS_ITER [" << BS_iter << std::scientific << "]: a=" << ak << " b=" << bk << " c=" << ck << " (max: " << max_delta_ice << ") " << delta_w_ck << " " << tmp_T << " " << EMS[i].meltfreeze_tk << ": fa: " << (delta_w_ak + ak*(Constants::density_ice/Constants::density_water)) << " fb: " << (delta_w_bk + bk*(Constants::density_ice/Constants::density_water)) << " fc: " << (delta_w_ck + ck*(Constants::density_ice/Constants::density_water)) << "\n" << std::fixed;
										std::cout << "  -- " << std::setprecision(15) << EMS[i].meltfreeze_tk << " " << EMS[i].Te << " " << delta_Te_adv[i] << " " << delta_Te_adv_i[i] << " " << delta_Te[i] << "   " << EMS[i].theta[WATER] << " " << EMS[i].theta[ICE] << "\n" << std::setprecision(6);
									}
									max_delta_h=2.*MAX_ALLOWED_DELTA_H;
									solver_result=-1;
								} else {
									//Final solution
									const double tmp_delta_i=ck;
									const double tmp_delta_w=delta_w_ck;
									const double tmp_delta_Te=delta_Te_ck;
									//Apply final solution
									delta_Te_i[i]=tmp_delta_Te;
									theta_i_np1_mp1[i]=theta_i_np1_m[i]+tmp_delta_i;
									theta_np1_mp1[i]+=tmp_delta_w;
								}
							} else {
								theta_i_np1_mp1[i]=0.;
								theta_np1_mp1[i]=EMS[i].VG.fromHtoTHETAforICE(h_np1_mp1[i], 0.);
							}
							//Update BS-solver statistics
							bs_stats_totiter+=BS_iter;
							if(BS_iter>bs_stats_maxiter) bs_stats_maxiter=BS_iter;
							if(WriteDebugOutputput)
								std::cout << "AFTER [" << i << std::setprecision(15) << "]: theta_w: " << theta_np1_mp1[i] << " theta_i_np1_m: " << theta_i_np1_mp1[i] << " theta_s:" << EMS[i].VG.theta_s << std::setprecision(3) << "  T: " << EMS[i].Te + delta_Te_adv[i] + delta_Te_adv_i[i] + delta_Te[i] + delta_Te_i[i] << " (niter=" << BS_iter << ")\n" << std::setprecision(6);
						} else { //END OF REPARTITIONING ICE/WATER
							theta_i_np1_mp1[i]=theta_i_np1_m[i];
						}
					}
					delta_theta[i]=theta_np1_mp1[i]-theta_np1_m[i];
					delta_theta_i[i]=theta_i_np1_mp1[i]-theta_i_np1_m[i];
				} else {
					// Solver failed, trigger rewind
					max_delta_h=2.*MAX_ALLOWED_DELTA_H;
				}

				//Update mass balance
				mass2+=(theta_np1_mp1[i]+(theta_i_np1_mp1[i]*(Constants::density_ice/Constants::density_water)))*dz[i];

				if(WriteDebugOutputput) {
					std::cout << "ITER: " << niter << " i: " << i << std::scientific << std::setprecision(10) << " ---  h1: " << h_np1_m[i] << " d_h: " << delta_h[memstate%nmemstates][i] << " h2: " << h_np1_mp1[i] << std::setprecision(12) << " --- theta1: " << theta_np1_m[i] << " d_theta: " << delta_theta[i] << " theta2: " << theta_np1_mp1[i] << "\n" << std::setprecision(6) << std::fixed;
				}


				// Note: boundaries are not considered for determination of the accuracy in case of Dirichlet (of course!).

				//Absolute accuracy in h: This seems to produce the best (in sense of most stable) behaviour.
				//Note: because we want to be able to very accurately determine the flux over the snow-soil boundary (for MS_SNOW_RUNOFF) and model boundaries (for MS_SOIL_RUNOFF),
				//we ALWAYS have to assess the accuracy in head in this region! If we don't do this, then in case of dry soil layers, the estimated pressure head can be quite
				//inaccurate, leading to a completely wrong estimation of these fluxes!
				if(Se[i]>convergencecriterionthreshold || i==Xdata.SoilNode-1 || i==Xdata.SoilNode || i==lowernode || i==lowernode-1) {
					if ((i!=lowernode || aBottomBC==NEUMANN) && (i!=uppernode || aTopBC==NEUMANN)) {
						//First check general accuarcy:
						if(fabs(delta_h[memstate%nmemstates][i])>track_accuracy_h) {
							track_accuracy_h=fabs(delta_h[memstate%nmemstates][i]);
						}
						//Now check against convergence criterion:
						//if(fabs(delta_h[memstate%nmemstates][i]/h_np1_m[i]) > REQUIRED_ACCURACY_H) {	//relative accuracy
						if(fabs(delta_h[memstate%nmemstates][i])>REQUIRED_ACCURACY_H) {			//absolute accuracy
							accuracy=fabs(delta_h[memstate%nmemstates][i]);
						}
					}
				}

				//Absolute accuracy in theta. This doesn't behave stable, especially during saturated soil conditions, when the accuracy is set too low.
				//See Huang (1996), which proposes this, and also discusses the need for a higher accuracy:
				//if(not(Se[i]>convergencecriterionthreshold || i==uppernode || i==lowernode || i==Xdata.SoilNode-1 || i==Xdata.SoilNode)) {
				if(not(Se[i]>convergencecriterionthreshold || i==Xdata.SoilNode-1 || i==Xdata.SoilNode || i==lowernode || i==lowernode-1)) {
					if ((i!=lowernode || aBottomBC==NEUMANN) && (i!=uppernode || aTopBC==NEUMANN)) {
						//First check general accuarcy:
						if(fabs(delta_theta[i]+delta_theta_i[i]*(Constants::density_ice/Constants::density_water))>track_accuracy_theta) {
							track_accuracy_theta=fabs(delta_theta[i]+delta_theta_i[i]*(Constants::density_ice/Constants::density_water));
						}
						//Now check against convergence criterion:
						if (fabs(delta_theta[i]+delta_theta_i[i]*(Constants::density_ice/Constants::density_water)) > REQUIRED_ACCURACY_THETA ) {
							accuracy=fabs(delta_theta[i]+delta_theta_i[i]*(Constants::density_ice/Constants::density_water));
						}
					}
				}
			}

			//Apply boundary conditions
			if(aTopBC==DIRICHLET) {
				h_np1_mp1[uppernode]=htop;		//Dirichlet
			}
			if(aBottomBC==DIRICHLET) {
				h_np1_mp1[lowernode]=hbottom;		//Dirichlet
			}

			//Check mass balance:
			//-- Calculate mass change:
			massbalanceerror=mass1-mass2;
			//-- Determine top and bottom flux:
			double tmp_mb_topflux=0.;
			double tmp_mb_bottomflux=0.;
			if(aTopBC==NEUMANN) {		//If we use Neumann, the massbalance should incorporate the applied TopFluxRate:
				tmp_mb_topflux=TopFluxRate*dt;
			} else {			//Else when using Dirichlet, we should estimate the influx: (Note that basically with Dirichlet, the change of theta in the element is 0., so the influx in the model domain is equal to the flux from the upper element to the one below.)
				tmp_mb_topflux=((theta_np1_mp1[uppernode]+theta_i_np1_mp1[uppernode]*(Constants::density_ice/Constants::density_water))-(theta_n[uppernode] + theta_i_n[uppernode]*(Constants::density_ice/Constants::density_water)))*dz[uppernode] + (2./(rho[uppernode]+rho[uppernode-1]))*((((h_np1_mp1[uppernode]*rho[uppernode]-h_np1_mp1[uppernode-1]*rho[uppernode-1])/dz_down[uppernode])+Xdata.cos_sl*rho[uppernode])*k_np1_m_im12[uppernode]*dt);
			}
			if(aBottomBC==NEUMANN) {	//If we use Neumann, the massbalance should incorporate the applied BottomFluxRate:
				tmp_mb_bottomflux=BottomFluxRate*dt;
			} else {			//Else when using Dirichlet, we should estimate the outflux: (Note that basically with Dirichlet, the change of theta in the element is 0., so the outflux in the model domain is equal to the flux from the element above the lowest one to the lowest one.)
				if(uppernode > 0) {
					if(variant != "SEAICE") {
						tmp_mb_bottomflux=-1.*(
							((theta_np1_mp1[lowernode]+theta_i_np1_mp1[lowernode]*(Constants::density_ice/Constants::density_water))-(theta_n[lowernode] + theta_i_n[lowernode]*(Constants::density_ice/Constants::density_water)))*dz[lowernode]
							-(2./(rho[lowernode+1]+rho[lowernode]))*
								((((h_np1_mp1[lowernode+1]*rho[lowernode+1]-h_np1_mp1[lowernode]*rho[lowernode])/dz_up[lowernode])
								+Xdata.cos_sl*(rho[lowernode+1]*z[lowernode+1]-rho[lowernode]*z[lowernode])/dz_up[lowernode])
								*k_np1_m_ip12[lowernode]*dt)
							);
					} else {
						tmp_mb_bottomflux=(Salinity.flux_down[lowernode] + Salinity.flux_down_2[lowernode]) * dt;		// Units: [m]
					}
				} else {
					//With Dirichlet lower boundary condition and only 1 element, we cannot really estimate the flux, so set it to 0.
					tmp_mb_bottomflux=0.;
				}
			}
			massbalanceerror+=tmp_mb_topflux;		//Add topflux (note: topflux>0. means influx)
			massbalanceerror-=tmp_mb_bottomflux;		//Substract bottomflufx (note: bottomflux>0. means outflux)
			massbalanceerror+=totalsourcetermflux*dt;	//Add the sink/source term flux.

			if(WriteDebugOutputput) printf("MASSBALANCETEST: mass1 %.8E    mass2 %.8E    topflux %.8E (%.8E)  bottomflux %.8E (%.8E) sourceflux %.8E    delta %.8E\n", mass1, mass2, tmp_mb_topflux, ((theta_np1_mp1[uppernode]+theta_i_np1_mp1[uppernode]*(Constants::density_ice/Constants::density_water))-(theta_n[uppernode] + theta_i_n[uppernode]*(Constants::density_ice/Constants::density_water)))*dz[uppernode] + ((((h_np1_m[uppernode]-h_np1_m[uppernode-1])/dz_down[uppernode])+1.)*k_np1_m_im12[uppernode]*dt), tmp_mb_bottomflux, -1.*(((theta_np1_mp1[lowernode]+theta_i_np1_mp1[lowernode]*(Constants::density_ice/Constants::density_water))-(theta_n[lowernode] + theta_i_n[lowernode]*(Constants::density_ice/Constants::density_water)))*dz[lowernode]-(1./rho[lowernode])*((((h_np1_m[lowernode+1]*rho[lowernode+1]-h_np1_m[lowernode]*rho[lowernode])/dz_up[lowernode])+Xdata.cos_sl*rho[lowernode])*k_np1_m_ip12[lowernode]*dt)), totalsourcetermflux*dt, massbalanceerror);

			//Make sure to trigger a rewind by making max_delta_h very large in case the mass balance is violated or change in head are too large.
			if(fabs(massbalanceerror)>1E-1 || max_delta_h>MAX_ALLOWED_DELTA_H) {
				max_delta_h=2.*MAX_ALLOWED_DELTA_H;
			}

			if (accuracy > 1E-20 || fabs(massbalanceerror)>maxallowedmassbalanceerror || solver_result==-1) {		//Check whether we converged. Note that accuracy is only assigned when the layer exceeds the prescribed required accuracy. This is because we want to have more than one convergence criterion (both h and theta based), we say accuracy=0 is a sign of convergence in the whole domain.
				boolConvergence=false;
			}


			if(WriteDebugOutputput) printf("CONVERGENCE:  acc_h: %.10f acc_theta: %.10f acc: %.10f converged? %s\n", track_accuracy_h, track_accuracy_theta, accuracy, (boolConvergence)?"yes":"no");

			//Copy solution, to prepare for next iteration
			for (i = lowernode; i <= uppernode; i++) {
				h_np1_m[i]=h_np1_mp1[i];
				theta_np1_m[i]=theta_np1_mp1[i];
				theta_i_np1_m[i]=theta_i_np1_mp1[i];
			}

			//Rewind time step control: retry finding solution with smaller time step when MAX_ITER is exceeded. Also when max_delta_h is too large we do a rewind, else the model is starting to blow up.
			if((niter>MAX_ITER || max_delta_h>MAX_ALLOWED_DELTA_H) && dt > MIN_VAL_TIMESTEP && boolConvergence==false) {

				if(WriteDebugOutputput) std::cout << "REWIND: timestep " << std::setprecision(20) << dt << std::setprecision(6) << " ---> ";

				niter_seqrewinds++;				//We increase the sequential rewinds counter. For this first rewind, this will give a power of 1 for the new time step, etc.
										//When we find a good solution again, we reset this counter to 0.

				TimeAdvance-=dt;				//We do a rewind of the time step, so adjust TimeAdvance with the time step.
				dt*=pow(0.333, double(niter_seqrewinds));	//Now make the time step smaller, we use niter_seqrewinds to speed up when we encounter multiple consecutive rewinds.
										//The value of 0.333 is taken from the HYDRUS-manual, where they do this in case a rewind is necessary.

				if(WriteDebugOutputput) std::cout << std::setprecision(20) << dt << "  accuracy: " << std::setprecision(10) << accuracy << " max_delta_h: " << max_delta_h << ")\n" << std::setprecision(6);

				niter=0;					//Because of the rewind, we start again with the iterations.
				niter_nrewinds++;				//Increase rewind counter
				stats_nrewinds++;				//Increase the statistics rewind counter
				boolConvergence=false;				//Of course, when we need to rewind, we have had no convergence.
				DoRewindFlag=true;				//Set DoRewindFlag to true, to quit the iteration loop.
				StopLoop=false;					//In case StopLoop was set true (last time step), we set it back to false. It might be that the smaller time step won't match the SNOWPACK time step any longer.
				for (i = lowernode; i <= uppernode; i++) {	//We have to reset the whole domain, because we do the time step for the whole domain.
					h_np1_m[i]=h_n[i];
					theta_np1_m[i]=theta_n[i];
					theta_i_np1_m[i]=theta_i_n[i];		//Set back ice content due to soil freezing/thawing
					delta_Te_i[i]=0.;			//Reset temperature change due to soil freezing/thawing
					delta_Te_adv_i[i]=0.;			//Reset temperature change due to heat advection by water flowing
				}
			}

			if(niter>500) {
				//Print latest state for debugging:
				bool DoThrow=false;
				if(SafeMode==false) {
					prn_msg(__FILE__, __LINE__, "err", Date(), "Richards-Equation solver did not converge: reached maximum number of iterations (500), with a time step: %G\n", dt);
					DoThrow=true;
				} else {
					if(seq_safemode>3) {
						std::cout << "[E] ERROR in Richards-Equation solver: no convergence! SafeMode was not able to continue simulation!\n";
						DoThrow=true;
					} else {
						std::cout << "[W] WARNING in Richards-Equation solver: no convergence! SafeMode was used to continue simulation! [" << seq_safemode << "].\n";
					}
				}
				std::cout << "    POSSIBLE SOLUTIONS:\n  ========================================================================================\n";
				if(sn_dt>900) std::cout << "      - SNOWPACK time step is larger than 15 minutes. This numerical problem\n      may be resolved by using a time step of 15 minutes.\n";
#ifndef CLAPACK
				std::cout << "      - SNOWPACK was not compiled with BLAS and CLAPACK libraries.\n      Try installing libraries BLAS and CLAPACK and use solver TGSV (default).\n";
#endif
				std::cout << "      - Verify that the soil is not initialized in a very dry or a very\n      wet state.\n";
				std::cout << "      - If the snow and/or soil saturates, try setting LB_COND_WATERFLUX = FREEDRAINAGE\n      in the [SnowpackAdvanced] section of the ini-file.\n";
				std::cout << "      - If the soil is drying out, try setting LB_COND_WATERFLUX = WATERTABLE in the\n      [SnowpackAdvanced] section of the ini-file.\n";
				std::cout << "      - Try bucket scheme, by setting WATERTRANSPORTMODEL_SNOW = BUCKET and\n      WATERTRANSPORTMODEL_SOIL = BUCKET in the [SnowpackAdvanced] section\n      of the ini-file.\n";
				std::cout << "\n  ----------------------------------------------------------------------------------------\n    SOLVER DUMP:\n";
				for (i = lowernode; i <= uppernode; i++) {
					printf("    layer [%d]:  h(t): %.3f  h(t+dt): %.3f  th(t): %.3f (%.3f-%.3f)  th(t+dt): %.3f  th_ice(t): %.3f  th_ice(t+dt): %.3f  (vg_params: %.2f %.2f %.2f)\n", int(i), h_n[i], h_np1_m[i], theta_n[i], EMS[i].VG.theta_r, EMS[i].VG.theta_s, theta_np1_m[i], (i<Xdata.SoilNode)?(theta_i_n[i]):(EMS[i].theta[ICE]), (i<Xdata.SoilNode)?(theta_i_np1_m[i]):(EMS[i].theta[ICE]), EMS[i].VG.alpha, EMS[i].VG.m, EMS[i].VG.n);
				}
				printf("    upper boundary [ boundary condition: %d ]:   prescribed flux: %G    applied flux: %G     domain: %s\n", TopBC, surfacefluxrate, TopFluxRate, (matrix)?("matrix"):("pref_flow"));
				if(DoThrow==true) {
					//We are lost. We cannot do another rewind and decrease time step (if we could, niter is reset).
					throw;
				} else {
					std::cout << "  ----------------------------------------------------------------------------------------\n    SAFE MODE:\n";

					//We rescue the simulation ...
					double SafeMode_MBE=0.;		// Mass balance error (kg/m^2) due to SafeMode

					//Do a rewind
					TimeAdvance-=dt;
					dt=1E-3;

					niter=0;
					niter_nrewinds++;				//Increase rewind counter
					stats_nrewinds++;				//Increase the statistics rewind counter
					seq_safemode++;					//Increase counter for sequential safemode
					boolConvergence=false;				//Of course, when we need to rewind, we have had no convergence.
					DoRewindFlag=true;				//Set DoRewindFlag to true, to quit the iteration loop.
					StopLoop=false;					//In case StopLoop was set true (last time step), we set it back to false. It might be that the smaller time step won't match the SNOWPACK time step any longer.
					if(seq_safemode==1 && totalsourcetermflux!=0.) {
						for (i = lowernode; i <= uppernode; i++) {	//We have to reset the whole domain, because we do the time step for the whole domain.
							// The first time a safe mode is required, set source/sink terms to 0.
							if(s[i]!=0) {
								SafeMode_MBE+=s[i]*(sn_dt-TimeAdvance)*Constants::density_water*dz[i];
								printf("    --> reset source/sink term at layer %d from %G ", int(i), s[i]);
								s[i]=0.;
								totalsourcetermflux=0.;
								printf("    to %G.\n", s[i]);
							}
						}
					} else {
						if(seq_safemode==3) {
							mass1=0.;					//Because we will fiddle around with theta, we should update mass1 (mass at beginning of time step)
							for (i = lowernode; i <= uppernode; i++) {	//We have to reset the whole domain, because we do the time step for the whole domain.
								// Update the SafeMode mass balance error tracking variable by "removing" all water
								SafeMode_MBE-=(theta_n[i]+theta_i_n[i])*dz[i]*Constants::density_water;
								// Make sure pressure head is in secure limits:
								h_n[i]=std::max(h_d, std::min(EMS[i].VG.h_e, h_n[i]));
								theta_n[i]=EMS[i].VG.fromHtoTHETAforICE(h_n[i], theta_i_n[i]);
								//Deal with dry layers
								if(theta_n[i]+theta_i_n[i] < EMS[i].VG.theta_r+(REQUIRED_ACCURACY_THETA/1000.)) {
									theta_n[i]=EMS[i].VG.theta_r+(REQUIRED_ACCURACY_THETA/1000.);
									h_n[i]=EMS[i].VG.fromTHETAtoHforICE(theta_n[i], h_d, theta_i_n[i]);
								}
								//Deal with wet layers
								if(theta_n[i]+theta_i_n[i] > EMS[i].VG.theta_s-(REQUIRED_ACCURACY_THETA/1000.)) {
									theta_i_n[i]*=0.90;
									theta_n[i]=((theta_n[i]-EMS[i].VG.theta_r)*0.9)+EMS[i].VG.theta_r;
									h_n[i]=EMS[i].VG.fromTHETAtoHforICE(theta_n[i], h_d, theta_i_n[i]);
								}
								// Update the SafeMode mass balance error tracking variable by "adding" the water again
								SafeMode_MBE+=(theta_n[i]+theta_i_n[i])*dz[i]*Constants::density_water;

								h_np1_m[i]=h_n[i];			//Reset initial guess for next iteration
								theta_np1_m[i]=theta_n[i];		//Reset initial guess for next iteration
								theta_i_np1_m[i]=theta_i_n[i];		//Set back ice content due to soil freezing/thawing

								delta_Te_i[i]=0.;			//Reset temperature change due to soil freezing/thawing
								delta_Te_adv_i[i]=0.;			//Reset temperature change due to heat advection by water flowing

								//Now update mass1, which may have changed due to SafeMode throwing some water away, or introducing some water:
								mass1+=(theta_n[i]+(theta_i_n[i]*(Constants::density_ice/Constants::density_water)))*dz[i];
							}
							std::cout << "    --> reset dry and wet layers.\n";

							//Deal with the TopFluxRate:
							if(surfacefluxrate!=0.) {
								SafeMode_MBE+=(surfacefluxrate/2.)*(sn_dt-TimeAdvance)*Constants::density_water;
								printf("    --> set surfacefluxrate from %G ", surfacefluxrate);
								surfacefluxrate=0.;
								printf("to %G.\n", surfacefluxrate);
							}
						} else {
							//Deal with the TopFluxRate:
							SafeMode_MBE+=(surfacefluxrate/2.)*(sn_dt-TimeAdvance)*Constants::density_water;
							printf("    --> set surfacefluxrate from %G ", surfacefluxrate);
							if(seq_safemode==2) {
								surfacefluxrate=0.;
							} else {
								surfacefluxrate/=2.;
							}
							printf("to %G.\n", surfacefluxrate);
						}
					}
					std::cout << "    Estimated mass balance error due to SafeMode: " << std::scientific << SafeMode_MBE << std::fixed << " kg/m^2\n";
				}
			}
		}	//End of iteration loop


		//If the solver wants to do a rewind, it exits the previous loop. However, in this case the time step is already adjusted and the matrices are put back to initial state. So we should skip the preparation for the next time step.
		if(DoRewindFlag==false) {
			niter_seqrewinds=0;		//We found a good solution (again), so we reset this niter_seqrewind counter.
			seq_safemode=0;
			stats_niters+=niter;		//Update the statistics with the number of iterations necessary to find the solution, ignoring possible iterations done before a rewind.
			stats_nsteps++;			//Update the statistics of the number of steps.

			//Prepare for next time step:
			if (Xdata.Seaice != NULL) {
				//
				// For sea ice, deal with salinity flux
				//

				// Set the SalinityTransport vector with the solution after liquid water flow
				std::vector<double> DeltaSal(nE, 0.);							//Salinity changes
				std::vector<double> DeltaSal2(nE, 0.);							//Salinity changes
				for (i = lowernode; i <= uppernode; i++) {						//We loop over all Richards solver domain layers
					Salinity.BrineSal[i] = EMS[i].salinity / theta_n[i];				//Calculate brine salinity
					Salinity.theta1[i] = theta_n[i];
					Salinity.theta2[i] = theta_np1_mp1[i];
				}
				Salinity.BottomSalinity = (Xdata.Seaice->OceanSalinity);
				Salinity.TopSalinity = (0.);

				// Solve the transport equation
				if(SalinityTransportSolver==SalinityTransport::EXPLICIT) {
					Salinity.SolveSalinityTransportEquationExplicit(dt, DeltaSal);
				} else {
					Salinity.SolveSalinityTransportEquationImplicit(dt, DeltaSal, 0.5, (SalinityTransportSolver==SalinityTransport::IMPLICIT2));
				}

				// Apply and verify solution
				const double tol = Constants::eps;
				for (i = lowernode; i <= uppernode; i++) {
					//EMS[i].salinity = Salinity.BrineSal[i] * theta_np1_mp1[i];
					//EMS[i].salinity += DeltaSal[i] * (theta_np1_mp1[i]);

					//Verify new salinity profile
					if(EMS[i].salinity < 0. && TimeAdvance > 900. - Constants::eps2) {
						if(EMS[i].salinity>-tol) {
							EMS[i].salinity = tol;
						} else {
							std::cout << "[E] Salinity at e=" << i << ": " << std::setprecision(8) << EMS[i].salinity << "!\n";
							EMS[i].salinity = tol;
							//throw;
						}
					}
					Xdata.Edata[i].meltfreeze_tk = -SeaIce::mu * Salinity.BrineSal[i] + Constants::meltfreeze_tk;
				}

				if (SALINITY_MIXING != NONE) {
					for (i = lowernode; i <= uppernode; i++) {						//We loop over all Richards solver domain layers
						DeltaSal2[i] = DeltaSal[i];
						DeltaSal[i] = 0.;
						Salinity.BrineSal[i] = EMS[i].salinity / theta_n[i];
						Salinity.flux_up[i] = Salinity.flux_up_2[i];
						Salinity.flux_down[i] = Salinity.flux_down_2[i];
						Salinity.flux_up_2[i] = Salinity.flux_down_2[i] = 0.;
						//Salinity.D[i] = 0.;
					}
					// Solve the transport equation
					if(SalinityTransportSolver==SalinityTransport::EXPLICIT) {
						Salinity.SolveSalinityTransportEquationExplicit(dt, DeltaSal2);
					} else {
						Salinity.SolveSalinityTransportEquationImplicit(dt, DeltaSal2, 0.5, SalinityTransportSolver==SalinityTransport::IMPLICIT2);
					}
				}

				// Apply and verify solution
				for (i = lowernode; i <= uppernode; i++) {
					EMS[i].salinity = Salinity.BrineSal[i] * theta_np1_mp1[i];
					//EMS[i].salinity = (((theta_n[i]!=0.) ? (EMS[i].salinity / theta_n[i]) : (0.)) + (DeltaSal[i] + DeltaSal2[i])) * (theta_np1_mp1[i]);
					Salinity.sb[i] = 0.;
					//Verify new salinity profile
					if(EMS[i].salinity < 0. && TimeAdvance > 900. - Constants::eps2) {
						if(EMS[i].salinity>-tol) {
							EMS[i].salinity = tol;
						} else {
							std::cout << "[E] Salinity at e=" << i << ": " << std::setprecision(8) << EMS[i].salinity << "!\n";
							EMS[i].salinity = tol;
						}
					}
					EMS[i].meltfreeze_tk = -SeaIce::mu * Salinity.BrineSal[i] + Constants::meltfreeze_tk;
					EMS[i].updDensity();
					EMS[i].M=EMS[i].L*EMS[i].Rho;
				}



				Xdata.Seaice->BottomSalFlux += Salinity.BottomSalFlux;
				Xdata.Seaice->TopSalFlux += Salinity.TopSalFlux;
				Salinity.BottomSalFlux = 0.;
				Salinity.TopSalFlux = 0.;
			}

			for (i = lowernode; i <= uppernode; i++) {				//Cycle through all Richards solver domain layers.
				//Apply change in temperature due to soil freezing or thawing and heat advection by flowing water:
				if(i<Xdata.SoilNode) {
					//Freezing and thawing
					if(fabs(delta_Te_i[i]) > 0.) {				//Check if phase change did occur in soil
						delta_Te[i]+=delta_Te_i[i];
						EMS[i].QIntmf+=(Constants::density_ice*(theta_i_np1_mp1[i]-theta_i_n[i])*(Constants::specific_heat_water-Constants::specific_heat_ice)*(EMS[i].meltfreeze_tk-Constants::meltfreeze_tk))/dt;
						// Now that we have performed a phase change, we should correct the nodal temperatures too. This will be done later in PhaseChange,
						// by using Qmf to determine amount of phase change that occurred.
					}

					delta_Te_adv[i]+=delta_Te_adv_i[i];
				}

				//We adapted the elements and nodes to the temperature change, so set it to 0.
				delta_Te_i[i]=0.;
				delta_Te_adv_i[i]=0.;

				//Set initial solution for next iteration
				theta_np1_mp1[i]=EMS[i].VG.fromHtoTHETAforICE(h_np1_m[i], theta_i_np1_mp1[i]);

				delta_h_dt[i]=(h_np1_mp1[i]-h_n[i])/dt;				//We make delta_h_dt relative to time step. If time step is allowed to change, we can use this delta_h_dt (actually a derivative) to better estimate solution in next time step.
				delta_theta_dt[i]=(theta_np1_mp1[i]-theta_n[i])/dt;		//We make delta_theta_dt relative to time step. If time step is allowed to change, we can use this delta_h_dt (actually a derivative) to better estimate solution in next time step.
				delta_theta_i_dt[i]=(theta_i_np1_mp1[i]-theta_i_n[i])/dt;	//We make delta_theta_i_dt relative to time step. If time step is allowed to change, we can use this delta_h_dt (actually a derivative) to better estimate solution in next time step.

				//Copy current state:
				h_n[i]=h_np1_mp1[i];
				theta_n[i]=theta_np1_mp1[i];
				theta_i_n[i]=theta_i_np1_mp1[i];
			}


			//Determine (estimate) flux across boundaries (downward ==> positive flux):
			//This is an additional check for the boundaries.
			actualtopflux+=TopFluxRate*dt;
			refusedtopflux+=(surfacefluxrate-TopFluxRate)*dt;
			if(aBottomBC==DIRICHLET) {
				if(uppernode > 0) {
					double tmp_flux = 0.;
					if(variant != "SEAICE") {
						tmp_flux=-1.*((delta_theta_dt[lowernode]+(delta_theta_i_dt[lowernode]*(Constants::density_ice/Constants::density_water))*dt*dz_[lowernode]) - (1./rho[lowernode])*((((h_np1_mp1[lowernode+1]*rho[lowernode+1]-h_np1_mp1[lowernode]*rho[lowernode])/dz_up[lowernode])+Xdata.cos_sl*rho[lowernode])*k_np1_m_ip12[lowernode]*dt));
					} else {
						tmp_flux=(Salinity.flux_down[0]+Salinity.flux_down_2[0])*dt;
					}
					actualbottomflux+=tmp_flux;
				} else {
					//With Dirichlet lower boundary condition and only 1 element, we cannot really estimate the flux, so set it to 0.
					const double tmp_flux=0.;
					actualbottomflux+=tmp_flux;
				}
			} else {
				actualbottomflux+=BottomFluxRate*dt;
			}


			massbalanceerror_sum+=massbalanceerror;
			if(WriteDebugOutputput) printf("MASSBALANCE: mass1 %.8f    mass2 %.8f    delta %.8f\n", mass1, mass2, massbalanceerror);


			//Determine flux at soil snow interface (note: postive=flux upward, negative=flux downward):
			if (Xdata.SoilNode<nE) {	//We have snow layers
				if(uppernode+1==Xdata.SoilNode) {	//We run RE-solver only for soil layers AND have snow layers in the model (meaning TopFluxRate is coming from snow)
					//These lines are not active, as this particular case (RE for soil, other for snow), is dealt with in compTransportMass.
					//snowsoilinterfaceflux+=surfacefluxrate*dt;
				} else {
					// See McCord (1996). Note: I think there is a minus sign missing there.
					// In any case: snowsoilinterfaceflux > 0 means influx!
					if(Xdata.SoilNode>0) {
						const double tmp_snowsoilinterfaceflux=(1./rho[Xdata.SoilNode])*((((h_n[Xdata.SoilNode]*rho[Xdata.SoilNode]-h_n[Xdata.SoilNode-1]*rho[Xdata.SoilNode-1])/dz_up[Xdata.SoilNode-1])+Xdata.cos_sl*rho[Xdata.SoilNode])*k_np1_m_ip12[Xdata.SoilNode-1]*dt);
						snowsoilinterfaceflux+=tmp_snowsoilinterfaceflux;
					} else {
						snowsoilinterfaceflux=actualbottomflux;
					}
				}
			} else {
				//Make the commented lines active if you whish to add the TopFluxRate to the snowsoilinterfaceflux even when no snow is present.
				//snowsoilinterfaceflux+=TopFluxRate*dt;
			}


			//Determine slope parallel flux, if lateral_flow is enabled.
			if (lateral_flow) {
				const double tmp_sin_sl = sqrt(1. - Xdata.cos_sl * Xdata.cos_sl); 	//Calculate sin of slope, from cos_sl
				for (i = lowernode; i <= uppernode; i++) {				//Cycle through all Richards solver domain layers.
					EMS[i].SlopeParFlux += tmp_sin_sl*K[i]*dt;
				}
			}


			if(WriteDebugOutputput) printf("CONTROL: %.15f %.15f %.15f %.15f %.15f %.15f %f\n", surfacefluxrate, TopFluxRate, actualtopflux, BottomFluxRate, actualbottomflux, snowsoilinterfaceflux, dt);

			//Time step control
			//This time step control increases the time step when niter is below a certain value. When rewinds occurred in the time step, no change is done (dt already adapted by the rewind-mechanim), if too many iterations, time step is decreased.
			if((niter+(MAX_ITER*niter_nrewinds))<INCR_ITER) {
				dt*=1.25;
			} else {
				if(niter_nrewinds==0 && niter>DECR_ITER) {
					dt*=0.5;
				}
			}

			//Limit time steps:
			if ( dt < MIN_VAL_TIMESTEP) dt=MIN_VAL_TIMESTEP;
			if ( dt > MAX_VAL_TIMESTEP) dt=MAX_VAL_TIMESTEP;

			//Time step statistics
			if(stats_min_dt>dt) stats_min_dt=dt;
			if(stats_max_dt<dt) stats_max_dt=dt;

			//Update mass balance status variable (mass1 becomes mass2 to serve as reference for the next iteration)
			mass1=mass2;

			if(WriteDebugOutputput) printf("NSTEPS: %d, TIME ADVANCE: %f, ITERS NEEDED: %d [%d], ACTUALTOPFLUX: %.10f     ---> new dt: %.15f\n", nsteps, TimeAdvance, niter, niter_nrewinds, actualtopflux, dt);
		}	//END DoRewindFlag==false
	}
	while(StopLoop==false);							//This is the main loop to perform 1 SNOWPACK time step

	// Copy results back to SNOWPACK
	for (i = lowernode; i <= uppernode; i++) {						//We loop over all Richards solver domain layers

		if(EMS[i].theta[SOIL]>Constants::eps2) {					//We are in soil
			EMS[i].theta[WATER]=EMS[i].VG.fromHtoTHETAforICE(h_n[i], theta_i_n[i]);
			EMS[i].theta[ICE]=theta_i_n[i];
		} else {									//We are in snow
			EMS[i].theta[WATERINDEX]=EMS[i].VG.fromHtoTHETAforICE(h_n[i], theta_i_n[i]);
		}
		EMS[i].h=h_n[i];

		//In case we had to melt ice to get theta_r, we have to adjust the temperature:
		EMS[i].Te -= dT[i];
		if(i==nE-1) {
			NDS[i+1].T-=dT[i];
			NDS[i].T-=dT[i];
		}

		//And adjust all the properties accordingly
		EMS[i].theta[AIR]=1.-EMS[i].theta[WATER]-EMS[i].theta[WATER_PREF]-EMS[i].theta[ICE]-EMS[i].theta[SOIL];
		//Now we have checked everything, we make it fit between [0, 1]: to get rid off all round-off errors
		EMS[i].theta[AIR]=std::max(0., std::min(1., EMS[i].theta[AIR]));
		EMS[i].theta[WATER]=std::max(0., std::min(1., EMS[i].theta[WATER]));
		EMS[i].theta[WATER_PREF]=std::max(0., std::min(1., EMS[i].theta[WATER_PREF]));
		EMS[i].theta[ICE]=std::max(0., std::min(1., EMS[i].theta[ICE]));
		EMS[i].updDensity();
		EMS[i].M=EMS[i].L*EMS[i].Rho;
		EMS[i].heatCapacity();

		//Every change in ice content in a specific layer must be associated with phase changes. Store the associated energy accordingly.
		EMS[i].Qmf += ((EMS[i].theta[ICE]-snowpackBACKUPTHETAICE[i]) * Constants::density_ice * Constants::lh_fusion) / sn_dt;	// Units: [W m-3]
		//We transferred the temperature change of the element due to soil freezing/thawing in Qmf, so reset delta_Te:
		delta_Te[i]=0.;
	}

	//double max_flux=0.;
	if(enable_pref_flow) {
		if(matrix==true) {
			// We calculate the pref_flow area now
			for (i = lowernode; i <= uppernode; i++) {
				// These commented lines may become useful for a criterion where the "system influx rate" is used, as is typical for preferential flow area
				//const double flux_compare = (i==0) ? (0.) : (
				//(i==uppernode)?(surfacefluxrate):(((((h_n[i]-h_n[i-1])/dz_up[i-1])+cos_sl)*sqrt(K[i]*K[i-1])*dt))
				//);
				//if(max_flux<flux_compare) max_flux=flux_compare;
				if(i>=Xdata.SoilNode) {	//For snow only
					// Volumetric area:
					//vol_area = exp(0.09904-3.557*(EMS[i].ogs));		// As presented at EGU 2016.
					const double vol_area = 0.0584 * pow((0.5*EMS[i].ogs), -1.1090277);

					const double area = std::max(0.01, std::min(0.90, vol_area));

					// Max is to ensure the pref flow area doesn't decrease below saturation of the pref flow path.
					EMS[i].PrefFlowArea = std::min(0.999*(1.-(EMS[i].theta[WATER]/((1.-EMS[i].theta[ICE])*(Constants::density_ice/Constants::density_water)))), std::max(1.001*(EMS[i].theta[WATER_PREF]/((1.-EMS[i].theta[ICE])*(Constants::density_ice/Constants::density_water))), area));
				} else {
					EMS[i].PrefFlowArea = 0.;
				}
			}
		}
	} else {
		for (i = lowernode; i <= uppernode; i++) {
			EMS[i].PrefFlowArea = 0.;
		}
	}

	// Here is a very very crucial part. Here the water wil be either transferred to matrix or preferential domain.
	// To determine the thresholds below, we use the water entry pressure, as provided in Eq. 15 in Hirashima et al. (2014).
	i = uppernode + 1;
	while (i-- > lowernode) {
		if(enable_pref_flow) {
			if(i>=Xdata.SoilNode) {	// For snow
				if(matrix) {
					// First from matrix to preferential flow ...
					if(i==Xdata.SoilNode) {
						// First snow layer should not put water in soil directly
						// Calculate threshold in the current layer that belongs to water entry pressure of the layer
						const double dummy=EMS[i].VG.theta_s;
						EMS[i].VG.theta_s=(1.-EMS[i].theta[ICE])*(Constants::density_ice/Constants::density_water)*(1.-EMS[i].PrefFlowArea);
						const double matrix_threshold=std::max(0.001, EMS[i].VG.fromHtoTHETA((-1.*((0.0437 / EMS[i].ogs) + 0.01074))));
						EMS[i].VG.theta_s=dummy;
						if(EMS[i].theta[WATER]>matrix_threshold) {
							const double dtheta_w=std::max(0., (EMS[i].theta[WATER]-matrix_threshold));
							EMS[i].theta[WATER_PREF]+=dtheta_w;
							EMS[i].theta[WATER]-=dtheta_w;
						}
						/*const double theta_thr=pref_threshold*pref_flowarea[i];
						if(EMS[i].theta[WATER_PREF]>theta_thr) {
							EMS[i].theta[WATER]+=(EMS[i].theta[WATER_PREF]-theta_thr);
							EMS[i].theta[WATER_PREF]=theta_thr;
						}*/
					} else {
						// Calculate threshold in the current layer that belongs to water entry pressure of the layer below
						const double dummy=EMS[i].VG.theta_s;
						EMS[i-1].VG.theta_s=(1.-EMS[i-1].theta[ICE])*(Constants::density_ice/Constants::density_water)*(1.-EMS[i-1].PrefFlowArea);
						const double matrix_threshold=std::max(0.001, EMS[i-1].VG.fromHtoTHETA((-1.*((0.0437 / (pref_flow_param_heterogeneity_factor * EMS[i-1].ogs)) + 0.01074))-dz_up[i-1]));
						EMS[i].VG.theta_s=dummy;
						if(EMS[i].theta[WATER]>matrix_threshold) {
							// Enforcing equal saturation between matrix part at [i] and preferential part at [i-1]
							const double tmp_theta_water_tot = EMS[i].theta[WATER]*EMS[i].L + EMS[i-1].theta[WATER_PREF]*EMS[i-1].L;
							const double s1=(1.-EMS[i].theta[ICE])*(Constants::density_ice/Constants::density_water)*(1.-EMS[i].PrefFlowArea);	// theta_s matrix flow
							const double s2=(1.-EMS[i-1].theta[ICE])*(Constants::density_ice/Constants::density_water)*(EMS[i-1].PrefFlowArea);	// theta_s pref flow
							const double dtheta_w_ideal = std::max(0., (-1.*(EMS[i].VG.theta_r*s2*EMS[i].L - tmp_theta_water_tot*s2) / (s2*EMS[i-1].L + (s1-EMS[i].VG.theta_r)*EMS[i].L) - EMS[i-1].theta[WATER_PREF]));

							const double dtheta_w=std::max(0.,												//No negative change!
										  std::min(std::max(dtheta_w_ideal, (EMS[i].theta[WATER]-matrix_threshold)*(EMS[i].L/EMS[i-1].L))	//The amount that is ideally transferred
										  , 0.999*(												//Keep a bit of room
										  std::min((1.-EMS[i-1].theta[ICE])*(Constants::density_ice/Constants::density_water)*EMS[i-1].PrefFlowArea-EMS[i-1].theta[WATER_PREF], ((EMS[i].theta[WATER]-theta_d[i])*(EMS[i].L/EMS[i-1].L)))	//Take MIN of: (i) Don't oversaturate preferential part, and (ii) don't take too much from the matrix part (TODO: actually, this should never happen.... remove it?)
										  )));
							if(WriteDebugOutputput) printf("MATRIX->PREF [%d]: %f %f %f %f %f\n", int(i), EMS[i].theta[WATER], EMS[i].theta[WATER_PREF], EMS[i-1].theta[WATER], EMS[i-1].theta[WATER_PREF], dtheta_w);
							EMS[i-1].lwc_source+=dtheta_w;	// This works because preferential flow is executed after matrix flow, so the source/sink term will be used directly afterwards.
							EMS[i].theta[WATER]-=dtheta_w*(EMS[i-1].L/EMS[i].L);
							// After moving the water, adjust the other properties
							EMS[i].theta[AIR]=1.-EMS[i].theta[WATER]-EMS[i].theta[WATER_PREF]-EMS[i].theta[ICE]-EMS[i].theta[SOIL];
							EMS[i].updDensity();
							EMS[i].M=EMS[i].Rho*EMS[i].L;
							EMS[i-1].theta[AIR]=1.-EMS[i-1].theta[WATER]-EMS[i-1].theta[WATER_PREF]-EMS[i-1].theta[ICE]-EMS[i-1].theta[SOIL];
							EMS[i-1].updDensity();
							EMS[i-1].M=EMS[i-1].Rho*EMS[i-1].L;
						}

						// If the matrix pressure head is larger than the preferential flow pressure head (ensured by std::max(0., ....)), we equalize both domains in terms of saturation.
						// This is because in wet snow, the preferential flow part is also wet. Moreover, it enables a smaller capillary suction in the preferential flow domain, and allows the water to flow downwards
						const double tmp_theta_water_tot = EMS[i].theta[WATER] + EMS[i].theta[WATER_PREF];
						const double dtheta_w = std::max(0., (-1. * ( (EMS[i].VG.theta_r - tmp_theta_water_tot) * (1.-EMS[i].theta[ICE])*(Constants::density_ice/Constants::density_water)*(EMS[i].PrefFlowArea) ) / ((1.-EMS[i].theta[ICE])*(Constants::density_ice/Constants::density_water) - EMS[i].VG.theta_r)) - EMS[i].theta[WATER_PREF]);
						EMS[i].lwc_source+=dtheta_w;	// This works because preferential flow is executed after matrix flow, so the source/sink term will be used directly afterwards.
						EMS[i].theta[WATER]-=dtheta_w;
						EMS[i].theta[AIR]=1.-EMS[i].theta[WATER]-EMS[i].theta[WATER_PREF]-EMS[i].theta[ICE]-EMS[i].theta[SOIL];
						EMS[i].updDensity();
						EMS[i].M=EMS[i].Rho*EMS[i].L;
					}
				} else {
					// Now from preferential to matrix flow
					if(i==Xdata.SoilNode) {
						//For the snow layer just above the soil, we equalize the saturation in the matrix and preferential flow domain
						//This leads to more realistic snowpack runoff behavior, as with the approach for the other snow layers, spiky behavior arises from whether or not pref_threshold is exceeded
						const double tmp_theta_water_tot = EMS[i].theta[WATER] + EMS[i].theta[WATER_PREF];
						//Note that the std::max(0., ...) ensures that the water flow is from preferential flow to matrix flow domain
						const double dtheta_w2 = std::max(0., EMS[i].theta[WATER_PREF] + ( (EMS[i].VG.theta_r - tmp_theta_water_tot) * (1.-EMS[i].theta[ICE])*(Constants::density_ice/Constants::density_water)*(EMS[i].PrefFlowArea) ) / ((1.-EMS[i].theta[ICE])*(Constants::density_ice/Constants::density_water) - EMS[i].VG.theta_r));
						//const double dtheta_w2 = std::min(EMS[i].theta[WATER_PREF], std::max(0., EMS[i].theta[WATER_PREF] + ( (EMS[i].VG.theta_r - tmp_theta_water_tot) * (1.-EMS[i].theta[ICE])*(Constants::density_ice/Constants::density_water)*(EMS[i].PrefFlowArea) ) / ((1.-EMS[i].theta[ICE])*(Constants::density_ice/Constants::density_water) - EMS[i].VG.theta_r)));
						EMS[i].theta[WATER_PREF] -= dtheta_w2;
						EMS[i].theta[WATER] += dtheta_w2;
					} else {
						// For other snow layers than the lowest snow layer above the soil
						if(EMS[i].theta[WATER_PREF]/((1.-EMS[i].theta[ICE])*(Constants::density_ice/Constants::density_water)*(EMS[i].PrefFlowArea)) > pref_flow_param_th) {
							// Using the code from PhaseChange.cc, we estimate the refreezing capacity
							const double deltaT = EMS[i].meltfreeze_tk - EMS[i].Te;
							// Adapt A to compute mass changes
							double A = (EMS[i].c[TEMPERATURE] * EMS[i].Rho) / ( Constants::density_ice * Constants::lh_fusion );
							// Compute the change in volumetric ice and water contents
							const double dth_i = - A * deltaT;
							const double dth_w = std::min(EMS[i].theta[WATER_PREF]-theta_d[i], - (Constants::density_ice / Constants::density_water) * dth_i);
							const double dtheta_w1 = std::max(0.,										//No negative change!
										std::min((dth_w)									//The amount that is ideally transferred
										, 0.999*(										//Keep a bit of room
										(1.-EMS[i].theta[ICE])*(Constants::density_ice/Constants::density_water)*(1.-EMS[i].PrefFlowArea)-EMS[i].theta[WATER])		//Don't over-saturate matrix part
										));

							EMS[i].theta[WATER]+=dtheta_w1;
							EMS[i].theta[WATER_PREF]-=dtheta_w1;

							if(EMS[i].theta[WATER_PREF]/((1.-EMS[i].theta[ICE])*(Constants::density_ice/Constants::density_water)*(EMS[i].PrefFlowArea)) > pref_flow_param_th) {
								// This approach is equalizing both domains in case we still exceed the threshold:
								const double tmp_theta_water_tot = EMS[i].theta[WATER] + EMS[i].theta[WATER_PREF];
								const double dtheta_w2 = std::max(0., EMS[i].theta[WATER_PREF] + ( (EMS[i].VG.theta_r - tmp_theta_water_tot) * (1.-EMS[i].theta[ICE])*(Constants::density_ice/Constants::density_water)*(EMS[i].PrefFlowArea) ) / ((1.-EMS[i].theta[ICE])*(Constants::density_ice/Constants::density_water) - EMS[i].VG.theta_r));
								EMS[i].theta[WATER_PREF] -= dtheta_w2;
								EMS[i].theta[WATER] += dtheta_w2;
							}
						}
						const double dx = sqrt((1. + EMS[i].PrefFlowArea)/(2. * Constants::pi)) - sqrt(EMS[i].PrefFlowArea/Constants::pi);	// Estimate of the typical length scale that determines the gradients

						// Now consider refreeze due to temperature difference (mimicked by transferring water from preferential flow to matrix domain)
						const double heat_flux = ((Constants::meltfreeze_tk - EMS[i].Te) / dx) * EMS[i].k[TEMPERATURE];	// Units: [W/m^2], Eq. 6 in Wever et al. (2016), TC. Note that the paper reports wrong units here.
						const double theta_move = (pref_flow_param_N * 2. * sqrt(EMS[i].PrefFlowArea * Constants::pi) * heat_flux * sn_dt) / Constants::lh_fusion / Constants::density_water;	// Units: [m^3/m^3], Eq. 7 in Wever et al. (2016), TC. Note that the paper reports a different and wrong equation here, which is not consistent with the units.

						// Make sure that theta[WATER_PREF] is not negative and do the actual transfer!
						const double dtheta_w3 = std::max(0., std::min(std::min(EMS[i].theta[WATER_PREF]-theta_d[i], 0.999*(1.-EMS[i-1].theta[ICE])*(Constants::density_ice/Constants::density_water)*(1.-EMS[i-1].PrefFlowArea)-EMS[i].theta[WATER]), theta_move));
						EMS[i].theta[WATER]+=dtheta_w3;
						EMS[i].theta[WATER_PREF]-=dtheta_w3;
					}
					// Check for first wetting to set microstructural marker correctly
					if ((EMS[i].theta[WATER] > 5E-6 * sn_dt) && (EMS[i].mk%100 < 10)) {
						EMS[i].mk += 10;
					}
				}
			} else {	// For soil, we suppress preferential flow
				const double pref_threshold=0.;
				if(EMS[i].theta[WATER_PREF]>pref_threshold) {
					EMS[i].theta[WATER]+=(EMS[i].theta[WATER_PREF]-pref_threshold);
					EMS[i].theta[WATER_PREF]=pref_threshold;
				}
				if(EMS[i].theta[WATER_PREF]<pref_threshold) {
					EMS[i].theta[WATER]+=(EMS[i].theta[WATER_PREF]-pref_threshold);
					EMS[i].theta[WATER_PREF]=pref_threshold;
				}
			}
		}

		//Then check the volumetric contents. This we do, to make a crash at this place, and we have information about the Richards solver available in the core file.
		//Do some checks on volumetric contents:
		const double sum=EMS[i].theta[AIR] + EMS[i].theta[WATER] + EMS[i].theta[WATER_PREF] + EMS[i].theta[ICE] + EMS[i].theta[SOIL];
		if(EMS[i].theta[WATER]<0.-Constants::eps2 || EMS[i].theta[WATER_PREF]<0.-Constants::eps2 || EMS[i].theta[AIR]<0.-Constants::eps2 || EMS[i].theta[AIR] > 1.+Constants::eps2 || EMS[i].theta[ICE]<0.-Constants::eps2 || EMS[i].theta[ICE] > 1.+Constants::eps2) {
			printf("ERROR in [%d] at layer %d: sum=%f air=%f ice=%f soil=%f water=%f water_pref=%f\n", (matrix)?(1):(0), int(i), sum, EMS[i].theta[AIR], EMS[i].theta[ICE], EMS[i].theta[SOIL], EMS[i].theta[WATER], EMS[i].theta[WATER_PREF]);
			printf("   -- if this happens and ice<0, check theta_d. Maybe there was so much water created, that it was more than there was ice. This is not accounted for.\n");
			throw;
		}
		if(sum > 1.+Constants::eps2) {
			printf("ERROR in [%d] at layer %d: sum=%f air=%f ice=%f soil=%f water=%f water_pref=%f\n", (matrix)?(1):(0), int(i), sum, EMS[i].theta[AIR], EMS[i].theta[ICE], EMS[i].theta[SOIL], EMS[i].theta[WATER], EMS[i].theta[WATER_PREF]);
			throw;
		}
		if(sum < 1.-Constants::eps2) {
			printf("ERROR in [%d] at layer %d: sum=%f air=%f ice=%f soil=%f water=%f water_pref=%f\n", (matrix)?(1):(0), int(i), sum, EMS[i].theta[AIR], EMS[i].theta[ICE], EMS[i].theta[SOIL], EMS[i].theta[WATER], EMS[i].theta[WATER_PREF]);
			throw;
		}
	}

	i = uppernode + 1;
	while (i-- > lowernode) {
		if(nE > 1) {
			//Heat advection by water flow
			double deltaN=0.;
			if(i == nE-1 && i > 0) {
				deltaN=(delta_Te_adv[i] * (EMS[i].c[TEMPERATURE]*EMS[i].Rho*EMS[i].L)) / (EMS[i].c[TEMPERATURE]*EMS[i].Rho*EMS[i].L + 0.5*EMS[i-1].c[TEMPERATURE]*EMS[i-1].Rho*EMS[i-1].L);
			} else {
				if(i==0) {
					deltaN=(delta_Te_adv[i] * (EMS[i].c[TEMPERATURE]*EMS[i].Rho*EMS[i].L)) / (0.5*EMS[i+1].c[TEMPERATURE]*EMS[i+1].Rho*EMS[i+1].L + EMS[i].c[TEMPERATURE]*EMS[i].Rho*EMS[i].L);
				} else {
					deltaN=(delta_Te_adv[i] * (EMS[i].c[TEMPERATURE]*EMS[i].Rho*EMS[i].L)) / (0.5*EMS[i+1].c[TEMPERATURE]*EMS[i+1].Rho*EMS[i+1].L + EMS[i].c[TEMPERATURE]*EMS[i].Rho*EMS[i].L + 0.5*EMS[i-1].c[TEMPERATURE]*EMS[i-1].Rho*EMS[i-1].L);
				}
			}
			NDS[i+1].T+=deltaN;
			NDS[i].T+=deltaN;
			if(fabs(deltaN)>0.) {
				if(i < nE-1) EMS[i+1].Te=0.5*(NDS[i+2].T+NDS[i+1].T);
				EMS[i].Te=0.5*(NDS[i+1].T+NDS[i].T);
				if(i > 0) EMS[i-1].Te=0.5*(NDS[i].T+NDS[i-1].T);
			}
		} else {
			// If there is only 1 element, we don't care about heat advection...
		}
		if (WriteDebugOutputput) printf("SENDING at layer %d: sum=%f air=%.15f ice=%.15f soil=%.15f water=%.15f water_pref=%.15f Te=%.15f h=%f %f\n", int(i), EMS[i].theta[AIR]+EMS[i].theta[ICE]+EMS[i].theta[SOIL]+EMS[i].theta[WATER]+EMS[i].theta[WATER_PREF], EMS[i].theta[AIR], EMS[i].theta[ICE], EMS[i].theta[SOIL], EMS[i].theta[WATER], EMS[i].theta[WATER_PREF], EMS[i].Te, EMS[i].h, EMS[i].VG.fromTHETAtoH(EMS[i].theta[WATER], h_d));
	}


	if(WriteDebugOutputput) {
		printf("ACTUALTOPFLUX: [ BC: %d ] %.15f %.15f %.15f CHK: %f\n", TopBC, actualtopflux/sn_dt, refusedtopflux/sn_dt, surfacefluxrate, (surfacefluxrate!=0.)?(actualtopflux/sn_dt)/surfacefluxrate:0.);
		printf("ACTUALBOTTOMFLUX: [ BC: %d ] %.15f %.15f %.15f %f    SNOWSOILINTERFACEFLUX=%.15f\n", BottomBC, actualbottomflux, actualbottomflux/sn_dt, BottomFluxRate, (BottomFluxRate!=0.)?(actualbottomflux/sn_dt)/BottomFluxRate:0., snowsoilinterfaceflux/sn_dt);
	}


	if(WriteDebugOutputput) printf("WATERBALANCE: %.15f %.15f %.15f CHK1: %.15f  MB_ERROR: %.15f\n", actualtopflux/sn_dt, refusedtopflux/sn_dt, surfacefluxrate, (surfacefluxrate!=0.)?(actualtopflux/sn_dt)/surfacefluxrate:0., massbalanceerror_sum);

	//Update soil runoff (mass[MS_SOIL_RUNOFF] = kg/m^2). Note: it does not matter whether SNOWPACK is run with soil or not. MS_SOIL_RUNOFF is always runoff from lower boundary.
	Sdata.mass[SurfaceFluxes::MS_SOIL_RUNOFF] += actualbottomflux*Constants::density_water;

	// Update snow pack runoff (mass[MS_SNOWPACK_RUNOFF] = kg/m^2 (almost equal to mm/m^2), surfacefluxrate=m^3/m^2/s and snowsoilinterfaceflux = m^3/m^2):
	// NOTE: snowsoilinterfaceflux will only be non-zero IF there is a snowpack AND we solve the richards equation also for snow! Else, snowpack runoff is calculated in the original WaterTransport functions.
	Sdata.mass[SurfaceFluxes::MS_SNOWPACK_RUNOFF] += snowsoilinterfaceflux*Constants::density_water;

	//Deal with the situation that evaporation flux was limited in case of snow. Then, sublimate ice matrix.
	if (refusedtopflux<0. && uppernode+1>=Xdata.SoilNode) {
		//Be careful: refusedtopflux = m^3/m^2 and not m^3/m^2/s!!!
		//Now invert the calculation of ql, using refusedtopflux. This amount of ql could not be used for evaporation and should be used for sublimation.
		ql += (refusedtopflux/sn_dt)*Constants::density_water*Constants::lh_vaporization;
		refusedtopflux = 0.;
		//First, we fully intepreted ql as evaporation. Now, remaining energy (ql) should not be counted as evaporation
		Sdata.mass[SurfaceFluxes::MS_EVAPORATION] -= ql*sn_dt/Constants::lh_vaporization;
		if(uppernode+1==Xdata.SoilNode) {
			//The energy is substracted from the top element
			//const double tmp_delta_Te = ql / (EMS[Xdata.SoilNode-1].c[TEMPERATURE] * EMS[Xdata.SoilNode-1].Rho);
			//NDS[Xdata.SoilNode].T += 2.*tmp_delta_Te;
			//EMS[Xdata.SoilNode-1].Te += tmp_delta_Te;
		}
	}

	//If we could not handle all incoming water at top boundary AND we have snow AND we solve RE for snow:
	if(refusedtopflux>0. && uppernode+1>Xdata.SoilNode) {
		Sdata.mass[SurfaceFluxes::MS_SNOWPACK_RUNOFF] += refusedtopflux*Constants::density_water;
	}

	//If we could not handle all snowpack runoff when not modelling snow with RE, add water layer
	if(allow_surface_ponding == true && refusedtopflux > Constants::eps) {
		Xdata.resize(nE+1);
		const size_t newnE = Xdata.getNumberOfElements();
		Xdata.Edata[newnE-1] = Xdata.Edata[Xdata.getNumberOfElements()-2];
		Xdata.Ndata[Xdata.getNumberOfNodes()-1] = Xdata.Ndata[Xdata.getNumberOfNodes()-2];
		Xdata.Edata[newnE-1].theta[WATER] = 1.;
		Xdata.Edata[newnE-1].theta[WATER_PREF] = 0.;
		Xdata.Edata[newnE-1].theta[ICE] = 0.;
		Xdata.Edata[newnE-1].theta[AIR] = 0.;
		Xdata.Edata[newnE-1].theta[SOIL] = 0.;
		Xdata.Edata[newnE-1].L = Xdata.Edata[newnE-1].L0 = refusedtopflux;
		EMS[newnE-1].updDensity();
		EMS[newnE-1].M = EMS[newnE-1].L*EMS[newnE-1].Rho;
		EMS[newnE-1].Te = (backupWATERLAYER_Te != Constants::undefined) ? (backupWATERLAYER_Te) : NDS[newnE-2].T;
		NDS[newnE].T = NDS[newnE-1].T = NDS[newnE-2].T = EMS[newnE-1].Te;
		Xdata.Edata[newnE-1].mk = 19;	// Mark the layer as a water layer
		prn_msg( __FILE__, __LINE__, "wrn", Date(), "Ponding occuring, water layer added! [depth = %lf m]", Xdata.Edata[newnE-1].L);
	}


	surfacefluxrate=0.;			//As we now have used the rate for the current time step, reset the value.

	//Now set freezing point depression correctly:
	if(matrix==true) {
		for (i = lowernode; i <= uppernode; i++) {
			if(EMS[i].theta[SOIL]<Constants::eps2) {
				EMS[i].meltfreeze_tk=Constants::meltfreeze_tk;
			} else {
				//For soil layers solved with Richards Equation, everything (water transport and phase change) is done in this routine, except calculating the heat equation.
				//To suppress phase changes in PhaseChange.cc, set the melting and freezing temperature equal to the element temperature:
				EMS[i].meltfreeze_tk=std::min(Constants::meltfreeze_tk, EMS[i].Te);
			}
		}
	}

	return;
}

#ifdef __clang__
#pragma clang diagnostic pop
#endif
