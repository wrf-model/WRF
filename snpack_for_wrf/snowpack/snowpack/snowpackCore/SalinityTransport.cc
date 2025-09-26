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

#include <snowpack/snowpackCore/SalinityTransport.h>
#include <snowpack/snowpackCore/ReSolver1d.h>
#include <snowpack/Utils.h>
#include <stdio.h>

static const bool ZeroFluxLowerBoundary_diffusion = true;
static const bool ZeroFluxUpperBoundary_diffusion = true;
static const bool ZeroFluxLowerBoundary_advection = false;
static const bool ZeroFluxUpperBoundary_advection = false;

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


#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#endif


/**
 * @brief Class for solving diffusion-advection equation for salinity using the Crank-Nicolson implicit method\n
 * Solve Richards Equation \n
 * @author Nander Wever
 * @param nE Domain size (number of elements)
 */
SalinityTransport::SalinityTransport(const size_t nE)
           : flux_up(), flux_down(), flux_up_2(), flux_down_2(),dz_(), dz_up(), dz_down(), theta1(), theta2(), BrineSal(), D(), sb(), BottomSalinity(0.), TopSalinity(0.),
	     BottomSalFlux(0.), TopSalFlux(0.), NumberOfElements(0)
{
	SetDomainSize(nE);
}


/**
 * @brief Resizing vectors to match given domain size \n
 * @author Nander Wever
 * @param nE Domain size (number of elements)
 */
void SalinityTransport::SetDomainSize(size_t nE) {
	NumberOfElements = nE;

	flux_up.resize(nE, 0.);
	flux_down.resize(nE, 0.);
	flux_up_2.resize(nE, 0.);
	flux_down_2.resize(nE, 0.);
	dz_.resize(nE, 0.);
	dz_up.resize(nE, 0.);
	dz_down.resize(nE, 0.);
	theta1.resize(nE, 0.);
	theta2.resize(nE, 0.);
	BrineSal.resize(nE, 0.);
	D.resize(nE, 0.);
	sb.resize(nE, 0.);
	return;
}


/**
 * @brief Solve diffusion-advection equation using the Crank-Nicolson implicit, or fully implicit method\n
 * @author Nander Wever
 * @par This function solves the following equation ($n$ and $i$ denoting time and spatial level, respectively), in Latex code:
\begin{multline}
\frac{ \left ( \theta^{n+1}_i S_{\mathrm{b}, i}^{n+1} - \theta^{n}_i S_{\mathrm{b}, i}^{n} \right ) } { \Delta t } \\
- f \left [ \left ( \frac{ 2 D_{i+1}^{n} \theta^{n+1}_{i+1} S_{\mathrm{b}, i+1}^{n+1} }{ \Delta z_{\mathrm{up}} \left ( \Delta z_{\mathrm{up}} + \Delta z_{\mathrm{down}} \right ) } - \frac{ 2 D_{i}^{n} \theta^{n+1}_{i} S_{\mathrm{b}, i}^{n+1} }{\left ( \Delta z_{\mathrm{up}} \Delta z_{\mathrm{down}} \right ) } + \frac{ D_{i-1}^{n} \theta^{n+1}_{i-1} S_{\mathrm{b}, i-1}^{n+1} }{ \Delta z_{\mathrm{down}} \left ( \Delta z_{\mathrm{up}} + \Delta z_{\mathrm{down}} \right ) } \right ) \right ] \\
- \left ( 1-f \right ) \left [ \left ( \frac{ 2 D_{i+1}^{n} \theta^{n}_{i+1} S_{\mathrm{b}, i+1}^{n} }{ \Delta z_{\mathrm{up}} \left ( \Delta z_{\mathrm{up}} + \Delta z_{\mathrm{down}} \right ) } - \frac{ 2 D_{i}^{n} \theta^{n}_{i} S_{\mathrm{b}, i}^{n} }{\left ( \Delta z_{\mathrm{up}} \Delta z_{\mathrm{down}} \right ) } + \frac{ D_{i-1}^{n} \theta^{n}_{i-1} S_{\mathrm{b}, i-1}^{n} }{ \Delta z_{\mathrm{down}} \left ( \Delta z_{\mathrm{up}} + \Delta z_{\mathrm{down}} \right ) } \right ) \right ] \\
- f \left [ \left ( \frac{q^{n}_{i+1} S_{\mathrm{b},i+1}^{n+1} - q^{n}_{i-1} S_{\mathrm{b},i-1}^{n+1}}{\left ( \Delta z_{\mathrm{up}} + \Delta z_{\mathrm{down}} \right ) } \right ) \right ] - \left ( 1-f \right ) \left [ \left ( \frac{q^{n}_{i+1} S_{\mathrm{b},i+1}^{n} - q^{n}_{i-1} S_{\mathrm{b},i-1}^{n}}{\left ( \Delta z_{\mathrm{up}} + \Delta z_{\mathrm{down}} \right ) } \right ) \right ] - s_{\mathrm{sb}} = 0
\end{multline}
Here, $f=1$ results in the fully implicit scheme, whereas $f=0.5$ corresponds to the Crank-Nicolson scheme. The implicit scheme is first order accurate, whereas the Crank-Nicolson scheme is second order accurate. Furthermore, both are unconditionally stable and suffer only minimal numerical diffusion for the advection part. As with many other common schemes, the advection part is not perfectly conserving sharp transitions. Futhermore, the reason to not use the fully implicit or the Crank Nicolson scheme is the occurrence of spurious oscillations in the solution, which negatively impact the accuracy of the simulations more than the negative effect on computational efficiency imposed by the CFL criterion required for the explicit method (see SalinityTransport::SolveSalinityTransportEquationExcplicit).
 * @param dt Time step (s)
 * @param DeltaSal Result vector (change in salinity over time step)
 * @return false on error, true otherwise
 */
bool SalinityTransport::SolveSalinityTransportEquationImplicit(const double dt, std::vector <double> &DeltaSal, const double f, const bool DonorCell) {

	if(NumberOfElements==0) return false;	// Nothing to do

	const bool WriteDebugOutput = false;
	if(WriteDebugOutput) setvbuf(stdout, NULL, _IONBF, 0);

	// Declare and initialize l.h.s. matrix and r.h.s. vector
	std::vector<double> ad(NumberOfElements, 0.);		// Matrix diagonal
	std::vector<double> adu(NumberOfElements-1, 0.);	// Matrix upper diagonal
	std::vector<double> adl(NumberOfElements-1, 0.);	// Matrix lower diagonal
	std::vector<double> b(NumberOfElements, 0.);		// Vector

	if(ZeroFluxUpperBoundary_advection) flux_up[NumberOfElements-1] = 0.;
	if(ZeroFluxLowerBoundary_advection) flux_down[0] = 0.;

	// Fill matrix and r.h.s. vector
	for(size_t i = 0; i < NumberOfElements; i++) {

		// The matrix diagonal, the time derivative:
		ad[i] += theta2[i] / dt;


		// The matrix diagonal, the diffusion part:
		if(ZeroFluxLowerBoundary_diffusion && i==0) {
			ad[i] += f * (2. * D[i] * theta2[i]) / (dz_up[i] * (dz_up[i] + dz_down[i]));
		} else if(ZeroFluxUpperBoundary_diffusion && i==NumberOfElements-1) {
			ad[i] += f * (2. * D[i] * theta2[i]) / (dz_down[i] * (dz_up[i] + dz_down[i]));
		} else {
			ad[i] += f * (2. * D[i] * theta2[i]) / (dz_up[i] * dz_down[i]);
		}


		// The lower diagonal
		if(i==0) {
			// the diffusion part:
			if(ZeroFluxLowerBoundary_diffusion) {
				// ZeroFluxLowerBoundary_diffusion: no contribution from below
			} else {
				// The diffusion term from below is added to the r.h.s.
			}

			// the advection part from below is a constant flux and is added to the r.h.s.
		} else if(i==NumberOfElements-1) {
			// the diffusion part:
			if(NumberOfElements>1) adl[i-1] += -f * 2. * D[i-1] * theta2[i-1] / (dz_down[i] * (dz_up[i] + dz_down[i]));

			// the advection part:
			if(!DonorCell || flux_down[i] < 0.) {
				adl[i-1] += f * flux_down[i] / (dz_up[i] + dz_down[i]) / ((DonorCell) ? (0.5) : (1.));
			} else {
				ad[i] += f * flux_down[i] / (dz_up[i] + dz_down[i]) / 0.5;
			}
		} else {
			// the diffusion part:
			adl[i-1] += -f * 2. * D[i-1] * theta2[i-1] / (dz_down[i] * (dz_up[i] + dz_down[i]));

			// the advection part:
			if(!DonorCell || flux_down[i] < 0.) {
				adl[i-1] += f * flux_down[i] / (dz_up[i] + dz_down[i]) / ((DonorCell) ? (0.5) : (1.));
			} else {
				ad[i] += f * flux_down[i] / (dz_up[i] + dz_down[i]) / 0.5;
			}
		}


		// The upper diagonal
		if(i==0) {
			// the diffusion part:
			if(NumberOfElements>1) adu[i] += -f * 2. * D[i+1] * theta2[i+1] / (dz_up[i] * (dz_up[i] + dz_down[i]));

			// the advection part:
			if(!DonorCell || flux_up[i] > 0.) {
				adu[i] += -f * flux_up[i] / (dz_up[i] + dz_down[i]) / ((DonorCell) ? (0.5) : (1.));
			} else {
				ad[i] += -f * flux_up[i] / (dz_up[i] + dz_down[i]) / 0.5;
			}
		} else if(i==NumberOfElements-1) {
			if(ZeroFluxUpperBoundary_diffusion) {
				// ZeroFluxUpperBoundary: no contribution from above
			} else {
				// The diffusion term from above is added to the r.h.s.
			}

			// the advection part from above is a constant flux and is added to the r.h.s.
		} else {
			// the diffusion part:
			adu[i] += -f * 2. * D[i+1] * theta2[i+1] / (dz_up[i] * (dz_up[i] + dz_down[i]));

			// the advection part:
			if(!DonorCell || flux_up[i] > 0.) {
				adu[i] += -f * flux_up[i] / (dz_up[i] + dz_down[i]) / ((DonorCell) ? (0.5) : (1.));
			} else {
				ad[i] += -f * flux_up[i] / (dz_up[i] + dz_down[i]) / 0.5;
			}
		}


		// The r.h.s. vector time derivative:
		b[i] += (theta1[i] * BrineSal[i]) / dt;


		// The r.h.s. vector diffusion part:
		if(i==NumberOfElements-1) {
			// No flux upper boundary for diffusion (mirroring the i-1 node)
			if(ZeroFluxUpperBoundary_diffusion) {
				if(NumberOfElements>1) {
					b[i] += + (1. - f) * (2. * D[i] * theta1[i] * BrineSal[i]) / (dz_down[i] * (dz_up[i] + dz_down[i]))
					        - (1. - f) * (2. * D[i-1] * theta1[i-1] * BrineSal[i-1]) / (dz_down[i] * (dz_up[i] + dz_down[i]));
				}
			} else {
				b[i] += (1. - f) * 2. * D[i] * theta1[i] * TopSalinity / (dz_up[i] * (dz_up[i] + dz_down[i]));
			}
		} else if(i==0) {
			// No flux lower boundary for diffusion (mirroring the i+1 node)
			if(ZeroFluxLowerBoundary_diffusion) {
				if(NumberOfElements>1) {
					b[i] += + (1. - f) * (2. * D[i] * theta1[i] * BrineSal[i]) / (dz_up[i] * (dz_up[i] + dz_down[i]))
					        - (1. - f) * (2. * D[i+1] * theta1[i+1] * BrineSal[i+1]) / (dz_up[i] * (dz_up[i] + dz_down[i]));
				}
			} else {
				b[i] += (1. - f) * 2. * D[i] * theta1[i] * BottomSalinity / (dz_down[i] * (dz_up[i] + dz_down[i]));
			}
		} else {
			b[i] +=  (1. - f) * (2. * D[i-1] * theta1[i-1] * BrineSal[i-1]) / (dz_down[i] * (dz_up[i] + dz_down[i]))
			     -   (1. - f) * (2. * D[i] * theta1[i] * BrineSal[i]) / (dz_up[i] * dz_down[i])
			     +   (1. - f) * (2. * D[i+1] * theta1[i+1] * BrineSal[i+1]) / (dz_up[i] * (dz_up[i] + dz_down[i]));
		}


		//The r.h.s. vector advection part:
		if(i==0 && i==NumberOfElements-1) {
			// TODO: What to do in the case of only 1 element??
			std::cerr << "Only one snow/ice element present, which is not implemented.\n";
			throw;
		} else if (i==0) {
			b[i] += (1. - f) * (flux_up[i] * ((!DonorCell || flux_up[i]>0.) ? (BrineSal[i+1]) : (BrineSal[i])) - flux_down[i] * ((!DonorCell || flux_down[i]<0.) ? (BottomSalinity) : (BrineSal[i]))) / (dz_up[i] + dz_down[i]) / ((DonorCell) ? (0.5) : (1.));
		} else if (i==NumberOfElements-1) {
			b[i] += (1. - f) * (flux_up[i] * ((!DonorCell || flux_up[i]>0.) ? (TopSalinity) : (BrineSal[i])) - flux_down[i] * ((!DonorCell || flux_down[i]<0.) ? (BrineSal[i-1]) : (BrineSal[i]))) / (dz_up[i] + dz_down[i]) / ((DonorCell) ? (0.5) : (1.));
		} else {
			b[i] += (1. - f) * (flux_up[i] * ((!DonorCell || flux_up[i]>0.) ? (BrineSal[i+1]) : (BrineSal[i])) - flux_down[i] * ((!DonorCell || flux_down[i]<0.) ? (BrineSal[i-1]) : (BrineSal[i]))) / (dz_up[i] + dz_down[i]) / ((DonorCell) ? (0.5) : (1.));
		}


		// The r.h.s. vector source/sink term:
		b[i] += -sb[i];
	}


	// Deal with boundary conditions:

	// Add the terms from "out of boundary" diffusion
	if(!ZeroFluxLowerBoundary_diffusion) b[0] += f * (2. * D[0] * theta2[0] * BottomSalinity) / (dz_down[0] * (dz_up[0] + dz_down[0]));
	if(!ZeroFluxUpperBoundary_diffusion) b[NumberOfElements-1] += f * (2. * D[NumberOfElements-1] * theta2[NumberOfElements-1] * TopSalinity) / (dz_up[NumberOfElements-1] * (dz_up[NumberOfElements-1] + dz_down[NumberOfElements-1]));


	// Add the terms from "out of boundary" advection
	b[0] += -f * (flux_down[0] * ((!DonorCell || flux_down[0]<0.) ? (BottomSalinity) : (0.*BrineSal[0]))) / (dz_up[0] + dz_down[0]) / ((DonorCell) ? (0.5) : (1.));
	b[NumberOfElements-1] += f * (flux_up[NumberOfElements-1] * ((!DonorCell || flux_up[NumberOfElements-1]>0.) ? (TopSalinity) : (0.*BrineSal[NumberOfElements-1]))) / (dz_up[NumberOfElements-1] + dz_down[NumberOfElements-1]) / ((DonorCell) ? (0.5) : (1.));


	// Dump solver info on stdout
	if(WriteDebugOutput) {
		std::cout << "SalinityTransport.cc > Solver:\n";
		std::cout << "   dt = " << dt << "\n";
		std::cout << "SalinityTransport.cc > Coefficients:\n";
		for(size_t i = 0; i < NumberOfElements; i++) {
			if(i==NumberOfElements-1) {
				std::cout << i << ": " << std::scientific << D[i] << " " << flux_up[i] << " " << flux_down[i] << " " << flux_up_2[i] << " " << flux_down_2[i] << " " << sb[i] << " " << theta1[i] << " " << theta2[i] << " " << BrineSal[i] << " " << ad[i] << " " << "---" << " " << "---" << " " << b[i] << "\n";
			} else {
				std::cout << i << ": " << std::scientific << D[i] << " " << flux_up[i] << " " << flux_down[i] << " " << flux_up_2[i] << " " << flux_down_2[i] << " " << sb[i] << " " << theta1[i] << " " << theta2[i] << " " << " " << BrineSal[i] << " " << ad[i] << " " << adl[i] << " " << adu[i] << " " << b[i] << "\n";
			}
		}
	}


	// Track boundary fluxes (time step t)
	BottomSalFlux += -(1. - f) * (flux_down[0] * dt * ((flux_down[0] > 0.) ? (BrineSal[0] * (Constants::density_water + SeaIce::betaS * BrineSal[0])) : (BottomSalinity * (Constants::density_water + SeaIce::betaS * BottomSalinity))));
	TopSalFlux    +=  (1. - f) * (flux_up[NumberOfElements-1] * dt * ((flux_up[NumberOfElements-1] > 0.) ? (TopSalinity * (Constants::density_water + SeaIce::betaS * TopSalinity)) : (BrineSal[NumberOfElements-1] * (Constants::density_water + SeaIce::betaS * BrineSal[NumberOfElements-1]))));


	// Call solver
	const int matrixdimensions=int(NumberOfElements);	// Cast from size_t to int is necessary, to interface correctly with LAPACK dgtsv_.
#ifdef CLAPACK
	// Call LAPACK DGTSV: Solver for tridiagonal matrices, with partial pivoting.
	int info=0;
	const int vectordimensions=1;
	dgtsv_( (integer*) &matrixdimensions, (integer*) &vectordimensions, &adl[0], &ad[0], &adu[0], &b[0], (integer*) &matrixdimensions, (integer*) &info );

	if(info!=0) {
		//= 0: successful exit
		//< 0: if INFO = -i, the i-th argument had an illegal value
		//> 0: if INFO = i, U(i,i) is exactly zero, and the solution
		//    has not been computed.  The factorization has not been
		//    completed unless i = N.
		std::cout << "[E] Error in SalinityTransport.cc: DGTSV failed [info = " << info << "].\n";
		return false;
	}
#else
	// Call TDMASolver: Thomas algorithm for tidiagonal matrices. Not the recommended choice, but useful when LAPACK is not available.
	std::vector<double> b_ = b;
	const int ret = ReSolver1d::TDMASolver(matrixdimensions, &adl[0], &ad[0], &adu[0], &b[0], &b_[0]);
	b=b_;
	if (ret != 0) {
		std::cout << "[E] Error in SalinityTransport.cc: TDMA failed.\n";
		std::cout << "    Using LAPACK (see compile options) may increase numerical stability in SalinityTransport.\n";
		return false;
	}
#endif


	// Apply solution
	if(WriteDebugOutput) std::cout << "SalinityTransport.cc > Solution vector:\n";
	for(size_t i=0; i<NumberOfElements; i++) {
		if(WriteDebugOutput) std::cout << i << ": " << b[i] << " " << b[i] * theta2[i] << "\n";
		DeltaSal[i]=b[i]-BrineSal[i];
		//DeltaSal[i]=b[i]*theta2[i] - BrineSal[i]*theta1[i];
		BrineSal[i]=b[i];
	}


	// Track boundary fluxes (time step t + Dt) in units [g/m^2].
	BottomSalFlux +=        -f * (flux_down[0] * dt * ((flux_down[0] > 0.) ? (BrineSal[0] * (Constants::density_water + SeaIce::betaS * BrineSal[0])) : (BottomSalinity * (Constants::density_water + SeaIce::betaS * BottomSalinity))));
	TopSalFlux    +=         f * (flux_up[NumberOfElements-1] * dt * ((flux_up[NumberOfElements-1] > 0.) ? (TopSalinity * (Constants::density_water + SeaIce::betaS * TopSalinity)) : (BrineSal[NumberOfElements-1] * (Constants::density_water + SeaIce::betaS * BrineSal[NumberOfElements-1]))));


	return true;
}


/**
 * @brief Solve diffusion-advection equation using the upwind explicit method\n
 * @author Nander Wever
 * @param dt Time step (s)
 * @param DeltaSal Result vector (change in salinity over time step)
 * @return false on error, true otherwise
 */
bool SalinityTransport::SolveSalinityTransportEquationExplicit(const double dt, std::vector <double> &DeltaSal) {

	if(NumberOfElements==0) return false;	// Nothing to do

	const bool WriteDebugOutput = false;
	if(WriteDebugOutput) setvbuf(stdout, NULL, _IONBF, 0);

	// Declare vectors
	std::vector<double> b(NumberOfElements, 0.);		// Solution vector

	if(ZeroFluxUpperBoundary_advection) flux_up[NumberOfElements-1] = 0.;
	if(ZeroFluxLowerBoundary_advection) flux_down[0] = 0.;

	// Fill matrix and r.h.s. vector
	for(size_t i = 0; i < NumberOfElements; i++) {
		b[i] += (theta1[i] * BrineSal[i]);

		// Explicit upwind scheme for advection:
		const double tmp_flux = (flux_up[i] * dz_up[i] + flux_down[i] * dz_down[i]) / (dz_up[i] + dz_down[i]);
		const double tmp_flux_2 = (flux_up_2[i] * dz_up[i] + flux_down_2[i] * dz_down[i]) / (dz_up[i] + dz_down[i]);

		// First advection term
//		b[i] += BrineSal[i] * (flux_up[i] * dt - flux_down[i] * dt) / dz_[i] +
//			tmp_flux * dt * (  (tmp_flux > 0.)   ? ((((i==NumberOfElements-1) ? (TopSalinity) : (BrineSal[i+1])) - BrineSal[i]) / dz_up[i])            : (((BrineSal[i] - ((i==0) ? (BottomSalinity) : (BrineSal[i-1]))) / dz_down[i]))  );
		b[i] += BrineSal[i] * (flux_up[i] * dt - flux_down[i] * dt) / dz_[i] +
			flux_up[i]   * dt * ( (flux_up[i]>0.) ? ((i==NumberOfElements-1) ? (TopSalinity) : (BrineSal[i+1])) : (- BrineSal[i]) ) /*/ dz_up[i]*/ +
			flux_down[i] * dt * ( (flux_down[i]>0.) ? (-BrineSal[i]) : ((i==0) ? (-BottomSalinity) : (-BrineSal[i-1])) ) /*/ dz_down[i]*/;

		// Second advection term
		/*b[i] += BrineSal[i] * (flux_up_2[i] * dt - flux_down_2[i] * dt) / dz_[i] +
			tmp_flux_2 * dt * (  (tmp_flux_2 > 0.)   ? ((((i==NumberOfElements-1) ? (TopSalinity) : (BrineSal[i+1])) - BrineSal[i]) / dz_up[i])            : (((BrineSal[i] - ((i==0) ? (BottomSalinity) : (BrineSal[i-1]))) / dz_down[i]))  );*/

		// Explicit scheme for diffusion, note that we force a zero flux condition at the top boundary (mirroring the cell i-1)
		//b[i] += dt * ( ((i==NumberOfElements-1) ? (2. * D[i-1] * theta1[i-1] * BrineSal[i-1]) : (2. * theta1[i+1] * D[i+1] * BrineSal[i+1])) / (dz_up[i]*(dz_up[i]+dz_down[i])) - (2. * theta1[i] * D[i] * BrineSal[i]) / (dz_up[i]*dz_down[i]) + (((i==0) ? (2. * D[i] * theta1[i] * BottomSalinity) : (2. * theta1[i-1] * D[i-1] * BrineSal[i-1]))) / (dz_down[i]*(dz_up[i]+dz_down[i])) );
		if (i == 0) {
			b[i] += dt * (2. * (theta1[i+1] * D[i+1] * BrineSal[i+1] - theta1[i] * D[i] * BrineSal[i])) / (dz_up[i]*(dz_up[i]+dz_down[i]));
		} else if (i == NumberOfElements-1) {
			//b[i] += dt * (2. * (theta1[i-1] * D[i-1] * BrineSal[i-1] - theta1[i] * D[i] * BrineSal[i])) / (dz_down[i]*(dz_up[i]+dz_down[i]));
		} else {
			b[i] += dt * ((2. * theta1[i+1] * D[i+1] * BrineSal[i+1]) / (dz_up[i]*(dz_up[i]+dz_down[i])) - ((2. * theta1[i] * D[i] * BrineSal[i]) / (dz_up[i]*dz_down[i])) + ((2. * theta1[i-1] * D[i-1] * BrineSal[i-1]) / (dz_down[i]*(dz_up[i]+dz_down[i]))) );
		}
		

		// Source/sink term
		b[i] += -sb[i];

		b[i] /= theta2[i];

		// Track boundary fluxes
		if(i==0) {
			// Lower boundary advection
			BottomSalFlux += -BrineSal[i] * flux_down[i] * dt - BrineSal[i] * flux_down_2[i] * dt +
				flux_down[i]   * dt * (  /*(tmp_flux   > 0.)   ?  (0.)  :*/  ((BrineSal[i] - BottomSalinity) / dz_down[i])  ) +
				flux_down_2[i] * dt * (  /*(tmp_flux_2 > 0.)   ?  (0.)  : */ ((BrineSal[i] - BottomSalinity) / dz_down[i])  );
			// Lower boundary diffusion
			BottomSalFlux += dt * (-D[i] * theta1[i] * BrineSal[i] + D[i] * theta1[i] * BottomSalinity) / (dz_down[i]);

		}
		if(i==NumberOfElements-1) {
			// Upper boundary advection
			TopSalFlux    =  BrineSal[i] * flux_up[i] * dt - BrineSal[i] * flux_up_2[i] * dt +
				tmp_flux   * dt * (  (tmp_flux   > 0.)   ?  ((TopSalinity - BrineSal[i]) / dz_down[i])  :  (0.)  ) +
				tmp_flux_2 * dt * (  (tmp_flux_2 > 0.)   ?  ((TopSalinity - BrineSal[i]) / dz_down[i])  :  (0.)  );
			// Upper boundary diffusion
			TopSalFlux += dt * (-D[i] * theta1[i] * BrineSal[i] + D[i] * theta1[i] * TopSalinity) / (dz_up[i]);
		}
	}

	// Apply solution
	for(size_t i=0; i<NumberOfElements; i++) {
		DeltaSal[i]=b[i] - BrineSal[i];
		BrineSal[i]=b[i];
	}

	return true;
}


/**
 * @brief Check for CFL criterion\n
 * @author Nander Wever
 * @param dt Time step (s)
 * @return true when provided time step dt satisfies CFL criterion, false otherwise.
 */
bool SalinityTransport::VerifyCFL(const double dt)
{
	const double CFL_limit = 0.499;
	for(size_t i = 0; i < NumberOfElements; i++) {
		// Check CFL for advection
//		const double tmp_flux   = (flux_up[i]   * dz_up[i] + flux_down[i]   * dz_down[i]) / (dz_up[i] + dz_down[i]);
//		const double tmp_flux_2 = (flux_up_2[i] * dz_up[i] + flux_down_2[i] * dz_down[i]) / (dz_up[i] + dz_down[i]);
//		if (tmp_flux * dt / std::min(dz_up[i], dz_down[i]) > CFL_limit) {printf("FAIL1@%d ", int(i)); return false;}
//		if (tmp_flux_2 * dt / std::min(dz_up[i], dz_down[i]) > CFL_limit) {printf("FAIL2@%d ", int(i)); return false;}
		if (std::max( fabs(flux_up[i]) , fabs(flux_down[i]) ) * (dt / (std::min(dz_up[i], dz_down[i]))) > CFL_limit) {printf("FAIL4A@%d ", int(i)); return false;}
		if (std::max( fabs(flux_up_2[i]) , fabs(flux_down_2[i]) ) * (dt / (std::min(dz_up[i], dz_down[i]))) > CFL_limit) {printf("FAIL4B@%d ", int(i)); return false;}
		// Check CFL for diffusion
		if (D[i] * theta1[i] * dt / (std::min(dz_up[i], dz_down[i]) * std::min(dz_up[i], dz_down[i])) > CFL_limit) {printf("FAIL3@%d ", int(i)); return false;}
	}
	return true;
}


/**
 * @brief Check for Implicit criterion\n
 * @author Nander Wever
 * @param dt Time step (s)
 * @return true when provided time step dt satisfies criterion to reduce spurious oscillations, false otherwise.
 */
bool SalinityTransport::VerifyImplicitDt(const double dt)
{
	const double limit = 0.999;
	for(size_t i = 0; i < NumberOfElements; i++) {
		// Check advection
		if (std::max( fabs(flux_up[i])/dz_up[i] , fabs(flux_down[i]/dz_down[i]) ) * dt > limit) {printf("FAIL4A@%d ", int(i)); return false;}
		if (std::max( fabs(flux_up_2[i])/dz_up[i] , fabs(flux_down_2[i]/dz_down[i]) ) * dt > limit) {printf("FAIL4B@%d ", int(i)); return false;}
		// Check diffusion
		if (i!=0 && D[i] * theta1[i] * dt / (std::min(dz_up[i], dz_down[i]) * std::min(dz_up[i], dz_down[i])) > limit) {printf("FAIL5@%d ", int(i)); return false;}
		if (i==0 && D[i] * dt / (std::min(dz_up[i], dz_down[i]) * std::min(dz_up[i], dz_down[i])) > limit) {printf("FAIL6@%d ", int(i)); return false;}
	}
	return true;
}
