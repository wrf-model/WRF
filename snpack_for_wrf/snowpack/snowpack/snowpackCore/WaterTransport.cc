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

#include <snowpack/snowpackCore/WaterTransport.h>
#include <snowpack/snowpackCore/Snowpack.h>
#include <snowpack/snowpackCore/ReSolver1d.h>
#include <snowpack/snowpackCore/PhaseChange.h>
#include <snowpack/Constants.h>
#include <snowpack/Utils.h>

#include <assert.h>

using namespace std;
using namespace mio;

WaterTransport::WaterTransport(const SnowpackConfig& cfg)
               : RichardsEquationSolver1d_matrix(cfg, true), RichardsEquationSolver1d_pref(cfg, false), variant(),
                 iwatertransportmodel_snow(BUCKET), iwatertransportmodel_soil(BUCKET), watertransportmodel_snow("BUCKET"), watertransportmodel_soil("BUCKET"), enable_pref_flow(false), pref_flow_rain_input_domain("MATRIX"),
                 sn_dt(IOUtils::nodata),
                 hoar_thresh_rh(IOUtils::nodata), hoar_thresh_vw(IOUtils::nodata), hoar_thresh_ta(IOUtils::nodata),
                 hoar_density_buried(IOUtils::nodata), hoar_density_surf(IOUtils::nodata), hoar_min_size_buried(IOUtils::nodata),
                 minimum_l_element(IOUtils::nodata), comb_thresh_l(IOUtils::nodata), useSoilLayers(false), water_layer(false), jam(false)
{
	cfg.getValue("VARIANT", "SnowpackAdvanced", variant);

	// Defines whether soil layers are used
	cfg.getValue("SNP_SOIL", "Snowpack", useSoilLayers);

	//To build a thin top rain-water layer over a thin top ice layer, rocks, roads etc.
	cfg.getValue("WATER_LAYER", "SnowpackAdvanced", water_layer);

	/**
	 * @brief No surface hoar will form for rH above threshold (1)
	 * - Original calibration with the 98/99 data set: 0.9
	 * - r141: HOAR_THRESH_RH set to 0.9
	 * - r719: HOAR_THRESH_RH set to 0.97
	 */
	cfg.getValue("HOAR_THRESH_RH", "SnowpackAdvanced", hoar_thresh_rh);

	/**
	 * @brief No surface hoar will form at wind speeds above threshold (m s-1)
	 * - Original calibration with the 98/99 data set: 3.5
	 * - r141: HOAR_THRESH_VW set to 3.0
	 * - r242: HOAR_THRESH_VW set to 3.5
	 */
	cfg.getValue("HOAR_THRESH_VW", "SnowpackAdvanced", hoar_thresh_vw);
	
	/**
	 * @brief No surface hoar will form at air temperatures above threshold (m s-1)
	 * - Originaly, using THRESH_RAIN
	 * - r787: HOAR_THRESH_TA set to 1.2
	 */
	cfg.getValue("HOAR_THRESH_TA", "SnowpackAdvanced", hoar_thresh_ta);

	//Calculation time step in seconds as derived from CALCULATION_STEP_LENGTH
	const double calculation_step_length = cfg.get("CALCULATION_STEP_LENGTH", "Snowpack");
	sn_dt = M_TO_S(calculation_step_length);

	//To build up a water table over impermeable layers
	cfg.getValue("JAM", "SnowpackAdvanced", jam);

	// Density of BURIED surface hoar (kg m-3), default: 125./ Antarctica: 200.
	cfg.getValue("HOAR_DENSITY_BURIED", "SnowpackAdvanced", hoar_density_buried);

	//Minimum surface hoar size to be buried (mm). Increased by 50% for Dirichlet bc.
	cfg.getValue("HOAR_MIN_SIZE_BURIED", "SnowpackAdvanced", hoar_min_size_buried);

	//Density of surface hoar (-> hoar index of surface node) (kg m-3)
	cfg.getValue("HOAR_DENSITY_SURF", "SnowpackAdvanced", hoar_density_surf);

	//Minimum element length (m)
	cfg.getValue("MINIMUM_L_ELEMENT", "SnowpackAdvanced", minimum_l_element);

	double dummy_height_new_elem;	//only temporarily needed
	cfg.getValue("HEIGHT_NEW_ELEM", "SnowpackAdvanced", dummy_height_new_elem);
	cfg.getValue("COMB_THRESH_L", "SnowpackAdvanced", comb_thresh_l, IOUtils::nothrow);
	if(comb_thresh_l == IOUtils::nodata) comb_thresh_l = SnowStation::comb_thresh_l_ratio * dummy_height_new_elem;	// If no comb_thres_l specified, use the default one (i.e., a fixed ratio from height_new_elem)

	//Water transport model snow
	cfg.getValue("WATERTRANSPORTMODEL_SNOW", "SnowpackAdvanced", watertransportmodel_snow);
	iwatertransportmodel_snow=UNDEFINED;
	enable_pref_flow=false;
	if (watertransportmodel_snow=="BUCKET") {
		iwatertransportmodel_snow=BUCKET;
	} else if (watertransportmodel_snow=="NIED") {
		iwatertransportmodel_snow=NIED;
	} else if (watertransportmodel_snow=="RICHARDSEQUATION") {
		iwatertransportmodel_snow=RICHARDSEQUATION;
	}
	cfg.getValue("PREF_FLOW", "SnowpackAdvanced", enable_pref_flow);
	if (enable_pref_flow && watertransportmodel_snow!="RICHARDSEQUATION") {
		prn_msg( __FILE__, __LINE__, "err", Date(), "PREF_FLOW = TRUE requires WATERTRANSPORTMODEL_SNOW = RICHARDSEQUATION. Preferential flow is only implemented as an extension of Richards equation.");
		throw;
	}
	if(enable_pref_flow) {
		cfg.getValue("PREF_FLOW_RAIN_INPUT_DOMAIN", "SnowpackAdvanced", pref_flow_rain_input_domain);
		if(pref_flow_rain_input_domain != "MATRIX" && pref_flow_rain_input_domain != "PREF_FLOW") {
			prn_msg( __FILE__, __LINE__, "err", Date(), "PREF_FLOW_RAIN_INPUT_DOMAIN is expected to be MATRIX or PREF_FLOW (mind the upper case!).");
		}
	} else {
		// Enforce the rain water into the matrix domain, in case PREF_FLOW model is not enabled.
		pref_flow_rain_input_domain="MATRIX";
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
}


/**
 * @brief Function needed by WaterTransport model "NIED" \n
 * The following function is used in the WaterTransport model "NIED", developed by Hiroyuki Hirashima, Snow and Ice Research Center, NIED \n
 * under support of the NIED project 'A research project for developing a snow disaster forecasting system and snow-hazard maps'. \n
 * For details, see: Hirashima et al. (2010) Numerical modeling of liquid water movement through layered snow based on new measurements of the water retention curve. Cold Regions Science and Technology, 64(2), 94-103. \n
 * @author Hiroyuki Hirashima
 * @param X Variable
 * @param P Variable
 */
double WaterTransport::BisFunc(const double X, const double P[])
{
	double Rh0,Rh1,Rk0,Rk1;

	KHCalcNaga (P[1], P[2], P[3], P[4] - X / (P[11]+Constants::eps) , 1., Rh0, Rk0);
	KHCalcNaga (P[6], P[7], P[8], P[9] + X / (P[12]+Constants::eps) , 1., Rh1, Rk1);
	return (Rh1 - Rh0 + (P[11]+P[12])/2.);
}


/**
 * @brief Function needed by WaterTransport model "NIED" \n
 * The following function is used in the WaterTransport model "NIED", developed by Hiroyuki Hirashima, Snow and Ice Research Center, NIED \n
 * under support of the NIED project 'A research project for developing a snow disaster forecasting system and snow-hazard maps'. \n
 * For details, see: Hirashima et al. (2010) Numerical modeling of liquid water movement through layered snow based on new measurements of the water retention curve. Cold Regions Science and Technology, 64(2), 94-103. \n
 * @author Hiroyuki Hirashima
 * @param minval Variable
 * @param maxval Variable
 * @param P Variable
 */
double WaterTransport::Bisection(const double minval, const double maxval, double P[])
{
	double XCen,YCen;
	double X1 = minval, X2 = maxval;
	double Y1 = BisFunc(X1, P), Y2 = BisFunc(X2, P);
	if (fabs(Y1) < Constants::eps) {
		return (X1);
	}
	if (fabs(Y2) < Constants::eps) {
		return (X2);
	}
	if ((Y1 * Y2) > 0.) {
		return (0.);
	}
	while (fabs(X2 - X1) > Constants::eps) {
		XCen = (X2 + X1) / 2.;
		YCen = BisFunc(XCen, P);
		if (YCen < Constants::eps) {
			return (XCen);
		}
		if (Y1 * YCen > 0.) {
			X1 = XCen;
		} else {
			X2 = XCen;
		}
	}
	XCen = (X1 + X2) / 2.;
	return (XCen);
}


/**
 * @brief Function needed by WaterTransport model "NIED" \n
 * The following function is used in the WaterTransport model "NIED", developed by Hiroyuki Hirashima, Snow and Ice Research Center, NIED \n
 * under support of the NIED project 'A research project for developing a snow disaster forecasting system and snow-hazard maps'. \n
 * For details, see: Hirashima et al. (2010) Numerical modeling of liquid water movement through layered snow based on new measurements of the water retention curve. Cold Regions Science and Technology, 64(2), 94-103. \n
 * @author Hiroyuki Hirashima
 * @param RG Grain radius
 * @param Dens Density
 * @param ThR Residual water content
 * @param WatCnt Water content
 * @param SatuK Saturated hydraulic conductivity
 * @param Rh Pressure head
 * @param Rk Hydraulic conductivity
 */
void WaterTransport::KHCalcNaga(const double RG, const double Dens, double ThR, const double WatCnt, const double SatuK, double &Rh, double &Rk)
{
	// This is a very ill-confined piece of code!
	if ( fabs(ThR) < Constants::eps2 ) {
		ThR += Constants::eps;
	}
	const double PA = 6.97 * (RG) + 1.985;
	const double PN = 7.593 * (1.0 / (RG * RG)) + 5.03;
	const double PM = 1.0 - (1.0 / PN);
	const double ThS = (1000. - (Dens / 0.917)) / 10.0 * 0.9 / 100.0;
	const double LTh = (WatCnt-ThR) / (ThS - ThR);

	if (WatCnt <= ThR * 1.01) {
		double SEffSub = ((ThR * 1.01) - ThR) / (ThS - ThR);
		const double hM =  pow(pow(SEffSub,(-1.0 / PM)) - 1., 1. / PN) / PA;
		SEffSub = ((ThR * 1.011) - ThR) / (ThS - ThR);
		const double hL =  pow(pow(SEffSub,(-1.0 / PM)) - 1., 1. / PN) / PA;
		SEffSub = ((ThR * 1.009) - ThR) / (ThS - ThR);
		const double hN =  pow(pow(SEffSub,(-1.0 / PM)) - 1., 1. / PN) / PA;
		const double hSlo = (hL - hN) / (ThR * 0.002);
		Rh = hM + (WatCnt - (ThR*1.01)) * hSlo;
	} else {
		if (LTh > 1.) {
			Rh = 0.;
		} else {
			Rh = pow(pow(LTh,(-1. / PM)) - 1., 1. / PN) / PA;
		}
	}
	if (LTh < 0.) {
		Rk = 0.;
	} else {
		if (LTh > 1.) {
			Rk = SatuK;
		} else {
			Rk = SatuK * sqrt(LTh) * Optim::pow2( 1.-pow(1.-pow(LTh,(1./PM)),PM) );
		}
	}
	//Fz 2010-05-02
	/*const double avoid_neg=Constants::eps; // To avoid base x <= 0. for pow(x,1/y) function!
	 if (WatCnt <= ThR * 1.01) {
		double SEffSub = std::max( ((ThR * 1.01) - ThR) / (ThS - ThR) , avoid_neg);
		const double hM =  pow(std::max(pow(SEffSub,(-1.0 / PM)) - 1., avoid_neg), 1. / PN) / PA;
		SEffSub = std::max( ((ThR * 1.011) - ThR) / (ThS - ThR) , avoid_neg);
		const double hL =  pow(std::max(pow(SEffSub,(-1.0 / PM)) - 1., avoid_neg), 1. / PN) / PA;
		SEffSub = std::max( ((ThR * 1.009) - ThR) / (ThS - ThR) , Constants::eps);
		const double hN =  pow(std::max(pow(SEffSub,(-1.0 / PM)) - 1., avoid_neg), 1. / PN) / PA;
		const double hSlo = (hL - hN) / (ThR * 0.002);
		Rh = hM + (WatCnt - (ThR*1.01)) * hSlo;
	}
	if (LTh < 0.) {
		Rk = 0.;
		Rh = 0.; //What else?
	} else if (LTh > 1.) {
		Rh = 0.;
		Rk = SatuK;
	} else {
		Rh = pow(std::max(pow(LTh,(-1. / PM)) - 1., avoid_neg), 1. / PN) / PA;
		Rk = SatuK * sqrt(LTh) * Optim::pow2( 1.-pow(1.-pow(LTh,(1./PM)),PM) );
	}*/
}

/**
 * @brief This function deals with the top flux for the bucket water transport scheme.\n
 * Determines the fraction of the latent heat flux ql that can be used for evaporation or
 * condensation. IMPORTANT: sublimation/deposition is treated by VapourTransport.
 * The variable ql is updated with the amount used for evaporation/condensation, such that
 * VapourTransport should interpret all remaining energy as sublimation/deposition and
 * additionally take care of surface hoar formation/destruction.
 * @param ql Latent heat flux (W m-2)
 * @param *Xdata
 * @param *Sdata
 */
void WaterTransport::compTopFlux(double& ql, SnowStation& Xdata, SurfaceFluxes& Sdata)
{
	double dM = 0.;              // Mass changes
	double M = 0.;               // Initial mass and volumetric content (water or ice)

	const size_t nN = Xdata.getNumberOfNodes();
	const size_t nE = nN-1;
	vector<NodeData>& NDS = Xdata.Ndata;
	vector<ElementData>& EMS = Xdata.Edata;
	const double Tss = NDS[nE].T; // Surface Temperature


	/*
	 * If there are elements and ql > 0:
	 * update densities and volumetric contents (ELEMENT data),
	 * add/subtract mass to MS_EVAPORATION,
	 * potential surface hoar formation/destruction is tested in VapourTransport.
	 */
	if (ql > Constants::eps2) { // Add Mass
		const double meltfreeze_tk = (Xdata.getNumberOfElements()>0)? Xdata.Edata[Xdata.getNumberOfElements()-1].meltfreeze_tk : Constants::meltfreeze_tk;
		if (!(Tss < meltfreeze_tk)) {
			// Add water
			if ((iwatertransportmodel_snow != RICHARDSEQUATION && nE>Xdata.SoilNode) || (iwatertransportmodel_soil != RICHARDSEQUATION && nE==Xdata.SoilNode)) {	//NANDER: check if the upper element is not part of the domain solved by the Richards Equation, because if so, we should put it in the surface flux
				// Add Water
				const double theta_w0 = EMS[nE-1].theta[WATER];
				dM = ql*sn_dt/Constants::lh_vaporization;
				ql = 0.;
				Sdata.mass[SurfaceFluxes::MS_EVAPORATION] += dM;
				if (nE == Xdata.SoilNode) {
					dM = std::min(dM,EMS[nE-1].theta[AIR]*(Constants::density_water*EMS[nE-1].L));
				}
				EMS[nE-1].theta[WATER] += dM/(Constants::density_water*EMS[nE-1].L);

				for (size_t ii = 0; ii < Xdata.number_of_solutes; ii++) {
					EMS[nE-1].conc[WATER][ii] *= theta_w0/EMS[nE-1].theta[WATER];
				}
			}
		}
		EMS[nE-1].M += dM;
		assert(EMS[nE-1].M >= (-Constants::eps2)); //mass must be positive

		// Update remaining volumetric contents and density
		EMS[nE-1].theta[AIR] = std::max(0., 1.0 - EMS[nE-1].theta[WATER] - EMS[nE-1].theta[WATER_PREF] - EMS[nE-1].theta[ICE] - EMS[nE-1].theta[SOIL]);
		EMS[nE-1].updDensity();
	} else if ((ql < (-Constants::eps2)) && (nE > 0)) {
		// If  there is water in some form and ql < 0, EVAPORATE some mass off
		std::vector<double> M_Solutes(Xdata.number_of_solutes, 0.); // Mass of solutes from disappearing phases
		size_t e = nE;
		double ql2 = ql; // Dummy of ql. We want to mimick the effect of evaporation from deeper layers, if the energy flux is so large, that complete elements disappear.
				 // But, since we now have separate locations for water and ice evaporation respectively sublimation, we need to calculate already here the 
				 // sublimation of ice to decide whether any water is evaporated from the next element below. So, ql2 also keeps track of sublimation, which is not
				 // applied here, but later in VapourTransport.
		while ((e > 0) && (ql2 < (-Constants::eps2))) {  // While energy is available
			e--;
			if ((iwatertransportmodel_snow != RICHARDSEQUATION && e>=Xdata.SoilNode) || (iwatertransportmodel_soil != RICHARDSEQUATION && e<Xdata.SoilNode)) {
				/*
				* Determine the amount of potential sublimation/evaporation and collect some variables
				* that will be continuously used: L0 and M
				*  - NOTE: if water is present, evaporate
				*/
				const double L0 = EMS[e].L;
				// If there is water ...
				if ((EMS[e].theta[WATER]+EMS[e].theta[WATER_PREF]) > ((e==nE-1)?(2.*Constants::eps):0.)) {
					//For the top layer, it is important to keep a tiny amount of liquid water, so we are able to detect whether we need the
					//implicit or explicit treatment of the top boundary condition when solving the heat equation.
					const double theta_w0 = (EMS[e].theta[WATER]+EMS[e].theta[WATER_PREF]) - ( (e==nE-1) ? (2.*Constants::eps) : 0. );
					dM = ql*sn_dt/Constants::lh_vaporization;
					M = theta_w0*Constants::density_water*L0;
					// Check that you only take the available mass of water
					if (-dM >= M) {
						dM = -M;
						// First empty preferential flow
						const double dM_pref = std::max(-EMS[e].theta[WATER_PREF], dM/(Constants::density_water*L0));
						EMS[e].theta[WATER_PREF] += dM_pref;
						// Then matrix flow
						EMS[e].theta[WATER] += (dM - dM_pref)/(Constants::density_water*L0);
						// Add solutes to Storage
						for (size_t ii = 0; ii < Xdata.number_of_solutes; ii++) {
							M_Solutes[ii] += EMS[e].conc[WATER][ii]*theta_w0*L0;
						}
					} else {
						// First empty preferential flow
						const double dM_pref = std::max(-EMS[e].theta[WATER_PREF], dM/(Constants::density_water*L0));
						EMS[e].theta[WATER_PREF] += dM_pref;
						// Then matrix flow
						EMS[e].theta[WATER] += (dM - dM_pref)/(Constants::density_water*L0);
						for (size_t ii = 0; ii < Xdata.number_of_solutes; ii++) {
							EMS[e].conc[WATER][ii] *= theta_w0/EMS[e].theta[WATER];
						}
					}
					EMS[e].M += dM;
					assert(EMS[e].M >= (-Constants::eps2)); //mass must be positive
					Sdata.mass[SurfaceFluxes::MS_EVAPORATION] += dM;
					ql -= dM*Constants::lh_vaporization/sn_dt; // Update the energy used
					ql2 -= dM*Constants::lh_vaporization/sn_dt; // Update the energy used
				}
				if (ql2 < (-Constants::eps2)) {
					// If there is no water or if there was not enough water ...
					const double theta_i0 = EMS[e].theta[ICE];
					M = theta_i0*Constants::density_ice*L0;
					dM = ql2*sn_dt/Constants::lh_sublimation;
					if (-dM > M) {
						dM = -M;
					}
					ql2 -= dM*Constants::lh_sublimation/sn_dt;     //Anticipated update of the energy that will be used for sublimation
				}
				// Update remaining volumetric contents and density
				EMS[e].theta[AIR] = std::max(0., 1.0 - EMS[e].theta[WATER] - EMS[e].theta[WATER_PREF] - EMS[e].theta[ICE] - EMS[e].theta[SOIL]);
				EMS[e].updDensity();
			}

			//check that thetas and densities are consistent
			assert(EMS[e].theta[SOIL] >= (-Constants::eps2) && EMS[e].theta[SOIL] <= (1.+Constants::eps2));
			assert(EMS[e].theta[ICE] >= (-Constants::eps2) && EMS[e].theta[ICE]<=(1.+Constants::eps2));
			assert(EMS[e].theta[WATER] >= (-Constants::eps2) && EMS[e].theta[WATER]<=(1.+Constants::eps2));
			assert(EMS[e].theta[WATER_PREF] >= (-Constants::eps2) && EMS[e].theta[WATER_PREF]<=(1.+Constants::eps2));
			assert(EMS[e].theta[AIR] >= (-Constants::eps2) && EMS[e].theta[AIR]<=(1.+Constants::eps2));
			assert(EMS[e].Rho >= (-Constants::eps2) || EMS[e].Rho==IOUtils::nodata); //we want positive density
		}

		// Now take care of left over solute mass.
		if (e == 0) { // Add Solute Mass to Runoff TODO HACK CHECK
			for (size_t ii = 0; ii < Xdata.number_of_solutes; ii++) {
				Sdata.load[ii] += M_Solutes[ii]/S_TO_H(sn_dt);
			}
		} else { // Add Solute Mass to Element below
			if (EMS[e].theta[WATER] > 0.) {
				for(size_t ii = 0; ii < Xdata.number_of_solutes; ii++) {
					EMS[e].conc[WATER][ii] += M_Solutes[ii]/EMS[e].theta[WATER]/EMS[e].L;
				}
			} else if (EMS[e].theta[ICE] > 0.) {
				for (size_t ii = 0; ii < Xdata.number_of_solutes; ii++) {
					EMS[e].conc[ICE][ii] += M_Solutes[ii]/EMS[e].theta[ICE]/EMS[e].L;
				}
			} else {
				for (size_t ii = 0; ii < Xdata.number_of_solutes; ii++) {
					EMS[e].conc[SOIL][ii] += M_Solutes[ii]/EMS[e].theta[SOIL]/EMS[e].L;
				}
			}
		}
	}
}

/**
 * @brief Merging snow elements \n
 * - Starting from the surface, merge as many snow elements as you can; \n
 * 	  Merge too thin elements with their lower neighbour; keep buried SH and tagged layers longer on \n
 * 	  but enforce merging for uncovered but previously buried SH \n
 * 	  => Use SnowStation::mergeElements() to compute the properties of the lower element
 * - NOTE
 * 	- Only the top or last element on the ground will be effectively removed if needed
 * 	- Water will be transported AFTER elements have been merged.
 * 	- If WATER_LAYER is set with soil, make sure that you keep a potential wet water layer over soil or ice
 * 	- Reset ground surface temperature if no snow is left and there is no soil
 * @param *Xdata
 * @param *Sdata
 */
void WaterTransport::mergingElements(SnowStation& Xdata, SurfaceFluxes& Sdata)
{
	const size_t nN = Xdata.getNumberOfNodes(), nE = nN-1;
	size_t rnN = nN, rnE = nN-1;
	vector<ElementData>& EMS = Xdata.Edata;

	if ((nN == Xdata.SoilNode+1)
	        || (water_layer && useSoilLayers && (nN == Xdata.SoilNode+2)
	                && (EMS[nE-1].theta[ICE] < Snowpack::min_ice_content)
	                    && (EMS[nE-1].theta[WATER] > SnowStation::thresh_moist_snow) && (EMS[nE-1].L > 0.0001))) {
		return;
	}

	bool verify_top_element = false;
	double removedMass = 0.;
	size_t eUpper = nE; // Index of the upper element, the properties of which will be transferred to the lower adjacent one
	while (eUpper-- > Xdata.SoilNode) {
		bool enforce_merge = true;	// To enforce merging in special cases
		if ((EMS[eUpper].L < minimum_l_element) || (EMS[eUpper].mk%100 == 3)) {
			if ((EMS[eUpper].mk >= 100 && int(EMS[eUpper].mk/1000)!=9) && (EMS[eUpper].L >= 0.5 * minimum_l_element)) {
				enforce_merge = false;
			}
			if (EMS[eUpper].mk%100 == 3) {
				if ((eUpper < nE-1) && (EMS[eUpper].L >= MM_TO_M(0.75*hoar_min_size_buried
				                                     * (hoar_density_surf/hoar_density_buried)))
				                    && (EMS[eUpper].Rho <= 300.)) {
					enforce_merge = false;
				} else {
					enforce_merge = true;
				}
			}
		} else {
			enforce_merge = false;
		}
		const double theta_r=((iwatertransportmodel_snow==RICHARDSEQUATION && eUpper>=Xdata.SoilNode) || (iwatertransportmodel_soil==RICHARDSEQUATION && eUpper<Xdata.SoilNode)) ? (PhaseChange::RE_theta_r) : (PhaseChange::theta_r);
		const bool do_merge = (EMS[eUpper].theta[ICE] < Snowpack::min_ice_content) || enforce_merge;
		const bool is_snow_layer = (EMS[eUpper].theta[SOIL] < Constants::eps2) && (EMS[eUpper].mk % 100 != 9); //exclude plastic or water_layer
		const bool wet_layer_exception = (eUpper > 0 && eUpper == nE-1 && EMS[eUpper].theta[ICE] > 0.2 * Snowpack::min_ice_content && EMS[eUpper].L > 0.2 * minimum_l_element && EMS[eUpper-1].theta[SOIL] < Constants::eps && EMS[eUpper].theta[ICE] > Constants::eps && EMS[eUpper].theta[WATER] < theta_r + Constants::eps && EMS[eUpper-1].theta[WATER] > theta_r + Constants::eps); // Don't merge a dry surface snow layer with a wet one below, as the surface node may then experience a sudden increase in temperature, destroying energy balance.
		
		if (do_merge && is_snow_layer && !wet_layer_exception) {
			bool UpperJoin=false;			// Default is joining with elements below
			bool merged = true;			// true: element is finally merged, false: element is finally removed.
			if (eUpper > Xdata.SoilNode) { 		// If we have snow elements below to merge with
				// We always merge snow elements
				merged=true;
				if ( (eUpper == rnE-1) && (EMS[eUpper].theta[ICE] < Snowpack::min_ice_content) ) {
					// In this case, we would prefer to keep the eUpper-1 element density constant, which is done in SnowStation::mergeElements(...)
					// In case we solve snow with Richards equation AND we remove the top element, we apply the water in the top layer as a Neumann boundary flux in the RE
					if (iwatertransportmodel_snow == RICHARDSEQUATION) {
						RichardsEquationSolver1d_matrix.surfacefluxrate+=((EMS[eUpper].theta[WATER]+EMS[eUpper].theta[WATER_PREF])*EMS[eUpper].L)/(sn_dt);
						// We remove water from the element, which is now in surfacefluxrate
						EMS[eUpper].theta[AIR]+=(EMS[eUpper].theta[WATER]+EMS[eUpper].theta[WATER_PREF]);
						removedMass += (EMS[eUpper].theta[WATER] + EMS[eUpper].theta[WATER_PREF]) * Constants::density_water * EMS[eUpper].L;
						EMS[eUpper].theta[WATER]=0.;
						EMS[eUpper].theta[WATER_PREF]=0.;
						// Adjust density and mass accordingly
						EMS[eUpper].updDensity();
						EMS[eUpper].M = EMS[eUpper].Rho*EMS[eUpper].L;
					}
				}

				// We never merge snow elements with elements containing soil inside the snowpack (e.g., for snow farming)
				if (EMS[eUpper-1].theta[SOIL]>Constants::eps) {
					merged=false;
				}

				// After dealing with all possibilities, now finally do the merge:
				if(!merged) removedMass += EMS[eUpper].M;
				SnowStation::mergeElements(EMS[eUpper-1], EMS[eUpper], merged, (eUpper==rnE-1));

				// The upper element may grow too much in length by subsequent element merging, limit this! Note that this has the desired effect of averaging the two top elements.
				if(eUpper==rnE-1 && merged==true) {
					verify_top_element=true;
				}
			} else {										// We are dealing with first snow element above soil
				if (rnE-1 > Xdata.SoilNode && EMS[eUpper+1].L > 0. && EMS[eUpper+1].Rho > 0.) { // If at least one snow layer above AND this layer above is not marked to be removed yet.
					// In case it is the lowest snow element and there are snow elements above, join with the element above:
					merged=true;
					SnowStation::mergeElements(EMS[eUpper], EMS[eUpper+1], true, (eUpper==nE-1));
					UpperJoin=true;
				} else {									// Else we remove element
					merged=false;

					// First, make sure there is no ice anymore, as we do not want to transfer ice over soil-snow interface:
					EMS[eUpper].theta[WATER] += EMS[eUpper].theta[ICE] * (Constants::density_ice/Constants::density_water);
					EMS[eUpper].theta[ICE] = 0.;

					// Take care of energy used for melting the ice:
					const double ql = (EMS[eUpper].theta[ICE] * EMS[eUpper].L * Constants::density_ice * Constants::lh_fusion);	// J/m^2

					// ql is energy crossing the soil-snow interface and should be considered part of the soil-snow heat flux:
					Sdata.qg0 += ql/sn_dt;

					if (Xdata.SoilNode > 0) {							// Case of soil present
						// Adjust upper soil element for the energy extracted to melt the ice:
						EMS[eUpper-1].Te -= ql / (EMS[eUpper-1].c[TEMPERATURE] * EMS[eUpper-1].Rho * EMS[eUpper-1].L);
					}

					// route mass and solute load to runoff
					removedMass += EMS[eUpper].M;
					if (iwatertransportmodel_snow != RICHARDSEQUATION) {
						// The mass from the snow element to be removed is snowpack runoff
						Sdata.mass[SurfaceFluxes::MS_SNOWPACK_RUNOFF] += EMS[eUpper].M;
						if (iwatertransportmodel_soil != RICHARDSEQUATION) {
							if (Xdata.SoilNode > 0) {
								// Only move water into soil when we don't run richardssolver for soil ...
								SnowStation::mergeElements(EMS[eUpper-1], EMS[eUpper], merged, (eUpper==rnE-1));
							}
						} else {
							// ... otherwise put it in surfacefluxrate
							RichardsEquationSolver1d_matrix.surfacefluxrate += EMS[eUpper].M / Constants::density_water / sn_dt;
						}
					} else {
						RichardsEquationSolver1d_matrix.soilsurfacesourceflux += EMS[eUpper].M / Constants::density_water / sn_dt;
					}

					//When snow water transport is solved by Richards Equation, we calculate snowpack runoff there.
					//However: when we remove the last snow element, there is no way for the Richards Solver to figure out that this surfacefluxrate is still coming from the snowpack. In that case, we should add the water to snowpack runoff here.
					if ( (rnE-1) == Xdata.SoilNode && (iwatertransportmodel_soil == RICHARDSEQUATION || Xdata.SoilNode == 0)) {
						// Special case for RE: if all snow elements disappear, soilsurfacesourceflux has no meaning, so it should become part of the surfacefluxrate:
						RichardsEquationSolver1d_matrix.surfacefluxrate += RichardsEquationSolver1d_matrix.soilsurfacesourceflux;
						RichardsEquationSolver1d_matrix.soilsurfacesourceflux = 0.;
						if (iwatertransportmodel_snow == RICHARDSEQUATION) {
							// Now make sure any left-over surfacefluxrate is considered snowpack runoff:
							Sdata.mass[SurfaceFluxes::MS_SNOWPACK_RUNOFF] += RichardsEquationSolver1d_matrix.surfacefluxrate*Constants::density_water*sn_dt;
						}
					}

					if (Xdata.SoilNode == 0) { // In case of no soil
						Sdata.mass[SurfaceFluxes::MS_SOIL_RUNOFF] += EMS[eUpper].M;
						for (size_t ii = 0; ii < Xdata.number_of_solutes; ii++) {
							Sdata.load[ii] += (EMS[eUpper].L*EMS[eUpper].theta[WATER]*EMS[eUpper].conc[WATER][ii]
							    + EMS[eUpper].L*EMS[eUpper].theta[ICE] * EMS[eUpper].conc[ICE][ii]) / S_TO_H(sn_dt);
						}
					}
				}
			}
			rnE--;
			rnN--;
			if(UpperJoin==false) {
				EMS[eUpper].Rho = Constants::undefined;
				if (!merged) {
					EMS[eUpper].L *= -1.;	// Mark element as "removed".
				}
				if ((eUpper < nE-1) && (EMS[eUpper+1].Rho < 0.) && (EMS[eUpper+1].L > 0.)) {
					// When upper+1 element is not marked to be removed, but we merge the upper element, we should remove the upper+1 element.
					EMS[eUpper+1].L *= -1.;
				}
			} else {
				if (EMS[eUpper+1].Rho == Constants::undefined) {
					// The upper join has the risk that an element (eUpper+1) could become marked Rho == Constants::undefined twice,
					// in which case we reduced rnE and rnN one too much.
					rnE++;
					rnN++;
				} else {
					EMS[eUpper+1].Rho = Constants::undefined;
					if (!merged && EMS[eUpper+1].L > 0.) {
						EMS[eUpper+1].L *= -1.;	// Mark element as "removed".
					}
					if ((eUpper+1 < nE-1) && (EMS[eUpper+2].Rho < 0.) && (EMS[eUpper+2].L > 0.)) {
						EMS[eUpper+2].L *= -1.;
					}
				}
			}
		}
	}
	if (rnE < nE) {
		Xdata.reduceNumberOfElements(rnE);
		if (!useSoilLayers && (rnE == Xdata.SoilNode)) {
			Xdata.Ndata[Xdata.SoilNode].T = std::min(Constants::meltfreeze_tk, Xdata.Ndata[Xdata.SoilNode].T);
		}
		if (verify_top_element && rnE > 0 && rnE > Xdata.SoilNode) {
			// Note: we have to check for the SoilNode, because verify_top_element may have been set to true, but multiple element removals may have
			// set rnE to the upper soil element, in case we should inhibit element splitting.
			if (.5 * (EMS[Xdata.getNumberOfElements()-1].L) > comb_thresh_l) {
				Xdata.splitElement(Xdata.getNumberOfElements()-1);
			}
		}
	}

	if (removedMass > 0. && variant == "SEAICE" && iwatertransportmodel_snow == RICHARDSEQUATION) {
		const double delta_h = (removedMass / (Constants::density_water + SeaIce::betaS * SeaIce::OceanSalinity));
		for (size_t e=Xdata.SoilNode; e<rnE; e++) {
			EMS[e].h -= delta_h;
		}
	}

	if (rnE >= Xdata.SoilNode) {
		Xdata.ColdContent = 0.;
		for (size_t e=Xdata.SoilNode; e<rnE; e++) {
			Xdata.ColdContent += EMS[e].coldContent();
		}
	}
}


/**
 * @brief Surface sublimation and melt artificially create surface elements that have a much
 * too low density and this needs to be corrected. \n
 * TODO Check description!
 * @param Xdata
 */
void WaterTransport::adjustDensity(SnowStation& Xdata)
{
	const size_t nN = Xdata.getNumberOfNodes();
	if (nN == Xdata.SoilNode + 1) return;

	const size_t nE = nN-1;
	vector<NodeData>& NDS = Xdata.Ndata;
	vector<ElementData>& EMS = Xdata.Edata;

	size_t e = nE;
	while ((e-- > Xdata.SoilNode) && (EMS[e].theta[SOIL] < Constants::eps2) && (EMS[e].mk%100 > 9)
	           && (EMS[e].theta[WATER] < 0.3) && (EMS[e].theta[ICE] < Snowpack::min_ice_content)
	               && (EMS[e].L > minimum_l_element)) {
		const double L = EMS[e].L;
		double dL=0.; // Element length change
		// For water layer go to water density
		if (water_layer && (EMS[e].theta[WATER] < 0.95) && (EMS[e].theta[ICE] < Snowpack::min_ice_content)
			&& (e > Xdata.SoilNode) && ((EMS[e-1].theta[SOIL] > 0.95) || (EMS[e-1].theta[ICE] > 0.95))) {
			dL = -L * (1. - EMS[e].theta[WATER]);
			EMS[e].theta[WATER] = 1.;
			EMS[e].theta[ICE] = 0.;
		} else if (variant == "JAPAN") {
			//NIED (H. Hirashima) //Fz: Please check this adaptation still works as you want it to work!
			double multif = 0.05 / EMS[e].theta[ICE];
			dL  = -L*((multif-1)/multif);
			EMS[e].theta[WATER] *= multif;
			EMS[e].theta[WATER_PREF] *= multif;
			EMS[e].theta[ICE]   *= multif;
		} else {
			dL  = -L / 3.; //TODO check whether this approach is correct even though it is a "very old SNOWPACK approach"
			// "dL = -L0/3" ist ein Urgestein in SNOWPACK und soll verhindern, dass Oberflächenelemente mit zu geringer Dichte entstehen. Die Setzung von Nassschnee ist weniger im Mittelpunkt hier.
			// Die Beschreibung der Funktion entspricht derjenigen, die in den ältesten Versionen zu finden ist :-(
			EMS[e].theta[WATER] *= 1.5;
			EMS[e].theta[WATER_PREF] *= 1.5;
			EMS[e].theta[ICE]   *= 1.5;
		}

		for (size_t eAbove = e; eAbove < nE; eAbove++) {
			NDS[eAbove+1].z += dL + NDS[eAbove+1].u;
			NDS[eAbove+1].u = 0.0;
			EMS[eAbove].E = EMS[eAbove].Eps = EMS[eAbove].dEps= EMS[eAbove].Eps_e=EMS[eAbove].Eps_v=0.0;
		}
		EMS[e].L0 = EMS[e].L = L + dL;
		EMS[e].theta[AIR] = 1.0 - EMS[e].theta[WATER] - EMS[e].theta[WATER_PREF] - EMS[e].theta[ICE];
		EMS[e].updDensity();
		if (!(EMS[e].Rho > Constants::eps && EMS[e].theta[AIR] >= 0.)) {
			prn_msg(__FILE__, __LINE__, "err", Date(), "Volume contents: e:%d nE:%d rho:%lf ice:%lf wat:%lf wat_pref:%lf air:%le",
			        e, nE, EMS[e].Rho, EMS[e].theta[ICE], EMS[e].theta[WATER], EMS[e].theta[WATER_PREF], EMS[e].theta[AIR]);
			throw IOException("Cannot evaluate mass balance in adjust density routine", AT);
		}
	}
	const double cH_old = Xdata.cH;
	Xdata.cH = NDS[Xdata.getNumberOfNodes()-1].z + NDS[Xdata.getNumberOfNodes()-1].u;
	Xdata.mH -= (cH_old - Xdata.cH);
}

/**
 * @brief Default version of WaterTransport \n
 * Now that the snowpack has been updated, you must move the water down and compute the amount
 * of water being released from the snowpack AFTER having gone through removeElements() \n
 * TODO Revise description!
 * @param *Xdata
 * @param *Sdata
 * @param *Mdata
 */
void WaterTransport::transportWater(CurrentMeteo& Mdata, SnowStation& Xdata, SurfaceFluxes& Sdata, double& ql)
{
	size_t nN = Xdata.getNumberOfNodes();
	size_t nE = nN-1;
	vector<NodeData>& NDS = Xdata.Ndata;
	vector<ElementData>& EMS = Xdata.Edata;

const int I = Xdata.meta.position.getGridI();
const int J = Xdata.meta.position.getGridJ();
        //std::cout << " I AM HERE: " << I << " , " << J << std::endl;
 

	//NIED (H. Hirashima) //Fz HACK Below follow some NIED specific declarations; please describe
	std::vector<double> Such(nE, 0.);		//Suction pressure head
	std::vector<double> HydK(nE, 0.);		//Hydraulic Conductivity
	double ThR,SatK;  		//Residual water content, saturated water content and saturated hydraulic conductivity for both layers respectively.
	double FluxQ;					//Flux between layers
	double Rh0,Rh1,Rk0,Rk1;
	double q0, qlim, qlim0, qlim1;
	double P[15]={0.};
	unsigned int WatCalc=1;					//Number of iterations in WaterTransport model "NIED".

	// First, consider no soil with no snow on the ground
	if (!useSoilLayers && nN == 1) {
		return;
	} else { // add rainfall to snow/soil pack
		if (Mdata.liq_psum > 0.) { //there is some rain
			double Store = (Mdata.liq_psum) / Constants::density_water; // Depth of liquid precipitation ready to infiltrate snow and/or soil (m)
			// Now find out whether you are on an impermeable surface and want to create a water layer ...
			if (water_layer && iwatertransportmodel_snow != RICHARDSEQUATION && iwatertransportmodel_soil != RICHARDSEQUATION && (Store > 0.)
			        && ((useSoilLayers && (nE == Xdata.SoilNode)
			                && (EMS[nE-1].theta[SOIL] > 0.95)) || ((nE-1 > 0) && (EMS[nE-2].theta[ICE] > 0.95)))) {
				nE++;
				nN++;
				Xdata.ErosionLevel = nE-1;
				Xdata.resize(nE);

				// Temperature of the uppermost node
				NDS[nN-1].T = Mdata.ta;
				// The new nodal position of upper node of top water-film layer (m)
				const double z_water = std::min(Store, std::max(0.001, 0.01 * Xdata.cos_sl));
				NDS[nN-1].z = NDS[nN-2].z + NDS[nN-2].u + z_water;
				Store -= z_water;
				// Fill the element data
				EMS[nE-1].depositionDate = Mdata.date;

				EMS[nE-1].Te = Mdata.ta;
				EMS[nE-1].L0 = EMS[nE-1].L = z_water;
				EMS[nE-1].theta[WATER] = 1.0;
				EMS[nE-1].updDensity();
				EMS[nE-1].M = EMS[nE-1].L0 * EMS[nE-1].Rho;
				assert(EMS[nE-1].M >= (-Constants::eps2)); //mass must be positive
				EMS[nE-1].mk = 19;
				//NOTE all other microstructure parameters should better be set to Constants::undefined but ...
				EMS[nE-1].N3 = 1.;
				EMS[nE-1].dd = 0.;
				EMS[nE-1].sp = 1.;
				EMS[nE-1].rg = 1.0;
				EMS[nE-1].rb = 0.5;
				Xdata.cH = Xdata.mH = NDS[nN-1].z + NDS[nN-1].u;
			} else if (water_layer && iwatertransportmodel_snow != RICHARDSEQUATION && iwatertransportmodel_soil != RICHARDSEQUATION && (Store > 0.)
			               && ((useSoilLayers && (nE == Xdata.SoilNode+1) && (EMS[nE-2].theta[SOIL] > 0.95))
			                       || ((nE > 1) && (EMS[nE-2].theta[ICE] > 0.95)))) {
				// Put rain water in existing wet layer
				// The new nodal position of upper node of top water-film layer (m)
				const double z_water = std::min(Store, std::max(0.0, (0.01 * Xdata.cos_sl - EMS[nE-1].L)));
				NDS[nN-1].z += z_water;
				Store -= z_water;
				EMS[nE-1].L0 = EMS[nE-1].L = (NDS[nN-1].z + NDS[nN-1].u) - (NDS[nN-2].z + NDS[nN-2].u);
				EMS[nE-1].M = EMS[nE-1].L0 * EMS[nE-1].Rho;
				assert(EMS[nE-1].M >= (-Constants::eps2)); //mass must be positive
				Xdata.cH = Xdata.mH = NDS[nN-1].z + NDS[nN-1].u;
			}

			//Put rain water in the layers, starting from the top element.
			size_t e = nE;
			while (Store > 0.0 && e > 0) {
				e--;	//This construct with e=nE and e-- is to circumvent troubles with the unsigned ints.
				if ((iwatertransportmodel_snow != RICHARDSEQUATION && e>=Xdata.SoilNode) || (iwatertransportmodel_soil != RICHARDSEQUATION && e<Xdata.SoilNode)) {
					const double L = EMS[e].L;
					//Check how much space is left to put rain water in OR for the lowest layer, put all rain water in, to prevent loosing the mass.
					const double dThetaW = (e>0)? std::min(Constants::density_ice/Constants::density_water*EMS[e].theta[AIR], Store/L) : Store / L;

					Store -= dThetaW*L;
					for (size_t ii = 0; ii < Xdata.number_of_solutes; ii++) {
						EMS[e].conc[WATER][ii] = (L * dThetaW * Mdata.conc[ii]
										  + L * EMS[e].theta[WATER] * EMS[e].conc[WATER][ii])
									      / (L * (EMS[e].theta[WATER] + dThetaW));
					}
					EMS[e].theta[WATER] += dThetaW;
					EMS[e].theta[AIR] -= dThetaW;
					EMS[e].M += dThetaW * L * Constants::density_water;
					assert(EMS[e].M >= (-Constants::eps2)); //mass must be positive
					// Update snowpack runoff with rain infiltrating into soil (equal to Store when e == Xdata.SoilNode)
					if (e == Xdata.SoilNode) {
						Sdata.mass[SurfaceFluxes::MS_SNOWPACK_RUNOFF] += Store * Constants::density_water;
					}
					// Update soil runoff with rain (equal to Store when e == 0)
					if (e == 0) {
						Sdata.mass[SurfaceFluxes::MS_SOIL_RUNOFF] += Store * Constants::density_water;
					}
				}
			}

			//This adds the left over rain input to the surfacefluxrate, to be used as BC in Richardssolver:
			if (pref_flow_rain_input_domain=="MATRIX") {
				// Put rain in matrix domain
				RichardsEquationSolver1d_matrix.surfacefluxrate+=(Store)/(sn_dt);	//NANDER: Store=[m], surfacefluxrate=[m^3/m^2/s]
			} else if (pref_flow_rain_input_domain=="PREF_FLOW") {
				// Put rain in preferential domain
				RichardsEquationSolver1d_pref.surfacefluxrate+=(Store)/(sn_dt);		//NANDER: Store=[m], surfacefluxrate=[m^3/m^2/s]
			} else {
				prn_msg( __FILE__, __LINE__, "err", Mdata.date, "Unknown domain to transfer rain water to (check key PREF_FLOW_RAIN_INPUT_DOMAIN).");
				throw;
			}

			Sdata.mass[SurfaceFluxes::MS_RAIN] += Mdata.liq_psum;
                        Mdata.liq_psum = 0.0;
		}
	}

	// Determine the number of iterations for the water transport
	unsigned int niterations;
	if(iwatertransportmodel_snow==NIED || iwatertransportmodel_soil==NIED) {
		niterations=static_cast<unsigned int>(double(sn_dt/double(60.)));	// Hirashima (2010) proposes maximum time step of 60s.
	} else {
		niterations=1;
	}
	WatCalc=niterations;

	double excess_water=0.;
	double Wres;          // Residual water content depending on snow or soil element

	for(unsigned int niter=1; niter<=niterations; niter++) {
		// Preferential flow system: excess water that cannot be retained in lower element is stored in
		//   excess_water and moved down the domain; NOTE units: [m^3/m^2]
		excess_water=0.;

		// Now move water as needed, starting from the top element ...
		if (iwatertransportmodel_snow != RICHARDSEQUATION && (iwatertransportmodel_soil != RICHARDSEQUATION || nE>Xdata.SoilNode)) {
			for (size_t eUpper = nE-1, eLower = nE-2; eUpper >= 1; eUpper--, eLower-- ) {
				// Determine the additional storage capacity due to refreezing
				const double dth_w = EMS[eUpper].c[TEMPERATURE] * EMS[eUpper].Rho / Constants::lh_fusion / Constants::density_water
							* std::max(0., EMS[eUpper].meltfreeze_tk-EMS[eUpper].Te);
				if ((variant=="SEAICE" && Xdata.Seaice!=NULL) && Xdata.Ndata[eUpper].z + 0.5 * Xdata.Edata[eUpper].L < Xdata.Seaice->SeaLevel) {
					// for sea ice: elements below sea level may fill entire pore space
					Wres = std::max(0., (1. - Xdata.Edata[eUpper].theta[ICE]) * (Constants::density_ice/Constants::density_water));
				} else if ((eUpper == nE-1) && (EMS[eLower].theta[AIR] <= 0.05) && water_layer) {
					// allow for a water table in the last layer above road/rock
					Wres = Constants::density_ice/Constants::density_water
						  * (1. - EMS[eUpper].theta[ICE] - EMS[eUpper].theta[SOIL] - 0.05);
				} else if (EMS[eUpper].theta[SOIL] < Constants::eps2) {
					Wres = std::min((1. - EMS[eUpper].theta[ICE]) * Constants::density_ice / Constants::density_water,
						  EMS[eUpper].res_wat_cont + dth_w);
				} else { // treat soil separately
					Wres = std::min(Constants::density_ice / Constants::density_water
						      * (1. - EMS[eUpper].theta[ICE] - EMS[eUpper].theta[SOIL]),
						  EMS[eUpper].soilFieldCapacity() + dth_w);
				}
				if ((iwatertransportmodel_snow == NIED && eUpper>=Xdata.SoilNode) || (iwatertransportmodel_soil == NIED && eUpper<Xdata.SoilNode)) {
					// For watertransport model NIED:
					// If we have too much water in the element (can happen when two quite saturated elements get joined), put the excess water in excess_water.
					// Note: we have to do this in case of NIED method, because else SEff will not be calculated correctly.
					if(EMS[eUpper].theta[WATER] > (1.-EMS[eUpper].theta[WATER_PREF]-EMS[eUpper].theta[ICE]-EMS[eUpper].theta[SOIL])*(Constants::density_ice/Constants::density_water)+Constants::eps2) {
						const double theta_water_orig=EMS[eUpper].theta[WATER];
						EMS[eUpper].theta[WATER]=(1.-EMS[eUpper].theta[WATER_PREF]-EMS[eUpper].theta[ICE]-EMS[eUpper].theta[SOIL])*(Constants::density_ice/Constants::density_water);
						EMS[eUpper].theta[AIR]=(1.-EMS[eUpper].theta[ICE]-EMS[eUpper].theta[SOIL]-EMS[eUpper].theta[WATER]-EMS[eUpper].theta[WATER_PREF]);
						EMS[eUpper].updDensity();
						EMS[eUpper].M = EMS[eUpper].Rho*EMS[eUpper].L;

						// Put excess water in excess_water
						excess_water+=(theta_water_orig-EMS[eUpper].theta[WATER])*EMS[eUpper].L;
					}
					//Wres = std::min(EMS[eUpper].theta[ICE], Wres); //NIED (H. Hirashima)
					Wres = std::max(0., Wres);
				} else {
					Wres = std::max(0., Wres);
				}

				const double W_upper = EMS[eUpper].theta[WATER];
				if (eUpper == nE-1 && (W_upper > 0.0 && W_upper <= Wres)) {
					// In that case you need to update the volumetric air content and the density of the top element
					// as it may have caught some rain! Only top element should be considered, as when rain would have
					// infiltrated lower elements as well, W_upper>Wres.
					EMS[eUpper].theta[AIR] = std::max(0., 1. - EMS[eUpper].theta[WATER] - EMS[eUpper].theta[WATER_PREF] - EMS[eUpper].theta[ICE] - EMS[eUpper].theta[SOIL]);
					EMS[eUpper].updDensity();
					if (!(EMS[eUpper].Rho > Constants::eps && EMS[eUpper].theta[AIR] >= 0.)) {
						prn_msg(__FILE__, __LINE__, "err", Mdata.date,
							"Volume contents: e:%d nE:%d rho:%lf ice:%lf wat:%lf wat_pref:%lf air:%le",
							eUpper, nE, EMS[eUpper].Rho, EMS[eUpper].theta[ICE], EMS[eUpper].theta[WATER], EMS[eUpper].theta[WATER_PREF], EMS[eUpper].theta[AIR]);
						throw IOException("Cannot transfer water within the snowpack in transportWater()", AT);
					}
				}

				if (W_upper > Wres || excess_water > 0.) {
					// Then water is being transferred between elements
					const double L_upper = EMS[eUpper].L;
					const double L_lower = EMS[eLower].L;
					const double W_lower = EMS[eLower].theta[WATER];
					double dThetaW_upper = std::max(0., W_upper - Wres);

					if ((iwatertransportmodel_snow == NIED && eUpper>Xdata.SoilNode) || (iwatertransportmodel_soil == NIED && eUpper<=Xdata.SoilNode)) {	//For watertransport model NIED we redefine dThetaW_upper
						// The WaterTransport model "NIED" was developed by Hiroyuki Hirashima, Snow and Ice Research Center, NIED under support of the NIED project 'A research project for developing a snow disaster forecasting system and snow-hazard maps'.
						// For details, see: Hirashima et al. (2010) Numerical modeling of liquid water movement through layered snow based on new measurements of the water retention curve. Cold Regions Science and Technology, 64(2), 94-103.
						ThR = 0.024;
						SatK = 0.077 * (EMS[eUpper].rg / 1000.)*(EMS[eUpper].rg / 1000.) * exp(-7.8 * EMS[eUpper].theta[ICE] * 0.917) * 1000. * 9.8 / 0.001792;

						KHCalcNaga(EMS[eUpper].rg * 2.0, EMS[eUpper].theta[ICE] * Constants::density_ice, ThR, EMS[eUpper].theta[WATER], SatK, Rh0, Rk0);
						KHCalcNaga(EMS[eLower].rg * 2.0, EMS[eLower].theta[ICE] * Constants::density_ice, ThR, EMS[eLower].theta[WATER], SatK, Rh1, Rk1);

						Such[eUpper] = Rh0;
						HydK[eUpper] = Rk0;
						Such[eLower] = Rh1;
						HydK[eLower] = Rk1;

						qlim0 = EMS[eUpper].theta[WATER] * L_upper;
						qlim1 = -EMS[eLower].theta[WATER] * L_lower;
						P[1] = EMS[eUpper].rg * 2.0; P[2] = EMS[eUpper].theta[ICE] * 0.917*1000; P[3] = ThR; P[4] = EMS[eUpper].theta[WATER]; P[5] = Such[eUpper];
						P[6] = EMS[eLower].rg * 2.0; P[7] = EMS[eLower].theta[ICE] * 0.917*1000; P[8] = ThR; P[9] = EMS[eLower].theta[WATER]; P[10] = Such[eLower];
						P[11] = L_upper; P[12] = L_lower;

 						qlim = Bisection (qlim1, qlim0, P);

						q0 = HydK[eUpper] * ((Such[eLower] - Such[eUpper]) / ((EMS[eUpper].L+EMS[eLower].L)/2.)+1.);
						if (qlim < Constants::eps) {
							FluxQ = qlim;
						} else {
							const double exp_argument = -q0 / qlim * (sn_dt/WatCalc);
							if(exp_argument<-50.) {
								FluxQ = qlim * (1. - 0.);
							} else if(exp_argument>20.) {
								FluxQ = qlim * (1 - exp(20.));
							} else {
								FluxQ = qlim * (1 - exp( exp_argument ));
							}
						}
						if(eUpper==Xdata.SoilNode && iwatertransportmodel_soil == RICHARDSEQUATION) {
							//dThetaW_upper = std::max(0., EMS[eUpper].theta[WATER]
						} else {
							dThetaW_upper = FluxQ / (EMS[eUpper].L);
						}
					}

					if (!(iwatertransportmodel_soil == RICHARDSEQUATION && eLower<Xdata.SoilNode)) {	//NANDER: Only if water is not transported INTO soil when we use RE in soil.
						if (dThetaW_upper > 0. || excess_water > 0.) {
							// dThetaW_lower is determined by also taking excess_water into account. Maybe excess_water can be stored in this layer.
							double dThetaW_lower = dThetaW_upper*(L_upper/L_lower)+(excess_water/L_lower);
							// Now check whether there is enough air left - in case of ice, rock or heavy
							// soil you might not be able to move the water or/and water may refreeze and expand.
							// Specifically, you might want to create a water table over ice or frozen soil
							if ((dThetaW_lower + W_lower) > (Constants::density_ice / Constants::density_water
								* (1. - EMS[eLower].theta[ICE] - EMS[eLower].theta[SOIL]))) {
								// Deal with excess water ... Look how much you can leave in the lower element eLower.
								// If you have too much water even for the lower element (more melt or rain per time
								// step than can be kept in this element), water is transferred to excess_water.
								// excess_water moves the water downward, trying to insert the water in lower elements.
								const double i_dThetaW_lower=dThetaW_lower;		//Make backup
								dThetaW_lower = std::max(0., (Constants::density_ice/Constants::density_water
									* (1. - EMS[eLower].theta[ICE] - EMS[eLower].theta[SOIL]) - W_lower));
								if (jam) {
									// In case of jam, the change in water content in the upper layer is the maximum possible
									// change in water content in the lower layer minus excess_water. This means excess_water
									// has the right of way to fill up the lower element. If the lower element cannot contain
									// all excess_water, it is transferred to the upper element. TODO: actually, if we fill
									// the upper layer completely and we still have excess_water left, we should put it in
									// the layer above the upper layer and so forth.
									dThetaW_upper = dThetaW_lower*L_lower/L_upper - excess_water/L_upper;
									if ((W_upper - dThetaW_upper) > (Constants::density_ice/Constants::density_water
												  * (1. - EMS[eUpper].theta[ICE] - EMS[eUpper].theta[SOIL]))) {
										const double i_dThetaW_upper = dThetaW_upper;	//Make backup
										dThetaW_upper = W_upper - Constants::density_ice/Constants::density_water
												    * (1. - EMS[eUpper].theta[ICE] - EMS[eUpper].theta[SOIL]);
										excess_water = (i_dThetaW_upper-dThetaW_upper)*L_upper;
									} else {
										excess_water = 0.;
									}
									if (EMS[eLower].theta[SOIL] < Constants::eps2) {
										Sdata.mass[SurfaceFluxes::MS_SNOWPACK_RUNOFF] += excess_water*Constants::density_water;
									}
									// Take care of Solutes
									for (size_t ii = 0; ii < Xdata.number_of_solutes; ii++) {
										Sdata.load[ii] += (EMS[eLower].conc[WATER][ii] * excess_water
												  * Constants::density_water/S_TO_H(sn_dt));
									}
									// We have moved the excess_water out of the snowpack.
									excess_water=0.;
								} else {
									// All the water that could not be stored in eLower is considered excess_water.
									excess_water = (i_dThetaW_lower-dThetaW_lower)*L_lower;
								}
							} else {
								// else EMS[eLower] can contain all water, so we have no excess_water anymore.
								excess_water=0.;
							}

							// Water movement from element eUpper to element eLower: move solutes also
							for (size_t ii = 0; ii < Xdata.number_of_solutes; ii++) {
								EMS[eLower].conc[WATER][ii] = (W_lower * EMS[eLower].conc[WATER][ii] + dThetaW_lower * EMS[eUpper].conc[WATER][ii])
											  / (W_lower+dThetaW_lower);
							}

							// update volumetric contents, masses and density
							EMS[eUpper].theta[WATER]=W_upper-dThetaW_upper;
							EMS[eLower].theta[WATER]=W_lower+dThetaW_lower;
							EMS[eUpper].theta[AIR] = 1. - EMS[eUpper].theta[WATER] - EMS[eUpper].theta[WATER_PREF] - EMS[eUpper].theta[ICE] - EMS[eUpper].theta[SOIL];
							EMS[eLower].theta[AIR] = 1. - EMS[eLower].theta[WATER] - EMS[eLower].theta[WATER_PREF] - EMS[eLower].theta[ICE] - EMS[eLower].theta[SOIL];
							EMS[eUpper].M -= L_upper * Constants::density_water * dThetaW_upper;
							assert(EMS[eUpper].M >= (-Constants::eps2)); //mass must be positive
							EMS[eLower].M += L_lower * Constants::density_water * dThetaW_lower;
							assert(EMS[eLower].M >= (-Constants::eps2)); //mass must be positive
							EMS[eUpper].updDensity();
							assert(EMS[eUpper].Rho>=0. || EMS[eUpper].Rho==IOUtils::nodata); //we want positive density
							EMS[eLower].updDensity();
							assert(EMS[eLower].Rho>=0. || EMS[eLower].Rho==IOUtils::nodata); //we want positive density
							if (EMS[eUpper].theta[SOIL] < Constants::eps2) {
								if (!(EMS[eUpper].theta[AIR] >= -Constants::eps)) {
									prn_msg(__FILE__, __LINE__, "err", Mdata.date,
										"Volume contents: e:%d nE:%d rho:%lf ice:%lf wat:%lf wat_pref:%lf air:%le",
										eUpper, nE, EMS[eUpper].Rho, EMS[eUpper].theta[ICE], EMS[eUpper].theta[WATER], EMS[eUpper].theta[WATER_PREF], EMS[eUpper].theta[AIR]);
									throw IOException("Cannot transfer water within the snowpack in transportWater()", AT);
								}
							}
							// Update snowpack runoff with soil. Note: in case of no soil layers, or lowest soil element: the runoff for the lowest element is updated outside the loop.
							if (useSoilLayers && eUpper == Xdata.SoilNode) {
								Sdata.mass[SurfaceFluxes::MS_SNOWPACK_RUNOFF] += L_lower * Constants::density_water * dThetaW_lower + excess_water * Constants::density_water;
							}
						} // end positive water movement
					} else { //If eLower is soil (so water would be transported INTO soil), only remove water from snow element and don't put in soil, but in surfacefluxrate:
						//dThetaW_upper = std::max(0, dThetaW_upper);
						if(eLower==Xdata.SoilNode-1 && eUpper==Xdata.SoilNode) {
							Sdata.mass[SurfaceFluxes::MS_SNOWPACK_RUNOFF] += L_upper * Constants::density_water * dThetaW_upper + excess_water * Constants::density_water;
							RichardsEquationSolver1d_matrix.surfacefluxrate+=((dThetaW_upper*L_upper)+excess_water)/(sn_dt);	//surfacefluxrate is used for the Neumann BC in the Richards solver. Note: W0 is m^3/m^3
																//note: we devide by snowpack time-step (sn_dt), and not by (sn_dt/WatCalc), as we will spread the amount of runoff evenly over the snowpack time step.
							excess_water=0.;

							//Remove water from e0:
							// update volumetric contents, masses and density
							EMS[eUpper].theta[WATER]=W_upper-dThetaW_upper;
							EMS[eUpper].theta[AIR] = 1. - EMS[eUpper].theta[WATER] - EMS[eUpper].theta[WATER_PREF] - EMS[eUpper].theta[ICE] - EMS[eUpper].theta[SOIL];
							EMS[eUpper].M -= L_upper * Constants::density_water * dThetaW_upper;
							EMS[eUpper].updDensity();
							if (EMS[eUpper].theta[SOIL] < Constants::eps2) {
								if (!(EMS[eUpper].theta[AIR] >= -Constants::eps)) {
									prn_msg(__FILE__, __LINE__, "err", Mdata.date,
										"Volume contents: e:%d nE:%d rho:%lf ice:%lf wat:%lf wat_pref:%lf air:%le",
										eUpper, nE, EMS[eUpper].Rho, EMS[eUpper].theta[ICE], EMS[eUpper].theta[WATER], EMS[eUpper].theta[WATER_PREF], EMS[eUpper].theta[AIR]);
									throw IOException("Cannot transfer water within the snowpack in transportWater()", AT);
								}
							}
						}
						break;	//Don't treat other elements. They will all be soil and destroy the calculations. Leave FOR-loop.
					} // end if water is transported into soil and richards solver is used for soil
				}  // end if (W_upper > Wres )
			}  // end FOR loop over the number of elements
		} // end of IF statement to branch between WaterTransport model RICHARDSEQUATION and NIED/BUCKET
	} // end FOR loop over time step iterations (loop executed only more than once in case of "NIED")

	//Now solve richards equation:
	if((iwatertransportmodel_snow == RICHARDSEQUATION && nE>0) || (iwatertransportmodel_soil == RICHARDSEQUATION && Xdata.SoilNode > 0)) {
		double dummy_ql = 0.;	// A dummy_ql, that may be sent to Richards equation, when evaporation / condensation doesn't need to be considered, to keep the original ql intact, so it can be treated as sublimation / deposition later

		// Only send ql if Richards equation will solve the upper element and thus should take care of evaporation / condensation:
		const bool isTopLayerSolvedByREQ = (nE == Xdata.SoilNode || (nE > Xdata.SoilNode && iwatertransportmodel_snow == RICHARDSEQUATION));

		// Only send ql if ql should first be considered as evaporation / condensation, and NOT sublimation / deposition, depending on surface temperature:
		const double meltfreeze_tk = (Xdata.getNumberOfElements()>0)? Xdata.Edata[Xdata.getNumberOfElements()-1].meltfreeze_tk : Constants::meltfreeze_tk;
		const bool isSurfaceMelting = !(NDS[nE].T < meltfreeze_tk);

		RichardsEquationSolver1d_matrix.SolveRichardsEquation(Xdata, Sdata, ((isTopLayerSolvedByREQ && isSurfaceMelting) || (variant == "SEAICE" && ql < 0.)) ? (ql) : (dummy_ql));					
		if(Xdata.getNumberOfElements() > Xdata.SoilNode && enable_pref_flow) RichardsEquationSolver1d_pref.SolveRichardsEquation(Xdata, Sdata, dummy_ql);	// Matrix flow will take care of potential evaporation/condensation, provided by ql, so send dummy_ql for preferential flow
	}

	// The TOP element is very important because it is always losing mass--the strain state
	// is becoming more and more deformed.  Update the strain state of the top element.
	const size_t eTop = nE-1;
	EMS[eTop].L0 = EMS[eTop].L;
	NDS[nN-1].z += NDS[nN-1].u; NDS[nN-1].u = 0.0;
	NDS[nN-2].z += NDS[nN-2].u; NDS[nN-2].u = 0.0;
	EMS[eTop].E = EMS[eTop].Eps = EMS[eTop].dEps = EMS[eTop].Eps_e = EMS[eTop].Eps_v = EMS[eTop].S = 0.0;

	// RUNOFF at bottom of either snowpack or soil
	if(variant != "SEAICE") {	//Not for sea ice, where we assume the lowest element to be under water.
		if((!useSoilLayers && iwatertransportmodel_snow != RICHARDSEQUATION) || iwatertransportmodel_soil != RICHARDSEQUATION) {	//Only if lowest element is snow or we do not use RE for soil.
			// Determine the additional storage capacity due to refreezing
			const double dth_w = EMS[0].c[TEMPERATURE] * EMS[0].Rho / Constants::lh_fusion / Constants::density_water
						* std::max(0., EMS[0].meltfreeze_tk-EMS[0].Te);
			if (EMS[0].theta[SOIL] < Constants::eps2) {
				Wres = std::min((1. - EMS[0].theta[ICE]) * Constants::density_ice / Constants::density_water,
					  EMS[0].res_wat_cont + dth_w);
			} else { // treat soil separately
				Wres = std::min(Constants::density_ice/Constants::density_water*(1. - EMS[0].theta[ICE] - EMS[0].theta[SOIL]),
				      EMS[0].soilFieldCapacity() + dth_w);
			}
			Wres = std::max(0., Wres);

			// Add excess water to the bottom element, such that it does not get lost
			EMS[0].theta[WATER] += excess_water / EMS[0].L;
			excess_water = 0.;

			const double W0 = EMS[0].theta[WATER];
			if ((W0 > Wres) // NOTE: if water_layer is set, do not drain water element on top of soil
				&& !(water_layer && (EMS[0].theta[ICE] < Snowpack::min_ice_content)
					&& (EMS[0].theta[SOIL] < Constants::eps2))) {
				const double dM = EMS[0].L * Constants::density_water * (W0 - Wres);
				EMS[0].M -= dM;
				assert(EMS[0].M >= (-Constants::eps2)); //mass must be positive
				EMS[0].theta[WATER] = Wres;
				EMS[0].theta[AIR] = 1. - EMS[0].theta[WATER] - EMS[0].theta[WATER_PREF] - EMS[0].theta[ICE] - EMS[0].theta[SOIL];
				EMS[0].updDensity();
				assert(EMS[0].Rho>=0. || EMS[0].Rho==IOUtils::nodata); //we want positive density
				// Note that remaining excess_water should also be routed to MS_SOIL_RUNOFF and MS_SNOWPACK_RUNOFF
				if (EMS[0].theta[SOIL] < Constants::eps2) {
					Sdata.mass[SurfaceFluxes::MS_SNOWPACK_RUNOFF] += dM + (excess_water * Constants::density_water);
				}
				Sdata.mass[SurfaceFluxes::MS_SOIL_RUNOFF] += dM + (excess_water * Constants::density_water);
				for (size_t ii = 0; ii < Xdata.number_of_solutes; ii++) {
					Sdata.load[ii] +=  (EMS[0].conc[WATER][ii] * dM / S_TO_H(sn_dt));
				}
			}
		}
	}
}

/**
 * @brief The mass transport procedure is called from sn_Snowpack -- AFTER calling
 * the NEWSNOW (sn_SnowFall) or SNOWDRIFT (sn_SnowDrift) modules but BEFORE
 * calling the TEMPERATURE (sn_SnowTemperature), PHASECHANGE (pc_PhaseChange)
 * or CREEP (sn_SnowCreep) routines. \n
 * The mass transport routines were inserted at this location since they can set the NEWMESH
 * variable which means the FEM data structures must be reallocated before solving the instationary heat equations(?) \n
 * These routines are responsible for MOVING MASS (water) in, out and through the
 * snowpack.  They are subsequently responsible for WATER TRANSPORT and SURFACE
 * SUBLIMATION.  Since surface sublimation does not change the FE data structure
 * it is treated FIRST. \n
 * The phase change routines will increment the volumetric water content of the
 * elements, then the WATER TRANSPORT routines will move the excess water from
 * element "e" to element "e-1". \n
 * NOTES:
 * -#  The water will only be moved if it is above the residual water content
 * -#   The water will only be moved if there is enough VOID SPACE in the element
 *      receiving the water
 * -#   Water in the last element will be DISCHARGED from the snowpack.  This
 *      amount of water is termed the MELTWATER RUNOFF
 * -#   It would be very possible to make the RESIDUAL_WATER_CONTENT a function
 *      of the MICRO-properties of the snow.  This is simply a few lines of code. \n
 *      This was done on 3 Dec 2006 -> ElementData::snowResidualWaterContent().
 * -#   IMPORTANT: the top surface element can be removed if the VOLUMETRIC ICE
 *      content is 0; that is, when the element contains only water and voids. \n
 * The routines were changed dramatically by Perry on June 3rd 1998 after Perry
 * and Michael worked the entirity of June 2nd together.  We were running
 * the model operationally in Davos and discovered that the code was bombing
 * during heavy melt periods.  Michael identified the problem that the elements
 * were being REMOVED after the water was TRANSPORTED.  Meaning that water contents
 * greater than the 0.03 were being picked up and if the melting was strong
 * enough, then negative volumetric AIR contents were predicted.  The solution
 * to the problem is simple enough: FIRST remove the elements, then caculate
 * the WATER TRANSPORT.
 * @param Xdata
 * @param ql Latent heat flux (W m-2)
 * @param Sdata
 * @param Mdata
 */
void WaterTransport::compTransportMass(CurrentMeteo& Mdata,
                                       SnowStation& Xdata, SurfaceFluxes& Sdata, double& ql)
{
	RichardsEquationSolver1d_matrix.surfacefluxrate=0.;		//These are for the interface of snowpack with the richards solver. Initialize it to 0.
	RichardsEquationSolver1d_matrix.soilsurfacesourceflux=0.;

	RichardsEquationSolver1d_pref.surfacefluxrate=0.;		//These are for the interface of snowpack with the richards solver. Initialize it to 0.
	RichardsEquationSolver1d_pref.soilsurfacesourceflux=0.;


	// Do the checks for the WaterTransport model chosen:
	if(iwatertransportmodel_snow != BUCKET && iwatertransportmodel_snow != NIED && iwatertransportmodel_snow != RICHARDSEQUATION) {
		prn_msg( __FILE__, __LINE__, "err", Mdata.date, "Unknown watertransport model for snow.");
		throw;
	}

	if(iwatertransportmodel_soil != BUCKET && iwatertransportmodel_soil != NIED && iwatertransportmodel_soil != RICHARDSEQUATION) {
		prn_msg( __FILE__, __LINE__, "err", Mdata.date, "Unknown watertransport model for soil.");
		throw;
	}

	if(iwatertransportmodel_snow == RICHARDSEQUATION && (iwatertransportmodel_soil != RICHARDSEQUATION && useSoilLayers)) {
		prn_msg( __FILE__, __LINE__, "err", Mdata.date, "You can only use RICHARDSEQUATION for snow when you also use RICHARDSEQUATION for soil!");
		throw;
	}

	if(iwatertransportmodel_soil == NIED) {
	  	prn_msg( __FILE__, __LINE__, "err", Mdata.date, "You can only use NIED for snow, not for soil (soil parameters of the Van Genuchten model not implemented)!");
		throw;
	}

	// First, consider no soil with no snow on the ground and deal with possible rain water
	if (!useSoilLayers && (Xdata.getNumberOfNodes() == Xdata.SoilNode+1)) {
		if (Mdata.liq_psum > 0.) { //there is some rain
			double precip_rain = Mdata.liq_psum;
			Sdata.mass[SurfaceFluxes::MS_RAIN] += precip_rain;
			Sdata.mass[SurfaceFluxes::MS_SOIL_RUNOFF] += precip_rain;
			for (size_t ii = 0; ii < Xdata.number_of_solutes; ii++) {
				Sdata.load[ii] += Mdata.conc[ii] * precip_rain /*/ S_TO_H(sn_dt)*/;
			}
                Mdata.liq_psum = 0.0;
		}
		return;
	}

	compTopFlux(ql, Xdata, Sdata);
	mergingElements(Xdata, Sdata);

	try {
		adjustDensity(Xdata);
		if (variant=="SEAICE" && Xdata.Seaice!=NULL && iwatertransportmodel_snow == BUCKET) Xdata.Seaice->compFlooding(Xdata);
		transportWater(Mdata, Xdata, Sdata, ql);
	} catch(const exception&){
		prn_msg( __FILE__, __LINE__, "err", Mdata.date, "Error in transportMass()");
		throw;
	}
}

/*
 * End of WaterTransport.cc
 */
