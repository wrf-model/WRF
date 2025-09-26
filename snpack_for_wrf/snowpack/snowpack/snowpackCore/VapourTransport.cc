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

#include <snowpack/snowpackCore/VapourTransport.h>
#include <snowpack/snowpackCore/Snowpack.h>
#include <snowpack/snowpackCore/PhaseChange.h>
#include <snowpack/Meteo.h>
#include <snowpack/snowpackCore/WaterTransport.h>
#include <snowpack/snowpackCore/ReSolver1d.h>
#include <snowpack/Utils.h>

#include <assert.h>

using namespace std;
using namespace mio;

VapourTransport::VapourTransport(const SnowpackConfig& cfg)
               : WaterTransport(cfg), RichardsEquationSolver1d(cfg, false), variant(),
                 iwatertransportmodel_snow(BUCKET), iwatertransportmodel_soil(BUCKET), watertransportmodel_snow("BUCKET"), watertransportmodel_soil("BUCKET"),
                 sn_dt(IOUtils::nodata),
                 hoar_thresh_rh(IOUtils::nodata), hoar_thresh_vw(IOUtils::nodata), hoar_thresh_ta(IOUtils::nodata),
                 useSoilLayers(false), water_layer(false), enable_vapour_transport(false)
                 //hoar_density_buried(IOUtils::nodata), hoar_density_surf(IOUtils::nodata), hoar_min_size_buried(IOUtils::nodata),
                 //minimum_l_element(IOUtils::nodata),
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

	//Enable vapour transport
	cfg.getValue("ENABLE_VAPOUR_TRANSPORT", "SnowpackAdvanced", enable_vapour_transport);

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
}

/**
 * @brief Calculate the surface sublimation / deposition (i.e., only gas-solid). \n
 * The fraction of the latent heat flux ql that has not been used so far will be used for
 * sublimation/deposition. If positive (and above a certain cutoff level) then there
 * is a possibility that surface hoar crystal have grown. Of course, if negative
 * then we are also loosing mass from the surface.\n
 * This function additionally takes care of surface hoar formation and destruction.
 * Note that surface hoar is a nodal property, altough the corresponding mass is carried
 * by the underlying element.
 * @param *Mdata
 * @param ql Latent heat flux (W m-2)
 * @param *Xdata
 * @param *Sdata
 */
void VapourTransport::compSurfaceSublimation(const CurrentMeteo& Mdata, double& ql, SnowStation& Xdata,
                                            SurfaceFluxes& Sdata)
{
	double dL = 0., dM = 0.;     // Length and mass changes
	double M = 0.;               // Initial mass and volumetric content (water or ice)
	double dHoar = 0.;           // Actual change in hoar mass
	double cH_old;               // Temporary variable to hold height of snow

	const size_t nN = Xdata.getNumberOfNodes();
	size_t nE = nN-1;
	vector<NodeData>& NDS = Xdata.Ndata;
	vector<ElementData>& EMS = Xdata.Edata;
	const double Tss = NDS[nE].T; // Surface Temperature

	/*
	 * If ql > 0:
	 * Surface hoar is formed when surface temperature is below freezing.
	 * If no surface hoar can be formed, ql is kept and is used as boundary condition
	 * when calculating vapour flux.
	 * If there are elements and ql < 0:
	 * If ql is large enough to remove full surface elements, remove them.
	 * left over ql is used as boundary condition when calculating vapour flux.
	 *
	 * In both cases: add/subtract mass to MS_SUBLIMATION
	 */
	if (ql > Constants::eps2) { // Add Mass
		const double meltfreeze_tk = (Xdata.getNumberOfElements()>0)? Xdata.Edata[Xdata.getNumberOfElements()-1].meltfreeze_tk : Constants::meltfreeze_tk;
		if (Tss < meltfreeze_tk) { // Add Ice
			dM = ql*sn_dt/Constants::lh_sublimation;
			//if rh is very close to 1, vw too high or ta too high, surface hoar is destroyed and should not be formed
			if (!((Mdata.rh > hoar_thresh_rh) || (Mdata.vw > hoar_thresh_vw) || (Mdata.ta >= IOUtils::C_TO_K(hoar_thresh_ta)))) {
				// Under these conditions, form surface hoar
				ql = 0.;
				Sdata.mass[SurfaceFluxes::MS_SUBLIMATION] += dM;
				dHoar = dM;

				// In this case adjust properties of element, keeping snow density constant
				const double L_top = EMS[nE-1].L;
				const double theta_i0 = EMS[nE-1].theta[ICE];
				dL = dM/(EMS[nE-1].Rho); // length change
				if (nE == Xdata.SoilNode) {
					dL = 0.;
					dM = std::min(dM,EMS[nE-1].theta[AIR]*(Constants::density_ice*EMS[nE-1].L));
				}
				NDS[nE].z += dL + NDS[nE].u; NDS[nE].u = 0.0;
				EMS[nE-1].L0 = EMS[nE-1].L = L_top + dL;
				EMS[nE-1].E = EMS[nE-1].Eps = EMS[nE-1].dEps = EMS[nE-1].Eps_e = EMS[nE-1].Eps_v = EMS[nE-1].S = 0.0;
				EMS[nE-1].theta[ICE] *= L_top/EMS[nE-1].L;
				EMS[nE-1].theta[ICE] += dM/(Constants::density_ice*EMS[nE-1].L);
				EMS[nE-1].theta[ICE] = std::max(0., std::min(1., EMS[nE-1].theta[ICE]));
				EMS[nE-1].theta[WATER] *= L_top/EMS[nE-1].L;
				EMS[nE-1].theta[WATER] = std::max(0., std::min(1., EMS[nE-1].theta[WATER]));
				EMS[nE-1].theta[WATER_PREF] *= L_top/EMS[nE-1].L;
				EMS[nE-1].theta[WATER_PREF] = std::max(0., std::min(1., EMS[nE-1].theta[WATER_PREF]));

				for (size_t ii = 0; ii < Xdata.number_of_solutes; ii++) {
					EMS[nE-1].conc[ICE][ii] *= L_top*theta_i0/(EMS[nE-1].theta[ICE]*EMS[nE-1].L);
				}

				EMS[nE-1].M += dM;
				assert(EMS[nE-1].M >= (-Constants::eps2)); //mass must be positive

				// Update remaining volumetric contents and density
				EMS[nE-1].theta[AIR] = std::max(0., 1.0 - EMS[nE-1].theta[WATER] - EMS[nE-1].theta[WATER_PREF] - EMS[nE-1].theta[ICE] - EMS[nE-1].theta[SOIL]);
				EMS[nE-1].updDensity();
			}
		}
	} else if ((ql < (-Constants::eps2)) && (nE > 0)) {
		// If ql < 0, SUBLIMATE mass off
		std::vector<double> M_Solutes(Xdata.number_of_solutes, 0.); // Mass of solutes from disappearing phases
		size_t e = nE;
		while ((e > 0) && (ql < (-Constants::eps2))) {  // While energy is available
			e--;
			/*
			* Determine the amount of potential sublimation and collect some variables
			* that will be continuously used: L0 and M
			*/
			const double L0 = EMS[e].L;
			const double theta_i0 = EMS[e].theta[ICE];
			M = theta_i0*Constants::density_ice*L0;
			dM = ql*sn_dt/Constants::lh_sublimation;
			if (-dM > M) {
				// Only if mass change is sufficient to remove the full element
				dM = -M;
				// Add solutes to Storage
				for (size_t ii = 0; ii < Xdata.number_of_solutes; ii++) {
					M_Solutes[ii] += EMS[e].conc[ICE][ii]*theta_i0*L0;
				}
				EMS[e].theta[ICE] = 0.;
				dL = 0.;

				EMS[e].M += dM;
				Sdata.mass[SurfaceFluxes::MS_SUBLIMATION] += dM;
				ql -= dM*Constants::lh_sublimation/sn_dt;     // Update the energy used

				// If present at surface, surface hoar is sublimated away
				if (e == nE-1) {
					dHoar = std::max(-NDS[nN-1].hoar, dM);
				}

				// Update remaining volumetric contents and density
				EMS[e].theta[AIR] = std::max(0., 1.0 - EMS[e].theta[WATER] - EMS[e].theta[WATER_PREF] - EMS[e].theta[ICE] - EMS[e].theta[SOIL]);
				EMS[e].updDensity();
				// Merge the element if it is a snow layer. This will take care of possible left over liquid water (will be put one layer down)
				// Keep layer if it is a soil layer inside the snowpack (for example with snow farming)
				if(e>=Xdata.SoilNode) {
					if(EMS[e].theta[SOIL]<Constants::eps) {
						if (e>0) SnowStation::mergeElements(EMS[e-1], EMS[e], false, true);
						// Now reduce the number of elements by one.
						nE--;
					}
					//In case e==Xdata.SoilNode, we removed the last snow element and we should break out of the loop.
					if(e==Xdata.SoilNode) break;
				}
			} else {
				// Not enough energy anymore to remove complete element, so we should break out of the loop.
				break;
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
		if (nE == Xdata.SoilNode) { // Add Solute Mass to Runoff TODO HACK CHECK
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
		Xdata.reduceNumberOfElements(nE);
	}

	// HACK: this code is under verification. The comment reads "surface hoar *is* destroyed, but the next line says surface hoar *may be* destroyed, depending on the sign of the latent heat flux.
	// If the code is correct, we can delete this part, if the comment is correct, we should modify the code to read: hoar = -NDS[nE].hoar;
	// Check for surface hoar destruction or formation (once upon a time ml_sn_SurfaceHoar)
	/*if ((Mdata.rh > hoar_thresh_rh) || (Mdata.vw > hoar_thresh_vw) || (Mdata.ta >= IOUtils::C_TO_K(hoar_thresh_ta))) {
		//if rh is very close to 1, vw too high or ta too high, surface hoar is destroyed
		hoar = std::min(hoar, 0.);
	}*/

	Sdata.hoar += dHoar;
	NDS[nN-1].hoar += dHoar;
	if (NDS[nN-1].hoar < 0.) {
		NDS[nN-1].hoar = 0.;
	}

	// Surface hoar cannot exist when the top element is wet
	if (nE > 0) {
		const double theta_r=((iwatertransportmodel_snow==RICHARDSEQUATION && nE-1>=Xdata.SoilNode) || (iwatertransportmodel_soil==RICHARDSEQUATION && nE-1<Xdata.SoilNode)) ? (PhaseChange::RE_theta_r) : (PhaseChange::theta_r);
		if (Xdata.Edata[nE-1].theta[WATER] > theta_r) {
			NDS[nE].hoar = 0.;
		}
	}

	// At the end also update the overall height
	cH_old = Xdata.cH;
	Xdata.cH = NDS[Xdata.getNumberOfNodes()-1].z + NDS[Xdata.getNumberOfNodes()-1].u;
	if (Xdata.mH!=Constants::undefined) Xdata.mH -= std::min(Xdata.mH - Xdata.Ground, (cH_old - Xdata.cH));	// TODO/HACK: why is this correction for Xdata.mH necessary?
}

/**
 * @brief Layer to layer vapour flux in both soil and snowpack
 * Calculates the difference in vapour flux between the snowpack elements and add/remove the corresponding mass
 * to the element ice lattice.
 * In soil the lowest element has a bottom flux equal to zero, the vapour fluxes in the rest of the soil are calculated according
 * to Saito et al., 2006 "Numerical analysis of coupled water, vapor, and heat transport in the vadose zone".
 * In snow, the formulation is similar to the one made in Metamorphism.cc for the vapour transfer at a microscopic level.
 * At the lowest snow element, the bottom flux is either equal to zero in case of no soil layer or to the uppermost soil flux if there is soil below.
 * At the uppermost element, the flux corresponds at the amount of atmospheric latent heat available for deposition/ sublimation.
 * @author Margaux Couttet
 * @param Xdata Nodes and elements data (temperature, temperature gradient, volumetric water/air/ice content)
 * @param Sdata Surface data (update the sublimated/deposited mass: surface flux MS_SUBLIMATION)
 * @param ql Latent heat flux (W m-2) from compSurfaceSublimation()
 */
void VapourTransport::LayerToLayer(SnowStation& Xdata, SurfaceFluxes& Sdata, double& ql) {
	const size_t nN = Xdata.getNumberOfNodes();
	const size_t nE = nN-1;
	vector<NodeData>& NDS = Xdata.Ndata;
	vector<ElementData>& EMS = Xdata.Edata;

	// For snow, update theta_s (i.e., pore space)
	if (nE > Xdata.SoilNode) {
		for (size_t el = Xdata.SoilNode; el < nE; el++) {
			EMS[el].VG.theta_s = (1. - EMS[el].theta[ICE])*(Constants::density_ice/Constants::density_water);	// TODO: link to van Genuchten parameterisation
		}
	}

	//Calculate layer2layer flux
	std::vector<double> deltaM(nE, 0.);
	size_t e = nE;

	double botFlux = 0.;	//bottom layer flux (kg m-2 s-1)
	//Going top-down through the model domain, the first estimate of the topFlux is the surface sublimation, we put it in the botFlux, so it will be assigned the topFlux inside the while-loop
	botFlux = -ql / Constants::lh_sublimation; //top flux when snowpack interacts with atmosphere, inverse sign with ql (upward vapour flux for sublimation/downward for deposition)
	ql = 0.; //Now that we used the remaining ql, put it to 0.

	if (enable_vapour_transport) {
		// Solve vapour transport in snow
		while (e-- > 0) {
			const double topFlux = botFlux;										//top layer flux (kg m-2 s-1)

			const double gradTbot = (e == 0) ? (0.) : .5 * (EMS[e-1].gradT + EMS[e].gradT);				//Temperature gradient at the upper node (K m-1)
			const double gradHbot = (e == 0) ? (0.) : (EMS[e].h - EMS[e-1].h) / (EMS[e].L/2. + EMS[e-1].L/2.);	//Pressure head gradient at the upper node (m m-1)

			const double clay_fraction = 0.2;							//Silty clay from Zhang et al., 2016
			double dM = 0.;										//mass change induced by vapor flux (kg m-2)

			if (e == 0) {
				botFlux = 0.;
			} else {
				if (e < Xdata.SoilNode) {
					//in soil
					botFlux = -(SnLaws::compSoilThermalVaporConductivity(EMS[e-1],EMS[e],EMS[e-1].Te,EMS[e].Te,clay_fraction)*Constants::density_water*gradTbot
						    + SnLaws::compSoilIsothermalVaporConductivity(EMS[e-1],EMS[e],EMS[e-1].Te,EMS[e].Te, NDS[e].T)*Constants::density_water*gradHbot);
				} else {
					//in snow
					botFlux = -Constants::diffusion_coefficient_in_snow/(Constants::gas_constant*NDS[e].T*NDS[e].T)
						  * (Constants::lh_sublimation/(Constants::gas_constant*NDS[e].T)-1.) * gradTbot;
					botFlux *= Atmosphere::vaporSaturationPressure(NDS[e].T);
				}
			}

			const double qL2L = -(topFlux - botFlux); //Layer to layer flux, (kg m-2 s-1)

			// Now, the mass change is limited by:
			// - we cannot remove more WATER and ICE than available
			// - we cannot add more WATER and ICE than pore space available
			dM = std::max(  -((EMS[e].theta[WATER] - EMS[e].VG.theta_r * (1. + Constants::eps)) * Constants::density_water * EMS[e].L + EMS[e].theta[ICE] * Constants::density_ice * EMS[e].L)  ,
				      std::min(  (EMS[e].theta[AIR] * Constants::density_ice * EMS[e].L), qL2L * sn_dt  )
				     ); // mass change due to difference in water vapor flux (kg m-2), at most can fill the pore space.

			// - we should not remove more ICE from the snow layer then just below the minimum ice content, such that the layer is removed in the next time step
			if (EMS[e].theta[ICE] > (1. - Constants::eps) * Snowpack::min_ice_content && EMS[e].theta[SOIL] < Constants::eps) {
				// Make sure elements don't get too light. By setting element ice content just below the threshold to merge, it will be merged in the next time step.
				dM = std::max(-Constants::density_ice * EMS[e].L * (EMS[e].theta[ICE] - (1. - Constants::eps) * Snowpack::min_ice_content), dM);
			}

			// If there is no pore space, or, in fact, only so much pore space to accomodate the larger volume occupied by ice when all water freezes,
			// we inhibit vapour flux. This is necessary to maintain saturated conditions when present, and this is in turn necessary for the stability in the Richards equation solver.
			if(EMS[e].theta[AIR] < EMS[e].theta[WATER]*(Constants::density_water/Constants::density_ice - 1.) + Constants::eps) {
				dM = 0.;
			}
			deltaM[e] += dM;

			// Correct botFlux if mass change was limited. Note that we cannot adapt topFlux anymore, as it was applied to the upper element already.
			botFlux += (deltaM[e] / sn_dt - qL2L);

			if (e == nE-1) Sdata.mass[SurfaceFluxes::MS_SUBLIMATION] -= topFlux*sn_dt;			//update surface flux (minus when the flux is leaving the snowpack)
			if (e == Xdata.SoilNode) Sdata.mass[SurfaceFluxes::MS_SNOWPACK_RUNOFF] -= botFlux*sn_dt;	//update mass loss of snowpack due to water transport
		}
	} else {
		// Only deal with the remaining ql (i.e., latent heat exchange at the surface)
		const double topFlux = botFlux;										//top layer flux (kg m-2 s-1)
		deltaM[nE-1] += std::max(-EMS[nE-1].theta[ICE] * (Constants::density_ice * EMS[nE-1].L), -(topFlux * sn_dt));
		// HACK: note that if we cannot satisfy the ql at this point, we overestimated the latent heat from soil.
		// We will not get mass from deeper layers, as to do that, one should work with enable_vapour_transport == true.
		Sdata.mass[SurfaceFluxes::MS_SUBLIMATION] -= topFlux * sn_dt;
	}

	double dHoar = 0.;
	for (e = 0; e < nE; e++) {
		EMS[e].M += deltaM[e];
		assert(EMS[e].M >= (-Constants::eps2)); //mass must be positive

		if (deltaM[e] < 0.) {
			// Mass loss: apply mass change first to water, then to ice, based on energy considerations
			// We can only do this partitioning here in this "simple" way, without checking if the mass is available, because we already limited dM above, based on available ICE + WATER.
			const double dTh_water = std::max( (EMS[e].VG.theta_r * (1. + Constants::eps) - EMS[e].theta[WATER])  ,  deltaM[e] / (Constants::density_water * EMS[e].L) );
			const double dTh_ice = ( deltaM[e] - (dTh_water * Constants::density_water * EMS[e].L) ) / (Constants::density_ice * EMS[e].L);
			EMS[e].theta[WATER] += dTh_water;
			EMS[e].theta[ICE] += dTh_ice;

			// If present at surface, surface hoar is sublimated away
			if (e == nE-1 && deltaM[e]<0) {
				dHoar = std::max(-NDS[nN-1].hoar, deltaM[e]);
			}
		} else {		// Mass gain: add water in case temperature at or above melting point, ice otherwise
			if (EMS[e].Te >= EMS[e].meltfreeze_tk) {
				EMS[e].theta[WATER] += deltaM[e] / (Constants::density_water * EMS[e].L);
			} else {
				if (e == nE-1 && e >= Xdata.SoilNode) {
					// The top layer will increase length due to deposition
					const double dL = deltaM[e] / (Constants::density_ice * EMS[e].theta[ICE]);
					const double L_old = EMS[e].L;
					const double L_new = EMS[e].L + dL;
					EMS[e].L = L_new;
					EMS[e].theta[WATER] *= L_old / L_new;
					EMS[e].theta[WATER_PREF] *= L_old / L_new;
					NDS[e+1].z = NDS[e].z + EMS[e].L;
					Xdata.cH = NDS[e+1].z + NDS[e+1].u;
				} else {
					EMS[e].theta[ICE] += deltaM[e] / (Constants::density_ice * EMS[e].L);
				}
			}
		}
		// Numerical rounding errors were found to lead to theta[AIR] < 0, so force the other components between [0,1]:
		EMS[e].theta[ICE] = std::max(0., std::min(1. - EMS[e].theta[SOIL], EMS[e].theta[ICE]));
		EMS[e].theta[WATER] = std::max(0., std::min(1. - EMS[e].theta[SOIL], EMS[e].theta[WATER]));
		EMS[e].theta[WATER_PREF] = std::max(0., std::min(1., EMS[e].theta[WATER_PREF]));
		// Update theta[AIR] and density:
		EMS[e].theta[AIR] = (1. - EMS[e].theta[WATER] - EMS[e].theta[WATER_PREF] - EMS[e].theta[ICE] - EMS[e].theta[SOIL]);
		EMS[e].updDensity();
		assert(EMS[e].Rho > 0 || EMS[e].Rho==IOUtils::nodata); //density must be positive

		if (!(EMS[e].Rho > Constants::eps && EMS[e].theta[AIR] >= 0.)) {
			if(EMS[e].theta[AIR] > -Constants::eps2) {
				EMS[e].theta[AIR] = 0.;
			} else {
				prn_msg(__FILE__, __LINE__, "err", Date(),
				    "Volume contents: e=%d nE=%d rho=%lf ice=%lf wat=%lf wat_pref=%le air=%le  soil=%le", e, nE, EMS[e].Rho, EMS[e].theta[ICE], EMS[e].theta[WATER], EMS[e].theta[WATER_PREF], EMS[e].theta[AIR], EMS[e].salinity);
				throw IOException("Cannot evaluate mass balance in vapour transport LayerToLayer routine", AT);
			}
		}
	}

	Sdata.hoar += dHoar;
	NDS[nN-1].hoar += dHoar;
	if (NDS[nN-1].hoar < 0.) {
		NDS[nN-1].hoar = 0.;
	}
}

void VapourTransport::compTransportMass(const CurrentMeteo& Mdata, double& ql,
                                       SnowStation& Xdata, SurfaceFluxes& Sdata)
{
	// First, consider no soil with no snow on the ground
	if (!useSoilLayers && Xdata.getNumberOfNodes() == Xdata.SoilNode+1) {
		return;
	}

	compSurfaceSublimation(Mdata, ql, Xdata, Sdata);

	// No snow (sublimation removed last snow element)
	if (Xdata.getNumberOfNodes() == Xdata.SoilNode+1) return;

	try {
		WaterTransport::adjustDensity(Xdata);
		LayerToLayer(Xdata, Sdata, ql);
	} catch(const exception&){
		prn_msg( __FILE__, __LINE__, "err", Mdata.date, "Error in transportVapourMass()");
		throw;
	}
}

/*
 * End of VapourTransport.cc
 */
