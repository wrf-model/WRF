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

#include <snowpack/snowpackCore/PhaseChange.h>
#include <snowpack/snowpackCore/ReSolver1d.h>
#include <snowpack/Constants.h>
#include <snowpack/Utils.h>

using namespace mio;
using namespace std;

/************************************************************
 * static section                                           *
 ************************************************************/

/*
 * Residual Water Content,  for now we say  0.0
 * - This constant is used in the PHASE CHANGE ROUTINES; moreover we will freeze
 *   ALL water in the snowpack.  This ensures that we can have DRY snow. (Perryanic comment!)
 */
const double PhaseChange::theta_r = 0.0;
const double PhaseChange::RE_theta_r = 1E-5/10.;		// Minimum amount of liquid water that will remain. It is recommended that this value is at least smaller than PhaseChange::RE_theta_threshold (see ReSolver1d.cc)
const double PhaseChange::RE_theta_threshold = 1E-5; 		// Above this threshold, the element is considered in melting of freezing state. It is recommended that this value is REQUIRED_ACCURACY_THETA (see ReSolver1d.cc)

//Saturated Water Content, for now we say 1.0
const double PhaseChange::theta_s = 1.0;

/************************************************************
 * non-static section                                       *
 ************************************************************/

static bool get_bool(const SnowpackConfig& cfg, const std::string& key, const std::string& section)
{
	bool value;
	cfg.getValue(key, section, value);
	return value;
}

static double get_double(const SnowpackConfig& cfg, const std::string& key, const std::string& section)
{
	double value;
	cfg.getValue(key, section, value);
	return value;
}

static double get_sn_dt(const SnowpackConfig& cfg) 
{
	//Calculation time step in seconds as derived from CALCULATION_STEP_LENGTH
	const double calculation_step_length = cfg.get("CALCULATION_STEP_LENGTH", "Snowpack");
	return M_TO_S(calculation_step_length);
}

PhaseChange::PhaseChange(const SnowpackConfig& cfg)
             : iwatertransportmodel_snow(BUCKET), iwatertransportmodel_soil(BUCKET),
               watertransportmodel_snow("BUCKET"), watertransportmodel_soil("BUCKET"),
               forcing("ATMOS"),
               sn_dt( get_sn_dt(cfg) ), cold_content_in(IOUtils::nodata), cold_content_soil_in(IOUtils::nodata),
               cold_content_out(IOUtils::nodata), cold_content_soil_out(IOUtils::nodata),
	       alpine3d( get_bool(cfg, "ALPINE3D", "SnowpackAdvanced") ), t_crazy_min( get_double(cfg, "T_CRAZY_MIN", "SnowpackAdvanced") ), t_crazy_max( get_double(cfg, "T_CRAZY_MAX", "SnowpackAdvanced") ), max_theta_ice(1.)
{
	//Water transport model snow
	cfg.getValue("WATERTRANSPORTMODEL_SNOW", "SnowpackAdvanced", watertransportmodel_snow);
	max_theta_ice=1.;
	if (watertransportmodel_snow=="BUCKET") {
		iwatertransportmodel_snow=BUCKET;
	} else if (watertransportmodel_snow=="NIED") {
		iwatertransportmodel_snow=NIED;
	} else if (watertransportmodel_snow=="RICHARDSEQUATION") {
		iwatertransportmodel_snow=RICHARDSEQUATION;
		max_theta_ice=ReSolver1d::max_theta_ice;
	}

	//Water transport model soil
	cfg.getValue("WATERTRANSPORTMODEL_SOIL", "SnowpackAdvanced", watertransportmodel_soil);
	if (watertransportmodel_soil=="BUCKET") {
		iwatertransportmodel_soil=BUCKET;
	} else if (watertransportmodel_soil=="NIED") {
		iwatertransportmodel_soil=NIED;
	} else if (watertransportmodel_soil=="RICHARDSEQUATION") {
		iwatertransportmodel_soil=RICHARDSEQUATION;
	}

	cfg.getValue("FORCING", "Snowpack", forcing);
}

void PhaseChange::reset()
{
	cold_content_in = cold_content_soil_in = cold_content_out = cold_content_soil_out = IOUtils::nodata;
}

/**
 * @brief Subsurface Melting:
 * -# Consistency check.
 * -# Temperature ice and water check: If the  element temperature is below melting
 * temperature, or if the volumetric ice content is nonpositive, or if the water content
 * equals or exceeds PhaseChange::theta_s, no melting occurs.
 * If the check is ok,  then the  difference dT between element and melting temperature
 * is computed.
 * -# The coefficient A is computed.
 * -# Analogous to Subsurface Freezing ...
 * -# Check if there is enough ice to be melted: If it is not the case, the values have to
 * be corrected.
 * -# If there is  TOO MUCH  water being melted  then d_th(w) ( = PhaseChange::theta_s - th(w) ) as welL
 * as the changes d_th(i) and dT are corrected.
 * -# Computation of contents.
 * -# Characterization of element ( Liquid, Dry, Void ).
 * -# Computation of densities and water generation rate.
 * @param *Edata
 * @param dt Time step (s)
 * @param *ql_Rest Latent heat flux balance (J m-2)
 */
void PhaseChange::compSubSurfaceMelt(ElementData& Edata, const unsigned int nSolutes, const double& dt,
                                     double& ql_Rest, const mio::Date& date_in, double& mass_melt)
{
	const double T_melt=Edata.meltfreeze_tk;		// Retrieve melting temperature from ElementData

	if(!Edata.checkVolContent()) prn_msg(__FILE__, __LINE__, "wrn", Date(), "wrong volumetric content");
	/*
	 * Now see if any melting is going on -- this implies that (1) the temperature of the element
	 * is above the melting temperature (2) there is something to melt and (3) there is enough
	 * room to place the meltwater ...
	 */
	if (((Edata.Te < T_melt) && (ql_Rest < Constants::eps2) && (forcing=="ATMOS"))
	        || (Edata.theta[ICE] <= 0.0) || (Edata.theta[WATER] >= PhaseChange::theta_s)) {
		return; // no melt with atmos forcing
	} else if (((mass_melt < Constants::eps2) && (forcing=="MASSBAL"))
	        || (Edata.theta[ICE] <= 0.0) || (Edata.theta[WATER] >= PhaseChange::theta_s)) {
		Edata.Te = std::min(Edata.Te, T_melt);	// Considering the case that the element is above melting temperature, but mass_melt==0, so we are not "allowed" to apply melt.
		return; // no melt with massbal forcing
	} else {
		double dth_i;
		double dth_w;
		if (forcing == "MASSBAL" && T_melt > Edata.Te) { // forced melt, when "normal" melt would not occur anymore (snowpack too cold)
			mass_melt += ql_Rest / Constants::lh_fusion;
			dth_i = - (mass_melt / (Constants::density_ice * Edata.L)); // dth_i must be negative defined !
			dth_w = - (Constants::density_ice / Constants::density_water) * dth_i; // change in volumetric water content
			// You can only melt so much ice as is there ....
			if ( (Edata.theta[ICE] + dth_i) < 0.0 ) {
				dth_i = - Edata.theta[ICE];
				dth_w = - (Constants::density_ice / Constants::density_water) * dth_i;
			}
			// It could also be that you are trying to produce more water than is allowed.
			if ( (Edata.theta[WATER] + dth_w) > PhaseChange::theta_s ) {
				dth_w = PhaseChange::theta_s - Edata.theta[WATER];
				dth_i = - (Constants::density_water / Constants::density_ice) * dth_w;
			}
			// Reset element properties
			ql_Rest = 0.0;
			Edata.Te = Edata.meltfreeze_tk;
		} else { // temperature induced ("normal") melt
			double dT = T_melt - Edata.Te; // Edata.meltfreeze_tk - Te > 0
			// Now we take into account that there might be some extra energy that could not
			// be used by the element above because of complete melting
			dT -= ql_Rest / (Edata.c[TEMPERATURE] * Edata.Rho * Edata.L);
			if (dT > 0.) {
				return;
			}
			// Determine the DECREASE in ice content and the INCREASE of water content
			// Adapt A to compute mass changes
			const double A = (Edata.c[TEMPERATURE] * Edata.Rho) / ( Constants::density_ice * Constants::lh_fusion );
			dth_i = A * dT; // change in volumetric ice content
			dth_w = - (Constants::density_ice / Constants::density_water) * dth_i; // change in volumetric water content
			// It could happen that there is enough energy available to melt more ice than is present.
			// You can only melt so much ice as is there ....
			if ( (Edata.theta[ICE] + dth_i) < 0.0 ) {
				dth_i = - Edata.theta[ICE];
				dth_w = - (Constants::density_ice / Constants::density_water) * dth_i;
				dT = dth_i / A;
			}
			// It could also be that you are trying to produce more water than is allowed.
			if ( (Edata.theta[WATER] + dth_w) > PhaseChange::theta_s ) {
				dth_w = PhaseChange::theta_s - Edata.theta[WATER];
				dth_i = - (Constants::density_water / Constants::density_ice) * dth_w;
				dT = dth_i / A;
			}
			// Treat the case for MASSBAL forcing where the melt in the element exceeds the prescribed melt
			if (forcing == "MASSBAL" && dth_i < -(mass_melt / (Constants::density_ice * Edata.L))) {
				dth_i = -(mass_melt / (Constants::density_ice * Edata.L));
				dth_w = - (Constants::density_ice / Constants::density_water) * dth_i;
				dT = dth_i / A;
			}
			// Reset element properties
			Edata.Te += dT;
			if (Edata.Te <= T_melt) {
				ql_Rest = 0.0;
				Edata.Te = T_melt;
			} else {
				ql_Rest = Edata.c[TEMPERATURE] * Edata.Rho * Edata.L * (Edata.Te - T_melt);
				Edata.Te = T_melt;
			}
		}
		mass_melt += (dth_i * Constants::density_ice * Edata.L); // update mass_melt (remove mass that was melted in the current layer)
		Edata.Qmf += (dth_i * Constants::density_ice * Constants::lh_fusion) / dt; // (W m-3)
		Edata.dth_w += dth_w; // (1)
		for (unsigned int ii = 0; ii < nSolutes; ii++) {
			if (dth_w > 0. ) {
				Edata.conc[WATER][ii] = (Edata.theta[WATER] * Edata.conc[WATER][ii]
				    + dth_w * Edata.conc[ICE][ii]) / (Edata.theta[WATER] + dth_w);
			}
		}
		Edata.theta[ICE] += dth_i;
		Edata.theta[WATER] += dth_w;
		Edata.theta[AIR] = (1. - Edata.theta[ICE] - Edata.theta[WATER] - Edata.theta[WATER_PREF] - Edata.theta[SOIL]);

		// Make sure the sum of all volumetric contents is near 1, and take care of rounding errors
		if (!Edata.checkVolContent()) {
			prn_msg(__FILE__, __LINE__, "err", date_in, "Sum theta[I,W,A,S] > 1");
			prn_msg(__FILE__, __LINE__, "msg-", Date(),
			        "Ice: %f, Water: %f, Water_pref: %f, Air: %f Soil: %f",
			        Edata.theta[ICE], Edata.theta[WATER], Edata.theta[WATER_PREF], Edata.theta[AIR], Edata.theta[SOIL]);
			throw IOException("In compSubSurfaceMelt!", AT);
		}
		Edata.updDensity();
		Edata.heatCapacity();
	}
}

/**
 * @brief Subsurface Freezing:
 * -# Check whether the conditions
 *    Sum(th(x)) = 1 [sum of volumetric contents soil, ice, water, and void] \n
 *    and \n
 *    rho <= th(s) * rho(s) + ( 1 - th(s) ) * rho(i) = rhomax \n
 *    with th(s), rho(s) volumetric content and density of soil, rho element density
 *    and rhomax maximal possible element density are fulfilled;
 *    if not return with an error code (violation  of the consistency requirements).
 * -# Temperature and water check: If the  element temperature is equal to or above
 *    freezing temperature, or if the volumetric water content is less or equal to the
 *    residual water content, no subsurface  freezing takes place. \n
 *    If the temperature and water check is ok, then the difference dT between actual
 *    element and freezing temperature is evaluated.
 * -# The coefficient A is computed.
 * -# The changes d th(i) and d th(w) in the volumetric ice and water content are computed.
 * -# If there is  NOT  enough water to perform the computed d th(w) then
 *    d_th(w) ( = th(w) ) as well as the changes d_th(i) and dT are corrected.
 * -# Check whether the element is frozen solid and edit a message.  Computation of the
 *    the corrected changes and contents is then performed:
 *    -# volumetric contents ( soil, ice, water, air )
 *    -# dry density
 *    -# element density
 *    -# fusion power.
 *    -# water loss rate
 * -# Again the consistency requirement of (1) is checked.
 * @param *Edata
 * @param dt Time step (s)
 */
void PhaseChange::compSubSurfaceFrze(ElementData& Edata, const unsigned int nSolutes, const double& dt,
                                     const mio::Date& date_in)
{
	const double T_freeze=Edata.meltfreeze_tk;	// Retrieve melting temperature from ElementData

	if(!Edata.checkVolContent()) prn_msg(__FILE__, __LINE__, "wrn", Date(), "wrong volumetric content");
	/*
	 * Freezing within the snowpack can occur if (1) the temperature of the element is below freezing
	 * and if water is present to be refrozen
	 */
	const double cmp_theta_r=((iwatertransportmodel_snow==RICHARDSEQUATION && Edata.theta[SOIL]<Constants::eps) || (iwatertransportmodel_soil==RICHARDSEQUATION && Edata.theta[SOIL]>Constants::eps)) ? (PhaseChange::RE_theta_r) : (PhaseChange::theta_r);
	if ((Edata.Te >= T_freeze) || (Edata.theta[WATER] <= cmp_theta_r)) {
		return;
	} else {
		double dT = T_freeze - Edata.Te;
		// Adapt A to compute mass changes
		double A = (Edata.c[TEMPERATURE] * Edata.Rho) / ( Constants::density_ice * Constants::lh_fusion);
		// Compute the change in volumetric ice and water contents
		double dth_i = A * dT;
		double dth_w = - (Constants::density_ice / Constants::density_water) * dth_i;
		// Make sure that there is enough water to refreeze
		if ((Edata.theta[WATER] + dth_w) < cmp_theta_r) {
			dth_w = std::min(0., - ( Edata.theta[WATER] - cmp_theta_r ));
			dth_i = - (Constants::density_water / Constants::density_ice) * dth_w;
		}
		// See if the element is pure ICE
		if (Edata.theta[ICE] + dth_i >= max_theta_ice) {
			dth_i = std::max(0., max_theta_ice - Edata.theta[ICE]);
			dth_w = - dth_i * (Constants::density_ice / Constants::density_water);
		} else {
			// Concentration of solutes
			for (unsigned int ii = 0; ii < nSolutes; ii++) {
				if (dth_i > 0.) {
					Edata.conc[ICE][ii] = (Edata.theta[ICE] * Edata.conc[ICE][ii] +
					                           dth_i * Edata.conc[WATER][ii]) / ( Edata.theta[ICE] + dth_i);
				}
			}
		}
		Edata.theta[ICE] += dth_i;
		Edata.theta[WATER] += dth_w;
		Edata.theta[AIR] = (1. - Edata.theta[ICE] - Edata.theta[WATER] - Edata.theta[WATER_PREF] - Edata.theta[SOIL]);

		// Make sure the sum of all volumetric contents is near 1, and take care of rounding errors
		if (!Edata.checkVolContent()) {
			prn_msg(__FILE__, __LINE__, "err", date_in, "Sum theta[I,W,A,S] > 1");
			prn_msg(__FILE__, __LINE__, "msg-", Date(),
			        "Ice: %f, Water: %f, Water_pref: %f, Air: %f Soil: %f",
			        Edata.theta[ICE], Edata.theta[WATER], Edata.theta[WATER_PREF], Edata.theta[AIR], Edata.theta[SOIL]);
			throw IOException("In compSubSurfaceFrze!", AT);
		}
		dT = dth_i / A;	// Recalculate temperature change, as phase change may be limited
		Edata.updDensity();
		Edata.heatCapacity();
		// Compute the volumetric refreezing power
		Edata.Qmf += (dth_i * Constants::density_ice * Constants::lh_fusion) / dt; // (W m-3)
		Edata.dth_w += dth_w;
		Edata.Te += dT;
	}
}




void PhaseChange::initialize(SnowStation& Xdata)
{
	// Initialize PhaseChange: execute this function before doing any call to PhaseChange::compPhaseChange for the current time step, to reset the energy balance values.
	size_t e, nE;
	ElementData* EMS;
	nE = Xdata.getNumberOfElements(); EMS = &Xdata.Edata[0];

	// Initialize and Determine Energy Content
	for (e = 0; e < nE; e++) {
		EMS[e].dth_w = EMS[e].Qmf = 0.;
	}

	// Get cold content
	cold_content_in=Xdata.ColdContent;
	cold_content_soil_in=Xdata.ColdContentSoil;

	// Reset meltFreezeEnergy and dIntEnergy
	Xdata.meltFreezeEnergy=0.;
	Xdata.meltFreezeEnergySoil=0.;
	Xdata.dIntEnergy=0.;
	Xdata.dIntEnergySoil=0.;

	// Reset melt and refreeze mass
	Xdata.meltMassTot = 0.;
	Xdata.refreezeMassTot = 0.;

	return;
}



void PhaseChange::finalize(const SurfaceFluxes& Sdata, SnowStation& Xdata, const mio::Date& date_in)
{
	// After all PhaseChange::compPhaseChange calls for the current time step, execute this function to finalize temperature structure calculations and energy balance values.
	size_t e, nE;
	double sum_Qmf=0.;
	cold_content_out=0.;
	cold_content_soil_out=0.;

	ElementData* EMS;
	bool prn_CK = false;
	nE = Xdata.getNumberOfElements(); EMS = &Xdata.Edata[0]; vector<NodeData>& NDS = Xdata.Ndata;

	try {
		// In the final step compute temperature and temperature gradient, check both density and mass balance
		for (e = 0; e < nE; e++) {
			//Restructure temperature arrays
                        EMS[e].gradT = (NDS[e+1].T - NDS[e].T) / EMS[e].L;
		        EMS[e].Te = (NDS[e].T + NDS[e+1].T) / 2.0;
			//if (((EMS[e].Te - EMS[e].meltfreeze_tk) > 0.2) && EMS[e].theta[ICE]>0.) //handle the case of soil layers above ice/snow layers
			//	prn_msg(__FILE__, __LINE__, "wrn", date_in,
			//	        "%s temperature Te=%f K is above melting point (%f K) in element %d (nE=%d; T0=%f K, T1=%f K, theta_ice=%f)",
			//	        (e < Xdata.SoilNode) ? ("Soil") : ("Snow"), EMS[e].Te, EMS[e].meltfreeze_tk, e, nE, NDS[e].T, NDS[e+1].T, EMS[e].theta[ICE]);
			// Verify element state against maximum possible density: only water
			if (!(EMS[e].Rho > Constants::eps && EMS[e].Rho <= (1.-EMS[e].theta[SOIL])*Constants::density_water + (EMS[e].theta[SOIL] * EMS[e].soil[SOIL_RHO]))) {
				prn_msg(__FILE__, __LINE__, "err", date_in, "Phase Change End: volume contents: e:%d nE:%d rho:%lf ice:%lf wat:%lf wat_pref:%lf air:%le",
									    e, nE, EMS[e].Rho, EMS[e].theta[ICE], EMS[e].theta[WATER], EMS[e].theta[WATER_PREF], EMS[e].theta[AIR]);
				throw IOException("Run-time error in compPhaseChange()", AT);
			}
			if (e>=Xdata.SoilNode) {
				// Snow element
				cold_content_out += EMS[e].c[TEMPERATURE] * EMS[e].Rho * (EMS[e].Te - EMS[e].meltfreeze_tk) * EMS[e].L;
				sum_Qmf += EMS[e].Qmf * EMS[e].L;
			} else {
				// Soil element
				cold_content_soil_out += EMS[e].c[TEMPERATURE] * EMS[e].Rho * (EMS[e].Te) * EMS[e].L;
			}
		}
		if (prn_CK && (sum_Qmf > 0.)) {
			prn_msg(__FILE__, __LINE__, "msg+", date_in, "Checking energy balance  (W/m2):");
			prn_msg(__FILE__, __LINE__, "msg+", date_in, " E1: %f   E0: %f  E1-E0: %f  sum_Qmf: %f  Surface EB : %f",
			           (cold_content_out) / sn_dt, (cold_content_in) / sn_dt,
			               (cold_content_out - cold_content_in) / sn_dt, sum_Qmf,
			                   Sdata.qs + Sdata.ql + Sdata.lw_net + Sdata.qr + Sdata.qw);
		}
	} catch (const exception& ) {
		throw;
	}

	return;
}



/**
 * @brief Driving routine for subsurface melting and refreezing as well as surface melting.
 * The basic equation in both subsurface processes is: \n
 * d_th(i) = A * dt \n
 * with th(i) volumetric ice content (1), c_p(T) specific heat capacity of ice (J kg-1 K-1),
 * Q_f the freezing / melting energy (J kg-1), T the absolute temperature (K),
 * and the coefficient: \n
 * A = c_p(T) * th(i) * Q_f \n
 * ql_Rest is the Energy that is transferred from the upper element to the lower one (J m-2)
 * in case of complete melting of the former
 * @param Xdata
 * @param date_in is the current date
 * @param verbose print detailed warnings for various situations? (default=true)
 */
double PhaseChange::compPhaseChange(SnowStation& Xdata, const mio::Date& date_in, const bool& verbose, const double& surf_melt)
{
	size_t e, nE;
	double ql_Rest;
	ElementData* EMS;
	nE = Xdata.getNumberOfElements(); EMS = &Xdata.Edata[0]; vector<NodeData>& NDS = Xdata.Ndata;
	double retTopNodeT=NDS[nE].T;

	// Backup the nodal temperatures, in order to reconstruct an energy conservative temperature array during phase changes
	e = nE;
	std::vector<double> tmp_N_T_up(nE, 0.);
	std::vector<double> tmp_N_T_down(nE, 0.);
	while (e > 0) {
		e--;
		tmp_N_T_up[e]=NDS[e+1].T;
		tmp_N_T_down[e]=NDS[e].T;
	}

	// Execute phase changes
	double mass_melt = surf_melt; // variable surf_melt is a constant
	try {
		ql_Rest = 0.;
		e = nE;
		while (e > 0) {
			e--;
			// Verify element state against maximum possible density: only water
			if (!(EMS[e].Rho > Constants::eps && EMS[e].Rho <= (1.-EMS[e].theta[SOIL])*Constants::density_water + (EMS[e].theta[SOIL] * EMS[e].soil[SOIL_RHO]))) {
				prn_msg(__FILE__, __LINE__, "err", date_in, "Phase Change Begin: volume contents: e:%d nE:%d rho:%lf ice:%lf wat:%lf wat_pref:%lf air:%le", e, EMS[e].Rho,
									    e, nE, EMS[e].Rho, EMS[e].theta[ICE], EMS[e].theta[WATER], EMS[e].theta[WATER_PREF], EMS[e].theta[AIR]);
				throw IOException("Run-time error in compPhaseChange()", AT);
			}
			// and make sure the sum of all volumetric contents is near 1 (Can make a 1% error)
			if (verbose && !EMS[e].checkVolContent()) {
				prn_msg(__FILE__, __LINE__, "msg+", date_in,
				        "Phase Change Begin: Element=%d, nE=%d  ICE %f, Water %f, Water_pref %f, Air %f, Soil %f",
				        e, nE, EMS[e].theta[ICE], EMS[e].theta[WATER], EMS[e].theta[WATER_PREF], EMS[e].theta[AIR], EMS[e].theta[SOIL]);
			}

			double i_Te = EMS[e].Te;
			// Determine whether a layer can be considered dry or not.
			const double cmp_theta = ((iwatertransportmodel_snow==RICHARDSEQUATION && EMS[e].theta[SOIL]<Constants::eps) || (iwatertransportmodel_soil==RICHARDSEQUATION && EMS[e].theta[SOIL]>Constants::eps)) ? (PhaseChange::RE_theta_threshold) : (PhaseChange::theta_r);

			const bool MoistLayer = (EMS[e].theta[WATER] > cmp_theta + Constants::eps && EMS[e].theta[ICE] < max_theta_ice) ? true : false;
			if(MoistLayer==true && e==nE-1 && nE>Xdata.SoilNode) retTopNodeT=EMS[nE-1].meltfreeze_tk;

			// Try melting
			try {
				if(!(iwatertransportmodel_soil==RICHARDSEQUATION && e<Xdata.SoilNode)) {
					compSubSurfaceMelt(EMS[e], Xdata.number_of_solutes, sn_dt, ql_Rest, date_in, mass_melt);
				}
			} catch(...) {
				prn_msg(__FILE__, __LINE__, "msg-", Date(), "in compSubSurfaceMelt at element %d of %d", e, nE);
				throw IOException("Run-time error in compPhaseChange()", AT);
			}
			// See whether hoar has melted
			if (EMS[e].theta[WATER] > cmp_theta + Constants::eps) {
				NDS[e].hoar = 0.;
				NDS[e+1].hoar = 0.;
			}

			// Try freezing
			try {
				if(!(iwatertransportmodel_soil==RICHARDSEQUATION && e<Xdata.SoilNode) &&
				   !(iwatertransportmodel_soil==RICHARDSEQUATION && e==Xdata.SoilNode && EMS[e].mk%100==19) &&
				   !(iwatertransportmodel_snow==RICHARDSEQUATION && e==nE-1           && EMS[e].mk%100==19)) {
					// For Richards Equation, phase changes in soil are taken care of in WaterTransport.
					// Furthermore, in case we use Richards Equation, we suppress phase changes in potential water layers on the top. As a water layer is 100% water, we cannot freeze it, as it is not programmed that the element length should increase in such cases HACK/TODO: improve upon this in case someone wants it.
					compSubSurfaceFrze(EMS[e], Xdata.number_of_solutes, sn_dt, date_in);
				}
			} catch(...) {
				prn_msg(__FILE__, __LINE__, "msg-", date_in, "SubSurfaceFrze at element %d of %d", e, nE);
				throw IOException("Run-time error in compPhaseChange()", AT);
			}

			// Make sure all nodal temperatures are consistent with the temperature change of the element
			if (e < Xdata.SoilNode && iwatertransportmodel_soil == RICHARDSEQUATION) {
				// In case we use Richards equation for soil, phase transisitions are calculated there. The solver for Richards equation adjusts the element temperatures
				// and stores the heat associated with the phase transitions in Qmf. Here, we use Qmf to calculate i_Te, and adjust the nodes here.
				i_Te = EMS[e].Te;
				EMS[e].Te += ( (EMS[e].Qmf * sn_dt) / (EMS[e].c[TEMPERATURE] * EMS[e].Rho));
			}
			if ( e >= Xdata.SoilNode || iwatertransportmodel_soil != RICHARDSEQUATION ) {
				// Check if phase change did occur
				// Note: the MoistLayer==true is mainly there for the Richards equation for snow, where there is always some amount of liquid water
				// present in a layer, so we should additionally check if the phase change was significant, or the phase change involved snow melt.
				if ((EMS[e].Te != i_Te && (iwatertransportmodel_snow != RICHARDSEQUATION || EMS[e].Te < i_Te || (MoistLayer == true && e >= Xdata.SoilNode)))) {
					// Adjust nodal temperatures based on change in element temperature
					if(e==nE-1) {
						// The top node is our starting point and treated special:

						// First estimate is a change in nodal temperatures equal to the change in element temperatures
						NDS[e+1].T += (EMS[e].Te - i_Te);
						NDS[e].T += (EMS[e].Te - i_Te);

						// Now check the nodal temperatures against the state of the element
						if(EMS[e].theta[WATER] > cmp_theta + Constants::eps && EMS[e].theta[ICE] < max_theta_ice) {
							// If there is water, nodal temperatures cannot exceed melting temperature
							NDS[e].T=std::max(NDS[e].T, EMS[e].meltfreeze_tk);
							NDS[e+1].T=std::max(NDS[e+1].T, EMS[e].meltfreeze_tk);
						}
						if(EMS[e].theta[ICE] > Constants::eps) {
							// If there is ice, nodal temperatures cannot exceed freezing temperature
							NDS[e].T=std::min(NDS[e].T, EMS[e].meltfreeze_tk);
							NDS[e+1].T=std::min(NDS[e+1].T, EMS[e].meltfreeze_tk);
						}

						// We now bring the nodal temperatures in agreement with the element temperature
						NDS[e+1].T=2.*EMS[e].Te-NDS[e].T;

						// If the element is in melting or freezing conditions (meaning that phase transition is incomplete due to limited energy available)
						// both nodes should be at melting or freezing temperature. We judge this by the presence of ice in melting conditions, or the presence of water in freezing conditions.
						if(((EMS[e].theta[ICE] > Constants::eps && EMS[e].Te < i_Te) || (EMS[e].theta[WATER] > cmp_theta + Constants::eps && EMS[e].Te > i_Te)) && EMS[e].theta[ICE] < max_theta_ice) {
							NDS[e+1].T = NDS[e].T = (EMS[e].Te < i_Te) ? EMS[e].meltfreeze_tk : EMS[e].meltfreeze_tk;
						}

						// Now that we adjust the nodal temperatures, correct the adjacent nodal temperatures, such that the energy content remains constant
						if(e>1) {
							NDS[e-1].T+=(EMS[e-1].c[TEMPERATURE]*EMS[e-1].Rho*EMS[e-1].L)/(EMS[e-1].c[TEMPERATURE]*EMS[e-1].Rho*EMS[e-1].L + EMS[e-2].c[TEMPERATURE]*EMS[e-2].Rho*EMS[e-2].L)*(tmp_N_T_down[e]-NDS[e].T);
						} else {
							if(e==1) NDS[e-1].T=2.*EMS[e-1].Te-NDS[e].T;
						}
					} else {
						// For nodes other than the top node:

						// Now make the nodal temperatures reflect the state of the element they represent. Either the element is in phase transition (if-block),
						// or the phase transition is either complete or not occurring (else-block),
						if(((EMS[e].theta[ICE] > Constants::eps2 && EMS[e].Te < i_Te) || (EMS[e].theta[WATER] > cmp_theta + Constants::eps && EMS[e].Te > i_Te)) && EMS[e].theta[ICE] < max_theta_ice) {
							// Ice present in melting conditions or water present in freezing conditions? Incomplete phase transition due to limited energy availability.

							// Backup current nodal temperatures, to reconstruct an energy conservative temperature array
							tmp_N_T_up[e] = NDS[e+1].T;

							// Make nodes in agreement with melting or freezing conditions
							NDS[e].T = NDS[e+1].T = (EMS[e].Te < i_Te) ? EMS[e].meltfreeze_tk : EMS[e].meltfreeze_tk;

							// Now that we adjust the nodal temperatures, correct the adjacent nodal temperatures, such that the energy content remains constant
							// First the node above
							if(e<nE-2) {
								NDS[e+2].T+=(EMS[e+1].c[TEMPERATURE]*EMS[e+1].Rho*EMS[e+1].L)/(EMS[e+1].c[TEMPERATURE]*EMS[e+1].Rho*EMS[e+1].L + EMS[e+2].c[TEMPERATURE]*EMS[e+2].Rho*EMS[e+2].L)*(tmp_N_T_up[e]-NDS[e+1].T);
							} else {
								if(e==nE-2) {
									NDS[e+2].T=2.*EMS[e+1].Te-NDS[e+1].T;
								}
							}
							// Then the node below
							if(e>1) {
								NDS[e-1].T+=(EMS[e-1].c[TEMPERATURE]*EMS[e-1].Rho*EMS[e-1].L)/(EMS[e-1].c[TEMPERATURE]*EMS[e-1].Rho*EMS[e-1].L + EMS[e-2].c[TEMPERATURE]*EMS[e-2].Rho*EMS[e-2].L)*(tmp_N_T_down[e]-NDS[e].T);
							} else {
								if(e==1) NDS[e-1].T=2.*EMS[e-1].Te-NDS[e].T;
							}
						} else {
							// Complete phase transitions or no phase transition
							// Energy conservative way to determine the temperature of the node below the element:
							NDS[e].T=2.*EMS[e].Te-NDS[e+1].T;
							// Now check if we would have phase change in the element below. If so, we don't need to consider an energy conservative adjustment, because
							// the element will be treated in the next step.
							if(e>0 && ((EMS[e-1].Te < EMS[e-1].meltfreeze_tk && EMS[e-1].theta[WATER] <= cmp_theta) || (EMS[e-1].Te > EMS[e-1].meltfreeze_tk && EMS[e-1].theta[ICE] <= 0.))) {
								// No phase change in element below, so adjust the adjacent nodes.
								if(e>1) {
									NDS[e-1].T+=(EMS[e-1].c[TEMPERATURE]*EMS[e-1].Rho*EMS[e-1].L)/(EMS[e-1].c[TEMPERATURE]*EMS[e-1].Rho*EMS[e-1].L + EMS[e-2].c[TEMPERATURE]*EMS[e-2].Rho*EMS[e-2].L)*(tmp_N_T_down[e]-NDS[e].T);
								} else {
									if(e==1) NDS[e-1].T=2.*EMS[e-1].Te-NDS[e].T;
								}
							}
						}

						// Check the new nodal temperatures to make sure
						if(e<nE-1) {
							if(EMS[e+1].theta[WATER] > cmp_theta + Constants::eps && EMS[e+1].theta[ICE] < max_theta_ice) {
								NDS[e+2].T=std::max(NDS[e+2].T, EMS[e+1].meltfreeze_tk);
							}
							if(EMS[e+1].theta[ICE] > Constants::eps) {
								NDS[e+2].T=std::min(NDS[e+2].T, EMS[e+1].meltfreeze_tk);
							}
						}
						if(e>0) {
							if(EMS[e-1].theta[WATER] > cmp_theta + Constants::eps && EMS[e-1].theta[ICE] < max_theta_ice) {
								NDS[e-1].T=std::max(NDS[e-1].T, EMS[e-1].meltfreeze_tk);
							}
							if(EMS[e-1].theta[ICE] > Constants::eps) {
								NDS[e-1].T=std::min(NDS[e-1].T, EMS[e-1].meltfreeze_tk);
							}
						}

						// Recalculate the element temperature of the affected nodes
						EMS[e].Te=0.5*(NDS[e].T+NDS[e+1].T);
						if(e < nE-1) EMS[e+1].Te=0.5*(NDS[e+1].T+NDS[e+2].T);
					}
				}
				// TODO If WATER_LAYER && ql_rest > 0, consider evaporating water left in the last element above soil!
			} else {
				if ( EMS[e].Te != i_Te && iwatertransportmodel_soil == RICHARDSEQUATION && e < Xdata.SoilNode ) {
					// In case we use Richards equation for soil and have recent phase changes (Te != i_Te), then, adjust nodes accordingly.
					if(e==nE-1) {
						NDS[e+1].T+=EMS[e].Te-i_Te;
						retTopNodeT=NDS[e+1].T;
					} else if (e==Xdata.SoilNode-1) {
						NDS[e+1].T=(EMS[e].Te + EMS[e+1].Te)/2.0;
					} else {
						NDS[e+1].T+=0.5*(EMS[e].Te-i_Te);
					}
					NDS[e].T+=0.5*(EMS[e].Te-i_Te);
				} else {
					// In case we use Richards equation for soil, phase changes will be calculated in ReSolver1d::SolveRichardsEquation
					// Nevertheless, we need to make sure to define the return value:
					if(e==nE-1 && nE==Xdata.SoilNode) {
						if(EMS[e].theta[ICE] > Constants::eps && EMS[e].theta[ICE] < max_theta_ice) {
							// When soil is freezing or thawing when using Richards Equation, we should return the melting temperature.
							retTopNodeT=EMS[e].meltfreeze_tk;
						} else {
							retTopNodeT=NDS[e+1].T;
						}
					}
				}
			}
		}
	} catch (const exception& ) {
		throw;
	}

	// Check surface node, in case TSS is above melting point, but the element itself is below melting point and consequently, phase changes did not occur.
	if (nE >= 1) {	// Only check when there are elements.
		const double cmp_theta_r=((iwatertransportmodel_snow==RICHARDSEQUATION && EMS[nE-1].theta[SOIL]<Constants::eps) || (iwatertransportmodel_soil==RICHARDSEQUATION && EMS[nE-1].theta[SOIL]>Constants::eps)) ? (PhaseChange::RE_theta_threshold) : (PhaseChange::theta_r);
		if ((NDS[nE].T > EMS[nE-1].meltfreeze_tk && EMS[nE-1].theta[ICE] > Constants::eps) || (NDS[nE].T < EMS[nE-1].meltfreeze_tk && EMS[nE-1].theta[WATER] > cmp_theta_r && EMS[nE-1].theta[ICE] < max_theta_ice)) {
			//In case the surface temperature is above the melting point of the upper element and it still consists of ice
			if(nE==1) {
				// If only 1 element is present, the bottom node is adjusted with the same amount as the upper node, so we don't alter the internal energy state of the element ...
				NDS[nE-1].T+=(NDS[nE].T-EMS[nE-1].meltfreeze_tk);
				// ... and the top node is set to melting conditions
				NDS[nE].T=EMS[nE-1].meltfreeze_tk;
			}
			if(nE>1) {
				// If we have more than 1 element, we adjust the nE-1 node such that internal energy is conserved between element nE-1 and nE-2 ...
				NDS[nE-1].T+=(EMS[nE-1].c[TEMPERATURE]*EMS[nE-1].Rho*EMS[nE-1].L)/(EMS[nE-1].c[TEMPERATURE]*EMS[nE-1].Rho*EMS[nE-1].L + EMS[nE-2].c[TEMPERATURE]*EMS[nE-2].Rho*EMS[nE-2].L)*(NDS[nE].T-EMS[nE-1].meltfreeze_tk);
				// ... and the top node is set to melting conditions
				NDS[nE].T=EMS[nE-1].meltfreeze_tk;
			}
		}
	}

	// Update element temperatures
	e = nE;
	while (e > 0) {
		e--;
		if (alpine3d) {
			// For alpine3d simulations, be strict in the nodal temperatures
			if(NDS[e+1].T <= t_crazy_min) {
				prn_msg(__FILE__, __LINE__, "wrn", date_in, "Crazy node (T=%f) at %d of %d corrected.", NDS[e+1].T, e+1, nE+1);
				NDS[e+1].T=t_crazy_min*1.001;
			}
			if(NDS[e+1].T >= t_crazy_max) {
				prn_msg(__FILE__, __LINE__, "wrn", date_in, "Crazy node (T=%f) at %d of %d corrected.", NDS[e+1].T, e+1, nE+1);
				NDS[e+1].T=t_crazy_max*0.999;
			}
			if(NDS[e].T <= t_crazy_min) {
				prn_msg(__FILE__, __LINE__, "wrn", date_in, "Crazy node (T=%f) at %d of %d corrected.", NDS[e].T, e, nE+1);
				NDS[e].T=t_crazy_min*1.001;
			}
			if(NDS[e].T >= t_crazy_max) {
				prn_msg(__FILE__, __LINE__, "wrn", date_in, "Crazy node (T=%f) at %d of %d corrected.", NDS[e].T, e, nE+1);
				NDS[e].T=t_crazy_max*0.999;
			}
		}

		// Recalculate the element temperatures
		EMS[e].Te=0.5*(NDS[e].T+NDS[e+1].T);
		if(e < nE-1) EMS[e+1].Te=0.5*(NDS[e+1].T+NDS[e+2].T);
		if(e > 0) EMS[e-1].Te=0.5*(NDS[e-1].T+NDS[e].T);
	}

	return retTopNodeT;
}

