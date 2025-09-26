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

#include <snowpack/Stability.h>
#include <snowpack/StabilityAlgorithms.h>
#include <snowpack/Laws_sn.h>
#include <snowpack/Constants.h>
#include <snowpack/Utils.h>

#include <assert.h>

using namespace mio;
using namespace std;

const double Stability::psi_ref = 38.0; ///< Reference slope angle
const double Stability::max_stability = 6.0; ///< Upper stability limit
const double Stability::minimum_slab = 0.1; ///< Minimum slab thickness for natural and deformation stability index (m)
const double Stability::ground_rough = 0.2; ///< The first GROUND_ROUGH m of snow will not be unstable due to ground roughness
const double Stability::min_depth_ssi = 0.1; ///< MIN_DEPTH_SSI m of snow must be left after discarding penetration depth
const double Stability::skier_depth = 1.0; ///< Skiers will not trigger failures SKIER_DEPTH m below penetration depth
const double Stability::min_thick_crust = 0.03; ///< Minimum thickness for a supporting melt-freeze crust (perp to slope, in m)
const size_t Stability::nmax_lemon = 2; ///< Maximum number of structural instabilities looked at ("lemons")
const int Stability::sh_mod = 2; ///< Defines regression model for surface hoar shear strength

/**
 * @brief Defines classification scheme for snow profiles
 * - 0: Based on Skier Stability Index (SSI) thresholds (3 classes)
 * - 1: Based on re-analysis by Schweizer/Bellaire of SSI and SK38 (2006)
 * - 2: Nov 2007: re-analysis after recalibration of settling (see rev 250/251)
 * - 3: According to Schweizer and Wiesinger (5 classes)
 */
const int Stability::prof_classi = 2;

map<string, StabMemFn> Stability::mapHandHardness;
map<string, StabFnShearStrength> Stability::mapShearStrength;
const bool Stability::__init = Stability::initStaticData();

bool Stability::initStaticData()
{
	mapHandHardness["MONTI"]    = &StabilityAlgorithms::getHandHardnessMONTI;
	mapHandHardness["BELLAIRE"]  = &StabilityAlgorithms::getHandHardnessBELLAIRE;
	mapHandHardness["ASARC"]    = &StabilityAlgorithms::getHandHardnessASARC;

	mapShearStrength["DEFAULT"] = &StabilityAlgorithms::setShearStrengthDEFAULT;
	mapShearStrength["NIED"]    = &StabilityAlgorithms::setShearStrength_NIED;

	return true;
}

/************************************************************
 * non-static section                                       *
 ************************************************************/

Stability::Stability(const SnowpackConfig& cfg, const bool& i_classify_profile)
           : strength_model(), hardness_parameterization(), hoar_density_buried(IOUtils::nodata), plastic(false),
             classify_profile(i_classify_profile), multi_layer_sk38(false), RTA_ssi(false)
{
	cfg.getValue("STRENGTH_MODEL", "SnowpackAdvanced", strength_model);
	cfg.getValue("HARDNESS_PARAMETERIZATION", "SnowpackAdvanced", hardness_parameterization);
	cfg.getValue("MULTI_LAYER_SK38", "SnowpackAdvanced", multi_layer_sk38); //HACK: temporary key until we decide for a permanent solution
	cfg.getValue("SSI_IS_RTA", "SnowpackAdvanced", RTA_ssi); //HACK: temporary key until we decide for a permanent solution

	const map<string, StabMemFn>::const_iterator it1 = mapHandHardness.find(hardness_parameterization);
	if (it1 == mapHandHardness.end()) throw InvalidArgumentException("Unknown hardness parameterization: "+hardness_parameterization, AT);

	const map<string, StabFnShearStrength>::const_iterator it2 = mapShearStrength.find(strength_model);
	if (it2 == mapShearStrength.end()) throw InvalidArgumentException("Unknown strength model: "+strength_model, AT);

	cfg.getValue("PLASTIC", "SnowpackAdvanced", plastic); //To build a sandwich with a non-snow layer (plastic or wood chips) on top;

	// Density of BURIED surface hoar (kg m-3), default: 125./ Antarctica: 200.
	cfg.getValue("HOAR_DENSITY_BURIED", "SnowpackAdvanced", hoar_density_buried);
}

/**
 * @brief Initializes stability parameters
 * @param Xdata
 */
void Stability::initStability(SnowStation& Xdata)
{
	const size_t nN = Xdata.getNumberOfNodes();
	for(size_t n=Xdata.SoilNode; n<nN; n++) {
		Xdata.Ndata[n].S_n = Stability::max_stability;
		Xdata.Ndata[n].S_s = Stability::max_stability;
		if (n < nN-1) Xdata.Edata[n].S_dr = Stability::max_stability;
	}

	Xdata.S_d = Xdata.S_n = Xdata.S_s = Xdata.S_4 = Xdata.S_5 = Stability::max_stability;
	Xdata.z_S_d = Xdata.z_S_n = Xdata.z_S_s = Xdata.z_S_4 = Xdata.z_S_5 = 0.;
	Xdata.S_class1 = Xdata.S_class2 = -1;
}

/**
 * @brief Returns the structural stability index SSI
 * Adds one lemon to Sk for each structural instability found, presently hardness and grain size differences
 * above a given threshold
 * @param Edata_lower Xdata->Edata[e]
 * @param Edata_upper Xdata->Edata[e+1]
 * @param Sk Skier stability index Sk (Xdata->Ndata[e+1].S_s)
 * @param[out] n_lemon
 * @return ssi
 */
double Stability::initStructuralStabilityIndex(const ElementData& Edata_lower, const ElementData& Edata_upper,
                                              const double& Sk, unsigned short &n_lemon)
{
	static const double thresh_dhard=1.5, thresh_dgsz=0.5; // Thresholds for structural instabilities

	n_lemon = 0;
	const double dhard = fabs(Edata_lower.hard - Edata_upper.hard);
	if ( dhard > thresh_dhard ) n_lemon++;

	const double dgsz = 2.*fabs(Edata_lower.rg - Edata_upper.rg);
	if ( dgsz > thresh_dgsz ) n_lemon++;

	// Skier Stability Index (SSI), limit stability index to range {0.05, Stability::max_stability}
	const double ssi = std::max( 0.05, std::min( Stability::max_stability, static_cast<double>(Stability::nmax_lemon - n_lemon) + Sk ) );

	return ssi;
}


/**
 * @brief The stability information is based on a very empirical principle. First
 * a distinction is made between "direct action" and "slab" situations. The former
 * have to do with strain weakening during heavy snowfalls or during melt situations.
 * The original Bob intra-layer stability has been adapted for this situation and
 * complemented by the Conway approach. The latter is handled by an adaptation
 * of the Schweizer - Wiesinger profile classification in combination with a more
 * conventional stability index based on critical shear strength values.
 * @param Mdata CurrentMeteo
 * @param Xdata Profile
 */
void Stability::checkStability(const CurrentMeteo& Mdata, SnowStation& Xdata)
{
	const double cos_sl = Xdata.cos_sl; // Cosine of slope angle
	// Dereference the element pointer containing micro-structure data
	const size_t nN = Xdata.getNumberOfNodes();
	const size_t nE = nN-1;
	vector<NodeData>& NDS = Xdata.Ndata;
	vector<ElementData>& EMS = Xdata.Edata;

	initStability(Xdata);
	if ( (nE <= Xdata.SoilNode) || plastic ) return; // Return if bare soil or PLASTIC or nE==0

	const double Pk = StabilityAlgorithms::compPenetrationDepth(Xdata); // Skier penetration depth
	double strength_upper = 1001.; //default initial value

	double slab_thick = 0.;	// Slab thickness
	double slab_mass = 0.;	// Slab mass
	double hi_Ei = 0.;		//this is the denominator of the multi layer Young's modulus

	std::vector<unsigned short> n_lemon(nN, 0.);
	size_t e = nE;
	while (e-- > Xdata.SoilNode) {
		EMS[e].hard = (mapHandHardness[hardness_parameterization])(EMS[e], hoar_density_buried);
		EMS[e].S_dr = StabilityAlgorithms::setDeformationRateIndex(EMS[e]);
		StabilityData  STpar(Stability::psi_ref);

		//update slab properties
		const double hi = EMS[e].L;
		slab_thick += hi;			//Increment slab depth
		slab_mass += EMS[e].M;	//Increment slab mass
		EMS[e].E = ElementData::getYoungModule(slab_mass/slab_thick, ElementData::Sigrist);
		STpar.strength_upper = strength_upper; //reset to previous value
		StabilityAlgorithms::compReducedStresses(EMS[e].C, cos_sl, STpar);

		if ( !(mapShearStrength[strength_model])(Xdata.cH, cos_sl, Mdata.date,
		                                                               EMS[e], NDS[e+1], STpar)) {
			prn_msg(__FILE__, __LINE__, "msg-", Date(), "Node %03d of %03d", e+1, nN);
		}
		strength_upper = STpar.strength_upper; //store previous value

		const double depth_lay = (Xdata.cH - (NDS[e+1].z + NDS[e+1].u))/cos_sl;
		NDS[e+1].S_n = StabilityAlgorithms::getNaturalStability(STpar);
		if (!multi_layer_sk38) {
			NDS[e+1].S_s = StabilityAlgorithms::getLayerSkierStability(Pk, depth_lay, STpar);
		} else {		//compute the multi-layer equivalent depth
			//current layer properties
			const double Ei_cbrt = pow(ElementData::getYoungModule(EMS[e].Rho, ElementData::Sigrist), 1./3.);
			hi_Ei +=  hi * Ei_cbrt / STpar.cos_psi_ref;
			const double h_e = hi_Ei / Ei_cbrt; //avoid computing cbrt, cube, cbrt again*/
			NDS[e+1].S_s = StabilityAlgorithms::getLayerSkierStability(Pk, h_e, STpar);
		}
		if (e < nE-1) {
			NDS[e+1].ssi = initStructuralStabilityIndex(EMS[e], EMS[e+1], NDS[e+1].S_s, n_lemon[e+1]);
			if(e>Xdata.SoilNode+1) // Calculate critical cut length
				EMS[e-1].crit_cut_length = StabilityAlgorithms::CriticalCutLength(slab_thick, slab_mass/slab_thick, cos_sl, EMS[e-1], STpar, EMS[e].C);
		} else {
			NDS[nN-1].ssi = Stability::max_stability;
			EMS[nE-1].crit_cut_length = Constants::undefined;
		}
	}

	// Now find the weakest point in the stability profiles for natural and skier indices
	double Swl_ssi, Swl_Sk38;
	size_t Swl_lemon;
	Stability::findWeakLayer(Pk, n_lemon, Xdata, Swl_ssi, Swl_Sk38, Swl_lemon);
	if (RTA_ssi) StabilityAlgorithms::getRelativeThresholdSum(Xdata); //HACK: overwrite the Ndata.ssi with the RTA

	switch (Stability::prof_classi) {
		case 0:
			StabilityAlgorithms::classifyStability_Bellaire(Swl_ssi, Xdata);
			break;
		case 1:
			StabilityAlgorithms::classifyStability_SchweizerBellaire(Swl_ssi, Swl_Sk38, Xdata);
			break;
		case 2:
			StabilityAlgorithms::classifyStability_SchweizerBellaire2(Swl_ssi, Swl_lemon, Swl_Sk38, Xdata);
			break;
		case 3:
			// Classify in 5 classes based on ideas from Schweizer & Wiesinger
			if (!StabilityAlgorithms::classifyStability_SchweizerWiesinger(Xdata)) {
				prn_msg( __FILE__, __LINE__, "wrn", Mdata.date,
					    "Profile classification failed! (classifyStability_SchweizerWiesinger)");
			}
			break;
		default:
			prn_msg( __FILE__, __LINE__, "err", Mdata.date,
						"Profile classification failed! Unknown prof. calss provided");
	}

	if (classify_profile) {
		// Profile type based on "pattern recognition"; N types out of 10
		// We assume that we don't need it in Alpine3D
		if (!StabilityAlgorithms::classifyType_SchweizerLuetschg(Xdata)) {
			prn_msg( __FILE__, __LINE__, "wrn", Mdata.date, "Profile not classifiable! (classifyType_SchweizerLuetschg)");
		}
	}
}

void Stability::findWeakLayer(const double& Pk, std::vector<unsigned short>& n_lemon, SnowStation& Xdata, double &Swl_ssi, double &Swl_Sk38, size_t &Swl_lemon)
{
	const double cos_sl = Xdata.cos_sl; // Cosine of slope angle
	// Dereference the element pointer containing micro-structure data
	const size_t nN = Xdata.getNumberOfNodes();
	const size_t nE = nN-1;
	vector<NodeData>& NDS = Xdata.Ndata;
	vector<ElementData>& EMS = Xdata.Edata;

	// Initialize
	Swl_lemon = 0; // Lemon counter
	double Swl_d, Swl_n, zwl_d, zwl_n, zwl_ssi, zwl_Sk38; // Temporary weak layer markers
	Swl_d = Swl_n = Swl_ssi = Swl_Sk38 = INIT_STABILITY;
	zwl_d = zwl_n = zwl_ssi = zwl_Sk38 = Xdata.cH;

	// Natural and "deformation rate" Stability Index
	// Discard Stability::minimum_slab (in m) at surface
	size_t e = nE;
	while ((e-- > Xdata.SoilNode) && (((Xdata.cH - (NDS[e+1].z + NDS[e+1].u))/cos_sl) < Stability::minimum_slab)) {};
	if (e==static_cast<size_t>(-1)) e=0; //HACK: this is ugly: e got corrupted if SoilNode==0

	if ((e > Xdata.SoilNode) && (e != IOUtils::unodata)) {
		// Slab must be thicker than Stability::ground_rough (m)  for an avalanche to release.
		while ((e-- > Xdata.SoilNode) && ((NDS[e+1].z + NDS[e+1].u)/cos_sl > Stability::ground_rough)) {
			// "deformation rate" Stability Index: find minimum ...
			if (Swl_d > EMS[e].S_dr) {
				Swl_d = EMS[e].S_dr;
				zwl_d = (NDS[e].z + NDS[e+1].z + NDS[e].u + NDS[e+1].u)/2.;
			}
			// Natural Stability Index: find minimum ...
			if ( Swl_n > NDS[e+1].S_n ) {
				Swl_n = NDS[e+1].S_n;
				zwl_n = NDS[e+1].z + NDS[e+1].u;
			}
		}
		// Assign minimum to stability indices
		Xdata.S_d = Swl_d;    Xdata.z_S_d = zwl_d - Xdata.Ground;
		Xdata.S_n = Swl_n;    Xdata.z_S_n = zwl_n - Xdata.Ground;
	} else {
		// Assign bottom values to stability indices
		Xdata.S_d = EMS[Xdata.SoilNode].S_dr;  Xdata.z_S_d = EMS[Xdata.SoilNode].L;
		Xdata.S_n = NDS[Xdata.SoilNode+1].S_n; Xdata.z_S_n = EMS[Xdata.SoilNode].L;
	}

	// Skier Stability Index
	//   Snow depth must be larger than Stability::ground_rough (m) and at least Stability::min_depth_ssi (m)
	//   snow must be left after discarding Pk for a SSI value to be searched.
	if ((Xdata.cH/cos_sl > Stability::ground_rough) && ((Xdata.cH/cos_sl - Pk) > Stability::min_depth_ssi)) {
		// Discard penetration depth Pk (in m) at surface
		e = nE;
		while ((e-- > Xdata.SoilNode) && (((Xdata.cH - (NDS[e+1].z + NDS[e+1].u))/cos_sl) < Pk)) {};
		if (e==static_cast<size_t>(-1)) e=0; //HACK: this is ugly: e got corrupted if SoilNode==0

		if ((e > Xdata.SoilNode) && (e != IOUtils::unodata)) {
			// Only down to Pk + Stability::skier_depth (m)

			while ((e-- > Xdata.SoilNode) && (((Xdata.cH - (NDS[e+1].z + NDS[e+1].u))/cos_sl) < (Pk + Stability::skier_depth)) && ((NDS[e+1].z + NDS[e+1].u)/cos_sl > Stability::ground_rough)) {
				// Skier Stability Index: find minimum OR consider number of structural instabilities in case of near equalities

				if ( (Swl_ssi > NDS[e+1].ssi) || ((fabs(Swl_ssi - NDS[e+1].ssi) < 0.09) && (n_lemon[e+1] > Swl_lemon)) ) {
					Swl_ssi = NDS[e+1].ssi;
					zwl_ssi = NDS[e+1].z + NDS[e+1].u ;
					Swl_lemon = n_lemon[e+1];
					Swl_Sk38 = NDS[e+1].S_s;
					zwl_Sk38 = NDS[e+1].z + NDS[e+1].u;
				}
			}
			// Assign minimum to stability indices
			Xdata.S_s = Swl_Sk38; Xdata.z_S_s = zwl_Sk38 - Xdata.Ground;
			Xdata.S_4 = Swl_ssi;  Xdata.z_S_4 = zwl_ssi - Xdata.Ground;
		} else {
			// Assign bottom values to stability indices
			Xdata.S_s = NDS[Xdata.SoilNode+1].S_s; Xdata.z_S_s = EMS[Xdata.SoilNode].L;
			Xdata.S_4 = NDS[Xdata.SoilNode+1].ssi; Xdata.z_S_4 = EMS[Xdata.SoilNode].L;
		}
	} else {
		// Assign top values to stability indices
		Xdata.S_s = Stability::max_stability; Xdata.z_S_s = Xdata.cH;
		Xdata.S_4 = NDS[nN-1].ssi; Xdata.z_S_4 = Xdata.cH;
	}
}
