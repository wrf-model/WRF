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

#include <snowpack/StabilityAlgorithms.h>
#include <snowpack/Laws_sn.h>
#include <snowpack/Constants.h>
#include <snowpack/Utils.h>
#include <snowpack/Stability.h>

#include <assert.h>

using namespace mio;
using namespace std;

/**
 * @brief Compute hand hardness for a given grain type and density
 * All the information about hardness parameterizations for PP DF RG FC DH MF FCxf are published in
 * Monti et al. (in progress)
 * @author Fabiano Monti
 * @date 2012-06-27
 * @param F grain type
 * @param rho snow density
 * @param water_content 
 * @param buried_hoar_density density of the burried hoar (kg m-3)
 * @return hand hardness index (1)
 */
double StabilityAlgorithms::getHandHardnessMONTI(const int& F, const double& rho, const double& water_content, const double& buried_hoar_density)
{
	switch(F) {
		case 0: { // Graupel PPgp; introduced by Yamaguchi & Fierz, Feb 2004
			const double A = 0.0078;
			const double B = 0.0105;
			return std::max(1., std::min(6., A + B*rho));
		}
		case 1: { // Precipitation Particles PP; obtained from median value for hand_hardness_1 (110 kg/m3) + standard dev (33.9397 kg/m3)
		          // if not the median value for hand_hardness_2 is 129.5 kg/m3 but it comes from only 6 observations;
			if ((rho >= 0.) && (rho < 143.9397))
				return 1.;
			else
				return 2.;
		}
		case 2: { // Decomposing and Fragmented precipitation particles DF
			if ((rho >= 0.) && (rho <= 214.2380))
				return 1.;
			else if ((rho > 214.2380) && (rho <= 268.2981))
				return 2.;
			else if ((rho > 268.2981) && (rho <= 387.4305))
				return 3.;
			else
				return 4.;
		}
		case 3: { // Rounded Grains RG
			if ((rho >= 0.) && (rho <= 189.2103))
				return 1.;
			else if ((rho > 189.2103) && (rho <= 277.8087))
				return 2.;
			else if ((rho > 277.8087) && (rho <= 368.4093))
				return 3.;
			else if ((rho > 368.4093) && (rho <= 442.4917))
				return 4.;
			else
				return 5.;
		}
		case 4: { // Faceted Crystals FC
			if ((rho >= 0.) && (rho <= 247.2748))
				return 1.;
			else if ((rho > 247.2748) && (rho <= 319.3549))
				return 2.;
			else if ((rho > 319.3549) && (rho <= 400.4450))
				return 3.;
			else if ((rho > 400.4450) && (rho <= 517.5751))
				return 4.;
			else
				return 5.;
		}
		case 5: { // Depth Hoar DH
			if ((rho >= 0.) && (rho <= 287.8198))
				return 1.;
			else if ((rho > 287.8198) && (rho <= 344.3826))
				return 2.;
			else
				return 3.;
		}
		case 6: { // Surface hoar SH; empirical: index 1 to 2 from buried_hoar_density to 250 kg m-3
			const double A = 1. - buried_hoar_density/(250. - buried_hoar_density);
			const double B = 1./(250. - buried_hoar_density);
			return std::max(1., std::min(6., A + B*rho));
		}
		case 7: { // Melt Forms MF
			if (water_content < SnowStation::thresh_moist_snow) { //dry melt forms
				if ((rho >= 0.) && (rho <= 213.7375))
					return 1.;
				else if ((rho > 213.7375) && (rho <= 317.3527))
					return 2.;
				else if ((rho > 317.3527) && (rho <= 406.9522))
					return 3.;
				else if ((rho > 406.9522) && (rho <= 739.8220))
					return 4.;
				else
					return 5.;
			} else { //moist melt forms
				if ((rho >= 0.) && (rho <= 338.3760))
					return 1.;
				else if ((rho > 338.3760) && (rho <= 417.4638))
					return 2.;
				else if ((rho > 417.4638) && (rho <= 541.6018))
					return 3.;
				else if ((rho > 541.6018) && (rho <= 614.6830))
					return 4.;
				else
					return 5.;
			}
		}
		case 8: { // Ice layer IFil
			return 6.;
		}
		case 9: { // Rounding faceted particles FCxr
			if ((rho >= 0.) && (rho <= 259.7887))
				return 1.;
			else if ((rho > 259.7887) && (rho <= 326.8632))
				return 2.;
			else if ((rho > 326.8632) && (rho <= 396.9411))
				return 3.;
			else if ((rho > 396.9411) && (rho <= 484.5384))
				return 4.;
			else
				return 5.;
		}
		default: {
			std::stringstream ss;
			ss << "Error: grain type " << F << " is unknown!";
			throw IOException(ss.str(), AT);
		}
	}
}

/**
 * @brief Assign hardness to snow types according to density, Swiss version by Sascha Bellaire
 * @author Implemented by C. Fierz: Regression by Sascha Bellaire January 2005 (all types except MFcr).
 * The original Swiss regression has been modified for PP, RG and FC to get a better agreement
 * to observed hardness. If there is a new settlement formulation in future, and therefore a
 * better agreement to observed density,it must be checked whether the new or the original
 * regression is the better choice. (18 September 2005; Fierz / S. Bellaire)
 * Original regression values are added as comments where needed.
 * @param Edata
 * @param buried_hoar_density density of the burried hoar (kg m-3)
 * @return hand hardness index (1)
 */
double StabilityAlgorithms::getHandHardnessBELLAIRE(const ElementData& Edata, const double& buried_hoar_density)
{
	const double gsz = 2.*Edata.rg;
	double hardness;

	if ( (Edata.mk%100) < 20 ) { // all types except MFcr (hardness 5)
		int F1, F2, F3; // grain shape
		typeToCode(&F1, &F2, &F3, Edata.type); // Decompose type in its constituents
		double A, B;
		switch(F1) {
			case 0: { // Graupel PPgp; introduced by Yamaguchi & Fierz, Feb 2004
				A = 0.0078;
				B = 0.0105;
				break;
			}
			case 1: { // Precipitation Particles PP; ori: A = 0.7927; B = 0.0038;
				A = 0.7927;
				B = 0.0036;
				break;
			}
			case 2: { // Decomposing and Fragmented precipitation particles DF
				A = 0.4967;
				B = 0.0074;
				break;
			}
			case 3: { // Rounded Grains RG; ori: A = 0.2027; B = 0.0092;
				A = 0.2027;
				B = 0.0072;
				break;
			}
			case 4: { // Faceted Crystals FC; ori: A = 0.3867; B = 0.0071;
				A = 0.3867;
				B = 0.0083;
				break;
			}
			case 5: { // Depth Hoar DH
				A = -0.00249;
				B = 0.0072;
				break;
			}
			case 6: { // Surface hoar SH; empirical: index 1 to 2 from buried_hoar_density to 250 kg m-3
				A = 1. - buried_hoar_density/(250. - buried_hoar_density);
				B = 1./(250. - buried_hoar_density);
				break;
			}
			case 7: { // Melt Forms MF
				A = 0.5852;
				B = 0.0056;
				break;
			}
			case 8: { // Ice layer IFil
				A = 6.;
				B = 0.;
				break;
			}
			case 9: { // Rounding faceted particles FCxr
				A = -0.5226;
				B = 0.0104;
				break;
			}
			default: {
				A = Constants::undefined;
				B = 0.;
				break;
			}
		}
		hardness = A + B*Edata.Rho;
		// Large surface hoar stays longer unstable! 1 dec 2007 (sb)
		if ((F1 == 6) && (gsz >= 5.)) {
			hardness = 1;
		} else if ((F1 == 6 ) && (gsz < 5.)) {
			hardness = std::min(hardness, 2.);
		}
	} else if (Edata.theta[ICE] <= 0.7) { // Melt-freeze crust MFcr
		if (Edata.theta[WATER] < 0.3 * Edata.res_wat_cont) {
			hardness = 5.;
		} else if (Edata.theta[WATER] < 0.6 * Edata.res_wat_cont) {
			hardness = 4.5;
		} else if (Edata.theta[WATER] < 0.85 * Edata.res_wat_cont) {
			hardness = 4.;
		} else {
			hardness = 3.;
		}
	} else { // Ice Formations IF
		hardness = 6.;
	}
	// Limit to range {1, 6}
	hardness = std::max(1., std::min(6., hardness));
	return hardness;
}

/**
 * @brief Assign hardness to snow types according to density
 * Implementation according to Fabiano Monti's work, June 2012 (all types except MFcr).
 * @author Fabiano Monti
 * @date 2012-06-27
 * @param Edata
 * @param buried_hoar_density density of the burried hoar (kg m-3)
 * @return hand hardness index (1)
 */
double StabilityAlgorithms::getHandHardnessMONTI(const ElementData& Edata, const double& buried_hoar_density)
{
	double hardness;

	if ( (Edata.mk%100) < 20 ) { // all types except MFcr (hardness 5)
		int F1, F2, F3; // grain shape
		typeToCode(&F1, &F2, &F3, Edata.type); // Decompose type in its constituents
		const double hardness_F1 = StabilityAlgorithms::getHandHardnessMONTI(F1, Edata.Rho, Edata.theta[WATER], buried_hoar_density);
		const double hardness_F2 = StabilityAlgorithms::getHandHardnessMONTI(F2, Edata.Rho, Edata.theta[WATER], buried_hoar_density);
		hardness = 0.5 * (hardness_F1 + hardness_F2);

		if (F1 == 6) {
			// Large surface hoar stays longer unstable! 1 dec 2007 (sb)
			const double grain_size = 2.*Edata.rg;
			if (grain_size >= 5.) {
				hardness = 1.;
			} else {
				hardness = std::min(hardness, 2.);
			}
		}
	} else if (Edata.theta[ICE] <= 0.7) { // Melt-freeze crust MFcr
		const double res_water_cont = Edata.res_wat_cont;
		if (Edata.theta[WATER] < 0.3 * res_water_cont) {
			hardness = 5.;
		} else if (Edata.theta[WATER] < 0.6 * res_water_cont) {
			hardness = 4.5;
		} else if (Edata.theta[WATER] < 0.85 * res_water_cont) {
			hardness = 4.;
		} else {
			hardness = 3.;
		}
	} else { // Ice Formations IF
		hardness = 6.;
	}
	// Limit to range {1, 6}
	hardness = std::max(1., std::min(6., hardness));
	return hardness;
}

/**
 * @brief Assign hand hardness to snow types according to density and grain size, original Canadian version
 * @author Implemented by C. Fierz: Regression from ASARC database by Bruce Jamieson on 2002-08-14
 * @param Edata
 * @param buried_hoar_density density of the burried hoar (kg m-3)
 * @return hand hardness index (1)
 */
double StabilityAlgorithms::getHandHardnessASARC(const ElementData& Edata, const double& buried_hoar_density)
{
	const double gsz = 2.*Edata.rg;
	double A=0., B=0., C=0.;

	int F1, F2, F3;
	typeToCode(&F1, &F2, &F3, Edata.type); // Decompose type in its constituents

	// all types except MFcr
	if (Edata.mk%100 < 20 ) {
		switch ( F1 ) {
			case 0: { // Graupel PPgp; empirical!
				A = 1.5;
				B = 0.;
				C = 0.;
				break;
			}
			case 1: { // Precipitation Particles PP
				A = 0.45;
				B = 0.0068;
				C =  0.;
				break;
			}
			case 2: { // Decomposing and Fragmented precipitation particles DF
				A =  0.;
				B = 0.0140;
				C =  0.;
				break;
			}
			case 3: { // Rounded Grains RG
				A =  1.94;
				B = 0.0073;
				C = -0.192;
				break;
			}
			case 4: {
				if ( F2 != 9 ) { // Faceted Crystals FC
					A =  0.;
					B = 0.0138;
					C = -0.284;
				} else { // Rounding faceted particles FCxr, because F1=9 does not occur in SNOWPACK
					A =  1.29;
					B = 0.0094;
					C = -0.350;
				};
				break;
			}
			case 5: {
				if ( gsz > 1.5 ) { // Depth hoar DH, small dataset (N=41) !!!
					A = -0.80;
					B = 0.0150;
					C = -0.140;
				} else { // use FC values for small depth hoar grains
					A =  0.00;
					B = 0.0138;
					C = -0.284;
				};
				break;
			}
			case 6: { // Surface hoar SH; empirical: index 1 to 2 from buried_hoar_density to 250 kg m-3
				A = 1. - buried_hoar_density/(250. - buried_hoar_density);
				B = 1./(250. - buried_hoar_density);
				C = 0.;
				break;
			}
			case 7: { // Melt Forms MF
				if ( Edata.theta[WATER] < 0.001 ) { // MF, dry
					A = 2.14;
					B = 0.0048;
					C =  0.;
				} else { // MF, wet (LWC > 0.)
					A = 3.00;
					B = 0.0000;
					C =  0.;
				};
				break;
			}
			case 8: { // Ice layer IFil
				A =  6.;
				B = 0.;
				C =  0.;
				break;
			}
			case 9: { // Rounding faceted particles FCxr
				A =  1.29;
				B = 0.0094;
				C = -0.350;
				break;
			}
			default: {
				A = Constants::undefined;
				B = 0.;
				C = 0.;
				break;
			}
		}
	} else if (Edata.theta[ICE] <= 0.7) { // Melt-freeze crust MFcr
		if (Edata.theta[WATER] < 0.3 * Edata.res_wat_cont) {
			A = 5.;
		} else if (Edata.theta[WATER] < 0.6 * Edata.res_wat_cont) {
			A = 4.5;
		} else if (Edata.theta[WATER] < 0.85 * Edata.res_wat_cont) {
			A = 4.;
		} else {
			A = 3.;
		}
	} else { // Ice Formations IF
		A = 6.;
	}

	double hardness = A + B*Edata.Rho + C*gsz;
	if (F1 == 6) hardness = std::min(hardness, 2.);

	// Limit to range {1, 6}
	hardness = std::max(1., std::min(6., hardness));
	return(hardness);
}

/**
 * @brief Returns the critical stress state of a layer given the temperature and plastic strain rate.
 * @param epsNeckDot Neck strain rate (s-1)
 * @param Ts Temperature of layer (K)
 * @return Critical stress (Pa)
 */
double StabilityAlgorithms::compCriticalStress(const double& epsNeckDot, const double& Ts)
{
	const double sigBrittle=1.e7;   // Brittle fracture stress of ice (Pa)
	const double C1=-6.6249;     // Constant
	const double C2=6.0780e-2;   // Constant
	const double C3=-1.3380e-4;  // Constant
	const double P1=70.000;      // Constant (Pa)

	// Find the rate dependent friction angle phi
	const double phi = P1*pow(fabs(epsNeckDot), 0.23)*mio::Cst::to_rad; // Function of strain rate dependent failure surface

	// Hydrostatic melting pressure
	// NOTE this function returns negative values for
	//   Ts <=181.2 K and Ts >= 273.15 K.
	//   The argument to the square root below becomes
	//   negative for Ts <= 180.4 K and Ts >= 274 K
	//   The maximum of the function is reached at 227.2 K
	//   HACK use this value for temperatures below 227.2 K (Quick and dirty fix;-)
	const double temp = (Ts >= 227.2)? Ts : 227.2;
	const double Pm = (C1 + C2*temp + C3*Optim::pow2(temp)) * 1.e9;

	// Return the critical stress. TODO check that argument of sqrt is correctly written
	return (Pm * tan(phi) * sqrt(1. - (Pm/(Pm + sigBrittle))));
}

/**
 * @brief Returns the layer stability index
 * The intra-layer stability criteria is given by the ratio S_f = S_c/S_n where
 * S_n is the neck stress and S_c is the critical stress.  The critical stress is determined
 * in the function st_CriticalStress. This function might get a little more involved as
 * time goes on.
 * @param *Edata
 * @return Deformation rate index
 */
double StabilityAlgorithms::setDeformationRateIndex(ElementData& Edata)
{
	// If you have less than 5% ice then say you know you have something unstable
	if ( Edata.theta[ICE] < 0.05 ) {
		return(0.1);
	}

	const double eps1Dot = 1.76e-7; // Unit strain rate (at stress = 1 MPa) (s-1)
	const double sig1 = 0.5e6;      // Unit stress from Sinha's formulation (Pa)
	const double sig = -Edata.C;   // Overburden stress, that is, absolute value of Cauchy stress (Pa)
	const double Te = std::min(Edata.Te, Edata.meltfreeze_tk); // Element temperature (K)

	// First find the absolute neck stress
	const double sigNeck = Edata.neckStressEnhancement() * (sig); // Neck stress (Pa)
	// Now find the strain rate in the neck
	const double epsNeckDot =  eps1Dot * SnLaws::snowViscosityTemperatureTerm(Te) * mio::Optim::pow3(sigNeck/sig1); // Total strain rate in the neck (s-1) NOTE is it used here only?
	// Return the stability index
	return (std::max(0.1, std::min(compCriticalStress(epsNeckDot, Te) / sigNeck, 6.)));
}

/**
 * @brief Returns the skier's penetration depth Pk
 * Adapted from Jamieson & Johnston, Ann. Glaciol., 26, 296-302 (1998)
 * @param *Xdata
 */
double StabilityAlgorithms::compPenetrationDepth(const SnowStation& Xdata)
{
	double rho_Pk = Constants::eps2, dz_Pk = Constants::eps2; // Penetration depth Pk, from mean slab density
	double top_crust = 0., thick_crust = 0.;  // Crust properties
	bool crust = false;                       // Checks for crust
	size_t e_crust = Constants::stundefined;

	const double cos_sl = Xdata.cos_sl; // Cosine of slope angle
	size_t e = Xdata.getNumberOfElements(); //HACK is this right? It should be nNodes+1
	while ((e-- > Xdata.SoilNode) && ((Xdata.cH - (Xdata.Ndata[e].z + Xdata.Ndata[e].u))/cos_sl < 0.3)) {
		rho_Pk += Xdata.Edata[e].Rho*Xdata.Edata[e].L;
		dz_Pk  += Xdata.Edata[e].L;
		// Test for strong mf-crusts MFcr.
		// Look for the first (from top) with thickness perp to slope > 3cm
		if (!crust) {
			if ( (Xdata.Edata[e].mk%100 >= 20) && (Xdata.Edata[e].Rho > 500.) ) {
				if (e_crust == Constants::stundefined) {
					e_crust = e;
					top_crust = (Xdata.Ndata[e+1].z + Xdata.Ndata[e+1].u)/cos_sl;
					thick_crust += Xdata.Edata[e].L;
				} else if ( ((e_crust - e) < 2) ) {
					thick_crust += Xdata.Edata[e].L;
					e_crust = e;
				}
			} else if (e_crust > 0) {
				if (thick_crust > Stability::min_thick_crust) {
					crust = true;
				} else {
					e_crust = Constants::stundefined;
					top_crust = 0.;
					thick_crust = 0.;
				}
			}
		}
	}
	rho_Pk /= dz_Pk; //weighted average density of the snow slab penetrated by the skier

	// NOTE Pre-factor 0.8 introduced May 2006 by S. Bellaire
	return std::min(0.8 * 43.3 / rho_Pk, ((Xdata.cH / cos_sl) - top_crust));
}

/**
 * @brief Computes normal and shear stresses (kPa) reduced to psi_ref
 * @param stress Overload perpendicular to slope (Pa)
 * @param cos_sl Cosine of slope angle (1)
 * @param STpar
 */
void StabilityAlgorithms::compReducedStresses(const double& stress, const double& cos_sl, StabilityData& STpar)
{
	STpar.sig_n = -stress * mio::Optim::pow2( STpar.cos_psi_ref/cos_sl ) / 1000.;
	STpar.sig_s = STpar.sig_n * STpar.sin_psi_ref / STpar.cos_psi_ref;
}

/**
 * @brief Returns the natural stability index Sn
 * The classic natural stability index Sn, that is, the ratio of shear stress to shear strength (static)
 * @param STpar
 */
double StabilityAlgorithms::getNaturalStability(const StabilityData& STpar)
{
	// Limit natural stability index to range {0.05, Stability::max_stability}
	return(std::max(0.05, std::min(((STpar.Sig_c2 + STpar.phi*STpar.sig_n)/STpar.sig_s), Stability::max_stability)));
}

/**
 * @brief Returns the skier stability index Sk reduced to psi_ref (usually 38 deg => Sk_38)
 * The classic skier stability index Sk(psi_ref), using P. Foehn's formula
 * (IAHS No162, 1987, p201) for the skier (load of 85 kg on 1.7 m long skis) induced shear stress.
 * 
 * This represents the skier contribution to shear stress at psi_ref and is around 0.1523 kPa / layer_depth 
 * at psi_ref = 38 deg and Alpha_max = 54.3 deg.
 * @param Pk Skier penetration depth (m)
 * @param depth_lay Depth of layer to investigate (m)
 * @param STpar
 */
double StabilityAlgorithms::getLayerSkierStability(const double& Pk, const double& depth_lay, const StabilityData& STpar)
{
	const double layer_depth = depth_lay - Pk;
	if ( layer_depth > Constants::eps ) {
		const double Alpha_max = STpar.alpha_max_rad;
		static const double skier_weight = 85.;
		static const double ski_length = 1.7;
		static const double load = skier_weight*Constants::g/ski_length;
		double delta_sig = 2. * load * cos(Alpha_max) * Optim::pow2( sin(Alpha_max) ) * sin(Alpha_max + STpar.psi_ref);
		delta_sig /= Constants::pi *  layer_depth * STpar.cos_psi_ref; // in Pa
		delta_sig /= 1000.; // convert to kPa
		// Limit skier stability index to range {0.05, Stability::max_stability}
		return(std::max(0.05, std::min(((STpar.Sig_c2 + STpar.phi*STpar.sig_n)/(STpar.sig_s + delta_sig)), Stability::max_stability)));
	} else {
		return(Stability::max_stability); // strictly speaking, Sk is not defined
	}
}

bool StabilityAlgorithms::normalizeLemon(std::vector<double>& vecData)
{
	if (vecData.empty()) return false;
	const double mean = mio::Interpol1D::arithmeticMean( vecData );
	const double std_dev = mio::Interpol1D::std_dev( vecData );
	if (std_dev==IOUtils::nodata || std_dev==0.) return false;
	
	for (size_t ii=0; ii<vecData.size(); ii++) {
		vecData[ii] = (vecData[ii] - mean) / std_dev;
	}
	return true;
}

/**
 * @brief Returns the Relative Threshold Sum approach  (RTA) weak layer. 
 * This is according to Monti, Fabiano, and Jürg Schweizer, <i>"A relative difference 
 * approach to detect potential weak layers within a snow profile"</i>, 2013, Proceedings ISSW.
 * @param Xdata all the element and node data for all the layers
 * @return false if error, true otherwise
 */
bool StabilityAlgorithms::getRelativeThresholdSum(SnowStation& Xdata)
{
	vector<NodeData>& NDS = Xdata.Ndata;
	vector<ElementData>& EMS = Xdata.Edata;
	const size_t nE = EMS.size();
	
	const double cos_sl = Xdata.cos_sl;
	const double hs_top = (NDS[nE].z+NDS[nE].u - NDS[Xdata.SoilNode].z) / cos_sl;
	std::vector<double> vecRG, vecRG_diff, vecHard, vecHard_diff, vecTypes;
	std::vector<double> weibull, crust_index;
	
	double crust_coeff = 0.;
	size_t e = nE-1;
	NDS[ nE ].ssi = 0.; //the top node is assumed perfectly stable
	while (e-- > Xdata.SoilNode) {
		NDS[ e ].ssi = 0.; //initialize with 0 so layers that can not get computed don't get in the way
		
		vecRG.push_back( EMS[e].rg );
		vecRG_diff.push_back( fabs(EMS[e+1].rg - EMS[e].rg) );
		vecHard.push_back( EMS[e].hard );
		vecHard_diff.push_back( fabs(EMS[e+1].hard - EMS[e].hard) );
		
		//grain types receive a score depending on their primary and secondary forms
		const unsigned short int primary = static_cast<unsigned short int>( EMS[e].type / 100 %100 );
		const unsigned short int secondary = static_cast<unsigned short int>( EMS[e].type / 10 %10 );
		const bool primary_is_persistent = (primary==4 || primary==5 || primary==6 || primary==9);
		const bool secondary_is_persistent = (secondary==4 || secondary==5 || secondary==6 || secondary==9);
		if (primary_is_persistent && secondary_is_persistent)
			vecTypes.push_back( 1. );
		else if (!primary_is_persistent && !secondary_is_persistent)
			vecTypes.push_back( 0. );
		else
			vecTypes.push_back( .5 );
		
		//compute the weibull function for the depth from the top
		const double layer_depth = hs_top - (NDS[e].z+NDS[e].u - NDS[Xdata.SoilNode].z)/cos_sl;
		static const double w1 = 2.5;
		static const double w2 = 50.;
		const double weibull_depth = (w1/w2) * pow(layer_depth, w1-1.) * exp( -1*pow(layer_depth/w2, w1) );
		
		//compute crust factor
		const bool crust_cond = (EMS[e].L>=1. && EMS[e].hard>=3 );
		const double crust_value = (crust_cond)? exp( -(hs_top -  (NDS[e+1].z+NDS[e+1].u - NDS[Xdata.SoilNode].z)/cos_sl/20. ) ) : 0.;
		crust_coeff += crust_value;
		weibull.push_back( weibull_depth - crust_coeff ); //store the weibull corrected for the crust coefficient
	}
	
	//calculate the normalization parameters
	if (!normalizeLemon(vecRG)) return false;
	const double RG_min = mio::Interpol1D::min_element( vecRG );
	const double RG_max = mio::Interpol1D::max_element( vecRG );
	if (RG_min==RG_max) return false;
	
	if (!normalizeLemon(vecRG_diff)) return false;
	const double RG_diff_min = mio::Interpol1D::min_element( vecRG_diff );
	const double RG_diff_max = mio::Interpol1D::max_element( vecRG_diff );
	if (RG_diff_min==RG_diff_max) return false;
	
	if (!normalizeLemon(vecHard)) return false;
	const double hard_min = mio::Interpol1D::min_element( vecHard );
	const double hard_max = mio::Interpol1D::max_element( vecHard );
	if (hard_min==hard_max) return false;
	
	if (!normalizeLemon(vecHard_diff)) return false;
	const double hard_diff_min = mio::Interpol1D::min_element( vecHard_diff );
	const double hard_diff_max = mio::Interpol1D::max_element( vecHard_diff );
	if (hard_diff_min==hard_diff_max) return false;
	
	if (!normalizeLemon(vecTypes)) return false;
	const double type_min = mio::Interpol1D::min_element( vecTypes );
	const double type_max = mio::Interpol1D::max_element( vecTypes );
	if (type_min==type_max) return false;
	
	const double dp_min = mio::Interpol1D::min_element( weibull );
	const double dp_max = mio::Interpol1D::max_element( weibull );
	if (dp_min==dp_max) return false;
	
	vector<double> index;
	double max_index = 0.;
	for (size_t ii=0; ii<vecRG.size(); ii++) {
		const double RG_norm = (vecRG[ii] - RG_min) / (RG_max - RG_min);
		const double RG_diff_norm = (vecRG_diff[ii] - RG_diff_min) / (RG_diff_max - RG_diff_min);
		const double hard_norm = 1. - (vecHard[ii] - hard_min) / (hard_max - hard_min);
		const double hard_diff_norm = (vecHard_diff[ii] - hard_diff_min) / (hard_diff_max - hard_diff_min);
		const double type_norm = (vecTypes[ii] - type_min) / (type_max - type_min);
		const double dp_norm = (weibull[ii] - dp_min) / (dp_max - dp_min);
		
		index.push_back( RG_norm + RG_diff_norm + hard_norm + hard_diff_norm + type_norm + dp_norm );
		if (index.back()>max_index) max_index = index.back();
	}
	
	for (size_t ii=0; ii<vecRG.size(); ii++) {
		NDS[ nE - ii -1 ].ssi = index[ii] / max_index;
	}
	
	return true;
}

/**
 * @brief Returns the Profile Stability Classification (Schweizer-Wiesinger Method)
 * @param Xdata
 * @return false if error, true otherwise
 */
bool StabilityAlgorithms::classifyStability_SchweizerWiesinger(SnowStation& Xdata)
{
	// Dereference the element pointer containing micro-structure data
	const size_t nE = Xdata.getNumberOfElements();
	vector<ElementData>& EMS = Xdata.Edata;
	vector<NodeData>& NDS = Xdata.Ndata;
	const double cos_sl = Xdata.cos_sl;

	// Classify only for Snowpacks thicker than Stability::minimum_slab (vertically)
	if ( (NDS[nE].z+NDS[nE].u)/cos_sl < Stability::minimum_slab ) {
		Xdata.S_class2 = 5;
		return true;
	}

	// First, find mean, maximum and minimum hardness
	double mH = 0., maxH = 0., minH = 7.; // Mean Hardness and Threshold
	for (size_t e = Xdata.SoilNode; e < nE; e++) {
		maxH = std::max(maxH, EMS[e].hard);
		minH = std::min(minH, EMS[e].hard);
		mH += EMS[e].hard;
	}
	mH /= (double)nE; //mean hardness of profile
	const double thH = std::min(0.5*mH, 1.); //threshold for critical transitions

	// Now make the classification for all critical transitions and keep track ....
	double h_Slab = EMS[nE-1].L/cos_sl;
	unsigned int count=0;
	signed char S = 5;
	for (size_t e = nE-2; e > Xdata.SoilNode; e--) {
		h_Slab += EMS[e].L/cos_sl;
		const double delta_H = EMS[e+1].hard - EMS[e].hard;
		if ( fabs(delta_H) > thH ) {
			count++;
			const size_t e_weak = (delta_H < 0.)? e+1 : e;

			// Decompose grain type to determine majority shape F1
			int F1, F2, F3; // Grain shape
			typeToCode(&F1, &F2, &F3, EMS[e_weak].type);

			// Remember that S is initialized to 5!!!
			// First consider wet weak layer
			if (EMS[e_weak].theta[WATER] > 0.75 * EMS[e_weak].res_wat_cont) {
				if ( EMS[e_weak].mk % 100 < 20 ) {
					S = 1;
				}
			} else { // Then do some stuff for dry snow
				double mH_u = 0.;
				for (size_t ii = e_weak; ii < nE; ii++) {
					mH_u += EMS[e].hard;
				}
				mH_u /= (double)(nE - e_weak);
				if ( mH > 2. ) {
					if ( delta_H < 0. ) {
						// Proposal Fz; (see original in version 7.4
						if ( (mH_u < 2.5) || (h_Slab < 0.7) ) {
							if ( (mH_u > 2.) && (h_Slab > 0.5) ) {
								S = std::min(S, (signed char)4);
							} else {
								S = std::min(S, (signed char)3);
							}
						} else {
							if ( minH > 2. ) {
								S = std::min(S, (signed char)4);
							} else {
								S = std::min(S, (signed char)3);
							}
						}
					}
				} else if ( mH < 1.5 ) {
					if ( (EMS[e_weak].rg > 0.5) && ((F1 > 3) && (F1 < 7)) ) {
						if ( (EMS[e_weak].rg > 0.75) && ((mH_u < 1.5) && (maxH < 2.5)) ) {
							S = 1;
						} else {
							S = std::min(S, (signed char)2);
						}
					} else {
						S = std::min(S, (signed char)3);
					}
				} else {
					S = std::min(S, (signed char)3);
				}
			} // end dry snow
		} // if weak layer found; also end of loop over elements
	} // end for

	if ( count == 0 ) {
		if ( mH > 2. ) {
			S = std::min(S, (signed char)4);
		} else if ( mH < 1.5) {
			if ( maxH > 2.3 ) {
				S = std::min(S, (signed char)2);
			} else {
				S = 1;
			}
		} else {
			S = std::min(S, (signed char)3);
		}
	}

	Xdata.S_class2 = S;

	if ( (S > 5) || (S < 1) ) {
		return false;
	} else {
		return true;
	}
}

/**
 * @brief Returns the Profile Stability Classification based on re-analysis after <b>recalibration of settling</b> (Nov. 2007)
 * This is based on Schweizer, J., Bellaire, S., Fierz, C., Lehning, M. and Pielmeier, C., <i>"Evaluating and improving the 
 * stability predictions of the snow cover model SNOWPACK"</i>, Cold Regions Science and Technology, 2006, <b>46(1)</b>, pp.52-59.
 * @param Swl_ssi
 * @param Swl_lemon
 * @param Swl_Sk38
 * @param Xdata
 */
void StabilityAlgorithms::classifyStability_SchweizerBellaire2(const double& Swl_ssi, const size_t& Swl_lemon, const double& Swl_Sk38, SnowStation& Xdata)
{
	if ((Swl_ssi > 0.) && (Swl_ssi < 100.)) {
		if ( Swl_lemon >= 2 ) {
			Xdata.S_class2 = 1;
		} else if (Swl_lemon == 1) {
			if (Swl_Sk38 < 0.48) {
				Xdata.S_class2 = 1;
			} else {
				if (Swl_Sk38 < 0.71) {
					Xdata.S_class2 = 3;
				} else {
					Xdata.S_class2 = 5;
				}
			}
		} else {
			Xdata.S_class2 = 3;
		}
	} else {
		Xdata.S_class2 = -1;
	}
}

/**
 * @brief Returns the Profile Stability Classification based on re-analysis by Schweizer/Bellaire, see
 * Schweizer, J., Bellaire, S., Fierz, C., Lehning, M. and Pielmeier, C., <i>"Evaluating and improving the 
 * stability predictions of the snow cover model SNOWPACK"</i>, Cold Regions Science and Technology, 2006, <b>46(1)</b>, pp.52-59.
 * @param Swl_ssi
 * @param Swl_Sk38
 * @param Xdata
 */
void StabilityAlgorithms::classifyStability_SchweizerBellaire(const double& Swl_ssi, const double& Swl_Sk38, SnowStation& Xdata)
{
	if ((Swl_ssi > 0.) && (Swl_ssi < 100.)) {
		if ( Swl_Sk38 >= 0.45 ) {
			Xdata.S_class2 = 5;
		} else if ( Swl_ssi >= 1.32 ) {
				Xdata.S_class2 = 3;
			} else {
				Xdata.S_class2 = 1;
			}
	} else {
		Xdata.S_class2 = -1;
	}
}

/**
 * @brief Returns the Profile Stability Classification based on the master thesis of S. Bellaire 
 * (September 2005)
 * @param Swl_ssi
 * @param Xdata
 */
void StabilityAlgorithms::classifyStability_Bellaire(const double& Swl_ssi, SnowStation& Xdata)
{
// Classify in poor, fair and good based on master thesis of S. Bellaire (September 2005)
	if ((Swl_ssi > 0.) && (Swl_ssi < 100.)) {
		if (Swl_ssi >= 1.55) {
			Xdata.S_class2 = 5;
		} else if (Swl_ssi >= 1.25) {
			Xdata.S_class2 = 3;
		} else {
			Xdata.S_class2 = 1;
		}
	} else {
		Xdata.S_class2 = -1;
	}
}

/**
 * @brief "Pattern recognition" of 10 profile types according to Schweizer, J. and M. Luetschg (2001).
 * Schweizer, J. and M. Luetschg, <i>Characteristics of human-triggered avalanches</i>, 2001, Cold Reg. Sci. Technol. 33(2-3): 147-162.
 * Note that analysis is done on vertical snow height.
 * @param *Xdata
 * @return false on error, true otherwise
 */
bool StabilityAlgorithms::classifyType_SchweizerLuetschg(SnowStation& Xdata)
{
	static const size_t n_window=5;                              // Window half-width in number of elements
	static const double L_base_0=0.2;
	static const double min_hard=19.472, slope_hard=150.;        // Constants to compute reduced hardness,
	                                                      // (N) and (N m-1), respectively
	const double cos_sl = Xdata.cos_sl;
	const double cH = (Xdata.cH - Xdata.Ground)/cos_sl; // Vertical snow depth

	// Check for snow profile shallower than 1.5*L_base_0 m (not classifiable)
	if ( cH <= 1.5*L_base_0 ) {
		Xdata.S_class1 = -1;
		return true;
	}

	const size_t nE_s = Xdata.getNumberOfElements() - Xdata.SoilNode; //number of snow elements

	// Dereference element and node pointers
	ElementData *EMS = &Xdata.Edata[0];
	vector<NodeData>& NDS = Xdata.Ndata;

	//temporary vectors
	vector<double> z_el(nE_s, 0.0);                            // Vertical element heigth (m)
	vector<double> L_el(nE_s, 0.0);                            // Vertical element thickness (m)
	vector<double> hard(nE_s, 0.0);                            // Hardness in N
	vector<double> red_hard(nE_s, 0.0);                        // Reduced hardness in N
	vector<double> deltaN(nE_s, 0.0);                          // Difference in hardness between layers in N

	// Absolute and reduced hardness profiles (N)
	for(size_t idx = nE_s; idx --> 0; ) { //because it is decremented before executing anything
		const size_t ii = idx+Xdata.SoilNode; //true element index
		z_el[idx] = ( (NDS[ii].z + NDS[ii].u) + (NDS[ii+1].z + NDS[ii+1].u) ) * .5 / cos_sl;
		L_el[idx] = EMS[ii].L/cos_sl;
		hard[idx] = min_hard*pow(EMS[ii].hard, 2.3607);
		red_hard[idx] = hard[idx] - (min_hard + slope_hard*(cH - z_el[idx]));
		if ( (unsigned)idx == nE_s-1 ) {
			deltaN[idx] = fabs(red_hard[idx] - min_hard);
		} else {
			deltaN[idx] = fabs(red_hard[idx] - red_hard[idx+1]);
		}
	}

	// Check for base strength (bottom L_base_0 m of snow cover)
	// not considering basal melt-freeze crusts
	double L_base = L_base_0;
	double L_sum = 0.;
	double mean_hard=0.,mean_gsz = 0.; // Means
	static const double thresh_hard = 19.472*pow(4., 2.3607); // Hardness threshold (N)
	bool mf_base = (hard[0] > thresh_hard);
	size_t e = 0; //element index

	while ( L_sum <= L_base ) {
		if ( mf_base && (hard[e] < thresh_hard) ) {
			mf_base = false;
			L_base -= L_sum;
			L_sum = 0.;
			mean_hard = mean_gsz = 0.;
		}
		L_sum += L_el[e];
		mean_hard += L_el[e]*hard[e];
		mean_gsz += L_el[e]*(2.*EMS[e+Xdata.SoilNode].rg);
		e++;
	}
	// Averages
	mean_hard /= L_sum;
	mean_gsz /= L_sum;

	// Weak or strong base?
	const bool weak_base = ((mean_hard <= 275.) && (mean_gsz > 0.9));

	// Seek extremes over profile depth
	// Initialise
	size_t e_min = std::min(e, nE_s - 1); //e is >=0
	size_t e_el = e_min;
	size_t e_max = std::min(e_el + n_window, nE_s - 1);
	// Extremes and extremes' absolute heights
	double sum_red_hard = 0.;
	double red_hard_max = -9999.;
	double red_hard_min = 9999.;
	double deltaN_max = -9999.;
	double z_red_hard_min = 9999.;
	double z_red_hard_max = 9999.;
	double z_deltaN_max = 9999.;

	// First evaluation
	L_sum = 0.;
	for (e = e_min; e <= e_max; e++) {
		L_sum += L_el[e];
		sum_red_hard += L_el[e]*red_hard[e];
	}

	// Use window width of 2*n_window+1 elements
	while ( e_el <= (nE_s-1) ) {
		if ( (e_el - e_min) > n_window ) {
			L_sum -= L_el[e_min];
			sum_red_hard -= L_el[e_min]*red_hard[e_min];
			e_min++;
		}
		if ( (e_max < (nE_s-1)) && ((e_max - e_el) < n_window) ) {
			e_max++;
			L_sum += L_el[e_max];
			sum_red_hard += L_el[e_max]*red_hard[e_max];
		}
		// Find extremes ...
		if ( sum_red_hard/L_sum > red_hard_max ) {
			red_hard_max = sum_red_hard/L_sum;
			z_red_hard_max = z_el[e_el];
		}
		if ( sum_red_hard/L_sum < red_hard_min ) {
			red_hard_min = sum_red_hard/L_sum;
			z_red_hard_min = z_el[e_el];
		}
		e_el++;
	}

	// Find extremes for deltaN (no window required)
	e = 0;
	while ( (z_el[e] < L_base_0) && (e < (nE_s-1)) ) {
		e++;
	}
	for (; e < (nE_s-1); e++) {
		if ( deltaN[e] > deltaN_max ) {
			deltaN_max = deltaN[e];
			z_deltaN_max = z_el[e];
		}
	}

	if ( !((red_hard_max > (-150.*cH)) && (red_hard_min < 1500.)) ) {
		Xdata.S_class1 = -1;
		return false;
	}

	// Classify
	// Max. Hardness at position of maximum
	const double hard_max = red_hard_max + (min_hard + slope_hard*(cH - z_red_hard_max));
	signed char prf_type=-1; // Profile type
	if ( weak_base ) {
		// Position of extremes
		const double pos_max = (z_red_hard_max - L_base)/(cH - L_base);
		// Assign weak profile type
		if ( red_hard_max < 50. ) {
			prf_type = 1;
		} else if ( pos_max <= 0.3 ) {
			prf_type = 4;
		} else if ( pos_max <= 0.7 ) {
			prf_type = 3;
		} else if ( pos_max <= 0.9 ) {
			prf_type = 2;
		} else if ( (pos_max) > 0.9 && (hard_max > thresh_hard) ) {
			prf_type = 5;
		} else {
			prf_type = 4;
		}
	} else {// strong base
		// Position of extremes
		const double pos_max = (z_red_hard_max - L_base)/(cH - L_base);
		const double pos_min = (z_red_hard_min - L_base)/(cH - L_base);
		const double pos_max_deltaN = (z_deltaN_max - L_base)/(cH - L_base);

		// Assign strong profile type
		if ( (pos_max_deltaN > 0.85) && (hard_max > thresh_hard) ) {
			prf_type = 9;
		} else if ( (deltaN_max > 150.) && (pos_max_deltaN > pos_min) ) {
			prf_type = 7;
		} else if ( pos_max < 0.3 ) {
			prf_type = 6;
		} else if ( hard_max > thresh_hard ) {
			if ( fabs(red_hard_max - red_hard_min) < 50. ) {
				prf_type = 10;
			} else if ( red_hard_max < 50. ) {
				prf_type = 8;
			} else {
				prf_type =6;
			}
		} else {
			prf_type = 0;
		}
	}
	// end of classify

	Xdata.S_class1 = prf_type;

	return true;
}

/**
 * @brief DEFAULT: Estimates the critical shear stress based on appropriate parameterisations
 * @param *Edata Xdata->Edata[e+1]
 * @param *Ndata Xdata->Ndata[e+1]
 * @param STpar
 * @param cH Computed height of snow (m)
 * @param cos_sl Cosine of slope angle (1)
 * @param date
 * @return return false on error, true otherwise
 */
bool StabilityAlgorithms::setShearStrengthDEFAULT(const double& cH, const double& cos_sl, const mio::Date& date,
                                        ElementData& Edata, NodeData& Ndata, StabilityData& STpar)
{
	bool prn_wrn = false; //turn to true to print warnings

	const double rho_ri = Edata.Rho/Constants::density_ice; // Snow density relative to ice
	int F1, F2, F3; // Grain shape
	typeToCode(&F1, &F2, &F3, Edata.type); // Determine majority grain shape

	// Determine critical shear stress of element (kPa)
	// 1. Conway
	const double Sig_cC = 19.5*rho_ri*rho_ri;

	// 2. Grain Type dependent mostly from Jamieson & Johnston,
	//    Ann. Glaciol., 26, 296-302 (1998) and Ann. Glaciol., 32, 59-69 (2001)
	double phi = 0.; // Normal load correction
	double Sig_c2 = -1.0; // Critical shear stress (kPa)
	double Sig_c3 = -1.0; // Critical shear stress (kPa)

	switch( F1 ) {
		case 0: // Graupel, from O. Abe, Ann. Glaciol., 38, (2004), size-effect corrected
			Sig_c2 = 0.65*(82.*pow(rho_ri, 2.8));
			phi = 0.08*Sig_c2 + 0.224;
			break;
		case 1: // PP
			Sig_c2 = 2.85*exp(1.13*log(rho_ri));
			phi = 0.08*Sig_c2 + 0.056 + 0.022*STpar.sig_n;
			break;
		case 2: // DF
			Sig_c2 = 8.75*exp(1.54*log(rho_ri));
			phi = 0.08*Sig_c2 + 0.224;
			break;
		case 3: // RG
			Sig_c2 = 7.39*exp(1.20*log(rho_ri));
			phi = 0.08*Sig_c2 + 0.224;
			break;
		case 6: // SH
			switch(Stability::sh_mod) {
				case 0: // original T. Chalmers
					Sig_c2 = 0.336 + 0.0139*(date.getJulian() - Edata.depositionDate.getJulian()) +
							1.18*STpar.sig_n/Optim::pow2(STpar.cos_psi_ref) - 0.625*(cH -
							(Ndata.z + Ndata.u))/cos_sl + 0.0804 *
							cH/cos_sl - 28.7*Edata.L/cos_sl +
							0.0187*IOUtils::K_TO_C(Edata.Te) + 0.0204*Edata.rg;
					break;
				case 1: // original T. Chalmers & accounting for Emin as 2*rg (ml 13 Feb 2003)
					Sig_c2 = 0.336 + 0.0139*(date.getJulian() - Edata.depositionDate.getJulian()) +
							1.18*STpar.sig_n/Optim::pow2(STpar.cos_psi_ref) - 0.625*(cH -
							(Ndata.z + Ndata.u))/cos_sl + 0.0804 *
							cH/cos_sl - 28.7*Edata.L/cos_sl +
							0.0187*IOUtils::K_TO_C(Edata.Te) + 0.0204*2.*Edata.rg;
					break;
				case 2: // New regression by Bruce Jamieson w/o Emin (14 Feb 2003)
					Sig_c2 = 0.429 + 0.0138*(date.getJulian() - Edata.depositionDate.getJulian()) +
							1.12*STpar.sig_n/Optim::pow2(STpar.cos_psi_ref) - 0.596*(cH -
							(Ndata.z + Ndata.u))/cos_sl + 0.0785 *
							cH/cos_sl - 27.1*Edata.L/cos_sl +
							0.0202*IOUtils::K_TO_C(Edata.Te);
					break;
				default:
					Sig_c2 = 1.0;
					break;
			}
			Sig_c2 = std::max(0.1, Sig_c2);
			Sig_c3 = 84.*exp(2.55*log(rho_ri));
			break;
		case 7: // MF
			Sig_c2 = 21.*exp(1.24*log(rho_ri));
			phi = 0.08*Sig_c2 + 0.224;
			break;
		default: // FC, DH, FCmx
			Sig_c2 = 18.5*exp(2.11*log(rho_ri));
			assert(STpar.sig_n>0.); //in a few cases, we have received sig_n<0 from the caller
			if (STpar.sig_n>0.)
				Sig_c3 = 1.36*exp(0.55*log(STpar.sig_n/STpar.cos_psi_ref));
			// phi = 0.08*Sig_c2 + 0.224;
			// Above correction not used by Jamieson & Johnston (1998), but considered by Lehning et al., Ann. Glaciol., 38, 331-338 (2004)
			break;
	}

		// Hack for MFCs
	if ( Edata.mk % 100 >= 20 ) {
		Sig_c2 = Sig_c3 = 4.;
	}

	// Final assignements
	Edata.s_strength = Sig_c2;
	STpar.Sig_c2 = std::min(Sig_c2, STpar.strength_upper);
	STpar.strength_upper = Sig_c2;
	STpar.phi = phi;

	// Warning message may be enabled for large differences in snow shear stength models
	if (prn_wrn
		    && (((fabs(Sig_c2-Sig_cC)/Sig_cC) > 10.) || ((Sig_c3 > 0.)
		        && ((fabs(Sig_c3-Sig_cC)/Sig_cC > 10.)))) ) {
		prn_msg( __FILE__, __LINE__, "wrn", date,"Large difference in Snow Shear Stength (type=%d)", F1);
		prn_msg(__FILE__, __LINE__, "msg-", Date(), "Conway: %lf Sig_c2: %lf Sig_c3: %lf\n", Sig_cC, Sig_c2, Sig_c3);
		return false;
	} else {
		return true;
	}
}

/**
 * @brief STRENGTH_NIED: Estimates the critical shear stress based on appropriate parameterisations adapted for Japan
 * @param *Edata Xdata->Edata[e+1]
 * @param *Ndata Xdata->Ndata[e+1]
 * @param STpar
 * @param cH Computed height of snow (m)
 * @param cos_sl Cosine of slope angle (1)
 * @param date
 * @return return false on error, true otherwise
 */
bool StabilityAlgorithms::setShearStrength_NIED(const double& cH, const double& cos_sl, const mio::Date& date,
                                              ElementData& Edata, NodeData& Ndata, StabilityData& STpar)
{
	bool prn_wrn = false;
	const double rho_ri = Edata.Rho/Constants::density_ice; // Snow density relative to ice
	// Determine majority grain shape
	int    F1, F2, F3;             // Grain shape
	typeToCode(&F1, &F2, &F3, Edata.type);

	// Determine critical shear stress of element (kPa)
	// 1. Conway
	const double Sig_cC = 19.5*rho_ri*rho_ri;
	// 2. Grain Type dependent mostly from Jamieson,
	//    Ann. Glaciol., 26, 296-302 (2001) and Ann. Glaciol., 32, 59-69 (1998)
	double phi = 0.; // Normal load correction
	double Sig_c2 = -1.0; // Critical shear stress (kPa)
	double Sig_c3 = -1.0; // Critical shear stress (kPa)
	switch ( F1 ) {
		case 0: // Graupel, from O. Abe, Ann. Glaciol. 38 (2004), size-effect corrected
			Sig_c2 = 0.65*(82.*pow(rho_ri, 2.8));
			phi = 0.08*Sig_c2 + 0.224;
			break;
		case 1: // PP //NIED (H. Hirashima)
			Sig_c2= 9.4*0.0001*pow(Edata.theta[ICE]*Constants::density_ice,2.91)*exp(-0.235*Edata.theta[WATER]*100.)/1000.;
			phi = 0.08*Sig_c2 + 0.056 + 0.022*STpar.sig_n;
			break;
		case 2: case 3: // DF & RG //NIED (H. Hirashima)
			Sig_c2= 9.4*0.0001*pow(Edata.theta[ICE]*Constants::density_ice,2.91)*exp(-0.235*Edata.theta[WATER]*100.)/1000.;
			phi = 0.08*Sig_c2 + 0.224;
			break;
		case 6: // SH
			switch(Stability::sh_mod) {
				case 0: // original T. Chalmers
					Sig_c2 = 0.336 + 0.0139*(date.getJulian() - Edata.depositionDate.getJulian()) +
							1.18*STpar.sig_n/Optim::pow2(STpar.cos_psi_ref) - 0.625*(cH -
							(Ndata.z + Ndata.u))/cos_sl + 0.0804 *
							cH/cos_sl - 28.7*Edata.L/cos_sl +
							0.0187*IOUtils::K_TO_C(Edata.Te) + 0.0204*Edata.rg;
					break;
				case 1: // original T. Chalmers & accounting for Emin as 2*rg (ml 13 Feb 2003)
					Sig_c2 = 0.336 + 0.0139*(date.getJulian() - Edata.depositionDate.getJulian()) +
							1.18*STpar.sig_n/Optim::pow2(STpar.cos_psi_ref) - 0.625*(cH -
							(Ndata.z + Ndata.u))/cos_sl + 0.0804 *
							cH/cos_sl - 28.7*Edata.L/cos_sl +
							0.0187*IOUtils::K_TO_C(Edata.Te) + 0.0204*2.*Edata.rg;
					break;
				case 2: // New regression by Bruce Jamieson w/o Emin (14 Feb 2003)
					Sig_c2 = 0.429 + 0.0138*(date.getJulian() - Edata.depositionDate.getJulian()) +
							1.12*STpar.sig_n/Optim::pow2(STpar.cos_psi_ref) - 0.596*(cH -
							(Ndata.z + Ndata.u))/cos_sl + 0.0785 *
							cH/cos_sl - 27.1*Edata.L/cos_sl +
							0.0202*IOUtils::K_TO_C(Edata.Te);
					break;
				default:
					Sig_c2 = 1.0;
					break;
			}
			Sig_c2 = std::max(0.1, Sig_c2);
			Sig_c3 = 84.*exp(2.55*log(rho_ri));
			break;
		case 7: // MF //NIED (H. Hirashima)
			Sig_c2= 4.97*0.0001*pow(Edata.theta[ICE]*Constants::density_ice,2.91)*exp(-0.235*Edata.theta[WATER]*100.)/1000.;
			phi = 0.08*Sig_c2 + 0.224;
			break;
		default: // FC, DH, FCmx
			Sig_c2 = 18.5*exp(2.11*log(rho_ri));
			//Sig_c2 = 0.0391*exp(0.0141*Edata.theta[ICE]*Constants::density_ice); //NIED (H. Hirashima)
			Sig_c3 = 1.36*exp(0.55*log(STpar.sig_n/STpar.cos_psi_ref));
			// phi = 0.08*Sig_c2 + 0.224;
			// Above correction not used by Jamieson & Johnston (1998), but considered by Lehning et al., Ann. Glaciol., 38, 331-338 (2004)
			break;
	}

	// Hack for MFCs; not used by //NIED (H. Hirashima)

	// Final assignements
	//NIED (H. Hirashima)
	const double Sig_ET = 9.4*0.0001*pow(Edata.theta[ICE]*Constants::density_ice,2.91)*exp(-0.235*Edata.theta[WATER]*100.)/1000.;
	const double Sig_DH = 2.3*0.0001*pow(Edata.theta[ICE]*Constants::density_ice,2.78)*exp(-0.235*Edata.theta[WATER]*100.)/1000.;
	Ndata.Sigdsm = Sig_ET - Edata.dsm*(Sig_ET - Sig_DH);
	Ndata.S_dsm = (Ndata.Sigdsm + phi*STpar.sig_n)/STpar.sig_s;
	// original SNOWPACK
	Edata.s_strength = Sig_c2;
	STpar.Sig_c2 = std::min(Sig_c2, STpar.strength_upper);
	STpar.strength_upper = Sig_c2;
	STpar.phi = phi;

	// Warning message may be enabled to warn for large differences in snow shear stength models
	if (prn_wrn
	        && (((fabs(Sig_c2-Sig_cC)/Sig_cC) > 10.) || ((Sig_c3 > 0.)
	            && ((fabs(Sig_c3-Sig_cC)/Sig_cC > 10.)))) ) {
		prn_msg( __FILE__, __LINE__, "wrn", date,"Large difference in Snow Shear Stength (type=%d)", F1);
		prn_msg(__FILE__, __LINE__, "msg-", Date(), "Conway: %lf Sig_c2: %lf Sig_c3: %lf\n", Sig_cC, Sig_c2, Sig_c3);
		return false;
	} else {
		return true;
	}
}


/**
 * @brief Critical cut length: Estimates the critical cut length
 * @author Johan Gaume, Nander Wever
 * @date 2016-20-01
 * @param H_slab Slab depth (m)
 * @param rho_slab Slab density (kg/m^3)
 * @param *Edata Xdata->Edata[e-1]
 * @param cos_sl cosinus of the simulated slope angle
 * @param STpar
 * @param stress stress to apply to the current slab, usually the sum of all layers above
 * @return return critical cut length (m)
 */
double StabilityAlgorithms::CriticalCutLength(const double& H_slab, const double& rho_slab, const double& cos_sl, const ElementData& Edata, const StabilityData& STpar, const double& stress)
{
	const double sin_sl = sqrt( 1. - mio::Optim::pow2(cos_sl) );
	const double D = H_slab;
	const double tau_p = STpar.Sig_c2;
	const double sigma_n = - stress / 1000.; 
	const double tau_g = sigma_n * sin_sl / cos_sl; 
	const double E = ElementData::getYoungModule(rho_slab, ElementData::Exp);
	
	const double E_prime = E / (1. - 0.2*0.2);	// 0.2 is poisson ratio
	static const double G_wl = 2e5;
	const double Dwl = Edata.L;
	const double lambda = sqrt( (E_prime * D * Dwl) / (G_wl) );
	const double sqrt_arg = Optim::pow2(tau_g) + 2.*sigma_n*(tau_p - tau_g);
	if (sqrt_arg<0.) return 0.;
	
	const double crit_length = lambda * (-tau_g + sqrt(sqrt_arg)) / sigma_n;
	
	//this will be needed to compute the propagation distance
	//const double sig_xx = rho_slab * Constants::g * (crit_length / 1.5) * STpar.sin_psi_ref + (3. * rho_slab * Constants::g * (crit_length / 1.5) * (crit_length / 1.5))/D;
	//const double sig_t = 2.4E5 * pow((rho_slab / Constants::density_ice), 2.44);
	//Edata.crit_length = (sig_xx > sig_t) ? 6.0 : (crit_length);
	 
	return (H_slab < Stability::minimum_slab || crit_length > 3.) ? (3.) : (crit_length);
}
