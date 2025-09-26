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
#ifndef STABILITYALGORITHMS_H
#define STABILITYALGORITHMS_H

#include <snowpack/DataClasses.h>

/**
 * @class StabilityData
 * @brief Layer shear strength evaluation parameters. 
 * This class contains layer properties useful for the shear strength evaluation.
 * 
 * @ingroup data_structures
 */
class StabilityData {
	public:
		/** @brief StabilityData constructor. 
		 * @param i_psi_ref slope angle to use for the stability evaluation (in degrees)
		 * @note alpha_max(38.) = 54.3 deg (J. Schweizer, IB 712, SLF)
		 */
		StabilityData(const double& i_psi_ref) :  Sig_c2(Constants::undefined), strength_upper(1001.), phi(0.0), 
		                                                                  sig_n(Constants::undefined), sig_s(Constants::undefined),
		                                                                  alpha_max_rad(54.3*mio::Cst::to_rad), psi_ref(i_psi_ref*mio::Cst::to_rad), cos_psi_ref(cos(i_psi_ref*mio::Cst::to_rad)), sin_psi_ref(sin(i_psi_ref*mio::Cst::to_rad)) {}

		double Sig_c2;         ///< Element shear strength (kPa)
		double strength_upper; ///< Shear strength of adjacent upper element
		double phi;            ///< Correction to normal load
		double sig_n;          ///< Normal load on upper element node, perpendicular to slope
		double sig_s;          ///< Shear stress on upper element node, parallel to slope
		double alpha_max_rad;  ///< Angle from snow surface to peak shear stress, 54.3 at 38 deg
		double psi_ref;        ///< Reference slope angle in radian, corresponds usually to 38 deg
		double cos_psi_ref;    ///< Cosine of psi_ref
		double sin_psi_ref;    ///< Sine of psi_ref
};

/** @brief Implementations of various algorithms useful for evaluating the stability.
 * These algorithms fall within the following categories:
 *    + the structural stability, that selects the potential weak layer, for example the SK38, SSI or Relative Threshold;
 *    + the stability index, based on a strength/stress relation, such as the natural stability index. This evaluates the stability of a selected layer;
 *    + the critical crack length that also evaluates the stability of a selected layer, for example the critical cut length or the anti-crack model.
 *
 * These methods will then be used by the Stability class to effectively compute the stability for a given profile, according to the user's configuration.
 * @ingroup postprocessing
 */
class StabilityAlgorithms {
	public:
		static void classifyStability_SchweizerBellaire(const double& Swl_ssi, const double& Swl_Sk38, SnowStation& Xdata);
		static void classifyStability_Bellaire(const double& Swl_ssi, SnowStation& Xdata);
		static void classifyStability_SchweizerBellaire2(const double& Swl_ssi, const size_t& Swl_lemon, const double& Swl_Sk38, SnowStation& Xdata);
		static bool classifyStability_SchweizerWiesinger(SnowStation& Xdata);
		static bool classifyType_SchweizerLuetschg(SnowStation& Xdata);
		
		static bool setShearStrengthDEFAULT(const double& cH, const double& cos_sl, const mio::Date& date,
		                             ElementData& Edata, NodeData& Ndata, StabilityData& STpar);
		static bool setShearStrength_NIED(const double& cH, const double& cos_sl, const mio::Date& date,
		                             ElementData& Edata, NodeData& Ndata, StabilityData& STpar);
		
		static double getHandHardnessBELLAIRE(const ElementData& Edata, const double& buried_hoar_density);
		static double getHandHardnessASARC(const ElementData& Edata, const double& buried_hoar_density);
		static double getHandHardnessMONTI(const ElementData& Edata, const double& buried_hoar_density);

		static double getHandHardnessMONTI(const int& F, const double& rho, const double& water_content, const double& buried_hoar_density);
		static double compCriticalStress(const double& epDotn, const double& T_s);
		static double setDeformationRateIndex(ElementData& Edata);
		static double compPenetrationDepth(const SnowStation& Xdata);
		static void compReducedStresses(const double& stress, const double& cos_sl, StabilityData& STpar);

		static double getNaturalStability(const StabilityData& STpar);
		static double getLayerSkierStability(const double& penetrationDepth, const double& depth_lay, const StabilityData& STpar);
		static bool getRelativeThresholdSum(SnowStation& Xdata);
		
		static double CriticalCutLength(const double& H_slab, const double& rho_slab, const double& cos_sl, const ElementData& Edata, const StabilityData& STpar, const double& stress);
	private:
		static bool normalizeLemon(std::vector<double>& vecData);
};

#endif
