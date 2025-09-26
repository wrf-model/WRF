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

#ifndef CANOPY_H
#define CANOPY_H

#include <snowpack/DataClasses.h>

#include <string>
#include <fstream>

/**
 * @brief Computes interception of precipitation and radiation, and reduction of windspeed
 * in a canopy layer above thesnow or soil surface.
 * This has been published in Gouttevin, I., M. Lehning, T. Jonas, D. Gustafsson, and Meelis Mölder,
 * <i>"A two-layer canopy model with thermal inertia for an improved snowpack energy balance below needleleaf forest
 * (model SNOWPACK, version 3.2. 1, revision 741)."</i>, Geoscientific Model Development <b>8.8</b>, pp 2379-2398, 2015.
 *
 * @section canopy_modeling Canopy modeling
 * -# 2layer canopy model:
 *	- key: TWO_LAYER_CANOPY = true [Snowpack]
 *	- logical in the code: Twolayercanopy
 *	- content: canopy is divided into a trunk layer (interception radiations with factor sigftrunk) and
 *		a leaf-layer (intercepting radiations with factor sigf). SW radiations reaching the ground are modified
 *		accordingly. An energy balance is computed for each layer, producing leaf-layer and trunk layer
 *		temperatures (TC and Ttrunk) that affect LW radiations to the ground.
 *		Optionally, trunks can get direct solar insolation (important for sparse canopies), look for
 *		CanClosDirTrunks in the code
 *	- further details: Gouttevin et al. (2014): A two-layer canopy with thermal inertia for an improved modelling
 *		of the sub-canopy snowpack energy-balance (in prep).
 *
 * -# canopy heat mass:
 *	- key: CANOPY_HEAT_MASS = true [Snowpack]
 *	- logical in the code: CanopyHeatMass
 *	- content: the canopy gets an heat mass (whole, or separate between trunks and leaves if Twolayercanopy) that
 *		adds a biomass heat flux term in the canopy energy balance.
 *	- an additionnal parameter is now required in the input/station.snoold file : CanopyBasalArea (m2/m2),
 *		to be placed after CanopyLeafAreaIndex. Example value for closed canopies like Alptal : 0.004.
 *	- further details: Gouttevin et al. (2014): A two-layer canopy with thermal inertia for an improved modelling
 *		of the sub-canopy snowpack energy-balance (in prep).
 *
 * -# forest-floor albedo:
 *	- key: FORESTFLOOR_ALB = true [Snowpack]
 *	- logical in the code: forestfloor_alb
 *	- content: Litter falling on the forest floor can reduce albedo. This effect is currently parameterized
 *		through an exponential decay of the albedo to a value of 0.3 with a time-constant of 7 days,
 *		based on parameterizations commonly used in Land-Surface models.
 *		There is room for improvement !
 *
 * @section canopy_comments Important comments:
 *	- Snowpack can take precipitation phase (relying on the psum_ph variable) for applications such as the
 *	  SnowMIP experiments (Rutter et al., 2009).
 *	- an additionnal parameter is now required in the input/station.snoold file : CanopyBasalArea (m2/m2),
 *	  to be placed after CanopyLeafAreaIndex.
 *	- Some cleaning was done to suppressed outputs that can be easily derived from other outputs.
 *	  There is now space for outputs specific to the 2layer model, which are written if variant = 2L_CANOPY in
 *	  [SnowpackAdvanced] (Canopy::writeTimeSeriesAdd2LCanopy).
 *
 */

class Canopy {

 	public:
		Canopy(const SnowpackConfig& i_cfg);

		static void DumpCanopyHeader(std::ofstream &fout);
		static void DumpCanopyUnits(std::ofstream &fout);
		static void DumpCanopyData(std::ofstream &fout, const CanopyData *Cdata,
                          const SurfaceFluxes *Sdata, const double cos_sl);
		bool runCanopyModel(CurrentMeteo &Mdata, SnowStation &Xdata,
                          const double& roughness_length, const double& height_of_wind_val,
                          const bool& adjust_VW_height=true);
		static void writeTimeSeriesAdd2LCanopy(std::ofstream &fout, const CanopyData *Cdata);
		//static const double can_alb_dry, can_alb_wet, can_alb_snow, krnt_lai; //public constants

 	private:
		static double get_f1(const double& ris);

		static double RootFraction(const double& zupper, const double& zlower, const double rootdepth);

		void SoilWaterUptake(const size_t& SoilNode, const double& transpiration,
                        ElementData* EMS, const double wp_fraction,
                        const double rootdepth, const double h_wilt) const;

		static double get_f4(const double& tempC);

		static double get_f2f4(const size_t& SoilNode, ElementData* EMS,
                          const double wp_fraction,const double rootdepth);

		static double get_f3(const double& vpd, const double f3_gd);

		double IntCapacity(const CurrentMeteo& Mdata, const SnowStation& Xdata,
                        const bool& force_rain=false) const;

		static double IntUnload(const double& capacity, const double& storage);

		static double IntRate(const double& capacity, const double& storage, const double& prec,
		                  const double& direct, const double interception_timecoef);

		static double CanopyAlbedo(const double& tair, const double& wetfrac, const SnowStation& Xdata);

		static double TotalAlbedo(double CanAlb, double sigf, double SurfAlb, double DirectThroughfall,
		                      double CanopyClosureDirect, double RadFracDirect, double sigfdirect);

		static double CanopyShadeSoilCover(const double& HEIGHT, const double& COVER, const double& ELEV, const double& can_diameter);

		static double CanopyWetFraction(const double& capacity, const double& storage);

		static double CanopyTransmissivity(const double& lai, const double& elev, const double krnt_lai);

		void LineariseNetRadiation(const CurrentMeteo& Mdata,const CanopyData& Cdata, const SnowStation& Xdata,
		                              double& iswrac, double& rsnet, double& ilwrac, double& r0,double& r1,
		                              const double& canopyalb, double& CanopyClosureDirect, double& RadFracDirect,
		                              const double& sigfdirect, double& r1p) const;

		void LineariseNetRadiation2L(const CurrentMeteo& Mdata, const CanopyData& Cdata, const SnowStation& Xdata,
                                      double& iswrac, double& rsnet, double& ilwrac, double& r0,double& r1, double& r2,
                                      double& rt0, double& rt1, double& rt2, const double& canopyalb, double& CanopyClosureDirect, double& RadFracDirect,
                                      const double& sigfdirect, const double& sigftrunkdirect, double& r1p, double& r2p) const;

		static void LineariseSensibleHeatFlux(const double& ch_canopy, const double& tair, double& h0, double& h1, double scalingfactor);

		static double DSaturationPressureDT(const double& L, const double& T);

		static void LineariseLatentHeatFlux(const double& ce_canopy, const double& tc_old, const double& vpair,
		                                double& le0, double& le1, double scalingfactor);

		static void CalculateHeatMass(const double& height, const double& BasalArea, double& lai ,double& HMLeaves,
						double& HMTrunks, const double biomass_density, const double biomass_heat_capacity);

		void LineariseConductiveHeatFlux(const double& tc_old, const double& HM, double& HM0, double& HM1,  const double& DT, const double& scalingfactor) const;

		static void CanopyEnergyBalance(const double& h0, const double& h1, const double& le0,
                                                         const double& le1, const double& HM0,  const double& HM1,
                                                         const double& ce_canopy,
                                                         const double& ce_condensation,
                                                         double& r0, double& r1, double& TCANOPY, double& RNCANOPY,
                                                         double& HCANOPY, double& LECANOPY);

		static void CanopyEnergyBalance2L(double& h0, double& h1, double& le0,
                                                         double& le1, double& HM0, double& HM1, double& TT0, double& TT1,
                                                         const double& ce_canopy,
                                                         const double& ce_condensation,
                                                         double& r0, double& r1, double& r2, double& TCANOPY, double& Ttrunk, double& RNCANOPY,
                                                         double& HCANOPY, double& LECANOPY);

		static void CanopyEvaporationComponents(const double& ce_canopy,
                                      const double& ce_transpiration, double& LECANOPY,
                                      const double& ta, const double& I, const double DT,
                                      double& CanopyEvaporation,
                                      double& INTEVAP, double& TRANSPIRATION,
                                      double& RNCANOPY, double& HCANOPY,double& TCANOPY,
                                      const double& r0, const double& r1, const double& h0, const double& h1,
                                      double& LECANOPYCORR,
                                      const double& wetfraction, const double& HM0, const double& HM1);

		static void CanopyEvaporationComponents2L(const double& ce_canopy,
                                      const double& ce_transpiration, double& LECANOPY,
                                      const double& ta, const double& I, const double DT,
                                      double& CanopyEvaporation,
                                      double& INTEVAP, double& TRANSPIRATION,
                                      double& RNCANOPY, double& HCANOPY,double& TCANOPY, double& Ttrunk,
                                      const double& TT0, const double& TT1,
                                      const double& r0, const double& r1, const double& r2, const double& h0, const double& h1,
                                      double& LECANOPYCORR,
                                      const double& wetfraction,
                                      const double& HM0, const double& HM1);

		static double get_psim(const double& xi);

		static double get_psih(const double& xi);

		static double RichardsonToAeta(double za, double TempAir, double DiffTemp, double Windspeed, double zom, double zoh, int maxitt);

		void CanopyTurbulentExchange(const CurrentMeteo& Mdata, const double& refheight, const double& zomg,
								  const double& wetfraction, SnowStation& Xdata, double& ch_canopy,
								  double& ce_canopy, double& ce_transpiration,
								  double& ce_interception, double& ce_condensation) const;

		void CanopyRadiationOutput(SnowStation& Xdata, const CurrentMeteo& Mdata, double ac,
								double &iswrac, double &rswrac,
								double &iswrbc, double &rswrbc, double &ilwrac,
								double &rlwrac, double &ilwrbc, double &rlwrbc,
								double CanopyClosureDirect, double RadFracDirect, double sigfdirect, double sigftrunkdirect) const;

		std::string hn_density, hn_density_parameterization, variant, watertransportmodel_soil;
		double hn_density_fixedValue, calculation_step_length;
		bool useSoilLayers;
		// variables for canopy heat mass and 2-layer canopy
		bool CanopyHeatMass;
		bool Twolayercanopy, Twolayercanopy_user;
		bool canopytransmission;
		bool forestfloor_alb;
};

#endif
