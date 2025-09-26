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

#include <snowpack/snowpackCore/Canopy.h>
#include <snowpack/Constants.h>
#include <snowpack/Utils.h>
#include <snowpack/Laws_sn.h>

#include <meteoio/MeteoIO.h>
#include <assert.h>

using namespace std;
using namespace mio;

/************************************************************
 * static section                                           *
 ************************************************************/

/**
 * @brief Write header for 28 canopy parameters to Outfile, columns 65-92
 * @param fout Dump file stream
 */
void Canopy::DumpCanopyHeader(std::ofstream &fout)
{
	// 28 canopy fields (27 assigned, 1 empty at the end)

	// PRIMARY "STATE" VARIABLES
	fout << ",Interception storage";
	fout << ",Canopy surface temperature";

	// SECONDARY "STATE" VARIABLES
	fout << ",Canopy albedo";
	fout << ",Wet fraction";
	fout << ",Interception capacity";

	// RADIATIVE FLUXES (W m-2)
	fout << ",Net shortwave radiation absorbed by canopy";
	fout << ",Net longwave radiation absorbed by canopy";
	fout << ",Net radiation to canopy";

	// HEAT FLUXES CANOPY (W m-2)
	fout << ",Sensible heat flux to canopy";
	fout << ",Latent heat flux to canopy";
	fout << ",Biomass heat storage flux towards Canopy";

	// WATER FLUXES CANOPY (kg m-2)
	fout << ",Transpiration of the canopy";
	fout << ",Evaporation and sublimation of interception (liquid and frozen)";
	fout << ",Interception rate";
	fout << ",Throughfall";
	fout << ",Snow unload";

	// TOTAL SURFACE FLUXES,EVAPORATION; ETC
	fout << ",Longwave radiation up above canopy";
	fout << ",Longwave radiation down above canopy";
	fout << ",Shortwave radiation up above canopy";
	fout << ",Shortwave radiation down above canopy";
	fout << ",Total land surface albedo";
	fout << ",Total net radiation to the surface (ground + canopy)";
	fout << ",Surface radiative temperature (ground + canopy)";
	fout << ",Forest floor albedo";
	fout << ",Snowfall rate Above Canopy";
	fout << ",Rainfall rate Above Canopy";
	fout << ",Evapotranspiration of the total surface (ground + canopy)";
	fout << ",";	//Note: 1 empty field here!
	return;
}

/**
 * @brief Write units for 28 canopy parameters to Outfile, columns 65-92
 * @param fout Dump file stream
 */
void Canopy::DumpCanopyUnits(std::ofstream &fout)
{
	// 28 canopy fields (27 assigned, 1 empty at the end)

	// PRIMARY "STATE" VARIABLES
	fout << ",kg m-2,degC";

	// SECONDARY "STATE" VARIABLES
	fout << ",-,-,kg m-2";

	// RADIATIVE FLUXES (W m-2)
	fout << ",W m-2,W m-2,W m-2";

	// HEAT FLUXES CANOPY (W m-2)
	fout << ",W m-2,W m-2,W m-2";

	// WATER FLUXES CANOPY (kg m-2)
	fout << ",kg m-2 per timestep,kg m-2 per timestep,kg m-2 per timestep,kg m-2 per timestep,kg m-2 per timestep";

	// TOTAL SURFACE FLUXES,EVAPORATION; ETC
	fout << ",W m-2,W m-2,W m-2,W m-2,-,W m-2,degC,-,kg m-2 per timestep,kg m-2 per timestep,kg m-2 per timestep";
	fout << ",";	//Note: 1 empty field here!
	return;
}

/**
 * @brief Dump 28 canopy parameters to Outfile, columns 65-92
 * @param fout Dump file stream
 * @param *Cdata
 * @param *Sdata
 * @param cos_sl Cosine of slope angle
 */
void Canopy::DumpCanopyData(std::ofstream &fout, const CanopyData *Cdata, const SurfaceFluxes *Sdata, const double cos_sl)
{
	// PRIMARY "STATE" VARIABLES
	fout << "," << Cdata->storage/cos_sl;        // intercepted water (mm or kg m-2)
	fout << "," << IOUtils::K_TO_C(Cdata->temp); // temperature (degC)

	// SECONDARY "STATE" VARIABLES
	fout << "," << Cdata->canopyalb;             // albedo (1)
	fout << "," << Cdata->wetfraction;           // wet fraction
	fout << "," << Cdata->intcapacity/cos_sl;    // interception capacity (kg m-2)

	// RADIATIVE FLUXES (W m-2)
	fout << "," << Cdata->rsnet;                 // net shortwave radiation to canopy
	fout << "," << Cdata->rlnet;                 // net longwave radiation to canopy
	fout << "," << Cdata->rsnet+Cdata->rlnet;    // net radiation to canopy

	// HEAT FLUXES CANOPY (W m-2)
	fout << "," << -Cdata->sensible;             // sensible heat flux to canopy (>0 towards canopy)
	fout << "," << -Cdata->latentcorr;           // latent heat flux to canopy (>0 towards canopy)
	fout << "," << Cdata->CondFluxCanop;         // biomass heat storage flux towards Canopy

	// WATER FLUXES CANOPY (kg m-2)
	fout << "," << Cdata->transp/cos_sl;         // transpiration
	fout << "," << Cdata->intevap/cos_sl;        // interception evaporation
	fout << "," << Cdata->interception/cos_sl;   // interception
	fout << "," << Cdata->throughfall/cos_sl;    // throughfall
	fout << "," << Cdata->snowunload/cos_sl;     // unload of snow

	// TOTAL SURFACE FLUXES,EVAPORATION; ETC
	fout << "," << Cdata->rlwrac;                // upward longwave radiation ABOVE canopy
	fout << "," << Cdata->ilwrac;                // downward longwave radiation ABOVE canopy
	fout << "," << Cdata->rswrac;                // upward shortwave above canopy
	fout << "," << Cdata->iswrac;                // downward shortwave radiation above canopy
	fout << "," << Cdata->totalalb;              // total albedo [-]
	fout << "," << Cdata->rlnet+Sdata->lw_net+Cdata->rsnet+Sdata->qw; // net radiation to the total surface
	fout << "," << IOUtils::K_TO_C(pow(Cdata->rlwrac/Constants::stefan_boltzmann, 0.25)); // surface (ground + canopy) temperature
	fout << "," << Cdata->forestfloor_alb;       // albedo of the forest floor [-]
	fout << "," << Cdata->snowfac/cos_sl;        // snowfall rate above canopy (mm per output timestep)
	fout << "," << Cdata->rainfac/cos_sl;        // rainfall rate above canopy (mm per output timestep)
	fout << "," << (Cdata->transp+Cdata->intevap-(Sdata->mass[SurfaceFluxes::MS_SUBLIMATION]+Sdata->mass[SurfaceFluxes::MS_EVAPORATION]))/cos_sl;//       evapotranspiration of total surface (mm h-1)
	fout << ",";                                 // 1 empty field here
}
//TODO: this function is not yet integrated into AsciiIO, and now this function is actually never called by any routine.
void Canopy::writeTimeSeriesAdd2LCanopy(std::ofstream &fout, const CanopyData *Cdata)
{
	fout << "," << IOUtils::K_TO_C(Cdata->Ttrunk); // Trunk temperature (degC)
	fout << "," << Cdata->CondFluxTrunks;          // Trunk biomass heat storage flux (W m-2)
	fout << "," << Cdata->LWnet_Trunks;            // net LW radiations to Trunk layer (W m-2)
	fout << "," << Cdata->SWnet_Trunks;            // net SW radiations to Trunk layer (W m-2)
	fout << "," << -Cdata->QStrunks;               // sensible heat flux to trunk layer  (W m-2), (>0 towards trunks)
	fout << ",,,";
}
/****i*******************************************************
 * non-static section                                       *
 ************************************************************/
Canopy::Canopy(const SnowpackConfig& cfg)
        : hn_density(), hn_density_parameterization(), variant(), watertransportmodel_soil(),
          hn_density_fixedValue(Constants::undefined), calculation_step_length(0.), useSoilLayers(false),
          CanopyHeatMass(true), Twolayercanopy(true), Twolayercanopy_user(true), canopytransmission(true), forestfloor_alb(true)
{
	cfg.getValue("VARIANT", "SnowpackAdvanced", variant);
	cfg.getValue("SNP_SOIL", "Snowpack", useSoilLayers);
	cfg.getValue("CALCULATION_STEP_LENGTH", "Snowpack", calculation_step_length);
	cfg.getValue("HN_DENSITY", "SnowpackAdvanced", hn_density);
	cfg.getValue("HN_DENSITY_PARAMETERIZATION", "SnowpackAdvanced", hn_density_parameterization);
	cfg.getValue("HN_DENSITY_FIXEDVALUE", "SnowpackAdvanced", hn_density_fixedValue);
	cfg.getValue("WATERTRANSPORTMODEL_SOIL", "SnowpackAdvanced", watertransportmodel_soil);
	cfg.getValue("CANOPY_HEAT_MASS", "SnowpackAdvanced", CanopyHeatMass);
	cfg.getValue("CANOPY_TRANSMISSION", "SnowpackAdvanced", canopytransmission);
	cfg.getValue("TWO_LAYER_CANOPY", "SnowpackAdvanced", Twolayercanopy_user);
	Twolayercanopy = Twolayercanopy_user;
	cfg.getValue("FORESTFLOOR_ALB", "SnowpackAdvanced", forestfloor_alb);
}

/**
 * @brief multiplicative increase of canopy surface resistance as
 * a function of downward solar radiation (van den Burk et al (2000):
 * Offline validation of the ERA40 surface scheme, ECMWF Tech.Mem.295)
 * @param ris
 * @return double
 */
double Canopy::get_f1(const double& ris)
{
	static const double a = 0.81;
	static const double b = 0.004;
	static const double c = 0.05;
	const double f1 = ( a * ( 1.0 + b * ris ) ) / ( b * ris + c );
	if (f1 < 1.0) {
		return 1.0;
	}
	return (f1);

}

/**
 * @brief Computes the FRACTION OF ROOTS in a soil layer between zupper and zlower meters
 * below the soil surface (van den Burk et al (2000): Offline validation of the ERA40
 * surface scheme, ECMWF Tech.Mem.295)
 * @param zupper
 * @param zlower
 * @return double
 */
double Canopy::RootFraction(const double& zupper, const double& zlower, const double rootdepth)
{
	double rf = 0.0;

	// Constants.h: Xdata.Cdata.rootdepth, default 0.5
	if ( zupper < rootdepth ) {
		static const double ar = 6.706; // evergreen needleleaf trees
		static const double br = 2.175; // evergreen needleleaf trees
		// fraction of roots below root depth (according to exponential distribution)
		const double tail = 0.5 * (exp(-ar * rootdepth)+ exp(-br * rootdepth));
		// multiplicative factor to distribute tail on layers above root depth
		rf = ( ( 1. + tail / ( 1. - tail ) ) * 0.5 *
			(exp(-ar * zupper) + exp(-br * zupper)
			-exp(-ar * std::min(zlower, rootdepth))
			-exp(-br * std::min(zlower, rootdepth))));
	}

	return (rf);
}


/**
 * @brief Computes ROOT WATER UPTAKE in soil layers above rootdepth.
 * Theory:
	- 1) Transpiration is partitioned on soil layers according to fraction of roots.
	- 2) Root water uptake is limited to the plant available water in each layer,
	- defined by the wilting point and the field capacity.
	- 3) Wilting point and field capacity is dependent on soil type:
	- Fieldcapacity = ElementData::soilFieldCapacity
	- Wilting point = WP_FRACTION * Fieldcapacity
 * Last update: David Gustafsson, 2005-03-16.
 * @param SoilNode
 * @param *EMS
 * @param transpiration
 */
void Canopy::SoilWaterUptake(const size_t& SoilNode, const double& transpiration,
				ElementData* EMS, const double wp_fraction,
				const double rootdepth, const double h_wilt) const
{
	// transpiration [mm]
	if ( transpiration == 0. ) return;

	// Mass of water [kg m-2] that is to be extracted from the soil
	double waterresidual = transpiration;
	double waterresidual_real = transpiration;

	// Loop over soil layers above rootdepth
	double zupper = 0.;
	size_t RootLayer = SoilNode;
	for (size_t e = SoilNode; e --> 0; ) {//e gets decremented right away -> start at SoilNode
		// fraction of roots in layer
		const double rootfr = RootFraction(zupper, zupper + EMS[e].L, rootdepth);
		const double water = transpiration;
		if (rootfr > 0.0 ){
			// Index of last layer with roots
			RootLayer = e;

			// Change in volumetric water content in layer
			double d_theta_l = 0.;
			if (watertransportmodel_soil == "RICHARDSEQUATION" && EMS[e].VG.defined == true) {
				const double theta_wilt = EMS[e].VG.fromHtoTHETAforICE(h_wilt, EMS[e].theta[ICE]);
				d_theta_l = std::min( std::max(0., ( EMS[e].theta[WATER] -
				    theta_wilt )),
				    rootfr*water / ( Constants::density_water * EMS[e].L ) );
			} else {
				d_theta_l = std::min( std::max(0., ( EMS[e].theta[WATER] -
				    wp_fraction * EMS[e].soilFieldCapacity() )),
				    rootfr*water / ( Constants::density_water * EMS[e].L ) );
			}

			// residual water to be extracted in layers below
			waterresidual -= rootfr * water;
			waterresidual_real -= d_theta_l * Constants::density_water * EMS[e].L;

			if (watertransportmodel_soil == "RICHARDSEQUATION") {
				// Transpiration is considered a source/sink term for Richards equation
				EMS[e].lwc_source -= d_theta_l;
			} else {
				// Update volumetric water content in layer
				EMS[e].theta[WATER] -= d_theta_l;
				assert(EMS[e].theta[WATER] >= -Constants::eps);
				EMS[e].theta[AIR] += d_theta_l;
				assert(EMS[e].theta[AIR] >= -Constants::eps);
			}
		}
		// Depth of the upper edge of layer below
		zupper += EMS[e].L;
	}// End of loop

	// Extract the residual water uptake from first layer below rootzone
	if ( RootLayer > 0 ) {
		// modify by Moustapha if there is a problem .
		RootLayer -= 1;
	}

	if (watertransportmodel_soil == "RICHARDSEQUATION" && EMS[RootLayer].VG.defined == true) {
		// Transpiration is considered a source/sink term for Richards equation
		const double theta_wilt = EMS[RootLayer].VG.fromHtoTHETAforICE(h_wilt, EMS[RootLayer].theta[ICE]);
		const double d_theta = std::min( std::max(0., ( EMS[RootLayer].theta[WATER] -
	                       theta_wilt ) ),
	                       waterresidual_real / ( Constants::density_water * EMS[RootLayer].L ) );
		EMS[RootLayer].lwc_source -= d_theta;
		waterresidual_real -= d_theta * Constants::density_water * EMS[RootLayer].L;
	} else {
		const double d_theta = std::min( std::max(0., ( EMS[RootLayer].theta[WATER] -
	                       wp_fraction * EMS[RootLayer].soilFieldCapacity() ) ),
	                       waterresidual_real / ( Constants::density_water * EMS[RootLayer].L ) );
		EMS[RootLayer].theta[WATER] -= d_theta;
		assert(EMS[RootLayer].theta[WATER] >= -Constants::eps);
		EMS[RootLayer].theta[AIR] += d_theta;
		assert(EMS[RootLayer].theta[AIR] >= -Constants::eps);
		waterresidual_real -= d_theta * Constants::density_water * EMS[RootLayer].L;
	}

	// Check if water content is below wilting point in last layer
	if ( waterresidual_real > 0.5 ) {
		// modify by Moustapha if there is problem .
		prn_msg(__FILE__, __LINE__, "wrn", mio::Date(), "Transpiration Error [mm]: %lf", waterresidual_real);
	}
}


/**
 * @brief multiplicative increase of canopy surface resistance as
 * a function of soil temperature, based on ï¿½gren et al (1976)
 * (A=0.8 and B=0.8 implies 1/f4=1 at 10oC)
 * Last update: David Gustafsson, 2005-03-16
 * Last Update: David Gustafsson, 2006-11-22>> A=1.75,B=0.5, implies that
		- 1/f4 increases much faster towards 1. 1/f4 = 0.9 already at 2oC, but 1/f4=1 still
		- at about 10oC.
 * @param tempC
 * @return double
 */
double Canopy::get_f4(const double& tempC)
{
	static const double F4_A = 1.75;
	static const double F4_B = 0.5;

	// OBS tempC in C
	const double f4 = 1.0 / ( 1.0 - exp( -F4_A * pow( std::max( 0.00001, tempC ), F4_B ) ) );
	return (f4);
}


/**
 * @brief multiplicative increase of canopy surface resistance as
 * a function of liquid water content[1] and soil temperature [2]
 * in the root zone: [1] van den Hurk et al (2000): Offline validation
 * of the ERA40 surface scheme, ECMWF Tech.Mem.295, [2] ï¿½gren (1976)/Mellander (2005)
 * @param SoilNode
 * @param *EMS
 * @return double
 */
double Canopy::get_f2f4(const size_t& SoilNode, ElementData* EMS, const double wp_fraction, const double rootdepth)
{
	double f2_wpwp; double f2_wcap;
	double thet_act;
	double rootresidual = 1.;
	double f2 = 0.0; double f4 = 0.0;
	size_t RootLayer = SoilNode;

	// loop over layers:
	double zupper = 0.;
	for (size_t e = SoilNode; e --> 0; ) { //e gets decremented right away -> start at SoilNode
		// 1) root fraction in layer
		const double rootfr = RootFraction(zupper, zupper + EMS[e].L, rootdepth);
		if (rootfr > 0.0 ){
			RootLayer = e;
			// 2) Field Capacity in layer
			f2_wcap = EMS[e].soilFieldCapacity();
			// 3) Wilting point in layer
			f2_wpwp = f2_wcap * wp_fraction;
			// 4) Soil water content in layer (from a root's point of view)
			thet_act = std::max(f2_wpwp, EMS[e].theta[WATER]);
			// 4) Inversed soilwater stress weighted by root fractin in layer
			f2 += rootfr * (thet_act-f2_wpwp) / (f2_wcap - f2_wpwp);
			// 5) Soil temperature stress weighted by root fraction in layer
			f4 += get_f4(IOUtils::K_TO_C(EMS[e].Te)) * rootfr;
			// 6) Update rootresidual and depth of upper edge of next layer
			rootresidual -= rootfr;
		}
		zupper += EMS[e].L;
	}// End of loop and now do the bottom layer

	if ( RootLayer > 0 ){
		RootLayer -= 1;
	}
	f2_wcap = EMS[RootLayer].soilFieldCapacity();
	f2_wpwp = f2_wcap * wp_fraction;
	thet_act = std::max(f2_wpwp, EMS[RootLayer].theta[WATER]);
	f2 += rootresidual * (thet_act - f2_wpwp) / (f2_wcap - f2_wpwp);
	f4 += get_f4(IOUtils::K_TO_C(EMS[RootLayer].Te)) * rootresidual;

	// Limit inverse of the f2 function between 0 and 1
	f2 = std::max( 0.00001, std::min( 1., f2 ) );
	// invert f2
	f2 = 1.0 / f2;
	// return maximum of f2 and f4
	return ( std::max(f2, f4) );
}


/**
 * @brief multiplicative increase of canopy surface resistance as
 * a function of atmospheric vapor pressure deficit (van den Burk et al (2000):
 * Offline validation of the ERA40 surface scheme, ECMWF Tech.Mem.295)
 * @param vpd
 * @return double
 */
double Canopy::get_f3(const double& vpd, const double f3_gd)
{
	/*
	 * double F3_GD=0.0003; => now defined in Constants.h
	 * gd [Pa-1] value is for trees (needle or bradleafs), other veg. as crops,
	 * grass tundra etc should have gd=0;
	 */
	const double f3 = 1.0 / exp( -f3_gd * vpd );
	return (f3);
}

double Canopy::IntCapacity(const CurrentMeteo& Mdata, const SnowStation& Xdata, const bool& force_rain) const
{
	const double rho_new_snow = SnLaws::compNewSnowDensity(hn_density, hn_density_parameterization,
	                                                   hn_density_fixedValue, Mdata, Xdata, Xdata.Cdata.temp, variant);

        double psum_ph = Mdata.liq_psum / (Mdata.solid_psum + Mdata.liq_psum ) ;
	if (!force_rain && rho_new_snow!=Constants::undefined && Mdata.solid_psum<1.) { //right conditions for snow
		const double density_of_mixed = rho_new_snow*(1. - psum_ph) + 1000. * psum_ph;
		return ( Xdata.Cdata.int_cap_snow * Xdata.Cdata.lai * ( 0.27+46.0 / density_of_mixed ));
	} else {
		return ( Xdata.Cdata.int_cap_rain * Xdata.Cdata.lai);
	}
}

/**
 * @brief Intercepted snow or rain above the capacity is unloaded imediately
 * @param capacity
 * @param storage
 * @param *unload
 */
double Canopy::IntUnload(const double& capacity, const double& storage)
{
	if (storage > capacity) {
		return (storage - capacity);
	} else {
		return 0.;
	}
}

/**
 * @brief interception rate according to exponential function from Ashton ()
 * as formulated by i.e. Pomeroy and Hedstrom (1998)
 * @param capacity
 * @param storage
 * @param prec
 * @param *interception
 * @param direct
 */
double Canopy::IntRate(const double& capacity, const double& storage, const double& prec, const double& direct, const double interception_timecoef)
{
	const double interception = std::min( ( 1.0 - direct ) * prec,
                                 interception_timecoef * ( capacity - storage)*
                                ( 1.0 - exp( -(1.0 - direct) * prec / capacity ) ) );
	if ( interception < 0.0)
		return 0.;

	return interception;
}


double Canopy::CanopyAlbedo(const double& tair, const double& wetfrac, const SnowStation& Xdata)
{
	// Albedo of partly "wet" canopy = weighted average of dry and wet parts
	if (tair > Constants::meltfreeze_tk ) {
		return (wetfrac *  Xdata.Cdata.can_alb_wet + (1. - wetfrac) *  Xdata.Cdata.can_alb_dry);
	} else {
		return (wetfrac * Xdata.Cdata.can_alb_snow + (1. - wetfrac) *  Xdata.Cdata.can_alb_dry);
	}
}

/**
 * @brief Function returning the total surface albedo of a canopy covered snow or soil surface
 * @param CanAlb
 * @param sigf
 * @param SurfAlb
 * @param DirectThroughfall
 * @param CanopyClosureDirect
 * @param RadFracDirect
 * @param sigfdirect
 * @return double
 */
double Canopy::TotalAlbedo(double CanAlb, double sigf, double SurfAlb, double DirectThroughfall,
			  double CanopyClosureDirect, double RadFracDirect, double sigfdirect)
{
	// Total surface albedo (diffuse fraction)
	const double albedo_diff = ( 1.0 - RadFracDirect ) * ( (sigf * CanAlb + SurfAlb * Optim::pow2(1.0 - sigf) /
			(1.0 - sigf * CanAlb * SurfAlb) ) * (1. - DirectThroughfall) + SurfAlb * DirectThroughfall);
	// Total surface albedo (direct fraction)
	const double albedo_dir = RadFracDirect * ( (sigfdirect * CanAlb + SurfAlb * Optim::pow2(1.0 - sigfdirect) /
			(1.0 - sigfdirect * CanAlb * SurfAlb) ) * CanopyClosureDirect + SurfAlb *
			(1.0 - CanopyClosureDirect) );
	return albedo_diff+albedo_dir;
}

/**
 * @brief Function returning the soil cover fraction of the canopy shade as a
 * function of canopy height, canopy diameter, and solar elevation
 * Computes the canopy shade soil cover fraction, as function of canopy height, crown diameter,
 * vertical canopy soil cover, and solar elevation angle. Derivation can be found in Gryning et al. (2001).
 * Boundary-Layer Meteorology, 99 (3), 465-488.
 * @param height
 * @param cover
 * @param elev in radiants
 * @return double
 */
double Canopy::CanopyShadeSoilCover(const double& height, const double& cover, const double& elev, const double& can_diameter)
{
	if ( elev > 0.0 ) {
		return std::min(1.0, cover * (1.0 + 4.0 * height / (Constants::pi * can_diameter * tan(elev))));
	} else {
		return 1.0;
	}
}

/**
 * @brief fraction of the canopy surface covered by intercepted rain or snow
 * @param capacity
 * @param storage
 */
double Canopy::CanopyWetFraction(const double& capacity, const double& storage)
{
	if ( storage > 0. ) {
		// limit the wet fraction to minimum 0.01 otherwise it will never completely dry
		return std::max(0.01, std::min(1.0, pow(storage / capacity, 2./3.)) );
	} else {
		return 0.;
	}
}

/**
 * @brief Transmissivity is now (optionally) a function of solar elevation (Chen et al, 1997)
 * @param *sigf
 * @param lai
 * @param elev
 * @return sigf
 */
double Canopy::CanopyTransmissivity(const double& lai, const double& elev, const double krnt_lai)
{
	const double pai = 0.; // pai [additional plant area index] could be a function of interception storage
	return (1. - exp(-krnt_lai * (lai + pai) / std::max(sin(elev), 0.0001))); // Beer-Lambert type of law
}

/**
 * @brief This routine estimates the radiation balance of a vegetation covered grid
 * inputs:
 * incoming shortwave (RG) and longwave (RA) radiation
 * vegetation temperature (TV) and surface temperature of the ground below (TG)
 * vegetation albedo (AV) and ground albedo (AG)
 * vegetation shielding coefficient (SIGF) [shortwave and longwave]
 * emissivity of vegetation (EV) and ground (EG)
 * outputs:
 * Net longwave and shortwave radiation of vegetation (RAV,RGV) and ground (RAG,RGG)
 * Total grid albedo (AGRID) and grid surface radiation temperature (TGRID)
 * @param Mdata
 * @param Cdata
 * @param Xdata
 * @param iswrac
 * @param rsnet
 * @param ilwrac
 * @param r0
 * @param r1
 * @param canopyalb
 * @param CanopyClosureDirect
 * @param RadFracDirect
 * @param sigfdirect
 * @param r1p
 */
void Canopy::LineariseNetRadiation(const CurrentMeteo& Mdata, const CanopyData& Cdata, const SnowStation& Xdata,
                                      double& iswrac, double& rsnet, double& ilwrac, double& r0,double& r1,
                                      const double& canopyalb, double& CanopyClosureDirect, double& RadFracDirect,
                                      const double& sigfdirect, double& r1p) const
{
	// Variables used a lot
	const bool snow = (Xdata.getNumberOfElements()>Xdata.SoilNode);
	const double Tsfc = (snow)? Xdata.Ndata[Xdata.getNumberOfElements()].T : Mdata.ta;
	//  modifs for forestfloor_alb : ag -> ag1
	const double ag1 = (snow)? Xdata.Albedo : Xdata.SoilAlb;
	const size_t nE = Xdata.getNumberOfElements();
	const double age = (snow && forestfloor_alb) ? std::max(0., Mdata.date.getJulian() - Xdata.Edata[nE-1].depositionDate.getJulian()) : 0.; // day
	const double ag = (ag1 -.3)* exp(-age/7.) + 0.3;

	//Canopy Closure = Canopy Soil Cover Fraction, is made a function of solar elevation for direct shortwave
	//First, check whether the solar elevation and splitted radiation data makes there is sense
	const double elev = Mdata.elev;
	const double diffuse = Mdata.diff;
	const double direct = Mdata.iswr - diffuse;
	double RadFracDiffuse;
	if ( direct > 0.0 ) {
		RadFracDirect = direct / (diffuse + direct);
		RadFracDiffuse = 1.0 - RadFracDirect;
	} else {
		RadFracDirect = 0.0;
		RadFracDiffuse = 1.0;
	}
	const double sigf = Cdata.sigf;
	// Canopy Closure for diffuse shortwave and longwave
	const double CanopyClosure = 1. - Cdata.direct_throughfall; //HACK: we already pass Cdata

	// Canopy Closure for direct shortwave
	if (canopytransmission ){
		CanopyClosureDirect = CanopyShadeSoilCover(Cdata.height, CanopyClosure, elev, Cdata.can_diameter);
	} else{
		CanopyClosureDirect = CanopyClosure;
	}
	// Shortwave radiation fluxes above and absorbed by canopy above
	iswrac = Mdata.iswr;

	// net absorbed by canopy

	// first only diffuse fraction
	rsnet = RadFracDiffuse * iswrac * (1. - canopyalb) * sigf *
		(1. + ag * (1. - sigf) / (1. - sigf * ag * canopyalb));
	// Longwave radiation above canopy:
	ilwrac = Mdata.ea * Constants::stefan_boltzmann * (Mdata.ta * Mdata.ta * Mdata.ta * Mdata.ta);

	// Longwave absorbed by canopy: auxiliary variables
	const double eg = 1.; // emissivity of ground assumed to be 1
	const double star = 1. - sigf * (1. - Cdata.ec) * (1. - eg);
	const double psi = (1. - sigf) * (1. - eg) * Cdata.ec;

	// RNC = RNSC + RNLC: r0p and r1p correpsonds to RNC(t) = r0p + r1p * TC(t)^4
	const double r0p = rsnet + sigf * ((Cdata.ec + psi / star) *
	ilwrac + Cdata.ec * eg * Constants::stefan_boltzmann * Optim::pow4(Tsfc) / star);
	r1p = -sigf * (Cdata.ec * Constants::stefan_boltzmann + Cdata.ec * eg * Constants::stefan_boltzmann /
		star + psi * Constants::stefan_boltzmann / star);

	// Linearise RNC arond TC(t) by using TC(t)=TC(t-1)^4+4*TC(t-1)^3*(TC(t)-TC(t-1)),
	// which gives us r0 and r1, correpsonding to RNC(t)=r0+r1*TC(t)
	const double TC_old = Cdata.temp;

	r0 = r0p - 3. * r1p * Optim::pow4(TC_old);
	r1 = 4.* r1p * Optim::pow3(TC_old);

	// Scaling by CanopyClosure (= 1-DirectThroughfall)
	rsnet *= CanopyClosure;
	r0 *= CanopyClosure;
	r1 *= CanopyClosure;

	// Now, add the direct component with different CanopyClosure
	const double rsnetdir = CanopyClosureDirect * RadFracDirect * iswrac *
		(1. - canopyalb) * sigfdirect * (1. + ag * (1. - sigfdirect) / (1. - sigfdirect * ag * canopyalb));

	rsnet += rsnetdir;
	r0 += rsnetdir;

}

/**
 * @brief Same as the LineariseNetRadiation routine, but for 2layer canopies.
 * Objective :
 * 		netradtrunk = rt0 + rt1 * Ttrunk + rt2 * TC
 *  		netradcanop = r0 + r1*TC + r2* Ttrunk
 * Method : using linearisation of TC**4 and Ttrunk**4
 * @param Mdata
 * @param Cdata
 * @param Xdata
 * @param iswrac
 * @param rsnet
 * @param ilwrac
 * @param r0
 * @param r1
 * @param canopyalb
 * @param CanopyClosureDirect
 * @param RadFracDirect
 * @param sigfdirect
 * @param r1p
 */
void Canopy::LineariseNetRadiation2L(const CurrentMeteo& Mdata, const CanopyData& Cdata, const SnowStation& Xdata,
                                      double& iswrac, double& rsnet, double& ilwrac, double& r0,double& r1, double& r2,
                                      double& rt0, double& rt1, double& rt2, const double& canopyalb, double& CanopyClosureDirect, double& RadFracDirect,
                                      const double& sigfdirect,const double& sigftrunkdirect, double& r1p, double& r2p) const
{
	// Variables used a lot
	const bool snow = (Xdata.getNumberOfElements()>Xdata.SoilNode);
	const double Tsfc = (snow)? Xdata.Ndata[Xdata.getNumberOfElements()].T : Mdata.ta;
	// modifs for forestfloor_alb : ag -> ag1
	const double ag1 = (snow)? Xdata.Albedo : Xdata.SoilAlb;

	// modifs for forestfloor_alb
	const size_t nE = Xdata.getNumberOfElements();
	const double age = (snow && forestfloor_alb) ? std::max(0., Mdata.date.getJulian() - Xdata.Edata[nE-1].depositionDate.getJulian()) : 0.; // days
	const double ag = (ag1 -.3)* exp(-age/7.) + 0.3;

	// Canopy Closure = Canopy Soil Cover Fraction, is made a function of solar elevation for direct shortwave
	// First, check whether the solar elevation and splitted radiation data makes there is sense
	const double elev = Mdata.elev;
	const double diffuse = Mdata.diff;
	const double direct = Mdata.iswr - diffuse;
	double RadFracDiffuse;
	if ( direct > 0.0 ) {
		RadFracDirect = direct / (diffuse + direct);
		RadFracDiffuse = 1.0 - RadFracDirect;
	} else {
		RadFracDirect = 0.0;
		RadFracDiffuse = 1.0;
	}
	const double sigf = Cdata.sigf;
	const double sigftrunk = Cdata.sigftrunk;
	// Canopy Closure for diffuse shortwave and longwave
	const double CanopyClosure = 1. - Cdata.direct_throughfall; //HACK: we already pass Cdata
	double CanClosDirLeaves, CanClosDirTrunks;
	// Canopy Closure for direct shortwave
	if (canopytransmission ){
		CanopyClosureDirect = CanopyShadeSoilCover(Cdata.height, CanopyClosure, elev, Cdata.can_diameter);
		CanClosDirLeaves= CanopyClosureDirect ;
		CanClosDirTrunks=0.;

		// below (optional): if uncommented, allows direct solar insolation of the trunks
		//CanClosDirLeaves = CanopyShadeSoilCover(Cdata.height*(1. - trunk_frac_height), CanopyClosure, elev);
		//CanClosDirTrunks = CanopyClosureDirect - CanClosDirLeaves;
	} else{
		CanopyClosureDirect = CanopyClosure;
		CanClosDirLeaves = CanopyClosure;
		CanClosDirTrunks = 0.;
	}

	// attenuation factors for the radiative impact of the trunk layer
	const double attfactor_SW = (1. - sigftrunk) ;
	const double attfactor_SWdir = (1. - sigftrunkdirect) ;
	const double attfactor_LW = (1. - sigftrunk);

	// 1. Radiations to leaf layer
	// 1.1 SW
	iswrac = Mdata.iswr;

	// net absorbed by canopy
	// first only diffuse fraction
	rsnet = RadFracDiffuse * iswrac * (1. - canopyalb) * sigf *
		(1. + ag * (1. - sigf) / (1. - sigf * ag * canopyalb)* attfactor_SW + (1. - sigf) * sigftrunk * Cdata.trunkalb);

	// 1.2. LW
	// Longwave radiation above canopy:
	ilwrac = Mdata.ea * Constants::stefan_boltzmann * (Mdata.ta * Mdata.ta * Mdata.ta * Mdata.ta);

	// Longwave absorbed by canopy: auxiliary variables
	const double eg = 1.0; // emissivity of ground assumed to be 1
	const double star = 1. - sigf * (1. - Cdata.ec) * (1. - eg);
	const double psi = (1. - sigf) * (1. - eg); // Change of formulation from the original for easier read : suppress ec from psi

	// 1.3. all
	// RNC = RNSC + RNLC: r0p, r1p  and r2p correpsonds to RNC(t) = r0p + r1p * TC(t)^4 + r2p * Ttrunk^4
	const double r0p = rsnet + sigf * Cdata.ec*((1. + psi / star * attfactor_LW) *
	ilwrac + eg * Constants::stefan_boltzmann * Optim::pow4(Tsfc) * attfactor_LW / star);
	r1p = -sigf *Cdata.ec * Constants::stefan_boltzmann * (2. - Cdata.ec * sigf * (1.-eg)/star * attfactor_LW);
	r2p = sigf * Cdata.ec * Constants::stefan_boltzmann * Cdata.et *sigftrunk * (1. + (1. -eg));

	// Linearise RNC around TC(t) and Ttrunk(t) by using TC(t)^4=TC(t-1)^4+4*TC(t-1)^3*(TC(t)-TC(t-1)),
	// which gives us r0, r1, and r2 correpsonding to RNC(t)=r0+r1*TC(t)+ r2* Ttrunk
	const double TC_old = Cdata.temp;
	const double Tt_old = Cdata.Ttrunk;

	r0 = r0p - 3. * r1p * Optim::pow4(TC_old) - 3. * r2p *  Optim::pow4(Tt_old) ;
	r1 = 4.* r1p * Optim::pow3(TC_old);
	r2 = 4.* r2p * Optim::pow3(Tt_old);

	// Scaling by CanopyClosure (= 1-DirectThroughfall)
	rsnet *= CanopyClosure;
	r0 *= CanopyClosure;
	r1 *= CanopyClosure;
	r2 *= CanopyClosure;

	// Now, add the direct component with different CanopyClosure
	const double rsnetdir = CanClosDirLeaves * RadFracDirect * iswrac *
		(1. - canopyalb) * sigfdirect * (1. + ag * (1. - sigfdirect) *attfactor_SWdir / (1. - sigfdirect * ag * canopyalb) + (1. - sigfdirect) * sigftrunkdirect * Cdata.trunkalb)
		+ CanClosDirTrunks * RadFracDirect * iswrac * (1. - canopyalb) * sigfdirect * ag *attfactor_SWdir/(1. - sigfdirect * ag * canopyalb);

	rsnet += rsnetdir;
	r0 += rsnetdir;

	// 2. Radiations to trunk layer
	// formulation : netradtrunk = rt0p + rt1p * Ttrunk**4 + rt2p * TC**4
	//			     ~ rt0 + rt1 * Ttrunk + rt2 * TC


	// 2.1. SW diffuse
	double rsnettrunk =  RadFracDiffuse * iswrac * (1. -sigf) * (1-attfactor_SW) * (1-Cdata.trunkalb);

	// 2.2. LW
	const double rt0p = rsnettrunk + Cdata.et*(1-attfactor_LW)* (eg * Constants::stefan_boltzmann * Optim::pow4(Tsfc) + ilwrac * (1. - sigf));
	const double rt1p = -2. * Constants::stefan_boltzmann *Cdata.et* (1-attfactor_LW) ;
	const double rt2p = Cdata.et*(1-attfactor_LW) * Cdata.ec * sigf * Constants::stefan_boltzmann ;

	rt0 = rt0p - 3. * rt1p * Optim::pow4(Tt_old) - 3. * rt2p *  Optim::pow4(TC_old) ;
	rt1 = 4.* rt1p * Optim::pow3(Tt_old);
	rt2 = 4.* rt2p * Optim::pow3(TC_old);

	rsnettrunk *= CanopyClosure;
	rt0 *= CanopyClosure;
	rt1 *= CanopyClosure;
	rt2 *= CanopyClosure;

	// 2.3. SW direct & NetRad to Trunks
	const double rsnettrunkdir = CanClosDirLeaves * RadFracDirect * iswrac * (1. -sigfdirect) * (1. - attfactor_SWdir)*(1.-Cdata.trunkalb)
				+ CanClosDirTrunks *  RadFracDirect * iswrac * (1. - attfactor_SWdir) *(1.-Cdata.trunkalb);
	rsnettrunk += rsnettrunkdir;
	rt0 += rsnettrunkdir;
}

/**
 * @brief the sensible heat flux is already a linear function of TC(t),
 * whould be different with stability corrections.
 * @param ch_canopy
 * @param tair
 * @param *h0
 * @param *h1
 */
void Canopy::LineariseSensibleHeatFlux(const double& ch_canopy, const double& tair, double& h0, double& h1, double scalingfactor)
{
	h1 = scalingfactor*ch_canopy;
	h0 = -scalingfactor*ch_canopy * tair;
}

/**
 * @brief Temperature derivative of the saturation pressure function
 * @param L
 * @param T
 * @return double
 */
double Canopy::DSaturationPressureDT(const double& L, const double& T)
{
	double c2, c3;

	if ( L != Constants::lh_sublimation ) {
		//c1 = 610.780;
		c2 = 17.08085;
		c3 = 234.175 ;
	} else {
		//c1 = 610.714;
		c2 = 22.44294;
		c3 = 272.440 ;
	}

	const double dpdt =  Atmosphere::vaporSaturationPressure(T) * c2 * c3 / ((c3 + IOUtils::K_TO_C(T)) * (c3 + IOUtils::K_TO_C(T)));

	return(dpdt);
}

/**
 * @brief the latent heat flux LE = ce_canopy * (esat(TC(t)) - eair) is linearised around TC(t) by:
 * applying esat(TC(t)) = esat(TC(t - 1)) + Desat(TC(t - 1)) / DT * (TC(t) - TC(t - 1))
 * @param ce_canopy
 * @param tc_old
 * @param vpair
 * @param le0
 * @param le1
 */
void Canopy::LineariseLatentHeatFlux(const double& ce_canopy, const double& tc_old, const double& vpair,
                                        double& le0, double& le1, double scalingfactor)
{
	if (tc_old > 273.15) {
		le1 = scalingfactor*ce_canopy * DSaturationPressureDT(Constants::lh_vaporization, tc_old);
		le0 = scalingfactor*ce_canopy * (Atmosphere::vaporSaturationPressure(tc_old) - vpair) - (le1) * tc_old;
	} else {
		le1 = scalingfactor*ce_canopy * DSaturationPressureDT(Constants::lh_sublimation, tc_old);
		le0 = scalingfactor*ce_canopy * (Atmosphere::vaporSaturationPressure(tc_old) - vpair) - (le1) * tc_old;
	}
}


/**
 * @brief Physical calculation of Heat Mass for the leaves and trunks compartments
 * - for leaves : mean leaf thickness assumed to be 1 mm = 0.001 m.
 * - for trunks : trunk volume calculated based on basal area, height and conic form.
 * @param height
 * @param BasalArea
 * @param lai
 * @param HMLeaves
 * @param HMTrunks
 */
void Canopy::CalculateHeatMass(const double& height, const double& BasalArea, double& lai, double& HMLeaves,  double& HMTrunks, const double biomass_density,  const double biomass_heat_capacity)
{
	HMLeaves= 0.001 * lai * biomass_density * biomass_heat_capacity;
	HMTrunks= 0.5 * BasalArea * height * biomass_density * biomass_heat_capacity;
}

/**
 * @brief the conductive heat flux to the leaf layer is already a linear function of TC(t),
 * @param tc_old
 * @param lai
 * @param HM0
 * @param HM1
 */
void Canopy::LineariseConductiveHeatFlux(const double& tc_old, const double& HM, double& HM0, double& HM1,  const double& DT, const double& scalingfactor) const
{
	if (CanopyHeatMass) {
		HM0 = -1.0 * scalingfactor * HM /H_TO_S(DT) *tc_old;
		HM1 =  scalingfactor * HM / H_TO_S(DT);
	} else {
		HM0 = 0.;
		HM1 = 0.;
	}
}

/**
 * @brief Last update: 2007-05-10, David Gustafsson
 * @param r0
 * @param r1
 * @param h0
 * @param h1
 * @param le0
 * @param le1
 * @param TCANOPY
 * @param RNCANOPY
 * @param HCANOPY
 * @param LECANOPY
 * @param ce_canopy
 * @param ce_condensation
 */

void Canopy::CanopyEnergyBalance(const double& h0, const double& h1, const double& le0,
                                                         const double& le1, const double& HM0,  const double& HM1,
                                                         const double& ce_canopy,
                                                         const double& ce_condensation,
                                                         double& r0, double& r1, double& TCANOPY, double& RNCANOPY,
                                                         double& HCANOPY, double& LECANOPY)
{
	// Introduced filter to reduce sensitivity of canopy energy balance solution:
	// Maximum allowed change of canopy temperature per hour
	// 1. infer TCANOPY from (R0 + R1 * TCANOPY) = (H0 + H1 * TCANOPY) + (LE0 + LE1 * TCANOPY)
	double TC_CHANGE = (h0 + le0 - r0 + HM0) / (r1 - h1 - le1 - HM1) - TCANOPY;

	TCANOPY += TC_CHANGE;

	// 3. and re-compute Rn, H, and LE
	//      Previously, r0 and r1 were updated after each TC change computed in the EB loop.
	//      With only 3 iterations of the EB, this prevented the closure of the canopy EB when looking at the output variables,
	//      because TC had not completely converged.
	//	The suggestion is to increase the iterations of the EB to 7 (instead of 3) and get rid off these artefacts.
	// 	Similarly, LECANOPY is put to its computed value
	RNCANOPY = r0 + r1  * TCANOPY ;
	HCANOPY = h0 + h1 * TCANOPY;
	LECANOPY = le0 + le1 * TCANOPY;
	// 3b. re-compute in case of condensation/sublimation on canopy
	if (LECANOPY < 0.0 ) {
		TCANOPY -= TC_CHANGE;
		TC_CHANGE = (h0 + le0 * ce_condensation / ce_canopy - r0 + HM0) /
				(r1 - h1 - le1 * ce_condensation / ce_canopy - HM1) - TCANOPY;
		TCANOPY += TC_CHANGE;
		RNCANOPY = r0 +  r1  * TCANOPY ;
		HCANOPY = h0 + h1 * TCANOPY;
		LECANOPY = le0 * ce_condensation / ce_canopy + le1 * ce_condensation / ce_canopy  * TCANOPY;
	}
}


void Canopy::CanopyEnergyBalance2L(double& h0, double& h1,  double& le0,
                                                         double& le1, double& HM0,  double& HM1, double& TT0, double& TT1,
                                                         const double& ce_canopy,
                                                         const double& ce_condensation,
                                                         double& r0, double& r1, double& r2, double& TCANOPY, double& Ttrunk, double& RNCANOPY,
                                                         double& HCANOPY, double& LECANOPY)
{
	/*
 	 * Introduced filter to reduce sensitivity of canopy energy balance solution:
	 * Maximum allowed change of canopy temperature per hour
	 */
	double TC_CHANGE = (h0 + le0 - r0 + HM0 -TT0) / (r1 - h1 - le1 - HM1 + TT1) - TCANOPY;
	// 2. minimize the rate of change to CANOPYTEMP_MAXCHANGE_PERHOUR [K hr - 1]
	TCANOPY += TC_CHANGE;
	Ttrunk = TT1 / r2 * TCANOPY + TT0 / r2 ;
	// 3. and re-compute Rn, H, and LE
	RNCANOPY = r0 + r1  * TCANOPY + r2 * Ttrunk;
	HCANOPY = h0 + h1 * TCANOPY;
	LECANOPY = le0 + le1 * TCANOPY;

	// 3b. re-compute in case of condensation/sublimation on canopy
	if (LECANOPY < 0.0 ) {
		TCANOPY -= TC_CHANGE;
		TC_CHANGE = (h0 + le0 * ce_condensation / ce_canopy - r0 + HM0 -TT0) /
				(r1 - h1 - le1 * ce_condensation / ce_canopy - HM1 + TT1) - TCANOPY;
		TCANOPY += TC_CHANGE;

		Ttrunk = TT1 / r2 * TCANOPY + TT0 / r2 ;

		RNCANOPY = r0 +  r1  * TCANOPY + r2 * Ttrunk;
		HCANOPY = h0 + h1 * TCANOPY;
		LECANOPY = le0 + le1 * TCANOPY;
	}
}


/**
 * @brief Partition latent heat flux on interception and transpiration
 * @param ce_canopy
 * @param ce_transpiration
 * @param LECANOPY
 * @param ta
 * @param I
 * @param DT
 * @param CanopyEvaporation
 * @param INTEVAP
 * @param TRANSPIRATION
 * @param RNCANOPY
 * @param HCANOPY
 * @param TCANOPY
 * @param r0
 * @param r1
 * @param h0
 * @param h1
 * @param LECANOPYCORR
 * @param wetfraction
 * @param HM0
 * @param HM1
 */
void Canopy::CanopyEvaporationComponents(const double& ce_canopy,
                                      const double& ce_transpiration, double& LECANOPY,
                                      const double& ta, const double& I, const double DT,
                                      double& CanopyEvaporation,
                                      double& INTEVAP, double& TRANSPIRATION,
                                      double& RNCANOPY, double& HCANOPY,double& TCANOPY,
                                      const double& r0, const double& r1, const double& h0, const double& h1,
                                      double& LECANOPYCORR,
                                      const double& wetfraction, const double& HM0, const double& HM1)
{
	if ( ta > Constants::meltfreeze_tk ) {
		CanopyEvaporation = DT * 3600.0 * LECANOPY / Constants::lh_vaporization; // [mm]
	} else {
		CanopyEvaporation = DT * 3600.0 * LECANOPY / Constants::lh_sublimation;  // [mm]
	}

	if ( CanopyEvaporation <= 0.0 ) {
		INTEVAP = CanopyEvaporation; // [mm]
		TRANSPIRATION = 0.0;            // [mm]
		LECANOPYCORR = LECANOPY;
	} else {
		TRANSPIRATION = CanopyEvaporation * ce_transpiration * (1.0 - wetfraction) / ce_canopy;
		INTEVAP = CanopyEvaporation - TRANSPIRATION;
                if ( INTEVAP > I ) {
			INTEVAP = I;
			CanopyEvaporation = INTEVAP + TRANSPIRATION;
			if ( ta > Constants::meltfreeze_tk ) {
				LECANOPYCORR = CanopyEvaporation * Constants::lh_vaporization / (DT * 3600.0);
			} else {
				LECANOPYCORR = CanopyEvaporation * Constants::lh_sublimation / (DT * 3600.0);
			}
			// re-compute TCANOPY from (R0 + R1 * TCANOPY) = (H0 + H1 * TCANOPY) + LECANOPYCORR
			TCANOPY  = (LECANOPYCORR + h0 - r0 + HM0) / (r1 - h1 - HM1);
			// Re-compute RNCANOPY, HCANOPY, and LECANOPY with new temperature
			RNCANOPY = r0 + r1  * TCANOPY ;
			HCANOPY  = h0 + h1 * TCANOPY;
			LECANOPY = LECANOPYCORR;
		} else {
			LECANOPYCORR = LECANOPY;
		}
	}
}


/**
 * @brief Partition latent heat flux on interception and transpiration, 2-Layer canopy
 * @param ce_canopy
 * @param ce_transpiration
 * @param LECANOPY
 * @param ta
 * @param I
 * @param DT
 * @param CanopyEvaporation
 * @param INTEVAP
 * @param TRANSPIRATION
 * @param RNCANOPY
 * @param HCANOPY
 * @param TCANOPY
 * @param Ttrunk
 * @param TT0
 * @param TT1
 * @param r0
 * @param r1
 * @param h0
 * @param h1
 * @param LECANOPYCORR
 * @param wetfraction
 * @param HM0
 * @param HM1
 */

void Canopy::CanopyEvaporationComponents2L(const double& ce_canopy,
                                      const double& ce_transpiration, double& LECANOPY,
                                      const double& ta, const double& I, const double DT,
                                      double& CanopyEvaporation,
                                      double& INTEVAP, double& TRANSPIRATION,
                                      double& RNCANOPY, double& HCANOPY,double& TCANOPY, double& Ttrunk,
                                      const double& TT0, const double& TT1,
                                      const double& r0, const double& r1, const double& r2, const double& h0, const double& h1,
                                      double& LECANOPYCORR,
                                      const double& wetfraction,
                                      const double& HM0, const double& HM1)
{
	if ( ta > Constants::meltfreeze_tk ) {
		CanopyEvaporation = DT * 3600.0 * LECANOPY / Constants::lh_vaporization; // [mm]
	} else {
		CanopyEvaporation = DT * 3600.0 * LECANOPY / Constants::lh_sublimation;  // [mm]
	}

	if ( CanopyEvaporation <= 0.0 ) {
		INTEVAP = CanopyEvaporation; // [mm]
		TRANSPIRATION = 0.0;            // [mm]
		LECANOPYCORR = LECANOPY;
	} else {
		TRANSPIRATION = CanopyEvaporation * ce_transpiration * (1.0 - wetfraction) / ce_canopy;
		INTEVAP = CanopyEvaporation - TRANSPIRATION;
                if ( INTEVAP > I ) {
			INTEVAP = I;
			CanopyEvaporation = INTEVAP + TRANSPIRATION;
			if ( ta > Constants::meltfreeze_tk ) {
				LECANOPYCORR = CanopyEvaporation * Constants::lh_vaporization / (DT * 3600.0);
			} else {
				LECANOPYCORR = CanopyEvaporation * Constants::lh_sublimation / (DT * 3600.0);
			}
			// re-compute TCANOPY from (R0 + R1 * TCANOPY + TT0 + TT1 * TCANOPY) = (H0 + H1 * TCANOPY + HM0 + HM1 * TCANOPY ) + LECANOPYCORR
			TCANOPY = (LECANOPYCORR + h0 - r0 + HM0 - TT0) / (r1 - h1- HM1 + TT1);
			Ttrunk = TT1 / r2 *TCANOPY + TT0 / r2;
			// Re-compute RNCANOPY, HCANOPY, and LECANOPY with new temperature
			RNCANOPY = r0 + r1  * TCANOPY + r2 * Ttrunk;
			HCANOPY  = h0 + h1 * TCANOPY;
			LECANOPY = LECANOPYCORR;
		} else {
			LECANOPYCORR = LECANOPY;
		}
	}
}

/**
 * @brief STABILITY FUNCTIONS FOR MOMENTUM
 * @param xi
 * @return double
 */
double Canopy::get_psim(const double& xi)
{
	if ( xi <= 0.0 ) {
		// unstable case from Paulsen et al 1970
		const double x = pow((1. - 19. * xi), 0.25); // 19 from Hï¿½gstrï¿½m, 1996
		return log((1. + x) * (1. + x) * (1. + x * x) / 8.) - 2 * atan(x) + mio::Cst::PI / 2.;
	} else {
		// stable case from Holstlag and Bruin, following Beljaars & Holtslag 1991
		static const double a = 1.;
		static const double b = 2./3.;
		static const double c = 5.;
		static const double d = 0.35;
		return -(a * xi + b * (xi - c/d) * exp(-d * xi) + b * c/d);
	}
}

/**
 * @brief STABILITY FUNCTIONS FOR HEAT
 * @param xi
 * @return double
 */
double Canopy::get_psih(const double& xi)
{
	if ( xi <= 0) {
		// Unstable case. Integrated by Paulsen, 1970 from phi-functions found by Businger et al, 1971
		const double x = pow((1. - 11.6 * xi), 0.25);   // 11.6 from Hï¿½gstrï¿½m, 1996
		return (2. * log((1. + x*x) / 2.) );
	} else {
		// Stable case, func=1, equation from Holtslag and De Bruin following Beljaars & Holstlag, 1991
		static const double a = 1.;
		static const double b = 2. / 3.;
		static const double c = 5.;
		static const double d = 0.35;
		return -(pow((1. + 2. / 3. * a * xi), 3. / 2.) + b * (xi - c/d) * exp(-d * xi) + b * c/d - 1.);
	}
}

/**
 * @brief SOLVES EQUATION eta=Ri*Ri2eta(eta) ITERATIVELY
 * @param za
 * @param TempAir
 * @param DiffTemp
 * @param Windspeed
 * @param zom
 * @param zoh
 * @param maxitt
 * @return double
 */
double Canopy::RichardsonToAeta(double za, double TempAir, double DiffTemp,
			      double Windspeed, double zom, double zoh, int maxitt)
{
	// CALCULATE RICHARDSON NUMBER
	const double Ri = 9.81 * DiffTemp * za / (TempAir * Windspeed * Windspeed);
	// STEP 1: Compute function Ri2Eta(Eta)
	double Eta = 0.0;
	double Ri2eta = (log(za / zom) - get_psim(Eta) + get_psim(Eta * zom / za)) *
		(log(za / zom) - get_psim(Eta) + get_psim(Eta * zom / za)) /
		(log(za / zoh) - get_psih(Eta) + get_psih(Eta*zoh/za));
	// STEP 2: Compute error in terms of Ri using etaOld and Ri2eta(etaOld)
	double Error = Eta / Ri2eta - Ri;
	// STEP 3: solve iteratively
	static const double acc = 0.0001;
	int itt=1;
	while ( fabs(Error) > acc && itt <= maxitt ) {
		// 3.1 new Eta
		Eta = Ri2eta * Ri;
		const double divider = (log(za / zoh) - get_psih(Eta) + get_psih(Eta * zoh / za)); //HACK: check with Davide
		if (divider!=0.) {
			Ri2eta = (log(za / zom) - get_psim(Eta) + get_psim(Eta * zom / za)) *
			(log(za / zom) - get_psim(Eta) + get_psim(Eta * zom / za)) /
			divider;
		} else {
			Ri2eta = 1.e12;
		}
		// 3.2 error in terms of Richardson number
		Error = Eta / Ri2eta - Ri;
		// 3.3 update number of iterations
		itt = itt + 1;
	}
	// STEP 4: Return new eta when convergance criteria is fullfilled
	Eta = Ri2eta * Ri;
	// check minimum Monin-Obukhov length (there isthing between 2 to some 100 meters in literature)
	if ( Eta > 0.0 ) {
		const double L = std::max(za, za / Eta);
		Eta = za / L;
	} else if ( Eta < 0.0 ) {
		const double L = std::max(-1000.0, za / Eta);
		Eta = za / L;
	}
	return (Eta);
}

/**
 * @brief Turbulent exchange coefficients canopy model
 * The Shuttelworth and Wallace (1985) approach contains rather complicated
 * formulas compared to the general low level of understanding of within canopy
 * turbulent exchange (which was rightly commented by the referees to our
 * paper Towards an improved...). Also, the old stability correction very
 * easily caused oscillations in the canopy energy balance.
 * To meet the referees and to get rid of the oscillations, David introduced
 * a more simple method, based only on roughness lengths (from Blyth, 1999)
 * Roughness lengths are estimated from canopy height and leaf area index.
 * A new stability correction method based on Monin-Obukhov theory was also
 * instead of the Richardsson number formulation
 * Last update: David, 2005-03-31
 * Stability correction now also below the canopy (using M-O) / DG, 2006-01-09
 * @param *Mdata
 * @param *Xdata
 * @param refheight
 * @param zomg
 * @param wetfraction
 * @param *ch_canopy
 * @param *ce_canopy
 * @param *ce_transpiration
 * @param *ce_interception
 * @param *ce_condensation
 */
void Canopy::CanopyTurbulentExchange(const CurrentMeteo& Mdata, const double& refheight, const double& zomg,
                                        const double& wetfraction, SnowStation& Xdata, double& ch_canopy,
                                        double& ce_canopy, double& ce_transpiration,
                                        double& ce_interception, double& ce_condensation) const
{
	const double karman = 0.4;
	const size_t nE = Xdata.getNumberOfElements();

	// check wind speed to be at least 0.1 m/s
	const double vw_local = (Mdata.vw>0.3)? Mdata.vw : 0.3;

	// canopy height above snow surface
	const double zcan = Xdata.Cdata.height - (Xdata.cH - Xdata.Ground);

	/*
	 * 1. displacement and roughness (mom) according to Shaw and Perreira (1981)
	 * zdisplcan = 0.803 + 0.108 * CanDensMax - (0.462 - 0.086 * CanDensMax) *->
	 *-> exp(-(0.163 + 0.283 * CanDensMax) * Xdata.Cdata.lai);
	 * zdisplcan = std::max(0., std::min(refheight - 0.5, zdisplcan * zcan));

	 * 1.3 roughness length
	 * const double EQ1 = (0.175 - 0.098 * CanDensMax) + (-0.098 + 0.045 * CanDensMax) * log10(Xdata.Cdata.lai);
	 * const double EQ2 = (0.150 - 0.025 * CanDensMax) + (0.122 - 0.0135 * CanDensMax) * log10(Xdata.Cdata.lai);
	 * zomc = std::min(RoughLmax, std::max(zcan * std::min(EQ1, EQ2), RoughLmin)) * CAN_Z0M_COEF;

	 * 1. displacement and roughness as simple scaling of canopy height.
	 * Please note:
	 * 1) Canopy roughness is not allowed to be smaller than roughness of
	 * snow surface as defined by zomg, otherwise ustar may be negative!
         * This is now guaranteed by the computation of RoughLmin = std::max(0.01,zomg)
	 * 2) refheight is already given as height relative the snow surface
	 */

	// Shaw Perreira parameters
	// double CanDensMax = 0.7;
	static const double RoughLmin = 0.01;
	static const double RoughLmax = 100.;
	const double zdisplcan = std::max(0., std::min(refheight - 0.5, Xdata.Cdata.displ_to_canopyheight_ratio * zcan));
	const double zomc = std::max(std::max(RoughLmin, zomg), std::min(RoughLmax, Xdata.Cdata.roughmom_to_canopyheight_ratio * zcan));

	//2. aerodynamic resistances simple approach (Blyth, 1999)
	//2.1 roughness length for scalars (heat and vapour)
	const double zohc = Xdata.Cdata.roughheat_to_roughmom_ratio * zomc;
	const double zohg = Xdata.Cdata.roughheat_to_roughmom_ratio * zomg;
	// update Cdata variables
	Xdata.Cdata.z0m = zomc;
	Xdata.Cdata.z0h = zohc;
	Xdata.Cdata.zdispl = zdisplcan;

	// 2.2 Stability correction (adopted from Beljaars and Holtslag, 1991)
	double psim = 0.0;
	double psih = 0.0;

	if ( Xdata.Cdata.canopy_stabilitycorrection ) {
		// 2.2.1 Get Aeta = Monin-Obukhov stabilityparameter from Richardson number
		const double aeta = RichardsonToAeta(refheight - zdisplcan, Mdata.ta,
			Mdata.ta - Xdata.Cdata.temp, vw_local, zomc, zohc, 5);
		psih = -get_psih(aeta) + get_psih(aeta * zohc / (refheight - zdisplcan));
		psim = -get_psim(aeta) + get_psim(aeta * zomc / (refheight - zdisplcan));
	}

	// 2.3 FRICTION VELOCITY ABOVE CANOPY
	const double ustar = vw_local * karman / (log((refheight - zdisplcan) / zomc) + psim);

	// 2.4 TRANSFER COEFFICIENT FOR SCALARS ABOVE CANOPY
	double ch = Xdata.Cdata.can_ch0 / (Constants::density_air * Constants::specific_heat_air)
		+ ustar * karman / (log((refheight - zdisplcan) / zohc) + psih);
	const double ch_e = ustar * karman / (log((refheight - zdisplcan) / zohc) + psih);

	// 2.5 AERODYNAMIC RESISTANCE ABOVE CANOPY
	Xdata.Cdata.ra = 1. / ch;
	const double ra_e = 1. / ch_e;

	// 2.6 CANOPY TO CANOPY LEVEL RESISTANCE
	if ( log(zomc / zohc) > 0.0 ) {
		Xdata.Cdata.rc = (log(zomc/zohc))/(karman * ustar );
	} else {
		Xdata.Cdata.rc = 0.0;
	}

	// 2.7 SURFACE TO CANOPY LEVEL RESISTANCE
	if (log(zomc / zohg) > 0.) {
		Xdata.Cdata.rs = (log(zomc / zohg)) / (karman * ustar ) * (1. + Xdata.Cdata.can_rs_mult * (1. - exp(-Xdata.Cdata.lai)));
	} else {
		Xdata.Cdata.rs = 0.;
	}

	// 2.8 a stability correction is needed for the surface to canopy level resistance
	if ( Xdata.Cdata.canopy_stabilitycorrection && (Xdata.Cdata.rs > 0.) ) {
		double aeta_g = 0.;
		int i = 0;
		double rs_change = 1.;
		while( (i < 100) && (fabs(rs_change) > 0.0001) ) {
			i++;
			// 1. estimate ustar and ua(zdisplcan) above surface from ras and zomg, zohg, and zref = zdisplcan
			const double ustar_below1 = (1. / Xdata.Cdata.rs) / karman * (log(zdisplcan / zohg)
			               - get_psih(aeta_g) + get_psih(aeta_g * zohg / (zdisplcan)));
			const double vw_zdisplcan = ustar_below1 / karman * (log(zdisplcan / zomg) -
			               get_psim(aeta_g) + get_psim(aeta_g * zomg / (zdisplcan)));
			// 2. estimate aeta above surface
			const double Tsup = (nE>0)? Xdata.Ndata[nE].T : Mdata.ta;
			aeta_g = RichardsonToAeta(zdisplcan, Xdata.Cdata.temp, Xdata.Cdata.temp -
			                             Tsup, vw_zdisplcan, zomg, zohg, 5);
			// 3. new guess of ustar based on uadisplcan and new aeta_g
			const double ustar_below2 = vw_zdisplcan * karman / (log((zdisplcan)/zomg) -
			              get_psim(aeta_g) + get_psim(aeta_g * zomg / (zdisplcan)));

			// 4. TRANSFER COEFFICIENT FOR SCALARS below CANOPY
			ch = ustar_below2 * karman / (log((zdisplcan) / zohg) -
			     get_psih(aeta_g) + get_psih(aeta_g * zohg / (zdisplcan)));

			// 5. new guess for AERODYNAMIC RESISTANCE below CANOPY
			rs_change = 1. / ch - Xdata.Cdata.rs;
			Xdata.Cdata.rs = 1. / ch;
		}
	}

	 /*
	  * Surface resistance for transpiration (van den Hurk et al, 2000)
	  * In case there is no soil data, use the air temperature as guess for the soil temperature,
	  * and skip soil moisture function
	  */
	if ( useSoilLayers ) {
		Xdata.Cdata.rstransp = Xdata.Cdata.rsmin * get_f1(Xdata.Cdata.iswrac)*get_f2f4(Xdata.SoilNode, &Xdata.Edata[0], Xdata.Cdata.wp_fraction, Xdata.Cdata.rootdepth) *
		                  get_f3((1. - Mdata.rh) * Atmosphere::vaporSaturationPressure(Mdata.ta), Xdata.Cdata.f3_gd) / Xdata.Cdata.lai;
	} else {
		const double Temp = (nE>0)? 0. : IOUtils::K_TO_C(Mdata.ta);
		Xdata.Cdata.rstransp = Xdata.Cdata.rsmin * get_f1(Xdata.Cdata.iswrac) * get_f4(Temp) * get_f3((1. - Mdata.rh) *
		                  Atmosphere::vaporSaturationPressure(Mdata.ta), Xdata.Cdata.f3_gd) / Xdata.Cdata.lai;
	}
	// Exchange coefficients sensible heat
	ch_canopy = Constants::density_air * Constants::specific_heat_air / (Xdata.Cdata.ra + Xdata.Cdata.rc);

	// latent heat interception
	if ( Mdata.ta < 273.15 ) {
		ce_condensation  = 0.622 * Constants::lh_sublimation / (Constants::gas_constant_air * Mdata.ta
		                   * Xdata.Cdata.raincrease_snow * (ra_e + Xdata.Cdata.rc));// * std::max(0.1,wetfraction);
		ce_interception  = 0.622 * Constants::lh_sublimation / (Constants::gas_constant_air * Mdata.ta
		                   * Xdata.Cdata.raincrease_snow * (ra_e + Xdata.Cdata.rc));// * wetfraction;
		ce_transpiration = 0.0;
	} else {
		ce_condensation  = 0.622 * Constants::lh_vaporization / (Constants::gas_constant_air * Mdata.ta
		                   * (ra_e + Xdata.Cdata.rc));// * std::max(0.1,wetfraction);
		ce_interception  = 0.622 * Constants::lh_vaporization / (Constants::gas_constant_air * Mdata.ta
		                   * (ra_e + Xdata.Cdata.rc));// * wetfraction;
		ce_transpiration = 0.622 * Constants::lh_vaporization / (Constants::gas_constant_air * Mdata.ta
		                   * (ra_e + Xdata.Cdata.rstransp + Xdata.Cdata.rc));// * (1.0-wetfraction);
	}

	ce_canopy = ce_interception * std::max(0.001, wetfraction) + ce_transpiration * (1.0 - wetfraction);
}

/**
 * @brief Computes upward and downward radiation below and above canopy
 * @param Xdata
 * @param Mdata
 * @param ac
 * @param *iswrac
 * @param *rswrac
 * @param *iswrbc
 * @param *rswrbc
 * @param *ilwrac
 * @param *rlwrac
 * @param *ilwrbc
 * @param *rlwrbc
 * @param CanopyClosureDirect
 * @param RadFracDirect
 * @param sigfdirect
 * @param sigftrunkdirect
 */
void Canopy::CanopyRadiationOutput(SnowStation& Xdata, const CurrentMeteo& Mdata, double ac, double &iswrac, double &rswrac, double &iswrbc, double &rswrbc, double &ilwrac, double &rlwrac, double &ilwrbc, double &rlwrbc, double CanopyClosureDirect, double RadFracDirect, double sigfdirect, double sigftrunkdirect) const
{
	const bool snow = (Xdata.getNumberOfElements() > Xdata.SoilNode);
	const double Tsfc4 = (snow)? Optim::pow4(Xdata.Ndata[Xdata.getNumberOfElements()].T) : Optim::pow4(Mdata.ta);
	// modifs for forestfloor_alb : ag -> ag1
	const double ag1 = (snow)? Xdata.Albedo : Xdata.SoilAlb;

	// modifs for forestfloor_alb
	const size_t nE = Xdata.getNumberOfElements();
	const double age = (snow && forestfloor_alb) ? std::max(0., Mdata.date.getJulian() - Xdata.Edata[nE-1].depositionDate.getJulian()) : 0.; // days
	const double ag = (ag1 -.3)* exp(-age/7.) + 0.3;
	Xdata.Cdata.forestfloor_alb += ag ;

	const double TC4 = Optim::pow4(Xdata.Cdata.temp);
	const double Tt4 = Optim::pow4(Xdata.Cdata.Ttrunk);
	const double sigf = Xdata.Cdata.sigf;
	const double sigftrunk = Xdata.Cdata.sigftrunk;
	const double ec = Xdata.Cdata.ec;
	const double eg = 1.0;
	const double et = Xdata.Cdata.et;
	const double trunkalb = Xdata.Cdata.trunkalb;

	// modifs for 2layercanopy: attfactor stands for "attenuation factor".

	double attfactor_SW = 1. ;
	double attfactor_LW = 1. ;
	double attfactor_SWdir = 1. ;

	if (Twolayercanopy) {
		attfactor_SW = (1. - sigftrunk) ;
		attfactor_LW = (1. - sigftrunk);
		attfactor_SWdir = (1. - sigftrunkdirect) ;
	}

	// Diffuse Shortwave radiation fluxes above and below canopy
	const double  rswrac_loc = iswrac * (sigf * ac + (1-sigf) * (1-sigf) * sigftrunk * trunkalb +  ag * (1.0 - sigf) * (1.0 - sigf) / (1.0 - sigf * ac * ag)*attfactor_SW);
	const double  iswrbc_loc = iswrac * (1. - sigf) / (1.0 - sigf * ac * ag)*attfactor_SW ;
	const double  rswrbc_loc = iswrbc_loc * ag;

	// Direct Shortwave radiation fluxes above and below canopy
	const double  rswrac_loc2 = iswrac * (sigfdirect * ac + (1-sigfdirect) * (1-sigfdirect) * sigftrunkdirect * trunkalb + ag * (1.0 - sigfdirect) * (1.0 - sigfdirect) / (1.0 - sigfdirect * ac * ag)*attfactor_SWdir);
        const double  iswrbc_loc2 = iswrac * (1. - sigfdirect) / (1.0 - sigfdirect * ac * ag) *attfactor_SWdir ;
        const double  rswrbc_loc2 = iswrbc_loc2 * ag;

	// Additional Direct Shortwave radiation term due to trunks direct insolation (optional, default = not used)
	// >> to be weighed by CanClosDirTrunks in the final EB equation
	const double  rswrac_loc3 = iswrac * (sigftrunkdirect * trunkalb +  ag * (1.0 - sigfdirect) / (1.0 - sigfdirect * ac * ag) *attfactor_SWdir) ;
	const double  iswrbc_loc3 = iswrac * attfactor_SWdir / (1.0 - sigfdirect * ac * ag) ;
	const double  rswrbc_loc3 = iswrbc_loc3 * ag;

	// Longwave radiation fluxes above and below canopy:
	const double  RAG = eg *(-Constants::stefan_boltzmann*Tsfc4 +((1.-sigf)*ilwrac*attfactor_LW + sigf*ec*Constants::stefan_boltzmann*TC4*attfactor_LW + eg*sigf*(1.-ec)*attfactor_LW*Constants::stefan_boltzmann*Tsfc4)/(1. - sigf * (1. - ec) * (1. - eg))) + eg * et * (1.-attfactor_LW)* Constants::stefan_boltzmann * Tt4 * (1. + sigf * (1. - ec));

	const double  RAV = sigf * ec*( ilwrac - 2. * Constants::stefan_boltzmann * TC4 + attfactor_LW *(Constants::stefan_boltzmann *( eg * Tsfc4 + ec * sigf * TC4 * (1.-eg) ) + (1.0 - sigf) * (1.0 - eg) * ilwrac) / (1.0 - sigf * (1.0 - ec)* ( 1.0 - eg)) + et * (1.-attfactor_LW)* Constants::stefan_boltzmann * Tt4 *(1. + (1. - eg)) );

	const double  RAT =  -2 * et *(1.-attfactor_LW)* Constants::stefan_boltzmann * Tt4 + et *(1.-attfactor_LW) * ( ec * sigf * Constants::stefan_boltzmann * TC4 + eg * Constants::stefan_boltzmann * Tsfc4 + ilwrac*(1. - sigf));

	ilwrbc = RAG / eg + Constants::stefan_boltzmann * Tsfc4;
	rlwrbc = (1 - eg)* ilwrbc + eg * Constants::stefan_boltzmann * Tsfc4;
	rlwrac = ilwrac - RAG - RAV -RAT;

	// Scaling of results with CanopyClosureDiffuse and CanopyClosureDirect
	const double  CanopyClosureDiffuse = 1. - Xdata.Cdata.direct_throughfall;

	if (Twolayercanopy) {
		double CanClosDirLeaves = (canopytransmission)? CanopyShadeSoilCover(Xdata.Cdata.height, CanopyClosureDiffuse, Mdata.elev,Xdata.Cdata.can_diameter) : CanopyClosureDiffuse;
		double CanClosDirTrunks = 0;
		/*if (canopytransmission) { // below (optional): if uncommented, allows direct solar insolation of the trunks
			CanClosDirLeaves = CanopyShadeSoilCover(Xdata.Cdata.height*(1. - trunk_frac_height), CanopyClosureDiffuse, Mdata.elev);
			CanClosDirTrunks = CanopyShadeSoilCover(Xdata.Cdata.height, CanopyClosureDiffuse, Mdata.elev)- CanClosDirLeaves;
		}*/

		// Shortwave fluxes (diffuse)
		rswrac = (rswrac_loc * CanopyClosureDiffuse + iswrac * ag * (1.0 - CanopyClosureDiffuse)) * (1.0 - RadFracDirect);
		iswrbc = (iswrbc_loc * CanopyClosureDiffuse + iswrac * (1.0 - CanopyClosureDiffuse)) * (1.0 - RadFracDirect);
		rswrbc = (rswrbc_loc * CanopyClosureDiffuse + iswrac * ag * (1.0 - CanopyClosureDiffuse)) * (1.0 - RadFracDirect);

		// Shortwave fluxes (direct)
		rswrac += (rswrac_loc2 * CanClosDirLeaves + rswrac_loc3 * CanClosDirTrunks + iswrac * ag * (1.0 - CanClosDirTrunks - CanClosDirLeaves)) * RadFracDirect;
		iswrbc += (iswrbc_loc2 * CanClosDirLeaves + iswrbc_loc3 * CanClosDirTrunks + iswrac * (1.0 - CanClosDirTrunks - CanClosDirLeaves)) * RadFracDirect;
		rswrbc += (rswrbc_loc2 * CanClosDirLeaves + rswrbc_loc3 * CanClosDirTrunks + iswrac * ag * (1.0 - CanClosDirTrunks - CanClosDirLeaves)) *RadFracDirect;

		// Longwave fluxes (treat as diffuse)
		rlwrac = rlwrac * CanopyClosureDiffuse + Constants::stefan_boltzmann * eg * Tsfc4 * (1.0-CanopyClosureDiffuse);
		ilwrbc = ilwrbc * CanopyClosureDiffuse + ilwrac * (1.0 - CanopyClosureDiffuse);
		rlwrbc = rlwrbc * CanopyClosureDiffuse + Constants::stefan_boltzmann * eg * Tsfc4 * (1.0-CanopyClosureDiffuse);

		// radiations to trunks
		Xdata.Cdata.SWnet_Trunks = (1.0 - RadFracDirect) * iswrac * (1. -sigf) * (1.-trunkalb)*(1-attfactor_SW) * CanopyClosureDiffuse
				+ CanClosDirLeaves * RadFracDirect *iswrac * (1. -sigfdirect) * (1.-trunkalb)*(1. - attfactor_SWdir)
                                + CanClosDirTrunks *  RadFracDirect *iswrac * (1.-trunkalb)*(1. - attfactor_SWdir) ;
		Xdata.Cdata.LWnet_Trunks = RAT  * CanopyClosureDiffuse ;

	 } else {
		// Shortwave fluxes (diffuse)
		rswrac = (rswrac_loc * CanopyClosureDiffuse + iswrac * ag * (1.0 - CanopyClosureDiffuse)) * (1.0 - RadFracDirect);
		iswrbc = (iswrbc_loc * CanopyClosureDiffuse + iswrac * (1.0 - CanopyClosureDiffuse)) * (1.0 - RadFracDirect);
		rswrbc = (rswrbc_loc * CanopyClosureDiffuse + iswrac * ag * (1.0 - CanopyClosureDiffuse)) * (1.0 - RadFracDirect);

		// Shortwave fluxes (direct)
		rswrac += (rswrac_loc2 * CanopyClosureDirect + iswrac * ag * (1.0 - CanopyClosureDirect)) * RadFracDirect;
		iswrbc += (iswrbc_loc2 * CanopyClosureDirect + iswrac * (1.0 - CanopyClosureDirect)) * RadFracDirect;
		rswrbc += (rswrbc_loc2 * CanopyClosureDirect + iswrac * ag * (1.0 - CanopyClosureDirect)) *RadFracDirect;

		// Longwave fluxes (treat as diffuse)
		rlwrac = rlwrac * CanopyClosureDiffuse + Constants::stefan_boltzmann * eg * Tsfc4 * (1.0-CanopyClosureDiffuse);
		ilwrbc = ilwrbc * CanopyClosureDiffuse + ilwrac * (1.0 - CanopyClosureDiffuse);
		rlwrbc = rlwrbc * CanopyClosureDiffuse + Constants::stefan_boltzmann * eg * Tsfc4 * (1.0-CanopyClosureDiffuse);
	 }
}

/**
 * @brief MAIN CANOPY FUNCTION CALLED BY Meteo.c
 * This routine computes interception of precipitation and radiation,
 * and reduction of turbulent exchange in a canopy layer above the ground.
 * Computations are made in the following order:
 * 1. Preliminar mass balance (interception and throughfall)
 * 2. Canopy surface energy balance (net radiation, sensible and latent
 * heat fluxes)
 * 3. Final mass balance (evaporation of intercepted water, and
 * transpiration
 * @param Mdata CurrentMeteo
 * @param Xdata Profile
 * @param roughness_length
 * @param height_of_wind_val
 * @param adjust_VW_height if set to false, assumes a constant measurement height for wind values (default: true, ie.
 * take into account the snow height decreasing the sensor height above the surface)
 * @return true if the canopy module could be used, false if not (canopy under the snow, etc)
 */
bool Canopy::runCanopyModel(CurrentMeteo &Mdata, SnowStation &Xdata, const double& roughness_length, const double& height_of_wind_val, const bool& adjust_VW_height)
{
	Twolayercanopy = Twolayercanopy_user; //so we can temporarily overwrite the user's choice if needed
	const double hs = Xdata.cH - Xdata.Ground;
	const size_t nE = Xdata.getNumberOfElements();
	//no canopy or no canopy above the snow
	if ( (Xdata.Cdata.lai <= 0.0) || (Xdata.Cdata.height <= 0.0) || ((Xdata.Cdata.height - 0.01) < hs)) {
		Xdata.Cdata.zdispl = -0.7;
		return false;
	}

	if (Xdata.Cdata.direct_throughfall==1.)
		throw InvalidArgumentException("Can not use Canopy with CanopyDirectThroughfall == 1", AT);

	// Check that some important initial values are within reasonable bounds
	if ( Xdata.Cdata.temp < 203.15 ) {
		Xdata.Cdata.temp = 273.15;
	}
	if ( Xdata.Cdata.storage < 0.0 ) {
		Xdata.Cdata.storage = 0.0;
	}

	Xdata.Cdata.snowfac += Mdata.solid_psum; // Mdata.psum * (1. - Mdata.psum_ph);
	Xdata.Cdata.rainfac += Mdata.liq_psum; //  Mdata.psum * Mdata.psum_ph;

	// 1.1 compute the interception capacity [mm m-2]
	const double intcapacity = IntCapacity(Mdata, Xdata);

	// 1.2 compute direct unload [mm timestep-1], update storage [mm]
	double unload = IntUnload(intcapacity, Xdata.Cdata.storage);
        double oldstorage = Xdata.Cdata.storage;
        Xdata.Cdata.storage -= unload;
	double liqmm_unload=0.0;
        double icemm_unload=0.0;
	const double intcaprain = IntCapacity(Mdata, Xdata, true);
	// determine liquid and frozen water unload
	liqmm_unload = std::max(0.0,std::min(unload * Xdata.Cdata.liquidfraction,
			oldstorage * Xdata.Cdata.liquidfraction-intcaprain));
	icemm_unload = std::max(unload - liqmm_unload,0.0);
	// Update liquid fraction
	if (Xdata.Cdata.storage>0.) {
		Xdata.Cdata.liquidfraction = std::max( 0.0, (oldstorage*Xdata.Cdata.liquidfraction-liqmm_unload)/Xdata.Cdata.storage );
	} else {
		Xdata.Cdata.liquidfraction = 0.0;
	}
	if ( unload < 0.0 ) {
		prn_msg(__FILE__, __LINE__, "wrn", Mdata.date, "Negative unloading!!!");
		unload = 0.0;
	}

	// 1.3 compute the interception [mm timestep-1] and update storage [mm]
	const double interception = IntRate(intcapacity, Xdata.Cdata.storage, Mdata.solid_psum+Mdata.liq_psum, Xdata.Cdata.direct_throughfall, Xdata.Cdata.interception_timecoef);
	oldstorage = Xdata.Cdata.storage;
	Xdata.Cdata.storage += interception;
	// 1.4 compute the throughfall [mm timestep-1] (and update liquid fraction if SnowMIP)
	const double throughfall = Mdata.solid_psum + Mdata.liq_psum - interception + unload;
	double icemm_interception = 0.0 ; // (Mdata.psum>0.)? interception * (1. - Mdata.psum_ph) : 0.;
	double liqmm_interception = 0.0 ; //(Mdata.psum>0.)? interception * Mdata.psum_ph : 0.;
	const double ground_solid_precip = Mdata.solid_psum; //  Mdata.psum * (1.-Mdata.psum_ph) - icemm_interception + icemm_unload;
	const double ground_liquid_precip = Mdata.liq_psum; //  Mdata.psum * Mdata.psum_ph - liqmm_interception + liqmm_unload;
	//Mdata.psum = ground_solid_precip + ground_liquid_precip;
	//Mdata.psum_ph = (Mdata.psum>0)? ground_liquid_precip / Mdata.psum : 1.;

	if (Xdata.Cdata.storage>0.) {
		Xdata.Cdata.liquidfraction = std::max(0.0,std::min(1.0,(oldstorage*Xdata.Cdata.liquidfraction+liqmm_interception)/Xdata.Cdata.storage));
	}

	// 2.1 prepare for canopy energy balance
	// Wetfraction update is moved to canopy energy balance loop  - use old value first
	double wetfrac = Xdata.Cdata.wetfraction;

	// Radiation Transmissivity
	//(could possibly be a function of interception - but is constant for the moment)
	//First, transmissivity of diffuse (and longwave) radiation
	const double epsilon = 1e-3;
	double lai_frac_top = Xdata.Cdata.lai_frac_top_default;// fraction of the total lai attributed to the uppermost layer. If 1.,equivalent to 1-layer canopy.
	if ((lai_frac_top < epsilon)||(1-lai_frac_top<epsilon)||Twolayercanopy==false) {
		Twolayercanopy = false;
		lai_frac_top = 1.;
	}
	Xdata.Cdata.sigf = CanopyTransmissivity(lai_frac_top*Xdata.Cdata.lai, Constants::pi / 2.0, Xdata.Cdata.krnt_lai );
	Xdata.Cdata.sigftrunk = CanopyTransmissivity((1-lai_frac_top)*Xdata.Cdata.lai, Constants::pi / 2.0, Xdata.Cdata.krnt_lai );

	// Secondly, transmissivity of direct solar radiation
	const double sigfdirect = (canopytransmission)? CanopyTransmissivity(lai_frac_top*Xdata.Cdata.lai, Mdata.elev,Xdata.Cdata.krnt_lai ) : Xdata.Cdata.sigf;
        const double sigftrunkdirect =(canopytransmission)? CanopyTransmissivity((1-lai_frac_top)*Xdata.Cdata.lai, Mdata.elev,Xdata.Cdata.krnt_lai ) : Xdata.Cdata.sigftrunk;

	/*
	 * Reference Height [m above snow surface] for meteo input, at least 2 m above canopy height above snow surface
	 * 2006-03-01: introduced new definition of reference height above canopy for Alpine3D applications
	 * 2006-06-02: this should work also without soil data, since Xdata->Ground is initialized to 0.0
	*/
	const double ref_height = (!adjust_VW_height)? Xdata.Cdata.height+height_of_wind_val : height_of_wind_val;
	double zref = std::max( 2.0 + (Xdata.Cdata.height - hs), ref_height - hs );
	const double z0m_ground = (hs>0.03)? roughness_length : Xdata.BareSoil_z0;
	Mdata.z0 = (hs>0.03)? roughness_length : Xdata.BareSoil_z0;

	/*
	 * Turbulent Transport Coefficients:
	 * ch_canopy        = sensible heat
	 * ce_transpiration = latent heat from dry fraction, incl stomatal control.
	 * ce_interception  = latent heat from wet fraction
	 * ce_canopy        = latent heat total
	 * cm_canopy        = momentum (through the canopy, i.e to estimate wind speed below)
	*/
	double ch_canopy, ce_transpiration, ce_interception, ce_canopy, ce_condensation;
	CanopyTurbulentExchange(Mdata, zref, z0m_ground, wetfrac, Xdata, ch_canopy, ce_canopy,
	                           ce_transpiration, ce_interception, ce_condensation);

	/*
	 * 2.2 Energy balance of the canopy
	 * The main purpose is to estimate the evapotranspiration loss,
	 * and the radiation balance of the canopy, which influences the
	 * snowpack below and the reflection/emittance to the atmosphere.
	 * Method:
	 * The energy balance of the canopy is assumed to be equal to,
	 * (1)   RnCanopy=HCanopy+LECanopy
	 * where RnCanopy is a function of a) incoming shortwave and longwave
	 * radiation, b) albedo, transmissivity, emissivity and temperature of
	 * the canopy, and c) albedo, emissivity and temperature of the ground
	 * below, taking multiple reflection into account (see documentation).
	 * Sensible and latent heat fluxes are computed using the standard
	 * bulk formulations, assuming an logarithmic wind profile above the
	 * canopy. The stomatal control of transpiration is represented by an
	 * additional surface resistance, estimated as a function of incoming
	 * solar radiation, atmospheric vapour pressure deficit and soil water
	 * content
	 * Numerical solution  following SiSPAT manual (Isabelle Braud, 2000):
	 * Equation (1) is linearised around the canopy temperature at time t
	 * using the temperature from the previous timestep t-1, so that:
	 * (2) RnCanopy  = r0  + r1  * TempCanopy(t)
	 * (3) HCanopy   = h0  + h1  * TempCanopy(t)
	 * (4) LECanopy  = le0 + le1 * TempCanopy(t)
	 * TempCanopy(t) is given by inserting eq (2)-(4) into (1).
	 * See the functions for r0,r1, h0,h1, le0, and le1 for details
	 * Alternative (to be implemented shortly):
	 * (1) RnCanopy = HCanopy + LECanopy + DQCanopy
	 * where DQCanopy = change of heat content of Canopy
	 * (5) DQCanopy = dq0+dq1 * TempCanopy(t)
	 * and dq/dt = HeatCapacity * (TempCanopy(t)-TempCanopy(t-1)
	 * HeatCapacity = HeatCapacityDry + IntStorage(t-1) * L
	 * INPUT: emissivity eground, surface temperature TGROUND, and albedo ground ALBGROUND
	 * Start of Energy Balance Loop, 3 iterations should be enough in most cases
	*/

	// local energy flux variables
	double RNCANOPY=0., HCANOPY=0., LECANOPY=0., LECANOPYCORR=0.;
	double iswrac, rswrac, iswrbc, rswrbc, ilwrac, rlwrac, ilwrbc, rlwrbc, rsnet=IOUtils::nodata;

	// local auxiliary variables
	double canopyalb=IOUtils::nodata;
	double h0, h1, ht0 = 0, ht1 = 0, le0, le1, let0, let1, HM0 = 0, HM1 = 0, HMt0 = 0, HMt1 = 0, TT0, TT1;
	double r0, r1, r2, rt0, rt1, rt2;
	double TC_previous_tstep = Xdata.Cdata.temp;
	double Tt_previous_tstep = Xdata.Cdata.Ttrunk;
	double canopyclosuredirect=IOUtils::nodata, radfracdirect=IOUtils::nodata, r1p, r2p;

	double CanopyEvaporation=0., INTEVAP=0., TRANSPIRATION=0.;

	// calculate Canopy Heat Mass based on canopy basal area and LAI
	CalculateHeatMass(Xdata.Cdata.height, Xdata.Cdata.BasalArea, Xdata.Cdata.lai, Xdata.Cdata.HMLeaves, Xdata.Cdata.HMTrunks, Xdata.Cdata.biomass_density, Xdata.Cdata.biomass_heat_capacity);

	for (int ebalitt = 0; ebalitt < 7; ebalitt++ ) {
		const double TC_OLD = Xdata.Cdata.temp; // Cdata.temp is updated in the iteration...

		// update ce_canopy as function of wetfraction
		ce_canopy = std::max(0.001 * ce_interception, ce_interception * wetfrac + ce_transpiration * (1.0 - wetfrac));
		ce_condensation = ce_interception * std::max(0.1, wetfrac);

		// canopy albedo
		canopyalb = CanopyAlbedo(Mdata.ta, wetfrac,Xdata);

		// compute properties r0 and r1 in eq (2) (and downward lw and sw for snowpack model)
		if (Twolayercanopy) {
			LineariseNetRadiation2L(Mdata, Xdata.Cdata, Xdata, iswrac, rsnet, ilwrac, r0, r1, r2, rt0, rt1, rt2,
					canopyalb, canopyclosuredirect, radfracdirect, sigfdirect,sigftrunkdirect, r1p, r2p);
		} else {
			LineariseNetRadiation(Mdata, Xdata.Cdata, Xdata, iswrac, rsnet, ilwrac, r0, r1,
						canopyalb, canopyclosuredirect, radfracdirect, sigfdirect, r1p);
			r2 = 1. ; rt0=0.; rt1 = 0.; rt2 = 0. ; r2p =0.;
		}


		// compute properties h0 and h1 in eq (3)
		// NOTE: for sparse canopies turbulent fluxes should be scaled in the
		// canopy EB calculation; for the moment scalingfactor is sigf*(1-direct_throughfall)
		LineariseSensibleHeatFlux(ch_canopy, Mdata.ta, h0, h1, Xdata.Cdata.sigf*(1. - Xdata.Cdata.direct_throughfall));
		if (Twolayercanopy) {
			LineariseSensibleHeatFlux(ch_canopy, Mdata.ta, ht0, ht1,  Xdata.Cdata.sigftrunk*(1. - Xdata.Cdata.direct_throughfall));
		}

		// compute properties le0 and le1 in eq (4)
		LineariseLatentHeatFlux(ce_canopy, Xdata.Cdata.temp, Mdata.rh*Atmosphere::vaporSaturationPressure(Mdata.ta), le0, le1, Xdata.Cdata.sigf*(1. - Xdata.Cdata.direct_throughfall));
		// NOTE: for the moment trunks do not exchange latent heat (no interception, no transpiration)
		let1= 0. ; let0 = 0. ;

		if (Twolayercanopy) {
			LineariseConductiveHeatFlux(TC_previous_tstep, Xdata.Cdata.HMLeaves, HM0, HM1, M_TO_H(calculation_step_length), 1.);
			LineariseConductiveHeatFlux(Tt_previous_tstep, Xdata.Cdata.HMTrunks, HMt0, HMt1, M_TO_H(calculation_step_length), 1.);
			// final canopy energy balance
			//Objective: Derive an analytical expression for Ttrunk = f(TC) from the Trunk Energy Balance equation, then solve easily the Canopy Energy Balance.
			//Trunk energy balance equation : rt0 + rt1 * Ttrunk + rt2 * TC = h0t + h1t *Ttrunk + let0 + let1 * Ttrunk + HMt0 + HMt1 * Ttrunk
			// =>      Ttrunk = (ht0 + let0 + HMt0 - rt0 -  r2t *TC) / (r1t - ht1 -let1 -HMt1) rewritten as :  Ttrunk = TT0/r2 + TT1/r2 * TC
			// so that :       netradcanopy = r0 + r1 * TC + TT0 + TT1 * TC, to be solved for TC
			TT0 = r2 * (ht0 + let0 + HMt0 - rt0) / (rt1 - ht1 -let1 -HMt1);
			TT1 = -r2 * rt2 /(rt1 - ht1 -let1 -HMt1);
			Xdata.Cdata.Ttrunk = (ht0 + let0 + HMt0 - rt0) / (rt1 - ht1 -let1 -HMt1) - rt2 /(rt1 - ht1 -let1 -HMt1) * Xdata.Cdata.temp ;

			CanopyEnergyBalance2L(h0, h1, le0, le1, HM0, HM1, TT0, TT1,
					ce_canopy, ce_condensation,
					r0, r1, r2, Xdata.Cdata.temp, Xdata.Cdata.Ttrunk, RNCANOPY, HCANOPY, LECANOPY);
		} else {
			LineariseConductiveHeatFlux(TC_previous_tstep, Xdata.Cdata.HMTrunks, HM0, HM1, M_TO_H(calculation_step_length),  1.);
			//final canopy energy balance
			TT0 = 0. ;
			TT1 = 0. ;

			CanopyEnergyBalance(h0, h1, le0, le1, HM0, HM1,
					ce_canopy, ce_condensation,
					r0, r1, Xdata.Cdata.temp, RNCANOPY, HCANOPY, LECANOPY);
		}

		// Partition latent heat flux on interception and transpiration
		// and correct energy balance for overestimated interception evaporation
		if (Twolayercanopy) {
			CanopyEvaporationComponents2L(ce_canopy, ce_transpiration, LECANOPY, Mdata.ta,
						Xdata.Cdata.storage,
						M_TO_H(calculation_step_length), CanopyEvaporation, INTEVAP, TRANSPIRATION,
						RNCANOPY, HCANOPY, Xdata.Cdata.temp, Xdata.Cdata.Ttrunk,
						TT0, TT1, r0, r1, r2, h0, h1, LECANOPYCORR,
						wetfrac, HM0, HM1);
		} else {
			CanopyEvaporationComponents(ce_canopy, ce_transpiration, LECANOPY, Mdata.ta,
						Xdata.Cdata.storage,
						M_TO_H(calculation_step_length), CanopyEvaporation, INTEVAP, TRANSPIRATION,
						RNCANOPY, HCANOPY, Xdata.Cdata.temp, r0, r1, h0, h1, LECANOPYCORR,
						wetfrac, HM0, HM1);
		}

		const double newstorage = Xdata.Cdata.storage - INTEVAP;

		// wet surface fraction
		wetfrac = CanopyWetFraction(intcapacity, newstorage);
		// Changes of temperature induce changes in stability correction.
		// re-computation of turbulent exchange coefficient is needed in case of big changes in TC.
		if (fabs(Xdata.Cdata.temp - TC_OLD) > Xdata.Cdata.canopytemp_maxchange_perhour * M_TO_H(calculation_step_length)) {
			CanopyTurbulentExchange(Mdata, zref, z0m_ground, wetfrac, Xdata, ch_canopy, ce_canopy,
			ce_transpiration, ce_interception, ce_condensation);
		}

               Xdata.Cdata.temp = (Xdata.Cdata.temp+TC_OLD)*0.5;
	       wetfrac = (Xdata.Cdata.wetfraction+wetfrac)*0.5;
	} // End of Energy Balance Loop

	// Now REDUCE WaterContent in the Soil Elements --- Could also be part of WaterTransport.c
	if (useSoilLayers)
		SoilWaterUptake(Xdata.SoilNode, TRANSPIRATION, &Xdata.Edata[0], Xdata.Cdata.wp_fraction, Xdata.Cdata.rootdepth, Xdata.Cdata.h_wilt);

	// final adjustment of interception storage due to evaporation
	Xdata.Cdata.storage = Xdata.Cdata.storage - INTEVAP;


	/*
	 * Preparation of output variables using += sign to allow for cumulated or averaged output
	 * (remember to reset these variables to 0 in Main.c before next integration step)
	*/
	// radiation above and below canopy
	CanopyRadiationOutput(Xdata, Mdata, canopyalb, iswrac, rswrac, iswrbc, rswrbc, ilwrac,
	                         rlwrac, ilwrbc, rlwrbc,canopyclosuredirect,radfracdirect,sigfdirect,sigftrunkdirect);

	// longwave and shortwave radiation components
	Xdata.Cdata.iswrac += iswrac;
	Xdata.Cdata.rswrac += rswrac;
	Xdata.Cdata.iswrbc += iswrbc;
	Xdata.Cdata.rswrbc += rswrbc;
	Xdata.Cdata.ilwrac += ilwrac;
	Xdata.Cdata.rlwrac += rlwrac;
	Xdata.Cdata.ilwrbc += ilwrbc;
	Xdata.Cdata.rlwrbc += rlwrbc;

	// Net longwave and shortwave radiation of canopy [W m-2]
	Xdata.Cdata.rlnet += RNCANOPY-rsnet;
	Xdata.Cdata.rsnet += rsnet;

	// atmospheric emissivity as seen by surface below canopy with regard to air temperature
	Mdata.ea = ilwrbc / (Constants::stefan_boltzmann * Optim::pow4(Mdata.ta));

	// downward and reflected shortwave below canopy
	Mdata.rswr = rswrbc;
	Mdata.iswr = iswrbc;

	// Adjust friction velocity below canopy using the same reference height as in Meteo.c
	zref = std::max(0.5, height_of_wind_val - hs);
	Mdata.ustar = 0.74 * log(zref / z0m_ground) / 0.4 / (Xdata.Cdata.ra + Xdata.Cdata.rs);

	// Canopy turbulent heat fluxes [W m-2]
	Xdata.Cdata.sensible += HCANOPY;
	Xdata.Cdata.latent += LECANOPY;
	Xdata.Cdata.latentcorr += LECANOPYCORR;

	// Canopy evaporative fluxes [mm]
	Xdata.Cdata.transp += TRANSPIRATION;
	Xdata.Cdata.intevap += INTEVAP;

	// Canopy mass fluxes [mm]
	Xdata.Cdata.interception += interception;
	Xdata.Cdata.throughfall += throughfall;
	// modifs for SnowMIP version
	// NOTE: in the standard version (PSUM version), water do not unload since intcaprain does not evolve in time.
	//       => all unload is therefore snow.
	Xdata.Cdata.snowunload += icemm_unload;

	// Canopy auxiliaries
	Xdata.Cdata.wetfraction = wetfrac;
	Xdata.Cdata.intcapacity += intcapacity;
	Xdata.Cdata.canopyalb += canopyalb;
	const double albedo = (nE>Xdata.SoilNode)? Xdata.Albedo : Xdata.SoilAlb;
	Xdata.Cdata.totalalb += TotalAlbedo(canopyalb, Xdata.Cdata.sigf, albedo,
	                                    Xdata.Cdata.direct_throughfall, canopyclosuredirect, radfracdirect, sigfdirect);
	// modifs for HeatMass and 2layercanopy: new fluxes, to be updated here for EB closure reasons
	Xdata.Cdata.CondFluxCanop += HM0 + HM1 * Xdata.Cdata.temp;
	if (Twolayercanopy) {
		Xdata.Cdata.CondFluxTrunks += HMt0 + HMt1 * Xdata.Cdata.Ttrunk;
		Xdata.Cdata.QStrunks += ht0 + ht1 * Xdata.Cdata.Ttrunk;
	}

	return true;
}
