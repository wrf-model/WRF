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

#include <cstddef> //needed for size_t

#include <snowpack/snowpackCore/Metamorphism.h>
#include <snowpack/Constants.h>
#include <snowpack/Utils.h>
#include <snowpack/snowpackCore/Snowpack.h>

using namespace std;
using namespace mio;

/**
 * @file Metamorphism.cc
 * @brief This module contains the snow metamorphism routines of the SLF one-dimensional snowpack model \n
 * It represents a truly international research effort:
 * Dr. Bob "Borolo" Brown of montana state university (USA) provided a lot of the motivation and
 * low temperature gradient micro-structure physics;
 * Dr. Michael provided project leadership, a sense of the practical and more importantly, the link to the
 * avalanche warning group, i.e. he told everybody what to do;
 * Dr. Pramod Sataywali (SASE, INDIA) came up with the high temperature gradient micro-structure
 * routines;
 * Dr. Perry Bartelt wrote the code and integrated it into a very sensible continuum mechanics
 * model that works pretty well.
 *
 * Definition of some of the essential variables:
 *
 * PRIMARY micro-structure parameters computed by the Metamorphism routine:
 *
 * - rb : bond radius in [mm]
 * - rg : grain radius in [mm]
 * - dd : snow dendricity (=0 old snow; =1 new snow)
 * - sp : snow sphericity (=0 faceted; =1 rounded)
 * - mk : microstructure marker:
 *                            - 0 dendritic snow or neither faceted nor rounded
 *                            - 1 reached a sp of 0
 *                            - 2 reached a sp of 1
 *                            - 3 Surface Hoar SH
 *                            - 4 Graupel PPgp
 *                            - 5 Not implemented yet --> thin crusts
 *                            - 6 Technical Snow
 *                            - 7 Glacier ice
 *                            - 8 Ice layer IFil
 *                            - 9 Pure water on top of snowpack, soil, or road
 *                            - mk < 10, mk=mk+10 : first complete wetting
 *                            - mk < 20, mk=mk+10 : first melt-freeze cycle completed
 *                            - mk / 100 >= 1     : tagged snow layer
 *                            - mk / 1000 >= 9    : marked reference level to reference height of wind and meteo values, as well as measured snow height 
 *
 * SECONDARY micro-structure parameters computed by Metamorphism routine:
 * - N3   : coordination number (1)
 *
 * SECONDARY micro-structure parameters computed by routines found in Laws_sn.c:
 * - keff : effective conductivity (W m-1 K-1)
 * - E    : modulus of elasticity  (Pa)
 * - eta  : viscosity (Pa s)
 *
 * These are the variables needed to compute these values of the
 * ElementData class within the snowStation class:
 * - theta[ICE]   : volumetric ice content (1)
 * - theta[WATER] : volumetric water (1)
 * - Te           : temperature (K)
 * - dTdZ         : temperature gradient (K m-1)
 * - dPdZ         : vapor pressure gradient (bar m-1)
 * - Rho          : bulk density (kg m-3)
 * - S            : overburden stress (Pa)
 *
 * The french metamorphism routines were written in November 1995 by Perry Bartelt
 * and Martin Schneebeli.  They were first used in the 2d snowpack code haefeli.
 */

/************************************************************
 * static section                                           *
 ************************************************************/

///Defines the vapor pressure gradient at which TG metamorphism begins (hPa m-1)
const double Metamorphism::mm_tg_dpdz = 5.;
const double Metamorphism::ba_g_fudge = 3.; ///< brief Defines Thorsten's Geometry FUDGE
const double Metamorphism::sa_g_fudge = 0.35; ///< Defines Satyawali's Geometry FUDGE

/// @name Grain and bond growth
//@
const double Metamorphism::max_grain_growth = 5.; ///< A grain cannot grow more than 5.0 mm d-1

///Bond radius usually will not grow larger than Metamorphism::bond_size_stop * grain radius
const double Metamorphism::bond_size_stop = 0.75;

///Absolute limit of grain to bond ratio (SH, initialisation)
const double Metamorphism::max_grain_bond_ratio = 0.95;
//@}

///@name Thresholds for wind slab formation
//@
///For no action, set strength factor for wind slab formation to 0.0
const double Metamorphism::wind_slab_enhance = 5.;

///Wind slab forms for winds stronger than Metamorphism::wind_slab_vw (m s-1)
const double Metamorphism::wind_slab_vw = 5.;

/// Wind slab formation down to Metamorphism::wind_slab_depth (m)
const double Metamorphism::wind_slab_depth = 0.07;
//@}


map<string, MetaModelFn> Metamorphism::mapMetamorphismModel;
map<string, MetaSpRateFn> Metamorphism::mapSpRate;
const bool Metamorphism::__init = Metamorphism::initStaticData();

bool Metamorphism::initStaticData()
{
	mapMetamorphismModel["DEFAULT"] = &Metamorphism::metamorphismDEFAULT;
	mapMetamorphismModel["NIED"]    = &Metamorphism::metamorphismNIED;

	mapSpRate["DEFAULT"] = &Metamorphism::spRateDEFAULT;
	mapSpRate["NIED"]    = &Metamorphism::spRateNIED;

	return true;
}

/**
 * @brief This routine estimates the cross sectional pore area
 * @param Edata
 * @return area (mm2)
 */
double Metamorphism::csPoreArea(const ElementData& Edata)
{
	const double rg = MM_TO_M(Edata.rg); // Grain and bond radius (m)

	return((Constants::pi * rg*rg / Edata.theta[ICE]) * (1. - Edata.theta[ICE] - Edata.theta[WATER]));
}

/**
 * @brief This routine estimates the coordination number as a function of the snow density,
 * used for both the FRENCH and MONTANA metamorphism models.  The MONTANA model
 * uses the coordination number to determine the thermal conductivity of snow. The
 * constants are based on experimental results. This is  a typical montana State
 * University routine -- a bunch of defines with a lot of unusual constants, a bunch
 * of multiplications and then a return.  And they expect us to believe this stuff.
 * @param Rho Snow density (kg m-3)
 * @return Coordination number (1)
 */
 /* This reminds me of a poem by Auden:  "The History of Tru.h>
 *
 * In that ago when being was believing,
 * TRUTH was the most of many CREDIBLES,  (UNLIKE this routine which is UNCREDIBLE)
 * More first, more always, than a bat-winged lion,
 * A fish-tailed dog or eagle-headed fish, (i.e, fantasy=this routine)
 * The least like mortals, doubted by their deaths. (the programmer should die)
 *
 * Truth was their MODEL as they strove to BUILD (we are doing this too)
 * A world of LASTING objects to believe in,(I would like to believe in this stuff)
 * Without believing earthernware and legend,
 * Archway and song, were truthful or untruthful,
 * The TRUTH was there already to be TRUE.
 *
 * This while when, PRACTICAL like paper-dishes, (This routine is, however, useful)
 * TRUTH is CONVERTIBLE to KILOWATTS,
 * Our last to do by is an ANTI-MODEL,
 * SOME UNTRUTH anyone can give the LIE to,
 * A NOTHING no one need believe is there.  (I wish this routine wasn't here)
 *
 * This poem basically sums up Perry's feeling about modelling micro-structure.
 */
double Metamorphism::getCoordinationNumberN3(const double& Rho)
{
	// Outside Rho between 100 kg/m3 and 670 kg/m3, use the following:
	if ( Rho >= 670. ) {
		return 10.5;             // upper limit on N3 near close-packing value for spheres.
	}
	if ( Rho <= 100.0 ) {
		return 1.75*(Rho/100.);  // Decreases N3 to zero as density goes to zero.
	}

	static const double N_0 = 1.4153;
	static const double N_1 = 7.5580e-5;
	static const double N_2 = 5.1495e-5;
	static const double N_3 = 1.7345e-7;
	static const double N_4 = 1.8082e-10;
	const double R_2 = Rho*Rho;
	const double R_3 = R_2*Rho;
	const double R_4 = R_2*R_2;

	// For Rho between 100 kg/m3 and 670 kg/m3, use the following.
	const double N3 = N_0 - N_1*Rho + N_2*R_2 - N_3*R_3 + N_4*R_4;
	return(N3);
}

/**
 * @brief Compute the rate of change of dendricity according to the Crocus model. \n
 * @version 10.11
 * @param Edata const ElementData
 * @return Rate of change (d-1)
 */
double Metamorphism::ddRate(const ElementData& Edata)
{
	const double dTdz = fabs(Edata.gradT);
	const double c = exp (-6000. / Edata.Te); // original CROCUS: -6000.; set to -5800. by Bellaire 2004; back to ori 2007
	const double f = c * sqrt(dTdz); // factor 1./7., version 98.03; original CROCUS: c*pow(dTdZ, 0.4)

	// dTdz < 5.0: Ml: ori -2.0; set to -5.0e8 by Bellaire 2004, then to -2.5e8 2007;
	//dTdz >= 5.0:Ml: ori -4.0; set to -1.5e8 by Bellaire 2004, then to -3.5e8 2007;
	const double ddDot = (dTdz < 5.0)? -3.0e8 * c :  -1.5e8 * f ;

	return std::max(-1., ddDot);
}

/************************************************************
 * non-static section                                       *
 ************************************************************/

static std::string get_model(const SnowpackConfig& cfg) 
{
	std::string model;
	cfg.getValue("METAMORPHISM_MODEL", "SnowpackAdvanced", model);
	return model;
}

static double get_sn_dt(const SnowpackConfig& cfg) 
{
	//Calculation time step in seconds as derived from CALCULATION_STEP_LENGTH
	const double calculation_step_length = cfg.get("CALCULATION_STEP_LENGTH", "Snowpack");
	return M_TO_S(calculation_step_length);
}

static double get_nsgs(const SnowpackConfig& cfg) 
{
	const double nsgs = cfg.get("NEW_SNOW_GRAIN_SIZE", "SnowpackAdvanced");
	return nsgs;
}

Metamorphism::Metamorphism(const SnowpackConfig& cfg)
              : metamorphism_model( get_model(cfg) ), sn_dt( get_sn_dt(cfg) ), new_snow_grain_size( get_nsgs(cfg) )
{
	const map<string, MetaModelFn>::const_iterator it1 = mapMetamorphismModel.find(metamorphism_model);
	if (it1 == mapMetamorphismModel.end())
		throw InvalidArgumentException("Unknown metamorphism model: "+metamorphism_model, AT);

	const map<string, MetaSpRateFn>::const_iterator it2 = mapSpRate.find(metamorphism_model);
	if (it2 == mapSpRate.end())
		throw InvalidArgumentException("Unknown metamorphism model: "+metamorphism_model, AT);
}

/**
 * @brief Rate of change for sphericity sp
 * Presently, it is based on the french parameterization (CROCUS)
 * @param Edata
 * @return Rate of change (d-1)
 */
double Metamorphism::spRateDEFAULT(const ElementData& Edata) const
{
	const double dTdZ = fabs(Edata.gradT);
	const double c = exp(-6000. / Edata.Te); // Original 6000.
	const double f = c * dTdZ / 17.; // Introduced by ml on 2004-10-11; original CROCUS: c*pow(dTdZ, 0.4)

	double spDot;
	if ( dTdZ < 5.0 ) {
		if ( Edata.sp < 0.49 ) {
			spDot = (5.0 - dTdZ)*0.2e8*c*Edata.sp;
		} else {
			spDot = (5.0 - dTdZ)*0.5e8*c;
		}
	} else {
		spDot = -((dTdZ - 5.)/dTdZ)*0.5e8*f; // ML: Ori 2.0
	}
	// Consider graupel separately
	if ( (Edata.mk % 100) == 4 ) {
		spDot = 0.;
	}

	return(spDot);
}

/**
 * @brief Rate of change for sphericity sp
 * Based on the french parameterization (CROCUS) but adapted to Japanese conditions
 * @param *Edata
 * @return Rate of change (d-1)
 */
double Metamorphism::spRateNIED(const ElementData& Edata) const
{
	const double dTdZ = fabs(Edata.gradT);
	const double c = exp(-6000. / Edata.Te); // Original 6000.
	const double f = c * dTdZ / 17.; // Introduced by ml on 2004-10-11; original CROCUS: c*pow(dTdZ, 0.4)

	double spDot;
	if ( dTdZ < 15.0 ) { //NIED (H. Hirashima)
		spDot = (15.0 - dTdZ)*0.5e9*c;
	} else if ( (dTdZ > 15.0) && (dTdZ < 25.0) ) {
		spDot = -1.e8*f;
	} else {
		if ( Edata.sp > 0. ) {
			spDot =  -1.e8*f;
		} else {
			spDot = 0.0;
		}
	}
	// Now consider Graupel
	if ( (Edata.mk % 100) == 4 ) {
		spDot = 0.;
	}

	return(spDot);
}

/**
 * @brief This is Satyawali's TG bond growth rate routine
 * @param *Edata
 * @return Bond radius growth rate (mm d-1)
 */
double Metamorphism::TGBondRate(const ElementData& Edata)
{
	const double rb = MM_TO_M(Edata.rb);    // initial bond radius (m)
	const double rg = MM_TO_M(Edata.rg);    // initial grain radius (m)
	const double TGrad = fabs(Edata.gradT); // absolute value of temp gradient within element (K m-1)

	const double A = 1./3. * (Constants::pi*(rb*rb + rg*rg) + csPoreArea(Edata)); // average cross sectional area (m2)
	// micro temp gradient across bonds (K m-1)
	const double TGradBond = Edata.k[TEMPERATURE] / Constants::conductivity_ice * A / (Constants::pi * rb*rb) * (-TGrad);       // (K m-1) NOTE Why take TGrad neg.?
	double flux = -Constants::diffusion_coefficient_in_air / (Constants::gas_constant * Edata.Te*Edata.Te) * (Constants::lh_sublimation / (Constants::gas_constant * Edata.Te) - 1.) * TGradBond; // mass flux of vapor in the pore space - in cgs units
	flux *= Atmosphere::vaporSaturationPressure(Edata.Te); // (kg s-1 m-2)
	// Bond radius growth rate (m s-1)
	const double rbDot = flux / Constants::density_ice * Metamorphism::sa_g_fudge; // Bond radius growth rate (mm d-1)
	// Convert to mm d-1
	return(M_TO_MM(D_TO_S(rbDot)));
}

/**
 * @brief This function computes the lattice constant (mm)
 * Note there is one source and one sink grain (1+1)
 * @param th_ice Volumetric ice content (1)
 * @return Lattice constant (mm)
*/
double Metamorphism::LatticeConstant0(const double& th_ice) const
{
	const double gsz0 = new_snow_grain_size;

	return( pow((1.+1.)*Metamorphism::ba_g_fudge*Optim::pow3(gsz0)/th_ice, 1./3.) );
}

/*
 * Kinetic grain growth routine
*/
/**
 * @brief Actual routine: mm_TGGrainRateImplicit from r7.7
 * Fierz' implicit version of Baunach's model
 * (see SnpR1.fz/Originaux/Metamorphism.fzV5.c) :
 * Formulation of graingrowth including intra- and layer-to-layer transport
 * --> see paper in Ann. Glaciol., 32 (IGS Innsbruck 2000).
 * lattice constant given as a function of both gsz-gsz0 and density (th_i),
 * assuming the overall snow density and thus theta_ice are not much
 * affected by L2L, which is quite true! (Ml2l ~ 1mg/element/time step.)
 * Note : gsz0 = new_snow_grain_size
 * @param *Edata
 * @param Tbot Lower node temperature (K)
 * @param Ttop Upper node temperature (K)
 * @param gradTSub Temperature gradient at lower node (K m-1)
 * @param gradTSup Temperature gradient at upper node (K m-1)
 * @return Grain radius growth rate (mm d-1)
 */
double Metamorphism::TGGrainRate(const ElementData& Edata, const double& Tbot, const double& Ttop,
                                 const double& gradTSub, const double& gradTSup) const
{
	// Collect the continuum values from the element data structures
	const double th_i = Edata.theta[ICE]; // Ice content
	const double Te = Edata.Te; // Temperature (K)
	const double gradT = Edata.gradT; // Temperature gradients (K m-1)
	const double gradTbot = .5 * (gradTSub + Edata.gradT); // Temperature gradients (K m-1)
	const double gradTtop = .5 * (Edata.gradT + gradTSup); // Temperature gradients (K m-1)
	const double gsz = 2. * (Edata.rg); // grain size (mm)
	const double hElem = M_TO_MM(Edata.L); // Element height (mm)

	// Compute the lattice constant a at time t but a <= hElem;  Units : mm
	const double a0 = LatticeConstant0( th_i );
	double a = a0;
	if ( gsz > new_snow_grain_size ) {
		// Use an empirical estimation of the lattice constant
		static const double reg0 = 0.15, reg1 = -0.00048; // Empirical regression coefficients
		const double a1 = reg0 + reg1*(th_i * Constants::density_ice);
		a  = a0 + a1*(gsz - new_snow_grain_size);
	}
	a  = std::min(a, hElem);

	// Intra layer flux, where the direction of flow does not matter! Units: kg/(sm2)
	double intraFlux =  fabs(Constants::diffusion_coefficient_in_snow / (Constants::gas_constant * Te*Te) * (Constants::lh_sublimation / (Constants::gas_constant * Te) - 1.) * gradT);
	intraFlux *= Atmosphere::vaporSaturationPressure(Te);

	// Layer to layer flux, where the direction of flow DOES matter! Units: kg/(sm2)
	double botFlux = - Constants::diffusion_coefficient_in_snow / (Constants::gas_constant * Tbot*Tbot) * (Constants::lh_sublimation / (Constants::gas_constant * Tbot) - 1.) * gradTbot;
	botFlux *= Atmosphere::vaporSaturationPressure(Tbot);
	double topFlux = - Constants::diffusion_coefficient_in_snow / (Constants::gas_constant * Ttop*Ttop) * (Constants::lh_sublimation / (Constants::gas_constant * Ttop) - 1.) * gradTtop;
	topFlux *= Atmosphere::vaporSaturationPressure(Ttop);
	const double dFluxL2L = -(topFlux - botFlux); // Flux divergence due to L2L transport
	// Compute the rate in m s-1
	const double rgDot = 0.5 * ( (intraFlux + dFluxL2L * (a / hElem) ) * a*a) / (2.0 * Metamorphism::ba_g_fudge * Constants::density_ice * (new_snow_grain_size) * gsz);

	// Conversion to mm d-1
	return M_TO_MM(D_TO_S(rgDot));
}


/**
 * @return Below is Borolo Bob's ET bond growth rate routine.  Determines the bond or neck
 * growth for low temperature gradients. Called from the routine
 * mm_Metamorphism. Note that the growth rate is converted from mm s-1 to
 * mm d-1 before returning rbDot.
 * @param *Edata
 * @return Bond radius growth rate (mm d-1)
 */
double Metamorphism::ETBondRate(ElementData& Edata)
{
	/*
	* B_1...B_3 are  constants computed with Brown's advanced and sophisticated
	* mixture theory. Bartelt is so jealous of that fine piece of work.   Please note
	* hist sarcastic tirade later in this  unreadable program.
	*/
	static const double B_1 = 0.1436e-3;         //  in mm/sec
	static const double B_2 = -1.8850e-6;        //  in mm
	static const double B_3 = 4.6690e+3;         //  deg K
	static const double B_R = 273.;
	const double rc = Edata.concaveNeckRadius();
	double rbDot; // Bond radius growth rate (mm s-1)

	if ( fabs(rc - Edata.rb) < Constants::eps ) {	//special case: thermodynamic neck radius rn is infinite
		rbDot  = B_1 * (1. - exp(B_2/Edata.rg) ) * exp( (B_3/B_R) - (B_3/Edata.Te)  );
	} else {
		const double rn = ( 2.*Edata.rb*rc) / (rc - Edata.rb);
		rbDot  = B_1 * (exp(B_2/ rn ) - exp(B_2/Edata.rg) ) * exp( (B_3/B_R) - (B_3/Edata.Te)  );
	}

	// Convert to mm d-1
	return D_TO_S(rbDot);
}


/**
 * @brief Below is Borolo Bob's ET grain growth rate routine : Determines the grain growth
 * for low temperature gradients.  Called from the routine mm_Metamorphism.
 * Note that the growth ate is converted from mm/sec to mm/day before returning rgDot
 * @param *Edata
 * @return Grain radius growth rate (mm s-1)
 */
double Metamorphism::ETGrainRate(const ElementData& Edata)
{
	// These are the routine's FUDGE FACTORs
	static const double C_1 = 9.403e-11;
	static const double C_2 = 5.860e-9;
	static const double C_3 = 2.900e3;
	static const double C_R = 273.;

	// Grain radius growth rate (mm s-1)
	const double rgDot = ((C_1 / Edata.rg) + C_2) * exp((C_3 / C_R) - (C_3 / Edata.Te));

	// Convert to mm d-1
	return D_TO_S(rgDot);
}


/**
 * @brief Bond radius growth due to pressure sintering \n
 * Relate bond growth to total deformation computed with actual (partly empirical) viscosity;
 * Previously this was done with microstructure viscosity, which is only used for main part of
 * settling, however.
 * @version 04.10
 * @param Edata
 * @return Additional bond radius growth rate (mm d-1)
 */
double Metamorphism::PressureSintering(ElementData& Edata)
{
	if (Edata.theta[ICE] < Snowpack::min_ice_content) {
		return 0.;
	}
	if (Edata.Te > Edata.meltfreeze_tk) {
		return 0.;
	}

	// Bond radius growth rate (mm s-1)
	const double rbdot = -0.1 * Edata.rb * Edata.Eps_vDot / Edata.neck2VolumetricStrain();
	// Convert from (mm s-1) to (mm d-1)
	return D_TO_S(rbdot);
}


/**
 * @brief Main routine for Metamorphism model
 * Actual routine: bb_mm_Metamorphism from r7.7
 * @param Mdata
 * @param Xdata
 */
void Metamorphism::metamorphismDEFAULT(const CurrentMeteo& Mdata, SnowStation& Xdata) const
{
	double rgDot;        // Grain growth rate (mm d-1)
	double rbDot;        // Bond growth rate (mm d-1)
	double rgDotMax, rbDotMax;  // Maximum grain and bond growth rates
	double ddDot;        // Rate of dendricity change (d-1)
	double spDot;        // Rate of sphericity change (d-1)
	static const double a1 = 1.11e-3, a2 = 3.65e-5;  // mm3 day-1 Volumetric growth coefficients for wet snow
	static const double cw = 1.e8 * exp(-6000. / 273.15);
	const size_t nE = Xdata.getNumberOfElements();

	// Dereference the element pointer containing micro-structure data
	ElementData *EMS = &Xdata.Edata[0];
	const vector<NodeData>& NDS = Xdata.Ndata;

	for (size_t e = Xdata.SoilNode; e < nE; e++) {
		// Set all rates of change to zero for element e
		ddDot = spDot = rbDot = rgDot = 0.0;

		if ( EMS[e].theta[ICE] < 0.00001 || EMS[e].theta[SOIL] > 0.00001 ) {
			continue;
		}

		// Determine the coordination number which is purely a function of the density
		EMS[e].N3 = getCoordinationNumberN3(EMS[e].Rho);

		// Compute local value of mass percentage of liquid water (Fig. 6 in Brun, 1989, https://doi.org/10.3189/S0260305500007576, shows the cut-off at 10%)
		const double thetam_w = std::min(10., 1.e2 * (Constants::density_water * EMS[e].theta[WATER] / EMS[e].Rho));

		 // Constants used to limit changes in sphericity after faceting
		double splim1 = 20. * (new_snow_grain_size/2. - EMS[e].rg);
		if ( splim1 > 0.0 ) {
			splim1=0.0;
		}
		double splim2 = EMS[e].sp / 0.5;
		if ( splim2 > 1.0 ) {
			splim2 = 1.0;
		}
		static const double splim3 = -0.7;
		const size_t marker = EMS[e].mk%100;  // untag EMS[e].mk

		// Compute the pressure gradient (kinetic or equilibrium growth metamorphism??)
		const double T1 = NDS[e].T; // Nodal temperature of element
		const double T2 = NDS[e+1].T;// Nodal temperature of element
		const double P1 = Atmosphere::vaporSaturationPressure(T1); //Nodal pressure of element
		const double P2 = Atmosphere::vaporSaturationPressure(T2); //Nodal pressure of element
		const double dPdZ = fabs((P2 - P1) / EMS[e].L) * 0.01;  //Vapor pressure gradient within element in hPa m-1

		// Equilibrium growth rates for old dry snow
		rgDot = ETGrainRate(EMS[e]);
		rbDot = ETBondRate(EMS[e]);

		// Kinetic growth rates
		// Since we need temperature gradients above and below the element we have to consider various cases for the kinetic grain growth
		if ( e > 0 && e < nE-1 ) { // inner element
			rgDotMax = TGGrainRate(EMS[e], T1, T2, EMS[e-1].gradT, EMS[e+1].gradT);
		} else if ( e == 0 ) { // bottom element
			if ( nE == 1 ) {
				// bottom element: use EMS[e].gradT twice in case nE=1
				rgDotMax = TGGrainRate(EMS[e], T1, T2, EMS[e].gradT, EMS[e].gradT);
			} else {
				// bottom element in other cases
				rgDotMax = TGGrainRate(EMS[e], T1, T2, EMS[e].gradT, EMS[e+1].gradT);
			}
		} else {// top element
			rgDotMax = TGGrainRate(EMS[e], T1, T2, EMS[e-1].gradT, EMS[e].gradT);
		}
		rgDotMax = std::max(0.0, rgDotMax);
		rbDotMax = TGBondRate(EMS[e]);

		if ( (EMS[e].theta[WATER] < 0.01) && (Mdata.vw > Metamorphism::wind_slab_vw) && ((NDS[nE].z - NDS[e].z < Metamorphism::wind_slab_depth) || e == nE-1) ) {
			//if snow is dry AND wind strong AND we are near the surface => wind densification of snow
			// Introduce heuristic metamorphism for wind slabs of Metamorphism::wind_slab_depth (m)
			double wind_slab = 1.;

			// Enhancement factor; see also sn_SnowCreep()
			wind_slab += Metamorphism::wind_slab_enhance * (Mdata.vw - Metamorphism::wind_slab_vw);

			ddDot = wind_slab * ddRate(EMS[e]);
			spDot = wind_slab * (CALL_MEMBER_FN(*this, mapSpRate[metamorphism_model])(EMS[e]));
			rgDot = 0.;
			rbDot = 0.5 * rgDotMax; //HACK Why should it be half the kinetic rate?
		} else {
			//normal processes for snow
			// NEW SNOW
			if ( EMS[e].dd > 0.0 ) {
				// WET new snow
				if ( EMS[e].theta[WATER] > 0.01 ) { //NIED if(EMS[e].theta[WATER] > 0.1) CORRECTED SINCE version 7.4
					ddDot = -Optim::pow3(thetam_w) / 16.;
					if ( (-ddDot) < cw ) {
						ddDot = -cw;
					}
					spDot = -0.5 * ddDot;
					rgDot = rbDot = 0.0; // no grain growth until dd <= 0.0
				} else {
					// DRY new snow
					ddDot = ddRate(EMS[e]);
					spDot = CALL_MEMBER_FN(*this, mapSpRate[metamorphism_model])(EMS[e]);
					if ( (EMS[e].dd < 0.8) && (dPdZ > 2.*Metamorphism::mm_tg_dpdz) ) {
						// TG metamorphism (new snow); mimicks the Japanese change according to Fierz
						rgDot += rgDotMax;
					} else {
						// no grain growth until dd <= 0.0 if gradient too small
						rgDot += 0.; // (dPdZ/2./Metamorphism::mm_tg_dpdz*rgDotMax);
					}
				}
			} else { // (OLD) SNOW
				// WET snow
				if (EMS[e].theta[WATER] > SnowStation::thresh_moist_snow) {
					ddDot = 0.0;
					spDot = Optim::pow3(thetam_w) / 16.;
					if ( spDot < 2.*cw ) {
						spDot = 2.*cw;
					}
					// Faceted grains, dry and wet, need first to be rounded (sp > 0.5) before they grow due to the presence of liquid water.
					if ( (marker%10 == 2) || EMS[e].sp > 0.5 ) {
						rgDot = 1. / (4. * Constants::pi * Optim::pow2(EMS[e].rg)) * (a1 + a2 * Optim::pow3(thetam_w));
						rbDot = 0.6 * rgDot;
					} else {
						rgDot = rbDot = 0.;
					}
				} else {
					// DRY snow
					ddDot = 0.0;
					spDot = CALL_MEMBER_FN(*this, mapSpRate[metamorphism_model])(EMS[e]);
					if ( fabs(EMS[e].gradT) < 5.0 ) {
						if ( (marker == 1 || marker == 3) && EMS[e].rg > 0.3 ) {
							spDot = spDot * exp(splim1);
						} else if ( (marker == 21 || marker == 23) && EMS[e].sp < 0.5 ) {
							spDot = spDot*(splim2);
						}
					} else if ( marker == 22 ) {
						spDot = spDot * exp(splim3);
					}
					if ( dPdZ > Metamorphism::mm_tg_dpdz ) { // HACK Why did we sum up both growth rates?
						rbDot = rbDotMax;
						rgDot = rgDotMax;
					} else { // HACK Why are grains growing more than predicted by Borolo Bob?
						rbDot += ( (dPdZ / Metamorphism::mm_tg_dpdz + 0.1) * rbDotMax );
						rgDot += ( (dPdZ / Metamorphism::mm_tg_dpdz + 0.1) * rgDotMax );
					}
				}
			}
		}

		// Pressure sintering: update bond growth rate, both wet and dry snow
		EMS[e].ps2rb = PressureSintering(EMS[e]);
		rbDot += EMS[e].ps2rb;

		// UPDATE THE MICROSTRUCTURE PARAMETERS
		// Time increment in days, specifically written to avoid confusion
		const double dDay = S_TO_D(sn_dt);
		// Update dendricity
		EMS[e].dd += ddDot * dDay;
		EMS[e].dd = std::max(0.0, std::min(1.0, EMS[e].dd));
		// Update sphericity
		EMS[e].sp += spDot * dDay;
		if ( (marker == 1) && (EMS[e].rg >= 0.4) ) {
			EMS[e].sp = std::max(0.0, std::min(0.5, EMS[e].sp)); // Limit effect of rounding on dry faceted grains
		} else {
			EMS[e].sp = std::max(0.0, std::min(1.0, EMS[e].sp));
		}
		// Update grain sizes ...
		rgDot = std::min(rgDot, Metamorphism::max_grain_growth);
		if ( marker != 3 ) {
			EMS[e].rg += rgDot*dDay;
		} else {
			//HACK ... but do not allow surface hoar to grow and limit its size to layer thickness.
			EMS[e].rg = std::min(EMS[e].rg, 0.5 * M_TO_MM(EMS[e].L));
		}
		EMS[e].opticalEquivalentGrainSize();
		// Update bond size and limit its growth to Metamorphism::bond_size_stop * EMS[e].rg
		rbDotMax = (Metamorphism::bond_size_stop * EMS[e].rg - EMS[e].rb) / dDay;
		rbDot = std::max(0., std::min(rbDot, rbDotMax));
		EMS[e].rb += rbDot * dDay;
		if ( marker == 3 ) { //HACK SH is only grain allowed to decrease its grain size!
			EMS[e].rb = std::min(EMS[e].rb, Metamorphism::max_grain_bond_ratio * EMS[e].rg);
		}

		// Compute proportion of grain bond growth due to pressure sintering
		if ( (EMS[e].dd < 0.005) && (rbDot > 0.) ) {
			EMS[e].ps2rb /= rbDot;
		} else {
			EMS[e].ps2rb = 0.0;
		}
		// Update the microstructure marker
		if (EMS[e].dd < 0.001) { //NIED EMS[e].dd < 0.001
			if ((EMS[e].sp < 0.1) && (marker % 10 == 0)) {
				EMS[e].mk += 1;  // grains become fully faceted
			}
			if ((EMS[e].sp > 0.999) && (marker % 10 == 0)) {
				EMS[e].mk += 2;  // grains become fully rounded
			}
			// An ice layer forms in the snowpack for dry densities above 700 kg m-3!
			if ((EMS[e].theta[ICE] > 0.763) && ((marker % 10 != 7) || (marker % 10 != 8))) {
				EMS[e].mk = (EMS[e].mk / 10) * 10 + 8;
			}
		}

		// Compute residual water content
		EMS[e].snowResidualWaterContent();
		// Check for first wetting
		if ((EMS[e].theta[WATER] > 0.015) && (marker < 10)) {
			// Non-dendritic snow: thrsh ori 0.3 changed by S.Bellaire to get thinner crusts (13.03.2006)
			// Dendritic snow: very rapid change to melt forms
			if ((EMS[e].theta[WATER] > 0.35 * EMS[e].res_wat_cont) || (marker < 1)) {
				EMS[e].mk += 10;
			}
		}
		// Check for first complete melt-freeze cycle
		else if ((marker < 20) && (marker >= 10) && (EMS[e].Te < EMS[e].meltfreeze_tk - 0.3)) {
			EMS[e].mk += 10;
		}

		EMS[e].snowType();
	}
}

/**
 * @brief Main routine for Metamorphism model adapted according to NIED (H. Hirashima)
 *        See: Hirashima H, Abe O, Sato A and Lehning M (2009) An adjustment for kinetic growth metamorphism
 *             to improve shear strength parameterization in the SNOWPACK model. Cold Reg. Sci. Technol.,
 *             59 (2-3), 169-177 (doi: 10.1016/j.coldregions.2009.05.001).
 * @param Mdata
 * @param Xdata
 */
void Metamorphism::metamorphismNIED(const CurrentMeteo& Mdata, SnowStation& Xdata) const
{
	double rgDot;        // Grain growth rate (mm d-1)
	double rbDot;        // Bond growth rate (mm d-1)
	double rgDotMax, rbDotMax;  // Maximum grain and bond growth rates
	double ddDot;        // Rate of dendricity change (d-1)
	double spDot;        // Rate of sphericity change (d-1)
	static const double a1 = 1.11e-3, a2 = 3.65e-5;  // mm3 day-1 Volumetric growth coefficients for wet snow
	static const double cw = 1.e8 * exp(-6000. / 273.15);
	double dsmDot = Constants::undefined;       //NIED (H. Hirashima) Dry snow metamorphism factor...
	const size_t nE = Xdata.getNumberOfElements();

	// Dereference the element pointer containing micro-structure data
	ElementData *EMS = &Xdata.Edata[0];
	const vector<NodeData>& NDS = Xdata.Ndata;

	for (size_t e = Xdata.SoilNode; e < nE; e++) {
		// Set all rates of change to zero for element e
		ddDot = spDot = rbDot = rgDot = 0.0;

		if ( EMS[e].theta[ICE] < 0.00001 || EMS[e].theta[SOIL] > 0.00001 ) {
			continue;
		}

		// Determine the coordination number which is purely a function of the density
		EMS[e].N3 = getCoordinationNumberN3(EMS[e].Rho);

		// Compute local value of mass percentage of liquid water (Fig. 6 in Brun, 1989, https://doi.org/10.3189/S0260305500007576, shows the cut-off at 10%)
		const double thetam_w = std::min(10., 1.e2 * (Constants::density_water * EMS[e].theta[WATER] / EMS[e].Rho));

		// Constants used to limit changes in sphericity after faceting
		double splim1 = 20. * (new_snow_grain_size/2. - EMS[e].rg);
		if ( splim1 > 0.0 ) {
			splim1=0.0;
		}
		double splim2 = EMS[e].sp / 0.5;
		if ( splim2 > 1.0 ) {
			splim2 = 1.0;
		}
		const size_t marker = EMS[e].mk % 100;  // untag EMS[e].mk

		// Compute the pressure gradient (kinetic or equilibrium growth metamorphism??)
		const double T1 = NDS[e].T;
		const double T2 = NDS[e+1].T;
		const double P1 = Atmosphere::vaporSaturationPressure(T1);
		const double P2 = Atmosphere::vaporSaturationPressure(T2);
		const double dPdZ = fabs((P2 - P1) / EMS[e].L) * 0.01;  //  Result is in mbar m-1

		// Equilibrium growth rates for old dry snow
		rgDot = ETGrainRate(EMS[e]);
		rbDot = ETBondRate(EMS[e]);

		// Kinetic growth rates
		// Since we need temperature gradients above and below the element we have to consider various cases for the kinetic grain growth
		if ( e > 0 && e < nE-1 ) { // inner element
			rgDotMax = TGGrainRate(EMS[e], T1, T2, EMS[e-1].gradT, EMS[e+1].gradT);
		} else if ( e == 0 ) { // bottom element
			if ( nE == 1 ) {
				// bottom element: use EMS[e].gradT twice in case nE=1
				rgDotMax = TGGrainRate(EMS[e], T1, T2, EMS[e].gradT, EMS[e].gradT);
			} else {
				// bottom element in other cases
				rgDotMax = TGGrainRate(EMS[e], T1, T2, EMS[e].gradT, EMS[e+1].gradT);
			}
		} else {// top element
			rgDotMax = TGGrainRate(EMS[e], T1, T2, EMS[e-1].gradT, EMS[e].gradT);
		}
		rgDotMax = std::max(0.0, rgDotMax);
		rbDotMax = TGBondRate(EMS[e]);

		if ( (EMS[e].theta[WATER] < 0.01) && (Mdata.vw > Metamorphism::wind_slab_vw) && ((NDS[nE].z - NDS[e].z < Metamorphism::wind_slab_depth) || e == nE-1) ) {
			//if snow is dry AND wind strong AND we are near the surface => wind densification of snow
			// Introduce heuristic metamorphism for wind slabs of Metamorphism::wind_slab_depth (m)
			double wind_slab = 1.;

			// Enhancement factor; see also sn_SnowCreep()
			wind_slab += Metamorphism::wind_slab_enhance * (Mdata.vw - Metamorphism::wind_slab_vw);

			ddDot = wind_slab * ddRate(EMS[e]);
			spDot = wind_slab * (CALL_MEMBER_FN(*this, mapSpRate[metamorphism_model])(EMS[e]));
			rgDot = 0.;
			rbDot = 0.5 * rgDotMax;
			//dsmdot = 0.; //Fz HACK You'd need to define dsmdot in this case also
		} else {
			//normal processes for snow
			// NEW SNOW
			if ( EMS[e].dd > 0.0 ) {
				// WET new snow
				if ( EMS[e].theta[WATER] > 0.01 ) { //NIED if(EMS[e].theta[WATER] > 0.1) CORRECTED SINCE version 7.4
					ddDot = -Optim::pow3(thetam_w) / 16.;
					if ( (-ddDot) < cw ) {
						ddDot = -cw;
					}
					spDot = -0.5 * ddDot;

					rgDot = rbDot = 0.0; // no grain growth until dd <= 0.0
					dsmDot = -Optim::pow3(thetam_w)/16./86400.; //NIED (H. Hirashima)
				} else {
					// DRY new snow //NIED (H. Hirashima)
					ddDot = ddRate(EMS[e]);
					spDot = -(fabs(EMS[e].gradT) - 15.)/17.; //NIED (H. Hirashima)
					rgDot = rbDot = 0.0; // no grain growth until dd <= 0.05

					if (fabs(EMS[e].gradT)>150.) { //NIED (H. Hirashima)
						ddDot=-0.5;
						spDot=-0.25;
					}
					const double gradV=dPdZ*7.93E-4;  //NIED (H. Hirashima) hPa/m��kg/m2�ɕϊ�
					const double DenFact = -0.136*EMS[e].Rho+4.56;
					const double Diffus = std::max((2.23E-5*(1013.25/1013.25)*pow((EMS[e].Te)/273.15,1.78)),((0.78*(EMS[e].Te-273.15))+10.84)*1.0E-5); //NIED (H. Hirashima)
					dsmDot = fabs(-DenFact*Diffus*gradV*(1.0-EMS[e].dsm));
					if (fabs(EMS[e].gradT)<5.0) {
						if (mio::IOUtils::K_TO_C(EMS[e].Te) <= -5) {
							dsmDot= -(2.44E-9*mio::IOUtils::K_TO_C(EMS[e].Te)+6.58E-8); //NIED (H. Hirashima)
						} else {
							dsmDot= -(-8.96E-9*mio::IOUtils::K_TO_C(EMS[e].Te)+8.46E-9); //NIED (H. Hirashima)
						}
					}
				}
			} else { // (OLD) SNOW
				// WET snow
				if (EMS[e].theta[WATER] > SnowStation::thresh_moist_snow) {
					ddDot = 0.0;
					spDot = Optim::pow3(thetam_w) / 16.;
					if ( spDot < 2.*cw ) {
						spDot = 2.*cw;
						if (spDot<-(fabs(EMS[e].gradT) - 15.)/17.) { //NIED (H. Hirashima)
							spDot = -(fabs(EMS[e].gradT) - 15.)/17.;
						}
					}
					// Faceted grains, dry and wet, need first to be rounded (sp > 0.5) before they grow due to the presence of liquid water.
					if ( (marker%10 == 2) || EMS[e].sp > 0.5 ) {
						rgDot = 1. / (4. * Constants::pi * Optim::pow2(EMS[e].rg)) * (a1 + a2 * Optim::pow3(thetam_w));
						rbDot = 0.6 * rgDot;
						dsmDot = -(Optim::pow3(thetam_w)/16./86400.);
						if ( dsmDot>-2.*cw/86400. ) {  //NIED (H. Hirashima)
							dsmDot=-2.*cw/86400.;
						}
					} else {
						rgDot = rbDot = 0.;
					}
				} else {
					// DRY snow  //NIED (H. Hirashima)
					ddDot = 0.0;
					spDot = CALL_MEMBER_FN(*this, mapSpRate[metamorphism_model])(EMS[e]);
					const double gradV=dPdZ*7.93E-4; //NIED (H. Hirashima) //hPa/m��kg/m2�ɕϊ�
					const double DenFact = -0.136*EMS[e].Rho+4.56;  //NIED (H. Hirashima)
					const double Diffus = std::max((2.23E-5*(1013.25/1013.25)*pow((EMS[e].Te)/273.15,1.78)),((0.78*(EMS[e].Te-273.15))+10.84)*1.0E-5); //NIED (H. Hirashima)
					dsmDot = fabs(-DenFact*Diffus*gradV*(1.0-EMS[e].dsm));
					if ( fabs(EMS[e].gradT)<5.0 ) {
						dsmDot=-500000000.0*exp(-6000.0/EMS[e].Te)*(5.-fabs(EMS[e].gradT))/86400.; //NIED (H. Hirashima)
					}
					if ( dPdZ > Metamorphism::mm_tg_dpdz ) {
						rbDot = TGBondRate( EMS[e] );
						// Since we need temperature gradients above and below the element we have to be careful for the grain growth
						if (e > 0 && e < nE-1) { // inner element
							rgDot = TGGrainRate(EMS[e], T1, T2, EMS[e-1].gradT, EMS[e+1].gradT);
						} else if ( e == 0 ) { // bottom element
							if ( nE == 1 ) {
								// bottom element: use EMS[e].gradT twice in case nE=1
								rgDot = TGGrainRate(EMS[e], T1, T2, EMS[e].gradT, EMS[e].gradT);
							} else {
								// bottom element in other cases
								rgDot = TGGrainRate(EMS[e], T1, T2, EMS[e].gradT, EMS[e+1].gradT);
							}
						} else {
							rgDot = TGGrainRate(EMS[e], T1, T2, EMS[e-1].gradT, EMS[e].gradT);
							// rgDot = mm_TGGrainRate( &EMS[e] );  Thorstens Formulation
						}
						if ( rgDot < 0.0 ) {
							rgDot = 0.0;
						}
						if ( fabs(EMS[e].gradT ) > 150. ) { //NIED (H. Hirashima)
							ddDot=-0.5;
							spDot=-0.25;
						}
						rgDot=(6.25E-12 * EMS[e].gradT + 6.48E-10)*86400.*1000.; //NIED (H. Hirashima)
					}
				}
			}
		}

		// Pressure sintering: update bond growth rate, both wet and dry  snow
		EMS[e].ps2rb = PressureSintering(EMS[e]);
		rbDot += EMS[e].ps2rb;

		// Time increment in days, specifically written to avoid confusion (see line 1012
		const double dDay = S_TO_D(sn_dt);

		// UPDATE THE MICROSTRUCTURE PARAMETERS
		if(EMS[e].theta[WATER] > 0.01 ) { //NIED (H. Hirashima)
			dsmDot = -(Optim::pow3(thetam_w)/16./86400.);
			if(dsmDot>-2.*cw/86400.) {
				dsmDot=-2.*cw/86400.;
			}
			if (EMS[e].dd == 0.) {
				dsmDot=dsmDot/2.; // HACK //Fz Hazardous comparison!
			}
		}
		EMS[e].dsm += dsmDot * sn_dt; //NIED (H. Hirashima) HACK //Fz use consistent units dDay instead of sn_dt
		EMS[e].dsm = std::max(0.0, std::min(1.0, EMS[e].dsm)); //NIED (H. Hirashima)
		// Update dendricity
		EMS[e].dd += ddDot * dDay;
		EMS[e].dd = std::max(0.0, std::min(1.0, EMS[e].dd));
		// Update sphericity
		EMS[e].sp += spDot * dDay;
		if ( (marker == 1) && (EMS[e].rg >= 2.) ) { //NIED (H. Hirashima)
			EMS[e].sp = std::max(0.0, std::min(0.5, EMS[e].sp)); // Limit effect of rounding on dry faceted grains
		} else {
			EMS[e].sp = std::max(0.0, std::min(1.0, EMS[e].sp));
		}
		// Update grain sizes ...
		//rgDotMax = Metamorphism::max_grain_growth;
		rgDot = std::min(rgDot, Metamorphism::max_grain_growth);
		if ( marker != 3 ) {
			EMS[e].rg += rgDot*dDay;
		} else {
			// ... but do not allow surface hoar to grow and limit its size to layer thickness.
			EMS[e].rg = std::min(EMS[e].rg, 0.5 * M_TO_MM(EMS[e].L));
		}
		EMS[e].opticalEquivalentGrainSize();
		// Update bond size
		rbDotMax = (Metamorphism::bond_size_stop * EMS[e].rg - EMS[e].rb) / dDay;
		rbDot = std::max(0., std::min(rbDot, rbDotMax));
		EMS[e].rb += rbDot * dDay;
		// Compute proportion of grain bond growth due to pressure sintering
		if ( (EMS[e].dd < 0.005) && (rbDot > 0.) ) {
			EMS[e].ps2rb /= rbDot;
		} else {
			EMS[e].ps2rb = 0.0;
		}
		// Update the microstructure marker
		if ( EMS[e].dd < 0.001 ) { //NIED EMS[e].dd < 0.001
			if ( (EMS[e].sp < 0.1) && (marker % 10 == 0) ) {
				EMS[e].mk += 1;  // grains become fully faceted
			}
			if ( (EMS[e].sp > 0.999) && (marker % 10 == 0) ) {
				EMS[e].mk += 2;  // grains become fully rounded
			}
			// An ice layer forms for dry densities above 700 kg m-3!
			if ((EMS[e].theta[ICE] > 0.763) && ((marker % 10 != 7) || (marker % 10 != 8))) {
				EMS[e].mk = (EMS[e].mk / 10) * 10 + 8;
			}
		}

		// First wetting //NIED (H. Hirashima)
		const double theta_residual = 0.024;	// See: Yamaguchi, S., Watanabe, K., Katsushima, T., Sato, A., and Kumakura, T.: Dependence of the water retention curve of snow on snow characteristics, Ann. Glaciol., 53, 6-12, doi:10.3189/2012AoG61A001, 2012.
		if ((marker < 10) && (EMS[e].theta[WATER] > 0.99 * theta_residual)) {
			EMS[e].mk += 10;
		}
		// First melt-freeze cycle completed
		else if ((marker < 20) && (marker >= 10) && (EMS[e].Te < EMS[e].meltfreeze_tk - 0.3)) {
			EMS[e].mk += 10;
		}

		EMS[e].snowType();
	}
}

void Metamorphism::runMetamorphismModel(const CurrentMeteo& Mdata, SnowStation& Xdata) const throw()
{
	CALL_MEMBER_FN(*this, mapMetamorphismModel[metamorphism_model])(Mdata, Xdata);
}
