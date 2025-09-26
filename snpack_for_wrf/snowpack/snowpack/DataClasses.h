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
/**
 * @file DataClasses.h
 * @version 11.08
 * This header file contains all the data structures needed for the 1d snowpack model
 */

#ifndef DATACLASSES_H
#define DATACLASSES_H

#include <snowpack/SnowpackConfig.h>
#include <snowpack/vanGenuchten.h>
#include <snowpack/snowpackCore/SeaIce.h>

#include <snowpack/Constants.h>
#include <meteoio/MeteoIO.h>

#include <string>
#include <vector>

/// @brief The 3 different phases in the matrix
enum {
	SOLID,  ///< Solid
	LIQUID, ///< Liquid
	GAS,   ///< Gas
	NUMBER_OF_PHASES
};

/// @brief Optical Properties of snow
struct SNOW_OPTIC {
	double ggg;     ///< Asymmetry Parameter
	double exteff;  ///< Extinction Efficiency
	double ssa;     ///< Single Scattering Albedo
};

/// @brief Spectrum of incoming solar radiation
struct WL_STRUCT {
	double nm;      ///< Wavelength
	double perc;    ///< Percentage of Energy
};

/**
 * @brief ZwischenData contains "memory" information mainly for operational use\n
 * It is used to prepare some parameters of qr_Hdata. This data is read from and written to *.sno
 * or .haz files respectively
 */
class ZwischenData {
	public:
		ZwischenData(): hoar24(48, 0.0), drift24(48, 0.0), hn3(144, 0.0), hn24(144, 0.0) {}
		void reset();                ///< Sets all the values in the vectors to 0.0

		friend std::ostream& operator<<(std::ostream& os, const ZwischenData& data);
		friend std::istream& operator>>(std::istream& is, ZwischenData& data);

		std::vector<double> hoar24;  ///< Twenty-four hour hoar index every half-hour over one day 48
		std::vector<double> drift24; ///< Twenty-four hour hoar index every half-hour over one day 48
		std::vector<double> hn3;    ///< Three hour new snow heights every half-hour over three days 144
		std::vector<double> hn24;   ///< Twenty-four hour snow heights every half-hour over three days 144
};

/**
 * @brief CurrentMeteo is the class of interpolated meteo data for the current calculation time step \n
 * It contains some additional and very important derived parameters such as the roughness length or running mean values.
 */
class CurrentMeteo {
	public:
		CurrentMeteo();
		CurrentMeteo(const SnowpackConfig& i_cfg);

		void reset(const SnowpackConfig& i_cfg);
		void setMeasTempParameters(const mio::MeteoData& md);
		size_t getNumberMeasTemperatures() const;
		size_t getNumberFixedRates() const;
		size_t getMaxNumberMeasTemperatures() const;
		void getFixedPositions(std::vector<double>& positions) const;
		size_t getNumberFixedPositions() const;
		void copySnowTemperatures(const mio::MeteoData& md, const unsigned int& current_slope);
		void copySolutes(const mio::MeteoData& md, const size_t& i_number_of_solutes);

		const std::string toString() const;
		friend std::ostream& operator<<(std::ostream& os, const CurrentMeteo& data);
		friend std::istream& operator>>(std::istream& is, CurrentMeteo& data);

		mio::Date date;  ///< Date of current meteo data
		double ta;       ///< Air temperature (K)
		double rh;       ///< Relative humidity (% or 1)
		double rh_avg;   ///< Running mean of relative humidity (1)
		double vw;       ///< Wind velocity at snow station (m s-1)
		double vw_avg;   ///< Running mean of wind velocity at snow station (m s-1)
		double vw_max;   ///< Maximum wind velocity at snow station (m s-1)
		double dw;       ///< Wind direction at snow station (deg)
		double vw_drift; ///< Wind velocity for blowing and drifting snow (operational: wind ridge station)
		double dw_drift; ///< Wind direction of blowing and drifting snow (operational: wind ridge station)
		double ustar;    ///< The friction velocity (m s-1) computed in mt_MicroMet() and also used later for the MeteoHeat fluxes
		double z0;       ///< The roughness length computed in SnowDrift and also used later for the MeteoHeat fluxes (m)
		double psi_s;    ///< Stability correction for scalar heat fluxes
		double psi_m;    ///< Stability correction for momentum
		double iswr;     ///< Incoming SHORTWAVE radiation (W m-2)
		double rswr;     ///< Reflected SHORTWAVE radiation (W m-2) divide this value by the ALBEDO to get iswr
                double ilwr_v;    ///< INCOMING LONGWAVE RADIATION ADDED BY VARUN
		double mAlbedo;  ///< Measured snow albedo
		double diff;     ///< Diffuse radiation from the sky (W m-2)
		double dir_h;    ///< Horizontal direct radiation from the sky (W m-2)
		double elev;     ///< Solar elevation to be used in Canopy.c (rad) => see also
		double ea;       ///< Atmospheric emissivity (1)
		double tss;      ///< Snow surface temperature (K)
		double tss_a12h; ///< Snow surface temperature averaged over past 12 hours (K)
		double tss_a24h; ///< Snow surface temperature averaged over past 24 hours (K)
		double ts0;      ///< Bottom temperatures of snow/soil pack (K)
		double liq_psum; ///< liquid precipitation sum (mm)
		double solid_psum;  ///< solid precipitation sum (mm)
		double psum_tech;///< Equivalent precipitation water sum for technical snow over the current timestep (mm)
		double hs;       ///< The measured height of snow (m)
		double hs_a3h;   ///< Snow depth averaged over 3 past hours
		double hs_rate;  ///< The rate of change in snow depth (m h-1)
		double geo_heat; ///< Geo heat flux (W/m^2), for the neumann lower boundary condition in the heat equation
		double adv_heat; ///< Advective heat to inject in the soil (if ADVECTIVE_HEAT and related parameters set to true)
 		double surf_melt;///< Surface melt (kg m-2) per CALCULATION_STEP_LENGTH
 		double snowdrift;///< Snow drift (erosion or deposition) (kg m-2) per CALCULATION_STEP_LENGTH
 		double sublim;   ///< Surface sublimation and evaporation (kg m-2) per CALCULATION_STEP_LENGTH
		double odc;      ///< Optical depth of the cloud (HACK: units??)
		double p;        ///< Atmospheric pressure (HACK: units??)
                int sim_counter;
                double density_air;
		std::vector<double> ts;    ///< Measured snow or/and soil temperatures (K)
		std::vector<double> zv_ts; ///< Positions of all measured snow or/and soil temperatures (m)
		std::vector<double> conc;  ///< Solute concentrations in precipitation
		double rho_hn;             ///< Measured new snow density (kg m-3)

	private:
		size_t getNumberMeasTemperatures(const mio::MeteoData& md);

		std::vector<double> fixedPositions; ///< Positions of fixed snow/soil temperatures (m)
		double minDepthSubsurf;             ///< Sensor must be covered by minDepthSubsurf (m) to be output
		size_t maxNumberMeasTemperatures;   ///< Max allowed number of measured snow/soil temperatures, depending on variant
		size_t numberMeasTemperatures;      ///< Number of measured snow/soil temperatures
		size_t numberFixedRates;
};

/// @brief The 3 mathematical fields that can be solved
enum SN_FIELD{
	TEMPERATURE, ///< Temperature (phase change and metamorphism)
	SEEPAGE,     ///< Water transport
	SETTLEMENT,  ///< Creep displacements
	N_SN_FIELDS
};

/// @brief The 4 different components of the porous matrix
enum {
	SOIL,       ///< Soil
	ICE,        ///< Ice
	WATER,      ///< Water
	WATER_PREF, ///< Water in preferential flow
	AIR,        ///< Air
	N_COMPONENTS
};

/// @brief The different soil fields (PERMAFROST)
enum SN_SOIL_DATA{
	SOIL_RHO, ///< Bulk density of dry soil component (without accounting for porosity)
	SOIL_K,   ///< Bulk conductivity of dry soil component
	SOIL_C,   ///< Specific heat of dry soil component
	N_SOIL_FIELDS
};

/**
 * @brief Parameters of the different layers of the snowpack.
 * @details The layers form a vector within the SSdata (profile) data structure.
 * This is only used by SN_SNOWSOIL_DATA and filled in the plugins
 */
class LayerData {
	public:
		LayerData();

		const std::string toString() const;
		friend std::ostream& operator<<(std::ostream& os, const LayerData& data);
		friend std::istream& operator>>(std::istream& is, LayerData& data);

		mio::Date depositionDate;   ///< Date of deposition (mainly used for snow layers)
		double hl;                  ///< The thickness of the layer in m
		size_t ne;                  ///< Number of finite elements in the the layer (hl/ne defines elm. size)
		double tl;                  ///< Temperature at the top of the layer in K
		double phiSoil;             ///< Volumetric soil content in %
		double phiIce;              ///< Volumetric ice content in %
		double phiWater;            ///< Volumetric water content in %
		double phiWaterPref;        ///< Volumetric preferential water content in %
		double phiVoids;            ///< Volumetric void content in %
		std::vector<double> cSoil;  ///< Solute concentrations in Soil
		std::vector<double> cIce;   ///< Solute concentrations in Ice
		std::vector<double> cWater; ///< Solute concentrations in Water
		std::vector<double> cVoids; ///< Solute concentrations in Air
		double SoilRho;             ///< Density of soil in kg m-3
		double SoilK;               ///< Conductivity of soil
		double SoilC;               ///< Heat Capacity of soil
		double rg;                  ///< Micro-structure : Grainsize in mm
		double sp;                  ///< Micro-structure : Sphericity
		double dd;                  ///< Micro-structure : Dendricity
		double rb;                  ///< Micro-structure : Bond Radius in mm
		unsigned short int mk;      ///< Micro-structure : Marker
		double hr;                  ///< Surface hoar Mass in kg m-2
		double CDot;                ///< Stress rate (Pa s-1), that is the LAST overload change rate
		double metamo;              ///< keep track of metamorphism
		double salinity;            ///< salinity (kg/kg)
		double h;                   ///< capillary pressure head (m)
};

/**
 * @brief SN_SNOWSOIL_DATA includes all important station parameters as well as LayerData.
 * @details This data structure will have to be replaced by something a little more complicated soon ???
 * For now it is simply an efficient way of creating a snowpack to investigate.
 */
class SN_SNOWSOIL_DATA {
	public:
		SN_SNOWSOIL_DATA() : meta(), profileDate(), nN(0), Height(0.),
                     nLayers(0), Ldata(), HS_last(0.), Albedo(mio::IOUtils::nodata),
								     SoilAlb(mio::IOUtils::nodata), BareSoil_z0(mio::IOUtils::nodata),
                     Canopy_Height(mio::IOUtils::nodata), Canopy_LAI(mio::IOUtils::nodata),
										 Canopy_Direct_Throughfall(mio::IOUtils::nodata),  WindScalingFactor(1.),
										 ErosionLevel(static_cast<int>(mio::IOUtils::nodata)), TimeCountDeltaHS(mio::IOUtils::nodata),
										 Canopy_BasalArea(mio::IOUtils::nodata), Canopy_diameter(mio::IOUtils::nodata),
										 Canopy_lai_frac_top_default(mio::IOUtils::nodata),Canopy_int_cap_snow(mio::IOUtils::nodata),
										 Canopy_alb_dry(mio::IOUtils::nodata),Canopy_alb_wet(mio::IOUtils::nodata),
										 Canopy_alb_snow(mio::IOUtils::nodata),Emissivity_soil(mio::IOUtils::nodata)
                    {}

		const std::string toString() const;
		friend std::ostream& operator<<(std::ostream& os, const SN_SNOWSOIL_DATA& data);
		friend std::istream& operator>>(std::istream& is, SN_SNOWSOIL_DATA& data);

		mio::StationData meta;            ///< Station meta data
		mio::Date profileDate;            ///< Date of profile
		size_t nN;                        ///< Total number of FE nodes after loading
		double Height;                    ///< Total height of soil-snow column in m (sum of the layer heights)
		size_t nLayers;                   ///< Total number of soil and snow layers at loading
		std::vector<LayerData> Ldata;     ///< contains all the information required to construct the Xdata
		double HS_last;                   ///< Last checked calculated snow depth used for albedo control
		/// REQUIRED PARAMETERS, an error will be thrown at reading (SnowpackIO) if no parameter are provided
		double Albedo;                    ///< Snow albedo
		double SoilAlb;                   ///< Soil albedo, default 0.2
		double BareSoil_z0;               ///< Bare soil roughness in m, default 0.02 m
		double Canopy_Height;             ///< Canopy Height in m
		double Canopy_LAI;                ///< Canopy Leaf Area Index in m2 m-2
		double Canopy_Direct_Throughfall; ///< Direct throughfall [fraction of precipitation]
		double WindScalingFactor;         ///< Local scaling factor for wind at drift station
		int    ErosionLevel;              ///< Erosion Level in operational mode (flat field virtual erosion)
		double TimeCountDeltaHS;          ///< Time counter tracking erroneous settlement in operational mode
		/// OPTIONNAL PARAMETERS, a warning will be thrown in CANOPY::Initialze if no value is provided
		double Canopy_BasalArea;          ///< Canopy Basal Area in m2 m-2
		double Canopy_diameter;						///< Average canopy (tree) diameter [m], parameter in the new radiation transfer model
		double Canopy_lai_frac_top_default;	///< fraction of total LAI that is attributed to the uppermost layer. Here calibrated for Alptal.
		double Canopy_int_cap_snow;				///< Specific interception capacity for rain (I_LAI) (mm/LAI)
		double Canopy_alb_dry;  // Albedo of dry canopy (calibr: 0.09, Alptal)
		double Canopy_alb_wet;  // Albedo of wet canopy (calibr: 0.09, Alptal)
		double Canopy_alb_snow;  // Albedo of snow covered albedo (calibr: 0.35, Alptal)
    /// OPTIONNAL PARAMETERS, if not provided Constants::emissivity_soil will be used
    double Emissivity_soil;


};

/**
 * @brief ELEMENT DATA used as a pointer in the SnowStation structure
 * NOTE on M below: this is the mass of an element that is neither changed by phase changes nor densification. \n
 * It is set in the data initialization and used to compute the stress field.
 * It can ONLY be changed by the WATER TRANSPORT or SURFACE SUBLIMATION or WIND TRANSPORT routines.
 */
class ElementData {
	public:
		/// This enum provides names for possible Young's modulus calculations
		typedef enum YOUNG_MODULUS {
		            Sigrist, ///< Sigrist, 2006
			    Pow, ///< another power law
		            Exp ///< exponential law
		} Young_Modulus;

		ElementData(const unsigned short int& in_ID);
		ElementData(const ElementData& cc); //required to get the correct back-reference in vanGenuchten object

		bool checkVolContent();
		void heatCapacity();
		double coldContent() const;
		void updDensity();
		double extinction() const;
		void opticalEquivalentGrainSize();
		void snowResidualWaterContent();
		static double snowResidualWaterContent(const double& theta_i);
		double soilFieldCapacity() const;
		double RelativeHumidity() const;

		double snowElasticity() const;
		double neckStressEnhancement() const;
		double concaveNeckRadius() const;
		double neckLength() const;
		double neck2VolumetricStrain() const;

		void snowType();
		unsigned short int getSnowType() const;
		static unsigned short int snowType(const double& dendricity, const double& sphericity, const double& grain_dia, const unsigned short int& marker,
                        const double& theta_w, const double& res_wat_cont_loc);
		static double getYoungModule(const double& rho_slab, const Young_Modulus& model);

		const std::string toString() const;
		friend std::ostream& operator<<(std::ostream& os, const ElementData& data);
		friend std::istream& operator>>(std::istream& is, ElementData& data);

		mio::Date depositionDate;  ///< Date of deposition
		double L0, L;              ///< Original and present element thickness (m)
		double Te;                 ///< mean element temperature (K)
		double gradT;              ///< temperature gradient over element (K m-1)
		double meltfreeze_tk;	   ///< melt/freeze temperature of layer (principally initialized as 0 degC, but enables possibility for freezing point depression)
		std::vector<double> theta; ///< volumetric contents: SOIL, ICE, WATER, WATER_PREF, AIR (1)
		double h;                  ///< capillary pressure head (m)
		mio::Array2D<double> conc; ///< Concentration for chemical constituents in (kg m-3)
		std::vector<double> k;     ///< For example, heat conductivity of TEMPERATURE field (W m-1 K-1)
		//   Stored in order to visualize constitutive laws
		//   Will be used for creep field hydraulic conductivity in m3 s kg-1
		std::vector<double> c;     ///< For example, specific heat of TEMPERATURE field (J kg-1 K-1)
		//   Will also be used for creep specific snow water capacity  in m3 J-1
		std::vector<double> soil;  ///< Contains the heat conductivity, capacity and dry density of the soil (solid, non-ice)  component phase
		double Rho;                ///< mean element density (or BULK density; kg m-3), that is, rho=M/V=sum( theta(i)*rho(i) )
		double M;                  ///< the total mass of the element (kg m-2)
		double sw_abs;             ///< total absorbed shortwave radiation by the element (W m-2)
		// Snow Metamorphism Data
		double rg;                 ///< grain radius (mm)
		double dd;                 ///< snow dendricity: 0 = none, 1 = newsnow
		double sp;                 ///< sphericity: 1 = round, 0 = angular
		double ogs;                ///< optical equivalent grain size (mm)
		double rb;                 ///< grain bond radius (mm)
		double N3;                 ///< grain Coordination number (1)
		size_t mk;                 ///< grain marker (history dependent)
		unsigned short int type;   ///< grain class
		double metamo;             ///< keep track of metamorphism
		double salinity;           ///< salinity (PSU, which is g/kg)
		double dth_w;              ///< Subsurface Melting & Freezing Data: change of water content
		double res_wat_cont;       ///< Residual water content
		double Qmf;                ///< Subsurface Melting & Freezing Data: change of energy due to phase changes (melt-freeze)
		double QIntmf;             ///< Apparent change in internal energy due to phase change (caused by difference in heat capacity of water and ice)
		double dEps, Eps, Eps_e, Eps_v;      ///< Total element strain (GREEN'S strains -- TOTAL LAGRANGIAN FORMULATION): Eps_e is elastic and Eps_v is viscous
		double Eps_Dot, Eps_vDot;        ///< Total Strain Rate, elastic and viscous, respectively (s-1) (Simply, Eps/sn_dt)
		double E;                  ///< Young's modulus of elasticity (Pa)
		double S;                  ///< Total Element Stress (Pa), S being the energy conjugate stress
		double C;                  ///< Total Element Stress (Pa), C being the real or the Cauchy stress, which is output
		double CDot;               ///< Stress rate (Pa s-1), that is the overload change rate
		double ps2rb;              ///< proportion of grain bond growth due to pressure sintering (1)
		double s_strength;         ///< Parameterized snow shear strength (kPa)
		double hard;               ///< Parameterized hand hardness (1)
		double S_dr;               ///< Stability Index based on deformation rate (Direct Action Avalanching)
		double crit_cut_length;    ///< Critical cut length (m)
		double soot_ppmv;          ///< Impurity content (ppmv) used for albedo calculation of Gardner
		vanGenuchten VG;           ///< Van Genuchten Model for water retention
		double lwc_source;         ///< Source/sink term for Richards equation (m^3/m^3 / timestep)
		double PrefFlowArea;       ///< Preferential flow path relative area (-)
		double SlopeParFlux;       ///< Slope parallel flux (m^3/m^3 * m / timestep)
		double Qph_up;             ///< Heat source/sink due to phase changes for the heat equation (W/m^3), at the upper node of the element
		double Qph_down;           ///< Heat source/sink due to phase changes for the heat equation (W/m^3), at the lower node of the element
		//NIED (H. Hirashima)
		double dsm;                ///< Dry snow metamorphism factor

		unsigned short int ID;    ///< Element ID used to track elements
		static const unsigned short int noID;
};

/// @brief NODAL DATA used as a pointer in the SnowStation structure
class NodeData {
	public:
		NodeData() : z(0.), u(0.), f(0.), udot(0.), T(0.), S_n(0.), S_s(0.), ssi(6.), hoar(0.),
		             dsm(0.), S_dsm(0.), Sigdsm(0.) {} //HACK: set ssi to max_stability!

		const std::string toString() const;
		friend std::ostream& operator<<(std::ostream& os, const NodeData& data);
		friend std::istream& operator>>(std::istream& is, NodeData& data);

		double z;    ///< nodal height from ground in m
		double u;    ///< creep displacements in m
		double f;    ///< reaction or unbalanced forces (CREEP)
		double udot; ///< downward creep velocity in m s-1
		double T;    ///< nodal temperature in K
		double S_n;  ///< Stability Index for natural avalanches
		double S_s;  ///< Stability Index for skier triggered avalanches
		double ssi;  ///< Structural Stability Index
		double hoar; ///< Mass of surface hoar collected while node was exposed to surface

		//NIED (H. Hirashima)
		double dsm;  ///< Dry snow metamorphism factor
		double S_dsm;
		double Sigdsm;
};

/**
 * @brief Canopy data used as a pointer in the SnowStation structure
 * -# INSTANTANEOUS VARIABLES
 * 	-# Canopy "state" variables, and some auxiliaries
 * 	-# Properties which could be given here or as a parameter field
 * 	-# Aerodynamic resistances
 * -# CUMULATED/AVERAGE VARIABLES \n
 *    cumulated between time series output timesteps - these variables can be moved or mirrored in a canopy "surface" data structure
 * 	-# Albedo and similar auxiliaries
 * 	-# Radiation fluxes
 * 	-# Canopy turbulent heat fluxes
 * 	-# Canopy evaporative fluxes
 * 	-# Canopy mass fluxes
 */
class CanopyData {
	public:
		CanopyData() : int_cap_snow(0.), int_cap_rain(0.),  interception_timecoef(0.), can_alb_dry(0.),
		can_alb_wet(0.), can_alb_snow(0.), krnt_lai(0.), can_diameter(0.), biomass_heat_capacity(0.),
		biomass_density(0.), lai_frac_top_default(0.), trunk_frac_height(0.), trunkalb(0.), et(0.),
		canopy_stabilitycorrection(true), roughmom_to_canopyheight_ratio(0.), displ_to_canopyheight_ratio(0.),
		raincrease_snow(0.), canopytemp_maxchange_perhour(0.), roughheat_to_roughmom_ratio(0.),
		can_ch0(0.), can_rs_mult(0.), rsmin(0.), f3_gd(0.), rootdepth(0.), wp_fraction(0.),
		h_wilt(0.), storage(0.), temp(0.), sigf(0.), ec(0.), lai(0.), z0m(0.), z0h(0.), zdispl(0.),
		height(0.), direct_throughfall(0.), ra(0.), rc(0.), rs(0.), rstransp(0.), canopyalb(0.),
		totalalb(0.), wetfraction(0.), intcapacity(0.), rswrac(0.), iswrac(0.), rswrbc(0.), iswrbc(0.),
		ilwrac(0.), rlwrac(0.), ilwrbc(0.), rlwrbc(0.), rsnet(0.), rlnet(0.), sensible(0.), latent(0.),
		latentcorr(0.), transp(0.), intevap(0.), interception(0.), throughfall(0.), snowunload(0.),
		snowfac(0.), rainfac(0.), liquidfraction(0.), sigftrunk(0.), Ttrunk(0.), CondFluxCanop(0.),
		CondFluxTrunks(0.), LWnet_Trunks(0.), SWnet_Trunks(0.), QStrunks(0.), forestfloor_alb(0.),
		BasalArea(0.), HMLeaves(0.), HMTrunks(0.) {}

		void initialize(const SN_SNOWSOIL_DATA& SSdata, const bool useCanopyModel);
		void reset(const bool& cumsum_mass);
		void initializeSurfaceExchangeData();
		void multiplyFluxes(const double& factor);

		const std::string toString() const;
		friend std::ostream& operator<<(std::ostream& os, const CanopyData& data);
		friend std::istream& operator>>(std::istream& is, CanopyData& data);

		/**
		* CANOPY PARAMETERS DEFAULT VALUES
		* These values can be changed in the .sno file
		* brief History of changed values:
		* - 2007-12-20: update based on data from all SnowMIP2 sites, and calibration using Alptal data
		* brief Specific interception capacity for snow (i_LAI) (mm/LAI) \n
		* Please note that this parameter is further multiplied with (0.27+46/new_snow_density[Ta]) following (Pomeroy et al, Hydr. Proc. 1998)
		* - 5.9 Spruce and 6.6 Pine (Schmidt&Glums,CanJForRes,1991)
		*/
	 	double int_cap_snow; //iMax in Gouttevin,2015
		/// Specific interception capacity for rain (I_LAI) (mm/LAI)
		double int_cap_rain;
		/** Coef in interception function, see (Pomeroy et al,1998) where a value of 0.7 was
		 * found to be appropriate for hourly time-step, but smaller time steps require smaller
		 * values, 0.5 was found reasoanble by using the SnowMIP2 data (2007-12-09)
	  	*/
		double interception_timecoef;

		/// RADIATION BALANCE
		double can_alb_dry;  	// Albedo of dry canopy (calibr: 0.09, Alptal)
		double can_alb_wet; 	// Albedo of wet canopy (calibr: 0.09, Alptal)
		double can_alb_snow; 	// Albedo of snow covered albedo (calibr: 0.35, Alptal)
		double krnt_lai;      	// Radiation transmissivity parameter, in the range 0.4-0.8 if the true LAI is used; higher if optical LAI is used.
		                        // (calibrated on Alptal)
		double can_diameter;	// average canopy (tree) diameter [m], parameter in the new radiation transfer model
		///  ENERGY BALANCE
		/// parameters for HeatMass and 2layercanopy
		double biomass_heat_capacity;	// from Linroth et al., 2013 (J Kg-1 K-1)
		double biomass_density;		// from Linroth et al., 2013 (Kg m-3)
		double lai_frac_top_default;	// fraction of total LAI that is attributed to the uppermost layer. Here calibrated for Alptal.
		double trunk_frac_height;	// (optional) fraction of total tree height occupied by trunks,
						// used to calculate direct solar insolation of trunks.
		double trunkalb;		// trunk albedo
		double et;			// trunk emissivity
		///  TURBULENT HEAT EXCHANGE
		/// Stab. corr. aerodyn. resist. above and below canopy: 0=off and 1=on (Monin-Obukhov formulation)
		bool canopy_stabilitycorrection;
		/// Ratio between canopy height and roughness length
		double roughmom_to_canopyheight_ratio;
		/// As above for displacement height
		double displ_to_canopyheight_ratio;
		/**
		 * Fractional increase of aerodynamic resistance for evaporation of intercepted snow.
		 * - 10.0 from Koivusalo and Kokkonen (2002)
		 * - 8.0 calibration with Alptal data
		 */
		double raincrease_snow;

		/// @brief Maximum allowed canopy temperature change (K hr-1)
		double canopytemp_maxchange_perhour;
		/// @brief (~=1, but Not allowed to be exactly 1)
		double roughheat_to_roughmom_ratio;
		/// @brief minimum heat exchange (Wm-2K-1) at zero wind
		double can_ch0;
		/// @brief 1+CAN_RS_MULT = maximum factor to increase Cdata->rs below canopy
		double can_rs_mult;
		/// @brief TRANSPIRATION
		/// @brief Minimum canopy surface resistance, 500 (sm-1) is for needle leaf treas van den Hurk et al (2000) *75% Gustafsson et al (2003)
		double rsmin;
		/**
		 * @brief gd (Pa-1) parameter for canopy surface resistance response to vapour pressure:
		 * - 0.0003 = trees (needle or broadleafs)
		 * - 0=crops, grass, tundra etc
		 */
		double f3_gd;
		/// @brief Root depth, determining the soil layers influenced by root water uptake
		double rootdepth;
		/// @brief Wilting point, defined as a fraction of water content at field capacity (-)
		double wp_fraction;
		/// @brief Wilting point pressure head, when using Richards equation for soil.
		double h_wilt;
		//@}

		// State variable
		double storage;     ///< intercepted water (mm or kg m-2)
		double temp;        ///< temperature (K)
		double sigf;        ///< radiation transmissivity (1)
		double ec;          ///< longwave emissivity (1)
		// parameters
		double lai;
		double z0m;
		double z0h;
		double zdispl;
		double height;
		double direct_throughfall;
		// aerodynamic resistances
		double ra;          ///< from canopy air to reference height
		double rc;          ///< from canopy to canopy air
		double rs;          ///< from subsurface to canpopy air
		double rstransp;    ///< stomatal surface resistance for transpiration
		// Averaged variables
		double canopyalb;   ///< canopy albedo [-]
		double totalalb;    ///< total albedo above canopy and snow/soil surface [-]
		double wetfraction; ///< fraction of canopy covered by interception [-]
		double intcapacity; ///< maximum interception storage [mm]
		// Radiations
		double rswrac;      ///< upward shortwave above canopy
		double iswrac;	    ///< downward shortwave radiation above canopy
		double rswrbc;      ///< upward shortwave below canopy
		double iswrbc;      ///< downward shortwave radiation below canopy
		double ilwrac;      ///< downward longwave radiation ABOVE canopy
		double rlwrac;      ///< upward longwave radiation ABOVE canopy
		double ilwrbc;      ///< downward longwave radiation BELOW canopy
		double rlwrbc;      ///< upward longwave radiation BELOW canopy
		double rsnet;       ///< net shortwave radiation
		double rlnet;       ///< net longwave radiation
		// Turbulent fluxes
		double sensible;
		double latent;
		double latentcorr;
		// Evap fluxes
		double transp;
		double intevap;
		// Mass fluxes
		double interception;
		double throughfall;
		double snowunload;

		double snowfac;     	///< snowfall above canopy
		double rainfac;     	///< rainfall above canopy
		double liquidfraction;
		double sigftrunk;   	///< radiation interception cross section for trunk layer ()
		double Ttrunk;      	///< trunk temperature (K)
		double CondFluxCanop; 	///< biomass heat storage flux towards Canopy (if 1L) towards Leaves (if 2L). (>0 towards canopy)
		double CondFluxTrunks; 	///< biomass heat storage flux towards Trunks (if 2L)
		double LWnet_Trunks; 	///< net LW to trunks (>0 towards trunks)
		double SWnet_Trunks; 	///< net SW to trunks (>0 towards trunks)
		double QStrunks;      	///< sensible heat flux from trunks (>0 if heat lost from trunk)
		double forestfloor_alb; ///< albedo of the forest floor
		double BasalArea;    	///< basal area of trees on the stand
		double HMLeaves;     	///< Leaves heat mass (J K-1 /m2 ground surface)
		double HMTrunks;     	///< Trunks heat mass (J K-1 /m2 ground surface)
};

/**
 * @brief Station data including all information on snowpack layers (elements and nodes) and on canopy \n
 * This is the PRIMARY data structure of the SNOWPACK program \n
 * It is used extensively not only during the finite element solution but also to control
 * the post-processing writes. It is initialized from SN_SNOWSOIL_DATA (at present).
 */
class SeaIce;	// Foreward-declare sea ice class
class SnowStation {
	public:
		explicit SnowStation(const bool& i_useCanopyModel=true, const bool& i_useSoilLayers=true, const bool& i_useSeaIceModule=false);
		SnowStation(const SnowStation& c);

		~SnowStation();
		SnowStation& operator=(const SnowStation&); ///<Assignement operator

		void initialize(const SN_SNOWSOIL_DATA& SSdata, const size_t& i_sector);
		void resize(const size_t& number_of_elements);

		void reduceNumberOfElements(const size_t& rnE);
		void combineElements(const size_t& number_top_elements, const int& reduce_n_elements, const size_t& cond, const double& comb_thresh_l);
		static bool combineCondition(const ElementData& Edata0, const ElementData& Edata1, const double& depth, const int& reduce_n_elements, const double& comb_thresh_l);
		static void mergeElements(ElementData& Edata0, const ElementData& Edata1, const bool& merge, const bool& topElement);
		void splitElement(const size_t& e);							//Split an element
		void splitElements(const double& max_element_length, const double& comb_thresh_l);	//Check for splitting, calls splitElement(...) for actual splitting
		void CheckMaxSimHS(const double& max_simulated_hs);					//Check for hs > max_simulated_hs

		void compSnowpackMasses();
		void compSnowpackInternalEnergyChange(const double& sn_dt);
		void compSoilInternalEnergyChange(const double& sn_dt);
		double getLiquidWaterIndex() const;
		double getModelledTemperature(const double& z) const;

		size_t getNumberOfElements() const;
		size_t getNumberOfNodes() const;
		bool isGlacier(const bool& hydro=false) const;
		bool hasSoilLayers() const;
		double findMarkedReferenceLayer() const;

		size_t find_tag(const size_t& tag) const;

		const std::string toString() const;
		friend std::ostream& operator<<(std::ostream& os, const SnowStation& data);
		friend std::istream& operator>>(std::istream& is, SnowStation& data);

		mio::StationData meta;      ///< Station meta data
		double cos_sl;              ///< Cosinus of slope angle, initialized once!
		size_t sector;              ///< current slope sector of width 360./max(1, nSlopes-1)

		CanopyData Cdata;           ///< Pointer to canopy data
		SeaIce* Seaice;             ///< Pointer to sea ice class
		double pAlbedo;             ///< Parameterized snow albedo
		double Albedo;              ///< Snow albedo used by the model
		double SoilAlb;             ///< Soil albedo
    double SoilEmissivity;      ///< Soil emissivity
		double BareSoil_z0;         ///< Bare soil roughness in m
		size_t SoilNode;            ///< The top soil node, 0 in case of SNP_SOIL == 0
		double Ground;              ///< The ground height -- meaning the height of the top soil node
		double cH;                  ///< The CALCULATED height, including soil depth if SNP_SOIL == 1
		double mH;                  ///< The MEASURED height, including soil depth if SNP_SOIL == 1
		double mass_sum;            ///< Total mass summing mass of snow elements
		double swe;                 ///< Total mass summing snow water equivalent of elements
		double lwc_sum;             ///< Total liquid water in snowpack
		double hn;                  ///< Depth of new snow to be used on slopes
		double rho_hn;              ///< Density of new snow to be used on slopes
		size_t ErosionLevel;        ///< Element where snow erosion stopped previously for the drift index
		double ErosionMass;         ///< Eroded mass either real or virtually (storage if less than one element)
		char S_class1;               ///< Stability class based on hand hardness, grain class ...
		char S_class2;               ///< Stability class based on hand hardness, grain class ...
		double S_d;                 ///< Minimum Direct Action Stability Index  ...
		double z_S_d;               ///< Depth of Minimum Direct Action Stability
		double S_n;                 ///< Minimum Natural Stability Index
		double z_S_n;               ///< Depth of Minimum Natural Stability
		double S_s;                 ///< Minimum Skier Stability Index (SSI)
		double z_S_s;               ///< Depth of Minimum SSI
		double S_4;                 ///< stab_index4
		double z_S_4;               ///< Depth of stab_index4
		double S_5;                 ///< stab_index5
		double z_S_5;               ///< Depth of stab_index5
		std::vector<NodeData> Ndata;    ///< pointer to nodal data array (e.g. T, z, u, etc..)
		std::vector<ElementData> Edata; ///< pointer to element data array (e.g. Te, L, Rho, etc..)
		void *Kt;                   ///< Pointer to pseudo-conductivity and stiffnes matrix
		double ColdContent;         ///< Cold content of snowpack (J m-2)
		double ColdContentSoil;     ///< Cold content of soil (J m-2)
		double dIntEnergy;          ///< Internal energy change of snowpack (J m-2)
		double dIntEnergySoil;      ///< Internal energy change of soil (J m-2)
		double meltFreezeEnergy;    ///< Melt freeze part of internal energy change of snowpack (J m-2)
		double meltFreezeEnergySoil;///< Melt freeze part of internal energy change of soil (J m-2)
		double meltMassTot;         ///< Vertically summed melt per model time step (kg m-2)
		double refreezeMassTot;     ///< Vertically summed refreeze per model time step (kg m-2)
		double ReSolver_dt;         ///< Last used RE time step in the previous SNOWPACK time step
		bool windward;              ///< True for windward (luv) slope
		double WindScalingFactor;   ///< Local scaling factor for wind at drift station
		double TimeCountDeltaHS;    ///< Time counter tracking erroneous settlement in operational mode
                double tau_thresh;          ///< ADDED BY VARUN
                double tau;                 ///< ADDED BY VARUN
                double density_air; 
		static const double comb_thresh_l_ratio, comb_thresh_ice, comb_thresh_water;
		static const double comb_thresh_dd, comb_thresh_sp, comb_thresh_rg;
		static const double thresh_moist_snow, thresh_moist_soil;
		static const size_t number_top_elements;
		static unsigned short number_of_solutes;  ///< The model treats that number of solutes

	private:
		size_t nNodes;                      ///< Actual number of nodes; different for each exposition
		size_t nElems;                      ///< Actual number of elements (nElems=nNodes-1)
		unsigned short int maxElementID;    ///< maximum ElementID currently used (so each element can get a unique ID)
		bool useCanopyModel, useSoilLayers; ///< The model includes soil layers
		static double flexibleMaxElemLength(const double& depth, const double& comb_thresh_l); ///< When using REDUCE_N_ELEMENTS, this function determines the max element length, depending on depth inside the snowpack.
};

/**
* @brief BoundCond is used to set Neumann boundary conditions
*/
class BoundCond {

	public:
		BoundCond() : lw_out(0.), lw_net(0.), qs(0.), ql(0.), qr(0.), qg(Constants::undefined) {}
		const std::string toString() const;
		void reset();

		double lw_out;  ///< outgoing longwave radiation
		double lw_net;  ///< net longwave radiation
		double qs;      ///< sensible heat
		double ql;      ///< latent heat
		double qr;      ///< rain energy
		double qg;      ///< geothermal heat flux or heat flux at lower boundary
};

/**
* @name Surface data
* @note Some of the most important results of the simulation are contained in these data structures
*/
//@{
class SurfaceFluxes {
	public:
		/**
		 * @brief The different types of mass fluxes:
		 * Mass fluxes in kg m-2 \n
		 * Rates in kg m-2 h-1 (MS_HNW, MS_RAIN and MS_WIND)
		 */
		enum SN_MASS_CHANGES {
			MS_TOTALMASS,      ///< This of course is the total mass of the snowpack at the present time
			MS_SWE,            ///< This too, of course, but summing rho*L
			MS_WATER,          ///< The total amount of water in the snowpack at the present time
			MS_HNW,            ///< Solid precipitation rate
			MS_RAIN,           ///< Rain rate
			MS_WIND,           ///< Mass loss rate due to wind erosion
			MS_EVAPORATION,    ///< The mass loss or gain of the top element due to water evaporating
			MS_SUBLIMATION,    ///< The mass loss or gain of the top element due to snow (ice) sublimating
			MS_SNOWPACK_RUNOFF,///< The total mass loss of snowpack due to water transport (virtual lysimeter)
			MS_SOIL_RUNOFF,    ///< Equivalent to MS_SNOWPACK_RUNOFF but at bottom soil node
			MS_FLOODING,       ///< Flooding of sea ice (Bucket scheme only)
                        MS_DEPOSITION,     ///< Deposition due to reduction of saltation concentration
			N_MASS_CHANGES     ///< Total number of different mass change types
		};

		const std::string toString() const;
		friend std::ostream& operator<<(std::ostream& os, const SurfaceFluxes& data);
		friend std::istream& operator>>(std::istream& is, SurfaceFluxes& data);

		SurfaceFluxes();

		void reset(const bool& cumsum_mass);
		void compSnowSoilHeatFlux(const SnowStation& Xdata);
		void collectSurfaceFluxes(const BoundCond& Bdata, SnowStation& Xdata, const CurrentMeteo& Mdata);
		void multiplyFluxes(const double& factor);

		/**
		 * @brief Energy fluxes:
		 * Energy change of snowpack in kJ m-2 (dIntEnergy)\n
		 * Fluxes in W m-2
		 */
		double lw_in;      ///< incoming longwave radiation
		double lw_out;     ///< outgoing longwave radiation
		double lw_net;     ///< net longwave radiation
		double qs;         ///< sensible heat
		double ql;         ///< latent heat
		double hoar;       ///< mass of surface hoar formed or sublimated
		double qr;         ///< rain energy
		double qg;         ///< geothermal heat flux or heat flux at lower boundary
		double qg0;        ///< ground heat flux at soil-snow interface
		double sw_hor;     ///< incoming global shortwave radiation on horizontal surface
		double sw_in;      ///< incoming global shortwave radiation; on slopes: projected
		double sw_out;     ///< reflected shortwave radiation
		double qw;         ///< net shortwave radiation at the surface (absorbed within the snowpack)
		double sw_dir;     ///< incoming direct shortwave radiation; on slopes: projected
		double sw_diff;    ///< incoming diffuse shortwave radiation
		double pAlbedo;    ///< parameterized Albedo (used for OUTPUT only)
		double mAlbedo;    ///< measured Albedo (used for OUTPUT only)
		double dIntEnergy;           ///< Internal energy change in J m-2 in snowpack (used for OUTPUT only)
		double dIntEnergySoil;       ///< Internal energy change in J m-2 in soil (used for OUTPUT only)
		double meltFreezeEnergy;     ///< Melt freeze part of internal energy change in J m-2 in snowpack (used for OUTPUT only)
		double meltFreezeEnergySoil; ///< Melt freeze part of internal energy change in J m-2 in soil (used for OUTPUT only)
		double meltMass;             ///< Melt mass (kg m-2) (used for OUTPUT only)
		double refreezeMass;         ///< Refreeze mass (kg m-2) (used for OUTPUT only)

		/// @brief Other surface data:
		double drift;      ///< the surface flux of drifting snow in kg m-1 s-1
		std::vector<double> mass; ///< Total mass of snowpack PLUS different amounts of total mass change, sublimation, runoff, erosion, etc. Basically the mass which crosses the surface
		std::vector<double> load; ///< Total load (kg m-2) in water runoff from solutes like nitrate
		double dhs_corr;   ///< operational mode only: snow depth correction in case of squeezing or blow-up (m)
		double cRho_hn;    ///< Computed new snow density (kg m-3)
		double mRho_hn;    ///< Measured new snow density (kg m-3)

};
//@}

/// @brief Defines structure for snow profile layers
//HACK: could it be moved to plugins? (as well as Aggregate)
class SnowProfileLayer {
	public:
		SnowProfileLayer();

		void average(const double& w1, const double& w2, const SnowProfileLayer& Pdata);
		static std::vector<SnowProfileLayer> generateProfile(const mio::Date& dateOfProfile, const SnowStation& Xdata, const double hoar_density_surf, const double hoar_min_size_surf);

		// Profile meta data
		mio::Date profileDate; ///< Date of profile
		std::string stationname;
		unsigned char  loc_for_snow;
		unsigned char  loc_for_wind;

		mio::Date depositionDate;   ///< Date of deposition (mainly used for snow layers)
		double height;         ///< Height of snow or snow depth; 0 to 1000      (cm)
		double rho;            ///< 0 to 1000      (kg m-3)
		double T;              ///< -50 to 50, snow temperature at top of layer (degC)
		double gradT;          ///< -1000 to 1000, temperature gradient across layer (K m-1)
		double v_strain_rate;  ///< 0 to 1.0e-5, viscous strain rate (s-1)
		double theta_i;        ///< 0 to 1, volume fraction of ice (-)
		double theta_w;        ///< 0 to 1, volume fraction of water (-)
		double theta_a;        ///< 0 to 1, volume fraction of air (-)
		double grain_size;     ///< 0 to 100       (mm)
		double bond_size;      ///< 0 to 100       (mm)
		double dendricity;     ///< 0 to 1         (1)
		double sphericity;     ///< 0 to 1         (1)
		double ogs;            ///< 0 to 100, optical equivalent grain size (mm)
		double coordin_num;    ///< 0 to 10        (1)
		unsigned short int marker;         ///< 0 to 999       (1)
		short unsigned int type; ///< 0 to 999     (1)
		double hard;           ///< 0. to 5.       (1)

	private:
		void generateLayer(const ElementData& Edata, const NodeData& Ndata);
		void generateLayer(const ElementData& Edata, const NodeData& Ndata,
		                   const mio::Date& dateOfProfile, const double hoar_density_surf);
};

/// @brief class to collect the information about the current simulation (version, date)
class RunInfo {
	public:
		RunInfo();
		RunInfo(const RunInfo& orig);
		RunInfo& operator=(const RunInfo&) {return *this;} //everything is static, so we can not change anything

		const std::string version;   ///< SNOWPACK version
		const mio::Date computation_date; ///< Date of computation
		const std::string compilation_date; ///< Date of compilation
		const std::string user; ///< logname of the user running the simulation

	private:
		static mio::Date getRunDate();
		static std::string getCompilationDate();
};

#endif
