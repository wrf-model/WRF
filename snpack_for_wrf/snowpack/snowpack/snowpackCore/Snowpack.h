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
 * @file Snowpack.h
 * @version 10.02
 * This header file contains all the data structures needed for the 1d snowpack model
 */

#ifndef SNOWPACK_H
#define SNOWPACK_H

#include <snowpack/Meteo.h>
#include <snowpack/DataClasses.h>
#include <snowpack/SnowDrift.h>
#include <snowpack/snowpackCore/Metamorphism.h>
#include <snowpack/snowpackCore/PhaseChange.h>

#include <meteoio/MeteoIO.h>
#include <vector>
#include <string>

/// @brief The number of element incidences
#define N_OF_INCIDENCES 2

class Snowpack {

 public:
		Snowpack(const SnowpackConfig& i_cfg);

		void runSnowpackModel(CurrentMeteo& Mdata, SnowStation& Xdata, double& cumu_precip,
		                      BoundCond& Bdata, SurfaceFluxes& Sdata);

		/**
		 * @brief Perform snow preparation (grooming, etc) on a given snowpack
		 * @param Xdata snowpack to work on
		 */
		static void snowPreparation(SnowStation& Xdata);

		void setUseSoilLayers(const bool& value);
		const static double new_snow_albedo, min_ice_content;

		double getSnDt() const { return sn_dt;}

		void setSnDt(const double& snDt) { sn_dt = snDt;}

		/**
		 * @brief Specifies what kind of boundary condition is to be implemented at the top surface.
		 * Either use surface fluxes (NEUMANN_BC) or use a prescribed surface temperature (DIRICHLET_BC)
		 */
		enum BoundaryCondition {
			NEUMANN_BC,
			DIRICHLET_BC
		};

		typedef enum SOIL_EVAP_MODEL {
			EVAP_RESISTANCE,
			EVAP_RELATIVE_HUMIDITY,
			EVAP_NONE
		} soil_evap_model;

		double getParameterizedAlbedo(const SnowStation& Xdata,
		                              const CurrentMeteo& Mdata) const;
		double getModelAlbedo(const SnowStation& Xdata, CurrentMeteo& Mdata) const;

 protected:

		bool compTemperatureProfile(const CurrentMeteo& Mdata, SnowStation& Xdata,
                              BoundCond& Bdata,
                              const bool& ThrowAtNoConvergence);

		BoundaryCondition surfaceCode;

 private:
		static void EL_INCID(const int &e, int Ie[]);
		static void EL_TEMP( const int Ie[], double Te0[], double Tei[], const std::vector<NodeData> &T0, const double Ti[] );
		static void EL_RGT_ASSEM(double F[], const int Ie[], const double Fe[]);

		void compSnowCreep(const CurrentMeteo& Mdata, SnowStation& Xdata);

		bool sn_ElementKtMatrix(ElementData &Edata, double dt, const double dvdz, double T0[ N_OF_INCIDENCES ],
		                        double Se[ N_OF_INCIDENCES ][ N_OF_INCIDENCES ], double Fe[ N_OF_INCIDENCES ],
		                        const double VaporEnhance);

 		void updateBoundHeatFluxes(BoundCond& Bdata, SnowStation& Xdata, const CurrentMeteo& Mdata);

		void neumannBoundaryConditions(const CurrentMeteo& Mdata, BoundCond& Bdata, const SnowStation& Xdata,
		                               const double& T_snow, const double& T_iter,
		                               double Se[ N_OF_INCIDENCES ][ N_OF_INCIDENCES ],
		                               double Fe[ N_OF_INCIDENCES ]);

		void neumannBoundaryConditionsSoil(const double& flux, const double& T_snow,
		                                   double Se[ N_OF_INCIDENCES ][ N_OF_INCIDENCES ],
		                                   double Fe[ N_OF_INCIDENCES ]);

		void assignSomeFluxes(SnowStation& Xdata, const CurrentMeteo& Mdata, const double& mAlb,
		                      SurfaceFluxes& Sdata);

		void setHydrometeorMicrostructure(const CurrentMeteo& Mdata, const bool& is_surface_hoar, ElementData &EMS);

		void fillNewSnowElement(const CurrentMeteo& Mdata, const double& length, const double& density,
		                        const bool& is_surface_hoar, const unsigned short& number_of_solutes, ElementData &elem);

		void compTechnicalSnow(const CurrentMeteo& Mdata, SnowStation& Xdata, double& cumu_precip);

		void compSnowFall(CurrentMeteo& Mdata, SnowStation& Xdata, double& cumu_precip,
		                  SurfaceFluxes& Sdata);

                void compSnowFall_simple(CurrentMeteo& Mdata, SnowStation& Xdata);
		const SnowpackConfig& cfg;

		std::string variant, forcing, viscosity_model, watertransportmodel_snow, watertransportmodel_soil;
		std::string hn_density, hn_density_parameterization;
		std::string sw_mode, snow_albedo, albedo_parameterization, albedo_average_schmucki, sw_absorption_scheme;
		Meteo::ATM_STABILITY atm_stability_model;
		double albedo_NIED_av;
		double albedo_fixedValue, hn_density_fixedValue;
		double meteo_step_length;
		double thresh_change_bc, geo_heat, height_of_meteo_values, height_new_elem, sn_dt;
		double t_crazy_min, t_crazy_max, thresh_rh, thresh_dtempAirSnow;
		double new_snow_dd, new_snow_sp, new_snow_dd_wind, new_snow_sp_wind, rh_lowlim, bond_factor_rh;
		double new_snow_grain_size, new_snow_bond_size;
		double hoar_density_buried, hoar_density_surf, hoar_min_size_buried;
		double minimum_l_element, comb_thresh_l;
		double t_surf;
		static const double min_snow_albedo;
		bool allow_adaptive_timestepping;
		bool research_mode, useCanopyModel, enforce_measured_snow_heights, detect_grass;
		bool soil_flux, useSoilLayers;
		bool useNewPhaseChange;
		bool combine_elements;
		int reduce_n_elements;
		bool force_add_snowfall;
		double max_simulated_hs; ///< if >0: lowest elements will be removed if simulated snow depth exceeds max_simulated_hs (in m)
		bool change_bc, meas_tss;
		bool vw_dendricity;
		bool enhanced_wind_slab; ///< to use an even stronger wind slab densification than implemented by default
		std::string snow_erosion;
		bool alpine3d; ///< triggers various tricks for Alpine3D (including reducing the number of warnings)
		bool ageAlbedo; ///< use the age of snow in the albedo parametrizations? default: true
		double soot_ppmv; ///< Impurity content in ppmv for albedo calculatoins

		const static double min_allowed_sn_dt; ///< minimum allowed snowpack time step for solving the heat equation
		const static bool hydrometeor;
		const static double snowfall_warning;
		const static unsigned int new_snow_marker;
		bool adjust_height_of_meteo_values;
		bool advective_heat;
		double heat_begin, heat_end;
		double temp_index_degree_day, temp_index_swr_factor;
		bool forestfloor_alb;
		soil_evap_model soil_evaporation;
}; //end class Snowpack

#endif
