/* **********************************************************************************************/
/*                                        stand-alone                                          */
/*                               Derived from RESEARCH VERSION 9.0                             */
/* **********************************************************************************************/
/* **********************************************************************************/
/*  Copyright WSL Institute for Snow and Avalanche Research    SLF-DAVOS           */
/* **********************************************************************************/
/* This file is part of Snowpack.
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
 * @file Hazard.h
 * @version 10.02
 * This module contains the hazard computation routines and structures
*/

#ifndef HAZARD_H
#define HAZARD_H

#include <meteoio/MeteoIO.h>
#include <snowpack/DataClasses.h>
#include <vector>

/// Structure of double values for output to SDB
struct ProcessDat {
	ProcessDat() : date(), nHz(0), stat_abbrev(), loc_for_snow(0), loc_for_wind(0),
	               ch(0.), swe(0.), tot_lwc(0.), runoff(0.), dewpt_def(0.), hoar_size(0.), hoar_ind6(0.), hoar_ind24(0.),
	               wind_trans(0.), wind_trans24(0.),
	               hn_half_hour(0.), hn3(0.), hn6(0.), hn12(0.), hn24(0.), hn72(0.), hn72_24(0.),
	               psum_half_hour(0.), psum3(0.), psum6(0.), psum12(0.), psum24(0.), psum72(0.),
	               stab_class1(0), stab_class2(0),
	               stab_index1(0.), stab_height1(0.), stab_index2(0.), stab_height2(0.), stab_index3(0.), stab_height3(0.), stab_index4(0.),stab_height4(0.), stab_index5(0.), stab_height5(0.),
	               crust(0.), en_bal(0.), sw_net(0.), t_top1(0.), t_top2(0.), lwi_N(0.), lwi_S(0.),
	               dhs_corr(0.), mass_corr(0.)
	{}

	mio::Date date;        ///< Process date
	unsigned int nHz;               ///< Number of hazard steps
	std::string stat_abbrev;
	unsigned char loc_for_snow;
	unsigned char loc_for_wind;
	// Data
	double ch;             ///< height of snow HS (cm)
	double swe;            ///< snow water equivalent SWE (kg m-2)
	double tot_lwc;        ///< total liquid water content (kg m-2)
	double runoff;         ///< runoff (kg m-2)
	double dewpt_def;      ///< dew point deficit (degC)
	double hoar_size;      ///< 24 h surface hoar size (mm)
	double hoar_ind6;      ///< 6 h surface hoar index (kg m-2)
	double hoar_ind24;     ///< 24 h surface hoar index (kg m-2)
	double wind_trans;     ///< 6 h drifting snow index (cm)
	double wind_trans24;   ///< 24 h drifting snow index (cm)
	double hn_half_hour;   ///< half_hour depth of snowfall (cm)
	double hn3;            ///< 3 h depth of snowfall (cm)
	double hn6;            ///< 6 h depth of snowfall (cm)
	double hn12;           ///< 12 h depth of snowfall (cm)
	double hn24;           ///< 24 depth of snowfall (cm)
	double hn72;           ///< 72 depth of snowfall (cm)
	double hn72_24;        ///< 3 d sum of 24 h depth of snowfall (cm)
	double psum_half_hour;  ///< half_hour new snow water equivalent (kg m-2)
	double psum3;           ///< 3 h new snow water equivalent (kg m-2)
	double psum6;           ///< 6 h new snow water equivalent (kg m-2)
	double psum12;          ///< 12 h new snow water equivalent (kg m-2)
	double psum24;          ///< 24 h new snow water equivalent (kg m-2)
	double psum72;          ///< 72 h new snow water equivalent (kg m-2)
	signed char stab_class1;       ///< stability classes 1,3,5
	signed char stab_class2;       ///< profile type 0..10
	double stab_index1;    ///< deformation index Sdef
	double stab_height1;   ///< depth of stab_index1 (cm)
	double stab_index2;    ///< natural stability index Sn38
	double stab_height2;   ///< depth of stab_index2 (cm)
	double stab_index3;    ///< skier stability index Sk38
	double stab_height3;   ///< depth of stab_index3 (cm)
	double stab_index4;    ///< structural stability index SSI
	double stab_height4;   ///< depth of stab_index4 (cm)
	double stab_index5;    ///< none
	double stab_height5;   ///< depth of stab_index5 (cm)
	// Special parameters
	double crust;          ///< height of melt-freeze crust on southern slope (cm)
	double en_bal;         ///< internal energy change (kJ m-2)
	double sw_net;         ///< surface energy input (kJ m-2)
	double t_top1, t_top2; ///< snow temperatures at depth 1 & 2, respectively (degC)
	double lwi_N, lwi_S;   ///< liquid water index for northerly and southerly slopes, respectively.
	// Control parameters
	double dhs_corr;  ///< snow depth correction in case of squezzing or blow-up (cm)
	double mass_corr; ///< mass correction from either forced erosion and squeezing (neg) or blowing up (pos) (cm)
};

struct ProcessInd {
	ProcessInd() : stat_abbrev(true), loc_for_snow(true), loc_for_wind(true),
	               ch(true), swe(true), tot_lwc(true), runoff(true), dewpt_def(true),
	               hoar_size(true), hoar_ind6(true), hoar_ind24(true),
	               wind_trans(true), wind_trans24(true),
	               hn3(true), hn6(true), hn12(true), hn24(true), hn72(true), hn72_24(true), psum3(true), psum6(true), psum12(true), psum24(true), psum72(true),
	               stab_class1(true), stab_class2(true),
	               stab_index1(true), stab_height1(true), stab_index2(true), stab_height2(true), stab_index3(true), stab_height3(true), stab_index4(true), stab_height4(true), stab_index5(true), stab_height5(true),
	               crust(true), en_bal(true), sw_net(true), t_top1(true), t_top2(true), lwi_N(true), lwi_S(true)
	{}

	bool stat_abbrev;
	bool loc_for_snow;
	bool loc_for_wind;
	// Data
	bool ch;
	bool swe;
	bool tot_lwc;
	bool runoff;
	bool dewpt_def;
	bool hoar_size;
	bool hoar_ind6, hoar_ind24;
	bool wind_trans, wind_trans24;
	bool hn3, hn6, hn12, hn24, hn72;
	bool hn72_24;
	bool psum3, psum6, psum12, psum24, psum72;
	bool stab_class1, stab_class2;
	bool stab_index1, stab_height1;
	bool stab_index2, stab_height2;
	bool stab_index3, stab_height3;
	bool stab_index4, stab_height4;
	bool stab_index5, stab_height5;
	bool crust;
	bool en_bal;
	bool sw_net;
	bool t_top1, t_top2;
	bool lwi_N, lwi_S;
};

/** @brief 
 *
 * @ingroup postprocessing
 */
class Hazard {
	public:
		Hazard(const SnowpackConfig& cfg, const double duration);

		void initializeHazard(std::vector<double>& vecDrift, double slope_angle,
		                      std::vector<ProcessDat>& Hdata, std::vector<ProcessInd>& Hdata_ind);

		void getHazardDataMainStation(ProcessDat& Hdata, ProcessInd& Hdata_ind,
                                      ZwischenData& Zdata, const double& newDrift, const bool stationDriftIndex,
                                      const SnowStation& Xdata, const CurrentMeteo& Mdata, const SurfaceFluxes& Sdata);

		void getHazardDataSlope(ProcessDat& Hdata, ProcessInd& Hdata_ind,
		                        std::vector<double>& drift24, const double& newDrift, const SnowStation& Xdata,
		                        const bool luvDriftIndex, const bool north, const bool south);

		static const double typical_slope_length, wind_slab_density;
		static const double minimum_drift, maximum_drift;

	private:
		enum ActVec {
			noAction=0,     // neither shift nor overwite index values
			overwrite,      // overwrite <index>[0] w/o shifting
			pushOverwrite   // push vector values and overwrite <index>[0]
		};

		void actOnVector(std::vector<double>& oldVector, const double& newValue, const ActVec& action);

		double compDriftIndex(std::vector<double>& vecDrift, const double& drift, const double& rho,
		                      const unsigned int& nHours, const double& slope_angle, const ActVec& action);

		void getDriftIndex(ProcessDat& Hdata, ProcessInd& Hdata_ind,
		                   std::vector<double>& vecDrift, const double& newDriftValue, const double slope_angle);

		double compHoarIndex(std::vector<double> &oldHoar, const double& newHoar,
                             const unsigned int& nHours, const ActVec& action);

		double compDewPointDeficit(double TA, double TSS, double RH);

		void compMeltFreezeCrust(const SnowStation& Xdata, ProcessDat& Hdata, ProcessInd& Hdata_ind);

		bool research_mode, enforce_measured_snow_heights, force_rh_water;
		unsigned int nHz, hazard_steps_between;
		double sn_dt;
		double hoar_density_surf, hoar_min_size_surf;
};

#endif
