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
 * @file Hazard.cc
 * @version 10.02
 * This module contains the hazard computation routines
 */
#include <stdio.h>

#include <snowpack/Hazard.h>
#include <snowpack/Stability.h>
#include <snowpack/Utils.h>

using namespace mio;
using namespace std;

/************************************************************
 * static section                                           *
 ************************************************************/

//Lee slope length (m) used to convert mass flux to drift index deposition depth (cm/24h)
const double Hazard::typical_slope_length = 70.0;

//Predefined snow density (kg m-3) used to convert mass flux to drift index deposition depth (cm/24h)
const double Hazard::wind_slab_density = 77.0;

//At least that mass flux (kg m-1 h-1) must have summed up for the drift index to be larger than 0.
const double Hazard::minimum_drift = 0.0;

//Largest additional accumulation allowed (cm h-1)
const double Hazard::maximum_drift = 5.0;

/************************************************************
 * non-static section                                       *
 ************************************************************/

Hazard::Hazard(const SnowpackConfig& cfg, const double duration)
        : research_mode(false), enforce_measured_snow_heights(false), force_rh_water(false),
        nHz(0), hazard_steps_between(0), sn_dt(IOUtils::nodata),
        hoar_density_surf(IOUtils::nodata), hoar_min_size_surf(IOUtils::nodata)

{
	/**
	 * @brief Defines how the height of snow is going to be handled
	 * - false: Depth of snowfall is determined from the water equivalent of snowfall (PSUM)
	 * - true: The measured height of snow is used to determine whether new snow has been deposited.
	 *      This setting MUST be chosen in operational mode. \n
	 *      This procedure has the disadvantage that if the snowpack settles too strongly
	 *      extra mass is added to the snowpack. \n
	 * New snow density is needed in both cases, either parameterized, measured, or fixed.
	 */
	cfg.getValue("ENFORCE_MEASURED_SNOW_HEIGHTS", "Snowpack", enforce_measured_snow_heights);
	//Calculation time step in seconds as derived from CALCULATION_STEP_LENGTH
	const double calculation_step_length = cfg.get("CALCULATION_STEP_LENGTH", "Snowpack");
	sn_dt = M_TO_S(calculation_step_length);
	/* Dew point relative to water or ice
	 * - default: true
	 * - Antarctica: false */
	cfg.getValue("FORCE_RH_WATER", "SnowpackAdvanced", force_rh_water);
	cfg.getValue("RESEARCH", "SnowpackAdvanced", research_mode);
	//Density of surface hoar (-> hoar index of surface node) (kg m-3)
	cfg.getValue("HOAR_DENSITY_SURF", "SnowpackAdvanced", hoar_density_surf);
	//Minimum size to show surface hoar on surface (mm)
	cfg.getValue("HOAR_MIN_SIZE_SURF", "SnowpackAdvanced", hoar_min_size_surf);

	/*
	* Hazard data interval in units of CALCULATION_STEP_LENGTH
	* WARNING: In operational mode, this has to result in a 30 min interval!
	* It is a matter of consitency. If you change this, a big mess will result!!!
	*/
	cfg.getValue("HAZARD_STEPS_BETWEEN", "Output", hazard_steps_between);
	if (duration<=0.) throw InvalidArgumentException("Hazard duration must be >0", AT);
	nHz = static_cast<unsigned int>( floor( (duration / (static_cast<double>(hazard_steps_between) * sn_dt)) ) + 2 );
	if (nHz == 0) nHz = 1;
}

/**
 * @brief Acts on values of a vector
 * @version 13.12
 * @param oldVector vector of previous N values
 * @param newValue to insert as zeroth value
 * @param action to be performed on index vector (see enum ActVec)
 */
void Hazard::actOnVector(std::vector<double>& oldVector, const double& newValue, const ActVec& action)
{
	switch (action) {
		case pushOverwrite: // If required, shift hoar index values, then ...
			for(size_t ii=oldVector.size()-1; ii>0; ii--) {
				oldVector[ii] = oldVector[ii-1];
			}
			oldVector[0] = newValue;
			break;
		case overwrite: // ... overwrite oldVector[0]
			oldVector[0] = newValue;
			break;
		case noAction:
			break;
    default:
      InvalidArgumentException("Unknown action provided to actOnVector", AT);
	}
}

/**
 * @brief Allocates and initializes Hazard data
 * - Computes a zeroth order drift index for the first time step w/o shifting old_drift!
 * @param *old_drift
 * @param slope_angle (degree)
 * @param Hdata
 * @param Hdata_ind
 */
void Hazard::initializeHazard(std::vector<double>& old_drift, double slope_angle,
                              std::vector<ProcessDat>& Hdata, std::vector<ProcessInd>& Hdata_ind)
{
	Hdata.resize(nHz, ProcessDat());
	Hdata_ind.resize(nHz, ProcessInd());

	Hdata[0].nHz = nHz;
	Hdata[nHz-1].nHz = nHz;

	Hdata[0].wind_trans = compDriftIndex(old_drift, Constants::undefined, Hazard::wind_slab_density, 6, slope_angle, noAction);
	Hdata[0].wind_trans24 = compDriftIndex(old_drift, Constants::undefined, Hazard::wind_slab_density, 24, slope_angle, noAction);
}

/**
 * @brief Computes drift index for past nHours in cm
 * @note At least min_percent_values of vecDrift need to be defined to obtain a valid drift index
 * @version 13.12
 * @param vecDrift vector of previous 48 half-hourly eroded masses (kg m-1)
 * @param newDrift last eroded mass (kg m-1)
 * @param rho snow density (kg m-3)
 * @param nHours (1)
 * @param slope_angle (deg)
 * @param action to be performed on index vector (see enum ActVec)
 */
double Hazard::compDriftIndex(std::vector<double>& vecDrift, const double& newDrift, const double& rho,
                              const unsigned int& nHours, const double& slope_angle, const ActVec& action)
{
	actOnVector(vecDrift, newDrift, action);

	unsigned int nValues=0;
	double sumVec = 0.;
	for (size_t ii = 0; ii < 2*nHours; ii++ ) {
		if (vecDrift[ii] == Constants::undefined) {
			continue;
		} else {
			sumVec += vecDrift[ii];
			nValues++;
		}
	}
	if (nValues <= (unsigned int)(floor(Constants::min_percent_values * 2. * nHours))) {
		return Constants::undefined;
	} else {
		const double flux = H_TO_S(std::max(0.,(sumVec - Hazard::minimum_drift)) / (2. * nHours)); // kg m-1 h-1
		double ero_depo = M_TO_CM(flux * nHours / (Hazard::typical_slope_length * rho));
		ero_depo = std::min(ero_depo, nHours * Hazard::maximum_drift * cos(slope_angle*mio::Cst::to_rad));
		ero_depo /= cos(slope_angle*mio::Cst::to_rad);
		return ero_depo;
	}
}

void Hazard::getDriftIndex(ProcessDat& Hdata, ProcessInd& Hdata_ind,
                           std::vector<double>& vecDrift, const double& newDriftValue, const double slope_angle)
{
	Hdata_ind.wind_trans = true;
	Hdata_ind.wind_trans24 = true;

	Hdata.wind_trans = compDriftIndex(vecDrift, newDriftValue, Hazard::wind_slab_density, 6, slope_angle, pushOverwrite);
	Hdata.wind_trans24 = compDriftIndex(vecDrift, Constants::undefined, Hazard::wind_slab_density, 24, slope_angle, noAction);

	if (Hdata.wind_trans < 0.) Hdata_ind.wind_trans = false;
	if (Hdata.wind_trans24 < 0.) Hdata_ind.wind_trans24 = false;
}

/**
 * @brief Determines hoar (mass) index for past nHours in kg m-2
 * @note At least min_percent_values of oldHoar need to be defined to obtain a valid hoar index
 * @version 12.13
 * @param OldHoar vector of previous 48 half-hourly hoar masses (kg m-2)
 * @param newHoar last deposited or sublimated mass of ice (hoar; kg m-2)
 * @param nHours (1)
 * @param action to be performed on index vector (see enum ActVec)
 */
double Hazard::compHoarIndex(std::vector<double> &oldHoar, const double& newHoar,
                             const unsigned int& nHours, const ActVec& action)
{
	actOnVector(oldHoar, newHoar, action);

	// Determine hoar_ind
	unsigned int nValues = 0;
	double hoar_ind = 0.;
	for (size_t ii = 0; ii < 2*nHours; ii++ ) {
		if (oldHoar[ii] == Constants::undefined){
			continue;
		} else {
			hoar_ind += oldHoar[ii];
			nValues++;
		}
	}
	if (nValues <= (unsigned int)(floor(Constants::min_percent_values * 2 * nHours)))
		return Constants::undefined;
	else
		return(hoar_ind);
}

/**
 * @brief Compute the dew point deficit in degC
 * @param TA  Air temperature in kelvins
 * @param TSS Snow surface temperature in kelvins
 * @param RH  Relative air humidity (over water) in (percents or 1? TODO)
 */
double Hazard::compDewPointDeficit(double TA, double TSS, double RH)
{ //HACK: use Atmosphere::RhtoDewPoint instead
	const double b=9.5, c=265.5;

	TA = IOUtils::K_TO_C(TA);
	TSS = IOUtils::K_TO_C(TSS);
	const double log10RH = log10(RH);
	const double Tdew = c * (log10RH + b*TA/(c+TA)) / (b - log10RH - b*TA/(c+TA));

	const double deficit = TSS - Tdew;
	return deficit;
}

void Hazard::compMeltFreezeCrust(const SnowStation& Xdata, ProcessDat& Hdata, ProcessInd& Hdata_ind)
{
	double crust_height=0.;
	const double cos_sl = cos(Xdata.meta.getSlopeAngle()*mio::Cst::to_rad);

	if (Xdata.getNumberOfElements() > 0) {
		size_t e = Xdata.getNumberOfElements()-1;
		double crust_dep = 0.;
		while ((e > Xdata.SoilNode) && (crust_dep <= 0.03)) {
			if ((Xdata.Edata[e].type == 772) || (Xdata.Edata[e].type == 880)) {
				crust_height += Xdata.Edata[e].L/cos_sl;
				if ((Xdata.Edata[e-1].type != 772) && (Xdata.Edata[e-1].type != 880)) {
					break;
				}
			} else {
				crust_dep += Xdata.Edata[e].L/cos_sl;
			}
			e--;
		}
	}
	if ( (crust_height >= 0.) && (crust_height <= Xdata.cH/cos_sl) ) {
		Hdata.crust = M_TO_CM(crust_height);
	} else {
		Hdata_ind.crust = false;
	}
}

/**
 * @brief Compute the Hdata from main station data
 * - depths of snowfall hn({0.5, 3., 6., 12., 24., 72.}h) including water equivalents (psum)
 * - 3 days sum of 24h depths of snowfall
 * - surface hoar size and hoar index for 6 and 24 hours
 * - dewpoint deficit, SWE and total liquid water content, runoff,
 *     Profile type, Stability classes and indices, energy balance, and snow temperatures
 * @note If there are no virtual slopes available, drifting snow index
 *         and thickness of surface crust may be computed from the main station if SNOW_EROSION is set
 * @param Hdata
 * @param Hdata_ind
 * @param Zdata
 * @param newDrift
 * @param stationDriftIndex needs to be computed
 * @param Xdata
 * @param Mdata
 * @param Sdata
 */
void Hazard::getHazardDataMainStation(ProcessDat& Hdata, ProcessInd& Hdata_ind,
                                      ZwischenData& Zdata, const double& newDrift, const bool stationDriftIndex,
                                      const SnowStation& Xdata, const CurrentMeteo& Mdata, const SurfaceFluxes& Sdata)
{
	const size_t nE = Xdata.getNumberOfElements();
	const double cos_sl = cos(Xdata.meta.getSlopeAngle()*mio::Cst::to_rad);
	const double hs = Xdata.cH / cos_sl;

	const ElementData *EMS;  // Pointer to element data
	EMS = &Xdata.Edata[0];

	// Initialization
	Hdata.date = Mdata.date;

	Hdata.dewpt_def = 21.7;    Hdata_ind.dewpt_def  = true;
	Hdata.hoar_ind6 = 21.7;    Hdata_ind.hoar_ind6 = true;
	Hdata.hoar_ind24 = 21.7;   Hdata_ind.hoar_ind24 = true;
	Hdata.hoar_size = 21.7;    Hdata_ind.hoar_ind24 = true;

	Hdata.hn3 = 21.7;          Hdata_ind.hn3 = true;
	Hdata.hn6 = 21.7;          Hdata_ind.hn6 = true;
	Hdata.hn12 = 21.7;         Hdata_ind.hn12 = true;
	Hdata.hn24 = 21.7;         Hdata_ind.hn24 = true;
	Hdata.hn72 = 21.7;         Hdata_ind.hn72 = true;
	Hdata.hn72_24 = 21.7;      Hdata_ind.hn72_24 = true;
	Hdata.psum3  = 21.7;        Hdata_ind.psum3  = true;
	Hdata.psum6  = 21.7;        Hdata_ind.psum6  = true;
	Hdata.psum12 = 21.7;        Hdata_ind.psum12 = true;
	Hdata.psum24 = 21.7;        Hdata_ind.psum24 = true;
	Hdata.psum72 = 21.7;        Hdata_ind.psum72 = true;

	Hdata.stab_class1 = 0;     Hdata_ind.stab_class1 = true;
	Hdata.stab_class2 = 5;     Hdata_ind.stab_class2 = true;

	Hdata.stab_index1 = Stability::max_stability;    Hdata_ind.stab_index1 = true;
	Hdata.stab_index2 = Stability::max_stability;    Hdata_ind.stab_index2 = true;
	Hdata.stab_index3 = Stability::max_stability;    Hdata_ind.stab_index3 = true;
	Hdata.stab_index4 = Stability::max_stability;    Hdata_ind.stab_index4 = true;
	Hdata.stab_index5 = Stability::max_stability;    Hdata_ind.stab_index5 = true;
	Hdata.stab_height1 = hs;   Hdata_ind.stab_height1 = true;
	Hdata.stab_height2 = hs;   Hdata_ind.stab_height2 = true;
	Hdata.stab_height3 = hs;   Hdata_ind.stab_height3 = true;
	Hdata.stab_height4 = hs;   Hdata_ind.stab_height4 = true;
	Hdata.stab_height5 = hs;   Hdata_ind.stab_height5 = true;

	Hdata.ch = M_TO_CM(hs);    Hdata_ind.ch = true;

	Hdata.swe     = 0.;        Hdata_ind.swe     = true;
	Hdata.tot_lwc = 0.;        Hdata_ind.tot_lwc = true;
	Hdata.runoff  = 0.;        Hdata_ind.runoff  = true;

	Hdata.crust  =  0.0;       Hdata_ind.crust  = true;
	Hdata.en_bal = 21.7;       Hdata_ind.en_bal = true;
	Hdata.sw_net = 21.7;       Hdata_ind.sw_net = true;
	Hdata.t_top1 = 21.7;       Hdata_ind.t_top1 = true;
	Hdata.t_top2 = 21.7;       Hdata_ind.t_top2 = true;

	// Compute depths of snowfall for given time intervals
	double t_hn[6] ={0.5, 3., 6., 12., 24., 72.}, hn[6], precip[6];
	double sum_hn = 0., sum_precip = 0.;
	int e = (signed)nE-1;
	for (unsigned int kk = 0; kk <= 5; kk++) {
		while ((e >= signed(Xdata.SoilNode)) && ((Mdata.date.getJulian() - EMS[e].depositionDate.getJulian()) < (H_TO_D(t_hn[kk])))) {
			sum_hn += EMS[e].L;
			sum_precip += EMS[e].L * EMS[e].Rho;
			e--;
		}
		hn[kk] = sum_hn;
		precip[kk] = sum_precip;
	}
	Hdata.hn_half_hour = M_TO_CM(hn[0] / cos_sl);
	Hdata.hn3 =  M_TO_CM(hn[1] / cos_sl);
	Hdata.hn6 =  M_TO_CM(hn[2] / cos_sl);
	Hdata.hn12 =  M_TO_CM(hn[3] / cos_sl);
	Hdata.hn24 =  M_TO_CM(hn[4] / cos_sl);
	Hdata.hn72 =  M_TO_CM(hn[5] / cos_sl);
	Hdata.psum_half_hour = precip[0] / cos_sl;
	Hdata.psum3 =  precip[1] / cos_sl;
	Hdata.psum6 =  precip[2] / cos_sl;
	Hdata.psum12 =  precip[3] / cos_sl;
	Hdata.psum24 =  precip[4] / cos_sl;
	Hdata.psum72 =  precip[5] / cos_sl;

	// Compute 3 days sum of 24h depths of snowfall
	actOnVector(Zdata.hn24, hn[4], pushOverwrite);
	Hdata.hn72_24 =  M_TO_CM((Zdata.hn24[0] + Zdata.hn24[48] + Zdata.hn24[96]) / cos_sl);

	// surface hoar size, size in mm assuming HOAR_DENSITY_SURF at surface
	Hdata.hoar_size = M_TO_MM(Xdata.Ndata[nE].hoar / hoar_density_surf);
	// Check for lower size limit
	if (Hdata.hoar_size <= hoar_min_size_surf)
		Hdata.hoar_size = 0.;
	if (!((Hdata.hoar_size >= 0.) && (Hdata.hoar_size < 100.)))
		Hdata_ind.hoar_size = false;
	// HOAR INDEX (6h and 24h), mass in kg m-2
	Hdata.hoar_ind6  = compHoarIndex(Zdata.hoar24, Sdata.hoar, 6, pushOverwrite);
	if (!((Hdata.hoar_ind6 > -10.) && (Hdata.hoar_ind6 < 10.)))
		Hdata_ind.hoar_ind6 = false;
	Hdata.hoar_ind24 = compHoarIndex(Zdata.hoar24, Sdata.hoar, 24, noAction);
	if (!((Hdata.hoar_ind24 > -10.) && (Hdata.hoar_ind24 < 10.)))
		Hdata_ind.hoar_ind24 = false;

	// Instantaneous dewpoint deficit between TSS and Td(air)
	if (research_mode) {
		Hdata.dewpt_def = Xdata.Ndata[Xdata.getNumberOfNodes()-1].T
		- Atmosphere::RhtoDewPoint(Mdata.rh, Mdata.ta, force_rh_water);
	} else {
		Hdata.dewpt_def = compDewPointDeficit(Mdata.ta, Xdata.Ndata[Xdata.getNumberOfNodes()-1].T, Mdata.rh);
	}

	if (!((Hdata.dewpt_def > -50.) && (Hdata.dewpt_def < 50.))) {
		Hdata_ind.dewpt_def = false;
	}

	// SWE and total liquid water content
	Hdata.swe = Sdata.mass[SurfaceFluxes::MS_SWE];
	Hdata.tot_lwc = Sdata.mass[SurfaceFluxes::MS_WATER];
	// Runoff rate (kg m-2 h-1)
	Hdata.runoff /= S_TO_H(sn_dt * static_cast<double>(hazard_steps_between));

	// Profile type
	if ((Xdata.S_class1 <= 10) && (Xdata.S_class1 >= 0))
		Hdata.stab_class1 = Xdata.S_class1;
	else
		Hdata_ind.stab_class1 = false;
	// Stability class
	if ((Xdata.S_class2 <= 5) && (Xdata.S_class2 >= 1))
		Hdata.stab_class2 = Xdata.S_class2;
	else
		Hdata_ind.stab_class2 = false;
	// 1: Stability index: Deformation index
	if ((Xdata.S_d < (Stability::max_stability + Constants::eps)) && (Xdata.S_d > 0.))
		Hdata.stab_index1 = Xdata.S_d;
	else
		Hdata_ind.stab_index1 = false;
	if ((Xdata.z_S_d < (hs + Constants::eps)) && (Xdata.z_S_d > 0.))
		Hdata.stab_height1 = M_TO_CM(Xdata.z_S_d / cos_sl);
	else
		Hdata_ind.stab_height1 = false;
	// 2: Natural stability index Sn38
	if ((Xdata.S_n < (Stability::max_stability + Constants::eps)) && (Xdata.S_n > 0.))
		Hdata.stab_index2 = Xdata.S_n;
	else
		Hdata_ind.stab_index2 = false;
	if ((Xdata.z_S_n < (hs + Constants::eps)) && (Xdata.z_S_n > 0.))
		Hdata.stab_height2 = M_TO_CM(Xdata.z_S_n / cos_sl);
	else
		Hdata_ind.stab_height2 = false;
	// 3: Skier stability index Sk38
	if ((Xdata.S_s < (Stability::max_stability + Constants::eps)) && (Xdata.S_s > 0.))
		Hdata.stab_index3 = Xdata.S_s;
	else
		Hdata_ind.stab_index3 = false;
	if ((Xdata.z_S_s < (hs + Constants::eps)) && (Xdata.z_S_s > 0.))
		Hdata.stab_height3 = M_TO_CM(Xdata.z_S_s / cos_sl);
	else
		Hdata_ind.stab_height3 = false;
	// 4: Structural stability index SSI
	if ((Xdata.S_4 < (Stability::max_stability + Constants::eps)) && (Xdata.S_4 > 0.))
		Hdata.stab_index4 = Xdata.S_4;
	else
		Hdata_ind.stab_index4 = false;
	if ((Xdata.z_S_4 < (hs + Constants::eps)) && (Xdata.z_S_4 > 0.))
		Hdata.stab_height4 = M_TO_CM(Xdata.z_S_4 / cos_sl);
	else
		Hdata_ind.stab_height4 = false;
	// 5: ???Index???
	if ((Xdata.S_5 < (Stability::max_stability + Constants::eps)) && (Xdata.S_5 > 0.))
		Hdata.stab_index5 = Xdata.S_5;
	else
		Hdata_ind.stab_index5 = false;
	if ((Xdata.z_S_5 < (hs + Constants::eps)) && (Xdata.z_S_5 > 0.))
		Hdata.stab_height5 = M_TO_CM(Xdata.z_S_5 / cos_sl);
	else
		Hdata_ind.stab_height5 = false;

	// Surface crust [type == 772] computed for southerly aspect outside compHazard()

	// Energy input ... (kJ m-2)
	if (nE > Xdata.SoilNode)
		Hdata.en_bal = ((Xdata.dIntEnergy - Sdata.qg0 * sn_dt)
		                    * hazard_steps_between) / 1000.;
	else
		Hdata.en_bal = ((Sdata.qw + Sdata.lw_net + Sdata.qs + Sdata.ql + Sdata.qr) * sn_dt
		                    * hazard_steps_between) / 1000.;
	if (!((Hdata.en_bal > -3000.) && (Hdata.en_bal < 3000.)))
		Hdata_ind.en_bal = false;

	// Net SW energy at surface (kJ m-2)
	if (Sdata.sw_in > 0.) {
		Hdata.sw_net = (Sdata.qw * sn_dt * hazard_steps_between) / 1000.;
		if (!((Hdata.sw_net > -3000.) && (Hdata.sw_net < 3000.)))
			Hdata_ind.sw_net = false;
	} else {
		Hdata.sw_net = 0.;
	}

	// Snow temperatures t_top1 and t_top2 in degC at 5 cm and 10 cm below the surface, respectively
	double h_top1 = hs - 0.05;
	Hdata.t_top1 = Xdata.getModelledTemperature(h_top1);
	if ( !((Hdata.t_top1 > -50.) && (Hdata.t_top1 <= 0.)) )
		Hdata_ind.t_top1 = false;
	double h_top2 = hs - 0.10;
	Hdata.t_top2 = Xdata.getModelledTemperature(h_top2);
	if (!((Hdata.t_top2 > -50.) && (Hdata.t_top2 <= 0.)))
		Hdata_ind.t_top2 = false;

	if (stationDriftIndex)
		getDriftIndex(Hdata, Hdata_ind, Zdata.drift24, newDrift, Xdata.cos_sl);
}

/**
 * @brief Compute Hdata from virtual slope data
 * - drifting snow index for last 6 and 24 h
 * - liquid water index for north and south slope
 * - vertical thickness of melt-freeze crust, not buried deeper than 3 cm
 * @note these values are only available for virtual slopes
 * @param Hdata
 * @param Hdata_ind
 * @param drift24 vector of 48 previous driting snow values
 * @param newDrift value
 * @param Xdata
 * @param luvDriftIndex needs to be computed
 * @param north is slope aspect
 * @param south is slope aspect
 */
void Hazard::getHazardDataSlope(ProcessDat& Hdata, ProcessInd& Hdata_ind,
		                        std::vector<double>& drift24, const double& newDrift, const SnowStation& Xdata,
		                        const bool luvDriftIndex, const bool north, const bool south)
{
	if (luvDriftIndex)
		getDriftIndex(Hdata, Hdata_ind, drift24, newDrift, Xdata.cos_sl);
	if (north) {
		Hdata.lwi_N = Xdata.getLiquidWaterIndex();
		if ((Hdata.lwi_N < -Constants::eps) || (Hdata.lwi_N >= 10.))
			Hdata_ind.lwi_N = false;
	}
	if (south) {
		Hdata.lwi_S = Xdata.getLiquidWaterIndex();
		if ((Hdata.lwi_S < -Constants::eps) || (Hdata.lwi_S >= 10.))
			Hdata_ind.lwi_S = false;
		compMeltFreezeCrust(Xdata, Hdata, Hdata_ind);
	}
}
