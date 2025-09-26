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
 * @file TechnicalSnow.cc
 * @brief This is the technical snow module
 */

#include <snowpack/TechnicalSnow.h>
#include <snowpack/Utils.h>

#include <meteoio/MeteoIO.h>

using namespace std;
using namespace mio;

/**
 * @brief Returns true if snow should be prepared
 * @param[in] snowPrep Should the snow preparation module be enabled?
 * @param[in] current_date current date
 * @param[in] Xdata Snow cover data
 * @return true if the snow should be prepared, false otherwise
 */
bool TechSnow::prepare(const bool& snowPrep, const mio::Date& current_date, const SnowStation& /*Xdata*/)
{
	if (!snowPrep) return false;
	const unsigned short iso_week = current_date.getISOWeekNr();
	if (iso_week>17 && iso_week<46) return false;

	int hour, minute;
	current_date.getTime(hour, minute);
	if (hour==20 && minute==30) return true;
	
	return false;
}

/**
 * @brief Compute the basic properties of newly produced technical snow (potentially mixed with natural snow)
 * @param[in] Mdata Meteorological data
 * @param[in] cumu_precip cumulated amount of precipitation (kg m-2)
 * @param[out] Tw technical snow temperature (K)
 * @param[out] rho_hn technical snow density (kg/m3)
 * @param[out] delta_cH new snow fall height (m)
 * @param[out] theta_w liquid water fraction of the new elements to be created (1)
 */
void TechSnow::productionPpt(const CurrentMeteo& Mdata, const double& cumu_precip, double &Tw, double &rho_hn, double &delta_cH, double &theta_w)
{
	static const double rho_w = 999.9; 	// (kg/m3) Density water (kg/m3) @ 1-4°C
	static const double tech_lost = 0.2;	// (-) Water loss due to wind, evaporation, etc
	static const double T_water = 1.5;		// (C) Average water temperature for the technical snow production
	static const double v_wind = 1.5;		// (m/s) Average wind condition for snow production
	static const double V_water = 100.;	// (l/min) average water supply by the snow guns
	
	Tw = (IOUtils::K_TO_C(Mdata.ta)) * atan(0.151977 * pow(Mdata.rh*100. + 8.313659, 0.5)) + atan(IOUtils::K_TO_C(Mdata.ta) + Mdata.rh*100.) - atan(Mdata.rh*100. - 1.676331) + 0.00391838 * pow(Mdata.rh*100, 1.5) * atan(0.023101 * Mdata.rh*100) - 4.686035; // (°C) Wet-bulb temperature
	rho_hn = 1.7261 * Optim::pow2(Tw) + 37.484 * Tw + 605.05; // (kg/m3) density of technical snow (kg/m3) dependent from the wet-bulb temperature
	
	double LWC_max = 29.76 - 11.71 * log(abs(Tw)) + 1.07*T_water - 1.6 * v_wind;	// (%vol) liquid water content at 55 l/min
	if (LWC_max < 0.) LWC_max = 0.;
	
	const double LWC = (0.004 * V_water + 0.52) * 100 * (rho_hn/917.) / 36.3 * LWC_max * 0.4;	// (%vol) liquid water content (average value multiply by 0.5)
	const double psum_snow_tech = Mdata.psum_tech * rho_w / rho_hn * (1. - tech_lost); 	// Technical snow production (mm) dependent from water loss and amount of provided water
	const double precip_snow = Mdata.solid_psum;// psum_snow_tech + cumu_precip * (1. -  Mdata.psum_ph);	// (mm)
	const double precip_rain = Mdata.liq_psum; //(Mdata.psum) * Mdata.psum_ph;			// (mm)
	
	delta_cH = (precip_snow / rho_hn); // Actual enforced snow depth		// (m4/kg)
	theta_w = precip_rain / (delta_cH * Constants::density_water) + LWC*0.01; // (-) volume fraction of liquid water in each element
	Tw = IOUtils::C_TO_K( Tw ); //convert Tw back to K
}

/**
 * @brief Perform technical snow preparation
 * @details The densification is done with a fit on the data found in Wolfsperger, F., H. Rhyner, and M. Schneebeli, 
 * <i>"Pistenpräparation und Pistenpflege. Das Handbuch für den Praktiker."</i>, Davos: WSL-Institut für Schnee-und Lawineforschung SLF, (2018).
 * @param Xdata Snow profile to prepare (grooming, etc)
 */
void TechSnow::preparation(SnowStation& Xdata)
{
	static const double max_grooming_density = 450.; //this is the maximum value of rho_groom (see equation below) and also a realistic achievable upper value
	static const double original_density_threshold = 415.; //this is EMS[e].Rho that produces the maximum value of rho_groom (see equation below)
	static const double max_depth = 0.4; //maximum depth of snow influenced by grooming
	const size_t nE = Xdata.getNumberOfElements();
	double depth = 0.;

	vector<NodeData>& NDS = Xdata.Ndata;
	vector<ElementData>& EMS = Xdata.Edata;
	for (size_t e=nE; e-- > Xdata.SoilNode; ) {
		const double L0 = EMS[e].L;
		depth += L0;
		
		if (EMS[e].Rho <= max_grooming_density) { //no grooming for snow that is already denser than what is achievable
			const double rho_groom = (EMS[e].Rho > original_density_threshold)? max_grooming_density : 12.152 * sqrt(448.78 - EMS[e].Rho) + 0.9963 * EMS[e].Rho - 35.41; // Density of the groomed snow, fit done on "Pistenpräparation und Pistenpflege. Das Handbuch für den Praktiker.", F Wolfsperger, H Rhyner, M Schneebeli, 2018
			const double L1 = EMS[e].L * EMS[e].Rho / rho_groom;	// New lenght of the element after grooming
			EMS[e].L0 = L1;
			EMS[e].L = L1;
			EMS[e].Rho = rho_groom;
			EMS[e].theta[WATER] *= L0 / L1;
			EMS[e].theta[WATER_PREF] *= L0 / L1;
			EMS[e].theta[ICE]   *= L0 / L1;
			EMS[e].dd = 0.;
			EMS[e].sp = 1.;
			EMS[e].rg = 0.2; // Have to adapt after some tests
			EMS[e].rb = EMS[e].rg/3.;
			NDS[e+1].z = NDS[e].z + EMS[e].L;
			EMS[e].theta[AIR] = 1.0 - EMS[e].theta[WATER] - EMS[e].theta[WATER_PREF] - EMS[e].theta[ICE] - EMS[e].theta[SOIL];
			if ( !(EMS[e].theta[AIR]>=0.1) ) {
				prn_msg(__FILE__, __LINE__, "err", Date(),
			          "Error in Slope Preparation (Densification) Volume contents: e=%d nE=%d rho=%lf ice=%lf wat=%lf wat_pref=%lf air=%le",
			            e, nE, EMS[e].Rho, EMS[e].theta[ICE], EMS[e].theta[WATER], EMS[e].theta[WATER_PREF], EMS[e].theta[AIR]);
				throw IOException("Runtime Error in snowPreparation()", AT);
			}
			Xdata.cH = NDS[nE].z + NDS[nE].u;		// Update computed snow depth
			if (depth > max_depth) break;		// Grooming has only an influence on the upper layers
		}
	}
}
