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
 * @file Meteo.cc
 * @author Michael Lehning and others
 * @brief Computes missing meteorological information such as friction velocity and roughness length
 * - 29.10.2002: Michael Lehning implements Micromet()
 * - 15.03.2005: Andy and Michi implement stability correction for turbulent fluxes in the hope
 *               that this will also improve the little bit too strong melting of the version 8.1
 */
#include <meteoio/MeteoIO.h>
using namespace mio;

#include <snowpack/Meteo.h>
#include <snowpack/Constants.h>
#include <snowpack/Laws_sn.h>
#include <snowpack/Utils.h>

/************************************************************
* non-static section                                       *
************************************************************/

Meteo::Meteo(const SnowpackConfig& cfg)
       : canopy(cfg), roughness_length(0.), height_of_wind_value(0.), adjust_height_of_wind_value(true), stability(MO_MICHLMAYR),
         research_mode(false), useCanopyModel(false)
{
	const std::string stability_model = cfg.get("ATMOSPHERIC_STABILITY", "Snowpack");
	stability = getStability(stability_model);
	
	//Initial estimate of the roughness length for the site; will be adjusted iteratively, default value and operational mode: 0.002 m
	cfg.getValue("ROUGHNESS_LENGTH", "Snowpack", roughness_length);

	//Defines whether the canopy model is used. OUT_CANOPY must also be set to dump canopy parameters to file; see Constants_local.h
	cfg.getValue("CANOPY", "Snowpack", useCanopyModel);

	//Define the heights of the meteo measurements above ground (m). Required for surface energy exchange computation and for drifting and blowing snow.
	cfg.getValue("HEIGHT_OF_WIND_VALUE", "Snowpack", height_of_wind_value);
	cfg.getValue("ADJUST_HEIGHT_OF_WIND_VALUE", "SnowpackAdvanced", adjust_height_of_wind_value);

	cfg.getValue("RESEARCH", "SnowpackAdvanced", research_mode);
}

/**
 * @brief Parse the given string an return the matching atmospheric stability algorithm
 * @param[in] stability_model atmospheric stability model specification
 * @return stability model
 */
Meteo::ATM_STABILITY Meteo::getStability(const std::string& stability_model)
{
	if (stability_model=="RICHARDSON")
		return RICHARDSON; //Simplified Richardson number stability correction
	else if (stability_model=="NEUTRAL")
		return NEUTRAL; //Assume neutral stratification. Should be used with BC_CHANGE=1, i.e., Dirichlet bc but also recommended with Neumann b.c., i.e., BC_CHANGE=0
	else if (stability_model=="MO_MICHLMAYR")
		return MO_MICHLMAYR; //Standard MO iteration with Paulson and Stearns & Weidner (can be used with BC_CHANGE=0)
	else if (stability_model=="MO_STEARNS")
		return MO_STEARNS;
	else if (stability_model=="MO_HOLTSLAG")
		return MO_HOLTSLAG; //should be much better during melt periods than MICHLMAYR_MO
	else if (stability_model=="MO_LOG_LINEAR")
		return MO_LOG_LINEAR;
	else if (stability_model=="MO_SCHLOEGL_UNI")
		return MO_SCHLOEGL_UNI;
	else if (stability_model=="MO_SCHLOEGL_MULTI")
		return MO_SCHLOEGL_MULTI;
	else if (stability_model=="MO_SCHLOEGL_MULTI_OFFSET")
		return MO_SCHLOEGL_MULTI_OFFSET;
	else if (stability_model=="MONIN_OBUKHOV") //HACK: temporary
		throw InvalidArgumentException("Atmospheric stability model \""+stability_model+"\" is now called 'MO_MICHLMAYR'", AT);
	else if (stability_model=="NEUTRAL_MO") //HACK: temporary
		throw InvalidArgumentException("Atmospheric stability model \""+stability_model+"\" is now called 'NEUTRAL'", AT);
	else
		throw InvalidArgumentException("Atmospheric stability model \""+stability_model+"\" is not supported!", AT);
}

/**
 * @brief set the atmosphere stability to a given value
 * @param i_stability stability model (see possible values in constructor)
 */
void Meteo::setStability(const Meteo::ATM_STABILITY& i_stability)
{
	stability = i_stability;
}

/**
 * @brief get the atmosphere stability
 * @return stability model (see possible values in constructor)
 */
Meteo::ATM_STABILITY Meteo::getStability() const
{
	return stability;
}

/**
 * @brief Projects precipitations and snow height perpendicular to slope
 * @param hs Height of snow (m)
 * @param precips precipitations (kg m-2)
 * @param slope_angle (deg)
 */
void Meteo::projectPrecipitations(const double& slope_angle, double& precips, double& hs)
{
	const double cos_sl = cos(slope_angle*mio::Cst::to_rad);
	precips *= cos_sl;
	hs *= cos_sl;
}

/**
 * @brief Applies the logarithmic wind profile to adjust provided wind speed to another height above surface.
 * Wind pumping is ignored. NOTE THAT THE FUNCTION IS DEACTIVATED AND WILL RETURN WIND SPEED FROM INPUT!!!
 * @param Mdata
 * @param target_z target height above surface (m) to translate the windspeed to.
 * @param source_vw (optional) Wind speed to use to scale ustar(Mdata.vw). If omitted, Mdata.vw is taken.
 * @return Wind speed (m/s) at target_z.
 */
double Meteo::windspeedProfile(const CurrentMeteo& Mdata, const double& target_z, const double& source_vw) {
	const double d_pump = 0.;
	const double z_ratio = log((target_z - d_pump) / Mdata.z0);
	const double ustar_corr = (source_vw < 0. || Mdata.vw < 0.) ? (Mdata.ustar) : (Mdata.ustar * (source_vw / Mdata.vw));
	const double vw_corr = (ustar_corr / Constants::karman) * (z_ratio - Mdata.psi_m);
	return (source_vw >= 0.) ? (source_vw) : (Mdata.vw);	// THIS LINE RETURNS WIND SPEED FROM INPUT, AND DEACTIVATES THE WORKING OF THIS FUNCTION!!!
	return vw_corr;						// TODO: THIS LINE NEEDS TO BE ACTIVATED IN THE FUTURE.
}

void Meteo::RichardsonStability(const double& ta_v, const double& t_surf_v, const double& zref, const double& vw, const double& z_ratio, double &ustar, double &psi_s)
{
	const double Ri = Constants::g / t_surf_v * (ta_v - t_surf_v) * zref / Optim::pow2(vw);
	double psi_m;

	if (Ri < 0.) { // unstable
		const double stab_ratio = Ri;
		const double dummy = pow((1. - 15. * stab_ratio), 0.25);
		psi_m = log((0.5 * (1. + dummy*dummy)) * (0.5 * (1. + dummy)) * (0.5 * (1. + dummy)))
				- 2. * atan(dummy) + 0.5 * Constants::pi;
		psi_s = 2. * log(0.5 * (1. + dummy*dummy));
	} else if (Ri < 0.1999) { // stable
		const double stab_ratio = Ri / (1. - 5. * Ri);
		psi_m = psi_s = -5. * stab_ratio;
	} else {
		const double stab_ratio = Ri / (1. - 5. * 0.1999);
		psi_m = psi_s = -5. * stab_ratio;
	}

	ustar = Constants::karman * vw / (z_ratio - psi_m);
}

void Meteo::MOStability(const ATM_STABILITY& use_stability, const double& ta_v, const double& t_surf_v, const double& t_surf, const double& zref, const double& vw, const double& z_ratio, double &ustar, double &psi_s, double &psi_m)
{
	if (use_stability==NEUTRAL) { //prevent recomputing ustar, for consistency
		psi_m = psi_s = 0.;
		return;
	}
	
	ustar = Constants::karman * vw / (z_ratio - psi_m);
	const double Tstar = Constants::karman * (t_surf_v - ta_v) / (z_ratio - psi_s);
	const double stab_ratio = -Constants::karman * zref * Tstar * Constants::g / (t_surf * Optim::pow2(ustar));
	
	if (stab_ratio > 0.) { // stable
		switch(use_stability) {
			case MO_HOLTSLAG: {
			// Holtslag and DeBruin (1988) prepared from Ed Andreas
			psi_m = psi_s = -(0.7 * stab_ratio + 0.75 * (stab_ratio - 14.28)
			                           * exp(-0.35 * stab_ratio) + 10.71);
			return;
			}
		
			case MO_STEARNS: {
			// Stearns & Weidner, 1993
			const double dummy1 = pow((1. + 5. * stab_ratio), 0.25);
			psi_m = log(1. + dummy1) * log(1. + dummy1) + log(1. + Optim::pow2(dummy1))
					- 2. * atan(dummy1) - 1.3333;
			const double dummy2 = Optim::pow2(dummy1);
			psi_s = log(1. + dummy2) * log(1. + dummy2)
					- 2. * dummy2 - 0.66667 * Optim::pow3(dummy2) + 1.2804;
			return;
			}
		
			case MO_MICHLMAYR: { //default, old MO
			// Stearns & Weidner, 1993 modified by Michlmayr, 2008
			const double dummy1 = pow((1. + 5. * stab_ratio), 0.25);
			psi_m = log(1. + dummy1) * log(1. + dummy1) + log(1. + Optim::pow2(dummy1))
					- 1. * atan(dummy1) - 0.5 * Optim::pow3(dummy1) + 0.8247;
			const double dummy2 = Optim::pow2(dummy1);
			psi_s = log(1. + dummy2) * log(1. + dummy2)
					- 1. * dummy2 - 0.3 * Optim::pow3(dummy2) + 1.2804;
			return;
			}
		
			case MO_LOG_LINEAR: {
			//log_linear
			psi_m = psi_s = -5.* stab_ratio;
			return;
			}
		
			case MO_SCHLOEGL_UNI: {
			//schloegl univariate: bin univariate 2/3 datasets
			psi_m = -1.62 * stab_ratio;
			psi_s = -2.96 * stab_ratio;
			return;
			}
			
			case MO_SCHLOEGL_MULTI: {
			//All multivariate 2/3 without offset
			psi_m = - 65.35 *(ta_v - t_surf_v)/(0.5 * (ta_v + t_surf_v)) + 0.0017 * zref * Constants::g/pow(vw,2);
			psi_s = - 813.21 *(ta_v - t_surf_v)/(0.5 *(ta_v + t_surf_v)) - 0.0014 * zref * Constants::g/pow(vw,2);
			return;
			}
			
			case MO_SCHLOEGL_MULTI_OFFSET: {
			//All multivariate 2/3 with offset
			psi_m = -0.69 - 15.47 * (ta_v - t_surf_v)/(0.5 * (ta_v + t_surf_v)) + 0.0059 * zref * Constants::g/pow(vw,2);
			psi_s = 6.73 -688.18 * (ta_v - t_surf_v)/(0.5 * (ta_v + t_surf_v)) - 0.0023 * zref * Constants::g/pow(vw,2);
			return;
			}
		
			default:
			throw InvalidArgumentException("Unsupported atmospheric stability parametrization", AT);
		}
	} else { //unstable
		// Paulson - the original
		const double dummy1 = pow((1. - 15. * stab_ratio), 0.25);
		psi_m = 2. * log(0.5 * (1. + dummy1)) + log(0.5 * (1. + Optim::pow2(dummy1)))
				- 2. * atan(dummy1) + 0.5 * Constants::pi;
		// Stearns & Weidner, 1993, for scalars
		const double dummy2 = pow((1. - 22.5 * stab_ratio), 0.33333);
		psi_s = pow(log(1. + dummy2 + Optim::pow2(dummy2)), 1.5) - 1.732 * atan(0.577 * (1. + 2. * dummy2)) + 0.1659;
	}
}

/**
 * @brief Atmospheric stability correction for wind values.
 * This makes an iteration to find z0 and ustar at the same time
 * @param Mdata
 * @param Xdata
 * @param adjust_VW_height if set to false, assumes a constant measurement height for wind values (default: true, ie.
 * take into account the snow height decreasing the sensor height above the surface)
 */
void Meteo::MicroMet(const SnowStation& Xdata, CurrentMeteo &Mdata, const bool& adjust_VW_height) const
{
	static const unsigned int max_iter = 100;
        //Adapting the roughness length value depending on the presence or absence of snow
	const double rough_len = ((Xdata.cH - Xdata.Ground) > 0.03)? roughness_length : Xdata.BareSoil_z0;

	// Ideal approximation of pressure and vapor pressure
	const double p0 = Atmosphere::stdAirPressure(Xdata.meta.position.getAltitude());
	const double sat_vap = Atmosphere::vaporSaturationPressure(Mdata.ta);
	const double vw = std::max(0.3, Mdata.vw);

	// Initialize snow surface temperature as well as virtual temperatures for stability
	const double t_surf = Xdata.Ndata[Xdata.getNumberOfElements()].T;
	const double ta_v = Mdata.ta * (1. + 0.377 * sat_vap / p0);
	const double t_surf_v = t_surf * (1. + 0.377 * sat_vap / p0);

	// Adjust for snow height if fixed_height_of_wind=false
	const double zref = (adjust_VW_height)
				? std::max(
					    0.5,
					    height_of_wind_value - (Xdata.cH - Xdata.Ground + ( (Xdata.findMarkedReferenceLayer() == Constants::undefined) ? (0.) : (Xdata.findMarkedReferenceLayer() - Xdata.Ground) ))
					  )
				: height_of_wind_value ;
	// In case of ventilation ... Wind pumping displacement depth (m)
	const double d_pump = (SnLaws::wind_pump)? SnLaws::compWindPumpingDisplacement(Xdata) : 0.;

const int I = Xdata.meta.position.getGridI();
const int J = Xdata.meta.position.getGridJ();

	// Iterate to find atmospheric stability
	// initial guess (neutral)
	static const double eps1 = 1.e-3;
	double psi_m = 0., psi_s = 0.;
	const double z_ratio = log((zref - d_pump) / rough_len);
	double ustar_old, ustar = Constants::karman * vw / (z_ratio - psi_m); //at first, psi_m=0
	unsigned int iter = 0;
	do {
		iter++;
		ustar_old = ustar;
		
		// Stability corrections: compute ustar, psi_s & potentially psi_m
		if (stability==RICHARDSON) {
			RichardsonStability(ta_v, t_surf_v, zref, vw, z_ratio, ustar, psi_s); //compute ustar & psi_s
		} else if (!research_mode && (Mdata.tss > 273.) && (Mdata.ta > 277.)) {
			//force MO_MICHLMAYR for operational mode when temperatures are high enough
			MOStability(Meteo::MO_MICHLMAYR, ta_v, t_surf_v, t_surf, zref, vw, z_ratio, ustar, psi_s, psi_m);
		} else {
			MOStability(stability, ta_v, t_surf_v, t_surf, zref, vw, z_ratio, ustar, psi_s, psi_m);
		}
	} while ( (iter<max_iter) && (fabs(ustar_old - ustar) > eps1) );

	if(iter==max_iter) {
		prn_msg(__FILE__, __LINE__, "wrn", Mdata.date,
		        "Stability correction did not converge (azi=%.0lf, slope=%.0lf) --> assume neutral",
		        Xdata.meta.getAzimuth(), Xdata.meta.getSlopeAngle());
		Mdata.z0 = rough_len;
		Mdata.ustar = Constants::karman * vw / z_ratio;
		Mdata.psi_s = 0.;
		return;
	}

	Mdata.ustar = ustar;
	Mdata.z0 = rough_len;
	Mdata.psi_s = psi_s;
	Mdata.psi_m = psi_m;

//        if((I==37) & (J==99)){
//          std::cout << "in micromet:\t" << z_ratio << "," << zref << "," << d_pump << "," << rough_len << "," << psi_m << "," << ta_v << "," << t_surf << "," << ustar << std::endl;
//        }

}

/**
 * @brief Compute measured snow depth change rate to detect growing grass (canopy) vs. snowfall on bare ground
 * @param Mdata
 * @param Xdata
 * @param hs_a3hl6 snow depth average from t_now - 6 h to t_now - 3 h
 * @return whether grass should be detected
 */
bool Meteo::compHSrate(CurrentMeteo& Mdata, const SnowStation& Xdata, const double& hs_a3hl6)
{
	if (Xdata.getNumberOfNodes() == Xdata.SoilNode+1) { //Detect only when there is no snow pack yet.
		if ((hs_a3hl6 != Constants::undefined) && (Mdata.hs_a3h != Constants::undefined)) {
			// NOTE we compare two consecutive time spans of 3 hours and take the rate from
			//      the "middle" of the two time spans. hs_rate is in m h-1.
			Mdata.hs_rate = (Mdata.hs_a3h - hs_a3hl6) / 3.;
			return true;
		} else {
			Mdata.hs_rate = Constants::undefined;
			return false;
		}
	} else {
		Mdata.tss_a12h = Constants::undefined;
		Mdata.tss_a24h = Constants::undefined;
		Mdata.hs_rate = Constants::undefined;
		return false;
	}
}

/**
 * @brief
 * \li with CANOPY set:
 * 		In case of an existing canopy, call canopy routine, which computes precipitation, radiation,
 * 		friction velocity and reference temperature for the surface below the canopy.
 * 		Note that solar radiation may change also in dg_cn_Canopy(). \n
 * 		- Mdata->iswr  incoming global solar radiation (direct + diffuse), adapted to canopy
 * 		- Mdata->rswr  reflected global solar radiation (diffuse), adapted to canopy
 * 		- Mdata->ustar friction velocity, adapted to canopy
 * 		- Mdata->z0    roughness length, adapted to canopy
 * 		- Mdata->ea    atmospheric emissivity below canopy, i.e., to give correct
 * 		               longwave radiation as function of air temperature, however
 * 		               modified to include effect of canopy
 * \li without canopy (CANOPY is not set):
 * 		For bare soil as well as snowed-in canopy or some other problems, compute the roughness
 * 		length z0, the friction velocity ustar as well as the atmospheric stability correction
 * 		psi_s for scalar heat fluxes
 * 		- Mdata->ustar friction velocity
 * 		- Mdata->z0    roughness length
 * 		- psi_s        stability correction for scalar heat fluxes
 * @param Mdata meteorological forcing
 * @param Xdata snow profile data
 * @param runCanopyModel should the canopy module also be called?
 */
void Meteo::compMeteo(CurrentMeteo &Mdata, SnowStation &Xdata, const bool& runCanopyModel)
{
	bool canopy_status = true;
	if (useCanopyModel && runCanopyModel) {	// The canopy model should not necessarily be called at every call to compMeteo
		canopy_status = canopy.runCanopyModel(Mdata, Xdata, roughness_length, height_of_wind_value, adjust_height_of_wind_value);
	}

	if (!(useCanopyModel) || canopy_status==false) {
		MicroMet(Xdata, Mdata, adjust_height_of_wind_value);
	}
}

void Meteo::compRadiation(const SnowStation &station, mio::SunObject &sun, SnowpackConfig &cfg, CurrentMeteo &Mdata)
{
	const std::string sw_mode = cfg.get("SW_MODE", "Snowpack");
	const bool force_sw_mode = cfg.get("FORCE_SW_MODE", "SnowpackAdvanced"); //Adjust for correct radiation input if ground is effectively bare. It HAS to be set to true in operational mode.
	const bool enforce_hs = cfg.get("ENFORCE_MEASURED_SNOW_HEIGHTS", "Snowpack");
	const double iswr_ref = (sw_mode == "REFLECTED") ?  Mdata.rswr/station.Albedo : Mdata.iswr;

	sun.calculateRadiation(Mdata.ta, Mdata.rh, station.Albedo);
	double H_toa, H_direct, H_diffuse;
	sun.getHorizontalRadiation(H_toa, H_direct, H_diffuse);
	const double Md = sun.getSplitting(iswr_ref);
	double dir_h, diff;
	if ((iswr_ref > 0.) && (H_direct > 0.)) {
		dir_h = (1. - Md)*iswr_ref;
		diff = Md*iswr_ref;
	} else {
		if (iswr_ref > 0.) {
			dir_h = 0.;
			diff = std::max(Md*iswr_ref, H_diffuse);
		} else {
			dir_h = 0.;
			diff = 0.;
		}
	}

	if (sw_mode == "REFLECTED") {
		Mdata.rswr = (dir_h + diff)*station.Albedo;
	} else {
		Mdata.iswr = dir_h + diff; //usually = iswr_ref except for corner cases
	}

	if (force_sw_mode) {
		// Sometimes, there is no snow left on the ground at the station (-> rswr is small)
		// but there is still some snow left in the simulation, which then is hard to melt
		// if we find this is such a situation, we set iswr to the potential radiation.
		// Such a correction is only needed for flat field, the others will inherit it
		// What snow depth should be used?
		const bool use_hs_meas = enforce_hs && (station.meta.getSlopeAngle() < Constants::min_slope_angle);
		const double hs = (use_hs_meas)? station.mH - station.Ground : station.cH - station.Ground;
		const double iswr_factor = Mdata.rswr / (dir_h+diff+Constants::eps); //avoiding "0/0"

		if ((hs>0 && hs<0.1) && Mdata.rh<0.7 && iswr_factor<0.3) {
			dir_h = H_direct;
			diff = H_diffuse;
			Mdata.iswr = dir_h+diff;
			if (Mdata.iswr>0. && (Mdata.rswr/Mdata.iswr) < (2.0*station.SoilAlb))
				Mdata.rswr = Mdata.iswr * 2.0*station.SoilAlb;
			else
				Mdata.rswr = 0.;
			cfg.addKey("SW_MODE", "Snowpack", "BOTH");  // as both Mdata.iswr and Mdata.rswr were reset
		}
	}

	Mdata.diff = diff;
	Mdata.dir_h = dir_h;
	double azimuth, elevation;
	sun.position.getHorizontalCoordinates(azimuth, elevation);
	Mdata.elev = elevation*mio::Cst::to_rad;
}

void Meteo::radiationOnSlope(const SnowStation &sector, const mio::SunObject &sun, CurrentMeteo &Mdata, SurfaceFluxes &surfFluxes)
{
	//diff remains the same as on flat field
	double dir_slope;
	if (sector.meta.getSlopeAngle() > Constants::min_slope_angle) {
		dir_slope = sun.position.getHorizontalOnSlope(sector.meta.getAzimuth(), sector.meta.getSlopeAngle(), Mdata.dir_h, 9.);
		if ( (Mdata.dir_h+Mdata.diff) > 0. ) {
			Mdata.iswr = std::min(dir_slope + Mdata.diff, Constants::solcon);
			Mdata.rswr = sector.Albedo*Mdata.iswr;
		} else {
			Mdata.iswr = 0.;
			Mdata.rswr = 0.;
		}
	} else {
		dir_slope = Mdata.dir_h;
	}

	// Assign radiation values to Sdata
	surfFluxes.sw_hor  += (Mdata.dir_h+Mdata.diff);
	surfFluxes.sw_dir  += dir_slope;
	surfFluxes.sw_diff += Mdata.diff;
}
