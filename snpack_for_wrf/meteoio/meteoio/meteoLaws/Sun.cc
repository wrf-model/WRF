/***********************************************************************************/
/*  Copyright 2009 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
/***********************************************************************************/
/* This file is part of MeteoIO.
    MeteoIO is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MeteoIO is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with MeteoIO.  If not, see <http://www.gnu.org/licenses/>.
*/
#include <cmath>
#include <string>
#include <iostream> //for "fixed"
#include <iomanip> //for "setprecision"
#include <algorithm>

#include <meteoio/meteoLaws/Sun.h>
#include <meteoio/meteoLaws/Atmosphere.h>
#include <meteoio/meteoLaws/Meteoconst.h>
#include <meteoio/MathOptim.h>
#include <meteoio/IOUtils.h>

namespace mio {

const double SunObject::elevation_dftlThreshold = 5.; //in degrees
const double SunObject::rad_threshold = 4.; //if radiation is below this threshold, it is assumed to be night

SunObject::SunObject(SunObject::position_algo /*alg*/)
           : position(), julian_gmt(IOUtils::nodata), TZ(IOUtils::nodata), latitude(IOUtils::nodata), longitude(IOUtils::nodata), altitude(IOUtils::nodata),
             elevation_threshold(elevation_dftlThreshold),
             beam_toa(IOUtils::nodata), beam_direct(IOUtils::nodata), beam_diffuse(IOUtils::nodata) {}

SunObject::SunObject(const double& i_latitude, const double& i_longitude, const double& i_altitude)
           : position(), julian_gmt(IOUtils::nodata), TZ(IOUtils::nodata), latitude(i_latitude), longitude(i_longitude), altitude(i_altitude),
             elevation_threshold(elevation_dftlThreshold),
             beam_toa(IOUtils::nodata), beam_direct(IOUtils::nodata), beam_diffuse(IOUtils::nodata) {}

SunObject::SunObject(const double& i_latitude, const double& i_longitude, const double& i_altitude, const double& i_julian, const double& i_TZ)
           : position(), julian_gmt(i_julian - i_TZ*1./24.), TZ(i_TZ), latitude(i_latitude), longitude(i_longitude), altitude(i_altitude),
             elevation_threshold(elevation_dftlThreshold),
             beam_toa(IOUtils::nodata), beam_direct(IOUtils::nodata), beam_diffuse(IOUtils::nodata)
{
	position.setAll(latitude, longitude, julian_gmt+TZ*1./24., TZ);
}

void SunObject::setDate(const double& i_julian, const double& i_TZ)
{
	const double i_julian_gmt = i_julian - i_TZ*1./24.;
	TZ = i_TZ;

	if (i_julian_gmt!=julian_gmt) { //invalidate all fields if receiving a new date
		position.reset();
		julian_gmt = i_julian_gmt;
		beam_toa = beam_direct = beam_diffuse = IOUtils::nodata;
	}

	//if the date was new or if the previous date had not lead to an update -> update now
	if (latitude!=IOUtils::nodata && longitude!=IOUtils::nodata && altitude!=IOUtils::nodata &&
	  (beam_toa==IOUtils::nodata || beam_direct==IOUtils::nodata || beam_diffuse==IOUtils::nodata)) {
		position.setAll(latitude, longitude, julian_gmt+TZ*1./24., TZ);
	}
}

void SunObject::setLatLon(const double& i_latitude, const double& i_longitude, const double& i_altitude)
{
	if (i_latitude==IOUtils::nodata || i_longitude==IOUtils::nodata || i_altitude==IOUtils::nodata) {
		std::stringstream ss;
		ss << "missing geolocalization parameters at (" << i_latitude << " , " << i_longitude << " , " << i_altitude << ")";
		throw NoDataException(ss.str(), AT);
	}
	
	if (i_latitude==latitude && i_longitude==longitude) {
		if (altitude==i_altitude) return; //everything is the same, nothing to do
		
		altitude = i_altitude;
		beam_toa = beam_direct = beam_diffuse = IOUtils::nodata;
		return;
	}
	
	position.reset();
	latitude = i_latitude;
	longitude = i_longitude;
	altitude = i_altitude;
	beam_toa = beam_direct = beam_diffuse = IOUtils::nodata;
	if (julian_gmt!=IOUtils::nodata && TZ!=IOUtils::nodata) {
		position.setAll(latitude, longitude, julian_gmt+TZ*1./24., TZ);
	}
}

void SunObject::resetAltitude(const double& i_altitude)
{
	if (i_altitude==IOUtils::nodata)
		throw NoDataException("the altitude can not be nodata", AT);
	if (altitude==i_altitude) return;
	
	altitude = i_altitude;
	beam_toa = beam_direct = beam_diffuse = IOUtils::nodata;
}

void SunObject::setElevationThresh(const double& i_elevation_threshold)
{
	elevation_threshold = i_elevation_threshold;
	beam_toa = beam_direct = beam_diffuse = IOUtils::nodata;
}

void SunObject::calculateRadiation(const double& ta, const double& rh, double pressure, const double& mean_albedo) 
{	//set beam_toa, beam_direct and beam_diffuse
	//ta in K, rh in [0,1], pressure in Pa, altitude in m
	double azimuth, elevation, eccentricity;
	position.getHorizontalCoordinates(azimuth, elevation, eccentricity);
	
	if (pressure==IOUtils::nodata) pressure = Atmosphere::stdAirPressure(altitude);
	getBeamPotential(elevation, eccentricity, ta, rh, pressure, mean_albedo, beam_toa, beam_direct, beam_diffuse);
}

void SunObject::calculateRadiation(const double& ta, const double& rh, const double& mean_albedo) 
{	//set beam_toa, beam_direct and beam_diffuse
	//ta in K, rh in [0,1], pressure in Pa, altitude in m
	double azimuth, elevation, eccentricity;
	position.getHorizontalCoordinates(azimuth, elevation, eccentricity);

	const double p = Atmosphere::stdAirPressure(altitude);
	getBeamPotential(elevation, eccentricity, ta, rh, p, mean_albedo, beam_toa, beam_direct, beam_diffuse);
}

//see http://www.meteoexploration.com/products/solarcalc.php for a validation calculator
void SunObject::getBeamPotential(const double& sun_elevation, const double& Eccentricity_corr,
                                 const double& ta, const double& rh, const double& pressure, const double& ground_albedo,
                                 double& R_toa, double& R_direct, double& R_diffuse) const
{
	if (ta==IOUtils::nodata || rh==IOUtils::nodata || pressure==IOUtils::nodata || ground_albedo==IOUtils::nodata) {
		R_toa = IOUtils::nodata;
		R_direct = IOUtils::nodata;
		R_diffuse = IOUtils::nodata;
		return;
	}
	if (sun_elevation<0.) { //the Sun is below the horizon, our formulas don't apply
		R_toa = R_direct = R_diffuse = 0.;
	} else {
		R_toa = Cst::solcon * (1.+Eccentricity_corr);
		getClearSky(sun_elevation, R_toa, ta, rh, pressure, ground_albedo, R_direct, R_diffuse);
	}
}

void SunObject::getClearSky(const double& sun_elevation, const double& R_toa,
                            const double& ta, const double& rh, const double& pressure, const double& ground_albedo,
                            double& R_direct, double& R_diffuse) const
{
	if (ta<0. || rh<0.) 
		throw InvalidArgumentException("When calling SunObject::getClearSky(), TA and RH must be >0, currently TA="+IOUtils::toString(ta)+", RH="+IOUtils::toString(rh), AT);
	
	//these pow cost us a lot here, but replacing them by fastPow() has a large impact on accuracy (because of the exp())
	static const double olt = 0.32;   //ozone layer thickness (cm) U.S.standard = 0.34 cm
	static const double w0 = 0.9;     //fraction of energy scattered to total attenuation by aerosols (Bird and Hulstrom(1981))
	static const double fc = 0.84;    //fraction of forward scattering to total scattering (Bird and Hulstrom(1981))
	static const double alpha = 1.3;  //wavelength exponent (Iqbal(1983) p.118). Good average value: 1.3+/-0.5. Related to the size distribution of the particules
	static const double beta = 0.03;  //amount of particules index (Iqbal(1983) p.118). Between 0 & .5 and above.
	const double zenith = 90. - sun_elevation; //this is the TRUE zenith because the elevation is the TRUE elevation
	const double cos_zenith = cos(zenith*Cst::to_rad); //this uses true zenith angle

	// relative optical air mass Young (1994), see http://en.wikipedia.org/wiki/Airmass
	//const double mr = 1. / (cos_zenith + 0.50572 * pow( 96.07995-zenith , -1.6364 )); //pbl: this should use apparent zenith angle, and we only get true zenith angle here...
	// relative optical air mass, Young, A. T. 1994. Air mass and refraction. Applied Optics. 33:1108–1110.
	const double mr = ( 1.002432*Optim::pow2(cos_zenith) + 0.148386*cos_zenith + 0.0096467) /
	                  ( Optim::pow3(cos_zenith) + 0.149864*Optim::pow2(cos_zenith)
	                  + 0.0102963*cos_zenith +0.000303978);

	// actual air mass: because mr is applicable for standard pressure
	// it is modified for other pressures (in Iqbal (1983), p.100)
	// pressure in Pa
	const double ma = mr * (pressure/101325.);

	// the equations for all the transmittances of the individual atmospheric constituents
	// are from Bird and Hulstrom (1980, 1981) and can be found summarized in Iqbal (1983)
	// on the quoted pages

	// broadband transmittance by Rayleigh scattering (Iqbal (1983), p.189)
	const double taur = exp( -0.0903 * pow(ma,0.84) * (1. + ma - pow(ma,1.01)) );

	// broadband transmittance by ozone (Iqbal (1983), p.189)
	const double u3 = olt * mr; // ozone relative optical path length
	const double alpha_oz = 0.1611 * u3 * pow(1. + 139.48 * u3, -0.3035) -
	                        0.002715 * u3 / ( 1. + 0.044  * u3 + 0.0003 * Optim::pow2(u3) ); //ozone absorbance
	const double tauoz = 1. - alpha_oz;

	// broadband transmittance by uniformly mixed gases (Iqbal (1983), p.189)
	const double taug = exp( -0.0127 * pow(ma, 0.26) );

	// saturation vapor pressure in Pa
	//const double Ps = exp( 26.23 - 5416./ta ); //as used for the parametrization
	const double Ps = Atmosphere::vaporSaturationPressure(ta);

	// Leckner (1978) (in Iqbal (1983), p.94), reduced precipitable water
	const double w = 0.493 * rh * Ps / ta;

	// pressure corrected relative optical path length of precipitable water (Iqbal (1983), p.176)
	// pressure and temperature correction not necessary since it is included in its numerical constant
	const double u1 = w * mr;

	// broadband transmittance by water vapor (in Iqbal (1983), p.189)
	const double tauw = 1. - 2.4959 * u1  / (pow(1.0 + 79.034 * u1, 0.6828) + 6.385 * u1);

	// broadband total transmittance by aerosols (in Iqbal (1983), pp.189-190)
	// using Angstroem's turbidity formula Angstroem (1929, 1930) for the aerosol thickness
	// in Iqbal (1983), pp.117-119
	// aerosol optical depth at wavelengths 0.38 and 0.5 micrometer
	static const double ka1 = beta * pow(0.38, -alpha);
	static const double ka2 = beta * pow(0.5, -alpha);

	// broadband aerosol optical depth:
	static const double ka  = 0.2758 * ka1 + 0.35 * ka2;

	// total aerosol transmittance function for the two wavelengths 0.38 and 0.5 micrometer:
	const double taua = exp( -pow(ka, 0.873) * (1. + ka - pow(ka, 0.7088)) * pow(ma, 0.9108) );

	// broadband transmittance by aerosols due to absorption only (Iqbal (1983) p. 190)
	const double tauaa = 1. - (1. - w0) * (1. - ma + pow(ma, 1.06)) * (1. - taua);

	// broadband transmittance function due to aerosols scattering only
	// Iqbal (1983) p. 146 (Bird and Hulstrom (1981))
	const double tauas = taua / tauaa;

	// direct normal solar irradiance in range 0.3 to 3.0 micrometer (Iqbal (1983) ,p.189)
	// 0.9751 is for this wavelength range.
	// Bintanja (1996) (see Corripio (2002)) introduced a correction beta_z for increased
	// transmittance with altitude that is linear up to 3000 m and than fairly constant up to 5000 - 6000 m
	const double beta_z = (altitude<3000.)? 2.2*1.e-5*altitude : 2.2*1.e-5*3000.;

	//Now calculating the radiation
	//Top of atmosphere radiation (it will always be positive, because we check for sun elevation before)
	const double tau_commons = tauoz * taug * tauw * taua;

	// Diffuse radiation from the sky
	const double factor = 0.79 * R_toa * tau_commons / (1. - ma + pow( ma,1.02 ));  //avoid recomputing pow() twice
	// Rayleigh-scattered diffuse radiation after the first pass through atmosphere (Iqbal (1983), p.190)
	const double Idr = factor * 0.5 * (1. - taur );

	// aerosol scattered diffuse radiation after the first pass through atmosphere (Iqbal (1983), p.190)
	const double Ida = factor * fc  * (1. - tauas);

	// cloudless sky albedo Bird and Hulstrom (1980, 1981) (in Iqbal (1983) p. 190)
	//in Iqbal, it is recomputed with ma=1.66*pressure/101325.; and alb_sky=0.0685+0.17*(1.-taua_p)*w0;
	const double alb_sky = 0.0685 + (1. - fc) * (1. - tauas);


	//Now, we compute the direct and diffuse radiation components
	//Direct radiation. All transmitances, including Rayleigh scattering (Iqbal (1983), p.189)
	R_direct = 0.9751*( taur * tau_commons + beta_z ) * R_toa ;

	// multiple reflected diffuse radiation between surface and sky (Iqbal (1983), p.154)
	const double Idm = (Idr + Ida + R_direct) * ground_albedo * alb_sky / (1. - ground_albedo * alb_sky);
	R_diffuse = (Idr + Ida + Idm)*cos_zenith; //Iqbal always "project" diffuse radiation on the horizontal

	if ( sun_elevation < elevation_threshold ) {
		//if the Sun is too low on the horizon, we put all the radiation as diffuse
		//the splitting calculation that might take place later on will reflect this
		//instead point radiation, it becomes the radiation of a horizontal sky above the domain
		R_diffuse += R_direct*cos_zenith; //HACK
		R_direct = 0.;
	}
}

void SunObject::getBeamRadiation(double& R_toa, double& R_direct, double& R_diffuse) const
{
	R_toa = beam_toa;
	R_direct = beam_direct;
	R_diffuse = beam_diffuse;
}

//diffuse remains beam_diffuse, nodatas are kept
void SunObject::getHorizontalRadiation(double& R_toa, double& R_direct, double& R_diffuse) const
{
	R_toa = position.getRadiationOnHorizontal(beam_toa);
	R_direct = position.getRadiationOnHorizontal(beam_direct);
	R_diffuse = beam_diffuse;
}

//nodatas are kept
void SunObject::getSlopeRadiation(const double& slope_azi, const double& slope_elev, double& R_toa, double& R_direct, double& R_diffuse) const
{
	R_toa = position.getRadiationOnSlope(slope_azi, slope_elev, beam_toa);
	R_direct = position.getRadiationOnSlope(slope_azi, slope_elev, beam_direct);
	R_diffuse = beam_diffuse;
}

/**
 * @brief Evaluate the splitting coefficient between direct and diffuse components of the
 * incoming short wave radiation. Splitting is based on "clearness of the sky", ie. the ratio of
 * measured incoming global radiation to top of the atmosphere radiation toa_h,
 * see
 * D. G. Erbs, S.A. Klein, J.A. Duffie, <i>"Estimation of the diffuse radiation fraction for hourly, daily and monthly-average global radiation"</i>, Solar Energy, <b>28</b>, 4, 1982, Pages 293-302 and
 * M. Iqbal, <i>"An introduction to solar radiation"</i>, 1983, Academic Press,  ISBN: 0-12-373750-8 and
 * D. T. Reindl, W. A. Beckman, J. A. Duffle, <i>"Diffuse fraction correlations</i>, Solar Energy, <b>45</b>, 1990, pp 1-7.
 * @param iswr_modeled modelled radiation, it should be horizontal Top Of %Atmosphere Radiation (W/m²)
 * @param iswr_measured measured Incoming Short Wave Radiation on the ground (W/m²)
 * @return splitting coefficient (between 0 and 1, 1 being 100% diffuse radiation)
 */
double SunObject::getSplitting(const double& iswr_modeled, const double& iswr_measured) const
{
	if (iswr_modeled==IOUtils::nodata)
		throw NoDataException("modelled ISWR can not be nodata, please call Sun::calculateRadiation() before!", AT);
	
	if (iswr_measured==IOUtils::nodata)
		throw NoDataException("measured ISWR can not be nodata", AT);
	
	double splitting_coef;
	double azimuth, elevation;
	position.getHorizontalCoordinates(azimuth, elevation);

	if ( elevation < elevation_threshold ) {
		//when the Sun is low above the horizon, Mt is getting abnormaly too large pretending
		// this is a clear sky day when almost all the radiation should be diffuse
		// no matter how the sky is
		splitting_coef = 1.0;
	} else {
		// clear sky index (ratio global measured to top of atmosphere radiation)
		const double Mt = iswr_measured / iswr_modeled; // should be <=1.2, aka clearness index Kt
		static const double clear_sky = 0.147;

		// diffuse fraction: hourly ratio of diffuse to global radiation incident on a horizontal surface
		// splitting according to a combination of Reindl et al.(1990)'s models (Mt-model and Mt&Psolar->elev-model):
		if ( Mt >= 0.78 ) { // Mt in [0.78;1] -> clear day
			 splitting_coef = clear_sky;
		} else {
			if ( Mt <= 0.3 ) { // Mt in [0;0.3] -> overcast
				splitting_coef = 1.02 - 0.248*Mt;
				if (splitting_coef>1.) splitting_coef=1.;
			} else {           // Mt in ]0.3;0.78[ -> cloudy
				splitting_coef = 1.4 - 1.749*Mt + 0.177*sin(elevation*Cst::to_rad);
				if (splitting_coef>0.97) splitting_coef = 0.97;
				if (splitting_coef<clear_sky) splitting_coef = clear_sky;
			}
		}
	}

	return splitting_coef; // should be <=1.1; diff/toa should be <=0.8
}

/**
 * @brief Evaluate the splitting coefficient between direct and diffuse components of the
 * incoming short wave radiation. Splitting is based on "clearness of the sky", ie. the ratio of
 * measured incoming global radiation to top of the atmosphere radiation toa_h.
 * This is based on Boland, John, Lynne Scott, and Mark Luther, <i>"Modelling the diffuse fraction 
 * of global solar radiation on a horizontal surface"</i>, Environmetrics <b>12.2</b>, 2001, pp103-116.
 * @param iswr_modeled modelled radiation, it should be horizontal Top Of %Atmosphere Radiation (W/m²)
 * @param iswr_measured measured Incoming Short Wave Radiation on the ground (W/m²)
 * @param t solar time of day, ie solar time between 0 and 24
 * @return splitting coefficient (between 0 and 1, 1 being 100% diffuse radiation)
 */
double SunObject::getSplittingBoland(const double& iswr_modeled, const double& iswr_measured, const double& t) const
{
	if (iswr_modeled==IOUtils::nodata)
		throw NoDataException("modelled ISWR can not be nodata, please call Sun::calculateRadiation() before!", AT);
	
	if (iswr_measured==IOUtils::nodata)
		throw NoDataException("measured ISWR can not be nodata", AT);
	
	static const double clear_sky = 0.147;
	double splitting_coef;
	double azimuth, elevation;
	position.getHorizontalCoordinates(azimuth, elevation);

	if ( elevation < elevation_threshold ) {
		//when the Sun is low above the horizon, Mt is getting abnormaly too large pretending
		// this is a clear sky day when almost all the radiation should be diffuse
		// no matter how the sky is
		splitting_coef = 1.0;
	} else {
		// clear sky index (ratio global measured to top of atmosphere radiation)
		const double kt = iswr_measured / iswr_modeled; // should be <=1.2, aka clearness index Kt
		static const double beta_0 = -8.769;
		static const double beta_1 = 7.325;
		static const double beta_2 = 0.377;
		static const double c = -0.039;
		
		splitting_coef = c + (1-c) / (1 + exp( beta_0 + beta_1*kt + beta_2*t) ); //complex fit
		//splitting_coef = 1. / (1 + exp(7.997*(kt-0.586))); //simple fit
	}

	return std::min(1., std::max(clear_sky, splitting_coef));
}

double SunObject::getSplitting(const double& iswr_measured) const
{
	double toa_h, direct_h, diffuse;
	getHorizontalRadiation(toa_h, direct_h, diffuse);
	return getSplitting(toa_h, iswr_measured);
}

/**
 * @brief Evaluate an atmospheric losses factor.
 * This correction factor is evaluated by comparing the global potential and the global measured radiation.
 * @param[in] iswr_measured measured Incoming Short Wave Radiation on the ground (W/m²)
 * @return correction coefficient (between 0 and 1)
 */
double SunObject::getCorrectionFactor(const double& iswr_measured) const
{
	double Md;
	bool day, night;
	return getCorrectionFactor(iswr_measured, Md, day, night);
}

/**
 * @brief Evaluate an atmospheric losses factor.
 * This correction factor is evaluated by comparing the global potential and the global measured radiation.
 * The booleans "day" and "night" are returned in order to allow further optimizations. At dawn or dusk, both
 * are set to *false*.
 * @param[in] iswr_measured measured Incoming Short Wave Radiation on the ground (W/m²)
 * @param[out] Md splitting coefficient (between 0 and 1, 1 being 100% diffuse radiation)
 * @param[out] day is it daytime?
 * @param[out] night ist it nightime?
 * @return correction coefficient (between 0 and 1)
 */
double SunObject::getCorrectionFactor(const double& iswr_measured, double &Md, bool &day, bool &night) const
{
	if (iswr_measured==IOUtils::nodata)
		throw NoDataException("measured ISWR can not be nodata", AT);
	
	double toa_h, direct_h, pot_diffuse;
	getHorizontalRadiation(toa_h, direct_h, pot_diffuse);
	Md = getSplitting(toa_h, iswr_measured);
	const double pot_glob_h = direct_h+pot_diffuse;
	
	//we compare the mesured radiation to the modeled radiation, in order to guess the cloudiness.
	//This comparison allows us to compute a global correction factor
	if ( pot_glob_h>rad_threshold && iswr_measured>rad_threshold ) {
		day = true;
		night = false;
		return std::min( iswr_measured / pot_glob_h, 1.);
	} else {
		day = false;
		night = (direct_h>rad_threshold || iswr_measured>rad_threshold)? false : true; //is it dawn/dusk?
		return 1.;
	}
}

const std::string SunObject::toString() const
{
	std::ostringstream os;
	const std::streamsize old_prec = os.precision();
	os << "<SunObject>\n";
	os << position.toString();
	os << std::fixed << std::setprecision(4);
	os << "Julian (gmt)\t" << julian_gmt << " (TZ=" << std::setprecision(2) << TZ << std::setprecision(4) << ")\n";
	os << "Lat/Long/Alt\t" << std::setw(7) << latitude << "° " << std::setw(7) << longitude << "° " << std::setprecision(0) << std::setw(4) << altitude << "\n";
	os << "Elev. thresh.\t" << std::setprecision(1) << elevation_threshold << "°\n";

	const int colw=10;
	os << std::setw(colw) << "" << std::fixed << std::setw(colw) << std::setprecision(1) << "toa";
	os << std::fixed << std::setw(colw) << std::setprecision(1) << "direct";
	os << std::fixed << std::setw(colw) << std::setprecision(1) << "diffuse";
	os << std::fixed << std::setw(colw) << std::setprecision(1) << "sum\n";

	os << std::setw(colw) << "Beam" << std::fixed << std::setw(colw) << std::setprecision(1) << beam_toa;
	os << std::fixed << std::setw(colw) << std::setprecision(1) << beam_direct;
	os << std::fixed << std::setw(colw) << std::setprecision(1) << beam_diffuse;
	os << std::fixed << std::setw(colw) << std::setprecision(1) << beam_direct+beam_diffuse << "\n";

	double R_toa, R_direct, R_diffuse;
	getHorizontalRadiation(R_toa, R_direct, R_diffuse);
	os << std::setw(colw) << "Horizontal" << std::fixed << std::setw(colw) << std::setprecision(1) << R_toa;
	os << std::fixed << std::setw(colw) << std::setprecision(1) << R_direct;
	os << std::fixed << std::setw(colw) << std::setprecision(1) << R_diffuse;
	os << std::fixed << std::setw(colw) << std::setprecision(1) << R_direct+R_diffuse << "\n";

	os << "</SunObject>\n";
	os.precision(old_prec);
	return os.str();
}

} //end namespace
