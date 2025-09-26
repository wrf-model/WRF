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
#include <iomanip>
#include <algorithm>

#include <meteoio/meteoLaws/Suntrajectory.h>
#include <meteoio/meteoLaws/Meteoconst.h> //for math constants
#include <meteoio/IOExceptions.h>
#include <meteoio/dataClasses/Date.h> //for printFractionalDay
#include <meteoio/MathOptim.h>
#include <meteoio/IOUtils.h>

namespace mio {

SunTrajectory::SunTrajectory() : julian_gmt(IOUtils::nodata), TZ(IOUtils::nodata), latitude(IOUtils::nodata), longitude(IOUtils::nodata),
                                 SolarAzimuthAngle(IOUtils::nodata), SolarElevation(IOUtils::nodata),
                                 eccentricityEarth(IOUtils::nodata), SunRise(IOUtils::nodata), SunSet(IOUtils::nodata),
                                 SunlightDuration(IOUtils::nodata), SolarNoon(IOUtils::nodata),
                                 SunRightAscension(IOUtils::nodata), SunDeclination(IOUtils::nodata),
                                 HourAngle(IOUtils::nodata) {}

SunTrajectory::SunTrajectory(const double& i_latitude, const double& i_longitude) : julian_gmt(IOUtils::nodata), TZ(IOUtils::nodata), latitude(i_latitude), longitude(i_longitude),
                                 SolarAzimuthAngle(IOUtils::nodata), SolarElevation(IOUtils::nodata),
                                 eccentricityEarth(IOUtils::nodata), SunRise(IOUtils::nodata), SunSet(IOUtils::nodata),
                                 SunlightDuration(IOUtils::nodata), SolarNoon(IOUtils::nodata),
                                 SunRightAscension(IOUtils::nodata), SunDeclination(IOUtils::nodata),
                                 HourAngle(IOUtils::nodata) {}

/**
 * @brief Compute the solar incidence (rad), i.e. the angle between the incident sun beam
 *   and the normal to the slope (simplified by Funk (1984 pp.139-140)).
 * @param slope_azi slope azimuth in degrees, bearing
 * @param slope_elev slope angle in degrees
 * @return double angle of incidence of the sun to the local slope
 */
double SunTrajectory::getAngleOfIncidence(const double& slope_azi, const double& slope_elev) const
{
	const double Z = (90.-SolarElevation)*Cst::to_rad;
	const double beta = slope_elev*Cst::to_rad;
	const double cos_theta = cos(beta)*cos(Z) + sin(beta)*sin(Z)*cos((SolarAzimuthAngle-slope_azi)*Cst::to_rad);

	return acos(cos_theta)*Cst::to_deg;
}

double SunTrajectory::getAngleOfIncidence(const double& sun_azi, const double& sun_elev,
                                          const double& slope_azi, const double& slope_elev)
{
	const double Z = (90.-sun_elev)*Cst::to_rad;
	const double beta = slope_elev*Cst::to_rad;
	const double cos_theta = cos(beta)*cos(Z) + sin(beta)*sin(Z)*cos((sun_azi-slope_azi)*Cst::to_rad);

	return acos(cos_theta)*Cst::to_deg;
}

double SunTrajectory::getRadiationOnHorizontal(const double& radiation) const
{ // Project a beam radiation (ie: perpendicular to the sun beam) to the horizontal
// Oke, T.R., Boundary Layer Climates. 2nd ed, 1987, Routledge, London, p345.
	if (radiation==IOUtils::nodata) return IOUtils::nodata;

	const double Z = (90.-SolarElevation)*Cst::to_rad;

	const double on_horizontal = radiation * cos(Z);
	return on_horizontal;
}

double SunTrajectory::getRadiationOnSlope(const double& slope_azi, const double& slope_elev, const double& radiation) const
{ // Project a beam radiation (ie: perpendicular to the sun beam) to a given slope
// Oke, T.R., Boundary Layer Climates. 2nd ed, 1987, Routledge, London, p345.
	if (radiation==IOUtils::nodata || slope_azi==IOUtils::nodata || slope_elev==IOUtils::nodata) return IOUtils::nodata;

	const double Z = (90.-SolarElevation)*Cst::to_rad;
	const double beta = slope_elev*Cst::to_rad;
	const double cos_theta = cos(beta)*cos(Z) + sin(beta)*sin(Z)*cos((SolarAzimuthAngle-slope_azi)*Cst::to_rad);

	const double on_slope = radiation*cos_theta;
	if (on_slope>0.) return on_slope;
	else return 0.;
}

/** @brief Project a given horizontal radiation to a given slope.
* The sun position is the current one, the radiation intensity is given as well as the slope parameters.
* This should be used after correcting the radiation on the horizontal (by applying atmospheric effects or
* special handling for dawn/dusk) in order to compute consistent radiation on slopes.
* @param slope_azi slope azimuth (compass orientation, in degrees)
* @param slope_elev slope angle (in degrees)
* @param H_radiation radiation intensity on the horizontal
* @param elev_threshold all solar elevations less than this threshold will be use this threshold (in degrees, default 5 degrees)
* @return radiation projected on the slope
*/
double SunTrajectory::getHorizontalOnSlope(const double& slope_azi, const double& slope_elev, const double& H_radiation, const double& elev_threshold) const
{// Project a given horizontal radiation to a given slope
// Oke, T.R., Boundary Layer Climates. 2nd ed, 1987, Routledge, London, p345.
	if (H_radiation==IOUtils::nodata || slope_azi==IOUtils::nodata || slope_elev==IOUtils::nodata) return IOUtils::nodata;
	
	const double Z = (SolarElevation>=elev_threshold)? (90.-SolarElevation)*Cst::to_rad : (90.-elev_threshold)*Cst::to_rad;
	const double cosZ = cos(Z);

	const double beta = slope_elev*Cst::to_rad;
	const double cos_theta = cos(beta)*cosZ + sin(beta)*sin(Z)*cos((SolarAzimuthAngle-slope_azi)*Cst::to_rad);
	const double on_slope = ( H_radiation/cosZ ) * cos_theta;
	if (on_slope>0.) return on_slope;
	return 0.;
}

//useful static methods
double SunTrajectory::projectHorizontalToSlope(const double& sun_azi, const double& sun_elev, const double& slope_azi, const double& slope_elev, const double& H_radiation, const double& elev_threshold)
{// Project a horizontal radiation to a given slope
// Oke, T.R., Boundary Layer Climates. 2nd ed, 1987, Routledge, London, p345.
	if (H_radiation==IOUtils::nodata || slope_azi==IOUtils::nodata || slope_elev==IOUtils::nodata) return IOUtils::nodata;
	
	const double Z = (sun_elev>elev_threshold)? (90.-sun_elev)*Cst::to_rad : (90.-elev_threshold)*Cst::to_rad;
	const double cosZ = cos(Z);

	if (cosZ==0.) {
		return 1.e12;
	} else {
		const double beta = slope_elev*Cst::to_rad;
		const double cos_theta = cos(beta)*cosZ + sin(beta)*sin(Z)*cos((sun_azi-slope_azi)*Cst::to_rad);
		const double on_slope = ( H_radiation/cosZ ) * cos_theta;
		if (on_slope>0.) return on_slope;
		return 0.;
	}
}

double SunTrajectory::projectSlopeToHorizontal(const double& sun_azi, const double& sun_elev, const double& slope_azi, const double& slope_elev, const double& S_radiation)
{// Project a slope radiation to horizontal
// Oke, T.R., Boundary Layer Climates. 2nd ed, 1987, Routledge, London, p345.
	if (S_radiation==IOUtils::nodata || slope_azi==IOUtils::nodata || slope_elev==IOUtils::nodata) return IOUtils::nodata;
	
	const double Z = (90.-sun_elev)*Cst::to_rad;
	const double beta = slope_elev*Cst::to_rad;
	const double cos_theta = cos(beta)*cos(Z) + sin(beta)*sin(Z)*cos((sun_azi-slope_azi)*Cst::to_rad);

	if (cos_theta==0.) {
		return 1.e12;
	} else {
		const double on_horizontal = ( S_radiation/cos_theta ) * cos(Z);
		return on_horizontal;
	}
}

double SunTrajectory::projectHorizontalToBeam(const double& sun_elev, const double& H_radiation)
{ // Project a beam radiation (ie: perpendicular to the sun beam) to the horizontal
// Oke, T.R., Boundary Layer Climates. 2nd ed, 1987, Routledge, London, p345.
	if (H_radiation==IOUtils::nodata) return IOUtils::nodata;
	
	const double Z = (90.-sun_elev)*Cst::to_rad;
	const double cosZ = cos(Z);

	if (cosZ==0.) {
		return 1.e12;
	} else {
		const double radiation = H_radiation / cos(Z);
		return radiation;
	}
}

/**
 * @brief Return the current solar time.
 * Since the Sun reaches its zenith at a time different than the local noon, the solar noon
 * does not happen at 12:00 local time. This defines a solar time that has a negative or positive offset
 * with the local time, depending on the seasons (see http://www.jaloxa.eu/resources/daylighting/sunpath.shtml).
 * @return actual solar time
 */
double SunTrajectory::getSolarTime() const
{
	if (julian_gmt!=IOUtils::nodata && TZ!=IOUtils::nodata && SolarNoon!=IOUtils::nodata)
		return julian_gmt + (SolarNoon - .5) + TZ*1./24.;
	else
		return IOUtils::nodata;
}

/**
 * @brief Return the current solar time of day.
 * Since the Sun reaches its zenith at a time different than the local noon, the solar noon
 * does not happen at 12:00 local time. This defines a solar time that has a negative or positive offset
 * with the local time, depending on the seasons (see http://www.jaloxa.eu/resources/daylighting/sunpath.shtml).
 * @return actual solar time of day, ie between 0 and 24
 */
double SunTrajectory::getSolarTimeOfDay() const
{
	if (julian_gmt!=IOUtils::nodata && TZ!=IOUtils::nodata && SolarNoon!=IOUtils::nodata) {
		const double timeOfDay = Optim::fracPart(julian_gmt + .5 + TZ*1./24.); //between 0 and 1
		const double SolarOffset = SolarNoon - .5;
		const double SolarTimeOfDay = timeOfDay + SolarOffset;
		if (SolarTimeOfDay>1) return SolarTimeOfDay - 1.;
		if (SolarTimeOfDay<0) return 1. + SolarTimeOfDay;
		return SolarTimeOfDay;
	} else
		return IOUtils::nodata;
}

/**
 * @brief Return the date of easter for the given year.
 * @details This is the method given in Meeus citing Butcher's Ecclesiastical Calendar.
 * It is valid for all years after October 1582 (since this was the year of introduction of the  
 * Gregorian calendar, suppressing 10 days (jumping from 4.10 to 15.10) compared to 
 * the previous Julian calendar).
 * @param[in] year the year of interest
 * @param[out] month the month of Easter
 * @param[out] day the day of Easter
 */
void SunTrajectory::getEaster(const int& year, int &month, int &day)
{
	const int a = year % 19;
	const int b = year / 100;
	const int c = year % 100;
	const int d = b / 4;
	const int e = b % 4;
	const int f = (b+8) / 25;
	const int g = (b-f+1) / 3;
	const int h = (19*a+b-d-g+15) % 30;
	const int i = c / 4;
	const int k = c % 4;
	const int l = (32+2*e+2*i-h-k) % 7;
	const int m = (a+11*h+22*l) / 451;
	const int n = (h+l-7*m+114) / 31;
	const int p = (h+l-7*m+114) % 31;
	
	month = n;
	day = p+1;
}

const std::string SunTrajectory::toString() const
{
	std::ostringstream os;
	os << "<SunTrajectory>\n";
	os << std::fixed << std::setprecision(4);
	os << "Julian\t" << julian_gmt + TZ*1./24. << " TZ=" << TZ << "\n";
	os << "SolarTimeOfDay\t" << getSolarTimeOfDay() << "\n";
	os << "Lat/Long\t" << std::setw(7) << latitude << "° " << std::setw(7) << longitude << "°\n";
	os << "Ecc. corr.\t" << std::setprecision(4) << std::setw(7) << eccentricityEarth << "°\n";
	os << "Hour Angle\t" << std::setprecision(4) << std::setw(7) << HourAngle << "°\n";
	os << std::setprecision(2);
	os << "Azi./Elev.\t" << std::setw(7)<< SolarAzimuthAngle << "° " << std::setw(7) << SolarElevation << "°\n";
	os << "RA/decl.\t" << std::setw(7) << SunRightAscension << "° " << std::setw(7) << SunDeclination << "°\n";
	os << "Sunrise\t\t" << Date::printFractionalDay(SunRise) << "\n";
	os << "SolarNoon\t" << Date::printFractionalDay(SolarNoon) << "\n";
	os << "Sunset\t\t" << Date::printFractionalDay(SunSet) << "\n";
	os << "Daylight\t" << Date::printFractionalDay(SunlightDuration/(60.*24.)) << "\n";
	os << "</SunTrajectory>\n";
	os << std::setfill(' ');
	return os.str();
}

} //end namespace

namespace mio {

//Class SunMeeus
SunMeeus::SunMeeus() : SunTrajectory(), SolarElevationAtm(IOUtils::nodata) {}

SunMeeus::SunMeeus(const double& i_latitude, const double& i_longitude) : SunTrajectory(i_latitude, i_longitude), SolarElevationAtm(IOUtils::nodata) {}

SunMeeus::SunMeeus(const double& i_latitude, const double& i_longitude, const double& i_julian, const double& i_TZ) : SunTrajectory(i_latitude, i_longitude), SolarElevationAtm(IOUtils::nodata)
{
	setAll(i_latitude, i_longitude, i_julian, i_TZ);
}

void SunMeeus::private_init()
{
	SolarAzimuthAngle = SolarElevation = IOUtils::nodata;
	SolarElevationAtm = IOUtils::nodata;
	eccentricityEarth = IOUtils::nodata;
	SunRise = SunSet = SunlightDuration = IOUtils::nodata;
	SolarNoon = IOUtils::nodata;
	SunRightAscension = SunDeclination = IOUtils::nodata;
	HourAngle = IOUtils::nodata;
}


void SunMeeus::setDate(const double& i_julian, const double& i_TZ)
{
	TZ = i_TZ;
	julian_gmt = i_julian - TZ*1./24.;
	private_init();
	if (latitude!=IOUtils::nodata && longitude!=IOUtils::nodata) {
		update();
	}
}

void SunMeeus::setLatLon(const double& i_latitude, const double& i_longitude)
{
	latitude = i_latitude;
	longitude = i_longitude;
	private_init();
	if (julian_gmt!=IOUtils::nodata && TZ!=IOUtils::nodata) {
		update();
	}
}

void SunMeeus::setAll(const double& i_latitude, const double& i_longitude, const double& i_julian, const double& i_TZ)
{
	TZ = i_TZ;
	latitude = i_latitude;
	longitude = i_longitude;
	julian_gmt = i_julian - TZ*1./24.;
	update();
}

void SunMeeus::reset() {;
	julian_gmt = TZ = IOUtils::nodata;
	latitude = longitude = IOUtils::nodata;
	private_init();
}

void SunMeeus::getHorizontalCoordinates(double& azimuth, double& elevation) const {
	double eccentricity;
	getHorizontalCoordinates(azimuth, elevation, eccentricity);
}

void SunMeeus::getHorizontalCoordinates(double& azimuth, double& elevation, double& eccentricity) const {
	if (julian_gmt!=IOUtils::nodata && TZ!=IOUtils::nodata && latitude!=IOUtils::nodata && longitude!=IOUtils::nodata) {
		azimuth = SolarAzimuthAngle;
		elevation = SolarElevation; //this is the TRUE elevation, not the apparent!
		eccentricity = eccentricityEarth;
	} else {
		throw InvalidArgumentException("Please set ALL required parameters to get Sun's position!!", AT);
	}
}

void SunMeeus::getDaylight(double& sunrise, double& sunset, double& MeeusDaylight) {
	if (julian_gmt!=IOUtils::nodata && TZ!=IOUtils::nodata && latitude!=IOUtils::nodata && longitude!=IOUtils::nodata) {
		sunrise = SunRise;
		sunset = SunSet;
		MeeusDaylight = SunlightDuration/(60.*24.);
	} else {
		throw InvalidArgumentException("Please set ALL required parameters to get Sun's position!!", AT);
	}
}

void SunMeeus::getEquatorialCoordinates(double& right_ascension, double& declination) {
	if (julian_gmt!=IOUtils::nodata && TZ!=IOUtils::nodata && latitude!=IOUtils::nodata && longitude!=IOUtils::nodata) {
		right_ascension = SunRightAscension;
		declination = SunDeclination;
	} else {
		throw InvalidArgumentException("Please set ALL required parameters to get Sun's position!!", AT);
	}
}

void SunMeeus::getEquatorialSunVector(double& sunx, double& suny, double& sunz) {
	double azi_Sacw;

	// Convert to angle measured from South, counterclockwise (rad)
	if ( SolarAzimuthAngle <= 90. ) {
		azi_Sacw = Cst::PI - SolarAzimuthAngle*Cst::to_rad;
	} else {
		azi_Sacw = 3.*Cst::PI - SolarAzimuthAngle*Cst::to_rad;
	}

	// derived as shown in Funk (1984) p. 107, but for a y-coordinate increasing northwards
	sunx =  sin( azi_Sacw ) * cos( SolarElevation ); // left handed coordinate system
	suny = -cos( azi_Sacw ) * cos( SolarElevation ); // inverse y-coordinate because of reference system
	sunz =  sin( SolarElevation );
}

double SunMeeus::SideralToLocal(const double& JD)
{
	const double T = (JD-2451545.)/36525.;
	double theta_0 = (280.46061837 + 360.98564736629*(JD-2451545.) + 0.000387933*T*T - T*T*T/38710000.); //in degrees
	theta_0 = fmod(fmod(theta_0, 360.)+360., 360.) / (15.*24); //in hours and then in days
	return theta_0;
}

void SunMeeus::update() {
	//calculating various time representations
	const double julian = julian_gmt;
	const double lst_TZ = longitude*1./15.;
	const double gmt_time = ((julian + 0.5) - floor(julian + 0.5))*24.; //in hours
	const double lst_hours=(gmt_time+longitude*1./15.); //Local Sidereal Time
	const double julian_century = (julian - 2451545.)/36525.;

	//parameters of the orbits of the Earth and Sun
	const double geomMeanLongSun = fmod( 280.46646 + julian_century*(36000.76983 + julian_century*0.0003032) , 360.);
	const double geomMeanAnomSun = 357.52911 + julian_century*(35999.05029 - 0.0001537*julian_century);
	eccentricityEarth = 0.016708634 - julian_century*(0.000042037 + 0.0000001267*julian_century);
	const double SunEqOfCtr =   sin(1.*geomMeanAnomSun*Cst::to_rad)*( 1.914602-julian_century*(0.004817+0.000014*julian_century))
	             + sin(2.*geomMeanAnomSun*Cst::to_rad)*(0.019993 - 0.000101*julian_century)
	             + sin(3.*geomMeanAnomSun*Cst::to_rad)*(0.000289);

	const double SunTrueLong = geomMeanLongSun + SunEqOfCtr;
	/*const double SunTrueAnom = geomMeanAnomSun + SunEqOfCtr;*/
	/*const double SunRadVector =   ( 1.000001018 * (1. - eccentricityEarth*eccentricityEarth) )
	               / ( 1. + eccentricityEarth*cos(SunTrueAnom*Cst::to_rad) );*/

	const double SunAppLong = SunTrueLong - 0.00569 - 0.00478*sin( (125.04-1934.136*julian_century)*Cst::to_rad );
	const double MeanObliqueEcl = 23. + (26.+
	                 (21.448-julian_century*(46.815+julian_century*(0.00059-julian_century*0.001813))) / 60. )
	                 / 60.;

	const double ObliqueCorr = MeanObliqueEcl + 0.00256*cos( (125.04-1934.136*julian_century)*Cst::to_rad );

	//Sun's position in the equatorial coordinate system
	SunRightAscension = atan2(
	                    cos(SunAppLong*Cst::to_rad) ,
	                    cos(ObliqueCorr*Cst::to_rad) * sin(SunAppLong*Cst::to_rad)
	                    ) * Cst::to_deg;

	SunDeclination = asin( sin(ObliqueCorr*Cst::to_rad) * sin(SunAppLong*Cst::to_rad) ) * Cst::to_deg;

	//time calculations
	const double var_y = tan( 0.5*ObliqueCorr*Cst::to_rad ) * tan( 0.5*ObliqueCorr*Cst::to_rad );
	const double EquationOfTime = 4. * ( var_y*sin(2.*geomMeanLongSun*Cst::to_rad)
	                 - 2.*eccentricityEarth*sin(geomMeanAnomSun*Cst::to_rad) +
	                 4.*eccentricityEarth*var_y*sin(geomMeanAnomSun*Cst::to_rad) * cos(2.*geomMeanLongSun*Cst::to_rad)
	                 - 0.5*var_y*var_y*sin(4.*geomMeanLongSun*Cst::to_rad)
	                 - 1.25*eccentricityEarth*eccentricityEarth*sin(2.*geomMeanAnomSun*Cst::to_rad)
	                 )*Cst::to_deg;

	SolarNoon = (720. - 4.*longitude - EquationOfTime + TZ*60.)/1440.; //in days, in LST time

	const double cos_HAsunrise = cos(90.833*Cst::to_rad) / (cos(latitude*Cst::to_rad) * cos(SunDeclination*Cst::to_rad))
	             - tan(latitude*Cst::to_rad)*tan(SunDeclination*Cst::to_rad);

	if (cos_HAsunrise>=-1. && cos_HAsunrise<=1.) {
		const double HA_sunrise = acos( cos_HAsunrise ) * Cst::to_deg;
		SunRise = SolarNoon - HA_sunrise*4./1440.; //in days, in LST
		SunSet = SolarNoon + HA_sunrise*4./1440.; //in days, in LST
		SunlightDuration = 8.*HA_sunrise;
	} else if (cos_HAsunrise<-1.) { //the sun never sets
		SunRise = IOUtils::nodata;
		SunSet = IOUtils::nodata;
		SunlightDuration = 24.;
	} else if (cos_HAsunrise>1.) { //the sun never rises
		SunRise = IOUtils::nodata;
		SunSet = IOUtils::nodata;
		SunlightDuration = 0.;
	}

	//Sun's position in the horizontal coordinate system (see http://en.wikipedia.org/wiki/Horizontal_coordinate_system)
	const double TrueSolarTime = fmod( lst_hours*60. + EquationOfTime + 4.*longitude - 60.*lst_TZ , 1440. ); //in LST time
	if ( TrueSolarTime<0. )
		HourAngle = TrueSolarTime/4.+180.;
	else
		HourAngle = TrueSolarTime/4.-180.;

	const double SolarZenithAngle = acos(
	                   sin(latitude*Cst::to_rad) * sin(SunDeclination*Cst::to_rad)
	                   + cos(latitude*Cst::to_rad) * cos(SunDeclination*Cst::to_rad) * cos(HourAngle*Cst::to_rad)
	                   )*Cst::to_deg;
	
	SolarElevation = 90. - SolarZenithAngle;

	double AtmosphericRefraction = 0.;
	if ( SolarElevation<=85. ) {
		if ( SolarElevation>5. ) {
			AtmosphericRefraction = 58.1 / tan(SolarElevation*Cst::to_rad) - 0.07 / pow( tan(SolarElevation*Cst::to_rad) , 3 ) + 0.000086/pow( tan(SolarElevation*Cst::to_rad), 5);
		} else {
			if ( SolarElevation>-0.575 ) {
				AtmosphericRefraction = 1735. + SolarElevation*(-518.2 + SolarElevation*(103.4 + SolarElevation*(-12.79 + SolarElevation*0.711)));
			} else {
				AtmosphericRefraction = -20.772 / tan( SolarElevation*Cst::to_rad );
			}
		}
		AtmosphericRefraction /= 3600.;
	}

	SolarElevationAtm = SolarElevation + AtmosphericRefraction; //correction for the effects of the atmosphere
	const double cos_SAA = (sin(latitude*Cst::to_rad)*cos(SolarZenithAngle*Cst::to_rad) - sin(SunDeclination*Cst::to_rad)) /
	                       (cos(latitude*Cst::to_rad)*sin(SolarZenithAngle*Cst::to_rad));
	if ( HourAngle>0. ) {
		SolarAzimuthAngle = fmod( acos( std::min( 1., std::max(-1., cos_SAA) ) )*Cst::to_deg + 180., 360. );
	} else {
		SolarAzimuthAngle = fmod( 540. - acos( std::min( 1., std::max(-1., cos_SAA) ) )*Cst::to_deg,  360. );
	}
}

} //namespace
