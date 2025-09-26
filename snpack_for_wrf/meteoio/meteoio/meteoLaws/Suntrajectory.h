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
#ifndef SUNTAJECTORY_H
#define SUNTAJECTORY_H

namespace mio {

/**
 * @class SunTrajectory
 * @brief A class to calculate the Sun's position
 * This class is purely virtual.
 * @ingroup meteoLaws
 * @author Mathias Bavay
 * @date   2010-06-10
 */
class SunTrajectory {
	public:
		SunTrajectory();
		SunTrajectory(const double& i_latitude, const double& i_longitude);

		virtual ~SunTrajectory() {}

		/** @brief Set the date and time
		* if no timezone is specified, GMT is assumed
		* @param i_julian julian date in the time zone of interest
		* @param i_TZ time zone
		*/
		virtual void setDate(const double& i_julian, const double& i_TZ=0.)=0;
		virtual void setLatLon(const double& i_latitude, const double& i_longitude)=0;
		virtual void setAll(const double& i_latitude, const double& i_longitude, const double& i_julian, const double& i_TZ=0.)=0;
		virtual void reset()=0;

		///(see http://en.wikipedia.org/wiki/Horizontal_coordinate_system)
		///please remember that zenith_angle = 90 - elevation
		virtual void getHorizontalCoordinates(double& azimuth, double& elevation) const=0;
		virtual void getHorizontalCoordinates(double& azimuth, double& elevation, double& eccentricity) const=0;
		virtual void getDaylight(double& sunrise, double& sunset, double& daylight)=0;
		virtual double getSolarTime() const;
		virtual double getSolarTimeOfDay() const;
		static void getEaster(const int& year, int &month, int &day);

		///(http://en.wikipedia.org/wiki/Equatorial_coordinate_system)
		virtual void getEquatorialCoordinates(double& right_ascension, double& declination)=0;

		///radiation projection methods
		double getAngleOfIncidence(const double& slope_azi, const double& slope_elev) const;
		double getRadiationOnHorizontal(const double& radiation) const;
		double getRadiationOnSlope(const double& slope_azi, const double& slope_elev, const double& radiation) const;
		double getHorizontalOnSlope(const double& slope_azi, const double& slope_elev, const double& H_radiation, const double& elev_threshold=5.) const;

		///static helper for radiation projection
		static double getAngleOfIncidence(const double& sun_azi, const double& sun_elev,
		                                  const double& slope_azi, const double& slope_elev);
		static double projectHorizontalToSlope(const double& sun_azi, const double& sun_elev,
		                                       const double& slope_azi, const double& slope_elev, const double& H_radiation, const double& elev_threshold=5.);
		static double projectSlopeToHorizontal(const double& sun_azi, const double& sun_elev,
		                                       const double& slope_azi, const double& slope_elev, const double& S_radiation);
		static double projectHorizontalToBeam(const double& sun_elev, const double& H_radiation);

		const std::string toString() const;

	protected:
		virtual void update()=0;

	protected:
		double julian_gmt, TZ;
		double latitude, longitude;
		double SolarAzimuthAngle, SolarElevation; ///>this is the TRUE solar elevation, not the apparent one
		double eccentricityEarth;
		double SunRise, SunSet, SunlightDuration, SolarNoon;
		double SunRightAscension, SunDeclination;
		double HourAngle;
		static const double nodata;
};

/**
 * @class SunMeeus
 * @brief Calculate the Sun's position based on the Meeus algorithm.
 * See J. Meeus, <i>"Astronomical Algorithms"</i>, 1998, 2nd ed, Willmann-Bell, Inc., Richmond, VA, USA, ISBN 0-943396-61-1.
 * A useful reference is also NOAA's spreadsheet at http://www.esrl.noaa.gov/gmd/grad/solcalc/calcdetails.html or
 * http://energyworksus.com/solar_installation_position.html for comparing positional data. The technical report
 * I. Reda, A. Andreas, <i>"Solar Position Algorithm for Solar Radiation Applications"</i>, 2008, NREL/TP-560-34302
 * also contains an alternative algorithm and very detailed validation data sets.
 *
 * @ingroup meteoLaws
 * @author Mathias Bavay
 * @date   2010-06-10
 */
class SunMeeus : public SunTrajectory {
	public:
		SunMeeus();
		~SunMeeus() {}
		SunMeeus(const double& i_latitude, const double& i_longitude);
		SunMeeus(const double& i_latitude, const double& i_longitude, const double& i_julian, const double& i_TZ=0.);

		void setDate(const double& i_julian, const double& i_TZ=0.);
		void setLatLon(const double& i_latitude, const double& i_longitude);
		void setAll(const double& i_latitude, const double& i_longitude, const double& i_julian, const double& i_TZ=0.);
		void reset();

		void getHorizontalCoordinates(double& azimuth, double& elevation) const;
		void getHorizontalCoordinates(double& azimuth, double& elevation, double& eccentricity) const;
		void getDaylight(double& sunrise, double& sunset, double& MeeusDaylight);
		void getEquatorialSunVector(double& sunx, double& suny, double& sunz);
		void getEquatorialCoordinates(double& right_ascension, double& declination);
		
		static double SideralToLocal(const double& JD);
	private:
		void private_init();
		void update();

	private:
		double SolarElevationAtm;

};

} //end namespace

#endif
