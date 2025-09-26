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
#ifndef SUN_H
#define SUN_H

#include <meteoio/meteoLaws/Suntrajectory.h>

namespace mio {

/**
 * @class SunObject
 * @brief A class to calculate Solar radiation characteristics
 * This is largely based on M. Iqbal, <i>"An introduction to solar radiation"</i>, 1983, Academic Press,  ISBN: 0-12-373750-8.
 * The Sun's position is provided by the SunTrajectory class (currently the only implemented algorithm is Meeus).
 * All units are SI. See http://www.meteoexploration.com/products/solarcalc.php for a validation calculator.
 * @ingroup meteoLaws
 * @author Mathias Bavay
 * @date   2010-06-10
 */
class SunObject {
	public:
		/// this enum provides the different position algorithms available
		typedef enum POSITION_ALGO {
			MEEUS ///<Jean Meeus' algorithm (Meeus, j. "Astronomical Algorithms", second edition, 1998, Willmann-Bell, Inc., Richmond, VA, USA)
		} position_algo;

		SunObject(const position_algo alg=MEEUS);
		SunObject(const double& i_latitude, const double& i_longitude, const double& i_altitude);
		SunObject(const double& i_latitude, const double& i_longitude, const double& i_altitude, const double& i_julian, const double& i_TZ=0.);

		//local julian date and timezone
		void setDate(const double& i_julian, const double& i_TZ=0.);
		void setLatLon(const double& i_latitude, const double& i_longitude, const double& i_altitude);
		void resetAltitude(const double& i_altitude);
		void setElevationThresh(const double& i_elevation_threshold);

		void calculateRadiation(const double& ta, const double& rh, double pressure, const double& ground_albedo);
		void calculateRadiation(const double& ta, const double& rh, const double& mean_albedo);
		void getBeamRadiation(double& R_toa, double& R_direct, double& R_diffuse) const;
		void getHorizontalRadiation(double& R_toa, double& R_direct, double& R_diffuse) const;
		void getSlopeRadiation(const double& slope_azi, const double& slope_elev, double& R_toa, double& R_direct, double& R_diffuse) const;
		double getElevationThresh() const {return elevation_threshold;}

		double getSplittingBoland(const double& iswr_modeled, const double& iswr_measured, const double& t) const;
		double getSplitting(const double& iswr_modeled, const double& iswr_measured) const;
		double getSplitting(const double& iswr_measured) const;
		double getCorrectionFactor(const double& iswr_measured, double &Md, bool &day, bool &night) const;
		double getCorrectionFactor(const double& iswr_measured) const;

		double getJulian(const double& o_TZ) const {return (julian_gmt+o_TZ*1./24.);}
		const std::string toString() const;
		
		//SunTrajectory position;
		SunMeeus position;
		static const double elevation_dftlThreshold, rad_threshold;
		
	private:
		void getBeamPotential(const double& sun_elevation, const double& Eccentricity_corr,
		                      const double& ta, const double& rh, const double& pressure, const double& mean_albedo,
		                      double& R_toa, double& R_direct, double& R_diffuse) const;
		void getClearSky(const double& sun_elevation, const double& R_toa,
		                 const double& ta, const double& rh, const double& pressure, const double& ground_albedo,
		                 double& R_direct, double& R_diffuse) const;

	private:
		double julian_gmt, TZ;
		double latitude, longitude, altitude;
		double elevation_threshold;
		double beam_toa, beam_direct, beam_diffuse;
};

} //end namespace

#endif
