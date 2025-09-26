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
#ifndef ATMOSPHERE_H
#define ATMOSPHERE_H

#include <meteoio/IOUtils.h>

namespace mio {

/**
 * @class Atmosphere
 * @brief A class to calculate the atmosphere's parameters
 *
 * @ingroup meteoLaws
 * @author Mathias Bavay
 * @date   2010-06-10
 */
class Atmosphere {
	public:
		static double gravity(const double& altitude, const double& latitude);
		static double stdAirPressure(const double& altitude);
		static double reducedAirPressure(const double& pressure, const double& altitude, const double& latitude);
		static double stdDryAirDensity(const double& altitude, const double& temperature);
		static double vaporSaturationPressure(const double& T);
		static double vaporSaturationPressureWater(const double& T);
		static double virtualTemperatureFactor(const double& e, const double& p);
		static double waterVaporDensity(const double& Temperature, const double& VaporPressure);
		static double wetBulbTemperature(const double& T, const double& RH, const double& altitude);
		static double blackGlobeTemperature(const double& TA, const double& RH, const double& VW, const double& iswr_dir, const double& iswr_diff, const double& cos_Z);
		static double windLogProfile(const double& v_ref, const double& z_ref, const double& z, const double& z0=0.03);

		static double windChill(const double& TA, const double& VW);
		static double heatIndex(const double& TA, const double& RH);
		static double WBGT_index(const double& TA, const double& RH, const double& VW, const double& iswr_dir, const double& iswr_diff, const double& cos_Z, const double& altitude);

		//clear sky emissivity
		static double Brutsaert_emissivity(const double& RH, const double& TA);
		static double Brutsaert_ilwr(const double& RH, const double& TA);
		static double Dilley_emissivity(const double& RH, const double& TA);
		static double Dilley_ilwr(const double& RH, const double& TA);
		static double Prata_emissivity(const double& RH, const double& TA);
		static double Prata_ilwr(const double& RH, const double& TA);
		static double Clark_emissivity(const double& RH, const double& TA);
		static double Clark_ilwr(const double& RH, const double& TA);
		static double Tang_emissivity(const double& RH, const double& TA);
		static double Tang_ilwr(const double& RH, const double& TA);
		static double Idso_emissivity(const double& RH, const double& TA);
		static double Idso_ilwr(const double& RH, const double& TA);

		//cloudy sky emissivity
		static double Omstedt_emissivity(const double& RH, const double& TA, const double& cloudiness);
		static double Omstedt_ilwr(const double& RH, const double& TA, const double& cloudiness);
		static double Konzelmann_emissivity(const double& RH, const double& TA, const double& cloudiness);
		static double Konzelmann_ilwr(const double& RH, const double& TA, const double& cloudiness);
		static double Carmona_emissivity(const double& RH, const double& TA, const double& cloudiness);
		static double Carmona_ilwr(const double& RH, const double& TA, const double& cloudiness);
		static double Crawford_ilwr(const double& RH, const double& TA, const double& iswr_meas, const double& iswr_clear_sky, const unsigned char& month, const double& cloudiness=IOUtils::nodata);
		static double Crawford_ilwr(const double& lat, const double& lon, const double& altitude,
		                            const double& julian, const double& TZ,
		                            const double& RH, const double& TA, const double& ISWR, const double& cloudiness=IOUtils::nodata);
		static double Unsworth_ilwr(const double& RH, const double& TA, const double& iswr_meas, const double& iswr_clear_sky, const double& cloudiness=IOUtils::nodata);
		static double Unsworth_ilwr(const double& lat, const double& lon, const double& altitude,
		                            const double& julian, const double& TZ,
		                            const double& RH, const double& TA, const double& ISWR, const double& cloudiness=IOUtils::nodata);
		static double Kasten_clearness(const double& cloudiness);
		static double Kasten_cloudiness(const double& solarIndex);
		static double ILWR_parametrized(const double& lat, const double& lon, const double& altitude,
		                                const double& julian, const double& TZ,
		                                const double& RH, const double& TA, const double& ISWR, const double& cloudiness=IOUtils::nodata);

		static double RhtoDewPoint(double RH, double TA, const bool& force_water);
		static double DewPointtoRh(double TD, double TA, const bool& force_water);
		static double specToRelHumidity(const double& altitude, const double& TA, const double& qi);
		static double relToSpecHumidity(const double& altitude, const double& TA, const double& RH);

		static double blkBody_Emissivity(const double& lwr, const double& T);
		static double blkBody_Radiation(const double& ea, const double& T);
};

} //end namespace

#endif
