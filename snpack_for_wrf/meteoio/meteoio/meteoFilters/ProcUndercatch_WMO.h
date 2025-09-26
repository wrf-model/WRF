/***********************************************************************************/
/*  Copyright 2012 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef PROCUNDERCATCH_WMO_H
#define PROCUNDERCATCH_WMO_H

#include <meteoio/meteoFilters/ProcessingBlock.h>
#include <vector>
#include <string>

namespace mio {

/**
 * @class  ProcUndercatch_WMO
 * @ingroup processing
 * @brief Correct precipitation for undercatch in winter conditions.
 * @details
 * This implements the standard methods for precipitation correction as described in
 * <i>"WMO Solid Precipitation Measurement Intercomparison"</i>, B. Goodison, P. Louie and D. Yang, <b>872</b>, 1998 as well as
 * the overview given by <i>"Literature Study on the Correction of Precipitation Measurements"</i>, Annette Wagner, 2009.
 * The correction parameters for the shielded Hellmann gauge (German version) are from <i>"Wind-induced Precipitation Undercatch
of the Hellmann Gauges"</i>, Daqing Yang et al, Nordic Hydrology, <b>30</b>, 1999, pp 57-80 while the correction for Japanese RT-3 gauges comes from
* <i>"Performance of Japanese precipitation gauges in winter"</i>, K. Yokoyama, H. Ohno, Y. Kominami, S. Inoue and T. Kawakata, Seppyo, <b>65</b>, 2003, pp 303-316 (in Japanese with English summary). The correction for the Chinese Standard Precipitation Gauge is from <i>"A Bias-Corrected Precipitation Climatology for China"</i>, B. Ye, Y. Daqing, D. Yongjian, H. Tianding, K. Toshio, 2004, Journal of Hydrometeorology, <b>5</b>, 1147–1160.
 *
 * These correction methods process pure snow and mixed precipitation differently, with the following thresholds:
 * - pure snow below -2 C
 * - mixed precipitation between -2 and +2 C
 * - pure rain above 2 C
 *
 * They also depend on the usage of a shield around the gauge as well as the type of rain gauge that does the measurements,
 * therefore this type must be specified as an argument. The coefficients are not always available both for shielded and
 * unshielded gauges, so most of the time only one variation will be available and is specified below.
 *
 * The following arguments are supported:
 * - TYPE: this gives the rain gauge and shield type. It must be one of:
 *     - cst - this applies a constant factor to the precipitation (the solid and mixed precipitation factor must be provided, see below);
 *     - Nipher - National standard rain gauge in Canada, shielded;
 *     - Tretyakov - Designed in USSR in the 1950s, deployed by some national networks in ex-USSR territories, shielded;
 *     - US8sh - US national 8\" rain gauge, shielded (Alter shield);
 *     - US8unsh - US national 8\" rain gauge, unshielded;
 *     - RT3_Jp - Japanese network RT-3 rain gauge. This uses an ad-hoc rain/snow splitting method;
 *     - Cspg - China Standard Precipitation Gauge, unshielded;
 *     - Geonorsh - Geonor rain gauge with Alter shield. The mixed precipitation is computed according to the same principles as in (Ye, 2004);
 *     - Hellmann - the most widely used rain gauge in the world, with some country specific variations, unshielded;
 *     - Hellmannsh - Hellmann rain gauge with shield, mixed precipitation from a fit on the published data;
 * - SNOW: constant factor for solid precipitation for the CST type (mandatory);
 * - MIXED: constant factor for mixed precipitation for the CST type (mandatory);
 * - T_SNOW: solid precipitation temperature threshold for the CST type (in K, optional);
 * - T_RAIN: liquid precipitation temperature threshold for the CST type (in K, optional);
 *
 * @code
 * PSUM::filter1     = undercatch_wmo
 * PSUM::arg1::type  = cst
 * PSUM::arg1::snow  = 1.3
 * PSUM::arg1::mixed = 1.1
 * @endcode
 */

class ProcUndercatch_WMO : public ProcessingBlock {
	public:
		ProcUndercatch_WMO(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);

	private:
		typedef enum SENSOR_TYPE {
			cst,
			nipher,
			tretyakov,
			us8sh,
			us8unsh,
			rt3_jp,
			cspg,
			geonorsh,
			hellmann,
			hellmannsh
		} sensor_type;

		typedef enum PRECIP_TYPE {
			rain,
			mixed,
			snow
		} precip_type;

		void parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs);

		sensor_type type;
		double factor_snow, factor_mixed;
		double Tsnow, Train;
		static const double Tsnow_WMO, Train_WMO;
};

} //end namespace

#endif
