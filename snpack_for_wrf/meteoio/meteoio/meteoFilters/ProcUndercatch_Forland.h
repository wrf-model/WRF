/***********************************************************************************/
/*  Copyright 2013 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef PROCUNDERCATCH_FORLAND_H
#define PROCUNDERCATCH_FORLAND_H

#include <meteoio/meteoFilters/ProcessingBlock.h>
#include <vector>
#include <string>

namespace mio {

/**
 * @class  ProcUndercatch_Forland
 * @ingroup processing
 * @brief Correct precipitation for undercatch in winter conditions.
 * @details
 * This implements the method for precipitation correction as described in <i>"Manual for operational correction of Nordic precipitation data"</i>, E. Førland, P. Allerup, B. Dahlström, E. Elomaa, T. Jónsson, H. Madsen, P. Perälä Rissanen, H. Vedin, and F. Vejen, 1996, Tech. Rep. <b>24/96</b>, Norske Meteorologiske Institutt.
 * Specific coefficients have been calculated by N. Wever for the Davos Weisflujoch experimental field. The type of gauge must be
 * provided with the TYPE argument as one of:
 * - wfj - shielded Hellmann gauge as used at Weissflujoch (Switzerland)
 * - Hellmann - unshielded
 * - Swedish - Nipher shield
 * - Norvegian - Nipher shield
 * - Finnish - Tretyakov
 * - Tretyakov - Tretyakov
 * - Belfort - Alter shield
 * - Geonor - Alter shield
 *
 * @code
 * PSUM::filter1    = undercatch_forland
 * PSUM::arg1::type = wfj
 * @endcode
 */

class ProcUndercatch_Forland : public ProcessingBlock {
	public:
		ProcUndercatch_Forland(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);

	private:
		typedef enum SENSOR_TYPE {
			wfj,
			hellmann,
			swedish,
			norvegian,
			finnish,
			tretyakov,
			belfort,
			geonor
		} sensor_type;

		double solidPrecipitation(double TA, double VW) const;
		double liquidPrecipitation(const double& Pint, double VW) const;
		void parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs);

		sensor_type type;
		static const double Tsnow_WMO, Train_WMO;
};

} //end namespace

#endif
