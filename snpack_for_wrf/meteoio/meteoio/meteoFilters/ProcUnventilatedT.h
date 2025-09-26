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
#ifndef UNVENTILATED_T_H
#define UNVENTILATED_T_H

#include <meteoio/meteoFilters/ProcessingBlock.h>
#include <vector>
#include <string>

namespace mio {

/**
 * @class  ProcUnventilatedT
 * @ingroup processing
 * @brief Filters and corrects temperatures from unventilated sensor.
 * @details
 * This either deletes all air temperature values when the wind speed is below a given threshold or corrects the air temperature data according to
 * <i>"Air Temperature Measurement Errors in Naturally Ventilated Radiation Shields"</i>, Reina Nakamura, L. Mahrt, J. Atmos. Oceanic Technol., <b>22</b>, 2005, pp 1046–1058
 * with an albedo dependency as introduced in <i>"Albedo effect on radiative errors in air temperature measurements"</i>, H. Huwald, C. W. Higgins, M.-O. Boldi, E. Bou-Zeid, M. Lehning, and M. B. Parlange, Water Resour. Res., <b>45</b>, W08431, 2009.
 *
 * When correcting the data, the albedo of the surroundings is required as well as the incoming or reflected short wave radiation. If a snow height is available, a generic
 * snow albedo is used when the snow height is above a given threshold, otherwise a soil albedo. This soil albedo can be provided as an option. Moreover, two corrections
 * are available: either HUWALD (Huwald, 2009)[default] or NAKAMURA (Nakamura, 2005).
 * When simply deleting all suspicious air temperatures (SUPPR), the wind speed threshold must be provided.
 *
 * Supported arguments:
 *  - TYPE: either NAKAMURA or HUWALD or SUPPR (mandatory);
 *  - THRESH_VW: wind velocity threshold when using the SUPPR type;
 *  - SOIL_ALB: soild albedo when using NAKAMURA or HUWALD (optional).
 *
 * @note This filter can ONLY be applied to air temperatures. Moreover, since it <i>might</i> depend on the radiation shield, it is highly recommended to do some tests (ie. comparisons between ventillated and unventillated sensors) before using it on a new sensor type. Hopefully a new paper would come and clarify its usability per sensor types...
 *
 * @code
 * #using (Huwald, 2009) with default soil albedo
 * TA::filter2   = unventilated_T
 *
 * #using (Nakamura, 2005) with specified soil albedo
 * TA::filter2        = unventilated_T
 * TA::arg2::type     = Nakamura
 * TA::arg2::soil_alb = 0.23
 *
 * #simply deleting all values when VW<3. m/s
 * TA::filter2         = unventilated_T
 * TA::arg2::type      = suppr
 * TA::arg2::thresh_vw = 3.
 * @endcode
 */

class ProcUnventilatedT : public ProcessingBlock {
	public:
		ProcUnventilatedT(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);

	private:
		void filterTA(const unsigned int& param, std::vector<MeteoData>& ovec) const;
		void correctTA(const unsigned int& param, std::vector<MeteoData>& ovec) const;
		void parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs);

		double usr_albedo, usr_vw_thresh;
		static const double dflt_albedo, vw_thresh;
		bool nakamura; //use Nakamura or Huwald model

};

} //end namespace

#endif
