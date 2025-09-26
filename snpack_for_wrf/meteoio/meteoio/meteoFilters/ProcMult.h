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
#ifndef PROCMULT_H
#define PROCMULT_H

#include <meteoio/meteoFilters/ProcAdd.h>
#include <vector>
#include <string>

namespace mio {

/**
 * @class  ProcMult
 * @ingroup processing
 * @brief Multiply values.
 * @details
 * This multiplies all values by a given factor. Either a fixed value is given as single argument or a period
 * (hourly/daily/monthly) as well as a filename (and absolute or relative path) containing the factors to apply.
 * This file must contain in the first column the indices (months from 1 to 12 or days from 1 to 366 or hours from 0 to 23)
 * and the matching factor in the second column (<a href="http://www.cplusplus.com/reference/cctype/isspace/">whitespace</a> delimited).
 * Comments following the same syntax as in the ini file are accepted, missing indices are treated as 1. It is also possible to multiply by a random  noise.
 *
 * It takes the following arguments:
 *  - TYPE: either CST (multiply by a constant) or FILE (multiply by values form a given file) or NOISE (multiply by random values).
 *  - when adding a constant:
 *      - CST: a constant to add to the data (optional);
 *  - when adding corrections from a file:
 *       - PERIOD: when reading corrections from a file, the period over which the corrections apply, either
 * HOURLY, DAILY or MONTHLY (optional);
 *       - CORRECTIONS: the file and path containing the corrections to apply (mandatory when using PERIOD);
 *       - COLUMN: when using a corrections file, which column should be used if this is a multi-column file (knowing that the indices is column 1. Default: 2).
 *  - when adding noise:
 *       - RANGE: the scaling factor to apply to the random values (see below);
 *       - DISTRIBUTION: to specify the random numbers distribution as either
 *           + uniform: the range represents the *maximum amplitude* of the noise;
 *           + normal: the range represents the *standard deviation* of the noise.
 *
 * @note When multiplying the input signal by the noise, the range is interpreted as a fraction. For example, to modify the input by
 * +/-10%, select "uniform" with "0.1" as a range.
 *
 * @code
 * PSUM::filter1    = mult
 * PSUM::arg1::type = CST
 * PSUM::arg1::cst  = 2.5
 *
 * HS::filter1           = mult
 * HS::arg1::type        = FILE
 * HS::arg1::period      = daily
 * HS::arg1::corrections = input/HS_corr.dat
 *
 * VW::filter1            = add
 * VW::arg1::type         = NOISE
 * VW::arg1::distribution = uniform
 * VW::arg1::range        = 0.1
 * @endcode
 *
 * Example of correction file (monthly correction, August will receive a correction of 1):
 * @code
 * 01 0.440593
 * 02 0.815111
 * 03 0.475562
 * 04 0.674975
 * 05 0.700086
 * 06 0.886783
 * 07 1.70733
 * 09 1.26533
 * 10 0.577152
 * 11 0.394095
 * 12 0.347335
 * @endcode
 */

class ProcMult : public ProcAdd {
	public:
		ProcMult(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name, const std::string& i_root_path);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);

	protected:
		virtual void uniform_noise(const unsigned int& param, std::vector<MeteoData>& ovec) const;
		virtual void normal_noise(const unsigned int& param, std::vector<MeteoData>& ovec) const;
};

} //end namespace

#endif
