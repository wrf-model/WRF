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
#ifndef PROCADD_H
#define PROCADD_H

#include <meteoio/meteoFilters/ProcessingBlock.h>
#include <vector>
#include <string>

namespace mio {

/**
 * @class  ProcAdd
 * @ingroup processing
 * @brief Add an offset to the values.
 * @details
 * This adds to all values a given offset. Either a fixed value is given as single argument or a period
 * (hourly/daily/monthly) as well as a filename (and absolute or relative path) containing the offsets to apply.
 * This file must contain in the first column the indices (months from 1 to 12 or days from 1 to 366 or hours from 0 to 23)
 * and the matching offset in the second column (<a href="http://www.cplusplus.com/reference/cctype/isspace/">whitespace</a> delimited).
 * Comments following the same syntax as in the ini file are accepted, missing indices are treated as 0. It is also possible to add random
 * noise.
 *
 * It takes the following arguments:
 *  - TYPE: either CST (add a constant) or FILE (add values form a given file) or NOISE (add random values).
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
 * @code
 * TA::filter1    = add
 * TA::arg1::type = CST
 * TA::arg1::cst  = 2.5
 *
 * TSG::filter1           = add
 * TSG::arg1::type        = FILE
 * TSG::arg1::period      = daily
 * TSG::arg1::corrections = input/TSG_corr.dat
 *
 * TSS::filter1            = add
 * TSS::arg1::type         = NOISE
 * TSS::arg1::distribution = normal
 * TSS::arg1::range        = 5
 * @endcode
 *
 * Example of correction file (monthly correction, December will receive a correction of 0):
 * @code
 * 01 -0.375
 * 02 -1.932
 * 03 -4.304
 * 04 -2.449
 * 05 -1.629
 * 06 -1.734
 * 07 -2.414
 * 09 -1.289
 * 10 -1.086
 * 11 -0.769
 * @endcode
 */

class ProcAdd : public ProcessingBlock {
	public:
		ProcAdd(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name, const std::string& i_root_path);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);

	protected:
		void parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs);
		virtual void uniform_noise(const unsigned int& param, std::vector<MeteoData>& ovec) const;
		virtual void normal_noise(const unsigned int& param, std::vector<MeteoData>& ovec) const;

		std::vector<double> vecCorrections;
		std::string root_path;
		double correction, range;
		char type, distribution, period;
};

} //end namespace

#endif
