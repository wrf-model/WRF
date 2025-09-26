/***********************************************************************************/
/*  Copyright 2014 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef TEMPLATE_H
#define TEMPLATE_H

//#include <meteoio/meteoFilters/WindowedFilter.h> //use this one for filters relying on a data window, for example std_dev
#include <meteoio/meteoFilters/ProcessingBlock.h> //use this one for all others

#include <vector>
#include <string>

namespace mio {

/**
 * @class TEMPLATE
 * @brief This (empty) class is to be used as a template for developing new filters
 * @details
 * Here, write a description of how the filter operates, references to papers, etc
 * @ingroup processing
 * @author Mathias Bavay
 * @date   2014-02-19
 * @code
 * ILWR::filter1	= TEMPLATE
 * @endcode
 */

class TEMPLATE : public ProcessingBlock { //use this one for simple filter that only look at one data point at a time, for example min_max
//class TEMPLATE : public WindowedFilter { //use this one for filters relying on a data window, for example std_dev
	public:
		TEMPLATE(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);

	private:
		void parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs);
};

} //end namespace

#endif
