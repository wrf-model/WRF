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
#include <meteoio/meteoFilters/TEMPLATE.h>

using namespace std;

namespace mio {

TEMPLATE::TEMPLATE(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name)
          : ProcessingBlock(vecArgs, name) //this has to match the class you are inheriting from! ie ProcessingBlock or WindowedFilter
{
	parse_args(vecArgs);
	//the filters can be called at two points: before the temporal resampling (first stage, ProcessingProperties::first)
	//or after the temporal resampling (second stage, ProcessingProperties::second) or both (ProcessingProperties::both)
	//filters that do not depend on past data can safely use "both" (such as min/max filters) while
	//corrections should only use "first" (such as undercatch correction) in order to avoid double-correcting the data
	//filters relying on past data (through variance or other statistical parameters) usually should use "first"
	properties.stage = ProcessingProperties::first;
}

void TEMPLATE::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
                        std::vector<MeteoData>& ovec)
{
	ovec = ivec;
	for (size_t ii=0; ii<ovec.size(); ii++){
		//here, implement what has to be done on each data point
		//for example:
		double& tmp = ovec[ii](param);
		if (tmp == IOUtils::nodata) continue; //preserve nodata values

		if (tmp < 0.){ //delete all values less than zero
			tmp = IOUtils::nodata;
		}
	}
}


void TEMPLATE::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	const std::string where( "Filters::"+block_name );
	//for a filter that does not take any arguments
	if ( !vecArgs.empty() ) //ie if there are arguments, throw an exception
		throw InvalidArgumentException("Wrong number of arguments for "+where, AT);

	/*
	//for a filter taking one or more arguments
	//if the filter is based on WindowedFilter, its constructor will automatically read the window parameters as well as the "soft" argument

	//to perform syntax checks (see after the "for" loop)
	bool has_max=false;

	//parse the arguments (the keys are all upper case)
	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="TYPE") {
			IOUtils::parseArg(vecArgs[ii], where, type);
		} else if (vecArgs[ii].first=="MAX") {
			IOUtils::parseArg(vecArgs[ii], where, max_val);
			has_max = true;
		}
	}

	//second part of the syntax check
	if (!has_max) throw InvalidArgumentException("Please provide a MAX value for "+where, AT);
	*/
}

} //end namespace
