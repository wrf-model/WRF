/***********************************************************************************/
/*  Copyright 2016 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#include <meteoio/meteoFilters/FilterNoChange.h>
#include <meteoio/meteoStats/libinterpol1D.h>

using namespace std;

namespace mio {

FilterNoChange::FilterNoChange(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name)
          : WindowedFilter(vecArgs, name), max_variance(0.)
{
	properties.stage = ProcessingProperties::first;
	
	properties.time_before = min_time_span;
	properties.time_after  = min_time_span;
	properties.points_before = min_data_points;
	properties.points_after = min_data_points;
	parse_args(vecArgs);
}

void FilterNoChange::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
                        std::vector<MeteoData>& ovec)
{
	ovec = ivec;
	for (size_t ii=0; ii<ovec.size(); ii++){ //for every element in ivec, get a window
		double& value = ovec[ii](param);
		if (value==IOUtils::nodata) continue;

		size_t start, end;
		if ( get_window_specs(ii, ivec, start, end) ) {
			std::vector<double> data( end-start+1 );
			for (size_t jj=start; jj<=end; jj++)
				data[jj-start] = ivec[jj](param);
			const double variance = Interpol1D::variance( data );
			if (variance<=max_variance)
				value = IOUtils::nodata;
		} else if (!is_soft) value = IOUtils::nodata;
	}
}

void FilterNoChange::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	const std::string where( "Filters::"+block_name );
	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="MAX_VARIANCE") {
			IOUtils::parseArg(vecArgs[ii], where, max_variance);
		}
	}
}

} //end namespace
