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
#include <meteoio/meteoFilters/FilterTimeconsistency.h>
#include <meteoio/meteoStats/libinterpol1D.h>

using namespace std;

namespace mio {

FilterTimeconsistency::FilterTimeconsistency(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name)
          : WindowedFilter(vecArgs, name)
{
	properties.stage = ProcessingProperties::first;
}

void FilterTimeconsistency::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
                        std::vector<MeteoData>& ovec)
{
	static const double std_factor = 4.;
	const size_t nr_points = ivec.size();

	ovec = ivec;
	for (size_t ii=0; ii<ivec.size(); ii++){ //for every element in ivec, get a window
		double& value = ovec[ii](param);
		if (value==IOUtils::nodata) continue;

		size_t start, end;
		if ( get_window_specs(ii, ivec, start, end) ) {
			std::vector<double> data( end-start+1 );
			for (size_t jj=start; jj<=end; jj++) data[jj-start] = ivec[jj](param);
			
			const double std_dev = Interpol1D::std_dev( data );
			if (std_dev==IOUtils::nodata) continue;
			if (ii==0 || ii==(nr_points-1)) continue;
			const double local_diff = fabs(value - ivec[ii-1](param)) + fabs(value - ivec[ii+1](param));
			if (local_diff > std_factor*std_dev) value = IOUtils::nodata;
			
		} else if (!is_soft) value = IOUtils::nodata;
	}
}

} //end namespace
