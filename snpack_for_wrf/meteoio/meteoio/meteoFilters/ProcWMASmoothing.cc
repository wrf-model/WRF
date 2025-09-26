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
#include <meteoio/meteoFilters/ProcWMASmoothing.h>
#include <cmath>
#include <algorithm>

using namespace std;

namespace mio {

ProcWMASmoothing::ProcWMASmoothing(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name) : WindowedFilter(vecArgs, name)
{
	//This is safe, but maybe too imprecise:
	properties.time_before = min_time_span;
	properties.time_after  = min_time_span;
	properties.points_before = min_data_points;
	properties.points_after = min_data_points;
}

void ProcWMASmoothing::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
                            std::vector<MeteoData>& ovec)
{
	ovec = ivec;
	for (size_t ii=0; ii<ovec.size(); ii++){ //for every element in ovec, get a window
		double& value = ovec[ii](param);

		size_t start, end;
		if ( get_window_specs(ii, ivec, start, end) ) {
			value = calcWMASmoothing(ivec, param, start, end, ii);
		} else if (!is_soft) value = IOUtils::nodata;
	}
}

//such as WMA = 1*X1 + 2*X2 + ... + n*Xn and then normalized by the sum of weights = (1+2+3+...+n)
double ProcWMASmoothing::calcWMASmoothing(const std::vector<MeteoData>& ivec, const unsigned int& param, const size_t& start, const size_t& end, const size_t& pos)
{
	const size_t max_len = max(pos-start, end-pos);
	double wma = 0.;
	size_t norm = 0;

	for (size_t ii=0; ii<=max_len; ii++) {
		//getting values left and right of the current point
		const double val1 = ( pos>=ii && (pos-ii)>=start )? ivec[pos-ii](param) : IOUtils::nodata;
		const double val2 = ( (pos+ii)<=end )? ivec[pos+ii](param) : IOUtils::nodata;

		//computing the average (centered window) or take the proper point (left or right window)
		double val;
		if (val1!=IOUtils::nodata && val2!=IOUtils::nodata) {
			val = (val1+val2) * .5;
		} else if (val1!=IOUtils::nodata) {
			val = val1;
		} else if (val2!=IOUtils::nodata) {
			val = val2;
		} else
			val = IOUtils::nodata;

		//compute the WMA contribution and normalization factor
		if (val!=IOUtils::nodata) {
			const size_t weight = max_len-ii+1;
			wma += static_cast<double>(weight)*val;
			norm += weight;
		}
	}

	if (norm > 0)
		return wma / static_cast<double>(norm);
	else
		return IOUtils::nodata;
}

} //namespace
