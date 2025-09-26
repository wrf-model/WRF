/***********************************************************************************/
/*  Copyright 2011 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#include <meteoio/meteoFilters/FilterTukey.h>
#include <meteoio/meteoStats/libinterpol1D.h>
#include <meteoio/IOUtils.h>
#include <cmath>
#include <algorithm>

using namespace std;

namespace mio {

const double FilterTukey::k = 1.5; ///<How many times the stddev allowed as deviation to the smooth signal for valid points

FilterTukey::FilterTukey(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name) : WindowedFilter(vecArgs, name)
{
	//This is safe, but maybe too imprecise:
	properties.time_before = min_time_span;
	properties.time_after  = min_time_span;
	properties.points_before = min_data_points;
	properties.points_after = min_data_points;
}

void FilterTukey::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
                           std::vector<MeteoData>& ovec)
{
	ovec = ivec;
	for (size_t ii=0; ii<ovec.size(); ii++){ //for every element in ivec, get a window
		double& value = ovec[ii](param);

		size_t start, end;
		if ( get_window_specs(ii, ivec, start, end) ) {
			//Calculate std deviation
			const double std_dev  = getStdDev(ivec, param, start, end);

			const double u3 = getU3(ivec, ii, param);
			if (std_dev!=IOUtils::nodata && u3!=IOUtils::nodata) {
				if ( abs(value-u3) > k*std_dev ) {
					value = IOUtils::nodata;
				}
			} else if (!is_soft) value = IOUtils::nodata;
		} else if (!is_soft) value = IOUtils::nodata;
	}
}

double FilterTukey::getStdDev(const std::vector<MeteoData>& ivec, const unsigned int& param, const size_t& start, const size_t& end)
{
	size_t count=0;
	double sum=0.;

	for (size_t ii=start; ii<=end; ii++) {
		const double& value = ivec[ii](param);
		if (value!=IOUtils::nodata) {
			sum += value;
			count++;
		}
	}

	if (count<=1) {
		return IOUtils::nodata;
	}

	//compensated variance algorithm, see https://secure.wikimedia.org/wikipedia/en/wiki/Algorithms_for_calculating_variance
	const double mean = sum/(double)count;
	double sum2=0., sum3=0.;
	for (size_t ii=start; ii<=end; ii++) {
		const double& value = ivec[ii](param);
		if (value!=IOUtils::nodata) {
			const double delta = value - mean;
			sum2 += delta*delta;
			sum3 += delta;
		}
	}
	const double variance = (sum2 - sum3*sum3/static_cast<double>(count)) / static_cast<double>(count - 1);

	return sqrt(variance);
}

double FilterTukey::getU3(const std::vector<MeteoData>& ivec, const size_t& i, const unsigned int& param)
{
	//exit if we don't have the required data points
	if ( i<4 || i>=(ivec.size()-4) ) {
		return IOUtils::nodata;
	}

	//prepare intermediate variances
	std::vector<double> u2;
	for (char ii=-1; ii<=1; ii++) {
		std::vector<double> u1;
		for (char jj=-1; jj<=1; jj++) {
			std::vector<double> u;
			for (char kk=-2; kk<=2; kk++) {
				const size_t index = static_cast<size_t>(i + (kk + jj + ii));
				const double value = ivec[index](param);
				if (value!=IOUtils::nodata)
					u.push_back( value );
			}
			if (!u.empty())
				u1.push_back( Interpol1D::getMedian(u) );
		}
		if (!u1.empty())
			u2.push_back( Interpol1D::getMedian(u1) );
		else
			u2.push_back( IOUtils::nodata );
	}

	//compute the variance u3
	//u3 = 1/4*( u2[0] + 2.*u2[1] + u2[2] )
	double u3=0.;
	size_t count=0;
	if (u2[0]!=IOUtils::nodata) {
		u3 += u2[0];
		count++;
	}
	if (u2[1]!=IOUtils::nodata) { //current timestep
		u3 += u2[1]*2.;
		count += 2;
	}
	if (u2[2]!=IOUtils::nodata) {
		u3 += u2[2];
		count++;
	}

	if (count>0)
		return u3/((double)count);
	else
		return IOUtils::nodata;
}

}
