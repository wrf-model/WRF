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

#include <meteoio/dataGenerators/ESOLIPGenerator.h>
#include <meteoio/meteoFilters/ProcPSUMDistribute.h> //for the precipitation distribution
#include <algorithm>

namespace mio {

bool ESOLIPGenerator::generate(const size_t& /*param*/, MeteoData& /*md*/)
{//HACK: modify prototype so we can get the full vector + the index of the replacement
	return false; //all missing values could be filled
}

//when we can not guarantee PSUM=0, we leave it at nodata. Therefore, it is highly recommended to
//run through a Cst=0 data generator afterward
bool ESOLIPGenerator::create(const size_t& param, std::vector<MeteoData>& vecMeteo)
{
	if (vecMeteo.empty()) return true;

	//Find first point that is not IOUtils::nodata
	size_t last_good = IOUtils::npos;
	for (size_t ii=0; ii<vecMeteo.size(); ii++){
		if (vecMeteo[ii](MeteoData::HS) != IOUtils::nodata){
			last_good = ii;
			break;
		}
	}

	if (last_good == IOUtils::npos) //can not find a good point to start
		return false;

	bool all_filled = (last_good>0)? false : true;
	for (size_t ii=last_good+1; ii<vecMeteo.size(); ii++) {
		const double HS_curr = vecMeteo[ii](MeteoData::HS);
		if (HS_curr==IOUtils::nodata) continue;

		const size_t start_idx = last_good+1;
		const double HS_prev = vecMeteo[last_good](MeteoData::HS);
		const double HS_delta = HS_curr - HS_prev;

		if (HS_delta>0.) {
			const double rho = newSnowDensity(vecMeteo[ii]);
			const double precip = HS_delta * rho; //in kg/m2 or mm
			ProcPSUMDistribute::SmartDistributePSUM(precip, start_idx, ii, param, vecMeteo);
		} else {
			all_filled = false;
		}

		last_good=ii;
	}

	return all_filled;
}

double ESOLIPGenerator::newSnowDensity(const MeteoData& md)
{ //C. Zwart, "Significance of new-snow properties for snowcover development",
//master's thesis, 2007, Institute for Marine and Atmospheric Research, University of Utrecht, 78 pp.
	const double vw = std::max(2., md(MeteoData::VW));
	const double rh = md(MeteoData::RH);
	const double ta = md(MeteoData::TA) - Cst::t_water_triple_pt;
	static const double beta01=3.28, beta1=0.03, beta02=-0.36, beta2=-0.75, beta3=0.3;

	double arg = beta01 + beta1*ta + beta2*asin(sqrt(rh)) + beta3*log10(vw);
	if (ta>=-14.)
		arg += beta02; // += beta2*ta;

	return std::min( std::max(30., pow(10., arg)), 250. ); //limit the density to the [30, 250] kg/m3 range
}

} //namespace
