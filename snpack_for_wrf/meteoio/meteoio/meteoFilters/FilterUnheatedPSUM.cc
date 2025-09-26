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
#include <meteoio/meteoFilters/FilterUnheatedPSUM.h>

using namespace std;

namespace mio {

FilterUnheatedPSUM::FilterUnheatedPSUM(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name)
                  : ProcessingBlock(vecArgs, name), thresh_rh(0.5), thresh_Dt(3.), is_soft(true)
{
	parse_args(vecArgs);
	properties.stage = ProcessingProperties::both; //for the rest: default values
}

void FilterUnheatedPSUM::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
                        std::vector<MeteoData>& ovec)
{
	if (param!=MeteoData::PSUM) {
		ostringstream ss;
		ss << "Can not use " << getName() << " processing on " << MeteoData::getParameterName(param);
		throw InvalidArgumentException(ss.str(), AT);
	}

	ovec = ivec;
	for (size_t ii=0; ii<ovec.size(); ii++){
		double& tmp = ovec[ii](param);
		if (tmp == IOUtils::nodata) continue; //preserve nodata values

		if (tmp>0.) {
			const double rh = ivec[ii](MeteoData::RH);
			const double ta = ivec[ii](MeteoData::TA);
			const double tss = ivec[ii](MeteoData::TSS);

			if (rh!=IOUtils::nodata && rh<thresh_rh) //not enough humidity for precipitation
				tmp = 0.;
			if (ta!=IOUtils::nodata && tss!=IOUtils::nodata && (ta-tss)>thresh_Dt ) //clear sky condition
				tmp = 0.;
			if (!is_soft && rh==IOUtils::nodata && ((ta==IOUtils::nodata) || (tss==IOUtils::nodata)) )
					tmp = IOUtils::nodata; //we could not even try to validate the data point -> we delete it for safety
		}
	}
}


void FilterUnheatedPSUM::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	const std::string where( "Filters::"+block_name );
	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="SOFT") {
			IOUtils::parseArg(vecArgs[ii], where, is_soft);
		} else if (vecArgs[ii].first=="THRESH_RH") {
			IOUtils::parseArg(vecArgs[ii], where, thresh_rh);
		} else if (vecArgs[ii].first=="THRESH_DT") {
			IOUtils::parseArg(vecArgs[ii], where, thresh_Dt);
		}
	}
}

} //end namespace
