/***********************************************************************************/
/*  Copyright 2013-2018 WSL Institute for Snow and Avalanche Research    SLF-DAVOS */
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

#include <meteoio/dataGenerators/PrecSplitting.h>
#include <algorithm>

namespace mio {

void PrecSplitting::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	bool has_model=false, has_snow=false, has_rain=false;
	double snow_T_thresh=273.15, rain_T_thresh=273.15; //to silence a warning

	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="TYPE") {
			const std::string user_algo( IOUtils::strToUpper(vecArgs[ii].second) );

			if (user_algo=="THRESH") model = THRESH;
			else if (user_algo=="RANGE") model = RANGE;
			else
				throw InvalidArgumentException("Unknown parametrization \""+user_algo+"\" supplied for "+where+" generator", AT);

			has_model = true;
		} else if(vecArgs[ii].first=="SNOW") {
			IOUtils::parseArg(vecArgs[ii], where, snow_T_thresh);
			has_snow = true;
		} else if(vecArgs[ii].first=="RAIN") {
			IOUtils::parseArg(vecArgs[ii], where, rain_T_thresh);
			has_rain = true;
		} else
			throw InvalidArgumentException("Unknown argument \""+vecArgs[ii].first+"\" supplied for "+where+" generator", AT);
	}

	if (!has_model) throw InvalidArgumentException("Please provide a MODEL for "+where, AT);
	if (model == THRESH) {
		if (!has_snow) throw InvalidArgumentException("Please provide a snow/rain threshold for "+where, AT);
		fixed_thresh = snow_T_thresh;
	}
	if (model == RANGE) {
		if (!has_snow || !has_rain) throw InvalidArgumentException("Please provide a a snow and a rain threshold for "+where, AT);
		if (snow_T_thresh==rain_T_thresh) throw InvalidArgumentException(where+" : the two provided threshold must be different", AT);
		if (snow_T_thresh>rain_T_thresh) std::swap(snow_T_thresh, rain_T_thresh);
		range_start = snow_T_thresh;
		range_norm = 1. / (rain_T_thresh-snow_T_thresh);
	}
}

bool PrecSplitting::create(const size_t& param, std::vector<MeteoData>& vecMeteo)
{
	if (vecMeteo.empty()) return true;
	const std::string parname( vecMeteo.front().getNameForParameter( param ) );
	const bool acceptedParameter = (param==MeteoData::PSUM || param==MeteoData::PSUM_PH || parname=="PSUM_L" || parname=="PSUM_S");
	if (!acceptedParameter)
		throw InvalidArgumentException("Only the PSUM_L and PSUM_S parameters are supported by the "+where+" generator", AT);
	
	bool all_filled = true;
	if (param==MeteoData::PSUM_PH) {
		for (size_t ii=0; ii<vecMeteo.size(); ii++) {
			double &value = vecMeteo[ii](param);
			if (value!=IOUtils::nodata) continue;
			if (!generatePSUM_PH(value, vecMeteo[ii]))
				all_filled = false;
		}
	} else if (param==MeteoData::PSUM) {
		for (size_t ii=0; ii<vecMeteo.size(); ii++) {
			double &value = vecMeteo[ii](param);
			if (value!=IOUtils::nodata) continue;
			if (!generatePSUM(value, vecMeteo[ii]))
				all_filled = false;
		}
	} else if (parname=="PSUM_L") {
		for (size_t ii=0; ii<vecMeteo.size(); ii++) {
			double &value = vecMeteo[ii](param);
			if (value!=IOUtils::nodata) continue;
			if (!generatePSUM_L(value, vecMeteo[ii]))
				all_filled = false;
		}
	} else if (parname=="PSUM_S") {
		for (size_t ii=0; ii<vecMeteo.size(); ii++) {
			double &value = vecMeteo[ii](param);
			if (value!=IOUtils::nodata) continue;
			if (!generatePSUM_S(value, vecMeteo[ii]))
				all_filled = false;
		}
	}
	
	return all_filled;
}

bool PrecSplitting::generate(const size_t& param, MeteoData& md)
{
	double &value = md(param);
	if (value!=IOUtils::nodata) return true;
	
	const std::string parname( md.getNameForParameter( param ) );
	const bool acceptedParameter = (param==MeteoData::PSUM || param==MeteoData::PSUM_PH || parname=="PSUM_L" || parname=="PSUM_S");
	if (!acceptedParameter)
		throw InvalidArgumentException("Only the PSUM_L and PSUM_S parameters are supported by the "+where+" generator", AT);
	
	if (param==MeteoData::PSUM_PH) return generatePSUM_PH(value, md);
	if (param==MeteoData::PSUM) return generatePSUM(value, md);
	if (parname=="PSUM_L") return generatePSUM_L(value, md);
	if (parname=="PSUM_S") return generatePSUM_S(value, md);
	
	return false;
}

bool PrecSplitting::generatePSUM_S(double& value, MeteoData& md) const
{
	const double PSUM = md(MeteoData::PSUM);
	const bool hasPsum = (PSUM!=IOUtils::nodata && PSUM>=0.); //very basic plausibility check
	const double PSUM_PH = md(MeteoData::PSUM_PH);
	const bool hasPsum_PH = (PSUM_PH!=IOUtils::nodata && PSUM_PH>=0. && PSUM_PH<=1.); //very basic plausibility check
	
	if (hasPsum && hasPsum_PH) {
		value = PSUM * (1. - PSUM_PH);
		return true;
	}
	
	const double PSUM_L = (md.param_exists("PSUM_L"))? md("PSUM_L") : IOUtils::nodata;
	const bool hasPSUML = (PSUM_L!=IOUtils::nodata && PSUM_L>=0.); //very basic plausibility check
	
	if (hasPSUML) {
		if (hasPsum) {
			value = PSUM - PSUM_L;
			return true;
		} else if (hasPsum_PH) {
			if (PSUM_PH==0) return false;
			value = PSUM_L * (1. - PSUM_PH) / PSUM_PH;
			return true;
		}
	} else if (hasPsum) {
		double phase;
		if (runModel(phase, md)) {
			value = PSUM * (1. - phase);
			return true;
		}
	}
	
	return false; //not enough data
}

bool PrecSplitting::generatePSUM_L(double& value, MeteoData& md) const
{
	const double PSUM = md(MeteoData::PSUM);
	const bool hasPsum = (PSUM!=IOUtils::nodata && PSUM>=0.); //very basic plausibility check
	const double PSUM_PH = md(MeteoData::PSUM_PH);
	const bool hasPsum_PH = (PSUM_PH!=IOUtils::nodata && PSUM_PH>=0. && PSUM_PH<=1.); //very basic plausibility check
	
	if (hasPsum && hasPsum_PH) {
		value = PSUM * PSUM_PH;
		return true;
	}
	
	const double PSUM_S = (md.param_exists("PSUM_S"))? md("PSUM_S") : IOUtils::nodata;
	const bool hasPSUMS = (PSUM_S!=IOUtils::nodata && PSUM_S>=0.); //very basic plausibility check
	
	if (hasPSUMS) {
		if (hasPsum) {
			value = PSUM - PSUM_S;
			return true;
		} else if (hasPsum_PH) {
			if (PSUM_PH==1) return false;
			value = PSUM_S * PSUM_PH / (1. - PSUM_PH);
			return true;
		}
	} else if (hasPsum) {
		double phase;
		if (runModel(phase, md)) {
			value = PSUM * phase;
			return true;
		}
	}
	
	return false; //not enough data
}

bool PrecSplitting::generatePSUM(double& value, MeteoData& md) const
{
	const double PSUM_L = (md.param_exists("PSUM_L"))? md("PSUM_L") : IOUtils::nodata;
	const double PSUM_S = (md.param_exists("PSUM_S"))? md("PSUM_S") : IOUtils::nodata;
	const bool hasPSUML = (PSUM_L!=IOUtils::nodata && PSUM_L>=0.); //very basic plausibility check
	const bool hasPSUMS = (PSUM_S!=IOUtils::nodata && PSUM_S>=0.); //very basic plausibility check
	
	if (hasPSUML && hasPSUMS) {
		value = PSUM_L + PSUM_S;
		return true;
	}
	
	const double PSUM_PH = md(MeteoData::PSUM_PH);
	if (PSUM_PH==IOUtils::nodata || PSUM_PH<0. || PSUM_PH>1.) return false;
	
	if (hasPSUML) {
		if (PSUM_PH==0.) return false; //precip is fully solid but we only have the liquid phase...
		value = PSUM_L / PSUM_PH;
		return true;
	} else if (hasPSUMS) {
		if (PSUM_PH==1.) return false; //precip is fully liquid but we only have the solid phase...
		value = PSUM_S / (1. - PSUM_PH);
		return true;
	}
	
	return false; //not enough data
}

bool PrecSplitting::generatePSUM_PH(double& value, MeteoData& md) const
{
	const double PSUM = md(MeteoData::PSUM);
	if (PSUM==0) {
		value = 0.;
		return true;
	}
	
	const double PSUM_L = (md.param_exists("PSUM_L"))? md("PSUM_L") : IOUtils::nodata;
	const double PSUM_S = (md.param_exists("PSUM_S"))? md("PSUM_S") : IOUtils::nodata;
	const bool hasPSUML = (PSUM_L!=IOUtils::nodata && PSUM_L>=0.); //very basic plausibility check
	const bool hasPSUMS = (PSUM_S!=IOUtils::nodata && PSUM_S>=0.); //very basic plausibility check
	const bool hasPsum = (PSUM!=IOUtils::nodata && PSUM>=0.); //very basic plausibility check
		
	if (hasPsum) {
		if (!hasPSUML && !hasPSUMS) {
			return runModel(value, md);
		} else if (hasPSUML) {
			value = std::min( std::max(PSUM_L / PSUM, 0.), 1.);
			return true;
		} else if (hasPSUMS) {
			value = 1. - std::min( std::max(PSUM_L / PSUM, 0.), 1.);
			return true;
		}
	}
	
	if (hasPSUML && hasPSUMS) {
		const double tot_precip = PSUM_L + PSUM_S;
		value = (tot_precip==0)? 0. : std::min( std::max(PSUM_L / tot_precip, 0.), 1.);
		return true;
	}

	return false; //not enough data
}

//generate PSUM_PH from PSUM and TA
bool PrecSplitting::runModel(double &value, MeteoData& md) const
{
	const double TA=md(MeteoData::TA);
	if (TA==IOUtils::nodata) return false;
	
	if (model==THRESH) {
		value = (TA>=fixed_thresh)? 1. : 0.;
	} else if (model==RANGE) {
		const double tmp_rainfraction = range_norm * (TA - range_start);
		value = (tmp_rainfraction>1)? 1. : (tmp_rainfraction<0.)? 0. : tmp_rainfraction;
	}
	
	return true;
}

} //namespace
