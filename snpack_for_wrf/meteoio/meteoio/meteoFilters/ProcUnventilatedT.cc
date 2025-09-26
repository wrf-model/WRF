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
#include <meteoio/meteoFilters/ProcUnventilatedT.h>
#include <cmath>

using namespace std;

namespace mio {

const double ProcUnventilatedT::dflt_albedo = .23;
const double ProcUnventilatedT::vw_thresh = 0.1; //wind speed threshold

ProcUnventilatedT::ProcUnventilatedT(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name)
                  : ProcessingBlock(vecArgs, name), usr_albedo(dflt_albedo),
                    usr_vw_thresh(IOUtils::nodata), nakamura(false)
{
	parse_args(vecArgs);
	properties.stage = ProcessingProperties::first; //for the rest: default values
}

void ProcUnventilatedT::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
                        std::vector<MeteoData>& ovec)
{
	ovec = ivec;

	if (usr_vw_thresh!=IOUtils::nodata)
		filterTA(param, ovec);
	else
		correctTA(param, ovec);

}

void ProcUnventilatedT::filterTA(const unsigned int& param, std::vector<MeteoData>& ovec) const
{
	for (size_t ii=0; ii<ovec.size(); ii++) {
		const double vw = ovec[ii](MeteoData::VW);
		if (vw!=IOUtils::nodata && vw<usr_vw_thresh)
			ovec[ii](param) = IOUtils::nodata;
	}
}

void ProcUnventilatedT::correctTA(const unsigned int& param, std::vector<MeteoData>& ovec) const
{
	if (nakamura) {
		for (size_t ii=0; ii<ovec.size(); ii++) {
			double& ta = ovec[ii](param);
			if (ta == IOUtils::nodata) continue; //preserve nodata values

			double iswr = ovec[ii](MeteoData::ISWR);
			const double rswr = ovec[ii](MeteoData::RSWR);
			double vw = ovec[ii](MeteoData::VW);
			const double hs = ovec[ii](MeteoData::HS);

			if (hs!=IOUtils::nodata && iswr==IOUtils::nodata && rswr!=IOUtils::nodata) {
				if (hs>snow_thresh) iswr = rswr / snow_albedo;
				else iswr = rswr / soil_albedo;
			}

			if (iswr==IOUtils::nodata || vw==IOUtils::nodata)
				continue;

			if (vw<vw_thresh) vw = vw_thresh; //this should be around the minimum measurable wind speed on regular instruments
			static const double rho = 1.2; // in kg/m3
			static const double Cp = 1004.;
			const double X = iswr / (rho*Cp*ta*vw);
			if (X<1e-4) continue; //the correction does not work well for small X values

			static const double C0 = 0.13;
			static const double C1 = 373.40;
			const double RE = C0 + C1*X;
			ta -= RE; //substracting the radiative error
		}
	} else { //Huwald
		for (size_t ii=0; ii<ovec.size(); ii++) {
			double& ta = ovec[ii](param);
			if (ta == IOUtils::nodata) continue; //preserve nodata values

			const double iswr = ovec[ii](MeteoData::ISWR);
			double rswr = ovec[ii](MeteoData::RSWR);
			double vw = ovec[ii](MeteoData::VW);
			const double hs = ovec[ii](MeteoData::HS);

			if (hs!=IOUtils::nodata && rswr==IOUtils::nodata && iswr!=IOUtils::nodata) {
				if (hs>snow_thresh) rswr = iswr * snow_albedo;
				else rswr = iswr * soil_albedo;
			}

			if (rswr==IOUtils::nodata || vw==IOUtils::nodata)
				continue;

			if (vw<vw_thresh) vw = vw_thresh; //this should be around the minimum measurable wind speed on regular instruments
			static const double rho = 1.2; // in kg/m3
			static const double Cp = 1004.;
			const double X = rswr / (rho*Cp*ta*vw) * 1e3;
			if (X<0.05) continue; //the correction does not work well for small X values

			const double RE = 3.1 * sqrt(X);
			ta -= RE; //substracting the radiative error
		}
	}
}

void ProcUnventilatedT::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	const std::string where( "Filters::"+block_name );
	bool has_vw_thresh=false, has_usr_albedo=false, suppr_filter=false;

	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="TYPE") {
			const std::string type_str( IOUtils::strToUpper( vecArgs[ii].second ) );
			if (type_str=="NAKAMURA") nakamura = true;
			else if (type_str=="HUWALD") nakamura = false;
			else if (type_str=="SUPPR") suppr_filter = true;
			else
				throw InvalidArgumentException("Invalid type \""+vecArgs[ii].second+"\" for \""+where+"\"", AT);
		} else if (vecArgs[ii].first=="THRESH_VW") {
			IOUtils::parseArg(vecArgs[ii], where, usr_vw_thresh);
			has_vw_thresh = true;
		} else if (vecArgs[ii].first=="SOIL_ALB") {
			IOUtils::parseArg(vecArgs[ii], where, usr_albedo);
			has_usr_albedo = true;
		}
	}

	if (suppr_filter && !has_vw_thresh) throw InvalidArgumentException("Please provide a wind velocity threshold for "+where, AT);
	if (has_usr_albedo && (usr_albedo<0. || usr_albedo>1.) )
		throw InvalidArgumentException("Albedo value should be between 0 and 1 for "+where, AT);
}

} //end namespace
