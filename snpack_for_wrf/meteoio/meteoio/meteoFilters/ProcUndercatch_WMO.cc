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
#include <meteoio/meteoFilters/ProcUndercatch_WMO.h>
#include <meteoio/meteoLaws/Atmosphere.h>
#include <cmath>
#include <algorithm>

using namespace std;

namespace mio {

const double ProcUndercatch_WMO::Tsnow_WMO=-2., ProcUndercatch_WMO::Train_WMO=2.; //WMO values from Yan et al (2001)

ProcUndercatch_WMO::ProcUndercatch_WMO(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name)
                   : ProcessingBlock(vecArgs, name), type(cst),
                     factor_snow(1.3), factor_mixed(1.1), Tsnow(Tsnow_WMO), Train(Train_WMO)
{
	parse_args(vecArgs);
	properties.stage = ProcessingProperties::first; //for the rest: default values
}

void ProcUndercatch_WMO::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
                        std::vector<MeteoData>& ovec)
{
	if (param!=MeteoData::PSUM)
		throw InvalidArgumentException("Trying to use "+getName()+" filter on " + MeteoData::getParameterName(param) + " but it can only be applied to precipitation!!" + getName(), AT);
	ovec = ivec;

	for (size_t ii=0; ii<ovec.size(); ii++){
		double& tmp = ovec[ii](param);
		double VW = ovec[ii](MeteoData::VW);
		if (VW!=IOUtils::nodata) VW = std::min(Atmosphere::windLogProfile(VW, 10., 2.), 7.); //impact seems minimal, but 7m/s restriction is important
		double t = ovec[ii](MeteoData::TA);
		if (t==IOUtils::nodata) continue; //we MUST have air temperature in order to filter
		t=std::max(IOUtils::K_TO_C(t), -15.); //t in celsius, restricted to >=-15
		precip_type precip = (t<=Tsnow)? snow : (t>=Train)? rain : mixed;

		//We don't use Tmax, Tmin, Tmean but only the current temperature instead
		if (tmp == IOUtils::nodata || tmp==0.) {
			continue; //preserve nodata values and no precip
		} else if (type==cst) {
			if (precip==snow) tmp *= factor_snow;
			if (precip==mixed) tmp *= factor_mixed;
		} else if (type==nipher) {
			if (VW==IOUtils::nodata) continue;
			double k=100.;
			if (precip==snow) k=100.-0.44*VW*VW-1.98*VW;
			if (precip==mixed) {
				k=97.29-3.18*VW+0.58*t-0.67*t; //Tmax, Tmin
			}
			tmp *= 100./k;
		} else if (type==tretyakov) {
			if (VW==IOUtils::nodata) continue;
			double k=100.;
			if (VW>8.5) VW=8.5; //the fits have been calibrated until 8.5 m/s
			if (precip==snow) k=103.11-8.67*VW+0.30*t; //Tmax
			if (precip==mixed) k=96.99-4.46*VW+0.88*t+0.22*t; //Tmax, Tmin
			tmp *= 100./k;
		} else if (type==us8sh) {
			if (VW==IOUtils::nodata) continue;
			double k=100.;
			if (precip==snow) k=exp(4.61-0.04*pow(VW, 1.75));
			if (precip==mixed) k=101.04-5.62*VW;
			tmp *= 100./k;
		} else if (type==us8unsh) {
			if (VW==IOUtils::nodata) continue;
			double k=100.;
			if (precip==snow) k=exp(4.61-0.16*pow(VW, 1.28));
			if (precip==mixed) k=100.77-8.34*VW;
			tmp *= 100./k;
		} else if (type==rt3_jp) {
			if (VW==IOUtils::nodata) continue;
			const double rh = ovec[ii](MeteoData::RH);
			const double alt = ovec[ii].meta.position.getAltitude();
			double k=100.;
			if (rh!=IOUtils::nodata && alt!=IOUtils::nodata) {
				const double t_wb = IOUtils::K_TO_C(Atmosphere::wetBulbTemperature(ovec[ii](MeteoData::TA), rh, alt));
				double ts_rate;
				if (t_wb<1.1) ts_rate = 1. - .5*exp(-2.2*pow(1.1-t_wb, 1.3));
				else ts_rate = .5*exp(-2.2*pow(t_wb-1.1, 1.3));
				if (ts_rate>.5) precip=snow; else precip=mixed;
			}
			if (precip==snow) k=100. / (1.+.346*VW);
			if (precip==mixed) k=100. / (1.+.0856*VW);
			tmp *= 100./k;
		} else if (type==cspg) {
			if (VW==IOUtils::nodata) continue;
			double k=100.;
			if (precip==snow) k=100.*exp(-0.056*VW); //VW in 0 - 6.2
			if (precip==rain) k=100.*exp(-0.041*VW); //VW in 0 - 7.3
			if (precip==mixed) {
				const double Ksnow = 100.*exp(-0.056*VW);
				const double Krain = 100.*exp(-0.041*VW);
				const double td = (t<-2.)? -2. : (t>2.)? 2. : t;
				k = Ksnow - (Ksnow-Krain)*(td+2.)/4.; //Tmean
			}
			tmp *= 100./k;
		} else if (type==geonorsh) {
			if (VW==IOUtils::nodata) continue;
			double k=100.;
			if (precip==snow) k=100.*exp(-0.135*VW); //VW in 0 - 6
			if (precip==rain) k=100.*exp(-0.113*VW); //VW in 0 - 5
			if (precip==mixed) {
				const double Ksnow = 100.*exp(-0.135*VW);
				const double Krain = 100.*exp(-0.113*VW);
				const double td = (t<-2.)? -2. : (t>2.)? 2. : t;
				k = Ksnow - (Ksnow-Krain)*(td+2.)/4.; //Tmean
			}
			tmp *= 100./k;
		} else if (type==hellmann) {
			if (VW==IOUtils::nodata) continue;
			double k=100.;
			if (precip==snow) k=100.+1.13*VW*VW-19.45*VW;
			if (precip==mixed) k=96.63+0.41*VW*VW-9.84*VW+5.95*t; //Tmean
			tmp *= 100./k;
		}  else if (type==hellmannsh) {
			if (VW==IOUtils::nodata) continue;
			double k=100.;
			if (precip==snow) k=100.+0.72*VW*VW-13.74*VW;
			if (precip==mixed) k=101.319+0.524*VW*VW-6.42*VW;
			tmp *= 100./k;
		}
	}
}

void ProcUndercatch_WMO::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	const std::string where( "Filters::"+block_name );
	bool has_type=false;

	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="TYPE") {
			const std::string type_str( IOUtils::strToUpper( vecArgs[ii].second ) );
			if (type_str=="CST") {
				type=cst;
			} else if (type_str=="NIPHER") {
				type=nipher;
			} else if (type_str=="TRETYAKOV") {
				type=tretyakov;
			} else if (type_str=="US8SH") {
				type=us8sh;
			} else if (type_str=="US8UNSH") {
				type=us8unsh;
			} else if (type_str=="RT3_JP") {
				type=rt3_jp;
			} else if (type_str=="CSPG") {
				type=cspg;
			} else if (type_str=="GEONORSH") {
				type=geonorsh;
			} else if (type_str=="HELLMANN") {
				type=hellmann;
			} else if (type_str=="HELLMANNSH") {
				type=hellmannsh;
			} else {
				throw InvalidArgumentException("Rain gauge type \""+ type_str +"\" unknown for "+where, AT);
			}
			has_type = true;
		} else if (vecArgs[ii].first=="SNOW") {
			IOUtils::parseArg(vecArgs[ii], where, factor_snow);
		} else if (vecArgs[ii].first=="MIXED") {
			IOUtils::parseArg(vecArgs[ii], where, factor_mixed);
		} else if (vecArgs[ii].first=="T_SNOW") {
			IOUtils::parseArg(vecArgs[ii], where, Tsnow);
		} else if (vecArgs[ii].first=="T_RAIN") {
			IOUtils::parseArg(vecArgs[ii], where, Train);
		}
	}

	if (!has_type) throw InvalidArgumentException("Please provide a TYPE for "+where, AT);
}

} //end namespace
