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
#include <meteoio/meteoFilters/ProcUndercatch_Forland.h>
#include <meteoio/meteoLaws/Meteoconst.h>
#include <meteoio/meteoLaws/Atmosphere.h>
#include <cmath>

using namespace std;

namespace mio {

//WMO values from Yan et al (2001)
const double ProcUndercatch_Forland::Tsnow_WMO=-2.+Cst::t_water_freezing_pt, ProcUndercatch_Forland::Train_WMO=2.+Cst::t_water_freezing_pt;

ProcUndercatch_Forland::ProcUndercatch_Forland(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name)
                       : ProcessingBlock(vecArgs, name), type(wfj)
{
	parse_args(vecArgs);
	properties.stage = ProcessingProperties::first; //for the rest: default values
}

void ProcUndercatch_Forland::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
                        std::vector<MeteoData>& ovec)
{
	if (param!=MeteoData::PSUM)
		throw InvalidArgumentException("Trying to use "+getName()+" filter on " + MeteoData::getParameterName(param) + " but it can only be applied to precipitation!!" + getName(), AT);
	ovec = ivec;

	for (size_t ii=0; ii<ovec.size(); ii++){
		double& tmp = ovec[ii](param);
		const double VW = ovec[ii](MeteoData::VW);
		const double TA = ovec[ii](MeteoData::TA);

		if (tmp == IOUtils::nodata || tmp==0. || VW==IOUtils::nodata || TA==IOUtils::nodata) {
			continue; //preserve nodata values and no precip
		}

		if (TA<=Tsnow_WMO)
			tmp *= solidPrecipitation(TA, VW);
		else {
			if (ii==0) {
				cerr << "[W] Could not correct " << ovec[0].getNameForParameter(param) << ": ";
				cerr << "not enough data for accumulation period at date " << ovec[0].date.toString(Date::ISO) << "\n";
				continue;
			}
			const Date timestep = ovec[ii].date - ovec[ii-1].date;
			const double Pint = ovec[ii](MeteoData::PSUM) / (timestep.getJulian(true)*24.);
			const double krain = liquidPrecipitation(Pint, VW);
			if (TA>=Train_WMO) {
				tmp *= krain;
			} else {
				const double ksnow = solidPrecipitation(TA, VW);
				tmp *= 0.5*ksnow + 0.5*krain;
			}
		}
	}
}

//TA in celsius
double ProcUndercatch_Forland::solidPrecipitation(double TA, double VW) const
{
	TA = IOUtils::K_TO_C(TA); //convert to celsius
	if (type!=wfj)
		VW = Atmosphere::windLogProfile(VW, 10., 2.); //impact seems minimal
	else
		VW *= 0.84;

	//restrict the range of T and VW (thresholds from the tech. report)
	if (VW<1.) return 1.;
	if (VW>7.) VW=7.;
	if (TA<-12.) TA=-12.;

	double beta0, beta1, beta2, beta3;
	if (type==hellmann) {
		beta0 = 0.04587;
		beta1 = 0.23677;
		beta2 = 0.017979;
		beta3 = -0.015407;
	} else if (type==swedish) {
		beta0 = -0.08871;
		beta1 = 0.16146;
		beta2 = 0.011276;
		beta3 = -0.008770;
	} else if (type==norvegian || type==belfort || type==geonor) {
		beta0 = -0.12159;
		beta1 = 0.18546;
		beta2 = 0.006918;
		beta3 = -0.005254;
	} else if (type==finnish || type==wfj) {
		beta0 = -0.07556;
		beta1 = 0.10999;
		beta2 = 0.012214;
		beta3 = -0.007071;
	} else if (type==tretyakov) {
		beta0 = -0.04816;
		beta1 = 0.13383;
		beta2 = 0.009064;
		beta3 = -0.005147;
	} else {
		throw InvalidArgumentException("Wrong rain gauge type for filter " + getName(), AT);
	}

	return exp( beta0 + beta1*VW + beta2*TA + beta3*VW*TA );
}

//TA in celsius, Pint in mm/h
double ProcUndercatch_Forland::liquidPrecipitation(const double& Pint, double VW) const
{
	if (type!=wfj)
		VW = Atmosphere::windLogProfile(VW, 10., 2.); //impact seems minimal
	else
		VW *= 0.84;

	//restrict the range of T and VW (thresholds from the tech. report)
	if (VW<1.) return 1.;
	if (VW>7.) VW=7.;

	const double c = (type==hellmann)? 0. : -0.05;
	const double lnI = log( Pint );

	return exp( -0.00101*lnI - 0.012177*VW*lnI + 0.034331*VW + 0.007697 + c );
}

void ProcUndercatch_Forland::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	const std::string where( "Filters::"+block_name );
	bool has_type=false;

	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="TYPE") {
			const std::string type_str( IOUtils::strToUpper( vecArgs[ii].second ) );
			if (type_str=="WFJ") {
				type=wfj;
			} else if (type_str=="HELLMANN") {
				type=hellmann;
			} else if (type_str=="SWEDISH") {
				type=swedish;
			} else if (type_str=="NORVEGIAN") {
				type=norvegian;
			} else if (type_str=="FINNISH") {
				type=finnish;
			} else if (type_str=="TRETYAKOV") {
				type=tretyakov;
			} else if (type_str=="BELFORT") {
				type=belfort;
			} else if (type_str=="GEONOR") {
				type=geonor;
			} else
				throw InvalidArgumentException("Rain gauge type \""+ type_str +"\" unknown for "+where, AT);
			has_type = true;
		}
	}

	if (!has_type) throw InvalidArgumentException("Please provide a TYPE for "+where, AT);
}

} //end namespace
