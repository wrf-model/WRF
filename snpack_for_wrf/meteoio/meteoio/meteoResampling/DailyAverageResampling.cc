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

#include <meteoio/meteoResampling/DailyAverageResampling.h>
#include <meteoio/IOUtils.h>
#include <meteoio/MathOptim.h>

#include <sstream>
#include <cmath>

namespace mio {

DailyAverage::DailyAverage(const std::string& i_algoname, const std::string& i_parname, const double& dflt_window_size, const std::vector< std::pair<std::string, std::string> >& vecArgs)
                 : ResamplingAlgorithms(i_algoname, i_parname, dflt_window_size, vecArgs), range(IOUtils::nodata), phase(0.)
{
	const std::string where( "Interpolations1D::"+i_parname+"::"+i_algoname );

	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="RANGE") {
			IOUtils::parseArg(vecArgs[ii], where, range);
		} else if (vecArgs[ii].first=="PHASE") {
			IOUtils::parseArg(vecArgs[ii], where, phase);
			phase *= -1; //shift the minimum *later* in the day
		}
	}
}

std::string DailyAverage::toString() const
{
	std::ostringstream ss;
	ss << std::right << std::setw(10) << parname << "::"  << std::left << std::setw(15) << algo;
	ss << "[ window_size=" << window_size;
	if (range!=IOUtils::nodata)
		ss << " range=" << range;
	if (phase!=0.)
		ss << " phase=" << phase;
	ss << " ]";
	return ss.str();
}

double DailyAverage::getValue(const std::vector<MeteoData>& vecM, const size_t& paramindex, const size_t& index, const Date& dayStart, const double& frac_day) const
{
	const Date dayEnd( dayStart+1. );
	const size_t indexP = getDailyValue(vecM, paramindex, index, dayStart, dayEnd);
	if (indexP==IOUtils::npos) return IOUtils::nodata;

	const double avg = vecM[indexP](paramindex); //from getDailyValue we are guaranteed to have a value

	//search for min or max or both
	const std::string param_name( vecM[indexP].getNameForParameter(paramindex) );
	const std::string min_name( param_name+"_MIN" );
	const std::string max_name( param_name+"_MAX" );
	const double min = (vecM[indexP].param_exists( min_name ))?  vecM[indexP](min_name) : IOUtils::nodata;
	const double max = (vecM[indexP].param_exists( max_name ))?  vecM[indexP](max_name) : IOUtils::nodata;
	double A = range; //fallback: user provided range

	if (min==IOUtils::nodata && max==IOUtils::nodata) {
		if (A==IOUtils::nodata) {
			std::ostringstream ss;
			ss << "Missing parameters for " << algo << " on " << dayStart.toString(Date::ISO_DATE) << ". Please consider providing a fallback range argument!";
			throw InvalidArgumentException(ss.str(), AT);
		}
	} else if (min!=IOUtils::nodata && max==IOUtils::nodata) {
		A = (avg -  min);
	} else if (min==IOUtils::nodata && max!=IOUtils::nodata) {
		A =(max - avg);
	} else
		throw InvalidArgumentException("Providing both AVG, MIN and MAX for \'"+algo+"\' is not supported!", AT);

	return A * sin( 2.*Cst::PI * (frac_day-.25+phase) ) + avg;
}

void DailyAverage::resample(const std::string& /*stationHash*/, const size_t& index, const ResamplingPosition& /*position*/, const size_t& paramindex,
                                const std::vector<MeteoData>& vecM, MeteoData& md)
{
	if (index >= vecM.size())
		throw IOException("The index of the element to be resampled is out of bounds", AT);

	//define various time parameters
	const double julian = md.date.getJulian();
	double frac_day = (julian+.5 - Optim::intPart(julian+.5)); // julian day starts at noon and we want the fraction of the day, from 0 to 1!
	if (frac_day==0.) frac_day = 1.; //because we considere here that midnight belongs to the day before

	const Date dayStart( getDailyStart(md.date) );
	const double val1 = getValue(vecM, paramindex, index, dayStart, frac_day);
	if (val1==IOUtils::nodata)
		return; //we could also decide to use val0 & val2

	//get the other values (day before, day after)
	const double val0 = getValue(vecM, paramindex, index, dayStart-1., frac_day);
	const double val2 = getValue(vecM, paramindex, index, dayStart+1., frac_day);

	double val = val1;
	if (frac_day!=0.5) { //at noon, we purely take val1
		const double w1 = 1./Optim::pow2( std::abs(.5-frac_day) );
		double norm = w1;
		val *= w1;

		if (val0!=IOUtils::nodata) {
			const double w0 = 1./Optim::pow2( frac_day+0.5 );
			val += w0*val0;
			norm += w0;
		}
		if (val2!=IOUtils::nodata) {
			const double w2 = 1./Optim::pow2( 1.5-frac_day );
			val += w2*val2;
			norm += w2;
		}
		val /= norm;
	}

	md(paramindex) = val;
	//very basic range checking
	//HACK this means that this should be implemented as a data creator so it could be filtered afterward
	//HACK or we could one more pass of filtering *after* the resampling
	if (paramindex==MeteoData::RH) {
		if (md(paramindex)<0.01) md(paramindex)=0.01;
		if (md(paramindex)>1.) md(paramindex)=1.;
	} else if (paramindex==MeteoData::TA ||  paramindex==MeteoData::VW ||  paramindex==MeteoData::VW_MAX || paramindex==MeteoData::ISWR || paramindex==MeteoData::RSWR || paramindex==MeteoData::ILWR || paramindex==MeteoData::TSG || paramindex==MeteoData::TSS || paramindex==MeteoData::TAU_CLD) {
		if (md(paramindex)<0.)
			md(paramindex)=0.;
	}

	md.setResampled(true);
}

} //namespace
