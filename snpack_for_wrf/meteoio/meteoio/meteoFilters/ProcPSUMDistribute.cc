/***********************************************************************************/
/*  Copyright 2014 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#include <meteoio/meteoFilters/ProcPSUMDistribute.h>
#include <cmath>

using namespace std;

namespace mio {

const double ProcPSUMDistribute::thresh_rh = .7;
const double ProcPSUMDistribute::thresh_Dt = 3.;

ProcPSUMDistribute::ProcPSUMDistribute(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name)
                  : ProcessingBlock(vecArgs, name), measured_period(IOUtils::nodata), is_soft(false)
{
	parse_args(vecArgs);
}

/** 
 * @brief Distributes a precipitation sum over the most probable preceeding time steps.
 * This works by looking at various criterias (like relative humidity, TA-TSS, etc) and picking up
 * the time steps that show the highest scores.
 * About this index usage in the code below: When looking into the ovec vector, they are:
 * \verbatim
 ___▒_____________▒_____________▒____
    ↑             ↑
    ii          endIdx
  \endverbatim
 * The goal is that the only "hard" references we can have are the accumulated value (ie the only values != nodata) 
 * and therefore their index are always kept (as ii and endIdx). This leads to the many "ii+1" in the code below since 
 * the period over which the data has to be distributed starts at ii+1 and goes until endIdx (which will become the new ii for the
 * next iteration).
 * 
 */
void ProcPSUMDistribute::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
                            std::vector<MeteoData>& ovec)
{
	ovec = ivec;
	const size_t nr_elems = ivec.size();
	bool first_true_point = true; //is it the first point with valid data?

	size_t ii=0;
	while (ii<nr_elems) {
		const Date startDate = ovec[ii].date;
		const Date endDate = startDate + measured_period;
		const size_t endIdx = findNextAccumulation(param, ovec, endDate, ii+1);

		//the proper end date is not in our vector
		if (endIdx==IOUtils::npos) {
			fillInterval(param, ovec, ii+1, nr_elems-1, 0.); //fill the rest with 0
			break;
		}
		
		if (endIdx==ii) { //no accumulation period found ahead of a large gap
			if (!is_soft) {
				std::ostringstream ss;
				ss << "Redistribution of precipitation before reaccumulation failed: precipitation value required ";
				ss << "in the " << startDate.toString(Date::ISO) << " - " << endDate.toString(Date::ISO) << " interval!\n";
				throw NoDataException(ss.str(), AT);
			}
			ovec[endIdx](param) = 0.;
			ii++;
			continue;
		}
		
		//at the end of the accumulation period, there is nodata
		if (ovec[endIdx](param)==IOUtils::nodata) {
			if (!is_soft) {
				std::ostringstream ss;
				ss << "Redistribution of precipitation before reaccumulation failed: precipitation value required ";
				ss << "in the " << startDate.toString(Date::ISO) << " - " << endDate.toString(Date::ISO) << " interval!\n";
				throw NoDataException(ss.str(), AT);
			}
			fillInterval(param, ovec, ii+1, endIdx, 0.); //fill the interval with 0
			ii = endIdx;
			continue;
		}

		//we might have found an accumulation that comes too early -> ok for first point, otherwise multiple accumulations
		if (ovec[endIdx].date<endDate && !first_true_point) {
			const double interval = endDate.getJulian(true) - startDate.getJulian(true);
			const string param_name = ovec[0].getNameForParameter(param);

			std::ostringstream ss;
			if (interval==measured_period) {
				ss << "The precipitation must be provided at the end of the accumulation period ";
				ss << "for the " << getName() << " filter, and this is not the case for " << endDate.toString(Date::ISO);
			} else {
				ss << "Accumulation period for \"" << param_name;
				ss << "\" found to be " << fixed << setprecision(0) << interval*86400. << " for " << endDate.toString(Date::ISO);
				ss << " when " << measured_period*86400. << " was given as arguments of \"" << getName() << "\"";
			}
			throw IOException(ss.str(), AT);
		}

		//we have a start, and end and a value -> we can redistribute it!
		const double precip = ovec[endIdx](param);
		//CstDistributePSUM(precip, ii+1, endIdx, param, ovec);
		SmartDistributePSUM(precip, ii+1, endIdx, param, ovec);

		ii = endIdx;
		first_true_point = false;
	}
}

//find the index of the first value > endDate. Please check if the matching value is not nodata!
//return IOUtils::npos if endDate is not in the provided vector.
size_t ProcPSUMDistribute::findNextAccumulation(const unsigned int& param, const std::vector<MeteoData>& ivec, const Date& endDate, size_t ii)
{
	const size_t nr_elems = ivec.size();
	while (ii<nr_elems && ivec[ii].date<=endDate && ivec[ii](param)==IOUtils::nodata) ii++;
	if (ii==nr_elems) return IOUtils::npos;
	
	if (ivec[ii].date>endDate) return ii-1; //we never found data!=nodata, so revert to last point in the period

	return ii;
}

void ProcPSUMDistribute::fillInterval(const unsigned int& param, std::vector<MeteoData>& ovec, const size_t& start, const size_t& end, const double value)
{
	for (size_t ii=start; ii<=end; ii++)
		ovec[ii](param) = value;
}


void ProcPSUMDistribute::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	const std::string where( "Filters::"+block_name );
	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="SOFT") {
			IOUtils::parseArg(vecArgs[ii], where, is_soft);
		} else if (vecArgs[ii].first=="MEAS_PERIOD") {
			IOUtils::parseArg(vecArgs[ii], where, measured_period);
			measured_period /= 86400.;
		}
	}
}

//distribute the precipitation equally on all time steps in [start_idx ; end_idx]
//it also handles non-uniform sampling rates
void ProcPSUMDistribute::CstDistributePSUM(const double& precip, const size_t& start_idx, const size_t& end_idx, const size_t& paramindex, std::vector<MeteoData>& vecM)
{
	if (precip==IOUtils::nodata) return;

	if (precip==0.) { //no precip, just fill with zeroes
		for (size_t ii=start_idx; ii<=end_idx; ii++)
			vecM[ii](paramindex) = 0.;
		return;
	} else {
		if (start_idx==0)
			throw IOException("Can not distribute without having the first data interval!", AT);

		const double total_dur = vecM[end_idx].date.getJulian(true) - vecM[start_idx-1].date.getJulian(true);
		for (size_t ii=start_idx; ii<=end_idx; ii++) {
			const double dur = vecM[ii].date.getJulian(true) - vecM[ii-1].date.getJulian(true);
			vecM[ii](paramindex) = precip * dur/total_dur;
		}
	}
}

//distribute the given precipitation amount equally on the timesteps that have the highest probability of precipitation
//it also handles non-uniform sampling rates
void ProcPSUMDistribute::SmartDistributePSUM(const double& precip, const size_t& start_idx, const size_t& end_idx, const size_t& paramindex, std::vector<MeteoData>& vecM)
{
	if (precip==IOUtils::nodata) return;
	if (precip==0.) {
		CstDistributePSUM(precip, start_idx, end_idx, paramindex, vecM);
		return;
	}

	if (start_idx==0)
		throw IOException("Can not distribute without having the first data interval!", AT);

	const size_t nr_elems = end_idx-start_idx+1;
	std::vector<unsigned char> vecScores(nr_elems);

	//assign a score for each timestep according to the possibility of precipitation
	size_t nr_score1 = 0, nr_score2 = 0; //count how many in each possible score categories
	double duration1 = 0., duration2 = 0.; //sum to calculate the total duration of each scores
	for (size_t ii=0; ii<nr_elems; ii++) { //we keep a local index "ii" for the scores
		unsigned char score = 0;
		const double rh = vecM[ii+start_idx](MeteoData::RH);
		const double ta = vecM[ii+start_idx](MeteoData::TA);
		const double tss = vecM[ii+start_idx](MeteoData::TSS);

		//assign the scores
		if (rh!=IOUtils::nodata && rh>=thresh_rh) //enough moisture for precip
			score++;
		if (ta!=IOUtils::nodata && tss!=IOUtils::nodata && (ta-tss)<=thresh_Dt ) //cloudy sky condition
			score++;

		//counters to know how many points received each possible scores
		if (score==1) {
			nr_score1++;
			const double duration = vecM[ii+start_idx].date.getJulian(true) - vecM[ii+start_idx-1].date.getJulian(true);
			duration1 += duration;
		}
		if (score==2) {
			nr_score2++;
			const double duration = vecM[ii+start_idx].date.getJulian(true) - vecM[ii+start_idx-1].date.getJulian(true);
			duration2 += duration;
		}
		vecScores[ii] = score; //score for the current point
	}

	//find out what is the highest score and how many points received it, so we can compute the precipitation increment for each point
	const unsigned char winning_scores = (nr_score2>0)? (unsigned char)2 : (nr_score1>0)? (unsigned char)1 : (unsigned char)0;
	const double whole_duration = vecM[end_idx].date.getJulian(true) - vecM[start_idx-1].date.getJulian(true);
	const double winning_duration = (winning_scores==2)? duration2 : (winning_scores==1)? duration1 : whole_duration;

	//distribute the precipitation on the time steps that have the highest scores
	for (size_t ii=0; ii<nr_elems; ii++) {
		if (vecScores[ii]==winning_scores) {
			const double duration = vecM[ii+start_idx].date.getJulian(true) - vecM[ii+start_idx-1].date.getJulian(true);
			vecM[ii+start_idx](paramindex) = precip * duration/winning_duration;
		} else
			vecM[ii+start_idx](paramindex) = 0.; //all other time steps set to 0
	}
}

} //namespace
