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

#include <meteoio/meteoResampling/Accumulate.h>
#include <meteoio/IOUtils.h>

#include <sstream>

namespace mio {

Accumulate::Accumulate(const std::string& i_algoname, const std::string& i_parname, const double& dflt_window_size, const std::vector< std::pair<std::string, std::string> >& vecArgs)
           : ResamplingAlgorithms(i_algoname, i_parname, dflt_window_size, vecArgs),
             accumulate_period(IOUtils::nodata), strict(false)
{
	const std::string where( "Interpolations1D::"+i_parname+"::"+i_algoname );
	bool has_period=false;

	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="PERIOD") {
			IOUtils::parseArg(vecArgs[ii], where, accumulate_period);
			accumulate_period /= 86400.; //user uses seconds, internally julian day is used
			if (accumulate_period<=0.) {
				std::ostringstream ss;
				ss << "Invalid accumulation period (" << accumulate_period << ") for \"" << where << "\"";
				throw InvalidArgumentException(ss.str(), AT);
			}
			has_period = true;
		} else if (vecArgs[ii].first=="STRICT") {
			IOUtils::parseArg(vecArgs[ii], where, strict);
		}
	}

	if (!has_period) throw InvalidArgumentException("Please provide a PERIOD for "+where, AT);
}

std::string Accumulate::toString() const
{
	std::ostringstream ss;
	ss << std::right << std::setw(10) << parname << "::"  << std::left << std::setw(15) << algo;
	ss << "[ period=" << accumulate_period << " strict=" << std::boolalpha << strict << std::noboolalpha << " ]";
	return ss.str();
}

//find the index just before the start of accumulation period
size_t Accumulate::findStartOfPeriod(const std::vector<MeteoData>& vecM, const size_t& index, const Date& dateStart)
{
	size_t start_idx = IOUtils::npos;
	for (size_t idx=index+1; idx--> 0; ) { //because idx gets decremented right away
		if ( vecM[idx].date <= dateStart) {
			start_idx = idx;
			break;
		}
	}

	return start_idx;
}

//accumulate within a single input time step
double Accumulate::easySampling(const std::vector<MeteoData>& vecM, const size_t& paramindex, const size_t& /*index*/, const size_t& start_idx, const Date& dateStart, const Date& resampling_date) const
{//to keep in mind: start_idx is last index <= dateStart and index is first index >= resampling_date
	double sum = IOUtils::nodata;
	const double start_val = partialAccumulateAtLeft(vecM, paramindex, start_idx, dateStart);
	const double end_val = partialAccumulateAtLeft(vecM, paramindex, start_idx, resampling_date);

	if (start_val!=IOUtils::nodata && end_val!=IOUtils::nodata)
		sum = end_val - start_val;

	return sum;
}

double Accumulate::complexSampling(const std::vector<MeteoData>& vecM, const size_t& paramindex, const size_t& index, const size_t& start_idx, const Date& dateStart, const Date& resampling_date) const
{//to keep in mind: start_idx is last index <= dateStart and index is first index >= resampling_date
	double sum = IOUtils::nodata;
	//resample beginning point, in the [start_idx ; start_idx+1] interval
	const double start_val = partialAccumulateAtRight(vecM, paramindex, start_idx, dateStart);
	if (start_val!=IOUtils::nodata)
		sum = start_val;
	else if (strict) return IOUtils::nodata;

	//sum all whole periods AFTER the beginning point, in the [start_idx+2 ; index-1] interval
	for (size_t idx=(start_idx+2); idx<index; idx++) {
		const double curr_value = vecM[idx](paramindex);
		if (curr_value!=IOUtils::nodata) {
			if (sum!=IOUtils::nodata) sum += curr_value;
			else sum = curr_value;
		} else if (strict) return IOUtils::nodata;
	}

	//resample end point, in the [index-1 ; index] interval
	if (vecM[index].date>=resampling_date) { //we have enough data points to cover the whole accumulating period
		const double end_val = partialAccumulateAtLeft(vecM, paramindex, index-1, resampling_date);
		if (end_val!=IOUtils::nodata) {
			if (sum!=IOUtils::nodata) sum += end_val;
			else sum = end_val;
		} else if (strict) return IOUtils::nodata;
	} else {  //some data points are missing to cover the whole accumulating period
		if (strict) return IOUtils::nodata;
		const double end_val = vecM[index](paramindex);
		if (end_val!=IOUtils::nodata) {
			if (sum!=IOUtils::nodata) sum += end_val;
			else sum = end_val;
		}
	}

	return sum;
}

//index is the first element AFTER the resampling_date
void Accumulate::resample(const std::string& /*stationHash*/, const size_t& index, const ResamplingPosition& position, const size_t& paramindex,
                          const std::vector<MeteoData>& vecM, MeteoData& md)
{
	if (index >= vecM.size())
		throw IOException("The index of the element to be resampled is out of bounds", AT);
	if (position==ResamplingAlgorithms::begin || index==0) {
		return;
	}

	md(paramindex) = IOUtils::nodata;

	const Date resampling_date( md.date );
	const Date dateStart( resampling_date.getJulian() - accumulate_period, resampling_date.getTimeZone() );
	const size_t start_idx = findStartOfPeriod(vecM, index, dateStart);
	if (start_idx==IOUtils::npos) {//No acceptable starting point found
		std::cerr << "[W] Could not accumulate " << vecM.at(0).getNameForParameter(paramindex) << ": ";
		std::cerr << "not enough data for accumulation period at date " << resampling_date.toString(Date::ISO) << "\n";
		return;
	}

	//the period that should be accumulated is not even in vecM
	if (position==ResamplingAlgorithms::end && start_idx>=(vecM.size()-1)) { //the first element before "position" is the last of the vector
		return;
	}

	if ((index - start_idx) <= 1) {//easy upsampling when start & stop are in the same input time step
		//upsampling (for example, generate 15min values from hourly data)
		const double sum = easySampling(vecM, paramindex, index, start_idx, dateStart, resampling_date); //if resampling was unsuccesful, sum==IOUtils::nodata
		md(paramindex) = sum;
	} else {
		//downsampling (for example, generate daily values from hourly data)
		//and upsampling when resampled period falls accross a measurement timestamp
		const double sum = complexSampling(vecM, paramindex, index, start_idx, dateStart, resampling_date); //if resampling was unsuccesful, sum==IOUtils::nodata
		md(paramindex) = sum;
	}
}

} //namespace
