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

#include <meteoio/meteoResampling/NearestNeighbour.h>
#include <meteoio/meteoStats/libinterpol1D.h>
#include <meteoio/IOUtils.h>

#include <sstream>

namespace mio {

NearestNeighbour::NearestNeighbour(const std::string& i_algoname, const std::string& i_parname, const double& dflt_window_size, const std::vector< std::pair<std::string, std::string> >& vecArgs)
                 : ResamplingAlgorithms(i_algoname, i_parname, dflt_window_size, vecArgs), extrapolate(false)
{
	const std::string where( "Interpolations1D::"+i_parname+"::"+i_algoname );
	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="WINDOW_SIZE") {
			IOUtils::parseArg(vecArgs[ii], where, window_size);
			window_size /= 86400.; //user uses seconds, internally julian day is used
			if (window_size<=0.) {
				std::ostringstream ss;
				ss << "Invalid accumulation period (" << window_size << ") for \"" << where << "\"";
				throw InvalidArgumentException(ss.str(), AT);
			}
		} else if (vecArgs[ii].first=="EXTRAPOLATE") {
			IOUtils::parseArg(vecArgs[ii], where, extrapolate);
		}
	}
}

std::string NearestNeighbour::toString() const
{
	std::ostringstream ss;
	ss << std::right << std::setw(10) << parname << "::"  << std::left << std::setw(15) << algo;
	ss << "[ window_size=" << window_size << " extrapolate=" << std::boolalpha << extrapolate << std::noboolalpha << " ]";
	return ss.str();
}

void NearestNeighbour::resample(const std::string& stationHash, const size_t& index, const ResamplingPosition& position, const size_t& paramindex,
                                const std::vector<MeteoData>& vecM, MeteoData& md)
{
	if (index >= vecM.size())
		throw IOException("The index of the element to be resampled is out of bounds", AT);

	if (position == ResamplingAlgorithms::exact_match) {
		const double value = vecM[index](paramindex);
		if (value != IOUtils::nodata) {
			md(paramindex) = value; //propagate value
			return;
		}
	}

	//if we are at the very beginning or end of vecM and !extrapolate, then there's nothing to do
	if (((!extrapolate) && (position == ResamplingAlgorithms::end))
	    || ((!extrapolate) && (position == ResamplingAlgorithms::begin)))
		return;

	const Date resampling_date( md.date );
	size_t indexP1=IOUtils::npos, indexP2=IOUtils::npos;
	getNearestValidPts(stationHash, index, paramindex, vecM, resampling_date, window_size, indexP1, indexP2);
	const bool foundP1=(indexP1!=IOUtils::npos), foundP2=(indexP2!=IOUtils::npos);

	//Try to find the nearest neighbour, if there are two equally distant, then return the arithmetic mean
	if (foundP1 && foundP2) { //standard behavior
		const Duration diff1( resampling_date - vecM[indexP1].date ); //calculate time interval to element at index
		const Duration diff2( vecM[indexP2].date - resampling_date ); //calculate time interval to element at index
		const double val1 = vecM[indexP1](paramindex);
		const double val2 = vecM[indexP2](paramindex);

		if (IOUtils::checkEpsilonEquality(diff1.getJulian(true), diff2.getJulian(true), 0.1/1440.)) { //within 6 seconds
			md(paramindex) = Interpol1D::weightedMean(val1, val2, 0.5);
		} else if (diff1 < diff2) {
			md(paramindex) = val1;
		} else if (diff1 > diff2) {
			md(paramindex) = val2;
		}
	} else if (extrapolate) {
		if (foundP1 && !foundP2) { //nearest neighbour on found after index 'index'
			md(paramindex) = vecM[indexP1](paramindex);
		} else if (!foundP1 && foundP2) { //nearest neighbour on found before index 'index'
			md(paramindex) = vecM[indexP2](paramindex);
		} else { // no nearest neighbour with a value different from IOUtils::nodata
			return;
		}
	}
}

} //namespace
