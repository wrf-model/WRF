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

#include <meteoio/meteoResampling/LinearResampling.h>
#include <meteoio/IOUtils.h>

#include <sstream>

namespace mio {

LinearResampling::LinearResampling(const std::string& i_algoname, const std::string& i_parname, const double& dflt_window_size, const std::vector< std::pair<std::string, std::string> >& vecArgs)
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

std::string LinearResampling::toString() const
{
	std::ostringstream ss;
	ss << std::right << std::setw(10) << parname << "::"  << std::left << std::setw(15) << algo;
	ss << "[ window_size=" << window_size << " extrapolate=" << std::boolalpha << extrapolate << std::noboolalpha << " ]";
	return ss.str();
}

void LinearResampling::resample(const std::string& stationHash, const size_t& index, const ResamplingPosition& position, const size_t& paramindex,
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

	const Date resampling_date = md.date;
	size_t indexP1=IOUtils::npos, indexP2=IOUtils::npos;
	getNearestValidPts(stationHash, index, paramindex, vecM, resampling_date, window_size, indexP1, indexP2);
	bool foundP1=(indexP1!=IOUtils::npos), foundP2=(indexP2!=IOUtils::npos);

	//do nothing if we can't interpolate, and extrapolation is not explicitly activated
	if ((!extrapolate) && ((!foundP1) || (!foundP2)))
		return;
	//do nothing if not at least one value different from IOUtils::nodata has been found
	if (!foundP1 && !foundP2)
		return;

	//At this point we either have a valid indexP1 or indexP2 and we can at least try to extrapolate
	if (!foundP1 && foundP2) { //only nodata values found before index, try looking after indexP2
		for (size_t ii=indexP2+1; ii<vecM.size(); ii++) {
			if (vecM[ii](paramindex) != IOUtils::nodata) {
				indexP1 = ii;
				foundP1 = true;
				break;
			}
		}
	} else if (foundP1 && !foundP2) { //only nodata found after index, try looking before indexP1
		for (size_t ii=indexP1; (ii--) > 0; ){
			if (vecM[ii](paramindex) != IOUtils::nodata){
				indexP2=ii;
				foundP2 = true;
				break;
			}
		}
	}

	if (!foundP1 || !foundP2) //now at least two points need to be present
		return;

	//At this point indexP1 and indexP2 point to values that are different from IOUtils::nodata
	const double val1 = vecM[indexP1](paramindex);
	const double jul1 = vecM[indexP1].date.getJulian(true);
	const double val2 = vecM[indexP2](paramindex);
	const double jul2 = vecM[indexP2].date.getJulian(true);

	md(paramindex) = linearInterpolation(jul1, val1, jul2, val2, resampling_date.getJulian(true));
}

} //namespace
