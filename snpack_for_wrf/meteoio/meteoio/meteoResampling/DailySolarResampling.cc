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

#include <meteoio/meteoResampling/DailySolarResampling.h>
#include <meteoio/meteoLaws/Sun.h>
#include <meteoio/meteoLaws/Atmosphere.h>
#include <meteoio/MathOptim.h>
#include <meteoio/IOUtils.h>
#include <meteoio/meteoStats/libinterpol1D.h>

#include <sstream>

namespace mio {

const size_t Daily_solar::samples_per_day = 24*3; //every 20 minutes

Daily_solar::Daily_solar(const std::string& i_algoname, const std::string& i_parname, const double& dflt_window_size, const std::vector< std::pair<std::string, std::string> >& vecArgs)
            : ResamplingAlgorithms(i_algoname, i_parname, dflt_window_size, vecArgs), radiation(), station_index(), dateStart(), dateEnd(), loss_factor()
{
	const std::string where( "Interpolations1D::"+i_parname+"::"+i_algoname );
	if (!vecArgs.empty()) {
		throw InvalidArgumentException("Too many arguments for \""+where+"\"", AT);
	}
}

std::string Daily_solar::toString() const
{
	std::ostringstream ss;
	ss << std::right << std::setw(10) << parname << "::"  << std::left << std::setw(15) << algo << "[ ]";
	return ss.str();
}

//compute the daily sum of solar radiation as well as fill a vector containing the solar radiation at a regular sampling rate
double Daily_solar::compRadiation(const double& lat, const double& lon, const double& alt, const double& HS, const size_t& stat_idx)
{
	const double P = Atmosphere::stdAirPressure(alt);
	double albedo = 0.5;
	if (HS!=IOUtils::nodata) //no big deal if we can not adapt the albedo
		albedo = (HS>=snow_thresh)? snow_albedo : soil_albedo;
	const double TA=274.98, RH=0.666; //the reduced precipitable water will get an average value

	SunObject sun(lat, lon, alt);
	double sum = 0.;
	size_t index=0;
	for (Date date(dateStart[stat_idx]); date<dateEnd[stat_idx]; date += 1./double(samples_per_day)) {
		//compute potential solar radiation at this time step
		sun.setDate(date.getJulian(), date.getTimeZone());
		sun.calculateRadiation(TA, RH, P, albedo);
		double toa, direct, diffuse;
		sun.getHorizontalRadiation(toa, direct, diffuse);
		const double global_h = direct+diffuse;

		//compute the integral by a simple triangle method
		sum += global_h * static_cast<double>(24*3600/samples_per_day);

		//store the radiation for later reuse
		radiation[stat_idx][index++] = global_h;
	}

	return sum;
}

//interpolate the solar radiation from the vector containing regularly sampled solar radiation
double Daily_solar::getSolarInterpol(const Date& resampling_date, const size_t& stat_idx) const
{
	const double in_day = (resampling_date.getJulian() - dateStart[stat_idx].getJulian()) / (dateEnd[stat_idx].getJulian() - dateStart[stat_idx].getJulian());
	const size_t vec_index = static_cast<size_t>( Optim::floor(in_day*samples_per_day) ); //so the sample will be between vec_index and vec_index+1
	const double weight = in_day*static_cast<double>(samples_per_day) - static_cast<double>(vec_index);

	return Interpol1D::weightedMean(radiation[stat_idx][vec_index], radiation[stat_idx][vec_index+1], weight);
}

//a new, previously unknown station has been found, allocate the memory
size_t Daily_solar::getStationIndex(const std::string& key)
{
	const size_t nr_stations = station_index.size();
	for (size_t ii=0; ii<nr_stations; ++ii) {
		if (station_index[ii]==key)
			return ii;
	}

	radiation.push_back( std::vector<double>(samples_per_day, 0.) );
	loss_factor.push_back( 0. );

	Date null_date(0., 0.);
	dateStart.push_back( null_date );
	dateEnd.push_back( null_date );

	station_index.push_back( key );
	return nr_stations; //the new index is the old nr_stations
}

void Daily_solar::resample(const std::string& stationHash, const size_t& index, const ResamplingPosition& /*position*/, const size_t& paramindex,
                           const std::vector<MeteoData>& vecM, MeteoData& md)
{
	if (index >= vecM.size())
		throw IOException("The index of the element to be resampled is out of bounds", AT);

	const double lat = md.meta.position.getLat();
	const double lon = md.meta.position.getLon();
	const double alt = md.meta.position.getAltitude();
	if (lat==IOUtils::nodata || lon==IOUtils::nodata || alt==IOUtils::nodata) return;
	const double HS = md(MeteoData::HS);

	//get station index
	const size_t stat_idx = getStationIndex(stationHash);

	//has the radiation already been calculated for this day and station?
	if (md.date<dateStart[stat_idx] || md.date>=dateEnd[stat_idx]) {
		dateStart[stat_idx] = getDailyStart(md.date);
		dateEnd[stat_idx] = dateStart[stat_idx]+1.;
		//const size_t indexP = getNearestValidPt(vecM, paramindex, stat_idx, index);
		const size_t indexP = getDailyValue(vecM, paramindex, index, dateStart[stat_idx], dateEnd[stat_idx]);
		if (indexP==IOUtils::npos) { //no daily sum found for the current day
			loss_factor[stat_idx] = IOUtils::nodata;
			return;
		}

		const double daily_sum = compRadiation(lat, lon, alt, HS, stat_idx);
		loss_factor[stat_idx] = (daily_sum>0)? vecM[indexP](paramindex) / daily_sum : 0.; //in case of polar night...
	}

	if (loss_factor[stat_idx]==IOUtils::nodata) //the station could not be calculated for this day
		return;

	//interpolate radiation for this timestep and write it out
	const double rad = getSolarInterpol(md.date, stat_idx);
	if (paramindex==MeteoData::ISWR) {
		md(paramindex) = loss_factor[stat_idx] * rad;
	} else {
		double albedo = 0.5;
		if (HS!=IOUtils::nodata) //no big deal if we can not adapt the albedo
			albedo = (HS>=snow_thresh)? snow_albedo : soil_albedo;
		md(paramindex) = loss_factor[stat_idx] * rad * albedo;
	}

	md.setResampled(true);
}

} //namespace
