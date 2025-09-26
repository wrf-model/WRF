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

#include <meteoio/TimeSeriesManager.h>

#include <algorithm>

using namespace std;

namespace mio {

bool TimeSeriesManager::compare(std::pair<Date, METEO_SET> p1, std::pair<Date, METEO_SET> p2) {
	return p1.first < p2.first;
}

TimeSeriesManager::TimeSeriesManager(IOHandler& in_iohandler, const Config& in_cfg, const char& rank, const IOUtils::OperationMode &mode) : cfg(in_cfg), iohandler(in_iohandler),
                                            meteoprocessor(in_cfg, rank, mode), dataGenerator(in_cfg),
                                            proc_properties(), point_cache(), raw_buffer(), filtered_cache(),
                                            raw_requested_start(), raw_requested_end(), chunk_size(), buff_before(),
                                            processing_level(IOUtils::raw | IOUtils::filtered | IOUtils::resampled | IOUtils::generated)
{
	meteoprocessor.getWindowSize(proc_properties);
	setDfltBufferProperties();
}

void TimeSeriesManager::setDfltBufferProperties()
{
	double chunk_size_days = 370.; //default chunk size value
	cfg.getValue("BUFFER_SIZE", "General", chunk_size_days, IOUtils::nothrow); //in days
	chunk_size = Duration(chunk_size_days, 0);

	//get buffer centering options
	double buff_centering = -1.;
	double buff_start = -1.;
	cfg.getValue("BUFF_CENTERING", "General", buff_centering, IOUtils::nothrow);
	cfg.getValue("BUFF_BEFORE", "General", buff_start, IOUtils::nothrow);
	if ((buff_centering != -1.) && (buff_start != -1.))
		throw InvalidArgumentException("Please do NOT provide both BUFF_CENTERING and BUFF_BEFORE!!", AT);

	if (buff_start != -1.) {
		buff_before = Duration(buff_start, 0);
	} else {
		if (buff_centering != -1.) {
			if ((buff_centering < 0.) || (buff_centering > 1.))
				throw InvalidArgumentException("BUFF_CENTERING must be between 0 and 1", AT);

			buff_before = chunk_size * buff_centering;
		} else {
			buff_before = chunk_size * 0.05; //5% centering by default
		}
	}

	//if buff_before>chunk_size, we will have a problem (ie: we won't ever read the whole data we need)
	if (buff_before>chunk_size) chunk_size = buff_before;
	//NOTE we still have the meteo1d window in the way
	//NOTE -> we end up not reading enough data and rebuffering... solution: never only use the buffer definition but add proc_properties
}

void TimeSeriesManager::setBufferProperties(const double& i_chunk_size, const double& i_buff_before)
{
	if (i_buff_before!=IOUtils::nodata) {
		const Duration app_buff_before(i_buff_before, 0);
		if (app_buff_before>buff_before) buff_before = app_buff_before;
	}
	if (i_chunk_size!=IOUtils::nodata) {
		const Duration app_chunk_size(i_chunk_size, 0);
		if (app_chunk_size>chunk_size) chunk_size = app_chunk_size;
	}

	//if buff_before>chunk_size, we will have a problem (ie: we won't ever read the whole data we need)
	if (buff_before>chunk_size) chunk_size = buff_before;
}

void TimeSeriesManager::setRawBufferProperties(const Date& raw_buffer_start, const Date& raw_buffer_end)
{
	if (!raw_buffer_start.isUndef()) raw_requested_start = raw_buffer_start;
	if (!raw_buffer_end.isUndef()) raw_requested_end = raw_buffer_end;
}

void TimeSeriesManager::getBufferProperties(Duration &o_buffer_size, Duration &o_buff_before) const
{
	o_buffer_size = chunk_size+proc_properties.time_before+proc_properties.time_after;
	o_buff_before = buff_before+proc_properties.time_before;
}

void TimeSeriesManager::setProcessingLevel(const unsigned int& i_level)
{
	if (i_level >= IOUtils::num_of_levels)
		throw InvalidArgumentException("The processing level is invalid", AT);

	processing_level = i_level;
}

double TimeSeriesManager::getAvgSamplingRate() const
{
	const double raw_rate = raw_buffer.getAvgSamplingRate();
	if (raw_rate!=IOUtils::nodata)
		return raw_rate;
	else
		return filtered_cache.getAvgSamplingRate();
}

Date TimeSeriesManager::getBufferStart(const cache_types& cache) const
{
	switch(cache) {
		case RAW: 
			return raw_buffer.getBufferStart();
		case FILTERED: 
			return filtered_cache.getBufferStart();
		default:
			throw InvalidArgumentException("Unsupported cache type provided", AT);
	}
}

Date TimeSeriesManager::getBufferEnd(const cache_types& cache) const 
{
	switch(cache) {
		case RAW: 
			return raw_buffer.getBufferEnd();
		case FILTERED: 
			return filtered_cache.getBufferEnd();
		default:
			throw InvalidArgumentException("Unsupported cache type provided", AT);
	}
}

Date TimeSeriesManager::getDataStart(const cache_types& cache) const
{
	switch(cache) {
		case RAW: 
			return raw_buffer.getDataStart();
		case FILTERED: 
			return filtered_cache.getDataStart();
		case POINTS: {
			const std::pair<Date, METEO_SET> min( *std::min_element(point_cache.begin(), point_cache.end(), compare) );
			return min.first;
		}
		default:
			throw InvalidArgumentException("Unsupported cache type provided", AT);
	}
}

Date TimeSeriesManager::getDataEnd(const cache_types& cache) const 
{
	switch(cache) {
		case RAW: 
			return raw_buffer.getDataEnd();
		case FILTERED: 
			return filtered_cache.getDataEnd();
		case POINTS: {
			const std::pair<Date, METEO_SET> max( *std::max_element(point_cache.begin(), point_cache.end(), compare) );
			return max.first;
		}
		default:
			throw InvalidArgumentException("Unsupported cache type provided", AT);
	}
}

void TimeSeriesManager::push_meteo_data(const IOUtils::ProcessingLevel& level, const Date& date_start, const Date& date_end,
                                const std::vector< METEO_SET >& vecMeteo)
{
	//perform check on date_start and date_end
	if (date_end < date_start) {
		const std::string ss( "Trying to push data set from " + date_start.toString(Date::ISO) + " to " + date_end.toString(Date::ISO) + ". " + " Obviously, date_start should be less than date_end!");
		throw InvalidArgumentException(ss, AT);
	}

	if (level == IOUtils::filtered) {
		filtered_cache.push(date_start, date_end, vecMeteo);
	} else if (level == IOUtils::raw) {
		filtered_cache.clear();
		raw_buffer.push(date_start, date_end, vecMeteo);
	} else {
		throw InvalidArgumentException("The processing level is invalid (should be raw OR filtered)", AT);
	}

	point_cache.clear(); //clear point cache, so that we don't return resampled values of deprecated data
}

void TimeSeriesManager::push_meteo_data(const IOUtils::ProcessingLevel& level, const Date& date_start, const Date& date_end,
		                     const std::vector< MeteoData >& vecMeteo, const bool& invalidate_cache)
{
	//perform check on date_start and date_end
	if (date_end < date_start) {
		const std::string ss( "Trying to push data set from " + date_start.toString(Date::ISO) + " to " + date_end.toString(Date::ISO) + ". " + " Obviously, date_start should be less than date_end!");
		throw InvalidArgumentException(ss, AT);
	}

	if (level == IOUtils::filtered) {
		filtered_cache.push(date_start, date_end, vecMeteo);
	} else if (level == IOUtils::raw) {
		filtered_cache.clear();
		raw_buffer.push(date_start, date_end, vecMeteo);
	} else {
		throw InvalidArgumentException("The processing level is invalid (should be raw OR filtered)", AT);
	}

	if (invalidate_cache) point_cache.clear(); //clear point cache, so that we don't return resampled values of deprecated data
}

//should we implement a cache for stationData?
size_t TimeSeriesManager::getStationData(const Date& date, STATIONS_SET& vecStation)
{
	vecStation.clear();
	iohandler.readStationData(date, vecStation);

	return vecStation.size();
}

//for an interval of data: decide whether data should be filtered or raw
size_t TimeSeriesManager::getMeteoData(const Date& dateStart, const Date& dateEnd, std::vector< METEO_SET >& vecVecMeteo)
{
	vecVecMeteo.clear();
	if (processing_level == IOUtils::raw) {
		iohandler.readMeteoData(dateStart, dateEnd, vecVecMeteo);
	} else {
		const bool success = filtered_cache.get(dateStart, dateEnd, vecVecMeteo);

		if (!success) {
			std::vector< std::vector<MeteoData> > tmp_meteo;
			const bool rebuffer_raw = raw_buffer.empty() || (raw_buffer.getBufferStart() > dateStart) || (raw_buffer.getBufferEnd() < dateEnd);
			if (rebuffer_raw && (IOUtils::raw & processing_level) == IOUtils::raw) fillRawBuffer(dateStart, dateEnd);
			raw_buffer.get(dateStart, dateEnd, tmp_meteo);

			//now it needs to be secured that the data is actually filtered, if configured
			if ((IOUtils::filtered & processing_level) == IOUtils::filtered) {
				fill_filtered_cache();
				filtered_cache.get(dateStart, dateEnd, vecVecMeteo);
			} else {
				vecVecMeteo = tmp_meteo;
			}
		}

		if ((IOUtils::generated & processing_level) == IOUtils::generated)
			dataGenerator.fillMissing(vecVecMeteo);
	}

	return vecVecMeteo.size(); //equivalent with the number of stations that have data
}

size_t TimeSeriesManager::getMeteoData(const Date& i_date, METEO_SET& vecMeteo)
{
	vecMeteo.clear();

	//1. Check whether user wants raw data or processed data
	//The first case: we are looking at raw data directly, only unresampled values are considered, exact date match
	if (processing_level == IOUtils::raw) {
		std::vector< std::vector<MeteoData> > vec_cache;
		static const Duration eps(1./(24.*3600.), 0.);
		iohandler.readMeteoData(i_date-eps, i_date+eps, vec_cache);
		for (size_t ii=0; ii<vec_cache.size(); ii++) { //for every station
			const size_t index = IOUtils::seek(i_date, vec_cache[ii], true);
			if (index != IOUtils::npos)
				vecMeteo.push_back( vec_cache[ii][index] ); //Insert station into vecMeteo
		}
		return vecMeteo.size();
	}

	//2.  Check which data point is available, buffered locally
	const map<Date, vector<MeteoData> >::const_iterator it = point_cache.find(i_date);
	if (it != point_cache.end()) {
		vecMeteo = it->second;
		return vecMeteo.size();
	}

	//Let's make sure we have the data we need, in the filtered_cache or in vec_cache
	Date buffer_start( i_date-proc_properties.time_before ), buffer_end( i_date+proc_properties.time_after );
	if (!raw_requested_start.isUndef()) {
		if (raw_requested_start<i_date) buffer_start = raw_requested_start - proc_properties.time_before;
		raw_requested_start.setUndef(true);
	}
	if (!raw_requested_end.isUndef()) {
		if (raw_requested_end>i_date) buffer_end = raw_requested_end + proc_properties.time_after;
		raw_requested_end.setUndef(true);
	}
	std::vector< vector<MeteoData> >* data = NULL; //reference to either filtered_cache or raw_buffer
	if ((IOUtils::filtered & processing_level) == IOUtils::filtered) {
		const bool rebuffer_filtered = filtered_cache.empty() || (filtered_cache.getBufferStart() > buffer_start) || (filtered_cache.getBufferEnd() < buffer_end);
		if (rebuffer_filtered) { //explicit caching, rebuffer if necessary
			if (!filtered_cache.empty())  //invalidate cached values in the resampling algorithms if necessary
				meteoprocessor.resetResampling();
				
			const bool rebuffer_raw = raw_buffer.empty() || (raw_buffer.getBufferStart() > buffer_start) || (raw_buffer.getBufferEnd() < buffer_end);
			if (rebuffer_raw && (IOUtils::raw & processing_level) == IOUtils::raw) {
				fillRawBuffer(buffer_start, buffer_end);
			}
			fill_filtered_cache();
		}
		data = &filtered_cache.getBuffer();
	} else { //data to be resampled should be IOUtils::raw
		if ((IOUtils::raw & processing_level) == IOUtils::raw) fillRawBuffer(buffer_start, buffer_end);
		data = &raw_buffer.getBuffer();
	}

	if ((IOUtils::resampled & processing_level) == IOUtils::resampled) { //resampling required
		for (size_t ii=0; ii<(*data).size(); ii++) { //for every station
			if ((*data)[ii].empty()) continue;
			const std::string stationHash( IOUtils::toString(ii)+"-"+(*data)[ii].front().meta.getHash() );
			MeteoData md;
			const bool success = meteoprocessor.resample(i_date, stationHash, (*data)[ii], md);
			if (success) vecMeteo.push_back( md );
		}
	} else { //no resampling required
		for (size_t ii=0; ii<(*data).size(); ii++) { //for every station
			const size_t index = IOUtils::seek(i_date, (*data)[ii], true); //needs to be an exact match
			if (index != IOUtils::npos)
				vecMeteo.push_back( (*data)[ii][index] ); //Insert station into vecMeteo
		}
	}

	if ((IOUtils::generated & processing_level) == IOUtils::generated)
		dataGenerator.fillMissing(vecMeteo);

	add_to_points_cache(i_date, vecMeteo); //Store result in the local cache

	return vecMeteo.size();
}

void TimeSeriesManager::writeMeteoData(const std::vector< METEO_SET >& vecMeteo, const std::string& name)
{
	iohandler.writeMeteoData(vecMeteo, name);
}

/**
 * @brief Filter the whole raw meteo data buffer
 */
void TimeSeriesManager::fill_filtered_cache()
{
	if ((IOUtils::filtered & processing_level) == IOUtils::filtered) {
		filtered_cache.clear(); //HACK until we get true ringbuffers, to prevent eating up all memory
		
		const Date filtered_start( raw_buffer.getBufferStart() );
		const Date filtered_end( raw_buffer.getBufferEnd() );
		std::vector< std::vector<MeteoData> > ivec;
		std::swap(ivec, raw_buffer.getBuffer()); //avoid one more copy of the whole dataset
		raw_buffer.clear(); //invalidate the raw data buffer since it has been swapped with the temporary ivec for filtering
		
		
		meteoprocessor.process(ivec, filtered_cache.getBuffer());
		filtered_cache.setBufferStart( filtered_start );
		filtered_cache.setBufferEnd( filtered_end );
	}
}

void TimeSeriesManager::add_to_points_cache(const Date& i_date, const METEO_SET& vecMeteo)
{
	//Check cache size, delete oldest elements if necessary
	if (point_cache.size() > 2000) {
		point_cache.clear(); //HACK: implement a true ring buffer!
	}

	point_cache[i_date] = vecMeteo;
}

void TimeSeriesManager::clear_cache(const cache_types& cache)
{
	switch(cache) {
		case RAW: 
			raw_buffer.clear(); 
			break;
		case FILTERED: 
			filtered_cache.clear(); 
			break;
		case POINTS: 
			point_cache.clear(); 
			break;
		case ALL: 
			raw_buffer.clear();
			filtered_cache.clear();
			point_cache.clear();
			break;
		default:
			throw InvalidArgumentException("Unknown cache type provided", AT);
	}
}

void TimeSeriesManager::fillRawBuffer(const Date& date_start, const Date& date_end)
{
	//computing the start and end date of the raw data request
	const Date new_start( date_start-buff_before ); //taking centering into account
	const Date new_end( max(date_start + chunk_size, date_end) );
	
	raw_buffer.clear(); //HACK until we have a proper ring buffer to avoid eating up all memory...

	if (raw_buffer.empty()) {
		std::vector< METEO_SET > vecMeteo;
		iohandler.readMeteoData(new_start, new_end, vecMeteo);
		raw_buffer.push(new_start, new_end, vecMeteo);
		return;
	}

	const Date buffer_start( raw_buffer.getBufferStart() );
	const Date buffer_end( raw_buffer.getBufferEnd() );
	if (new_start>buffer_end || new_end<buffer_start) { //easy: full rebuffer
		std::vector< METEO_SET > vecMeteo;
		iohandler.readMeteoData(new_start, new_end, vecMeteo);
		raw_buffer.push(new_start, new_end, vecMeteo);
		return;
	}

	if (new_start<buffer_start) { //some data must be inserted before
		std::vector< METEO_SET > vecMeteo;
		iohandler.readMeteoData(new_start, buffer_start, vecMeteo);
		raw_buffer.push(new_start, buffer_start, vecMeteo);
	}

	if (new_end>buffer_end) { //some data must be inserted after. Keep in mind both before and after could happen simultaneously!
		std::vector< METEO_SET > vecMeteo;
		iohandler.readMeteoData(buffer_end, new_end, vecMeteo);
		raw_buffer.push(buffer_end, new_end, vecMeteo);
	}

}

const std::string TimeSeriesManager::toString() const {
	ostringstream os;
	os << "<TimeSeriesManager>\n";
	os << "Config& cfg = " << hex << &cfg << dec << "\n";
	os << "IOHandler& iohandler = " << hex << &iohandler << dec << "\n";
	os << meteoprocessor.toString();
	os << "Processing level = " << processing_level << "\n";
	os << dataGenerator.toString();

	os << "RawBuffer:\n" << raw_buffer.toString();
	os << "Filteredcache:\n" << filtered_cache.toString();

	//display point_cache
	size_t count=0;
	size_t min_stations=std::numeric_limits<size_t>::max();
	size_t max_stations=0;
	std::map<Date, std::vector<MeteoData> >::const_iterator iter = point_cache.begin();
	for (; iter != point_cache.end(); ++iter) {
		const size_t nb_stations = iter->second.size();
		if (nb_stations>max_stations) max_stations=nb_stations;
		if (nb_stations<min_stations) min_stations=nb_stations;
		count++;
	}

	if (count==0) {
		os << "Resampled cache is empty\n";
	}
	if (count==1) {
		os << "Resampled cache content (";
		if (max_stations==min_stations)
			os << min_stations;
		else
			os << min_stations << " to " << max_stations;
		os << " station(s))\n";
		os << std::setw(22) << point_cache.begin()->first.toString(Date::ISO) << " - 1 timestep\n";
	}
	if (count>1) {
		const double avg_sampling = ( (point_cache.rbegin()->first.getJulian()) - (point_cache.begin()->first.getJulian()) ) / (double)(count-1);

		os << "Resampled cache content (";
		if (max_stations==min_stations)
			os << min_stations;
		else
			os << min_stations << " to " << max_stations;
		os << " station(s))\n";
		os << std::setw(22) << point_cache.begin()->first.toString(Date::ISO);
		os << " - " << point_cache.rbegin()->first.toString(Date::ISO);
		os << " - " << count << " timesteps (" << setprecision(3) << fixed << avg_sampling*24.*3600. << " s sampling rate)";
	}

	os << "</TimeSeriesManager>\n";
	return os.str();
}

} //namespace
