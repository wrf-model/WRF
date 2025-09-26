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
#ifndef TIMESERIESMANAGER_H
#define TIMESERIESMANAGER_H

#include <meteoio/DataGenerator.h>
#include <meteoio/MeteoProcessor.h>
#include <meteoio/dataClasses/MeteoData.h>
#include <meteoio/dataClasses/Buffer.h>
#include <meteoio/IOHandler.h>
#include <meteoio/Config.h>

namespace mio {

class TimeSeriesManager {
	public:
		enum cache_types {
			RAW,	///< the raw data cache
			FILTERED,	///< the filtered data cache
			POINTS,	///< the (interpolated) points cache
			ALL	///< all caches
		};
		
		/**
		 * @brief Default constructor
		 * @details The mode and first_rank variables are used to decide which processing might have
		 * to be skipped when running "special" modes, such as virtual stations (for example, re-accumulating
		 * precipitation on VSTATIONS_REFRESH_RATE in the first TimeSeriesManager and as specified by
		 * the user in the second one).
		 * @param[in] in_iohandler IOHandler to get the data from
		 * @param[in] in_cfg User configuration
		 * @param[in] rank in case of multiple TimeSeriesManager, rank in the stack? (default: 1)
		 * @param[in] mode spatial resampling operation mode (see IOUtils::OperationMode), default IOUtils::STD
		 */
		TimeSeriesManager(IOHandler& in_iohandler, const Config& in_cfg, const char& rank=1, const IOUtils::OperationMode &mode=IOUtils::STD);

		size_t getStationData(const Date& date, STATIONS_SET& vecStation);

		/**
		* @brief Fill vecMeteo with a time series of objects
		* corresponding to the interval indicated by dateStart and dateEnd.
		* Depending on the ProcessingLevel for the instance of the TimeSeriesManager
		* the data returned will be either raw (read directly from the IOHandler)
		* or processed (read from a buffer and filtered through the MeteoProcessor).
		* vecMeteo will be empty if no datasets were retrieved in the interval defined
		* by dateStart and dateEnd
		*
		* @param[in] dateStart   A Date object representing the beginning of an interval (inclusive)
		* @param[in] dateEnd     A Date object representing the end of an interval (inclusive)
		* @param[out] vecVecMeteo A vector of vector<MeteoData> objects to be filled with data
		* @return            Number of stations for which data has been found in the interval
		*/
		size_t getMeteoData(const Date& dateStart, const Date& dateEnd,
		                    std::vector< METEO_SET >& vecVecMeteo);

		/**
		 * @brief Fill vector<MeteoData> object with multiple instances of MeteoData
		 * corresponding to the instant indicated by a Date object. Each MeteoData
		 * instance within the vector represents the data for one station at the given
		 * instant. Depending on the ProcessingLevel configured data will be either
		 * raw (read directly from the IOHandler)
		 * or processed (read from a buffer and filtered through the MeteoProcessor).
		 *
		 * NOTE:
		 * - vecMeteo will be empty if there is no data found for any station
		 *
		 * @param[in] i_date      A Date object representing the date/time for the sought MeteoData objects
		 * @param[out] vecMeteo    A vector of MeteoData objects to be filled with data
		 * @return            Number of stations for which data has been found in the interval
		 */
		size_t getMeteoData(const Date& i_date, METEO_SET& vecMeteo);

		/**
		 * @brief Push a vector of time series of MeteoData objects into the TimeSeriesManager. This overwrites
		 *        any internal buffers that are used and subsequent calls to getMeteoData or interpolate
		 *        will be performed upon this data. This method is a way to bypass the internal reading
		 *        of MeteoData from a certain source and is useful in case the user is only interested
		 *        in data processing and interpolation performed by the TimeSeriesManager object.
		 * @param[in] level Level of processing that has already been performed on the data (raw XOR filtered)
		 * @param[in] date_start Representing the beginning of the data
		 * @param[in] date_end Representing the end of the data
		 * @param[in] vecMeteo The actual data being pushed into the TimeSeriesManager object
		 * @param[in] invalidate_cache Should the point_cache be reset? (set it to false when pushing resampled data!)
		 */
		void push_meteo_data(const IOUtils::ProcessingLevel& level, const Date& date_start, const Date& date_end,
		                     const std::vector< MeteoData >& vecMeteo, const bool& invalidate_cache=true);
		
		void push_meteo_data(const IOUtils::ProcessingLevel& level, const Date& date_start, const Date& date_end,
		                     const std::vector< METEO_SET >& vecMeteo);

		/**
		 * @brief Set the desired ProcessingLevel of the TimeSeriesManager instance
		 *        The processing level affects the way meteo data is read and processed
		 *        Three values are possible:
		 *        - IOUtils::raw data shall be read directly from the buffer
		 *        - IOUtils::filtered data shall be filtered before returned to the user
		 *        - IOUtils::resampled data shall be resampled before returned to the user
		 *          this only affects the function getMeteoData(const Date&, METEO_DATASET&);
		 *
		 *        The three values can be combined: e.g. IOUtils::filtered | IOUtils:resampled
		 * @param[in] i_level The ProcessingLevel values that shall be used to process data
		 */
		void setProcessingLevel(const unsigned int& i_level);

		/**
		 * @brief Set buffer window properties requirements as known to the application itself.
		 * @details This will compare these requirements with the ones expressed by the end user and keep the max between them.
		 * The method can be called several times, it will NOT reset the calculated buffer's requirements but keep
		 * on merging with new submissions. Any parameter given as IOUtils::nodata will be ignored.
		 * @param[in] buffer_size buffer size in days
		 * @param[in] buff_before buffer centering in days
		 */
		void setBufferProperties(const double& buffer_size, const double& buff_before);

		void setRawBufferProperties(const Date& raw_buffer_start, const Date& raw_buffer_end);
		
		/**
		 * @brief Get the current buffer requirements (ie user defined buffer + filters/resampling requirements)
		 * @param[out] o_buffer_size buffer size in days
		 * @param[out] o_buff_before buffer centering in days
		 */
		void getBufferProperties(Duration &o_buffer_size, Duration &o_buff_before) const;

		/**
		 * @brief Returns the average sampling rate in the data.
		 * @details This computes the average sampling rate of the data that is contained in the buffer. This is a quick
		 * estimate, centered on how often a station measures "something" (ie, how many timestamps do we have
		 * for this station in the buffer). if the station measures TA at h+0 and h+30 and
		 * RH at h+15 and h+45, it would return 4 measurements per hour. If the station measures TA and RH at h+0 and h+30,
		 * it would return 2 measurements per hour.
		 * @return average sampling rate in Hz, nodata if the buffer is empty
		 */
		double getAvgSamplingRate() const;

		void writeMeteoData(const std::vector< METEO_SET >& vecMeteo, const std::string& name="");

		const std::string toString() const;

		/**
		 * @brief Add a METEO_SET for a specific instance to the point cache. 
		 * @details This is a way to manipulate
		 * MeteoData variables and be sure that the manipulated values are later used for requests
		 * regarding that specific date (e.g. 2D interpolations)
		 *
		 * @param[in] i_date Representing a point in time
		 * @param[in] vecMeteo A vector of MeteoData objects to be copied into the point cache
		 */
		void add_to_points_cache(const Date& i_date, const METEO_SET& vecMeteo);

		/**
		 * @brief Clear the chosen cache.
		 * @param[in] cache Select the cache to clear
		 */
		void clear_cache(const cache_types& cache);
		
		/**
		 * @brief Returns a copy of the internal Config object.
		 * @details This is convenient to clone an iomanager
		 * @return new Config object as a copy of the internal Config
		 */
		const Config getConfig() const {return cfg;}

		/**
		 * @brief Returns a copy of the internal IOHandler object.
		 * This is convenient to clone an iomanager
		 * @return new IOHandler object as a copy of the internal IOHandler
		 */
		IOHandler& getIOHandler() const {return iohandler;}

		/**
		 * @brief Returns the beginning of the raw buffer.
		 * @details This is the start date of the <b>request</b> that was given to the IOHandler. If there was no data
		 * at this date, then the date of the first data would be greater.
		 * @param[in] cache Select the cache query (obvisouly, ALL and POINTS are not supported)
		 * @return start date of the buffer
		 */
		Date getBufferStart(const cache_types& cache) const;

		/**
		 * @brief Returns the end of the raw buffer.
		 * @details This is the end date of the <b>request</b> that was given to the IOHandler. If there was no data
		 * at this date, then the date of the last data would be less.
		 * @param[in] cache Select the cache query (obvisouly, ALL and POINTS are not supported)
		 * @return end date of the buffer
		 */
		Date getBufferEnd(const cache_types& cache) const;
		
		/**
		 * @brief Returns the real beginning of the raw data in buffer.
		 * @details This is the start date of the <b>available data</b> that is in the buffer 
		 * (it can be much later than the requested start date).
		 * @param[in] cache Select the cache query
		 * @return start date of the data or Date::undefined if no data is available
		 */
		Date getDataStart(const cache_types& cache) const;
		
		/**
		 * @brief Returns the real end of the raw data in buffer.
		 * @details This is the end date of the <b>available data</b> that is in the buffer 
		 * (it can be much earlier than the requested end date).
		 * @param[in] cache Select the cache query
		 * @return end date of the data or Date::undefined if no data is available
		 */
		Date getDataEnd(const cache_types& cache) const;

		
	private:
		static bool compare(std::pair<Date, METEO_SET> p1, std::pair<Date, METEO_SET> p2);
		void setDfltBufferProperties();
		void fill_filtered_cache();
		void fillRawBuffer(const Date& date_start, const Date& date_end);

		const Config& cfg;
		IOHandler& iohandler;
		MeteoProcessor meteoprocessor;
		DataGenerator dataGenerator;

		ProcessingProperties proc_properties; ///< buffer constraints in order to be able to compute the requested values
		std::map<Date, METEO_SET > point_cache;  ///< stores already resampled data points
		MeteoBuffer raw_buffer; ///< stores raw data
		MeteoBuffer filtered_cache; ///< stores already filtered data intervals

		Date raw_requested_start, raw_requested_end; ///< in order to externally request that a given time period should be in the raw buffer
		Duration chunk_size; ///< How much data to read at once
		Duration buff_before; ///< How much data to read before the requested date in buffer
		unsigned int processing_level;
};
} //end namespace
#endif
