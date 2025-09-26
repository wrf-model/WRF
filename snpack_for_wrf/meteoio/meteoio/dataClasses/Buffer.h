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
#ifndef BUFFER_H
#define BUFFER_H

#include <meteoio/dataClasses/Grid2DObject.h>
#include <meteoio/dataClasses/DEMObject.h>
#include <meteoio/dataClasses/Date.h>
#include <meteoio/dataClasses/MeteoData.h>

namespace mio {

/**
 * @class MeteoBuffer
 * @brief A class to buffer meteorological data.
 * This class buffers MeteoData objects. It is currently NOT a proper ring buffer, this should come
 * in a later implementation.
 *
 * @ingroup data_str
 * @author Mathias Bavay
 * @date   2014-12-01
*/
class MeteoBuffer {
	public:
		MeteoBuffer() : ts_buffer(), ts_start(), ts_end() {}

		/**
		 * @brief Get buffer data for a specific date
		 * @param date        A Date object representing the date/time for the sought MeteoData objects
		 * @param vecMeteo    A vector of MeteoData objects to be filled with data
		 * @return            true if the data was in the buffer
		 */
		bool get(const Date& date, METEO_SET &vecMeteo) const;

		/**
		 * @brief Get buffer data between two dates
		 * @param date_start      A Date object representing the beginning of an interval (inclusive)
		 * @param date_end        A Date object representing the end of an interval (inclusive)
		 * @param vecMeteo        A vector of vector<MeteoData> objects to be filled with data
		 * @return            true if the data was in the buffer
		 */
		bool get(const Date& date_start, const Date& date_end, std::vector< METEO_SET > &vecMeteo) const;

		/**
		 * @brief Returns the average sampling rate in the data.
		 * This computes the average sampling rate of the data that is contained in the buffer. This is a quick
		 * estimate, centered on how often a station measures "something" (ie, how many timestamps do we have
		 * for this station in the buffer). if the station measures TA at h+0 and h+30 and
		 * RH at h+15 and h+45, it would return 4 measurements per hour. If the station measures TA and RH at h+0 and h+30,
		 * it would return 2 measurements per hour.
		 * @return average sampling rate in Hz, nodata if the buffer is empty
		 */
		double getAvgSamplingRate() const;

		/**
		 * @brief Returns the beginning of the buffer.
		 * This is the start date of the <b>request</b> that was given to the IOHandler. If there was no data
		 * at this date, then the date of the first data would be greater.
		 * @return start date of the buffer
		 */
		Date getBufferStart() const;

		/**
		 * @brief Returns the end of the buffer.
		 * This is the end date of the <b>request</b> that was given to the IOHandler. If there was no data
		 * at this date, then the date of the last data would be less.
		 * @return end date of the buffer
		 */
		Date getBufferEnd() const;
		
		/**
		 * @brief Returns the real beginning of the data contained in the buffer.
		 * This is the start date of the <b>available data</b> that is in the buffer (it can be much later than the requested start date).
		 * @return start date of the data or Date::undefined if no data is available
		 */
		Date getDataStart() const;
		
		/**
		 * @brief Returns the real end of the data contained in the buffer.
		 * This is the end date of the <b>available data</b> that is in the buffer (it can be much earlier than the requested end date).
		 * @return end date of the data or Date::undefined if no data is available
		 */
		Date getDataEnd() const;

		/**
		* @brief Check if the buffer is empty
		* @return true if the buffer is empty
		*/
		bool empty() const;

		/**
		* @brief Returns the number of stations present in the buffer
		* @return number of stations in buffer
		*/
		size_t size() const { return ts_buffer.size();}

		/**
		* @brief Clear the buffer; the data is deleted and the start and end dates reset to <i>undef</i>
		*/
		void clear();

		/**
		 * @brief Add data representing the available data between two dates.
		 * @param date_start      A Date object representing the beginning of an interval (inclusive)
		 * @param date_end        A Date object representing the end of an interval (inclusive)
		 * @param vecMeteo        A vector of vector<MeteoData> objects providing the data
		 */
		void push(const Date& date_start, const Date& date_end, const std::vector< METEO_SET >& vecMeteo);


		/**
		 * @brief Add a data point for several stations. The data is considered valid within the provided two dates
		 * @param date_start      A Date object representing the beginning of an interval (inclusive)
		 * @param date_end        A Date object representing the end of an interval (inclusive)
		 * @param vecMeteo        A vector of MeteoData objects providing the data
		 */
		void push(const Date& date_start, const Date& date_end, const std::vector<MeteoData>& vecMeteo);

		const std::string toString() const;

		//HACK: these should be removed in order to hide the internals! But this requires a re-write of MeteoProcessor
		std::vector< METEO_SET >& getBuffer();
		void setBufferStart(const Date& date);
		void setBufferEnd(const Date& date);
	private:
		std::vector< METEO_SET > ts_buffer; ///< stores raw data
		Date ts_start, ts_end; ///< store the beginning and the end date of the ts_buffer
};

/**
 * @class GridBuffer
 * @brief A class to buffer gridded data.
 * This class buffers Grid2D objects. It implements a proper ring buffer, thus removing old buffered grids
 * when necessary.
 *
 * @ingroup data_str
 * @author Mathias Bavay
 * @date   2015-02-06
*/
//TODO: make it a template so it can buffer dems, 2D, 3D grids
class GridBuffer {
	public:
		GridBuffer(const size_t& in_max_grids);

		bool empty() const {return IndexBufferedGrids.empty();}
		void clear() {mapBufferedGrids.clear(); mapBufferedInfos.clear(); IndexBufferedGrids.clear();}
		size_t size() const {return IndexBufferedGrids.size();}

		void setMaxGrids(const size_t& in_max_grids) {max_grids=in_max_grids;}

		bool get(DEMObject& grid, const std::string& grid_hash) const;
		bool get(Grid2DObject& grid, const std::string& grid_hash) const;
		bool get(Grid2DObject& grid, const std::string& grid_hash, std::string& grid_info) const;
		bool get(Grid2DObject& grid, const MeteoGrids::Parameters& parameter, const Date& date) const;
		
		bool has(const std::string& grid_hash) const;
		bool has(const MeteoGrids::Parameters& parameter, const Date& date) const;

		void push(const DEMObject& grid, const std::string& grid_hash);
		void push(const Grid2DObject& grid, const std::string& grid_hash);
		void push(const Grid2DObject& grid, const std::string& grid_hash, const std::string& grid_info);
		void push(const Grid2DObject& grid, const MeteoGrids::Parameters& parameter, const Date& date);

		const std::string toString() const;
	private:
		std::map<std::string, Grid2DObject> mapBufferedGrids;  ///< Buffer interpolated grids
		std::map<std::string, DEMObject> mapBufferedDEMs;  ///< Buffer interpolated grids
		std::map<std::string, std::string> mapBufferedInfos; ///< Buffer interpolations info messages
		std::vector<std::string> IndexBufferedGrids; // this is required in order to know which grid is the oldest one
		std::vector<std::string> IndexBufferedDEMs; // this is required in order to know which grid is the oldest one
		size_t max_grids; ///< How many grids to buffer (grids, dem, landuse and assimilation grids together)
};

}
#endif
