/***********************************************************************************/
/*  Copyright 2009 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef IOMANAGER_H
#define IOMANAGER_H

#include <meteoio/DataGenerator.h>
#include <meteoio/Meteo2DInterpolator.h>
#include <meteoio/dataClasses/MeteoData.h>
#include <meteoio/TimeSeriesManager.h>
#include <meteoio/GridsManager.h>

namespace mio {

class TimeSeriesManager;
class GridsManager;
class Meteo2DInterpolator;

class IOManager {
	public:
		IOManager(const std::string& filename_in);
		IOManager(const Config& i_cfg);

		//Legacy support to support functionality of the IOInterface superclass:
		void read2DGrid(Grid2DObject& grid_out, const std::string& options="") {gdm1.read2DGrid(grid_out, options);}
		void read2DGrid(Grid2DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date) {gdm1.read2DGrid(grid_out, parameter, date);}
		void read3DGrid(Grid3DObject& grid_out, const std::string& options="") {gdm1.read3DGrid(grid_out, options);}
		void read3DGrid(Grid3DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date) {gdm1.read3DGrid(grid_out, parameter, date);}
		void readDEM(DEMObject& dem_out) {gdm1.readDEM(dem_out);}
		void readAssimilationData(const Date& date_in, Grid2DObject& da_out) {gdm1.readAssimilationData(date_in, da_out);}
		void readLanduse(Grid2DObject& landuse_out) {gdm1.readLanduse(landuse_out);}
		void readGlacier(Grid2DObject& glacier_out) {gdm1.readGlacier(glacier_out);}
		void readPOI(std::vector<Coords>& pts) {iohandler.readPOI(pts);}
		void write2DGrid(const Grid2DObject& grid_in, const std::string& options="") {gdm1.write2DGrid(grid_in, options);}
		void write2DGrid(const Grid2DObject& grid_in, const MeteoGrids::Parameters& parameter, const Date& date) {gdm1.write2DGrid(grid_in, parameter, date);}
		void write3DGrid(const Grid3DObject& grid_in, const std::string& options="") {gdm1.write3DGrid(grid_in, options);}
		void write3DGrid(const Grid3DObject& grid_in, const MeteoGrids::Parameters& parameter, const Date& date) {gdm1.write3DGrid(grid_in, parameter, date);}
		bool list2DGrids(const Date& start, const Date& end, std::map<Date, std::set<size_t> > &list){return iohandler.list2DGrids(start,end,list);}
		//end legacy support

		size_t getStationData(const Date& date, STATIONS_SET& vecStation);

		/**
		* @brief Fill vecMeteo with a time series of objects
		* corresponding to the interval indicated by dateStart and dateEnd.
		* Depending on the ProcessingLevel for the instance of the IOManager
		* the data returned will be either raw (read directly from the IOHandler)
		* or processed (read from a buffer and filtered through the MeteoProcessor)
		*
		* vecMeteo will be empty if no datasets were retrieved in the interval defined
		* by dateStart and dateEnd
		*
		* Example Usage:
		* @code
		* vector< vector<MeteoData> > vecMeteo;      //empty vector
		* Date d1(2008,06,21,11,0, 1.);       //21.6.2008 11:00 UTC+1
		* Date d2(2008,07,21,11,0, 1.);       //21.7.2008 11:00 UTC+1
		* IOManager iom("io.ini");
		* unsigned int nstations = iom.getMeteoData(d1, d2, vecMeteo);
		* @endcode
		* @param dateStart   A Date object representing the beginning of an interval (inclusive)
		* @param dateEnd     A Date object representing the end of an interval (inclusive)
		* @param vecVecMeteo A vector of vector<MeteoData> objects to be filled with data
		* @return            Number of stations for which data has been found in the interval
		*/
		size_t getMeteoData(const Date& dateStart, const Date& dateEnd, std::vector< METEO_SET >& vecVecMeteo);

		/**
		 * @brief Fill vector<MeteoData> object with multiple instances of MeteoData
		 * corresponding to the instant indicated by a Date object. Each MeteoData
		 * instance within the vector represents the data for one station at the given
		 * instant. Depending on the ProcessingLevel configured data will be either
		 * raw (read directly from the IOHandler)
		 * or processed (read from a buffer, filtered and resampled).
		 *
		 * NOTE:
		 * - vecMeteo will be empty if there is no data found for any station
		 *
		 * Example Usage:
		 * @code
		 * vector<MeteoData> vecMeteo;      //empty vector
		 * IOManager iomanager("io.ini");
		 * iomanager.getMeteoData(Date(2008,06,21,11,0, 1.), vecMeteo); //21.6.2008 11:00 UTC+1
		 * @endcode
		 * @param i_date      A Date object representing the date/time for the sought MeteoData objects
		 * @param vecMeteo    A vector of MeteoData objects to be filled with data
		 * @return            Number of stations for which data has been found in the interval
		 */
		size_t getMeteoData(const Date& i_date, METEO_SET& vecMeteo);

		/**
		 * @brief Push a vector of time series of MeteoData objects into the IOManager. This overwrites
		 *        any internal buffers that are used and subsequent calls to getMeteoData or interpolate
		 *        will be performed upon this data. This method is a way to bypass the internal reading
		 *        of MeteoData from a certain source and is useful in case the user is only interested
		 *        in data processing and interpolation performed by the IOManager object.
		 * @param level Level of processing that has already been performed on the data (raw XOR filtered)
		 * @param date_start Representing the beginning of the data
		 * @param date_end Representing the end of the data
		 * @param vecMeteo The actual data being pushed into the IOManager object
		 */
		void push_meteo_data(const IOUtils::ProcessingLevel& level, const Date& date_start, const Date& date_end,
		                     const std::vector< METEO_SET >& vecMeteo) {tsm1.push_meteo_data(level, date_start, date_end, vecMeteo);}

		/**
		 * @brief Fill Grid2DObject with spatial data.
		 * Depending on which meteo plugin is in use, this might be spatially interpolated
		 * point measurements or grids as provided by the data source itself.
		 * Depending on the ProcessingLevel configured data will be either
		 * raw (read directly from the IOHandler)
		 * or processed (read from a buffer, filtered and resampled).
		 *
		 * NOTE:
		 * - grid will be empty if there is no data found
		 *
		 * Example Usage:
		 * @code
		 * Grid2DObject grid;      //empty grid
		 * IOManager iomanager("io.ini");
		 * iomanager.getMeteoData(Date(2008,06,21,11,0, 1.), MeteoData::TA, grid); //21.6.2008 11:00 UTC+1
		 * @endcode
		 * @param date A Date object representing the date/time for the sought MeteoData objects
		 * @param dem Digital Elevation Model data
		 * @param meteoparam which meteo parameter to return
		 * @param result grid returned filled with the requested data
		 * @return true if the grid got filled
		 */

		bool getMeteoData(const Date& date, const DEMObject& dem, const MeteoData::Parameters& meteoparam,
		                 Grid2DObject& result);

		bool getMeteoData(const Date& date, const DEMObject& dem, const std::string& param_name,
		                 Grid2DObject& result);

		bool getMeteoData(const Date& date, const DEMObject& dem, const MeteoData::Parameters& meteoparam,
		                 Grid2DObject& result, std::string& info_string);

		bool getMeteoData(const Date& date, const DEMObject& dem, const std::string& param_name,
		                 Grid2DObject& result, std::string& info_string);

		void interpolate(const Date& date, const DEMObject& dem, const MeteoData::Parameters& meteoparam,
				 const std::vector<Coords>& in_coords, std::vector<double>& result);

		void interpolate(const Date& date, const DEMObject& dem, const MeteoData::Parameters& meteoparam,
				 const std::vector<Coords>& in_coords, std::vector<double>& result, std::string& info_string);

		void interpolate(const Date& date, const DEMObject& dem, const MeteoData::Parameters& meteoparam,
				 const std::vector<StationData>& in_stations, std::vector<double>& result, std::string& info_string);

		/**
		 * @brief Set the desired ProcessingLevel of the IOManager instance
		 *        The processing level affects the way meteo data is read and processed
		 *        Three values are possible:
		 *        - IOUtils::raw data shall be read directly from the buffer
		 *        - IOUtils::filtered data shall be filtered before returned to the user
		 *        - IOUtils::resampled data shall be resampled before returned to the user
		 *          this only affects the function getMeteoData(const Date&, METEO_DATASET&);
		 *
		 *        The three values can be combined: e.g. IOUtils::filtered | IOUtils:resampled
		 * @param i_level The ProcessingLevel values that shall be used to process data
		 */
		void setProcessingLevel(const unsigned int& i_level);

		/**
		 * @brief Set buffer window properties requirements as known to the application itself.
		 * This will compare these requirements with the ones expressed by the end user and keep the max between them.
		 * The method can be called several times, it will NOT reset the calculated buffer's requirements but keep
		 * on merging with new submissions. Any parameter given as IOUtils::nodata will be ignored.
		 * @param buffer_size buffer size in days
		 * @param buff_before buffer centering in days
		 */
		void setMinBufferRequirements(const double& buffer_size, const double& buff_before) {tsm1.setBufferProperties(buffer_size, buff_before);}

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

		void writeMeteoData(const std::vector< METEO_SET >& vecMeteo, const std::string& option="") {tsm1.writeMeteoData(vecMeteo, option);}

		/**
		 * @brief Returns a copy of the internal Config object.
		 * This is convenient to clone an iomanager
		 * @return new Config object as a copy of the internal Config
		 */
		const Config getConfig() const {return cfg;}

		const std::string toString() const;

		/**
		 * @brief Add a METEO_SET for a specific instance to the point cache. This is a way to manipulate
		 * MeteoData variables and be sure that the manipulated values are later used for requests
		 * regarding that specific date (e.g. 2D interpolations)
		 *
		 * @param i_date Representing a point in time
		 * @param vecMeteo A vector of MeteoData objects to be copied into the point cache
		 */
		void add_to_points_cache(const Date& i_date, const METEO_SET& vecMeteo);

		/**
		 * @brief Clear the all cache. All raw, filtered and resampled values are dismissed, will need to be re-read and/or recalculated.
		 */
		void clear_cache();

	private:
		/**
		 * @brief Returns the mode to be used for the IOManager for TimeSeries
		 * @param i_cfg configuration object
		 * @return mode as of IOUtils::OperationMode
		 */
		static IOUtils::OperationMode getIOManagerTSMode(const Config& i_cfg);

		/**
		 * @brief Returns the mode to be used for the IOManager for Grids
		 * @param i_cfg configuration object
		 * @return mode as of IOUtils::OperationMode
		 */
		static IOUtils::OperationMode getIOManagerGridMode(const Config& i_cfg);

		void initVirtualStations();
		std::vector<METEO_SET> getVirtualStationsData(const DEMObject& dem, const Date& dateStart, const Date& dateEnd);
		void initIOManager();

		const Config cfg; ///< we keep this Config object as full copy, so the original one can get out of scope/be destroyed
		const IOUtils::OperationMode ts_mode;
		IOHandler iohandler;
		TimeSeriesManager tsm1, tsm2;
		GridsManager gdm1;
		Meteo2DInterpolator interpolator;
		DEMObject source_dem;
		std::vector<size_t> v_params, grids_params; ///< Parameters for virtual stations
		std::vector<StationData> v_stations, v_gridstations; ///< metadata for virtual stations
		unsigned int vstations_refresh_rate, vstations_refresh_offset; ///< when using virtual stations, how often should the data be spatially re-interpolated? (in seconds)
};
} //end namespace
#endif
