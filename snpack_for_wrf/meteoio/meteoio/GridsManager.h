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
#ifndef GRIDSMANAGER_H
#define GRIDSMANAGER_H

#include <meteoio/dataClasses/Buffer.h>
#include <meteoio/dataClasses/MeteoData.h>
#include <meteoio/IOHandler.h>
#include <meteoio/Config.h>

#include <set>
#include <map>

namespace mio {

class GridsManager {
	public:
		GridsManager(IOHandler& in_iohandler, const Config& in_cfg);

		//Legacy support to support functionality of the IOInterface superclass:
		void read2DGrid(Grid2DObject& grid_out, const std::string& option="");
		void read2DGrid(Grid2DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date);

		//HACK buffer 3D grids!
		void read3DGrid(Grid3DObject& grid_out, const std::string& i_filename="") {iohandler.read3DGrid(grid_out, i_filename);}
		void read3DGrid(Grid3DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date) {iohandler.read3DGrid(grid_out, parameter, date);}

		void readDEM(DEMObject& dem_out);
		void readAssimilationData(const Date& date_in, Grid2DObject& da_out);
		void readLanduse(Grid2DObject& landuse_out);
		void readGlacier(Grid2DObject& landuse_out);

		void write2DGrid(const Grid2DObject& grid_in, const std::string& options="") {iohandler.write2DGrid(grid_in, options);}
		void write2DGrid(const Grid2DObject& grid_in, const MeteoGrids::Parameters& parameter, const Date& date) {iohandler.write2DGrid(grid_in, parameter, date);}
		void write3DGrid(const Grid3DObject& grid_out, const std::string& options="") {iohandler.write3DGrid(grid_out, options);}
		void write3DGrid(const Grid3DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date) {iohandler.write3DGrid(grid_out, parameter, date);}
		//end legacy support

		void setProcessingLevel(const unsigned int& i_level);
		void clear_cache() {buffer.clear();}

		/**
		 * @brief Returns a copy of the internal Config object.
		 * This is convenient to clone an iomanager
		 * @return new Config object as a copy of the internal Config
		 */
		const Config getConfig() const {return cfg;}

		/**
		 * @brief Returns a copy of the internal IOHandler object.
		 * This is convenient to clone an iomanager
		 * @return new IOHandler object as a copy of the internal IOHandler
		 */
		IOHandler& getIOHandler() const {return iohandler;}

		std::vector<StationData> initVirtualStationsAtAllGridPoints(const DEMObject& dem) const;
		std::vector<StationData> initVirtualStations(const DEMObject& dem, const bool& adjust_coordinates, const bool& fourNeighbors) const;
		METEO_SET getVirtualStationsFromGrid(const DEMObject& dem, const std::vector<size_t>& v_params, const std::vector<StationData>& v_stations, const Date& date);
		std::vector<METEO_SET> getVirtualStationsFromGrid(const DEMObject& dem, const std::vector<size_t>& v_params, const std::vector<StationData>& v_stations, const Date& dateStart, const Date& dateEnd);

		const std::string toString() const;

	private:
		bool isAvailable(const std::set<size_t>& available_params, const MeteoGrids::Parameters& parameter, const Date& date) const;
		bool setGrids2d_list(const Date& date);
		bool setGrids2d_list(const Date& dateStart, const Date& dateEnd);
		Grid2DObject getRawGrid(const MeteoGrids::Parameters& parameter, const Date& date);
		Grid2DObject getGrid(const MeteoGrids::Parameters& parameter, const Date& date, const bool& enforce_cartesian=true);
		bool generateGrid(Grid2DObject& grid2D, const std::set<size_t>& available_params, const MeteoGrids::Parameters& parameter, const Date& date);

		IOHandler& iohandler;
		const Config& cfg;
		GridBuffer buffer;
		std::map<Date, std::set<size_t> > grids2d_list; ///< list of available 2d grids
		Date grids2d_start, grids2d_end; ///< validity range of the grids2d_list

		double grid2d_list_buffer_size; ///< how many days to read the list of grids2d for?
		unsigned int processing_level;
		bool dem_altimeter; ///< use the pressure to compute the elevation?
};
} //end namespace
#endif
