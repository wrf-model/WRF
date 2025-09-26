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
#ifndef IOHANDLER_H
#define IOHANDLER_H

#include <meteoio/IOInterface.h>
#include <meteoio/DataCreator.h>
#include <meteoio/meteoFilters/TimeFilters.h>

#include <map>
#include <set>
#include <string>

namespace mio {

/**
* @file IOHandler.h
* @class IOHandler
* @brief This class is the class to use for raw I/O operations. It is responsible for transparently loading the plugins
* and it follows the interface defined by the IOInterface class with the addition of a few convenience methods.
*/
class IOHandler : public IOInterface {
	public:
		IOHandler(const IOHandler&);
		IOHandler(const Config&);

		virtual ~IOHandler() throw();

		IOHandler& operator=(const IOHandler&); ///<Assignement operator

		//methods defined in the IOInterface class
		virtual bool list2DGrids(const Date& start, const Date& end, std::map<Date, std::set<size_t> > &list);
		virtual void read2DGrid(Grid2DObject& out_grid, const std::string& parameter="");
		virtual void read2DGrid(Grid2DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date);
		virtual void read3DGrid(Grid3DObject& grid_out, const std::string& i_filename="");
		virtual void read3DGrid(Grid3DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date);

		virtual void readDEM(DEMObject& dem_out);
		virtual void readLanduse(Grid2DObject& landuse_out);
		virtual void readGlacier(Grid2DObject& glacier_out);

		virtual void readStationData(const Date& date,
		                             STATIONS_SET& vecStation);

		virtual void writeMeteoData(const std::vector<METEO_SET>& vecMeteo,
		                            const std::string& name="");
		virtual void readMeteoData(const Date& dateStart, const Date& dateEnd,
		                           std::vector<METEO_SET>& vecMeteo);

		virtual void readAssimilationData(const Date&, Grid2DObject& da_out);
		virtual void readPOI(std::vector<Coords>& pts);
		virtual void write2DGrid(const Grid2DObject& grid_in, const std::string& name);
		virtual void write2DGrid(const Grid2DObject& grid_in, const MeteoGrids::Parameters& parameter, const Date& date);
		virtual void write3DGrid(const Grid3DObject& grid_out, const std::string& options);
		virtual void write3DGrid(const Grid3DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date);

		const std::string toString() const;

	private:
		IOInterface* getPlugin(std::string plugin_name, const Config& i_cfg) const;
		IOInterface* getPlugin(const std::string& cfgkey, const std::string& cfgsection, const std::string& sec_rename="");
		std::vector<std::string> getListOfSources(const std::string& plugin_key, const std::string& sec_pattern) const;
		void create_copy_map();
		void create_move_map();
		void create_exclude_map();
		void create_keep_map();
		void create_merge_map();
		static void purgeNodata(std::vector<METEO_SET>& vecMeteo);

		void copy_params(std::vector< METEO_SET >& vecMeteo) const;
		void move_params(std::vector< METEO_SET >& vecMeteo) const;
		void exclude_params(std::vector<METEO_SET>& vecVecMeteo) const;
		void keep_params(std::vector<METEO_SET>& vecVecMeteo) const;
		void merge_stations(std::vector<METEO_SET>& vecVecMeteo) const;
		void merge_stations(STATIONS_SET& vecStation) const;
		void automerge_stations(std::vector<METEO_SET>& vecVecMeteo) const;
		void automerge_stations(STATIONS_SET& vecStation) const;

		const Config& cfg;
		DataCreator dataCreator;
		TimeProcStack timeproc;
		std::map<std::string, IOInterface*> mapPlugins;
		std::map< std::string, std::set<std::string> > excluded_params; //station_id, set of params
		std::map< std::string, std::set<std::string> > kept_params; //station_id, set of params
		std::map< std::string, std::vector<std::string> > merge_commands;
		std::map< std::string, std::string > copy_commands;
		std::map< std::string, std::set<std::string> > move_commands;
		std::vector<std::string> merged_stations;
		int merge_strategy;
		bool copy_ready, move_ready, excludes_ready, keeps_ready, merge_ready, automerge;
};

} //namespace

#endif
