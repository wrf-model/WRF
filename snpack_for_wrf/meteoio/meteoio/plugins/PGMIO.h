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
#ifndef PGMIO_H
#define PGMIO_H

#include <meteoio/IOInterface.h>

#include <string>
#include <sstream>

namespace mio {

/**
 * @class PGMIO
 * @brief This class writes 2D grids as Portable Grey Map file format.
 *
 * @ingroup plugins
 * @author Mathias Bavay
 * @date   2010-06-17
 */
class PGMIO : public IOInterface {
	public:
		PGMIO(const std::string& configfile);
		PGMIO(const PGMIO&);
		PGMIO(const Config& cfgreader);

		virtual bool list2DGrids(const Date& start, const Date& end, std::map<Date, std::set<size_t> > &list);
		virtual void read2DGrid(Grid2DObject& grid_out, const std::string& parameter="");
		virtual void read2DGrid(Grid2DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date);

		virtual void readDEM(DEMObject& dem_out);
		
		virtual void write2DGrid(const Grid2DObject& grid_in, const std::string& filename);
		virtual void write2DGrid(const Grid2DObject& grid_in, const MeteoGrids::Parameters& parameter, const Date& date);

	private:
		void getGridPaths();
		static bool readGridded(std::ifstream& fin, const std::string& full_name, const double& scale_factor, const double& val_min, Grid2DObject& grid_out);
		static void readColumn(std::ifstream& fin, const std::string& full_name, const double& scale_factor, const double& val_min, Grid2DObject& grid_out);
		void read2DGrid_internal(Grid2DObject& grid_out, const std::string& full_name) const;
		static size_t getNextHeader(std::vector<std::string>& vecString, const std::string& filename, std::ifstream& fin);

		const Config cfg;
		static const double plugin_nodata; //plugin specific nodata value, e.g. -999
		std::string coordin, coordinparam, coordout, coordoutparam; //projection parameters
		std::string grid2dpath_in, grid2dpath_out;
};

} //namespace
#endif
