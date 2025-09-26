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
#ifndef ARCIO_H
#define ARCIO_H

#include <meteoio/IOInterface.h>

#include <string>

namespace mio {

/**
 * @class ARCIO
 * @brief This class enables the access to 2D grids stored in ESRI ASCII (ARCGIS) format
 *
 * @ingroup plugins
 * @author Thomas Egger
 * @date   2009-08-28
 */

class ARCIO : public IOInterface {
	public:
		ARCIO(const std::string& configfile);
		ARCIO(const ARCIO&);
		ARCIO(const Config&);

		virtual bool list2DGrids(const Date& start, const Date& end, std::map<Date, std::set<size_t> > &list);
		virtual void read2DGrid(Grid2DObject& dem_out, const std::string& parameter="");
		virtual void read2DGrid(Grid2DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date);

		virtual void readDEM(DEMObject& dem_out);
		virtual void readLanduse(Grid2DObject& landuse_out);
		virtual void readGlacier(Grid2DObject& glaciers_out);

		virtual void readAssimilationData(const Date&, Grid2DObject& da_out);

		virtual void write2DGrid(const Grid2DObject& grid_in, const std::string& options);
		virtual void write2DGrid(const Grid2DObject& grid_in, const MeteoGrids::Parameters& parameter, const Date& date);

	private:
		void getGridPaths();
		void read2DGrid_internal(Grid2DObject& grid_out, const std::string& full_name);
		void write2DGrid_internal(const Grid2DObject& grid_in, const std::string& options) const;
		const Config cfg;

		std::string coordin, coordinparam, coordout, coordoutparam; //projection parameters
		std::string grid2dpath_in, grid2dpath_out;
		std::string grid2d_ext_in, grid2d_ext_out; //file extension

		bool a3d_view_in, a3d_view_out; ///< make filename compatible with the Alpine3D's viewer?
};

} //end namespace mio

#endif
