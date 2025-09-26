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
#ifndef ARPSIO_H
#define ARPSIO_H

#include <meteoio/IOInterface.h>

#include <string>
#include <sstream>
#include <iostream>
#include <cstring>

namespace mio {

/**
 * @class ARPSIO
 * @brief This class enables the access to 2D grids stored in ARPS format
 *
 * @ingroup plugins
 * @author Mathias Bavay
 * @date   2009-12-04
 */
class ARPSIO : public IOInterface {
	public:
		ARPSIO(const std::string& configfile);
		ARPSIO(const ARPSIO&);
		ARPSIO(const Config& cfgreader);

		ARPSIO& operator=(const ARPSIO&); ///<Assignement operator, required because of pointer member

		virtual bool list2DGrids(const Date& /*start*/, const Date& /*end*/, std::map<Date, std::set<size_t> >& /*list*/) {return false;}
		virtual void read2DGrid(Grid2DObject& grid_out, const std::string& parameter="");
		virtual void read2DGrid(Grid2DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date);

		virtual void readDEM(DEMObject& dem_out);

		using IOInterface::read3DGrid; //to call before overriding the method
		virtual void read3DGrid(Grid3DObject& grid_out, const std::string& parameter="");

	private:
		void setOptions();
		void listFields(const std::string& filename);
		void read2DGrid_internal(FILE* &fin, const std::string& filename, Grid2DObject& grid_out, const MeteoGrids::Parameters& parameter);
		void initializeGRIDARPS(FILE* &fin, const std::string& filename);
		void initializeTrueARPS(FILE* &fin, const std::string& filename, const std::string& curr_line);
		void openGridFile(FILE* &fin, const std::string& filename);
		void readGridLayer(FILE* &fin, const std::string& filename, const std::string& parameter, const unsigned int& layer, Grid2DObject& grid);
		static void moveToMarker(FILE* &fin, const std::string& filename, const std::string& marker);
		void skipToLayer(FILE* &fin, const std::string& filename, const unsigned int& layers) const;

		const Config cfg;
		static const double plugin_nodata; //plugin specific nodata value, e.g. -999
		static const char* default_ext;
		std::string coordin, coordinparam, coordout, coordoutparam; //projection parameters
		std::string grid2dpath_in, grid3dpath_in; //where are input grids stored
		std::string ext; //file extension
		unsigned int dimx, dimy, dimz;
		double cellsize;
		double xcoord, ycoord;
		std::vector<double> zcoord;
		bool is_true_arps; //is it an original arps file or is it a truncated file?
};

} //namespace
#endif
