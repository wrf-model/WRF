/***********************************************************************************/
/*  Copyright 2009 EPFL                                                            */
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
#ifndef GRASSIO_H
#define GRASSIO_H

#include <meteoio/IOInterface.h>

#include <string>

namespace mio {

/**
 * @class GrassIO
 * @brief This class enables the access to 2D grids stored in GRASS ASCII (e.g. JGrass) format
 *
 * @ingroup plugins
 * @author Thomas Egger
 * @date   2008-08-03
 */
class GrassIO : public IOInterface {
	public:
		GrassIO(const std::string& configfile);
		GrassIO(const GrassIO&);
		GrassIO(const Config&);

		virtual bool list2DGrids(const Date& /*start*/, const Date& /*end*/, std::map<Date, std::set<size_t> >& /*list*/) {return false;}
		using IOInterface::read2DGrid; //to call before overriding the method
		virtual void read2DGrid(Grid2DObject& dem_out, const std::string& parameter="");

		virtual void readDEM(DEMObject& dem_out);
		virtual void readLanduse(Grid2DObject& landuse_out);
		virtual void readGlacier(Grid2DObject& glacier_out);

		virtual void readAssimilationData(const Date&, Grid2DObject& da_out);

		using IOInterface::write2DGrid; //to call before overriding the method
		virtual void write2DGrid(const Grid2DObject& grid_in, const std::string& options);

	private:
		const Config cfg;
		static const double plugin_nodata;
		std::string coordin, coordinparam, coordout, coordoutparam; //projection parameters
};

} //end namespace mio

#endif
