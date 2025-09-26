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
#ifndef TEMPLATE_H
#define TEMPLATE_H

#include <meteoio/IOInterface.h>

#include <string>

namespace mio {

/**
 * @class TEMPLATE
 * @brief This (empty) class is to be used as a template for developing new plugins
 *
 * @ingroup plugins
 * @author Mathias Bavay
 * @date   2010-06-14
 */
class TEMPLATE : public IOInterface {
	public:
		TEMPLATE(const std::string& configfile);
		TEMPLATE(const TEMPLATE&);
		TEMPLATE(const Config& cfgreader);
		~TEMPLATE() throw();

		using IOInterface::read2DGrid; //to call before overriding the method when NOT all the polymorphic calls are implemented (see http://bojolais.livejournal.com/222428.html)
		virtual void read2DGrid(Grid2DObject& grid_out, const std::string& parameter="");
		
		virtual void readMeteoData(const Date& dateStart, const Date& dateEnd,
		                           std::vector< std::vector<MeteoData> >& vecMeteo);

	private:
		void cleanup() throw();

		const Config cfg;
		static const double plugin_nodata; //plugin specific nodata value, e.g. -999
		std::string coordin, coordinparam, coordout, coordoutparam; //projection parameters
};

} //namespace
#endif
