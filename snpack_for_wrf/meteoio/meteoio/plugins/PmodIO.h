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
#ifndef PMODIO_H
#define PMODIO_H

#include <meteoio/IOInterface.h>
#include <meteoio/dataClasses/Coords.h>

#include <string>

namespace mio {

/**
 * @class PmodIO
 * @brief This class reads radiation data in raw file format from PMOD/WRC Institute.
 *
 * @ingroup plugins
 * @author Mathias Bavay
 * @date   2019-01-23
 */
class PmodIO : public IOInterface {
	public:
		PmodIO(const std::string& configfile);
		PmodIO(const PmodIO&);
		PmodIO(const Config& cfgreader);
		
		virtual void readStationData(const Date& date, std::vector<StationData>& vecStation);
		virtual void readMeteoData(const Date& dateStart, const Date& dateEnd,
		                           std::vector< std::vector<MeteoData> >& vecvecMeteo);

	private:
		void parseInputOutputSection();
		MeteoData createTemplate(const std::vector<std::string>& fields) const;
		MeteoData createTemplate(std::ifstream &fin, const char& eoln) const;

		Coords location;
		std::string name, id, filename, inpath;
		const Config cfg;
		double in_TZ;
		static const double plugin_nodata; //plugin specific nodata value, e.g. -999
};

} //namespace
#endif
