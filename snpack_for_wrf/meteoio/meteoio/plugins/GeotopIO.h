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
#ifndef GEOTOPIO_H
#define GEOTOPIO_H

#include <meteoio/IOInterface.h>

#include <string>

namespace mio {

/**
 * @class GeotopIO
 * @brief This class enables the access meteo data in legacy Geotop format
 *
 * @ingroup plugins
 * @author Thomas Egger
 * @date   2009-07-02
 */
class GeotopIO : public IOInterface {
	public:
		GeotopIO(const std::string& configfile);
		GeotopIO(const GeotopIO&);
		GeotopIO(const Config&);

		virtual void readStationData(const Date& date, std::vector<StationData>& vecStation);
		virtual void readMeteoData(const Date& dateStart, const Date& dateEnd,
		                           std::vector< std::vector<MeteoData> >& vecMeteo);

		virtual void writeMeteoData(const std::vector< std::vector<MeteoData> >& vecMeteo, const std::string& name="");

	private:
		std::string getValueForKey(const std::string& line);
		void initParamNames(std::map<std::string, size_t>& mapParam);
		void readMetaData(const std::string& metafile);
		void identify_fields(const std::vector<std::string>& tmpvec, const std::string& filename,
		                     std::vector<size_t>& indices, MeteoData& md);
		void convertUnits(MeteoData& meteo);
		void convertUnitsBack(MeteoData& meteo);
		void parseDate(const std::string& datestring, const std::string& fileandline, Date& date);
		void parseMetaData(const std::string& head, const std::string& datastr, std::vector<std::string>& tmpvec);

		const Config cfg;
		double in_tz, out_tz;
		size_t nr_of_stations;
		std::vector< std::map <Date, std::streampos> > vec_streampos; //in order to save file pointers
		std::vector<mio::StationData> vecStation;
		std::map<std::string, size_t> mapColumnNames;
		static const double plugin_nodata; //plugin specific nodata value, e.g. -999
		std::string coordin, coordinparam, coordout, coordoutparam; //projection parameters
		static const size_t sw_direct, sw_diffuse, cloud_factor;
};

} //end namespace

#endif
