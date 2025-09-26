/***********************************************************************************/
/*  Copyright 2019 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef ARGOS_H
#define ARGOS_H

#include <meteoio/IOInterface.h>

namespace mio {

class ArgosStation {
	public:
		ArgosStation();
		ArgosStation(const std::string& argosID, const Config& metaCfg, const float& in_nodata, const double& in_TZ, const std::string& coordin, const std::string& coordinparam, const bool& isDebug);
		Date parseDate(const std::string& str, const size_t &linenr) const;
		MeteoData parseDataLine(const Date& dt, const std::vector<unsigned int>& raw_data, const size_t& linenr) const;
		StationData getStationData() const {return md_template.meta;}
		bool isValid() const {return validStation;}
		bool isValidMessage(const size_t& msg_length) const {return (msg_length==fields_idx.size());}

		size_t meteoIdx; ///< index within vecMeteo
	private:
		void parseFieldsSpecs(const std::vector<std::string>& fieldsNames, MeteoData &meteo_template, std::vector<size_t> &idx);
		static std::vector<float> decodeData(const std::vector<unsigned int> &raw);

		std::vector<size_t> fields_idx; ///< vector of MeteoData field index
		std::vector<double> units_offset, units_multiplier;
		std::vector<size_t> fields_idx2; ///< vector of MeteoData field index
		std::vector<double> units_offset2, units_multiplier2;
		MeteoData md_template;
		double TZ;
		float nodata;
		bool validStation, debug, wsl_hack;
};


/**
 * @class ArgosIO
 * @brief This plugin deals with data that has been transmitted through the
 * <A HREF="http://www.argos-system.org/wp-content/uploads/2016/08/r286_9_argos3_metop_en.pdf">ARGOS</A> satellites. 
 * In order to reduce data transfers, no headers are provided with the data and therefore all the metadata 
 * will have to be provided to the plugin.
 *
 * @ingroup plugins
 * @author Mathias Bavay
 * @date   2019-03-21
 */
class ArgosIO : public IOInterface {
	public:
		ArgosIO(const std::string& configfile);
		ArgosIO(const ArgosIO&);
		ArgosIO(const Config& cfgreader);
		
		virtual void readStationData(const Date& date, std::vector<StationData>& vecStation);

		virtual void readMeteoData(const Date& dateStart, const Date& dateEnd,
		                           std::vector< std::vector<MeteoData> >& vecMeteo);

	private:
		void parseInputOutputSection(const Config& cfgreader);
		static std::string readLine(std::ifstream &fin, size_t &linenr);
		bool readTimestamp(std::ifstream &fin, size_t &linenr, const unsigned int& nFields, const ArgosStation& station, const Date& dateStart, const Date& dateEnd, MeteoData &md) const;
		void readMessage(std::ifstream &fin, size_t &linenr, const Date& dateStart, const Date& dateEnd, std::vector< std::vector<MeteoData> >& vecMeteo);
		void readRaw(const std::string& file_and_path, const Date& dateStart, const Date& dateEnd, std::vector< std::vector<MeteoData> >& vecMeteo);
		void addStation(const std::string& argosID);

		std::vector<std::string> vecFilenames;
		std::map<std::string, ArgosStation> stations; ///< the stations' properties for each argosID
		Config metaCfg;
		std::string meteopath;
		std::string coordin, coordinparam; //projection parameters
		double in_TZ;
		float in_nodata;
		bool debug;
};

} //namespace
#endif
