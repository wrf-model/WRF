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
#ifndef METEOPROCESSOR_H
#define METEOPROCESSOR_H

#include <meteoio/dataClasses/MeteoData.h>
#include <meteoio/Config.h>
#include <meteoio/Meteo1DInterpolator.h>
#include <meteoio/meteoFilters/ProcessingStack.h>

#include <vector>
#include <set>

namespace mio {

/**
 * @class MeteoProcessor
 * @brief A facade class that invokes the processing of the filters and the resampling
 * @author Thomas Egger
 * @date   2010-06-25
 */

class MeteoProcessor {
	public:
		/**
		 * @brief The default constructor - Set up a processing stack for each parameter
		 *        The different stacks are created on the heap and pointers to the objects
		 *        are stored in the map<string,ProcessingStack*> object processing_stack
		 * @param[in] cfg Config object that holds the config of the filters in the [Filters] section
		 * @param[in] rank in case of multiple TimeSeriesManager, rank in the stack? (default: 1)
		 * @param[in] mode spatial resampling operation mode (see IOUtils::OperationMode), default IOUtils::STD
		 */
		MeteoProcessor(const Config& cfg, const char& rank=1, const IOUtils::OperationMode &mode=IOUtils::STD);

		/**
		 * @brief The destructor - It is necessary because the ProcessingStack objects referenced in
		 *        the map<string, ProcessingStack*> processing_stack have to be freed from the heap
		 */
		~MeteoProcessor();

		/**
		 * @brief A function that executes all the filters for all meteo parameters
		 *        configuered by the user
		 * @param[in] ivec The raw sequence of MeteoData objects for all stations
		 * @param[in] ovec The filtered output of MeteoData object for all stations
		 * @param[in] second_pass Whether this is the second pass (check only filters)
		 */
		void process(std::vector< std::vector<MeteoData> >& ivec,
		             std::vector< std::vector<MeteoData> >& ovec, const bool& second_pass=false);

		bool resample(const Date& date, const std::string& stationHash, const std::vector<MeteoData>& ivec, MeteoData& md) {return mi1d.resampleData(date, stationHash, ivec, md);}
		
		void resetResampling() {mi1d.resetResampling();}

		void getWindowSize(ProcessingProperties& o_properties) const;

		const std::string toString() const;

 	private:
		static std::set<std::string> getParameters(const Config& cfg);
		static void compareProperties(const ProcessingProperties& newprop, ProcessingProperties& current);

		Meteo1DInterpolator mi1d;
		std::map<std::string, ProcessingStack*> processing_stack;
};
} //end namespace

#endif
