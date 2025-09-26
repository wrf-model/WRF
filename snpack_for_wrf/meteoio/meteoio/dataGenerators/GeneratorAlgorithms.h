/***********************************************************************************/
/*  Copyright 2013 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef GENERATORALGORITHMS_H
#define GENERATORALGORITHMS_H

#include <meteoio/dataClasses/MeteoData.h>
#include <meteoio/Config.h>

#include <vector>
#include <set>
#include <string>

#ifdef _MSC_VER
	#pragma warning(disable:4512) //we don't need any = operator!
#endif

namespace mio {

/**
 * @page dev_DataGenerator How to write a data generator
 * One important thing to keep in mind is that data generators will be used at two totally different stages (see the \ref general_structure "MeteoIO workflow"):
 *     + in raw data editing, when calling a data creator;
 *     + when the requested data could not be provided as last resort as data generator.
 *
 * In the first case, the *GeneratorAlgorithm::create()* call will be used and the sampling rate will be the original sampling rate none of the data
 * (such as the other parameters) being filtered or resampled. In the second case, most of the time the *GeneratorAlgorithm::generate()* call
 * will be used and all available data has already been filtered and resampled. In such a case, the goal is to provide reasonable values
 * for the data points that might still be missing. These are either a few isolated periods (a sensor was not functioning) that are too large for performing
 * a statistical temporal interpolation or that a meteorological parameter was not even measured. In such a case,
 * we generate data, generally relying on some parametrization using other meteorological parameters. Sometimes,
 * even fully arbitrary data might be helpful (replacing missing value by a given constant so a model can
 * run over the data gap).
 *
 * @section structure_DataGenerator Structure
 * The selection of which data generator to use at any given time step, for a given parameter is
 * performed by the DataGenerator class. This class acts as an interface, presenting a higher level view to the
 * caller. The data generators themselves derive from the GeneratorAlgorithm class that standardizes their
 * public API. An object factory creates the generator during intialization (keeping all constructed generators
 * in a vector during the whole life time of the DataGenerator object), based on the strings contained in the user's
 * io.ini configuration file.
 *
 * The API also defines two public "generate" methods, taking a meteorological parameter index (see MeteoData) and either
 * a set of meteo data for one station and at one point in time or a meteo time series for one station.
 * These methods walk through the meteo data looking for nodata values for the requested meteo parameter index.
 * If the generator could successfully generate data for <b>all</b> the nodata values it found, it returns <i>true</i>,
 * <i>false</i> otherwise. If <i>false</i> was returned, the DataGenerator object that manages the process would
 * call the next data generator, <b>in the order that was declared by the user</b>. For a given meteo parameter, the
 * whole process stops as soon as a <i>true</i> is returned or there are no more data generators to try
 * (as declared by the user in his configuration file).
 *
 * @section implementation_DataGenerator Implementation
 * It is therefore necessary to create a new class as two new files in the dataGenerators subdirectory (the implementation in the .cc
 * and the declaration in the .h), named after the generator that will be implemented and inheriting GeneratorAlgorithm. You can
 * start by making a copy of the dataGenerators/template.cc (or .h) that you rename according to your generator. Please make sure
 * to update the header guard (the line "#ifndef GENERATORTEMPLATE_H" in the header) to reflect your generator name.
 * Three methods need to be implemented:
 * - the constructor with (const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo)
 * - bool generate(const size_t& param, MeteoData& md)
 * - bool generate(const size_t& param, std::vector<MeteoData>& vecMeteo)
 *
 * The constructor is responsible for parsing the arguments as a vector of strings and saving its own name internally, for
 * error messages, warnings, etc. It should set all internal variables it sees fit according to the parsed arguments. The
 * goal is <i>to not</i> do any parsing anywhere else (for performances reasons).
 *
 * The <i>generate(const size_t& param, MeteoData& md)</i> method compares <i>md(param)</i> with <i>IOUtils::nodata</i> and replaces
 * it by its generated value if necessary. It returns <i>true</i> if no further processing is needed
 * (ie. no replacement was needed or the replacement could be done) or <i>false</i> otherwise.
 *
 * The <i>generate(const size_t& param, std::vector<MeteoData>& vecMeteo)</i> method compares
 * <i>vecMeteo[ii](param)</i> with <i>IOUtils::nodata</i> for each timestamp in the vector and tries to generate data when necessary.
 * If all missing data points could be generated (or if no data point required to be generated), it returns <i>true</i>,
 * and <i>false</i> otherwise.
 *
 * Finally, a new entry must be added in the object factory GeneratorAlgorithmFactory::getAlgorithm method at the top of file
 * GeneratorAlgorithms.cc.
 *
 * @section doc_DataGenerator Documentation
 * The newly added data generator must be added to the list of available algorithms in
 * GeneratorAlgorithms.h with a proper description. Its class must be properly documented, similarly to the other data
 * generators. An example can also be given in the example section of the same file.
 * Please feel free to add necessary bibliographic references to the bibliographic section!
 */

/**
 * @class GeneratorAlgorithm
 * @brief Interface class for the generator models.
 * @details
 * These models generate data for a specific parameter when all other options failed (the resampling could not help).
 * Therefore, there is nothing more that could be done with the temporal history of the data, we have to use
 * a totally different approach: either generic data (constant value, etc) or generate the data from other
 * meteorological parameters (relying on a parametrization, like clear sky for ILWR).
 *
 * @ingroup meteoLaws
*/
class GeneratorAlgorithm {
	public:
		virtual ~GeneratorAlgorithm() {}
		//fill one MeteoData, for one station. This is used by the dataGenerators
		virtual bool generate(const size_t& param, MeteoData& md) = 0;
		//fill one time series of MeteoData for one station. This is used by the dataCreators
		virtual bool create(const size_t& param, std::vector<MeteoData>& vecMeteo) = 0;

		bool skipStation(const std::string& station_id) const;
		std::string getAlgo() const {return algo;}
 	protected:
		GeneratorAlgorithm(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo); ///< protected constructor only to be called by children
		virtual void parse_args(const std::vector< std::pair<std::string, std::string> >& /*vecArgs*/) {}
		static std::set<std::string> initStationSet(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& keyword);

		const std::set<std::string> excluded_stations, kept_stations;
		const std::string algo;
		
		//These are used by several generators in order work with radiation data by looking at HS and deciding which albedo should be used
		static const double soil_albedo, snow_albedo, snow_thresh;
};

class GeneratorAlgorithmFactory {
	public:
		static GeneratorAlgorithm* getAlgorithm(const Config& cfg, const std::string& i_algoname, const std::vector< std::pair<std::string, std::string> >& vecArgs);
};

} //end namespace mio

#endif
