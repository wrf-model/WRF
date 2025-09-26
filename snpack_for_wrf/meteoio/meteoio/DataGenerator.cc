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

#include <meteoio/DataGenerator.h>

using namespace std;

namespace mio {
DataGenerator::DataGenerator(const Config& cfg)
              : DataCreator(cfg), data_qa_logs(false)
{
	cfg.getValue("DATA_QA_LOGS", "GENERAL", data_qa_logs, IOUtils::nothrow);
	
	static const std::string section( "Generators" );
	static const std::string key_pattern( "::generators" );
	const std::set<std::string> set_of_used_parameters( getParameters(cfg, key_pattern, section) );

	std::set<std::string>::const_iterator it;
	for (it = set_of_used_parameters.begin(); it != set_of_used_parameters.end(); ++it) {
		const std::string parname( *it );
		const std::vector<std::string> tmpAlgorithms( getAlgorithmsForParameter(cfg, key_pattern, section, parname) );
		const size_t nrOfAlgorithms = tmpAlgorithms.size();

		std::vector<GeneratorAlgorithm*> vecGenerators( nrOfAlgorithms );
		for (size_t jj=0; jj<nrOfAlgorithms; jj++) {
			const std::vector< std::pair<std::string, std::string> > vecArgs( getArgumentsForAlgorithm(cfg, parname, tmpAlgorithms[jj], section) );
			vecGenerators[jj] = GeneratorAlgorithmFactory::getAlgorithm( cfg, tmpAlgorithms[jj], vecArgs);
		}

		if (nrOfAlgorithms>0) {
			mapAlgorithms[parname] = vecGenerators;
		}
	}
}

DataGenerator& DataGenerator::operator=(const DataGenerator& source)
{
	if (this != &source) {
		mapAlgorithms = source.mapAlgorithms;
	}
	return *this;
}

/**
 * @brief generate data to fill missing data points.
 * This relies on data generators defined by the user for each meteo parameters.
 * This loops over the defined generators and stops as soon as all missing points
 * have been successfully replaced.
 * @param vecMeteo vector containing one point for each station
 */
void DataGenerator::fillMissing(METEO_SET& vecMeteo) const
{
	if (mapAlgorithms.empty()) return; //no generators defined by the end user

	std::map< std::string, std::vector<GeneratorAlgorithm*> >::const_iterator it;
	for (it=mapAlgorithms.begin(); it!=mapAlgorithms.end(); ++it) {
		const std::vector<GeneratorAlgorithm*> vecGenerators( it->second );

		for (size_t station=0; station<vecMeteo.size(); ++station) { //process this parameter on all stations
			const size_t param = vecMeteo[station].getParameterIndex(it->first);
			if (param==IOUtils::npos) continue;

			const std::string statID( vecMeteo[station].meta.getStationID() );
			//these are only required by data_qa_logs
			const double old_val = vecMeteo[station](param);
			const std::string statName( vecMeteo[station].meta.getStationName() );
			const std::string stat = (!statID.empty())? statID : statName;

			bool status = false;
			size_t jj=0;
			while (jj<vecGenerators.size() && status != true) { //loop over the generators
				if (!vecGenerators[jj]->skipStation( statID )) {
					status = vecGenerators[jj]->generate(param, vecMeteo[station]);
					if (vecMeteo[station](param) != old_val) {
						vecMeteo[station].setGenerated(param);
						if (data_qa_logs) {
							const std::string parname( it->first );
							const std::string algo_name( vecGenerators[jj]->getAlgo() );
							const Date date( vecMeteo[station].date );
							cout << "[DATA_QA] Generating " << stat << "::" << parname << "::" << algo_name << " " << date.toString(Date::ISO_TZ) << " [" << date.toString(Date::ISO_WEEK) << "]\n";
						}
					} //endif new=old
				}
				jj++;
			}
		}
	}
}

/**
 * @brief generate data to fill missing data points.
 * This relies on data generators defined by the user for each meteo parameters.
 * This loops over the defined generators and stops as soon as all missing points
 * have been successfully replaced.
 * @param vecVecMeteo vector containing a timeserie for each station
 */
void DataGenerator::fillMissing(std::vector<METEO_SET>& vecVecMeteo) const
{
	if (mapAlgorithms.empty()) return; //no generators defined by the end user

	std::map< std::string, std::vector<GeneratorAlgorithm*> >::const_iterator it;
	for (it=mapAlgorithms.begin(); it!=mapAlgorithms.end(); ++it) {
		const std::vector<GeneratorAlgorithm*> vecGenerators( it->second );

		for (size_t station=0; station<vecVecMeteo.size(); ++station) { //process this parameter on all stations
			if (vecVecMeteo[station].empty()) continue; //the station does not have any data
			
			const size_t param = vecVecMeteo[station][0].getParameterIndex(it->first);
			if (param==IOUtils::npos) continue;

			const std::string statID( vecVecMeteo[station][0].meta.getStationID() );
			//these are only required by data_qa_logs
			const METEO_SET old_val( vecVecMeteo[station] );
			const std::string statName( old_val[0].meta.getStationName() );
			const std::string stat = (!statID.empty())? statID : statName;

			bool status = false;
			size_t jj=0;
			while (jj<vecGenerators.size() && status != true) { //loop over the generators
				if (!vecGenerators[jj]->skipStation( statID )) {
					status = vecGenerators[jj]->create(param, vecVecMeteo[station]);
					for (size_t kk=0; kk<old_val.size(); kk++) {
						if (old_val[kk](param) != vecVecMeteo[station][kk](param)) {
							vecVecMeteo[station][kk].setGenerated(param);
							if (data_qa_logs) {
								const std::string parname( it->first );
								const std::string algo_name( vecGenerators[jj]->getAlgo() );
								cout << "[DATA_QA] Generating " << stat << "::" << parname << "::" << algo_name << " " << old_val[kk].date.toString(Date::ISO_TZ) << "\n";
							}
						}
					}
				}
				jj++;
			}
		}
	}
}

const std::string DataGenerator::toString() const {
	std::ostringstream os;
	os << "<DataGenerator>\n";
	os << "Generators defined: " << std::boolalpha << !mapAlgorithms.empty() << std::noboolalpha << "\n";
	if (!mapAlgorithms.empty()) {
		os << "User list of generators:\n";
		for (std::map< std::string, std::vector<GeneratorAlgorithm*> >::const_iterator iter = mapAlgorithms.begin(); iter != mapAlgorithms.end(); ++iter) {
			os << setw(10) << iter->first << " :: ";
			for (size_t jj=0; jj<iter->second.size(); jj++) {
				os << iter->second[jj]->getAlgo() << " ";
			}
			os << "\n";
		}
	}

	os << "</DataGenerator>\n";
	return os.str();
}

} //namespace
