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

#include <meteoio/DataCreator.h>

using namespace std;

namespace mio {

DataCreator::DataCreator(const Config& cfg)
              : mapAlgorithms()
{
	static const std::string section( "Input" );
	static const std::string key_pattern( "::create" );
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

DataCreator::~DataCreator()
{ //we have to deallocate the memory allocated by "new GeneratorAlgorithm()"
	std::map< std::string, std::vector<GeneratorAlgorithm*> >::iterator it;

	for (it=mapAlgorithms.begin(); it!=mapAlgorithms.end(); ++it) {
		std::vector<GeneratorAlgorithm*> &vec( it->second );
		for (size_t ii=0; ii<vec.size(); ii++)
			delete vec[ii];
	}
}

DataCreator& DataCreator::operator=(const DataCreator& source)
{
	if (this != &source) {
		mapAlgorithms = source.mapAlgorithms;
	}
	return *this;
}

/**
 * @brief create new parameters from parametrizations
 * This relies on data creators defined by the user for each meteo parameters.
 * This loops over the defined generators and stops as soon as all points
 * have been successfully created.
 * @param vecVecMeteo vector containing a timeserie for each station
 */
void DataCreator::createParameters(std::vector<METEO_SET>& vecVecMeteo) const
{
	if (mapAlgorithms.empty()) return; //no creators defined by the end user

	std::map< std::string, std::vector<GeneratorAlgorithm*> >::const_iterator it;
	for (it=mapAlgorithms.begin(); it!=mapAlgorithms.end(); ++it) {
		const std::vector<GeneratorAlgorithm*> vecGenerators( it->second );

		for (size_t station=0; station<vecVecMeteo.size(); ++station) { //process this parameter on all stations
			if (vecVecMeteo[station].empty()) continue; //the station does not have any data

			//create the new parameter
			for (size_t ii=0; ii<vecVecMeteo[station].size(); ++ii) {
				vecVecMeteo[station][ii].addParameter( it->first );
			}
			const size_t param = vecVecMeteo[station][0].getParameterIndex(it->first);
			const std::string statID( vecVecMeteo[station][0].meta.getStationID() );

			//fill the new parameter
			bool status = false;
			size_t jj=0;
			while (jj<vecGenerators.size() && status != true) { //loop over the generators
				if (!vecGenerators[jj]->skipStation( statID ))
					status = vecGenerators[jj]->create(param, vecVecMeteo[station]);
				jj++;
			}
		}
	}
}

std::set<std::string> DataCreator::getParameters(const Config& cfg, const std::string& key_pattern, const std::string& section)
{
	std::set<std::string> set_parameters;
	const std::vector<std::string> vec_keys( cfg.getKeys(key_pattern, section, true) ); //search anywhere in key

	for (size_t ii=0; ii<vec_keys.size(); ii++) {
		const size_t found = vec_keys[ii].find_first_of(":");
		if (found != std::string::npos){
			if (vec_keys[ii].length()<=(found+2))
				throw InvalidFormatException("Invalid syntax: \""+vec_keys[ii]+"\"", AT);
			if (vec_keys[ii][found+1]!=':')
				throw InvalidFormatException("Missing ':' in \""+vec_keys[ii]+"\"", AT);
			std::string tmp( vec_keys[ii].substr(0,found) );
			IOUtils::toUpper( tmp );
			set_parameters.insert( tmp );
		}
	}

	return set_parameters;
}

// This function retrieves the user defined generator algorithms for
// parameter 'parname' by querying the Config object
std::vector<std::string> DataCreator::getAlgorithmsForParameter(const Config& cfg, const std::string& key_pattern, const std::string& section, const std::string& parname)
{
	std::vector<std::string> vecAlgorithms;
	const std::vector<std::string> vecKeys( cfg.getKeys(parname+key_pattern, section) );

	if (vecKeys.size() > 1) throw IOException("Multiple definitions of " + parname + key_pattern + " in config file", AT);;
	if (vecKeys.empty()) return vecAlgorithms;

	cfg.getValue(vecKeys[0], section, vecAlgorithms, IOUtils::nothrow);
	return vecAlgorithms;
}

std::vector< std::pair<std::string, std::string> > DataCreator::getArgumentsForAlgorithm(const Config& cfg,
                                               const std::string& parname, const std::string& algorithm, const std::string& section)
{
	const std::string key_prefix( parname+"::"+algorithm+"::" );
	std::vector< std::pair<std::string, std::string> > vecArgs( cfg.getValues(key_prefix, section) );

	//clean the arguments up (ie remove the {Param}::{algo}:: in front of the argument key itself)
	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		const size_t beg_arg_name = vecArgs[ii].first.find_first_not_of(":", key_prefix.length());
		if (beg_arg_name==std::string::npos)
			throw InvalidFormatException("Wrong argument format for '"+vecArgs[ii].first+"'", AT);
		vecArgs[ii].first = vecArgs[ii].first.substr(beg_arg_name);
	}

	return vecArgs;
}

const std::string DataCreator::toString() const {
	std::ostringstream os;
	os << "<DataCreator>\n";
	os << "Creators defined: " << std::boolalpha << !mapAlgorithms.empty() << std::noboolalpha << "\n";
	if (!mapAlgorithms.empty()) {
		os << "User list of creators:\n";
		for (std::map< std::string, std::vector<GeneratorAlgorithm*> >::const_iterator iter = mapAlgorithms.begin(); iter != mapAlgorithms.end(); ++iter) {
			os << setw(10) << iter->first << " :: ";
			for (size_t jj=0; jj<iter->second.size(); jj++) {
				os << iter->second[jj]->getAlgo() << " ";
			}
			os << "\n";
		}
	}

	os << "</DataCreator>\n";
	return os.str();
}

} //namespace
