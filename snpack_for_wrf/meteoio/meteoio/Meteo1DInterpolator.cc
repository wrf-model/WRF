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
#include <meteoio/Meteo1DInterpolator.h>
#include <meteoio/dataClasses/StationData.h>

#include <iostream>
#include <utility>

using namespace std;

namespace mio {

Meteo1DInterpolator::Meteo1DInterpolator(const Config& in_cfg, const char& rank, const IOUtils::OperationMode &mode)
                     : mapAlgorithms(), cfg(in_cfg), window_size(86400.), enable_resampling(true), data_qa_logs(false)
{
	cfg.getValue("DATA_QA_LOGS", "GENERAL", data_qa_logs, IOUtils::nothrow);
	
	//default window_size is 2 julian days
	cfg.getValue("WINDOW_SIZE", "Interpolations1D", window_size, IOUtils::nothrow);
	if (window_size <= 1.)
		throw IOException("WINDOW_SIZE not valid, it should be a duration in seconds at least greater than 1", AT);
	window_size /= 86400.; //user uses seconds, internally julian day is used
	
	cfg.getValue("ENABLE_RESAMPLING", "Interpolations1D", enable_resampling, IOUtils::nothrow);

	//create the resampling algorithms for each MeteoData::Parameters parameter
	for (size_t ii=0; ii<MeteoData::nrOfParameters; ii++){ //loop over all MeteoData member variables
		const std::string parname( MeteoData::getParameterName(ii) ); //Current parameter name
		const std::string algo_name( IOUtils::strToUpper(getAlgorithmsForParameter(parname)) );

		if (algo_name=="ACCUMULATE" && mode==IOUtils::VSTATIONS && rank==1) {
			//when doing VSTATIONS, the first pass must re-accumulate over the refresh rate while the second will do over the original user choice
			const std::string vstations_refresh_rate = cfg.get("VSTATIONS_REFRESH_RATE", "Input");
			const std::vector< std::pair<std::string, std::string> > vecArgs( 1, std::make_pair("PERIOD", vstations_refresh_rate));
			mapAlgorithms[parname] = ResamplingAlgorithmsFactory::getAlgorithm(algo_name, parname, window_size, vecArgs);
		} else {
			const std::vector< std::pair<std::string, std::string> > vecArgs( getArgumentsForAlgorithm(parname, algo_name) );
			mapAlgorithms[parname] = ResamplingAlgorithmsFactory::getAlgorithm(algo_name, parname, window_size, vecArgs);
		}
	}
}

Meteo1DInterpolator::~Meteo1DInterpolator()
{
	std::map< std::string, ResamplingAlgorithms* >::iterator it;
	for (it=mapAlgorithms.begin(); it!=mapAlgorithms.end(); ++it)
		delete it->second;
}

void Meteo1DInterpolator::getWindowSize(ProcessingProperties& o_properties) const
{
	o_properties.points_before = 1;
	o_properties.points_after  = 1;
	o_properties.time_before   = Duration(window_size, 0.); //we will need to cut a window 2x larger so we can interpolate each point in the window
	o_properties.time_after    = Duration(window_size, 0.);
}

bool Meteo1DInterpolator::resampleData(const Date& date, const std::string& stationHash, const std::vector<MeteoData>& vecM, MeteoData& md)
{
	if (vecM.empty()) //Deal with case of the empty vector
		return false; //nothing left to do

	//Find element in the vector or the next index
	size_t index = IOUtils::seek(date, vecM, false);

	//Three cases
	bool isResampled = true;
	ResamplingAlgorithms::ResamplingPosition elementpos = ResamplingAlgorithms::exact_match;
	if (index == IOUtils::npos) { //nothing found append new element at the left or right
		if (date < vecM.front().date) {
			elementpos = ResamplingAlgorithms::begin;
			index = 0;
		} else if (date >= vecM.back().date) {
			elementpos = ResamplingAlgorithms::end;
			index = vecM.size() - 1;
		}
	} else if ((index != IOUtils::npos) && (vecM[index].date != date)) { //element found nearby
		elementpos = ResamplingAlgorithms::before;
	} else { //element found at the right time
		isResampled = false;
	}
	md = vecM[index]; //create a clone of the found element

	if (!enable_resampling) {
		if (isResampled==false) return true; //the element was found at the right time
		else { //not found or wrong time: return a nodata element
			md.reset();
			md.setDate(date);
			return true;
		}
	}
	
	md.reset();   //set all values to IOUtils::nodata
	md.setDate(date);
	md.setResampled( isResampled );

	//now, perform the resampling
	for (size_t ii=0; ii<md.getNrOfParameters(); ii++) {
		const std::string parname( md.getNameForParameter(ii) ); //Current parameter name
		const std::map< std::string, ResamplingAlgorithms* >::const_iterator it = mapAlgorithms.find(parname);
		if (it!=mapAlgorithms.end()) { //the parameter has been found
			it->second->resample(stationHash, index, elementpos, ii, vecM, md);
		} else { //we are dealing with an extra parameter, we need to add it to the map first, so it will exist next time...
			const std::string algo_name( getAlgorithmsForParameter(parname) );
			const std::vector< std::pair<std::string, std::string> > vecArgs( getArgumentsForAlgorithm(parname, algo_name) );
			mapAlgorithms[parname] = ResamplingAlgorithmsFactory::getAlgorithm(algo_name, parname, window_size, vecArgs);;
			mapAlgorithms[parname]->resample(stationHash, index, elementpos, ii, vecM, md);
		}

		if ((index != IOUtils::npos) && vecM[index](ii)!=md(ii)) {
			md.setResampledParam(ii);
			if (data_qa_logs) {
				const std::map< std::string, ResamplingAlgorithms* >::const_iterator it2 = mapAlgorithms.find(parname); //we have to re-find it in order to handle extra parameters
				const std::string statName( md.meta.getStationName() );
				const std::string statID( md.meta.getStationID() );
				const std::string stat = (!statID.empty())? statID : statName;
				const std::string algo_name( it2->second->getAlgo() );
				cout << "[DATA_QA] Resampling " << stat << "::" << parname << "::" << algo_name << " " << md.date.toString(Date::ISO_TZ) << " [" << md.date.toString(Date::ISO_WEEK) << "]\n";
			}
		}
	} //endfor ii

	return true; //successfull resampling
}

void Meteo1DInterpolator::resetResampling()
{
	std::map< std::string, ResamplingAlgorithms* >::iterator it;
	for (it=mapAlgorithms.begin(); it!=mapAlgorithms.end(); ++it) {
		it->second->resetResampling();
	}
}

std::string Meteo1DInterpolator::getAlgorithmsForParameter(const std::string& parname) const
{
	std::string algo("linear"); //default value
	cfg.getValue(parname+"::resample", "Interpolations1D", algo, IOUtils::nothrow);
	return algo;
}

/**
 * @brief retrieve the resampling algorithm to be used for the 1D interpolation of meteo parameters.
 * The potential arguments are also extracted.
 * @param parname meteo parameter to deal with
 * @param algorithm algorithm name
 * @return vector of named arguments
 */
std::vector< std::pair<std::string, std::string> > Meteo1DInterpolator::getArgumentsForAlgorithm(const std::string& parname, const std::string& algorithm) const
{
	const std::string key_prefix( parname+"::"+algorithm+"::" );
	std::vector< std::pair<std::string, std::string> > vecArgs( cfg.getValues(key_prefix, "Interpolations1D") );

	//clean the arguments up (ie remove the {Param}::{algo}:: in front of the argument key itself)
	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		const size_t beg_arg_name = vecArgs[ii].first.find_first_not_of(":", key_prefix.length());
		if (beg_arg_name==std::string::npos)
			throw InvalidFormatException("Wrong argument format for '"+vecArgs[ii].first+"'", AT);
		vecArgs[ii].first = vecArgs[ii].first.substr(beg_arg_name);
	}

	return vecArgs;
}

Meteo1DInterpolator& Meteo1DInterpolator::operator=(const Meteo1DInterpolator& source) {
	if (this != &source) {
		window_size = source.window_size;
		mapAlgorithms= source.mapAlgorithms;
	}
	return *this;
}

const std::string Meteo1DInterpolator::toString() const
{
	ostringstream os;
	os << "<Meteo1DInterpolator>\n";
	os << "Config& cfg = " << hex << &cfg << dec <<"\n";
	if (enable_resampling) {
		os << "Resampling algorithms:\n";
		map< string, ResamplingAlgorithms* >::const_iterator it;
		for (it=mapAlgorithms.begin(); it!=mapAlgorithms.end(); ++it) {
			//os << setw(10) << it->first << "::" << it->second->getAlgo() << "\n";
			os << it->second->toString() << "\n";
		}
	} else {
		os << "Resampling disabled\n";
	}
	os << "</Meteo1DInterpolator>\n";

	return os.str();
}

} //namespace
