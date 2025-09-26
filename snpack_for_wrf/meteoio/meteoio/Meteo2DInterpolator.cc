/***********************************************************************************/
/*  Copyright 2014 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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

#include <meteoio/Meteo2DInterpolator.h>
#include <meteoio/Timer.h>

using namespace std;

namespace mio {

Meteo2DInterpolator::Meteo2DInterpolator(const Config& i_cfg, TimeSeriesManager& i_tsmanager, GridsManager& i_gridsmanager)
                    : cfg(i_cfg), tsmanager(&i_tsmanager), gridsmanager(&i_gridsmanager),
                      grid_buffer(0), mapAlgorithms(),
                      algorithms_ready(false), use_full_dem(false)
{
	size_t max_grids = 10; //default number of grids to keep in buffer
	cfg.getValue("BUFF_GRIDS", "Interpolations2D", max_grids, IOUtils::nothrow);
	grid_buffer.setMaxGrids(max_grids);
	
	setAlgorithms();
}

Meteo2DInterpolator::Meteo2DInterpolator(const Meteo2DInterpolator& source)
           : cfg(source.cfg), tsmanager(source.tsmanager), gridsmanager(source.gridsmanager),
                      grid_buffer(source.grid_buffer), mapAlgorithms(source.mapAlgorithms),
                      algorithms_ready(source.algorithms_ready), use_full_dem(source.use_full_dem)
{}

Meteo2DInterpolator& Meteo2DInterpolator::operator=(const Meteo2DInterpolator& source) {
	if (this != &source) {
		//cfg = source.cfg;
		tsmanager = source.tsmanager;
		gridsmanager = source.gridsmanager;
		grid_buffer = source.grid_buffer;
		mapAlgorithms = source.mapAlgorithms;
		algorithms_ready = source.algorithms_ready;
		use_full_dem = source.use_full_dem;
	}
	return *this;
}

Meteo2DInterpolator::~Meteo2DInterpolator()
{
	std::map<std::string, std::vector<InterpolationAlgorithm*> >::iterator iter;
	for (iter = mapAlgorithms.begin(); iter != mapAlgorithms.end(); ++iter) {
		const vector<InterpolationAlgorithm*>& vecAlgs( iter->second );
		for (size_t ii=0; ii<vecAlgs.size(); ++ii)
			delete vecAlgs[ii];
	}
}

/* By reading the Config object build up a list of user configured algorithms
* for each MeteoData::Parameters parameter (i.e. each member variable of MeteoData like ta, p, psum, ...)
* Concept of this constructor: loop over all MeteoData::Parameters and then look
* for configuration of interpolation algorithms within the Config object.
*/
void Meteo2DInterpolator::setAlgorithms()
{
	const std::set<std::string> set_of_used_parameters( getParameters(cfg) );

	std::set<std::string>::const_iterator it  = set_of_used_parameters.begin();
	for (; it != set_of_used_parameters.end(); ++it) {
		const std::string parname( *it );
		const std::vector<std::string> tmpAlgorithms( getAlgorithmsForParameter(cfg, parname) );
		const size_t nrOfAlgorithms = tmpAlgorithms.size();

		std::vector<InterpolationAlgorithm*> vecAlgorithms( nrOfAlgorithms );
		for (size_t jj=0; jj<nrOfAlgorithms; jj++) {
			const std::vector< std::pair<std::string, std::string> > vecArgs( getArgumentsForAlgorithm(parname, tmpAlgorithms[jj], "Interpolations2D") );
			vecAlgorithms[jj] = AlgorithmFactory::getAlgorithm( tmpAlgorithms[jj], *this, vecArgs, *tsmanager, *gridsmanager, parname);
		}

		if (nrOfAlgorithms>0) {
			mapAlgorithms[parname] = vecAlgorithms;
		}
	}
	algorithms_ready = true;
}

//get a list of all meteoparameters referenced in the Interpolations2D section
std::set<std::string> Meteo2DInterpolator::getParameters(const Config& i_cfg)
{
	const std::vector<std::string> vec_keys( i_cfg.getKeys("::algorithms", "Interpolations2D", true) );

	std::set<std::string> set_parameters;
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


std::vector<std::string> Meteo2DInterpolator::getAlgorithmsForParameter(const Config& i_cfg, const std::string& parname)
{
	// This function retrieves the user defined interpolation algorithms for
	// parameter 'parname' by querying the Config object
	std::vector<std::string> vecAlgorithms;
	const std::vector<std::string> vecKeys( i_cfg.getKeys(parname+"::algorithms", "Interpolations2D") );

	if (vecKeys.size() > 1)
		throw IOException("Multiple definitions of " + parname + "::algorithms in config file", AT);;

	if (vecKeys.empty()) return vecAlgorithms;

	i_cfg.getValue(vecKeys[0], "Interpolations2D", vecAlgorithms, IOUtils::nothrow);
	return vecAlgorithms;
}

std::vector< std::pair<std::string, std::string> > Meteo2DInterpolator::getArgumentsForAlgorithm(const std::string& parname,
                                                     const std::string& algorithm, const std::string& section) const
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

std::string Meteo2DInterpolator::interpolate(const Date& date, const DEMObject& dem, const MeteoData::Parameters& meteoparam,
                                      Grid2DObject& result, const bool& quiet)
{
	if (!algorithms_ready) setAlgorithms();
	const std::string param_name( MeteoData::getParameterName(meteoparam) );
	
	return interpolate(date, dem, param_name, result, quiet);
}

std::string Meteo2DInterpolator::interpolate(const Date& date, const DEMObject& dem, const std::string& param_name,
                                      Grid2DObject& result, const bool& quiet)
{
	std::string InfoString;
	if (!algorithms_ready) setAlgorithms();

	//Get grid from buffer if it exists
	std::ostringstream grid_hash;
	grid_hash << dem.llcorner.toString(Coords::LATLON) << " " << dem.getNx() << "x" << dem.getNy() << " @" << dem.cellsize << " " << date.toString(Date::ISO) << " " << param_name;
	if (grid_buffer.get(result, grid_hash.str(), InfoString)) return InfoString;

	//Show algorithms to be used for this parameter
	const std::map<string, vector<InterpolationAlgorithm*> >::iterator it = mapAlgorithms.find(param_name);
	if (it==mapAlgorithms.end())
		throw IOException("No interpolation algorithms configured for parameter "+param_name, AT);

	//look for algorithm with the highest quality rating
	const std::vector<InterpolationAlgorithm*>& vecAlgs( it->second );
	double maxQualityRating = -1.;
	size_t bestalgorithm = 0;
	for (size_t ii=0; ii < vecAlgs.size(); ++ii){
		const double rating = vecAlgs[ii]->getQualityRating(date);
		if ((rating != 0.0) && (rating > maxQualityRating)) { //we use ">" so that in case of equality, the first choice will be kept
			bestalgorithm = ii;
			maxQualityRating = rating;
		}
	}

	//finally execute the algorithm with the best quality rating or throw an exception
	if (maxQualityRating<=0.0) {
		const std::string msg( "No suitable interpolation algorithm for parameter "+param_name+" on "+date.toString(Date::ISO_TZ) );
		if (quiet) {
			std::cerr << "[E] " << msg << "\n";
			result.set(dem, IOUtils::nodata);
			grid_buffer.push(result, grid_hash.str(), msg); //HACK is it the proper way of doing this? Could we have a valid grid later on?
			return msg;
		} else throw IOException(msg, AT);
	}
	vecAlgs[bestalgorithm]->calculate(dem, result);
	InfoString = vecAlgs[bestalgorithm]->getInfo();

	//Run soft min/max filter for RH, PSUM and HS
	if (param_name == "RH"){
		Meteo2DInterpolator::checkMinMax(0.0, 1.0, result);
	} else if (param_name == "PSUM"){
		Meteo2DInterpolator::checkMinMax(0.0, 10000.0, result);
	} else if (param_name == "HS"){
		Meteo2DInterpolator::checkMinMax(0.0, 10000.0, result);
	} else if (param_name == "VW"){
		Meteo2DInterpolator::checkMinMax(0.0, 10000.0, result);
	}

	//save grid in buffer
	grid_buffer.push(result, grid_hash.str(), InfoString);
	return InfoString;
}

//NOTE make sure that skip_virtual_stations = true before calling this method when using virtual stations!
std::string Meteo2DInterpolator::interpolate(const Date& date, const DEMObject& dem, const MeteoData::Parameters& meteoparam,
                            std::vector<Coords> vec_coords, std::vector<double>& result, const bool& quiet)
{
	result.clear();
	result.resize( vec_coords.size() );

	if (use_full_dem) {
		Grid2DObject result_grid;
		const std::string InfoString( interpolate(date, dem, meteoparam, result_grid, quiet) );
		const bool gridify_success = dem.gridify(vec_coords);
		if (!gridify_success)
			throw InvalidArgumentException("Coordinate given to interpolate is outside of dem", AT);

		for (size_t ii=0; ii<vec_coords.size(); ii++) {
			//we know the i,j are positive because of gridify_success
			const size_t pt_i = static_cast<size_t>( vec_coords[ii].getGridI() );
			const size_t pt_j = static_cast<size_t>( vec_coords[ii].getGridJ() );
			result[ii] = result_grid(pt_i,pt_j);
		}
		return InfoString;
	} else {
		std::string InfoString;
		for (size_t ii=0; ii<vec_coords.size(); ii++) {
			const bool gridify_success = dem.gridify(vec_coords[ii]);
			if (!gridify_success)
				throw InvalidArgumentException("Coordinate given to interpolate is outside of dem", AT);

			//we know the i,j are positive because of gridify_success
			const size_t pt_i = static_cast<size_t>( vec_coords[ii].getGridI() );
			const size_t pt_j = static_cast<size_t>( vec_coords[ii].getGridJ() );

			//Make new DEM with just one point, namely the one specified by vec_coord[ii]
			//Copy all other properties of the big DEM into the new one
			DEMObject one_point_dem(dem, pt_i, pt_j, 1, 1, false);

			one_point_dem.min_altitude = dem.min_altitude;
			one_point_dem.max_altitude = dem.max_altitude;
			one_point_dem.min_slope = dem.min_slope;
			one_point_dem.max_slope = dem.max_slope;
			one_point_dem.min_curvature = dem.min_curvature;
			one_point_dem.max_curvature = dem.max_curvature;

			Grid2DObject result_grid;
			InfoString = interpolate(date, one_point_dem, meteoparam, result_grid, quiet);
			result[ii] = result_grid(0,0);
		}
		return InfoString;
	}
}

//NOTE make sure that skip_virtual_stations = true before calling this method when using virtual stations!
std::string Meteo2DInterpolator::interpolate(const Date& date, const DEMObject& dem, const MeteoData::Parameters& meteoparam,
                            std::vector<StationData> vec_stations, std::vector<double>& result, const bool& quiet)
{
	result.clear();
	result.reserve( vec_stations.size() );

	if (use_full_dem) {
		Grid2DObject result_grid;
		const std::string InfoString( interpolate(date, dem, meteoparam, result_grid, quiet) );
		const bool gridify_success = dem.gridify(vec_stations);
		if (!gridify_success)
			throw InvalidArgumentException("Coordinate given to interpolate is outside of dem", AT);

		for (size_t ii=0; ii<vec_stations.size(); ii++) {
			//we know the i,j are positive because of gridify_success
			const size_t pt_i = static_cast<size_t>( vec_stations[ii].position.getGridI() );
			const size_t pt_j = static_cast<size_t>( vec_stations[ii].position.getGridJ() );
			result.push_back( result_grid(pt_i,pt_j) );
		}
		return InfoString;
	} else {
		std::string InfoString;
		for (size_t ii=0; ii<vec_stations.size(); ii++) {
			//Make new DEM with just one point, namely the one specified by vec_coord[ii]
			const DEMObject one_point_dem(1, 1, vec_stations[ii].position, vec_stations[ii].position.getAltitude());

			Grid2DObject result_grid;
			InfoString = interpolate(date, one_point_dem, meteoparam, result_grid, quiet);
			result.push_back( result_grid(0,0) );
		}
		return InfoString;
	}
}

void Meteo2DInterpolator::checkMinMax(const double& minval, const double& maxval, Grid2DObject& gridobj)
{
	for (size_t ii=0; ii<gridobj.size(); ii++){
		double& value = gridobj(ii);
		if (value == IOUtils::nodata) continue;

		if (value < minval) {
			value = minval;
		} else if (value > maxval) {
			value = maxval;
		}
	}
}

void Meteo2DInterpolator::check_projections(const DEMObject& dem, const std::vector<MeteoData>& vec_meteo)
{
	//check that the stations are using the same projection as the dem
	for (size_t ii=0; ii<vec_meteo.size(); ii++) {
		const StationData& meta( vec_meteo[ii].meta );
		if (!meta.position.isSameProj(dem.llcorner)) {
			std::string type, args;
			meta.position.getProj(type, args);
			const std::string station_str( "Station "+meta.stationID+" is using projection ("+type+" "+args+") " );
			dem.llcorner.getProj(type, args);
			const std::string dem_str( "while DEM is using projection ("+type+" "+args+") " );
			throw IOException(station_str+dem_str, AT);
		}
	}
}

const std::string Meteo2DInterpolator::toString() const
{
	ostringstream os;
	os << "<Meteo2DInterpolator>\n";
	os << "Config& cfg = " << hex << &cfg << dec << "\n";
	os << "TimeSeriesManager& tsmanager = "  << hex << &tsmanager << dec << "\n";
	os << "GridsManager& gridsmanager = "  << hex << &gridsmanager << dec << "\n";

	os << "Spatial resampling algorithms:\n";
	std::map<std::string, std::vector<InterpolationAlgorithm*> >::const_iterator iter;
	for (iter = mapAlgorithms.begin(); iter != mapAlgorithms.end(); ++iter) {
		os << setw(10) << iter->first << "::";
		for (size_t jj=0; jj<iter->second.size(); jj++) {
			os << iter->second[jj]->algo << " ";
		}
		os << "\n";
	}

	//cache content
	os << grid_buffer.toString();
	os << "</Meteo2DInterpolator>\n";
	return os.str();
}

} //namespace
