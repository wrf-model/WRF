/***********************************************************************************/
/*  Copyright 2012 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#include <fstream>
#include <sstream>
#include <cerrno>
#include <cstring>
#include <algorithm>

#include <meteoio/meteoFilters/ProcShade.h>
#include <meteoio/meteoLaws/Sun.h>
#include <meteoio/IOHandler.h>
#include <meteoio/FileUtils.h>
#include <meteoio/dataClasses/DEMAlgorithms.h>

using namespace std;

namespace mio {
//custom function for sorting cache_meteo_files
struct sort_pred {
	bool operator()(const std::pair<double,double> &left, const std::pair<double,double> &right) {
		if (left.first < right.first) return true; else return false;
	}
};

const double ProcShade::diffuse_thresh = 15.; //below this threshold, not correction is performed since it will only be diffuse

ProcShade::ProcShade(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name, const Config &i_cfg)
        : ProcessingBlock(vecArgs, name), cfg(i_cfg), dem(), masks(), write_mask_out(false)
{
	parse_args(vecArgs);
	properties.stage = ProcessingProperties::first; //for the rest: default values
}

void ProcShade::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
                        std::vector<MeteoData>& ovec)
{
	ovec = ivec;
	if (ovec.empty()) return;
	
	const std::string stationHash( ovec[0].meta.getHash() );
	SunObject Sun;
	
	//check if the station already has an associated mask, first as wildcard then by station hash
	std::map< std::string , std::vector< std::pair<double,double> > >::iterator mask = masks.find( "*" );
	if (mask==masks.end()) {
		//now look for our specific station hash
		mask = masks.find( stationHash );
		if (mask==masks.end()) {
			masks[ stationHash ] = computeMask(dem, ovec[0].meta, write_mask_out);
			mask = masks.find( stationHash);
		}
	}
	
	double Md_prev = IOUtils::nodata;
	double julian_prev = 0.;
	for (size_t ii=0; ii<ovec.size(); ii++) { //now correct all timesteps
		double& tmp = ovec[ii](param);
		if (tmp == IOUtils::nodata) continue; //preserve nodata values
		if (tmp<diffuse_thresh) continue; //only diffuse radiation, there is nothing to correct

		const Coords position( ovec[ii].meta.position );
		Sun.setLatLon(position.getLat(), position.getLon(), position.getAltitude()); //if they are constant, nothing will be recomputed
		Sun.setDate(ovec[ii].date.getJulian(true), 0.); //quicker: we stick to gmt
		double sun_azi, sun_elev;
		Sun.position.getHorizontalCoordinates(sun_azi, sun_elev);
		
		const double mask_elev = getMaskElevation(mask->second, sun_azi);
		if (mask_elev>0 && mask_elev>sun_elev) { //the point is in the shade
			const double TA=ovec[ii](MeteoData::TA), RH=ovec[ii](MeteoData::RH), HS=ovec[ii](MeteoData::HS), RSWR=ovec[ii](MeteoData::RSWR);
			double ISWR=ovec[ii](MeteoData::ISWR);

			double albedo = .5;
			if (RSWR==IOUtils::nodata || ISWR==IOUtils::nodata || RSWR<=0 || ISWR<=0) {
				if (HS!=IOUtils::nodata) //no big deal if we can not adapt the albedo
					albedo = (HS>=snow_thresh)? snow_albedo : soil_albedo;

				if (ISWR==IOUtils::nodata && (RSWR!=IOUtils::nodata && HS!=IOUtils::nodata))
					ISWR = RSWR / albedo;
			} else {
				albedo = RSWR / ISWR;
				if (albedo>=1.) albedo=0.99;
				if (albedo<=0.) albedo=0.01;
			}

			const bool has_potRad = (ISWR!=IOUtils::nodata && TA!=IOUtils::nodata && RH!=IOUtils::nodata);
			if (has_potRad) 
				Sun.calculateRadiation(TA, RH, albedo);
			else 
				if (ovec[ii].date.getJulian(true) - julian_prev > 1.) continue; //no way to get ISWR and/or potential radiation, previous Md is too old
			
			const double Md = (has_potRad)? Sun.getSplitting(ISWR) : Md_prev; //fallback: use previous valid value
			tmp *= Md; //only keep the diffuse radiation, either on RSWR or ISWR
			if (has_potRad) {
				Md_prev = Md;
				julian_prev = ovec[ii].date.getJulian(true);
			}
		}
	}
}

//linear interpolation between the available points
double ProcShade::getMaskElevation(const std::vector< std::pair<double,double> > &mask, const double& azimuth) const
{
	const std::vector< std::pair<double, double> >::const_iterator next = std::upper_bound(mask.begin(), mask.end(), make_pair(azimuth, 0.), sort_pred()); //first element that is > azimuth
	
	double x1, y1, x2, y2;
	if (next!=mask.begin() && next!=mask.end()) { //normal case
		const size_t ii = next - mask.begin();
		x1 = mask[ii-1].first;
		y1 = mask[ii-1].second;
		x2 = mask[ii].first;
		y2 = mask[ii].second;
	} else {
		x1 = mask.back().first - 360.;
		y1 = mask.back().second;
		x2 = mask.front().first;
		y2 = mask.front().second;
	}
	
	const double a = (y2 - y1) / (x2 - x1);
	const double b = y2 - a * x2;
	
	return a*azimuth + b;
}

std::vector< std::pair<double,double> > ProcShade::readMask(const std::string& filter, const std::string& filename)
{
	std::vector< std::pair<double,double> > o_mask;
	std::ifstream fin(filename.c_str());
	if (fin.fail()) {
		std::ostringstream ss;
		ss << "Filter " << filter << ": " << "error opening file \"" << filename << "\", possible reason: " << std::strerror(errno);
		throw AccessException(ss.str(), AT);
	}

	const char eoln = FileUtils::getEoln(fin); //get the end of line character for the file

	try {
		size_t lcount=0;
		double azimuth, value;
		std::string line;
		do {
			lcount++;
			getline(fin, line, eoln); //read complete line
			IOUtils::stripComments(line);
			IOUtils::trim(line);
			if (line.empty()) continue;

			std::istringstream iss(line);
			iss.setf(std::ios::fixed);
			iss.precision(std::numeric_limits<double>::digits10);
			iss >> std::skipws >> azimuth;
			if ( !iss || azimuth<0. || azimuth>360.) {
				std::ostringstream ss;
				ss << "Invalid azimuth in file " << filename << " at line " << lcount;
				throw InvalidArgumentException(ss.str(), AT);
			}
			iss >> std::skipws >> value;
			if ( !iss ){
				std::ostringstream ss;
				ss << "Invalid value in file " << filename << " at line " << lcount;
				throw InvalidArgumentException(ss.str(), AT);
			}

			o_mask.push_back( make_pair(azimuth, value) );
		} while (!fin.eof());
		fin.close();
	} catch (const std::exception&){
		if (fin.is_open()) {//close fin if open
			fin.close();
		}
		throw;
	}
	
	if (o_mask.empty()) throw InvalidArgumentException("In filter 'SHADE', no valid mask found in file '"+filename+"'", AT);
	std::sort(o_mask.begin(), o_mask.end(), sort_pred());
	return o_mask;
}

std::vector< std::pair<double,double> > ProcShade::computeMask(const DEMObject& i_dem, const StationData& sd, const bool& dump_mask)
{
	std::vector< std::pair<double,double> > o_mask;
	Coords position( sd.position );
	if (!i_dem.gridify(position)) {
		const string msg = "In filter 'SHADE', station '"+sd.stationID+"' "+position.toString(Coords::LATLON)+" is not included in the DEM "+i_dem.llcorner.toString(Coords::LATLON);
		throw NoDataException(msg, AT);
	}
	
	DEMAlgorithms::getHorizon(i_dem, position, 10., o_mask); //by 10deg increments
	if (o_mask.empty()) throw InvalidArgumentException( "In filter 'SHADE', could not compute mask from DEM '"+i_dem.llcorner.toString(Coords::LATLON)+"'", AT);

	if (dump_mask) {
		std::cout << "Horizon mask for station '" << sd.stationID << "'\n";
		for (size_t ii=0; ii<o_mask.size(); ii++)
			std::cout << o_mask[ii].first << " " << o_mask[ii].second << "\n";
		std::cout << "\n";
	}

	return o_mask;
}

void ProcShade::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	const std::string where( "Filters::"+block_name );
	bool from_dem=true;

	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="FILE") {
			const std::string root_path( cfg.getConfigRootDir() );
			//if this is a relative path, prefix the path with the current path
			const std::string in_filename( vecArgs[ii].second );
			const std::string prefix = ( FileUtils::isAbsolutePath(in_filename) )? "" : root_path+"/";
			const std::string path( FileUtils::getPath(prefix+in_filename, true) );  //clean & resolve path
			const std::string filename( path + "/" + FileUtils::getFilename(in_filename) );
			masks["*"] = readMask(getName(), filename); //this mask is valid for ALL stations
			from_dem = false;
		} else if (vecArgs[ii].first=="DUMP_MASK") {
			IOUtils::parseArg(vecArgs[ii], where, write_mask_out);
		}
	}

	if (from_dem) {
		IOHandler io(cfg);
		dem.setUpdatePpt( DEMObject::NO_UPDATE ); //we only need the elevations
		io.readDEM(dem);
	}
}

} //end namespace
