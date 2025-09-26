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
#include <meteoio/plugins/ARCIO.h>
#include <meteoio/IOUtils.h>
#include <meteoio/FileUtils.h>
#include <meteoio/IOExceptions.h>

#include <cerrno>
#include <cstring>
#include <string.h>
#include <algorithm>
#include <limits>
#include <sstream>
#include <fstream>
#include <iostream>

using namespace std;

namespace mio {
/**
 * @page arc ARC
 * @section arc_format Format
 * This is for reading grid data in the ARC-GIS format, or more properly, ESRI ascii grid format (see http://en.wikipedia.org/wiki/ESRI_grid). We consider the following specification (in the absence of an official specification):
 * - a single space character is used as field spearator
 * - the header data is right aligned to the 23rd column
 * - float header data has 3 digits precision
 * - all grid data is written as float (which might cause some trouble for some softwares)
 *
 * These specifications should reflect commonly accepted practise. Finally, the naming scheme for meteo grids should be:
 * YYYY-MM-DDTHH.mm_{MeteoGrids::Parameters}.asc
 *
 * @section arc_units Units
 * The distances are assumed to be in meters.
 *
 * @section arc_keywords Keywords
 * This plugin uses the following keywords:
 * - COORDSYS: input coordinate system (see Coords) specified in the [Input] section
 * - COORDPARAM: extra input coordinates parameters (see Coords) specified in the [Input] section
 * - COORDSYS: output coordinate system (see Coords) specified in the [Output] section
 * - COORDPARAM: extra output coordinates parameters (see Coords) specified in the [Output] section
 * - GRID2DPATH: meteo grids directory where to read/write the grids; [Input] and [Output] sections
 * - GRID2DEXT: grid file extension, or <i>none</i> for no file extension (default: .asc)
 * - A3D_VIEW: use Alpine3D's grid viewer naming scheme (default=false)? [Input] and [Output] sections.
 * - DEMFILE: for reading the data as a DEMObject
 * - LANDUSE: for interpreting the data as landuse codes
 * - DAPATH: path+prefix of file containing data assimilation grids (named with ISO 8601 basic date and .sca extension,
 * example ./input/dagrids/sdp_200812011530.sca)
 *
 * @code
 * [Input]
 * GRID2D     = ARC
 * GRID2DPATH = ./input/surface-grids
 *
 * #reading ARC dem
 * DEM     = ARC
 * DEMFILE = ./input/surface-grids/Switzerland_1000m.asc
 * @endcode
 */

ARCIO::ARCIO(const std::string& configfile)
       : cfg(configfile),
         coordin(), coordinparam(), coordout(), coordoutparam(),
         grid2dpath_in(), grid2dpath_out(), grid2d_ext_in(".asc"), grid2d_ext_out(".asc"),
         a3d_view_in(false), a3d_view_out(false)
{
	IOUtils::getProjectionParameters(cfg, coordin, coordinparam, coordout, coordoutparam);
	cfg.getValue("A3D_VIEW", "Input", a3d_view_in, IOUtils::nothrow);
	cfg.getValue("A3D_VIEW", "Output", a3d_view_out, IOUtils::nothrow);
	getGridPaths();
}

ARCIO::ARCIO(const Config& cfgreader)
       : cfg(cfgreader),
         coordin(), coordinparam(), coordout(), coordoutparam(),
         grid2dpath_in(), grid2dpath_out(), grid2d_ext_in(".asc"), grid2d_ext_out(".asc"),
         a3d_view_in(false), a3d_view_out(false)
{
	IOUtils::getProjectionParameters(cfg, coordin, coordinparam, coordout, coordoutparam);
	cfg.getValue("A3D_VIEW", "Input", a3d_view_in, IOUtils::nothrow);
	cfg.getValue("A3D_VIEW", "Output", a3d_view_out, IOUtils::nothrow);
	getGridPaths();
}

void ARCIO::getGridPaths()
{
	grid2dpath_in.clear();
	grid2dpath_out.clear();
	const std::string grid_in = cfg.get("GRID2D", "Input", "");
	if (grid_in == "ARC") //keep it synchronized with IOHandler.cc for plugin mapping!!
		cfg.getValue("GRID2DPATH", "Input", grid2dpath_in);
	const std::string grid_out = cfg.get("GRID2D", "Output", "");
	if (grid_out == "ARC") //keep it synchronized with IOHandler.cc for plugin mapping!!
		cfg.getValue("GRID2DPATH", "Output", grid2dpath_out);

	cfg.getValue("GRID2DEXT", "Input", grid2d_ext_in, IOUtils::nothrow);
	if (grid2d_ext_in=="none") grid2d_ext_in.clear();
	cfg.getValue("GRID2DEXT", "Output", grid2d_ext_out, IOUtils::nothrow);
	if (grid2d_ext_out=="none") grid2d_ext_out.clear();
}

void ARCIO::read2DGrid_internal(Grid2DObject& grid_out, const std::string& full_name)
{
	if (!FileUtils::validFileAndPath(full_name)) throw InvalidNameException(full_name, AT);
	if (!FileUtils::fileExists(full_name)) throw NotFoundException(full_name, AT);

	errno = 0;
	std::ifstream fin(full_name.c_str(), ifstream::in);
	if (fin.fail()) {
		std::ostringstream ss;
		ss << "Error opening file \"" << full_name << "\", possible reason: " << std::strerror(errno);
		throw AccessException(ss.str(), AT);
	}

	const char eoln = FileUtils::getEoln(fin); //get the end of line character for the file

	//Go through file, save key value pairs
	int i_ncols, i_nrows;
	double xllcorner, yllcorner, cellsize, plugin_nodata;
	try {
		const std::map<std::string, std::string> header( FileUtils::readKeyValueHeader(fin, 6, " ") ); //Read in 6 lines as header into a key/value map
		IOUtils::getValueForKey(header, "ncols", i_ncols);
		IOUtils::getValueForKey(header, "nrows", i_nrows);
		IOUtils::getValueForKey(header, "xllcorner", xllcorner);
		IOUtils::getValueForKey(header, "yllcorner", yllcorner);
		IOUtils::getValueForKey(header, "cellsize", cellsize);
		if (cellsize<0.01) throw IOException("Very small cellsize (" + IOUtils::toString(cellsize) + " in file " + full_name +". Is the grid lat/lon instead of x/y?", AT);
		if (header.count("nodata_value")==0) throw IOException("Missing nodata_value in the header of file " + full_name, AT);
		IOUtils::getValueForKey(header, "nodata_value", plugin_nodata);

		i_ncols = IOUtils::standardizeNodata(i_ncols, plugin_nodata);
		i_nrows = IOUtils::standardizeNodata(i_nrows, plugin_nodata);
		xllcorner = IOUtils::standardizeNodata(xllcorner, plugin_nodata);
		yllcorner = IOUtils::standardizeNodata(yllcorner, plugin_nodata);
		cellsize = IOUtils::standardizeNodata(cellsize, plugin_nodata);

		if ((i_ncols==0) || (i_nrows==0)) {
			throw IOException("Number of rows or columns in 2D Grid given is zero, in file: " + full_name, AT);
		}
		if ((i_ncols<0) || (i_nrows<0)) {
			throw IOException("Number of rows or columns in 2D Grid read as \"nodata\", in file: " + full_name, AT);
		}
		const size_t ncols = static_cast<size_t>(i_ncols);
		const size_t nrows = static_cast<size_t>(i_nrows);

		//compute/check WGS coordinates (considered as the true reference) according to the projection as defined in cfg
		Coords location(coordin, coordinparam);
		location.setXY(xllcorner, yllcorner, IOUtils::nodata);

		//Initialize the 2D grid
		grid_out.set(ncols, nrows, cellsize, location);

		size_t nr_empty=0;
		//Read one line after the other and parse values into Grid2DObject
		std::string line;
		double tmp;
		for (size_t kk=nrows-1; (kk < nrows); kk--) {
			getline(fin, line, eoln);
			if (line.empty()) { //so we can tolerate empty lines
				kk++; //to keep the same kk at the next iteration
				nr_empty++;
				if (nr_empty>1000) throw InvalidFormatException("Too many empty lines, most probably the file format is wrong (check the end-of-lines character!)", AT);
				continue;
			}
			std::istringstream iss(line);
			iss.setf(std::ios::fixed);
			iss.precision(std::numeric_limits<double>::digits10);

			for (size_t ll=0; ll < ncols; ll++) {
				iss >> std::skipws >> tmp;
				if (iss.fail()) {
					ostringstream ss;
					ss << "Can not read column " << ll+1 << " of data line " << nrows-kk+nr_empty << " in file " << full_name << ": ";
					ss << ncols << " columns of doubles expected";
					throw InvalidFormatException(ss.str(), AT);
				}
				grid_out(ll, kk) = IOUtils::standardizeNodata(tmp, plugin_nodata);
			}
		}
	} catch(const std::exception& e) {
		fin.close();
		std::ostringstream msg;
		msg << "[E] Error when reading ARC grid \"" << full_name << "\" : " << e.what();
		throw InvalidFormatException(msg.str(), AT);
	}
	fin.close();
}

void ARCIO::read2DGrid(Grid2DObject& grid_out, const std::string& filename)
{
	read2DGrid_internal(grid_out, grid2dpath_in+"/"+filename);
}

bool ARCIO::list2DGrids(const Date& start, const Date& end, std::map<Date, std::set<size_t> > &results)
{
	results.clear();
	const double TZ = cfg.get("TIME_ZONE", "Input");
	std::list<std::string> dirlist( FileUtils::readDirectory(grid2dpath_in) ); //read everything. Toggle it to recursive if this changes in the plugin!
	dirlist.sort();

	if (a3d_view_in) {
		static const char NUM[] = "0123456789";
		static const size_t date_str_len = 12; //fix format for this plugin

		for (std::list<std::string>::const_iterator it = dirlist.begin(); it != dirlist.end(); ++it) {
			const std::string::size_type pos = it->find_first_not_of(NUM);
			if (pos==std::string::npos || pos!=date_str_len) continue; //for ARC, we skip the seconds -> date is 12 chars
			if (it->length() < (date_str_len+1)) continue; //we must have either '.' after the date
			if ((*it)[date_str_len]!='.') continue;

			Date date;
			if (!IOUtils::convertString(date, it->substr(0, date_str_len), TZ)) continue;
			if (date<start) continue;
			if (date>end) return true;

			const std::string ext( IOUtils::strToUpper(FileUtils::getExtension( *it )) );
			size_t param;
			if (ext=="SDP")
				param = MeteoGrids::HS;
			else if (ext=="SWR")
				param = MeteoGrids::ISWR;
			else if (ext=="LWR")
				param = MeteoGrids::ILWR;
			else if (ext=="ASC")
				param = MeteoGrids::DEM;
			else
				param = MeteoGrids::getParameterIndex( ext );
			if (param==IOUtils::npos) continue;
			results[date].insert( param );
		}
	} else {
		static const char DATE_CHAR[] = "0123456789-+T:.";
		static const size_t max_date_str_len = 19+6; //worst case scenario, with 6 chars for the timezone
		for (std::list<std::string>::const_iterator it = dirlist.begin(); it != dirlist.end(); ++it) {
			const std::string ext( "."+FileUtils::getExtension( *it ) );
			if (ext!=grid2d_ext_in) continue;

			const std::string::size_type pos = it->find_first_not_of(DATE_CHAR);
			if (pos==std::string::npos || pos>max_date_str_len) continue;

			std::string date_str(  it->substr(0, pos) );
			std::replace( date_str.begin(), date_str.end(), '.', ':');
			Date date;
			if (!IOUtils::convertString(date, date_str, TZ)) continue;
			if (date<start) continue;
			if (date>end) return true;

			const std::string::size_type pos_underscore = it->find('_');
			const std::string::size_type pos_dot = it->find_last_of('.');
			if (pos==std::string::npos || pos_dot==std::string::npos) continue;
			const std::string param_str( it->substr(pos_underscore+1, (pos_dot - pos_underscore - 1)) );
			const size_t param = MeteoGrids::getParameterIndex( param_str );
			if (param==IOUtils::npos) continue;
			results[date].insert( param );
		}
	}

	return true;
}

void ARCIO::read2DGrid(Grid2DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date)
{
	if (a3d_view_in) {
		// the A3D grid viewer looks for the following extensions:
		//sdp, tss, swr, lwr, swe, alb, wet
		std::string ext;
		if (parameter==MeteoGrids::HS)
			ext="sdp";
		else if (parameter==MeteoGrids::ISWR)
			ext="swr";
		else if (parameter==MeteoGrids::ILWR)
			ext="lwr";
		else if (parameter==MeteoGrids::DEM)
			ext="asc";
		else {
			ext = MeteoGrids::getParameterName(parameter);
			IOUtils::toLower(ext);
		}
		std::string dateStr( date.toString(Date::NUM) );
		dateStr.erase( dateStr.size()-2, string::npos); //remove the seconds
		read2DGrid_internal(grid_out, grid2dpath_in + "/" + dateStr + "." + ext );
	} else {
		std::string date_str( date.toString(Date::ISO) );
		std::replace( date_str.begin(), date_str.end(), ':', '.');
		read2DGrid_internal(grid_out, grid2dpath_in + "/" + date_str + "_" + MeteoGrids::getParameterName(parameter) + grid2d_ext_in);
	}
}

void ARCIO::readDEM(DEMObject& dem_out)
{
	const std::string filename = cfg.get("DEMFILE", "Input");
	read2DGrid_internal(dem_out, filename);
}

void ARCIO::readLanduse(Grid2DObject& landuse_out)
{
	const std::string filename = cfg.get("LANDUSEFILE", "Input");
	read2DGrid_internal(landuse_out, filename);
}

void ARCIO::readGlacier(Grid2DObject& glacier_out)
{
	const std::string filename = cfg.get("GLACIERFILE", "Input");
	read2DGrid_internal(glacier_out, filename);
}

void ARCIO::readAssimilationData(const Date& date_in, Grid2DObject& da_out)
{
	const std::string filepath = cfg.get("DAPATH", "Input");

	std::string dateStr( date_in.toString(Date::NUM) );
	dateStr.erase( dateStr.size()-2, string::npos); //remove the seconds
	read2DGrid_internal(da_out, filepath+"/"+dateStr+".sca");
}

void ARCIO::write2DGrid(const Grid2DObject& grid_in, const std::string& options)
{
	write2DGrid_internal(grid_in, options+grid2d_ext_out);
}

void ARCIO::write2DGrid_internal(const Grid2DObject& grid_in, const std::string& name) const
{
	const std::string full_name( grid2dpath_out+"/"+name );
	if (!FileUtils::validFileAndPath(full_name)) throw InvalidNameException(full_name,AT);
	errno = 0;
	std::ofstream fout(full_name.c_str(), ios::out);
	if (fout.fail()) {
		std::ostringstream ss;
		ss << "Error opening file \"" << full_name << "\", possible reason: " << std::strerror(errno);
		throw AccessException(ss.str(), AT);
	}

	try {
		Coords llcorner=grid_in.llcorner;
		//we want to make sure that we are using the provided projection parameters
		//so that we output is done in the same system as the inputs
		llcorner.setProj(coordout, coordoutparam);

		const size_t ncols = grid_in.getNx();
		const size_t nrows = grid_in.getNy();
		fout << fixed << showpoint << setprecision(6);
		fout << "ncols " << setw(23-6) << ncols << "\n";
		fout << "nrows " << setw(23-6) << nrows << "\n";
		fout << "xllcorner " << setw(23-10) << setprecision(3) << llcorner.getEasting() << "\n";
		fout << "yllcorner " << setw(23-10) << setprecision(3) << llcorner.getNorthing() << "\n";
		fout << "cellsize " << setw(23-9) << setprecision(3) << grid_in.cellsize << "\n";
		fout << "NODATA_value " << (int)(IOUtils::nodata) << "\n";

		if (nrows>0) {
			for (size_t kk=nrows; kk-->0; ) {
				for (size_t ll=0; ll < ncols; ll++){
					fout << grid_in(ll, kk) << " ";
				}
				fout << "\n";
			}
		}
	} catch(...) {
		cerr << "[E] error when writing ARC grid \"" << full_name << "\" " << AT << ": "<< endl;
		fout.close();
		throw;
	}

	fout.close();
}

void ARCIO::write2DGrid(const Grid2DObject& grid_in, const MeteoGrids::Parameters& parameter, const Date& date)
{
	//the path will be added by write2DGrid_internal
	if (a3d_view_out) {
		// the A3D grid viewer looks for the following extensions:
		//sdp, tss, swr, lwr, swe, alb, wet
		std::string ext;
		if (parameter==MeteoGrids::HS)
			ext="sdp";
		else if (parameter==MeteoGrids::ISWR)
			ext="swr";
		else if (parameter==MeteoGrids::ILWR)
			ext="lwr";
		else if (parameter==MeteoGrids::DEM)
			ext="asc";
		else {
			ext = MeteoGrids::getParameterName(parameter);
			IOUtils::toLower(ext);
		}
		std::string dateStr( date.toString(Date::NUM) );
		dateStr.erase( dateStr.size()-2, string::npos); //remove the seconds
		write2DGrid_internal(grid_in, dateStr+"."+ext );
	} else {
		if (parameter==MeteoGrids::DEM || parameter==MeteoGrids::AZI || parameter==MeteoGrids::SLOPE) {
			write2DGrid_internal(grid_in, MeteoGrids::getParameterName(parameter) + grid2d_ext_out);
		} else {
			std::string date_str( date.toString(Date::ISO) );
			std::replace( date_str.begin(), date_str.end(), ':', '.');
			write2DGrid_internal(grid_in, date_str + "_" + MeteoGrids::getParameterName(parameter) + grid2d_ext_out);
		}
	}
}

} //namespace
