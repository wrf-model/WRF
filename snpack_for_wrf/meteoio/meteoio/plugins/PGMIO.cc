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
#include <meteoio/plugins/PGMIO.h>
#include <meteoio/IOUtils.h>
#include <meteoio/FileUtils.h>
#include <meteoio/IOExceptions.h>

#include <iostream>
#include <fstream>
#include <cerrno>
#include <cstring>
#include <string.h>
#include <algorithm>

using namespace std;

namespace mio {
/**
 * @page pgmio PGMIO
 * @section pgmio_format Format
 * This reads a grid file in PGM format (see http://www.fileformat.info/format/pbm/egff.htm). This is a graphic format that is supported by a wide range of graphics programs (Gimp, Irfanview, Paint Shop Pro, gqview, etc). This allows to write a grid as an image (one pixel equals one cell), read an image as a grid (useful for creating synthetic DEMs). Since there is no geolocalization information in this format, such data is either encoded as a comment (when writing a file) or read from io.ini (for reading).
 * Finally, the naming scheme for meteo grids should be: YYYY-MM-DDTHH.mm_{MeteoGrids::Parameters}.pgm
 *
 * Please keep in mind that only a finite number of greyscales are used, making a discretization of the data. Moreover, we consider that a color of "0" is NODATA.
 *
 * @section pgmio_units Units
 * Cellsize is in meters, x/y coords in the section's coordinate system.
 *
 * @section pgmio_keywords Keywords
 * This plugin uses the following keywords:
 * - COORDSYS: coordinate system (see Coords); [Input] and [Output] section
 * - COORDPARAM: extra coordinates parameters (see Coords); [Input] and [Output] section
 * - GRID2DPATH: meteo grids directory where to read/write the grids; [Input] and [Output] sections
 * - PGM_XCOORD: lower left x coordinate; [Input] section
 * - PGM_YCOORD: lower left y coordinate; [Input] section
 * - PGM_CELLSIZE: cellsize in meters; [Input] section
 * - PGM_MIN: minimum value in real world coordinates to match with the minimum value read out of the PGM file (such minimum being greater than 0 because 0 is NODATA)
 * - PGM_MAX: maximum value in real world coordinates to match with the maximum value read out of the PGM file
 *
 * Example:
 * @code
 * [Input]
 * DEM          = PGM
 * DEMFILE      = ./input/surface-grids/synthetic.pgm
 * PGM_XCOORD   = 479500.
 * PGM_YCOORD   = 73500.
 * PGM_CELLSIZE = 1000.
 * PGM_MIN      = 193.
 * PGM_MAX      = 4204.
 * @endcode
 */

const double PGMIO::plugin_nodata = 0.; //plugin specific nodata value. It can also be read by the plugin (depending on what is appropriate)

PGMIO::PGMIO(const std::string& configfile)
       : cfg(configfile),
         coordin(), coordinparam(), coordout(), coordoutparam(),
         grid2dpath_in(), grid2dpath_out()
{
	IOUtils::getProjectionParameters(cfg, coordin, coordinparam, coordout, coordoutparam);
	getGridPaths();
}

PGMIO::PGMIO(const Config& cfgreader)
       : cfg(cfgreader),
         coordin(), coordinparam(), coordout(), coordoutparam(),
         grid2dpath_in(), grid2dpath_out()
{
	IOUtils::getProjectionParameters(cfg, coordin, coordinparam, coordout, coordoutparam);
	getGridPaths();
}

void PGMIO::getGridPaths()
{
	const std::string grid_in = cfg.get("GRID2D", "Input", "");
	if (grid_in == "PGM") //keep it synchronized with IOHandler.cc for plugin mapping!!
		cfg.getValue("GRID2DPATH", "Input", grid2dpath_in);
	const std::string grid_out = cfg.get("GRID2D", "Output", "");
	if (grid_out == "PGM") //keep it synchronized with IOHandler.cc for plugin mapping!!
		cfg.getValue("GRID2DPATH", "Output", grid2dpath_out);
}

size_t PGMIO::getNextHeader(std::vector<std::string>& vecString, const std::string& filename, std::ifstream& fin) 
{
	std::string line;
	while (!fin.eof()) {
		getline(fin, line);
		IOUtils::trim(line);
		if (!line.empty() && line.at(0)!='#') {
			return IOUtils::readLineToVec(line, vecString);
		}
	}
	throw IOException("Can not read necessary header lines in " + filename, AT);
}

bool PGMIO::readGridded(std::ifstream& fin, const std::string& full_name, const double& scale_factor, const double& val_min, Grid2DObject& grid_out)
{
	std::vector<std::string> tmpvec;
	std::string line;
	double tmp_val;
	const size_t ncols = grid_out.getNx();
	const size_t nrows = grid_out.getNy();
	const char eoln = FileUtils::getEoln(fin); //get the end of line character for the file

	for (size_t kk=nrows-1; (kk < nrows); kk--) {
		getline(fin, line, eoln); //read complete line

		const size_t nr_read = IOUtils::readLineToVec(line, tmpvec);
		if (nr_read==1) return false; //most probably it has not been written in gridded form

		if (nr_read != ncols) {
			ostringstream ss;
			ss << "Invalid number of columns at line " << nrows-kk << " in file \"" << full_name << "\". ";
			ss << "Expecting " << ncols << " columns\n";
			throw InvalidFormatException(ss.str(), AT);
		}

		for (size_t ll=0; ll < ncols; ll++){
			if (!IOUtils::convertString(tmp_val, tmpvec[ll], std::dec)) {
				throw ConversionFailedException("For Grid2D value in line: " + line + " in file " + full_name, AT);
			}

			if (tmp_val==plugin_nodata) {
				grid_out(ll, kk) = IOUtils::nodata; //replace file's nodata by uniform, internal nodata
			} else {
				grid_out(ll, kk) = (tmp_val-1)*scale_factor+val_min; //because color0 = nodata
			}
		}
	}

	return true;
}

void PGMIO::readColumn(std::ifstream& fin, const std::string& full_name, const double& scale_factor, const double& val_min, Grid2DObject& grid_out)
{
	std::vector<std::string> tmpvec;
	std::string line;
	double tmp_val;
	const size_t nr_cells = grid_out.size();
	const size_t ncols = grid_out.getNx();
	const char eoln = FileUtils::getEoln(fin); //get the end of line character for the file

	for (size_t kk=(nr_cells-1); kk-->0; ) {
		getline(fin, line, eoln); //read complete line
		const size_t row = kk/ncols;
		const size_t col = ncols - (kk - row*ncols) - 1;

		if (!IOUtils::convertString(tmp_val, line, std::dec)) {
			ostringstream ss;
			ss << "Error reading single column PGM file '" << full_name << "' at line " << row;
			throw InvalidFormatException(ss.str(), AT);
		}

		if (tmp_val==plugin_nodata) {
			grid_out(col, row) = IOUtils::nodata; //replace file's nodata by uniform, internal nodata
		} else {
			grid_out(col, row) = (tmp_val-1)*scale_factor+val_min; //because color0 = nodata
		}
	}
}

void PGMIO::read2DGrid_internal(Grid2DObject& grid_out, const std::string& full_name) const
{
	if (!FileUtils::validFileAndPath(full_name)) throw InvalidNameException(full_name, AT);
	if (!FileUtils::fileExists(full_name)) throw NotFoundException(full_name, AT);

	errno = 0;
	std::ifstream fin;
	fin.open (full_name.c_str(), ifstream::in);
	if (fin.fail()) {
		std::ostringstream ss;
		ss << "Error opening file \"" << full_name << "\", possible reason: " << std::strerror(errno);
		throw AccessException(ss.str(), AT);
	}

	size_t ncols, nrows;
	unsigned int nr_colors;
	double xllcorner, yllcorner, cellsize;
	double val_min, val_max;
	std::vector<std::string> tmpvec;
	//Go through file, save key value pairs
	try {
		//read header: magic value
		if (getNextHeader(tmpvec, full_name, fin)!=1) {
			throw IOException("Can not read necessary header in " + full_name, AT);
		}
		if (tmpvec.size()!=1) {
			throw IOException("Invalid PGM signature for file " + full_name, AT);
		}
		if (tmpvec[0]!="P2") {
			throw IOException("Invalid PGM signature for file " + full_name, AT);
		}

		//read header: image width and height
		if (getNextHeader(tmpvec, full_name, fin)!=2) {
			throw IOException("Can not read necessary header in " + full_name, AT);
		}
		IOUtils::convertString(ncols, tmpvec[0]);
		IOUtils::convertString(nrows, tmpvec[1]);
		//read header: number of greys
		if (getNextHeader(tmpvec, full_name, fin)!=1) {
			throw IOException("Can not read necessary header in " + full_name, AT);
		}
		IOUtils::convertString(nr_colors, tmpvec[0]);

		cfg.getValue("PGM_XCOORD", "Input", xllcorner, IOUtils::dothrow);
		cfg.getValue("PGM_YCOORD", "Input", yllcorner, IOUtils::dothrow);
		cfg.getValue("PGM_CELLSIZE", "Input", cellsize, IOUtils::dothrow);
		cfg.getValue("PGM_MIN", "Input", val_min, IOUtils::dothrow);
		cfg.getValue("PGM_MAX", "Input", val_max, IOUtils::dothrow);

		Coords location(coordin, coordinparam);
		location.setXY(xllcorner, yllcorner, IOUtils::nodata);

		//Initialize the 2D grid
		grid_out.set(ncols, nrows, cellsize, location);

		//initialize scale factor
		const double scale_factor = (val_max-val_min)/(double)(nr_colors-2); //because 256 colors = 0 to 255!! and color0 = nodata

		if (!readGridded(fin, full_name, scale_factor, val_min, grid_out)) //if the data is not written in gridded form, then it must be as a single column
			readColumn(fin, full_name, scale_factor, val_min, grid_out);
	} catch(const std::exception&) {
		cerr << "[E] error when reading PGM grid \"" << full_name << "\" " << AT << ": "<< endl;
		fin.close();
		throw;
	}

	fin.close();
}

void PGMIO::read2DGrid(Grid2DObject& grid_out, const std::string& filename) {
	read2DGrid_internal(grid_out, grid2dpath_in+"/"+filename);
}

bool PGMIO::list2DGrids(const Date& start, const Date& end, std::map<Date, std::set<size_t> > &results)
{
	results.clear();
	std::list<std::string> dirlist( FileUtils::readDirectory(grid2dpath_in) ); //read everything. Toggle it to recursive if this changes in the plugin!
	dirlist.sort();
	
	for (std::list<std::string>::const_iterator it = dirlist.begin(); it != dirlist.end(); ++it) {
		const std::string ext( FileUtils::getExtension( *it ) );
		if (ext!=".pgm") continue;
		const std::string filename( FileUtils::removeExtension( FileUtils::getFilename(*it) ) );
		const std::string::size_type pos = filename.find_last_of('_');
		if (pos==std::string::npos) continue;
		const std::string date_str( filename.substr(0, pos) );
		
		Date date;
		if (!IOUtils::convertString(date, date_str, start.getTimeZone())) continue;
		if (date<start) continue;
		if (date>end) return true;
		
		const std::string param_str( filename.substr(pos) );
		const size_t param = MeteoGrids::getParameterIndex( param_str );
		if (param==IOUtils::npos) continue;
		results[date].insert( param );
	}
	
	return true;
}

void PGMIO::read2DGrid(Grid2DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date)
{
	std::string date_str( date.toString(Date::ISO) );
	std::replace( date_str.begin(), date_str.end(), ':', '.');
	read2DGrid_internal(grid_out, grid2dpath_in+"/"+date_str+"_"+MeteoGrids::getParameterName(parameter)+".pgm");
}

void PGMIO::readDEM(DEMObject& dem_out)
{
	std::string filename;
	cfg.getValue("DEMFILE", "Input", filename);
	read2DGrid_internal(dem_out, filename);
}

void PGMIO::write2DGrid(const Grid2DObject& grid_in, const std::string& name)
{
	const std::string full_name( grid2dpath_out+"/"+name );
	static const unsigned int nr_colors = 256;
	if (!FileUtils::validFileAndPath(full_name)) throw InvalidNameException(full_name, AT);
	errno = 0;
	std::ofstream fout;
	fout.open(full_name.c_str(), ios::out);
	if (fout.fail()) {
		std::ostringstream ss;
		ss << "Error opening file \"" << full_name << "\", possible reason: " << std::strerror(errno);
		throw AccessException(ss.str(), AT);
	}

	Coords llcorner( grid_in.llcorner );
	//we want to make sure that we are using the provided projection parameters
	//so that we output is done in the same system as the inputs
	llcorner.setProj(coordout, coordoutparam);

	fout << fixed << showpoint << setprecision(6);

	try {
		const double min_value = grid_in.grid2D.getMin();
		const double max_value = grid_in.grid2D.getMax();
		const double scaling = (min_value==max_value)? 1. : 1./(max_value - min_value) * (double)(nr_colors-1); //so we keep color 0 for nodata

		//writing the header
		fout << "P2\n";
		fout << "#Generated by MeteoIO - https://models.slf.ch/p/meteoio\n";
		fout << "#llcorner latitude = " << setprecision(6) << llcorner.getLat() << "\n";
		fout << "#llcorner longitude = " << setprecision(6) << llcorner.getLon() << "\n";
		fout << "#cellsize = " << setprecision(2) << grid_in.cellsize << " m\n";
		fout << "#minimum = " << setprecision(6) << min_value << "\n";
		fout << "#maximum = " << setprecision(6) << max_value << "\n";
		fout << grid_in.getNx() << " " << grid_in.getNy() << "\n";
		fout << nr_colors << "\n";

		//writing the data
		if (grid_in.getNy()>0) {
			for (size_t kk=grid_in.getNy()-1; kk < grid_in.getNy(); kk--) {
				for (size_t ll=0; ll < grid_in.getNx(); ll++) {
					const double value = grid_in(ll, kk);
					if (value!=IOUtils::nodata)
						fout << static_cast<unsigned int>( floor((grid_in(ll, kk)-min_value)*scaling)+1 ) << " ";
					else
						fout << "0" << " ";
				}
				fout << "\n";
			}
		}
	} catch(...) {
		cerr << "[E] error when writing PGM grid \"" << full_name << "\" " << AT << ": "<< endl;
		fout.close();
		throw;
	}

	fout.close();
}

void PGMIO::write2DGrid(const Grid2DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date)
{
	//path will be added by calling write2Dgrid
	std::string date_str = date.toString(Date::ISO);
	std::replace( date_str.begin(), date_str.end(), ':', '.');
	write2DGrid(grid_out, date_str+"_"+MeteoGrids::getParameterName(parameter)+".pgm");
}

} //namespace
