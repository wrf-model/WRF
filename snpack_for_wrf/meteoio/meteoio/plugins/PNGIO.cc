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
#include <meteoio/plugins/PNGIO.h>
#include <meteoio/meteoStats/libresampling2D.h>
#include <meteoio/Graphics.h>
#include <meteoio/FileUtils.h>
#include <meteoio/meteoLaws/Meteoconst.h>

#include <fstream>
#include <cstring>
#include <algorithm>
#include <cerrno>
#include <zlib.h>

using namespace std;

namespace mio {
/**
 * @page pngio PNGIO
 * @section pngio_format Format
 * This plugin write data to the Portable Network Graphics format (see https://secure.wikimedia.org/wikipedia/en/wiki/Portable_Network_Graphics).
 * No data read has been implemented, because reading an existing file would require the exact knowlege of the color gradient that has been used
 * to create it. When writing grids, various color gradients will be used depending on the parameter that the data represents. Nodata values
 * are represented by transparent pixels (transparency is acheived through a transparent color instead of a true alpha channel for size and performance).
 * If a grid containing no data (ie: size 0x0) is sent to the plugin, then no file will be written.
 * Finally, the naming scheme for meteo grids should be: YYYY-MM-DDTHH.mm_{MeteoGrids::Parameters}.png
 *
 * @section pngio_units Units
 * All units are MKSA except temperatures that are expressed in celcius.
 *
 * @section pngio_keywords Keywords
 * This plugin uses the following keywords:
 * - COORDSYS: input coordinate system (see Coords) specified in the [Output] section
 * - COORDPARAM: extra input coordinates parameters (see Coords) specified in the [Output] section
 * - GRID2DPATH: meteo grids directory where to read the grids; [Output] section
 * - PNG_LEGEND: plot legend on the side of the graph? (default: true)
 * - PNG_MIN_SIZE: guarantee that a 2D plot will have at least the given size
 * - PNG_MAX_SIZE: guarantee that a 2D plot will have at most the given size
 * - PNG_SCALING: scaling algorithm, either nearest or bilinear (default=bilinear)
 * - PNG_AUTOSCALE: autoscale for the color gradient? (default=true)
 * - PNG_WORLD_FILE: create world file with each file? (default=false)
 *
 * Advanced parameters (ie: don't mess up with them if you don't know what you're doing):
 * - PNG_INDEXED: create an indexed PNG? (default=true)
 * - PNG_NR_LEVELS: number of colors to use (less=smaller files, but it must be at least 5 and less than 255. default=30)
 * - PNG_SPEED_OPTIMIZE: optimize file creation for speed? (default=true, otherwise optimize for file size)
 *
 * The size are specified as width followed by height, with the separator being either a space, 'x' or '*'. If a minimum and a maximum size are given, the average of the smallest and largest permissible sizes will be used.
 * The world file is used for geolocalization and goes alongside the graphics output. By convention,
 * the file has the same name as the image file, with the third letter of the extension jammed with a w: tif->tfw, jpg->jqw.
 * The format is the following:
 * @code
 *    5.000000000000 (size of pixel in x direction)
 *    0.000000000000 (rotation term for row)
 *    0.000000000000 (rotation term for column)
 *    -5.000000000000 (size of pixel in y direction)
 *    492169.690845528910 (x coordinate of centre of upper left pixel in map units)
 *    5426523.318065105000 (y coordinate of centre of upper left pixel in map units)
 * @endcode
 *
 * @section pngio_example Example use
 * @code
 * [ouput]
 * GRID2D     = PNG
 * png_legend = false
 * png_min_size = 400x400
 * png_max_size = 1366*768
 * @endcode
 *
 * @section pngio_compilation
 * In order to compile this plugin, you need libpng and zlib. For Linux, please select both the libraries and their development files in your package manager.
 *
 * For Mac, starting with Sierra, you can use the system png library: from a terminal window, run
 * <i>/Library/Frameworks/UnixImageIO.framework/Programs/libpng-config --libdir</i> and provide this path with <i>libpng.dylib</i> appended
 * to cmake as <i>PNG_LIBRARY_RELEASE</i> (you might have to press <i>t</i> in cmake in order to show the advanced options.).
 * Otherwise (ie if you are not using Sierra or do not want to use the system version), you can either install
 * using <A href="http://www.finkproject.org/">Fink</A> or directly from <a href="http://www.libpng.org">source</a>.
 * In this case, please install first <a href="http://zlib.net/">zlib</a> at the prefix of your choice (with the <i>prefix</i> option of its configure script, for
 * example <i>"./configure --prefix=/usr/local"</i>, then build with <i>"make"</i> and install with <i>"sudo make install"</i>). Then run the
 * libpng configure script with the <i>--enable-shared</i> and <i>prefix</i> options, for example <i>"./configure --enable-shared --prefix=/usr/local"</i>
 * (and then build with <i>"make"</i> followed by <i>"sudo make install"</i>).
 * 
 * For Windows, you can find zlib at http://switch.dl.sourceforge.net/project/gnuwin32/zlib/1.2.3/zlib-1.2.3.exe
 * and libpng at http://switch.dl.sourceforge.net/project/gnuwin32/libpng/1.2.37/libpng-1.2.37-setup.exe . Once this has been installed, if you plan on using
 * Visual c++, you also need to edit the file zconf.h in the libpng installation directory and transform the line 287:
 * @code
 * #if 0           // HAVE_UNISTD_H etc etc
 * @endcode
 * should become
 * @code
 * #if 1           // HAVE_UNISTD_H etc etc
 * @endcode
 */

const double PNGIO::plugin_nodata = -999.; //plugin specific nodata value. It can also be read by the plugin (depending on what is appropriate)
const unsigned char PNGIO::channel_depth = 8;
const unsigned char PNGIO::channel_max_color = 255;
const unsigned char PNGIO::transparent_grey = channel_max_color;

PNGIO::PNGIO(const std::string& configfile)
       : cfg(configfile),
         fp(NULL), autoscale(true), has_legend(true), has_world_file(false), optimize_for_speed(true),
         indexed_png(true), nr_levels(30),
         coordout(), coordoutparam(), grid2dpath(),
         scaling("bilinear"), min_w(IOUtils::unodata), min_h(IOUtils::unodata), max_w(IOUtils::unodata), max_h(IOUtils::unodata),
         metadata_key(), metadata_text()
{
	setOptions();
}

PNGIO::PNGIO(const Config& cfgreader)
       : cfg(cfgreader),
         fp(NULL), autoscale(true), has_legend(true), has_world_file(false), optimize_for_speed(true),
         indexed_png(true), nr_levels(30),
         coordout(), coordoutparam(), grid2dpath(),
         scaling("bilinear"), min_w(IOUtils::unodata), min_h(IOUtils::unodata), max_w(IOUtils::unodata), max_h(IOUtils::unodata),
         metadata_key(), metadata_text()
{
	setOptions();
}

PNGIO& PNGIO::operator=(const PNGIO& source) {
	if (this != &source) {
		fp = NULL;
		autoscale = source.autoscale;
		has_legend = source.has_legend;
		has_world_file = source.has_world_file;
		optimize_for_speed = source.optimize_for_speed;
		indexed_png = source.indexed_png;
		nr_levels = source.nr_levels;
		coordout = source.coordout;
		coordoutparam = source.coordoutparam;
		grid2dpath = source.grid2dpath;
		scaling = source.scaling;
		min_w = source.min_w;
		min_h = source.min_h;
		max_w = source.max_w;
		max_h = source.max_h;
		metadata_key = source.metadata_key;
		metadata_text = source.metadata_text;
	}
	return *this;
}

PNGIO::~PNGIO() throw() 
{
	if (fp!=NULL) fclose(fp); 
	fp=NULL;
}

void PNGIO::setOptions()
{
	//default values have been set by the constructors
	cfg.getValue("COORDSYS", "Output", coordout);
	cfg.getValue("COORDPARAM", "Output", coordoutparam, IOUtils::nothrow);
	cfg.getValue("GRID2DPATH", "Output", grid2dpath);
	//cfg.getValue("TIME_ZONE", "Output", tz_out, IOUtils::nothrow);

	//get size specifications
	std::string min_size, max_size;
	cfg.getValue("PNG_MIN_SIZE", "Output", min_size, IOUtils::nothrow);
	if (!min_size.empty()) parse_size(min_size, min_w, min_h);
	cfg.getValue("PNG_MAX_SIZE", "Output", max_size, IOUtils::nothrow);
	if (!max_size.empty()) parse_size(max_size, max_w, max_h);

	cfg.getValue("PNG_AUTOSCALE", "Output", autoscale, IOUtils::nothrow);
	cfg.getValue("PNG_LEGEND", "Output", has_legend, IOUtils::nothrow);
	cfg.getValue("PNG_SCALING", "Output", scaling, IOUtils::nothrow);
	cfg.getValue("PNG_WORLD_FILE", "Output", has_world_file, IOUtils::nothrow);

	if (has_legend) { //we need to save room for the legend
		if (min_w!=IOUtils::unodata) min_w -= Legend::getLegendWidth();
		if (max_w!=IOUtils::unodata) max_w -= Legend::getLegendWidth();
	}

	cfg.getValue("PNG_INDEXED", "Output", indexed_png, IOUtils::nothrow);
	cfg.getValue("PNG_SPEED_OPTIMIZE", "Output", optimize_for_speed, IOUtils::nothrow);
	unsigned int tmp=IOUtils::unodata;
	cfg.getValue("PNG_NR_LEVELS", "Output", tmp, IOUtils::nothrow);
	if (tmp!=IOUtils::unodata && (tmp>255 || tmp<5)) {
		throw InvalidFormatException("PNG_NR_LEVELS must be between 5 and 255!", AT);
	}
	if (tmp!=IOUtils::unodata) nr_levels=static_cast<unsigned char>(tmp);
}

void PNGIO::parse_size(const std::string& size_spec, size_t& width, size_t& height)
{
	char rest[32] = "";
	unsigned int w,h;
	if (sscanf(size_spec.c_str(), "%u %u%31s", &w, &h, rest) < 2)
	if (sscanf(size_spec.c_str(), "%u*%u%31s", &w, &h, rest) < 2)
	if (sscanf(size_spec.c_str(), "%ux%u%31s", &w, &h, rest) < 2) {
		std::ostringstream ss;
		ss << "Can not parse PNGIO size specification \"" << size_spec << "\"";
		throw InvalidFormatException(ss.str(), AT);
	}

	width = static_cast<size_t>( w );
	height = static_cast<size_t>( h );

	std::string tmp(rest);
	IOUtils::trim(tmp);
	if ((tmp.length() > 0) && tmp[0] != '#' && tmp[0] != ';') {//if line holds more than one value it's invalid
		throw InvalidFormatException("Invalid PNGIO size specification \"" + size_spec + "\"", AT);
	}
}

double PNGIO::getScaleFactor(const size_t& grid_w, const size_t& grid_h) const
{
	if (grid_w==0 || grid_h==0) {
		return 1.;
	}

	double min_factor = IOUtils::nodata;
	if (min_w!=IOUtils::unodata) { //min_w & min_w are read together
		const double min_w_factor = (double)min_w / (double)grid_w;
		const double min_h_factor = (double)min_h / (double)grid_h;
		min_factor = std::max(min_w_factor, min_h_factor);
	}

	double max_factor = IOUtils::nodata;
	if (max_w!=IOUtils::unodata) { //max_w & max_h are read together
		const double max_w_factor = (double)max_w / (double)grid_w;
		const double max_h_factor = (double)max_h / (double)grid_h;
		max_factor = std::min(max_w_factor, max_h_factor);
	}

	if (min_factor==IOUtils::nodata && max_factor==IOUtils::nodata)
		return 1.; //no user given specification
	if (min_factor!=IOUtils::nodata && max_factor!=IOUtils::nodata)
		return (min_factor+max_factor)/2.; //both min & max -> average

	//only one size specification provided -> return its matching factor
	if (min_factor!=IOUtils::nodata)
		return min_factor;
	else
		return max_factor;
}

Grid2DObject PNGIO::scaleGrid(const Grid2DObject& grid_in) const
{ //scale input image
	const double factor = getScaleFactor(grid_in.getNx(), grid_in.getNy());
	if (scaling=="nearest")
		return LibResampling2D::Nearest(grid_in, factor);
	else if (scaling=="bilinear")
		return LibResampling2D::Bilinear(grid_in, factor);
	else {
		ostringstream ss;
		ss << "Grid scaling algorithm \"" << scaling << "\" unknown";
		throw UnknownValueException(ss.str(), AT);
	}
}

void PNGIO::setFile(const std::string& filename, png_structp& png_ptr, png_infop& info_ptr, const size_t &width, const size_t &height)
{
	// Open file for writing (binary mode)
	if (!FileUtils::validFileAndPath(filename)) throw InvalidNameException(filename, AT);
	errno=0;
	fp = fopen(filename.c_str(), "wb");
	if (fp == NULL)
		throw AccessException("Error opening file \""+filename+"\", possible reason: "+std::string(std::strerror(errno)), AT);

	// Initialize write structure
	png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
	if (png_ptr == NULL) {
		fclose(fp); fp=NULL;
		throw IOException("Could not allocate write structure. Are you running with the same version of libpng that you linked to?", AT);
	}

	// Initialize info structure
	info_ptr = png_create_info_struct(png_ptr);
	if (info_ptr == NULL) {
		fclose(fp); fp=NULL;
		png_destroy_write_struct(&png_ptr, (png_infopp)NULL);
		free(png_ptr);
		throw IOException("Could not allocate info structure", AT);
	}

	// Setup Exception handling
#ifdef _MSC_VER
	#pragma warning(disable:4611) //the setjmp of libpng has been set up so that it can safely be called from c++
#endif
	if (setjmp(png_jmpbuf(png_ptr))) {
		closePNG(png_ptr, info_ptr, NULL);
		throw IOException("Error during png creation. Can not set jump pointer (I have no clue what it means too!)", AT);
	}

	png_init_io(png_ptr, fp);

	if (optimize_for_speed) png_set_compression_level(png_ptr, Z_BEST_SPEED);
	else png_set_compression_level(png_ptr, Z_BEST_COMPRESSION);

	png_set_filter(png_ptr, PNG_FILTER_TYPE_BASE, PNG_FILTER_SUB|PNG_FILTER_UP); //any other filter is costly and brings close to nothing...
	if (indexed_png) png_set_compression_strategy(png_ptr, Z_RLE); //Z_DEFAULT_STRATEGY, Z_FILTERED, Z_HUFFMAN_ONLY, Z_RLE

	// Write header (8 bit colour depth). Full alpha channel with PNG_COLOR_TYPE_RGB_ALPHA
	if (indexed_png) {
		png_set_IHDR(png_ptr, info_ptr, static_cast<png_uint_32>(width), static_cast<png_uint_32>(height),
			channel_depth, PNG_COLOR_TYPE_PALETTE, PNG_INTERLACE_NONE,
			PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);
		//set transparent color (ie: cheap transparency: leads to smaller files and shorter run times)
		png_byte trans = 0; //by convention, the gradient define it as color 0
		png_set_tRNS(png_ptr, info_ptr, &trans, 1, 0);
	} else {
		png_set_IHDR(png_ptr, info_ptr, static_cast<png_uint_32>(width), static_cast<png_uint_32>(height),
			channel_depth, PNG_COLOR_TYPE_RGB, PNG_INTERLACE_NONE,
			PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);
		//set transparent color (ie: cheap transparency: leads to smaller files and shorter run times)
		png_color_16 trans_rgb_value = {transparent_grey, transparent_grey, transparent_grey, transparent_grey, transparent_grey};
		png_set_tRNS(png_ptr, info_ptr, 0, 0, &trans_rgb_value);
	}

	//set background color to help applications show the picture when no background is present
	png_color_16 background = {channel_max_color, channel_max_color, channel_max_color, channel_max_color, channel_max_color};
	png_set_background(png_ptr, &background, PNG_BACKGROUND_GAMMA_SCREEN, true, 1.0);
}

size_t PNGIO::setLegend(const size_t &ncols, const size_t &nrows, const double &min, const double &max, Array2D<double> &legend_array) const
{
	if (has_legend) {
		const Legend legend(static_cast<unsigned int>(nrows), min, max);
		legend_array = legend.getLegend();
		const size_t nx = legend_array.getNx();
		return (ncols+nx);
	} else {
		return ncols;
	}
}

png_color* PNGIO::setPalette(const Gradient &gradient, png_structp& png_ptr, png_infop& info_ptr)
{
	std::vector<unsigned char> pal;
	size_t nr_colors;
	gradient.getPalette(pal, nr_colors);
	png_color *palette = (png_color*)calloc(nr_colors, sizeof (png_color)); //ie: three png_bytes, each being an unsigned char
	for (size_t ii=0; ii<nr_colors; ii++) {
		const size_t interlace = ii*3; //colors from Gradient interlaced
		palette[ii].red = static_cast<png_byte>(pal[interlace]);
		palette[ii].green = static_cast<png_byte>(pal[interlace+1]);
		palette[ii].blue = static_cast<png_byte>(pal[interlace+2]);
	}
	png_set_PLTE(png_ptr, info_ptr, palette, static_cast<int>(nr_colors));
	return palette;
}

void PNGIO::writeDataSection(const Grid2DObject& grid, const Array2D<double>& legend_array, const Gradient& gradient, const size_t& full_width, const png_structp& png_ptr, png_infop& info_ptr)
{
	const size_t ncols = grid.getNx();
	const size_t nrows = grid.getNy();

	// Allocate memory for one row (3 bytes per pixel - RGB)
	const unsigned char channels = (indexed_png)? 1 : 3; //4 for rgba
	png_bytep row = (png_bytep)calloc(full_width, channels*sizeof(png_byte));
	if (row==NULL) {
		fclose(fp); fp=NULL;
		png_free_data(png_ptr, info_ptr, PNG_FREE_ALL, -1);
		free(png_ptr);
		throw IOException("Can not allocate row memory for PNG!", AT);
	}

	// Write image data
	if (indexed_png) {
		for (size_t y=nrows ; y-- > 0; ) {
			size_t x=0;
			for (; x<ncols ; x++) {
				const size_t i=x*channels;
				unsigned char index;
				gradient.getColor(grid(x,y), index);
				row[i]=static_cast<png_byte>(index);
			}
			for (; x<full_width; x++) {
				const size_t i=x*channels;
				unsigned char index;
				gradient.getColor(legend_array(x-ncols,y), index);
				row[i]=static_cast<png_byte>(index);
			}
			png_write_row(png_ptr, row);
		}
	} else {
		for (size_t y=nrows ; y -- > 0; ) {
			size_t x=0;
			for (; x<ncols ; x++) {
				const size_t i=x*channels;
				unsigned char r,g,b;
				bool a;
				gradient.getColor(grid(x,y), r,g,b,a);
				if (a==true) {
					row[i]=static_cast<png_byte>(transparent_grey); row[i+1]=static_cast<png_byte>(transparent_grey); row[i+2]=static_cast<png_byte>(transparent_grey);
				} else {
					row[i]=static_cast<png_byte>(r); row[i+1]=static_cast<png_byte>(g); row[i+2]=static_cast<png_byte>(b);
				}
			}
			for (; x<full_width; x++) {
				const size_t i=x*channels;
				unsigned char r,g,b;
				bool a;
				gradient.getColor(legend_array(x-ncols,y), r,g,b,a);
				if (a==true) {
					row[i]=static_cast<png_byte>(transparent_grey); row[i+1]=static_cast<png_byte>(transparent_grey); row[i+2]=static_cast<png_byte>(transparent_grey);
				} else {
					row[i]=static_cast<png_byte>(r); row[i+1]=static_cast<png_byte>(g); row[i+2]=static_cast<png_byte>(b);
				}
			}
			png_write_row(png_ptr, row);
		}
	}

	png_write_flush(png_ptr);
	png_free(png_ptr, row);
}

void PNGIO::closePNG(png_structp& png_ptr, png_infop& info_ptr, png_color *palette)
{
	png_free_data(png_ptr, info_ptr, PNG_FREE_ALL, -1);
	png_destroy_write_struct(&png_ptr, &info_ptr);
	if (indexed_png && palette!=NULL) free(palette);
	fclose(fp); fp=NULL;
	free(info_ptr);
	free(png_ptr);
}

void PNGIO::write2DGrid(const Grid2DObject& grid_in, const std::string& filename)
{
	const std::string full_name( grid2dpath+"/"+filename );
	fp=NULL;
	png_structp png_ptr=NULL;
	png_infop info_ptr=NULL;

	//scale input image
	const Grid2DObject grid( scaleGrid(grid_in) );
	const size_t ncols = grid.getNx(), nrows = grid.getNy();
	if (ncols==0 || nrows==0) return;

	const double min = grid.grid2D.getMin();
	const double max = grid.grid2D.getMax();

	Gradient gradient(Gradient::heat, min, max, autoscale);
	if (indexed_png) gradient.setNrOfLevels(nr_levels);

	Array2D<double> legend_array; //it will remain empty if there is no legend
	const size_t full_width = setLegend(ncols, nrows, min, max, legend_array);

	setFile(full_name, png_ptr, info_ptr, full_width, nrows);
	png_color *palette = (indexed_png)? setPalette(gradient, png_ptr, info_ptr) : NULL;
	if (has_world_file) writeWorldFile(grid, full_name);

	createMetadata(grid);
	metadata_key.push_back("Title"); //adding generic title
	metadata_text.push_back("Unknown Gridded data");
	writeMetadata(png_ptr, info_ptr);

	writeDataSection(grid, legend_array, gradient, full_width, png_ptr, info_ptr);
	png_write_end(png_ptr, NULL);

	closePNG(png_ptr, info_ptr, palette);
}

void PNGIO::write2DGrid(const Grid2DObject& grid_in, const MeteoGrids::Parameters& parameter, const Date& date)
{
	const bool isNormalParam = (parameter!=MeteoGrids::DEM && parameter!=MeteoGrids::SLOPE && parameter!=MeteoGrids::AZI);
	std::string date_str = (isNormalParam)? date.toString(Date::ISO)+"_" : "";
	std::replace( date_str.begin(), date_str.end(), ':', '.');
	const std::string filename( grid2dpath + "/" + date_str + MeteoGrids::getParameterName(parameter) + ".png" );

	fp=NULL;
	png_structp png_ptr=NULL;
	png_infop info_ptr=NULL;

	//scale input image
	Grid2DObject grid( scaleGrid(grid_in) );
	const size_t ncols = grid.getNx(), nrows = grid.getNy();
	if (ncols==0 || nrows==0) return;

	double min = grid.grid2D.getMin();
	double max = grid.grid2D.getMax();

	Gradient gradient;
	if (parameter==MeteoGrids::DEM) {
		if (!autoscale) {
			min = 0.; //we want a 3000 snow line with a full scale legend
			max = 3500.;
			gradient.set(Gradient::terrain, min, max, autoscale); //max used as snow line reference
		} else
			gradient.set(Gradient::terrain, min, max, autoscale);
	} else if (parameter==MeteoGrids::SLOPE) {
		gradient.set(Gradient::slope, min, max, autoscale);
	} else if (parameter==MeteoGrids::SHADE) {
		if (!autoscale) {
			min = 0.; max = 1.;
		}
		gradient.set(Gradient::blktowhite, min, max, autoscale);
	} else if (parameter==MeteoGrids::AZI) {
		if (!autoscale) {
			min = 0.;
			max = 360.;
		}
		gradient.set(Gradient::azi, min, max, autoscale);
	} else if (parameter==MeteoGrids::DW) {
		if (!autoscale) {
			min = 0.;
			max = 360.;
		}
		gradient.set(Gradient::azi, min, max, autoscale);
	} else if (parameter==MeteoGrids::HS) {
		if (!autoscale) {
			min = 0.; max = 2.5;
		}
		gradient.set(Gradient::blue, min, max, autoscale);
	} else if (parameter==MeteoGrids::TA) {
		grid.grid2D -= Cst::t_water_freezing_pt; //convert to celsius
		if (!autoscale) {
			min = -15.; max = 15.;
		} else {
			min -= Cst::t_water_freezing_pt;
			max -= Cst::t_water_freezing_pt;
		}
		gradient.set(Gradient::heat, min, max, autoscale);
	} else if (parameter==MeteoGrids::TSS) {
		grid.grid2D -= Cst::t_water_freezing_pt; //convert to celsius
		if (!autoscale) {
			min = -20.; max = 5.;
		} else {
			min -= Cst::t_water_freezing_pt;
			max -= Cst::t_water_freezing_pt;
		}
		gradient.set(Gradient::freeze, min, max, autoscale);
	} else if (parameter==MeteoGrids::TSOIL) {
		grid.grid2D -= Cst::t_water_freezing_pt; //convert to celsius
		if (!autoscale) {
			min = -5.; max = 5.;
		} else {
			min -= Cst::t_water_freezing_pt;
			max -= Cst::t_water_freezing_pt;
		}
		gradient.set(Gradient::heat, min, max, autoscale);
	} else if (parameter==MeteoGrids::RH) {
		if (!autoscale) {
			min = 0.; max = 1.;
		}
		gradient.set(Gradient::bg_isomorphic, min, max, autoscale);
	} else if (parameter==MeteoGrids::P) {
		if (!autoscale) {
			//lowest and highest sea level pressures ever recorded on Earth: 87000 and 108570
			min = 87000.; max = 115650.; //centered around 1 atm
			gradient.set(Gradient::bluewhitered, min, max, autoscale);
		} else {
			const double delta1 = fabs(Cst::std_press-min);
			const double delta2 = fabs(max - Cst::std_press);
			const double delta = (delta1>delta2)?delta1:delta2;
			gradient.set(Gradient::bluewhitered, Cst::std_press-delta, Cst::std_press+delta, autoscale);
		}
	} else if (parameter==MeteoGrids::ALB) {
		if (!autoscale) {
			min = 0.; max = 1.;
		}
		gradient.set(Gradient::blktowhite, min, max, autoscale);
	} else if (parameter==MeteoGrids::TAU_CLD) {
		if (!autoscale) {
			min = 0.; max = 1.;
		}
		gradient.set(Gradient::blktowhite, min, max, autoscale);
	} else if (parameter==MeteoGrids::ISWR) {
		if (!autoscale) {
			min = 0.; max = 800.;
		}
		gradient.set(Gradient::heat, min, max, autoscale);
	} else if (parameter==MeteoGrids::ILWR) {
		if (!autoscale) {
			min = 150.; max = 400.;
		}
		gradient.set(Gradient::heat, min, max, autoscale);
	} else if (parameter==MeteoGrids::SWE) {
		if (!autoscale) {
			min = 0.; max = 250.;
		}
		gradient.set(Gradient::blue_pink, min, max, autoscale);
	} else if (parameter==MeteoGrids::PSUM_PH) {
		min = 0.; max = 1.;
		gradient.set(Gradient::bluewhitered, min, max, autoscale);
	} else {
		gradient.set(Gradient::heat, min, max, autoscale);
	}
	gradient.setNrOfLevels(nr_levels);

	Array2D<double> legend_array; //it will remain empty if there is no legend
	const size_t full_width = setLegend(ncols, nrows, min, max, legend_array);

	setFile(filename, png_ptr, info_ptr, full_width, nrows);
	png_color *palette = (indexed_png)? setPalette(gradient, png_ptr, info_ptr) : NULL;
	if (has_world_file) writeWorldFile(grid, filename);

	createMetadata(grid);
	metadata_key.push_back("Title"); //adding title
	metadata_text.push_back( MeteoGrids::getParameterName(parameter)+" on "+date.toString(Date::ISO) );
	metadata_key.push_back("Simulation Date");
	metadata_text.push_back( date.toString(Date::ISO) );
	metadata_key.push_back("Simulation Parameter");
	metadata_text.push_back( MeteoGrids::getParameterName(parameter) );
	writeMetadata(png_ptr, info_ptr);

	writeDataSection(grid, legend_array, gradient, full_width, png_ptr, info_ptr);
	png_write_end(png_ptr, NULL);

	closePNG(png_ptr, info_ptr, palette);
}

void PNGIO::writeWorldFile(const Grid2DObject& grid_in, const std::string& filename) const
{
	const std::string world_file( FileUtils::removeExtension(filename)+".pnw" );
	if (!FileUtils::validFileAndPath(world_file)) throw InvalidNameException(world_file, AT);
	std::ofstream fout(world_file.c_str(), ios::out);
	if (fout.fail()) throw AccessException(world_file, AT);

	const double cellsize = grid_in.cellsize;
	Coords world_ref( grid_in.llcorner );
	world_ref.setProj(coordout, coordoutparam);
	world_ref.moveByXY(.5*cellsize, (double(grid_in.getNy())+.5)*cellsize); //moving to center of upper left cell

	try {
		fout << std::setprecision(12) << cellsize << "\n";
		fout << "0.000000000000\n";
		fout << "0.000000000000\n";
		fout << std::setprecision(12) << -cellsize << "\n";
		fout << std::setprecision(12) << world_ref.getEasting() << "\n";
		fout << std::setprecision(12) << world_ref.getNorthing() << "\n";
	} catch(...) {
		fout.close();
		throw AccessException("Failed when writing to PNG world file \""+world_file+"\"", AT);
	}

	fout.close();
}

void PNGIO::createMetadata(const Grid2DObject& grid)
{
	const double lat = grid.llcorner.getLat();
	const double lon = grid.llcorner.getLon();
	ostringstream ss;

	metadata_key.clear();
	metadata_text.clear();

	metadata_key.push_back("Creation Time");
	Date cr_date;
	cr_date.setFromSys();
	metadata_text.push_back( cr_date.toString(Date::ISO) );
	metadata_key.push_back("Author");
	metadata_text.push_back(IOUtils::getLogName());
	metadata_key.push_back("Software");
	metadata_text.push_back("MeteoIO "+getLibVersion());
	metadata_key.push_back("Position");
	metadata_text.push_back("llcorner");
	metadata_key.push_back("Cellsize");
	ss.str(""); ss << fixed << setprecision(2) << grid.cellsize;
	metadata_text.push_back(ss.str());
	metadata_key.push_back("LL_Latitude");
	ss.str(""); ss << fixed << setprecision(7) << lat;
	metadata_text.push_back(ss.str());
	metadata_key.push_back("LL_Longitude");
	ss.str(""); ss << fixed << setprecision(7) << lon;
	metadata_text.push_back(ss.str());
	
	Coords UR(grid.llcorner);
	UR.moveByXY( grid.cellsize*static_cast<double>(grid.getNx()) , grid.cellsize*static_cast<double>(grid.getNy()) );
	metadata_key.push_back("UR_Latitude");
	ss.str(""); ss << fixed << setprecision(7) << UR.getLat();
	metadata_text.push_back(ss.str());
	metadata_key.push_back("UR_Longitude");
	ss.str(""); ss << fixed << setprecision(7) << UR.getLon();
	metadata_text.push_back(ss.str());

	if (lat<0.) {
		metadata_key.push_back("LatitudeRef");
		metadata_text.push_back("S");
		metadata_key.push_back("GPSLatitude");
		metadata_text.push_back(decimal_to_dms(-lat));
	} else {
		metadata_key.push_back("LatitudeRef");
		metadata_text.push_back("N");
		metadata_key.push_back("GPSLatitude");
		metadata_text.push_back(decimal_to_dms(lat));
	}
	if (lon<0.) {
		metadata_key.push_back("LongitudeRef");
		metadata_text.push_back("W");
		metadata_key.push_back("GPSLongitude");
		metadata_text.push_back(decimal_to_dms(-lon));
	} else {
		metadata_key.push_back("LongitudeRef");
		metadata_text.push_back("E");
		metadata_key.push_back("GPSLongitude");
		metadata_text.push_back(decimal_to_dms(lon));
	}
}

void PNGIO::writeMetadata(png_structp &png_ptr, png_infop &info_ptr)
{
	const size_t max_len = 79; //according to the official specs' recommendation
	const size_t nr = metadata_key.size();
	png_text *info_text = (png_text *)calloc(nr, sizeof(png_text));
	char **key = (char**)calloc(nr, sizeof(char)*max_len);
	char **text = (char**)calloc(nr, sizeof(char)*max_len);

	for (size_t ii=0; ii<nr; ii++) {
		key[ii] = (char *)calloc(max_len, sizeof(char));
		text[ii] = (char *)calloc(max_len, sizeof(char));
		strncpy(key[ii], metadata_key[ii].c_str(), max_len-1); //in case the '\0' was not counted by maxlen
		key[ii][max_len-1] = '\0'; //make sure it is null terminated
		strncpy(text[ii], metadata_text[ii].c_str(), max_len-1);
		text[ii][max_len-1] = '\0'; //make sure it is null terminated
		info_text[ii].key = key[ii];
		info_text[ii].text = text[ii];
		info_text[ii].compression = PNG_TEXT_COMPRESSION_NONE;
	}

	png_set_text(png_ptr, info_ptr, info_text, static_cast<int>(nr));
	png_write_info(png_ptr, info_ptr);

	free(info_text);
	for (size_t ii=0; ii<nr; ii++) {
		free(key[ii]);
		free(text[ii]);
	}
	free(key);
	free(text);
}

std::string PNGIO::decimal_to_dms(const double& decimal) {
	const int d = static_cast<int>( floor(decimal) );
	const double m = floor( ((decimal - (double)d)*60.)*100. ) / 100.;
	const double s = 3600.*(decimal - (double)d) - 60.*m;

	std::ostringstream dms;
	dms << d << "/1 " << static_cast<int>(m*100.) << "/100 " << fixed << setprecision(6) << s << "/1";
	return dms.str();
}

} //namespace
