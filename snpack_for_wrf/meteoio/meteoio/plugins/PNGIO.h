/***********************************************************************************/
/*  Copyright 2011 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef PNGIO_H
#define PNGIO_H

#include <meteoio/IOInterface.h>
#include <meteoio/Graphics.h>

#include <string>
#include <png.h>

namespace mio {
/**
 * @class PNGIO
 * @brief This plugin write 2D grids as PNG images.
 *
 * @ingroup plugins
 * @author Mathias Bavay
 * @date   2012-02-26
 */
class PNGIO : public IOInterface {
	public:
		PNGIO(const std::string& configfile);
		PNGIO(const PNGIO&);
		PNGIO(const Config& cfgreader);
		~PNGIO() throw();

		PNGIO& operator=(const PNGIO&); ///<Assignement operator, required because of pointer member

		virtual void write2DGrid(const Grid2DObject& grid_in, const std::string& filename);
		virtual void write2DGrid(const Grid2DObject& grid_in, const MeteoGrids::Parameters& parameter, const Date& date);

	private:
		void setOptions();
		static void parse_size(const std::string& size_spec, size_t& width, size_t& height);
		double getScaleFactor(const size_t& grid_w, const size_t& grid_h) const;
		Grid2DObject scaleGrid(const Grid2DObject& grid_in) const;
		size_t setLegend(const size_t &ncols, const size_t &nrows, const double &min, const double &max, Array2D<double> &legend_array) const;
		static png_color* setPalette(const Gradient &gradient, png_structp& png_ptr, png_infop& info_ptr);
		void createMetadata(const Grid2DObject& grid);
		void writeMetadata(png_structp &png_ptr, png_infop &info_ptr);
		void setFile(const std::string& filename, png_structp& png_ptr, png_infop& info_ptr, const size_t &width, const size_t &height);
		void writeWorldFile(const Grid2DObject& grid_in, const std::string& filename) const;
		void writeDataSection(const Grid2DObject &grid, const Array2D<double> &legend_array, const Gradient &gradient, const size_t &full_width, const png_structp &png_ptr, png_infop& info_ptr);
		void closePNG(png_structp& png_ptr, png_infop& info_ptr, png_color *palette);
		static std::string decimal_to_dms(const double& decimal);

		const Config cfg;
		FILE *fp; //since passing fp always fail...
		bool autoscale;
		bool has_legend;
		bool has_world_file; ///< create world file with each file?
		bool optimize_for_speed; ///< optimize for speed instead of compression?
		bool indexed_png; ///< write an indexed png?
		unsigned char nr_levels; ///< number of levels to represent? (less-> smaller file size and faster)
		std::string coordout, coordoutparam; //projection parameters
		std::string grid2dpath;

		std::string scaling;
		size_t min_w, min_h, max_w, max_h;

		std::vector<std::string> metadata_key, metadata_text;

		static const double plugin_nodata; //plugin specific nodata value, e.g. -999
		static const unsigned char channel_depth;
		static const unsigned char channel_max_color;
		static const unsigned char transparent_grey;
};

} //namespace
#endif
