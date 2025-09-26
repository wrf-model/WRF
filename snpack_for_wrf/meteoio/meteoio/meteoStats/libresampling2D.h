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
#ifndef LIBRESAMPLING2D_H
#define LIBRESAMPLING2D_H

#include <meteoio/dataClasses/Grid2DObject.h> //this contains Array2D

#include <iostream>
#include <string>

namespace mio {

/**
 * @class LibResampling2D
 * @brief Spatial resampling algorithms
 *
 * @ingroup stats
 * @author Mathias Bavay
 * @date   2011-06-29
 */
class LibResampling2D {
	public:
		//Available algorithms
		static const Grid2DObject Nearest(const Grid2DObject &i_grid, const double &factor);
		static const Grid2DObject Bilinear(const Grid2DObject &i_grid, const double &factor);
		static const Grid2DObject cubicBSpline(const Grid2DObject &i_grid, const double &factor);

		static const Array2D<double> Nearest(const Array2D<double> &i_grid, const double &factor_x, const double &factor_y);
		static const Array2D<double> Bilinear(const Array2D<double> &i_grid, const double &factor_x, const double &factor_y);
		static const Array2D<double> cubicBSpline(const Array2D<double> &i_grid, const double &factor_x, const double &factor_y);

	private:
		static void cubicBSpline(Array2D<double> &o_grid, const Array2D<double> &i_grid);
		static void Bilinear(Array2D<double> &o_grid, const Array2D<double> &i_grid);
		static void Nearest(Array2D<double> &o_grid, const Array2D<double> &i_grid);

		static double bilinear_pixel(const Array2D<double> &i_grid, const size_t &org_ii, const size_t &org_jj, const size_t &org_ncols, const size_t &org_nrows, const double &x, const double &y);
		static double BSpline_weight(const double &x);
};
} //end namespace

#endif
