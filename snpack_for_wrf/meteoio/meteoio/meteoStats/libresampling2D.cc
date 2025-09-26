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
#include <meteoio/IOUtils.h>
#include <meteoio/MathOptim.h>
#include <meteoio/meteoStats/libresampling2D.h>
#include <cmath>
#include <sstream>
#include <algorithm>

using namespace std;

namespace mio {

const Grid2DObject LibResampling2D::Nearest(const Grid2DObject &i_grid, const double &factor)
{
	if (factor<=0) {
		ostringstream ss;
		ss << "Rescaling factor " << factor << " is invalid!";
		throw InvalidArgumentException(ss.str(), AT);
	}
	const double cellsize = i_grid.cellsize/factor;
	const size_t ncols = static_cast<size_t>(Optim::round( static_cast<double>(i_grid.getNx())*factor ));
	const size_t nrows = static_cast<size_t>(Optim::round( static_cast<double>(i_grid.getNy())*factor ));
	Grid2DObject o_grid(ncols, nrows, cellsize, i_grid.llcorner);

	Nearest(o_grid.grid2D, i_grid.grid2D); //GridObjects always keep nodata
	return o_grid;
}

/**
 * @brief Bilinear spatial data resampling
 */
const Grid2DObject LibResampling2D::Bilinear(const Grid2DObject &i_grid, const double &factor)
{
	if (factor<=0) {
		ostringstream ss;
		ss << "Rescaling factor " << factor << " is invalid!";
		throw InvalidArgumentException(ss.str(), AT);
	}
	const double cellsize = i_grid.cellsize/factor;
	const size_t ncols = static_cast<size_t>(Optim::round( static_cast<double>(i_grid.getNx())*factor ));
	const size_t nrows = static_cast<size_t>(Optim::round( static_cast<double>(i_grid.getNy())*factor ));
	Grid2DObject o_grid(ncols, nrows, cellsize, i_grid.llcorner);

	Bilinear(o_grid.grid2D, i_grid.grid2D); //GridObjects always keep nodata
	return o_grid;
}

const Grid2DObject LibResampling2D::cubicBSpline(const Grid2DObject &i_grid, const double &factor)
{
	if (factor<=0) {
		ostringstream ss;
		ss << "Rescaling factor " << factor << " is invalid!";
		throw InvalidArgumentException(ss.str(), AT);
	}
	const double cellsize = i_grid.cellsize/factor;
	const size_t ncols = static_cast<size_t>(Optim::round( static_cast<double>(i_grid.getNx())*factor ));
	const size_t nrows = static_cast<size_t>(Optim::round( static_cast<double>(i_grid.getNy())*factor ));
	Grid2DObject o_grid(ncols, nrows, cellsize, i_grid.llcorner);

	cubicBSpline(o_grid.grid2D, i_grid.grid2D); //GridObjects always keep nodata
	return o_grid;
}

const Array2D<double> LibResampling2D::Nearest(const Array2D<double> &i_grid, const double &factor_x, const double &factor_y) {
	if (factor_x<=0 || factor_y<=0) {
		ostringstream ss;
		ss << "Rescaling factors (" << factor_x << "," << factor_y << ") are invalid!";
		throw InvalidArgumentException(ss.str(), AT);
	}
	const size_t nx = static_cast<size_t>(Optim::round( static_cast<double>(i_grid.getNx())*factor_x ));
	const size_t ny = static_cast<size_t>(Optim::round( static_cast<double>(i_grid.getNy())*factor_y ));
	Array2D<double> o_grid(nx, ny);

	Nearest(o_grid, i_grid);
	return o_grid;
}

const Array2D<double> LibResampling2D::Bilinear(const Array2D<double> &i_grid, const double &factor_x, const double &factor_y) {
	if (factor_x<=0 || factor_y<=0) {
		ostringstream ss;
		ss << "Rescaling factors (" << factor_x << "," << factor_y << ") are invalid!";
		throw InvalidArgumentException(ss.str(), AT);
	}
	const size_t nx = static_cast<size_t>(Optim::round( static_cast<double>(i_grid.getNx())*factor_x ));
	const size_t ny = static_cast<size_t>(Optim::round( static_cast<double>(i_grid.getNy())*factor_y ));
	Array2D<double> o_grid(nx, ny);
	Bilinear(o_grid, i_grid);
	return o_grid;
}

const Array2D<double> LibResampling2D::cubicBSpline(const Array2D<double> &i_grid, const double &factor_x, const double &factor_y) {
	if (factor_x<=0 || factor_y<=0) {
		ostringstream ss;
		ss << "Rescaling factors (" << factor_x << "," << factor_y << ") are invalid!";
		throw InvalidArgumentException(ss.str(), AT);
	}
	const size_t nx = static_cast<size_t>(Optim::round( static_cast<double>(i_grid.getNx())*factor_x ));
	const size_t ny = static_cast<size_t>(Optim::round( static_cast<double>(i_grid.getNy())*factor_y ));
	Array2D<double> o_grid(nx, ny);

	cubicBSpline(o_grid, i_grid);
	return o_grid;
}

///////////////////////////////////////////////////////////////////////
//Private Methods
///////////////////////////////////////////////////////////////////////
void LibResampling2D::Nearest(Array2D<double> &o_grid, const Array2D<double> &i_grid)
{
	const size_t org_nx = i_grid.getNx(), org_ny = i_grid.getNy();
	const size_t dest_nx = o_grid.getNx(), dest_ny = o_grid.getNy();
	const double scale_x = (double)dest_nx / (double)org_nx;
	const double scale_y = (double)dest_ny / (double)org_ny;

	for (size_t jj=0; jj<dest_ny; jj++) {
		const size_t org_jj = std::min( (size_t) Optim::floor( (double)jj/scale_y ) , org_ny-1 );

		for (size_t ii=0; ii<dest_nx; ii++) {
			const size_t org_ii = std::min( (size_t) Optim::floor( (double)ii/scale_x ) , org_nx-1 );
			o_grid(ii,jj) = i_grid(org_ii, org_jj);
		}
	}
}

double LibResampling2D::bilinear_pixel(const Array2D<double> &i_grid, const size_t &org_ii, const size_t &org_jj, const size_t &org_nx, const size_t &org_ny, const double &x, const double &y)
{
	if (org_jj>=(org_ny-1) || org_ii>=(org_nx-1)) return i_grid(org_ii, org_jj);

	const double f_0_0 = i_grid(org_ii, org_jj);
	const double f_1_0 = i_grid(org_ii+1, org_jj);
	const double f_0_1 = i_grid(org_ii, org_jj+1);
	const double f_1_1 = i_grid(org_ii+1, org_jj+1);

	double avg_value = 0.;
	unsigned int avg_count = 0;
	if (f_0_0!=IOUtils::nodata) {
		avg_value += f_0_0;
		avg_count++;
	}
	if (f_1_0!=IOUtils::nodata) {
		avg_value += f_1_0;
		avg_count++;
	}
	if (f_0_1!=IOUtils::nodata) {
		avg_value += f_0_1;
		avg_count++;
	}
	if (f_1_1!=IOUtils::nodata) {
		avg_value += f_1_1;
		avg_count++;
	}

	if (avg_count==4) return f_0_0 * (1.-x)*(1.-y) + f_1_0 * x*(1.-y) + f_0_1 * (1.-x)*y + f_1_1 *x*y;

	//special cases: less than two neighbours or three neighbours
	if (avg_count<=2) return IOUtils::nodata;

	double value = 0.;
	const double avg = avg_value/(double)avg_count;
	if (f_0_0!=IOUtils::nodata) value += f_0_0 * (1.-x)*(1.-y);
	else value += avg * (1.-x)*(1.-y);
	if (f_1_0!=IOUtils::nodata) value += f_1_0 * x*(1.-y);
	else value += avg * x*(1.-y);
	if (f_0_1!=IOUtils::nodata) value += f_0_1 * (1.-x)*y;
	else value += avg * (1.-x)*y;
	if (f_1_1!=IOUtils::nodata) value += f_1_1 *x*y;
	else value += avg *x*y;

	return value;
}

void LibResampling2D::Bilinear(Array2D<double> &o_grid, const Array2D<double> &i_grid)
{
	const size_t org_nx = i_grid.getNx(), org_ny = i_grid.getNy();
	const size_t dest_nx = o_grid.getNx(), dest_ny = o_grid.getNy();
	const double scale_x = (double)dest_nx / (double)org_nx;
	const double scale_y = (double)dest_ny / (double)org_ny;

	for (size_t jj=0; jj<dest_ny; jj++) {
		const double org_y = (double)jj/scale_y;
		const size_t org_jj = static_cast<size_t>( org_y );
		const double y = org_y - (double)org_jj; //normalized y, between 0 and 1

		for (size_t ii=0; ii<dest_nx; ii++) {
			const double org_x = (double)ii/scale_x;
			const size_t org_ii = static_cast<size_t>( org_x );
			const double x = org_x - (double)org_ii; //normalized x, between 0 and 1

			o_grid(ii,jj) = bilinear_pixel(i_grid, org_ii, org_jj, org_nx, org_ny, x, y);
		}
	}
}

double LibResampling2D::BSpline_weight(const double &x)
{
	double R = 0.;
	if ((x+2.)>0.) R += Optim::pow3(x+2.);
	if ((x+1.)>0.) R += -4.*Optim::pow3(x+1.);
	if ((x)>0.) R += 6.*Optim::pow3(x);
	if ((x-1.)>0.) R += -4.*Optim::pow3(x-1.);

	return 1./6.*R;
}

void LibResampling2D::cubicBSpline(Array2D<double> &o_grid, const Array2D<double> &i_grid)
{//see http://paulbourke.net/texture_colour/imageprocess/
	const size_t org_nx = i_grid.getNx(), org_ny = i_grid.getNy();
	const size_t dest_nx = o_grid.getNx(), dest_ny = o_grid.getNy();
	const double scale_x = (double)dest_nx / (double)org_nx;
	const double scale_y = (double)dest_ny / (double)org_ny;

	for (size_t jj=0; jj<dest_ny; jj++) {
		const double org_y = (double)jj/scale_y;
		const size_t org_jj = static_cast<size_t>( org_y );
		const double dy = org_y - (double)org_jj; //normalized y, between 0 and 1

		for (size_t ii=0; ii<dest_nx; ii++) {
			const double org_x = (double)ii/scale_x;
			const size_t org_ii = static_cast<size_t>( org_x );
			const double dx = org_x - (double)org_ii; //normalized x, between 0 and 1

			double F = 0., max=-std::numeric_limits<double>::max(), min=std::numeric_limits<double>::max();
			unsigned int avg_count = 0;
			for (char n=-1; n<=2; n++) {
				for (char m=-1; m<=2; m++) {
					if (((signed)org_ii+m)<0 || ((signed)org_ii+m)>=(signed)org_nx || ((signed)org_jj+n)<0 || ((signed)org_jj+n)>=(signed)org_ny) continue;
					const double pixel = i_grid(static_cast<size_t>(org_ii+m), static_cast<size_t>(org_jj+n));
					if (pixel!=IOUtils::nodata) {
						F += pixel * BSpline_weight(m-dx) * BSpline_weight(dy-n);
						avg_count++;
						if (pixel>max) max=pixel;
						if (pixel<min) min=pixel;
					}
				}
			}

			if (avg_count==16) { //normal bicubic
				o_grid(ii,jj) = F;
				if (o_grid(ii,jj)>max) o_grid(ii,jj)=max; //try to limit overshoot
				else if (o_grid(ii,jj)<min) o_grid(ii,jj)=min; //try to limit overshoot
			} else if (avg_count==0) o_grid(ii,jj) = IOUtils::nodata; //nodata-> nodata
			else //not enought data points -> bilinear for this pixel
				o_grid(ii,jj) = bilinear_pixel(i_grid, org_ii, org_jj, org_nx, org_ny, dx, dy);
		}
	}
}

} //namespace

