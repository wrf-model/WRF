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
#include <cmath>
#include <limits.h>
#include <algorithm>

#include <meteoio/dataClasses/DEMObject.h>
#include <meteoio/MathOptim.h>
#include <meteoio/IOUtils.h>
#include <meteoio/meteoLaws/Meteoconst.h> //for math constants

/**
* @file DEMObject.cc
* @brief implementation of the DEMBoject class
*/

using namespace std;

namespace mio {

/**
* @brief Default constructor.
* Initializes all variables to 0, except lat/long which are initialized to IOUtils::nodata
* @param i_algorithm specify the default algorithm to use for slope computation (default=DFLT)
*/
DEMObject::DEMObject(const slope_type& i_algorithm)
           : Grid2DObject(), slope(), azi(), curvature(), Nx(), Ny(), Nz(),
             min_altitude(Cst::dbl_max), min_slope(Cst::dbl_max), min_curvature(Cst::dbl_max),
             max_altitude(Cst::dbl_min), max_slope(Cst::dbl_min), max_curvature(Cst::dbl_min),
             CalculateSlope(&DEMObject::CalculateCorripio),
             max_shade_distance(IOUtils::nodata), update_flag(UPDATE_UNSET), dflt_algorithm(i_algorithm),
             slope_failures(0), curvature_failures(0)
{
	setDefaultAlgorithm(i_algorithm);
}

/**
* @brief Constructor that sets a constant DEM. 
* @details The resulting DEM will have the same elevation everywhere, constant slope (flat) and curvature (flat).
* @param ncols_in number of colums in the grid2D
* @param nrows_in number of rows in the grid2D
* @param llcorner_in lower lower corner point
* @param init initial value to fill the DEM with
*/
DEMObject::DEMObject(const size_t& ncols_in, const size_t& nrows_in, const Coords& llcorner_in, const double& init)
           : Grid2DObject(ncols_in, nrows_in, 10., llcorner_in, init), 
             slope(ncols_in, nrows_in, 0.), azi(ncols_in, nrows_in, 0.), curvature(ncols_in, nrows_in, 0.), 
             Nx(ncols_in, nrows_in, 0.), Ny(ncols_in, nrows_in, 0.), Nz(ncols_in, nrows_in, 1.),
             min_altitude(init), min_slope(0.), min_curvature(0.),
             max_altitude(init), max_slope(0.), max_curvature(0.),
             CalculateSlope(&DEMObject::CalculateCorripio),
             max_shade_distance(IOUtils::nodata), update_flag(UPDATE_UNSET), dflt_algorithm(DFLT),
             slope_failures(0), curvature_failures(0)
{}

/**
* @brief Constructor that sets variables.
* @param i_ncols number of colums in the grid2D
* @param i_nrows number of rows in the grid2D
* @param i_cellsize value for cellsize in grid2D
* @param i_llcorner lower lower corner point
* @param i_algorithm specify the default algorithm to use for slope computation (default=DFLT)
*/
DEMObject::DEMObject(const size_t& i_ncols, const size_t& i_nrows,
                     const double& i_cellsize, const Coords& i_llcorner, const slope_type& i_algorithm)
           : Grid2DObject(i_ncols, i_nrows, i_cellsize, i_llcorner),
             slope(), azi(), curvature(), Nx(), Ny(), Nz(),
             min_altitude(Cst::dbl_max), min_slope(Cst::dbl_max), min_curvature(Cst::dbl_max),
             max_altitude(Cst::dbl_min), max_slope(Cst::dbl_min), max_curvature(Cst::dbl_min),
             CalculateSlope(&DEMObject::CalculateCorripio),
             max_shade_distance(IOUtils::nodata), update_flag(UPDATE_UNSET), dflt_algorithm(i_algorithm),
             slope_failures(0), curvature_failures(0)
{
	setDefaultAlgorithm(i_algorithm);
}

/**
* @brief Constructor that sets variables.
* @param i_cellsize value for cellsize in grid2D
* @param i_llcorner lower lower corner point
* @param i_altitude grid2D of elevations
* @param i_update also update slope/normals/curvatures and their min/max? (default=true)
* @param i_algorithm specify the default algorithm to use for slope computation (default=DFLT)
*/
DEMObject::DEMObject(const double& i_cellsize, const Coords& i_llcorner, const Array2D<double>& i_altitude,
                     const bool& i_update, const slope_type& i_algorithm)
           : Grid2DObject(i_cellsize, i_llcorner, i_altitude),
             slope(), azi(), curvature(), Nx(), Ny(), Nz(),
             min_altitude(Cst::dbl_max), min_slope(Cst::dbl_max), min_curvature(Cst::dbl_max),
             max_altitude(Cst::dbl_min), max_slope(Cst::dbl_min), max_curvature(Cst::dbl_min),
             CalculateSlope(&DEMObject::CalculateCorripio),
             max_shade_distance(IOUtils::nodata), update_flag(UPDATE_UNSET), dflt_algorithm(i_algorithm),
             slope_failures(0), curvature_failures(0)
{
	setDefaultAlgorithm(i_algorithm);
	if (i_update==false) {
		updateAllMinMax();
	} else {
		update(i_algorithm);
	}
}

/**
* @brief Constructor that sets variables from a Grid2DObject
* @param i_dem grid contained in a Grid2DObject
* @param i_update also update slope/normals/curvatures and their min/max? (default=true)
* @param i_algorithm specify the default algorithm to use for slope computation (default=DFLT)
*/
DEMObject::DEMObject(const Grid2DObject& i_dem, const bool& i_update, const slope_type& i_algorithm)
           : Grid2DObject(i_dem),
             slope(), azi(), curvature(), Nx(), Ny(), Nz(),
             min_altitude(Cst::dbl_max), min_slope(Cst::dbl_max), min_curvature(Cst::dbl_max),
             max_altitude(Cst::dbl_min), max_slope(Cst::dbl_min), max_curvature(Cst::dbl_min),
             CalculateSlope(&DEMObject::CalculateCorripio),
             max_shade_distance(IOUtils::nodata), update_flag(UPDATE_UNSET), dflt_algorithm(i_algorithm),
             slope_failures(0), curvature_failures(0)
{
	setDefaultAlgorithm(i_algorithm);
	if (i_update==false) {
		updateAllMinMax();
	} else {
		update(i_algorithm);
	}
}

/**
* @brief Constructor that sets variables from a subset of another DEMObject,
* given an origin (X,Y) (first index being 0) and a number of columns and rows
* @param i_dem dem contained in a DEMDObject
* @param i_nx X coordinate of the new origin
* @param i_ny Y coordinate of the new origin
* @param i_ncols number of columns for the subset dem
* @param i_nrows number of rows for the subset dem
* @param i_update also update slope/normals/curvatures and their min/max? (default=true)
* @param i_algorithm specify the default algorithm to use for slope computation (default=DFLT)
*/
DEMObject::DEMObject(const DEMObject& i_dem, const size_t& i_nx, const size_t& i_ny,
                     const size_t& i_ncols, const size_t& i_nrows,
                     const bool& i_update, const slope_type& i_algorithm)
           : Grid2DObject(i_dem, i_nx,i_ny, i_ncols,i_nrows),
             slope(), azi(), curvature(), Nx(), Ny(), Nz(),
             min_altitude(Cst::dbl_max), min_slope(Cst::dbl_max), min_curvature(Cst::dbl_max),
             max_altitude(Cst::dbl_min), max_slope(Cst::dbl_min), max_curvature(Cst::dbl_min),
             CalculateSlope(&DEMObject::CalculateCorripio),
             max_shade_distance(IOUtils::nodata), update_flag(i_dem.update_flag), dflt_algorithm(i_algorithm),
             slope_failures(0), curvature_failures(0)
{
	if ((i_ncols==0) || (i_nrows==0)) {
		throw InvalidArgumentException("requesting a subset of 0 columns or rows for DEMObject", AT);
	}

	//handling of the update properties
	setDefaultAlgorithm(i_algorithm);
	if (i_update==true) {
		//if the object is in automatic update, then we only process the arrays according to
		//the update_flag
		update(i_algorithm);
	} else {
		//if the object is NOT in automatic update, we manually copy all non-empty arrays
		//from the original set
		size_t nx, ny;

		i_dem.slope.size(nx, ny);
		if (nx>0 && ny>0) {
			slope.subset(i_dem.slope,i_nx,i_ny, i_ncols,i_nrows);
		}
		i_dem.azi.size(nx, ny);
		if (nx>0 && ny>0) {
			azi.subset(i_dem.azi,i_nx,i_ny, i_ncols,i_nrows);
		}
		i_dem.curvature.size(nx, ny);
		if (nx>0 && ny>0) {
			curvature.subset(i_dem.curvature,i_nx,i_ny, i_ncols,i_nrows);
		}
		i_dem.Nx.size(nx, ny);
		if (nx>0 && ny>0) {
			Nx.subset(i_dem.Nx,i_nx,i_ny, i_ncols,i_nrows);
		}
		i_dem.Ny.size(nx, ny);
		if (nx>0 && ny>0) {
			Ny.subset(i_dem.Ny,i_nx,i_ny, i_ncols,i_nrows);
		}
		i_dem.Nz.size(nx, ny);
		if (nx>0 && ny>0) {
			Nz.subset(i_dem.Nz,i_nx,i_ny, i_ncols,i_nrows);
		}

		updateAllMinMax();
	}
}

/**
* @brief Set the properties that will be calculated by the object when updating
* The following properties can be turned on/off: slope/azimuth and/or normals, and/or curvatures.
* Flags are combined using the binary "|" operator.
* @param in_update_flag parameters to update
*/
void DEMObject::setUpdatePpt(const update_type& in_update_flag) {
	update_flag = in_update_flag;
}

/**
* @brief Get the properties that will be calculated by the object when updating
* @return combination of flags set with the binary "|" operator
*/
int DEMObject::getUpdatePpt() const {
	return update_flag;
}

/**
* @brief Force the computation of the local slope, azimuth, normal vector and curvature.
* It has to be called manually since it can require some time to compute. Without this call,
* the above mentionned parameters are NOT up to date.
* @param algorithm algorithm to use for computing slope, azimuth and normals
*/
void DEMObject::update(const slope_type& algorithm) {
//This method recomputes the attributes that are not read as parameters
//(such as slope, azimuth, normal vector)

	// Creating tables
	if (update_flag&SLOPE) {
		slope.resize(getNx(), getNy());
		azi.resize(getNx(), getNy());
	}
	if (update_flag&CURVATURE) {
		curvature.resize(getNx(), getNy());
	}
	if (update_flag&NORMAL) {
		Nx.resize(getNx(), getNy());
		Ny.resize(getNx(), getNy());
		Nz.resize(getNx(), getNy());
	}

	CalculateAziSlopeCurve(algorithm);
	updateAllMinMax();
}

/**
* @brief Force the computation of the local slope, azimuth, normal vector and curvature.
* It has to be called manually since it can require some time to compute. Without this call,
* the above mentionned parameters are NOT up to date.
* @param algorithm algorithm to use for computing slope, azimuth and normals
* it is either:
* - HICK that uses the maximum downhill slope method (Dunn and Hickey, 1998)
* - FLEMING uses a 4 neighbors algorithm (Fleming and Hoffer, 1979)
* - CORRIPIO that uses the surface normal vector using the two triangle method given in Corripio (2002)
* and the eight-neighbor algorithm of Horn (1981) for border cells.
* - D8 uses CORRIPIO but discretizes the resulting azimuth to 8 cardinal directions and the slope is rounded to the nearest degree. Curvature and normals are left untouched.
*
* The azimuth is always computed using the Hodgson (1998) algorithm.
*/
void DEMObject::update(const std::string& algorithm) {
//This method recomputes the attributes that are not read as parameters
//(such as slope, azimuth, normal vector)
	slope_type type;

	if (algorithm.compare("HICK")==0) {
		type=HICK;
	} else if (algorithm.compare("FLEMING")==0) {
		type=FLEM;
	} else if (algorithm.compare("HORN")==0) {
		type=HORN;
	} else if (algorithm.compare("CORRIPIO")==0) {
		type=CORR;
	} else if (algorithm.compare("D8")==0) {
		type=D8;
	} else if (algorithm.compare("DEFAULT")==0) {
		type=DFLT;
	} else {
		throw InvalidArgumentException("Chosen slope algorithm " + algorithm + " not available", AT);
	}

	update(type);
}

/**
* @brief Sets the default slope calculation algorithm
* @param algorithm specify the default algorithm to use for slope computation
*/
void DEMObject::setDefaultAlgorithm(const std::string& algorithm) {
	slope_type type;

	if (algorithm.compare("HICK")==0) {
		type=HICK;
	} else if (algorithm.compare("FLEMING")==0) {
		type=FLEM;
	} else if (algorithm.compare("HORN")==0) {
		type=HORN;
	} else if (algorithm.compare("CORRIPIO")==0) {
		type=CORR;
	} else if (algorithm.compare("D8")==0) {
		type=D8;
	} else {
		throw InvalidArgumentException("Chosen slope algorithm " + algorithm + " not available", AT);
	}
	
	dflt_algorithm = type;
}

/**
* @brief Sets the default slope calculation algorithm
* @param i_algorithm specify the default algorithm to use for slope computation
*/
void DEMObject::setDefaultAlgorithm(const slope_type& i_algorithm) {
//This method MUST be called by each constructor!
	if (i_algorithm==DFLT) {
		dflt_algorithm = CORR;
	} else {
		dflt_algorithm = i_algorithm;
	}
}

/**
* @brief Get the default slope calculation algorithm
* @return default algorithm to use for slope computation
*/
int DEMObject::getDefaultAlgorithm() const {
	return dflt_algorithm;
}

/**
* @brief Recomputes the min/max of altitude, slope and curvature
* It returns +/- std::numeric_limits\<double\> \::max() for a given parameter if its grid was empty/undefined
*/
void DEMObject::updateAllMinMax() {
//updates the min/max parameters of all 2D tables
	if (update_flag&SLOPE) {
		min_slope = slope.getMin();
		max_slope = slope.getMax();
	}
	if (update_flag&CURVATURE) {
		min_curvature = curvature.getMin();
		max_curvature = curvature.getMax();
	}

	min_altitude = grid2D.getMin();
	max_altitude = grid2D.getMax();
}

/**
* @brief Prints the list of points that have an elevation different than nodata but no slope or curvature
* Such points can happen if they are surrounded by too many points whose elevation is nodata
* If no such points exist, it prints nothing.
*/
void DEMObject::printFailures() {
	bool header=true;
	const size_t ncols = getNx();
	const size_t nrows = getNy();

	if (update_flag&SLOPE) {
		for ( size_t j = 0; j < nrows; j++ ) {
			for ( size_t i = 0; i < ncols; i++ ) {
				if ((slope(i,j)==IOUtils::nodata) && (grid2D(i,j)!=IOUtils::nodata)) {
					if (header==true) {
						cerr << "[i] DEM slope could not be computed at the following points \n";
						cerr << "[i]\tGrid Point\tElevation\tSlope\n";
						header=false;
					}
					cerr << "[i]\t(" << i << "," << j << ")" << "\t\t" << grid2D(i,j) << "\t\t" << slope(i,j) << "\n";
				}
			}
		}
	}

	if (update_flag&CURVATURE) {
		for ( size_t j = 0; j < nrows; j++ ) {
			for ( size_t i = 0; i < ncols; i++ ) {
				if ((curvature(i,j)==IOUtils::nodata) && (grid2D(i,j)!=IOUtils::nodata)) {
					if (header==true) {
						cerr << "[i] DEM curvature could not be computed at the following points \n";
						cerr << "[i]\tGrid Point\tElevation\tCurvature\n";
						header=false;
					}
					cerr << "[i]\t(" << i << "," << j << ")" << "\t\t" << grid2D(i,j) << "\t\t" <<  curvature(i,j) << "\n";
				}
			}
		}
	}
	if (header==false) {
		cerr << std::endl;
	}
}

/**
* @brief Clean up the DEM Object
* When computing the slope and curvature, it is possible to get points where the elevation is known
* but where no slope/azimuth/normals/curvature could be computed. This method sets the elevation to nodata for such points,
* so that latter use of the DEM would be simpler (simply test the elevation in order to know if the point can be used
* and it guarantees that all other information is available).
*
* @note The update flags are used in order to know which properties should be checked. So if update_flag==SLOPE
* but there are curvatures and they are later used, they won't have been cleaned!
*
* @note IMPORTANT: calling this method DOES change the table of elevations!
*/
void DEMObject::sanitize() {
	const size_t ncols = getNx();
	const size_t nrows = getNy();

	for ( size_t jj = 0; jj < nrows; jj++ ) {
		for ( size_t ii = 0; ii < ncols; ii++ ) {
			if (grid2D(ii,jj)==IOUtils::nodata) continue;

			if (update_flag&SLOPE && slope(ii,jj)==IOUtils::nodata) {
				grid2D(ii,jj) = IOUtils::nodata;
			}
			if (update_flag&CURVATURE && curvature(ii,jj)==IOUtils::nodata) {
				grid2D(ii,jj) = IOUtils::nodata;
			}
		}
	}
}

void DEMObject::CalculateAziSlopeCurve(slope_type algorithm) {
//This computes the slope and the aspect at a given cell as well as the x and y components of the normal vector
	double A[4][4]; //table to store neigbouring heights: 3x3 matrix but we want to start at [1][1]
	                //we use matrix notation: A[y][x]
	if (algorithm==DFLT) {
		algorithm = dflt_algorithm;
	}

	slope_failures = curvature_failures = 0;
	if (algorithm==HICK) {
		CalculateSlope = &DEMObject::CalculateHick;
	} else if (algorithm==HORN) {
		CalculateSlope = &DEMObject::CalculateHorn;
	} else if (algorithm==CORR) {
		CalculateSlope = &DEMObject::CalculateCorripio;
	} else if (algorithm==FLEM) {
		CalculateSlope = &DEMObject::CalculateFleming;
	} else if (algorithm==D8) {
		CalculateSlope = &DEMObject::CalculateHick;
	} else {
		throw InvalidArgumentException("Chosen slope algorithm not available", AT);
	}

	//Now, calculate the parameters using the previously defined function pointer
	const size_t ncols = getNx();
	const size_t nrows = getNy();
	for ( size_t j = 0; j < nrows; j++ ) {
		for ( size_t i = 0; i < ncols; i++ ) {
			if ( grid2D(i,j) == IOUtils::nodata ) {
				if (update_flag&SLOPE) {
					slope(i,j) = azi(i,j) = IOUtils::nodata;
				}
				if (update_flag&CURVATURE) {
					curvature(i,j) = IOUtils::nodata;
				}
				if (update_flag&NORMAL) {
					Nx(i,j) = Ny(i,j) = Nz(i,j) = IOUtils::nodata;
				}
			} else {
				getNeighbours(i, j, A);
				double new_slope, new_Nx, new_Ny, new_Nz;
				(this->*CalculateSlope)(A, new_slope, new_Nx, new_Ny, new_Nz);
				const double new_azi = CalculateAspect(new_Nx, new_Ny, new_Nz, new_slope);
				const double new_curvature = getCurvature(A);
				if (update_flag&SLOPE) {
					slope(i,j) = new_slope;
					azi(i,j) = new_azi;
				}
				if (update_flag&CURVATURE) {
					curvature(i,j) = new_curvature;
				}
				if (update_flag&NORMAL) {
					Nx(i,j) = new_Nx;
					Ny(i,j) = new_Ny;
					Nz(i,j) = new_Nz;
				}
			}
		}
	}

	if ((update_flag&SLOPE) && (algorithm==D8)) { //extra processing required: discretization
		for ( size_t j = 0; j < nrows; j++ ) {
			for ( size_t i = 0; i < ncols; i++ ) {
					//TODO: process flats by an extra algorithm
					if (azi(i,j)!=IOUtils::nodata)
						azi(i,j) = fmod(floor( (azi(i,j)+22.5)/45. )*45., 360.);
					if (slope(i,j)!=IOUtils::nodata)
						slope(i,j) = floor( slope(i,j)+0.5 );
			}
		}
	}

	//Inform the user is some points have unexpectidly not been computed
	//(ie: there was an altitude but some parameters could not be computed)
	if (slope_failures>0 || curvature_failures>0) {
		cerr << "[W] DEMObject: " << slope_failures << " point(s) have an elevation but no slope, " << curvature_failures << " point(s) have an elevation but no curvature." << std::endl;
	}

} // end of CalculateAziSlope

double DEMObject::CalculateAspect(const double& o_Nx, const double& o_Ny, const double& o_Nz, const double& o_slope, const double no_slope) {
//Calculates the aspect at a given point knowing its normal vector and slope
//(direction of the normal pointing out of the surface, clockwise from north)
//This azimuth calculation is similar to Hodgson (1998)
//local_nodata is the value that we want to give to the aspect of points that don't have a slope
//The value is a bearing (ie: deg, clockwise, 0=North)

	if (o_Nx==IOUtils::nodata || o_Ny==IOUtils::nodata || o_Nz==IOUtils::nodata || o_slope==IOUtils::nodata) {
		return IOUtils::nodata;
	}

	if ( o_slope > 0. ) { //there is some slope
		if ( o_Nx == 0. ) { //no E-W slope, so it is purely N-S
			const double aspect = (o_Ny < 0.)? 180. : 0.;
			return aspect;
		} else { //there is a E-W slope
			const double angle_deg = static_cast<double>(Optim::round( atan2(o_Ny, o_Nx)*Cst::to_deg * 10.)) / 10.; //round angle to 0.1
			const double aspect = fmod( 90. - angle_deg + 360., 360.); //convert angle to bearing
			return aspect;
		}
	} else { // if slope = 0
		return (no_slope);          // undefined or plain surface
	}
}


void DEMObject::CalculateHick(double A[4][4], double& o_slope, double& o_Nx, double& o_Ny, double& o_Nz) {
//This calculates the surface normal vector using the steepest slope method (Dunn and Hickey, 1998):
//the steepest slope found in the eight cells surrounding (i,j) is given to be the slope in (i,j)
//Beware, sudden steps could happen
	const double smax = steepestGradient(cellsize, A); //steepest local gradient

	if (smax==IOUtils::nodata) {
		o_slope = IOUtils::nodata;
		o_Nx = IOUtils::nodata;
		o_Ny = IOUtils::nodata;
		o_Nz = IOUtils::nodata;
		slope_failures++;
	} else {
		o_slope = atan(smax)*Cst::to_deg;

		//Nx and Ny: x and y components of the normal pointing OUT of the surface
		if ( smax > 0. ) { //ie: there is some slope
			double dx_sum, dy_sum;
			surfaceGradient(dx_sum, dy_sum, A);
			if (dx_sum==IOUtils::nodata || dy_sum==IOUtils::nodata) {
				o_slope = IOUtils::nodata;
				o_Nx = IOUtils::nodata;
				o_Ny = IOUtils::nodata;
				o_Nz = IOUtils::nodata;
				slope_failures++;
			} else {
				o_Nx = -1.0 * dx_sum / (2. * cellsize);	//Nx=-dz/dx
				o_Ny = -1.0 * dy_sum / (2. * cellsize);	//Ny=-dz/dy
				o_Nz = 1.;				//Nz=1 (normalized by definition of Nx and Ny)
			}
		} else { //ie: there is no slope
			o_Nx = 0.;
			o_Ny = 0.;
			o_Nz = 1.;
		}
	}
}

void DEMObject::CalculateFleming(double A[4][4], double& o_slope, double& o_Nx, double& o_Ny, double& o_Nz) {
//This calculates the surface normal vector using method by Fleming and Hoffer (1979)
	if (A[2][1]!=IOUtils::nodata && A[2][3]!=IOUtils::nodata && A[3][2]!=IOUtils::nodata && A[1][2]!=IOUtils::nodata) {
		o_Nx = 0.5 * (A[2][1] - A[2][3]) / cellsize;
		o_Ny = 0.5 * (A[3][2] - A[1][2]) / cellsize;
		o_Nz = 1.;
		o_slope = atan( sqrt(o_Nx*o_Nx+o_Ny*o_Ny) ) * Cst::to_deg;
	} else {
		CalculateHick(A, o_slope, o_Nx, o_Ny, o_Nz);
	}
}

void DEMObject::CalculateHorn(double A[4][4], double& o_slope, double& o_Nx, double& o_Ny, double& o_Nz) {
//This calculates the slope using the two eight neighbors method given in Horn (1981)
//This is also the algorithm used by ArcGIS
	if ( A[1][1]!=IOUtils::nodata && A[1][2]!=IOUtils::nodata && A[1][3]!=IOUtils::nodata &&
	     A[2][1]!=IOUtils::nodata && A[2][2]!=IOUtils::nodata && A[2][3]!=IOUtils::nodata &&
	     A[3][1]!=IOUtils::nodata && A[3][2]!=IOUtils::nodata && A[3][3]!=IOUtils::nodata) {
		o_Nx = ((A[1][1]+2*A[2][1]+A[3][1]) - (A[1][3]+2*A[2][3]+A[3][3])) / (8.*cellsize);
		o_Ny = ((A[3][3]+2*A[3][2]+A[3][1]) - (A[1][3]+2*A[1][2]+A[1][1])) / (8.*cellsize);
		o_Nz = 1.;

		//There is no difference between slope = acos(n_z/|n|) and slope = atan(sqrt(sx*sx+sy*sy))
		//slope = acos( (Nz / sqrt( Nx*Nx + Ny*Ny + Nz*Nz )) );
		o_slope = atan( sqrt(o_Nx*o_Nx+o_Ny*o_Ny) ) * Cst::to_deg;
	} else {
		//steepest slope method (Dunn and Hickey, 1998)
		CalculateHick(A, o_slope, o_Nx, o_Ny, o_Nz);
	}
}

void DEMObject::CalculateCorripio(double A[4][4], double& o_slope, double& o_Nx, double& o_Ny, double& o_Nz) {
//This calculates the surface normal vector using the two triangle method given in Corripio (2003) but cell centered instead of node centered (ie using a 3x3 grid instead of 2x2)
	if ( A[1][1]!=IOUtils::nodata && A[1][3]!=IOUtils::nodata && A[3][1]!=IOUtils::nodata && A[3][3]!=IOUtils::nodata) {
		// See Corripio (2003), knowing that here we normalize the result (divided by Nz=cellsize*cellsize) and that we are cell centered instead of node centered
		o_Nx = (A[3][1] + A[1][1] - A[3][3] - A[1][3]) / (2.*2.*cellsize);
		o_Ny = (A[3][1] - A[1][1] + A[3][3] - A[1][3]) / (2.*2.*cellsize);
		o_Nz = 1.;
		//There is no difference between slope = acos(n_z/|n|) and slope = atan(sqrt(sx*sx+sy*sy))
		//slope = acos( (Nz / sqrt( Nx*Nx + Ny*Ny + Nz*Nz )) );
		o_slope = atan( sqrt(o_Nx*o_Nx+o_Ny*o_Ny) ) * Cst::to_deg;
	} else {
		//steepest slope method (Dunn and Hickey, 1998)
		CalculateHick(A, o_slope, o_Nx, o_Ny, o_Nz);
	}
}

double DEMObject::getCurvature(double A[4][4]) 
{ //This methode computes the curvature of a specific cell
	if (A[2][2]!=IOUtils::nodata) {
		const double Zwe   = avgHeight(A[2][1], A[2][2], A[2][3]);
		const double Zsn   = avgHeight(A[1][2], A[2][2], A[3][2]);
		const double Zswne = avgHeight(A[3][1], A[2][2], A[1][3]);
		const double Znwse = avgHeight(A[1][1], A[2][2], A[3][3]);

		static const double sqrt2 = sqrt(2.);
		double sum=0.;
		size_t count=0;

		if (Zwe!=IOUtils::nodata) {
			sum += 0.5*(A[2][2]-Zwe);
			count++;
		}
		if (Zsn!=IOUtils::nodata) {
			sum += 0.5*(A[2][2]-Zsn);
			count++;
		}
		if (Zswne!=IOUtils::nodata) {
			sum += 0.5*(A[2][2]-Zswne)/sqrt2;
			count++;
		}
		if (Znwse!=IOUtils::nodata) {
			sum += 0.5*(A[2][2]-Znwse)/sqrt2;
			count++;
		}

		if (count != 0) return 1./(double)count * sum;
	}
	curvature_failures++;
	return IOUtils::nodata;
}

double DEMObject::steepestGradient(const double& i_cellsize, double A[4][4]) 
{ //best effort to calculate the local steepest gradient
	double smax=-1.;		//maximum slope of all neighboring slopes
	static const double sqrt2 = sqrt(2.);	//the weight of the 4 corner cells is increased by sqrt(2)

	if (A[2][2]!=IOUtils::nodata) {
		if (A[1][1]!=IOUtils::nodata)
			smax = max( smax, fabs(A[2][2] - A[1][1])/(i_cellsize*sqrt2) );
		if (A[1][2]!=IOUtils::nodata)
			smax = max( smax, fabs(A[2][2] - A[1][2])/(i_cellsize) );
		if (A[1][3]!=IOUtils::nodata)
			smax = max( smax, fabs(A[2][2] - A[1][3])/(i_cellsize*sqrt2) );
		if (A[2][1]!=IOUtils::nodata)
			smax = max( smax, fabs(A[2][2] - A[2][1])/(i_cellsize) );
		if (A[2][3]!=IOUtils::nodata)
			smax = max( smax, fabs(A[2][2] - A[2][3])/(i_cellsize) );
		if (A[3][1]!=IOUtils::nodata)
			smax = max( smax, fabs(A[2][2] - A[3][1])/(i_cellsize*sqrt2) );
		if (A[3][2]!=IOUtils::nodata)
			smax = max( smax, fabs(A[2][2] - A[3][2])/(i_cellsize) );
		if (A[3][3]!=IOUtils::nodata)
			smax = max( smax, fabs(A[2][2] - A[3][3])/(i_cellsize*sqrt2) );
	}

	if (smax<0.)
		return IOUtils::nodata;
	return smax;
}

double DEMObject::lineGradient(const double& A1, const double& A2, const double& A3) 
{ //best effort to calculate the local gradient
	if (A3!=IOUtils::nodata && A1!=IOUtils::nodata) {
		return A3 - A1;
	} else {
		if (A2!=IOUtils::nodata) {
			if (A3!=IOUtils::nodata)
				return (A3 - A2)*2.;
			if (A1!=IOUtils::nodata)
				return (A2 - A1)*2.;
		}
	}

	return IOUtils::nodata;
}

double DEMObject::fillMissingGradient(const double& delta1, const double& delta2) {
//If a gradient could not be computed, try to fill it with some neighboring value
	if (delta1!=IOUtils::nodata && delta2!=IOUtils::nodata) {
		return 0.5*(delta1+delta2);
	} else {
		if (delta1!=IOUtils::nodata) return delta1;
		if (delta2!=IOUtils::nodata) return delta2;
	}

	return IOUtils::nodata;
}

void DEMObject::surfaceGradient(double& dx_sum, double& dy_sum, double A[4][4]) {
//Compute the gradient for a given cell (i,j) accross its eight surrounding cells (Horn, 1981)
	double dx1 = lineGradient(A[3][1], A[3][2], A[3][3]);
	double dx2 = lineGradient(A[2][1], A[2][2], A[2][3]);
	double dx3 = lineGradient(A[1][1], A[1][2], A[1][3]);

	double dy1 = lineGradient(A[3][1], A[2][1], A[1][1]);
	double dy2 = lineGradient(A[3][2], A[2][2], A[1][2]);
	double dy3 = lineGradient(A[3][3], A[2][3], A[1][3]);

	//now trying to fill whatever could not be filled...
	if (dx1==IOUtils::nodata) dx1 = fillMissingGradient(dx2, dx3);
	if (dx2==IOUtils::nodata) dx2 = fillMissingGradient(dx1, dx3);
	if (dx3==IOUtils::nodata) dx3 = fillMissingGradient(dx1, dx2);
	if (dy1==IOUtils::nodata) dy1 = fillMissingGradient(dy2, dy3);
	if (dy2==IOUtils::nodata) dy2 = fillMissingGradient(dy1, dy3);
	if (dy3==IOUtils::nodata) dy3 = fillMissingGradient(dy1, dy2);

	if (dx1!=IOUtils::nodata && dy1!=IOUtils::nodata) {
		// principal axis twice to emphasize height difference in that direction
		dx_sum = (dx1 + 2.*dx2 + dx3) * 0.25;
		dy_sum = (dy1 + 2.*dy2 + dy3) * 0.25;
	} else {
		//if dx1==nodata, this also means that dx2==nodata and dx3==nodata
		//(otherwise, dx1 would have received a copy of either dx2 or dx3)
		dx_sum = IOUtils::nodata;
		dy_sum = IOUtils::nodata;
	}
}

double DEMObject::avgHeight(const double& z1, const double &z2, const double& z3) {
//this safely computes the average height accross a vector

	if (z1!=IOUtils::nodata && z3!=IOUtils::nodata) {
		return 0.5*(z1+z3);
	}
	if (z1!=IOUtils::nodata && z2!=IOUtils::nodata) {
		return 0.5*(z1+z2);
	}
	if (z3!=IOUtils::nodata && z2!=IOUtils::nodata) {
		return 0.5*(z3+z2);
	}

	return IOUtils::nodata;
}

void DEMObject::getNeighbours(const size_t& i, const size_t& j, double A[4][4]) const 
{ //this fills a 3x3 table containing the neighboring values
	if ((i>0 && i<(getNx()-1)) && (j>0 && j<(getNy()-1))) {
		//this is the normal case
		A[1][1] = grid2D(i-1, j+1);
		A[1][2] = grid2D(i, j+1);
		A[1][3] = grid2D(i+1, j+1);
		A[2][1] = grid2D(i-1, j);
		A[2][2] = grid2D(i, j);
		A[2][3] = grid2D(i+1, j);
		A[3][1] = grid2D(i-1, j-1);
		A[3][2] = grid2D(i, j-1);
		A[3][3] = grid2D(i+1, j-1);
	} else { //somewhere on the border
		A[1][1] = safeGet((signed)i-1, (signed)j+1);
		A[1][2] = safeGet((signed)i, (signed)j+1);
		A[1][3] = safeGet((signed)i+1, (signed)j+1);
		A[2][1] = safeGet((signed)i-1, (signed)j);
		A[2][2] = safeGet((signed)i, (signed)j);
		A[2][3] = safeGet((signed)i+1, (signed)j);
		A[3][1] = safeGet((signed)i-1, (signed)j-1);
		A[3][2] = safeGet((signed)i, (signed)j-1);
		A[3][3] = safeGet((signed)i+1, (signed)j-1);
	}
}

double DEMObject::safeGet(const int& i, const int& j) const
{//this function would allow reading the value of *any* point,
//that is, even for coordinates outside of the grid (where it would return nodata)
//this is to make implementing the slope/curvature computation easier for edges, holes, etc

	if (i<0 || i>=(signed)getNx()) {
		return IOUtils::nodata;
	}
	if (j<0 || j>=(signed)getNy()) {
		return IOUtils::nodata;
	}

	return grid2D((unsigned)i, (unsigned)j);
}

const std::string DEMObject::toString(const FORMATS& type) const
{
	std::ostringstream os;

	switch(type) {
		case(FULL):
		{
			os << "<DEMObject>\n";
			os << llcorner.toString();
			os << grid2D.getNx() << " x " << grid2D.getNy() << " @ " << cellsize << "m\t[ " << min_altitude << " - " << max_altitude << " ]\n";
			//os << grid2D.toString();
			os << "Slope: " << slope.getNx() << " x " << slope.getNy() << "\t[ " << min_slope << " - " << max_slope << " ]\n";
			os << "Azi: " << azi.getNx() << " x " << azi.getNy() << "\n";
			os << "Curvature: " << curvature.getNx() << " x " << curvature.getNy()<< "\t[ " << min_curvature << " - " << max_curvature << " ]\n";
			os << "Nx: " << Nx.getNx() << " x " << Nx.getNy() << " , Ny: " << Ny.getNx() << " x " << Ny.getNy() << " , Nz: " << Nz.getNx() << " x " << Nz.getNy() << "\n";
			os << "</DEMObject>\n";
			break;
		}
		case(SHORT):
		{
			Coords urcorner(llcorner);
			urcorner.moveByXY(static_cast<double>(grid2D.getNx())*cellsize, static_cast<double>(grid2D.getNy())*cellsize);
			os << llcorner.toString(Coords::LATLON) << " / " << urcorner.toString(Coords::LATLON);
			break;
		}
		default:
			throw InvalidFormatException("Unsupported DEM information format", AT);
	}

	return os.str();
}

std::ostream& operator<<(std::ostream& os, const DEMObject& dem) {
	operator<<(os, *((Grid2DObject*)&dem));

	os << dem.slope;
	os << dem.azi;
	os << dem.curvature;
	os << dem.Nx << dem.Ny << dem.Nz;

	os.write(reinterpret_cast<const char*>(&dem.min_altitude), sizeof(dem.min_altitude));
	os.write(reinterpret_cast<const char*>(&dem.min_slope), sizeof(dem.min_slope));
	os.write(reinterpret_cast<const char*>(&dem.min_curvature), sizeof(dem.min_curvature));
	os.write(reinterpret_cast<const char*>(&dem.max_altitude), sizeof(dem.max_altitude));
	os.write(reinterpret_cast<const char*>(&dem.max_slope), sizeof(dem.max_slope));
	os.write(reinterpret_cast<const char*>(&dem.max_curvature), sizeof(dem.max_curvature));

	os.write(reinterpret_cast<const char*>(&dem.max_shade_distance), sizeof(dem.max_shade_distance));
	os.write(reinterpret_cast<const char*>(&dem.update_flag), sizeof(dem.update_flag));
	os.write(reinterpret_cast<const char*>(&dem.dflt_algorithm), sizeof(dem.dflt_algorithm));
	os.write(reinterpret_cast<const char*>(&dem.slope_failures), sizeof(dem.slope_failures));
	os.write(reinterpret_cast<const char*>(&dem.curvature_failures), sizeof(dem.curvature_failures));
	return os;
}

std::istream& operator>>(std::istream& is, DEMObject& dem) {
	operator>>(is, *((Grid2DObject*)&dem));

	is >> dem.slope;
	is >> dem.azi;
	is >> dem.curvature;
	is >> dem.Nx >> dem.Ny >> dem.Nz;

	is.read(reinterpret_cast<char*>(&dem.min_altitude), sizeof(dem.min_altitude));
	is.read(reinterpret_cast<char*>(&dem.min_slope), sizeof(dem.min_slope));
	is.read(reinterpret_cast<char*>(&dem.min_curvature), sizeof(dem.min_curvature));
	is.read(reinterpret_cast<char*>(&dem.max_altitude), sizeof(dem.max_altitude));
	is.read(reinterpret_cast<char*>(&dem.max_slope), sizeof(dem.max_slope));
	is.read(reinterpret_cast<char*>(&dem.max_curvature), sizeof(dem.max_curvature));

	is.read(reinterpret_cast<char*>(&dem.max_shade_distance), sizeof(dem.max_shade_distance));
	is.read(reinterpret_cast<char*>(&dem.update_flag), sizeof(dem.update_flag));
	is.read(reinterpret_cast<char*>(&dem.dflt_algorithm), sizeof(dem.dflt_algorithm));
	is.read(reinterpret_cast<char*>(&dem.slope_failures), sizeof(dem.slope_failures));
	is.read(reinterpret_cast<char*>(&dem.curvature_failures), sizeof(dem.curvature_failures));
	return is;
}

DEMObject& DEMObject::operator=(const double& value) {
	grid2D = value;
	const size_t nx = getNx(), ny = getNy();
	slope.resize(nx, ny, 0.);
	azi.resize(nx, ny, 0.);
	curvature.resize(nx, ny, 0.);
	Nx.resize(nx, ny, 0.);
	Ny.resize(nx, ny, 0.);
	Nz.resize(nx, ny, 0.);
	min_altitude = grid2D.getMin();
	max_altitude = grid2D.getMax();
	min_slope = 0.;
	max_slope = 0.;
	min_curvature = 0.;
	max_curvature = 0.;
	return *this;
}

DEMObject& DEMObject::operator+=(const double& rhs) {
	grid2D += rhs;
	updateAllMinMax();
	return *this;
}

const DEMObject DEMObject::operator+(const double& rhs) const {
	DEMObject result = *this;
	result.grid2D += rhs;
	result.updateAllMinMax();
	return result;
}

DEMObject& DEMObject::operator+=(const Grid2DObject& rhs) {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	grid2D += rhs.grid2D;
	update();
	return *this;
}

const DEMObject DEMObject::operator+(const Grid2DObject& rhs) const {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	DEMObject result(*this);
	result.grid2D += rhs.grid2D;
	result.update();
	return result;
}

DEMObject& DEMObject::operator-=(const double& rhs) {
	grid2D -= rhs;
	updateAllMinMax();
	return *this;
}

const DEMObject DEMObject::operator-(const double& rhs) const {
	DEMObject result(*this);
	result.grid2D -= rhs;
	result.updateAllMinMax();
	return result;
}

DEMObject& DEMObject::operator-=(const Grid2DObject& rhs) {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	grid2D -= rhs.grid2D;
	update();
	return *this;
}

const DEMObject DEMObject::operator-(const Grid2DObject& rhs) const {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	DEMObject result(*this);
	result.grid2D -= rhs.grid2D;
	result.update();
	return result;
}

DEMObject& DEMObject::operator*=(const double& rhs) {
	grid2D *= rhs;
	update();
	return *this;
}

const DEMObject DEMObject::operator*(const double& rhs) const {
	DEMObject result(*this);
	result.grid2D *= rhs;
	result.update();
	return result;
}

DEMObject& DEMObject::operator*=(const Grid2DObject& rhs) {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	grid2D *= rhs.grid2D;
	update();
	return *this;
}

const DEMObject DEMObject::operator*(const Grid2DObject& rhs) const {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	DEMObject result(*this);
	result.grid2D *= rhs.grid2D;
	result.update();
	return result;
}

DEMObject& DEMObject::operator/=(const double& rhs) {
	grid2D /= rhs;
	update();
	return *this;
}

const DEMObject DEMObject::operator/(const double& rhs) const {
	DEMObject result(*this);
	result.grid2D /= rhs;
	result.update();
	return result;
}

DEMObject& DEMObject::operator/=(const Grid2DObject& rhs) {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	grid2D /= rhs.grid2D;
	update();
	return *this;
}

const DEMObject DEMObject::operator/(const Grid2DObject& rhs) const {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	DEMObject result(*this);
	result.grid2D /= rhs.grid2D;
	result.update();
	return result;
}

bool DEMObject::operator==(const DEMObject& in) const {
	return (isSameGeolocalization(in) && grid2D==in.grid2D);
}

bool DEMObject::operator!=(const DEMObject& in) const {
	return !(*this==in);
}

} //end namespace
