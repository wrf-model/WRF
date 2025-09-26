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
#include <algorithm>

#include <meteoio/meteoStats/libinterpol2D.h>
#include <meteoio/meteoLaws/Atmosphere.h>
#include <meteoio/meteoLaws/Meteoconst.h> //for math constants
#include <meteoio/MathOptim.h> //math optimizations

using namespace std;

namespace mio {

//Useful functions
/**
 * @brief check if the points measurements are all at zero
 * This check can be performed to trigger optimizations: it is quicker
 * to fill the grid directly with zeroes instead of running a complicated
 * algorithm.
 * @return true if all data is set to zero
 */
bool Interpol2D::allZeroes(const std::vector<double>& vecData)
{
	for (size_t ii=0; ii<vecData.size(); ++ii) {
		if (abs(vecData[ii])>0) return false;
	}
	return true;
}

/**
* @brief Computes the horizontal distance between points, given by coordinates in a geographic grid
* @param X1 (const double) first point's X coordinate
* @param Y1 (const double) first point's Y coordinate
* @param X2 (const double) second point's X coordinate
* @param Y2 (const double) second point's Y coordinate
* @return (double) distance in m
*/
inline double Interpol2D::HorizontalDistance(const double& X1, const double& Y1, const double& X2, const double& Y2)
{
	//This function computes the horizontaldistance between two points
	//coordinates are given in a square, metric grid system
	const double DX=(X1-X2), DY=(Y1-Y2);
	return sqrt( DX*DX + DY*DY );
}

/**
* @brief Computes the 1/horizontal distance between points, given by coordinates in a geographic grid
* @param X1 (const double) first point's X coordinate
* @param Y1 (const double) first point's Y coordinate
* @param X2 (const double) second point's X coordinate
* @param Y2 (const double) second point's Y coordinate
* @return (double) 1/distance in m
*/
inline double Interpol2D::InvHorizontalDistance(const double& X1, const double& Y1, const double& X2, const double& Y2)
{
	//This function computes 1/horizontaldistance between two points
	//coordinates are given in a square, metric grid system
	const double DX=(X1-X2), DY=(Y1-Y2);
	return Optim::invSqrt( DX*DX + DY*DY ); //we use the optimized approximation for 1/sqrt
}

/**
* @brief Computes the horizontal distance between points, given by their cells indexes
* @param X1 (const double) first point's i index
* @param Y1 (const double) first point's j index
* @param X2 (const double) second point's X coordinate
* @param Y2 (const double) second point's Y coordinate
* @return (double) distance in m
*/
inline double Interpol2D::HorizontalDistance(const DEMObject& dem, const int& i, const int& j, const double& X2, const double& Y2)
{
	//This function computes the horizontal distance between two points
	//coordinates are given in a square, metric grid system
	//for grid points toward real coordinates
	const double X1 = (dem.llcorner.getEasting()+i*dem.cellsize);
	const double Y1 = (dem.llcorner.getNorthing()+j*dem.cellsize);
	const double DX=(X1-X2), DY=(Y1-Y2);
	return sqrt( DX*DX + DY*DY );
}

/**
* @brief Build the list of (distance to grid cell, stations index) ordered by their distance to a grid cell
* @param x x coordinate of cell
* @param y y coordinate of cell
* @param list list of pairs (distance to grid cell, stations index)
*/
void Interpol2D::getNeighbors(const double& x, const double& y,
                              const std::vector<StationData>& vecStations,
                              std::vector< std::pair<double, size_t> >& list)
{
	list.resize(vecStations.size());

	for (size_t i=0; i<vecStations.size(); i++) {
		const Coords& position = vecStations[i].position;
		const double DX = x-position.getEasting();
		const double DY = y-position.getNorthing();
		const double d2 = (DX*DX + DY*DY);
		const std::pair <double, size_t> tmp(d2,i);
		list[i] = tmp;
	}

	sort (list.begin(), list.end());
}

//convert a vector of stations into two vectors of eastings and northings
void Interpol2D::buildPositionsVectors(const std::vector<StationData>& vecStations, std::vector<double>& vecEastings, std::vector<double>& vecNorthings)
{
	const size_t nr_stations = vecStations.size();
	vecEastings.resize( nr_stations );
	vecNorthings.resize( nr_stations );
	for (size_t i=0; i<nr_stations; i++) {
		const Coords& position = vecStations[i].position;
		vecEastings[i] = position.getEasting();
		vecNorthings[i] = position.getNorthing();
	}
}

//these weighting functions take the square of a distance as an argument and return a weight
inline double Interpol2D::weightInvDist(const double& d2)
{
	return Optim::invSqrt( d2 ); //we use the optimized approximation for 1/sqrt
}
inline double Interpol2D::weightInvDistSqrt(const double& d2)
{
	return Optim::fastSqrt_Q3( Optim::invSqrt(d2) ); //we use the optimized approximation for 1/sqrt
}
inline double Interpol2D::weightInvDist2(const double& d2)
{
	return 1./d2;
}
inline double Interpol2D::weightInvDistN(const double& d2)
{
	return pow( Optim::invSqrt(d2) , dist_pow); //we use the optimized approximation for 1/sqrt
}

//Filling Functions
/**
* @brief Grid filling function:
* This implementation builds a standard air pressure as a function of the elevation
* @param dem array of elevations (dem)
* @param grid 2D array to fill
*/
void Interpol2D::stdPressure(const DEMObject& dem, Grid2DObject& grid)
{
	grid.set(dem, IOUtils::nodata);
 
	//provide each point with an altitude dependant pressure... it is worth what it is...
	for (size_t ii=0; ii<grid.size(); ii++) {
		const double cell_altitude=dem(ii);
		if (cell_altitude!=IOUtils::nodata)
			grid(ii) = Atmosphere::stdAirPressure(cell_altitude);
	}
}

/**
* @brief Grid filling function:
* This implementation fills the grid with a constant value
* @param value value to put in the grid
* @param dem array of elevations (dem). This is needed in order to know if a point is "nodata"
* @param grid 2D array to fill
*/
void Interpol2D::constant(const double& value, const DEMObject& dem, Grid2DObject& grid)
{
	grid.set(dem, IOUtils::nodata);

	//fills a data table with constant values
	for (size_t ii=0; ii<grid.size(); ii++) {
		if (dem(ii)!=IOUtils::nodata) {
			grid(ii) = value;
		}
	}
}

double Interpol2D::IDWCore(const double& x, const double& y, const std::vector<double>& vecData_in,
                           const std::vector<double>& vecEastings, const std::vector<double>& vecNorthings, const double& scale, const double& alpha)
{
	//The value at any given cell is the sum of the weighted contribution from each source
	const size_t n_stations = vecEastings.size();
	double parameter = 0., norm = 0.;

	for (size_t ii=0; ii<n_stations; ii++) {
		const double DX = x-vecEastings[ii];
		const double DY = y-vecNorthings[ii];
		const double dist = Optim::invSqrt( DX*DX + DY*DY + scale*scale ); //use the optimized 1/sqrt approximation
		const double weight = (alpha==1.)? dist : Optim::fastPow(dist, alpha);
		parameter += weight*vecData_in[ii];
		norm += weight;
	}
	return (parameter/norm); //normalization
}

double Interpol2D::IDWCore(const std::vector<double>& vecData_in, const std::vector<double>& vecDistance_sq, const double& scale, const double& alpha)
{
	//The value at any given cell is the sum of the weighted contribution from each source
	const size_t n_stations = vecDistance_sq.size();
	double parameter = 0., norm = 0.;

	for (size_t ii=0; ii<n_stations; ii++) {
		const double dist = Optim::invSqrt( vecDistance_sq[ii] + scale*scale ); //use the optimized 1/sqrt approximation
		const double weight = (alpha==1.)? dist : Optim::fastPow(dist, alpha);
		parameter += weight*vecData_in[ii];
		norm += weight;
	}
	return (parameter/norm); //normalization
}

/** @brief Grid filling function:
* Similar to Interpol2D::LapseIDW but using a limited number of stations for each cell.
* @param vecData_in input values to use for the IDW
* @param vecStations_in position of the "values" (altitude and coordinates)
* @param dem array of elevations (dem)
* @param nrOfNeighbors number of neighboring stations to use for each pixel
* @param grid 2D array to fill
* @param scale The scale factor is used to smooth the grid. It is added to the distance before applying the weights in order to come into the tail of "1/d".
* @param alpha The weights are computed as 1/dist^alpha, so give alpha=1 for standards 1/dist weights.
*/
void Interpol2D::LocalLapseIDW(const std::vector<double>& vecData_in, const std::vector<StationData>& vecStations_in,
                               const DEMObject& dem, const size_t& nrOfNeighbors,
                               Grid2DObject& grid, const double& scale, const double& alpha)
{
	grid.set(dem, IOUtils::nodata);

	//run algorithm
	for (size_t j=0; j<grid.getNy(); j++) {
		for (size_t i=0; i<grid.getNx(); i++) {
			//LL_IDW_pixel returns nodata when appropriate
			grid(i,j) = LLIDW_pixel(i, j, vecData_in, vecStations_in, dem, nrOfNeighbors, scale, alpha);
		}
	}
}

//calculate a local pixel for LocalLapseIDW
double Interpol2D::LLIDW_pixel(const size_t& i, const size_t& j,
                               const std::vector<double>& vecData_in, const std::vector<StationData>& vecStations_in,
                               const DEMObject& dem, const size_t& nrOfNeighbors, const double& scale, const double& alpha)
{
	const double cell_altitude = dem(i,j);
	if (cell_altitude==IOUtils::nodata)
		return IOUtils::nodata;

	//fill vectors with neighbors sorted by the square of their distance to (i,j)
	const double x = dem.llcorner.getEasting()+static_cast<double>(i)*dem.cellsize;
	const double y = dem.llcorner.getNorthing()+static_cast<double>(j)*dem.cellsize;
	std::vector< std::pair<double, size_t> > sorted_neighbors;
	getNeighbors(x, y, vecStations_in, sorted_neighbors);

	//build the vectors of valid data, with max nrOfNeighbors
	std::vector<double> altitudes, values, distances_sq;
	for (size_t st=0; st<sorted_neighbors.size(); st++) {
		const size_t st_index = sorted_neighbors[st].second;
		const double value = vecData_in[st_index];
		const double altitude = vecStations_in[st_index].position.getAltitude();
		if ((value != IOUtils::nodata) && (altitude != IOUtils::nodata)) {
			altitudes.push_back( altitude );
			values.push_back( value );
			distances_sq.push_back( sorted_neighbors[st].first );
			if (altitudes.size()>=nrOfNeighbors) break;
		}

	}

	//compute lapse rate and detrend the stations' data
	if (altitudes.empty()) return IOUtils::nodata;
	const Fit1D trend(Fit1D::NOISY_LINEAR, altitudes, values);
	for (size_t ii=0; ii<altitudes.size(); ii++) {
		values[ii] -= trend( altitudes[ii] );
	}

	//compute the local pixel value, retrend
	const double pixel_value = IDWCore(values, distances_sq, scale, alpha);
	if (pixel_value!=IOUtils::nodata)
		return pixel_value + trend(cell_altitude);
	else
		return IOUtils::nodata;
}

/**
* @brief Grid filling function:
* This implementation fills a grid using Inverse Distance Weighting.
* for example, the air temperatures measured at several stations would be given as values, the stations positions
* as positions and projected to a grid. No elevation detrending is performed, the DEM is only used for checking if a grid point is "nodata".
* @param vecData_in input values to use for the IDW
* @param vecStations_in position of the "values" (altitude and coordinates)
* @param dem array of elevations (dem). This is needed in order to know if a point is "nodata"
* @param grid 2D array to fill
* @param scale The scale factor is used to smooth the grid. It is added to the distance before applying the weights in order to come into the tail of "1/d".
* @param alpha The weights are computed as 1/dist^alpha, so give alpha=1 for standards 1/dist weights.
*/
void Interpol2D::IDW(const std::vector<double>& vecData_in, const std::vector<StationData>& vecStations_in,
                     const DEMObject& dem, Grid2DObject& grid, const double& scale, const double& alpha)
{
	if (allZeroes(vecData_in)) { //if all data points are zero, simply fill the grid with zeroes
		constant(0., dem, grid);
		return;
	}
	if (vecData_in.size()==1) { //if only one station, fill the grid with this value
		constant(vecData_in[0], dem, grid);
		return;
	}

	grid.set(dem, IOUtils::nodata);
	std::vector<double> vecEastings, vecNorthings;
	buildPositionsVectors(vecStations_in, vecEastings, vecNorthings);

	//multiple source stations: simple IDW Kriging
	const double xllcorner = dem.llcorner.getEasting();
	const double yllcorner = dem.llcorner.getNorthing();
	const double cellsize = dem.cellsize;
	for (size_t jj=0; jj<grid.getNy(); jj++) {
		for (size_t ii=0; ii<grid.getNx(); ii++) {
			if (dem(ii,jj)!=IOUtils::nodata) {
				grid(ii,jj) = IDWCore((xllcorner+double(ii)*cellsize), (yllcorner+double(jj)*cellsize),
				                           vecData_in, vecEastings, vecNorthings, scale, alpha);
			}
		}
	}
}

/**
* @brief Grid filling function:
* This implementation fills a grid using a curvature and slope algorithm, as described in
* G. E. Liston and K. Elder, <i>"A meteorological distribution system for high-resolution terrestrial modeling (MicroMet)"</i>, Journal of Hydrometeorology, <b>7.2</b>, 2006.
* @param i_dem array of elevations (dem). The slope must have been updated as it is required for the DEM analysis.
* @param VW 2D array of Wind Velocity to fill
* @param DW 2D array of Wind Direction to fill
*/
void Interpol2D::ListonWind(const DEMObject& i_dem, Grid2DObject& VW, Grid2DObject& DW)
{
	static const double eps = 1e-3;
	if ((!VW.isSameGeolocalization(DW)) || (!VW.isSameGeolocalization(i_dem))){
		throw IOException("Requested grid VW and grid DW don't match the geolocalization of the DEM", AT);
	}

	//make sure dem has the curvature that we need
	const bool recomputeDEM = i_dem.curvature.empty();
	DEMObject *intern_dem = NULL;
	if (recomputeDEM) {
		std::cerr << "[W] WIND_CURV spatial interpolations algorithm selected but no dem curvature available! Computing it...\n";
		intern_dem = new DEMObject(i_dem);
		intern_dem->setUpdatePpt((DEMObject::update_type)(DEMObject::SLOPE|DEMObject::CURVATURE));
		intern_dem->update();
	}
	const DEMObject *dem = (recomputeDEM)? intern_dem : &i_dem;

	//calculate terrain slope in the direction of the wind
	Array2D<double> Omega_s(VW.getNx(), VW.getNy());
	for (size_t ii=0; ii<Omega_s.size(); ii++) {
		const double theta = DW(ii);
		const double beta = dem->slope(ii);
		const double xi = dem->azi(ii);

		if (theta!=IOUtils::nodata && beta!=IOUtils::nodata && xi!=IOUtils::nodata)
			Omega_s(ii) = beta*Cst::to_rad * cos((theta-xi)*Cst::to_rad);
		else
			Omega_s(ii) = IOUtils::nodata;
	}

	//compute normalization factors
	const double omega_s_min=Omega_s.getMin();
	const double omega_s_range=(Omega_s.getMax()-omega_s_min);
	const double omega_c_min=dem->min_curvature;
	const double omega_c_range=(dem->max_curvature-omega_c_min);

	//compute modified VW and DW
	static const double gamma_s = 0.58; //speed weighting factor
	static const double gamma_c = 0.42; //direction weighting factor
	for (size_t ii=0; ii<VW.size(); ii++) {
		const double vw = VW(ii);
		if (vw==0. || vw==IOUtils::nodata) continue; //we can not apply any correction factor!
		const double dw = DW(ii);
		if (dw==IOUtils::nodata) continue; //we can not apply any correction factor!

		if (Omega_s(ii)==IOUtils::nodata) continue; //we can not calculate any correction factor!
		const double omega_s = (omega_s_range>eps)? (Omega_s(ii)-omega_s_min)/omega_s_range - 0.5 : 0.;
		const double omega_c = (dem->curvature(ii)!=IOUtils::nodata && omega_c_range>eps)? (dem->curvature(ii) - omega_c_min)/omega_c_range - 0.5 : 0.;

		const double Ww = 1. + gamma_s*omega_s + gamma_c*omega_c;
		VW(ii) *= Ww;

		const double theta = DW(ii);
		const double xi = dem->azi(ii);
		const double theta_t = -0.5 * omega_s * sin( 2.*(xi-theta)*Cst::to_rad ) * Cst::to_deg;
		DW(ii) = fmod(dw+theta_t + 360., 360.);
	}

	if (intern_dem!=NULL) delete (intern_dem);
}

/**
* @brief Distribute precipitation in a way that reflects snow redistribution on the ground, according to (Huss, 2008)
* This method modifies the solid precipitation distribution according to the local slope and curvature. See
* <i>"Quantitative evaluation of different hydrological modelling approaches in a partly glacierized Swiss watershed"</i>, Magnusson et All., Hydrological Processes, 2010, under review.
* and
* <i>"Modelling runoff from highly glacierized alpine catchments in a changing climate"</i>, Huss et All., Hydrological Processes, <b>22</b>, 3888-3902, 2008.
* @param dem array of elevations (dem). The slope must have been updated as it is required for the DEM analysis.
* @param ta array of air temperatures used to determine if precipitation is rain or snow
* @param grid 2D array of precipitation to fill
* @author Florian Kobierska, Jan Magnusson, Rob Spence and Mathias Bavay
*/
void Interpol2D::CurvatureCorrection(DEMObject& dem, const Grid2DObject& ta, Grid2DObject& grid)
{
	if (!grid.isSameGeolocalization(dem)) {
		throw IOException("Requested grid does not match the geolocalization of the DEM", AT);
	}
	const double dem_max_curvature = dem.max_curvature, dem_range_curvature=(dem.max_curvature-dem.min_curvature);
	if (dem_range_curvature==0.) return;

	const double orig_mean = grid.grid2D.getMean();

	for (size_t ii=0; ii<grid.size(); ii++) {
		if (ta(ii)>Cst::t_water_freezing_pt) continue; //modify the grid of precipitations only if air temperature is below or at freezing

		const double slope = dem.slope(ii);
		const double curvature = dem.curvature(ii);
		if (slope==IOUtils::nodata || curvature==IOUtils::nodata) continue;

		double& val = grid(ii);
		if (val!=IOUtils::nodata && dem_range_curvature!=0.) { //cf Huss
			val *= 0.5-(curvature-dem_max_curvature) / dem_range_curvature;
		}
	}

	//HACK: correction for precipitation sum over the whole domain
	//this is a cheap/crappy way of compensating for the spatial redistribution of snow on the slopes
	const double new_mean = grid.grid2D.getMean();
	if (new_mean!=0.) grid.grid2D *= orig_mean/new_mean;

}

void Interpol2D::steepestDescentDisplacement(const DEMObject& dem, const Grid2DObject& grid, const size_t& ii, const size_t& jj, char &d_i_dest, char &d_j_dest)
{
	double max_slope = 0.;
	d_i_dest = 0;
	d_j_dest = 0;

	//loop around all adjacent cells to find the cell with the steepest downhill slope
	for (char d_i=-1; d_i<=1; d_i++) {
		for (char d_j=-1; d_j<=1; d_j++) {
			const double elev_pt1 = dem(ii, jj);
			const double elev_pt2 = dem(static_cast<size_t>(ii + d_i), static_cast<size_t>(jj + d_j));
			const double precip_1 = grid(ii, jj);
			const double precip_2 = grid(static_cast<size_t>(ii + d_i), static_cast<size_t>(jj + d_j));
			const double height_ratio = (elev_pt1+precip_1) / (elev_pt2+precip_2);
			const double new_slope = dem.slope(static_cast<size_t>(ii + d_i), static_cast<size_t>(jj + d_j));

			if ((new_slope>max_slope) && (height_ratio>1.)){
				max_slope = new_slope;
				d_i_dest = d_i;
				d_j_dest = d_j;
			}
		}
	}
}

double Interpol2D::depositAroundCell(const DEMObject& dem, const size_t& ii, const size_t& jj, const double& precip, Grid2DObject &grid)
{
	//else add precip to the cell and remove the same amount from the precip variable
	grid(ii, jj) += precip;
	double distributed_precip = precip;

	for (char d_i=-1;d_i<=1;d_i++){
		for (char d_j=-1;d_j<=1;d_j++){
			const double elev_pt1 = dem(ii, jj);
			const double elev_pt2 = dem(static_cast<size_t>(ii + d_i), static_cast<size_t>(jj + d_j));
			const double precip_1 = grid(ii, jj);
			const double precip_2 = grid(static_cast<size_t>(ii + d_i), static_cast<size_t>(jj + d_j));
			const double height_ratio = (elev_pt1+precip_1) / (elev_pt2+precip_2);

			if ((d_i!=0)||(d_j!=0)){
				if (height_ratio>1.){
					grid(static_cast<size_t>(ii + d_i), static_cast<size_t>(jj + d_j)) += precip;
					distributed_precip += precip;
				}
			}
		}
	}

	return distributed_precip;
}

/**
 * @brief redistribute precip from steeper slopes to gentler slopes by following the steepest path from top to bottom
 * and gradually depositing precip during descent
 * @param dem array of elevations (dem). The slope must have been updated as it is required for the DEM analysis.
 * @param ta array of air temperatures used to determine if precipitation is rain or snow
 * @param grid 2D array of precipitation to fill
 * @author Rob Spence and Mathias Bavay
 */
void Interpol2D::SteepSlopeRedistribution(const DEMObject& dem, const Grid2DObject& ta, Grid2DObject& grid)
{
	for (size_t jj=1; jj<(grid.getNy()-1); jj++) {
		for (size_t ii=1; ii<(grid.getNx()-1); ii++) {
			if (grid(ii,jj)==IOUtils::nodata) continue;
			if (ta(ii, jj)>Cst::t_water_freezing_pt) continue; //modify precipitation only for air temperatures at or below freezing

			const double slope = dem.slope(ii, jj);
			const double curvature = dem.curvature(ii, jj);
			if (slope==IOUtils::nodata || curvature==IOUtils::nodata) continue;
			if (slope<=40.) continue; //redistribution only above 40 degrees

			//remove all precip above 60 deg or linearly decrease it
			double precip = (slope>60.)? grid(ii, jj) : grid(ii, jj) * ((40.-slope)/-30.);
			grid(ii, jj) -= precip; //we will redistribute the precipitation in a different way

			const double increment = precip / 50.; //break removed precip into smaller amounts to be redistributed
			double counter = 0.5;                  //counter will determine amount of precip deposited

			size_t ii_dest = ii, jj_dest = jj;
			while (precip>0.) {
				char d_i, d_j;
				steepestDescentDisplacement(dem, grid, ii_dest, jj_dest, d_i, d_j);
				//move to the destination cell
				ii_dest += d_i;
				jj_dest += d_j;

				if ((ii_dest==0) || (jj_dest==0) || (ii_dest==(grid.getNx()-1))|| (jj_dest==(grid.getNy()-1))){
					//we are getting out of the domain: deposit local contribution
					grid(ii_dest, jj_dest) += counter*increment;
					break;
				}
				if (d_i==0 && d_j==0) {
					//local minimum, everything stays here...
					grid(ii_dest, jj_dest) += precip;
					break;
				}

				precip -= depositAroundCell(dem, ii_dest, jj_dest, counter*increment, grid);
				counter += 0.25; //greater amount of precip is deposited as we move down the slope
			}
		}
	}
}

/**
* @brief Distribute precipitation in a way that reflects snow redistribution on the ground, according to (Huss, 2008)
* This method modifies the solid precipitation distribution according to the local slope and curvature: all pixels whose slope
* is greater than 60° will not receive any snow at all. All pixels whose slope is less than 40° will receive full snow
* and any pixel between 40° and 60° sees a linear correction between 100% and 0% snow. After this step, a curvature
* correction is applied: pixels having the minimu curvature see 50% snow more, pixels having the maximum curvature see
* 50% snow less and pixels ate the middle of the curvature range are unaffected.
*
* For more, see <i>"Quantitative evaluation of different hydrological modelling approaches in a partly glacierized Swiss watershed"</i>, Magnusson et All., Hydrological Processes, 2010, under review.
* and
* <i>"Modelling runoff from highly glacierized alpine catchments in a changing climate"</i>, Huss et All., Hydrological Processes, <b>22</b>, 3888-3902, 2008.
* @param dem array of elevations (dem). The slope must have been updated as it is required for the DEM analysis.
* @param ta array of air temperatures used to determine if precipitation is rain or snow
* @param grid 2D array of precipitation to fill
* @author Florian Kobierska, Jan Magnusson and Mathias Bavay
*/
void Interpol2D::PrecipSnow(const DEMObject& dem, const Grid2DObject& ta, Grid2DObject& grid)
{
	if (!grid.isSameGeolocalization(dem))
		throw IOException("Requested grid does not match the geolocalization of the DEM", AT);

	const double dem_max_curvature=dem.max_curvature, dem_range_curvature=(dem.max_curvature-dem.min_curvature);

	for (size_t ii=0; ii<grid.size(); ii++) {
		//we only modify the grid of precipitations if air temperature
		//at this point is below or at freezing
		if (ta.grid2D(ii)<=Cst::t_water_freezing_pt) {
			const double slope = dem.slope(ii);
			const double curvature = dem.curvature(ii);
			double val = grid.grid2D(ii);

			if (slope==IOUtils::nodata || curvature==IOUtils::nodata) {
				val = IOUtils::nodata;
			} else if (slope>60.) { //No snow precipitation happens for these slopes
				val = 0.;
			} else if (slope>40.) { //Linear transition from no snow to 100% snow
				val *= (60.-slope)/20.;
			} //else: unchanged

			if (val!=IOUtils::nodata && dem_range_curvature!=0.) { //cf Huss
				grid.grid2D(ii) = val*(0.5-(curvature-dem_max_curvature)/dem_range_curvature);
			}
		}
	}
}

//Compute the wind direction changes by the terrain, see Ryan, "a mathematical model for diagnosis
//and prediction of surface winds in mountainous terrain", 1977, journal of applied meteorology, 16, 6
/**
 * @brief compute the change of wind direction by the local terrain
 * This is according to Ryan, <i>"a mathematical model for diagnosis and prediction of surface
 * winds in mountainous terrain"</i>, 1977, journal of applied meteorology, <b>16</b>, 6.
 * @param dem array of elevations (dem). The slope and azimuth must have been updated as they are required for the DEM analysis.
 * @param VW 2D array of wind speed to fill
 * @param DW 2D array of wind direction to fill
 * @author Mathias Bavay
 */
void Interpol2D::RyanWind(const DEMObject& dem, Grid2DObject& VW, Grid2DObject& DW)
{
	if ((!VW.isSameGeolocalization(DW)) || (!VW.isSameGeolocalization(dem)))
		throw IOException("Requested grid VW and grid DW don't match the geolocalization of the DEM", AT);

	static const double shade_factor = 5.;
	const double cellsize = dem.cellsize;
	const double max_alt = dem.grid2D.getMax();

	for (size_t jj=0; jj<VW.getNy(); jj++) {
		for (size_t ii=0; ii<VW.getNx(); ii++) {
			const double azi = dem.azi(ii,jj);
			const double slope = dem.slope(ii,jj);
			if (azi==IOUtils::nodata || slope==IOUtils::nodata) {
				VW(ii,jj) = IOUtils::nodata;
				DW(ii,jj) = IOUtils::nodata;
				continue;
			}

			const double dw = DW(ii,jj);
			const double Yd = 100.*tan(slope*Cst::to_rad);
			const double Fd = -0.225 * std::min(Yd, 100.) * sin(2.*(azi-dw)*Cst::to_rad);
			DW(ii,jj) = fmod(dw+Fd + 360., 360.);

			const double alt_ref = dem(ii,jj); //the altitude exists, because a slope exists!
			const double dmax = (max_alt - alt_ref) * shade_factor;
			if (dmax<=cellsize) continue;

			const double Yu = 100.*getTanMaxSlope(dem, cellsize, dmax, dw, ii, jj); //slope to the horizon upwind
			const double Fu = atan(0.17*std::min(Yu, 100.)) / 100.;
			VW(ii,jj) *= (1. - Fu);
		}
	}
}

/**
 * @brief compute the max slope angle looking toward the horizon in a given direction.
 * The search distance is limited between dmin and dmax from the starting point (i,j) and the elvation difference
 * must be at least 2 meters (otherwise it is considered flat). If a slope start to be positive/negative before turning negative/positive,
 * the first one would be returned (so a pixel just behind a ridge would still see an uphill slope to the ridge even if the
 * slope behind the ridge would be greater).
 *
 * This is exactly identical with the Winstral Sx factor for a single direction. Or the upwind slope for Ryan.
 * @param[in] dem DEM to work with
 * @param[in] dmin minimum search distance (ie all points at less than dmin are skipped)
 * @param[in] dmax maximum search distance
 * @param[in] bearing direction of the search
 * @param[in] i x index of the cell to start the search from
 * @param[in] j y index of the cell to start the search from
 * @return tan of the maximum slope angle from the (i,j) cell in the given direction
 */
double Interpol2D::getTanMaxSlope(const Grid2DObject& dem, const double& dmin, const double& dmax, const double& bearing, const size_t& i, const size_t& j)
{
	const double ref_altitude = dem(i, j);
	if (ref_altitude==IOUtils::nodata) return 0.; //nothing better to do...
	
	const double inv_dmin = (dmin>0.)? 1./dmin : Cst::dbl_max;
	const double inv_dmax = 1./dmax;
	const double sin_alpha = sin(bearing*Cst::to_rad);
	const double cos_alpha = cos(bearing*Cst::to_rad);
	static const double altitude_thresh = 1.;
	const double cellsize_sq = Optim::pow2(dem.cellsize);
	const int ii = static_cast<int>(i), jj = static_cast<int>(j);
	const int ncols = static_cast<int>(dem.getNx()), nrows = static_cast<int>(dem.getNy());

	int ll=ii, mm=jj;

	double max_tan_slope = 0.;
	size_t nb_cells = 0;
	while ( !(ll<0 || ll>ncols-1 || mm<0 || mm>nrows-1) ) {
		const double altitude = dem((unsigned)ll, (unsigned)mm);
		if ( (altitude!=mio::IOUtils::nodata) && !(ll==ii && mm==jj) ) {
			//compute local sx
			const double delta_elev = altitude - ref_altitude;
			const double inv_distance = Optim::invSqrt( cellsize_sq*(Optim::pow2(ll-ii) + Optim::pow2(mm-jj)) );
			if (inv_distance<=inv_dmin && fabs(delta_elev)>=altitude_thresh) { //only for cells further than dmin
				if (inv_distance<inv_dmax) break; //stop if distance>dmax

				const double tan_slope = delta_elev*inv_distance;
				//update max_tan_sx if necessary. We compare and tan(sx) in order to avoid computing atan()
				if (max_tan_slope>=0. && tan_slope>max_tan_slope) max_tan_slope = tan_slope;
				if (max_tan_slope<=0. && tan_slope<max_tan_slope) max_tan_slope = tan_slope;
			}
		}

		//move to next cell
		nb_cells++;
		ll = ii + (int)round( ((double)nb_cells)*sin_alpha ); //alpha is a bearing
		mm = jj + (int)round( ((double)nb_cells)*cos_alpha ); //alpha is a bearing
	}
	
	return max_tan_slope;
}

/**
* @brief Compute Winstral Sx exposure coefficient
* This implements the wind exposure coefficient for one bearing as in
* <i>"Simulating wind fields and snow redistribution using terrain‐based parameters to model
* snow accumulation and melt over a semi‐arid mountain catchment."</i>, Winstral, Adam, and Danny Marks, Hydrological Processes <b>16.18</b> (2002), pp3585-3603.
* @param dem digital elevation model
* @param dmax search radius
* @param in_bearing wind direction to consider
* @param grid 2D array of precipitation to fill
* @author Mathias Bavay
*/
void Interpol2D::WinstralSX(const DEMObject& dem, const double& dmax, const double& in_bearing, Grid2DObject& grid)
{
	grid.set(dem, IOUtils::nodata);

	static const double dmin = 0.;
	static const double bearing_inc = 5.;
	static const double bearing_width = 30.;
	double bearing1 = fmod( in_bearing - bearing_width/2., 360. );
	double bearing2 = fmod( in_bearing + bearing_width/2., 360. );
	if (bearing1>bearing2) std::swap(bearing1, bearing2);

	const size_t ncols = dem.getNx(), nrows = dem.getNy();
	for (size_t jj = 0; jj<nrows; jj++) {
		for (size_t ii = 0; ii<ncols; ii++) {
			if (dem(ii,jj)==IOUtils::nodata) continue;
			double sum = 0.;
			unsigned short count=0;
			for (double bearing=bearing1; bearing<=bearing2; bearing += bearing_inc) {
				sum += atan( getTanMaxSlope(dem, dmin, dmax, bearing, ii, jj) );
				count++;
			}

			grid(ii,jj) = (count>0)? sum/(double)count : IOUtils::nodata;
		}
	}
}

void Interpol2D::WinstralSX(const DEMObject& dem, const double& dmax, const Grid2DObject& DW, Grid2DObject& grid)
{
	if (!DW.isSameGeolocalization(dem)){
		throw IOException("Requested grid DW doesn't match the geolocalization of the DEM", AT);
	}
	
	grid.set(dem, IOUtils::nodata);

	static const double dmin = 0.;
	static const double bearing_inc = 5.;
	static const double bearing_width = 30.;

	const size_t ncols = dem.getNx(), nrows = dem.getNy();
	for (size_t jj = 0; jj<nrows; jj++) {
		for (size_t ii = 0; ii<ncols; ii++) {
			if (dem(ii,jj)==IOUtils::nodata) continue;
			const double in_bearing = DW(ii,jj);
			double bearing1 = fmod( in_bearing - bearing_width/2., 360. );
			double bearing2 = fmod( in_bearing + bearing_width/2., 360. );
			if (bearing1>bearing2) std::swap(bearing1, bearing2);
			double sum = 0.;
			unsigned short count=0;
			for (double bearing=bearing1; bearing<=bearing2; bearing += bearing_inc) {
				sum += atan( getTanMaxSlope(dem, dmin, dmax, bearing, ii, jj) );
				count++;
			}

			grid(ii,jj) = (count>0)? sum/(double)count : IOUtils::nodata;
		}
	}
}

/**
* @brief Alter a precipitation field with the Winstral Sx exposure coefficient
* This implements the wind exposure coefficient (Sx) for one bearing as in
* <i>"Simulating wind fields and snow redistribution using terrain‐based parameters to model
* snow accumulation and melt over a semi‐arid mountain catchment."</i>, Winstral, Adam, and Danny Marks, Hydrological Processes <b>16.18</b> (2002), pp3585-3603.
*
* A linear correlation between erosion coefficients and eroded mass is assumed, that is that the points with maximum erosion get all their
* precipitation removed. The eroded mass is then distributed on the cells with positive Sx (with a linear correlation between positive Sx and deposited
* mass) and enforcing mass conservation within the domain.
* @remarks Only cells with an air temperature below freezing participate in the redistribution
*
* @param dem digital elevation model
* @param TA air temperature grid (in order to discriminate between solid and liquid precipitation)
* @param dmax search radius
* @param in_bearing wind direction to consider
* @param grid 2D array of precipitation to fill
* @author Mathias Bavay
*/
void Interpol2D::Winstral(const DEMObject& dem, const Grid2DObject& TA, const double& dmax, const double& in_bearing, Grid2DObject& grid)
{
	//compute wind exposure factor
	Grid2DObject Sx;
	WinstralSX(dem, dmax, in_bearing, Sx);
	
	//don't change liquid precipitation
	for (size_t ii=0; ii<Sx.size(); ii++) {
		if (TA(ii)>Cst::t_water_freezing_pt) Sx(ii)=IOUtils::nodata;
	}
	
	//get the scaling parameters
	const double min_sx = Sx.grid2D.getMin(); //negative
	const double max_sx = Sx.grid2D.getMax(); //positive
	double sum_erosion=0., sum_deposition=0.;
	//erosion: fully eroded at min_sx
	for (size_t ii=0; ii<Sx.size(); ii++) {
		const double sx = Sx(ii);
		if (sx==IOUtils::nodata) continue;
		double &val = grid(ii);
		if (sx<0.) {
			const double eroded = val * sx/min_sx; //sx<0, so there is min_sx!=0
			sum_erosion += eroded;
			val -= eroded;
		} else if (sx>0.) { //at this point, we can only compute the sum of deposition
			const double deposited = sx/max_sx; //sx>0 so there is max_sx!=0
			sum_deposition += deposited;
		}
	}
	
	//no cells can take the eroded mass or no cells even got freezing temperatures
	if (sum_deposition==0 || sum_erosion==0) return; //if max_sx==0, sum_deposition==0
	
	//deposition: garantee mass balance conservation
	//-> we now have the proper scaling factor so we can deposit in individual cells
	const double ratio = sum_erosion/sum_deposition;
	for (size_t ii=0; ii<Sx.size(); ii++) {
		const double sx = Sx(ii);
		if (sx==IOUtils::nodata) continue;
		double &val = grid(ii);
		if (sx>0.) {
			const double deposited = ratio * sx/max_sx;
			val += deposited;
		}
	}
}

void Interpol2D::Winstral(const DEMObject& dem, const Grid2DObject& TA, const Grid2DObject& DW, const Grid2DObject& VW, const double& dmax, Grid2DObject& grid)
{
	static const double vw_thresh = 5.; //m/s
	//compute wind exposure factor
	Grid2DObject Sx;
	WinstralSX(dem, dmax, DW, Sx);
	
	//don't change liquid precipitation
	for (size_t ii=0; ii<Sx.size(); ii++) {
		if (TA(ii)>Cst::t_water_freezing_pt) Sx(ii)=IOUtils::nodata;
	}
	
	//get the scaling parameters
	const double min_sx = Sx.grid2D.getMin(); //negative
	const double max_sx = Sx.grid2D.getMax(); //positive
	double sum_erosion=0., sum_deposition=0.;
	//erosion: fully eroded at min_sx
	for (size_t ii=0; ii<Sx.size(); ii++) {
		const double sx = Sx(ii);
		if (sx==IOUtils::nodata || (sx<0 && VW(ii)<vw_thresh)) continue; //low wind speed pixels don't contribute to erosion
		double &val = grid(ii);
		if (sx<0.) {
			const double eroded = val * sx/min_sx; //sx<0, so there is min_sx!=0
			sum_erosion += eroded;
			val -= eroded;
		} else if (sx>0.) { //at this point, we can only compute the sum of deposition
			const double deposited = sx/max_sx; //sx>0 so there is max_sx!=0
			sum_deposition += deposited;
		}
	}
	
	//no cells can take the eroded mass or no cells even got freezing temperatures
	if (sum_deposition==0 || sum_erosion==0) return; //if max_sx==0, sum_deposition==0
	
	//deposition: garantee mass balance conservation
	//-> we now have the proper scaling factor so we can deposit in individual cells
	const double ratio = sum_erosion/sum_deposition;
	for (size_t ii=0; ii<Sx.size(); ii++) {
		const double sx = Sx(ii);
		if (sx==IOUtils::nodata) continue;
		double &val = grid(ii);
		if (sx>0.) {
			const double deposited = ratio * sx/max_sx;
			val += deposited;
		}
	}
}

/**
* @brief Ordinary Kriging matrix formulation
* This implements the matrix formulation of Ordinary Kriging, as shown (for example) in
* <i>"Statistics for spatial data"</i>, Noel A. C. Cressie, John Wiley & Sons, revised edition, 1993, pp122.
*
* First, Ordinary kriging assumes stationarity of the mean of all random variables. We start by solving the following system:
* \f{eqnarray*}{
* \mathbf{\lambda} &  = & \mathbf{\Gamma_0^{-1}} \cdot \mathbf{\gamma^*} \\
* \left[
* \begin{array}{c}
* \lambda_1 \\
* \vdots \\
* \lambda_i \\
* \mu
* \end{array}
* \right]
* &
* =
* &
* {
* \left[
* \begin{array}{cccc}
* \Gamma_{1,1} & \cdots & \Gamma_{1,i} & 1      \\
* \vdots       & \ddots & \vdots       & \vdots \\
* \Gamma_{i,1} & \cdots & \Gamma_{i,i} & 1      \\
* 1            & \cdots & 1            & 0
* \end{array}
* \right]
* }^{-1}
* \cdot
* \left[
* \begin{array}{c}
* \gamma^*_1 \\
* \vdots \\
* \gamma^*_i \\
* 1
* \end{array}
* \right]
* \f}
* where the \f$\lambda_i\f$ are the interpolation weights (at each station i), \f$\mu\f$ is the Lagrange multiplier (used to minimize the error),
* \f$\Gamma_{i,j}\f$ is the covariance between the stations i and j and \f$\gamma^*_i\f$ the covariances between the station i and
* the local position where the interpolation has to be computed. This covariance is computed based on distance, using the variogram that gives
* covariance = f(distance). The variogram is established by fitting a statistical model to all the (distance, covariance) points originating from
* the station measurements. The statistical model of the variogram enables computing the covariance for any distance, therefore it is possible
* to compute the \f$\gamma^*_i\f$.
*
* Once the \f$\lambda_i\f$ have been computed, the locally interpolated value is computed as
* \f[
* \mathbf{X^*} = \sum \mathbf{\lambda_i} * \mathbf{X_i}
* \f]
* where \f$X_i\f$ is the value measured at station i.
*
* @param vecData vector containing the values as measured at the stations
* @param vecStations vector of stations
* @param dem digital elevation model
* @param variogram variogram regression model
* @param grid 2D array of precipitation to fill
* @author Mathias Bavay
*/
void Interpol2D::ODKriging(const std::vector<double>& vecData, const std::vector<StationData>& vecStations, const DEMObject& dem, const Fit1D& variogram, Grid2DObject& grid)
{
	//if all data points are zero, simply fill the grid with zeroes
	if (allZeroes(vecData)) {
		constant(0., dem, grid);
		return;
	}
	if (vecData.size()==1) { //if only one station, fill the grid with this value
		constant(vecData[0], dem, grid);
		return;
	}

	grid.set(dem, IOUtils::nodata);
	const size_t nrOfMeasurments = vecStations.size();
	//precompute various coordinates in the grid
	const double llcorner_x = dem.llcorner.getEasting();
	const double llcorner_y = dem.llcorner.getNorthing();
	const double cellsize = dem.cellsize;

	Matrix Ginv(nrOfMeasurments+1, nrOfMeasurments+1);

	//fill the Ginv matrix
	for (size_t j=1; j<=nrOfMeasurments; j++) {
		const Coords& st1 = vecStations[j-1].position;
		const double x1 = st1.getEasting();
		const double y1 = st1.getNorthing();

		for (size_t i=1; i<=j; i++) {
			//compute distance between stations
			const Coords& st2 = vecStations[i-1].position;
			const double DX = x1-st2.getEasting();
			const double DY = y1-st2.getNorthing();
			const double distance = Optim::fastSqrt_Q3(DX*DX + DY*DY);
			Ginv(i,j) = variogram.f(distance);
		}
		Ginv(j,j)=1.; //HACK diagonal should contain the nugget...
		Ginv(nrOfMeasurments+1,j) = 1.; //last line filled with 1s
	}
	//fill the upper half (an exact copy of the lower half)
	for (size_t j=1; j<=nrOfMeasurments; j++) {
		for (size_t i=j+1; i<=nrOfMeasurments; i++) {
			Ginv(i,j) = Ginv(j,i);
		}
	}
	//add last column of 1's and a zero
	for (size_t i=1; i<=nrOfMeasurments; i++) Ginv(i,nrOfMeasurments+1) = 1.;
	Ginv(nrOfMeasurments+1,nrOfMeasurments+1) = 0.;
	//invert the matrix
	Ginv.inv();

	Matrix G0(nrOfMeasurments+1, (size_t)1);
	//now, calculate each point
	for (size_t j=0; j<grid.getNy(); j++) {
		for (size_t i=0; i<grid.getNx(); i++) {
			if (dem(i,j)==IOUtils::nodata) continue;
				
			const double x = llcorner_x+static_cast<double>(i)*cellsize;
			const double y = llcorner_y+static_cast<double>(j)*cellsize;

			//fill gamma
			for (size_t st=0; st<nrOfMeasurments; st++) {
				//compute distance between cell and each station
				const Coords& position = vecStations[st].position;
				const double DX = x-position.getEasting();
				const double DY = y-position.getNorthing();
				const double distance = Optim::fastSqrt_Q3(DX*DX + DY*DY);

				G0(st+1,1) = variogram.f(distance); //matrix starts at 1, not 0
			}
			G0(nrOfMeasurments+1,1) = 1.; //last value is always 1

			const Matrix lambda = Ginv*G0;

			//calculate local parameter interpolation
			double p = 0.;
			for (size_t st=0; st<nrOfMeasurments; st++) {
				p += lambda(st+1,1) * vecData[st]; //matrix starts at 1, not 0
			}
			grid(i,j) = p;
		}
	}
}

} //namespace
