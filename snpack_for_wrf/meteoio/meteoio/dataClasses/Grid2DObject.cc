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
#include <meteoio/dataClasses/Grid2DObject.h>
#include <meteoio/dataClasses/CoordsAlgorithms.h>
#include <meteoio/IOExceptions.h>
#include <meteoio/IOUtils.h>
#include <meteoio/MathOptim.h>
#include <meteoio/meteoStats/libresampling2D.h>
#include <cmath>

using namespace std;

namespace mio {

Grid2DObject& Grid2DObject::operator=(const Grid2DObject& source) {
	if (this != &source) {
		grid2D = source.grid2D;
		cellsize = source.cellsize;
		llcorner = source.llcorner;
		ur_lat = source.ur_lat;
		ur_lon = source.ur_lon;
		isLatLon = source.isLatLon;
	}
	return *this;
}

Grid2DObject& Grid2DObject::operator=(const double& value) {
	grid2D = value;
	return *this;
}

Grid2DObject& Grid2DObject::operator+=(const double& rhs) {
	grid2D += rhs;
	return *this;
}

const Grid2DObject Grid2DObject::operator+(const double& rhs) const {
	Grid2DObject result = *this;
	result.grid2D += rhs;
	return result;
}

Grid2DObject& Grid2DObject::operator+=(const Grid2DObject& rhs) {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	grid2D += rhs.grid2D;
	return *this;
}

const Grid2DObject Grid2DObject::operator+(const Grid2DObject& rhs) const {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	Grid2DObject result(*this);
	result.grid2D += rhs.grid2D;
	return result;
}

Grid2DObject& Grid2DObject::operator-=(const double& rhs) {
	grid2D -= rhs;
	return *this;
}

const Grid2DObject Grid2DObject::operator-(const double& rhs) const {
	Grid2DObject result(*this);
	result.grid2D -= rhs;
	return result;
}

Grid2DObject& Grid2DObject::operator-=(const Grid2DObject& rhs) {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	grid2D -= rhs.grid2D;
	return *this;
}

const Grid2DObject Grid2DObject::operator-(const Grid2DObject& rhs) const {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	Grid2DObject result(*this);
	result.grid2D -= rhs.grid2D;
	return result;
}

Grid2DObject& Grid2DObject::operator*=(const double& rhs) {
	grid2D *= rhs;
	return *this;
}

const Grid2DObject Grid2DObject::operator*(const double& rhs) const {
	Grid2DObject result(*this);
	result.grid2D *= rhs;
	return result;
}

Grid2DObject& Grid2DObject::operator*=(const Grid2DObject& rhs) {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	grid2D *= rhs.grid2D;
	return *this;
}

const Grid2DObject Grid2DObject::operator*(const Grid2DObject& rhs) const {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	Grid2DObject result(*this);
	result.grid2D *= rhs.grid2D;
	return result;
}

Grid2DObject& Grid2DObject::operator/=(const double& rhs) {
	grid2D /= rhs;
	return *this;
}

const Grid2DObject Grid2DObject::operator/(const double& rhs) const {
	Grid2DObject result(*this);
	result.grid2D /= rhs;
	return result;
}

Grid2DObject& Grid2DObject::operator/=(const Grid2DObject& rhs) {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	grid2D /= rhs.grid2D;
	return *this;
}

const Grid2DObject Grid2DObject::operator/(const Grid2DObject& rhs) const {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	Grid2DObject result(*this);
	result.grid2D /= rhs.grid2D;
	return result;
}

bool Grid2DObject::operator==(const Grid2DObject& in) const {
	return (isSameGeolocalization(in) && grid2D==in.grid2D);
}

bool Grid2DObject::operator!=(const Grid2DObject& in) const {
	return !(*this==in);
}

/*
 * Default constructor.
 * grid2D attribute is initialized by Array2D default constructor.
 */
Grid2DObject::Grid2DObject() : grid2D(), llcorner(), cellsize(0.), ur_lat(IOUtils::nodata), ur_lon(IOUtils::nodata), isLatLon(false) {}

Grid2DObject::Grid2DObject(const size_t& i_ncols, const size_t& i_nrows,
                           const double& i_cellsize, const Coords& i_llcorner)
              : grid2D(i_ncols, i_nrows, IOUtils::nodata), llcorner(i_llcorner),
                cellsize(i_cellsize), ur_lat(IOUtils::nodata), ur_lon(IOUtils::nodata), isLatLon(false) {}

Grid2DObject::Grid2DObject(const double& i_cellsize, const Coords& i_llcorner, const Array2D<double>& i_grid2D)
              : grid2D(i_grid2D), llcorner(i_llcorner), cellsize(i_cellsize), ur_lat(IOUtils::nodata), ur_lon(IOUtils::nodata), isLatLon(false) {}

Grid2DObject::Grid2DObject(const size_t& i_ncols, const size_t& i_nrows,
                           const double& i_cellsize, const Coords& i_llcorner, const double& init)
              : grid2D(i_ncols, i_nrows, init), llcorner(i_llcorner), cellsize(i_cellsize), ur_lat(IOUtils::nodata), ur_lon(IOUtils::nodata), isLatLon(false) {}

Grid2DObject::Grid2DObject(const Grid2DObject& i_grid, const double& init)
              : grid2D(i_grid.grid2D.getNx(), i_grid.grid2D.getNy(), init),
                llcorner(i_grid.llcorner), cellsize(i_grid.cellsize), ur_lat(i_grid.ur_lat), ur_lon(i_grid.ur_lon), isLatLon(i_grid.isLatLon) {}

Grid2DObject::Grid2DObject(const Grid2DObject& i_grid2Dobj, const size_t& i_nx, const size_t& i_ny,
                           const size_t& i_ncols, const size_t& i_nrows)
              : grid2D(i_grid2Dobj.grid2D, i_nx,i_ny, i_ncols,i_nrows), llcorner(i_grid2Dobj.llcorner), cellsize(i_grid2Dobj.cellsize),
                ur_lat(i_grid2Dobj.ur_lat), ur_lon(i_grid2Dobj.ur_lon), isLatLon(i_grid2Dobj.isLatLon)
{
	//we take the previous corner (so we use the same projection parameters)
	//and we shift it by the correct X and Y distance
	//llcorner = i_grid2Dobj.llcorner;
	if ( (llcorner.getEasting()!=IOUtils::nodata) && (llcorner.getNorthing()!=IOUtils::nodata) ) {
		llcorner.setXY( llcorner.getEasting()+static_cast<double>(i_nx)*i_grid2Dobj.cellsize,
		                llcorner.getNorthing()+static_cast<double>(i_ny)*i_grid2Dobj.cellsize, IOUtils::nodata);
	}
}

bool Grid2DObject::gridify(std::vector<Coords>& vec_points, const bool& keep_invalid) const 
{
	bool status=true;
	std::vector<Coords>::iterator v_Itr = vec_points.begin();
	while ( v_Itr != vec_points.end() ) {
		if ( gridify(*v_Itr)==false ) {
			if (!keep_invalid) v_Itr = vec_points.erase(v_Itr);
			else ++v_Itr;
			status=false;
		} else {
			++v_Itr;
		}
	}

	return status;
}

bool Grid2DObject::gridify(std::vector<StationData>& vec_points, const bool& keep_invalid) const 
{
	bool status=true;
	std::vector<StationData>::iterator v_Itr = vec_points.begin();
	while ( v_Itr != vec_points.end() ) {
		if ( gridify(v_Itr->position)==false ) {
			if (!keep_invalid) v_Itr = vec_points.erase(v_Itr);
			else ++v_Itr;
			status=false;
		} else {
			++v_Itr;
		}
	}

	return status;
}

bool Grid2DObject::gridify(Coords& point) const 
{
	std::string proj_type, proj_args;
	point.getProj(proj_type, proj_args);
	if (proj_type=="NULL") {
		//if the projection was "NULL", we set it to the grid's
		point.copyProj(llcorner);
	}

	if (point.getGridI()!=IOUtils::inodata && point.getGridJ()!=IOUtils::inodata) {
		//we need to compute (easting,northing) and (lat,lon)
		return( grid_to_WGS84(point) );
	} else {
		//we need to compute (i,j)
		return( WGS84_to_grid(point) );
	}
}

bool Grid2DObject::grid_to_WGS84(Coords& point) const
{
	int ii = point.getGridI(), jj = point.getGridJ();
	if (ii==IOUtils::inodata || jj==IOUtils::inodata) {
		//the point is invalid (outside the grid or contains nodata)
		point.setGridIndex(IOUtils::inodata, IOUtils::inodata, IOUtils::inodata, false); //mark the point as invalid
		return false;
	}

	const int ncols = (signed)getNx();
	const int nrows = (signed)getNy();

	if (ii>ncols || ii<0 || jj>nrows || jj<0) {
		//the point is outside the grid, we reset the indices to the closest values
		//still fitting in the grid and return an error
		if (ii<0) ii=0;
		if (jj<0) jj=0;
		if (ii>ncols) ii=ncols;
		if (jj>nrows) jj=nrows;
		point.setGridIndex(ii, jj, IOUtils::inodata, false); //mark the point as invalid
		return false;
	}
	
	if (ur_lat!=IOUtils::nodata && ur_lon!=IOUtils::nodata) { //use lat/lon extraction if possible
		//remember that cellsize = (ur_lon - llcorner.getLon()) / (getNx()-1.)
		const double lon = point.getLon() + ii * (ur_lon - llcorner.getLon()) / static_cast<double>(getNx()-1);
		const double lat = point.getLat() + jj * (ur_lat - llcorner.getLat()) / static_cast<double>(getNy()-1);
		point.setLatLon(lat, lon, IOUtils::nodata);
	} else {
		//easting and northing in the grid's projection
		const double easting = ((double)ii) * cellsize + llcorner.getEasting();
		const double northing = ((double)jj) * cellsize + llcorner.getNorthing();
		
		if (point.isSameProj(llcorner)==true) {
			//same projection between the grid and the point -> precise, simple and efficient arithmetics
			point.setXY(easting, northing, IOUtils::nodata);
		} else {
			//projections are different, so we have to do an intermediate step...
			Coords tmp_proj;
			tmp_proj.copyProj(point); //making a copy of the original projection
			point.copyProj(llcorner); //taking the grid's projection
			point.setXY(easting, northing, IOUtils::nodata);
			point.copyProj(tmp_proj); //back to the original projection -> reproject the coordinates
		}
	}

	point.setGridIndex(ii, jj, IOUtils::inodata, true); //mark the point as valid
	return true;
}

bool Grid2DObject::WGS84_to_grid(Coords& point) const
{
	if (point.getLat()==IOUtils::nodata || point.getLon()==IOUtils::nodata) {
			//if the point is invalid, there is nothing we can do
			point.setGridIndex(IOUtils::inodata, IOUtils::inodata, IOUtils::inodata, false); //mark the point as invalid
			return false;
	}

	bool status=true;
	int ii, jj;
	
	if (ur_lat!=IOUtils::nodata && ur_lon!=IOUtils::nodata) { //use lat/lon extraction if possible
		//remember that cellsize = (ur_lon - llcorner.getLon()) / (getNx()-1.)
		ii = (int)floor( (point.getLon() - llcorner.getLon()) / (ur_lon - llcorner.getLon()) * static_cast<double>(getNx()-1)); //round to lowest
		jj = (int)floor( (point.getLat() - llcorner.getLat()) / (ur_lat - llcorner.getLat()) * static_cast<double>(getNy()-1));
	} else {
		if (point.isSameProj(llcorner)==true) {
			//same projection between the grid and the point -> precise, simple and efficient arithmetics
			ii = (int)floor( (point.getEasting()-llcorner.getEasting()) / cellsize); //round to lowest
			jj = (int)floor( (point.getNorthing()-llcorner.getNorthing()) / cellsize);
		} else {
			//projections are different, so we have to do an intermediate step...
			Coords tmp_point(point);
			tmp_point.copyProj(llcorner); //getting the east/north coordinates in the grid's projection
			ii = (int)floor( (tmp_point.getEasting()-llcorner.getEasting()) / cellsize); //round to lowest
			jj = (int)floor( (tmp_point.getNorthing()-llcorner.getNorthing()) / cellsize);
		}
	}

	//checking that the calculated indices fit in the grid2D
	//and giving them the closest value within the grid if not.
	if (ii<0) {
		ii=0;
		status=false;
	}
	if (ii>=(signed)getNx()) {
		ii=(signed)getNx();
		status=false;
	}
	if (jj<0) {
		jj=0;
		status=false;
	}
	if (jj>=(signed)getNy()) {
		jj=(signed)getNy();
		status=false;
	}

	point.setGridIndex(ii, jj, IOUtils::inodata, status); //mark as valid or invalid according to status
	return status;
}

void Grid2DObject::set(const size_t& i_ncols, const size_t& i_nrows,
                       const double& i_cellsize, const Coords& i_llcorner)
{
	grid2D.resize(i_ncols, i_nrows, IOUtils::nodata);
	setValues(i_cellsize, i_llcorner);
}

void Grid2DObject::set(const size_t& i_ncols, const size_t& i_nrows,
                       const double& i_cellsize, const Coords& i_llcorner, const double& init)
{
	grid2D.resize(i_ncols, i_nrows, init);
	setValues(i_cellsize, i_llcorner);
}

void Grid2DObject::set(const double& i_cellsize, const Coords& i_llcorner, const Array2D<double>& i_grid2D)
{
	setValues(i_cellsize, i_llcorner);
	grid2D = i_grid2D;
}

void Grid2DObject::set(const Grid2DObject& i_grid, const double& init)
{
	setValues(i_grid.cellsize, i_grid.llcorner);
	grid2D.resize(i_grid.grid2D.getNx(), i_grid.grid2D.getNy(), init);
	isLatLon = i_grid.isLatLon;
	ur_lat = i_grid.ur_lat;
	ur_lon = i_grid.ur_lon;
}

void Grid2DObject::rescale(const double& i_cellsize)
{
	if (grid2D.getNx()==0 || grid2D.getNy()==0)
		throw InvalidArgumentException("Can not rescale an empty grid!", AT);
	
	const double factor_x = cellsize / i_cellsize;
	const double factor_y = cellsize / i_cellsize;
	grid2D = LibResampling2D::Bilinear(grid2D, factor_x, factor_y);
	cellsize = i_cellsize;
}

void Grid2DObject::size(size_t& o_ncols, size_t& o_nrows) const {
	o_ncols = getNx();
	o_nrows = getNy();
}

size_t Grid2DObject::size() const {
	return grid2D.size();
}

size_t Grid2DObject::getNx() const {
	return grid2D.getNx();
}

size_t Grid2DObject::getNy() const {
	return grid2D.getNy();
}


void Grid2DObject::clear() {
	grid2D.clear();
}

bool Grid2DObject::empty() const {
	return (grid2D.getNx()==0 && grid2D.getNy()==0);
}

void Grid2DObject::setValues(const double& i_cellsize, const Coords& i_llcorner)
{
	cellsize = i_cellsize;
	llcorner = i_llcorner;
}

bool Grid2DObject::isSameGeolocalization(const Grid2DObject& target) const
{
	const bool isSameLoc = grid2D.getNx()==target.grid2D.getNx() &&
	                     grid2D.getNy()==target.grid2D.getNy() &&
	                     llcorner==target.llcorner &&
	                     (cellsize==target.cellsize 
	                     || (ur_lat==target.ur_lat && ur_lon==target.ur_lon));

	return isSameLoc;
}

bool Grid2DObject::clusterization(const std::vector<double>& thresholds, const std::vector<double>& ids)
{
	if (thresholds.empty()==0) {
		throw IOException("Can't start clusterization, cluster definition list is empty", AT);
	}
	if ((thresholds.size()+1) != ids.size()) {
		throw IOException("Can't start clusterization, cluster definition list doesnt fit id definition list", AT);
	}
	const size_t nscl = thresholds.size();
	for (size_t jj = 0; jj< grid2D.size(); jj++){
		const double& val = grid2D(jj);
		if (val!=IOUtils::nodata){
			size_t i = 0;
			for ( ;i<nscl; i++)
				if (thresholds[i] >= val)
					break;
			grid2D(jj) = ids[i];
		}
	}
	return true;
}

double Grid2DObject::calculate_XYcellsize(const std::vector<double>& vecX, const std::vector<double>& vecY)
{
	const double distanceX = fabs(vecX.front() - vecX.back());
	const double distanceY = fabs(vecY.front() - vecY.back());

	//round to 1cm precision for numerical stability (and size()-1 because of the intervals thing)
	const double cellsize_x = static_cast<double>(mio::Optim::round( distanceX / static_cast<double>(vecX.size()-1)*100. )) / 100.;
	const double cellsize_y = static_cast<double>(mio::Optim::round( distanceY / static_cast<double>(vecY.size()-1)*100. )) / 100.;

	return std::min(cellsize_x, cellsize_y);
}

double Grid2DObject::calculate_cellsize(const double& i_ur_lat, const double& i_ur_lon) const
{
	double alpha;
	const double cntr_lat = .5*(llcorner.getLat() + i_ur_lat);
	const double cntr_lon = .5*(llcorner.getLon() + i_ur_lon);
	const double distanceX = mio::CoordsAlgorithms::VincentyDistance(cntr_lat, llcorner.getLon(), cntr_lat, i_ur_lon, alpha);
	const double distanceY = mio::CoordsAlgorithms::VincentyDistance(llcorner.getLat(), cntr_lon, i_ur_lat, cntr_lon, alpha);

	//round to 1cm precision for numerical stability (and size()-1 because of the intervals thing)
	const double cellsize_x = static_cast<double>(mio::Optim::round( distanceX / static_cast<double>(getNx()-1)*100. )) / 100.;
	const double cellsize_y = static_cast<double>(mio::Optim::round( distanceY / static_cast<double>(getNy()-1)*100. )) / 100.;
	
	return std::min(cellsize_x, cellsize_y);
}

void Grid2DObject::reproject()
{
	cellsize = calculate_cellsize(ur_lat, ur_lon);
}

double& Grid2DObject::operator()(const size_t& ix, const size_t& iy) {
	return grid2D(ix,iy);
}

double Grid2DObject::operator()(const size_t& ix, const size_t& iy) const {
	return grid2D(ix,iy);
}

double& Grid2DObject::operator()(const size_t& i) {
	return grid2D(i);
}

double Grid2DObject::operator()(const size_t& i) const {
	return grid2D(i);
}

const std::string Grid2DObject::toString() const {
	std::ostringstream os;
	os << "<Grid2DObject>\n";
	os << llcorner.toString();
	os << grid2D.getNx() << " x " << grid2D.getNy() << " @ " << cellsize << "m\n";
	if (isLatLon) os << "isLatLon=true" << " ur_lat=" << ur_lat << " ur_lon=" << ur_lon << "\n";
	os << grid2D.toString();
	os << "</Grid2DObject>\n";
	return os.str();
}

std::ostream& operator<<(std::ostream& os, const Grid2DObject& grid) {
	os.write(reinterpret_cast<const char*>(&grid.cellsize), sizeof(grid.cellsize));
	os.write(reinterpret_cast<const char*>(&grid.ur_lat), sizeof(grid.ur_lat));
	os.write(reinterpret_cast<const char*>(&grid.ur_lon), sizeof(grid.ur_lon));
	os.write(reinterpret_cast<const char*>(&grid.isLatLon), sizeof(grid.isLatLon));
	os << grid.llcorner;
	os << grid.grid2D;
	return os;
}

std::istream& operator>>(std::istream& is, Grid2DObject& grid) {
	is.read(reinterpret_cast<char*>(&grid.cellsize), sizeof(grid.cellsize));
	is.read(reinterpret_cast<char*>(&grid.ur_lat), sizeof(grid.ur_lat));
	is.read(reinterpret_cast<char*>(&grid.ur_lon), sizeof(grid.ur_lon));
	is.read(reinterpret_cast<char*>(&grid.isLatLon), sizeof(grid.isLatLon));
	is >> grid.llcorner;
	is >> grid.grid2D;
	return is;
}

} //namespace
