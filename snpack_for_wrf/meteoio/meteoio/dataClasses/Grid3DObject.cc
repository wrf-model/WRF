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
#include <meteoio/dataClasses/Grid3DObject.h>
#include <meteoio/IOExceptions.h>
#include <cmath>

using namespace std;

namespace mio {

Grid3DObject& Grid3DObject::operator=(const Grid3DObject& source) {
	if (this != &source) {
		grid3D = source.grid3D;
		cellsize = source.cellsize;
 		llcorner = source.llcorner;
		z = source.z;
		z_is_absolute = source.z_is_absolute;
	}
	return *this;
}

Grid3DObject& Grid3DObject::operator=(const double& value) {
	grid3D = value;
	return *this;
}

Grid3DObject& Grid3DObject::operator+=(const double& rhs) {
	grid3D += rhs;
	return *this;
}

const Grid3DObject Grid3DObject::operator+(const double& rhs) const {
	Grid3DObject result = *this;
	result.grid3D += rhs;
	return result;
}

Grid3DObject& Grid3DObject::operator+=(const Grid3DObject& rhs) {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	grid3D += rhs.grid3D;
	return *this;
}

const Grid3DObject Grid3DObject::operator+(const Grid3DObject& rhs) const {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	Grid3DObject result(*this);
	result.grid3D += rhs.grid3D;
	return result;
}

Grid3DObject& Grid3DObject::operator-=(const double& rhs) {
	grid3D -= rhs;
	return *this;
}

const Grid3DObject Grid3DObject::operator-(const double& rhs) const {
	Grid3DObject result(*this);
	result.grid3D -= rhs;
	return result;
}

Grid3DObject& Grid3DObject::operator-=(const Grid3DObject& rhs) {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	grid3D -= rhs.grid3D;
	return *this;
}

const Grid3DObject Grid3DObject::operator-(const Grid3DObject& rhs) const {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	Grid3DObject result(*this);
	result.grid3D -= rhs.grid3D;
	return result;
}

Grid3DObject& Grid3DObject::operator*=(const double& rhs) {
	grid3D *= rhs;
	return *this;
}

const Grid3DObject Grid3DObject::operator*(const double& rhs) const {
	Grid3DObject result(*this);
	result.grid3D *= rhs;
	return result;
}

Grid3DObject& Grid3DObject::operator*=(const Grid3DObject& rhs) {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	grid3D *= rhs.grid3D;
	return *this;
}

const Grid3DObject Grid3DObject::operator*(const Grid3DObject& rhs) const {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	Grid3DObject result(*this);
	result.grid3D *= rhs.grid3D;
	return result;
}

Grid3DObject& Grid3DObject::operator/=(const double& rhs) {
	grid3D /= rhs;
	return *this;
}

const Grid3DObject Grid3DObject::operator/(const double& rhs) const {
	Grid3DObject result(*this);
	result.grid3D /= rhs;
	return result;
}

Grid3DObject& Grid3DObject::operator/=(const Grid3DObject& rhs) {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	grid3D /= rhs.grid3D;
	return *this;
}

const Grid3DObject Grid3DObject::operator/(const Grid3DObject& rhs) const {
	if (!isSameGeolocalization(rhs))
		throw InvalidArgumentException("[E] grids must have the same geolocalization in order to do arithmetic operations!", AT);
	Grid3DObject result(*this);
	result.grid3D /= rhs.grid3D;
	return result;
}

bool Grid3DObject::operator==(const Grid3DObject& in) const {
	return (isSameGeolocalization(in) && grid3D==in.grid3D);
}

bool Grid3DObject::operator!=(const Grid3DObject& in) const {
	return !(*this==in);
}

Grid3DObject::Grid3DObject()
             : grid3D(), z(), llcorner(), cellsize(0.), z_is_absolute(true) {}

Grid3DObject::Grid3DObject(const Grid3DObject& i_grid3D,
                           const size_t& i_nx, const size_t& i_ny, const size_t& i_nz,
                           const size_t& i_nwidths, const size_t& i_nheights, const size_t& i_ndepths)
      : grid3D(i_grid3D.grid3D, i_nx,i_ny,i_nz, i_nwidths,i_nheights,i_ndepths), z(i_grid3D.z), llcorner(i_grid3D.llcorner),
        cellsize(i_grid3D.cellsize), z_is_absolute(true)
{
	//we take the previous corner (so we use the same projection parameters)
	//and we shift it by the correct X and Y distance
	if ( (llcorner.getEasting()!=IOUtils::nodata) && (llcorner.getNorthing()!=IOUtils::nodata) ) {
		llcorner.setXY( llcorner.getEasting()+static_cast<double>(i_nx)*i_grid3D.cellsize,
		                llcorner.getNorthing()+static_cast<double>(i_ny)*i_grid3D.cellsize,
		                llcorner.getAltitude()+static_cast<double>(i_nz)*i_grid3D.cellsize );
	}
}

Grid3DObject::Grid3DObject(const size_t& i_ncols, const size_t& i_nrows, const size_t& i_ndepths,
                           const double& i_cellsize, const Coords& i_llcorner)
              : grid3D(i_ncols, i_nrows, i_ndepths, IOUtils::nodata), z(), llcorner(i_llcorner),
                cellsize(i_cellsize), z_is_absolute(true) {}

Grid3DObject::Grid3DObject(const size_t& i_ncols, const size_t& i_nrows, const size_t& i_ndepths,
                           const double& i_cellsize, const Coords& i_llcorner, const double& init)
              : grid3D(i_ncols, i_nrows, i_ndepths, init), z(), llcorner(i_llcorner),
                cellsize(i_cellsize), z_is_absolute(true) {}

Grid3DObject::Grid3DObject(const Grid3DObject& i_grid, const double& init)
              : grid3D(i_grid.grid3D.getNx(), i_grid.grid3D.getNy(), i_grid.grid3D.getNz(), init), z(i_grid.z), llcorner(i_grid.llcorner),
                cellsize(i_grid.cellsize), z_is_absolute(i_grid.z_is_absolute) {}

Grid3DObject::Grid3DObject(const double& i_cellsize, const Coords& i_llcorner, const Array3D<double>& i_grid3D)
              : grid3D(i_grid3D), z(), llcorner(i_llcorner),
                cellsize(i_cellsize), z_is_absolute(true) {}

                
bool Grid3DObject::gridify(std::vector<Coords>& vec_points, const bool& keep_invalid) const 
{
	bool status=true;

	std::vector<Coords>::iterator v_Itr = vec_points.begin();
	while ( v_Itr != vec_points.end() ) {
		if ( gridify(*v_Itr)==false ) {
			if (!keep_invalid) v_Itr = vec_points.erase(v_Itr);
			status=false;
		} else {
			++v_Itr;
		}
	}

	return status;
}

bool Grid3DObject::gridify(Coords& point) const
{
	std::string proj_type, proj_args;
	point.getProj(proj_type, proj_args);
	if (proj_type=="NULL") {
		//if the projection was "NULL", we set it to the grid's
		point.copyProj(llcorner);
	}

	if (point.getGridI()!=IOUtils::inodata && point.getGridJ()!=IOUtils::inodata && point.getGridK()!=IOUtils::inodata) {
		//we need to compute (easting,northing) and (lat,lon) and altitude
		return( grid_to_WGS84(point) );
	} else {
		//we need to compute (i,j,k)
		return( WGS84_to_grid(point) );
	}
}

bool Grid3DObject::grid_to_WGS84(Coords& point) const
{
	int i=point.getGridI(), j=point.getGridJ(), k=point.getGridK();

	if (i==IOUtils::inodata || j==IOUtils::inodata || k==IOUtils::inodata) {
		//the point is invalid (outside the grid or contains nodata)
		point.setGridIndex(IOUtils::inodata, IOUtils::inodata, IOUtils::inodata, false); //mark the point as invalid
		return false;
	}

	const int ncols = (signed)getNx();
	const int nrows = (signed)getNy();
	const int ndepths = (signed)getNz();
	if (i>ncols || i<0 || j>nrows || j<0 || k>ndepths || k<0) {
		//the point is outside the grid, we reset the indices to the closest values
		//still fitting in the grid and return an error
		if (i<0) i=0;
		if (j<0) j=0;
		if (k<0) k=0;
		if (i>ncols) i=ncols;
		if (j>nrows) j=nrows;
		if (k>ndepths) k=ndepths;
		point.setGridIndex(i, j, k, false); //mark the point as invalid
		return false;
	}

	//easting and northing in the grid's projection
	const double easting = ((double)i) * cellsize + llcorner.getEasting();
	const double northing = ((double)j) * cellsize + llcorner.getNorthing();
	const double altitude = ((double)k) * cellsize + llcorner.getAltitude();

	if (point.isSameProj(llcorner)==true) {
		//same projection between the grid and the point -> precise, simple and efficient arithmetics
		point.setXY(easting, northing, altitude);
	} else {
		//projections are different, so we have to do an intermediate step...
		Coords tmp_proj;
		tmp_proj.copyProj(point); //making a copy of the original projection
		point.copyProj(llcorner); //taking the grid's projection
		point.setXY(easting, northing, altitude);
		point.copyProj(tmp_proj); //back to the original projection -> reproject the coordinates
	}
	
	point.setGridIndex(i, j, k, true); //mark the point as valid
	return true;
}

bool Grid3DObject::WGS84_to_grid(Coords point) const
{
	if (point.getLat()==IOUtils::nodata || point.getLon()==IOUtils::nodata || point.getAltitude()==IOUtils::nodata) {
		//if the point is invalid, there is nothing we can do
		point.setGridIndex(IOUtils::inodata, IOUtils::inodata, IOUtils::inodata, false); //mark the point as invalid
		return false;
	}

	int i,j,k;

	if (point.isSameProj(llcorner)==true) {
		//same projection between the grid and the point -> precise, simple and efficient arithmetics
		i = (int)floor( (point.getEasting()-llcorner.getEasting()) / cellsize );
		j = (int)floor( (point.getNorthing()-llcorner.getNorthing()) / cellsize );
		k = (int)floor( (point.getAltitude()-llcorner.getAltitude()) / cellsize );
	} else {
		//projections are different, so we have to do an intermediate step...
		Coords tmp_point(point);
		tmp_point.copyProj(llcorner); //getting the east/north coordinates in the grid's projection
		i = (int)floor( (tmp_point.getEasting()-llcorner.getEasting()) / cellsize );
		j = (int)floor( (tmp_point.getNorthing()-llcorner.getNorthing()) / cellsize );
		k = (int)floor( (point.getAltitude()-llcorner.getAltitude()) / cellsize );
	}

	bool status=true;
	//checking that the calculated indices fit in the grid2D
	//and giving them the closest value within the grid if not.
	if (i<0) {
		i=0;
		status=false;
	}
	if (i>=(signed)grid3D.getNx()) {
		i=(signed)grid3D.getNx();
		status=false;
	}
	if (j<0) {
		j=0;
		status=false;
	}
	if (j>=(signed)grid3D.getNy()) {
		j=(signed)grid3D.getNy();
		status=false;
	}
	if (k<0) {
		k=0;
		status=false;
	}
	if (k>=(signed)grid3D.getNz()) {
		k=(signed)grid3D.getNz();
		status=false;
	}

	point.setGridIndex(i, j, k, status);  //mark as valid or invalid according to status
	return status;
}

void Grid3DObject::set(const size_t& i_ncols, const size_t& i_nrows, const size_t& i_ndepths,
                       const double& i_cellsize, const Coords& i_llcorner)
{
	grid3D.resize(i_ncols, i_nrows, i_ndepths, IOUtils::nodata);
	setValues(i_cellsize, i_llcorner);
}

void Grid3DObject::set(const size_t& i_ncols, const size_t& i_nrows, const size_t& i_ndepths,
                       const double& i_cellsize, const Coords& i_llcorner, const double& init)
{
	grid3D.resize(i_ncols, i_nrows, i_ndepths, init);
	setValues(i_cellsize, i_llcorner);
}

void Grid3DObject::set(const double& i_cellsize, const Coords& i_llcorner, const Array3D<double>& i_grid3D)
{
	setValues(i_cellsize, i_llcorner);
	grid3D = i_grid3D; //copy by value
}

void Grid3DObject::set(const Grid3DObject& i_grid, const double& init)
{
	setValues(i_grid.cellsize, i_grid.llcorner);
	grid3D.resize(i_grid.grid3D.getNx(), i_grid.grid3D.getNy(), i_grid.grid3D.getNz(), init);
	z = i_grid.z;
	z_is_absolute = i_grid.z_is_absolute;
}

void Grid3DObject::size(size_t& o_ncols, size_t& o_nrows, size_t& o_ndepths) const
{
	o_ncols = grid3D.getNx();
	o_nrows = grid3D.getNy();
	o_ndepths = grid3D.getNz();
}

size_t Grid3DObject::size() const {
	return grid3D.size();
}

size_t Grid3DObject::getNx() const {
	return grid3D.getNx();
}

size_t Grid3DObject::getNy() const {
	return grid3D.getNy();
}

size_t Grid3DObject::getNz() const {
	return grid3D.getNz();
}

void Grid3DObject::clear() {
	grid3D.clear();
}

bool Grid3DObject::empty() const {
	return (grid3D.getNx()==0 && grid3D.getNy()==0 && grid3D.getNz()==0);
}

void Grid3DObject::setValues(const double& i_cellsize, const Coords& i_llcorner)
{
	cellsize = i_cellsize;
	llcorner = i_llcorner;
}

bool Grid3DObject::isSameGeolocalization(const Grid3DObject& target) const
{
	if ( grid3D.getNx()==target.grid3D.getNx() &&
	    grid3D.getNy()==target.grid3D.getNy() &&
	    grid3D.getNz()==target.grid3D.getNz() &&
	    cellsize==target.cellsize &&
	    llcorner==target.llcorner) {
		return true;
	} else {
		return false;
	}
}

void Grid3DObject::extractLayer(const size_t& i_z, Grid2DObject& layer)
{
	const size_t ncols = grid3D.getNx();
	const size_t nrows = grid3D.getNy();
	layer.set(ncols, nrows, cellsize, llcorner);
	for (size_t jj=0; jj<nrows; jj++) {
		for (size_t ii=0; ii<ncols; ii++) {
			layer(ii,jj) = grid3D(ii,jj,i_z);
		}
	}
}

double& Grid3DObject::operator()(const size_t& ix, const size_t& iy, const size_t& iz) {
	return grid3D(ix,iy,iz);
}

double Grid3DObject::operator()(const size_t& ix, const size_t& iy, const size_t& iz) const {
	return grid3D(ix,iy,iz);
}

double& Grid3DObject::operator()(const size_t& i) {
	return grid3D(i);
}

double Grid3DObject::operator()(const size_t& i) const {
	return grid3D(i);
}

const std::string Grid3DObject::toString() const {
	std::ostringstream os;
	os << "<Grid3DObject>\n";
	os << llcorner.toString();
	os << grid3D.getNx() << " x " << grid3D.getNy()  << " x " << grid3D.getNz() << " @ " << cellsize << "m\n";
	os << grid3D.toString();
	os << "</Grid3DObject>\n";
	return os.str();
}

std::ostream& operator<<(std::ostream& os, const Grid3DObject& grid) {
	os.write(reinterpret_cast<const char*>(&grid.cellsize), sizeof(grid.cellsize));
	os.write(reinterpret_cast<const char*>(&grid.z_is_absolute), sizeof(grid.z_is_absolute));

	const size_t s_z = grid.z.size();
	os.write(reinterpret_cast<const char*>(&s_z), sizeof(size_t));
	os.write(reinterpret_cast<const char*>(&grid.z[0]), s_z*sizeof(grid.z[0]));

	os << grid.llcorner;
	os << grid.grid3D;
	return os;
}

std::istream& operator>>(std::istream& is, Grid3DObject& grid) {
	is.read(reinterpret_cast<char*>(&grid.cellsize), sizeof(grid.cellsize));
	is.read(reinterpret_cast<char*>(&grid.z_is_absolute), sizeof(grid.z_is_absolute));

	size_t s_z;
	is.read(reinterpret_cast<char*>(&s_z), sizeof(size_t));
	grid.z.resize(s_z);
	is.read(reinterpret_cast<char*>(&grid.z[0]), s_z*sizeof(grid.z[0]));

	is >> grid.llcorner;
	is >> grid.grid3D;
	return is;
}

} //end namespace
