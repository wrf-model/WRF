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
#ifndef GRID3DOBJECT_H
#define GRID3DOBJECT_H

#include <meteoio/dataClasses/Coords.h>
#include <meteoio/dataClasses/Array3D.h>
#include <meteoio/dataClasses/Grid2DObject.h>

#include <iostream>

namespace mio {

/**
 * @class Grid3DObject
 * @brief A class to represent 3D Grids. Typical application: wind field
 *
 * @ingroup data_str
 * @author Thomas Egger
 * @date   2009-07-20
 */

class Grid3DObject{
	public:
		typedef struct GRID_POINT_3D { //TODO: this potentially conflicts with the definition in Grid2DObject
			size_t ix; ///<grid index along X
			size_t iy; ///<grid index along Y
			size_t iz; ///<grid index along Z
		} grid_point_3d;

		double& operator ()(const size_t& ix, const size_t& iy, const size_t& iz);
		double operator ()(const size_t& ix, const size_t& iy, const size_t& iz) const;
		double& operator ()(const size_t& i);
		double operator ()(const size_t& i) const;

		const std::string toString() const;
		friend std::ostream& operator<<(std::ostream& os, const Grid3DObject& grid);
		friend std::istream& operator>>(std::istream& is, Grid3DObject& grid);

		/**
		* Default constructor.
		* Initializes all variables to 0, except nodata, which is initialized to -9999.0
		*/
		Grid3DObject();

		/**
		* A constructor that can be used to create a Grid3DObject that is contained in the
		* one passed as i_grid3Dobj argument. The resulting Grid3DObject is a by value copy of
		* a subspace of the space spanned by the i_grid3Dobj
		*/
		Grid3DObject(const Grid3DObject& i_grid3D,
		             const size_t& i_nx, const size_t& i_ny, const size_t& i_nz,
		             const size_t& i_ncols, const size_t& i_nrows, const size_t& i_ndepths);

		Grid3DObject(const size_t& ncols, const size_t& nrows, const size_t& ndepths,
		             const double& cellsize, const Coords& i_llcorner);

		Grid3DObject(const size_t& ncols, const size_t& nrows, const size_t& ndepths,
		             const double& cellsize, const Coords& i_llcorner, const double& init);

		Grid3DObject(const Grid3DObject& i_grid, const double& init);

		Grid3DObject(const double& cellsize, const Coords& i_llcorner, const Array3D<double>& grid3D);

		/**
		* @brief Set all variables in one go.
		* @param ncols number of colums in the grid3D (1st dimension)
		* @param nrows number of rows in the grid3D (2nd dimension)
		* @param depths number of depth in the grid3D (3rd dimension)
		* @param cellsize value for cellsize in grid3D
		* @param i_llcorner lower left corner coordinates
		*/
		void set(const size_t& ncols, const size_t& nrows, const size_t& depths,
		         const double& cellsize, const Coords& i_llcorner);
		/**
		* @brief Set all variables in one go. Notably the member grid3D of type Array3D<double>
		* will be destroyed and recreated to size ncols x nrows.
		*
		* @param cellsize value for cellsize in grid3D
		* @param i_llcorner lower left corner coordinates
		* @param grid3D_in grid to be copied by value
		*/
		void set(const double& cellsize, const Coords& i_llcorner, const Array3D<double>& grid3D_in);

		void set(const size_t& ncols, const size_t& nrows, const size_t& ndepths,
		         const double& cellsize, const Coords& i_llcorner, const double& init);

		void set(const Grid3DObject& i_grid, const double& init);

		void size(size_t& o_ncols, size_t& o_nrows, size_t& o_ndepths) const;
		size_t size() const;
		size_t getNx() const;
		size_t getNy() const;
		size_t getNz() const;

		/**
		* @brief deletes the data, but keeps geolocalization
		*/
		void clear();

		/**
		* @brief Check if a grid does not contain any data (but it can contain geolocalization)
		* @return true if the grid is 0x0
		*/
		bool empty() const;

		/**
		* @brief Compute the positional parameters that are not already known
		* This means that the Coords::point object that is given either contains geographic coordinates or
		* grid indices. This method will calculate the missing ones (so that (i,j,k) match with (lat,lon,alt)
		* and (east,north,alt).
		* @param point coordinate to convert
		* @return false if the given point was invalid or outside the grid (sets (i,j) to closest values within the grid)
		*/
		bool gridify(Coords& point) const;

		/**
		* @brief Compute the positional parameters that are not already known
		* This means that the Coords::point object that is given either contains geographic coordinates or
		* grid indices. This method will calculate the missing ones (so that (i,j) match with (lat,lon)
		* and (east,north)). Any point that is either invalid or outside the grid is removed from the vector.
		* If the given point had a "NULL" projection, it will be set to the grid's.
		* @param vec_points vector containing the coordinates to convert
		* @param keep_invalid keep invalid coordinates? (default: false)
		* @return false if invalid or external points had to be removed
		*/
		bool gridify(std::vector<Coords>& vec_points, const bool& keep_invalid=false) const;

		/**
		* @brief check if the current Grid3DObject has the same geolocalization attributes
		* as another Grid3DObject. The grid coordinates (xllcorner & yllcorner) are NOT
		* checked as these might be tweaked for convenience (like between input grid and local grid)
		* @param target grid to compare to
		* @return true if same geolocalization
		*/
		bool isSameGeolocalization(const Grid3DObject& target) const;

		/**
		* @brief Extract a 2D grid for a given height
		* @param i_z layer to extract
		* @param layer extracted 2D grid
		*/
		void extractLayer(const size_t& i_z, Grid2DObject& layer);

		Grid3DObject& operator=(const Grid3DObject&); ///<Assignement operator
		Grid3DObject& operator=(const double& value); ///<Assignement operator

		Grid3DObject& operator+=(const double& rhs);
		const Grid3DObject operator+(const double& rhs) const;
		Grid3DObject& operator+=(const Grid3DObject& rhs);
		const Grid3DObject operator+(const Grid3DObject& rhs) const;

		Grid3DObject& operator-=(const double& rhs);
		const Grid3DObject operator-(const double& rhs) const;
		Grid3DObject& operator-=(const Grid3DObject& rhs);
		const Grid3DObject operator-(const Grid3DObject& rhs) const;

		Grid3DObject& operator*=(const double& rhs);
		const Grid3DObject operator*(const double& rhs) const;
		Grid3DObject& operator*=(const Grid3DObject& rhs);
		const Grid3DObject operator*(const Grid3DObject& rhs) const;

		Grid3DObject& operator/=(const double& rhs);
		const Grid3DObject operator/(const double& rhs) const;
		Grid3DObject& operator/=(const Grid3DObject& rhs);
		const Grid3DObject operator/(const Grid3DObject& rhs) const;

		bool operator==(const Grid3DObject& in) const; ///<Operator that tests for equality
		bool operator!=(const Grid3DObject& in) const; ///<Operator that tests for inequality

		Array3D<double> grid3D;
		std::vector<double> z; ///> Vector of depths
		Coords llcorner;
		double cellsize;
		bool z_is_absolute; ///> Are z coordinates absolute or relative to a DEM?

 protected:
		void setValues(const double& cellsize, const Coords& i_llcorner);

		/**
		* @brief Converts WGS84 coordinates into grid coordinates (i,j)
		* @param point coordinate to convert
		* @return false if the given point was outside the grid (sets (i,j) to closest values within the grid)
		*/
		bool WGS84_to_grid(Coords point) const;

		/**
		* @brief Converts grid coordinates (i,j) into WGS84 coordinates
		* @param point coordinate to convert
		* @return false if the given point was invalid (outside the grid or nodata and if possible sets (i,j) to closest values within the grid)
		*/
		bool grid_to_WGS84(Coords& point) const;
};
} //end namespace

#endif
