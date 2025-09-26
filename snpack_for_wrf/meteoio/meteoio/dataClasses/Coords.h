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
#ifndef COORDS_H
#define COORDS_H

#include <string>
#include <iostream>

namespace mio {
/**
 * @page dev_coords Coordinate systems support developement guide
 * Geographic coordinates are transparently supported (ie converted to/from lat/lon as well as between cartesian coordinate systems) through
 * the Coords class. Therefore, coordinate system conversions are quite easy, as illustrated by the example below.
 * @code
 * Coords point1("CH1903","");
 * point1.setXY(785425. , 191124., 1400.);
 * std::cout << "Lat=" << point1.getLat() << " lon=" << point1.getLon() << "\n"
 * @endcode
 * Adding support for another cartesian coordinate system is simple, once you have the official conversion wgs84 to/from this system.
 * 
* @section coords_implementation Conversion implementation
* National cartesian coordinate systems often come with an official conversion formula to and from WGS84 lat/lon. This could be found by
* the national geographic authority. Then implement two <b>private</b> methods into the Coords class (replacing the 
* "XXX" by an abbreviation for the coordinate system you are implementing):
* @code
 * void WGS84_to_XXX(double lat_in, double long_in, double& east_out, double& north_out) const;
 * void XXX_to_WGS84(double east_in, double north_in, double& lat_out, double& long_out) const;
 * @endcode
 * It is highly recommended to add the link to the official conversion specification in the doxygen comments of these two methods.
 * 
 * @section coords_registering Registering the coordinate system
 * Once these two methods have been implemented, they need to be registered for the user to be able to use them. This is done by following
 * these steps:
 *      -# retrieve the appropriate EPSG code at http://www.epsg-registry.org/ for the coordinate system. It is a good idea to use the associated abbreviation
 *         as keyword/abbreviation in the source code as well as for the end user.
 *      -# map the coordinate system abbreviation to its EPSG code in Coords::setEPSG()
 *      -# map the coordinate system abbreviation to its EPSG code in Coords::getEPSG()
 *      -# link the coordinate system abbreviation to the conversion methods you have implemented in Coords::convert_to_WGS84() and Coords::convert_from_WGS84()
 * 
 * @section coords_documentation Documenting the coordinate system
 * Please update and expand the doxygen documentation at the beginning of the Coords.cc file in order to specify the coordinate system keywords 
 * that has been used (ie the the coordinate system abbreviation). Feel free to add links to official documentation about this coordinate system. 
 * Please also properly document the conversion (with links to the official specification) in the conversion implementations. 
 * 
 * @section coord_testing Testing the implementation
 * Try to set a Coords object constructed with the chosen keywords to a set of Easting/Northing and then retrieve the lat/lon as well as the 
 * opposite. Make sure that for various points, including points close to the boundaries of the coordinate system definition, the conversion 
 * remains correct. Often the official specifications come with a set of test coordinates that you can use. Then try to set the coordinate
 * system by EPSG code and make sure Easting/Northing to and from WGS84 conversions work properly.
 * 
 */

/**
 * @class Coords
 * @brief A class to handle geographic coordinate systems.
 * This class offers an easy way to transparently convert between various coordinate systems. For any
 * given object, as soon as a latitude/longitude can be computed/read, it will be used as a reference.
 * This means that every subsequent change of projection system or read will be the conversion of this
 * reference lat/lon position (until a new "set" is called). See Coords::setProj for the supported coordinate systems.
 *
 * @ingroup data_str
 * @author Mathias Bavay
 * @date   2009-01-20
*/

class Coords {
	public:
		///Keywords for selecting the algorithm for computing geodesic distances
		typedef enum GEO_DISTANCES {
			GEO_COSINE, ///< Spherical law of cosine (See http://www.movable-type.co.uk/scripts/latlong.html)
			GEO_VINCENTY ///< Vincenty ellispoid formula (See T. Vincenty, "Closed formulas for the direct and reverse geodetic problems", Journal of Geodesy, 51, 3, 1977, DOI:10.1007/BF02521599, or http://www.springerlink.com/content/y7108u6862473583 for more)
		} geo_distances;
		
		///Keywords for selecting the toString formats
		typedef enum {
			DEBUG, ///< As much information as possible, useful for debugging
			FULL, ///< Provide all the usually necessary information
			LATLON, ///< Simplified, lat/lon only
			XY, ///< Simplified cartesian, only easting/northing
			CARTESIAN ///< Compact representation only containing the X/Y and I/J coordinates
		} FORMATS;

		//Constructors
		Coords();
		Coords(const std::string& in_coordinatesystem, const std::string& in_parameters="");
		Coords(const std::string& in_coordinatesystem, const std::string& in_parameters, std::string coord_spec);
		Coords(const double& in_lat_ref, const double& in_long_ref);
		Coords(const Coords& c);

		//Operators
		Coords& operator=(const Coords&); ///<Assignement operator
		bool operator==(const Coords&) const; ///<Operator that tests for equality
		bool operator!=(const Coords&) const; ///<Operator that tests for inequality
		bool isNodata() const;
		void moveByXY(const double& x_displacement, const double& y_displacement);
		void moveByBearing(const double& i_bearing, const double& i_distance);

		static Coords merge(const Coords& coord1, const Coords& coord2);
		void merge(const Coords& coord2);

		//Getter methods
		double getEasting() const {return easting;}
		double getNorthing() const {return northing;}
		double getLat() const {return latitude;}
		double getLon() const {return longitude;}
		double getAltitude() const {return altitude;}
		int getGridI() const {return grid_i;}
		int getGridJ() const {return grid_j;}
		int getGridK() const {return grid_k;}
		bool indexIsValid() const {return validIndex;} ///< Returns true if the (i,j,k) index are valid
		void getProj(std::string& proj_type, std::string& proj_args) const;
		short int getEPSG() const;

		const std::string toString(const FORMATS& type = DEBUG) const;
		friend std::ostream& operator<<(std::ostream& os, const Coords& coord);
		friend std::istream& operator>>(std::istream& is, Coords& coord);

		//Setter methods
		void setLatLon(const double in_latitude, const double in_longitude, const double in_altitude, const bool in_update=true);
		void setLatLon(const std::string& in_coordinates, const double in_altitude, const bool in_update=true);
		void setXY(const double in_easting, const double in_northing, const double in_altitude, const bool in_update=true);
		void setGridIndex(const int in_grid_i, const int in_grid_j, const int in_grid_k, const bool setValid=false);
		void setAltitude(const double in_altitude, const bool in_update=true);
		void setProj(const std::string& in_coordinatesystem, const std::string& in_parameters="");
		void setLocalRef(const double in_ref_latitude, const double in_ref_longitude);
		void setLocalRef(const std::string in_coordparam);
		void setDistances(const geo_distances in_algo);
		void setEPSG(const int& epsg);

		void check(const std::string& pre_msg="");
		double distance(const Coords& destination) const;
		bool isSameProj(const Coords& target) const;
		void copyProj(const Coords& source, const bool i_update=true);

	private:
		//Coordinates conversions
		void convert_to_WGS84(double i_easting, double i_northing, double& o_latitude, double& o_longitude) const;
		void convert_from_WGS84(double i_latitude, double i_longitude, double& o_easting, double& o_northing) const;

		void WGS84_to_local(double lat_in, double long_in, double& east_out, double& north_out) const;
		void local_to_WGS84(double east_in, double north_in, double& lat_out, double& long_out) const;
		void WGS84_to_NULL(double lat_in, double long_in, double& east_out, double& north_out) const;
		void NULL_to_WGS84(double east_in, double north_in, double& lat_out, double& long_out) const;

		//Distances calculations
		void distance(const Coords& destination, double& o_distance, double& o_bearing) const;

	private:
		void clearCoordinates();
		void setDefaultValues();

	private:
		double ref_latitude, ref_longitude;
		double altitude; ///<altitude of the point (the altitude is currently NOT dependant on the projection)
		double latitude, longitude; ///<latitude and longitude of the point
		double easting, northing; ///<east and north coordinate of the point in a cartesian grid
		int grid_i, grid_j, grid_k; ///<grid index i, j, k (please notice that this index is NOT automatically regenerated NOR checked)
		bool validIndex; ///< are grid index invalid?

		std::string coordsystem, coordparam;
		geo_distances distance_algo;
};
} //end namespace

#endif
