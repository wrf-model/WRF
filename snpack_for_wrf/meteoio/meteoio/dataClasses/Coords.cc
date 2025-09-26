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
#include <meteoio/dataClasses/Coords.h>
#include <meteoio/dataClasses/CoordsAlgorithms.h>
#include <meteoio/IOUtils.h>
#include <meteoio/IOExceptions.h>
#include <meteoio/MathOptim.h>
#include <meteoio/meteoLaws/Meteoconst.h> //for math constants

#include <cmath>
#include <cstdio>
#include <iomanip>

using namespace std;

namespace mio {
 /**
 * @page coords Available coordinate systems
 * Geographic coordinates will be transparently and automatically converted to lat/lon and any other coordinate system that
 * the client program uses. However, in order to do so, the input coordinate system must be specified. In order to output
 * geolocalized data, the desired coordinate system must also be specified for the outputs (in the output section).
 * This is done through the use of the COORDIN and COORDPARAM keys (see the documentation for each plugin).
 *
 * \anchor Coordinate_types
 * There are two ways of supporting a given coordinate system: through the use of an adhoc implementation
 * (that becomes part of MeteoIO) or through the use of an external library, Proj4 [ref: http://trac.osgeo.org/proj/].
 * The current internal implementations are the following (given by their keyword):
 * - <a href="https://en.wikipedia.org/wiki/Swiss_coordinate_system">CH1903 or CH1903+</a> for coordinates in the Swiss Grid [ref: http://geomatics.ladetto.ch/ch1903_wgs84_de.pdf] (epsg codes, respectively 21781 and 2056)
 * - <a href="https://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system">UTM</a> for UTM coordinates, the zone must be specified in the parameters, for example 31T [ref: http://www.oc.nps.edu/oc2902w/maps/utmups.pdf] (epsg codes as 32600+zoneNumber in the northern hemisphere or as 32700+zoneNumber in the southern hemisphere)
 * - <a href="https://en.wikipedia.org/wiki/Universal_polar_stereographic_coordinate_system">UPS</a> for Universal Polar Stereographic coordinates (the zone, either N or S, must be specified in the parameters). [ref: J. Hager, J. Behensky, B. Drew, <i>THE UNIVERSAL GRIDS: Universal Transverse Mercator (UTM) and Universal Polar Stereographic (UPS)</i>, 1989, Defense Mapping Agency, DMATM 8358.2] (epsg codes as 32661 for the north pole and 32761 for the south pole)
 * - LOCAL for local coordinate system (using the horizontal and vertical distance from a reference point, see Coords::geo_distances for the available choice of distance algorithms)
 *
 * Such an example of use is the following:
 * @code
 * COORDSYS	= UTM
 * COORDPARAM	= 31T
 * @endcode
 *
 * On the other hand, when using the <a href="https://en.wikipedia.org/wiki/PROJ.4">Proj4</a> library for handling the coordinate conversion, the EPSG codes of
 * the chosen projection must be specified (such codes can be found at http://spatialreference.org/ref/epsg/?page=1)
 * as illustrated below (21781 is the EPSG code for the CH1903 coordinate system. Such a code is 32767 at the maximum):
 * @code
 * COORDSYS	= PROJ4
 * COORDPARAM	= 21781
 * @endcode
 *
 */

//Adding a coordinate system is done by editing convert_to_WGS84 and convert_from_WGS84
//and implementing the XXX_to_WGS84 and WGS84_to_XXX methods in CoordsAlgorithms.

/**
* @brief Equality operator that checks that two coordinates objects represent the same 3D point.
* The comparison checks either lat/lon/alt or easting/northing/alt. If both objects have nodata coordinates,
* then they are equal (even if the internal projections might be set to different systems).
* @param[in] in Coord object to compare to
* @return true or false
*/
bool Coords::operator==(const Coords& in) const {
	//check on lat/lon
	if (latitude!=IOUtils::nodata && longitude!=IOUtils::nodata) {
		const bool comparison = ( IOUtils::checkEpsilonEquality(getLat(), in.getLat(), IOUtils::lat_epsilon) &&
		                          IOUtils::checkEpsilonEquality(getLon(), in.getLon(), IOUtils::lon_epsilon) &&
		                          IOUtils::checkEpsilonEquality(getAltitude(), in.getAltitude(), IOUtils::grid_epsilon) );
		return comparison;
	}
	//check on easting/northing
	if (easting!=IOUtils::nodata && northing!=IOUtils::nodata) {
		//in this case, it means that we don't know anything about the projection parameters
		//otherwise the lat/long would have been calculated. So EPSG should be nodata
		const bool comparison = ( IOUtils::checkEpsilonEquality(getEasting(), in.getEasting(), IOUtils::grid_epsilon) &&
		                          IOUtils::checkEpsilonEquality(getNorthing(), in.getNorthing(), IOUtils::grid_epsilon) &&
		                          IOUtils::checkEpsilonEquality(getAltitude(), in.getAltitude(), IOUtils::grid_epsilon) &&
		                          getEPSG()==in.getEPSG());
		return comparison;
	}
	if (validIndex) {
		//only available information is grid indices
		const bool comparison = ( grid_i==in.grid_i && grid_j==in.grid_j && grid_k==in.grid_k );
		return comparison;
	}
	//every field is nodata... the objects can only be equal if both are nodata
	if (in.isNodata()==true) return true;
	else return false;
}

/**
* @brief Inequality operator that checks that lat/lon don't match
* @param[in] in Coord object to compare to
* @return true or false
*/
bool Coords::operator!=(const Coords& in) const {
	return !(*this==in);
}

Coords& Coords::operator=(const Coords& source) {
	if (this != &source) {
		altitude = source.altitude;
		latitude = source.latitude;
		longitude = source.longitude;
		easting = source.easting;
		northing = source.northing;
		ref_latitude = source.ref_latitude;
		ref_longitude = source.ref_longitude;
		distance_algo = source.distance_algo;
		coordsystem = source.coordsystem;
		coordparam = source.coordparam;
		grid_i = source.grid_i;
		grid_j = source.grid_j;
		grid_k = source.grid_k;
		validIndex = source.validIndex;
	}
	return *this;
}

bool Coords::isNodata() const {
	if (latitude==IOUtils::nodata && longitude==IOUtils::nodata &&
	    easting==IOUtils::nodata && northing==IOUtils::nodata &&
	    altitude==IOUtils::nodata &&
	    grid_i==IOUtils::nodata && grid_j==IOUtils::nodata && grid_k==IOUtils::nodata &&
	    validIndex==false) {
		return true;
	}
	return false;
}

///< move the point by the specified distance (in m) along easting and northing
void Coords::moveByXY(const double& x_displacement, const double& y_displacement) {
	setXY(easting+x_displacement, northing+y_displacement, altitude, true);
}

///< move the point by the specified bearing and distance (in m)
void Coords::moveByBearing(const double& i_bearing, const double& i_distance) {
	double new_lat, new_lon;

	switch(distance_algo) {
		case GEO_COSINE:
			CoordsAlgorithms::cosineInverse(latitude, longitude, i_distance, i_bearing, new_lat, new_lon);
			break;
		case GEO_VINCENTY:
			CoordsAlgorithms::VincentyInverse(latitude, longitude, i_distance, i_bearing, new_lat, new_lon);
			break;
		default:
			throw InvalidArgumentException("Unsupported distance algorithm", AT);
	}

	setLatLon(new_lat, new_lon, altitude, true);
}

/**
* @brief Simple merge strategy.
* If some fields of the first argument are empty, they will be filled by the matching field from the
* second argument.
* @param coord1 first Coords to merge, highest priority
* @param coord2 second Coords to merge, lowest priority
* @return new Coords object
*/
Coords Coords::merge(const Coords& coord1, const Coords& coord2) {
	Coords tmp(coord1);
	tmp.merge(coord2);
	return tmp;
}

/**
* @brief Simple merge strategy.
* If some fields of the current object are empty, they will be filled by the matching field from the
* provided argument.
* @param coord2 extra Coords to merge, lowest priority
*/
void Coords::merge(const Coords& coord2) {
	if (altitude==IOUtils::nodata) altitude=coord2.altitude;
	if (latitude==IOUtils::nodata) latitude=coord2.latitude;
	if (longitude==IOUtils::nodata) longitude=coord2.longitude;
	if (easting==IOUtils::nodata) easting=coord2.easting;
	if (northing==IOUtils::nodata) northing=coord2.northing;

	if (validIndex==false) validIndex=coord2.validIndex;
	if (grid_i==IOUtils::nodata) grid_i=coord2.grid_i;
	if (grid_j==IOUtils::nodata) grid_j=coord2.grid_j;
	if (grid_k==IOUtils::nodata) grid_k=coord2.grid_k;

	if (ref_latitude==IOUtils::nodata) ref_latitude=coord2.ref_latitude;
	if (ref_longitude==IOUtils::nodata) ref_longitude=coord2.ref_longitude;

	if (coordsystem=="NULL") coordsystem=coord2.coordsystem;
	if (coordparam=="NULL") coordparam=coord2.coordparam;

	if (distance_algo==IOUtils::nodata) distance_algo=coord2.distance_algo;

	//in LOCAL projection, the check for the existence of the ref point will be done in the projection functions
	if (coordsystem=="LOCAL" && !coordparam.empty()) {
		CoordsAlgorithms::parseLatLon(coordparam, ref_latitude, ref_longitude);
	}
	if (latitude!=IOUtils::nodata && coordsystem!="NULL") {
		convert_from_WGS84(latitude, longitude, easting, northing);
	}
	if (latitude==IOUtils::nodata && coordsystem!="NULL") {
		convert_to_WGS84(easting, northing, latitude, longitude);
	}
}

/**
* @brief Print the content of the Coords object (useful for debugging)
* The Coords is bound by "<Coords>" and "</Coords>" on separate lines
*/
const std::string Coords::toString(const FORMATS& type) const 
{
	std::ostringstream os;
	std::streamsize p = os.precision();
	if (type==DEBUG) {
		os << "<Coords>\n";
		os << "Altitude\t" << altitude << "\n";
		os << "Lat/Long\t" << CoordsAlgorithms::printLatLon(latitude, longitude) << "\n";
		os << "Lat/Long\t" <<std::fixed << std::setprecision(10) << "(" << getLat() << " , " << getLon() << ")" << "\n";
		os << "X/Y_coords\t" << std::fixed << std::setprecision(0) << "(" << getEasting() << " , " << getNorthing() << ")" << "\n";
		os << std::resetiosflags(std::ios_base::fixed|std::ios_base::floatfield);
		os.precision(p);
		if (validIndex) 
			os << "I/J_indices\t" << "(" << getGridI() << " , " << getGridJ() << ")" << "\n";
		
		os << "Projection\t" << coordsystem;
		if (!coordparam.empty())
			os << " (" << coordparam << ")";
		if (coordsystem=="LOCAL")
			os << " ref=(" << ref_latitude << "," << ref_longitude << ")";
		os << "\n";
		os << "EPSG\t\t" << getEPSG() << "\n";
		os << "</Coords>\n";
	} else if (type==FULL) {
		os << altitude << " a.s.l.;\t";
		os << "WGS84: (" << getLat() << "," << getLon() << ");\t";
		os << "EPSG " << getEPSG() << ": (" << getEasting() << "," << getNorthing() << ");\t";
		if (validIndex) 
			os << "grid: (" << getGridI() << "," << getGridJ() << ")";
	} else if (type==LATLON) {
		os << std::fixed << std::setprecision(6) << "(" << getLat() << " , " << getLon() << ")";
		os << std::resetiosflags(std::ios_base::fixed|std::ios_base::floatfield);
		os.precision(p);
	} else if (type==XY) {
		os << std::fixed << std::setprecision(0) << "(" << getEasting() << " , " << getNorthing() << ")";
		os << std::resetiosflags(std::ios_base::fixed|std::ios_base::floatfield);
		os.precision(p);
	}else if (type==CARTESIAN) {
		os << "[ (" << getEasting() << "," << getNorthing() << ");(" << getGridI() << "," << getGridJ() << ");@" << altitude << " ]";
	} else
		throw InvalidArgumentException("Selected output type is not supported!", AT);
	return os.str();
}

std::ostream& operator<<(std::ostream& os, const Coords& coord) {
	os.write(reinterpret_cast<const char*>(&coord.ref_latitude), sizeof(coord.ref_latitude));
	os.write(reinterpret_cast<const char*>(&coord.ref_longitude), sizeof(coord.ref_longitude));
	os.write(reinterpret_cast<const char*>(&coord.altitude), sizeof(coord.altitude));
	os.write(reinterpret_cast<const char*>(&coord.latitude), sizeof(coord.latitude));
	os.write(reinterpret_cast<const char*>(&coord.longitude), sizeof(coord.longitude));
	os.write(reinterpret_cast<const char*>(&coord.easting), sizeof(coord.easting));
	os.write(reinterpret_cast<const char*>(&coord.northing), sizeof(coord.northing));
	os.write(reinterpret_cast<const char*>(&coord.grid_i), sizeof(coord.grid_i));
	os.write(reinterpret_cast<const char*>(&coord.grid_j), sizeof(coord.grid_j));
	os.write(reinterpret_cast<const char*>(&coord.grid_k), sizeof(coord.grid_k));
	os.write(reinterpret_cast<const char*>(&coord.validIndex), sizeof(coord.validIndex));

	const size_t s_coordsystem = coord.coordsystem.size();
	os.write(reinterpret_cast<const char*>(&s_coordsystem), sizeof(size_t));
	os.write(reinterpret_cast<const char*>(&coord.coordsystem[0]), s_coordsystem*sizeof(coord.coordsystem[0]));
	const size_t s_coordparam = coord.coordparam.size();
	os.write(reinterpret_cast<const char*>(&s_coordparam), sizeof(size_t));
	os.write(reinterpret_cast<const char*>(&coord.coordparam[0]), s_coordparam*sizeof(coord.coordparam[0]));

	os.write(reinterpret_cast<const char*>(&coord.distance_algo), sizeof(coord.distance_algo));
	return os;
}

std::istream& operator>>(std::istream& is, Coords& coord) {
	is.read(reinterpret_cast<char*>(&coord.ref_latitude), sizeof(coord.ref_latitude));
	is.read(reinterpret_cast<char*>(&coord.ref_longitude), sizeof(coord.ref_longitude));
	is.read(reinterpret_cast<char*>(&coord.altitude), sizeof(coord.altitude));
	is.read(reinterpret_cast<char*>(&coord.latitude), sizeof(coord.latitude));
	is.read(reinterpret_cast<char*>(&coord.longitude), sizeof(coord.longitude));
	is.read(reinterpret_cast<char*>(&coord.easting), sizeof(coord.easting));
	is.read(reinterpret_cast<char*>(&coord.northing), sizeof(coord.northing));
	is.read(reinterpret_cast<char*>(&coord.grid_i), sizeof(coord.grid_i));
	is.read(reinterpret_cast<char*>(&coord.grid_j), sizeof(coord.grid_j));
	is.read(reinterpret_cast<char*>(&coord.grid_k), sizeof(coord.grid_k));
	is.read(reinterpret_cast<char*>(&coord.validIndex), sizeof(coord.validIndex));

	size_t s_coordsystem, s_coordparam;
	is.read(reinterpret_cast<char*>(&s_coordsystem), sizeof(size_t));
	coord.coordsystem.resize(s_coordsystem);
	is.read(reinterpret_cast<char*>(&coord.coordsystem[0]), s_coordsystem*sizeof(coord.coordsystem[0]));
	is.read(reinterpret_cast<char*>(&s_coordparam), sizeof(size_t));
	coord.coordparam.resize(s_coordparam);
	is.read(reinterpret_cast<char*>(&coord.coordparam[0]), s_coordparam*sizeof(coord.coordparam[0]));

	is.read(reinterpret_cast<char*>(&coord.distance_algo), sizeof(coord.distance_algo));
	return is;
}

/**
* @brief Default constructor
* This constructor builds a dummy object that performs no conversions but can be used for comparison
* purpose. This is more or less the equivalent of NULL for a pointer...
*/
Coords::Coords() : ref_latitude(IOUtils::nodata), ref_longitude(IOUtils::nodata),
                   altitude(IOUtils::nodata), latitude(IOUtils::nodata), longitude(IOUtils::nodata),
                   easting(IOUtils::nodata), northing(IOUtils::nodata),
                   grid_i(IOUtils::inodata), grid_j(IOUtils::inodata), grid_k(IOUtils::inodata), validIndex(false),
                   coordsystem("NULL"), coordparam("NULL"), distance_algo(GEO_COSINE) {}

/**
* @brief Regular constructor: usually, this is the constructor to use
* @param[in] in_coordinatesystem string identifying the coordinate system to use
* @param[in] in_parameters string giving some additional parameters for the projection (optional)
*
* See setProj() for a full description of these strings
*/
Coords::Coords(const std::string& in_coordinatesystem, const std::string& in_parameters) :
                   ref_latitude(IOUtils::nodata), ref_longitude(IOUtils::nodata),
                   altitude(IOUtils::nodata), latitude(IOUtils::nodata), longitude(IOUtils::nodata),
                   easting(IOUtils::nodata), northing(IOUtils::nodata),
                   grid_i(IOUtils::inodata), grid_j(IOUtils::inodata), grid_k(IOUtils::inodata), validIndex(false), 
                   coordsystem(in_coordinatesystem), coordparam(in_parameters), distance_algo(GEO_COSINE)
{
	setProj(in_coordinatesystem, in_parameters);
}

/**
* @brief Local projection onstructor: this constructor is only suitable for building a local projection.
* Such a projection defines easting and northing as the distance (in meters) to a reference point
* which coordinates have to be provided here.
* @param[in] in_lat_ref latitude of the reference point
* @param[in] in_long_ref longitude of the reference point
*/
Coords::Coords(const double& in_lat_ref, const double& in_long_ref) :
                   ref_latitude(in_lat_ref), ref_longitude(in_long_ref),
                   altitude(IOUtils::nodata), latitude(IOUtils::nodata), longitude(IOUtils::nodata),
                   easting(IOUtils::nodata), northing(IOUtils::nodata),
                   grid_i(IOUtils::inodata), grid_j(IOUtils::inodata), grid_k(IOUtils::inodata), validIndex(false),
                   coordsystem("LOCAL"), coordparam(), distance_algo(GEO_COSINE)
{
	setLocalRef(in_lat_ref, in_long_ref);
	setProj("LOCAL", "");
}

Coords::Coords(const Coords& c) : ref_latitude(c.ref_latitude), ref_longitude(c.ref_longitude),
                   altitude(c.altitude), latitude(c.latitude), longitude(c.longitude),
                   easting(c.easting), northing(c.northing),
                   grid_i(c.grid_i), grid_j(c.grid_j), grid_k(c.grid_k), validIndex(c.validIndex),
                   coordsystem(c.coordsystem), coordparam(c.coordparam), distance_algo(c.distance_algo) {}

/**
* @brief Local projection constructor: this constructor is only suitable for building a local projection.
* @details Such a projection defines easting and northing as the distance (in meters) to a reference point
* which coordinates have to be provided here.
* @param[in] in_coordinatesystem string identifying the coordinate system to use
* @param[in] in_parameters string giving some additional parameters for the projection (empty string if not applicable)
* @param[in] coord_spec coordinate specification
*
* The coordinate specification is prefixed by either "latlon" or "xy" (for cartesian coordinates) given as either:
* - latitude longitude altitude
* - latitude/longitude/altitude
* - (latitude; longitude; altitude)
* - (latitude, longitude, altitude)
* 
* Of course, for cartesian coordinates systems, the easting/northing/altitudes are provided instead of latitude/longitude/altitude.
* Latitudes and longitudes can be in any format supported by CoordsAlgorithms::dms_to_decimal.
*
* For example, those are valid coordinates specification strings:
* @code
* latlon (46.75; 9.80; 2200)
* latlon (46d 43' 51", 9.80, 2200)
* xy (198754, 723458, 2200)
* @endcode
*/
Coords::Coords(const std::string& in_coordinatesystem, const std::string& in_parameters, std::string coord_spec)
       : ref_latitude(IOUtils::nodata), ref_longitude(IOUtils::nodata),
         altitude(IOUtils::nodata), latitude(IOUtils::nodata), longitude(IOUtils::nodata),
         easting(IOUtils::nodata), northing(IOUtils::nodata),
         grid_i(IOUtils::inodata), grid_j(IOUtils::inodata), grid_k(IOUtils::inodata), validIndex(false), 
         coordsystem(in_coordinatesystem), coordparam(in_parameters), distance_algo(GEO_COSINE)
{
	static const char format1[] = " %[0-9.,°d'\"-] %[0-9.,°d'\"-] %[0-9.,-]";
	static const char format2[] = " %[0-9.,°d'\"- ]/%[0-9.,°d'\"- ]/%[0-9.,- ]";
	static const char format3[] = " (%[0-9.,°d'\"- ];%[0-9.,°d'\"- ];%[0-9.,- ])";
	static const char format4[] = " (%[0-9.°d'\"- ],%[0-9.°d'\"- ],%[0-9.- ])";

	static const size_t len=128;
	if (coord_spec.size()>=len)
			throw InvalidFormatException("Given coordinate string is too long! ",AT);

	static const std::string latlon( "latlon" );
	static const std::string xy( "xy" );
	IOUtils::toLower( coord_spec );
	size_t pos_type = coord_spec.find( latlon );

	char alt_str[len]="";
	if (pos_type!=std::string::npos) {
		coord_spec.erase(pos_type, latlon.length());
		char lat_str[len]=""; //each string must be able to accomodate the whole length to avoid buffer overflow
		char lon_str[len]="";

		if     ((sscanf(coord_spec.c_str(), format1, lat_str, lon_str, alt_str) < 3) &&
			(sscanf(coord_spec.c_str(), format2, lat_str, lon_str, alt_str) < 3) &&
			(sscanf(coord_spec.c_str(), format3, lat_str, lon_str, alt_str) < 3) &&
			(sscanf(coord_spec.c_str(), format4, lat_str, lon_str, alt_str) < 3)) {
				throw InvalidFormatException("Can not parse given coordinates: "+coord_spec,AT);
		}
	#if defined _WIN32 || defined __MINGW32__
		if (alt_str[ strlen(alt_str)-1 ]==')') alt_str[ strlen(alt_str)-1 ] = '\0'; //for ms_scanf bug
	#endif

		double alt;
		if (!IOUtils::convertString(alt, alt_str)) throw InvalidFormatException("Can not parse the altitude given in the coordinates: "+coord_spec,AT);
		setLatLon( CoordsAlgorithms::dms_to_decimal(std::string(lat_str)), CoordsAlgorithms::dms_to_decimal(std::string(lon_str)), alt);
	} else {
		pos_type = coord_spec.find( xy );
		if (pos_type==std::string::npos)
			throw InvalidFormatException("Can not parse given coordinates: "+coord_spec, AT);
		coord_spec.erase(pos_type, xy.length());
		char easting_str[len]=""; //each string must be able to accomodate the whole length to avoid buffer overflow
		char northing_str[len]="";

		if     ((sscanf(coord_spec.c_str(), format1, easting_str, northing_str, alt_str) < 3) &&
			(sscanf(coord_spec.c_str(), format2, easting_str, northing_str, alt_str) < 3) &&
			(sscanf(coord_spec.c_str(), format3, easting_str, northing_str, alt_str) < 3) &&
			(sscanf(coord_spec.c_str(), format4, easting_str, northing_str, alt_str) < 3)) {
				throw InvalidFormatException("Can not parse given coordinates: "+coord_spec,AT);
		}
	#if defined _WIN32 || defined __MINGW32__
		if (alt_str[ strlen(alt_str)-1 ]==')') alt_str[ strlen(alt_str)-1 ] = '\0'; //for ms_scanf bug
	#endif

		double east, north, alt;
		if (!IOUtils::convertString(east, easting_str)) throw InvalidFormatException("Can not parse the easting given in the coordinates: "+coord_spec,AT);
		if (!IOUtils::convertString(north, northing_str)) throw InvalidFormatException("Can not parse the northing given in the coordinates: "+coord_spec,AT);
		if (!IOUtils::convertString(alt, alt_str)) throw InvalidFormatException("Can not parse the altitude given in the coordinates: "+coord_spec,AT);
		setXY( east, north, alt);
	}
}

/**
* @brief Returns the projection parameters
* @param[out] proj_type projection type
* @param[out] proj_args optional arguments
*/
void Coords::getProj(std::string& proj_type, std::string& proj_args) const {
	proj_type = coordsystem;
	if (coordsystem=="LOCAL") {
		std::ostringstream dms;
		dms << "(" << CoordsAlgorithms::decimal_to_dms(ref_latitude) << " , " << CoordsAlgorithms::decimal_to_dms(ref_longitude) << ")";
		proj_args=dms.str();
	} else {
		proj_args = coordparam;
	}
}

/**
* @brief Set latitude and longitude
* The automatic update of the easting/northing can be turned off so that
* both lat/lon and east/north coordinates can be provided in order to thereafter check the
* coordinates by calling check().
* @param[in] in_coordinates string containing the lat/lon to read
* @param[in] in_altitude altitude to set (optional)
* @param[in] in_update should the easting/northing be updated? (default=true)
*/
void Coords::setLatLon(const std::string& in_coordinates, const double in_altitude, const bool in_update) {
	double lat, lon;
	CoordsAlgorithms::parseLatLon(in_coordinates, lat, lon);
	setLatLon(lat, lon, in_altitude, in_update);
}

/**
* @brief Set latitude and longitude
* The automatic update of the easting/northing can be turned off so that
* both lat/lon and east/north coordinates can be provided in order to thereafter check the
* coordinates by calling check().
* @param[in] in_latitude latitude to set
* @param[in] in_longitude longitude to set
* @param[in] in_altitude altitude to set
* @param[in] in_update should the easting/northing be updated? (default=true)
*/
void Coords::setLatLon(const double in_latitude, const double in_longitude, const double in_altitude, const bool in_update) 
{
	if ((in_latitude!=IOUtils::nodata && fabs(in_latitude)>90.) || (in_longitude!=IOUtils::nodata && fabs(in_longitude)>360.)) {
		std::ostringstream ss;
		ss << "(" << in_latitude << "," << in_longitude << ")";
		throw InvalidArgumentException("Invalid latitude/longitude: "+ss.str(), AT);
	}
	
	latitude = in_latitude;
	longitude = in_longitude;
	if (in_altitude!=IOUtils::nodata) {
		altitude = in_altitude;
	}
	if (coordsystem!="NULL" && in_update==true) {
		convert_from_WGS84(latitude, longitude, easting, northing);
	}
	grid_i = grid_j = grid_k = IOUtils::inodata;
	validIndex = false;
}

/**
* @brief Set easting and northing
* The automatic update of the latitude/longitude can be turned off so that
* both lat/lon and east/north coordinates can be provided in order to thereafter check the
* coordinates by calling check().
* @param[in] in_easting easting to set
* @param[in] in_northing northing to set
* @param[in] in_altitude altitude to set
* @param[in] in_update should the easting/northing be updated? (default=true)
*/
void Coords::setXY(const double in_easting, const double in_northing, const double in_altitude, const bool in_update) {
	easting = in_easting;
	northing = in_northing;
	if (in_altitude!=IOUtils::nodata) {
		altitude = in_altitude;
	}
	if (coordsystem!="NULL" && in_update==true) {
		convert_to_WGS84(easting, northing, latitude, longitude);
	}
	grid_i = grid_j = grid_k = IOUtils::inodata;
	validIndex = false;
}

/**
* @brief Set grid indices
* This index represent the position in a cartesian grid. It can not be automatically matched with
* a set of geographic coordinates because it needs the information about the said grid.
* Therefore, the coordinate object needs to be given to a grid object that will either set (i,j) or
* (lat,lon)/(easting,northing) as well as the grid's matching altitude if it is not already set.
* Any subsequent change of either (lat,lon) or (easting,northing) will reset these indexes to IOUtils::inodata.
* By default, setting (i,j) will mark the coordinates as invalid (meaning that there is no match between
* the (i,j,k) coordinates and the lat/lon or x/y coordinates).
* Finally, the given indices are <b>NOT checked for validity</b>: such check must be done
* by calling Grid2DObject::gridify or Grid3DObject::gridify .
*
* @note To make it short: <b>use this method with caution!!</b>
* @param[in] in_grid_i grid index along the X direction
* @param[in] in_grid_j grid index along the Y direction
* @param[in] in_grid_k grid index along the Z direction
* @param[in] setValid should the geographic coordinates be marked as valid? (default=false, this flag should be true ONLY when calling from Grid2/3DObject)
*/
void Coords::setGridIndex(const int in_grid_i, const int in_grid_j, const int in_grid_k, const bool setValid) {
	if (in_grid_i==IOUtils::inodata || in_grid_j==IOUtils::inodata) {
		validIndex = false;
		return;
	}
	grid_i = in_grid_i;
	grid_j = in_grid_j;
	grid_k = in_grid_k;
	validIndex = setValid;
}

/**
* @brief Set altitude at a given value.
* If the i,j,k indices were set, reset them to inodata,
* except if specified otherwise with in_update=false.
* @param[in] in_altitude altitude above sea level, in meters
* @param[in] in_update should the indices be (if necessary) recalculated? (default=true)
*/
void Coords::setAltitude(const double in_altitude, const bool in_update) {
	altitude = in_altitude;
	if (in_update==true) {
		grid_i = grid_j = grid_k = IOUtils::inodata;
		validIndex = false;
	}
}

/**
* @brief Set projection to use
* This projection will be used for converting between lat/lon and East/North (see the \ref Coordinate_types "supported projections")
* @param[in] in_coordinatesystem string identifying the coordinate system to use
* @param[in] in_parameters string giving some additional parameters for the projection (optional)
*/
void Coords::setProj(const std::string& in_coordinatesystem, const std::string& in_parameters) 
{
	if (in_coordinatesystem=="PROJ4" && in_parameters=="4326")
		throw InvalidArgumentException("Please define a cartesian coordinate system, not a spehrical one!", AT);

	//the latitude/longitude had not been calculated, so we do it first in order to have our reference
	//before further conversions (usage scenario: giving a x,y and then converting to anyother x,y in another system
	if ((coordsystem != "NULL") && ((latitude==IOUtils::nodata) || (longitude==IOUtils::nodata))) {
		convert_to_WGS84(easting, northing, latitude, longitude);
	}
	
	//If the new coordinate system is exactly the same as the old one, do not recompute X/Y 
	//to avoids the inaccuracies due to conversion to and from WGS84
	if ((coordsystem == in_coordinatesystem) && (coordparam == in_parameters))
		return;

	if (in_coordinatesystem.empty()) {
		coordsystem = "NULL";
	} else {
		coordsystem = in_coordinatesystem;
	}
	coordparam  = in_parameters;
	if (coordsystem=="LOCAL" && !coordparam.empty()) {
		CoordsAlgorithms::parseLatLon(coordparam, ref_latitude, ref_longitude);
	}
	
	//since lat/long is our reference, we refresh x,y (only if lat/lon exist)
	if (latitude!=IOUtils::nodata && longitude!=IOUtils::nodata) {
		convert_from_WGS84(latitude, longitude, easting, northing);
	}
	//if we only had x/y but not even a coord system, we could not compute lat/long. We now do it
	if ( (latitude==IOUtils::nodata || longitude==IOUtils::nodata) &&
	    (easting!=IOUtils::nodata && northing!=IOUtils::nodata) &&
	    (coordsystem != "NULL") ) {
		convert_to_WGS84(easting, northing, latitude, longitude);
	}
}

/**
* @brief Set the local projection reference coordinates
* This projection will be used for converting between lat/lon and East/North
* @param[in] in_ref_latitude latitude of the local origin
* @param[in] in_ref_longitude longitude of the local origin
*/
void Coords::setLocalRef(const double in_ref_latitude, const double in_ref_longitude) {
	if (in_ref_latitude==IOUtils::nodata || in_ref_longitude==IOUtils::nodata) {
		throw InvalidArgumentException("For LOCAL projection, please provide both reference latitude and longitude!", AT);
	}
	ref_latitude = in_ref_latitude;
	ref_longitude = in_ref_longitude;
	if (coordsystem=="LOCAL") {
		convert_from_WGS84(latitude, longitude, easting, northing);
	}
}

/**
* @brief Set the local projection reference coordinates
* This projection will be used for converting between lat/lon and East/North
* @param[in] in_coordparam string containing the (lat,lon) of the local origin
*/
void Coords::setLocalRef(const std::string in_coordparam) {
	coordparam = in_coordparam;
	CoordsAlgorithms::parseLatLon(coordparam, ref_latitude, ref_longitude);
	if (coordsystem=="LOCAL") {
		convert_from_WGS84(latitude, longitude, easting, northing);
	}
}

/**
* @brief Set the algorithm to use to compute distances
* Various algorithm exist that offer various precision/complexity tradeoffs.
* @param[in] in_algo enum giving the algorithm to be used (see documentation for geo_distances)
*/
void Coords::setDistances(const geo_distances in_algo) {
	distance_algo = in_algo;
	if (coordsystem=="LOCAL") {
		convert_from_WGS84(latitude, longitude, easting, northing);
	}
}

/**
* @brief Check consistency of coordinates
* When both latitude/longitude and easting/northing are given, there is
* a risk of inconsistency between these two sets of coordinates.
* This method checks that enough information is available (ie: at least one set
* of coordinates is present) and if more than one is present, that it is consistent (within 5 meters)
* It throws an exception if something is not right.
* @param[in] pre_msg String to prepend to errors messages (for example, giving the file name that contains the error)
*/
void Coords::check(const std::string& pre_msg)
{
	//calculate/check coordinates if necessary
	if (coordsystem=="LOCAL" && (ref_latitude==IOUtils::nodata || ref_longitude==IOUtils::nodata)) {
		throw InvalidArgumentException(pre_msg+"please define a reference point for LOCAL coordinate system", AT);
	}

	if (latitude==IOUtils::nodata || longitude==IOUtils::nodata) {
		if (easting==IOUtils::nodata || northing==IOUtils::nodata) {
			throw InvalidArgumentException(pre_msg+"missing positional parameters (easting,northing) or (lat,long) for given coordinate "+toString(FULL), AT);
		}
		convert_to_WGS84(easting, northing, latitude, longitude);
	} else {
		if (easting==IOUtils::nodata || northing==IOUtils::nodata) {
			convert_from_WGS84(latitude, longitude, easting, northing);
		} else {
			double tmp_lat, tmp_lon;
			convert_to_WGS84(easting, northing, tmp_lat, tmp_lon);

			if (!IOUtils::checkEpsilonEquality(latitude, tmp_lat, IOUtils::lat_epsilon) || !IOUtils::checkEpsilonEquality(longitude, tmp_lon, IOUtils::lon_epsilon)) {
				std::string extra_info( toString(FULL) );
				if (coordsystem=="UTM") {
					std::string zone_out;
					CoordsAlgorithms::getUTMZone(latitude, longitude, zone_out);
					if (zone_out!=coordparam)
						extra_info = "(UTM zone should probably be "+zone_out+" instead of "+coordparam;
				}
				
				throw InvalidArgumentException(pre_msg+"Latitude/longitude and xllcorner/yllcorner don't match for given coordinate "+extra_info, AT);
			}
		}
	}
}

/**
* @brief Calculate the distance between two points
* @param destination destination coordinate
* @return distance in meters
*/
double Coords::distance(const Coords& destination) const {
	double dist, bearing;
	distance(destination, dist, bearing);
	return dist;
}

/**
* @brief Check if two Coords object are using the same projection
* @param target coordinate to compare to
* @return true or false
*/
bool Coords::isSameProj(const Coords& target) const {
	if (coordsystem=="LOCAL") {
		return ( target.coordsystem=="LOCAL" && ref_latitude==target.ref_latitude && ref_longitude==target.ref_longitude );
	} else {
		return ( coordsystem==target.coordsystem && coordparam==target.coordparam );
	}
}

/**
* @brief Copy the projection parameters of another Coords object
* @param source source object to copy the projection from
* @param i_update should the necessary coordinates be updated? (default=true)
*/
void Coords::copyProj(const Coords& source, const bool i_update) {
	if (!isSameProj(source)) {
		//we only do a copy if we are not already using the same projection
		if (source.coordsystem=="LOCAL") {
			coordsystem="LOCAL";
			coordparam=source.coordparam;
			ref_latitude=source.ref_latitude;
			ref_longitude=source.ref_longitude;
		} else {
			coordsystem=source.coordsystem;
			coordparam=source.coordparam;
		}

		if (i_update==true) {
			if ((latitude!=IOUtils::nodata) && (longitude!=IOUtils::nodata)) {
				convert_from_WGS84(latitude, longitude, easting, northing);
			} else {
				convert_to_WGS84(easting, northing, latitude, longitude);
			}
		}
	}
}

/**
* @brief returns the epsg code of the current projection
* @return epsg code
*/
short int Coords::getEPSG() const
{
	return CoordsAlgorithms::str_to_EPSG(coordsystem, coordparam);
}

/**
* @brief set the current projection to a given EPSG-defined projection
* @param epsg epsg code
*/
void Coords::setEPSG(const int& epsg) {
//TODO: get rid of the zone letter. This is not part of the standard and redundant (and messy)
	std::string coord_sys, coord_param;
	CoordsAlgorithms::EPSG_to_str(epsg, coord_sys, coord_param);
	setProj(coord_sys, coord_param);
}

/////////////////////////////////////////////////////private methods
/**
* @brief Method converting towards WGS84
* @param[in] i_easting easting of the coordinate to convert
* @param[in] i_northing northing of the coordinate to convert
* @param[out] o_latitude converted latitude
* @param[out] o_longitude converted longitude
*/
void Coords::convert_to_WGS84(double i_easting, double i_northing, double& o_latitude, double& o_longitude) const
{
	if ((i_easting!=IOUtils::nodata) && (i_northing!=IOUtils::nodata)) {
		if (coordsystem=="UTM") CoordsAlgorithms::UTM_to_WGS84(i_easting, i_northing, coordparam, o_latitude, o_longitude);
		else if (coordsystem=="UPS") CoordsAlgorithms::UPS_to_WGS84(i_easting, i_northing, coordparam, o_latitude, o_longitude);
		else if (coordsystem=="CH1903") CoordsAlgorithms::CH1903_to_WGS84(i_easting, i_northing, o_latitude, o_longitude);
		else if (coordsystem=="CH1903+") CoordsAlgorithms::CH1903_to_WGS84(i_easting-2.e6, i_northing-1.e6, o_latitude, o_longitude);
		else if (coordsystem=="LOCAL") local_to_WGS84(i_easting, i_northing, o_latitude, o_longitude);
		else if (coordsystem=="PROJ4") CoordsAlgorithms::PROJ4_to_WGS84(i_easting, i_northing, coordparam, o_latitude, o_longitude);
		else if (coordsystem=="NULL") NULL_to_WGS84(i_easting, i_northing, o_latitude, o_longitude);
		else throw UnknownValueException("Unknown coordinate system \""+coordsystem+"\"", AT);
	} else {
		o_latitude = IOUtils::nodata;
		o_longitude = IOUtils::nodata;
	}
}

/**
* @brief Method converting towards WGS84
* @param[in] i_latitude latitude of the coordinate to convert
* @param[in] i_longitude longitude of the coordinate to convert
* @param[out] o_easting converted easting
* @param[out] o_northing converted northing
*/
void Coords::convert_from_WGS84(double i_latitude, double i_longitude, double& o_easting, double& o_northing) const
{
	if ((i_latitude!=IOUtils::nodata) && (i_longitude!=IOUtils::nodata)) {
		if (coordsystem=="UTM") CoordsAlgorithms::WGS84_to_UTM(i_latitude, i_longitude, coordparam, o_easting, o_northing);
		else if (coordsystem=="UPS") CoordsAlgorithms::WGS84_to_UPS(i_latitude, i_longitude, coordparam, o_easting, o_northing);
		else if (coordsystem=="CH1903") CoordsAlgorithms::WGS84_to_CH1903(i_latitude, i_longitude, o_easting, o_northing);
		else if (coordsystem=="CH1903+") {
			CoordsAlgorithms::WGS84_to_CH1903(i_latitude, i_longitude, o_easting, o_northing);
			o_easting += 2.e6;
			o_northing += 1.e6;
		}
		else if (coordsystem=="LOCAL") WGS84_to_local(i_latitude, i_longitude, o_easting, o_northing);
		else if (coordsystem=="PROJ4") CoordsAlgorithms::WGS84_to_PROJ4(i_latitude, i_longitude, coordparam, o_easting, o_northing);
		else if (coordsystem=="NULL") WGS84_to_NULL(i_latitude, i_longitude, o_easting, o_northing);
		else throw UnknownValueException("Unknown coordinate system \""+coordsystem+"\"", AT);
	} else {
		o_easting = IOUtils::nodata;
		o_northing = IOUtils::nodata;
	}
}

void Coords::distance(const Coords& destination, double& o_distance, double& o_bearing) const {
//HACK: this is the 2D distance, it does not work in 3D!!
	if (isSameProj(destination)) {
		//we can use simple cartesian grid arithmetic
		o_distance = sqrt( Optim::pow2(easting - destination.getEasting()) + Optim::pow2(northing - destination.getNorthing()) );
		o_bearing = atan2( northing - destination.getNorthing() , easting - destination.getEasting() );
		o_bearing = fmod( o_bearing*Cst::to_deg+360. , 360. );
	} else {
		switch(distance_algo) {
			case GEO_COSINE:
				o_distance = CoordsAlgorithms::cosineDistance(latitude, longitude, destination.getLat(), destination.getLon(), o_bearing);
				break;
			case GEO_VINCENTY:
				o_distance = CoordsAlgorithms::VincentyDistance(latitude, longitude, destination.getLat(), destination.getLon(), o_bearing);
				break;
			default:
				throw InvalidArgumentException("Unsupported distance algorithm", AT);
		}
	}
}

/**
* @brief Coordinate conversion: from WGS84 Lat/Long to local grid as given in coordparam
* @param lat_in Decimal Latitude
* @param long_in Decimal Longitude
* @param east_out easting coordinate (target system)
* @param north_out northing coordinate (target system)
*/
void Coords::WGS84_to_local(double lat_in, double long_in, double& east_out, double& north_out) const
{
	double alpha;
	double dist;

	if ((ref_latitude==IOUtils::nodata) || (ref_longitude==IOUtils::nodata)) {
		east_out = IOUtils::nodata;
		north_out = IOUtils::nodata;
		//throw InvalidArgumentException("No reference coordinate provided for LOCAL projection", AT);
	} else {
		switch(distance_algo) {
			case GEO_COSINE:
				dist = CoordsAlgorithms::cosineDistance(ref_latitude, ref_longitude, lat_in, long_in, alpha);
				break;
			case GEO_VINCENTY:
				dist = CoordsAlgorithms::VincentyDistance(ref_latitude, ref_longitude, lat_in, long_in, alpha);
				break;
			default:
				throw InvalidArgumentException("Unsupported distance algorithm", AT);
		}

		east_out = dist*sin(alpha*Cst::to_rad);
		north_out = dist*cos(alpha*Cst::to_rad);
	}
}

/**
* @brief Coordinate conversion: from local grid as given in coordparam to WGS84 Lat/Long
* @param east_in easting coordinate (in local system)
* @param north_in northing coordinate (in local system)
* @param lat_out Decimal Latitude
* @param long_out Decimal Longitude
*/
void Coords::local_to_WGS84(double east_in, double north_in, double& lat_out, double& long_out) const
{
	const double dist = sqrt( Optim::pow2(east_in) + Optim::pow2(north_in) );
	const double bearing = fmod( atan2(east_in, north_in)*Cst::to_deg+360. , 360.);

	if ((ref_latitude==IOUtils::nodata) || (ref_longitude==IOUtils::nodata)) {
		lat_out = IOUtils::nodata;
		long_out = IOUtils::nodata;
		//throw InvalidArgumentException("No reference coordinate provided for LOCAL projection", AT);
	} else {
		switch(distance_algo) {
			case GEO_COSINE:
				CoordsAlgorithms::cosineInverse(ref_latitude, ref_longitude, dist, bearing, lat_out, long_out);
				break;
			case GEO_VINCENTY:
				CoordsAlgorithms::VincentyInverse(ref_latitude, ref_longitude, dist, bearing, lat_out, long_out);
				break;
			default:
				throw InvalidArgumentException("Unsupported distance algorithm", AT);
		}
	}
}

void Coords::NULL_to_WGS84(double /*east_in*/, double /*north_in*/, double& /*lat_out*/, double& /*long_out*/) const
{
	throw InvalidArgumentException("The projection has not been initialized!", AT);
}

void Coords::WGS84_to_NULL(double /*lat_in*/, double /*long_in*/, double& /*east_out*/, double& /*north_out*/) const
{
	throw InvalidArgumentException("The projection has not been initialized!", AT);
}

void Coords::clearCoordinates() {
//sets safe defaults for all internal variables (except function pointers and maps)
	latitude = longitude = IOUtils::nodata;
	altitude = IOUtils::nodata;
	easting = northing = IOUtils::nodata;
	grid_i = grid_j = grid_k = IOUtils::inodata;
	validIndex = false;
}

void Coords::setDefaultValues() {
//sets safe defaults for all internal variables (except function pointers and maps)
	clearCoordinates();
	ref_latitude = ref_longitude = IOUtils::nodata;
	distance_algo = GEO_COSINE;
}

} //end namespace
