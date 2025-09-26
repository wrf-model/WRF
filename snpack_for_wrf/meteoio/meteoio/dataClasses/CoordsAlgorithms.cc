/***********************************************************************************/
/*  Copyright 2015 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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

#include <meteoio/dataClasses/CoordsAlgorithms.h>
#include <meteoio/IOExceptions.h>
#include <meteoio/MathOptim.h>
#include <meteoio/IOUtils.h>

#include <sstream>
#include <cstdio> //for sscanf
#include <iomanip> //for setprecision

#ifdef PROJ4
	#include <proj_api.h>
#endif

namespace mio {

const struct CoordsAlgorithms::ELLIPSOID CoordsAlgorithms::ellipsoids[12] = {
	{ 6378137.,        6356752.31424 }, ///< E_WGS84
	{ 6378135.,        6356750.52 }, /// E_WGS72
	{ 6378137.,        6356752.3141 }, ///< E_GRS80
	{ 6377563.396,  6356256.909 }, ///< E_AIRY
	{ 6378388.,        6356911.946 }, ///< E_INTL1924
	{ 6378249.145,  6356514.86955 }, ///< E_CLARKE1880
	{ 6378206.4,      6356583.8 }, ///< E_CLARKE1866
	{ 6378160.,        6356774.719 }, ///< E_GRS67
	{ 6377299.365,  6356098.359 }, ///<E_EVEREST1830
	{ 6378136.6,      6356751.9 }, ///< E_IERS2003
	{ 6378245.,        6356863.019 }, ///< E_KRASSOVSKY
	{ 6370000.,        6370000. } ///< spherical earth
};

/**
* @brief Print a nicely formatted lat/lon in degrees, minutes, seconds
* @return lat/lon
*/
std::string CoordsAlgorithms::printLatLon(const double& latitude, const double& longitude) {
	std::ostringstream dms;
	dms << "(" << decimal_to_dms(latitude) << " , " << decimal_to_dms(longitude) << ")";

	return dms.str();
}

/**
* @brief Parse a latitude or longitude
* It can be formatted as any of the following examples:
* - 46&deg; 48' 03" (with or without spaces, decimal or integer numbers)
* - 46d 48' 03" (with or without spaces, decimal or integer numbers)
* - 46 48' 03" (with spaces, decimal or integer numbers)
* - 48&deg; 48.02'(with or without spaces, decimal or integer numbers)
* - 46d 48.02'(with or without spaces, decimal or integer numbers)
* - 46 48.02'(with spaces, decimal or integer numbers)
* - 46.802&deg;
* - 46.802d
* - 46.802
* @param[in] dms string containing the coordinate
* @return coordinate in decimal
*/
double CoordsAlgorithms::dms_to_decimal(const std::string& dms) {
	double d=IOUtils::nodata, m=IOUtils::nodata, s=IOUtils::nodata, decimal=IOUtils::nodata;

	if 	((sscanf(dms.c_str(), "%lf°%lf'%lf\"", &d, &m ,&s) < 3) &&
		(sscanf(dms.c_str(), "%lf° %lf' %lf\"", &d, &m ,&s) < 3) &&
		(sscanf(dms.c_str(), "%lfd%lf'%lf\"", &d, &m ,&s) < 3) &&
		(sscanf(dms.c_str(), "%lfd %lf' %lf\"", &d, &m ,&s) < 3) &&
		(sscanf(dms.c_str(), "%lf %lf' %lf\"", &d, &m ,&s) < 3) &&
		(sscanf(dms.c_str(), "%lf° %lf'", &d, &m) < 2) &&
		(sscanf(dms.c_str(), "%lf°%lf'", &d, &m) < 2) &&
		(sscanf(dms.c_str(), "%lfd %lf'", &d, &m) < 2) &&
		(sscanf(dms.c_str(), "%lfd%lf'", &d, &m) < 2) &&
		(sscanf(dms.c_str(), "%lf %lf'", &d, &m) < 2) &&
		(sscanf(dms.c_str(), "%lf°", &d) < 1) &&
		(sscanf(dms.c_str(), "%lfd", &d) < 1) &&
		(sscanf(dms.c_str(), "%lf", &d) < 1)) {
			throw InvalidFormatException("Can not parse given latitude or longitude: "+dms,AT);
	}

	decimal = fabs(d);
	if (m!=IOUtils::nodata) decimal += m/60.;
	if (s!=IOUtils::nodata) decimal += s/3600.;

	if (d<0.) return (-decimal);
	else return decimal;
}

/**
* @brief Parse a latitude-longitude pair
* It can be formatted as any of the following examples:
* - lat lon (without any spaces in the latitude or longitude string)
* - lat/lon
* - (lat;lon)
* - (lat,lon)
* @param[in] coordinates string containing the coordinates
* @param[out] lat parsed latitude
* @param[out] lon parsed longitude
*/
void CoordsAlgorithms::parseLatLon(const std::string& coordinates, double &lat, double &lon)
{
	static const size_t len=64;
	char lat_str[len]=""; //each string must be able to accomodate the whole length to avoid buffer overflow
	char lon_str[len]="";

	if (coordinates.size()>=len)
		throw InvalidFormatException("Given lat/lon string is too long! ",AT);

	if     ((sscanf(coordinates.c_str(), "%[0-9.,°d'\"-] %[0-9.,°d'\"-]", lat_str, lon_str) < 2) &&
		(sscanf(coordinates.c_str(), "%[0-9.,°d'\"- ]/%[0-9.,°d'\"- ]", lat_str, lon_str) < 2) &&
		(sscanf(coordinates.c_str(), "(%[0-9.,°d'\"- ];%[0-9.,°d'\"- ])", lat_str, lon_str) < 2) &&
		(sscanf(coordinates.c_str(), "(%[0-9.°d'\"- ],%[0-9.°d'\"- ])", lat_str, lon_str) < 2)) {
			throw InvalidFormatException("Can not parse given lat/lon: "+coordinates,AT);
	}

	lat = dms_to_decimal(std::string(lat_str));
	lon = dms_to_decimal(std::string(lon_str));
}

/**
* @brief Converts a decimal latitude or longitude to degrees, minutes, seconds
* It formats its arguments as in the following example: 46&deg;48'03"
* @param[in] decimal decimal coordinate to convert
* @return string containing the formatted coordinate
*/
std::string CoordsAlgorithms::decimal_to_dms(const double& decimal) {
	std::ostringstream dms;
	const double abs_dec = fabs(decimal);
	const int d = (int)floor(abs_dec);
	const int m = (int)floor( (abs_dec - (double)d)*60. );
	const double s = 3600.*(abs_dec - (double)d) - 60.*(double)m;

	if (decimal<0.)
		dms << "-";
	dms << d << "°" << m << "'" << std::fixed << std::setprecision(6) << s << "\"";
	return dms.str();
}

/**
* @brief Lenght of one degree of latitude
* This returns the lenght in meters of one degree of latitude around the given latitude
* (ie: latitude-.5, latitude+.5). See https://en.wikipedia.org/wiki/Latitude#The_length_of_a_degree_of_latitude
* @param[in] latitude latitude where to perform the computation
* @return lenght of one degree of latitude
*/
double CoordsAlgorithms::lat_degree_lenght(const double& latitude) {
	static const double a = ellipsoids[E_WGS84].a; //major ellipsoid semi-axis
	static const double b = ellipsoids[E_WGS84].b;	//minor ellipsoid semi-axis
	static const double e2 = (a*a-b*b) / (a*a);	//ellispoid eccentricity, squared

	const double degree_length = (Cst::PI*a*(1.-e2)) / ( 180.*pow(1.-e2*Optim::pow2(sin(latitude*Cst::to_rad)), 1.5) );
	return fabs( degree_length );
}

/**
* @brief Lenght of one degree of longitude
* This returns the lenght in meters of one degree of longitude around the given latitude
* (ie: latitude-.5, latitude+.5). See https://en.wikipedia.org/wiki/Latitude#The_length_of_a_degree_of_latitude
* @param[in] latitude latitude where to perform the computation
* @return lenght of one degree of longitude
*/
double CoordsAlgorithms::lon_degree_lenght(const double& latitude) {
	static const double a = ellipsoids[E_WGS84].a; //major ellipsoid semi-axis
	static const double b = ellipsoids[E_WGS84].b;	//minor ellipsoid semi-axis
	static const double e2 = (a*a-b*b) / (a*a);	//ellispoid eccentricity, squared

	const double degree_length = (Cst::PI*a*cos(latitude*Cst::to_rad)) / ( 180.*sqrt(1.-e2*Optim::pow2(sin(latitude*Cst::to_rad))) );
	return fabs( degree_length );
}

/**
* @brief Convert rotated lat/lon into geographic lat/lon
* Rotated coordinates are created by moving the North pole by a given offset along latitude and longitude.
* The goal is to put the equator through the center of the domain of interest, so a lat/lon grid can easily be
* approximated by a tangential cartesian coordinate system.
* (see http://www.cosmo-model.org/content/model/documentation/core/default.htm, part I, chapter 3.3 for more)
* @note To convert South Pole coordinates to North Pole coordinates, multiply the latitude by -1 and add 180 to the longitude.
* @param[in] lat_N North pole latitude offset
* @param[in] lon_N North pole longitude offset
* @param[in] lat_rot rotated latitude
* @param[in] lon_rot rotated longitude
* @param[out] lat_true geographic latitude
* @param[out] lon_true geographic longitude
*/
void CoordsAlgorithms::rotatedToTrueLatLon(const double& lat_N, const double& lon_N, const double& lat_rot, const double& lon_rot, double &lat_true, double &lon_true)
{
	if (lat_N==IOUtils::nodata || lon_N==IOUtils::nodata || lat_rot==IOUtils::nodata || lon_rot==IOUtils::nodata) {
		lat_true = IOUtils::nodata;
		lon_true = IOUtils::nodata;
		return;
	}

	const double lat_pole_rad = lat_N*Cst::to_rad;
	const double lat_rot_rad = lat_rot*Cst::to_rad;
	const double lon_rot_rad = (fmod(lon_rot+180., 360.)-180.)*Cst::to_rad; //putting lon_rot_rad in [-PI; PI]

	lat_true = asin( sin(lat_rot_rad)*sin(lat_pole_rad) + cos(lat_rot_rad)*cos(lon_rot_rad)*cos(lat_pole_rad) ) * Cst::to_deg;
	lon_true = atan2( cos(lat_rot_rad)*sin(lon_rot_rad) , (sin(lat_pole_rad)*cos(lat_rot_rad)*cos(lon_rot_rad) - sin(lat_rot_rad)*cos(lat_pole_rad)) )*Cst::to_deg + lon_N;
	lon_true -= 180.; //HACK
	lon_true = fmod(lon_true+180., 360.)-180.; //putting lon_rot_rad in [-180; 180]
}

/**
* @brief Convert geographic lat/lon into rotated lat/lon
* Rotated coordinates are created by moving the North pole by a given offset along latitude and longitude.
* The goal is to put the equator through the center of the domain of interest, so a lat/lon grid can easily be
* approximated by a tangential cartesian coordinate system.
* (see http://www.cosmo-model.org/content/model/documentation/core/default.htm, part I, chapter 3.3 for more)
* @note To convert South Pole coordinates to North Pole coordinates, multiply the latitude by -1 and add 180 to the longitude.
* @param[in] lat_N North pole latitude offset
* @param[in] lon_N North pole longitude offset
* @param[in] lat_true geographic latitude
* @param[in] lon_true geographic longitude
* @param[out] lat_rot rotated latitude
* @param[out] lon_rot rotated longitude
*/
void CoordsAlgorithms::trueLatLonToRotated(const double& lat_N, const double& lon_N, const double& lat_true, const double& lon_true, double &lat_rot, double &lon_rot)
{
	if (lat_N==IOUtils::nodata || lon_N==IOUtils::nodata || lat_true==IOUtils::nodata || lon_true==IOUtils::nodata) {
		lat_rot = IOUtils::nodata;
		lon_rot = IOUtils::nodata;
		return;
	}

	const double lon_norm = (fmod(lon_true+180., 360.)-180.)*Cst::to_rad; //putting lon_true in [-PI; PI]
	const double lat_true_rad = lat_true*Cst::to_rad;
	const double lat_pole_rad = lat_N*Cst::to_rad;
	const double lon_pole_rad = lon_N*Cst::to_rad;
	const double delta_lon_rad = lon_norm-lon_pole_rad;

	lat_rot = asin( sin(lat_true_rad)*sin(lat_pole_rad) + cos(lat_true_rad)*cos(lat_pole_rad)*cos(delta_lon_rad) ) * Cst::to_deg;
	lon_rot = atan2( cos(lat_true_rad)*sin(delta_lon_rad) , (cos(lat_true_rad)*sin(lat_pole_rad)*cos(delta_lon_rad) - sin(lat_true_rad)*cos(lat_pole_rad)) ) * Cst::to_deg;
	lon_rot += 180.; //HACK
	lon_rot = fmod(lon_rot+180., 360.)-180.; //putting lon_rot_rad in [-180; 180]
}

/**
* @brief Molodensky datum transformation.
* This converts lat/lon from one datum to another (for example, NAD27 to WGS84, or spherical to WGS84). The ellipsoid
* names (see CoordsAlgorithms::ELLIPSOIDS_NAMES) and delta_x/y/z that describe the datums must be provided (if no deltas are provided, they are assumed to be zeroes).
* For more information, see https://en.wikipedia.org/wiki/Geographic_coordinate_conversion#Molodensky_transformation
* or http://www.colorado.edu/geography/gcraft/notes/datum/gif/molodens.gif
* @param[in] lat_in input latitude (degrees)
* @param[in] lon_in input longitude (degrees)
* @param[in] alt_in input altitude above sea level
* @param[in] ellipsoid_in ellipsoid of the input datum (see CoordsAlgorithms::ELLIPSOIDS_NAMES)
* @param[out] lat_out output latitude (degrees)
* @param[out] lon_out output longitude (degrees)
* @param[out] alt_out output altitude above sea level
* @param[out] ellipsoid_out ellipsoid of the output datum (see CoordsAlgorithms::ELLIPSOIDS_NAMES)
* @param[in] delta_x Origin shift, x coordinate (default: 0)
* @param[in] delta_y Origin shift, y coordinate (default: 0)
* @param[in] delta_z Origin shift, z coordinate (default: 0)
*/
void CoordsAlgorithms::Molodensky(const double& lat_in, const double& lon_in, const double& alt_in, const ELLIPSOIDS_NAMES& ellipsoid_in, double &lat_out, double &lon_out, double &alt_out, const ELLIPSOIDS_NAMES& ellipsoid_out, const double& delta_x, const double& delta_y, const double& delta_z)
{
	//FROM datum parameters
	const double from_h = alt_in;
	const double from_a = ellipsoids[ellipsoid_in].a;
	const double from_b = ellipsoids[ellipsoid_in].b;
	const double from_f = (from_a - from_b) / from_a; //flattening
	const double from_es = (from_a*from_a - from_b*from_b) / (from_a*from_a); //first eccentricity, squared

	//TO datum parameters
	const double to_a = ellipsoids[ellipsoid_out].a;
	const double to_b = ellipsoids[ellipsoid_out].b;
	const double to_f = (to_a - to_b) / to_a; //flattening

	//radii of curvature
	const double bda = from_b / from_a;
	const double delta_a = to_a - from_a;
	const double delta_f =to_f - from_f;
	const double sin_lat = sin( lat_in*Cst::to_rad ), cos_lat = cos( lat_in*Cst::to_rad );
	const double sin_lon = sin( lon_in*Cst::to_rad ), cos_lon = cos( lon_in*Cst::to_rad );
	const double Rn = from_a / ( sqrt(1. - from_es * Optim::pow2(sin_lat)) ); //radius of curvature in prime vertical
	const double Rm = from_a * (1. - from_es) / pow(1. - from_es*Optim::pow2(sin_lat), 1.5); //radius of curvature in prime meridian

	//geodetic position shifts
	const double numerator1 = -delta_x*sin_lat*cos_lon - delta_y*sin_lat*sin_lon + delta_z*cos_lat + delta_a * (Rn*from_es*sin_lat*cos_lat) / from_a;
	const double numerator2 = delta_f * (Rm/bda + Rn*bda) * sin_lat * cos_lat;
	const double delta_lat = (numerator1 + numerator2) / (Rm + from_h);
	const double delta_lon = (-delta_x*sin_lon + delta_y*cos_lon) / ((Rn + from_h)*cos_lat);
	const double delta_h = delta_x*cos_lat*cos_lon + delta_y*cos_lat*sin_lon + delta_z*sin_lat - delta_a*from_a/Rn + delta_f*bda*Rn*sin_lat*sin_lat;

	//write out TO coordinates
	lat_out = lat_in + delta_lat*Cst::to_deg;
	lon_out = lon_in + delta_lon*Cst::to_deg;
	alt_out = alt_in + delta_h;
}

/**
* @brief returns the epsg code matching a provided string representation
* For example, when given "CH1903" with empty coordparam, it will return "21781". For "LOCAL" coordinates, it returns IOUtils::snodata.
* @param[in] coordsystem string representation of the coordinate system
* @param[in] coordparam string representation of the optional coordinate system parameters (such as zone for utm, etc) 
* @return epsg code
*/
short int CoordsAlgorithms::str_to_EPSG(const std::string& coordsystem, const std::string& coordparam) 
{
	if (coordsystem=="CH1903") return 21781;
	if (coordsystem=="CH1903+") return 2056;
	if (coordsystem=="UTM") {
		//UTM Zone information
		short int zoneNumber;
		char zoneLetter;
		parseUTMZone(coordparam, zoneLetter, zoneNumber);
		if (zoneLetter >= 'N') {
			//northern hemisphere. We KNOW it will fit in a short int
			return (static_cast<short int>(32600+zoneNumber));
		} else {
			//southern hemisphere. We KNOW it will fit in a short int
			return (static_cast<short int>(32700+zoneNumber));
		}
	}
	if (coordsystem=="UPS") {
		//UPS Zone
		if (coordparam == "N") {
			//northern hemisphere
			return (5041);
		} else {
			//southern hemisphere
			return (5042);
		}
	}
	if (coordsystem=="PROJ4") {
		const int tmp = atoi(coordparam.c_str());
		if (tmp<0 || tmp>32767) {
			std::ostringstream ss;
			ss << "Invalid EPSG code argument: " << tmp << ". It should be between 0 and 32767! (please check EPSG registry)";
			throw InvalidArgumentException(ss.str(), AT);
		}
		return static_cast<short>(tmp);
	}

	//all others have no associated EPSG code
	return IOUtils::snodata;
}

/**
* @brief Does a given epsg code describe UTM coordinates?
* @param[in] epsg epsg code
* @return true if this is for UTM coordinates, false otherwise
*/
bool CoordsAlgorithms::isUTM(const int& epsg)
{
	//return true for ERTS as well as WGC84 based UTM coordinates
	return ((epsg>=25828 && epsg<=25837) || (epsg>=32601 && epsg<=32660) || (epsg>=32701 && epsg<=32760));
}

/**
* @brief Build the string representation for a given EPSG code.
* @note We assume that the ETRS datum is equal to the WGS84 (although there is a .5m difference and 
* <A href="https://confluence.qps.nl/qinsy/en/how-to-deal-with-etrs89-datum-and-time-dependent-transformation-parameters-45353274.html">growing</A>). 
* Therefore, all ETRS coordinates will be written out as WGS84 coordinates.
* @param[in] epsg epsg code
* @param[out] coordsystem string representation of the coordinate system
* @param[out] coordparam string representation of the optional coordinate system parameters (such as zone for utm, etc) 
*/
void CoordsAlgorithms::EPSG_to_str(const int& epsg, std::string& coordsystem, std::string& coordparam)
{
	coordsystem.clear();
	coordparam.clear();
	//TODO: get rid of the zone letter. This is not part of the standard and redundant (and messy)
	if (epsg<0 || epsg>32767) {
		std::ostringstream ss;
		ss << "Invalid epsg code " << epsg << " (it should be between 0 and 32767)!";
		throw InvalidArgumentException(ss.str(), AT);
	}

	if (epsg==21781) {
		coordsystem.assign("CH1903");
		return;
	}
	if (epsg==2056) {
		coordsystem.assign("CH1903+");
		return;
	}
	if ((epsg>=25828) && (epsg<=25837)) {
		//as ETRS89 and further, this is only Europe. We assume ETRS==WGS84 although there is ~.5m difference
		coordsystem.assign("UTM");
		const int zoneNumber = epsg-25800;
		std::ostringstream osstream;
		osstream << zoneNumber << "N";
		coordparam=osstream.str();
		return;
	}
	if ((epsg>=32601) && (epsg<=32660)) {
		//northern hemisphere
		coordsystem.assign("UTM");
		const int zoneNumber = epsg-32600;
		std::ostringstream osstream;
		osstream << zoneNumber << "N";
		coordparam=osstream.str();
		return;
	}
	if ((epsg>=32701) && (epsg<=32760)) {
		//southern hemisphere
		coordsystem.assign("UTM");
		const int zoneNumber = epsg-32700;
		std::ostringstream osstream;
		osstream << zoneNumber << "M";
		coordparam=osstream.str();
		return;
	}
	if ((epsg==5041 || epsg==5042)) {
		coordsystem.assign("UPS");
		coordparam = (epsg==5041)? "N" : "S";
		return;
	}
	
	//anything else has to be processed by proj4
	coordsystem.assign("PROJ4");
	std::ostringstream osstream;
	osstream << epsg;
	coordparam=osstream.str();
}

/**
* @brief Coordinate conversion: from WGS84 Lat/Long to Swiss grid
* See https://www.swisstopo.admin.ch/de/karten-daten-online/calculation-services.html in the "Documents" section for more.
* @param[in] lat_in Decimal Latitude
* @param[in] long_in Decimal Longitude
* @param[out] east_out easting coordinate (Swiss system)
* @param[out] north_out northing coordinate (Swiss system)
*/
void CoordsAlgorithms::WGS84_to_CH1903(const double& lat_in, const double& long_in, double& east_out, double& north_out)
{
	//converts WGS84 coordinates (lat,long) to the Swiss coordinates. See http://geomatics.ladetto.ch/ch1903_wgs84_de.pdf
	//The elevation is supposed to be above sea level, so it does not require any conversion
	//lat and long must be decimal (and they will be converted to seconds)
	const double phi_p = (lat_in*3600. - 169028.66) / 10000.;
	const double lambda_p = (long_in*3600. - 26782.5) / 10000.;

	east_out = 600072.37
		+ 211455.93	* lambda_p
		- 10938.51	* lambda_p * phi_p
		- 0.36		* lambda_p * (phi_p*phi_p)
		- 44.54		* (lambda_p*lambda_p*lambda_p);

	north_out = 200147.07
		+ 308807.95	* phi_p
		+ 3745.25	* (lambda_p*lambda_p)
		+ 76.63		* (phi_p*phi_p)
		- 194.56	* (lambda_p*lambda_p) * phi_p
		+ 119.79	* (phi_p*phi_p*phi_p);

	/*// if necessary for the elevation, uncomment this block
	h_out = h_in - 49.55
		+ 2.73		* lambda_p
		+ 6.94		* phi_p;
	*/
}

/**
* @brief Coordinate conversion: from Swiss grid to WGS84 Lat/Long
* See https://www.swisstopo.admin.ch/de/karten-daten-online/calculation-services.html in the "Documents" section for more.
* @param[in] east_in easting coordinate (Swiss system)
* @param[in] north_in northing coordinate (Swiss system)
* @param[out] lat_out Decimal Latitude
* @param[out] long_out Decimal Longitude
*/
void CoordsAlgorithms::CH1903_to_WGS84(const double& east_in, const double& north_in, double& lat_out, double& long_out)
{
	//converts Swiss coordinates to WGS84 coordinates (lat,long). See http://geomatics.ladetto.ch/ch1903_wgs84_de.pdf
	//The elevation is supposed to be above sea level, so it does not require any conversion
	//lat and long are decimal
	const double y_p = (east_in - 600000.) / 1000000.;
	const double x_p = (north_in - 200000.) / 1000000.;

	const double lambda_p = 2.6779094
		+ 4.728982	* y_p
		+ 0.791484	* y_p * x_p
		+ 0.1306	* y_p * (x_p*x_p)
		- 0.0436	* (y_p*y_p*y_p);

	const double phi_p = 16.9023892
		+ 3.238272	* x_p
		- 0.270978	* (y_p*y_p)
		- 0.002528	* (x_p*x_p)
		- 0.0447	* (y_p*y_p) * x_p
		- 0.0140	* (x_p*x_p*x_p);

	lat_out = phi_p * 100./36.;
	long_out = lambda_p * 100./36.;

	/*// if necessary for the elevation, uncomment this block
	h_out = h_in + 49.55
		- 12.60		* y_p
		- 22.64		* x_p;
	*/
}

int CoordsAlgorithms::getUTMZone(const double& i_latitude, const double& i_longitude, std::string& zone_out)
{//This routine determines the correct UTM letter designator for the given latitude
//UTM limits its coverage to [80S , 84N], outside of this, returns Y/Z/A/B for the zone

	//computing zone number, assuming longitude in [-180. ; 180[
	int ZoneNumber = int((i_longitude + 180.)/6.) + 1;

	// Special zones for Scandinavia
	if ( i_latitude >= 72.0 && i_latitude < 84.0 ) {
		if (      i_longitude >= 0.0  && i_longitude <  9.0 ) ZoneNumber = 31;
		else if ( i_longitude >= 9.0  && i_longitude < 21.0 ) ZoneNumber = 33;
		else if ( i_longitude >= 21.0 && i_longitude < 33.0 ) ZoneNumber = 35;
		else if ( i_longitude >= 33.0 && i_longitude < 42.0 ) ZoneNumber = 37;
	 }
	if ( i_latitude >= 56.0 && i_latitude < 64.0 && i_longitude >= 3.0 && i_longitude < 12.0 ) {
		ZoneNumber = 32;
	}

	//getting zone letter
	char zoneLetter='Z';
	if     ((0 >= i_longitude) && (i_latitude >  84)) zoneLetter = 'Y';
	else if ((0 <  i_longitude) && (i_latitude >  84)) zoneLetter = 'Z';
	else if ((84 >= i_latitude) && (i_latitude >= 72)) zoneLetter = 'X';
	else if ((72 > i_latitude) && (i_latitude >= 64)) zoneLetter = 'W';
	else if ((64 > i_latitude) && (i_latitude >= 56)) zoneLetter = 'V';
	else if ((56 > i_latitude) && (i_latitude >= 48)) zoneLetter = 'U';
	else if ((48 > i_latitude) && (i_latitude >= 40)) zoneLetter = 'T';
	else if ((40 > i_latitude) && (i_latitude >= 32)) zoneLetter = 'S';
	else if ((32 > i_latitude) && (i_latitude >= 24)) zoneLetter = 'R';
	else if ((24 > i_latitude) && (i_latitude >= 16)) zoneLetter = 'Q';
	else if ((16 > i_latitude) && (i_latitude >= 8)) zoneLetter = 'P';
	else if (( 8 > i_latitude) && (i_latitude >= 0)) zoneLetter = 'N'; //first zone of Northern hemisphere
	else if (( 0 > i_latitude) && (i_latitude >= -8)) zoneLetter = 'M'; //first zone of Southern hemisphere
	else if ((-8 > i_latitude) && (i_latitude >= -16)) zoneLetter = 'L';
	else if ((-16 > i_latitude) && (i_latitude >= -24)) zoneLetter = 'K';
	else if ((-24 > i_latitude) && (i_latitude >= -32)) zoneLetter = 'J';
	else if ((-32 > i_latitude) && (i_latitude >= -40)) zoneLetter = 'H';
	else if ((-40 > i_latitude) && (i_latitude >= -48)) zoneLetter = 'G';
	else if ((-48 > i_latitude) && (i_latitude >= -56)) zoneLetter = 'F';
	else if ((-56 > i_latitude) && (i_latitude >= -64)) zoneLetter = 'E';
	else if ((-64 > i_latitude) && (i_latitude >= -72)) zoneLetter = 'D';
	else if ((-72 > i_latitude) && (i_latitude >= -80)) zoneLetter = 'C';
	else if ((0 >=  i_longitude) && (i_latitude <= -80)) zoneLetter = 'A';
	else if ((0 <   i_longitude) && (i_latitude <= -80)) zoneLetter = 'B';

	std::ostringstream zone;
	zone << ZoneNumber << zoneLetter;
	zone_out = zone.str();

	return ZoneNumber;
}

/**
* @brief Coordinate conversion: from WGS84 Lat/Long to UTM grid.
* For more, see http://www.oc.nps.edu/oc2902w/maps/utmups.pdf, USGS Bulletin 1532 or http://earth-info.nga.mil/GandG/publications/tm8358.2/TM8358_2.pdf,
* http://www.uwgb.edu/dutchs/usefuldata/UTMFormulas.HTM or
* Chuck Gantz (http://www.gpsy.com/gpsinfo/geotoutm/).
* @param[in] lat_in Decimal Latitude
* @param[in] long_in Decimal Longitude
* @param[in] coordparam UTM zone to convert to
* @param[out] east_out easting coordinate (Swiss system)
* @param[out] north_out northing coordinate (Swiss system)
*/
void CoordsAlgorithms::WGS84_to_UTM(const double& lat_in, double long_in, const std::string& coordparam, double& east_out, double& north_out)
{
	//Geometric constants
	static const double a = ellipsoids[E_WGS84].a; //major ellipsoid semi-axis
	static const double b = ellipsoids[E_WGS84].b;	//minor ellipsoid semi-axis
	static const double e2 = (a*a-b*b) / (a*a);	//ellispoid eccentricity, squared (=(a²-b²)/a²)
	static const double eP2 = e2 / (1.-e2);	//second ellispoid eccentricity, squared (=(a²-b²)/b²)
	static const double k0 = 0.9996;	//scale factor for the projection

	//getting posistion parameters
	std::string zone;
	long_in = fmod(long_in+360.+180., 360.) - 180.; //normalized to [-180. ; 180.[
	const double Long = long_in * Cst::to_rad;
	const double Lat = lat_in * Cst::to_rad;
	int zoneNumber = getUTMZone(lat_in, long_in, zone);
	short int in_zoneNumber;
	char in_zoneLetter;
	parseUTMZone(coordparam, in_zoneLetter, in_zoneNumber);
	if (in_zoneNumber!=zoneNumber) {
		/*std::cerr << "[W] requested UTM zone is not appropriate for the given coordinates. Normally, It should be zone ";
		std::cerr << zoneNumber << "\n";*/
		zoneNumber = in_zoneNumber;
	}
	const double long0 = (double)((zoneNumber - 1)*6 - 180 + 3) * Cst::to_rad; //+3 puts origin in middle of zone

	//Geometrical parameters
	const double nu = a / sqrt(1.-e2*Optim::pow2(sin(Lat))); //radius of curvature of the earth perpendicular to the meridian plane
	const double p = (Long-long0);

	//calculating first the coefficients of the series, then the Meridional Arc M itself
	static const double n = (a-b)/(a+b);
	static const double n2=n*n, n3=n*n*n, n4=n2*n2, n5=n2*n3, n6=n3*n3;
	static const double A = a           * (1. - n + 5./4.*(n2 - n3) + 81./64.*(n4 - n5));
	static const double B = (3./2.*a)   * (n - n2 + 7./8.*(n3 - n4) + 55./64.*(n5 - n6));
	static const double C = (15./16.*a) * (n2 - n3 + 3./4.*(n4 - n5));
	static const double D = (35./48.*a) * (n3 - n4 + 11./16.*(n5 - n6));
	static const double E = (315./512.*a) * (n4 - n5); //correction of ~0.03mm
	const double M = A*Lat - B*sin(2.*Lat) + C*sin(4.*Lat) - D*sin(6.*Lat) + E*sin(8.*Lat);

	//calculating the coefficients for the series
	const double K1 = M*k0;
	const double K2 = 1./4.*k0*nu*sin(2.*Lat);
	const double K3 = (k0*nu*sin(Lat)*Optim::pow3(cos(Lat))*1./24.) * (5. - Optim::pow2(tan(Lat)) + 9.*eP2*Optim::pow2(cos(Lat)) + 4.*eP2*eP2*Optim::pow4(cos(Lat)));
	const double K4 = k0*nu*cos(Lat);
	const double K5 = (k0*nu*Optim::pow3(cos(Lat))*1./6.) * (1. - Optim::pow2(tan(Lat)) + eP2*Optim::pow2(cos(Lat)));

	north_out = K1 + K2*p*p + K3*p*p*p*p;
	east_out = K4*p + K5*p*p*p + 500000.0;

	if (Lat < 0)
		north_out += 10000000.0; //offset for southern hemisphere
}

/**
* @brief Coordinate conversion: from UTM grid to WGS84 Lat/Long.
* For more, see http://www.oc.nps.edu/oc2902w/maps/utmups.pdf, USGS Bulletin 1532 or http://earth-info.nga.mil/GandG/publications/tm8358.2/TM8358_2.pdf,
* http://www.uwgb.edu/dutchs/usefuldata/UTMFormulas.HTM or
* Chuck Gantz (http://www.gpsy.com/gpsinfo/geotoutm/).
* @param[in] east_in easting coordinate (UTM)
* @param[in] north_in northing coordinate (UTM)
* @param[in] coordparam UTM zone of the easting/northing
* @param[out] lat_out Decimal Latitude
* @param[out] long_out Decimal Longitude
*/
void CoordsAlgorithms::UTM_to_WGS84(double east_in, double north_in, const std::string& coordparam, double& lat_out, double& long_out)
{
	//Geometric constants
	static const double a = ellipsoids[E_WGS84].a; //major ellipsoid semi-axis
	static const double b = ellipsoids[E_WGS84].b;	//minor ellipsoid semi-axis
	static const double e2 = (a*a-b*b) / (a*a);	//ellispoid eccentricity, squared (=(a²-b²)/a²)
	static const double eP2 = e2 / (1.-e2);	//second ellispoid eccentricity, squared (=(a²-b²)/b²)
	static const double k0 = 0.9996;		//scale factor for the projection

	//UTM Zone information
	short int zoneNumber;
	char zoneLetter;
	parseUTMZone(coordparam, zoneLetter, zoneNumber);

	//set reference parameters: central meridian of the zone, true northing and easting
	//please note that the special zones still use the reference meridian as given by their zone number (ie: even if it might not be central anymore)
	const int long0 = ((int)zoneNumber - 1)*6 - 180 + 3;  //+3 puts origin in "middle" of zone as required for the projection meridian (might not be the middle for special zones)
	if (zoneLetter<='M') {
		north_in -= 10000000.0; //offset used for southern hemisphere
	}
	east_in -= 500000.0; //longitude offset: x coordinate is relative to central meridian

	//calculating footprint latitude fp (it should be done using a few iterations)
	const double arc = north_in/k0; //Meridional arc
	const double mu = arc / (a*(1.-e2/4.-3.*e2*e2/64.-5.*e2*e2*e2/256.));
	static const double e1 = (1.-b/a) / (1.+b/a); //simplification of [1 - (1 - e2)1/2]/[1 + (1 - e2)1/2]
	static const double J1 = (3./2.*e1 - 27./32.*e1*e1*e1);
	static const double J2 = (21./16.*e1*e1 - 55./32.*e1*e1*e1*e1);
	static const double J3 = (151./96.*e1*e1*e1);
	static const double J4 = (1097./512.*e1*e1*e1*e1);
	const double fp = mu + J1*sin(2.*mu) + J2*sin(4.*mu) + J3*sin(6.*mu) + J4*sin(8.*mu);

	//calculating the parameters
	const double C1 = eP2 * Optim::pow2(cos(fp));
	const double T1 = Optim::pow2( tan(fp) );
	const double R1 = a*(1.-e2) / pow((1.-e2*Optim::pow2(sin(fp))), 1.5);
	const double N1 = a / sqrt(1.-e2*Optim::pow2(sin(fp)));
	const double D = east_in / (N1*k0);
	const double D2=D*D, D3=D*D*D;

	//calculating the coefficients of the series for latitude and longitude
	const double Q1 = N1*tan(fp)/R1;
	const double Q2 = 0.5*D2;
	const double Q3 = (5. + 3.*T1 + 10.*C1 - 4.*C1*C1 - 9.*eP2) * 1./24.*D2*D2;
	const double Q4 = (61. + 90.*T1 + 298.*C1 + 45.*T1*T1 - 3.*C1*C1 - 252.*eP2) * 1./720.*D3*D3;
	//const double Q4extra = (1385. + 3633.*T1 + 4095.*T1*T1 + 1575.*T1*T1*T1) * 1./40320.*D3*D3*D2;

	const double Q5 = D;
	const double Q6 = (1. + 2.*T1 + C1) * 1./6.*D3;
	const double Q7 = (5. - 2.*C1 + 28.*T1 - 3.*C1*C1 + 8.*eP2 + 24.*T1*T1) * 1./120.*D2*D3;
	//const double Q7extra = (61. + 662.*T1 + 1320.*T1*T1 +720.*T1*T1*T1) * 1./5040.*D3*D3*D;

	lat_out = (fp - Q1 * (Q2 - Q3 + Q4 /*+Q4extra*/))*Cst::to_deg;
	long_out = (double)long0 + ((Q5 - Q6 + Q7 /*-Q7extra*/)/cos(fp))*Cst::to_deg;
}

/**
* @brief Coordinate conversion: from WGS84 Lat/Long to Universal Polar Stereographic grid
* see J. Hager, J. Behensky, B. Drew, <i>THE UNIVERSAL GRIDS: Universal Transverse Mercator (UTM) and Universal Polar Stereographic (UPS)</i>, 1989, Defense Mapping Agency, DMATM 8358.2.
* This is valid above latitudes 84N or above 80S.
* @param[in] lat_in Decimal Latitude
* @param[in] long_in Decimal Longitude
* @param[in] coordparam UPS zone, either "N" or "S"
* @param[out] east_out easting coordinate (Swiss system)
* @param[out] north_out northing coordinate (Swiss system)
*/
void CoordsAlgorithms::WGS84_to_UPS(const double& lat_in, const double& long_in, const std::string& coordparam, double& east_out, double& north_out)
{
	static const double a = ellipsoids[E_WGS84].a; //major ellipsoid semi-axis
	static const double b = ellipsoids[E_WGS84].b;	//minor ellipsoid semi-axis
	static const double e2 = (a*a-b*b) / (a*a);	//ellispoid eccentricity, squared (=(a²-b²)/a²)
	static const double k0 = 0.994;		//scale factor for the projection
	static const double FN = 2000000.;		//false northing
	static const double FE = 2000000.;		//false easting

	static const double e = sqrt(e2);
	static const double C0 = 2.*a / sqrt(1.-e2) * pow( (1.-e)/(1.+e) , e/2.);
	const double lat_abs = fabs(lat_in); //since the computation assumes positive latitudes even for the Antarctis
	const double tan_Z2 = pow( (1.+e*sin(lat_abs*Cst::to_rad))/(1.-e*sin(lat_abs*Cst::to_rad)) , e/2. ) * tan( Cst::PI/4. - lat_abs*Cst::to_rad/2.);
	const double R = k0*C0*tan_Z2;

	if (coordparam=="N") {
		north_out = FN - R*cos(long_in*Cst::to_rad);
	} else if (coordparam=="S") {
		north_out = FN + R*cos(long_in*Cst::to_rad);
	} else {
		throw InvalidFormatException("Invalid UPS zone: "+coordparam+". It should be either N or S.",AT);
	}
	east_out = FE + R*sin(long_in*Cst::to_rad);
}

/**
* @brief Coordinate conversion: from Universal Polar Stereographic grid to WGS84 Lat/Long
* see J. Hager, J. Behensky, B. Drew, <i>THE UNIVERSAL GRIDS: Universal Transverse Mercator (UTM) and Universal Polar Stereographic (UPS)</i>, 1989, Defense Mapping Agency, DMATM 8358.2.
* This is valid above latitudes 84N or above 80S.
* @param[in] east_in easting coordinate (UTM)
* @param[in] north_in northing coordinate (UTM)
* @param[in] coordparam UPS zone, either "N" or "S"
* @param[out] lat_out Decimal Latitude
* @param[out] long_out Decimal Longitude
*/
void CoordsAlgorithms::UPS_to_WGS84(const double& east_in, const double& north_in, const std::string& coordparam, double& lat_out, double& long_out)
{
	static const double a = ellipsoids[E_WGS84].a; //major ellipsoid semi-axis
	static const double b = ellipsoids[E_WGS84].b;	//minor ellipsoid semi-axis
	static const double e2 = (a*a-b*b) / (a*a);	//ellispoid eccentricity, squared (=(a²-b²)/a²)
	static const double k0 = 0.994;		//scale factor for the projection
	static const double FN = 2000000.;		//false northing
	static const double FE = 2000000.;		//false easting

	const double Delta_N = north_in - FN;
	const double Delta_E = east_in - FE;

	//which hemisphere are we in?
	if (coordparam!="N" && coordparam!="S")
		throw InvalidFormatException("Invalid UPS zone: "+coordparam+". It should be either N or S.",AT);
	
	const bool northern_hemisphere = (coordparam=="N")? true : false;
	//computing longitude
	if (Delta_N==0.) {
		if (Delta_E==0.) {
			long_out = 0.;
			lat_out = (northern_hemisphere)? 90. : -90.;
			return;
		} else {
			long_out = (Delta_E>0.)? 90. : -90.;
		}
	} else {
		long_out = (northern_hemisphere)? atan2( Delta_E , -Delta_N) * Cst::to_deg : atan2( Delta_E , Delta_N) * Cst::to_deg;
	}

	//computing latitude
	double R;
	if (Delta_N==0.) {
		R = fabs(Delta_E);
	} else if (Delta_E==0.) {
		R = fabs(Delta_N);
	} else {
		R = (Delta_N>Delta_E)? fabs( Delta_N / cos(long_out*Cst::to_rad)) : fabs( Delta_E / sin(long_out*Cst::to_rad) );
	}
	static const double e = sqrt(e2), e4=e2*e2;
	static const double C0 = 2.*a / sqrt(1.-e2) * pow( (1.-e)/(1.+e) , e/2.);
	const double tan_Zz = R / (k0*C0); //isometric colatitude
	const double chi = Cst::PI/2. - atan( tan_Zz )*2.;

	static const double A = e2/2. + 5.*e4/24. + e4*e2/12. + 13.*e4*e4/360.;
	static const double B = 7.*e4/48. + 29.*e4*e2/240. + 811.*e4*e4/11520.;
	static const double C = 7.*e4*e2/120. + 81.*e4*e4/1120.;
	static const double D = 4279.*e4*e4/161280.;
	lat_out = chi + A*sin(2.*chi) + B*sin(4.*chi) + C*sin(6.*chi) + D*sin(8.*chi);
	lat_out *= Cst::to_deg;

	if (!northern_hemisphere) lat_out *=-1.;
}

void CoordsAlgorithms::parseUTMZone(const std::string& zone_info, char& zoneLetter, short int& zoneNumber)
{ //helper method: parse a UTM zone specification string into letter and number
	if ((sscanf(zone_info.c_str(), "%hd%c", &zoneNumber, &zoneLetter) < 2) &&
		(sscanf(zone_info.c_str(), "%hd %c)", &zoneNumber, &zoneLetter) < 2) &&
		(sscanf(zone_info.c_str(), "%c%hd", &zoneLetter, &zoneNumber) < 2) &&
		(sscanf(zone_info.c_str(), "%c %hd)", &zoneLetter, &zoneNumber) < 2)) {
			throw InvalidFormatException("Can not parse given UTM zone: "+zone_info,AT);
	}
	if (zoneNumber<1 || zoneNumber>60) {
		throw InvalidFormatException("Invalid UTM zone: "+zone_info+" (zone should be between 1 and 60, zone letter in [C-M, N-X])",AT);
	}
	zoneLetter = (char)toupper(zoneLetter); //just in case... (sorry for the pun!)
	if (zoneLetter=='Y' || zoneLetter=='Z' || zoneLetter=='A' || zoneLetter=='B') {
			//Special zones for the poles: we should NOT use UTM in these regions!
			throw InvalidFormatException("Invalid UTM zone: "+zone_info+" (trying to use UTM in polar regions)",AT);
	}
}

/**
* @brief Coordinate conversion: from WGS84 Lat/Long to proj4 parameters
* @param[in] lat_in Decimal Latitude
* @param[in] long_in Decimal Longitude
* @param[in] coordparam Extra parameters necessary for the conversion (such as UTM zone, etc)
* @param[out] east_out easting coordinate (target system)
* @param[out] north_out northing coordinate (target system)
* 
* \note Using libproj is currently not thread safe (see https://trac.osgeo.org/proj/wiki/ThreadSafety)
*/
void CoordsAlgorithms::WGS84_to_PROJ4(const double& lat_in, const double& long_in, const std::string& coordparam, double& east_out, double& north_out)
{
#ifdef PROJ4
	static const std::string src_param("+proj=latlong +datum=WGS84 +ellps=WGS84");
	const std::string dest_param("+init=epsg:"+coordparam);
	projPJ pj_latlong, pj_dest;
	double x=long_in*Cst::to_rad, y=lat_in*Cst::to_rad;

	if ( !(pj_dest = pj_init_plus(dest_param.c_str())) ) {
		pj_free(pj_dest);
		throw InvalidArgumentException("Failed to initalize Proj4 with given arguments: "+dest_param, AT);
	}
	if ( !(pj_latlong = pj_init_plus(src_param.c_str())) ) {
		pj_free(pj_latlong);
		pj_free(pj_dest);
		throw InvalidArgumentException("Failed to initalize Proj4 with given arguments: "+src_param, AT);
	}

	const int p = pj_transform(pj_latlong, pj_dest, 1, 1, &x, &y, NULL );
	if (p!=0) {
		pj_free(pj_latlong);
		pj_free(pj_dest);
		throw ConversionFailedException("PROJ4 conversion failed: "+p, AT);
	}
	east_out = x;
	north_out = y;
	pj_free(pj_latlong);
	pj_free(pj_dest);
#else
	(void)lat_in;
	(void)long_in;
	(void)coordparam;
	(void)east_out;
	(void)north_out;
	throw IOException("Not compiled with PROJ4 support", AT);
#endif
}

/**
* @brief Coordinate conversion: from proj4 parameters to WGS84 Lat/Long
* @param east_in easting coordinate (Swiss system)
* @param north_in northing coordinate (Swiss system)
* @param[in] coordparam Extra parameters necessary for the conversion (such as UTM zone, etc)
* @param lat_out Decimal Latitude
* @param long_out Decimal Longitude
*/
void CoordsAlgorithms::PROJ4_to_WGS84(const double& east_in, const double& north_in, const std::string& coordparam, double& lat_out, double& long_out)
{
#ifdef PROJ4
	const std::string src_param("+init=epsg:"+coordparam);
	static const std::string dest_param("+proj=latlong +datum=WGS84 +ellps=WGS84");
	projPJ pj_latlong, pj_src;
	double x=east_in, y=north_in;

	if ( !(pj_src = pj_init_plus(src_param.c_str())) ) {
		pj_free(pj_src);
		throw InvalidArgumentException("Failed to initalize Proj4 with given arguments: "+src_param, AT);
	}
	if ( !(pj_latlong = pj_init_plus(dest_param.c_str())) ) {
		pj_free(pj_latlong);
		pj_free(pj_src);
		throw InvalidArgumentException("Failed to initalize Proj4 with given arguments: "+dest_param, AT);
	}

	const int p = pj_transform(pj_src, pj_latlong, 1, 1, &x, &y, NULL );
	if (p!=0) {
		pj_free(pj_latlong);
		pj_free(pj_src);
		throw ConversionFailedException("PROJ4 conversion failed: "+p, AT);
	}
	long_out = x*RAD_TO_DEG;
	lat_out = y*RAD_TO_DEG;
	pj_free(pj_latlong);
	pj_free(pj_src);
#else
	(void)east_in;
	(void)north_in;
	(void)coordparam;
	(void)lat_out;
	(void)long_out;
	throw IOException("Not compiled with PROJ4 support", AT);
#endif
}

/**
* @brief Spherical law of cosine Distance calculation between points in WGS84 (decimal Lat/Long)
* See http://www.movable-type.co.uk/scripts/latlong.html for more
* @param[in] lat_ref Decimal Latitude (const double&)
* @param[in] lon_ref Decimal Longitude (const double&)
* @param[in] distance Distance in meters (const double&)
* @param[in] bearing bearing in degrees, 0 being north (const double&)
* @param[out] lat Decimal latitude of target point (double&)
* @param[out] lon Decimal longitude of target point (double&)
*/
void CoordsAlgorithms::cosineInverse(const double& lat_ref, const double& lon_ref, const double& distance, const double& bearing, double& lat, double& lon)
{
	const double lat_ref_rad = lat_ref*Cst::to_rad;
	const double bearing_rad = bearing*Cst::to_rad;

	if (IOUtils::checkEpsilonEquality(distance, 0., .01)) {
		//distance is too small, it could create numerical problems
		lat = lat_ref;
		lon = lon_ref;
		return;
	}

	lat = asin( sin(lat_ref_rad)*cos(distance/Cst::earth_R0) +
	            cos(lat_ref_rad)*sin(distance/Cst::earth_R0)*cos(bearing_rad) );
	lon = lon_ref*Cst::to_rad + atan2( sin(bearing_rad)*sin(distance/Cst::earth_R0)*cos(lat_ref_rad) ,
	                              cos(distance/Cst::earth_R0) - sin(lat_ref_rad)*sin(lat) );
	lon = fmod(lon+Cst::PI, 2.*Cst::PI) - Cst::PI;

	lat *= Cst::to_deg;
	lon *= Cst::to_deg;
}

/**
* @brief Spherical law of cosine Distance calculation between points in WGS84 (decimal Lat/Long)
* See http://www.movable-type.co.uk/scripts/latlong.html for more
* @param[in] lat1 Decimal Latitude (const double&)
* @param[in] lon1 Decimal Longitude (const double&)
* @param[in] lat2 Decimal Latitude (const double&)
* @param[in] lon2 Decimal Longitude (const double&)
* @param[out] alpha average bearing
* @return distance (double)
*/
double CoordsAlgorithms::cosineDistance(const double& lat1, const double& lon1, const double& lat2, const double& lon2, double& alpha)
{
	if (lat1==lat2 && lon1==lon2) {
		//distance is zero, it creates numerical problems -> skip calculation
		alpha = 0.;
		return 0.;
	}

	const double d = acos(
		sin(lat1*Cst::to_rad) * sin(lat2*Cst::to_rad)
		+ cos(lat1*Cst::to_rad) * cos(lat2*Cst::to_rad) * cos((lon2-lon1)*Cst::to_rad)
		) * Cst::earth_R0;

	alpha = atan2( sin((lon2-lon1)*Cst::to_rad)*cos(lat2*Cst::to_rad) ,
			cos(lat1*Cst::to_rad)*sin(lat2*Cst::to_rad) - sin(lat1*Cst::to_rad)*cos(lat2*Cst::to_rad)*cos((lon2-lon1)*Cst::to_rad)
 		) / Cst::to_rad;
	alpha = fmod((alpha+360.), 360.);

	return d;
}

/**
* @brief Vincenty Distance calculation between points in WGS84 (decimal Lat/Long)
* See T. Vincenty, "Closed formulas for the direct and reverse geodetic problems",
* Journal of Geodesy, 51, 3, 1977, DOI:10.1007/BF02521599,
* see http://www.springerlink.com/content/y7108u6862473583 for more
* @param[in] lat1 Decimal Latitude (const double&)
* @param[in] lon1 Decimal Longitude (const double&)
* @param[in] lat2 Decimal Latitude (const double&)
* @param[in] lon2 Decimal Longitude (const double&)
* @param[in] alpha average bearing (double&)
* @return distance (double)
*/
double CoordsAlgorithms::VincentyDistance(const double& lat1, const double& lon1, const double& lat2, const double& lon2, double& alpha)
{
	static const double thresh = 1.e-12;	//convergence absolute threshold
	static const int n_max = 100;		//maximum number of iterations
	static const double a = ellipsoids[E_WGS84].a; //major ellipsoid semi-axis
	static const double b = ellipsoids[E_WGS84].b;	//minor ellipsoid semi-axis
	static const double f = (a - b) / a;	//ellispoid flattening

	const double L = (lon1 - lon2)*Cst::to_rad;
	const double U1 = atan( (1.-f)*tan(lat1*Cst::to_rad) );
	const double U2 = atan( (1.-f)*tan(lat2*Cst::to_rad) );

	double lambda = L, lambda_p=0.;
	double sin_sigma, cos_sigma, sigma, cos_alpha2, cos_2sigma_m;
	int n=0;
	do {
		sin_sigma = sqrt( Optim::pow2(cos(U2)*sin(lambda)) + Optim::pow2(cos(U1)*sin(U2) - sin(U1)*cos(U2)*cos(lambda)) );
		if (sin_sigma==0.) return 0.; //co-incident points
		cos_sigma = sin(U1)*sin(U2) + cos(U1)*cos(U2)*cos(lambda);
		sigma = atan2(sin_sigma,cos_sigma);
		const double sin_alpha = cos(U1)*cos(U2)*sin(lambda) / sin_sigma;
		cos_alpha2 = 1. - Optim::pow2(sin_alpha);
		if (lat1==0. && lat2==0.) {
			cos_2sigma_m = 0.;
		} else {
			cos_2sigma_m = cos_sigma - 2.*sin(U1)*sin(U2)/cos_alpha2;
		}
		const double C = f/16. * cos_alpha2*(4.+f*(4.-3.*cos_alpha2));
		lambda_p = lambda;
		lambda = L + (1.-C)*f*sin_alpha*(
			sigma + C*sin_sigma*( cos_2sigma_m + C * cos_sigma * (-1.+2.*Optim::pow2(cos_2sigma_m)) )
			);
		n++;
	} while ( (n<n_max) && (fabs(lambda - lambda_p) > thresh) );

	if (n>n_max) {
		throw IOException("Distance calculation not converging", AT);
	}

	const double u2 = cos_alpha2 * (a*a - b*b) / (b*b);
	const double A = 1. + u2/16384. * ( 4096.+u2*(-768.+u2*(320.-175.*u2)) );
	const double B = u2/1024. * ( 256.+u2*(-128.+u2*(74.-47.*u2)) );
	const double delta_sigma = B*sin_sigma*( cos_2sigma_m+B/4.*( cos_sigma*(-1.+2.*Optim::pow2(cos_2sigma_m)) - B/6.*(cos_2sigma_m*(-3.+4.*Optim::pow2(sin_sigma))*(-3.+4.*Optim::pow2(cos_2sigma_m))) ) );

	const double s = b*A*(sigma - delta_sigma);	//distance between the two points

	//computation of the average forward bearing
	double alpha1 = atan2(cos(U2)*sin(lambda), cos(U1)*sin(U2)-sin(U1)*cos(U2)*cos(lambda)) / Cst::to_rad; //forward azimuth
	//double alpha2 = atan2(cos(U1)*sin(lambda), sin(U1)*cos(U2)-cos(U1)*sin(U2)*cos(lambda)) / Cst::to_rad; //reverse azimuth

	//trying to get a normal compass bearing... TODO: make sure it works and understand why
	alpha1 = fmod(-alpha1+360., 360.);
	//alpha2 = fmod(alpha2+180., 360.);

	//we only keep the forward bearing, otherwise the reverse projection will not produce the initial point
	alpha = alpha1;
	return s;
}

/**
* @brief Vincenty Inverse calculation giving WGS84 (decimal Lat/Long) position
* given a start location (lat,lon) a distance and a bearing
* See T. Vincenty, "Closed formulas for the direct and reverse geodetic problems",
* Journal of Geodesy, 51, 3, 1977, DOI:10.1007/BF02521599,
* see http://www.springerlink.com/content/y7108u6862473583 for more
* @param[in] lat_ref Decimal Latitude (const double&)
* @param[in] lon_ref Decimal Longitude (const double&)
* @param[in] distance Distance in meters (const double&)
* @param[in] bearing bearing in degrees, 0 being north (const double&)
* @param[out] lat Decimal latitude of target point (double&)
* @param[out] lon Decimal longitude of target point (double&)
*/
void CoordsAlgorithms::VincentyInverse(const double& lat_ref, const double& lon_ref, const double& distance, const double& bearing, double& lat, double& lon)
{//well, actually this is the DIRECT Vincenty formula
	static const double thresh = 1.e-12;	//convergence absolute threshold
	static const double a = ellipsoids[E_WGS84].a;	//major ellipsoid semi-axis, value for wgs84
	static const double b = ellipsoids[E_WGS84].b;	//minor ellipsoid semi-axis, value for wgs84
	static const double f = (a - b) / a;	//ellispoid flattening

	const double alpha1 = bearing*Cst::to_rad;
	const double tanU1 = (1.-f)*tan(lat_ref*Cst::to_rad);
	const double cosU1 = 1./sqrt(1.+tanU1*tanU1);
	const double sinU1 = tanU1*cosU1;
	const double sigma1 = atan2(tanU1,cos(alpha1));
	const double sinAlpha = cosU1*sin(alpha1);
	const double cos2alpha = 1. - sinAlpha*sinAlpha;
	const double u2 = cos2alpha * (a*a - b*b) / (b*b);
	const double A = 1. + u2/16384. * (4096. + u2*(-768.+u2*(320.-175.*u2)) );
	const double B = u2/1024. * (256. + u2*(-128.+u2*(74.-47.*u2)));

	double sigma = distance / (b*A);
	static double sigma_p = 2.*Cst::PI;
	double cos2sigma_m = cos( 2.*sigma1 + sigma ); //required to avoid uninitialized value

	while (fabs(sigma - sigma_p) > thresh) {
		cos2sigma_m = cos( 2.*sigma1 + sigma );
		double delta_sigma = B*sin(sigma) * ( cos2sigma_m + B/4. * (
			cos(sigma)*(-1.+2.*cos2sigma_m*cos2sigma_m)
			-B/6. * cos2sigma_m * (-3.+4.*Optim::pow2(sin(sigma))) * (-3.+4.*cos2sigma_m*cos2sigma_m)
			) );
		sigma_p = sigma;
		sigma = distance / (b*A) + delta_sigma;
	}

	lat = atan2( sinU1*cos(sigma) + cosU1*sin(sigma)*cos(alpha1),
		     (1.-f) * sqrt( sinAlpha*sinAlpha + Optim::pow2(sinU1*sin(sigma) - cosU1*cos(sigma)*cos(alpha1)) )
		   );
	const double lambda = atan2( sin(sigma)*sin(alpha1), cosU1*cos(sigma) - sinU1*sin(sigma)*cos(alpha1) );
	const double C = f/16. * cos2alpha * (4.+f*(4.-3.*cos2alpha));
	const double L = lambda - (1.-C) * f * sinAlpha * (
				sigma + C * sin(sigma) * ( cos2sigma_m+C*cos(sigma) * (-1.+2.*cos2sigma_m*cos2sigma_m) )
				);

	lat = lat * Cst::to_deg;
	lon = lon_ref + (L*Cst::to_deg);
	//const double alpha2 = atan2( sinAlpha, -(sinU1*sin(sigma)-cosU1*cos(sigma)*cos(alpha1)) ); //reverse azimuth
}

} //end namespace
