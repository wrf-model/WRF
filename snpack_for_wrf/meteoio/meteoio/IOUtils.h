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
#ifndef IOUTILS_H
#define IOUTILS_H

#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <set>
#include <cstdlib>
#include <limits>
#include <cmath>

#include <meteoio/dataClasses/Coords.h>
#include <meteoio/dataClasses/Date.h>
#include <meteoio/IOExceptions.h>
#include <meteoio/meteoLaws/Meteoconst.h>

namespace mio {

#ifdef _MSC_VER
double round(const double& x);
#endif

class MeteoData;
class Coords;
class Config;

/**
* @brief Return the library version
* @param[in] short_version only return the version number (ie skip the compilation date)
* @return library version string
*/
std::string getLibVersion(const bool& short_version=false);

namespace IOUtils {
	enum ProcessingLevel {
		raw           = 1,
		filtered      = 1 << 1,
		resampled     = 1 << 2,
		generated     = 1 << 3,
		num_of_levels = 1 << 4
	};

	///Keywords for mode of operation. Please keep all the GRID_xxx last!
	enum OperationMode {
		STD, ///< default: extract timeseries from timeseries or grids from grids or spatially interpolate timeseries
		VSTATIONS, ///< extract virtual stations as specified in the ini file
		GRID_EXTRACT, ///< extract data from grids at locations provided in the ini file
		GRID_SMART, ///< extract all relevant grid points from a provided grid
		GRID_ALL, ///< extract all grid points from a provided grid
		GRID_RESAMPLE ///< generate a grid at a different resolution
	};

	enum ThrowOptions { dothrow, nothrow };
	const double nodata = -999.0; ///<This is the internal nodata value
	const unsigned int unodata = static_cast<unsigned int>(-1);
	const int inodata = -999;
	const short int snodata = -999;
	const char cnodata = std::numeric_limits<char>::max();
	const size_t npos = static_cast<size_t>(-1);  ///<npos is the out-of-range value

	const double grid_epsilon = 5.; ///<What is an acceptable small distance on a grid, in meters
	const double lon_epsilon = grid_epsilon / Cst::earth_R0 *  Cst::to_deg; ///<in degrees. Small angle for longitudes, so sin(x)=x
	const double lat_epsilon = lon_epsilon; ///<in degrees. Small angle for latitudes.

	inline double C_TO_K(const double& T) {return ((T==nodata)? T : T + Cst::t_water_freezing_pt);}
	inline double K_TO_C(const double& T) {return ((T==nodata)? T : T - Cst::t_water_freezing_pt);}
	
	/**
	* @brief Check whether two values are equal regarding a certain epsilon environment (within certain radius of each other)
	* @param val1
	* @param val2
	* @param epsilon is a radius around val1
	* @return true if val2 is within the radius around val1, false otherwise.
	*/
	inline bool checkEpsilonEquality(const double& val1, const double& val2, const double& epsilon) {return (fabs(val1-val2) < epsilon);}

	/**
	* @brief Search for an element at a given date in a vector of MeteoData.
	* The position of the matching date is returned or IOUtils::npos if not found. If \em exactmatch=false,
	* the position of the first element \em after \em soughtdate is returned (or IOUtils::npos if this is
	* not possible / relevant).
	* @param[in] soughtdate date that should be found
	* @param[in] vecM vector that should contain the data
	* @param[in] exactmatch if the exact requested date is not found, return npos
	*/
	size_t seek(const Date& soughtdate, const std::vector<MeteoData>& vecM, const bool& exactmatch=true);

	/**
	* @brief Converts a compass bearing to a trigonometric angle
	* @param bearing compass bearing (0° on top, clockwise, in [0°, 360°[)
	* @return trigonometric angle (0° on the right, counterclockwise, in [0, 2PI[)
	*/
	double bearing_to_angle(const double& bearing);
	/**
	* @brief Converts a trigonometric angle to a compass bearing
	* @param angle trigonometric angle (0° on the right, counterclockwise, in [0, 2PI[)
	* @return bearing (0° on top, clockwise, in [0°, 360°[)
	*/
	double angle_to_bearing(const double& angle);
	/**
	* @brief Converts a string bearing to a compass bearing
	* @param bearing_str as N, NE, SSW, etc
	* @return bearing (0° on top, clockwise, in [0°, 360°[)
	*/
	double bearing(std::string bearing_str);
	/**
	* @brief Converts a compass bearing to a string bearing
	* @param bearing (0° on top, clockwise, in [0°, 360°[)
	* @return bearing_str as N, NE, SSW, etc
	*/
	std::string bearing(double bearing);

	/**
	* @brief Retrieve the user name
	* This checks various environment variables (USERNAME, USER, LOGNAME).
	* @return user name
	*/
	std::string getLogName();
	
	/**
	* @brief Retrieve the name of the computer running the binary
	* @return host name
	*/
	std::string getHostName();
	
	/**
	* @brief Retrieve the domain name of the computer running the binary
	* @return domain with TLD
	*/
	std::string getDomainName();

	/**
	* @brief Removes trailing and leading whitespaces, tabs and newlines from a string.
	* @param s The reference of the string to trim (in/out parameter)
	*/
	void trim(std::string &s);

	/**
	* @brief Removes trailing and leading whitespaces, tabs and newlines from a string.
	* @param s The string to trim
	* @return The trimmed string
	*/
	std::string trim(const std::string &s);

	void stripComments(std::string& str);
	
	/**
	 * @brief Replace a substring within a given string by another one.
	 * @details This should be quite similar to Boost::replace_all.
	 * @param input string to manipulate
	 * @param[in] search substring to be searched for 
	 * @param[in] format substitute string 
	 */
	void replace_all(std::string &input, const std::string& search, const std::string& format);

	/**
	* @brief Removes consecutive occurrences of spaces and tabs
	* @param line The string to read and modify
	*/
	void removeDuplicateWhitespaces(std::string& line);

	/**
	* @brief Replaces spaces and tabs with a single character or removes them
	* @param line The string to read and modify
	* @param rep The character to replace with (default: empty)
	*/
	void replaceWhitespaces(std::string& line, const char& rep = '\0');

	/**
	* @brief Replaces invalid characters with a single character or removes them
	* @param line The string to read and modify
	* @param rep The character to replace with (default: empty)
	*/
	void replaceInvalidChars(std::string& line, const char& rep = '\0');

	/**
	* @brief Removes single and double quotation marks
	* @param line The string to read and modify
	*/
	void removeQuotes(std::string& line);

	/**
	 * @brief Cleans up a string to be usable as, for example, a parameter name
	 * @details This replaces all whitespaces (including consecutive ones) with a single
	 * character or removes them, and removes invalid characters and quotes.
	 * @param field The string to read and modify
	 * @param[in] clean_whitespaces Should tabs and spaces be replaced? (default: true)
	 * @param[in] rep Substitute string (default: '-'). Set to empty ('\0') to remove.
	 */
	void cleanFieldName(std::string& field, const bool& clean_whitespaces = true, const char& rep = '-');

	/**
	 * @brief count how many times a substring appears in a string
	 * @details This should be quite similar to Boost::replace_all.
	 * @param[in] input string to manipulate
	 * @param[in] search substring to be searched for 
	 * @return number of non-overlapping matches or std::string::npos if nothing could be found (empty "search", etc)
	 */
	size_t count(const std::string &input, const std::string& search);
	
	/**
	 * @brief Fowler/Noll/Vo hash function (FNV-1a)
	 * @details This returns a non-cryptographic, 32 bits hash for the string given as argument 
	 * (see https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
	 * or https://tools.ietf.org/html/draft-eastlake-fnv-16)
	 * @param[in] text string to get a hash for
	 * @return 32 bits hash 
	 */
	size_t FNV_hash(const std::string& text);
	
	/**
	* @brief read a string line, parse it and save it into a map object, that is passed by reference
	* @param in_line (const string&) string to parse
	* @param delimiter (const string&) delimiter to use for the parsing
	* @param setToUpperCase If set to true the key will be put into upper case (for case insensitivity)
	* @param key retrieved key
	* @param value retrieved value
	* @return (bool) true when line is empty
	*/
	bool readKeyValuePair(const std::string& in_line, const std::string& delimiter,
	                      std::string &key, std::string &value, const bool& setToUpperCase=false);

	void toUpper(std::string& str);
	std::string strToUpper(std::string str);
	void toLower(std::string& str);
	std::string strToLower(std::string str);
	bool isNumeric(std::string input, const unsigned int& nBase=10);
	size_t readLineToVec(const std::string& line_in, std::vector<double>& vec_data);
	size_t readLineToSet(const std::string& line_in, std::set<std::string>& setString);
	size_t readLineToVec(const std::string& line_in, std::vector<std::string>& vecString);
	size_t readLineToVec(const std::string& line_in, std::vector<std::string>& vecString, const char& delim);
	size_t readLineToVec(const std::string& line_in, std::vector<double>& vecRet, const char& delim);
	
	template <class T> std::string toString(const T& t) {
		std::ostringstream os;
		os << t;
		return os.str();
	}

	/**
	* @brief Convert a string to the requested type (template function).
	* @tparam T   [in] The type wanted for the return value (template type parameter).
	* @param t   [out] The value converted to the requested type.
	* @param str   [in] The input string to convert; trailling whitespaces are ignored,
	*              comment after non-string values are allowed, but multiple values are not allowed.
	* @param f   [in] The radix for reading numbers, such as std::dec or std::oct; default is std::dec.
	* @return true if everything went fine, false otherwise
	*/
	template <class T> bool convertString(T& t, std::string str, std::ios_base& (*f)(std::ios_base&) = std::dec) {
		trim(str); //delete trailing and leading whitespaces and tabs
		if (str.empty()) {
			t = static_cast<T> (nodata);
			return true;
		} else {
			std::istringstream iss(str);
			iss.setf(std::ios::fixed);
			iss.precision(std::numeric_limits<double>::digits10); //try to read values with maximum precision
			iss >> f >> t; //Convert first part of stream with the formatter (e.g. std::dec, std::oct)
			if (iss.fail()) {
				//Conversion failed
				return false;
			}
			std::string tmp;
			getline(iss,  tmp); //get rest of line, if any
			trim(tmp);
			if (!tmp.empty() && tmp[0] != '#' && tmp[0] != ';') {
				//if line holds more than one value it's invalid
				return false;
			}
			return true;
		}
	}
	// fully specialized template functions (implementation must not be in header)
	template<> bool convertString<double>(double& t, std::string str, std::ios_base& (*f)(std::ios_base&));
	template<> bool convertString<std::string>(std::string& t, std::string str, std::ios_base& (*f)(std::ios_base&));
	template<> bool convertString<bool>(bool& t, std::string str, std::ios_base& (*f)(std::ios_base&));
	template<> bool convertString<char>(char& t, std::string str, std::ios_base& (*f)(std::ios_base&));
	template<> bool convertString<unsigned int>(unsigned int& t, std::string str, std::ios_base& (*f)(std::ios_base&));
	template<> bool convertString<Coords>(Coords& t, std::string str, std::ios_base& (*f)(std::ios_base&));

	bool convertString(Date& t, std::string str, const double& time_zone, std::ios_base& (*f)(std::ios_base&) = std::dec);

	/**
	* @brief Returns, with the requested type, the value associated to a key (template function).
	* @tparam T   [in] The type wanted for the return value (template type parameter).
	* @param[in]  properties  A map containing all the parameters.
	* @param[in]  key         The key of the parameter to retrieve.
	* @param[out] t           The value associated to the key, converted to the requested type
	* @param[in]  options     Extra options, by default IOUtils::dothrow
	*/
	template <class T> void getValueForKey(const std::map<std::string,std::string>& properties,
	                                       const std::string& key, T& t, const ThrowOptions& options=IOUtils::dothrow) {
		if (key.empty() && options!=IOUtils::nothrow)
			throw InvalidArgumentException("Empty key", AT);

		const std::map<std::string, std::string>::const_iterator it( properties.find(key) );
		if (it == properties.end()){
			if (options == IOUtils::nothrow)
				return;
			else
				throw UnknownValueException("No value for key " + key, AT);
		}
		const std::string& value = it->second;

		if (!convertString<T>(t, value, std::dec) && options!=IOUtils::nothrow) {
			std::cerr << "[E] When reading \"" << key << "\" = \"" << t << "\"\n";
			throw ConversionFailedException(value, AT);
		}
	}

	/**
	* @brief Returns, with the requested type, the value associated to a key (template function).
	* @tparam T           [in] The type wanted for the return value (template type parameter).
	* @param[in]  properties  A map containing all the parameters.
	* @param[in]  key         The key of the parameter to retrieve.
	* @param[out] vecT        The vector of values associated to the key, each value is converted to the requested type
	* @param[in]  options     Extra options, by default IOUtils::dothrow
	*/
	template <class T> void getValueForKey(const std::map<std::string,std::string>& properties,
	                                       const std::string& key, std::vector<T>& vecT, const ThrowOptions& options=IOUtils::dothrow)
	{
		if (key.empty() && options!=IOUtils::nothrow)
			throw InvalidArgumentException("Empty key", AT);

		const std::map<std::string, std::string>::const_iterator it( properties.find(key) );
		if (it == properties.end()) {
			if (options == IOUtils::nothrow) {
				return;
			} else {
				throw UnknownValueException("No value for key " + key, AT);
			}
		}
		const std::string& value = it->second;

		//split value string
		std::vector<std::string> vecUnconvertedValues;
		const size_t counter = readLineToVec(value, vecUnconvertedValues);
		vecT.resize( counter );
		for (size_t ii=0; ii<counter; ii++){
			T myvar;
			if (!convertString<T>(myvar, vecUnconvertedValues.at(ii), std::dec) && options!=IOUtils::nothrow){
				std::cerr << "[E] When reading \"" << key << "\" = \"" << myvar << "\"\n";
				throw ConversionFailedException(vecUnconvertedValues.at(ii), AT);
			}
			vecT[ii] = myvar;
		}
	}

	/**
	* @brief Standardize a given value to use MeteoIO's internal nodata value (if applicable)
	* @tparam T[in] The type wanted for the return value (template type parameter).
	* @param[in] value  the value to check/convert
	* @param[in] plugin_nodata plugin-specific nodata value
	* @return checked/converted value
	*/
	template <class T> T standardizeNodata(const T& value, const double& plugin_nodata) {
		if (value==plugin_nodata) return static_cast<T> (nodata);
		else return value;
	}

	/**
	* @brief Parse a given named argument
	* @tparam T[in] The type wanted for the return value (template type parameter).
	* @param[in] arg  key/value pair to be parsed
	* @param[in] algo  the name of the filter or algorithm (for error messages)
	* @param[out] val the parsed value
	*/
	template <class T> static void parseArg(const std::pair< std::string, std::string>& arg, const std::string& algo, T& val) {
			if (!IOUtils::convertString(val, arg.second))
				throw InvalidArgumentException("Can not parse argument '"+arg.first+"::"+arg.second+"' for " + algo, AT);
		}

	/**
	* @brief A function that parses a Config object for COORSYS, COORDPARAM keywords in [Input] and [Output]
	*        section and sets the respective strings to the values of those keywords
	* @param[in] cfg  A Config object
	* @param[out] coordin The coordinate system to be used for input data
	* @param[out] coordinparam The coordinate system parameters to be used for output data
	* @param[out] coordout The coordinate system to be used for output data
	* @param[out] coordoutparam The coordinate system parameters to be used for output data
	*/
	void getProjectionParameters(const Config& cfg, std::string& coordin, std::string& coordinparam,
	                             std::string& coordout, std::string& coordoutparam);
	
	/**
	* @brief A function that parses a Config object for COORSYS, COORDPARAM keywords in the [Input]
	*        section and sets the respective strings to the values of those keywords
	* @param[in] cfg  A Config object
	* @param[out] coordin The coordinate system to be used for input data
	* @param[out] coordinparam The coordinate system parameters to be used for output data
	*/
	void getProjectionParameters(const Config& cfg, std::string& coordin, std::string& coordinparam);

	/**
	* @brief A function that parses a Config object for the time_zone keyword and returns the timezone
	* @param[in] cfg  A Config object
	* @param[out] tz_in value to be used for the input timezone
	* @param[out] tz_out value to be used for the output timezone
	*/
	void getTimeZoneParameters(const Config& cfg, double& tz_in, double& tz_out);

	/**
	* @brief Convert a textual representation of a unit prefix (like 'm' or 'G') to multiplying factor
	* @param prefix unit prefix
	* return multiplying factor
	*/
	double unitsPrefix(const char& prefix);

	/**
	* @brief Performs simple unit conversion (supports temperature, prefixes and exponents)
	* NOTE "composite" units such as 'K/m' or 'N/m2' are currently NOT supported.
	* @param val value to convert
	* @param unitIn units of the input
	* @param unitOut units to convert to
	* return value, expressed in unitOut
	*/
	double unitsConversion(const double& val, std::string unitIn, std::string unitOut);
} //end namespace IOUtils

} //end namespace mio

#endif
