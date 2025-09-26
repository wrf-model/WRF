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
#include <cstdio>
#include <cmath>
#include <cstring>
#include <ctype.h>
#if (defined _WIN32 || defined __MINGW32__) && ! defined __CYGWIN__
	#ifndef NOMINMAX
		#define NOMINMAX
	#endif
	#include <winsock.h>
#else
	#include <unistd.h>
#endif
#include <algorithm>

#include <meteoio/IOUtils.h>
#include <meteoio/MathOptim.h>
#include <meteoio/Config.h>    // to avoid forward declaration hell
#include <meteoio/dataClasses/MeteoData.h> // to avoid forward declaration hell
#include <meteoio/dataClasses/CoordsAlgorithms.h>

namespace mio {

#ifdef _MSC_VER
//This is C99, Microsoft should move on and suppport it, it is almost 15 years old!!
double round(const double& x) {
	//middle value point test
	if (ceil(x+0.5) == floor(x+0.5)) {
		const int a = (int)ceil(x);
		if (a%2 == 0) {
			return ceil(x);
		} else {
			return floor(x);
		}
	} else {
		return floor(x+0.5);
	}
}
#endif

std::string getLibVersion(const bool& short_version) {
	std::ostringstream ss;
	ss << MIO_VERSION;
	if (!short_version) ss << " compiled on " << __DATE__ << " " << __TIME__;
	return ss.str();
}

namespace IOUtils {

double bearing_to_angle(const double& bearing) {
	return (fmod(360.-bearing+90., 360.)*Cst::to_rad);
}

double angle_to_bearing(const double& angle) {
	return (fmod( 90.-angle*Cst::to_deg+360. , 360. ));
}

double bearing(std::string bearing_str)
{
	trim(bearing_str);
	toUpper(bearing_str);

	if (bearing_str=="N") return 0.;
	if (bearing_str=="NNE") return 22.5;
	if (bearing_str=="NE") return 45.;
	if (bearing_str=="ENE") return 67.5;
	if (bearing_str=="E") return 90.;
	if (bearing_str=="ESE") return 112.5;
	if (bearing_str=="SE") return 135.;
	if (bearing_str=="SSE") return 157.5;
	if (bearing_str=="S") return 180.;
	if (bearing_str=="SSW") return 202.5;
	if (bearing_str=="SW") return 225.;
	if (bearing_str=="WSW") return 247.5;
	if (bearing_str=="W") return 270.;
	if (bearing_str=="WNW") return 292.5;
	if (bearing_str=="NW") return 315.;
	if (bearing_str=="NNW") return 337.5;

	//no match
	return nodata;
}

std::string bearing(double bearing)
{
	if (bearing==nodata) return "n/a";

	bearing = fmod( fmod(bearing, 360.)+360., 360.); //from -infty to +infty back to [0, 360]

	if (bearing<=11.25 || bearing>348.75) return "N";
	if (bearing<=33.75) return "NNE";
	if (bearing<=56.25) return "NE";
	if (bearing<=78.75) return "ENE";
	if (bearing<=101.25) return "E";
	if (bearing<=123.75) return "ESE";
	if (bearing<=146.25) return "SE";
	if (bearing<=168.75) return "SSE";
	if (bearing<=191.25) return "S";
	if (bearing<=213.75) return "SSW";
	if (bearing<=236.25) return "SW";
	if (bearing<=258.75) return "WSW";
	if (bearing<=281.25) return "W";
	if (bearing<=303.75) return "WNW";
	if (bearing<=326.25) return "NW";
	if (bearing<=348.75) return "NNW";

	//should not be reached
	return "";
}

void stripComments(std::string& str)
{
	const size_t found = str.find_first_of("#;");
	if (found != std::string::npos){
		str.erase(found); //rest of line disregarded
	}
}

void trim(std::string& str)
{
	const std::string whitespaces(" \t\f\v\n\r");
	const size_t startpos = str.find_first_not_of(whitespaces); // Find the first character position after excluding leading blank spaces
	const size_t endpos = str.find_last_not_of(whitespaces); // Find the first character position from reverse af

	// if all spaces or empty return an empty string
	if (startpos!=std::string::npos && endpos!=std::string::npos) {
		str.erase(endpos+1); //right trim
		str.erase(0, startpos); //left trim
	} else {
		str.clear();
	}
}

std::string trim(const std::string &str_in)
{
	std::string str(str_in);
	trim(str);
	return str;
}

void replace_all(std::string &input, const std::string& search, const std::string& format)
{
	const size_t len = search.length();
	size_t pos = 0;
	while (true) {
		pos = input.find(search, pos);
		if (pos == std::string::npos) break; //done!
		input.replace(pos, len, format);
		pos += len;
	}
}

//consider spaces and tabs to be whitespaces
inline bool isWhitespace(const char& c) { if (c == 9 || c == 32) return true; return false; }
inline bool isTwoSpaces(const char& a, const char& b) { return (a == b) && (a == ' '); }
inline bool isTwoTabs(const char& a, const char& b) { return (a == b) && (a == '\t'); }
inline bool isInvalidChar(const char& c) { return (c < 32 || c > 126); }
inline bool isQuote(const char& c) { if (c=='"' || c=='\'') return true; return false; }

void removeDuplicateWhitespaces(std::string& line)
{ //remove consecutive occurrences of either spaces or tabs
	std::string::iterator close_str = std::unique(line.begin(), line.end(), &isTwoSpaces);
	line.erase(close_str, line.end());
	close_str = std::unique(line.begin(), line.end(), &isTwoTabs);
	line.erase(close_str, line.end());
}

void replaceWhitespaces(std::string& line, const char& rep /* = '\0' */)
{ //replace/remove tabs and spaces
	std::replace_if(line.begin(), line.end(), &isWhitespace, rep);
}

void replaceInvalidChars(std::string& line, const char& rep /* = '\0' */)
{ //replace/remove accentuated characters etc.
	std::replace_if(line.begin(), line.end(), &isInvalidChar, rep);
}

void removeQuotes(std::string& line)
{ //remove single and double quotation marks
	const std::string::iterator close_str = std::remove_if(line.begin(), line.end(), &isQuote);
	line.erase(close_str, line.end());
}

void cleanFieldName(std::string& field, const bool& clean_whitespaces, const char& rep)
{ //combination of the methods above for something like parameter names
	if (clean_whitespaces) { //lines are expected to already be trimmed when arriving here
		removeDuplicateWhitespaces(field);
		replaceWhitespaces(field, rep);
	}
	replaceInvalidChars(field, rep);
	removeQuotes(field);
}

size_t count(const std::string &input, const std::string& search)
{
	if (search.empty() || input.empty()) return std::string::npos;
	
	const size_t len = search.length();
	size_t pos = input.find(search);
	size_t count = 0;
	while (pos!=std::string::npos) {
		pos = input.find(search, pos+len);
		count++;
	}

	return count;
}

size_t FNV_hash(const std::string& text)
{
	static const size_t FNV_offset_basis = 2166136261U;
	static const size_t FNV_prime = 16777619U;
	size_t hash = FNV_offset_basis;

	for (size_t ii=0; ii<text.size(); ii++){
		hash = hash ^ (text[ii]); //XOR the lower 8 bits
		hash *= FNV_prime;
	}
	return hash;
}

void toUpper(std::string& str) {
	std::transform(str.begin(), str.end(), str.begin(), ::toupper);
}

void toLower(std::string& str) {
	std::transform(str.begin(), str.end(), str.begin(), ::tolower);
}

std::string strToUpper(std::string str) {
	//based on http://cpp-next.com/archive/2009/08/want-speed-pass-by-value/
	//it is better to let the compiler copy (or not!) the parameters
	std::transform(str.begin(), str.end(), str.begin(), ::toupper);
	return str;
}

std::string strToLower(std::string str) {
	std::transform(str.begin(), str.end(), str.begin(), ::tolower);
	return str;
}

bool isNumeric(std::string str, const unsigned int& nBase)
{
	trim(str); //delete trailing and leading whitespaces and tabs
	std::istringstream iss(str);

	if ( nBase == 10 ) {
		double tmp;
		iss >> tmp;
	} else if ( nBase == 8 || nBase == 16 ) {
		int tmp;
		iss >> ( ( nBase == 8 ) ? std::oct : std::hex ) >> tmp;
	} else
		return false;

	if ( !iss ) //the conversion failed
		return false;

	return ( iss.rdbuf()->in_avail() == 0 ); //true if nothing was left after conversion
}

bool readKeyValuePair(const std::string& in_line, const std::string& delimiter, std::string &key, std::string &value, const bool& setToUpperCase)
{
	const size_t pos = ((delimiter==" ") || (delimiter=="\t"))? in_line.find_first_of(" \t", 0) : in_line.find(delimiter); //first occurence of delimiter

	if (pos != std::string::npos) { //ignore in_lines that are empty or without '='
		key = in_line.substr(0, pos);
		value = in_line.substr(pos + 1);

		trim(key);
		trim(value);

		if (key.empty() || value.empty()) return false;
		if (setToUpperCase) toUpper(key);
	} else {
		key="";
		value="";
		return false;
	}

	return true;
}

std::string getLogName()
{
	char *tmp;

	if ((tmp=getenv("USERNAME"))==NULL) { //Windows & Unix
		if ((tmp=getenv("LOGNAME"))==NULL) { //Unix
			tmp=getenv("USER"); //Windows & Unix
		}
	}

	if (tmp==NULL) return std::string("");
	return std::string(tmp);
}

std::string getHostName() {
	static const size_t len = 4096;

	#if (defined _WIN32 || defined __MINGW32__) && ! defined __CYGWIN__
		TCHAR infoBuf[len];
		DWORD bufCharCount = len;
		if ( !GetComputerName( infoBuf, &bufCharCount ) )
			return std::string("");

		return std::string(infoBuf);
    #else
		char name[len];
		if (gethostname(name, len) != 0) {
			return std::string("");
		}

		if (name[0] == '\0') return std::string("");
		return std::string(name);
	#endif
}

std::string getDomainName() {
	const std::string hostname( getHostName() );
	if (hostname.empty()) return "";

	std::size_t tld_pos = hostname.find_last_of(".");
	if (tld_pos==std::string::npos || tld_pos==0) return "";
	std::size_t domain_pos = hostname.find_last_of(".", tld_pos-1);
	if (domain_pos==std::string::npos) return "";

	return hostname.substr(domain_pos+1);
}

size_t readLineToVec(const std::string& line_in, std::vector<double>& vec_data)
{
	vec_data.clear();
	std::istringstream iss(line_in); //construct inputstream with the string line as input
	iss.setf(std::ios::fixed);
	iss.precision(std::numeric_limits<double>::digits10);

	double tmp;
	while (!iss.eof()) {
		iss >> std::skipws >> tmp;

		if (iss.fail()) {
			std::ostringstream ss;
			ss << "Can not read column " << vec_data.size()+1 << " in data line \"" << line_in << "\"";
			throw InvalidFormatException(ss.str(), AT);
		}
		vec_data.push_back(tmp);
	}

	return vec_data.size();
}

size_t readLineToSet(const std::string& line_in, std::set<std::string>& setString)
{
	setString.clear();
	std::istringstream iss(line_in); //construct inputstream with the string line as input

	std::string word;
	while (iss >> word){
		setString.insert(word);
	}

	return setString.size();
}

size_t readLineToVec(const std::string& line_in, std::vector<std::string>& vecString)
{
	vecString.clear();
	std::istringstream iss(line_in); //construct inputstream with the string line as input

	std::string word;
	while (iss >> word){
		vecString.push_back(word);
	}

	return vecString.size();
}

size_t readLineToVec(const std::string& line_in, std::vector<std::string>& vecString, const char& delim)
{
	vecString.clear();
	std::string word;
	std::istringstream iss(line_in);

	while (getline(iss, word, delim)){
		vecString.push_back(word);
	}

	if (!line_in.empty()) {
		const char lastChar = line_in[ line_in.length() - 1 ];
		if (lastChar==delim) vecString.push_back( "" );
	}

	return vecString.size();
}

size_t readLineToVec(const std::string& line_in, std::vector<double>& vecRet, const char& delim)
{ //split a line into strings, and extract a double vector from there
	vecRet.clear();
	std::istringstream iss(line_in);
	std::string word;

	double num;
	while (getline(iss, word, delim)) {
		std::istringstream tok(word);
		tok >> num;
		if (tok.fail())
			throw InvalidFormatException("Can not read column in data line \"" + line_in + "\"", AT);
		vecRet.push_back(num);
	}
	return vecRet.size();
}

// generic template function convertString must be defined in the header

static const char ALPHANUM[] = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
static const char NUM[] = "0123456789";

template<> bool convertString<std::string>(std::string& t, std::string str, std::ios_base& (*f)(std::ios_base&))
{
	(void)f;
	t =str;
	trim(t); //delete trailing and leading whitespaces and tabs
	return true;
}

template<> bool convertString<bool>(bool& t, std::string str, std::ios_base& (*f)(std::ios_base&))
{
	trim(str); //delete trailing and leading whitespaces and tabs

	if (toupper(str[0])=='T' || toupper(str[0])=='Y') {
		t = true;
	} else if (toupper(str[0])=='F' || toupper(str[0])=='N') {
		t = false;
	} else {
		std::istringstream iss(str);
		int i;
		iss >> f >> i; //Convert first part of stream with the formatter (e.g. std::dec, std::oct)
		//Conversion failed
		if (iss.fail()) return false;
		t = (i != 0);
	}

	const std::string::size_type pos = str.find_first_not_of(ALPHANUM);
	if (pos != std::string::npos) {
		std::string tmp( str.substr(pos) );
		trim(tmp);
		if (!tmp.empty() && tmp[0] != '#' && tmp[0] != ';') {//if line holds more than one value it's invalid
			return false;
		}
	}

	return true;
}

template<> bool convertString<double>(double& t, std::string str, std::ios_base& (*f)(std::ios_base&))
{
	if (f == std::dec) {
		//First check if string is empty
		const char* start = str.c_str();
		while (*start && isspace(*start)) start++;
		if (*start == '\0' || *start == '#' || *start == ';') { // line empty or comment
			t = static_cast<double> (nodata);
			return true;
		}

		//string is not empty
		char* end;
		t = strtod(str.c_str(), &end); //would strip leading whitespaces, but already done

		if (*end == '\0') { //conversion successful
			return true;
		} else { // conversion might have worked, let's check what is left
			while ((*end != '\0') && isspace(*end)) end++;

			if (*end == '\0' || *end == '#' || *end == ';') { // we allow the number to be followed by a comment
				return true;
			}

			return false; // Invalid string to convert to double
		}
	}

	trim(str); //delete trailing and leading whitespaces and tabs
	if (str.empty()) {
		t = static_cast<double> (nodata);
		return true;
	}

	std::istringstream iss(str);
	iss.setf(std::ios::fixed);
	iss.precision(std::numeric_limits<double>::digits10); //try to read values with maximum precision
	iss >> f >> t; //Convert first part of stream with the formatter (e.g. std::dec, std::oct)

	//Conversion failed
	if (iss.fail()) return false;
	std::string tmp;
	getline(iss,  tmp); //get rest of line, if any
	trim(tmp);
	if (!tmp.empty() && tmp[0] != '#' && tmp[0] != ';') {
		//if line holds more than one value it's invalid
		return false;
	}
	return true;
}

template<> bool convertString<char>(char& t, std::string str, std::ios_base& (*f)(std::ios_base&))
{
	trim(str); //delete trailing and leading whitespaces and tabs
	if (str.empty()) {
		t = cnodata;
		return true;
	} else {
		std::istringstream iss(str);
		iss.setf(std::ios::fixed);
		iss.precision(std::numeric_limits<double>::digits10); //try to read values with maximum precision
		iss >> f >> t; //Convert first part of stream with the formatter (e.g. std::dec, std::oct)
		//Conversion failed
		if (iss.fail()) return false;
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

template<> bool convertString<unsigned int>(unsigned int& t, std::string str, std::ios_base& (*f)(std::ios_base&))
{
	trim(str); //delete trailing and leading whitespaces and tabs
	if (str.empty()) {
		t = unodata;
		return true;
	} else {
		std::istringstream iss(str);
		iss.setf(std::ios::fixed);
		iss.precision(std::numeric_limits<double>::digits10); //try to read values with maximum precision
		iss >> f >> t; //Convert first part of stream with the formatter (e.g. std::dec, std::oct)
		//Conversion failed
		if (iss.fail()) return false;
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

/**
* @brief Convert a string to a date (template specialization of convertString)
* @details The date formats that are recognized are described in the \ref date_formats "Date class".
* @param[out] t   The value converted to a Date object.
* @param[in] str    The input string to convert; trailling whitespaces are ignored,
*              comment after non-string values are allowed, but multiple values are not allowed.
* @param[in] time_zone The timezone the provided date is into
* @param[in] f  The radix for reading numbers, such as std::dec or std::oct; default is std::dec.
* @return true if everything went fine, false otherwise
*/
bool convertString(Date& t, std::string str, const double& time_zone, std::ios_base& (*f)(std::ios_base&))
{
	trim(str); //delete trailing and leading whitespaces and tabs
	stripComments(str);

	(void)f;
	int year;
	unsigned int month, day, hour, minute;
	double second;
	char rest[32] = "";

	const char *c_str = str.c_str();
	//special case: NOW or NOW±xxx (offset in seconds or hh:mm)
	if (str.substr(0, 3)=="NOW") {
		t.setFromSys();
		t.setTimeZone(time_zone);
		if (str.size()>3) {
			unsigned int secs;
			bool status = true;
			if (sscanf(c_str, "NOW+%u:%u%31s", &hour, &minute, rest) >= 2) {
				t += (hour*60.+minute)/(60.*24.);
			} else if (sscanf(c_str, "NOW-%u:%u%31s", &hour, &minute, rest) >= 2) {
				t -= (hour*60.+minute)/(60.*24.);
			} else if (sscanf(c_str, "NOW+%u%31s", &secs, rest) >= 1) {
				t += (secs)/(3600.*24.);
			} else if (sscanf(c_str, "NOW-%u%31s", &secs, rest) >= 1) {
				t -= (secs)/(3600.*24.);
			} else
				status = false;

			if (status==false || strlen(rest)>0)
				throw InvalidFormatException("Invalid date specification '"+str+"'", AT);
		}
		return true;
	}

	if (sscanf(c_str, "%d-%u-%u %u:%u:%lg%31s", &year, &month, &day, &hour, &minute, &second, rest) >= 6) {
		const std::string timezone_iso(rest);
		const double tz = (timezone_iso.empty())? time_zone : Date::parseTimeZone(timezone_iso);
		if (tz==nodata) return false;
		t.setDate(year, month, day, hour, minute, second, tz);
		return true;

	} else if (sscanf(c_str, "%d-%u-%uT%u:%u:%lg%31s", &year, &month, &day, &hour, &minute, &second, rest) >= 6) { //ISO
		const std::string timezone_iso(rest);
		const double tz = (timezone_iso.empty())? time_zone : Date::parseTimeZone(timezone_iso);
		if (tz==nodata) return false;
		t.setDate(year, month, day, hour, minute, second, tz);
		return true;

	} else if (sscanf(c_str, "%d-%u-%u %u:%u%31s", &year, &month, &day, &hour, &minute, rest) >= 5) {
		const std::string timezone_iso(rest);
		const double tz = (timezone_iso.empty())? time_zone : Date::parseTimeZone(timezone_iso);
		if (tz==nodata) return false;
		t.setDate(year, month, day, hour, minute, static_cast<unsigned>(0), tz);
		return true;

	} else if (sscanf(c_str, "%d-%u-%uT%u:%u%31s", &year, &month, &day, &hour, &minute, rest) >= 5) {
		const std::string timezone_iso(rest);
		const double tz = (timezone_iso.empty())? time_zone : Date::parseTimeZone(timezone_iso);
		if (tz==nodata) return false;
		t.setDate(year, month, day, hour, minute, static_cast<unsigned>(0), tz);
		return true;

	} else if (sscanf(c_str, "%d-%u-%u%31s", &year, &month, &day, rest) >= 3) {
		const std::string timezone_iso(rest);
		const double tz = (timezone_iso.empty())? time_zone : Date::parseTimeZone(timezone_iso);
		if (tz==nodata) return false;
		t.setDate(year, month, day, static_cast<unsigned>(0), static_cast<unsigned>(0), static_cast<unsigned>(0), tz);
		return true;

	} else if (sscanf(c_str, "%u:%u%31s", &hour, &minute, rest) >= 2) {
		const std::string timezone_iso(rest);
		const double tz = (timezone_iso.empty())? time_zone : Date::parseTimeZone(timezone_iso);
		if (tz==nodata) return false;
		t.setDate( (static_cast<double>(hour))/24. + (static_cast<double>(minute))/24./60. , tz);
		return true;

	} else { //purely numeric date: YYYYMMDDHHmmss where ss and mm can be skipped
		const size_t wrong_dash1 = str.find("–");
		const size_t wrong_dash2 = str.find("Ð");
		if (wrong_dash1!=std::string::npos || wrong_dash2!=std::string::npos)
			throw InvalidFormatException("Invalid date '"+str+"', please use the '-' character as date delimiter", AT);

		//try to read purely numerical date, potentially surrounded by other chars
		//and potentially containing an ISO time zone string
		const size_t in_len = str.length();

		//extract date/time
		const size_t date_beg = str.find_first_of(NUM);
		if (date_beg==npos || date_beg==in_len) return false;
		size_t date_end = str.find_first_not_of(NUM, date_beg+1);
		if (date_end==npos) date_end = in_len;
		const std::string date( str.substr(date_beg, date_end-date_beg) );

		//parse date/time
		const size_t date_len = date.length();
		if (date_len<10 || date_len>14) return false;
		if (convertString(year,date.substr(0,4))==false) return false;
		if (convertString(month,date.substr(4,2))==false) return false;
		if (convertString(day,date.substr(6,2))==false) return false;
		if (convertString(hour,date.substr(8,2))==false) return false;
		if (date_len==10)
			minute=0;
		else {
			if (date_len>=12) {
				if ( convertString(minute,date.substr(10,2))==false ) return false;
			} else
				return false;
			if (date_len==12)
				second=0;
			else {
				if (date_len==14) {
					if (convertString(second,date.substr(12,2))==false) return false;
				} else
					return false;
			}
		}

		//extract potential ISO time zone string
		double tz = time_zone;
		const size_t tz_beg = str.find_first_of("+-", date_end);
		if (tz_beg!=npos && tz_beg!=in_len) {
			size_t tz_end = str.find_first_not_of("0123456789:", date_end+1);
			if (tz_end==npos) tz_end = in_len;
			const std::string timezone_iso( str.substr(tz_beg, tz_end-tz_beg) );
			if (!timezone_iso.empty()) tz = Date::parseTimeZone(timezone_iso);
		}

		t.setDate( year, month, day, hour, minute, second, tz );
	}

	return true;
}

template<> bool convertString<Coords>(Coords& t, std::string str, std::ios_base& (*f)(std::ios_base&))
{
	trim(str); //delete trailing and leading whitespaces and tabs

	(void)f;
	double lat, lon;
	try {
		CoordsAlgorithms::parseLatLon(str, lat, lon);
	} catch(const IOException&) {
		return false;
	}
	t.setLatLon(lat, lon, nodata);

	return true;
}


void getProjectionParameters(const Config& cfg, std::string& coordin, std::string& coordinparam,
                                      std::string& coordout, std::string& coordoutparam)
{
	cfg.getValue("COORDSYS", "Input", coordin);
	cfg.getValue("COORDPARAM", "Input", coordinparam, IOUtils::nothrow);
	cfg.getValue("COORDSYS", "Output", coordout, IOUtils::nothrow);
	cfg.getValue("COORDPARAM", "Output", coordoutparam, IOUtils::nothrow);
}

void getProjectionParameters(const Config& cfg, std::string& coordin, std::string& coordinparam)
{
	cfg.getValue("COORDSYS", "Input", coordin);
	cfg.getValue("COORDPARAM", "Input", coordinparam, IOUtils::nothrow);
}

void getTimeZoneParameters(const Config& cfg, double& tz_in, double& tz_out)
{
	cfg.getValue("TIME_ZONE", "Input", tz_in, IOUtils::nothrow);
	cfg.getValue("TIME_ZONE", "Output", tz_out, IOUtils::nothrow);
}

size_t seek(const Date& soughtdate, const std::vector<MeteoData>& vecM, const bool& exactmatch)
{
	if (vecM.empty() || soughtdate < vecM.front().date || soughtdate > vecM.back().date) {
		//the sought date is not contained in the vector, return npos
		return npos;
	}

	const size_t max_idx = vecM.size()-1; //obviously, the index must be <= max_idx

	//since usually the sampling rate is quite constant, try to guess where our point
	//should be and provide a much smaller search interval around it
	const double start_date = vecM.front().date.getJulian(true);
	const double end_date = vecM.back().date.getJulian(true);
	if (start_date==end_date) return 0; //there is only one element

	const double curr_date = soughtdate.getJulian(true);
	const double raw_pos = (curr_date-start_date) / (end_date-start_date) * static_cast<double>(max_idx); //always >=0
	const size_t start_idx = static_cast<size_t>( floor(raw_pos*.9) );
	const size_t end_idx = std::min( static_cast<size_t>( ceil(raw_pos*1.1) ), max_idx);

	//first and last index of the search interval, either using our initial guess or the full vector
	size_t first = (curr_date >= vecM[start_idx].date.getJulian(true))? start_idx : 0;
	size_t last = (curr_date <= vecM[end_idx].date.getJulian(true))? end_idx : max_idx;

	//if we reach this point: the date is spanned by the buffer and there are at least two elements
	if (exactmatch){
		//perform binary search
		while (first <= last) {
			const size_t mid = (first + last) / 2;  // compute mid point
			if (soughtdate > vecM[mid].date)
				first = mid + 1;                   // repeat search in top half
			else if (soughtdate < vecM[mid].date)
				last = mid - 1;                    // repeat search in bottom half
			else
				return mid;                        // found it. return position
		}
	} else {
		//perform binary search
		while (first <= last) {
			const size_t mid = (first + last) / 2;  // compute mid point

			if (mid < max_idx) {
				if ((soughtdate > vecM[mid].date) && (soughtdate < vecM[mid+1].date))
					return mid+1;
			}

			if (soughtdate > vecM[mid].date)
				first = mid + 1;                   // repeat search in top half
			else if (soughtdate < vecM[mid].date)
				last = mid - 1;                    // repeat search in bottom half
			else
				return mid;                        // found it. return position
		}
	}

	return npos;
}

double unitsPrefix(const char& prefix)
{
	switch(prefix) {
		case 'f' : return 1e-15;
		case 'p' : return 1e-12;
		case 'n' : return 1e-9;
		case 'u' : return 1e-6;
		case 'm' : return 1e-3;
		case 'c' : return 1e-2;
		case 'd' : return 1e-1;
		case 'h' : return 1e2;
		case 'k' : return 1e3;
		case 'M' : return 1e6;
		case 'G' : return 1e9;
		case 'T' : return 1e12;
		case 'P' : return 1e15;
		default: {
			const std::string prefix_str( 1, prefix );
			throw InvalidArgumentException("Invalid unit prefix '"+prefix_str+"'", AT);
		}
	}
}

//currently, only the most simple units are handled. Composite units
//such as 'W/m2 <-> mW/cm2' are NOT handled.
double unitsConversion(const double& val, std::string unitIn, std::string unitOut)
{
	if (val==IOUtils::nodata)
		return IOUtils::nodata;
	if (unitIn.empty() || unitOut.empty())
		throw InvalidArgumentException("Units can not be empty!", AT);

	if (unitIn=="degK" || unitIn=="°K" || unitIn=="Kelvin")
		unitIn = "K";
	if (unitOut=="degK" || unitOut=="°K" || unitOut=="Kelvin")
		unitOut = "K";
	if (unitIn=="degC" || unitIn=="Celsius")
		unitIn = "°C";
	if (unitOut=="degC" || unitOut=="Celsius")
		unitOut = "°C";
	if (unitIn=="degF" || unitIn=="Fahrenheit")
		unitIn = "°F";
	if (unitOut=="degF" || unitOut=="Fahrenheit")
		unitOut = "°F";

	if (unitIn=="°C" && unitOut=="K") {
		return (val+Cst::t_water_triple_pt);
	} else if (unitIn=="K" && unitOut=="°C") {
		return (val-Cst::t_water_triple_pt);
	} else if (unitIn=="K" && unitOut=="°F") {
		return ((val-Cst::t_water_triple_pt)*1.8+32.);
	} else if (unitIn=="°F" && unitOut=="K") {
		return ((val-32.)/1.8+Cst::t_water_triple_pt);
	}  else if (unitIn=="°F" && unitOut=="°C") {
		return ((val-32.)/1.8);
	}  else if (unitIn=="°C" && unitOut=="°F") {
		return (val*1.8+32.);
	} else {
		//extract the unit prefix
		const double inPrefix_factor = (isalpha(unitIn[0]) && isalpha(unitIn[1]))? unitsPrefix( unitIn[0] ) : 1;
		const double outPrefix_factor = (isalpha(unitOut[0]) && isalpha(unitOut[1]))? unitsPrefix( unitOut[0] ) : 1;

		//extract the unit exponent
		const char in_last_char = unitIn[ unitIn.size()-1 ];
		const char out_last_char = unitOut[ unitOut.size()-1 ];
		const unsigned char inExponent = (isdigit(in_last_char))? static_cast<unsigned char>( in_last_char-'0' ) : static_cast<unsigned char>( 1 );
		const unsigned char outExponent = (isdigit(out_last_char))? static_cast<unsigned char>( out_last_char-'0' ) : static_cast<unsigned char>( 1 );

		//compute the input and output units factor
		const double inFactor = (inExponent==1)? inPrefix_factor : Optim::fastPow(inPrefix_factor, inExponent);
		const double outFactor = (outExponent==1)? outPrefix_factor : Optim::fastPow(outPrefix_factor, outExponent);

		const double ratio = inFactor / outFactor;
		return val*ratio;
	}
	//throw ConversionFailedException("Unable to perform unit conversion.", AT);
}

} //namespace IOUtils
} //namespace
