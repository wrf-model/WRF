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
#include <meteoio/plugins/libsmet.h>
#include <cerrno>
#include <cstring>
#include <string.h>
#include <limits>
#include <cmath>
#include <cstdlib>
#include <sstream>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <algorithm>

#if defined _WIN32 || defined __MINGW32__
	#include <windows.h>
#else
	#include <sys/stat.h>
#endif

using namespace std;

namespace smet {

const char* SMETCommon::smet_version = "1.1";
set<string> SMETCommon::all_mandatory_header_keys = set<std::string>();
set<string> SMETCommon::all_optional_header_keys  = set<std::string>();
set<string> SMETCommon::all_decimal_header_values = set<std::string>();

SMETException::SMETException(const std::string& message, const std::string& position)
              : msg( (position.empty())? "At unknown position: " + message : position + ": " + message + "\n") {}

SMETException::~SMETException() throw() {}

const char* SMETException::what() const throw()
{
	return msg.c_str();
}

const bool SMETCommon::__init = SMETCommon::initStaticData();
bool SMETCommon::initStaticData()
{
	all_mandatory_header_keys.insert("station_id");
	all_mandatory_header_keys.insert("nodata");
	all_mandatory_header_keys.insert("fields");

	all_optional_header_keys.insert("latitude");
	all_optional_header_keys.insert("longitude");
	all_optional_header_keys.insert("altitude");
	all_optional_header_keys.insert("easting");
	all_optional_header_keys.insert("northing");
	all_optional_header_keys.insert("epsg");

	all_optional_header_keys.insert("station_name");
	all_optional_header_keys.insert("tz");
	all_optional_header_keys.insert("creation");
	all_optional_header_keys.insert("source");
	all_optional_header_keys.insert("units_offset");
	all_optional_header_keys.insert("units_multiplier");
	all_optional_header_keys.insert("comment");

	all_decimal_header_values.insert("nodata");
	all_decimal_header_values.insert("latitude");
	all_decimal_header_values.insert("longitude");
	all_decimal_header_values.insert("altitude");
	all_decimal_header_values.insert("easting");
	all_decimal_header_values.insert("northing");
	all_decimal_header_values.insert("epsg");
	all_decimal_header_values.insert("tz");

	return true;
}

#if defined _WIN32 || defined __MINGW32__
bool SMETCommon::fileExists(const std::string& filename)
{
	const DWORD attributes = GetFileAttributes( filename.c_str() );

	if (attributes==INVALID_FILE_ATTRIBUTES || attributes==FILE_ATTRIBUTE_VIRTUAL
	     || attributes==FILE_ATTRIBUTE_DIRECTORY || attributes==FILE_ATTRIBUTE_DEVICE)
		return false;

	return true;
}
#else
bool SMETCommon::fileExists(const std::string& filename)
{
	struct stat buffer ;

	if ((stat( filename.c_str(), &buffer))!=0) {//File exists if stat returns 0
		return false;
	}

	if (S_ISREG(buffer.st_mode) || S_ISFIFO(buffer.st_mode) || S_ISLNK(buffer.st_mode))
		return true;
	else
		return false; //exclude char device, block device, sockets, etc
}
#endif

bool SMETCommon::validFileAndPath(const std::string& filename)
{
#if defined _WIN32 || defined __MINGW32__ || defined __CYGWIN__
	const size_t startpos = filename.find_first_not_of(" \t\n"); // Find the first character position after excluding leading blank spaces
	const size_t invalid_char = filename.find_first_of("\000*:<>?|"); //find possible invalid characters
#else
	const size_t startpos = filename.find_first_not_of(" \t\n"); // Find the first character position after excluding leading blank spaces
	const size_t invalid_char = filename.find_first_of("\000"); //find possible invalid characters
#endif

	if ((startpos!=0) || (invalid_char!=std::string::npos) || (filename==".") || (filename=="..")) {
		return false;
	}
	return true;
}

void SMETCommon::copy_file(const std::string& src, const std::string& dest)
{
	if (src == dest) return; //copying to the same file doesn't make sense, but is no crime either

	if (!fileExists(src)) throw SMETException("File '"+src+"' does not exist", AT);
	std::ifstream fin(src.c_str(), std::ios::binary);
	if (fin.fail()) throw SMETException("Failed to open file '"+src+"'", AT);

	if (!validFileAndPath(dest)) throw SMETException("Destination file name '"+dest+"' is invalid", AT);
	std::ofstream fout(dest.c_str(), std::ios::binary);
	if (fout.fail()) {
		fin.close();
		throw SMETException("Failed to open destination file '"+dest+"'", AT);
	}

	fout << fin.rdbuf();

	fin.close();
	fout.close();
}

double SMETCommon::convert_to_double(const std::string& in_string)
{
	char* conversion_end = NULL;
	const double conversion_value = strtod(in_string.c_str(), &conversion_end);

	if (*conversion_end == '\0') {
		return conversion_value;
	} else {
		throw SMETException("Value \"" + in_string + "\" cannot be converted to double", SMET_AT);
	}
}

int SMETCommon::convert_to_int(const std::string& in_string)
{
	istringstream ss(in_string);
	int value;
	if (!(ss >> value)) throw SMETException("Value \"" + in_string + "\" cannot be converted to int", SMET_AT);

	return value;
}

void SMETCommon::trim(std::string& str)
{
	const std::string whitespaces(" \t\f\v\n\r");
	const size_t startpos = str.find_first_not_of(whitespaces); // Find the first character position after excluding leading blank spaces
	const size_t endpos = str.find_last_not_of(whitespaces); // Find the first character position from reverse af

	// if all spaces or empty return an empty string
	if (( std::string::npos == startpos ) || ( std::string::npos == endpos)) {
		str.clear();
	} else {
		str.erase(endpos+1); //right trim
		str.erase(0, startpos); //left trim
	}
}

bool SMETCommon::readKeyValuePair(const std::string& in_line, const std::string& delimiter,
                                  std::map<std::string,std::string>& out_map)
{
	//size_t pos = in_line.find(delimiter); //first occurence of '='

	size_t pos = std::string::npos;
	if ((delimiter==" ") || (delimiter=="\t")) {
		pos = in_line.find_first_of(" \t", 0);
	} else {
		pos = in_line.find(delimiter); //first occurence of '='
	}


	if (pos != std::string::npos) { //ignore in_lines that are empty or without '='
		std::string key = in_line.substr(0, pos);
		std::string value = in_line.substr(pos + 1);

		SMETCommon::trim(key);
		SMETCommon::trim(value);

		if (key.empty() || value.empty()) {
			return false;
		}

		out_map[key] = value;
	} else {
		return false;
	}

	return true;
}

void SMETCommon::stripComments(std::string& str)
{
	const size_t found = str.find_first_of("#;");
	if (found != std::string::npos){
		str.erase(found); //rest of line disregarded
	}
}

void SMETCommon::toUpper(std::string& str)
{
	std::transform(str.begin(), str.end(), str.begin(), ::toupper);
}

char SMETCommon::getEoln(std::istream& fin)
{
	std::streambuf* pbuf;
	char tmp = '0';
	int chars = 0;

	const std::streampos position = fin.tellg();

	do {
		fin.get(tmp);
		chars++;

		if ((tmp == '\r') || (tmp == '\n')) {
			char peekc = tmp;
			while ((!fin.eof() && ((peekc=='\r') || (peekc=='\n')))) {
				tmp = peekc;
				fin.get(peekc);
				chars++;
			}
			pbuf = fin.rdbuf();
			pbuf->pubseekpos(position); //rewind
			fin.clear(); //reset eof flag, etc
			return tmp;
		}
	} while ((chars < 3000) && (!fin.eof()));

	pbuf = fin.rdbuf();
	pbuf->pubseekpos(position); //rewind
	fin.clear(); //reset eof flag, etc

	return '\n';
}

bool SMETCommon::is_decimal(const std::string& value)
{
	istringstream ss(value);
	double doublevalue;
	if (!(ss >> doublevalue)) return false;

	return true;
}

size_t SMETCommon::readLineToVec(const std::string& line_in, std::vector<std::string>& vec_string)
{
	vec_string.clear();
	std::istringstream iss(line_in); //construct inputstream with the string line as input

	std::string tmp_string;
	while (!iss.eof()) {
		iss >> std::skipws >> tmp_string;

		if (!tmp_string.empty()) {
			vec_string.push_back( tmp_string );
		}
		tmp_string.clear();
	}

	return vec_string.size();
}


////////////////////////////////////////////////////////////
//// SMETWriter class
SMETWriter::SMETWriter(const std::string& in_filename, const SMETType& in_type)
           : other_header_keys(), ascii_precision(), ascii_width(), header(), mandatory_header_keys(),
             filename(in_filename), nodata_string(), smet_type(in_type), nodata_value(-999.), nr_of_fields(0),
             julian_field(0), timestamp_field(0), location_wgs84(0), location_epsg(0),
             location_in_header(false), location_in_data_wgs84(false), location_in_data_epsg(false),
             timestamp_present(false), julian_present(false), file_is_binary(false), append_mode(false), append_possible(false) {}

SMETWriter::SMETWriter(const std::string& in_filename, const std::string& in_fields, const double& in_nodata)
           : other_header_keys(), ascii_precision(), ascii_width(), header(), mandatory_header_keys(),
             filename(in_filename), nodata_string(), smet_type(ASCII), nodata_value(in_nodata), nr_of_fields(0),
             julian_field(0), timestamp_field(0), location_wgs84(0), location_epsg(0),
             location_in_header(false), location_in_data_wgs84(false), location_in_data_epsg(false),
             timestamp_present(false), julian_present(false), file_is_binary(false), append_mode(true), append_possible(false)
{
	std::vector<std::string> vecFields;
	SMETCommon::readLineToVec(in_fields, vecFields);
	setAppendMode(vecFields);
}

//what the caller MUST provide:
//	* the fields header (for checks)
//	* the ascii width and precision
//	* nodata_value provided through set_header_value() calls
//HACK: the multipliers and offsets are not handled yet...
//for now, we do the most basic append: all the data after the first insertion point is deleted
void SMETWriter::setAppendMode(std::vector<std::string> vecFields)
{
	SMETReader reader(filename);
	smet_type = (reader.isAscii)? ASCII : BINARY;
	nodata_string = reader.get_header_value("nodata");
	//nodata_value = reader.nodata_value; //we trust the value provided to the constructor

	nr_of_fields = reader.nr_of_fields; //in SMETReader, this does NOT contain the timestamp
	julian_field = reader.julian_field;
	timestamp_field = reader.timestamp_field;
	timestamp_present = reader.timestamp_present;
	julian_present = reader.julian_present;

	location_wgs84 = reader.location_wgs84;
	location_epsg = reader.location_epsg;
	location_in_header = reader.location_in_header(WGS84) || reader.location_in_header(EPSG);
	location_in_data_wgs84 = reader.location_in_data(WGS84);
	location_in_data_epsg = reader.location_in_data(EPSG);

	//check that the fields match
	if (timestamp_present) {
		if (vecFields[timestamp_field]!="timestamp") {
			std::ostringstream ss;
			ss << "Timestamp should be in field at position " << timestamp_field << " when  appending data to file '" << filename << "' but instead '" << reader.get_field_name(timestamp_field) << "' was found";
			throw SMETException(ss.str(), AT);
		}
		vecFields.erase( vecFields.begin()+timestamp_field );
	}
	if (julian_present) {
		if (vecFields[julian_field]!="julian") {
			std::ostringstream ss;
			ss << "Julian should be in field at position " << julian_field << "when  appending data to file '" << filename << "' but instead '" << reader.get_field_name(julian_field) << "' was found";
			throw SMETException(ss.str(), AT);
		}
		vecFields.erase( vecFields.begin()+julian_field );
	}

	if (nr_of_fields!=vecFields.size()) {
		std::ostringstream ss;
		ss << "Trying to append " << vecFields.size() << " fields in existing file '" << filename << "' that has " << nr_of_fields << " fields";
		throw SMETException(ss.str(), AT);
	}
	for (size_t ii=0; ii<nr_of_fields; ii++) {
		if (vecFields[ii] != reader.get_field_name(ii)) {
			std::ostringstream ss;
			ss << "Trying to append field '" << vecFields[ii] << "' at position " << ii << " in existing file '" << filename << "' when it should be '" << reader.get_field_name(ii);
			throw SMETException(ss.str(), AT);
		}
	}

	//adjust the number of fields to reflect the potential presence of timestamp and julian
	if (timestamp_present) nr_of_fields++;
	if (julian_present) nr_of_fields++;
}

const std::string SMETWriter::toString() const {
	ostringstream os;
	os << "<SMETWriter>\n";
	os << "\tfilename: " << filename << "\n";
	os << "\ttype: " << ((smet_type==ASCII)? "Ascii" : "Binary") << " append_mode: " << std::boolalpha << append_mode << " append_possible: " << append_possible << "\n";
	os << "\ttimestamp_present: " << timestamp_present << " field: " << timestamp_field << " julian_present: " << julian_present << " field: " << julian_field << "\n";
	os << "\tlocation: in header? " << location_in_header << " in data_wgs84? " << location_in_data_wgs84 << " in data_epsg? " << location_in_data_epsg << "\n";
	os << "\tlocation_wgs84: " << location_wgs84 << " location_epsg: " << location_epsg << "\n";
	os << "\tnodata: " << nodata_value << " = \"" << nodata_string << "\"\n";
	os << "\tnr_of_fields: " << nr_of_fields << "\n";
	os << "</SMETWriter>\n";
	return os.str();
}

void SMETWriter::set_header_value(const std::string& key, const double& value)
{
	//check if key is decimal, transform to string and add to header
	if (SMETCommon::all_decimal_header_values.find(key) != SMETCommon::all_decimal_header_values.end()){
		ostringstream ss;
		if ((key == "latitude") || (key == "longitude") || (key == "easting") || (key == "northing")){
			ss << fixed << setprecision(6) << value;
		} else if (key == "altitude"){
			ss << fixed << setprecision(1) << value;
		} else if ((key == "epsg") || (key == "tz")){
			ss << fixed << setprecision(0) << value;
		} else {
			ss << value; //for nodata
		}

		set_header_value(key, ss.str());
	} else { //It's a non-standard header value
		ostringstream ss;
		ss << value; //for nodata
		set_header_value(key, ss.str());
	}
}

void SMETWriter::set_header_value(const std::string& key, const std::string& value)
{
	//check if header key/value pair is valid
	if (valid_header_pair(key, value)){
		header[key] = value;

		if ((SMETCommon::all_optional_header_keys.find(key) == SMETCommon::all_optional_header_keys.end())
		    && (SMETCommon::all_mandatory_header_keys.find(key) == SMETCommon::all_mandatory_header_keys.end()))
			other_header_keys.push_back(key);
	} else {
		throw SMETException("Invalid, inconsistent or unknown key/value pair: " + key + " = " + value, SMET_AT);
	}
}

bool SMETWriter::valid_header_pair(const std::string& key, const std::string& value)
{
	bool key_ok = true;

	if (SMETCommon::all_mandatory_header_keys.find(key) != SMETCommon::all_mandatory_header_keys.end()){
		mandatory_header_keys.insert(key);
		key_ok = true;
	}

	//nodata value needs extra treatment
	if (key == "nodata"){
		std::istringstream ss(value);
		if (!(ss >> nodata_value)) return false;
		nodata_string = value;
	}

	//Using WGS_84 coordinate system
	if (key == "latitude")  location_wgs84 |= 1;
	else if (key == "longitude") location_wgs84 |= 2;
	else if (key == "altitude")  location_wgs84 |= 4;

	//Using an EPSG coordinate system
	if (key == "easting")  location_epsg |= 1;
	else if (key == "northing") location_epsg |= 2;
	else if (key == "altitude") location_epsg |= 4;
	else if (key == "epsg")     location_epsg |= 8;

	//Now do some value checks
	if (key == "epsg"){
		std::istringstream ss(value);
		int intvalue;
		if (!(ss >> intvalue)) return false;
	}

	if (SMETCommon::all_decimal_header_values.find(key) != SMETCommon::all_decimal_header_values.end()){
		//check if value is a double value
		if (!SMETCommon::is_decimal(value)) return false;
	}

	if ((location_epsg == 15) || (location_wgs84 == 7))
		location_in_header = true;

	//Rudimentary check on keys: fields, units_offset, units_multiplier
	if ((key == "fields") || (key == "units_offset") || (key == "units_multiplier"))
		key_ok = check_fields(key, value);

	return key_ok;
}

bool SMETWriter::check_fields(const std::string& key, const std::string& value)
{
	vector<string> tmp_vec;
	const size_t counter = SMETCommon::readLineToVec(value, tmp_vec);

	//Firstly: Check if number of fields is consistent
	if (nr_of_fields != 0){
		if (counter != nr_of_fields) return false;
	} else {
		nr_of_fields = counter;
	}

	if (key == "fields"){
		size_t count_wgs84 = 0, count_epsg = 0;

		//check if location is in data and if timestamp is present
		for (size_t ii = 0; ii<tmp_vec.size(); ii++){
			if (tmp_vec[ii] == "latitude") count_wgs84++;
			else if (tmp_vec[ii] == "longitude") count_wgs84++;
			else if (tmp_vec[ii] == "easting") count_epsg++;
			else if (tmp_vec[ii] == "northing") count_epsg++;

			if (tmp_vec[ii] == "altitude") {
				count_wgs84++;
				count_epsg++;
			}

			if (tmp_vec[ii] == "timestamp"){
				if (timestamp_present) return false; //no duplicate timestamp field allowed
				timestamp_present = true;
				timestamp_field = ii;
			}

			if (tmp_vec[ii] == "julian"){
				if (julian_present) return false; //no duplicate julian field allowed
				julian_present = true;
				julian_field = ii;
			}
		}

		if (count_wgs84 == 3)
			location_in_data_wgs84 = true;

		if (count_epsg == 3)
			location_in_data_epsg = true;
	} else {
		//every value in units_offset and units_multiplier must be a decimal
		for (vector<string>::const_iterator it = tmp_vec.begin(); it != tmp_vec.end(); ++it)
			if (!SMETCommon::is_decimal(*it)) return false;
	}

	return true;
}

bool SMETWriter::valid_header()
{
	if (mandatory_header_keys.size() != 3)
		return false;

	if (location_in_data_epsg){ //EPSG code must be in the header anyway
		const map<string,string>::const_iterator it = header.find("epsg");
		if (it == header.end()){
			return false;
		} else {
			return true;
		}
	}

	if (location_in_header || location_in_data_wgs84)
		return true;

	return false;
}

void SMETWriter::write(const std::vector<std::string>& vec_timestamp, const std::vector<double>& data)
{
	if (!SMETCommon::validFileAndPath(filename)) throw SMETException("Invalid file name \""+filename+"\"", AT);
	errno = 0;

	bool write_headers = false;
	ios_base::openmode mode_flags = ios::binary; //read as binary to avoid eol mess
	if (append_mode) {
		if (!append_possible) {
			//check where to insert the new data
			SMETReader reader(filename);
			const std::string last_timestamp( reader.getLastTimestamp() );
			if (last_timestamp.empty() || last_timestamp>=vec_timestamp[0])
				reader.truncate_file(vec_timestamp[0]);
			append_possible = true;
		}
		mode_flags = ios::binary | ofstream::app;
	} else { //normal mode
		if (!append_possible) { //first write -> overwrite potential previous content
			write_headers = true;
			append_possible = true; //now all other calls to "open" will be in append mode
		} else { //after the first write: append
			mode_flags = ios::binary | ofstream::app;
		}
	}
	
	std::ofstream fout(filename.c_str(), mode_flags);
	if (fout.fail())
		throw SMETException("Error opening file \"" + filename + "\" for writing, possible reason: " + std::string(std::strerror(errno)), SMET_AT);
	if (write_headers) write_header(fout); //Write the header info, always in ASCII format
	

	if (vec_timestamp.empty() || data.empty() || nr_of_fields == 0) {//the header has been written, nothing to add
		fout.close();
		return;
	}

	if (!timestamp_present) {
		fout.close();
		throw SMETException("No timestamp present when writing file \""+filename+"\", use write(const vector<double>& data)", SMET_AT);
	}

	const size_t nr_of_data_fields = nr_of_fields - 1;
	if (nr_of_fields<=1) { //avoid division by zero in the next block
		cerr << "[W] Attempting to write a dataset that contains no fields to file \"" + filename + "\"!\n";
		fout.close();
		return;
	}

	const size_t nr_of_lines = data.size() / (nr_of_fields-1);
	if ((nr_of_lines != vec_timestamp.size()) || ((data.size() % (nr_of_fields-1)) != 0)) {
		fout.close();
		ostringstream os;
		os << "Inconsistency between the number of timestamp (" << vec_timestamp.size() << ")  and number of data points (" << data.size() << " for " << nr_of_fields-1 << " fields) detected for file \""+filename+"\", recheck your data";
		throw SMETException(os.str(), SMET_AT);
	}

	std::vector<double> current_data(nr_of_fields-1);
	check_formatting();

	if (smet_type == ASCII) {
		for (size_t ii=0; ii<nr_of_lines; ii++) {
			const size_t offset = ii*(nr_of_fields-1);
			if (!data.empty())
				copy(data.begin()+offset, data.begin()+offset+nr_of_data_fields, current_data.begin());
			write_data_line_ascii(vec_timestamp[ii], current_data, fout);
		}
	} else {
		fout.close();
		throw SMETException("Cannot write binary file \""+filename+"\" with a timestamp, use julian instead", SMET_AT);
	}

	fout.close();
}

void SMETWriter::write(const std::vector<double>& data)
{
	if (!SMETCommon::validFileAndPath(filename)) throw SMETException("Invalid file name \""+filename+"\"", AT);
	errno = 0;
	std::ofstream fout(filename.c_str(), ios::binary);
	if (fout.fail()) {
		std::ostringstream ss;
		ss << "Error opening file \"" << filename << "\" for writing, possible reason: " << std::strerror(errno);
		throw SMETException(ss.str(), SMET_AT);
	}

	write_header(fout); //Write the header info, always in ASCII format

	if (nr_of_fields == 0){
		fout.close();
		return;
	}

	const size_t nr_of_lines = data.size() / nr_of_fields;
	if ((data.size() % nr_of_fields) != 0) {
		fout.close();
		throw SMETException("Inconsistency between data and header fields detected in file \""+filename+"\", recheck your data", SMET_AT);
	}

	std::vector<double> current_data(nr_of_fields);
	check_formatting();

	if (smet_type == ASCII){
		for (size_t ii=0; ii<nr_of_lines; ii++){
			if (!data.empty())
				copy(data.begin()+ii*nr_of_fields, data.begin()+ii*nr_of_fields+nr_of_fields, current_data.begin());
			write_data_line_ascii("0000-01-01T00:00", current_data, fout); //dummy time
		}
	} else {
		for (size_t ii=0; ii<nr_of_lines; ii++){
			if (!data.empty())
				copy(data.begin()+ii*nr_of_fields, data.begin()+ii*nr_of_fields+nr_of_fields, current_data.begin());
			write_data_line_binary(current_data, fout);
		}

		file_is_binary = false;
	}

	fout.close();
}

void SMETWriter::print_if_exists(const std::string& header_field, std::ofstream& fout) const
{
	const std::map<string,string>::const_iterator it = header.find(header_field);
	if (it != header.end()) {
		const std::ios_base::fmtflags flags = fout.setf(std::ios::left);
		const std::streamsize width = fout.width(16);
		fout << header_field << " = " << it->second << "\n";
		fout.width(width);
		fout.setf(flags);
	}
}

void SMETWriter::write_header(std::ofstream& fout)
{
	if (!valid_header()) {
		fout.close();
		throw SMETException("The header data you supplied is not valid, file \""+filename+"\" cannot be written", SMET_AT);
	}

	//write signature
	fout << "SMET " << SMETCommon::smet_version << " ";
	if (smet_type == ASCII) fout << "ASCII" << "\n";
	else fout << "BINARY" << "\n";

	fout << "[HEADER]" << "\n";
	print_if_exists("station_id", fout);
	print_if_exists("station_name", fout);

	if (location_in_header){
		if (location_wgs84 == 7){
			print_if_exists("latitude", fout);
			print_if_exists("longitude", fout);
			print_if_exists("altitude", fout);
		}

		if (location_epsg == 15){
			print_if_exists("easting", fout);
			print_if_exists("northing", fout);
			if (location_wgs84 != 7)
				print_if_exists("altitude", fout);
			print_if_exists("epsg", fout);
		}
	} else {
		if (location_in_data_epsg)
			print_if_exists("epsg", fout);
	}

	print_if_exists("nodata", fout);

	print_if_exists("tz", fout);
	print_if_exists("creation", fout);
	print_if_exists("source", fout);
	print_if_exists("units_offset", fout);
	print_if_exists("units_multiplier", fout);
	print_if_exists("comment", fout);

	for (size_t ii=0; ii<other_header_keys.size(); ii++){
		fout << std::left << std::setw(16) << other_header_keys[ii] << " = " << header[other_header_keys[ii]] << "\n";
	}

	print_if_exists("fields", fout);
	fout << "[DATA]" << endl;
}

void SMETWriter::write_data_line_binary(const std::vector<double>& data, std::ofstream& fout)
{
	const char eoln = '\n';

	if (!file_is_binary){ //open it as binary file
		fout.close();
		errno = 0;
		fout.open(filename.c_str(), ios::out | ios::app | ios::binary); //reopen as binary file
		if (fout.fail()) {
			std::ostringstream ss;
			ss << "Error writing to file \"" << filename << "\", possible reason: " << std::strerror(errno);
			throw SMETException(ss.str(), SMET_AT);
		}

		file_is_binary = true;
	}

	for (size_t ii = 0; ii < data.size(); ii++){
		if (julian_present && (julian_field == ii)){
			const double julian = data[ii];
			fout.write((const char*)&julian, sizeof(double)); //the julian date is written in 64bit IEEE754 precision
		} else {
			const float val = (float)data[ii];
			fout.write((const char*)&val, sizeof(float)); //normal data fields are written in 32bit IEEE754 precision
		}
	}

	fout.write((const char*)&eoln, sizeof(char));
}

void SMETWriter::write_data_line_ascii(const std::string& timestamp, const std::vector<double>& data, std::ofstream& fout)
{
	fout.fill(' ');
	fout << right;
	fout << fixed;

	if ((data.empty()) && timestamp_present) fout << timestamp;

	for (size_t ii = 0; ii < data.size(); ii++){
		if (ii > 0) fout << " ";
		if (timestamp_present && (timestamp_field == ii))	fout << timestamp << " ";
		fout << setw(ascii_width[ii]) << setprecision(ascii_precision[ii]);
		if (data[ii] == nodata_value) fout << nodata_string; //to have a nicer representation
		else fout << data[ii];
	}
	fout << "\n";
}

void SMETWriter::check_formatting()
{
	const size_t nr_of_data_fields = (timestamp_present)? nr_of_fields-1 : nr_of_fields;

	if ((ascii_precision.size() != nr_of_data_fields) || (ascii_width.size() != nr_of_data_fields)){
		ascii_precision.resize(nr_of_data_fields, 3);
		ascii_width.resize(nr_of_data_fields, 8);
	}

	if (julian_present){
		ascii_precision[julian_field] = 8;
		ascii_width[julian_field] = 16;
	}
}

void SMETWriter::set_width(const std::vector<int>& vec_width)
{
	ascii_width = vec_width;
}

void SMETWriter::set_precision(const std::vector<int>& vec_precision)
{
	ascii_precision = vec_precision;
}


////////////////////////////////////////////////////////////
//// SMETReader class
const size_t SMETReader::streampos_every_n_lines = 2000; //save streampos every 2000 lines of data

SMETReader::SMETReader(const std::string& in_fname)
            : data_start_fpointer(), vec_offset(), vec_multiplier(), vec_fieldnames(),
              header(), indexer(),
              filename(in_fname), timestamp_start("-4714-11-24T00:00"),
              timestamp_end("9999-12-31T00:00"), nodata_value(-999.),
              julian_start(0.), julian_end(5373483.5),
              nr_of_fields(0), timestamp_field(0), julian_field(0),
              location_wgs84(0), location_epsg(0), location_data_wgs84(0), location_data_epsg(0),
              eoln('\n'),
              timestamp_present(false), julian_present(false), isAscii(true), mksa(true),
              timestamp_interval(false), julian_interval(false)
{
	if (!SMETCommon::fileExists(filename)) throw SMETException("File '"+filename+"' does not exists", AT); //prevent invalid filenames
	errno = 0;
	std::ifstream fin(filename.c_str(), ios::in|ios::binary); //ascii does end of line translation, which messes up the pointer code
	if (fin.fail()) {
		std::ostringstream ss;
		ss << "Error opening file \"" << filename << "\" for reading, possible reason: " << std::strerror(errno);
		ss << " Please check file existence and permissions!";
		throw SMETException(ss.str(), SMET_AT);
	}

	try {
		eoln = SMETCommon::getEoln(fin); //get the end of line character for the file
		read_header(fin);
		process_header();
	} catch(...){
		cleanup(fin); //closes file
		throw;
	}
	cleanup(fin); //closes file
}

void SMETReader::cleanup(std::ifstream& fin) throw()
{
	if (fin.is_open()) //close fin if open
		fin.close();
}

void SMETReader::convert_to_MKSA(const bool& in_mksa)
{
	mksa = in_mksa;
}

std::string SMETReader::get_field_name(const size_t& nr_of_field)
{
	if (nr_of_field < nr_of_fields){
		return vec_fieldnames[nr_of_field];
	} else {
		ostringstream ss;
		ss << "Trying to access field #" << nr_of_field << " (starting from 0) of " << nr_of_fields << " fields in file \"" << filename << "\". ";
		ss << "This is out of bounds!";
		throw SMETException(ss.str(), SMET_AT);
	}
}

size_t SMETReader::get_nr_of_fields() const
{
	return nr_of_fields;
}

void SMETReader::get_units_conversion(std::vector<double>& offset, std::vector<double>& multiplier) const
{
	multiplier = vec_multiplier;
	offset = vec_offset;
}

bool SMETReader::location_in_header(const LocationType& type) const
{
	if ((location_epsg == 15) && (type == EPSG)){
		return true;
	} else if ((location_wgs84 == 7) && (type == WGS84)){
		return true;
	}

	return false;
}

bool SMETReader::location_in_data(const LocationType& type) const
{
	if ((location_epsg == 8) && (location_data_epsg == 7) && (type == EPSG)){
		return true;
	} else if ((location_data_wgs84 == 7) && (type == WGS84)){
		return true;
	}

	return false;
}

void SMETReader::process_header()
{
	vector<string> tmp_vec;
	set<string> obligatory_keys;
	for (map<string,string>::iterator it = header.begin(); it != header.end(); ++it){
		if (SMETCommon::all_mandatory_header_keys.find(it->first) != SMETCommon::all_mandatory_header_keys.end())
			obligatory_keys.insert(it->first);

		if (it->first == "fields"){
			SMETCommon::readLineToVec(it->second, tmp_vec);
			string newfields;
			if (!tmp_vec.empty()){
				for (size_t ii=0; ii<tmp_vec.size(); ii++){
					if (tmp_vec[ii] == "timestamp"){
						timestamp_present = true;
						timestamp_field = ii;
					} else {
						if (nr_of_fields > 0) newfields += " ";
						vec_fieldnames.push_back(tmp_vec[ii]);
						newfields += tmp_vec[ii];
						nr_of_fields++;
					}

					if (tmp_vec[ii] == "julian"){
						julian_present = true;
						julian_field = ii;
					}

					//Using WGS_84 coordinate system
					if (tmp_vec[ii] == "latitude")  location_data_wgs84 |= 1;
					else if (tmp_vec[ii] == "longitude") location_data_wgs84 |= 2;
					else if (tmp_vec[ii] == "altitude")  location_data_wgs84 |= 4;

					//Using an EPSG coordinate system
					if (tmp_vec[ii] == "easting")  location_data_epsg |= 1;
					else if (tmp_vec[ii] == "northing") location_data_epsg |= 2;
					else if (tmp_vec[ii] == "altitude") location_data_epsg |= 4;
				}
				it->second = newfields;
			}
		}

		if (it->first == "nodata")
			nodata_value = SMETCommon::convert_to_double(it->second);

		//Using WGS_84 coordinate system
		if (it->first == "latitude")  location_wgs84 |= 1;
		else if (it->first == "longitude") location_wgs84 |= 2;
		else if (it->first == "altitude")  location_wgs84 |= 4;

		//Using an EPSG coordinate system
		if (it->first == "easting")  location_epsg |= 1;
		else if (it->first == "northing") location_epsg |= 2;
		else if (it->first == "altitude") location_epsg |= 4;
		else if (it->first == "epsg")     location_epsg |= 8;

		//Now do a value check on EPSG
		if (it->first == "epsg")
			SMETCommon::convert_to_int(it->second);
	}

	if (get_header_value("units_offset") != ""){
		vector<string> tmp_offset;
		string offsetstring;
		SMETCommon::readLineToVec(get_header_value("units_offset"), tmp_offset);

		for (size_t ii=0; ii<tmp_offset.size(); ii++){
			if (!timestamp_present || (ii != timestamp_field)){
				if (ii>0) offsetstring += " ";
				offsetstring += tmp_offset[ii];
				vec_offset.push_back(SMETCommon::convert_to_double(tmp_offset[ii]));
			}
		}
		header["units_offset"] = offsetstring;
	} else {
		vec_offset.resize(nr_of_fields, 0.0);
	}

	if (get_header_value("units_multiplier") != ""){
		vector<string> tmp_multiplier;
		string multiplierstring;
		SMETCommon::readLineToVec(get_header_value("units_multiplier"), tmp_multiplier);

		for (size_t ii=0; ii<tmp_multiplier.size(); ii++){
			if (!timestamp_present || (ii != timestamp_field)){
				if (ii>0) multiplierstring += " ";
				multiplierstring += tmp_multiplier[ii];
				vec_multiplier.push_back(SMETCommon::convert_to_double(tmp_multiplier[ii]));
			}
		}
		header["units_multiplier"] = multiplierstring;
	} else {
		vec_multiplier.resize(nr_of_fields, 1.0);
	}

	if (obligatory_keys.size() != SMETCommon::all_mandatory_header_keys.size())
		throw SMETException("\""+filename+"\" is not a valid SMET file, mandatory information in header is missing", SMET_AT);

	const bool location_meta_present = (location_wgs84 == 7) || (location_epsg == 15) || (location_data_wgs84 == 7)
	    || ((location_epsg == 8) && (location_data_epsg == 7));
	if (!location_meta_present) {
		ostringstream ss;
		ss << "Mandatory location data is missing in file '" << filename << "'";
		std::string missing;
		if ((location_epsg>0 && !(location_epsg & 4)) || (location_wgs84>0 && !(location_wgs84 & 4))) //altitude meta is missing
			missing += "altitude";
		if (location_epsg>0 && location_epsg<8) {//epsg meta is missing
			if (!missing.empty()) missing += ", ";
			missing += "epsg";
		}
		if (missing.empty()) missing = "lat/lon or easting/northing";
		ss << ": " << missing;

		throw SMETException(ss.str(), SMET_AT);
	}

	const size_t nr_offset = vec_offset.size();
	const size_t nr_multiplier = vec_multiplier.size();
	if (nr_offset!=nr_of_fields || nr_multiplier!=nr_of_fields) {
		const size_t Nfields = (timestamp_present)? nr_of_fields+1 : nr_of_fields;
		const size_t Noffset = (timestamp_present)? nr_offset+1 : nr_offset;
		const size_t Nmultiplier =(timestamp_present)? nr_multiplier+1 : nr_multiplier;
		std::ostringstream ss;
		ss << "File \"" << filename << "\" has " << Nfields << " data fields, but specifies ";
		ss << Noffset << " offsets and " << Nmultiplier << " multipliers. ";
		ss << "The number of offsets and multipliers MUST fit the number of fields, including the (optional) timestamp column.";
		throw SMETException(ss.str(), SMET_AT);
	}
}

void SMETReader::read_header(std::ifstream& fin)
{
	//1. Read signature
	std::string line;
	vector<string> tmpvec;
	getline(fin, line, eoln); //read complete signature line
	SMETCommon::stripComments(line);
	SMETCommon::readLineToVec(line, tmpvec);
	checkSignature(tmpvec, isAscii);

	//2. Read Header
	while (!fin.eof() && (fin.peek() != '[')) //skip lines until '[' is found
		getline(fin, line, eoln);

	getline(fin, line, eoln);

	SMETCommon::stripComments(line);
	SMETCommon::trim(line);
	SMETCommon::toUpper(line);

	if (line != "[HEADER]")
		throw SMETException("Read \"" + line + "\" in "+ filename + " when expecting \"[HEADER]\"", AT);

	while (!fin.eof() && (fin.peek() != '[')){ //Read until next section
		getline(fin, line, eoln);
		SMETCommon::stripComments(line);
		SMETCommon::trim(line);

		if (!line.empty()) {
			if (!SMETCommon::readKeyValuePair(line, "=", header))
				throw SMETException("Invalid key value pair in file \""+filename+"\", in [Header] section: " + line, SMET_AT);
		}

	}

	//Read [DATA] section tag
	getline(fin, line, eoln);
	SMETCommon::stripComments(line);
	SMETCommon::trim(line);
	SMETCommon::toUpper(line);

	if (line != "[DATA]")
		throw SMETException("In '"+filename+"', expected [DATA] but found: "+line, SMET_AT);

	data_start_fpointer = fin.tellg();
}

void SMETReader::truncate_file(const std::string& date_stop) const
{
	if (!SMETCommon::fileExists(filename)) throw SMETException("File '"+filename+"' does not exists", AT); //prevent invalid filenames
	errno = 0;
	std::ifstream fin(filename.c_str(), ios::in|ios::binary); //ascii does end of line translation, which messes up the pointer code
	if (fin.fail()) {
		std::ostringstream ss;
		ss << "Error opening file \"" << filename << "\" for reading, possible reason: " << std::strerror(errno);
		ss << " Please check file existence and permissions!";
		throw SMETException(ss.str(), SMET_AT);
	}

	const std::string filename_tmp( filename + ".tmp" );
	std::ofstream fout(filename_tmp.c_str(), ios::out|ios::binary); //for the tmp file
	if (fout.fail()) {
		std::ostringstream ss;
		ss << "Error opening temporary file \"" << filename_tmp << "\" for reading, possible reason: " << std::strerror(errno);
		ss << " Please check file existence and permissions!";
		throw SMETException(ss.str(), SMET_AT);
	}

	copy_file_header(fin, fout);
	copy_file_data(date_stop, fin, fout);

	fout.close();
	fin.close();

	SMETCommon::copy_file(filename_tmp, filename);

	errno = 0;
	if (remove(filename_tmp.c_str())!=0) { //delete temporary file
		std::ostringstream ss;
		ss << "Error deleting file \"" << filename << "\", possible reason: " << std::strerror(errno);
		throw SMETException(ss.str(), SMET_AT);
	}
}

void SMETReader::copy_file_header(std::ifstream& fin, std::ofstream& fout) const
{
	std::string line;
	while (!fin.eof()){ //Read until end of file or break
		line.clear();
		getline(fin, line, eoln); //read complete signature line
		fout << line << "\n";
		if (!line.empty() && line[0]=='[' && (line != "[HEADER]" && line != "[DATA]"))
			throw SMETException("In '"+filename+"', expected [DATA] but found: "+line, SMET_AT);
		if (!line.empty() && line == "[DATA]") break;
	}
}

void SMETReader::checkSignature(const std::vector<std::string>& vecSignature, bool& o_isAscii)
{
	if ((vecSignature.size() != 3) || (vecSignature[0] != "SMET"))
		throw SMETException("The signature of file " + filename + " is invalid. Is it really a SMET file?", SMET_AT);

	const std::string version = vecSignature[1];
	if ((version != "0.9") && (version != "0.95") && (version != "0.99")
	    && (version != "1.0") && (version != SMETCommon::smet_version))
		throw SMETException("Unsupported file format version for file " + filename, SMET_AT);

	if (version=="0.9" || version=="0.95" || version=="0.99" || version=="1.0") {
		cerr << "[W] SMET specification 1.1 changes the priorities of units_multiplier and units_offset. "
			<< "Please check/update your files and bring them to 1.1!!" << endl;
	}

	const std::string type = vecSignature[2];
	if (type == "ASCII")
		o_isAscii = true;
	else if (type == "BINARY")
		o_isAscii = false;
	else
		throw SMETException("The 3rd column of file " + filename + " must be either ASCII or BINARY", SMET_AT);
}

void SMETReader::read(const std::string& i_timestamp_start, const std::string& i_timestamp_end,
                      std::vector<std::string>& vec_timestamp, std::vector<double>& vec_data)
{
	if (!timestamp_present){
		read(vec_timestamp, vec_data);
	} else {
		timestamp_interval = true;
		timestamp_start = i_timestamp_start;
		timestamp_end = i_timestamp_end;

		read(vec_timestamp, vec_data);

		timestamp_interval = false;
	}
}

void SMETReader::read(const double& i_julian_start, const double& i_julian_end, std::vector<double>& vec_data)
{
	if (!julian_present){
		read(vec_data);
	} else {
		julian_interval = true;
		julian_start = i_julian_start;
		julian_end = i_julian_end;

		read(vec_data);

		julian_interval = false;
	}
}

void SMETReader::read(std::vector<std::string>& vec_timestamp, std::vector<double>& vec_data)
{
	if (!timestamp_present)
		throw SMETException("Requesting to read timestamp when there is none present in \""+filename+"\"", SMET_AT);

	if (!SMETCommon::fileExists(filename)) throw SMETException("File '"+filename+"' does not exists", AT); //prevent invalid filenames
	errno = 0;
	std::ifstream fin(filename.c_str(), ios::in|ios::binary); //ascii mode messes up pointer code on windows (automatic eol translation)
	if (fin.fail()) {
		std::ostringstream ss;
		ss << "Error opening file \"" << filename << "\" for reading, possible reason: " << std::strerror(errno);
		throw SMETException(ss.str(), SMET_AT);
	}

	try {
		streampos fpointer = static_cast<streampos>(-1);
		if (timestamp_interval && timestamp_present){
			fpointer = indexer.getIndex(timestamp_start);
		} else if (julian_interval && julian_present){
			fpointer = indexer.getIndex(julian_start);
		}

		if (fpointer!=static_cast<streampos>(-1))
			fin.seekg(fpointer); //a previous pointer was found, jump to it
		else {
			if (data_start_fpointer!=static_cast<streampos>(-1))
				fin.seekg(data_start_fpointer); //nothing was found, jump to data start position in the file
			else { //the data section was itself empty (not even containg \n)
				cleanup(fin);
				return;
			}
		}

		if (fin.fail() || fin.bad())
			fin.seekg(data_start_fpointer);

		if (isAscii)
			read_data_ascii(fin, vec_timestamp, vec_data);
		else
			throw SMETException("Binary SMET file \""+filename+"\" has no field timestamp, only julian date", SMET_AT);
	} catch(...) {
		cleanup(fin);
		throw;
	}

	cleanup(fin);
}

void SMETReader::read(std::vector<double>& vec_data)
{
	if (timestamp_present)
		throw SMETException("Requesting not to read timestamp when there is one present in \""+filename+"\"", SMET_AT);

	if (!SMETCommon::fileExists(filename)) throw SMETException("File '"+filename+"' does not exists", AT); //prevent invalid filenames
	errno = 0;
	const ios_base::openmode mode = ios::in|ios::binary; //read as binary to avoid eol mess
	std::ifstream fin(filename.c_str(), mode);
	if (fin.fail()) {
		std::ostringstream ss;
		ss << "Error opening file \"" << filename << "\" for reading, possible reason: " << std::strerror(errno);
		throw SMETException(ss.str(), SMET_AT);
	}

	try {
		streampos fpointer = static_cast<streampos>(-1);
		if (julian_interval && julian_present){
			fpointer = indexer.getIndex(julian_start);
		}
		if (fpointer!=static_cast<streampos>(-1))
			fin.seekg(fpointer); //a previous pointer was found, jump to it
		else {
			if (data_start_fpointer!=static_cast<streampos>(-1))
				fin.seekg(data_start_fpointer); //nothing was found, jump to data start position in the file
			else { //the data section was itself empty (not even containg \n)
				cleanup(fin);
				return;
			}
		}

		if (fin.fail() || fin.bad())
			fin.seekg(data_start_fpointer);

		if (isAscii) {
			std::vector<std::string> tmp_vec;
			read_data_ascii(fin, tmp_vec, vec_data);
		} else {
			read_data_binary(fin, vec_data);
		}
	} catch(...) {
		cleanup(fin);
		throw;
	}

	cleanup(fin);
}

std::string SMETReader::getLastTimestamp() const
{
	if (!SMETCommon::fileExists(filename)) throw SMETException("File '"+filename+"' does not exists", AT); //prevent invalid filenames
	errno = 0;
	std::ifstream fin(filename.c_str(), ios::in|ios::binary); //ascii mode messes up pointer code on windows (automatic eol translation)
	if (fin.fail()) {
		std::ostringstream ss;
		ss << "Error opening file \"" << filename << "\" for reading, possible reason: " << std::strerror(errno);
		throw SMETException(ss.str(), SMET_AT);
	}

	if (!fin.is_open())
		throw SMETException("Please open the file before attempting to read its last line!", AT);

	fin.seekg(0, std::ifstream::end);
	const std::streamoff length = fin.tellg();
	const int buff_size = 1024; //a better approach would be to loop over this buff_size in order to accomodate very different line lengths
	if (buff_size<length)
		fin.seekg(0-buff_size, fin.end);
	else
		return "";

	const size_t nr_of_data_fields = (timestamp_present)? nr_of_fields+1 : nr_of_fields;
	std::string line, timestamp;
	vector<string> tmp_vec;

	getline(fin, line, eoln); //scrap the first line since it will be a partial line
	while (!fin.eof()){ //Read until end of file or break
		line.clear();
		getline(fin, line, eoln);
		SMETCommon::stripComments(line);
		SMETCommon::trim(line);
		if (line.empty()) continue; //Pure comment lines and empty lines are ignored

		if (SMETCommon::readLineToVec(line, tmp_vec) == nr_of_data_fields) {
			timestamp = tmp_vec[timestamp_field];
		}
	}

	fin.close();
	return timestamp;
}

//copy fin to fout until encountering date_stop or the end of the file
void SMETReader::copy_file_data(const std::string& date_stop, std::ifstream& fin, std::ofstream& fout) const
{
	//either ascii or binary
	if (!isAscii) throw SMETException("Truncating binary SMET files is currently not supported", AT);
	if (!timestamp_present) throw SMETException("Truncating SMET files without timestamps is currently not supported", AT);

	const size_t nr_of_data_fields = (timestamp_present)? nr_of_fields+1 : nr_of_fields;
	const size_t date_stop_len = date_stop.length();
	std::string line;
	vector<string> tmp_vec;

	while (!fin.eof()){ //Read until end of file or break
		line.clear();
		getline(fin, line, eoln); //read complete signature line
		SMETCommon::stripComments(line);
		SMETCommon::trim(line);
		if (line.empty()) continue; //Pure comment lines and empty lines are ignored

		if (SMETCommon::readLineToVec(line, tmp_vec) == nr_of_data_fields) {
			const string& current_timestamp = tmp_vec[timestamp_field];
			const size_t cmp_len = min(date_stop_len, current_timestamp.length());

			if (current_timestamp.compare(0, cmp_len, date_stop) >= 0) break;
		} else {
			std::ostringstream ss;
			ss << "File \'" << filename << "\' declares " << nr_of_data_fields << " columns ";
			ss << "but this does not match the following line:\n" << line << "\n";
			throw SMETException(ss.str(), SMET_AT);
		}

		fout << line << "\n";
	}
}

void SMETReader::read_data_ascii(std::ifstream& fin, std::vector<std::string>& vec_timestamp, std::vector<double>& vec_data)
{
	const size_t nr_of_data_fields = (timestamp_present)? nr_of_fields+1 : nr_of_fields;
	std::vector<std::string> tmp_vec;
	std::string line;
	size_t linenr = 0;
	streampos current_fpointer = static_cast<streampos>(-1);

	while (!fin.eof()){
		const std::streampos tmp_fpointer = fin.tellg();
		line.clear();
		getline(fin, line, eoln);
		linenr++;
		SMETCommon::stripComments(line);
		SMETCommon::trim(line);
		if (line.empty()) continue; //Pure comment lines and empty lines are ignored

		if (SMETCommon::readLineToVec(line, tmp_vec) == nr_of_data_fields){
			try {
				size_t shift = 0;
				if (julian_interval && julian_present){
					const double current_julian = SMETCommon::convert_to_double(tmp_vec[julian_field]);
					if ( (linenr % streampos_every_n_lines)==0 && (current_fpointer != static_cast<streampos>(-1)) )
						indexer.setIndex(current_julian, tmp_fpointer);
					if (current_julian < julian_start)
						continue; //skip lines that don't hold the dates we're interested in
					else if (current_julian > julian_end)
						break; //skip the rest of the file
				}

				if (timestamp_interval && timestamp_present){
					const string& current_timestamp = tmp_vec[timestamp_field];
					if ( (linenr % streampos_every_n_lines)==0 && (tmp_fpointer != static_cast<streampos>(-1)) )
						indexer.setIndex(current_timestamp, tmp_fpointer);
					if (current_timestamp < timestamp_start)
						continue; //skip lines that don't hold the dates we're interested in
					else if (current_timestamp > timestamp_end)
						break; //skip the rest of the file
				}

				for (size_t ii=0; ii<tmp_vec.size(); ii++){
					if (timestamp_present && (ii == timestamp_field)) {
						vec_timestamp.push_back(tmp_vec[ii]);
						shift = 1;
					} else {
						double tmp = SMETCommon::convert_to_double(tmp_vec[ii]);
						if ((mksa) && (tmp != nodata_value)){
							tmp *= vec_multiplier[ii-shift];
							tmp += vec_offset[ii-shift];
						}
						vec_data.push_back(tmp);
					}
				}
				current_fpointer = tmp_fpointer;
			} catch(SMETException&) {
				cerr << "Error reading file \"" << filename << "\" at line \"" << line << "\"" << endl;
				throw;
			}
		} else {
			std::ostringstream ss;
			ss << "File \'" << filename << "\' declares " << nr_of_data_fields << " columns ";
			ss << "but this does not match the following line:\n" << line << "\n";
			throw SMETException(ss.str(), SMET_AT);
		}
	}

	if (current_fpointer != static_cast<streampos>(-1)){
		if (timestamp_interval && timestamp_present)
			indexer.setIndex(timestamp_end, current_fpointer);
		else if (julian_interval && julian_present)
			indexer.setIndex(julian_end, current_fpointer);
	}
}

void SMETReader::read_data_binary(std::ifstream& fin, std::vector<double>& vec_data)
{
	size_t linenr = 0;
	streampos current_fpointer = static_cast<streampos>(-1);
	while (!fin.eof()){
		const streampos tmp_fpointer = fin.tellg();
		double julian = -1.0;
		for (size_t ii=0; ii<nr_of_fields; ii++){
			if (julian_present && (ii == julian_field)){
				fin.read(reinterpret_cast < char * > (&julian), sizeof(double));
				vec_data.push_back(julian);
			} else {
				float tmpval;
				fin.read(reinterpret_cast < char * > (&tmpval), sizeof(float));
				const double val = (double)tmpval;

				vec_data.push_back(val);
			}
			linenr++;

			if (mksa){
				double& tmp = vec_data.back();
				if (tmp != nodata_value){
					tmp *= vec_multiplier[ii];
					tmp += vec_offset[ii];
				}
			}
		}

		if (julian_present && julian_interval){
			if ( (linenr % streampos_every_n_lines)==0 && (tmp_fpointer != static_cast<streampos>(-1)) )
				indexer.setIndex(julian, tmp_fpointer);
			if ((julian < julian_start) || (julian > julian_end)){
				for (size_t ii=0; ii<nr_of_fields; ii++){
					vec_data.pop_back();
				}

				if (julian > julian_end) break; //skip the rest of the file
			} else {//we have a valid date
				current_fpointer = tmp_fpointer;
			}
		}

		char c;
		fin.read(&c, sizeof(char));
		if (c != '\n')
			throw SMETException("Corrupted data in section [DATA] of binary SMET file \""+filename+"\"", SMET_AT);
	}

	if (current_fpointer != static_cast<streampos>(-1)){
		if (julian_interval && julian_present)
			indexer.setIndex(julian_end, current_fpointer);
	}
}

double SMETReader::get_header_doublevalue(const std::string& key) const
{
	const map<string,string>::const_iterator it = header.find(key);
	if (it != header.end())
		return SMETCommon::convert_to_double(it->second);

	return nodata_value;
}

int SMETReader::get_header_intvalue(const std::string& key) const
{
	const map<string,string>::const_iterator it = header.find(key);
	if (it != header.end())
		return SMETCommon::convert_to_int(it->second);

	return (int)floor(nodata_value + 0.1);
}

std::string SMETReader::get_header_value(const std::string& key) const
{
	const map<string,string>::const_iterator it = header.find(key);
	if (it != header.end())
		return it->second;

	return std::string();
}

bool SMETReader::contains_timestamp() const
{
	return timestamp_present;
}

std::string SMETReader::get_filename() const
{
	return filename;
}

} //end namespace
