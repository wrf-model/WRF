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
#ifndef LIBSMET_H
#define LIBSMET_H

#include <meteoio/FileUtils.h>

#include <string>
#include <iostream>
#include <vector>
#include <set>
#include <map>

#define SMET_STRINGIFY(x) #x
#define SMET_TOSTRING(x) SMET_STRINGIFY(x)
#define SMET_AT __FILE__ ":" SMET_TOSTRING(__LINE__)

namespace smet {

enum SMETType {ASCII, BINARY};
enum LocationType {WGS84, EPSG};

/**
 * @class SMETException
 * @brief A basic exception class adjusted for the needs of the SMET library
 * @author Thomas Egger
 */
class SMETException : public std::exception {
	public:
		SMETException(const std::string& message="SMETException occured", const std::string& position="");
		~SMETException() throw();
		const char* what() const throw();

	protected:
		std::string msg;
};

/**
 * @class SMETCommon
 * @brief A static class to provide basic operations and variables for the libsmet library
 * @author Thomas Egger
 */
class SMETCommon {
	public:
		static bool validFileAndPath(const std::string& filename);
		static void copy_file(const std::string& src, const std::string& dest);
		static bool fileExists(const std::string& filename);
		static double convert_to_double(const std::string& in_string);
		static int convert_to_int(const std::string& in_string);
		static void stripComments(std::string& str);
		static char getEoln(std::istream& fin);
		static void trim(std::string& str);
		static void toUpper(std::string& str);
		static bool readKeyValuePair(const std::string& in_line, const std::string& delimiter,
		                             std::map<std::string,std::string>& out_map);
		static size_t readLineToVec(const std::string& line_in, std::vector<std::string>& vec_string);
		static bool is_decimal(const std::string& value);

	public:
		static std::set<std::string> all_optional_header_keys;
		static std::set<std::string> all_decimal_header_values;
		static std::set<std::string> all_mandatory_header_keys;
		static const char* smet_version;

	private:
		static const bool __init;     ///<helper variable to enable the init of static collection data
		static bool initStaticData(); ///<initialize the static collections
};

/**
 * @class SMETWriter
 * @brief The SMETWriter class that enables to write a SMET formatted file.
 *        The user constructs a SMETWriter class and fills in the header values
 *        Finally the user may call write(...) and pass the data to be written
 *
 * @author Thomas Egger
 * @date   2011-07-14
 */
class SMETWriter {
	public:
		/**
		 * @brief The constructor allows to set the filename, the type and whether the file should be gzipped
		 * @param[in] in_filename The filename of the SMET file to be written
		 * @param[in] in_type  The type of the SMET file, i.e. smet::ASCII or smet::BINARY (default: ASCII)
		 */
		SMETWriter(const std::string& in_filename, const SMETType& in_type=ASCII);
		
		/**
		 * @brief The constructor allows to append data to an existing file
		 * @param[in] in_filename The filename of the SMET file to be written
		 * @param[in] in_fields  The fields that should be written in order to check that we write the
		 * same data at the same place.
		 * @param[in] in_nodata Value representing nodata
		 */
		SMETWriter(const std::string& in_filename, const std::string& in_fields, const double& in_nodata);
		
		/**
		 * @brief Set a key, value pair in the SMET header (both strings)
		 * @param[in] key A string key to set in the header (overwritten if already present)
		 * @param[in] value A string value associated with the key
		 */
		void set_header_value(const std::string& key, const std::string& value);

		/**
		 * @brief Set a double value for a string key in a SMET header
		 * @param[in] key A string key to set in the header (overwritten if already present)
		 * @param[in] value A double value to be converted into a string and stored in the header
		 */
		void set_header_value(const std::string& key, const double& value);

		/**
		 * @brief Write a SMET file, providing ASCII ISO formatted timestamps and data
		 * @param[in] vec_timestamp A vector with one ASCII date/time combined string for each line
		 * @param[in] data All the data to be written sequentially into the columns, the data
		 *            is aligned sequentially, not per line; Total size of the vector:
		 *            Total size of the vector: vec_timestamp.size() * nr_of_fields
		 *            (timestamp is not counted as field)
		 */
		void write(const std::vector<std::string>& vec_timestamp, const std::vector<double>& data);

		/**
		 * @brief Write a SMET file, providing a vector of doubles
		 * @param[in] data All the data to be written sequentially into the columns, the data
		 *            is aligned sequentially, not per line;
		 */
		void write(const std::vector<double>& data);

		/**
		 * @brief Set precision for each field (except timestamp), otherwise a default
		 *        precision of 3 is used for each column
		 * @param[in] vec_precision Set the precision for every column to be written
		 *            (timestamp is not counted if present)
		 */
		void set_precision(const std::vector<int>& vec_precision);

		/**
		 * @brief Set width for each field (except timestamp), otherwise a default
		 *        width of 8 is used for each column
		 * @param[in] vec_width Set the width for every column to be written
		 *            (timestamp is not counted if present)
		 */
		void set_width(const std::vector<int>& vec_width);

		const std::string toString() const;
		
	private:
		void setAppendMode(std::vector<std::string> vecFields);
		void print_if_exists(const std::string& header_field, std::ofstream& fout) const;
		void write_header(std::ofstream& fout); //only writes when all necessary header values are set
		void write_data_line_ascii(const std::string& timestamp, const std::vector<double>& data, std::ofstream& fout);
		void write_data_line_binary(const std::vector<double>& data, std::ofstream& fout);
		bool check_fields(const std::string& key, const std::string& value);
		void check_formatting();
		bool valid_header_pair(const std::string& key, const std::string& value);
		bool valid_header();

		std::vector<std::string> other_header_keys; //this vector is used to preserve the sequence of header keys
		std::vector<int> ascii_precision, ascii_width;
		std::map< std::string, std::string > header;
		std::set<std::string> mandatory_header_keys;

		std::string filename;
		std::string nodata_string;
		SMETType smet_type;
		double nodata_value;
		size_t nr_of_fields, julian_field, timestamp_field;
		char location_wgs84, location_epsg;
		bool location_in_header, location_in_data_wgs84, location_in_data_epsg;
		bool timestamp_present, julian_present;
		bool file_is_binary, append_mode, append_possible;
};

/**
 * @class SMETReader
 * @brief The SMETReader class enables to read a SMET formatted file.
 *        Data and header info can be extracted through this class.
 *
 * @author Thomas Egger
 * @date   2011-07-18
 */
class SMETReader {
	public:
		friend class SMETWriter; //so the writer can call the reader for handling append mode
		
		/**
		 * @brief A constructor that will immediately parse the header of the SMET file
		 * @param[in] in_fname The filename of the SMET file
		 */
		SMETReader(const std::string& in_fname);
		~SMETReader(){}

		/**
		 * @brief Read the data in a SMET file for a given interval of time
		 *        if no timestamp is present in the file, the whole file is read
		 * @param[in] timestamp_start ISO formatted string, beginning of interval (inclusive)
		 * @param[in] timestamp_end ISO formatted string, end of interval (inclusive)
		 * @param[out] vec_timestamp A vector of string to hold the timestamp of each line
		 * @param[out] vec_data A vector of double holding all double values of all lines sequentially
		 */
		void read(const std::string& timestamp_start, const std::string& timestamp_end,
		          std::vector<std::string>& vec_timestamp, std::vector<double>& vec_data);

		/**
		 * @brief Read the data in a SMET file for a given interval of time
		 *        if no julian field is present in the file, the whole file is read
		 * @param[in] julian_start beginning of interval (inclusive), julian day
		 * @param[in] julian_end end of interval (inclusive), julian day
		 * @param[out] vec_data A vector of double holding all double values of all lines sequentially
		 */
		void read(const double& julian_start, const double& julian_end, std::vector<double>& vec_data);

		/**
		 * @brief Read all the data in a SMET file, if a timestamp is present
		 * @param[out] vec_timestamp A vector of string to hold the timestamp of each line
		 * @param[out] vec_data A vector of double holding all double values of all lines sequentially
		 */
		void read(std::vector<std::string>& vec_timestamp, std::vector<double>& vec_data);

		/**
		 * @brief Read all the data in a SMET file, if no timestamp is present
		 * @param[out] vec_data A vector of double holding all double values of all lines sequentially
		 */
		void read(std::vector<double>& vec_data);

		/**
		 * @brief Get a string value for a header key in a SMET file
		 * @param[in] key A key in the header section of a SMET file
		 * @return The value for the key, if the key is not present return ""
		 */
		std::string get_header_value(const std::string& key) const;

		/**
		 * @brief Get a double value for a header key in a SMET file
		 * @param[in] key A key in the header section of a SMET file
		 * @return The value for the key, if the key is not present return nodata
		 */
		double get_header_doublevalue(const std::string& key) const;

		/**
		 * @brief Get an int value for a header key in a SMET file
		 * @param[in] key A key in the header section of a SMET file
		 * @return The value for the key, if the key is not present return (int)nodata
		 */
		int get_header_intvalue(const std::string& key) const;

		/**
		 * @brief Check whether timestamp is a part of the fields
		 * @return true if timestamp is a column in the SMET file, false otherwise
		 */
		bool contains_timestamp() const;

		/**
		 * @brief Get a name for a certain column in the SMET file
		 * @param[in] nr_of_field Column index (the column 'timestamp' is not counted)
		 * @return The string name of the column
		 */
		std::string get_field_name(const size_t& nr_of_field);

		/**
		 * @brief Check whether location information is written in the header
		 * @param[in] type Either smet::WGS84 or smet::EPSG
		 * @return true if type of location information is in header, false otherwise
		 */
		bool location_in_header(const LocationType& type) const;

		/**
		 * @brief Check whether location information is written in the data section
		 * @param[in] type Either smet::WGS84 or smet::EPSG
		 * @return true if type of location information is in data section, false otherwise
		 */
		bool location_in_data(const LocationType& type) const;

		/**
		 * @brief Get number of fields (=columns) in SMET file (timestamp excluded)
		 * @return A size_t value representing number of double valued columns
		 */
		size_t get_nr_of_fields() const;

		/**
		 * @brief Get the unit conversion (offset and multiplier) that are used for this SMET object
		 *        If the fields units_offset or units_multiplier are present in the header
		 *        they are returned, otherwise the default conversion vectors for the offset
		 *        (consisting of zeros) and for the multipliers (consisting of ones) are returned
		 * @param[out] offset A vector of doubles representing the offset for each column
		 * @param[out] multiplier A vector of doubles representing the multiplier for each column
		 */
		void get_units_conversion(std::vector<double>& offset, std::vector<double>& multiplier) const;

		/**
		 * @brief Set whether the values returned should be converted according to
		 *        unit_offset and multiplier or whether the user should get the raw data returned
		 * @param[in] in_mksa True if the user wants MKSA values returned, false otherwise (default)
		 */
		void convert_to_MKSA(const bool& in_mksa);

		/**
		 * @brief Retrieve the filename that this reader operates upon
		 * @return a std::string representing the filename
		 */
		std::string get_filename() const;
		
	private:
		void truncate_file(const std::string& date_stop) const;
		void copy_file_header(std::ifstream& fin, std::ofstream& fout) const;
		void copy_file_data(const std::string& date_stop, std::ifstream& fin, std::ofstream& fout) const;
		std::string getLastTimestamp() const;
		void read_data_ascii(std::ifstream& fin, std::vector<std::string>& vec_timestamp, std::vector<double>& vec_data);
		void read_data_binary(std::ifstream& fin, std::vector<double>& vec_data);
		void cleanup(std::ifstream& fin) throw();
		void checkSignature(const std::vector<std::string>& vecSignature, bool& o_isAscii);
		void read_header(std::ifstream& fin);
		void process_header();

		std::streampos data_start_fpointer;

		std::vector<double> vec_offset;              //an offset for every column, except timestamp
		std::vector<double> vec_multiplier;          //a multiplier for every column, except timestamp
		std::vector<std::string> vec_fieldnames;     //holds the column names, except for timestamp column
		std::map< std::string, std::string > header; //holds the header
		mio::FileUtils::FileIndexer indexer; //in order to save file pointers

		std::string filename;
		std::string timestamp_start, timestamp_end; //the beginning and end date of the current timestamp_interval
		double nodata_value; //The nodata value as seen in the header section of the SMET file
		double julian_start, julian_end; //the beginning and end date of the current julian_interval
		static const size_t streampos_every_n_lines; //save current stream pos every n lines of data
		size_t nr_of_fields; //is always the number of fields minus the timestamp field, if present
		size_t timestamp_field, julian_field; //index of the timestamp and julian column, if present
		char location_wgs84, location_epsg, location_data_wgs84, location_data_epsg;
		char eoln; //end of line character for this file
		bool timestamp_present, julian_present;
		bool isAscii; //true if the file is in SMET ASCII format, false if it is in binary format
		bool mksa; //true if MKSA converted values have to be returned
		bool timestamp_interval, julian_interval; //true if data shall only be read for a time interval
};

}

#endif
