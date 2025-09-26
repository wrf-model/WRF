/*  Copyright 2014 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef FILEUTILS_H
#define FILEUTILS_H

#include <sstream>
#include <string>
#include <map>
#include <vector>
#include <list>

#include <meteoio/dataClasses/Date.h>

namespace mio {
namespace FileUtils {

	/**
	 * @brief Copies a files from one location to another
	 * @author Thomas Egger
	 * @param src  The filename of the file to be copied
	 * @param dest The filename of the file to copy to (will be created or overwritten)
	 */
	void copy_file(const std::string& src, const std::string& dest);
	
	/**
	* @brief Build a list of file in a given directory.
	* @details
	* The matching is very primitive: it only looks for the substring "pattern" in the file names.
	* If this substrings exists, the file matches. In the case of recursive search, the filenames will be
	* prefixed by their relative path based on the provided path.
	* @note Hidden files/directories are excluded, links are followed
	* @param path directory containing the files
	* @param dirlist list of matching file names
	* @param pattern optional pattern that must be part of the file names
	* @param isRecursive should the search recurse through sub-directories? (default: false)
	*/
	void readDirectory(const std::string& path, std::list<std::string>& dirlist, const std::string& pattern="", const bool& isRecursive=false);

	std::list<std::string> readDirectory(const std::string& path, const std::string& pattern="", const bool& isRecursive=false);

	bool validFileAndPath(const std::string& filename);

	bool fileExists(const std::string& filename);

	/**
	* @brief Replace "\" by "/" in a string so that a path string is cross plateform, optionally resolve
	* links, convert relative paths to absolute paths, etc
	* @param in_path the path string to cleanup
	* @param resolve resolve links, convert relative paths, etc? (default=false)
	*/
	std::string cleanPath(std::string in_path, const bool& resolve=false);

	/**
	* @brief returns the extension part of a given filename.
	* @details
	* The extension is defined as all the non-whitespace characters after the last '.'
	* in the filename.
	* @param filename filename to extract the extension from
	* @return extension
	*/
	std::string getExtension(const std::string& filename);

	/**
	* @brief remove the extension part of a given filename.
	* @details
	* The extension is defined as all the non-whitespace characters after the last '.'
	* in the filename.
	* @param filename filename to remove the extension from
	* @return filename without extension (the '.' is also removed)
	*/
	std::string removeExtension(const std::string& filename);

	/**
	* @brief returns the current working directory.
	* @return path
	*/
	std::string getCWD();

	/**
	* @brief returns the path preceeding a given filename.
	* @param filename filename to extract the path from
	* @param resolve resolve links, convert relative paths, etc? (default=false)
	* @return path
	*/
	std::string getPath(const std::string& filename, const bool& resolve=false);

	/**
	* @brief checks if a path is an absolute path
	* @param in_path string containing the path to check
	* @return true if the given string represents an absolute path
	*/
	bool isAbsolutePath(const std::string& in_path);

	/**
	* @brief extract the file name from a path+filename string.
	* @param path path to extract the true filename from
	* @return filename
	*/
	std::string getFilename(const std::string& path);

	char getEoln(std::istream& fin);

	void skipLines(std::istream& fin, const size_t& nbLines, const char& eoln='\n');

	std::map<std::string,std::string> readKeyValueHeader(std::istream& fin,
	                        const size_t& linecount=1,
	                        const std::string& delimiter="=", const bool& keep_case=false);

	/**
	* @class file_indexer
	* @brief helps building an index of stream positions
	* to quickly jump closer to the proper position in a file
	*
	* @ingroup plugins
	* @author Mathias Bavay
	* @date   2012-11-30
	*/
	class FileIndexer {
		public:
			FileIndexer() : vecIndex() {}

			/**
			* @brief Add a new position to the index
			* @param[in] i_date date of the new position
			* @param[in] i_pos streampos position
			*/
			void setIndex(const Date& i_date, const std::streampos& i_pos);
			void setIndex(const std::string& i_date, const std::streampos& i_pos);
			void setIndex(const double& i_date, const std::streampos& i_pos);

			/**
			* @brief Get the file position suitable for a given date
			* @param[in] i_date date for which a position is requested
			* @return closest streampos position before the requested date,
			* -1 if nothing could be found (empty index)
			*/
			std::streampos getIndex(const Date& i_date) const;
			std::streampos getIndex(const std::string& i_date) const;
			std::streampos getIndex(const double& i_date) const;

			const std::string toString() const;

		private:
			struct file_index {
				file_index(const Date& i_date, const std::streampos& i_pos) : date(i_date), pos(i_pos) {}
				bool operator<(const file_index& a) const {
					return date < a.date;
				}
				bool operator>(const file_index& a) const {
					return date > a.date;
				}
				Date date;
				std::streampos pos;
			};
			size_t binarySearch(const Date& soughtdate) const;

			std::vector< struct file_index > vecIndex;
	};

} //end namespace FileUtils
} //end namespace mio

#endif
