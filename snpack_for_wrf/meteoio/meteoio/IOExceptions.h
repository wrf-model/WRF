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
#ifndef IOEXCEPTIONS_H
#define IOEXCEPTIONS_H

#include <exception>
#include <string>
#include <stdlib.h>

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)
#define AT __FILE__ ":" TOSTRING(__LINE__)

namespace mio {

/**
 * @class IOException
 * @brief The basic exception class adjusted for the needs of SLF software
 *
 * @author Thomas Egger
 */


class IOException : public std::exception {
	public:
		IOException(const std::string& message="IOException occured", const std::string& position="");
		~IOException() throw() {}
		virtual const char* what() const throw();

	protected:
	#if defined(__linux) && !defined(ANDROID) && !defined(__CYGWIN__)
		std::string resolveSymbols(char *symbols, const unsigned int& ii, bool& found_main) const;
	#endif
		std::string msg, full_output;
};

/**
 * @class NotFoundException
 * @brief thrown when a there is an unsuccessful attempt to locate a file/server/...
 *
 * @author Thomas Egger
 */
class NotFoundException : public IOException {
	public:
		NotFoundException(const std::string& filename="",
		                      const std::string& position="") : IOException("NotFound: " + filename, position){}
};

/**
 * @class AccessException
 * @brief thrown when a there are insufficient rights to access a file/server/... in a certain way (e.g. read, write)
 *
 * @author Thomas Egger
 */
class AccessException : public IOException {
	public:
		AccessException(const std::string& filename="",
		                    const std::string& position="") : IOException("InvalidAccess: " + filename, position){}
};

/**
 * @class InvalidNameException
 * @brief thrown when a given filename/servername/... is not valid (e.g. "..", "." or empty)
 *
 * @author Thomas Egger
 */
class InvalidNameException : public IOException {
	public:
		InvalidNameException(const std::string& filename="",
		                         const std::string& position="") : IOException("InvalidName: " + filename, position){}
};

/**
 * @class InvalidFormatException
 * @brief thrown when parsed data does not reflect an expected format (e.g. premature end of a line, file)
 *
 * @author Thomas Egger
 */
class InvalidFormatException : public IOException {
	public:
		InvalidFormatException(const std::string& message="",
		                       const std::string& position="") : IOException("InvalidFormat: " + message, position){}
};

/**
 * @class IndexOutOfBoundsException
 * @brief thrown when an index is out of bounds
 *
 * @author Thomas Egger
 */
class IndexOutOfBoundsException : public IOException {
	public:
		IndexOutOfBoundsException(const std::string& message="",
		                          const std::string& position="") : IOException("IndexOutOfBounds: " + message, position){}
};

/**
 * @class ConversionFailedException
 * @brief thrown when an unsuccessful attempt to convert data types/classes is made (e.g. attempt to convert a literal into a number)
 *
 * @author Thomas Egger
 */
class ConversionFailedException : public IOException {
	public:
		ConversionFailedException(const std::string& message="",
		                          const std::string& position="") : IOException("ConversionFailed: " + message, position){}
};

/**
 * @class InvalidArgumentException
 * @brief thrown when encountered an unexpected function's argument (e.g. bad index, bad or missing parameter name, etc.)
 *
 * @author Florian Hof
 */
class InvalidArgumentException : public IOException {
	public:
		InvalidArgumentException(const std::string& message="",
		                         const std::string& position="") : IOException("InvalidArgument: " + message, position){}
};

/**
 * @class UnknownValueException
 * @brief thrown when encountered an unexpected value (e.g. unknown name or key)
 *
 * @author Florian Hof
 */
class UnknownValueException : public IOException {
	public:
		UnknownValueException(const std::string& message="",
		                      const std::string& position="") : IOException("UnknownValue: " + message, position){}
};

/**
 * @class NoDataException
 * @brief thrown when no data is available
 *
 * @author Florian Hof
 */
class NoDataException : public IOException
{
	public:
		NoDataException(const std::string& message="",
		                         const std::string& position="") : IOException("NoData: " + message, position){}
};
} //end namespace

#endif
