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
#ifndef CONFIGREADER_H
#define CONFIGREADER_H

#include <meteoio/IOUtils.h>
#include <meteoio/IOExceptions.h>

#include <string>
#include <sstream>
#include <map>
#include <vector>
#include <typeinfo> //for typeid()

namespace mio {

/**
 * @class Config
 * @brief A class that reads a key/value file. These files (typically named *.ini) follow the INI file format standard (see http://en.wikipedia.org/wiki/INI_file) and have the following structure:
 * - they consist of 0 or more explicitly indicated sections, which start with a sectionname enclosed in square brackets
 *   e.g. [General] or [Filter]
 * - within each section there are 0 or more key value pairs defined: KEY = VALUE
 * - in this implementation each key is unique within its section
 * - lines that start with a semicolon ';' or a hash sign '#' are ignored (regarded as comments)
 * - empty lines are ignored
 * - if there is no section name given in a file, the default section called "GENERAL" is assumed
 * - a VALUE for a KEY can consist of multiple whitespace separated values (e.g. MYNUMBERS = 17.77 -18.55 8888 99.99)
 * - special values: there is a special syntax to refer to environment variables, to other keys or to evaluate arithmetic expressions:
 *       - environment variables are called by using the following syntax: ${env:my_env_var};
 *       - refering to another key (it only needs to be defined at some point in the ini file, even in an included file is enough): ${other_key} or ${section::other_key} (make sure to prefix the key with its section if it refers to another section!)
 *       - evaluating an arithmetic expression: ${{arithm. expression}}
 * 
 * @note The arithemic expressions are evaluated thanks to the <A HREF="https://codeplea.com/tinyexpr">tinyexpr</A> math library (under the 
 * <A HREF="https://opensource.org/licenses/Zlib">zlib license</A>) and can use standard operators (including "^"), 
 * standard functions (such as "sin", "sqrt", "ln", "log", "exp", "floor"...) as well as the "pi" and "e" constants.
 * 
 * @section config_import Imports
 * It is possible to import another ini file, by specifying as many of the keys listed below as necessary.
 *   Please note that there is a check to prevent circular dependencies.
 *      - IMPORT_BEFORE = {file and path to import}. This must take place before any non-import
 *        key or section header. This imports the specified file before processing the current file, allowing
 *        to overwrite the imported parameters in the current configuration file.
 *      - IMPORT_AFTER = {file and path to import}. This can occur anywhere and imports the specified file
 *        after processing the current file, allowing to overwrite the local parameters by the imported parameters.
 *
 * @section config_examples Exemples
 * @code
 * [Input]					; this defines a section
 * InputFile = ./input/myfile.dat		; this defines a key "InputFile" with the associated value "./input/myfile.dat"
 * #oldInput = ./test/test.dat		; this is commented out
 * smart_read = false			; this defines the boolean key "smart_read"
 * fast_read = T				; this defines another boolean key, "fast_read"
 *
 * user = ${env:LOGNAME}			; this uses the value of the environment variable "LOGNAME" for the key "user"
 * output_log = ${env:LOGNAME}_output.log	; we can even concatenate environment variables with other elements
 *
 * ConfigBackup = ${Input::user}_${smart_read}.bak	; using other keys to build a value (the section reference can be omitted within the same section)
 * Target_rate = ${{24*3600}}		; arithmetic expression that will be evaluated when reading the key
 * @endcode
 */

class ConfigProxy;

class Config {
	public:
		/**
		 * @brief Empty constructor. The user MUST later one fill the internal key/value map object
		 */
		Config();

		//virtual ~Config() {}

		/**
		 * @brief Main constructor. The file is parsed and a key/value map object is internally created
		 * @param[in] filename_in string representing the absolute filename of the key/value file
		 */
		Config(const std::string& filename_in);

		/**
		 * @brief Write the Config object to a file
		 * @param filename The filename including the path, e.g. "/tmp/test.ini"
		 */
		void write(const std::string& filename) const;

		/**
		 * @brief Add the content of a file to the internal key/value map object
		 * @param[in] filename_in string representing the absolute filename of the key/value file
		 */
		void addFile(const std::string& filename_in);

		/**
		 * @brief Add a specific key/value pair to the internal key/value map object.
		 *        key and section are case insensitive
		 * @param[in] key string representing the key to be added
		 * @param[in] section std::string representing a section name; the key has to be part of this section
		 * @param[in] value string representing the matching value to be added
		 */
		void addKey(std::string key, std::string section, const std::string& value);

		/**
		 * @brief Delete a specific key/value pair from the internal map object, key/section are case insensitive
		 * @param[in] key string representing the key to be added
		 * @param[in] section std::string representing a section name; the key has to be part of this section
		*/
		void deleteKey(std::string key, std::string section);

		/**
		 * @brief Delete keys matching a specific pattern from the internal map object, key/section are case insensitive
		 * @param[in] keymatch A string representing the beginning of a key to search for
		 * @param[in] section A string defining which section to search through (default: GENERAL)
		 * @param[in] anywhere Match substring anywhere in the key string (default=false, ie at the beginning only)
		 * @code
		 *  Config cfg("io.ini");
		 *  cfg.deleteKeys("STATION", "Input");
		 * @endcode
		*/
		void deleteKeys(std::string keymatch, std::string section, const bool& anywhere=false);

		/**
		 * @brief Returns the filename that the Config object was constructed with.
		 * @return The absolute filename of the key/value file.
		 */
		std::string getSourceName() const {return sourcename;}

		/**
		 * @brief Returns the directory where the root configuration file is (needed to resolv relative paths).
		 * @return The absolute path to the root config file (resolved for symlinks, relative paths, etc).
		 */
		std::string getConfigRootDir() const {return configRootDir;}

		/**
		 * @brief Return if a given key exists in a given section
		 * @param[in] key string representing the key to be searched
		 * @param[in] section std::string representing a section name; the key has to be part of this section
		 * @return true if the key exists
		 */
		bool keyExists(std::string key, std::string section) const;

		/**
		 * @brief Return if a given section exists in the Config object
		 * @param[in] section std::string representing a section name
		 * @return true if the section exists
		 */
		bool sectionExists(std::string section) const;

		/**
		 * @brief Print the content of the Config object (useful for debugging)
		 * The Config is bound by "<Config>" and "</Config>" on separate lines
		 */
		const std::string toString() const;

		friend std::ostream& operator<<(std::ostream& os, const Config& cfg);
		friend std::istream& operator>>(std::istream& is, Config& cfg);

		template <typename T> std::vector<T> getValue(const std::string& key, std::string& section,
		                                              const IOUtils::ThrowOptions& opt=IOUtils::dothrow) const
		{
			std::vector<T> tmp;
			getValue(key, section, tmp, opt);
			return tmp;
		}

		/**
		 * @brief Template function to retrieve a vector of values of class T for a certain key
		 * @code
		 * algorithms = lsm linregres idw_kriging\n
		 * MYNUMBERS = 17.87 19.89 -19.89 +7777.007
		 * @endcode
		 * @param[in] key std::string representing a KEY in the key/value file (default section "GENERAL" is assumed)
		 * @param[out] vecT a variable of class vector<T> into which the values for the corresponding key are saved
		 * @param[in] opt indicating whether an exception should be raised, when key is not present
		 */
		template <typename T> void getValue(const std::string& key,
		                                    std::vector<T>& vecT,
		                                    const IOUtils::ThrowOptions& opt=IOUtils::dothrow) const
		{
			getValue(key, "GENERAL", vecT, opt);
		}

		/**
		 * @brief Template function to retrieve a vector of values of class T for a certain key
		 * @code
		 * algorithms = lsm linregres idw_kriging\n
		 * MYNUMBERS = 17.87 19.89 -19.89 +7777.007
		 * @endcode
		 * @param[in] key std::string representing a KEY in the key/value file
		 * @param[in] section std::string representing a section name; the key has to be part of this section
		 * @param[out] vecT a variable of class vector<T> into which the values for the corresponding key are saved
		 * @param[in] opt indicating whether an exception should be raised, when key is not present
		 */
		template <typename T> void getValue(std::string key, std::string section,
		                                    std::vector<T>& vecT, const IOUtils::ThrowOptions& opt=IOUtils::dothrow) const
		{
			vecT.clear();
			IOUtils::toUpper(key);
			IOUtils::toUpper(section);

			try {
				IOUtils::getValueForKey<T>(properties, section + "::" + key, vecT, opt);
			} catch(const std::exception&){
				throw UnknownValueException("[E] Error in "+sourcename+": no value for key "+section+"::"+key, AT);
			}
		}

		/**
		 * @brief A function that allows to retrieve a value for a key as return parameter (vectors of values too). 
		 * @details If the key is not found, an exception is thrown.
		 * @param[in] key std::string representing a KEY in the key/value file
		 * @param[in] section std::string representing a section name; the key has to be part of this section
		 * @return A value of type T
		 *
		 * Example Usage:
		 * @code
		 * Config cfg("io.ini");
		 * vector<int> = cfg.get("DEPTHS", "INPUT");
		 * string mystr = cfg.get("PATH", "OUTPUT");
		 * @endcode
		 */
		const ConfigProxy get(const std::string& key, const std::string& section) const;
		
		/**
		 * @brief A function that allows to retrieve a value for a key as return parameter (vectors of values too). 
		 * @details If the key is not found, the provided default value is returned.
		 * @param[in] key std::string representing a KEY in the key/value file
		 * @param[in] section std::string representing a section name; the key has to be part of this section
		 * @param[in] dflt default value, if the key is not found
		 * @return A value of type T
		 *
		 * Example Usage:
		 * @code
		 * Config cfg("io.ini");
		 * const double factor = cfg.get("factor", "Input", 1.);
		 * @endcode
		 */
		template <typename T> T get(const std::string& key, const std::string& section, const T& dflt) const;
		
		/**
		 * @brief A function that allows to retrieve a value for a key as return parameter (vectors of values too). 
		 * @details If the key is not found, the provided default value is returned.
		 * @param[in] key std::string representing a KEY in the key/value file
		 * @param[in] section std::string representing a section name; the key has to be part of this section
		 * @param[in] dflt default value, if the key is not found
		 * @return A value of type T
		 *
		 * Example Usage:
		 * @code
		 * Config cfg("io.ini");
		 * const std::string factor = cfg.get("model", "Input", "default");
		 * @endcode
		 * @note this is a specialized version of the template method, since strings are tricky: they can be initialized 
		 * with "" but this needs casting (since this is either a char or a char[]), therefore template argument deduction would fail.
		 */
		std::string get(const std::string& key, const std::string& section, const std::string& dflt) const;
		std::string get(const std::string& key, const std::string& section, const char dflt[]) const;
		bool get(const std::string& key, const std::string& section, const bool& dflt) const;


		/**
		 * @brief Template function to retrieve a value of class T for a certain key
		 * @param[in] key std::string representing a KEY in the key/value file (default section "GENERAL" is assumed)
		 * @param[out] t a variable of class T into which the value for the corresponding key is saved (e.g. double, int, std::string)
		 * @param[in] opt indicating whether an exception should be raised, when key is not present
		 */
		template <typename T> void getValue(const std::string& key, T& t, const IOUtils::ThrowOptions& opt=IOUtils::dothrow) const
		{
			getValue(key, "GENERAL", t, opt);
		}

		/**
		 * @brief Template function to retrieve a value of class T for a certain key
		 * @param[in] key std::string representing a KEY in the key/value file
		 * @param[in] section std::string representing a section name; the key has to be part of this section
		 * @param[out] t a variable of class T into which the value for the corresponding key is saved (e.g. double, int, std::string)
		 * @param[in] opt indicating whether an exception should be raised, when key is not present
		 */
		template <typename T> void getValue(std::string key, std::string section, T& t,
                                              const IOUtils::ThrowOptions& opt=IOUtils::dothrow) const
		{
			IOUtils::toUpper(key);
			IOUtils::toUpper(section);

			try {
				IOUtils::getValueForKey<T>(properties, section + "::" + key, t, opt);
			} catch(const std::exception&){
				throw UnknownValueException("[E] Error in "+sourcename+": no value for key "+section+"::"+key, AT);
			}
		}
		
		/**
		 * @brief Function to retrieve a Date value for a certain key
		 * @param[in] key std::string representing a KEY in the key/value file
		 * @param[in] section std::string representing a section name; the key has to be part of this section
		 * @param[out] t a variable of class Date into which the value for the corresponding key is saved 
		 * @param[out] time_zone timezone for the date (if the date provides its own timezone, it will be ignored)
		 * @param[in] opt indicating whether an exception should be raised, when key is not present
		 */
		void getValue(std::string key, std::string section, Date& t, const double& time_zone, 
                                              const IOUtils::ThrowOptions& opt=IOUtils::dothrow) const
		{
			t.setUndef(true);
			IOUtils::toUpper(key);
			IOUtils::toUpper(section);
			std::string tmp;
			
			try {
				IOUtils::getValueForKey<std::string>(properties, section + "::" + key, tmp, opt);
			} catch(const std::exception&){
				throw UnknownValueException("[E] Error in "+sourcename+": no value for key "+section+"::"+key, AT);
			}
			
			bool parse_ok = false;
			try {
				parse_ok = IOUtils::convertString(t, tmp, time_zone);
			} catch(const std::exception&){
				parse_ok = false;
			}
			if (!parse_ok && opt==IOUtils::dothrow)
				throw InvalidFormatException("Could not parse date '"+tmp+"' in "+sourcename+"for key "+section+"::"+key, AT);
		}
		
		/**
		 * @brief Template function to retrieve a vector of values of class T for a certain key pattern
		 * @param[in] keymatch std::string representing a pattern for the key in the key/value file
		 * @param[in] section std::string representing a section name; the key has to be part of this section
		 * @param[out] vecT a vector of class T into which the values for the corresponding keys are saved
		 */
		template <typename T> void getValues(std::string keymatch, std::string section, std::vector<T>& vecT) const
		{
			vecT.clear();
			IOUtils::toUpper(keymatch);
			IOUtils::toUpper(section);
			const std::vector< std::string > vecKeys( getKeys(keymatch, section) );

			for (size_t ii=0; ii<vecKeys.size(); ++ii) {
				const std::string full_key( section + "::" + vecKeys[ii] );
				T tmp;
				try {
					IOUtils::getValueForKey<T>(properties, full_key, tmp, IOUtils::dothrow);
				} catch(const std::exception&){
					throw UnknownValueException("[E] Error in "+sourcename+" reading key "+full_key, AT);
				}
				vecT.push_back( tmp );
			}
		}

		template <typename T> void getValues(std::string keymatch, std::string section, std::vector<T>& vecT, std::vector<std::string>& vecKeys) const
		{
			vecT.clear();
			IOUtils::toUpper(keymatch);
			IOUtils::toUpper(section);
			vecKeys = getKeys(keymatch, section);

			for (size_t ii=0; ii<vecKeys.size(); ++ii) {
				const std::string full_key = section + "::" + vecKeys[ii];
				T tmp;
				try {
					IOUtils::getValueForKey<T>(properties, full_key, tmp, IOUtils::dothrow);
				} catch(const std::exception&){
					throw UnknownValueException("[E] Error in "+sourcename+" reading key "+full_key, AT);
				}
				vecT.push_back( tmp );
			}
		}

		std::vector< std::pair<std::string, std::string> > getValues(std::string keymatch, std::string section, const bool& anywhere=false) const;

		/**
		 * @brief Function that searches for a given string within the keys of a given section (default: GENERAL)
		 *         it returns all the keys that match (partial matches are considered) into a vector\<string\>.
		 * @param[in] keymatch A string representing the beginning of a key to search for
		 * @param[in] section A string defining which section to search through (default: GENERAL)
		 * @param[in] anywhere Match substring anywhere in the key string (default=false, ie at the beginning only)
		 * @return a vector that holds all keys that partially match keymatch
		 * @code
		 *  const std::vector<std::string> myVec( cfg.findKeys("TA::", "Filters") );
		 * @endcode
		 */
		std::vector<std::string> getKeys(std::string keymatch, std::string section, const bool& anywhere=false) const;
		
		/**
		 * @brief Returns all the sections that are present in the config object
		 * @return a set that holds all the sections names
		 */
		std::set<std::string> getSections() const {return sections;}

		/**
		 * @brief Move all keys of the \e org section to the \e dest section.
		 * @param[in] org Section of origin
		 * @param[in] dest Section of destination
		 * @param[in] overwrite if true, all keys in the destination section are erased before creating the new keys
		 */
		void moveSection(std::string org, std::string dest, const bool& overwrite);

	private:
		std::map<std::string, std::string> properties; ///< Save key value pairs
 		std::set<std::string> sections; ///< list of all the sections that have been found
		std::string sourcename; ///< description of the data source for the key/value pair
		std::string configRootDir; ///< directory of the root config file
}; //end class definition Config

class ConfigProxy {
	public:
		const Config& proxycfg;
		const std::string& key;
		const std::string& section;

		ConfigProxy(const Config& i_cfg, const std::string& i_key,
		            const std::string& i_section)
		            : proxycfg(i_cfg), key(i_key),section(i_section) { }

		template<typename T> operator T() const {
			T tmp;
			proxycfg.getValue(key, section, tmp, IOUtils::dothrow);
			return tmp;
		}

		ConfigProxy& operator =(const ConfigProxy& /*i_cfg*/) {return *this;} //making VC++ happy...
};

class ConfigParser {
	public:
		ConfigParser(const std::string& filename, std::map<std::string, std::string> &i_properties, std::set<std::string> &i_sections);
		
		static std::string extract_section(std::string key);
	private:
		void parseFile(const std::string& filename);
		void parseLine(const unsigned int& linenr, std::vector<std::string> &import_after, bool &accept_import_before, std::string &line, std::string &section);
		static void processEnvVars(std::string& value);
		static void processExpr(std::string& value);
		bool processVars(std::string& value, const std::string& section);
		bool processSectionHeader(const std::string& line, std::string &section, const unsigned int& linenr);
		std::string clean_import_path(const std::string& in_path) const;
		bool processImports(const std::string& key, const std::string& value, std::vector<std::string> &import_after, const bool &accept_import_before);
		void handleNonKeyValue(const std::string& line_backup, const std::string& section, const unsigned int& linenr, bool &accept_import_before);

		std::map<std::string, std::string> properties; ///< Save key value pairs
		std::set<std::string> imported; ///< list of files already imported (to avoid circular references)
 		std::set<std::string> sections; ///< list of all the sections that have been found
 		std::set<std::string> deferred_vars; ///< list of all the variables that have been found and will have to be resolved
 		std::string sourcename; ///< description of the data source for the key/value pair
};

} //end namespace mio

#endif
