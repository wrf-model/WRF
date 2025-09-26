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
#include <meteoio/IOInterface.h>

namespace mio {

/**
 * @page dev_plugins Plugins developement guide
 * The data access is handled by a system of plugins. They all offer the same interface, meaning that a plugin can transparently be replaced by another one. This means that plugins should follow some common rules, which are described in this guide. Templates header and code files are available to get you started, look into the "plugins" subdirectory of the source directory (files "template.cc" and "template.h").
 *
 * @section plugins_implementation Plugins implementation
 * Each plugin must inherit the class IOInterface and implement all or part of its interface (at least for public methods). The plugins are then free to implement the private methods that they see fit. Because the interface described in IOInterface is quite generic, some methods might not be relevant for a given plugin. In such a case, the plugin should throw an exception as illustrated in the example below:
 * @code
 * void MyPlugin::read2DGrid(Grid2DObject&, const std::string&)
 * {
 * 	//Nothing so far
 * 	throw IOException("Nothing implemented here", AT);
 * }
 * @endcode
 *
 * It is the responsibility of the plugin to properly convert the units toward the SI as used in MeteoIO (see the MeteoData class for a list of parameters and their units). This includes converting the nodata value used internally in the plugin (as could be defined in the data itself) into the unified IOUtils::nodata nodata value. Moreover, it is required by the BufferedIOHandler that each plugin that implements readMeteoData @em also implements the readStationData method. This is required so that the metadata is available even if no data exists for the requested time period.
 *
 * Finally, plugins must properly handle time zones. The Date class provides everything that is necessary, but the plugin developer must still properly set the time zone to each Date object (using the "TZ" key in the io.ini configuration file at least as a default value and afterwards overwriting with a plugin specified time zone specification if available). The time zone should be set \em before setting the date (so that the date that is given is understood as a date within the specified time zone).
 *
 * The meteorological data must be returned in a vector of vectors of MeteoData (and similarly, of StationData in order to provide the metadata). This consists of building a vector of MeteoData objects, each containing a set of measurements for a given timestamp, at a given location. This vector that contains the time series at one specific location is then added to a vector (pushed) that will then contain all locations.
 * \image html vector_vector.png "vector of vector structure"
 * \image latex vector_vector.eps "vector of vector structure" width=0.9\textwidth
 *
 * Various classes from MeteoIO can prove convenient for use by plugins: for example the Coords class should be used for geographic coordinates conversions, while the Config class should be used for getting configuration information from the user's configuration file. Please do NOT implement your own version of this kind of feature in your plugin but exclusively rely on the matching classes of MeteoIO, extending them if necessary.
 * A template for writing a plugin class is available in the plugin directory under the name template.cc and template.h. Simply copy these two files under a new name and fill the methods that you want to implement. Some easy example implementation can be found in ARCIO or A3DIO.
 *
 * @section plugins_registration Plugins registration
 * Once a plugin has been written, it must be "registered" so that it is known by the rest of the library. This is done in IOHandler.cmake.cc by a) adding a compilation flag similar to the other plugins (`cmakedefine PLUGIN_...`), b) including its header file
 * (optionally compiling it only if it is being installed), c) giving it a plugin key in `IOHandler::getPlugin` (that will be used by the user in the configuration file when they want to use said plugin), and d) adding it to the doc in the HTML table.
 *
 * Finally, the build system has to be updated so that it offers the plugin to be built: a local file (<a href="../../meteoio/plugins/CMakeLists.txt">meteoio/plugins/CMakeLists.txt</a>) has to be edited so that the plugin is really built, then the toplevel file has to be modified so the user can choose to build the plugin if he wishes (<a href="../../CMakeLists.txt">CMakeLists.txt</a>). Please keep in mind that all plugins should be optional (ie: they should not prevent the build of MeteoIO without them) and please call your plugin compilation flag similarly as the other plugins (ie: PLUGIN_MYNAME).
 *
 * @section plugins_documentation Plugins documentation
 * It is the responsibility of the plugin developer to properly document his plugin. The documentation should be placed as doxygen comments in the implementation file, following the example of A3DIO.cc: a subpage named after the plugin should be created (and referenced in MainPage.h) with at least the following sections:
 * - format, for describing the data format
 * - units, expressing what the input units should be
 * - keywords, listing (with a brief description) the keywords that are recognized by the plugin for its configuration
 *
 * The internal documentation of the plugin can remain as normal C++ comments (since they are addressed to the maintainer of the plugin).
 *
 * @section plugins_testing Plugins testing and validation
 * In order to check that a plugin operates properly, developers are invited to test the following (among others), for a plugin that reads meteorological data (these tests can be performed using the example code <a href="../../doc/examples/meteo_reading.cc">doc/examples/meteo_reading.cc</a>):
 * - request one time stamp part of the original data set
 * - request one time stamp within the period of the original data set, but not matching an existing time stamp
 * - request a data point @em before the original data set
 * - request a data point @em after the original data set
 * - request a data point at the end of a very large data set (tens of Mb) and check that memory consumption remains the same as when requesting a data point at the beginning of the data set (ie: that the memory for one buffer is used, but that the temporary memory usage of the plugin does not increase with the amount of data it has to parse before returning it to the BufferedIOHandler)
 */

bool IOInterface::list2DGrids(const Date& /*start*/, const Date& /*end*/, std::map<Date, std::set<size_t> >& /*list*/)
{
	throw IOException("Nothing implemented here", AT);
}

void IOInterface::read2DGrid(Grid2DObject& /*grid_out*/, const std::string& /*parameter=""*/)
{
	throw IOException("Nothing implemented here", AT);
}

void IOInterface::read2DGrid(Grid2DObject& /*grid_out*/, const MeteoGrids::Parameters& /*parameter*/, const Date& /*date*/)
{
	throw IOException("Nothing implemented here", AT);
}

void IOInterface::read3DGrid(Grid3DObject& /*grid_out*/, const std::string& /*parameter=""*/)
{
	throw IOException("Nothing implemented here", AT);
}

void IOInterface::read3DGrid(Grid3DObject& /*grid_out*/, const MeteoGrids::Parameters& /*parameter*/, const Date& /*date*/)
{
	throw IOException("Nothing implemented here", AT);
}

void IOInterface::readDEM(DEMObject& /*dem_out*/)
{
	throw IOException("Nothing implemented here", AT);
}

void IOInterface::readLanduse(Grid2DObject& /*landuse_out*/)
{
	throw IOException("Nothing implemented here", AT);
}

void IOInterface::readGlacier(Grid2DObject& /*landuse_out*/) 
{
	throw IOException("Nothing implemented here", AT);
}

void IOInterface::readStationData(const Date& /*date*/, std::vector<StationData>& /*vecStation*/)
{
	throw IOException("Nothing implemented here", AT);
}

void IOInterface::readMeteoData(const Date& /*dateStart*/, const Date& /*dateEnd*/,
		                           std::vector< std::vector<MeteoData> >& /*vecMeteo*/)
{
	 throw IOException("Nothing implemented here", AT);
}

void IOInterface::writeMeteoData(const std::vector< std::vector<MeteoData> >& /*vecMeteo*/,
		                            const std::string& /*name=""*/)
{
	 throw IOException("Nothing implemented here", AT);
}

void IOInterface::readAssimilationData(const Date& /*date_in*/, Grid2DObject& /*da_out*/)
{
	throw IOException("Nothing implemented here", AT);
}

void IOInterface::readPOI(std::vector<Coords>& /*pts*/)
{
	throw IOException("Nothing implemented here", AT);
}

void IOInterface::write2DGrid(const Grid2DObject& /*grid_out*/, const std::string& /*options=""*/)
{
	throw IOException("Nothing implemented here", AT);
}

void IOInterface::write2DGrid(const Grid2DObject& /*grid_out*/, const MeteoGrids::Parameters& /*parameter*/, const Date& /*date*/)
{
	throw IOException("Nothing implemented here", AT);
}

void IOInterface::write3DGrid(const Grid3DObject& /*grid_out*/, const std::string& /*options*/)
{
	throw IOException("Nothing implemented here", AT);
}

void IOInterface::write3DGrid(const Grid3DObject& /*grid_out*/, const MeteoGrids::Parameters& /*parameter*/, const Date& /*date*/)
{
	throw IOException("Nothing implemented here", AT);
}

void IOInterface::set2DGridLatLon(Grid2DObject &grid, const double& i_ur_lat, const double& i_ur_lon)
{
	grid.ur_lat = i_ur_lat;
	grid.ur_lon = i_ur_lon;
	grid.isLatLon = true;
}

double IOInterface::computeGridXYCellsize(const std::vector<double>& vecX, const std::vector<double>& vecY)
{
	return Grid2DObject::calculate_XYcellsize(vecX, vecY);
}

} //end namespace
