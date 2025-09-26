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
#ifndef IOINTERFACE_H
#define IOINTERFACE_H

#include <meteoio/Config.h> //so the plugins can get access to Config for their constructor
#include <meteoio/dataClasses/Array2D.h>
#include <meteoio/dataClasses/DEMObject.h>
#include <meteoio/dataClasses/Grid2DObject.h>
#include <meteoio/dataClasses/Grid3DObject.h>
#include <meteoio/dataClasses/MeteoData.h>

#include <vector>
#include <string>

namespace mio {

/**
 * @class IOInterface
 * @brief A class representing the IO Layer of the software Alpine3D. For each type of IO (File, DB, Webservice, etc)
 * a derived class is to be created that holds the specific implementation of the appropriate virtual methods.
 * The IOHandler class is a wrapper class that is able to deal with all above implementations of the IOInterface abstract base class.
 *
 * @ingroup plugins
 * @author Thomas Egger
 * @date   2009-01-08
 */
class IOInterface {
	public:
		virtual ~IOInterface() {}

		/**
		* @brief Return the list of grids within a given time period that could be read by the plugin, if requested.
		* @details This call should be implemented by all plugins reading grids, so the GridsManager can perform all kinds of
		* advanced features with grids (computing a parameter from other ones, temporally interpolating, etc)
		* @param[in] start the start of the time interval
		* @param[in] end the end of the interval
		* @param[out] list the list of grids. The first key is the date, then a set of parameters (from MeteoGrids::Parameters)
		* @return true if the list could be filled (even if empty), false if such as list can not be filled by the plugin (for example, it does not have
		* the data before actually reading it)
		*/
		virtual bool list2DGrids(const Date& start, const Date& end, std::map<Date, std::set<size_t> >& list);

		/**
		* @brief A generic function for parsing 2D grids into a Grid2DObject. The string parameter shall be used for addressing the
		* specific 2D grid to be parsed into the Grid2DObject, relative to GRID2DPATH for most plugins.
		* @param grid_out A Grid2DObject instance
		* @param parameter A std::string representing some information for the function on what grid to retrieve
		*/
		virtual void read2DGrid(Grid2DObject& grid_out, const std::string& parameter="");

		/**
		* @brief Read the given meteo parameter into a Grid2DObject.
		* Each plugin has its own logic for finding the requested meteo parameter grid relative to GRID2DPATH for most plugins
		* @param grid_out A Grid2DObject instance
		* @param parameter The meteo parameter grid type to return (ie: air temperature, wind component, etc)
		* @param date date of the data to read
		*/
		virtual void read2DGrid(Grid2DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date);

		/**
		* @brief A generic function for parsing 3D grids into a Grid3DObject. The string parameter shall be used for addressing the
		* specific 3D grid to be parsed into the Grid3DObject, relative to GRID3DPATH for most plugins.
		* @param grid_out A Grid3DObject instance
		* @param parameter A std::string representing some information for the function on what grid to retrieve
		*/
		virtual void read3DGrid(Grid3DObject& grid_out, const std::string& parameter="");

		/**
		* @brief Read the given meteo parameter into a Grid3DObject.
		* Each plugin has its own logic for finding the requested meteo parameter grid relative to GRID3DPATH for most plugins
		* @param grid_out A Grid3DObject instance
		* @param parameter The meteo parameter grid type to return (ie: air temperature, wind component, etc)
		* @param date date of the data to read
		*/
		virtual void read3DGrid(Grid3DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date);

		/**
		* @brief Parse the DEM (Digital Elevation Model) into the Grid2DObject
		*
		* Example Usage:
		* @code
		* Grid2DObject dem;
		* IOHandler io1("io.ini");
		* io1.readDEM(dem);
		* @endcode
		* @param dem_out A Grid2DObject that holds the DEM
		*/
		virtual void readDEM(DEMObject& dem_out);

		/**
		* @brief Parse the landuse model into the Grid2DObject
		*
		* Example Usage:
		* @code
		* Grid2DObject landuse;
		* IOHandler io1("io.ini");
		* io1.readLanduse(landuse);
		* @endcode
		* @param landuse_out A Grid2DObject that holds the landuse model
		*/
		virtual void readLanduse(Grid2DObject& landuse_out);

    /**
		* @brief Parse the input glacier grid into the Grid2DObject
		*
		* Example Usage:
		* @code
		* Grid2DObject glacier;
		* IOHandler io1("io.ini");
		* io1.readGlacier(glacier);
		* @endcode
		* @param glacier_out A Grid2DObject that holds the glacier height
		*/
		virtual void readGlacier(Grid2DObject& glacier_out);

		/**
		* @brief Fill vecStation with StationData objects for a certain date of interest
		*
		* Example Usage:
		* @code
		* vector<StationData> vecStation;  //empty vector
		* Date d1(2008,06,21,11,0, 1.);       //21.6.2008 11:00 UTC+1
		* IOHandler io1("io.ini");
		* io1.readStationData(d1, vecStation);
		* @endcode
		* @param date A Date object representing the date for which the meta data is to be fetched
		* @param vecStation  A vector of StationData objects to be filled with meta data
		*/
		virtual void readStationData(const Date& date, std::vector<StationData>& vecStation);

		/**
		* @brief Fill vecMeteo with a time series of objects
		* corresponding to the interval indicated by dateStart and dateEnd.
		*
		* Matching rules:
		* - if dateStart and dateEnd are the same: return exact match for date
		* - if dateStart > dateEnd: return first data set with date > dateStart
		* - read in all data starting with dateStart until dateEnd
		* - if there is no data at all then the vectors will be empty, no exception will be thrown
		*
		* Example Usage:
		* @code
		* vector< vector<MeteoData> > vecMeteo;      //empty vector
		* Date d1(2008,06,21,11,0, 1);       //21.6.2008 11:00 UTC+1
		* Date d2(2008,07,21,11,0, 1);       //21.7.2008 11:00 UTC+1
		* IOHandler io1("io.ini");
		* io1.readMeteoData(d1, d2, vecMeteo);
		* @endcode
		* @param dateStart   A Date object representing the beginning of an interval (inclusive)
		* @param dateEnd     A Date object representing the end of an interval (inclusive)
		* @param vecMeteo    A vector of vector<MeteoData> objects to be filled with data
		*/
		virtual void readMeteoData(const Date& dateStart, const Date& dateEnd,
		                           std::vector< std::vector<MeteoData> >& vecMeteo);

		/**
		* @brief Write vecMeteo time series to a certain destination
		*
		* Example Usage:
		* Configure the io.ini to use a certain plugin for the output:
		* @code
		* METEODEST     = GEOTOP
		* METEODESTPATH = /tmp
		* METEODESTSEQ  = Iprec SWglobal
		* @endcode
		* An example implementation (reading and writing):
		* @code
		* vector< vector<MeteoData> > vecMeteo;      //empty vector
		* Date d1(2008,06,21,11,0, 1.);       //21.6.2008 11:00 UTC+1
		* Date d2(2008,07,21,11,0, 1.);       //21.7.2008 11:00 UTC+1
		* IOHandler io1("io.ini");
		* io1.readMeteoData(d1, d2, vecMeteo);
		* io1.writeMeteoData(vecMeteo)
		* @endcode
		* @param vecMeteo    A vector of vector<MeteoData> objects to be filled with data
		* @param name        (optional string) Identifier useful for the output plugin (it could become part
		*                    of a file name, a db table, etc)
		*/
		virtual void writeMeteoData(const std::vector< std::vector<MeteoData> >& vecMeteo,
		                            const std::string& name="");

		/**
		* @brief Parse the assimilation data into a Grid2DObject for a certain date represented by the Date object
		*
		* Example Usage:
		* @code
		* Grid2DObject adata;
		* Date d1(2008,06,21,11,0, 1.);       //21.6.2008 11:00 UTC+1
		* IOHandler io1("io.ini");
		* io1.readAssimilationData(d1, adata);
		* @endcode
		* @param date_in A Date object representing the date of the assimilation data
		* @param da_out  A Grid2DObject that holds the assimilation data for every grid point
		*/
		virtual void readAssimilationData(const Date& date_in, Grid2DObject& da_out);

		/**
		* @brief Read a list of points by their grid coordinates
		* This allows for example to get a list of points where to produce more detailed outputs.
		* @param pts (std::vector<Coords>) A vector of points coordinates
		*/
		virtual void readPOI(std::vector<Coords>& pts);

		/**
		* @brief Write a Grid2DObject
		* The filename is specified relative to GRID2DPATH for most plugins
		* @param grid_out (Grid2DObject) The grid to write
		* @param options (string) Identifier useful for the output plugin (it could become part of a file name, a db table, etc)
		*/
		virtual void write2DGrid(const Grid2DObject& grid_out, const std::string& options="");

		/**
		* @brief Write a Grid2DObject comtaining a known meteorological parameter
		* A filename is build relative to GRID2DPATH for most plugins.
		* @param grid_out (Grid2DObject) The grid to write
		* @param parameter The meteo parameter grid type of the provided grid object (ie: air temperature, wind component, etc)
		* @param date date of the data to write
		*/
		virtual void write2DGrid(const Grid2DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date);

		/**
		* @brief Write a Grid3DObject
		* The filename is specified relative to GRID3DPATH for most plugins
		* @param grid_out (Grid3DObject) The grid to write
		* @param options (string) Identifier useful for the output plugin (it could become part of a file name, a db table, etc)
		*/
		virtual void write3DGrid(const Grid3DObject& grid_out, const std::string& options="");

		/**
		* @brief Write a Grid3DObject comtaining a known meteorological parameter
		* A filename is build relative to GRID3DPATH for most plugins.
		* @param grid_out (Grid3DObject) The grid to write
		* @param parameter The meteo parameter grid type of the provided grid object (ie: air temperature, wind component, etc)
		* @param date date of the data to write
		*/
		virtual void write3DGrid(const Grid3DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date);


		static void set2DGridLatLon(Grid2DObject &grid, const double& i_ur_lat, const double& i_ur_lon);
		static double computeGridXYCellsize(const std::vector<double>& vecX, const std::vector<double>& vecY);
};

} //end namespace

#endif
