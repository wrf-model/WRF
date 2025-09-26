/***********************************************************************************/
/*  Copyright 2012 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef GRIBIO_H
#define GRIBIO_H

#include <meteoio/IOInterface.h>

#include <string>
#include <grib_api.h>

namespace mio {

/**
 * @class GRIBIO
 * @brief This plugin reads GRIB 1 or 2 data files
 *
 * @ingroup plugins
 * @author Mathias Bavay
 * @date   2012-01-25
 */
class GRIBIO : public IOInterface {
	public:
		GRIBIO(const std::string& configfile);
		GRIBIO(const GRIBIO&);
		GRIBIO(const Config& cfgreader);
		~GRIBIO() throw();

		GRIBIO& operator=(const GRIBIO&); ///<Assignement operator, required because of pointer member

		virtual bool list2DGrids(const Date& /*start*/, const Date& /*end*/, std::map<Date, std::set<size_t> >& /*list*/) {return false;}
		virtual void read2DGrid(Grid2DObject& grid_out, const std::string& parameter="");
		virtual void read2DGrid(Grid2DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date);
		virtual void readDEM(DEMObject& dem_out);

		virtual void readMeteoData(const Date& dateStart, const Date& dateEnd,
		                           std::vector< std::vector<MeteoData> >& vecMeteo);
		
	private:
		void setOptions();
		void getDate(grib_handle* h, Date &base, double &d1, double &d2);
		Coords getGeolocalization(grib_handle* h, double &cellsize_x, double &cellsize_y);
		void read2Dlevel(grib_handle* h, Grid2DObject& grid_out);
		bool read2DGrid_indexed(const double& in_marsParam, const long& i_levelType, const long& i_level, const Date i_date, Grid2DObject& grid_out);
		void read2DGrid(const std::string& filename, Grid2DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date);
		void readWind(const std::string& filename, const Date& date);
		void indexFile(const std::string& filename);
		void readStations(std::vector<Coords> &vecPoints);
		void listFields(const std::string& filename);
		void listKeys(grib_handle** h, const std::string& filename);
		void scanMeteoPath();
		void cleanup() throw();

		bool removeDuplicatePoints(std::vector<Coords>& vecPoints, double *lats, double *lons);
		bool readMeteoMeta(std::vector<Coords>& vecPoints, std::vector<StationData> &stations, double *lats, double *lons);
		bool readMeteoValues(const double& marsParam, const long& levelType, const long& i_level, const Date& i_date, const size_t& npoints, double *lats, double *lons, double *values);
		void fillMeteo(double *values, const MeteoData::Parameters& param, const size_t& npoints, std::vector<MeteoData> &Meteo);
		void readMeteoStep(std::vector<StationData> &stations, double *lats, double *lons, const Date i_date, std::vector<MeteoData> &Meteo);

		const Config cfg;
		std::string grid2dpath_in;
		std::string meteopath_in;
		std::vector<Coords> vecPts; //points to use for virtual stations if METEO=GRIB
		std::vector< std::pair<Date,std::string> > cache_meteo_files; //cache of meteo files in METEOPATH
		std::string meteo_ext; //file extension
		std::string grid2d_ext; //file extension
		std::string grid2d_prefix; //filename prefix, like "laf"
		std::string idx_filename; //matching file name for the index
		std::string coordin, coordinparam; //projection parameters
		Grid2DObject VW, DW; //for caching wind fields, since they require quite some calculations
		Date wind_date;
		Coords llcorner;

		FILE *fp; //since passing fp always fail...
		grib_index *idx; //because it needs to be kept between calls
		double latitudeOfNorthernPole, longitudeOfNorthernPole; //for rotated coordinates
		double bearing_offset; //to correct vectors coming from rotated lat/lon, we will add an offset to the bearing
		double cellsize, factor_x, factor_y;

		static const std::string default_ext;
		static const double plugin_nodata; //plugin specific nodata value, e.g. -999
		static const double tz_in; //GRIB time zone
		bool indexed; //flag to know if the file has already been indexed
		bool meteo_initialized; //set to true after we scanned METEOPATH, filed the cache, read the virtual stations from io.ini
		bool llcorner_initialized; //set to true after we properly computed llcorner
		bool update_dem, debug;

};

} //namespace
#endif
