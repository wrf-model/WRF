/***********************************************************************************/
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
#ifndef NetCDFIO_H
#define NetCDFIO_H

#include <meteoio/IOInterface.h>
#include <meteoio/plugins/libncpp.h>

#include <string>

namespace mio {

class ncFiles {
	public:
		enum Mode {READ, WRITE};

		ncFiles(const std::string& filename, const Mode& mode, const Config& cfg, const std::string& schema_name, const bool& i_debug=false);
		ncFiles(const ncFiles& c);
		ncFiles& operator = (const ncFiles& c);
		~ncFiles();

		std::pair<Date, Date> getDateRange() const;
		std::set<size_t> getParams() const;
		std::vector<Date> getTimestamps() const {return vecTime;}
		Grid2DObject read2DGrid(const size_t& param, const Date& date);
		Grid2DObject read2DGrid(const std::string& varname);

		void write2DGrid(const Grid2DObject& grid_in, ncpp::nc_variable& var, const Date& date);
		void write2DGrid(const Grid2DObject& grid_in, size_t param, std::string param_name, const Date& date);

		void writeMeteo(const std::vector< std::vector<MeteoData> >& vecMeteo, const size_t& station_idx=IOUtils::npos);

		std::vector<StationData> readStationData();
		std::vector< std::vector<MeteoData> > readMeteoData(const Date& dateStart, const Date& dateEnd);

	private:
		void initFromFile(const std::string& filename);
		void initVariablesFromFile();
		void initDimensionsFromFile();

		Grid2DObject read2DGrid(const ncpp::nc_variable& var, const size_t& time_pos, const bool& m2mm=false, const bool& reZero=false);
		double read_0Dvariable(const size_t& param) const;
		std::vector<Date> read_1Dvariable() const;
		std::vector<double> read_1Dvariable(const size_t& param) const;
		std::vector<std::string> read_1Dstringvariable(const size_t& param) const;
		std::vector<std::string> read_stationIDs() const;
		std::vector< std::pair<size_t, std::string> > getTSParameters() const;
		size_t read_1DvariableLength(const ncpp::nc_variable& var) const;
		size_t readDimension(const int& dimid) const;
		bool hasDimension(const size_t& dim) const;
		bool hasVariable(const size_t& var) const;

		void writeGridMetadataHeader(const Grid2DObject& grid_in);
		void writeMeteoMetadataHeader(const std::vector< std::vector<MeteoData> >& vecMeteo, const size_t& station_idx);
		static Date getRefDate(const std::vector< std::vector<MeteoData> >& vecMeteo, const size_t& station_idx);
		static std::vector<Date> createCommonTimeBase(const std::vector< std::vector<MeteoData> >& vecMeteo, const size_t& station_idx);
		static void pushVar(std::vector<size_t> &nc_variables, const size_t& param);
		size_t addToVars(const size_t& param);
		size_t addToVars(const std::string& name);
		void appendVariablesList(std::vector<size_t> &nc_variables, const std::vector< std::vector<MeteoData> >& vecMeteo, const size_t& station_idx);
		bool setAssociatedVariable(const size_t& param, const Date& ref_date);
		size_t addTimestamp(const Date& date);
		const std::vector<double> fillBufferForAssociatedVar(const std::vector< std::vector<MeteoData> >& vecMeteo, const size_t& station_idx, const ncpp::nc_variable& var) const;
		const std::vector<double> fillBufferForVar(const std::vector< std::vector<MeteoData> >& vecMeteo, const size_t& station_idx, const ncpp::nc_variable& var) const;
		static const std::vector<double> fillBufferForVar(const Grid2DObject& grid, ncpp::nc_variable& var);
		void applyUnits(Grid2DObject& grid, const std::string& units, const size_t& time_pos, const bool& m2mm) const;
		static void applyUnits(std::vector< std::vector<MeteoData> >& vecMeteo, const size_t& nrStations, const size_t& nrSteps, const std::string& units, const std::string& parname);
		size_t getParameterIndex(const std::string& param_name);

		ACDD acdd; ///< Object that contains the ACDD metadata
		NC_SCHEMA schema; ///<Object that contain all the schema information
		std::map<size_t, ncpp::nc_variable> vars; ///< all the recognized variables for the selected schema_name and current file
		std::map<std::string, ncpp::nc_variable> unknown_vars; ///< all the unrecognized variables for the current file, as map< name, nc_variable>
		std::vector<Date> vecTime;
		std::vector<double> vecX, vecY; ///< caching the lats/lons or eastings/northings to deal with grids
		std::map<size_t, ncpp::nc_dimension> dimensions_map; ///< all the dimensions for the current schema, as found in the current file
		std::string file_and_path, coord_sys, coord_param;
		double TZ; ///< this is the timezone used for reading data
		double dflt_zref, dflt_uref; ///< default reference height for all data or wind data (respectively)
		double dflt_slope, dflt_azi; ///< default slope and azimuth
		size_t max_unknown_param_idx; ///< when writing non-standard parameters, we have to manually assign them a parameter index
		bool strict_schema, lax_schema, debug, isLatLon;
		std::string nc_filename;
		int ncid;
		bool keep_input_files_open, keep_output_files_open;
};

/**
 * @class NetCDFIO
 * @brief This plug-in allows reading and writing of NetCDF files for gridded data.
 *
 * @ingroup plugins
 */
class NetCDFIO : public IOInterface {
	public:
		NetCDFIO(const std::string& configfile);
		NetCDFIO(const NetCDFIO&);
		NetCDFIO(const Config& cfgreader);

		virtual bool list2DGrids(const Date& start, const Date& end, std::map<Date, std::set<size_t> >& list);
		virtual void read2DGrid(Grid2DObject& grid_out, const std::string& parameter="");
		virtual void read2DGrid(Grid2DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date);
		virtual void readDEM(DEMObject& dem_out);

		virtual void write2DGrid(const Grid2DObject& grid_in, const std::string& filename);
		virtual void write2DGrid(const Grid2DObject& grid_in, const MeteoGrids::Parameters& parameter, const Date& date);

		virtual void writeMeteoData(const std::vector< std::vector<MeteoData> >& vecMeteo, const std::string& name="");

		virtual void readStationData(const Date& date, std::vector<StationData>& vecStation);
		virtual void readMeteoData(const Date& dateStart, const Date& dateEnd,
		                           std::vector< std::vector<MeteoData> >& vecMeteo);

	private:
		void parseInputOutputSection();
		void scanPath(const std::string& in_path, const std::string& nc_ext, std::vector< std::pair<std::pair<Date,Date>, ncFiles> > &meteo_files);
		void cleanMeteoCache(std::vector< std::pair<std::pair<Date,Date>, ncFiles> > &meteo_files);

		const Config cfg;
		std::vector< std::pair<std::pair<Date,Date>, ncFiles> > cache_grid_files; //cache of grid files in GRID2DPATH
		std::map<std::string, ncFiles> cache_grids_out; //cache of output GRID2D files
		std::vector< ncFiles > cache_inmeteo_files; //cache of meteo files in input METEOPATH
		std::set<std::string> in_stations; ///< only the stations IDs listed here will be returned by a call to readMeteoData/readStationData
		std::vector<MeteoGrids::Parameters> available_params;
		std::string in_schema, out_schema, in_grid2d_path, in_nc_ext, out_grid2d_path, grid2d_out_file;
		std::string out_meteo_path, out_meteo_file;
		bool debug, out_single_file;
};

} //namespace
#endif
