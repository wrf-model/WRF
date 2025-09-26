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
#include <meteoio/plugins/NetCDFIO.h>
#include <meteoio/meteoLaws/Meteoconst.h>
#include <meteoio/meteoLaws/Atmosphere.h>
#include <meteoio/FileUtils.h>
#include <meteoio/MathOptim.h>
#include <meteoio/plugins/libncpp.h>
#include <meteoio/meteoStats/libresampling2D.h>
#include <meteoio/dataClasses/Coords.h>
#include <meteoio/dataClasses/CoordsAlgorithms.h>

#include <cmath>
#include <cstdio>
#include <sstream>
#include <algorithm>
#include <netcdf.h>

#define DFLT_STAT_STR_LEN 16

using namespace std;

namespace mio {
/**
 * @page netcdf NetCDF
 * @section netcdf_format Format
 * In order to promote creation, access and sharing of scientific data, the NetCDF format has been
 * created as a machine-independent format. NetCDF (network Common Data Form) is therefore an interface
 * for array-oriented data access and a library that provides an implementation of the interface. The
 * <A HREF="http://www.unidata.ucar.edu/downloads/netcdf/index.jsp">NetCDF software</A> was developed
 * at the <A HREF="http://www.unidata.ucar.edu/">Unidata Program Center</A> in Boulder, Colorado.
 * In order to graphicaly explore the content and structure of NetCDF files, you can use the
 * <A HREF="http://www.epic.noaa.gov/java/ncBrowse/">ncBrowse</A> java software or
 * <A HREF="http://meteora.ucsd.edu/~pierce/ncview_home_page.html">ncview</A>. It is also possible to run *ncdump* on a given
 * file in order to have a look at its structure (such as *ncdump {my_netcdf_file} | more*) and specially the parameters names
 * (this is useful if remapping is needed, see below in the \ref netcdf_keywords "Keywords" or in the \ref netcdf_renaming "Renaming" section).
 *
 * The NetCDF format does not impose a specific set of metadata and therefore in order to easily exchange data
 * within a given field, it is a good idea to standardize the metadata. Several such metadata schema can be used
 * by this plugin:
 * - CF1 - the <A HREF="http://cfconventions.org">conventions</A> for climate and forecast (CF) metadata (there is an 
 * <a href="https://pumatest.nerc.ac.uk/cgi-bin/cf-checker.pl">online validator</a>);
 * - ECMWF - from the <A HREF="http://www.ecmwf.int/">European Centre for Medium-Range Weather Forecasts</A>, see the <A HREF="https://software.ecmwf.int/wiki/display/TIGGE/Soil+temperature">ECMWF Wiki</A> for a description of the available fields;
 * - CROCUS - from the <A HREF="http://www.cnrm.meteo.fr/">National Centre for Meteorological Research</A>;
 * - AMUNDSEN - from the <A HREF="https://geographie.uibk.ac.at/blog/ahc/models/">Alpine, Hydro, climatology</A> group in Innsbruck;
 * - WRF - the <A HREF="http://www.wrf-model.org/index.php">Weather Research & Forecasting</A> model;
 * - METEOCH - for datasets produced by <A HREF="https://www.meteoswiss.admin.ch/home/climate/swiss-climate-in-detail/raeumliche-klimaanalysen.html">Meteo Swiss</A>.
 *
 * Moreover, when writing NetCDF files with MeteoIO, all the generated files will contain as much of the Attribute Conventions Dataset Discovery
 * <A href="http://wiki.esipfed.org/index.php?title=Category:Attribute_Conventions_Dataset_Discovery">(ACDD)</A> metadata as (automatically) possible,
 * but some fields must be filled by the user for full compliance. This is detailed in section \ref netcdf_editing "Editing" below.
 *
 * If you want to better understand the structure of the NetCDF file format, you are highly encouraged to read about
 * its <A HREF="https://www.unidata.ucar.edu/software/netcdf/docs/netcdf_data_set_components.html">components</A>.
 *
 * @section netcdf_compilation Compilation
 * In order to compile this plugin, you need libnetcdf (for C). For Linux, please select both the libraries and
 * their development files in your package manager.
 *
 * @section netcdf_keywords Keywords
 * This plugin uses the following keywords:
 * - General keys:
 *     - NC_EXT: only the files containing this pattern in their filename will be used; [Input] section (default: .nc)
 *     - NETCDF_SCHEMA: the schema to use (either CF-1.6, CROCUS, AMUNDSEN,  ERA-INTERIM, ERA5 or WRF); [Input] and [Output] section (default: CF-1.6)
 *     - NETCDF_VAR::{MeteoGrids::Parameters} = {netcdf_param_name} : this allows to remap the names as found in the NetCDF file to the MeteoIO grid parameters; [Input] section;
 *     - NETCDF_DIM::{MeteoGrids::Parameters} = {netcdf_dimension_name} : this allows to remap the names as found in the NetCDF file to the ncFiles Dimensions; [Input] section;
 *     - NC_DEBUG : print some low level details about the file being read (default: false); [Input] section;
 *     - NC_KEEP_FILES_OPEN: keep files open for efficient access (default for input: true, default for output: false). Reading from or writing to many NetCDF files may cause the
 *       plugin to exceed the maximum allowed concurrent open files determined by system limits. Also, when multiple modules write to the same output file, file corruption may occur.
 *       For those cases, NC_KEEP_FILES_OPEN = FALSE forces the plugin to open only one file at a time for reading (when in [Input] section), or writing (when in [Output] section,
 *       default behavior).
 * - Gridded data handling:
 *     - DEMFILE: The filename of the file containing the DEM; [Input] section
 *     - DEMVAR: The variable name of the DEM within the DEMFILE; [Input] section
 *     - GRID2DPATH: if this directory contains files, they will be used for reading the input from; [Input] and [Output] section
 *     - GRID2DFILE: force reading the data from a single file within GRID2DPATH or specify the output file name; [Input] and [Output] section
 * - Time series handling:
 *     - STATION#: if provided, only the given station IDs will be kept in the input data (this is specially useful when reading a file containing multiple stations); [Input]
 *     - METEOPATH: meteo files directory where to read the meteofiles from; [Input] section. Two modes are available when reading input files:
 *          - a fixed list of files is provided:
 *               - METEOFILE#: input filename (in METEOPATH). As many meteofiles as needed may be specified (the extension can be skipped if it is NC_EXT); [Input]
 *          - METEOPATH is scanned for files having the NC_EXT extension:
 *               - METEOPATH_RECURSIVE: should the scan for files be recursive (default: false)?; [Input]
 *     - NC_SINGLE_FILE: when writing timeseries of station data, force all stations to be contained in a single file (default: false); [Output]
 *     - METEOFILE: when NC_SINGLE_FILE is set, the output file name to use [Output];
 *     - NC_STRICT_SCHEMA: only write out parameters that are specifically described in the chosen schema (default: false, all parameters in
 * MeteoGrids::Parameters are also written out); [Output]
 *     - NC_LAX_SCHEMA: write out all provided parameters even if no metadata can be associated with them (default: false); [Output]
 *     - For some applications, some extra information must be provided for meteorological time series (for example, for Crocus), in the [Output] section:
 *          - ZREF: the reference height for meteorological measurements;
 *          - UREF: the reference height for wind measurements;
 *          - DEFAULT_SLOPE: a default value for the slope when none is available;
 *          - DEFAULT_AZI: a default value for the azimuth when none is available;
 *
 * Some of the ACDD metadata can also be configured, see the ACDD class.
 *
 * @note The timezone is assumed to be GMT. When reading a NetCDF file, the stationID and stationName are supposed to be provided by either
 * global attributes or through a variable. If nothing is provided, the filename (stripped of its extension) is used.
 * @note When providing multiple grid files in one directory, in case of overlapping files (because each file can provide multiple timestamps),
 * the file containing the newest data has priority. This is convenient when using forecast data to automatically use the most short-term forecast.
 * @note When using the CROCUS schema, please note that the humidity should be provided as specific humidity, so please use a data
 * creator if don't already have a QI parameter (see HumidityGenerator). Crocus also requires split precipitation, this can be generated by the PrecSplitting creator.
 * Finally, Crocus does not handles missing data, so make sure you define some data generators in case some data might be missing (specially for the precipitation).
 *
 *
 * @section netcdf_example Example use
 * Using this plugin to build downscaled time series at virtual stations, with the ECMWF Era Interim data set (see section below):
 * @code
 * [Input]
 * GRID2D    = NETCDF
 * GRID2DPATH =  /data/meteo_reanalysis
 * NETCDF_SCHEMA = ECMWF
 *
 * DEM = NETCDF
 * DEMFILE = /data/meteo_reanalysis/ECMWF_Europe_20150101-20150701.nc
 *
 * #The lines below have nothing to do with this plugin
 * Downscaling = true
 * VSTATION1 = 46.793029 9.821343 ;this is Davos
 * Virtual_parameters = TA RH PSUM ISWR ILWR P VW DW TSS HS RSWR TSG ;this has to fit the parameter set in the data files
 * @endcode
 *
 * Another example, to extract precipitation from the MeteoSwiss daily precipitation reanalysis, RhiresD
 * @code
 * [Input]
 * DEM     = NETCDF
 * DEMFILE = ./input/ch02_lonlat.nc
 *
 * GRID2D    = NETCDF
 * GRID2DPATH =  /data/meteo_reanalysis
 * NC_EXT = .nc
 * NETCDF_VAR::PSUM = RhiresD               ;overwrite the PSUM parameter with "RhiresD", for example for MeteoCH reanalysis
 *
 * #The lines below have nothing to do with this plugin
 * Downscaling = true
 * VSTATION1 = 46.793029 9.821343 ;this is Davos
 * Virtual_parameters = PSUM ;this has to fit the parameter set in the data files
 * @endcode
 *
 * @section netcdf_meteoch MeteoCH RhiresD & similar products
 * <A HREF="http://www.meteoswiss.admin.ch/home.html?tab=overview">MeteoSwiss</A> provides <A HREF="http://www.ifu.ethz.ch/hydrologie/research/research_data/proddoc.pdf">reanalysis</A> of precipitation and other meteo fields from 1961 to present over Switzerland for different time scales: daily, monthly, yearly, as well as hourly (CPC dataset). The DEM are also provided, either in lat/lon,
 * Swiss coordinates, rotated lat/lon, ... These data sets must be requested from MeteoSwiss and are available with a specific license for research.
 *
 * @section netcdf_wrf WRF output files
 * While <A HREF="http://www.wrf-model.org/index.php">WRF</A> can write its <A HREF="http://www2.mmm.ucar.edu/wrf/users/docs/user_guide_V3/users_guide_chap5.htm#fields">outputs in NetCDF</A>, unfortunately
 * it does not follow the CF1 convention and relies on lots of idiosyncracies (see http://www.ncl.ucar.edu/Applications/wrfnetcdf.shtml) that break lots of
 * applications dealing with NetCDF. If some fields are not read by MeteoIO,
 * please follow the tips given \ref netcdf_tricks "below". Moreover, WRF assumes that latitudes / longitudes are given on an ideal sphere while standard
 * coordinates systems assume an ellipsoid. This may lead to trouble when converting model coordinates to real world coordinates (see
 * http://www.pkrc.net/wrf-lambert.html).
 *
 * @section netcdf_ecmwf ECMWF Era Interim
 * The Era Interim data can be downloaded on the <A HREF="http://apps.ecmwf.int/datasets/data/interim-full-daily/levtype=sfc/">ECMWF dataserver</A>
 * after creating an account and login in.
 *
 * For Era Interim, it is recommended to extract data at 00:00, and 12:00 for all steps 3, 6, 9, 12. The select the following fields:
 * 10 metre U wind component, 10 metre V wind component, 2 metre dewpoint temperature, 2 metre temperature, Forecast albedo, Skin temperature, Snow density, Snow depth, Soil temperature level 1, Surface pressure, Surface solar radiation downwards, Surface thermal radiation downwards, Total precipitation
 *
 * Here we have included the *forecast albedo* so the RSWR can be computed from ISWR. You should download the altitude separately (it is in the
 * "invariants" section on the left hand side of the page where you select the fields to download).
 *
 * @note The radiation fields are accumulated since the start of the forecast period (00:00 and 12:00 as recommended above), so they must be corrected
 * before using them (see ProcDeAccumulate)!
 *
 * You should therefore have the following request:
 * @code
 * Parameter: 10 metre U wind component, 10 metre V wind component, 2 metre dewpoint temperature, 2 metre temperature, Forecast albedo,
 *            Skin temperature, Snow density, Snow depth, Soil temperature level 1, Surface pressure,
 *            Surface solar radiation downwards, Surface thermal radiation downwards, Total precipitation
 *      Step: 3 to 12 by 3
 *      Type: Forecast
 *      Time: 00:00:00, 12:00:00
 * @endcode
 *
 * With the <A HREF="https://software.ecmwf.int/wiki/display/WEBAPI/Access+ECMWF+Public+Datasets">ECMWF Python Library</A>, the request
 * would be for example (the area is defined as North/West/South/East, see the
 * <A HREF="https://software.ecmwf.int/wiki/display/UDOC/Post-processing+keywords#Post-processingkeywords-area">WEBAPI</a> documentation):
 * @code
 * #!/usr/bin/env python
 * from ecmwfapi import ECMWFDataServer
 * server = ECMWFDataServer()
 * server.retrieve({
 * "class": "ei",
 * "dataset": "interim",
 * "date": "2015-01-01/to/2015-01-31",
 * "expver": "1",
 * "grid": "0.75/0.75",
 * "levtype": "sfc",
 * "param": "33.128/134.128/139.128/141.128/165.128/166.128/167.128/168.128/169.128/175.128/205.128/228.128/235.128/243.128",
 * "step": "3/6/9/12",
 * "area":"42.2/-1.5/51.7/15.7",
 * "stream": "oper",
 * "format":"netcdf",
 * "target": "my-era-interim.nc",
 * "time": "00/12",
 * "type": "fc",
 * })
 * @endcode
 * 
 * @section netcdf_copernicus Copernicus Era 5
 * @note This is a work in progress, stay tuned!
 * 
 * The Era 5 data can be downloaded from the <a href="https://cds.climate.copernicus.eu/cdsapp#!/home">Copernicus Climate Data Store</a> 
 * (either from the web interface or using the <a href="https://pypi.org/project/cdsapi/">cdsapi</a>), after registering and creating an api key.
 *
 * Ideally, download the hourly data and select the following fields:
 * 10 metre U wind component, 10 metre V wind component, 2 metre dewpoint temperature, 2 metre temperature, Near IR albedo for direct radiation, Skin temperature, Snow density, Snow depth, Soil temperature level 1, Surface pressure, Mean surface downward short-wave radiation flux, Mean surface downward long-wave radiation flux, Total precipitation
 *
 * Here we have included the *albedo* so the RSWR can be computed from ISWR. You should download the altitude separately (it is in the
 * "others" section on the bottom of the page where you select the data to download).
 *
 * @note The reanalysis runs offer the mean fluxes over the last hour as well as accumulated precipitation over the last hour, making it very easy to work with.
 *
 * With the <A HREF="https://pypi.org/project/cdsapi/">cdsapi Python Library</A>, the request
 * would be for example (the time period and spatial extend should be defined properly...):
 * @code
 * #!/usr/bin/env python
 * import cdsapi
 * c = cdsapi.Client()
 * 
 * c.retrieve(
 *             'reanalysis-era5-single-levels',{
 *                 'product_type':'reanalysis',
 *                 'variable':[
 *                     '10m_u_component_of_wind','10m_v_component_of_wind','2m_dewpoint_temperature',
 *                     '2m_temperature','forecast_albedo','skin_temperature',
 *                     'snow_density','snow_depth','soil_temperature_level_1',
 *                     'surface_pressure','mean_surface_downward_short_wave_radiation_flux',
 *                     'mean_surface_downward_long_wave_radiation_flux',
 *                     'total_precipitation'
 *                 ],
 *                 'year':['2018'],
 *                 'month':['01','02','03','04','05','06','07','08','09','10','11','12'],
 *                 'day':['01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31'],
 *                 'time':'00/to/23/by/1',
 *                 'area':'42.2/-1.5/51.7/15.7', # North, West, South, East. Default: global
 *                 'grid':'0.25/0.25',
 *                 'format':'netcdf'
 *           },'download.nc')
 * @endcode
 *
 * @section netcdf_tricks External tools and tricks to work with NetCDF
 * @subsection netcdf_editing Editing the metadata of a NetCDF file
 * In order to ensure that a NetCDF is <A href="http://wiki.esipfed.org/index.php?title=Category:Attribute_Conventions_Dataset_Discovery">ACDD</A> compliant,
 * several global variables must be defined. Most of them have already been populated by MeteoIO but a few could benefit from further editing. You can have a
 * look at what has already been defined by dumping the header content with ncdump:
 * @code
 * ncdump -h {my_netcdf_file}
 * @endcode
 *
 * Then, to add your metadata, you can use <A href="http://nco.sourceforge.net/">ncatted</A> (it is often packaged as "nco"). For example,
 * to replace the previous global attribute <i>summary</i> by yours or to append a line to the <i>history</i> global attribute:
 * @code
 * ncatted -a summary,global,o,c,"My summary" {my_netcdf_file}	#Overwrite the summary field
 * ncatted -a history,global,a,c,"Edited by me on 2018-06-06\n" {my_netcdf_file}	#Append a line to the history field
 * @endcode
 *
 * @subsection netcdf_renaming Saving the day when a file is not standard compliant
 * Unfortunatelly, the naming of the parameters and dimensions within the files is not always standard nor consistent. In order to handle the parameters names,
 * simply run *ncdump {my_netcdf_file} | more* and use the name mapping facility of this plugin to map the non-standard parameters to our internal names
 * (see the \ref netcdf_keywords "plugin keywords"). When the dimensions are not standard (for example the time axis being called "TIME_T"),
 * use first the <A HREF="http://linux.die.net/man/1/ncrename">ncrename</A> tool that is part of the
 * <A HREF="http://nco.sourceforge.net/">NCO utilities</A> to rename both the dimension (-d) and the variable (-v):
 * @code
 * ncrename -d TIME_T,time -v TIME_T,time {my_netcdf_file}
 * @endcode
 */

//helper function to sort the cache of grid files
inline bool sort_cache_grids(const std::pair<std::pair<Date,Date>,ncFiles> &left, const std::pair<std::pair<Date,Date>,ncFiles> &right) {
	if (left.first.first < right.first.first) return true;
	if (left.first.first > right.first.first) return false;
	return left.first.second < right.first.second; //date_start equallity case
}

NetCDFIO::NetCDFIO(const std::string& configfile)
         : cfg(configfile), cache_grid_files(), cache_grids_out(), cache_inmeteo_files(), in_stations(), available_params(), in_schema("CF-1.6"), out_schema("CF-1.6"), in_grid2d_path(), in_nc_ext(".nc"), out_grid2d_path(), grid2d_out_file(),
         out_meteo_path(), out_meteo_file(), debug(false), out_single_file(false)
{
	parseInputOutputSection();
}

NetCDFIO::NetCDFIO(const Config& cfgreader)
         : cfg(cfgreader), cache_grid_files(), cache_grids_out(), cache_inmeteo_files(), in_stations(), available_params(), in_schema("CF-1.6"), out_schema("CF-1.6"), in_grid2d_path(), in_nc_ext(".nc"), out_grid2d_path(), grid2d_out_file(),
         out_meteo_path(), out_meteo_file(), debug(false), out_single_file(false)
{
	parseInputOutputSection();
}

void NetCDFIO::parseInputOutputSection()
{
	const std::string in_grid2d = cfg.get("GRID2D", "Input", "");
	if (in_grid2d=="NETCDF") { //keep it synchronized with IOHandler.cc for plugin mapping!!
		cfg.getValue("NC_DEBUG", "INPUT", debug, IOUtils::nothrow);
		cfg.getValue("NETCDF_SCHEMA", "Input", in_schema, IOUtils::nothrow); IOUtils::toUpper(in_schema);

		const std::string grid2d_in_file = cfg.get("GRID2DFILE", "Input", "");
		if (!grid2d_in_file.empty()) {
			if (!FileUtils::fileExists(grid2d_in_file)) throw AccessException(grid2d_in_file, AT); //prevent invalid filenames
			ncFiles file(grid2d_in_file, ncFiles::READ, cfg, in_schema, debug);
			cache_grid_files.push_back( make_pair(file.getDateRange(), file) );
		} else {
			cfg.getValue("GRID2DPATH", "Input", in_grid2d_path);
			cfg.getValue("NC_EXT", "INPUT", in_nc_ext, IOUtils::nothrow);
		}
	}

	const std::string out_grid2d = cfg.get("GRID2D", "Output", "");
	if (out_grid2d=="NETCDF") { //keep it synchronized with IOHandler.cc for plugin mapping!!
		cfg.getValue("NETCDF_SCHEMA", "Output", out_schema, IOUtils::nothrow); IOUtils::toUpper(out_schema);
		cfg.getValue("GRID2DPATH", "Output", out_grid2d_path);
		cfg.getValue("GRID2DFILE", "Output", grid2d_out_file);
	}

	const std::string in_meteo = cfg.get("METEO", "Input", "");
	if (in_meteo=="NETCDF") { //keep it synchronized with IOHandler.cc for plugin mapping!!
		cfg.getValue("NC_DEBUG", "INPUT", debug, IOUtils::nothrow);
		cfg.getValue("NETCDF_SCHEMA", "Input", in_schema, IOUtils::nothrow); IOUtils::toUpper(in_schema);
		cfg.getValue("NC_EXT", "INPUT", in_nc_ext, IOUtils::nothrow);

		std::vector<std::string> vecFilenames;
		cfg.getValues("METEOFILE", "INPUT", vecFilenames);
		const std::string inpath = cfg.get("METEOPATH", "Input");

		if (!vecFilenames.empty()) {
			for (size_t ii=0; ii<vecFilenames.size(); ii++) {
				const std::string filename( vecFilenames[ii] );
				const std::string extension( FileUtils::getExtension(filename) );
				const std::string file_and_path = (!extension.empty())? inpath+"/"+filename : inpath+"/"+filename+in_nc_ext;
				if (!FileUtils::validFileAndPath(file_and_path)) throw InvalidNameException(file_and_path, AT);

				cache_inmeteo_files.push_back( ncFiles(file_and_path, ncFiles::READ, cfg, in_schema, debug) );
			}
		} else { //no filenames provided, so get all the files within METEOPATH having the right extension
			bool is_recursive = false;
			cfg.getValue("METEOPATH_RECURSIVE", "Input", is_recursive, IOUtils::nothrow);
			std::list<std::string> dirlist( FileUtils::readDirectory(inpath, in_nc_ext, is_recursive) );
			dirlist.sort();
			
			for (std::list<std::string>::const_iterator it = dirlist.begin(); it != dirlist.end(); ++it) {
				if (!FileUtils::validFileAndPath( *it )) throw InvalidNameException( *it , AT);
				cache_inmeteo_files.push_back( ncFiles(inpath+"/"+*it, ncFiles::READ, cfg, in_schema, debug) );
			}
		}
		
		if (cache_inmeteo_files.empty())
				throw InvalidArgumentException("No valid input meteo files, either provide a list of files or check that there are files within METEOPATH with the NC_EXT="+in_nc_ext+" extension", AT);
		
		std::vector<std::string> vecTmp;
		cfg.getValues("STATION", "INPUT", vecTmp);
		in_stations = std::set<std::string>(vecTmp.begin(), vecTmp.end());
	}

	const std::string out_meteo = cfg.get("METEO", "Output", "");
	if (out_meteo=="NETCDF") { //keep it synchronized with IOHandler.cc for plugin mapping!!
		cfg.getValue("NETCDF_SCHEMA", "Output", out_schema, IOUtils::nothrow); IOUtils::toUpper(out_schema);
		cfg.getValue("METEOPATH", "Output", out_meteo_path);
		cfg.getValue("NC_SINGLE_FILE", "Output", out_single_file, IOUtils::nothrow);
		if (out_single_file) cfg.getValue("METEOFILE", "Output", out_meteo_file);
	}
}

void NetCDFIO::scanPath(const std::string& in_path, const std::string& nc_ext, std::vector< std::pair<std::pair<Date,Date>, ncFiles> > &meteo_files)
{
	meteo_files.clear();
	std::list<std::string> dirlist( FileUtils::readDirectory(in_path, nc_ext) );
	if (dirlist.empty()) return; //nothing to do if the directory is empty, we will transparently swap to using GRID2DFILE
	dirlist.sort();

	//Check date range in every filename and cache it
	std::list<std::string>::const_iterator it = dirlist.begin();
	while ((it != dirlist.end())) {
		const std::string filename( in_path + "/" + *it );
		if (!FileUtils::fileExists(filename)) throw AccessException(filename, AT); //prevent invalid filenames
		ncFiles file(filename, ncFiles::READ, cfg, in_schema, debug);
		meteo_files.push_back( make_pair(file.getDateRange(), file) );
		it++;
	}
	std::sort(meteo_files.begin(), meteo_files.end(), &sort_cache_grids);
}

bool NetCDFIO::list2DGrids(const Date& start, const Date& end, std::map<Date, std::set<size_t> >& list)
{
	if (cache_grid_files.empty()) scanPath(in_grid2d_path, in_nc_ext, cache_grid_files);
	if (cache_grid_files.empty()) return true; //there are no grids to read

	//TODO handle the case of file_start & file_end are undef() (example: DEM)
	for (size_t ii=0; ii<cache_grid_files.size(); ii++) {
		const Date file_start( cache_grid_files[ii].first.first );
		const Date file_end( cache_grid_files[ii].first.second );

		if (file_start > end) return true; //no more files to process (since the files are sorted in cache_grid_files)
		if (file_end < start) continue;

		//we consider that the exact same parameters are available at all time steps in the current file
		const std::set<size_t> params_set( cache_grid_files[ii].second.getParams() );
		const std::vector<Date> ts( cache_grid_files[ii].second.getTimestamps() );
		for (size_t jj=0; jj<ts.size(); jj++) {
			if (ts[jj]>end) break; //no more timestamps in the right range
			if (ts[jj]<start) continue;
			list[ ts[jj] ].insert( params_set.begin(), params_set.end() );
		}
	}

	return true;
}

void NetCDFIO::read2DGrid(Grid2DObject& grid_out, const std::string& arguments)
{
	std::vector<std::string> vec_argument;
	IOUtils::readLineToVec(arguments, vec_argument, ':');

	if (vec_argument.size() == 2) {
		ncFiles file(vec_argument[0], ncFiles::READ, cfg, in_schema, debug);
		grid_out = file.read2DGrid(vec_argument[1]);
	} else if (vec_argument.size() == 1) {
		ncFiles file(vec_argument[0], ncFiles::READ, cfg, in_schema, debug);
		grid_out = file.read2DGrid("");
	} else {
		throw InvalidArgumentException("The format for the arguments to NetCDFIO::read2DGrid is filename:varname", AT);
	}
}

void NetCDFIO::read2DGrid(Grid2DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date)
{
	if (cache_grid_files.empty()) scanPath(in_grid2d_path, in_nc_ext, cache_grid_files);

	if (!cache_grid_files.empty()) {
		for (size_t ii=0; ii<cache_grid_files.size(); ii++) {
			// Get available time period for file
			const Date file_start( cache_grid_files[ii].first.first );
			const Date file_end( cache_grid_files[ii].first.second );
			// Get available params for file
			const std::set<size_t> params_set( cache_grid_files[ii].second.getParams() );

			// Check for time period
			if (file_start > date) continue;
			if (file_end < date) continue;

			// Check for param
			if (params_set.find(parameter) == params_set.end()) continue;

			grid_out = cache_grid_files[ii].second.read2DGrid(parameter, date);
			return;
		}
		//the date/param was not found
		throw InvalidArgumentException("No Gridded data found for "+MeteoGrids::getParameterName( parameter )+" at "+date.toString(Date::ISO)+" in '"+in_grid2d_path+"'", AT);
	} else {
		const std::string filename = cfg.get("GRID2DFILE", "Input");
		if (!FileUtils::fileExists(filename)) throw NotFoundException(filename, AT); //prevent invalid filenames
		ncFiles file(filename, ncFiles::READ, cfg, in_schema, debug);
		grid_out = file.read2DGrid(parameter, date);
	}
}

void NetCDFIO::readDEM(DEMObject& dem_out)
{
	const std::string filename = cfg.get("DEMFILE", "Input");
	const std::string varname = cfg.get("DEMVAR", "Input", "");

	if (!FileUtils::fileExists(filename)) throw NotFoundException(filename, AT);
	ncFiles file(filename, ncFiles::READ, cfg, in_schema, debug);
	const Grid2DObject grid = (varname.empty())? file.read2DGrid(MeteoGrids::DEM, Date()) : file.read2DGrid(varname);
	dem_out = DEMObject( grid ); //we can not directly assign a Grid2DObject to a DEMObject
}

void NetCDFIO::write2DGrid(const Grid2DObject& grid_in, const std::string& arguments)
{
	// arguments is a string of the format varname@Date
	std::vector<std::string> vec_argument;
	if (IOUtils::readLineToVec(arguments, vec_argument, '@')  != 2)
		throw InvalidArgumentException("The format for the arguments to NetCDFIO::write2DGrid is varname@Date, received instead '"+arguments+"'", AT);

	const std::string file_and_path( out_grid2d_path + "/" + grid2d_out_file );
	if (!FileUtils::validFileAndPath(file_and_path)) {
		throw InvalidNameException("Invalid output file name '"+file_and_path+"'", AT);
	}

	mio::Date date;
	if(!mio::IOUtils::convertString(date, vec_argument[1], cfg.get("TIME_ZONE","input"))) {
		throw InvalidArgumentException("Unable to convert date '"+vec_argument[1]+"'", AT);
	}

	if (cache_grids_out.find(file_and_path) == cache_grids_out.end()) {
		const ncFiles::Mode file_mode = (FileUtils::fileExists(file_and_path))? ncFiles::READ : ncFiles::WRITE;
		cache_grids_out.insert(std::pair<std::string, ncFiles> (file_and_path,ncFiles(file_and_path, file_mode, cfg, out_schema, debug)));
	}
	cache_grids_out.at(file_and_path).write2DGrid(grid_in, IOUtils::npos, vec_argument[0], date);
}

void NetCDFIO::write2DGrid(const Grid2DObject& grid_in, const MeteoGrids::Parameters& parameter, const Date& date)
{
	const std::string file_and_path( out_grid2d_path + "/" + grid2d_out_file );
	if (!FileUtils::validFileAndPath(file_and_path)) throw InvalidNameException("Invalid output file name '"+file_and_path+"'", AT);

	if (cache_grids_out.find(file_and_path) == cache_grids_out.end()) {
		const ncFiles::Mode file_mode = (FileUtils::fileExists(file_and_path))? ncFiles::READ : ncFiles::WRITE;
		cache_grids_out.insert(std::pair<std::string, ncFiles> (file_and_path,ncFiles(file_and_path, file_mode, cfg, out_schema, debug)));
	}
	
	if (parameter==MeteoGrids::DEM || parameter==MeteoGrids::SHADE || parameter==MeteoGrids::SLOPE || parameter==MeteoGrids::AZI)
		cache_grids_out.at(file_and_path).write2DGrid(grid_in, parameter, std::string(), Date()); //do not assign a date to a DEM?
	else
		cache_grids_out.at(file_and_path).write2DGrid(grid_in, parameter, std::string(), date);
}

void NetCDFIO::writeMeteoData(const std::vector< std::vector<MeteoData> >& vecMeteo, const std::string&)
{
	if (out_single_file) {
		const std::string file_and_path( out_meteo_path + "/" + out_meteo_file );
		if (!FileUtils::validFileAndPath(file_and_path)) throw InvalidNameException("Invalid output file name '"+file_and_path+"'", AT);
		if (FileUtils::fileExists(file_and_path)) throw IOException("Appending data to timeseries is currently non-functional for NetCDF, please delete file "+file_and_path, AT);

		const ncFiles::Mode file_mode = (FileUtils::fileExists(file_and_path))? ncFiles::READ : ncFiles::WRITE;
		ncFiles file(file_and_path, file_mode, cfg, out_schema, debug);
		file.writeMeteo(vecMeteo);
	} else {
		for (size_t ii=0; ii<vecMeteo.size(); ii++) {
			if (vecMeteo[ii].empty()) continue;
			const std::string file_and_path( out_meteo_path + "/" + vecMeteo[ii].front().meta.stationID + ".nc" );
			if (!FileUtils::validFileAndPath(file_and_path)) throw InvalidNameException("Invalid output file name '"+file_and_path+"'", AT);
			if (FileUtils::fileExists(file_and_path)) throw IOException("Appending data to timeseries is currently non-functional for NetCDF, please delete file "+file_and_path, AT);

			const ncFiles::Mode file_mode = (FileUtils::fileExists(file_and_path))? ncFiles::READ : ncFiles::WRITE;
			ncFiles file(file_and_path, file_mode, cfg, out_schema, debug);
			file.writeMeteo(vecMeteo, ii);
		}
	}
}

void NetCDFIO::readStationData(const Date& /*date*/, std::vector<StationData>& vecStation)
{
	if (cache_inmeteo_files.empty()) return;

	const bool filterStations = !in_stations.empty();

	//read and append all other stations / files
	for (size_t ii=0; ii<cache_inmeteo_files.size(); ii++) {
		const std::vector<StationData> vecTmp( cache_inmeteo_files[ii].readStationData() );

		for (size_t jj=0; jj<vecTmp.size(); jj++) {
			if (filterStations && in_stations.count( vecTmp[jj].stationID )==0) continue;
			vecStation.push_back( vecTmp[jj] );
		}
	}

}

void NetCDFIO::readMeteoData(const Date& dateStart, const Date& dateEnd, std::vector< std::vector<MeteoData> >& vecMeteo)
{
	if (cache_inmeteo_files.empty()) return;

	const bool filterStations = !in_stations.empty();

	//read and append all other stations / files
	for (size_t ii=0; ii<cache_inmeteo_files.size(); ii++) {
		const std::vector< std::vector<MeteoData> > vecTmp( cache_inmeteo_files[ii].readMeteoData(dateStart, dateEnd) );

		for (size_t jj=0; jj<vecTmp.size(); jj++) {
			if (vecTmp[jj].empty()) continue;
			if (filterStations && in_stations.count( vecTmp[jj].front().getStationID() )==0) continue;
			vecMeteo.push_back( vecTmp[jj] );
		}
	}
}


///////////////////////////////////////////////////// Now the ncFiles class starts //////////////////////////////////////////
//Regarding data packing: grids are usually NOT packed (since we often write the grids just after they have been computed, so
//we don't know what the maximum range will be). Time is almost always packed.
//we have: unpacked_value = packed_value * scale + offset

ncFiles::ncFiles(const std::string& filename, const Mode& mode, const Config& cfg, const std::string& schema_name, const bool& i_debug)
             : acdd(), schema(cfg, schema_name), vars(), unknown_vars(), vecTime(), vecX(), vecY(), dimensions_map(),
               file_and_path(filename), coord_sys(), coord_param(), TZ(0.), dflt_zref(IOUtils::nodata),
               dflt_uref(IOUtils::nodata), dflt_slope(IOUtils::nodata), dflt_azi(IOUtils::nodata),
               max_unknown_param_idx(ncpp::lastdimension),
               strict_schema(false), lax_schema(false), debug(i_debug), isLatLon(false), nc_filename(std::string()), ncid(-1), keep_input_files_open(true), keep_output_files_open(false)
{
	IOUtils::getProjectionParameters(cfg, coord_sys, coord_param);

	//TODO handle these parameter in a more generic way in MeteoIO (ie outside of this plugin)
	cfg.getValue("ZREF", "Output", dflt_zref, IOUtils::nothrow);
	cfg.getValue("UREF", "Output", dflt_uref, IOUtils::nothrow);
	cfg.getValue("DEFAULT_SLOPE", "Output", dflt_slope, IOUtils::nothrow);
	cfg.getValue("DEFAULT_AZI", "Output", dflt_azi, IOUtils::nothrow);
	cfg.getValue("NC_KEEP_FILES_OPEN", "Output", keep_output_files_open, IOUtils::nothrow);
	cfg.getValue("NC_KEEP_FILES_OPEN", "Input", keep_input_files_open, IOUtils::nothrow);
	schema.initFromSchema(vars, dimensions_map);

	if (mode==WRITE) {
		cfg.getValue("NC_STRICT_SCHEMA", "Output", strict_schema, IOUtils::nothrow);
		cfg.getValue("NC_LAX_SCHEMA", "Output", lax_schema, IOUtils::nothrow);
		if (strict_schema && lax_schema)
			throw InvalidArgumentException("It is not possible to have NC_STRICT_SCHEMA and NC_LAX_SCHEMA true at the same time!", AT);
		acdd.setUserConfig( cfg, "Output" );
		if (FileUtils::fileExists(filename)) initFromFile(filename);
	} else if (mode==READ) {
		initFromFile(filename);
	}

	if (debug) {
		std::cout << filename << ":\n";
		std::cout << "\tDimensions:\n";
		for (std::map<size_t, ncpp::nc_dimension>::const_iterator it = dimensions_map.begin(); it!=dimensions_map.end(); ++it)
			std::cout << "\t\t" << it->second.toString() << "\n";
		if (!vecTime.empty()) std::cout << "\ttime range: [" << vecTime.front().toString(Date::ISO) << " - " << vecTime.back().toString(Date::ISO) << "]\n";
		std::cout << "\tVariables:\n";
		for (std::map<size_t, ncpp::nc_variable>::const_iterator it=vars.begin(); it!=vars.end(); ++it)
			std::cout << "\t\t" << ncpp::getParameterName( it->first ) << " -> " << it->second.toString() << "\n";
		std::cout << "\tUnrecognized variables:\n";
		for (std::map<std::string, ncpp::nc_variable>::const_iterator it=unknown_vars.begin(); it!=unknown_vars.end(); ++it)
			std::cout << "\t\t" << it->first << " -> " << it->second.toString() << "\n";
	}

	//check that the used schema has declared the minimum required dimensions and potentially, associated variables (if based on schema)
	const bool hasLatLon = ((dimensions_map.count(ncpp::LATITUDE)!=0 && dimensions_map.count(ncpp::LONGITUDE)!=0) && (mode!=WRITE || (vars.count(ncpp::LATITUDE)!=0 && vars.count(ncpp::LONGITUDE)!=0)));
	const bool hasEastNorth = ((dimensions_map.count(ncpp::EASTING)!=0 && dimensions_map.count(ncpp::NORTHING)!=0) && (mode!=WRITE || (vars.count(ncpp::EASTING)!=0 && vars.count(ncpp::NORTHING)!=0)));
	const bool hasTime = dimensions_map.count(ncpp::TIME)!=0 && (mode!=WRITE || vars.count(ncpp::TIME)!=0);
	if (!hasLatLon || !hasEastNorth || !hasTime) throw IOException("Error in the schema definition, some basic quantities are not defined!", AT);
}

ncFiles::ncFiles(const ncFiles& c) :
	acdd(c.acdd), schema(c.schema), vars(c.vars), unknown_vars(c.unknown_vars), vecTime(c.vecTime), vecX(c.vecX), vecY(c.vecY),
	dimensions_map(c.dimensions_map), file_and_path(c.file_and_path), coord_sys(c.coord_sys), coord_param(c.coord_param), TZ(c.TZ),
	dflt_zref(c.dflt_zref), dflt_uref(c.dflt_uref), dflt_slope(c.dflt_slope), dflt_azi(c.dflt_azi), max_unknown_param_idx(c.max_unknown_param_idx),
	strict_schema(c.strict_schema), lax_schema(c.lax_schema), debug(c.debug), isLatLon(c.isLatLon), nc_filename(c.nc_filename), ncid(-1),
	keep_input_files_open(c.keep_input_files_open), keep_output_files_open(c.keep_output_files_open)
{
	// The copy constructor ensures that the copy doesn't inherit the ncid from an opened file, to prevent trying to close the same file twice.
}

ncFiles& ncFiles::operator=(const ncFiles& source) {
	// The assignment constructor ensures that the assignment doesn't inherit the ncid from an opened file, to prevent trying to close the same file twice.
	if (this != &source) {
		acdd = source.acdd;
		schema = source.schema;
		vars = source.vars;
		unknown_vars = source.unknown_vars;
		vecTime = source.vecTime;
		vecX = source.vecX;
		vecY = source.vecY;
		dimensions_map = source.dimensions_map;
		file_and_path  = source.file_and_path;
		coord_sys = source.coord_sys;
		coord_param = source.coord_param;
		TZ = source.TZ;
		dflt_zref = source.dflt_zref;
		dflt_uref = source.dflt_uref;
		dflt_slope = source.dflt_slope;
		dflt_azi = source.dflt_azi;
		max_unknown_param_idx = source.max_unknown_param_idx;
		strict_schema = source.strict_schema;
		lax_schema = source.lax_schema;
		debug = source.debug;
		isLatLon = source.isLatLon;
		nc_filename = source.nc_filename;
		ncid = -1;
		keep_input_files_open = source.keep_input_files_open;
		keep_output_files_open = source.keep_output_files_open;
	}
	return *this;
}

ncFiles::~ncFiles()
{
	if (ncid!=-1) {
		ncpp::close_file(nc_filename, ncid);
		ncid = -1;
	}
}

//populate the dimensions_map and vars and unknown_vars from the file
void ncFiles::initFromFile(const std::string& filename)
{
	if (!FileUtils::fileExists(filename)) throw AccessException(filename, AT); //prevent invalid filenames

	if (ncid==-1) {
		ncpp::open_file(filename, NC_NOWRITE, ncid);
		nc_filename = filename;
	}

	//read the dimensions and variables
	initDimensionsFromFile();
	initVariablesFromFile();

	isLatLon = hasDimension(ncpp::LATITUDE) && hasDimension(ncpp::LONGITUDE);
	const bool isXY = hasDimension(ncpp::EASTING) && hasDimension(ncpp::NORTHING);
	if (isLatLon) {
		vecY = read_1Dvariable(ncpp::LATITUDE);
		vecX = read_1Dvariable(ncpp::LONGITUDE);
	} else if (isXY) {
		vecX = read_1Dvariable(ncpp::EASTING);
		vecY = read_1Dvariable(ncpp::NORTHING);
	}
	if (hasDimension(ncpp::TIME)) vecTime = read_1Dvariable();

	int epsg = IOUtils::inodata;
	ncpp::getGlobalAttribute(ncid, "epsg", epsg);
	if (epsg!=IOUtils::inodata) CoordsAlgorithms::EPSG_to_str(epsg, coord_sys, coord_param);

	if (!keep_input_files_open) {
		ncpp::close_file(filename, ncid);
		ncid = -1;
	}
}

std::pair<Date, Date> ncFiles::getDateRange() const
{
	if (vecTime.empty()) return make_pair( Date(), Date() );
	return make_pair( vecTime.front(), vecTime.back() );
}

std::set<size_t> ncFiles::getParams() const
{
	std::set<size_t> available_params;
	for (std::map<size_t, ncpp::nc_variable>::const_iterator it=vars.begin(); it!=vars.end(); ++it) {
		if (it->second.varid!=-1) available_params.insert( it->first );
	}

	return available_params;
}

Grid2DObject ncFiles::read2DGrid(const std::string& varname)
{
	ncpp::nc_variable var;

	if (varname.empty()) { //there must be only 1 valid variable
		for (std::map<size_t, ncpp::nc_variable>::const_iterator it = vars.begin(); it!=vars.end(); ++it) {
			if (it->second.attributes.param>=MeteoGrids::nrOfParameters) continue; //exclude associated variables
			if (it->second.varid!=-1) {
				if (!var.isUndef()) throw InvalidFormatException("Multiple variables found in file '"+file_and_path+"', please provide a variable name to read", AT);
				var = it->second;
			}
		}
		for (std::map<std::string, ncpp::nc_variable>::const_iterator it = unknown_vars.begin(); it!=unknown_vars.end(); ++it) {
			if (it->second.varid!=-1) {
				if (!var.isUndef()) throw InvalidFormatException("Multiple variables found in file '"+file_and_path+"', please provide a variable name to read", AT);
				var = it->second;
			}
		}

		if (var.isUndef()) throw NotFoundException("No variable could be found in file '"+file_and_path+"'", AT);
	} else {
		//look for variable as normal variable
		for (std::map<size_t, ncpp::nc_variable>::const_iterator it = vars.begin(); it!=vars.end(); ++it) {
			if (it->second.attributes.name==varname && it->second.varid!=-1) {
				var = it->second;
				break;
			}
		}

		//look for variable as unknown variable
		if (var.isUndef()) {
			for (std::map<std::string, ncpp::nc_variable>::const_iterator it = unknown_vars.begin(); it!=unknown_vars.end(); ++it) {
				if (it->first==varname && it->second.varid!=-1) {
					var = it->second;
					break;
				}
			}
		}

		if (var.isUndef()) throw NotFoundException("The variable '"+varname+"' could not be found in file '"+file_and_path+"'", AT);
	}

	//now check that the variable has no dependency on time or there is only 1 time step
	const std::map<size_t, ncpp::nc_dimension>::const_iterator it2 = dimensions_map.find(ncpp::TIME);
	const int time_id = (it2!=dimensions_map.end())? it2->second.dimid : -1;
	const bool depend_on_time = (std::find(var.dimids.begin(), var.dimids.end(), time_id) != var.dimids.end());
	if (depend_on_time && vecTime.size()>1) //if only one timestep is present, we take it otherwise, we throw
		throw InvalidFormatException("No time requirement has been provided for a file that contains multiple timestamps", AT);

	return read2DGrid(var, IOUtils::npos);
}

Grid2DObject ncFiles::read2DGrid(const size_t& param, const Date& date)
{
	const std::map <size_t, ncpp::nc_variable>::const_iterator it = vars.find( param );
	if (it==vars.end() || it->second.varid==-1)
		NoDataException("No "+MeteoGrids::getParameterName( param )+" grid in file "+file_and_path, AT);

	size_t time_pos = IOUtils::npos;
	if (!date.isUndef()) {
		const std::vector<Date>::iterator low = std::lower_bound(vecTime.begin(), vecTime.end(), date);
		if (*low!=date) throw NoDataException("No data at "+date.toString(Date::ISO)+" in file "+file_and_path, AT);
		time_pos = static_cast<size_t>( std::distance(vecTime.begin(), low) );
	} else {
		//no date has been provided, check if this parameter depends on time
		const std::map<size_t, ncpp::nc_dimension>::const_iterator it2 = dimensions_map.find(ncpp::TIME);
		const int time_id = (it2!=dimensions_map.end())? it2->second.dimid : -1;
		const bool depend_on_time = (std::find(it->second.dimids.begin(), it->second.dimids.end(), time_id) != it->second.dimids.end());
		if (depend_on_time && vecTime.size()>1) //if only one timestep is present, we take it, otherwise we throw
			throw InvalidFormatException("No time requirement has been provided for a file that contains multiple timestamps", AT);
	}

	const bool isPrecip = (param==MeteoGrids::PSUM || param==MeteoGrids::PSUM_L || param==MeteoGrids::PSUM_S);
	const bool isRad = (param==MeteoGrids::ISWR || param==MeteoGrids::RSWR || param==MeteoGrids::ISWR_DIFF || param==MeteoGrids::ISWR_DIR);
	return read2DGrid(it->second, time_pos, isPrecip, (isPrecip || isRad));
}

Grid2DObject ncFiles::read2DGrid(const ncpp::nc_variable& var, const size_t& time_pos, const bool& m2mm, const bool& reZero)
{
	if (isLatLon && (!hasDimension(ncpp::LATITUDE) || !hasDimension(ncpp::LONGITUDE))) throw IOException("No latitude / longitude could be identified in file "+file_and_path, AT);
	if (!isLatLon && (!hasDimension(ncpp::EASTING) || !hasDimension(ncpp::NORTHING))) throw IOException("No easting / northing could be identified in file "+file_and_path, AT);

	//define the results grid
	Grid2DObject grid;
	if (isLatLon) { //the reprojection (if necessary) will be handled by GridsManager
		mio::Coords llcorner(coord_sys, coord_param);
		llcorner.setLatLon( std::min(vecY.front(), vecY.back()), std::min(vecX.front(), vecX.back()), IOUtils::nodata);
		grid.set(vecX.size(), vecY.size(), IOUtils::nodata, llcorner);
		IOInterface::set2DGridLatLon(grid, std::max(vecY.front(), vecY.back()), std::max(vecX.front(), vecX.back()));
	} else {
		mio::Coords llcorner(coord_sys, coord_param);
		llcorner.setXY( std::min(vecX.front(), vecX.back()), std::min(vecY.front(), vecY.back()), IOUtils::nodata);
		const double cellsize = IOInterface::computeGridXYCellsize(vecX, vecY);
		grid.set(vecX.size(), vecY.size(), cellsize, llcorner);
	}

	//read the raw data, copy it into the Grid2DObject
	if (ncid==-1) {
		ncpp::open_file(file_and_path, NC_NOWRITE, ncid);
		nc_filename = file_and_path;
	}
	double *data = new double[vecY.size()*vecX.size()];
	if (time_pos!=IOUtils::npos)
		ncpp::read_data(ncid, var, time_pos, vecY.size(), vecX.size(), data);
	else
		ncpp::read_data(ncid, var, data);
	ncpp::fill2DGrid(grid, data, var.nodata, (vecX.front()<=vecX.back()), (vecY.front()<=vecY.back()) );
	delete[] data;
	if (!keep_input_files_open) {
		ncpp::close_file(file_and_path, ncid);
		ncid = -1;
	}

	//handle data packing and units, if necessary
	if (var.scale!=1.) grid *= var.scale;
	if (var.offset!=0.) grid += var.offset;
	applyUnits(grid, var.attributes.units, time_pos, m2mm);
	if (reZero) {//reset very low values to zero
		for (size_t ii=0; ii<grid.size(); ii++)
			if (grid(ii)<1e-6 && grid(ii)!=mio::IOUtils::nodata) grid(ii)=0.;
	}

	return grid;
}

//this should be most often used as wrapper, to select the proper parameters for a given param or param_name
//If both are provided, param has the priority
void ncFiles::write2DGrid(const Grid2DObject& grid_in, size_t param, std::string param_name, const Date& date)
{
	if (!param_name.empty() && param==IOUtils::npos) param = MeteoGrids::getParameterIndex( param_name );

	if (param!=IOUtils::npos && vars.count( param )>0) {
		write2DGrid(grid_in, vars[param], date);
	} else {
		if (param!=IOUtils::npos && param_name.empty()) param_name = MeteoGrids::getParameterName(param);
		ncpp::var_attr tmp_attr( schema.getSchemaAttributes(param) );
		if (tmp_attr.name.empty()) tmp_attr.name = param_name; //ie it was not found
		ncpp::nc_variable tmp_var(tmp_attr, schema.nodata);
		if (unknown_vars.count( param_name )>0) tmp_var = unknown_vars[ param_name ];

		write2DGrid(grid_in, tmp_var, date);
		unknown_vars[ param_name ] = tmp_var; //save varid, etc for next calls
	}
}

void ncFiles::write2DGrid(const Grid2DObject& grid_in, ncpp::nc_variable& var, const Date& date)
{
	if ( FileUtils::fileExists(file_and_path) ) {
		if (ncid==-1) {
			ncpp::open_file(file_and_path, NC_WRITE, ncid);
			nc_filename = file_and_path;
		}
		ncpp::file_redef(file_and_path, ncid);
	} else {
		if (!FileUtils::validFileAndPath(file_and_path)) throw InvalidNameException(file_and_path, AT);
		if (ncid==-1) {
			ncpp::create_file(file_and_path, NC_CLASSIC_MODEL, ncid);
			nc_filename = file_and_path;
		}
		writeGridMetadataHeader(grid_in);
	}

	//create any potentially missing definition, otherwise check that everything is consistent
	std::vector<size_t> nc_variables, dimensions;
	if (!date.isUndef()) dimensions.push_back( ncpp::TIME );
	if (isLatLon) {
		dimensions.push_back( ncpp::LATITUDE );
		dimensions.push_back( ncpp::LONGITUDE );
	} else {
		dimensions.push_back( ncpp::NORTHING );
		dimensions.push_back( ncpp::EASTING );
	}

	for (size_t ii=0; ii<dimensions.size(); ii++) {
		const size_t param = dimensions[ii];
		size_t length = 0;
		if (param==ncpp::LATITUDE || param==ncpp::NORTHING) length = grid_in.getNy();
		if (param==ncpp::LONGITUDE || param==ncpp::EASTING) length = grid_in.getNx();
		ncpp::createDimension(ncid, dimensions_map[ param ], length);
		if (setAssociatedVariable(param, date)) nc_variables.push_back( param ); //associated variable will have to be filled
		if (var.varid == -1) var.dimids.push_back( dimensions_map[param].dimid );
	}
	if (var.varid == -1) ncpp::create_variable(ncid, var); //create the "main" variable if necessary
	nc_variables.push_back( var.attributes.param );
	vars[ var.attributes.param ] = var;

	ncpp::end_definitions(file_and_path, ncid);

	//now write the data
	const size_t time_pos = (!date.isUndef())? addTimestamp(date) : IOUtils::npos;
	for (size_t ii=0; ii<nc_variables.size(); ii++) {
		const size_t param = nc_variables[ii];
		if (param==ncpp::TIME) continue; //this was done above

		const std::vector<double> data( fillBufferForVar(grid_in, vars[ param ]) );
		if (data.empty()) continue;

		if (vars[ param ].dimids.size()>0 && vars[ param ].dimids.front()==ncpp::TIME) { //as unlimited dimension, TIME is always first
			ncpp::write_data(ncid, vars[ param ], time_pos, grid_in.getNy(), grid_in.getNx(), &data[0]);
		} else {
			if (param==ncpp::LATITUDE || param==ncpp::NORTHING)
				ncpp::write_1Ddata(ncid, vars[ param ], data);
			else if (param==ncpp::LONGITUDE || param==ncpp::EASTING)
				ncpp::write_1Ddata(ncid, vars[ param ], data);
			else
				ncpp::write_data(ncid, vars[ param ], time_pos, grid_in.getNy(), grid_in.getNx(), &data[0]);
		}
	}

	if (!keep_output_files_open) {
		ncpp::close_file(file_and_path, ncid);
		ncid = -1;
	}
}

//When writing multiple stations in one file, this assumes that they all have the same parameters, the same timestamps and the same coordinate system
//if station_idx==IOUtils::npos, the user has requested all stations in one file
void ncFiles::writeMeteo(const std::vector< std::vector<MeteoData> >& vecMeteo, const size_t& station_idx)
{
	const bool station_dimension = (station_idx==IOUtils::npos) || schema.force_station_dimension;
	const size_t ref_station_idx = (station_idx==IOUtils::npos)? 0 : station_idx;
	if (vecMeteo.empty()) return;
	if (vecMeteo[ref_station_idx].empty()) return;
	isLatLon = true; //for now, we force lat/lon coordinates for time series

	if ( FileUtils::fileExists(file_and_path) ) {
		if (ncid==-1) {
			ncpp::open_file(file_and_path, NC_WRITE, ncid);
			nc_filename = file_and_path;
		}
		ncpp::file_redef(file_and_path, ncid);
	} else {
		if (!FileUtils::validFileAndPath(file_and_path)) throw InvalidNameException(file_and_path, AT);
		if (ncid==-1) {
			ncpp::create_file(file_and_path, NC_CLASSIC_MODEL, ncid);
			nc_filename = file_and_path;
		}
		writeMeteoMetadataHeader(vecMeteo, station_idx);
	}

	std::vector<size_t> nc_variables, dimensions;
	dimensions.push_back( ncpp::TIME );
	vecTime = createCommonTimeBase(vecMeteo, station_idx); //create a common time base for all content of this file
	if (station_dimension) {
		dimensions.push_back( ncpp::STATSTRLEN ); //this MUST be before STATION
		dimensions.push_back( ncpp::STATION );
	}
	const Date ref_date( getRefDate(vecMeteo, station_idx) );
	const size_t nrStations = (station_idx==IOUtils::npos)? vecMeteo.size() : 1; //only if station_idx==IOUtils::npos, has the user requested all stations in one file
	for (size_t ii=0; ii<dimensions.size(); ii++) {
		const size_t param = dimensions[ii];
		const size_t length = (param==ncpp::TIME)? 0 : ((param==ncpp::STATSTRLEN)? DFLT_STAT_STR_LEN : nrStations) ;
		ncpp::createDimension(ncid, dimensions_map[ param ], length);

		if (param==ncpp::STATSTRLEN) continue; //no associated variable for STATSTRLEN
		if (!station_dimension && param==ncpp::STATION) continue;
		if (setAssociatedVariable(param, ref_date)) nc_variables.push_back( dimensions[ii] ); //associated variable will have to be filled
	}

	appendVariablesList(nc_variables, vecMeteo, station_idx);

	//associate dimids to vars and create the variables
	for (size_t ii=0; ii<nc_variables.size(); ii++) {
		const size_t param = nc_variables[ii];
		if (vars[ param ].varid == -1) { //skip existing nc_variables
			const bool varIsDEM = (param==MeteoGrids::DEM || param==MeteoGrids::SLOPE || param==MeteoGrids::AZI);
			const bool varIsDimension = (param>=ncpp::firstdimension && param<=ncpp::lastdimension);
			if (!varIsDimension && !varIsDEM) vars[ param ].dimids.push_back( dimensions_map[ncpp::TIME].dimid );
			if (station_dimension) vars[ param ].dimids.push_back( dimensions_map[ncpp::STATION].dimid );
			if (param==ncpp::STATION) vars[ param ].dimids.push_back( dimensions_map[ncpp::STATSTRLEN].dimid );

			ncpp::create_variable(ncid, vars[ param ]);
		}
	}

	ncpp::end_definitions(file_and_path, ncid);

	//write data: fill associated nc_variables and normal nc_variables
	for (size_t ii=0; ii<nc_variables.size(); ii++) {
		const size_t param = nc_variables[ii];

		if (param==ncpp::STATION) { //this is not present if !station_dimension
			if (station_idx==IOUtils::npos) { //writing all stations into one file
				std::vector<std::string> txtdata( vecMeteo.size() );
				for (size_t jj=0; jj<vecMeteo.size(); jj++) txtdata[jj] = vecMeteo[jj].front().meta.stationID.substr(0, DFLT_STAT_STR_LEN-1);
				ncpp::write_1Ddata(ncid, vars[param], txtdata, DFLT_STAT_STR_LEN);
			} else { //only one station per file
				std::vector<std::string> txtdata( 1, vecMeteo[station_idx].front().meta.stationID.substr(0, DFLT_STAT_STR_LEN-1));
				ncpp::write_1Ddata(ncid, vars[param], txtdata, DFLT_STAT_STR_LEN);
			}
		} else {
			const std::vector<double> data( fillBufferForVar(vecMeteo, station_idx, vars[ param ]) );
			if (data.empty()) continue;
			ncpp::write_1Ddata(ncid, vars[ param ], data, (param==ncpp::TIME));
		}
	}

	if (!keep_output_files_open) {
		ncpp::close_file(file_and_path, ncid);
		ncid = -1;
	}
}

void ncFiles::writeGridMetadataHeader(const Grid2DObject& grid_in)
{
	acdd.addAttribute("Conventions", schema.name+",ACDD-1.3");
	if (schema.name=="CF-1.6") acdd.addAttribute("standard_name_vocabulary", "CF-1.6");
	acdd.addAttribute("cdm_data_type", "Grid");
	acdd.addAttribute("title", "Gridded data for various parameters and timesteps");
	acdd.setGeometry(grid_in, isLatLon);

	acdd.writeAttributes(ncid);
}

void ncFiles::writeMeteoMetadataHeader(const std::vector< std::vector<MeteoData> >& vecMeteo, const size_t& station_idx)
{
	acdd.addAttribute("Conventions", schema.name+",ACDD-1.3");
	if (schema.name=="CF-1.6") acdd.addAttribute("standard_name_vocabulary", "CF-1.6");
	acdd.addAttribute("cdm_data_type", "Station");
	acdd.addAttribute("featureType", "timeSeries");
	acdd.addAttribute("keywords", "Time series analysis", "", ACDD::APPEND);

	if (station_idx==IOUtils::npos) { //multiple stations per file
		if (vecMeteo.size()<30) {
			std::string stats_list( vecMeteo[0].front().meta.stationID );
			for (size_t ii=1; ii<vecMeteo.size(); ii++) {
				stats_list = stats_list + ", " + vecMeteo[ii].front().meta.stationID;
			}
			if (stats_list.length()<=140)
				acdd.addAttribute("title", "Meteorological data timeseries for stations "+stats_list);
			else
				acdd.addAttribute("title", "Meteorological data timeseries for multiple stations");
		} else {
			acdd.addAttribute("title", "Meteorological data timeseries for multiple stations");
		}
		acdd.setGeometry(vecMeteo, isLatLon);
		acdd.setTimeCoverage(vecMeteo);
	} else { //one station per file
		const std::string stationName( vecMeteo[station_idx].front().meta.stationName );
		const std::string name = (!stationName.empty())? stationName : vecMeteo[station_idx].front().meta.stationID;
		acdd.addAttribute("title", "Meteorological data timeseries for the "+name+" station");
		acdd.addAttribute("station_name", name);
		acdd.addAttribute("station_id", vecMeteo[station_idx].front().meta.stationID);
		acdd.setGeometry( vecMeteo[station_idx].front().meta.position, isLatLon );
		acdd.setTimeCoverage(vecMeteo[station_idx]);
	}

	acdd.writeAttributes(ncid);
}

std::vector<StationData> ncFiles::readStationData()
{
	std::vector<StationData> vecStation;

	const bool hasLatLon = (hasVariable(ncpp::LONGITUDE) && hasVariable(ncpp::LATITUDE));
	const bool hasEastNorth = (hasVariable(ncpp::EASTING) && hasVariable(ncpp::NORTHING));
	if (!hasVariable(MeteoGrids::DEM) || (!hasLatLon && !hasEastNorth))
		throw InvalidFormatException("No station geolocalization found in file "+file_and_path, AT);

	if (hasDimension(ncpp::STATION)) { //multiple stations per file or one station but still with STATION dimension
		if (ncid==-1) {
			ncpp::open_file(file_and_path, NC_NOWRITE, ncid);
			nc_filename = file_and_path;
		}

		const std::vector<double> vecAlt( read_1Dvariable(MeteoGrids::DEM) );
		const size_t nrStations = vecAlt.size();
		const std::vector<double> vecSlope = (hasVariable(MeteoGrids::SLOPE))? read_1Dvariable(MeteoGrids::SLOPE) : std::vector<double>();
		const std::vector<double> vecAzi = (hasVariable(MeteoGrids::AZI))? read_1Dvariable(MeteoGrids::AZI) : std::vector<double>(); //HACK to be read, they must be schema vars
		const bool hasSlope = (!vecSlope.empty());
		if (hasSlope && (vecSlope.size()!=nrStations || vecAzi.size()!=nrStations))
			throw InvalidFormatException("Vectors of altitudes, slopes and azimuths don't match in file "+file_and_path, AT);

		std::vector<Coords> vecPosition( nrStations, Coords(coord_sys, coord_param) );
		if (hasLatLon) {
			const std::vector<double> vecLat( read_1Dvariable(ncpp::LATITUDE) );
			const std::vector<double> vecLon( read_1Dvariable(ncpp::LONGITUDE) );
			if (vecLat.size()!=nrStations || vecLon.size()!=nrStations)
				throw InvalidFormatException("Vectors of altitudes, latitudes and longitudes don't match in file "+file_and_path, AT);

			for (size_t ii=0; ii<nrStations; ii++) vecPosition[ii].setLatLon(vecLat[ii], vecLon[ii], vecAlt[ii]);
		} else {
			const std::vector<double> vecEast( read_1Dvariable(ncpp::EASTING) );
			const std::vector<double> vecNorth( read_1Dvariable(ncpp::NORTHING) );
			if (vecEast.size()!=nrStations || vecNorth.size()!=nrStations)
				throw InvalidFormatException("Vectors of altitudes, eastings and northings don't match in file "+file_and_path, AT);

			for (size_t ii=0; ii<nrStations; ii++) vecPosition[ii].setXY(vecEast[ii], vecNorth[ii], vecAlt[ii]);
		}

		const std::vector<std::string> vecIDs( read_stationIDs() );
		vecStation.resize( nrStations );
		for (size_t ii=0; ii<nrStations; ii++) {
			StationData sd(vecPosition[ii], vecIDs[ii], vecIDs[ii]);
			if (hasSlope) sd.setSlope(vecSlope[ii], vecAzi[ii]);
			vecStation[ii] = sd;
		}

		if (!keep_input_files_open) {
			ncpp::close_file(file_and_path, ncid);
			ncid = -1;
		}

	} else { //only one station, no station dimension
		if (ncid==-1) {
			ncpp::open_file(file_and_path, NC_NOWRITE, ncid);
			nc_filename = file_and_path;
		}
		const double alt = read_0Dvariable(MeteoGrids::DEM);
		const double slope = (hasVariable(MeteoGrids::SLOPE))? read_0Dvariable(MeteoGrids::SLOPE) : IOUtils::nodata;
		const double azi = (hasVariable(MeteoGrids::AZI))? read_0Dvariable(MeteoGrids::AZI) : IOUtils::nodata;
		std::string stationID, stationName;
		ncpp::getGlobalAttribute(ncid, "station_id", stationID);
		ncpp::getGlobalAttribute(ncid, "station_name", stationName);
		if (stationName.empty()) stationName = FileUtils::removeExtension( FileUtils::getFilename(file_and_path) );
		if (stationID.empty()) stationID = stationName;
		
		Coords position;
		if (hasLatLon) {
			const double lat = read_0Dvariable(ncpp::LATITUDE);
			const double lon = read_0Dvariable(ncpp::LONGITUDE);
			position.setLatLon(lat, lon, alt);
		} else {
			const double easting = read_0Dvariable(ncpp::EASTING);
			const double northing = read_0Dvariable(ncpp::NORTHING);
			position.setXY(easting, northing, alt);
		}
		StationData sd(position, stationID, stationName);
		sd.setSlope(slope, azi);
		vecStation.push_back( sd );

		if (!keep_input_files_open) {
			ncpp::close_file(file_and_path, ncid);
			ncid = -1;
		}
	}

	return vecStation;
}

//build a list of valid timeseries to read in the current file as a vector of (Parameter, mio name)
std::vector< std::pair<size_t, std::string> > ncFiles::getTSParameters() const
{
	//get the dimensions that are required for timeseries
	int time_dim=IOUtils::inodata, station_dim=IOUtils::inodata;
	for (std::map<size_t, ncpp::nc_dimension>::const_iterator it=dimensions_map.begin(); it!=dimensions_map.end(); ++it) {
		if (it->second.dimid==-1) continue;
		if (it->second.param==ncpp::TIME) time_dim = it->second.dimid;
		if (it->second.param==ncpp::STATION) station_dim = it->second.dimid;
	}
	if (time_dim==IOUtils::inodata) throw InvalidFormatException("No valid time dimension could be found in file "+file_and_path, AT);

	//get the variables that only depend on the found dimensions in the schema vars
	std::vector< std::pair<size_t, std::string> > results;
	for (std::map<size_t, ncpp::nc_variable>::const_iterator it=vars.begin(); it!=vars.end(); ++it) {
		if (it->second.varid<0) continue;
		if (it->second.attributes.param==ncpp::TIME) continue;

		bool has_time = false, has_station=false;
		for (size_t ii=0; ii<it->second.dimids.size(); ii++) {
			if (it->second.dimids[ii]==time_dim) {
				has_time = true;
			} else if (station_dim!=IOUtils::inodata && it->second.dimids[ii]==station_dim) {
				has_station = true;
			}
		}

		if (has_time && (station_dim==IOUtils::inodata || (station_dim!=IOUtils::inodata && has_station))) {
			results.push_back( std::make_pair(it->first, ncpp::getParameterName( it->first )) );
		}
	}

	//same as above but in the unknown_vars
	for (std::map<std::string, ncpp::nc_variable>::const_iterator it=unknown_vars.begin(); it!=unknown_vars.end(); ++it) {
		bool has_time = false, has_station=false;
		for (size_t ii=0; ii<it->second.dimids.size(); ii++) {
			if (it->second.dimids[ii]==time_dim) {
				has_time = true;
			} else if (station_dim!=IOUtils::inodata && it->second.dimids[ii]==station_dim) {
				has_station = true;
			}
		}

		if (has_time && (station_dim==IOUtils::inodata || (station_dim!=IOUtils::inodata && has_station))) {
			results.push_back( std::make_pair(IOUtils::npos, it->second.attributes.name) );
		}
	}

	return results;
}

std::vector< std::vector<MeteoData> > ncFiles::readMeteoData(const Date& dateStart, const Date& dateEnd)
{
	const std::vector<StationData> vecStation = readStationData();
	const size_t nrStations = vecStation.size();
	if (nrStations==0 || vecTime.empty()) return std::vector< std::vector<MeteoData> >();

	if (ncid==-1) {
		ncpp::open_file(file_and_path, NC_NOWRITE, ncid);
		nc_filename = file_and_path;
	}

	//the time has been read in the constructor, but we must find the section of interest for the current call
	size_t start_idx=0, end_idx=0;
	for (size_t ii=0; ii<vecTime.size(); ii++) {
		if (vecTime[ii]<dateStart) start_idx++;
		end_idx++;
		if (vecTime[ii]>dateEnd) break;
	}
	if (start_idx==vecTime.size() || end_idx==0) { //the data is either after or before the requested period
		if (!keep_input_files_open) {
			ncpp::close_file(file_and_path, ncid);
			ncid = -1;
		}
		return std::vector< std::vector<MeteoData> >(nrStations);
	}

	//list all the parameters that appear to be timeseries
	const std::vector< std::pair<size_t, std::string> > tsParams = getTSParameters();

	//build a MeteoData template (all the stations share the same parameters)
	MeteoData mdGeneric;
	for (size_t ii=0; ii<tsParams.size(); ii++) {
		if (!mdGeneric.param_exists(tsParams[ii].second))
			mdGeneric.addParameter( tsParams[ii].second );
	}

	//now populate the vector of vectors with the metadata and time steps
	const size_t nrSteps = end_idx - start_idx;
	std::vector< std::vector<MeteoData> > vecMeteo(nrStations, std::vector<MeteoData>(nrSteps, mdGeneric));
	for (size_t st=0; st<nrStations; st++) {
		for (size_t ii=start_idx; ii<end_idx; ii++) {
			vecMeteo[st][ii-start_idx].meta = vecStation[st];
			vecMeteo[st][ii-start_idx].date = vecTime[ii];
		}
	}

	//populate the vectors with the variables' values
	const bool multiple_stations_per_file = hasDimension(ncpp::STATION);
	for (size_t ii=0; ii<tsParams.size(); ii++) {
		const size_t parindex = tsParams[ii].first;
		const std::string parname( tsParams[ii].second );
		const bool fromSchema = (parindex!=IOUtils::npos);
		const int varid = (fromSchema)? vars[ parindex ].varid : unknown_vars[ parname ].varid;

		//get the data for this variables for all stations but only the requested timesteps
		double *data = (double*)calloc(nrStations, sizeof(double)*nrSteps);
		int status;
		if (!multiple_stations_per_file) {
			const size_t start[] = {start_idx};
			const size_t count[] = {nrSteps};
			status = nc_get_vara_double(ncid, varid, start, count, data);
		} else {
			const size_t start[] = {start_idx, 0};
			const size_t count[] = {nrSteps, nrStations};
			status = nc_get_vara_double(ncid, varid, start, count, data);
		}
		if (status != NC_NOERR) {
			free(data);
			throw mio::IOException("Could not retrieve data for variable '" + parname + "': " + nc_strerror(status), AT);
		}

		const double scale = (fromSchema)? vars[ parindex ].scale : unknown_vars[ parname ].scale;
		const double offset = (fromSchema)? vars[ parindex ].offset : unknown_vars[ parname ].offset;
		const double nodata = (fromSchema)? vars[ parindex ].nodata : unknown_vars[ parname ].nodata;
		const std::string units( (fromSchema)? vars[ parindex ].attributes.units : "" );
		const bool isPrecip = (fromSchema)? (parindex==MeteoGrids::PSUM || parindex==MeteoGrids::PSUM_S || parindex==MeteoGrids::PSUM_L) : false;
		const bool isPrecipIntensity = (isPrecip)? units=="kg/m2/s" : false;
		if (isPrecipIntensity) { //in this case, we need to accumulate the precipitation
			for (size_t st=0; st<nrStations; st++) {
				for (size_t jj=0; jj<nrSteps; jj++) {
					const double curr_julian = vecMeteo[st][jj].date.getJulian(true);
					//trick: in order to get an accumulation period at start, we take the one from the next timestep and assume they are the same
					const double ts_duration_s = ((jj>0)? (curr_julian - vecMeteo[st][jj-1].date.getJulian(true)) : (vecMeteo[st][jj+1].date.getJulian(true) - curr_julian) ) * (24.*3600.);

					const double tmp( data[st + jj*nrStations] );
					if (tmp!=nodata) vecMeteo[st][jj](parname) = ((tmp * scale) + offset) * ts_duration_s;
				}
			}
		} else  {
			for (size_t st=0; st<nrStations; st++) {
				for (size_t jj=0; jj<nrSteps; jj++) {
					const double tmp( data[st + jj*nrStations] );
					if (tmp!=nodata) vecMeteo[st][jj](parname) = (tmp * scale) + offset;
				}
			}
		}
		free(data);

		applyUnits(vecMeteo, nrStations, nrSteps, units, parname);
	}

	return vecMeteo;
}

Date ncFiles::getRefDate(const std::vector< std::vector<MeteoData> >& vecMeteo, const size_t& station_idx)
{
	if (station_idx==IOUtils::npos) { //multiple stations per file
		Date refDate;
		for (size_t ii=0; ii<vecMeteo.size(); ii++) {
			if (vecMeteo[ii].empty()) continue;
			if (refDate.isUndef()) refDate = vecMeteo[ii].front().date;
			if (vecMeteo[ii].front().date<refDate) refDate = vecMeteo[ii].front().date;
		}
		return refDate;
	} else { //one station per file
		if (vecMeteo[station_idx].empty()) return Date();
		return vecMeteo[station_idx].front().date;
	}
}

std::vector<Date> ncFiles::createCommonTimeBase(const std::vector< std::vector<MeteoData> >& vecMeteo, const size_t& station_idx)
{
	if (station_idx==IOUtils::npos) { //all stations into one file
		std::set<Date> tmp; //a set is sorted and contains unique values
		for (size_t st=0; st<vecMeteo.size(); st++) {
			for (size_t ii=0; ii<vecMeteo[st].size(); ii++)
				tmp.insert( vecMeteo[st][ii].date );
		}

		const std::vector<Date> result(tmp.begin(), tmp.end());
		return result;
	} else { //one file per station
		std::vector<Date> result(vecMeteo[station_idx].size());
		for (size_t ii=0; ii<vecMeteo[station_idx].size(); ii++) result[ii] = vecMeteo[station_idx][ii].date;
		return result;
	}
}

void ncFiles::pushVar(std::vector<size_t> &nc_variables, const size_t& param)
{
	if (std::find(nc_variables.begin(), nc_variables.end(), param) == nc_variables.end())
		nc_variables.push_back( param );
}

//add an out-of-schema parameter to the vars map (if necessary)
size_t ncFiles::addToVars(const size_t& param)
{
	if (vars.count(param)==0) { //ie unrecognized in loaded schema, adding it
		const std::string varname( ncpp::getParameterName(param) );
		const std::string long_name( ncpp::getParameterDescription(param) );
		const std::string units( ncpp::getParameterUnits(param) );

		const ncpp::var_attr tmp_attr(param, varname, "", long_name, units, IOUtils::nodata, schema.dflt_type);
		vars[param] = ncpp::nc_variable(tmp_attr, schema.nodata);
	}

	return param;
}

size_t ncFiles::addToVars(const std::string& name)
{
	const size_t param = getParameterIndex( name );

	if (vars.count(param)==0) { //ie unrecognized in loaded schema, adding it
		if (strict_schema) return IOUtils::npos;
		if (param<=ncpp::lastdimension) {
			const std::string varname( ncpp::getParameterName(param) );
			const std::string long_name( ncpp::getParameterDescription(param) );
			const std::string units( ncpp::getParameterUnits(param) );

			const ncpp::var_attr tmp_attr(param, varname, "", long_name, units, IOUtils::nodata, schema.dflt_type);
			vars[param] = ncpp::nc_variable(tmp_attr, schema.nodata);
		} else if (lax_schema) { //non-standard parameter, not even in MeteoGrids
			const ncpp::var_attr tmp_attr(param, name, "", "Non-standard", "", IOUtils::nodata, schema.dflt_type);
			vars[param] = ncpp::nc_variable(tmp_attr, schema.nodata);
		} else {
			return IOUtils::npos;
		}
	}

	return param;
}

//if station_idx==IOUtils::npos, the user has requested all stations in one file
void ncFiles::appendVariablesList(std::vector<size_t> &nc_variables, const std::vector< std::vector<MeteoData> >& vecMeteo, const size_t& station_idx)
{
	//add metadata variables
	pushVar(nc_variables, MeteoGrids::DEM);
	if (isLatLon) {
		pushVar(nc_variables, ncpp::LATITUDE );
		pushVar(nc_variables, ncpp::LONGITUDE );
	} else {
		pushVar(nc_variables, ncpp::EASTING );
		pushVar(nc_variables, ncpp::NORTHING );
	}
	pushVar(nc_variables, addToVars( MeteoGrids::SLOPE) );
	pushVar(nc_variables, addToVars( MeteoGrids::AZI ) );
	if (dflt_zref!=IOUtils::nodata || dflt_uref!=IOUtils::nodata) { //This is required by Crocus and we don't have any better solution for now...
		pushVar(nc_variables, addToVars( ncpp::ZREF ) );
		pushVar(nc_variables, addToVars( ncpp::UREF ) );
	}

	//add all vars found in vecMeteo
	const size_t st_start = (station_idx==IOUtils::npos)? 0 : station_idx;
	const size_t st_end = (station_idx==IOUtils::npos)? vecMeteo.size() : station_idx+1;
	for (size_t st=st_start; st<st_end; st++) {
		const std::set<std::string> parameters( MeteoData::listAvailableParameters( vecMeteo[st] ));

		for (std::set<std::string>::const_iterator it=parameters.begin(); it!=parameters.end(); ++it) {
			const size_t param = addToVars( *it );
			if (param!=IOUtils::npos) pushVar(nc_variables, param);
		}
	}
}

//if returning TRUE, the variable must be created
//NOTE the scale parameter is used as a divisor for TIME
bool ncFiles::setAssociatedVariable(const size_t& param, const Date& ref_date)
{
	if (vars[ param ].varid == -1) {
		vars[ param ].dimids.push_back( dimensions_map[ param ].dimid );
		if (param==ncpp::STATION) vars[ param ].dimids.push_back( dimensions_map[ ncpp::STATSTRLEN ].dimid );
		if (param==ncpp::TIME) {
			int year, month, day;
			ref_date.getDate(year, month, day, true); //force GMT
			const Date ref_date_simplified(year, month, day, 0, 0, 0.); //force writing to GMT
			std::string date_str( ref_date_simplified.toString(Date::ISO) );
			date_str[ 10 ] = ' '; //replace "T" by " "

			const std::string units( vars[ param ].attributes.units );
			if (units=="d") {
				vars[param].attributes.units = "days since " + date_str;
				vars[param].scale = 1.;
			} else if (units=="h") {
				vars[param].attributes.units = "hours since " + date_str;
				vars[param].scale = 24.;
			} else if (units=="min") {
				vars[param].attributes.units = "minutes since " + date_str;
				vars[param].scale = (24.*60);
			} else if (units=="s") {
				vars[param].attributes.units = "seconds since " + date_str;
				vars[param].scale = (24.*3600.);
			} else
				throw InvalidArgumentException("Unsupported time unit specified in schema: '"+units+"'", AT);
			vars[param].offset = ref_date_simplified.getJulian(true);
		}

		ncpp::create_variable(ncid, vars[ param ]);
		return true;
	}
	return false;
}

//This function computes the NetCDF representation of time, rounded to the given precision. It is inlined for performance reasons.
inline double transformTime(const double& julian, const double& offset, const double& scale, const double& precision)
{
	const double packed_julian = (julian - offset) * scale; //the scale parameter is used as a divisor for TIME
	double integral;
	const double fractional = modf(packed_julian-.5, &integral);
	const double rounded_time = integral + (double)Optim::round( fractional/precision ) * precision + .5;
	return rounded_time;
}

const std::vector<double> ncFiles::fillBufferForAssociatedVar(const std::vector< std::vector<MeteoData> >& vecMeteo, const size_t& station_idx, const ncpp::nc_variable& var) const
{
	const size_t param = var.attributes.param;
	const size_t st_start = (station_idx==IOUtils::npos)? 0 : station_idx;
	const size_t st_end = (station_idx==IOUtils::npos)? vecMeteo.size() : station_idx+1;
	const size_t nrStations = (station_idx==IOUtils::npos)? vecMeteo.size() : 1;

	if (param==ncpp::TIME) { //TRICK to reduce rounding errors, we use the scale as a divisor for TIME
		const size_t nrTimeSteps = vecTime.size();
		std::vector<double> data(nrTimeSteps);

		if (var.attributes.type==NC_INT) {
			double prev = IOUtils::nodata;
			for (size_t ll=0; ll<nrTimeSteps; ll++) {
				//we pre-round the data so when libnetcdf will cast, it will fall on what we want
					//note: the scale parameter is used as a divisor for TIME
				data[ll] = static_cast<double>( Optim::round( (vecTime[ll].getJulian(true) - var.offset) * var.scale) );
				if (prev!=IOUtils::nodata && data[ll]==prev)
					throw InvalidArgumentException("When writing time as INT or in seconds, some timesteps are rounded to identical values. Please change your sampling rate!", AT);
				prev = data[ll];
			}
		} else {
			const double time_precision = Date::epsilon_sec / (24.*3600.) * var.scale; //packed time precision: sec_precision converted to days, converted back to user-defined units
			double prev = IOUtils::nodata;
			for (size_t ll=0; ll<nrTimeSteps; ll++) {
				//for better numerical consistency, we round the data to Date::epsilon_sec in NetCDF internal representation
				data[ll] = transformTime(vecTime[ll].getJulian(true), var.offset, var.scale, time_precision);
				if (prev!=IOUtils::nodata && data[ll]==prev)
					throw InvalidArgumentException("When writing time as INT or in seconds, some timesteps are rounded to identical values. Please change your sampling rate!", AT);
				prev = data[ll];
			}
		}

		return data;
	} else {
		std::vector<double> data(nrStations, var.nodata);
		for (size_t jj=st_start; jj<st_end; jj++) {
			double value = IOUtils::nodata;
			if (param==MeteoGrids::DEM) {
				value = vecMeteo[jj].front().meta.position.getAltitude();
			} else if (param==MeteoGrids::SLOPE) {
				value = vecMeteo[jj].front().meta.getSlopeAngle();
				if (value==IOUtils::nodata) value = dflt_slope;
			} else if (param==MeteoGrids::AZI) {
				value = vecMeteo[jj].front().meta.getAzimuth();
				if (value==IOUtils::nodata) value = dflt_azi;
			} else if (param==ncpp::EASTING) {
				value = vecMeteo[jj].front().meta.position.getEasting();
			} else if (param==ncpp::NORTHING) {
				value = vecMeteo[jj].front().meta.position.getNorthing();
			} else if (param==ncpp::LATITUDE) {
				value = vecMeteo[jj].front().meta.position.getLat();
			} else if (param==ncpp::LONGITUDE) {
				value = vecMeteo[jj].front().meta.position.getLon();
			} else if (param==ncpp::ZREF) { //this two are required by Crocus and we don't have anything better for now...
				value = dflt_zref;
			} else if (param==ncpp::UREF) {
				value = dflt_uref;
			} else
				throw UnknownValueException("Unknown dimension found when trying to write out netcdf file", AT);

			if (value!=IOUtils::nodata) data[jj-st_start] = value;
		}
		return data;
	}
}

//in order to write MetoData, we need to serialize each parameter...
//if station_idx==IOUtils::npos, the user has requested all stations in one file
const std::vector<double> ncFiles::fillBufferForVar(const std::vector< std::vector<MeteoData> >& vecMeteo, const size_t& station_idx, const ncpp::nc_variable& var) const
{
	const size_t param = var.attributes.param;

	const bool varIsLocation = (param==MeteoGrids::DEM || param==MeteoGrids::SLOPE || param==MeteoGrids::AZI || param==ncpp::ZREF || param==ncpp::UREF);
	const bool varIsDimension = (param>=ncpp::firstdimension && param<=ncpp::lastdimension);
	if (varIsDimension || varIsLocation) { //associated nc_variables
		return fillBufferForAssociatedVar(vecMeteo, station_idx, var);
	} else { //normal nc_variables
		const size_t st_start = (station_idx==IOUtils::npos)? 0 : station_idx;
		const size_t st_end = (station_idx==IOUtils::npos)? vecMeteo.size() : station_idx+1;
		const size_t nrStations = (station_idx==IOUtils::npos)? vecMeteo.size() : 1;

		//build MeteoData template for each station, get the param_idx for each station
		std::vector<size_t> param_idx(nrStations);
		for (size_t st=st_start; st<st_end; st++) {
			if (param<=ncpp::lastdimension)
				param_idx[ st-st_start ] = vecMeteo[ st ].front().getParameterIndex( MeteoGrids::getParameterName( param ) ); //retrieve the equivalent parameter in vecMeteo
			else
				param_idx[ st-st_start ] = vecMeteo[ st ].front().getParameterIndex( var.attributes.name );
		}

		//now fill the data vector
		std::vector<size_t> st_idx(nrStations, 0); //keep the current index for each station
		const size_t nrTimeSteps = vecTime.size();
		std::vector<double> data(nrTimeSteps*nrStations, var.nodata);
		const bool isPrecip = (var.attributes.param==MeteoGrids::PSUM || var.attributes.param==MeteoGrids::PSUM_S || var.attributes.param==MeteoGrids::PSUM_L);

		if (isPrecip && var.attributes.units=="kg/m2/s") { //convert precipitation to rates
			for (size_t ll=0; ll<nrTimeSteps; ll++) {
				for (size_t st=st_start; st<st_end; st++) {
					const size_t meteodata_param = param_idx[ st-st_start ];
					if (meteodata_param == IOUtils::npos) continue; //the station does not have this parameter
					size_t &ii = st_idx[st-st_start]; //this is the index for the current station
					if (ii>=vecMeteo[st].size()) continue; //this station does not have data anymore

					const MeteoData md( vecMeteo[st][ii] );
					if (md.date != vecTime[ll]) continue; //every time step is in vecTime but each station does not necessarily have all timesteps

					const double curr_julian = md.date.getJulian(true);
					//trick: in order to get an accumulation period at start, we take the one from the next timestep and assume they are the same
					const double ts_duration_s = ((ii>0)? (curr_julian - vecMeteo[st][ii-1].date.getJulian(true)) : (vecMeteo[st][ii+1].date.getJulian(true) - curr_julian) ) * (24.*3600.);

					if (md( meteodata_param ) != IOUtils::nodata && ts_duration_s>0.)
						data[ll*nrStations + (st-st_start)] = md( meteodata_param ) / ts_duration_s;
					ii++;
				}
			}
		} else { //normal variables
			for (size_t ll=0; ll<nrTimeSteps; ll++) {
				for (size_t st=st_start; st<st_end; st++) {
					const size_t meteodata_param = param_idx[ st-st_start ];
					if (meteodata_param == IOUtils::npos) continue; //the station does not have this parameter
					size_t &ii = st_idx[st-st_start]; //this is the index for the current station
					if (ii>=vecMeteo[st].size()) continue; //this station does not have data anymore

					const MeteoData md( vecMeteo[st][ii] );
					if (md.date != vecTime[ll]) continue; //every time step is in vecTime but each station does not necessarily have all timesteps

					if (md( meteodata_param ) != IOUtils::nodata)
						data[ll*nrStations + (st-st_start)] = md( meteodata_param );
					ii++;
				}
			}

			//perform some units corrections, if necessary
			if (var.attributes.units=="%") for (size_t ii=0; ii<data.size(); ii++) if (data[ii]!=var.nodata) data[ii] *= 100.;
			if (var.attributes.units=="kilometer") for (size_t ii=0; ii<data.size(); ii++) if (data[ii]!=var.nodata) data[ii] *= 1e-3;
		}

		return data;
	}
}

//in order to write 2D grids, we need to serialize them
const std::vector<double> ncFiles::fillBufferForVar(const Grid2DObject& grid, ncpp::nc_variable& var)
{
	const size_t param = var.attributes.param;

	if (param>=ncpp::firstdimension && param!=IOUtils::npos) { //associated nc_variables
		const double cellsize = grid.cellsize;
		const size_t nrPts = (param==ncpp::NORTHING)? grid.getNy() : grid.getNx();
		if (nrPts==0) return std::vector<double>(); //this should not have happened...
		std::vector<double> data( nrPts, var.nodata );

		if (param==ncpp::NORTHING || param==ncpp::EASTING) {
			data[0] = (param==ncpp::NORTHING)? grid.llcorner.getNorthing() : grid.llcorner.getEasting();
			for (size_t ii=1; ii<nrPts; ii++)
				data[ii] = data[0] + static_cast<double>(ii)*cellsize;
		} else if (param==ncpp::LATITUDE || param==ncpp::LONGITUDE) {
			//this is (very cheap) approximation of some kind of projection from x/y to lat/lon
			//There is a trick here: walking along a line of constant northing does NOT lead to a constant latitude. Both grids
			//are shifted (even if a little), which means that the center of lat/lon is != center of east./north..
			//So, in order to find the center of the domain, we do a few iteration to converge toward a reasonable approximation
			double alpha;
			double lat_length=0., lon_length=0., cntr_lat=grid.llcorner.getLat(), cntr_lon=grid.llcorner.getLon();
			for (size_t ii=0; ii<5; ii++) {
				lat_length = CoordsAlgorithms::VincentyDistance(cntr_lat-.5, cntr_lon, cntr_lat+.5, cntr_lon, alpha);
				lon_length = CoordsAlgorithms::VincentyDistance(cntr_lat, cntr_lon-.5, cntr_lat, cntr_lon+.5, alpha);
				cntr_lat = (.5*static_cast<double>(grid.getNy())*grid.cellsize) / lat_length + grid.llcorner.getLat();
				cntr_lon = (.5*static_cast<double>(grid.getNx())*grid.cellsize) / lon_length + grid.llcorner.getLon();
			}

			const double center = (param==ncpp::NORTHING)? cntr_lat : cntr_lon;
			const double length = (param==ncpp::NORTHING)? lat_length : lon_length;
			const double min = center - (.5*static_cast<double>(nrPts)*cellsize) / length;
			const double max = center + (.5*static_cast<double>(nrPts)*cellsize) / length;
			const double interval =  abs(max - min);

			for (size_t ii=0; ii<nrPts; ii++)
				data[ii] = min + (interval * static_cast<double>(ii)) / (static_cast<double>(nrPts)-1.);
		} else
			throw UnknownValueException("Unsupported dimension '"+ncpp::getParameterName(param)+"' found when trying to write out netcdf file", AT);
		return data;
	} else { //normal grid variable
		std::vector<double> data( grid.size(), var.nodata );
		const size_t nrows = grid.getNy(), ncols = grid.getNx();
		for (size_t kk=nrows-1; kk-- > 0; ) {
			for (size_t ll=0; ll<ncols; ++ll) {
				const size_t serial_idx = kk*ncols + ll;
				if (grid.grid2D(ll,kk)!=IOUtils::nodata) data[serial_idx] = grid.grid2D(ll,kk);
			}
		}
		return data;
	}
}

//bring back known units to MKSA
void ncFiles::applyUnits(Grid2DObject& grid, const std::string& units, const size_t& time_pos, const bool& m2mm) const
{
	if (units.empty()) return;

	if (units=="m2/s2" || units=="m**2 s**-2") grid /= Cst::gravity;
	else if (units=="%") grid /= 100.;
	else if (units=="J/m2" || units=="J m**-2") {
		if (vecTime.size()>1 && time_pos!=IOUtils::npos) {
			const Date integration_period = (time_pos>0)? (vecTime[time_pos] - vecTime[time_pos-1]) : (vecTime[time_pos+1] - vecTime[time_pos]);
			grid /= (integration_period.getJulian()*24.*3600.); //converting back to W/m2
		}
	}
	else if (m2mm && units=="m") grid *= 1000.;
}

void ncFiles::applyUnits(std::vector< std::vector<MeteoData> >& vecMeteo, const size_t& nrStations, const size_t& nrSteps, const std::string& units, const std::string& parname)
{
	if (units.empty()) return;

	double factor = 1.;
	if (units=="%") factor = 0.01;

	if (factor==1.) return;

	for (size_t st=0; st<nrStations; st++) {
		for (size_t jj=0; jj<nrSteps; jj++) {
			vecMeteo[st][jj](parname) *= factor;
		}
	}
}

//this returns the index where to insert the new grid
size_t ncFiles::addTimestamp(const Date& date)
{
	size_t time_pos = vecTime.size();
	bool create_timestamp = true;

	//search for the proper insertion position
	if (time_pos>0) {
		//we keep vecTime in sync with the file, so we can do the searches into vecTime
		if (date==vecTime.back()) {
			create_timestamp = false;
			time_pos--;
		} else if (date<vecTime.back()) {
			const std::vector<Date>::const_iterator low = std::lower_bound(vecTime.begin(), vecTime.end(), date);
			if (*low==date) create_timestamp = false;
			time_pos = static_cast<size_t>( std::distance<std::vector<Date>::const_iterator>(vecTime.begin(), low) ); //weird syntax to take the proper template
		}
	}

	if (create_timestamp) {
		const double packed_dt = (date.getJulian(true) - vars[ncpp::TIME].offset) * vars[ncpp::TIME].scale; //the scale parameter is used as a divisor for TIME
		const size_t start[] = {time_pos};
		const size_t count[] = {1};
		const int status = nc_put_vara_double(ncid, vars[ncpp::TIME].varid, start, count, &packed_dt);
		if (status != NC_NOERR) throw IOException("Could not write data for record variable '" + vars[ncpp::TIME].attributes.name + "': " + nc_strerror(status), AT);
		if (time_pos==vecTime.size())
			vecTime.push_back( date );
		else
			vecTime.insert(vecTime.begin()+time_pos, date);
	}

	return time_pos;
}

void ncFiles::initDimensionsFromFile()
{
	int status;
	int ndims;
	status = nc_inq_ndims(ncid, &ndims);
	if (status != NC_NOERR) throw IOException("Could not retrieve number of dimensions: " + std::string(nc_strerror(status)), AT);

	int unlim_id;
	status = nc_inq_unlimdim(ncid, &unlim_id);
	if (status != NC_NOERR) throw IOException("Could not retrieve unlimited dimension: " + std::string(nc_strerror(status)), AT);

	int *dimids = (int*)malloc(ndims * sizeof(int));
	status = nc_inq_dimids(ncid, &ndims, dimids, 0);
	if (status != NC_NOERR) throw IOException("Could not retrieve dimensions IDs: " + std::string(nc_strerror(status)), AT);

	for (int idx=0; idx<ndims; idx++) {
		char name[NC_MAX_NAME+1];
		status = nc_inq_dimname(ncid, dimids[idx], name);
		const std::string dimname( name );
		if (status != NC_NOERR) throw IOException("Could not retrieve dimension name: " + std::string(nc_strerror(status)), AT);
		ncpp::nc_dimension tmp_dim( schema.getSchemaDimension(dimname) ); //set name and param
		if (tmp_dim.param==IOUtils::npos) { //unrecognized dimension -> try harder with some typical names that are not in the schema
			if (dimname=="lat")
				tmp_dim.param = ncpp::LATITUDE;
			else if (dimname=="lon")
				tmp_dim.param = ncpp::LONGITUDE;
			else if (IOUtils::strToLower(dimname)=="time")
				tmp_dim.param = ncpp::TIME;
			else continue;
			
			if (dimensions_map.count( tmp_dim.param )>0) {
				//if this parameter has already been read, skip it (so the schema naming has priority)
				if (dimensions_map[ tmp_dim.param ].dimid != -1) continue;
			}
		}

		tmp_dim.dimid = idx;
		tmp_dim.isUnlimited = (idx==unlim_id);
		status = nc_inq_dimlen(ncid, dimids[idx], &tmp_dim.length);
		if (status != NC_NOERR) throw IOException("Could not retrieve dimension lenght: " + std::string(nc_strerror(status)), AT);

		dimensions_map[ tmp_dim.param ] = tmp_dim;
	}

	free( dimids );
}

void ncFiles::initVariablesFromFile()
{
	int nr_of_variables = -1;
	int status = nc_inq_nvars(ncid, &nr_of_variables);
	if (status != NC_NOERR)
		throw IOException("Could not retrieve variables for dataset: " + string(nc_strerror(status)), AT);

	// Variable IDs in a NetCDF file are consecutive integers starting with 0
	for (int ii=0; ii<nr_of_variables; ++ii) {
		char name[NC_MAX_NAME+1];
		status = nc_inq_varname(ncid, ii, name);
		const std::string varname( name );
		ncpp::var_attr tmp_attr( schema.getSchemaAttributes(varname) );

		//try harder: we try some typical names that are NOT part of the schema (like for DEM)
		if (tmp_attr.param==IOUtils::npos) {
			if (varname=="Band1" || varname=="z" || varname=="height" || varname=="HGT")
				tmp_attr.param = MeteoGrids::DEM;
			else if (varname=="lat")
				tmp_attr.param = ncpp::LATITUDE;
			else if (varname=="lon")
				tmp_attr.param = ncpp::LONGITUDE;
			else if (IOUtils::strToLower(varname)=="time")
				tmp_attr.param = ncpp::TIME;
			
			if (vars.count( tmp_attr.param )>0) {
				//if this parameter has already been read, skip it (so the schema naming has priority)
				if (vars[ tmp_attr.param ].varid != -1) continue; 
			}
		}

		ncpp::nc_variable tmp_var( tmp_attr, schema.nodata );
		const bool readTimeTransform = (tmp_var.attributes.param==ncpp::TIME && tmp_var.attributes.type!=NC_CHAR);
		ncpp::readVariableMetadata(ncid, tmp_var, readTimeTransform, TZ);

		if (tmp_var.attributes.param!=IOUtils::npos) {
			vars[ tmp_var.attributes.param ] = tmp_var;
		} else {
			unknown_vars[ tmp_var.attributes.name ] = tmp_var;
		}
	}
}

double ncFiles::read_0Dvariable(const size_t& param) const
{
	const std::map<size_t, ncpp::nc_variable>::const_iterator it = vars.find( param );
	if (it==vars.end() || it->second.varid==-1) throw InvalidArgumentException("Could not find parameter \""+ncpp::getParameterName(param)+"\" in file \""+file_and_path+"\"", AT);
	if (!it->second.dimids.empty())
		throw InvalidFormatException("Trying to open variable '"+it->second.attributes.name+"' in file '"+file_and_path+"' as a 0D variable when it is "+IOUtils::toString(it->second.dimids.size())+"D", AT);
	
	double data;
	ncpp::read_data(ncid, it->second, &data);
	return data;
}

std::vector<Date> ncFiles::read_1Dvariable() const
{
	const std::map<size_t, ncpp::nc_variable>::const_iterator it = vars.find( ncpp::TIME );
	if (it==vars.end() || it->second.varid==-1) throw InvalidArgumentException("Could not find parameter \""+ncpp::getParameterName(ncpp::TIME)+"\" in file \""+file_and_path+"\"", AT);
	
	const bool timestamps_as_str = (it->second.attributes.type==NC_CHAR);

	if (!timestamps_as_str) {
		const std::vector<double> tmp_results( read_1Dvariable(ncpp::TIME) );
		std::vector<Date> results( tmp_results.size() );
		for (size_t ii=0; ii<tmp_results.size(); ii++)
			results[ii].setDate(tmp_results[ii]/it->second.scale + it->second.offset, TZ); //the scale parameter is used as a divisor for TIME
		return results;
	} else {
		std::vector<std::string> tmp_results( read_1Dstringvariable(ncpp::TIME) );
		const size_t length = tmp_results.size();
		std::vector<Date> results( length );
		for (size_t ii=0; ii<length; ii++) {
			tmp_results[ii][ 10 ] = 'T';
			IOUtils::convertString(results[ii], tmp_results[ii], TZ);
		}
		return results;
	}
}

std::vector<double> ncFiles::read_1Dvariable(const size_t& param) const
{
	const std::map<size_t, ncpp::nc_variable>::const_iterator it = vars.find( param );
	if (it==vars.end() || it->second.varid==-1) throw InvalidArgumentException("Could not find parameter \""+ncpp::getParameterName(param)+"\" in file \""+file_and_path+"\"", AT);
	if (it->second.dimids.size()!=1)
		throw InvalidFormatException("Trying to open variable '"+it->second.attributes.name+"' in file '"+file_and_path+"' as a 1D variable when it is "+IOUtils::toString(it->second.dimids.size())+"D", AT);
	
	const size_t length = read_1DvariableLength(it->second);

	std::vector<double> results( length );
	double *data = new double[ length ];
	ncpp::read_data(ncid, it->second, data);
	std::copy(data, data+length, results.begin());
	delete[] data;

	//potential units transformations
	if (schema.name!="METEOCH" && it->second.attributes.units=="kilometer") for (size_t ii=0; ii<length; ii++) results[ii] *= 1000.;
	return results;
}

std::vector<std::string> ncFiles::read_1Dstringvariable(const size_t& param) const
{
	const std::map<size_t, ncpp::nc_variable>::const_iterator it = vars.find( param );
	if (it==vars.end() || it->second.varid==-1) throw InvalidArgumentException("Could not find parameter \""+ncpp::getParameterName(param)+"\" in file \""+file_and_path+"\"", AT);
	if (it->second.attributes.type!=NC_CHAR) throw InvalidArgumentException("Wrong data type for parameter \""+ncpp::getParameterName(param)+"\" in file \""+file_and_path+"\"", AT);
	if (it->second.dimids.size()!=2) //strings also depend on variable str_len
		throw InvalidFormatException("Trying to open variable '"+it->second.attributes.name+"' in file '"+file_and_path+"' as a 2D variable when it is "+IOUtils::toString(it->second.dimids.size())+"D", AT);

	const size_t length = read_1DvariableLength(it->second); //this also checks that it depends on 2 dimensions for NC_CHAR
	const size_t strMaxLen = readDimension(it->second.dimids[1]); //string lenght is in second position
	char *data = (char*)calloc(length, sizeof(char)*strMaxLen);
	const int status = nc_get_var_text(ncid, it->second.varid, data);
	if (status != NC_NOERR) throw IOException("Could not retrieve data for Time variable: " + std::string(nc_strerror(status)), AT);

	std::vector<std::string> results(length);
	for (size_t ii=0; ii<length; ii++)
		results[ii] = std::string( &data[ii*strMaxLen] ); //this way the string ends at the first \0 char
	
	free( data );
	return results;
}

//This method handles the possibility of numeric station IDs by converting them to strings
std::vector<std::string> ncFiles::read_stationIDs() const
{
	static const size_t param = ncpp::STATION;
	const std::map<size_t, ncpp::nc_variable>::const_iterator it = vars.find( param );
	if (it==vars.end() || it->second.varid==-1) throw InvalidArgumentException("Could not find parameter \""+ncpp::getParameterName(param)+"\" in file \""+file_and_path+"\"", AT);

	const int type = it->second.attributes.type;
	if (type==NC_CHAR) {
		return read_1Dstringvariable(param);
	} else { //numeric station IDs
		if (it->second.dimids.size()!=1)
			throw InvalidFormatException("Trying to open variable '"+it->second.attributes.name+"' in file '"+file_and_path+"' as a 1D variable when it is "+IOUtils::toString(it->second.dimids.size())+"D", AT);

		const size_t length = read_1DvariableLength(it->second);
		std::vector<std::string> results(length);
		int *data = new int[ length ];
		ncpp::read_data(ncid, it->second, data);
		for (size_t ii=0; ii<length; ii++)
			results[ii] = IOUtils::toString( data[ii] );
		delete[] data;

		return results;
	}
}

size_t ncFiles::read_1DvariableLength(const ncpp::nc_variable& var) const
{
	const size_t ndims = var.dimids.size();
	if ((var.attributes.type==NC_CHAR && ndims!=2) || (var.attributes.type!=NC_CHAR && ndims!=1))
		throw InvalidArgumentException("Parameter \""+ncpp::getParameterName(var.attributes.param)+"\" in file \""+file_and_path+"\" is not a 1D variable", AT);

	return readDimension( var.dimids[0] ); //in the case of vector of strings, the first dimension is the vector size
}

size_t ncFiles::readDimension(const int& dimid) const
{
	std::map<size_t, ncpp::nc_dimension>::const_iterator it = dimensions_map.begin();
	for (; it!=dimensions_map.end(); ++it) {
		if (it->second.dimid==dimid) break;
	}
	if (it==dimensions_map.end()) throw InvalidArgumentException("Could not find a dimension in file \""+file_and_path+"\"", AT);

	return it->second.length;
}

//check that a given dimension exists, has a dimid and an associated variable with a varid
bool ncFiles::hasDimension(const size_t& dim) const
{
	const std::map<size_t, ncpp::nc_dimension>::const_iterator it_dim = dimensions_map.find( dim );
	if (it_dim==dimensions_map.end()) return false;
	if (it_dim->second.dimid==-1) return false;

	return hasVariable( dim );
}

//check that a given variable exists and has a positive varid
bool ncFiles::hasVariable(const size_t& var) const
{
	const std::map<size_t, ncpp::nc_variable>::const_iterator it_var = vars.find( var );
	if (it_var==vars.end()) return false;
	if (it_var->second.varid==-1) return false;

	return true;
}

//This method handles non-standard parameters, ie parameters that are not even in MeteoGrids / Dimensions
size_t ncFiles::getParameterIndex(const std::string& param_name)
{
	const size_t std_idx = ncpp::getParameterIndex( param_name );
	if (std_idx!=IOUtils::npos) return std_idx;

	//then it is a non-standard parameter
	const std::map<std::string, ncpp::nc_variable>::const_iterator it_var = unknown_vars.find( param_name );
	if (it_var!=unknown_vars.end()) return it_var->second.attributes.param;

	//the parameter must be created
	max_unknown_param_idx++;
	const size_t param_idx = max_unknown_param_idx;
	const ncpp::var_attr tmp_attr(param_idx, param_name, "", "", "", IOUtils::nodata, schema.dflt_type);
	unknown_vars[ param_name ] = ncpp::nc_variable(tmp_attr, schema.nodata);
	return param_idx;
}

} //namespace
