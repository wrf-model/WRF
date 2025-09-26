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
#include <meteoio/plugins/GRIBIO.h>

#include <meteoio/meteoStats/libresampling2D.h>
#include <meteoio/meteoLaws/Atmosphere.h>
#include <meteoio/meteoLaws/Meteoconst.h> //for PI
#include <meteoio/dataClasses/DEMObject.h>
#include <meteoio/dataClasses/CoordsAlgorithms.h>
#include <meteoio/MathOptim.h>
#include <meteoio/FileUtils.h>

#include <cmath>
#include <iostream>
#include <cerrno>
#include <algorithm>
#include <grib_api.h>

using namespace std;

namespace mio {
/**
 * @page gribio GRIBIO
 * @section gribio_format Format and limitations
 * This plugin reads GRIB (https://en.wikipedia.org/wiki/GRIB) files as produced by meteorological models.
 * Being based on GRIB API (https://software.ecmwf.int/wiki/display/GRIB/Home), it should support both version 1 and 2 of the format (please note that grib_api must be compiled with Position Independent Code ("fPIC" flag)).
 * Fields are read based on their marsParam code (this is built as {grib parameter number}.{grib table number} the table being preferably table 2, the parameter being preferably WMO standardized, as in http://rda.ucar.edu/docs/formats/grib/gribdoc/params.html) and levels
 * (levels description is available at http://www.nco.ncep.noaa.gov/pmb/docs/on388/). Standard COSMO grids are listed at http://zephyr.ucd.ie/mediawiki/index.php/COSMO_GRIB while NCEP are listed at http://www.cpc.ncep.noaa.gov/products/wesley/opn_gribtable.html. Users with access to <a href="http://www.cscs.ch">CSCS</a> can find the up-to-date parameters description under <i>/oprusers/osm/opr/lib/dictionary_cosmo.txt</i>
 *
 * Several assumptions/approximations are held/made when reading grids:
 * - since models usually use rotated latitude/longitude (see http://www.cosmo-model.org/content/model/documentation/core/default.htm , part I, chapter 3.3), the center of the domain can be approximated by a tangential cartesian coordinate system. We therefore don't re-project the lat/lon grid and use it "as is".
 * - however, no correction for grid rotation is currently performed. If a grid rotation is specified on top of the rotated coordinate system, an error message will be given
 * - the cell size is computed at the center of the domain. This is performed by retrieving the latitude and longitude increments in the rotated coordinates, computing the point at center+increment in geographic coordinates and computing the equivalent geographic latitude and longitude increment. These increments are then converted to distances along the parallel and meridian at the true center latitude (see https://en.wikipedia.org/wiki/Latitude#The_length_of_a_degree_of_latitude).
 * - the average cell size (ie: average between x and y) is used to move the center point to the lower left corner. This will be returned as lower left corner geolocalization of the grid.
 *
 * This means that close to the center of the grid, coordinates and distances will work as expected, but the distortion will increase when moving away from the center and can become significant. As examples for domain size, cone can look at the MeteoSwiss domain definition at http://www.cosmo-model.org/content/tasks/operational/meteoSwiss/default.htm .
 *
 * As a side note, when calling read2DGrid(grid, filename), it will returns the first grid that is found. When using the standard call, it will look for the the file 
 * within the provided directory that contains the requested timestamp and read the requested field from it. Each file should contain all the fields but 
 * only one timestamp per file.
 *
 * @section gribio_cosmo_partners COSMO Group
 * This plugin has been developed primarily for reading GRIB files produced by COSMO (http://www.cosmo-model.org/) at MeteoSwiss.
 * COSMO (COnsortium for Small scale MOdelling) represents a non-hydrostatic limited-area atmospheric model, to be used both for operational and for research applications by the members of the consortium. The Consortium has the following members:
 *  - Germany, DWD, Deutscher Wetterdienst
 *  - Switzerland, MCH, MeteoSchweiz
 *  - Italy, USAM, Ufficio Generale Spazio Aereo e Meteorologia
 *  - Greece, HNMS, Hellenic National Meteorological Service
 *  - Poland, IMGW, Institute of Meteorology and Water Management
 *  - Romania, NMA, National Meteorological Administration
 *  - Russia, RHM, Federal Service for Hydrometeorology and Environmental Monitoring
 *  - Germany, AGeoBw, Amt für GeoInformationswesen der Bundeswehr
 *  - Italy, CIRA, Centro Italiano Ricerche Aerospaziali
 *  - Italy, ARPA-SIMC, ARPA Emilia Romagna Servizio Idro Meteo Clima
 *  - Italy, ARPA Piemonte, Agenzia Regionale per la Protezione Ambientale Piemonte
 *
 * @section gribio_units Units
 * As specified by WMO.
 *
 * @section gribio_files_naming Files naming scheme
 * When a grid is read by providing the filename to open, any file name will obviously work. Otherwise, the file names have to follow the pattern:\n
 * {GRID2DPREFIX}{ISO8601 numerical UTC date}{GRID2DEXT}\n
 * By default, GRID2DPREFIX is empty and GRID2DEXT is ".grb". This means that by default, a grib file containing data for 2013-10-15T12:00 would be:
 * "201310151200.grb". Since the grid extension contains the ".", it is possible to use it for a file name suffix as well. For example, to read files
 * named like "lfff201310151200_z" one would provide GRID2DPREFIX="lfff" and GRID2DEXT="_z".
 *
 * @section gribio_keywords Keywords
 * This plugin uses the following keywords:
 * - COORDSYS: coordinate system (see Coords)
 * - COORDPARAM: extra coordinates parameters (see Coords)
 * - GRID2DPATH: path where to find the grids
 * - GRID2DPREFIX: prefix to append when generating a file name for reading (ie: something like "laf" for Cosmo-Analysis-full domain), optional
 * - GRID2DEXT: grib file extension, or <i>none</i> for no file extension (default: ".grb")
 * - GRIB_DEM_UPDATE: recompute slope/azimuth from the elevations when reading a DEM (default=false,
 * that is we use the slope and azimuth included in the GRIB file)
 * - METEOPATH: path where to find the grids for extracting time series at the given points
 * - METEOEXT: file extension, or <i>none</i> for no file extension (default: ".grb")
 * - STATION#: coordinates for virtual stations (if using GRIB as METEO plugin). Each station is given by its coordinates and the closest
 * grid point will be chosen. Coordinates are given one one line as "lat lon" or "xcoord ycoord epsg_code". If a point leads to duplicate grid points,
 * it will be removed from the list.
 * - GRIB_DEBUG: output more information about the grib files in order to help fix potential problems (default: false).
 *
 * @code
 * [Input]
 * METEO           = GRIB
 * METEOPATH       = ./input
 * METEOEXT        = none
 * STATION1        = 779932 188182 21781 ;WFJ
 * STATION2        = 788889 192513 21781 ;Kloster
 * STATION3        = 793274 183770 21781 ;Weisshorn
 *
 * GRID2D          = GRIB
 * GRID2DPATH      = ./input
 * GRID2DPREFIX    = lfff
 * GRID2DEXT       = none
 *
 * DEM             = GRIB
 * DEMFILE         = ./input/laf2012031001
 * @endcode
 */

const double GRIBIO::plugin_nodata = -999.; //plugin specific nodata value. It can also be read by the plugin (depending on what is appropriate)
const double GRIBIO::tz_in = 0.; //GRIB time zone, always UTC
const std::string GRIBIO::default_ext=".grb"; //filename extension

GRIBIO::GRIBIO(const std::string& configfile)
        : cfg(configfile), grid2dpath_in(), meteopath_in(), vecPts(), cache_meteo_files(),
          meteo_ext(default_ext), grid2d_ext(default_ext), grid2d_prefix(), idx_filename(), coordin(), coordinparam(),
          VW(), DW(), wind_date(), llcorner(), fp(NULL), idx(NULL),
          latitudeOfNorthernPole(IOUtils::nodata), longitudeOfNorthernPole(IOUtils::nodata), bearing_offset(IOUtils::nodata),
          cellsize(IOUtils::nodata), factor_x(IOUtils::nodata), factor_y(IOUtils::nodata),
          indexed(false), meteo_initialized(false), llcorner_initialized(false), update_dem(false), debug(false)
{
	setOptions();
}

GRIBIO::GRIBIO(const Config& cfgreader)
        : cfg(cfgreader), grid2dpath_in(), meteopath_in(), vecPts(), cache_meteo_files(),
          meteo_ext(default_ext), grid2d_ext(default_ext), grid2d_prefix(), idx_filename(), coordin(), coordinparam(),
          VW(), DW(), wind_date(), llcorner(), fp(NULL), idx(NULL),
          latitudeOfNorthernPole(IOUtils::nodata), longitudeOfNorthernPole(IOUtils::nodata), bearing_offset(IOUtils::nodata),
          cellsize(IOUtils::nodata), factor_x(IOUtils::nodata), factor_y(IOUtils::nodata),
          indexed(false), meteo_initialized(false), llcorner_initialized(false), update_dem(false), debug(false)
{
	setOptions();
}

GRIBIO& GRIBIO::operator=(const GRIBIO& source) {
	if (this != &source) {
		fp = NULL;
		idx = NULL;
		grid2dpath_in = source.grid2dpath_in;
		meteopath_in = source.meteopath_in;
		vecPts = source.vecPts;
		cache_meteo_files = source.cache_meteo_files;
		meteo_ext = source.meteo_ext;
		grid2d_ext = source.grid2d_ext;
		grid2d_prefix = source.grid2d_prefix;
		idx_filename = source.idx_filename;
		coordin = source.coordin;
		coordinparam = source.coordinparam;
		VW = source.VW;
		DW = source.DW;
		wind_date = source.wind_date;
		llcorner = source.llcorner;
		latitudeOfNorthernPole = source.latitudeOfNorthernPole;
		longitudeOfNorthernPole = source.longitudeOfNorthernPole;
		bearing_offset = source.bearing_offset;
		cellsize = source.cellsize;
		factor_x = source.factor_x;
		factor_y = source.factor_y;
		indexed = source.indexed;
		meteo_initialized = source.meteo_initialized;
		llcorner_initialized = source.llcorner_initialized;
		update_dem = source.update_dem;
		debug = source.debug;
	}
	return *this;
}

GRIBIO::~GRIBIO() throw()
{
	cleanup();
}

void GRIBIO::setOptions()
{
	std::string coordout, coordoutparam;
	IOUtils::getProjectionParameters(cfg, coordin, coordinparam, coordout, coordoutparam);

	const std::string tmp = cfg.get("GRID2D", "Input", "");
	if (tmp == "GRIB") { //keep it synchronized with IOHandler.cc for plugin mapping!!
		cfg.getValue("GRID2DPATH", "Input", grid2dpath_in);
		cfg.getValue("GRIB_DEM_UPDATE", "Input", update_dem, IOUtils::nothrow);
	}
	cfg.getValue("GRID2DPREFIX", "Input", grid2d_prefix, IOUtils::nothrow);

	cfg.getValue("METEOEXT", "Input", meteo_ext, IOUtils::nothrow);
	if (meteo_ext=="none") meteo_ext.clear();

	cfg.getValue("GRID2DEXT", "Input", grid2d_ext, IOUtils::nothrow);
	if (grid2d_ext=="none") grid2d_ext.clear();
	
	cfg.getValue("GRIB_DEBUG", "Input", debug, IOUtils::nothrow);
}

void GRIBIO::readStations(std::vector<Coords> &vecPoints)
{
	cfg.getValue("METEOPATH", "Input", meteopath_in);

	std::vector<std::string> vecStation;
	cfg.getValues("STATION", "INPUT", vecStation);
	for (size_t ii=0; ii<vecStation.size(); ii++) {
		Coords tmp(coordin, coordinparam, vecStation[ii]);
		if (!tmp.isNodata())
			vecPoints.push_back( tmp );

		std::cerr <<  "\tRead virtual station " << vecPoints.back().toString(Coords::LATLON) << "\n";
	}
}

//this prints the list of all global variables
void GRIBIO::listKeys(grib_handle** h, const std::string& filename)
{
	static const unsigned int MAX_VAL_LEN = 1024; //max value string length in GRIB
	unsigned long key_iterator_filter_flags = GRIB_KEYS_ITERATOR_ALL_KEYS;
	char* name_space = NULL;
	grib_keys_iterator* kiter = grib_keys_iterator_new(*h,key_iterator_filter_flags,name_space);

	if (!kiter) {
		cleanup();
		throw IOException("Unable to create keys iterator for \""+filename+"\"", AT);
	}

	//Iterating over all keys
	while (grib_keys_iterator_next(kiter)) {
		const char* name = grib_keys_iterator_get_name(kiter);
		char value[MAX_VAL_LEN];
		size_t vlen = MAX_VAL_LEN;
		bzero(value,vlen);
		GRIB_CHECK(grib_get_string(*h,name,value,&vlen),name);
		std::cerr << name << " = " << value << "\n";
	}

	grib_keys_iterator_delete(kiter);
}

//this lists all parameters
void  GRIBIO::listFields(const std::string& filename)
{
	grib_handle* h=NULL;
	int err=0;
	fpos_t pos;
	if (fgetpos(fp,&pos) != 0)
		throw IOException("Unable to get a file pointer for \""+filename+"\"", AT);

	std::cerr << "Fields found in " << filename << "\n";
	//loop over the messages
	while ((h = grib_handle_new_from_file(0,fp,&err)) != NULL) {
		if (!h) {
			cleanup();
			throw IOException("Unable to create grib handle for \""+filename+"\"", AT);
		}

		double marsParam;
		GRIB_CHECK(grib_get_double(h,"marsParam",&marsParam),0);

		size_t len=500;
		char name[500], shortname[500];
		GRIB_CHECK(grib_get_string(h,"name",name, &len),0);
		len=500; //this has been reset by the previous call... no comments
		GRIB_CHECK(grib_get_string(h,"shortName",shortname, &len),0);
		long levelType;
		GRIB_CHECK(grib_get_long(h,"indicatorOfTypeOfLevel", &levelType),0);
		long level=0;
		if (levelType!=1) GRIB_CHECK(grib_get_long(h,"level", &level),0);
		std::cerr << marsParam << " " << shortname << " " << name << " type " << levelType << " level " << level << "\n";
		grib_handle_delete(h);
	}
	if (fsetpos(fp, &pos) != 0)
		throw IOException("Unable to reset the file pointer for \""+filename+"\"", AT);
	std::cerr << "******\n";
}

void GRIBIO::getDate(grib_handle* h, Date &base, double &d1, double &d2) {
	long dataDate, dataTime;
	GRIB_CHECK(grib_get_long(h,"dataDate",&dataDate),0);
	GRIB_CHECK(grib_get_long(h,"dataTime",&dataTime),0);

	const int year=static_cast<int>(dataDate/10000), month=static_cast<int>(dataDate/100-year*100), day=static_cast<int>(dataDate-month*100-year*10000);
	const int hour=static_cast<int>(dataTime/100), minutes=static_cast<int>(dataTime-hour*100); //HACK: handle seconds!
	base.setDate(year, month, day, hour, minutes, tz_in);

	//reading offset to base date/time, as used for forecast, computed at time t for t+offset
	long stepUnits, startStep, endStep;
	GRIB_CHECK(grib_get_long(h,"stepUnits",&stepUnits),0);
	GRIB_CHECK(grib_get_long(h,"startStep",&startStep),0);
	GRIB_CHECK(grib_get_long(h,"endStep",&endStep),0);

	double step_units; //in julian, ie. in days
	switch(stepUnits) {
		case 0: //minutes
			step_units = 1./(24.*60.);
			break;
		case 1: //hours
			step_units = 1./24.;
			break;
		case 2: //days
			step_units = 1.;
			break;
		case 10: //3 hours
			step_units = 3./24.;
			break;
		case 11: //6 hours
			step_units = 6./24.;
			break;
		case 12: //12 hours
			step_units = 12./24.;
			break;
		case 13: //seconds
			step_units = 1./(24.*3600.);
			break;
		case 14: //15 minutes
			step_units = 15./(24.*60.);
			break;
		case 15: //30 minutes
			step_units = 30./(24.*60.);
			break;
		default:
			std::ostringstream ss;
			ss << "GRIB file using stepUnits=" << stepUnits << ", which is not supported";
			throw InvalidFormatException(ss.str(), AT);
	}

	d1 = static_cast<double>(startStep)*step_units;
	d2 = static_cast<double>(endStep)*step_units;
}

Coords GRIBIO::getGeolocalization(grib_handle* h, double &cell_x, double &cell_y)
{
	//getting transformation parameters
	double angleOfRotationInDegrees = 0.; //if the key was not found, we consider there is no rotation
	grib_get_double(h,"angleOfRotationInDegrees",&angleOfRotationInDegrees);
	if (angleOfRotationInDegrees!=0.) {
		throw InvalidArgumentException("Rotated grids not supported!", AT);
	}
	double latitudeOfSouthernPole = -90, longitudeOfSouthernPole = -180;
	grib_get_double(h,"latitudeOfSouthernPoleInDegrees",&latitudeOfSouthernPole);
	grib_get_double(h,"longitudeOfSouthernPoleInDegrees",&longitudeOfSouthernPole);
	latitudeOfNorthernPole = -latitudeOfSouthernPole;
	longitudeOfNorthernPole = longitudeOfSouthernPole+180.;

	//determining llcorner, urcorner and center coordinates
	double ll_latitude, ll_longitude, ll_lat, ll_lon;
	GRIB_CHECK(grib_get_double(h,"latitudeOfFirstGridPointInDegrees",&ll_latitude),0);
	GRIB_CHECK(grib_get_double(h,"longitudeOfFirstGridPointInDegrees",&ll_longitude),0);
	CoordsAlgorithms::rotatedToTrueLatLon(latitudeOfNorthernPole, longitudeOfNorthernPole, ll_latitude, ll_longitude, ll_lat, ll_lon);
	double ur_latitude, ur_longitude, ur_lat, ur_lon;
	GRIB_CHECK(grib_get_double(h,"latitudeOfLastGridPointInDegrees",&ur_latitude),0);
	GRIB_CHECK(grib_get_double(h,"longitudeOfLastGridPointInDegrees",&ur_longitude),0);
	CoordsAlgorithms::rotatedToTrueLatLon(latitudeOfNorthernPole, longitudeOfNorthernPole, ur_latitude, ur_longitude, ur_lat, ur_lon);
	double cntr_lat, cntr_lon; //geographic coordinates
	CoordsAlgorithms::rotatedToTrueLatLon(latitudeOfNorthernPole, longitudeOfNorthernPole, .5*(ll_latitude+ur_latitude), .5*(ll_longitude+ur_longitude), cntr_lat, cntr_lon);

	//determining cell size
	long Ni, Nj;
	GRIB_CHECK(grib_get_long(h,"Ni",&Ni),0);
	GRIB_CHECK(grib_get_long(h,"Nj",&Nj),0);
	double bearing;
	cell_x = CoordsAlgorithms::VincentyDistance(cntr_lat, ll_lon, cntr_lat, ur_lon, bearing) / (double)Ni;
	cell_y = CoordsAlgorithms::VincentyDistance(ll_lat, cntr_lon, ur_lat, cntr_lon, bearing) / (double)Nj;

	//determining bearing offset
	double delta_lat, delta_lon; //geographic coordinates
	CoordsAlgorithms::rotatedToTrueLatLon(latitudeOfNorthernPole, longitudeOfNorthernPole, .5*(ll_latitude+ur_latitude)+1., .5*(ll_longitude+ur_longitude), delta_lat, delta_lon);
	CoordsAlgorithms::VincentyDistance(cntr_lat, cntr_lon, delta_lat, delta_lon, bearing_offset);
	bearing_offset = fmod( bearing_offset + 180., 360.) - 180.; // turn into [-180;180)

	//returning the center point as reference
	Coords cntr(coordin, coordinparam);
	cntr.setLatLon(cntr_lat, cntr_lon, IOUtils::nodata);

	return cntr;
}

void GRIBIO::read2Dlevel(grib_handle* h, Grid2DObject& grid_out)
{
	long Ni, Nj;
	GRIB_CHECK(grib_get_long(h,"Ni",&Ni),0);
	GRIB_CHECK(grib_get_long(h,"Nj",&Nj),0);

	size_t values_len= 0;
	GRIB_CHECK(grib_get_size(h,"values",&values_len),0);
	if (values_len!=(unsigned)(Ni*Nj)) {
		ostringstream ss;
		ss << "Declaring grid of size " << Ni << "x" << Nj << "=" << Ni*Nj << " ";
		ss << "but containing " << values_len << " values. This is inconsistent!";
		throw InvalidArgumentException(ss.str(), AT);
	}
	double *values = (double*)calloc(values_len, sizeof(double));

	GRIB_CHECK(grib_get_double_array(h,"values",values,&values_len),0);

	if (!llcorner_initialized) {
		//most important: get cellsize. llcorner will be finalized AFTER aspect ration correction
		double cellsize_x, cellsize_y;
		llcorner = getGeolocalization(h, cellsize_x, cellsize_y); //this is the center cell
		cellsize = (double)Optim::round( std::min(cellsize_x, cellsize_y) * 100. ) / 100.; // round to 1cm precision for numerical stability
		if ( fabs(cellsize_x-cellsize_y)/cellsize_x > 1./100.) {
			factor_x = cellsize_x / cellsize;
			factor_y = cellsize_y / cellsize;
		}
	}
	
	grid_out.set(static_cast<size_t>(Ni), static_cast<size_t>(Nj),  cellsize, llcorner);
	size_t i=0;
	for (size_t jj=0; jj<(unsigned)Nj; jj++) {
		for (size_t ii=0; ii<(unsigned)Ni; ii++)
			grid_out(ii,jj) = values[i++];
	}
	free(values);
	
	//cells were not square, we have to resample
	if (factor_x!=IOUtils::nodata && factor_y!=IOUtils::nodata) {
		grid_out.grid2D = LibResampling2D::Bilinear(grid_out.grid2D, factor_x, factor_y);
	}
	
	if (!llcorner_initialized) { //take into account aspect ration conversion for computing true llcorner
		llcorner.moveByXY(-.5*(double)grid_out.getNx()*cellsize, -.5*(double)grid_out.getNy()*cellsize );
		llcorner_initialized = true;
		
		grid_out.llcorner = llcorner;
	}
}

bool GRIBIO::read2DGrid_indexed(const double& in_marsParam, const long& i_levelType, const long& i_level, const Date i_date, Grid2DObject& grid_out)
{
	GRIB_CHECK(grib_index_select_double(idx,"marsParam",in_marsParam),0);
	GRIB_CHECK(grib_index_select_long(idx,"indicatorOfTypeOfLevel", i_levelType),0);

	grib_handle* h=NULL;
	int err=0;
	while ((h = grib_handle_new_from_index(idx,&err)) != NULL) {
		if (!h) {
			cleanup();
			throw IOException("Unable to create grib handle from index", AT);
		}

		Date base_date;
		double P1, P2;
		getDate(h, base_date, P1, P2);

		//see WMO code table5 for definitions of timeRangeIndicator. http://dss.ucar.edu/docs/formats/grib/gribdoc/timer.html
		// 0 -> at base_date + P1
		// 1 -> at base_date
		// 2 -> valid between base_date+P1 and base_date+P2
		// 3 -> average within [base_date+P1 , base_date+P2]
		// 4 -> accumulation from base_date+P1 to base_date+P2
		// 5 -> difference (base_date+P2) - (base_date+P1)
		long timeRange;
		GRIB_CHECK(grib_get_long(h,"timeRangeIndicator", &timeRange),0);

		long level=0;
		if (i_level!=0) GRIB_CHECK(grib_get_long(h,"level", &level),0);
		if (level==i_level) {
			if ( (i_date.isUndef()) ||
			    (timeRange==0 && i_date==base_date+P1) ||
			    (timeRange==1 && i_date==base_date) ||
			    ((timeRange==2 || timeRange==3) && i_date>=base_date+P1 && i_date<=base_date+P2) ||
			    ((timeRange==4 || timeRange==5) && i_date==base_date+P2) ) {
				read2Dlevel(h, grid_out);
				if (timeRange==3) grid_out *= ((P2-P1)*24.*3600.); //convert avg to sum
				grib_handle_delete(h);
				return true;
			}
		}
		grib_handle_delete(h);
	}
	return false;
}

void GRIBIO::read2DGrid(Grid2DObject& grid_out, const std::string& i_name)
{
	const std::string filename( grid2dpath_in+"/"+i_name );
	if (!FileUtils::fileExists(filename)) throw AccessException(filename, AT); //prevent invalid filenames
	errno = 0;
	fp = fopen(filename.c_str(),"r");
	if (fp==NULL) {
		std::ostringstream ss;
		ss << "Error opening file \"" << filename << "\", possible reason: " << std::strerror(errno);
		throw AccessException(ss.str(), AT);
	}
	if (debug) listFields(filename);

	grib_handle* h=NULL;
	int err=0;
	if ((h = grib_handle_new_from_file(0,fp,&err)) != NULL) {
		if (!h) {
			cleanup();
			throw IOException("Unable to create grib handle for \""+filename+"\"", AT);
		}

		read2Dlevel(h, grid_out);
		grib_handle_delete(h);
	} else {
		cleanup();
		throw IOException("No grid found in file \""+filename+"\"", AT);
	}

	cleanup();
}

void GRIBIO::indexFile(const std::string& filename)
{
	if (!FileUtils::fileExists(filename)) throw AccessException(filename, AT); //prevent invalid filenames
	errno = 0;
	fp = fopen(filename.c_str(),"r");
	if (fp==NULL) {
		std::ostringstream ss;
		ss << "Error opening file \"" << filename << "\", possible reason: " << std::strerror(errno);
		throw AccessException(ss.str(), AT);
	}
	if (debug) listFields(filename);

	int err=0;
	static const std::string keys("marsParam:d,indicatorOfTypeOfLevel:l"); //indexing keys
	char *c_filename = (char *)filename.c_str();
	idx = grib_index_new_from_file(0, c_filename, keys.c_str(), &err);
	if (err!=0) {
		cleanup();
		throw IOException("Failed to index GRIB file \""+filename+"\". Is it a valid GRIB file?", AT);
	}
	indexed=true;
	idx_filename = filename;
}

void GRIBIO::read2DGrid(Grid2DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date)
{
	Date UTC_date = date;
	UTC_date.setTimeZone(tz_in);

	const std::string filename( grid2dpath_in+"/"+grid2d_prefix+UTC_date.toString(Date::NUM).substr(0,10)+grid2d_ext );

	read2DGrid(filename, grid_out, parameter, UTC_date);
}

void GRIBIO::readWind(const std::string& filename, const Date& date)
{
	if (wind_date==date) return; //wind fields are already up to date

	if (read2DGrid_indexed(32.2, 105, 10, date, VW)) { //FF_10M
		if (!read2DGrid_indexed(31.2, 105, 10, date, DW)) //DD_10M
			throw NoDataException("Can not read wind direction in file \""+filename+"\"", AT);
	} else {
		Grid2DObject U,V;
		read2DGrid_indexed(33.2, 105, 10, date, U); //U_10M, also in 110, 10 as U
		read2DGrid_indexed(34.2, 105, 10, date, V); //V_10M, also in 110, 10 as V

		VW.set(U.getNx(), U.getNy(), U.cellsize, U.llcorner);
		DW.set(U.getNx(), U.getNy(), U.cellsize, U.llcorner);
		for (size_t jj=0; jj<VW.getNy(); jj++) {
			for (size_t ii=0; ii<VW.getNx(); ii++) {
				VW(ii,jj) = sqrt( Optim::pow2(U(ii,jj)) + Optim::pow2(V(ii,jj)) );
				DW(ii,jj) = fmod( atan2( U(ii,jj), V(ii,jj) ) * Cst::to_deg + 360. + bearing_offset, 360.); // turn into degrees [0;360)
			}
		}
	}

	wind_date = date;
}

void GRIBIO::read2DGrid(const std::string& filename, Grid2DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date)
{ //Parameters should be read in table 2 if available since this table is the one standardized by WMO
	if (!indexed || idx_filename!=filename) {
		cleanup();
		indexFile(filename);
	}

	//Basic meteo parameters
	if (parameter==MeteoGrids::P) read2DGrid_indexed(1.2, 1, 0, date, grid_out); //PS
	if (parameter==MeteoGrids::TA) read2DGrid_indexed(11.2, 105, 2, date, grid_out); //T_2M
	if (parameter==MeteoGrids::RH) {
		if (!read2DGrid_indexed(52.2, 105, 2, date, grid_out)) { //RELHUM_2M
			Grid2DObject ta;
			read2DGrid_indexed(11.2, 105, 2, date, ta); //T_2M
			read2DGrid_indexed(17.2, 105, 2, date, grid_out); //TD_2M
			for (size_t jj=0; jj<grid_out.getNy(); jj++) {
				for (size_t ii=0; ii<grid_out.getNx(); ii++) {
					grid_out(ii,jj) = Atmosphere::DewPointtoRh(grid_out(ii,jj), ta(ii,jj), true);
				}
			}
		}
	}
	if (parameter==MeteoGrids::TSS) read2DGrid_indexed(197.201, 111, 0, date, grid_out); //T_SO
	if (parameter==MeteoGrids::TSG) read2DGrid_indexed(11.2, 1, 0, date, grid_out); //T_G

	//hydrological parameters
	if (parameter==MeteoGrids::PSUM) read2DGrid_indexed(61.2, 1, 0, date, grid_out); //tp
	if (parameter==MeteoGrids::ROT) read2DGrid_indexed(90.2, 112, 0, date, grid_out); //RUNOFF
	if (parameter==MeteoGrids::SWE) read2DGrid_indexed(65.2, 1, 0, date, grid_out); //W_SNOW
	if (parameter==MeteoGrids::HS) {
		if (!read2DGrid_indexed(66.2, 1, 0, date, grid_out)) {
			Grid2DObject snow_density;
			read2DGrid_indexed(133.201, 1, 0, date, snow_density); //RHO_SNOW
			read2DGrid_indexed(65.2, 1, 0, date, grid_out); //W_SNOW
			grid_out.grid2D /= snow_density.grid2D;
		}
	}

	//radiation parameters
	if (parameter==MeteoGrids::ALB) {
		read2DGrid_indexed(84.2, 1, 0, date, grid_out); //ALB_RAD
		grid_out.grid2D /= 100.;
	}
	if (parameter==MeteoGrids::ILWR) {
		if (read2DGrid_indexed(115.2, 1, 0, date, grid_out)) { //long wave
			grid_out.grid2D *= -1.;
		} else read2DGrid_indexed(25.201, 1, 0, date, grid_out); //ALWD_S
	}
	if (parameter==MeteoGrids::TAU_CLD) { //cloudiness
		if (read2DGrid_indexed(74.2, 1, 0, date, grid_out)) //CLCM
		grid_out.grid2D /= 100.;
	}

	if (parameter==MeteoGrids::ISWR) {
		if (read2DGrid_indexed(116.2, 1, 0, date, grid_out)) { //short wave
			grid_out.grid2D *= -1.;
		} else if (!read2DGrid_indexed(111.250, 1, 0, date, grid_out)) { //GLOB
			Grid2DObject diff;
			read2DGrid_indexed(23.201, 1, 0, date, diff); //diffuse rad, ASWDIFD_S
			read2DGrid_indexed(22.201, 1, 0, date, grid_out); //direct rad, ASWDIR_S
			grid_out.grid2D += diff.grid2D;
		}
	}

	//DEM parameters
	if (parameter==MeteoGrids::DEM) read2DGrid_indexed(8.2, 1, 0, date, grid_out); //HSURF
	if (parameter==MeteoGrids::SLOPE) {
		read2DGrid_indexed(98.202, 1, 0, date, grid_out); //SLO_ANG
		grid_out.grid2D *= Cst::to_deg;
	}
	if (parameter==MeteoGrids::AZI) {
		read2DGrid_indexed(99.202, 1, 0, date, grid_out); //SLO_ASP
		for (size_t jj=0; jj<grid_out.getNy(); jj++) {
			for (size_t ii=0; ii<grid_out.getNx(); ii++) {
				grid_out(ii,jj) = fmod( grid_out(ii,jj)*Cst::to_deg + 360. + bearing_offset, 360.); // turn into degrees [0;360)
			}
		}
	}

	//Wind parameters
	if (parameter==MeteoGrids::VW_MAX) read2DGrid_indexed(187.201, 105, 10, date, grid_out); //VMAX_10M 10m
	if (parameter==MeteoGrids::W) read2DGrid_indexed(40.2, 109, 10, date, grid_out); //W, 10m
	 //we need to use VW, DW, correct for re-projection and recompute U,V
	if (parameter==MeteoGrids::U) {
		readWind(filename, date);
		for (size_t jj=0; jj<grid_out.getNy(); jj++) {
			for (size_t ii=0; ii<grid_out.getNx(); ii++) {
				grid_out(ii,jj) = VW(ii,jj)*sin(DW(ii,jj)*Cst::to_rad);
			}
		}
	}
	if (parameter==MeteoGrids::V) {
		readWind(filename, date);
		for (size_t jj=0; jj<grid_out.getNy(); jj++) {
			for (size_t ii=0; ii<grid_out.getNx(); ii++) {
				grid_out(ii,jj) = VW(ii,jj)*cos(DW(ii,jj)*Cst::to_rad);
			}
		}
	}
	if (parameter==MeteoGrids::DW) {
		readWind(filename, date);
		grid_out = DW;
	}
	if (parameter==MeteoGrids::VW) {
		readWind(filename, date);
		grid_out = VW;
	}

	if (grid_out.empty()) {
		ostringstream ss;
		ss << "No suitable data found for parameter " << MeteoGrids::getParameterName(parameter) << " ";
		ss << "at time step " << date.toString(Date::ISO) << " in file \"" << filename << "\"";
		throw NoDataException(ss.str(), AT);
	}

	//correcting wind speeds
	/*if (parameter==MeteoGrids::U || parameter==MeteoGrids::V || parameter==MeteoGrids::W || parameter==MeteoGrids::VW || parameter==MeteoGrids::VW_MAX) {
		//we need to compute the wind at 7.5m
		Grid2DObject Z0;
		if (read2DGrid_indexed(83.2, 1, 0, date, Z0)) { //Z0
			for (size_t jj=0; jj<grid_out.getNy(); jj++) {
				for (size_t ii=0; ii<grid_out.getNx(); ii++) {
					grid_out(ii,jj) = Atmosphere::windLogProfile(grid_out(ii,jj), 10., 7.5, Z0(ii,jj));
				}
			}
		} else {
			const double wind_factor = Atmosphere::windLogProfile(1., 10., 7.5, 0.03);
			grid_out.grid2D *= wind_factor;
		}
	}*/
}

void GRIBIO::readDEM(DEMObject& dem_out)
{
	const Date d; //ie: undef. This will be caught when reading the GRIB file
	const std::string filename = cfg.get("DEMFILE", "Input");
	read2DGrid(filename, dem_out, MeteoGrids::DEM, d);
	if (update_dem) {
		dem_out.update();
	} else {
		const int dem_ppt=dem_out.getUpdatePpt();
		if (dem_ppt&DEMObject::SLOPE) {
			Grid2DObject slope;
			read2DGrid(filename, slope, MeteoGrids::SLOPE, d);
			dem_out.slope=slope.grid2D;
			Grid2DObject azi;
			read2DGrid(filename, azi, MeteoGrids::AZI, d);
			dem_out.azi=azi.grid2D;
		}
		if (dem_ppt&DEMObject::NORMAL || dem_ppt&DEMObject::CURVATURE) {
			//we will only update the normals and/or curvatures, then revert update properties
			if (dem_ppt&DEMObject::NORMAL && dem_ppt&DEMObject::CURVATURE) dem_out.setUpdatePpt((DEMObject::update_type)(DEMObject::NORMAL|DEMObject::CURVATURE));
			else if (dem_ppt&DEMObject::NORMAL) dem_out.setUpdatePpt(DEMObject::NORMAL);
			else if (dem_ppt&DEMObject::CURVATURE) dem_out.setUpdatePpt(DEMObject::CURVATURE);

			dem_out.update();
			dem_out.setUpdatePpt((DEMObject::update_type)dem_ppt);
		}

		dem_out.updateAllMinMax();
	}
}

void GRIBIO::scanMeteoPath()
{
	std::list<std::string> dirlist;
	FileUtils::readDirectory(meteopath_in, dirlist, meteo_ext);
	dirlist.sort();

	//Check date in every filename and cache it
	std::list<std::string>::const_iterator it = dirlist.begin();
	while ((it != dirlist.end())) {
		const std::string& filename = *it;
		const std::string::size_type spos = filename.find_first_of("0123456789");
		Date date;
		IOUtils::convertString(date, filename.substr(spos,10), tz_in);
		const std::pair<Date,std::string> tmp(date, filename);

		cache_meteo_files.push_back(tmp);
		it++;
	}
}

void GRIBIO::readMeteoData(const Date& dateStart, const Date& dateEnd,
                             std::vector< std::vector<MeteoData> >& vecMeteo)
{
	if (!meteo_initialized) {
		readStations(vecPts);
		scanMeteoPath();
		meteo_initialized=true;
	}

	vecMeteo.clear();

	double *lats = (double*)malloc(vecPts.size()*sizeof(double));
	double *lons = (double*)malloc(vecPts.size()*sizeof(double));
	std::vector<StationData> meta; //metadata for meteo time series
	bool meta_ok=false; //set to true once the metadata have been read

	//find index of first time step
	size_t idx_start;
	bool start_found=false;
	for (idx_start=0; idx_start<cache_meteo_files.size(); idx_start++) {
		if (dateStart<cache_meteo_files[idx_start].first) {
			start_found=true;
			break;
		}
	}

	if (start_found==false) {
		free(lats); free(lons);
		return;
	}

	if (idx_start>0) idx_start--; //start with first element before dateStart (useful for resampling)

	try {
		for (size_t ii=idx_start; ii<cache_meteo_files.size(); ii++) {
			const Date& date = cache_meteo_files[ii].first;
			if (date>dateEnd) break;
			const std::string filename( meteopath_in+"/"+cache_meteo_files[ii].second );

			if (!indexed || idx_filename!=filename) {
				cleanup();
				indexFile(filename); //this will also read geolocalization
			}
			if (!meta_ok) {
				if (readMeteoMeta(vecPts, meta, lats, lons)==false) {
					//some points have been removed vecPts has been changed -> re-reading
					free(lats); free(lons);
					lats = (double*)malloc(vecPts.size()*sizeof(double));
					lons = (double*)malloc(vecPts.size()*sizeof(double));
					readMeteoMeta(vecPts, meta, lats, lons);
				}
				vecMeteo.insert(vecMeteo.begin(), vecPts.size(), std::vector<MeteoData>()); //allocation for the vectors now that we know how many true stations we have
				meta_ok=true;
			}

			std::vector<MeteoData> Meteo;
			readMeteoStep(meta, lats, lons, date, Meteo);
			for (size_t jj=0; jj<vecPts.size(); jj++)
				vecMeteo[jj].push_back(Meteo[jj]);
		}
	} catch(...) {
		free(lats); free(lons);
		cleanup();
		throw;
	}

	free(lats); free(lons);
}

bool GRIBIO::removeDuplicatePoints(std::vector<Coords>& vecPoints, double *lats, double *lons)
{ //remove potential duplicates. Returns true if some have been removed
	const size_t npoints = vecPoints.size();
	std::vector<size_t> deletions;
	deletions.reserve(npoints);
	for (size_t ii=0; ii<npoints; ii++) {
		const double lat = lats[ii];
		const double lon = lons[ii];
		for (size_t jj=ii+1; jj<npoints; jj++) {
			if (lat==lats[jj] && lon==lons[jj]) {
				deletions.push_back(jj);
			}
		}
	}

	//we need to erase from the end in order to keep the index unchanged...
	for (size_t ii=deletions.size(); ii>0; ii--) {
		const size_t index=deletions[ii-1];
		vecPoints.erase(vecPoints.begin()+index);
	}

	if (!deletions.empty()) return true;
	return false;
}

bool GRIBIO::readMeteoMeta(std::vector<Coords>& vecPoints, std::vector<StationData> &stations, double *lats, double *lons)
{//return true if the metadata have been read, false if it needs to be re-read (ie: some points were leading to duplicates -> vecPoints has been changed)
	stations.clear();

	GRIB_CHECK(grib_index_select_double(idx,"marsParam",8.2),0); //This is the DEM
	GRIB_CHECK(grib_index_select_long(idx,"indicatorOfTypeOfLevel", 1),0);

	int err=0;
	grib_handle* h = grib_handle_new_from_index(idx,&err);
	if (h==NULL) {
		cleanup();
		throw IOException("Can not find DEM grid in GRIB file!", AT);
	}

	const size_t npoints = vecPoints.size();
	double latitudeOfSouthernPole, longitudeOfSouthernPole;
	GRIB_CHECK(grib_get_double(h,"latitudeOfSouthernPoleInDegrees",&latitudeOfSouthernPole),0);
	GRIB_CHECK(grib_get_double(h,"longitudeOfSouthernPoleInDegrees",&longitudeOfSouthernPole),0);
	latitudeOfNorthernPole = -latitudeOfSouthernPole;
	longitudeOfNorthernPole = longitudeOfSouthernPole+180.;

	long Ni;
	GRIB_CHECK(grib_get_long(h,"Ni",&Ni),0);

	//build GRIB local coordinates for the points
	for (size_t ii=0; ii<npoints; ii++) {
		CoordsAlgorithms::trueLatLonToRotated(latitudeOfNorthernPole, longitudeOfNorthernPole, vecPoints[ii].getLat(), vecPoints[ii].getLon(), lats[ii], lons[ii]);
	}

	//retrieve nearest points
	double *outlats = (double*)malloc(npoints*sizeof(double));
	double *outlons = (double*)malloc(npoints*sizeof(double));
	double *values = (double*)malloc(npoints*sizeof(double));
	double *distances = (double*)malloc(npoints*sizeof(double));
	int *indexes = (int *)malloc(npoints*sizeof(int));
	if (grib_nearest_find_multiple(h, 0, lats, lons, npoints, outlats, outlons, values, distances, indexes)!=0) {
		grib_handle_delete(h);
		cleanup();
		throw IOException("Errro when searching for nearest points in DEM", AT);
	}

	//remove potential duplicates
	if (removeDuplicatePoints(vecPoints, outlats, outlons)==true) {
		free(outlats); free(outlons); free(values); free(distances); free(indexes);
		grib_handle_delete(h);
		return false;
	}

	//fill metadata
	for (size_t ii=0; ii<npoints; ii++) {
		StationData sd;
		sd.position.setProj(coordin, coordinparam);
		double true_lat, true_lon;
		CoordsAlgorithms::rotatedToTrueLatLon(latitudeOfNorthernPole, longitudeOfNorthernPole, outlats[ii], outlons[ii], true_lat, true_lon);
		sd.position.setLatLon(true_lat, true_lon, values[ii]);
		sd.stationID = "Point_" + IOUtils::toString(indexes[ii]);
		ostringstream ss2;
		ss2 << "GRIB point (" << indexes[ii] % Ni << "," << indexes[ii] / Ni << ")";
		sd.stationName=ss2.str();
		stations.push_back(sd);
	}

	free(outlats); free(outlons); free(values); free(distances); free(indexes);
	grib_handle_delete(h);
	return true;
}

bool GRIBIO::readMeteoValues(const double& marsParam, const long& levelType, const long& i_level, const Date& i_date, const size_t& npoints, double *lats, double *lons, double *values)
{
	GRIB_CHECK(grib_index_select_double(idx,"marsParam",marsParam),0);
	GRIB_CHECK(grib_index_select_long(idx,"indicatorOfTypeOfLevel", levelType),0);

	grib_handle* h=NULL;
	int err=0;
	while ((h = grib_handle_new_from_index(idx,&err)) != NULL) {
		if (!h) {
			cleanup();
			throw IOException("Unable to create grib handle from index", AT);
		}

		Date base_date;
		double P1, P2;
		getDate(h, base_date, P1, P2);

		//see WMO code table5 for definitions of timeRangeIndicator. http://dss.ucar.edu/docs/formats/grib/gribdoc/timer.html
		long timeRange;
		GRIB_CHECK(grib_get_long(h,"timeRangeIndicator", &timeRange),0);

		long level=0;
		if (i_level!=0) GRIB_CHECK(grib_get_long(h,"level", &level),0);
		if (level==i_level) {
			if ( (i_date.isUndef()) ||
			    (timeRange==0 && i_date==base_date+P1) ||
			    (timeRange==1 && i_date==base_date) ||
			    ((timeRange==2 || timeRange==3) && i_date>=base_date+P1 && i_date<=base_date+P2) ||
			    ((timeRange==4 || timeRange==5) && i_date==base_date+P2) ) {
				double *outlats = (double*)malloc(npoints*sizeof(double));
				double *outlons = (double*)malloc(npoints*sizeof(double));
				double *distances = (double*)malloc(npoints*sizeof(double));
				int *indexes = (int *)malloc(npoints*sizeof(int));
				if (grib_nearest_find_multiple(h, 0, lats, lons, npoints, outlats, outlons, values, distances, indexes)!=0) {
					grib_handle_delete(h);
					cleanup();
					throw IOException("Errro when searching for nearest points in DEM", AT);
				}

				free(outlats); free(outlons); free(distances); free(indexes);
				grib_handle_delete(h);
				return true;
			}
		}
		grib_handle_delete(h);
	}
	return false;
}

void GRIBIO::fillMeteo(double *values, const MeteoData::Parameters& param, const size_t& npoints, std::vector<MeteoData> &Meteo) {
	for (size_t ii=0; ii<npoints; ii++) {
		Meteo[ii](param) = values[ii];
	}
}

void GRIBIO::readMeteoStep(std::vector<StationData> &stations, double *lats, double *lons, const Date i_date, std::vector<MeteoData> &Meteo)
{
	const size_t npoints = stations.size();

	for (size_t ii=0; ii<npoints; ii++) {
		MeteoData md;
		md.meta = stations[ii];
		md.date = i_date;
		Meteo.push_back(md);
	}

	double *values = (double*)malloc(npoints*sizeof(double));
	double *values2 = (double*)malloc(npoints*sizeof(double)); //for extra parameters

	//basic meteorological parameters
	if (readMeteoValues(1.2, 1, 0, i_date, npoints, lats, lons, values)) fillMeteo(values, MeteoData::P, npoints, Meteo); //PS
	if (readMeteoValues(11.2, 105, 2, i_date, npoints, lats, lons, values)) fillMeteo(values, MeteoData::TA, npoints, Meteo); //T_2M
	if (readMeteoValues(197.201, 111, 0, i_date, npoints, lats, lons, values)) fillMeteo(values, MeteoData::TSS, npoints, Meteo); //T_SO take 118, BRTMP instead?
	if (readMeteoValues(11.2, 1, 0, i_date, npoints, lats, lons, values)) fillMeteo(values, MeteoData::TSG, npoints, Meteo); //T_G
	if (readMeteoValues(52.2, 105, 2, i_date, npoints, lats, lons, values)) fillMeteo(values, MeteoData::RH, npoints, Meteo); //RELHUM_2M
	else if (readMeteoValues(17.2, 105, 2, i_date, npoints, lats, lons, values)) { //TD_2M
		for (size_t ii=0; ii<npoints; ii++) {
			if (Meteo[ii](MeteoData::TA)!=IOUtils::nodata)
				Meteo[ii](MeteoData::RH) = Atmosphere::DewPointtoRh(values[ii], Meteo[ii](MeteoData::TA), true);
		}
	}

	//hydrological parameters
	if (readMeteoValues(61.2, 1, 0, i_date, npoints, lats, lons, values)) fillMeteo(values, MeteoData::PSUM, npoints, Meteo); //tp
	if (readMeteoValues(66.2, 1, 0, i_date, npoints, lats, lons, values)) fillMeteo(values, MeteoData::HS, npoints, Meteo);
	else if (readMeteoValues(133.201, 1, 0, i_date, npoints, lats, lons, values)  //RHO_SNOW
	   && readMeteoValues(65.2, 1, 0, i_date, npoints, lats, lons, values2)) { //W_SNOW
		for (size_t ii=0; ii<npoints; ii++) {
			Meteo[ii](MeteoData::HS) = values2[ii] / values[ii];
		}
	}

	//radiation parameters
	if (readMeteoValues(115.2, 1, 0, i_date, npoints, lats, lons, values)) { //long wave
		for (size_t ii=0; ii<npoints; ii++) {
			Meteo[ii](MeteoData::ISWR) = -values[ii];
		}
	} else if (readMeteoValues(25.201, 1, 0, i_date, npoints, lats, lons, values)) fillMeteo(values, MeteoData::ILWR, npoints, Meteo); //ALWD_S
	if (readMeteoValues(109.250, 1, 0, i_date, npoints, lats, lons, values)) fillMeteo(values, MeteoData::ISWR, npoints, Meteo); //GLOB_H
	else {
		const bool read_dir = (readMeteoValues(108.250, 1, 0, i_date, npoints, lats, lons, values) //ASWDIR_SH
		                                 || readMeteoValues(115.2, 1, 0, i_date, npoints, lats, lons, values) //O_ASWDIR_S
		                                 || readMeteoValues(22.201, 1, 0, i_date, npoints, lats, lons, values)); //ASWDIR_S
		const bool read_diff = (readMeteoValues(117.2, 1, 0, i_date, npoints, lats, lons, values2) //O_ASWDIFD_S
		                                  || readMeteoValues(23.201, 1, 0, i_date, npoints, lats, lons, values2)); //ASWDIFD_S
		if (read_dir && read_diff){
			for (size_t ii=0; ii<npoints; ii++) {
				Meteo[ii](MeteoData::ISWR) = values[ii] + values2[ii];
			}
		}
	}
	if (readMeteoValues(84.2, 1, 0, i_date, npoints, lats, lons, values)) { //ALB_RAD
		for (size_t ii=0; ii<npoints; ii++) {
			if (Meteo[ii](MeteoData::ISWR)!=IOUtils::nodata) Meteo[ii](MeteoData::RSWR) = Meteo[ii](MeteoData::ISWR) * values[ii]/100.;
		}
	}

	//Wind parameters
	if (readMeteoValues(187.201, 105, 10, i_date, npoints, lats, lons, values)) fillMeteo(values, MeteoData::VW_MAX, npoints, Meteo); //VMAX_10M
	if (readMeteoValues(31.2, 105, 10, i_date, npoints, lats, lons, values)) fillMeteo(values, MeteoData::DW, npoints, Meteo); //DD_10M
	else {
		if (readMeteoValues(34.2, 105, 10, i_date, npoints, lats, lons, values) //V_10M
		   && readMeteoValues(33.2, 105, 10, i_date, npoints, lats, lons, values2)) { //U_10M
			for (size_t ii=0; ii<npoints; ii++) {
				Meteo[ii](MeteoData::DW) = fmod( atan2( values2[ii], values[ii] ) * Cst::to_deg + 360. + bearing_offset, 360.); // turn into degrees [0;360)
			}
		}
	}
	if (readMeteoValues(32.2, 105, 10, i_date, npoints, lats, lons, values)) fillMeteo(values, MeteoData::VW, npoints, Meteo); //FF_10M
	else {
		if (readMeteoValues(34.2, 105, 10, i_date, npoints, lats, lons, values) //V_10M
		   && readMeteoValues(33.2, 105, 10, i_date, npoints, lats, lons, values2)) { //U_10M
			for (size_t ii=0; ii<npoints; ii++) {
				Meteo[ii](MeteoData::VW) =  sqrt( Optim::pow2(values[ii]) + Optim::pow2(values2[ii]) );
			}
		}
	}

	free(values); free(values2);
}

void GRIBIO::cleanup() throw()
{
	if (fp!=NULL) fclose(fp); fp=NULL;
	if (idx!=NULL) grib_index_delete(idx); idx=NULL;
	idx_filename.clear();
}

} //namespace
