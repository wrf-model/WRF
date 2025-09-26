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

#include <meteoio/IOManager.h>

using namespace std;

namespace mio {
/**
 * @page spatial_resampling Spatial resampling and regridding handling
 * @section sp_resampling Spatial resampling
 * It is possible to use spatially interpolated meteorological fields or time series of 2D grids to extract meteorological time series for a set of points.
 * This is handled as "spatial resampling" and the data **will seem to originate from these virtual stations points** where no station is present. This obviously
 * comes at the cost of much higher run times. Several strategies are available (with the *RESAMPLING_STRATEGY* keyword):
 *     + VSTATIONS: points measurements are spatially interpolated at the chosen locations;
 *     + GRID_EXTRACT: gridded values are extracted for the cells containing the given locations;
 *     + GRID_SMART: the four nodes surrounding each given locations are extracted and potential duplicates are removed, then the meteo fields are 
 *       spatially interpolated at the chosen locations;
 *     + GRID_ALL: all grid points are extracted;
 * 
 * For VSTATIONS and GRID_SMART, it is necessary to provide a hint on how often the data should be extracted versus temporally interpolated between extracted 
 * points. This is done by providing a refresh rate and an offset (both in seconds, with the VSTATIONS_REFRESH_RATE and VSTATIONS_REFRESH_OFFSET keywords, respectively)
 * \image html vstations_sampling.png "Resampling workflow"
 * \image latex vstations_sampling.eps "Resampling workflow" width=0.9\textwidth
 * 
 * Behind the scene, this is a two stages setup: the IOManager uses either a TimeSeriesManager or a GridsManager object to retrieve the real data
 * and then injects this data as raw data into another TimeSeriesManager (so the temporal interpolations can be computed, the data generators can be called, etc).
 * Then the IOManager request the data as usual from the final TimeSeriesManager or from the Meteo2DInterpolator (in the case of grids resampling).
 *
 * @subsection vstations Virtual stations
 * The data from real input stations (as read by the plugin defined with the METEO key in the [input] section) is filtered/processed, temporally interpolated and 
 * spatially interpolated as defined in the configuration file. Then time series are reconstructed from these grids at a set of defined points (which will receive
 * station IDs such as <i>VIR#</i> for each station). This behavior is configured by the following keys (in the [Input] section):
 *    + RESAMPLING_STRATEGY set to VSTATIONS;
 *    + VSTATION# : provide the lat, lon and altitude or easting, northing and altitude for a virtual station (see \link Coords::Coords(const std::string& in_coordinatesystem, const std::string& in_parameters, std::string coord_spec) Coords()\endlink for the syntax);
 *    + VIRTUAL_PARAMETERS: list of MeteoData::Parameters that have to be interpolated to populate the virtual stations;
 *    + VSTATIONS_REFRESH_RATE: how often to rebuild the spatial interpolations, in seconds;
 *    + VSTATIONS_REFRESH_OFFSET: time offset to the stations' refresh rate, in seconds (default: 0);
 *    + INTERPOL_USE_FULL_DEM: should the spatial interpolations be performed on the whole DEM? (this is necessary for some algorithms, for example WINSTRAL).
 * 
 * @note Please keep in mind that the WINDOW_SIZE in the [Interpolations1D] section and BUFFER_SIZE in the [General] section have a direct influence on the success 
 * (or lack of) the virtual stations: they will contribute to define how much raw data should be read and can lead to an exception being thrown when working with
 * little data and large buffer or resampling windows values. 
 * 
 * @note Currently, a DEM also has to be provided since this will be used to retrieve the elevation, slope and azimuth of the virtual stations.
 * 
 * In the example provided below, 4 stations provide the original data that will be spatially interpolated at 2 points (or virtual stations, VIR1 and VIR2) for
 * 7 meteorological parameters. Every 6 hours, with starting offset of one hour, the original data will be spatially interpolated (so at 01:00, 07:00, 13:00 and 19:00).
 * Any data requested at other time steps will be temporally resampled from the spatially interpolated data.
 * @code
 * [Input]
 * DEM = ARC
 * DEMFILE = ./input/surface-grids/davos.asc
 *
 * #here, the real data as measured by some stations 
 * METEO		= IMIS
 * DBNAME		= sdbo
 * DBUSER		= xxx
 * DBPASS		= xxx
 * STATION1	= *WFJ
 * STATION2	= STB2
 * STATION3	= WFJ2
 * STATION4	= *DAV
 * 
 * #here the locations where the data will be generated. The caller will only see these stations!
 * RESAMPLING_STRATEGY = VSTATIONS
 * VSTATION1 = latlon 46.793029 9.821343 1987
 * VSTATION2 = latlon 46.793031 9.831572 2300
 * Virtual_parameters = TA RH PSUM ILWR P VW RSWR
 * VSTATIONS_REFRESH_RATE = 21600
 * VSTATIONS_REFRESH_OFFSET = 3600
 * @endcode
 * 
 * @subsection grids_extract From gridded data
 * The meteorological time series are extracted from time series of user-provided grids. Therefore a plugin for 2D grids must have been defined (with the GRID2D key in
 * the [Input] section). The following keys control this downscaling process:
 *    + RESAMPLING_STRATEGY set to either *GRID_EXTRACT* or *GRID_ALL*;
 *    + VSTATION# : provide the lat, lon and altitude for the virtual station (only required for GRID_EXTRACT);
 *    + VIRTUAL_PARAMETERS: list of MeteoData::Parameters that have to be interpolated to populate the virtual stations.
 *
 * Currently, a DEM has to be provided in order to check the position of the stations and the consistency of the grids.
 * @code
 * [Input]
 * DEM     = NETCDF
 * DEMFILE = ./input/grids/era-interim-dem.nc
 * 
 * GRID2D    = NETCDF
 * GRID2DFILE = ./input/grids/era-interim.nc
 * NETCDF_SCHEMA = ECMWF
 * 
 * RESAMPLING_STRATEGY = GRID_EXTRACT
 * Virtual_parameters = TA RH PSUM ILWR P VW ISWR
 * 
 * #here the locations where the data will be generated. The caller will only see these stations!
 * VSTATION1 = latlon 43.359188 6.693612 150 ;great station
 * VSTATION2 = latlon 43.324887 6.629711 57 ;another great station
 * @endcode
 * @note If the temporal resolution of the gridded data is greater than the WINDOW_SIZE of the [Interpolations1D], then no data will be interpolated.
 * 
 * @subsubsection grids_smart From gridded data, with spatial interpolations
 * This is a combination between \ref grids_extract "gridded data extraction" and \ref vstations "Vstations": the meteorological time series are extracted 
 * from time series of user-provided grids at 4 points around each provided location and then spatially interpolated at the said locations. 
 * Therefore, the following additional keys are required:
 *    + RESAMPLING_STRATEGY set to *GRID_SMART*;
 *    + VSTATION# : provide the lat, lon and altitude for the virtual station;
 *    + VIRTUAL_PARAMETERS: list of MeteoData::Parameters that have to be interpolated to populate the virtual stations;
 *    + VSTATIONS_REFRESH_RATE: how often to rebuild the spatial interpolations, in seconds;
 *    + VSTATIONS_REFRESH_OFFSET: time offset to the stations' refresh rate, in seconds;
 *    + INTERPOL_USE_FULL_DEM: should the spatial interpolations be performed on the whole DEM? (this is necessary for some algorithms, for example WINSTRAL).
 *
 * @code
 * [Input]
 * DEM     = NETCDF
 * DEMFILE = ./input/grids/era-interim-dem.nc
 * 
 * GRID2D    = NETCDF
 * GRID2DFILE = ./input/grids/era-interim.nc
 * NETCDF_SCHEMA = ECMWF
 * 
 * RESAMPLING_STRATEGY = GRID_SMART
 * Virtual_parameters = TA RH PSUM ILWR P VW ISWR
 * 
 * #here the locations where the data will be generated. The caller will only see these stations!
 * VSTATION1 = latlon 43.359188 6.693612 150 ;great station
 * VSTATION2 = latlon 43.324887 6.629711 57 ;another great station
 * @endcode
 * 
 * @section grids_resample Resampling of gridded data
 * A new keyword has been introduce and will be expanded: *REGRIDDING_STRATEGY*. It describes how to generate gridded data out of other gridded data. 
 * It currently only accepts the "GRID_RESAMPLE" argument (all grid points are extracted and used with an additional DEM to resample the grids to 
 * a different resolution (by calling spatial interpolations)). Ultimately, it should offer various methods... Please note that it is currently NOT 
 * possible to use both RESAMPLING_STRATEGY and REGRIDDING_STRATEGY
 * 
 * 
 * The process is very similar to what has been laid out for the \ref sp_resampling "spatial resampling". The meteorological time series are extracted 
 * as described out in \ref grids_extract "From gridded data" for each grid point and forwarded to a 
 * Meteo2DInterpolator to be spatially interpolated over the provided DEM. This therefore performs grid resampling and accounts for elevation gradients, etc
 * as configured in the [2DInterpolations] section. The following keys control this downscaling process:
 *    + REGRIDDING_STRATEGY set to *GRID_RESAMPLE*;
 *    + SOURCE_DEM : filename of the DEM to be read by the GRID2D plugin. This DEM provides the elevations, slopes, etc for the source grids.
 *    + VIRTUAL_PARAMETERS: list of MeteoData::Parameters that have to be interpolated to populate the virtual stations.
 *
 * @code
 * [Input]
 * DEM     = ARC
 * DEMFILE = ./input/grids/davos.asc
 * 
 * GRID2D    = NETCDF
 * GRID2DFILE = ./input/grids/era-interim.nc
 * SOURCE_DEM = ./input/grids/era-interim-dem.nc
 * NETCDF_SCHEMA = ECMWF
 * 
 * REGRIDDING_STRATEGY = GRID_RESAMPLE
 * Virtual_parameters = TA RH PSUM ILWR P VW ISWR
 * @endcode
 * @note The resampled grids won't be provided by the read2DGrid() call but by the getMeteoData() call since they are considered as spatial interpolations.
 * 
 */

IOUtils::OperationMode IOManager::getIOManagerTSMode(const Config& i_cfg)
{
	//HACK for now, we consider that EITHER Resampling_strategy or Regridding_strategy is provided
	const std::string resampling_strategy_str = i_cfg.get("Resampling_strategy", "Input", "");
	const std::string regridding_strategy_str = i_cfg.get("Regridding_strategy", "Input", "");
	if (!resampling_strategy_str.empty() && !regridding_strategy_str.empty())
		throw InvalidArgumentException("Currently, it is not posible to use both resampling_strategy and regridding_strategy", AT);
	
	if (resampling_strategy_str.empty())
		return getIOManagerGridMode(i_cfg); //either STD or a regriding strategy
	if (resampling_strategy_str=="VSTATIONS")
		return IOUtils::VSTATIONS;
	if (resampling_strategy_str=="GRID_EXTRACT")
		return IOUtils::GRID_EXTRACT;
	if (resampling_strategy_str=="GRID_ALL")
		return IOUtils::GRID_ALL;
	if (resampling_strategy_str=="GRID_SMART")
		return IOUtils::GRID_SMART;
	//TODO TRAJECTORY
	
	throw InvalidArgumentException("The selected resampling_strategy is not supported", AT);
}

IOUtils::OperationMode IOManager::getIOManagerGridMode(const Config& i_cfg)
{
	const std::string regridding_strategy_str = i_cfg.get("Regridding_strategy", "Input", "");
	if (regridding_strategy_str.empty())
		return IOUtils::STD;
	if (regridding_strategy_str=="GRID_RESAMPLE")
		return IOUtils::GRID_RESAMPLE;
	
	throw InvalidArgumentException("The selected regridding_strategy is not supported", AT);
}

//TODO write an IOHandler that can directly tap into the buffers of a tsm or gdm
IOManager::IOManager(const std::string& filename_in) : cfg(filename_in), ts_mode(getIOManagerTSMode(cfg)), iohandler(cfg),
                                                       tsm1(iohandler, cfg, 1, ts_mode), tsm2(iohandler, cfg, 2), gdm1(iohandler, cfg), interpolator(cfg, tsm1, gdm1), source_dem(),
                                                       v_params(), grids_params(), v_stations(), v_gridstations(), vstations_refresh_rate(3600), vstations_refresh_offset(0)
{
	initIOManager();
}

IOManager::IOManager(const Config& i_cfg) : cfg(i_cfg), ts_mode(getIOManagerTSMode(cfg)), iohandler(cfg),
                                            tsm1(iohandler, cfg, 1, ts_mode), tsm2(iohandler, cfg, 2), gdm1(iohandler, cfg), interpolator(cfg, tsm1, gdm1), source_dem(),
                                            v_params(), grids_params(), v_stations(), v_gridstations(), vstations_refresh_rate(3600), vstations_refresh_offset(0)
{
	initIOManager();
}

void IOManager::initIOManager()
{
	//TODO support extra parameters by getting the param index from vecTrueMeteo[0]
	if (ts_mode>=IOUtils::GRID_EXTRACT) {
		std::vector<std::string> vecStr;
		cfg.getValue("Virtual_parameters", "Input", vecStr);
		for (size_t ii=0; ii<vecStr.size(); ii++) {
			const size_t param_idx = MeteoGrids::getParameterIndex( vecStr[ii] );
			if (param_idx==IOUtils::npos)
				throw InvalidArgumentException("Invalid parameter '" + vecStr[ii] + "', only standard parameters can be extracted from grids for virtual stations! ", AT);
			grids_params.push_back( param_idx );
		}
	}
	
	if (ts_mode==IOUtils::VSTATIONS || ts_mode==IOUtils::GRID_SMART) {
		std::vector<std::string> vecStr;
		cfg.getValue("Virtual_parameters", "Input", vecStr);
		for (size_t ii=0; ii<vecStr.size(); ii++) {
			const size_t param_idx = MeteoData::getStaticParameterIndex( vecStr[ii] );
			if (param_idx==IOUtils::npos)
				throw InvalidArgumentException("Invalid parameter '" + vecStr[ii] + "', only standard parameters can be extracted from grids for virtual stations! ", AT);
			v_params.push_back( param_idx );
		}
		tsm2.setProcessingLevel(IOUtils::resampled | IOUtils::generated); //in this case, we do not want to re-apply the filters (or force filter pass=2 on tsm2?)
	}
	
	if (ts_mode!=IOUtils::STD) initVirtualStations();
}

void IOManager::initVirtualStations()
{
	if (ts_mode==IOUtils::GRID_RESAMPLE) {
		source_dem.setUpdatePpt((DEMObject::update_type)(DEMObject::SLOPE));
		const std::string source_dem_str = cfg.get("Source_dem", "Input");
		gdm1.read2DGrid(source_dem, source_dem_str);
		source_dem.update();
	} else {
		gdm1.readDEM(source_dem);
	}
	
	if (ts_mode==IOUtils::GRID_ALL || ts_mode==IOUtils::GRID_RESAMPLE) {
		v_gridstations = gdm1.initVirtualStationsAtAllGridPoints(source_dem);
	} else {
		if (ts_mode==IOUtils::GRID_EXTRACT || ts_mode==IOUtils::GRID_SMART) {
			const bool fourNeighbors = (ts_mode==IOUtils::GRID_SMART);
			v_gridstations = gdm1.initVirtualStations(source_dem, true, fourNeighbors);
		}
		
		if (ts_mode==IOUtils::VSTATIONS || ts_mode==IOUtils::GRID_SMART) {
			cfg.getValue("VSTATIONS_REFRESH_RATE", "Input", vstations_refresh_rate);
			cfg.getValue("VSTATIONS_REFRESH_OFFSET", "Input", vstations_refresh_offset, IOUtils::nothrow);
			v_stations = gdm1.initVirtualStations(source_dem, false, false);
		}
	}
}

void IOManager::setProcessingLevel(const unsigned int& i_level)
{
	tsm1.setProcessingLevel(i_level);
	gdm1.setProcessingLevel(i_level);
}

void IOManager::clear_cache()
{
	tsm1.clear_cache( TimeSeriesManager::ALL );
	tsm2.clear_cache( TimeSeriesManager::ALL );
	gdm1.clear_cache();
}

size_t IOManager::getStationData(const Date& date, STATIONS_SET& vecStation)
{
	vecStation.clear();

	if (ts_mode==IOUtils::STD) return tsm1.getStationData(date, vecStation);
	
	if (ts_mode==IOUtils::VSTATIONS || ts_mode==IOUtils::GRID_SMART) {
		if (v_stations.empty()) initVirtualStations();
		vecStation = v_stations;
	} else {
		if (v_gridstations.empty()) initVirtualStations();
		vecStation = v_gridstations;
	}
	
	return vecStation.size();
}

//TODO: smarter rebuffer! (ie partial)
size_t IOManager::getMeteoData(const Date& dateStart, const Date& dateEnd, std::vector< METEO_SET >& vecVecMeteo) 
{
	if (ts_mode==IOUtils::STD) return tsm1.getMeteoData(dateStart, dateEnd, vecVecMeteo);
	
	if (ts_mode>=IOUtils::GRID_EXTRACT && ts_mode!=IOUtils::GRID_SMART) {
		const Date bufferStart( tsm1.getBufferStart( TimeSeriesManager::RAW ) );
		const Date bufferEnd( tsm1.getBufferEnd(  TimeSeriesManager::RAW  ) );
		
		if (bufferStart.isUndef() || dateStart<bufferStart || dateEnd>bufferEnd) {
			vecVecMeteo = gdm1.getVirtualStationsFromGrid(source_dem, grids_params, v_gridstations, dateStart, dateEnd);
			tsm1.push_meteo_data(IOUtils::raw, dateStart, dateEnd, vecVecMeteo);
		}
		
		return tsm1.getMeteoData(dateStart, dateEnd, vecVecMeteo);
	}
	
	if (ts_mode==IOUtils::GRID_SMART) {
		const Date bufferStart( tsm2.getBufferStart( TimeSeriesManager::RAW ) );
		const Date bufferEnd( tsm2.getBufferEnd( TimeSeriesManager::RAW ) );
		if (bufferStart.isUndef() || dateStart<bufferStart || dateEnd>bufferEnd) {
			tsm1.push_meteo_data(IOUtils::raw, dateStart, dateEnd, gdm1.getVirtualStationsFromGrid(source_dem, grids_params, v_gridstations, dateStart, dateEnd));
			tsm2.push_meteo_data(IOUtils::raw, dateStart, dateEnd, getVirtualStationsData(source_dem, dateStart, dateEnd));
		}
		
		return tsm2.getMeteoData(dateStart, dateEnd, vecVecMeteo);
	}
	
	if (ts_mode==IOUtils::VSTATIONS) {
		const Date bufferStart( tsm2.getBufferStart( TimeSeriesManager::RAW ) );
		const Date bufferEnd( tsm2.getBufferEnd( TimeSeriesManager::RAW ) );
		if (bufferStart.isUndef() || dateStart<bufferStart || dateEnd>bufferEnd) {
			tsm2.push_meteo_data(IOUtils::raw, dateStart, dateEnd, getVirtualStationsData(source_dem, dateStart, dateEnd));
		}
		
		return tsm2.getMeteoData(dateStart, dateEnd, vecVecMeteo);
	}
	
	throw InvalidArgumentException("Unsuppported operation_mode", AT);
}

//data can be raw or processed (filtered, resampled)
//TODO: smarter rebuffer! (ie partial)
size_t IOManager::getMeteoData(const Date& i_date, METEO_SET& vecMeteo)
{
	vecMeteo.clear();

	if (ts_mode==IOUtils::STD) {
		return tsm1.getMeteoData(i_date, vecMeteo);
	}
	
	if (ts_mode>=IOUtils::GRID_EXTRACT && ts_mode!=IOUtils::GRID_SMART) {
		const Date bufferStart( tsm1.getBufferStart( TimeSeriesManager::RAW ) );
		const Date bufferEnd( tsm1.getBufferEnd( TimeSeriesManager::RAW ) );
		if (bufferStart.isUndef() || i_date<bufferStart || i_date>bufferEnd) {
			Duration buffer_size, buffer_before;
			tsm1.getBufferProperties(buffer_size, buffer_before);
			
			const Date dateStart = i_date - buffer_before;
			const Date dateEnd( i_date - buffer_before + buffer_size + 1 );
			tsm1.push_meteo_data(IOUtils::raw, dateStart, dateEnd, gdm1.getVirtualStationsFromGrid(source_dem, grids_params, v_gridstations, dateStart, dateEnd));
		}
		return tsm1.getMeteoData(i_date, vecMeteo);
	}
	
	if (ts_mode==IOUtils::GRID_SMART) {
		//The user requirements must be fulfilled by tsm2's buffer and then we back-calculate the boundaries for tsm1
		const Date bufferStart( tsm2.getBufferStart( TimeSeriesManager::RAW ) );
		const Date bufferEnd( tsm2.getBufferEnd( TimeSeriesManager::RAW ) );

		if (bufferStart.isUndef() || i_date<bufferStart || i_date>bufferEnd) {
			Duration buffer_size, buffer_before;
			tsm2.getBufferProperties(buffer_size, buffer_before);
			
			//for GRID_SMART, we need to start before, because to spatially interpolate at dt-buff_before we need the data at dt-buff_before*2
			const Date dateStart1 = i_date - buffer_before*2.;
			const Date buff_start1( Date::rnd(dateStart1-vstations_refresh_offset/(24.*3600.), vstations_refresh_rate, Date::DOWN) + vstations_refresh_offset/(24.*3600.) );	
			const Date dateEnd1( i_date - buffer_before + buffer_size + 1 );
			tsm1.push_meteo_data(IOUtils::raw, buff_start1, dateEnd1, gdm1.getVirtualStationsFromGrid(source_dem, grids_params, v_gridstations, buff_start1, dateEnd1));
			
			const Date dateStart2( i_date - buffer_before );
			const Date dateEnd2( i_date - buffer_before + buffer_size );
			tsm2.push_meteo_data(IOUtils::raw, dateStart2, dateEnd2, getVirtualStationsData(source_dem, dateStart2, dateEnd2));
		}
		
		return tsm2.getMeteoData(i_date, vecMeteo);
	}
	
	if (ts_mode==IOUtils::VSTATIONS) {
		//The user requirements must be fulfilled by tsm2's buffer and then we back-calculate the boundaries for tsm1
		const Date bufferStart( tsm2.getBufferStart( TimeSeriesManager::RAW ) );
		const Date bufferEnd( tsm2.getBufferEnd( TimeSeriesManager::RAW ) );

		if (bufferStart.isUndef() || i_date<bufferStart || i_date>bufferEnd) {
			Duration buffer_size, buffer_before;
			tsm2.getBufferProperties(buffer_size, buffer_before);
		
			const Date dateStart( i_date - buffer_before );
			const Date buff_start( Date::rnd(dateStart-vstations_refresh_offset/(24.*3600.), vstations_refresh_rate, Date::DOWN) + vstations_refresh_offset/(24.*3600.) );	
			const Date dateEnd( i_date - buffer_before + buffer_size );
			tsm2.push_meteo_data(IOUtils::raw, buff_start, dateEnd, getVirtualStationsData(source_dem, buff_start, dateEnd));
		}
		
		return tsm2.getMeteoData(i_date, vecMeteo);
	}
	
	throw InvalidArgumentException("Unsuppported operation_mode", AT);
}

bool IOManager::getMeteoData(const Date& date, const DEMObject& dem, const MeteoData::Parameters& meteoparam,
                  Grid2DObject& result)
{
	std::string info_string;
	const bool status = getMeteoData(date, dem, meteoparam, result, info_string);
	cerr << "[i] Interpolating " << MeteoData::getParameterName(meteoparam);
	cerr << " (" << info_string << ") " << endl;
	return status;
}

bool IOManager::getMeteoData(const Date& date, const DEMObject& dem, const std::string& param_name,
                  Grid2DObject& result)
{
	std::string info_string;
	const bool status = getMeteoData(date, dem, param_name, result, info_string);
	cerr << "[i] Interpolating " << param_name;
	cerr << " (" << info_string << ") " << endl;
	return status;
}

bool IOManager::getMeteoData(const Date& date, const DEMObject& dem, const MeteoData::Parameters& meteoparam,
                  Grid2DObject& result, std::string& info_string)
{
	if (ts_mode==IOUtils::GRID_RESAMPLE) { //fill tsm1's buffer
		const Date bufferStart( tsm1.getBufferStart( TimeSeriesManager::RAW ) );
		const Date bufferEnd( tsm1.getBufferEnd( TimeSeriesManager::RAW ) );
		
		if (bufferStart.isUndef() || date<bufferStart || date>bufferEnd) {
			Duration buffer_size, buffer_before;
			tsm1.getBufferProperties(buffer_size, buffer_before);
			
			const Date dateStart( date - buffer_before );
			const Date dateEnd( date - buffer_before + buffer_size );
			tsm1.push_meteo_data(IOUtils::raw, dateStart, dateEnd, gdm1.getVirtualStationsFromGrid(source_dem, grids_params, v_gridstations, dateStart, dateEnd));
		}
	}

	info_string = interpolator.interpolate(date, dem, meteoparam, result);
	return (!result.empty());
}

bool IOManager::getMeteoData(const Date& date, const DEMObject& dem, const std::string& param_name,
                  Grid2DObject& result, std::string& info_string)
{
	if (ts_mode==IOUtils::GRID_RESAMPLE) { //fill tsm1's buffer
		const Date bufferStart( tsm1.getBufferStart( TimeSeriesManager::RAW ) );
		const Date bufferEnd( tsm1.getBufferEnd( TimeSeriesManager::RAW ) );
		
		if (bufferStart.isUndef() || date<bufferStart || date>bufferEnd) {
			Duration buffer_size, buffer_before;
			tsm1.getBufferProperties(buffer_size, buffer_before);
			
			const Date dateStart( date - buffer_before );
			const Date dateEnd( date - buffer_before + buffer_size );
			tsm1.push_meteo_data(IOUtils::raw, dateStart, dateEnd, gdm1.getVirtualStationsFromGrid(source_dem, grids_params, v_gridstations, dateStart, dateEnd));
		}
	}

	info_string = interpolator.interpolate(date, dem, param_name, result);
	return (!result.empty());
}

void IOManager::interpolate(const Date& date, const DEMObject& dem, const MeteoData::Parameters& meteoparam,
                            const std::vector<Coords>& in_coords, std::vector<double>& result)
{
	const std::string info_string( interpolator.interpolate(date, dem, meteoparam, in_coords, result) );
	cerr << "[i] Interpolating " << MeteoData::getParameterName(meteoparam);
	cerr << " (" << info_string << ") " << endl;
}

void IOManager::interpolate(const Date& date, const DEMObject& dem, const MeteoData::Parameters& meteoparam,
                            const std::vector<Coords>& in_coords, std::vector<double>& result, std::string& info_string)
{
	info_string = interpolator.interpolate(date, dem, meteoparam, in_coords, result);
}

void IOManager::interpolate(const Date& date, const DEMObject& dem, const MeteoData::Parameters& meteoparam,
                            const std::vector<StationData>& in_stations, std::vector<double>& result, std::string& info_string)
{
	info_string = interpolator.interpolate(date, dem, meteoparam, in_stations, result);
}

double IOManager::getAvgSamplingRate() const 
{
	return tsm1.getAvgSamplingRate();
}

void IOManager::add_to_points_cache(const Date& i_date, const METEO_SET& vecMeteo) 
{
	tsm1.add_to_points_cache(i_date, vecMeteo);
}

//this is only called when ts_mode==IOUtils::VSTATIONS or ts_mode==IOUtils::GRID_SMART
//the provided dateStart and dateEnd will be used to fill the buffers while the sampling rate will be 
//computed internally
std::vector<METEO_SET> IOManager::getVirtualStationsData(const DEMObject& dem, const Date& dateStart, const Date& dateEnd)
{
	std::vector<METEO_SET> vecvecMeteo(v_stations.size());
	const double date_inc = static_cast<double>(vstations_refresh_rate) / (24.*3600.);

	if (ts_mode==IOUtils::VSTATIONS) {
		tsm1.clear_cache( TimeSeriesManager::ALL );
	} else {
		tsm1.clear_cache( TimeSeriesManager::FILTERED );
		tsm1.clear_cache( TimeSeriesManager::POINTS );
	}
	tsm1.setRawBufferProperties(dateStart, dateEnd);
	
	METEO_SET vecMeteo;
	tsm1.getMeteoData(dateStart, vecMeteo); //force filling the filtered buffer (we know it will contain all the necessary data thanks to the setRawBufferProperties() call)
	const Date dataEnd( tsm1.getDataEnd( TimeSeriesManager::FILTERED ) ); //we won't try to spatially interpolate after the end of data
	if (dataEnd.isUndef()) return vecvecMeteo;

	for (Date date=dateStart; date<=std::min(dataEnd, dateEnd); date += date_inc) {
		//fill vecvecMeteo with metadata
		for (size_t ii=0; ii<v_stations.size(); ii++) {
			MeteoData md(date, v_stations[ii]);
			vecvecMeteo[ii].push_back( md );
		}

		//interpolate each field and fill vecvecMeteo with data
		for (size_t param=0; param<v_params.size(); param++) {
			std::vector<double> result;
			interpolator.interpolate(date, dem, static_cast<MeteoData::Parameters>(v_params[param]), v_stations, result, true); //do not throw exceptions for missing data
			for (size_t ii=0; ii<v_stations.size(); ii++) {
				vecvecMeteo[ii].back()(v_params[param]) = result[ii];
			}
		}
	}
	
	return vecvecMeteo;
}

const std::string IOManager::toString() const {
	ostringstream os;
	os << "<IOManager>\n";
	os << "Config cfg = " << hex << &cfg << dec << "\n";
	os << iohandler.toString();
	os << tsm1.toString();
	os << gdm1.toString();
	os << interpolator.toString();
	os << "</IOManager>\n";
	return os.str();
}

} //namespace
