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
#include <meteoio/plugins/OshdIO.h>
#include <meteoio/plugins/libMatioWrapper.h>
#include <meteoio/meteoLaws/Meteoconst.h>
#include <meteoio/FileUtils.h>

#include <matio.h>
#include <algorithm>

using namespace std;

namespace mio {
/**
 * @page oshd OshdIO
 * This plugin reads the meteorological forecast data from COSMO either as gridded data or downscaled for each of the Swiss meteorological
 * networks IMIS/ANETZ stations as preprocessed by the
 * <A HREF="www.wsl.ch/fe/gebirgshydrologie/schnee_hydro/oshd/index_EN">Operational Snow-Hydrological Service</A>
 * of the <A HREF="www.wsl.ch">WSL/SLF</A>. The data is written as Matlab
 * <A HREF="http://www.mathworks.com/help/pdf_doc/matlab/matfile_format.pdf">binary files (.mat)</A>, one per meteorological parameter and per timestep, 
 * available on an access-controlled server after each new <A HREF="www.cosmo-model.org/">COSMO</A> run. It therefore requires a third party 
 * library to read this file format: the Open Source <A HREF="https://sourceforge.net/projects/matio/">MatIO</A> library. This can be installed directly from
 * the default repositories under Linux or installed by downloading the proper package for Windows or OsX.
 * 
 * \note If non-ascii characters have been used and the file has been created under Windows, some of the strings might end up using the UTF-16 encoding.
 * This requires a recent version of libmatio (see <A HREF="https://github.com/tbeu/matio/issues/34">this issue</A>). Another option would be to 
 * add at the beginning of the Matlab routine a call to *feature('DefaultCharacterSet', 'UTF8')* in order to switch from the current default (which can be read by the same call, 
 * ommitting the 'UTF8' option) to the (<A HREF="http://blog.omega-prime.co.uk/?p=150">partial</A>) UTF-8  encoding of Matlab.
 * 
 * @section oshd_data_structure Data structure
 * The files are named with the following schema: <i>{parameter}_{timestep}_{cosmo model version}_{mode}_{run time}.mat</i> with the following possible values:
 *     + *parameter* is one of idfc, idrc, albd, ilwc, pair, prec, rcor, tcor, wcor, wdir;
 *     + *timestep* is written as purely numeric ISO with minute resolution;
 *     + *cosmo model version* could be any of cosmo7, cosmo2, cosmo1, cosmoE;
 *     + *mode* is a one letter code (F for Forecast, A for Analysis);
 *     + *run time* is the purely numeric ISO date and time of when COSMO produced the dataset.
 * 
 * The station data files have the following internal data structure (represented as "name {data type}"):
 * @verbatim
      stat {1x1 struct}
        ├── time {1x1 array of doubles}
        ├── data {1x623 array of doubles}
        ├── acro {1x623 array of arrays of char}
        ├── dunit {array of char}
        ├── type {array of char}
        └── name {array of char}
  @endverbatim
 * 
 * The stations' acronyms follow a fixed order but their coordinates must be provided in a separate file, given as *METAFILE* key (see below). 
 * If the number of stations in this list does not match the number of stations declared in one of the meteorological file, an exception will be thrown. 
 * This file must have the following structure (the *x* and *y* coordinates being the CH1903 easting and northing, respectively): 
 * @verbatim
      statlist {1x1 struct}
        ├── acro {1x623 array of arrays of char}
        ├── name {1x623 array of arrays of char}
        ├── x {1x623 array of doubles}
        ├── y {1x623 array of doubles}
        └── z {1x623 array of doubles}
  @endverbatim
 *
 * The gridded data have the following structure:
 * @verbatim
      grid {1x1 struct}
        ├── ncols {1x1 array of doubles}
        ├── nrows {1x1 array of doubles}
        ├── xllcorner {1x1 array of doubles}
        ├── yllcorner {1x1 array of doubles}
        ├── cellsize {1x1 array of doubles}
        ├── NODATA_value {1x1 array of doubles}
        ├── data {nrows x ncols array of double}
        └── desc {array of char}
  @endverbatim
 *
 *
 * @section oshd_keywords Keywords
 * This plugin uses the following keywords:
 * - COORDSYS: coordinate system (see Coords); [Input] and [Output] section
 * - COORDPARAM: extra coordinates parameters (see Coords); [Input] and [Output] section
 * - METEOPATH: directory containing all the data files with the proper file naming schema; [Input] section
 * - METEOPATH_RECURSIVE: should *meteopath* be searched recursively for files? (default: false); [Input] section
 * - STATION#: input stations' IDs (in METEOPATH). As many meteofiles as needed may be specified
 * - METAFILE: file containing the stations' IDs, names and location; [Input] section (either within METEOPATH if not path is 
 provided or within the provided path)
 * - DEMFILE: for reading the data as a DEMObject
 * - GRID2DPATH: meteo grids directory where to read/write the grids; [Input] and [Output] sections
 * - GRIDPATH_RECURSIVE: if set to true, grids will be searched recursively in GRID2DPATH (default: false)
 * - OSHD_DEBUG: write out extra information to better show what is in the files
 *
 * @section oshd_example Example use
 * @code
 * [Input]
 * METEO = OSHD
 * METEOPATH = /local/LATEST_03h_RUN
 * METEOPATH_RECURSIVE = true
 * METAFILE  = STAT_LIST.mat ;another possibility could be /local/metadata/STAT_LIST.mat
 * STATION1  = ATT2
 * STATION2  = WFJ2
 * @endcode
 *
 */

const char* OshdIO::meteo_ext = ".mat";
const double OshdIO::in_dflt_TZ = 0.; //COSMO data is always GMT
std::vector< std::pair<MeteoData::Parameters, std::string> > OshdIO::params_map;
std::map< MeteoGrids::Parameters, std::string > OshdIO::grids_map;
const bool OshdIO::__init = OshdIO::initStaticData();

OshdIO::OshdIO(const std::string& configfile) : cfg(configfile), cache_meteo_files(), cache_grid_files(), vecMeta(), vecIDs(), vecIdx(),
               coordin(), coordinparam(), grid2dpath_in(), in_meteopath(), in_metafile(), nrMetadata(0), debug(false)
{
	parseInputOutputSection();
}

OshdIO::OshdIO(const Config& cfgreader) : cfg(cfgreader), cache_meteo_files(), cache_grid_files(), vecMeta(), vecIDs(), vecIdx(),
               coordin(), coordinparam(), grid2dpath_in(), in_meteopath(), in_metafile(), nrMetadata(0), debug(false)
{
	parseInputOutputSection();
}

void OshdIO::parseInputOutputSection()
{
	cfg.getValue("OSHD_DEBUG", "INPUT", debug, IOUtils::nothrow);
	cfg.getValue("COORDSYS", "Input", coordin);
	cfg.getValue("COORDPARAM", "Input", coordinparam, IOUtils::nothrow);
	
	const std::string meteo_in = cfg.get("METEO", "Input", "");
	if (meteo_in == "OSHD") {//keep it synchronized with IOHandler.cc for plugin mapping!!
		cfg.getValues("STATION", "INPUT", vecIDs);
		cfg.getValue("METEOPATH", "Input", in_meteopath);
		const bool is_recursive = cfg.get("METEOPATH_RECURSIVE", "Input", false);
		cache_meteo_files = scanMeteoPath(in_meteopath, is_recursive);
		if (debug) {
			std::cout << "Meteo files cache content:\n";
			for(size_t ii=0; ii<cache_meteo_files.size(); ii++) std::cout << cache_meteo_files[ii].toString() << "\n";
		}

		cfg.getValue("METAFILE", "INPUT", in_metafile);
		if (FileUtils::getFilename(in_metafile) == in_metafile) { //ie there is no path in the provided filename
			in_metafile = in_meteopath + "/" + in_metafile;
		}
	}

	const std::string grid_in = cfg.get("GRID2D", "Input", "");
	if (grid_in == "OSHD") {//keep it synchronized with IOHandler.cc for plugin mapping!!
		grid2dpath_in.clear();
		cfg.getValue("GRID2DPATH", "Input", grid2dpath_in);
		const bool is_recursive = cfg.get("GRIDPATH_RECURSIVE", "Input", false);
		cache_grid_files = scanMeteoPath(grid2dpath_in, is_recursive);
		if (debug) {
			std::cout << "Grid files cache content:\n";
			for(size_t ii=0; ii<cache_grid_files.size(); ii++) std::cout << cache_grid_files[ii].toString() << "\n";
		}
	}
}

bool OshdIO::initStaticData()
{
	params_map.push_back( std::make_pair(MeteoData::ILWR, "ilwc") );
	params_map.push_back( std::make_pair(MeteoData::P, "pair") );
	params_map.push_back( std::make_pair(MeteoData::PSUM, "prec") ); //in mm/ts
	params_map.push_back( std::make_pair(MeteoData::RH, "rcor") ); //old: rhum
	params_map.push_back( std::make_pair(MeteoData::TA, "tcor") ); //old:tair
	params_map.push_back( std::make_pair(MeteoData::VW, "wcor") ); //old: wind
	params_map.push_back( std::make_pair(MeteoData::DW, "wdir") );

	grids_map[ MeteoGrids::ILWR ] = "ilwc";
	grids_map[ MeteoGrids::P ] = "pair";
	grids_map[ MeteoGrids::PSUM ] = "prec"; //in mm/ts
	grids_map[ MeteoGrids::RH ] = "rcor"; //old: rhum
	grids_map[ MeteoGrids::TA ] = "tcor"; //old:tair
	grids_map[ MeteoGrids::VW ] = "wcor"; //old: wind
	grids_map[ MeteoGrids::DW ] = "wdir";
	grids_map[ MeteoGrids::ISWR_DIFF ] = "idfc";
	grids_map[ MeteoGrids::ISWR_DIR ] = "idrc";
	grids_map[ MeteoGrids::ALB ] = "albd";

	return true;
}

//This builds an index of which timesteps are provided by which files, always keeping the most recent run when
//multiple files provide the same timesteps. The file names are read as: {prec}_{timestep}_XXX_{runtime}.{meteo_ext}
std::vector< struct OshdIO::file_index > OshdIO::scanMeteoPath(const std::string& meteopath_in, const bool& is_recursive)
{
	std::vector< struct OshdIO::file_index > data_files;
	const std::list<std::string> dirlist( FileUtils::readDirectory(meteopath_in, "prec", is_recursive) ); //we consider that if we have found one parameter, the others are also there

	std::map<std::string, size_t> mapIdx; //make sure each timestamp only appears once, ie remove duplicates
	for (std::list<std::string>::const_iterator it = dirlist.begin(); it != dirlist.end(); ++it) {
		const std::string file_and_path( *it );
		const std::string filename( FileUtils::getFilename(file_and_path) );

		//we need to split the file name into its components: parameter, date, run_date
		const std::string::size_type rundate_end = filename.rfind('.');
		if (rundate_end==string::npos) continue;
		const std::string ext( filename.substr(rundate_end) );
		if (ext!=meteo_ext) continue;
		const std::string::size_type pos_param = filename.find('_');
		if (pos_param==string::npos) continue;
		const std::string::size_type date_start = filename.find_first_of("0123456789");
		if (date_start==string::npos) continue;
		const std::string::size_type date_end = filename.find('_', date_start);
		if (date_end==string::npos) continue;
		const std::string::size_type rundate_start = filename.rfind('_');
		if (rundate_start==string::npos) continue;

		const std::string date_str( filename.substr(date_start, date_end-date_start) );
		const std::string run_date( filename.substr(rundate_start+1, rundate_end-rundate_start-1) );

		//do we already have an entry for this date?
		size_t idx = IOUtils::npos;
		const std::map<std::string, size_t>::const_iterator it_map = mapIdx.find( date_str );
		if (it_map!=mapIdx.end()) {
			idx = it_map->second;
			if (data_files[idx].run_date>run_date) continue;
		}

		//we don't have an entry or it is too old -> create new entry / replace existing one
		const std::string path( FileUtils::getPath(file_and_path) );
		Date date;
		IOUtils::convertString(date, date_str, in_dflt_TZ);
		const file_index elem(date, path, filename.substr(pos_param), run_date);
		if (idx==IOUtils::npos) {
			data_files.push_back( elem );
			mapIdx[ date_str ] = data_files.size()-1;
		} else {
			data_files[ idx] = elem;
			mapIdx[ date_str ] = idx;
		}
	}

	std::sort(data_files.begin(), data_files.end());
	return data_files;
}

bool OshdIO::list2DGrids(const Date& start, const Date& end, std::map<Date, std::set<size_t> >& results)
{
	results.clear();
	
	for (size_t ii=0; ii<cache_grid_files.size(); ii++) {
		const Date date( cache_grid_files[ii].date );
		if (date<start) continue;
		if (date>end) break;
		
		//we consider that all parameters are present at all timesteps
		std::map< MeteoGrids::Parameters, std::string >::const_iterator it;
		for (it=grids_map.begin(); it!=grids_map.end(); ++it) {
			results[date].insert( it->first );
		}
	}
	
	return true;
}

size_t OshdIO::getFileIdx(const std::vector< struct file_index >& cache, const Date& start_date)
{
	if (cache.empty()) throw InvalidArgumentException("No input files found or configured!", AT);
	if (cache.size()==1) return 0; //no other possibility

	//try to find a good match
	for (size_t idx=1; idx<cache.size(); idx++) {
		if (start_date>=cache[idx-1].date && start_date<cache[idx].date) {
			return --idx;
		}
	}

	//not found, we take the closest timestamp we have (ie very eginning or very end)
	if (start_date<cache.front().date) return 0;
	else return cache.size()-1;
}

void OshdIO::readStationData(const Date& /*date*/, std::vector<StationData>& vecStation)
{
	vecStation.clear();
	if (vecMeta.empty()) fillStationMeta();
	vecStation = vecMeta;
}

void OshdIO::readMeteoData(const Date& dateStart, const Date& dateEnd,
                             std::vector< std::vector<MeteoData> >& vecMeteo)
{
	vecMeteo.clear();
	size_t file_idx = getFileIdx( cache_meteo_files, dateStart );
	Date station_date( cache_meteo_files[file_idx].date );
	if (station_date>dateEnd || cache_meteo_files.back().date<dateStart) return; //the requested period is NOT in the available files

	const size_t nr_files = cache_meteo_files.size();
	const size_t nrIDs = vecIDs.size();
	
	if (vecMeta.empty()) fillStationMeta(); //this also fills vecIdx
	vecMeteo.resize( nrIDs );
	do {
		//create empty MeteoData for the current timestep
		for (size_t jj=0; jj<nrIDs; jj++) {
			const MeteoData md( station_date, vecMeta[jj] );
			vecMeteo[jj].push_back( md );
		}
		
		//read the data and fill vecMeteo
		const std::string file_suffix( cache_meteo_files[ file_idx ].file_suffix );
		const std::string path( in_meteopath + "/" + cache_meteo_files[ file_idx ].path );
		for (size_t ii=0; ii<params_map.size(); ii++) {
			const MeteoData::Parameters param( params_map[ii].first );
			const std::string filename( path + "/" + params_map[ii].second + file_suffix );
			const std::vector<double> vecData( readFromFile(filename, param, station_date) );

			for (size_t jj=0; jj<nrIDs; jj++)
				vecMeteo[jj].back()( param ) =  vecData[jj];
		}
		
		readSWRad(station_date, path, file_suffix, nrIDs, vecMeteo); //the short wave radiation is a little different...
		readPPhase(station_date, path, file_suffix, nrIDs, vecMeteo); //the precipitation phase is a little different...

		file_idx++;
		station_date = ((file_idx)<nr_files)? cache_meteo_files[file_idx].date : dateEnd+1.;
	} while (file_idx<nr_files && station_date<=dateEnd);
}

void OshdIO::readSWRad(const Date& station_date, const std::string& path, const std::string& file_suffix, const size_t& nrIDs, std::vector< std::vector<MeteoData> >& vecMeteo) const
{
	const std::string filename_dir( path + "/" + grids_map[MeteoGrids::ISWR_DIR] + file_suffix );
	const std::vector<double> vecDir( readFromFile(filename_dir, MeteoData::ISWR, station_date) );
	
	const std::string filename_diff( path + "/" + grids_map[MeteoGrids::ISWR_DIFF] + file_suffix );
	const std::vector<double> vecDiff( readFromFile(filename_diff, MeteoData::ISWR, station_date) );

	const std::string filename_albd( path + "/" + grids_map[MeteoGrids::ALB] + file_suffix );
	if (FileUtils::fileExists(filename_albd)) {
		const std::vector<double> vecAlbd( readFromFile(filename_albd, MeteoData::RSWR, station_date) ); //We read ALBD and use it to build RSWR
		for (size_t jj=0; jj<nrIDs; jj++) {
			vecMeteo[jj].back()( MeteoData::ISWR ) =  vecDir[jj]+vecDiff[jj];
			vecMeteo[jj].back()( MeteoData::RSWR ) =  (vecDir[jj]+vecDiff[jj])*vecAlbd[jj];
		}
	} else {
		for (size_t jj=0; jj<nrIDs; jj++) {
			vecMeteo[jj].back()( MeteoData::ISWR ) =  vecDir[jj]+vecDiff[jj];
		}
	}
}

void OshdIO::readPPhase(const Date& station_date, const std::string& path, const std::string& file_suffix, const size_t& nrIDs, std::vector< std::vector<MeteoData> >& vecMeteo) const
{
	const std::string filename( path + "/" + "snfl" + file_suffix );
	if (FileUtils::fileExists(filename)) {
		static const double half_elevation_band = 50.;  //we consider that there are mixed precip in the elevation range snow_line ± half_elevation_band
		const std::vector<double> vecSnowLine( readFromFile(filename, MeteoData::PSUM_PH, station_date) );

		for (size_t jj=0; jj<nrIDs; jj++) {
			const double altitude = vecMeteo[jj].front().meta.getAltitude();
			if (altitude>(vecSnowLine[jj]+half_elevation_band))
				vecMeteo[jj].back()( MeteoData::PSUM_PH ) = 0.;
			else if (altitude<(vecSnowLine[jj]-half_elevation_band))
				vecMeteo[jj].back()( MeteoData::PSUM_PH ) = 1.;
			else
				vecMeteo[jj].back()( MeteoData::PSUM_PH ) = .5;
		}
	}
}

std::vector<double> OshdIO::readFromFile(const std::string& filename, const MeteoData::Parameters& param, const Date& in_timestep) const
{
	if (debug) matWrap::printFileStructure(filename, in_dflt_TZ);
	mat_t *matfp = Mat_Open(filename.c_str(), MAT_ACC_RDONLY);
	if ( NULL == matfp ) throw AccessException(filename, AT);

	//open the file and read some metadata
	matvar_t *matvar = Mat_VarReadInfo(matfp, "stat");
	if (matvar==NULL) throw NotFoundException("structure 'stat' not found in file '"+filename+"'", AT);
	if (matvar->class_type!=MAT_C_STRUCT) throw InvalidFormatException("The matlab file should contain 1 structure", AT);
	
	const std::string type( matWrap::readString(filename, "type", matfp, matvar) );
	checkFieldType(param, type);

	//check that the timestep is as expected
	const std::vector<double> vecTime( matWrap::readDoubleVector(filename, "time", matfp, matvar) );
	if (vecTime.size()!=1) throw InvalidFormatException("one and only one time step must be present in the 'time' vector", AT);
	Date timestep;
	timestep.setMatlabDate( vecTime[0], in_dflt_TZ );
	if (in_timestep!=timestep) throw InvalidArgumentException("the in-file timestep and the filename time step don't match for for '"+filename+"'", AT);
	
	//check that each station is still at the same index, build the index cache if necessary
	const std::vector<std::string> vecAcro( matWrap::readStringVector(filename, "acro", matfp, matvar) );
	if (vecAcro.size()!=nrMetadata) {
		std::ostringstream ss;
		ss << "the number of stations changes between the metadata file (" << nrMetadata << ") and the data file '";
		ss << FileUtils::getFilename( filename ) << "' (" << vecAcro.size() << ")\n";
		throw IndexOutOfBoundsException(ss.str(), AT);
	}
	const size_t nrIDs = vecIDs.size();
	for (size_t ii=0; ii<nrIDs; ii++) { //check that the IDs still match
		if (vecIDs[ii] != vecAcro[ vecIdx[ii] ])
			throw InvalidFormatException("station '"+vecIDs[ii]+"' is not listed in the same position as previously in file '"+filename+"'", AT);
	}
	
	//extract the data for the selected stations
	const std::string units( matWrap::readString(filename, "dunit", matfp, matvar) );
	const std::vector<double> vecRaw( matWrap::readDoubleVector(filename, "data", matfp, matvar) );
	if (vecAcro.size() != vecRaw.size()) throw InvalidFormatException("'acro' and 'data' arrays don't match in file '"+filename+"'", AT);

	std::vector<double> vecData(nrIDs, IOUtils::nodata);
	for (size_t ii=0; ii<nrIDs; ii++)
		vecData[ii] = convertUnits( vecRaw[ vecIdx[ii] ], units, param, filename);
	
	Mat_VarFree(matvar);
	Mat_Close(matfp);
	return vecData;
}

void OshdIO::checkFieldType(const MeteoData::Parameters& param, const std::string& type)
{
	if (param==MeteoData::TA && type=="TA") return;
	if (param==MeteoData::RH && type=="RH") return;
	if (param==MeteoData::PSUM && type=="PREC") return;
	if (param==MeteoData::VW && type=="WS") return;
	if (param==MeteoData::DW && type=="WD") return;
	if (param==MeteoData::ILWR && type=="LWR") return;
	if (param==MeteoData::ISWR && type=="SWR") return;
	if (param==MeteoData::P && type=="other") return;
	if (param==MeteoData::PSUM_PH && type=="other") return;
	if (param==MeteoData::RSWR && type=="other") return; //this is in fact ALBD
	
	throw InvalidArgumentException("trying to read "+MeteoData::getParameterName(param)+" but found '"+type+"'", AT);
}

//NOTE It seems that recent versions contain multibyte encoding and this is not supported by matio, leading to trucated units (at best)
double OshdIO::convertUnits(const double& val, const std::string& units, const MeteoData::Parameters& param, const std::string& filename)
{
	if (units=="%") return val/100.;
	if (units=="m") return val;
	if (units=="cm") return val/100.;
	if (units=="mm") {
		if (param==MeteoData::PSUM) return val;
		else return val/1000.;
	}
	if (units=="\xB0\x43") return val+Cst::t_water_freezing_pt; //ISO-8859-1 hex for '°C'
	if (units=="\xB0" && param==MeteoData::TA) return val+Cst::t_water_freezing_pt; //ISO-8859-1 hex for '°'
	if (units=="\xB0" && param==MeteoData::DW) return val; //ISO-8859-1 hex for '°'
	if (units.empty()) return val;
	if (units=="Pa") return val;
	if (units=="W/m2") return val;
	if (units=="m/s") return val;
	else {
		std::ostringstream os;
		for(size_t ii=0; ii<units.size(); ++ii)
			os << " " << std::hex << static_cast<unsigned int>( units[ii] );
		throw IOException("Unknown units '"+units+"' (#"+os.str()+") for parameter "+MeteoData::getParameterName(param)+" in file "+filename, AT);
	}
	
	return val;
}

void OshdIO::fillStationMeta()
{
	if (debug) matWrap::printFileStructure(in_metafile, in_dflt_TZ);
	vecMeta.resize( vecIDs.size(), StationData() );
	mat_t *matfp = Mat_Open(in_metafile.c_str(), MAT_ACC_RDONLY);
	if ( NULL == matfp ) throw AccessException(in_metafile, AT);

	matvar_t *matvar = Mat_VarReadInfo(matfp, "statlist");
	if (matvar==NULL) throw NotFoundException("structure 'statlist' not found in file '"+in_metafile+"'", AT);
	
	const std::vector<std::string> vecAcro( matWrap::readStringVector(in_metafile, "acro", matfp, matvar) );
	const std::vector<std::string> vecNames( matWrap::readStringVector(in_metafile, "name", matfp, matvar) );
	const std::vector<double> easting( matWrap::readDoubleVector(in_metafile, "x", matfp, matvar) );
	const std::vector<double> northing( matWrap::readDoubleVector(in_metafile, "y", matfp, matvar) );
	const std::vector<double> altitude( matWrap::readDoubleVector(in_metafile, "z", matfp, matvar) );
	nrMetadata = vecAcro.size();
	Mat_VarFree(matvar);
	Mat_Close(matfp);
	
	if (debug) {
		for (size_t ii=0; ii<nrMetadata; ii++) 
			std::cout << std::setw(8) << vecAcro[ii] << std::setw(40) << vecNames[ii] << std::setw(8) << easting[ii] << std::setw(8) << northing[ii] << std::setw(8) << altitude[ii] << "\n";
		std::cout << endl;
	}
	
	buildVecIdx(vecAcro);
	for (size_t ii=0; ii<vecIdx.size(); ii++) {
		const size_t idx = vecIdx[ii];
		Coords location(coordin, coordinparam);
		location.setXY(easting[idx], northing[idx], altitude[idx]);
		std::string name( vecNames[idx] );

		//if the network name has been appended, remove it. We also remove spaces, just in case
		const size_t netz_pos = name.find(" (");
		if (netz_pos!=std::string::npos) name.erase(netz_pos);
		std::replace( name.begin(), name.end(), ' ', '_');

		const StationData sd(location, vecAcro[idx], name);
		vecMeta[ii] = sd;
	}
}

void OshdIO::buildVecIdx(const std::vector<std::string>& vecAcro)
{
	const size_t nrIDs = vecIDs.size();
	if (nrIDs==0) throw InvalidArgumentException("Please provide at least one station ID to read!", AT);
	vecIdx.resize( nrIDs, 0 );
	
	for (size_t ii=0; ii<nrIDs; ii++) {
		bool found = false;
		for (size_t jj=0; jj<vecAcro.size(); jj++) {
			if (vecIDs[ii]==vecAcro[jj]) {
				vecIdx[ii] = jj;
				found = true;
				break;
			}
		}
		if (!found) 
			throw NotFoundException("station ID '"+vecIDs[ii]+"' could not be found in the provided metadata", AT);
	}
}

void OshdIO::read2DGrid(Grid2DObject& grid_out, const std::string& filename)
{
	if (debug) matWrap::printFileStructure(filename, in_dflt_TZ);
	mat_t *matfp = Mat_Open(filename.c_str(), MAT_ACC_RDONLY);
	if ( NULL == matfp ) throw AccessException(filename, AT);

	matvar_t *matvar = Mat_VarReadInfo(matfp, "grid");
	if (matvar==NULL) throw NotFoundException("structure 'grid' not found in file '"+filename+"'", AT);
	if (matvar->class_type!=MAT_C_STRUCT) throw InvalidFormatException("The matlab file should contain 1 structure", AT);

	const double xllcorner( matWrap::readDouble(filename, "xllcorner", matfp, matvar) );
	const double yllcorner( matWrap::readDouble(filename, "yllcorner", matfp, matvar) );
	Coords location(coordin, coordinparam);
	location.setXY(xllcorner, yllcorner, IOUtils::nodata);

	const double cellsize( matWrap::readDouble(filename, "cellsize", matfp, matvar) );
	const double ncols( matWrap::readDouble(filename, "ncols", matfp, matvar) );
	const double nrows( matWrap::readDouble(filename, "nrows", matfp, matvar) );

	//Initialize the 2D grid
	grid_out.set(static_cast<size_t>(ncols), static_cast<size_t>(nrows), cellsize, location);
	matWrap::readDoubleArray(filename, "data", matfp, matvar, grid_out.grid2D);

	Mat_Close(matfp);
}

void OshdIO::read2DGrid(Grid2DObject& grid_out, const MeteoGrids::Parameters& parameter, const Date& date)
{
	const size_t file_idx = getFileIdx( cache_grid_files, date );
	Date grid_date( cache_grid_files[file_idx].date );
	if (grid_date!=date) return; //the requested date is NOT in the available files

	//build the proper file name
	const std::string file_suffix( cache_grid_files[ file_idx ].file_suffix );
	const std::string path( grid2dpath_in + "/" + cache_grid_files[ file_idx ].path );

	if (grids_map.find(parameter)!=grids_map.end()) {
		const std::string filename( path + "/" + grids_map[parameter] +file_suffix );
		read2DGrid(grid_out, filename);
	} else
		throw NotFoundException("Parameter "+MeteoGrids::getParameterName(parameter)+" currently not supported", AT);

	//units corrections
	if (parameter==MeteoGrids::TA) grid_out += Cst::t_water_freezing_pt;
	else if (parameter==MeteoGrids::RH) grid_out /= 100.;
}

void OshdIO::readDEM(DEMObject& dem_out)
{
	const std::string filename = cfg.get("DEMFILE", "Input");
	read2DGrid(dem_out, filename);
}

} //namespace
