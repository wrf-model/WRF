/***********************************************************************************/
/*  Copyright 2013 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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

#include <meteoio/spatialInterpolations/WinstralAlgorithm.h>
#include <meteoio/meteoStats/libinterpol2D.h>
#include <meteoio/MathOptim.h>
#include <algorithm>

namespace mio {

WinstralAlgorithm::WinstralAlgorithm(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm,
		                               GridsManager& i_gdm, Meteo2DInterpolator& i_mi)
                  : InterpolationAlgorithm(vecArgs, i_algo, i_param, i_tsm), mi(i_mi), gdm(i_gdm), base_algo_user("IDW_LAPSE"), ref_station(),
                    user_synoptic_bearing(IOUtils::nodata), inputIsAllZeroes(false), dmax(300.)
{
	const std::string where( "Interpolations2D::"+i_param+"::"+i_algo );
	synoptic_wind_type type = AUTO;
	bool has_ref=false, has_synop=false;
	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="TYPE") {
			const std::string user_type( IOUtils::strToUpper(vecArgs[ii].second) );

			if (user_type=="FIXED") type = FIXED;
			else if (user_type=="REF_STATION") type = REF_STATION;
			else
				throw InvalidArgumentException("Unknown algorithm \""+user_type+"\" supplied for "+where, AT);
		} else if (vecArgs[ii].first=="REF_STATION") {
			ref_station = vecArgs[ii].second;
			has_ref = true;
		} else if(vecArgs[ii].first=="BASE") {
			base_algo_user = IOUtils::strToUpper( vecArgs[ii].second );
		} else if(vecArgs[ii].first=="DW_SYNOP") {
			IOUtils::parseArg(vecArgs[ii], where, user_synoptic_bearing);
			has_synop = true;
		} else if (vecArgs[ii].first=="DMAX") {
			IOUtils::parseArg(vecArgs[ii], where, dmax);
		}
	}

	if (type==AUTO && (has_synop || has_ref)) throw InvalidArgumentException("No REF_STATION or DW_SYNOP arguments expected when TYPE=AUTO for "+where, AT);
	if (has_synop && has_ref) throw InvalidArgumentException("It is not possible to provide both REF and DW_SYNOP for "+where, AT);
	if (type==FIXED && !has_synop) throw InvalidArgumentException("Please provide DW_SYNOP for "+where, AT);
	if (type==REF_STATION && !has_ref) throw InvalidArgumentException("Please provide REF_STATION for "+where, AT);
}

double WinstralAlgorithm::getQualityRating(const Date& i_date)
{
	date = i_date;
	nrOfMeasurments = getData(date, param, vecData, vecMeta);
	inputIsAllZeroes = Interpol2D::allZeroes(vecData);

	if (inputIsAllZeroes) return 0.99;

	if (nrOfMeasurments==0) return 0.0;

	//check that the necessary wind data is available
	if (user_synoptic_bearing==IOUtils::nodata) {
		if (!windIsAvailable(vecMeteo, ref_station))
			return 0.0;
	}

	return 0.99;
}

void WinstralAlgorithm::initGrid(const DEMObject& dem, Grid2DObject& grid)
{
	//initialize precipitation grid with user supplied algorithm (IDW_LAPSE by default)
	const std::string base_algo = (nrOfMeasurments==1)? "AVG" : base_algo_user; //if there is only one station, revert to a safe default

	const std::vector< std::pair<std::string, std::string> > vecArgs( mi.getArgumentsForAlgorithm(param, base_algo, "Interpolations2D") );
	InterpolationAlgorithm* algorithm( AlgorithmFactory::getAlgorithm(base_algo, mi, vecArgs, tsmanager, gdm, param) );
	algorithm->getQualityRating(date);
	algorithm->calculate(dem, grid);
	info << algorithm->getInfo();
	delete algorithm;
}

bool WinstralAlgorithm::windIsAvailable(const std::vector<MeteoData>& i_vecMeteo, const std::string& i_ref_station)
{
	if (i_ref_station.empty()) {
		for (size_t ii=0; ii<i_vecMeteo.size(); ii++) {
			const double VW = i_vecMeteo[ii](MeteoData::VW);
			const double DW = i_vecMeteo[ii](MeteoData::DW);
			if (VW!=IOUtils::nodata && DW!=IOUtils::nodata)
				return true; //at least one station is enough
		}
	} else {
		if (getSynopticBearing(i_vecMeteo, i_ref_station) != IOUtils::nodata)
			return true;
	}

	return false;
}

double WinstralAlgorithm::getSynopticBearing(const std::vector<MeteoData>& i_vecMeteo, const std::string& i_ref_station)
{
	for (size_t ii=0; ii<i_vecMeteo.size(); ++ii) {
		if (i_vecMeteo[ii].meta.stationID==i_ref_station)
			return i_vecMeteo[ii](MeteoData::DW);
	}

	return IOUtils::nodata;
}

//this method scans a square centered on the station for summmits
//that would shelter the station for wind.
//the sheltering criteria is: if ( height_difference*shade_factor > distance ) sheltered=true
bool WinstralAlgorithm::isExposed(const DEMObject& dem, Coords location)
{
	if (!dem.gridify(location)) {
		return false;
	}

	const int i_ref = location.getGridI();
	const int j_ref = location.getGridJ();
	const double alt_ref = location.getAltitude()+4.; //4 m mast added so flat terrain behaves properly
	const size_t ii_ref = static_cast<size_t>(i_ref);
	const size_t jj_ref = static_cast<size_t>(j_ref);

	static const double shade_factor = 5.;
	const double cellsize = dem.cellsize;
	const double min_dh = cellsize/shade_factor; //min_dist=cellsize -> min_dh
	const double search_dist = (dem.grid2D.getMax() - alt_ref) * shade_factor;
	if (search_dist<=cellsize) return true;
	const int search_idx = static_cast<int>( Optim::ceil( search_dist/cellsize ) );

	const size_t i_min = static_cast<size_t>(std::max( 0, i_ref-search_idx ));
	const size_t i_max = static_cast<size_t>(std::min( static_cast<int>(dem.getNx()-1), i_ref+search_idx ));
	const size_t j_min = static_cast<size_t>(std::max( 0, j_ref-search_idx ));
	const size_t j_max = static_cast<size_t>(std::min( static_cast<int>(dem.getNy()-1), j_ref+search_idx ));

	for (size_t jj=j_min; jj<=j_max; jj++) {
		for (size_t ii=i_min; ii<=i_max; ii++) {
			if (ii==ii_ref && jj==jj_ref) continue; //skip the cell containing the station!

			const double dh = dem.grid2D(ii,jj) - alt_ref;
			if (dh<=min_dh) continue; //for negative or too small dh
			const double distance = Optim::fastSqrt_Q3( Optim::pow2(static_cast<int>(ii)-i_ref) + Optim::pow2(static_cast<int>(jj)-j_ref) ) * cellsize;
			if (distance<dh*shade_factor) {
				return false;
			}
		}
	}

	return true;
}

double WinstralAlgorithm::getSynopticBearing(const std::vector<MeteoData>& i_vecMeteo)
{
	double ve=0.0, vn=0.0;
	size_t count=0;
	for (size_t ii=0; ii<i_vecMeteo.size(); ii++) {
		const double VW = i_vecMeteo[ii](MeteoData::VW);
		const double DW = i_vecMeteo[ii](MeteoData::DW);
		if (VW!=IOUtils::nodata && DW!=IOUtils::nodata) {
			ve += VW * sin(DW*Cst::to_rad);
			vn += VW * cos(DW*Cst::to_rad);
			count++;
		}
	}

	if (count!=0) {
		ve /= static_cast<double>(count);
		vn /= static_cast<double>(count);

		//const double meanspeed = sqrt(ve*ve + vn*vn);
		const double meandirection = fmod( atan2(ve,vn) * Cst::to_deg + 360., 360.);
		return static_cast<double>(Optim::round(meandirection/10.))*10.; //round to nearest 10 deg
	}

	return IOUtils::nodata;
}

double WinstralAlgorithm::getSynopticBearing(const DEMObject& dem, const std::vector<MeteoData>& i_vecMeteo)
{
	// 1) locate the stations in DEM and check if they are higher than their surroundings within a given radius
	// 2) simply compute a mean or median direction
	// (2) can be used on all the stations selected in (1)

	std::vector<MeteoData> stationsSubset;
	for (size_t ii=0; ii<i_vecMeteo.size(); ii++) {
		if (isExposed(dem, i_vecMeteo[ii].meta.position))
			stationsSubset.push_back( i_vecMeteo[ii] );
	}

	if (!stationsSubset.empty()) {
		return getSynopticBearing(stationsSubset);
	} else {
		//std::cerr << "[W] Synoptic wind direction computed from wind-sheltered stations only\n";
		return getSynopticBearing(i_vecMeteo);
	}
}

void WinstralAlgorithm::calculate(const DEMObject& dem, Grid2DObject& grid)
{
	info.clear(); info.str("");

	//if all data points are zero, simply fill the grid with zeroes
	if (inputIsAllZeroes) {
		Interpol2D::constant(0., dem, grid);
		return;
	}

	double synoptic_bearing = user_synoptic_bearing;
	if (synoptic_bearing==IOUtils::nodata) {
		if (!ref_station.empty())
			synoptic_bearing = getSynopticBearing(vecMeteo, ref_station);
		else
			synoptic_bearing = getSynopticBearing(dem, vecMeteo);
	}
	info << "DW=" << synoptic_bearing << " - ";
	initGrid(dem, grid);

	//get TA interpolation from call back to Meteo2DInterpolator
	Grid2DObject ta;
	mi.interpolate(date, dem, MeteoData::TA, ta);

	//alter the field with Winstral and the chosen wind direction
	Interpol2D::Winstral(dem, ta,  dmax, synoptic_bearing, grid);
}

} //namespace
