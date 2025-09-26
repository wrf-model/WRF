/***********************************************************************************/
/*  Copyright 2010 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#include <meteoio/spatialInterpolations/InterpolationAlgorithms.h>
#include <meteoio/meteoStats/libinterpol2D.h>
#include <meteoio/MathOptim.h>

#include <meteoio/spatialInterpolations/ALSScaleAlgorithm.h>
#include <meteoio/spatialInterpolations/AvgAlgorithm.h>
#include <meteoio/spatialInterpolations/AvgLapseAlgorithm.h>
#include <meteoio/spatialInterpolations/ConstAlgorithm.h>
#include <meteoio/spatialInterpolations/IDWAlgorithm.h>
#include <meteoio/spatialInterpolations/IDWSlopesAlgorithm.h>
#include <meteoio/spatialInterpolations/IDWLapseAlgorithm.h>
#include <meteoio/spatialInterpolations/IDWLapseLocalAlgorithm.h>
#include <meteoio/spatialInterpolations/ILWREpsAlgorithm.h>
#include <meteoio/spatialInterpolations/ListonWindAlgorithm.h>
#include <meteoio/spatialInterpolations/NearestNeighbourAlgorithm.h>
#include <meteoio/spatialInterpolations/NoneAlgorithm.h>
#include <meteoio/spatialInterpolations/ODKrigAlgorithm.h>
#include <meteoio/spatialInterpolations/ODKrigLapseAlgorithm.h>
#include <meteoio/spatialInterpolations/PPhaseAlgorithm.h>
#include <meteoio/spatialInterpolations/RHListonAlgorithm.h>
#include <meteoio/spatialInterpolations/RyanWindAlgorithm.h>
#include <meteoio/spatialInterpolations/SnowPsumAlgorithm.h>
#include <meteoio/spatialInterpolations/StdPressAlgorithm.h>
#include <meteoio/spatialInterpolations/SwRadAlgorithm.h>
#include <meteoio/spatialInterpolations/template.h>
#include <meteoio/spatialInterpolations/UserAlgorithm.h>
#include <meteoio/spatialInterpolations/WinstralAlgorithm.h>
#include <meteoio/spatialInterpolations/WinstralListonAlgorithm.h>

#include <sstream>
#include <vector>
#include <algorithm>

namespace mio {

/**
 * @page interpol2d Spatial interpolations
 * Using the vectors of MeteoData and StationData as filled by the IOInterface::readMeteoData call
 * as well as a grid of elevations (DEM, stored as a DEMObject), it is possible to get spatially
 * interpolated parameters.
 *
 * First, an interpolation method has to be selected for each variable which needs interpolation. Then the class computes
 * the interpolation for each 2D grid point, combining the inputs provided by the available data sources.
 * Any parameter of MeteoData can be interpolated, using the names given by \ref meteoparam. One has to keep
 * in mind that the interpolations are time-independent: each interpolation is done at a given time step and no
 * memory of (eventual) previous time steps is kept. This means that all parameters and variables that are
 * automatically calculated get recalculated anew for each time step.
 * 
 * @note Please keep in mind that you need to specify a proper 
 * <a href="https://en.wikipedia.org/wiki/Coordinate_system#Cartesian_coordinate_system">cartesian coordinate system</a> in your [Input] section 
 * in order to be able to perform most of the spatial interpolations (Lat/lon coordinates are <b>not</b> cartesian, they are spherical!).
 *
 * @section interpol2D_section Spatial interpolations section
 * Practically, the user
 * has to specify in his configuration file (typically io.ini), for each parameter to be interpolated, which
 * spatial interpolations algorithms should be considered, in the [Interpolations2D] section. This is provided as a space separated list of keywords
 * (one per interpolation algorithm). Please notice that some algorithms may require extra arguments.
 * Then, each algorithm will be evaluated (through the use of its rating method) and receive a grade (that might
 * depend on the number of available data, the quality of the data, etc). The algorithm that receives the higher
 * score within the user list, will be used for interpolating the selected variable at the given timestep. This means that at another
 * timestep, the same parameter might get interpolated by a different algorithm.
 * An example of such section is given below:
 * @code
 * [Interpolations2D]
 * TA::algorithms      = IDW_LAPSE AVG_LAPSE
 * TA::avg_lapse::rate = -0.008
 *
 * RH::algorithms = LISTON_RH IDW_LAPSE AVG_LAPSE AVG
 *
 * PSUM::algorithms      = IDW_LAPSE AVG_LAPSE AVG CST
 * PSUM::avg_lapse::rate = 0.0005
 * PSUM::avg_lapse::frac = true
 * PSUM::cst::value      = 0
 *
 * VW::algorithms = IDW_LAPSE AVG_LAPSE
 *
 * P::algorithms	= STD_PRESS
 * P::std_press::USE_RESIDUALS = true
 * 
 * ILWR::algorithms = AVG_LAPSE
 * ILWR::avg_lapse::rate = -0.03125
 * 
 * RSWR::algorithms = IDW AVG
 * 
 * ISWR::algorithms = SWRAD
 * @endcode
 *
 * @section interpol2D_keywords Available algorithms
 * The keywords defining the algorithms are the following:
 * - NONE: returns a nodata filled grid (see NoneAlgorithm)
 * - STD_PRESS: standard atmospheric pressure as a function of the elevation of each cell (see StandardPressureAlgorithm)
 * - CST: constant value in each cell (see ConstAlgorithm)
 * - NEAREST: the value at the closest station is taken for each cell (see NearestNeighbourAlgorithm)
 * - AVG: average of the measurements in each cell (see AvgAlgorithm)
 * - AVG_LAPSE: constant value reprojected to the elevation of the cell (see AvgLapseRateAlgorithm)
 * - IDW: Inverse Distance Weighting averaging (see IDWAlgorithm)
 * - IDW_LAPSE: Inverse Distance Weighting averaging with reprojection to the elevation of the cell (see IDWLapseAlgorithm)
 * - IDW_SLOPES: IDW_LAPSE with separate processing for each of the 4 aspects+flat before merging with weighted average (see IDWSlopesAlgorithm)
 * - LIDW_LAPSE: IDW_LAPSE restricted to a local scale (n neighbor stations, see LocalIDWLapseAlgorithm)
 * - LISTON_RH: the dew point temperatures are interpolated using IDW_LAPSE, then reconverted locally to relative humidity (see RHListonAlgorithm)
 * - ILWR_EPS: the incoming long wave radiation is converted to emissivity and then interpolated (see ILWREpsAlgorithm)
 * - SWRAD: The atmospheric attenuation and splitting coefficients are evaluated and used to compute the short wave radiation with topographic shading (see SWRadInterpolation)
 * - LISTON_WIND: the wind field (VW and DW) is interpolated using IDW_LAPSE and then altered depending on the local curvature and slope (taken from the DEM, see ListonWindAlgorithm)
 * - RYAN: the wind direction is interpolated using IDW and then altered depending on the local slope (see RyanAlgorithm)
 * - WINSTRAL: the solid precipitation is redistributed by wind according to (Winstral, 2002) (see WinstralAlgorithm)
 * - PSUM_SNOW: precipitation interpolation according to (Magnusson, 2011) (see SnowPSUMInterpolation)
 * - PPHASE: precipitation phase parametrization performed at each cell (see PPHASEInterpolation)
 * - ODKRIG: ordinary kriging (see OrdinaryKrigingAlgorithm)
 * - ODKRIG_LAPSE: ordinary kriging with lapse rate (see LapseOrdinaryKrigingAlgorithm)
 * - USER: user provided grids to be read from disk (if available, see USERInterpolation)
 * - ALS_SCALING: scaling from Airborn Laser Scan data (see ALS_Interpolation)
 *
 * @section interpol2D_trends Altitudinal trends
 * Several algorithms use elevation trends, all of them relying on the same principles: the lapse rates are recomputed at each time steps
 * (see section \ref interpol2D_lapse), all stations' data are detrended with this lapse rate, the residuals are spatially interpolated
 * with the algorithm as configured by the user and finally, the values at each cell are retrended (ie the lapse rates are re-applied
 * using the cell's elevation).
 *
 * @subsection interpol2D_lapse Lapse rates
 * The altitudinal trends are currently modelled as a linear relation. The slope of this linear relation can
 * sometimes be provided by the end user (through his io.ini configuration file), otherwise it is computed from the data.
 * In order to bring slightly more robustness, if the correlation between the input data and the computed linear regression
 * is not good enought (below 0.7, as defined in Interpol2D::LinRegression), the same regression will get re-calculated
 * with one point less (cycling throught all the points). The best result (ie: highest correlation coefficient) will be
 * kept. If the final correlation coefficient is less than 0.7, a warning is displayed.
 *
 * A list of supported options controlling the lapse rates is given in Trend::Trend().
 *
 * @section interpol2D_dev_use Developer usage
 * From the developer's point of view, all that has to be done is instantiate an IOManager object and call its
 * IOManager::getMeteoData method with an elevation model and a grid.
 * @code
 * 	Config cfg("io.ini");
 * 	IOManager io(cfg);
 *
 * 	//reading the dem (necessary for several spatial interpolations algoritms)
 * 	DEMObject dem;
 * 	io.readDEM(dem);
 *
 * 	//performing spatial interpolations
 * 	Grid2DObject param;
 * 	io.getMeteoData(date, dem, MeteoData::TA, param);
 *
 * @endcode
 *
 * @section interpol2D_biblio Bibliography
 * The interpolation algorithms have been inspired by the following papers:
 * - <i>"A Meteorological Distribution System for High-Resolution Terrestrial Modeling (MicroMet)"</i>, Liston and Elder, Journal of Hydrometeorology <b>7</b> (2006), 217-234.
 * - <i>"Simulating wind ﬁelds and snow redistribution using terrain-based parameters to model snow accumulation and melt over a semi-arid mountain catchment"</i>, Adam Winstral and Danny Marks, Hydrological Processes <b>16</b> (2002), 3585– 3603. DOI: 10.1002/hyp.1238
 * - <i>"Quantitative evaluation of different hydrological modelling approaches in a partly glacierized Swiss watershed"</i>, Jan Magnusson, Daniel Farinotti, Tobias Jonas and Mathias Bavay, Hydrological Processes, 2010, under review.
 * - <i>"Modelling runoff from highly glacierized alpine catchments in a changing climate"</i>, Matthias Huss, Daniel Farinotti, Andreas Bauder and Martin Funk, Hydrological Processes, <b>22</b>, 3888-3902, 2008.
 * - <i>"Geostatistics for Natural Resources Evaluation"</i>, Pierre Goovaerts, Oxford University Press, Applied Geostatistics Series, 1997, 483 p., ISBN 0-19-511538-4
 * - <i>"Statistics for spatial data"</i>, Noel A. C. Cressie, John Wiley & Sons, revised edition, 1993, 900 p.
 *
 */

InterpolationAlgorithm* AlgorithmFactory::getAlgorithm(std::string algoname,
                                                       Meteo2DInterpolator& mi,
                                                       const std::vector< std::pair<std::string, std::string> >& vecArgs, TimeSeriesManager& tsm, GridsManager& gdm, const std::string& param)
{
	IOUtils::toUpper(algoname);

	if (algoname == "NONE") {// return a nodata grid
		return new NoneAlgorithm(vecArgs, algoname, param, tsm);
	} else if (algoname == "STD_PRESS") {// standard air pressure interpolation
		return new StandardPressureAlgorithm(vecArgs, algoname, param, tsm);
	} else if (algoname == "CST") {// constant fill
		return new ConstAlgorithm(vecArgs, algoname, param, tsm);
	} else if (algoname == "NEAREST") {// nearest neighbour
		return new NearestNeighbourAlgorithm(vecArgs, algoname, param, tsm);
	} else if (algoname == "AVG") {// average fill
		return new AvgAlgorithm(vecArgs, algoname, param, tsm);
	} else if (algoname == "AVG_LAPSE") {// average fill with an elevation lapse rate
		return new AvgLapseRateAlgorithm(vecArgs, algoname, param, tsm);
	} else if (algoname == "IDW") {// Inverse Distance Weighting fill
		return new IDWAlgorithm(vecArgs, algoname, param, tsm);
	} else if (algoname == "IDW_SLOPES") {// Inverse Distance Weighting fill
		return new IDWSlopesAlgorithm(vecArgs, algoname, param, tsm);
	} else if (algoname == "IDW_LAPSE") {// Inverse Distance Weighting with an elevation lapse rate fill
		return new IDWLapseAlgorithm(vecArgs, algoname, param, tsm);
	} else if (algoname == "LIDW_LAPSE") {// Inverse Distance Weighting with an elevation lapse rate fill, restricted to a local scale
		return new LocalIDWLapseAlgorithm(vecArgs, algoname, param, tsm);
	} else if (algoname == "LISTON_RH") {// relative humidity interpolation
		return new RHListonAlgorithm(vecArgs, algoname, param, tsm, mi);
	} else if (algoname == "ILWR_EPS") {// long wave radiation interpolation
		return new ILWREpsAlgorithm(vecArgs, algoname, param, tsm, mi);
	} else if (algoname == "LISTON_WIND") {// wind velocity interpolation (using a heuristic terrain effect)
		return new ListonWindAlgorithm(vecArgs, algoname, param, tsm);
	} else if (algoname == "RYAN") {// RYAN wind direction
		return new RyanAlgorithm(vecArgs, algoname, param, tsm);
	} else if (algoname == "WINSTRAL") {// Winstral wind exposure factor
		return new WinstralAlgorithm(vecArgs, algoname, param, tsm, gdm, mi);
	} else if (algoname == "WINSTRAL++") {// Winstral/Liston wind exposure factor
		return new WinstralListonAlgorithm(vecArgs, algoname, param, tsm, gdm, mi);
	} else if (algoname == "ODKRIG") {// ordinary kriging
		return new OrdinaryKrigingAlgorithm(vecArgs, algoname, param, tsm);
	} else if (algoname == "ODKRIG_LAPSE") {// ordinary kriging with lapse rate
		return new LapseOrdinaryKrigingAlgorithm(vecArgs, algoname, param, tsm);
	} else if (algoname == "USER") {// read user provided grid
		return new USERInterpolation(vecArgs, algoname, param, tsm, gdm);
	} else if (algoname == "ALS_SCALING") {// scale from ALS grid
		return new ALS_Interpolation(vecArgs, algoname, param, tsm, gdm, mi);
	} else if (algoname == "PPHASE") {// precipitation phase parametrization
		return new PPHASEInterpolation(vecArgs, algoname, param, tsm, mi);
	} else if (algoname == "PSUM_SNOW") {// precipitation interpolation according to (Magnusson, 2010)
		return new SnowPSUMInterpolation(vecArgs, algoname, param, tsm, gdm, mi);
	} else if (algoname == "SWRAD") {// terrain shadding interpolation
		return new SWRadInterpolation(vecArgs, algoname, param, tsm, mi);
	} else {
		throw IOException("The interpolation algorithm '"+algoname+"' is not implemented" , AT);
	}
}

std::vector<double> InterpolationAlgorithm::getData(const Date& i_date, const std::string& i_param)
{
	tsmanager.getMeteoData(i_date, vecMeteo);

	std::vector<double> o_vecData;
	for (size_t ii=0; ii<vecMeteo.size(); ii++){
		if (!vecMeteo[ii].param_exists(i_param)) continue;
		const double val = vecMeteo[ii](i_param);
		if (val != IOUtils::nodata) {
			o_vecData.push_back( val );
		}
	}

	return o_vecData;
}

size_t InterpolationAlgorithm::getData(const Date& i_date, const std::string& i_param,
                                       std::vector<double>& o_vecData, std::vector<StationData>& o_vecMeta)
{
	tsmanager.getMeteoData(i_date, vecMeteo);
	o_vecData.clear();
	o_vecMeta.clear();
	for (size_t ii=0; ii<vecMeteo.size(); ii++){
		if (!vecMeteo[ii].param_exists(i_param)) continue;
		const double val = vecMeteo[ii](i_param);
		if (val != IOUtils::nodata){
			o_vecData.push_back( val );
			o_vecMeta.push_back( vecMeteo[ii].meta );
		}
	}

	return o_vecData.size();
}

/**
 * @brief Return an information string about the interpolation process
 * @return string containing some information (algorithm used, number of stations)
*/
std::string InterpolationAlgorithm::getInfo() const
{
	std::ostringstream os;
	os << algo << ", " << nrOfMeasurments << " station";
	if (nrOfMeasurments!=1) os << "s"; //add plural mark

	const std::string tmp( info.str() );
	if (!tmp.empty()) os << ", " << tmp;

	return os.str();
}


/**
* @brief Build a Trend object
* @details This object is responsible for detrending / retrending the meteorological data.
* The following arguments are recognized:
*  - MULTILINEAR: use a multiple linear regression (on altitude, easting and northing) if possible, otherwise a "standard", simple linear
* regression will be used as fallback (default: false);
*  - RATE: to provide a user-defined lapse rate (SI units);
*  - SOFT: if set to true, the user provided lapse rate is only used when no lapse rate could be computed from the data (or if it was too bad, ie r²<0.6);
*  - FRAC: if set to true, the user provided lapse rate will be interpreted as "fractional", that is a relative change
* of the value as a function of the elevation (for example, +0.05% per meters given as 0.0005). In this case, no attempt to calculate
* the fractional lapse from the data is made. The lapse rate that might be reported to the user will be computed as {data average}*{user-defined rate}
* and is therefore NOT directly the user-defined lapse rate.
*  - TREND_MIN_ALT: all points at elevations less than this will be detrended/retrended as if at this provided elevation (optional);
*  - TREND_MAX_ALT: all points at elevations more than this will be detrended/retrended as if at this provided elevation (optional);
*
* @param[in] vecArgs a vector containing all the arguments
* @param[in] algo the name of the algorithm calling it, for user-friendly error messages
* @param[in] i_param the meteorological parameter that is handled, for user-friendly error messages
*/
Trend::Trend(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& algo, const std::string& i_param)
          : multi_trend(), trend_model(), param(i_param), user_lapse(IOUtils::nodata), trend_min_alt(-1e12), trend_max_alt(1e12),
          frac(false), soft(false), multilinear(false)
{
	const std::string where( "Interpolations2D::"+i_param+"::"+algo );
	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="RATE") {
			IOUtils::parseArg(vecArgs[ii], where, user_lapse);
		} else if (vecArgs[ii].first=="FRAC") {
			IOUtils::parseArg(vecArgs[ii], where, frac);
		} else if (vecArgs[ii].first=="SOFT") {
			IOUtils::parseArg(vecArgs[ii], where, soft);
		} else if (vecArgs[ii].first=="TREND_MIN_ALT") {
			IOUtils::parseArg(vecArgs[ii], where, trend_min_alt);
		} else if (vecArgs[ii].first=="TREND_MAX_ALT") {
			IOUtils::parseArg(vecArgs[ii], where, trend_max_alt);
		} else if (vecArgs[ii].first=="MULTILINEAR") {
			IOUtils::parseArg(vecArgs[ii], where, multilinear);
		}
	}

	if (frac && user_lapse==IOUtils::nodata) throw InvalidArgumentException("Please provide a lapse rate when using FRAC for "+where, AT);
	if (soft && user_lapse==IOUtils::nodata) throw InvalidArgumentException("Please provide a fallback lapse rate when using SOFT for "+where, AT);
	if (soft && frac) throw InvalidArgumentException("It is not possible to use SOFT and FRAC at the same time for "+where, AT);
}

std::vector<double> Trend::getStationAltitudes(const std::vector<StationData>& vecMeta)
{
	std::vector<double> o_vecData;
	for (size_t ii=0; ii<vecMeta.size(); ii++) {
		const double alt = vecMeta[ii].position.getAltitude();
		if (alt != IOUtils::nodata) {
			o_vecData.push_back( alt );
		}
	}

	return o_vecData;
}

bool Trend::multilinearDetrend(const std::vector<StationData>& vecMeta, std::vector<double> &vecDat)
{
	if (vecDat.size() != vecMeta.size()) {
		std::ostringstream ss;
		ss << "Number of station data (" << vecDat.size() << ") and number of metadata (" << vecMeta.size() << ") don't match!";
		throw InvalidArgumentException(ss.str(), AT);
	}

	//set the trend data
	//HACK the positions MUST be in the same coordsys as the dem!
	for (size_t ii=0; ii<vecMeta.size(); ii++) {
		std::vector<double> predictors(3);
		predictors[0] = vecMeta[ii].position.getAltitude();
		predictors[1] = vecMeta[ii].position.getEasting();
		predictors[2] = vecMeta[ii].position.getNorthing();
		multi_trend.addData(predictors, vecDat[ii]);
	}

	//compute the trend
	const bool status = multi_trend.fit();

	//detrend the data if successful
	if (status==true) {
		for (size_t ii=0; ii<vecMeta.size(); ii++) {
			double &val = vecDat[ii];
			if (val==IOUtils::nodata) continue;

			std::vector<double> predictors(3);
			predictors[0] = vecMeta[ii].position.getAltitude();
			predictors[1] = vecMeta[ii].position.getEasting();
			predictors[2] = vecMeta[ii].position.getNorthing();
			const double trend_offset = multi_trend( predictors );

			if (trend_offset!=IOUtils::nodata) {
				val -= trend_offset;
			} else {
				val = IOUtils::nodata;
			}
		}
	}

	return status;
}

/**
 * @brief Compute the trend according to the provided data and detrend vecDat
 * @param[in] vecMeta Location informations sorted similarly as the data in vecDat
 * @param vecDat data for the interpolated parameter
*/
void Trend::detrend(const std::vector<StationData>& vecMeta, std::vector<double> &vecDat)
{
	if (multilinear) {
		if (multilinearDetrend(vecMeta, vecDat)==true) return;
	}

	//simple, linear fit
	const std::vector<double> vecAltitudes( getStationAltitudes(vecMeta) );
	if (vecAltitudes.empty())
		throw IOException("Not enough altitude data for spatially interpolating parameter " + param, AT);

	if (vecDat.size() != vecAltitudes.size()) {
		std::ostringstream ss;
		ss << "Number of station data (" << vecDat.size() << ") and number of elevations (" << vecAltitudes.size() << ") don't match!";
		throw InvalidArgumentException(ss.str(), AT);
	}

	initTrendModel(vecAltitudes, vecDat); //This must be done before using the trend model

	for (size_t ii=0; ii<vecAltitudes.size(); ii++) {
		double &val = vecDat[ii];
		if (val!=IOUtils::nodata) {
			const double altitude = std::min( std::max(vecAltitudes[ii], trend_min_alt), trend_max_alt );
			val -= trend_model( altitude );
		}
	}
}

void Trend::multilinearRetrend(const DEMObject& dem, Grid2DObject &grid) const
{
	const double xllcorner = dem.llcorner.getEasting();
	const double yllcorner = dem.llcorner.getNorthing();
	const double cellsize = dem.cellsize;

	for (size_t jj=0; jj<dem.getNy(); jj++) {
		for (size_t ii=0; ii<dem.getNx(); ii++) {
			double &val = grid(ii, jj);
			if (val==IOUtils::nodata) continue;

			std::vector<double> predictors(3);
			predictors[0] = dem(ii, jj);
			predictors[1] = xllcorner + static_cast<double>(ii)*cellsize;
			predictors[2] = yllcorner + static_cast<double>(jj)*cellsize;
			const double trend_offset = multi_trend( predictors );
			val += trend_offset;
		}
	}
}

/**
 * @brief Re-apply the trend computed in a previous call to detrend() to the gridded results
 * @param[in] dem digital elevation model (DEM)
 * @param grid matching grid filled with data that has to be re-trended
*/
void Trend::retrend(const DEMObject& dem, Grid2DObject &grid) const
{
	const size_t nxy = grid.size();
	if (dem.size() != nxy) {
		std::ostringstream ss;
		ss << "Dem size (" << dem.grid2D.getNx() << "," << dem.grid2D.getNy() << ") and";
		ss << "grid size (" << grid.getNx() << "," << grid.getNy() << ") don't match!";
		throw InvalidArgumentException(ss.str(), AT);
	}

	if (multi_trend.isReady()) { //ie the fit could be computed during the detrend stage
		multilinearRetrend(dem, grid);
		return;
	}

	for (size_t ii=0; ii<nxy; ii++) {
		double &val = grid(ii);
		if (val==IOUtils::nodata) continue;

		const double altitude = std::min( std::max(dem(ii), trend_min_alt), trend_max_alt );
		val += trend_model( altitude );
	}
}

/**
 * @brief Read the interpolation arguments and set the trend properties accordingly
 * @param[in] vecAltitudes altitudes sorted similarly as the data in vecDat
 * @param[in] vecDat data for the interpolated parameter
*/
void Trend::initTrendModel(const std::vector<double>& vecAltitudes, const std::vector<double>& vecDat)
{
	bool status=false;
	if (user_lapse==IOUtils::nodata) {
		trend_model.setModel(Fit1D::NOISY_LINEAR, vecAltitudes, vecDat, false);
		status = trend_model.fit();
	} else {
		if (soft) {
			trend_model.setModel(Fit1D::NOISY_LINEAR, vecAltitudes, vecDat, false);
			status = trend_model.fit();
			if (!status) {
				trend_model.setModel(Fit1D::NOISY_LINEAR, vecAltitudes, vecDat, false);
				trend_model.setLapseRate(user_lapse);
				status = trend_model.fit();
			}
		} else {
			if (frac) { //forced FRAC
				const double avgData = Interpol1D::arithmeticMean(vecDat);
				if (avgData==0.) {//since we only have zeroes, we should generate zeroes...
					trend_model.setModel(Fit1D::ZERO, vecAltitudes, vecDat, false);
					status = trend_model.fit();
					trend_model.setInfo(trend_model.getInfo() + " (null average input for frac lapse rate)");
				} else {
					trend_model.setModel(Fit1D::NOISY_LINEAR, vecAltitudes, vecDat, false);
					trend_model.setLapseRate(user_lapse*avgData);
					status = trend_model.fit();
				}
			} else { //forced user lapse rate
				trend_model.setModel(Fit1D::NOISY_LINEAR, vecAltitudes, vecDat, false);
				trend_model.setLapseRate(user_lapse);
				status = trend_model.fit();
			}
		}
	}

	if (!status)
		throw IOException("Interpolation FAILED for parameter " + param + ": " + trend_model.getInfo(), AT);
}

std::string Trend::getInfo() const
{
	if (multi_trend.isReady()) //ie the fit could be computed during the detrend stage
		return multi_trend.getInfo();
	else
		return trend_model.getInfo();
}

std::string Trend::toString() const //HACK include multi_trend
{
	std::ostringstream os;
	os << "<Trend>\n";
	os << "Parameter: " << param << " user_lapse: " << user_lapse << " min_alt: " << trend_min_alt << " max_alt: " << trend_max_alt << "\n";
	os << "frac: " << std::boolalpha << frac << " soft: " << soft << "\n";
	os << trend_model.toString() << "\n";
	os << "</Trend>\n";

	return os.str();
}

} //namespace
