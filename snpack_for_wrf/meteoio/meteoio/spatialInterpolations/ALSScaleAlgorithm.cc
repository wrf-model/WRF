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

#include <meteoio/spatialInterpolations/ALSScaleAlgorithm.h>
#include <meteoio/FileUtils.h>
#include <meteoio/meteoStats/libinterpol2D.h>

namespace mio {

ALS_Interpolation::ALS_Interpolation(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm,
		                               GridsManager& i_gdm, Meteo2DInterpolator& i_mi)
		  : InterpolationAlgorithm(vecArgs, i_algo, i_param, i_tsm), mi(i_mi), gdm(i_gdm), ALS_scan(), filename(),
		    grid2d_path(), base_algo(), base_algo_user(), ta_thresh(IOUtils::nodata), als_mean(IOUtils::nodata), inputIsAllZeroes(false)
{
	const std::string where( "Interpolations2D::"+i_param+"::"+i_algo );
	gdm.getConfig().getValue("GRID2DPATH", "Input", grid2d_path);
	bool has_grid=false, has_base=false;

	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="GRID") {
			filename = vecArgs[ii].second;
			has_grid = true;
		} else if(vecArgs[ii].first=="BASE") {
			base_algo_user = IOUtils::strToUpper( vecArgs[ii].second );
			has_base = true;
		} else if(vecArgs[ii].first=="TA_THRESH") {
			IOUtils::parseArg(vecArgs[ii], where, ta_thresh);
		}
	}

	if (!has_grid) throw InvalidArgumentException("Please provide a grid filename for the "+where, AT);
	if (!has_base) throw InvalidArgumentException("Please provide a base algorithm for the "+where, AT);

	if (!FileUtils::validFileAndPath(grid2d_path+"/"+filename)) {
		throw InvalidNameException("[E] Invalid grid filename for "+where+" : "+grid2d_path+"/"+filename, AT);
	}
}

void ALS_Interpolation::initGrid(const DEMObject& dem, Grid2DObject& grid)
{
	//initialize precipitation grid with user supplied algorithm (IDW_LAPSE by default)
	const std::vector< std::pair<std::string, std::string> > vecArgs( mi.getArgumentsForAlgorithm(param, base_algo, "Interpolations2D") );
	InterpolationAlgorithm* algorithm(AlgorithmFactory::getAlgorithm(base_algo, mi, vecArgs, tsmanager, gdm, param));
	algorithm->getQualityRating(date);
	algorithm->calculate(dem, grid);
	info << algorithm->getInfo();
	delete algorithm;
}

double ALS_Interpolation::getQualityRating(const Date& i_date)
{
	date = i_date;
	nrOfMeasurments = getData(date, param, vecData, vecMeta);
	inputIsAllZeroes = Interpol2D::allZeroes(vecData);

	if (nrOfMeasurments==0) return 0.;
	if (inputIsAllZeroes) return 1.;

	base_algo = (nrOfMeasurments>1)? base_algo_user : "AVG";

	return (FileUtils::fileExists(grid2d_path+"/"+filename))? 1. : 0.;
}

void ALS_Interpolation::calculate(const DEMObject& dem, Grid2DObject& grid)
{
	info.clear(); info.str("");

	//if all data points are zero, simply fill the grid with zeroes
	if (inputIsAllZeroes) {
		Interpol2D::constant(0., dem, grid);
		return;
	}

	if (ALS_scan.empty()) { //read the ALS scan if necessary
		gdm.read2DGrid(ALS_scan, filename);
		if (ta_thresh==IOUtils::nodata) {
			als_mean = ALS_scan.grid2D.getMean();
			if (als_mean==0. || als_mean==IOUtils::nodata)
				throw InvalidArgumentException("[E] the scaling grid(" + filename + ") can not have a nul or nodata mean for the '"+algo+"' method!", AT);
		}
	}

	//check that the ALS scan matches the provided DEM
	if (!ALS_scan.isSameGeolocalization(dem))
		throw InvalidArgumentException("[E] trying to load a grid(" + filename + ") that does not have the same georeferencing as the DEM!", AT);
	else
		info << FileUtils::getFilename(filename) << " - ";

	initGrid(dem, grid);

	// create map of TA to differ between solid and liquid precipitation
	Grid2DObject ta;
	mi.interpolate(date, dem, MeteoData::TA, ta); //get TA interpolation from call back to Meteo2DInterpolator

	//Compute scaling factors: psum_mean and als_mean according to the pixels selection criteria
	double psum_mean = 0.;
	if (ta_thresh==IOUtils::nodata) { //simple case: no TA_THRESH
		double psum_sum = 0.;
		size_t count = 0;
		for (size_t jj=0; jj<grid.size(); jj++) {
			const double val = grid(jj);
			const bool has_Scan = (ALS_scan(jj)!=IOUtils::nodata);
			if (val!=IOUtils::nodata && has_Scan ) {
				psum_sum += val;
				count++;
			}
		}
		if (count==0) return; //no overlap between ALS, TA and initial grid
		psum_mean = psum_sum / static_cast<double>( count );
	} else { //use local air temperature
		double psum_sum = 0., als_sum = 0.;
		size_t count = 0;
		for (size_t jj=0; jj<grid.size(); jj++) {
			const double val = grid(jj);
			const bool has_Scan = (ALS_scan(jj)!=IOUtils::nodata);
			const bool has_TA = (ta(jj)!=IOUtils::nodata);
			if (val!=IOUtils::nodata && has_Scan && has_TA && ta(jj) < ta_thresh) {
				psum_sum += val;
				als_sum += ALS_scan(jj);
				count++;
			}
		}
		if (count==0) return; //no overlap between ALS, TA and initial grid
		psum_mean = psum_sum / static_cast<double>( count );
		als_mean = als_sum / static_cast<double>( count );
		if (als_mean==0.)
			throw InvalidArgumentException("[E] the scaling grid(" + filename + ") can not have a nul mean for the '"+algo+"' method!", AT);
	}

	//pixels that are nodata are kept such as computed by "base_algo", otherwise we take the newly computed values
	if (ta_thresh==IOUtils::nodata) { //simple case: no TA_THRESH
		for (size_t jj=0; jj<grid.size(); jj++) {
			double &val = grid(jj);
			const bool has_Scan = (ALS_scan(jj)!=IOUtils::nodata);
			if (val!=IOUtils::nodata && has_Scan)
				val = ALS_scan(jj) * ( psum_mean / als_mean );
		}
	} else { //use local air temperature
		for (size_t jj=0; jj<grid.size(); jj++) {
			double &val = grid(jj);
			const bool has_Scan = (ALS_scan(jj)!=IOUtils::nodata);
			const bool has_TA = (ta(jj)!=IOUtils::nodata);
			if (val!=IOUtils::nodata && has_Scan && has_TA && ta(jj) < ta_thresh)
				val = ALS_scan(jj) * ( psum_mean / als_mean );
		}
	}
}

} //namespace
