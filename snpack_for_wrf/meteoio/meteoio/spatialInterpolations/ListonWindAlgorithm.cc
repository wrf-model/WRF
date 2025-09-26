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

#include <meteoio/spatialInterpolations/ListonWindAlgorithm.h>
#include <meteoio/meteoStats/libinterpol2D.h>
#include <meteoio/MathOptim.h>

namespace mio {

ListonWindAlgorithm::ListonWindAlgorithm(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm)
                                   : InterpolationAlgorithm(vecArgs, i_algo, i_param, i_tsm), trend(vecArgs, i_algo, i_param), vecDataVW(), vecDataDW(),
                                   scale(1e3), alpha(1.), param_idx(MeteoData::firstparam), inputIsAllZeroes(false)
{
	const std::string where( "Interpolations2D::"+i_param+"::"+i_algo );
	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="SCALE") {
			IOUtils::parseArg(vecArgs[ii], where, scale);
		} else if (vecArgs[ii].first=="ALPHA") {
			IOUtils::parseArg(vecArgs[ii], where, alpha);
		}
	}
}

double ListonWindAlgorithm::getQualityRating(const Date& i_date)
{
	date = i_date;
	vecMeta.clear();
	vecDataVW.clear(); vecDataDW.clear();

	tsmanager.getMeteoData(date, vecMeteo);
	if (!vecMeteo.empty()) param_idx = vecMeteo[0].getParameterIndex( param );
	for (size_t ii=0; ii<vecMeteo.size(); ii++){
		if ((vecMeteo[ii](MeteoData::VW) != IOUtils::nodata) && (vecMeteo[ii](MeteoData::DW) != IOUtils::nodata)){
			vecDataVW.push_back(vecMeteo[ii](MeteoData::VW));
			vecDataDW.push_back(vecMeteo[ii](MeteoData::DW));
			vecMeta.push_back(vecMeteo[ii].meta);
		}
	}

	nrOfMeasurments = vecMeta.size();

	if (nrOfMeasurments==0) return 0.0;

	if (Interpol2D::allZeroes(vecDataVW)) {
		inputIsAllZeroes = true;
		return 0.9;
	}

	if ( (nrOfMeasurments<vecDataVW.size()/2) || ( nrOfMeasurments<2 ) )
		return 0.6;

	return 0.9;
}

void ListonWindAlgorithm::calculate(const DEMObject& dem, Grid2DObject& grid)
{
	info.clear(); info.str("");

	//if all data points are zero, simply fill the grid with zeroes
	if (inputIsAllZeroes) {
		Interpol2D::constant(0., dem, grid);
		return;
	}

	if (param_idx==MeteoData::VW) {
		Grid2DObject DW;
		simpleWindInterpolate(dem, grid, DW);
		Interpol2D::ListonWind(dem, grid, DW);
	}
	if (param_idx==MeteoData::DW) {
		Grid2DObject VW;
		simpleWindInterpolate(dem, VW, grid);
		Interpol2D::ListonWind(dem, VW, grid);
	}
}

//this interpolates VW, DW by converting to u,v and then doing IDW_LAPSE before reconverting to VW, DW
void ListonWindAlgorithm::simpleWindInterpolate(const DEMObject& dem, Grid2DObject &VW, Grid2DObject &DW)
{
	if (vecDataVW.size() != vecDataDW.size())
		throw InvalidArgumentException("VW and DW vectors should have the same size!", AT);

	//compute U,v
	std::vector<double> Ve(vecDataVW.size()), Vn(vecDataVW.size());
	for (size_t ii=0; ii<vecDataVW.size(); ii++) {
		Ve[ii] = vecDataVW[ii]*sin(vecDataDW[ii]*Cst::to_rad);
		Vn[ii] = vecDataVW[ii]*cos(vecDataDW[ii]*Cst::to_rad);
	}

	//spatially interpolate U,V
	if (vecDataVW.size()>=4) { //at least for points to perform detrending
		trend.detrend(vecMeta, Ve);
		info << trend.getInfo();
		Interpol2D::IDW(Ve, vecMeta, dem, VW, scale, alpha);
		trend.retrend(dem, VW);

		trend.detrend(vecMeta, Vn);
		info << trend.getInfo();
		Interpol2D::IDW(Vn, vecMeta, dem, DW, scale, alpha);
		trend.retrend(dem, DW);
	} else {
		Interpol2D::IDW(Ve, vecMeta, dem, VW, scale, alpha);
		Interpol2D::IDW(Vn, vecMeta, dem, DW, scale, alpha);
	}

	//recompute VW, DW in each cell
	for (size_t ii=0; ii<VW.size(); ii++) {
		const double ve = VW(ii);
		const double vn = DW(ii);

		if (ve!=IOUtils::nodata && vn!=IOUtils::nodata) {
			VW(ii) = Optim::fastSqrt_Q3(ve*ve + vn*vn);
			DW(ii) = fmod( atan2(ve,vn) * Cst::to_deg + 360., 360.);
		}
	}
}

} //namespace
