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

#include <meteoio/spatialInterpolations/ILWREpsAlgorithm.h>
#include <meteoio/meteoStats/libinterpol1D.h>
#include <meteoio/meteoStats/libinterpol2D.h>
#include <meteoio/meteoLaws/Atmosphere.h>

namespace mio {

ILWREpsAlgorithm::ILWREpsAlgorithm(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm,
		                                 Meteo2DInterpolator& i_mi)
                                  : InterpolationAlgorithm(vecArgs, i_algo, i_param, i_tsm), trend(vecArgs, i_algo, i_param), mi(i_mi), vecDataEA(), scale(1e3), alpha(1.)
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

double ILWREpsAlgorithm::getQualityRating(const Date& i_date)
{
	date = i_date;
	vecData.clear(); vecMeta.clear();
	vecDataEA.clear();

	tsmanager.getMeteoData(date, vecMeteo); //getData has not been called, so manually fill vecMeteo
	for (size_t ii=0; ii<vecMeteo.size(); ii++){
		if ((vecMeteo[ii](MeteoData::ILWR) != IOUtils::nodata) && (vecMeteo[ii](MeteoData::TA) != IOUtils::nodata)){
			vecDataEA.push_back( Atmosphere::blkBody_Emissivity( vecMeteo[ii](MeteoData::ILWR), vecMeteo[ii](MeteoData::TA)) );
			vecMeta.push_back(vecMeteo[ii].meta);
		}
	}

	nrOfMeasurments = vecDataEA.size();
	if (nrOfMeasurments==0) return 0.0;

	return 0.9;
}

void ILWREpsAlgorithm::calculate(const DEMObject& dem, Grid2DObject& grid)
{
	info.clear(); info.str("");

	Grid2DObject ta_grid;
	mi.interpolate(date, dem, MeteoData::TA, ta_grid); //get TA interpolation from call back to Meteo2DInterpolator

	trend.detrend(vecMeta, vecDataEA);
	info << trend.getInfo();
	Interpol2D::IDW(vecDataEA, vecMeta, dem, grid, scale, alpha); //the meta should NOT be used for elevations!
	trend.retrend(dem, grid);

	//Recompute ILWR from the interpolated ea
	for (size_t ii=0; ii<grid.size(); ii++) {
		double &value = grid(ii);
		const double ta = ta_grid(ii);
		if (value!=IOUtils::nodata && ta!=IOUtils::nodata) value = Atmosphere::blkBody_Radiation(value, ta);
	}
}

} //namespace
