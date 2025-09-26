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

#include <meteoio/spatialInterpolations/RHListonAlgorithm.h>
#include <meteoio/meteoStats/libinterpol2D.h>
#include <meteoio/meteoLaws/Atmosphere.h>

#include <sstream>

namespace mio {

RHListonAlgorithm::RHListonAlgorithm(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm, Meteo2DInterpolator& i_mi)
                                : InterpolationAlgorithm(vecArgs, i_algo, i_param, i_tsm), trend(vecArgs, i_algo, i_param), mi(i_mi), vecDataTA(), vecDataRH(), scale(1e3), alpha(1.)
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

double RHListonAlgorithm::getQualityRating(const Date& i_date)
{
	date = i_date;
	vecData.clear(); vecMeta.clear();
	vecDataTA.clear(); vecDataRH.clear();

	tsmanager.getMeteoData(date, vecMeteo);

	for (size_t ii=0; ii<vecMeteo.size(); ii++){
		if ((vecMeteo[ii](MeteoData::RH) != IOUtils::nodata) && (vecMeteo[ii](MeteoData::TA) != IOUtils::nodata)){
			vecDataTA.push_back(vecMeteo[ii](MeteoData::TA));
			vecDataRH.push_back(vecMeteo[ii](MeteoData::RH));
			vecMeta.push_back(vecMeteo[ii].meta);
		}
	}

	nrOfMeasurments = vecMeta.size();
	if (nrOfMeasurments==0)
		return 0.0;
	if ( (nrOfMeasurments<vecDataRH.size()/2) || ( nrOfMeasurments<2 ) )
		return 0.6;

	return 0.9;
}

void RHListonAlgorithm::calculate(const DEMObject& dem, Grid2DObject& grid)
{
	info.clear(); info.str("");

	Grid2DObject ta;
	mi.interpolate(date, dem, MeteoData::TA, ta); //get TA interpolation from call back to Meteo2DInterpolator

	//RH->Td, interpolations, Td->RH
	//Compute dew point temperatures at stations
	std::vector<double> vecTd( vecDataRH.size() );
	for (size_t ii=0; ii<vecDataRH.size(); ii++){
		const double rh = vecDataRH[ii];
		if (rh<0. || rh>1.) {
			std::ostringstream ss;
			ss << "Invalid relative humidity: " << rh << " on " << date.toString(Date::ISO) << "\n";
			throw InvalidArgumentException(ss.str(), AT);
		}
		vecTd[ii] = Atmosphere::RhtoDewPoint(rh, vecDataTA[ii], 1);
	}

	if (nrOfMeasurments>=2) {
		trend.detrend(vecMeta, vecTd);
		info << trend.getInfo();
		Interpol2D::IDW(vecTd, vecMeta, dem, grid, scale, alpha); //the meta should NOT be used for elevations!
		trend.retrend(dem, grid);
	} else {
		Interpol2D::IDW(vecTd, vecMeta, dem, grid, scale, alpha); //the meta should NOT be used for elevations!
	}

	//Recompute Rh from the interpolated td
	for (size_t ii=0; ii<grid.size(); ii++) {
		double &value = grid(ii);
		if (value!=IOUtils::nodata)
			value = Atmosphere::DewPointtoRh(value, ta(ii), 1);
	}
}

} //namespace
