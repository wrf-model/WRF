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
#include <meteoio/meteoFilters/ProcAggregate.h>
#include <meteoio/meteoStats/libinterpol1D.h>
#include <meteoio/meteoLaws/Meteoconst.h>

using namespace std;

namespace mio {

ProcAggregate::ProcAggregate(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name)
              : WindowedFilter(vecArgs, name, true), type(mean_agg)
{
	parse_args(vecArgs);

	//This is safe, but maybe too imprecise:
	properties.time_before = min_time_span;
	properties.time_after  = min_time_span;
	properties.points_before = min_data_points;
	properties.points_after = min_data_points;
}

void ProcAggregate::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
                            std::vector<MeteoData>& ovec)
{
	ovec = ivec;
	
	if (type==step_sum) {
		sumOverLastStep(ovec, param);
		return;
	}
	
	for (size_t ii=0; ii<ovec.size(); ii++){ //for every element in ivec, get a window
		double& value = ovec[ii](param);
		size_t start, end;
		if ( get_window_specs(ii, ivec, start, end) ) {
			switch (type) {
				case min_agg:
					value = calc_min(ivec, param, start, end); break;
				case max_agg:
					value = calc_max(ivec, param, start, end); break;
				case mean_agg:
					value = calc_mean(ivec, param, start, end); break;
				case median_agg:
					value = calc_median(ivec, param, start, end); break;
				case wind_avg_agg:
					value = calc_wind_avg(ivec, param, start, end); break;
				default:
					throw UnknownValueException("Unknown aggregation algorithm selected!", AT);
			}
		} else {
			if (!is_soft) value = IOUtils::nodata;
		}
	}
}

void ProcAggregate::sumOverLastStep(std::vector<MeteoData>& ovec, const unsigned int& param)
{
	double previous_ts = IOUtils::nodata;
	for (size_t ii=0; ii<ovec.size(); ii++) {
		double& value = ovec[ii](param);
		if (value!=IOUtils::nodata && value!=0) {
			if (previous_ts!=IOUtils::nodata) {
				const double acc_period = (ovec[ii].date.getJulian() - previous_ts) * 24.; //in hours
				value *= acc_period;
			} else {
				value = IOUtils::nodata;
			}
		}
		
		previous_ts = ovec[ii].date.getJulian();
	}
}

double ProcAggregate::calc_min(const std::vector<MeteoData>& ivec, const unsigned int& param, const size_t& start, const size_t& end)
{
	double min = Cst::dbl_max;
	for (size_t ii=start; ii<=end; ii++){
		const double& value = ivec[ii](param);
		if (value!=IOUtils::nodata && value<min) min = value;
	}
	
	if (min == Cst::dbl_max) return IOUtils::nodata;
	return min;
}

double ProcAggregate::calc_max(const std::vector<MeteoData>& ivec, const unsigned int& param, const size_t& start, const size_t& end)
{
	double max = Cst::dbl_min;
	for (size_t ii=start; ii<=end; ii++){
		const double& value = ivec[ii](param);
		if (value!=IOUtils::nodata && value>max) max = value;
	}

	if (max == Cst::dbl_min) return IOUtils::nodata;
	return max;
}

double ProcAggregate::calc_mean(const std::vector<MeteoData>& ivec, const unsigned int& param, const size_t& start, const size_t& end)
{
	double sum = 0;
	size_t counter = 0;
	for (size_t ii=start; ii<=end; ii++){
		const double& value = ivec[ii](param);
		if (value != IOUtils::nodata){
			sum += value;
			counter++;
		}
	}

	if (counter == 0) return IOUtils::nodata;
	return (sum / (double)counter);
}

double ProcAggregate::calc_median(const std::vector<MeteoData>& ivec, const unsigned int& param, const size_t& start, const size_t& end)
{
	std::vector<double> vecTemp;
	vecTemp.reserve( end-start+1 );
	for (size_t ii=start; ii<=end; ii++){ //get rid of nodata elements
		const double& value = ivec[ii](param);
		if (value != IOUtils::nodata)
			vecTemp.push_back(value);
	}

	return Interpol1D::getMedian(vecTemp, false);
}

double ProcAggregate::calc_wind_avg(const std::vector<MeteoData>& ivec, const unsigned int& param, const size_t& start, const size_t& end)
{
	//calculate ve and vn
	double ve=0.0, vn=0.0;
	size_t count=0;
	for (size_t ii=start; ii<=end; ii++) {
		const double VW = ivec[ii](MeteoData::VW);
		const double DW = ivec[ii](MeteoData::DW);
		if (VW!=IOUtils::nodata && DW!=IOUtils::nodata) {
			ve += VW * sin(DW*Cst::to_rad);
			vn += VW * cos(DW*Cst::to_rad);
			count++;
		}
	}

	if (count==0) return IOUtils::nodata;

	ve /= static_cast<double>(count);
	vn /= static_cast<double>(count);

	if (param==MeteoData::VW) {
		const double meanspeed = sqrt(ve*ve + vn*vn);
		return meanspeed;
	} else {
		const double meandirection = fmod( atan2(ve,vn) * Cst::to_deg + 360. , 360.); // turn into degrees [0;360)
		return meandirection;
	}
}

void ProcAggregate::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	const std::string where( "Filters::"+block_name );
	//"soft" is already read with the window parameters by the constructor of WindowedFilter
	bool has_type=false;

	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="TYPE") {
			const std::string type_str( IOUtils::strToUpper( vecArgs[ii].second ) );
			if (type_str=="MIN") type=min_agg;
			else if (type_str=="MAX") type=max_agg;
			else if (type_str=="MEAN") type=mean_agg;
			else if (type_str=="MEDIAN") type=median_agg;
			else if (type_str=="STEP_SUM") type=step_sum;
			else if (type_str=="WIND_AVG") type=wind_avg_agg;
			else
				throw InvalidArgumentException("Unknown type '"+type_str+"' for " + where, AT);

			has_type = true;
		}
	}

	if (!has_type) throw InvalidArgumentException("Please provide a TYPE for "+where, AT);
	
	if (type!=step_sum) setWindowFParams( vecArgs );
}

} //namespace
