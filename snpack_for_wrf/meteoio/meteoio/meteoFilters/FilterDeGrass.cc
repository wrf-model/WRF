/***********************************************************************************/
/*  Copyright 2014 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#include <meteoio/meteoFilters/FilterDeGrass.h>
#include <meteoio/meteoStats/libinterpol1D.h>

using namespace std;

namespace mio {

FilterDeGrass::FilterDeGrass(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name)
          : ProcessingBlock(vecArgs, name), prev_day(), TSS_daily_max(IOUtils::nodata), TSS_daily_min(IOUtils::nodata),
		    TSS_daily_mean(IOUtils::nodata), TSG_daily_var(IOUtils::nodata), TSS_user_offset(IOUtils::nodata), month(7)
{
	properties.stage = ProcessingProperties::first;
	const std::string where( "Filters::"+block_name );

	//parse the arguments (the keys are all upper case)
	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="TSS_OFFSET") {
			IOUtils::parseArg(vecArgs[ii], where, TSS_user_offset);
		}
	}
}

void FilterDeGrass::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
                        std::vector<MeteoData>& ovec)
{
	ovec = ivec;
	double TSS_offset = (TSS_user_offset==IOUtils::nodata)? getTSSOffset(param, ovec) : TSS_user_offset;
	if (TSS_offset==IOUtils::nodata) TSS_offset = 0.;
	
	//find first day of the Spring when min(TSS)>1C for 24 hours
	size_t tssWarmDay_idx, tsgWarmDay_idx;
	findFirstWarmDay(ovec, tssWarmDay_idx, tsgWarmDay_idx);
	const Date warmDayTSS = (tssWarmDay_idx!=IOUtils::npos)? ovec[tssWarmDay_idx].date : Date(0., 0.); //ie disable the check if not found
	const Date warmDayTSG = (tsgWarmDay_idx!=IOUtils::npos)? ovec[tsgWarmDay_idx].date : ovec.back().date; //ie disable the check if not found
	
	//find correlation between TSS and TSG for the 7 days after the firstWarmDay
	const double tss_tsg_correlation = getTssTsgCorrelation(ovec, tssWarmDay_idx);
	const bool has_correlation = (tss_tsg_correlation!=IOUtils::nodata);
	
	//now perform the filtering, one point after another
	for (size_t ii=0; ii<ovec.size(); ii++) {
		const Date curr_date( ovec[ii].date );
		const Date day_start( getDailyStart( curr_date ) );
		if (day_start!=prev_day) {
			int year, day;
			curr_date.getDate(year, month, day);
			getTSSDailyPpt(ovec, day_start, TSS_daily_min, TSS_daily_max, TSS_daily_mean);
			prev_day = day_start;
		}
		
		const bool has_TSS = (TSS_daily_min!=IOUtils::nodata); //for TSS, only daily values are required
		const bool has_TSG = (ovec[ii](MeteoData::TSG)!=IOUtils::nodata);
		if (has_TSS && has_TSG) {
			if (has_correlation) {
				if (curr_date>warmDayTSG || curr_date<warmDayTSS || warmDayTSG<warmDayTSS) {
					filterOnTss(param, ii, 0., ovec);
				} else {
					if (TSS_offset>1.5 || tss_tsg_correlation<0.2) {
						filterOnTsg(param, ii, ovec);
					} else {
						filterOnTss(param, ii, 0., ovec);
					}
				}
			} else {
				filterOnTss(param, ii, TSS_offset, ovec);
			}
		} else {
			if (has_TSS) {
				filterOnTss(param, ii, TSS_offset, ovec);
			} else {
				filterOnTsg(param, ii, ovec);
			}
		}
	}
}

void FilterDeGrass::filterOnTsg(const unsigned int& param, const size_t& ii, std::vector<MeteoData>& ovec)
{
	double& value = ovec[ii](param);
	if (value==IOUtils::nodata) return;
	
	const double TSG = ovec[ii](MeteoData::TSG);
	if (TSG==IOUtils::nodata) return;
	
	//compute TSG daily variance
	const Date day_start = getDailyStart(ovec[ii].date);
	if (day_start!=prev_day) {
		TSG_daily_var = getDailyTSGVariance(ovec, day_start);
		prev_day = day_start;
	}
	
	if (TSG>IOUtils::C_TO_K(7.) || TSG_daily_var>1.) value = 0.;
}

void FilterDeGrass::filterOnTss(const unsigned int& param, const size_t& ii, const double& tss_offset, std::vector<MeteoData>& ovec)
{
	double& value = ovec[ii](param);
	if (value==IOUtils::nodata) return;
	if (TSS_daily_min==IOUtils::nodata) return;
	
	if ((TSS_daily_mean+tss_offset)>IOUtils::C_TO_K(0.) && (TSS_daily_max+tss_offset)>=IOUtils::C_TO_K(5.)) {
		value = 0.;
		return;
	}
	
	//HACK: this is location dependant...
	if (month>=7 && month<=12) { //early snow season
		if ((TSS_daily_min+tss_offset)>=IOUtils::C_TO_K(2.)) {
			value = 0.;
		} else {
			if ((TSS_daily_min+tss_offset)>=IOUtils::C_TO_K(0.) && value<0.4) {
				value = 0.;
			} else if ((TSS_daily_min+tss_offset)<IOUtils::C_TO_K(0.) && (TSS_daily_max+tss_offset)>=IOUtils::C_TO_K(2.)) {
				value = 0.;
			}
		}
	}
}

double FilterDeGrass::getTssTsgCorrelation(const std::vector<MeteoData>& ovec, const size_t& firstWarmDay_idx)
{
	std::vector<double> vecTSS, vecTSG;
	if (firstWarmDay_idx!=IOUtils::npos) {
		const Date firstWarmDay( ovec[firstWarmDay_idx].date );
		for (size_t ii=firstWarmDay_idx; ii<ovec.size(); ii++) {
			if (ovec[ii].date > firstWarmDay+7.) break;
			const double TSS = ovec[ii](MeteoData::TSS);
			vecTSS.push_back( TSS );
			const double TSG = ovec[ii](MeteoData::TSG);
			vecTSG.push_back( TSG );
		}
	} else {
		return IOUtils::nodata;
	}
	
	return Interpol1D::corr(vecTSS, vecTSG);
}

void FilterDeGrass::findFirstWarmDay(const std::vector<MeteoData>& ovec, size_t &tssWarmDay_idx, size_t &tsgWarmDay_idx)
{
	tssWarmDay_idx = IOUtils::npos;
	tsgWarmDay_idx = IOUtils::npos;
	for (size_t ii=10; ii<ovec.size(); ii++) {
		const Date current( ovec[ii].date );
		int curr_year, curr_month, curr_day;
		current.getDate(curr_year, curr_month, curr_day);
		if (curr_month>=9) continue; //HACK: this is location dependant...
			
		double TSS_min = Cst::dbl_max;
		double TSG_min = Cst::dbl_max;
		for (size_t jj=ii; jj>0; jj--) {
			if (ovec[jj].date<current-1.) break;
			const double TSS = ovec[ii](MeteoData::TSS);
			if (TSS!=IOUtils::nodata && TSS<TSS_min) TSS_min=TSS;
			
			const double TSG = ovec[ii](MeteoData::TSG);
			if (TSG!=IOUtils::nodata && TSG<TSG_min) TSG_min=TSG;
		}
		if (TSS_min!=Cst::dbl_max && TSS_min>IOUtils::C_TO_K(1.)) {
			tssWarmDay_idx = ii;
		} else if (TSG_min!=Cst::dbl_max && TSG_min>IOUtils::C_TO_K(1.)) {
			tsgWarmDay_idx = ii;
		}
		if (tssWarmDay_idx!=IOUtils::npos && tsgWarmDay_idx!=IOUtils::npos) return;
	}
}

//compute the TSS offset/correction, in Celsius
double FilterDeGrass::getTSSOffset(const unsigned int& param, const std::vector<MeteoData>& ivec) 
{
	Date tmp_prev_day;
	double HS_daily_median, TSS_daily_median, RSWR_daily_10pc;
	bool high_tss_day = false;
	std::vector<double> tss_dat;
	
	for (size_t ii=0; ii<ivec.size(); ii++){
		if (ivec[ii](param)==IOUtils::nodata) continue;

		const Date day_start( getDailyStart(ivec[ii].date) );
		if (day_start!=tmp_prev_day) {
			const bool status = getDailyParameters(ivec, day_start, HS_daily_median, TSS_daily_median, RSWR_daily_10pc);
			if (!status || HS_daily_median==IOUtils::nodata || TSS_daily_median==IOUtils::nodata || RSWR_daily_10pc==IOUtils::nodata) 
				continue;
			high_tss_day = (HS_daily_median>0.3) && (TSS_daily_median>IOUtils::C_TO_K(1.)) && (RSWR_daily_10pc>350.);
			tmp_prev_day = day_start;
		}
		
		if (high_tss_day) tss_dat.push_back( ivec[ii](MeteoData::TSS) );
	}
	
	return IOUtils::K_TO_C( Interpol1D::getMedian(tss_dat) );
}

//daily values for TSS offset calc 
bool FilterDeGrass::getDailyParameters(const std::vector<MeteoData>& ivec, const Date day_start, double &HS_daily_median, double &TSS_daily_median, double &RSWR_daily_10pc)
{
	const Date day_end( day_start + 1 );
	
	//extract values for the day
	std::vector<double> HS_dat, TSS_dat, RSWR_dat;
	for (size_t jj=0; jj<ivec.size(); jj++) {
		if (ivec[jj].date>=day_end) break;
		if (ivec[jj].date>=day_start) {
			HS_dat.push_back( ivec[jj](MeteoData::HS) );
			TSS_dat.push_back( ivec[jj](MeteoData::TSS) );
			RSWR_dat.push_back( ivec[jj](MeteoData::RSWR) );
		}
	}
	if (HS_dat.empty()) return false; //this happens at the beginning of the period
	
	std::vector<double> quantiles;
	quantiles.push_back( 0.9 );
	std::vector<double> rswr_quantiles( Interpol1D::quantiles(RSWR_dat, quantiles) );
	
	RSWR_daily_10pc = rswr_quantiles[0];
	HS_daily_median = Interpol1D::getMedian(HS_dat);
	TSS_daily_median = Interpol1D::getMedian(TSS_dat);
	return true;
}

//daily values for TSS-based correction
void FilterDeGrass::getTSSDailyPpt(const std::vector<MeteoData>& ivec, const Date day_start, double &o_TSS_daily_min, double &o_TSS_daily_max, double &o_TSS_daily_mean)
{
	const Date day_end( day_start + 1 );
	
	//extract values for the day
	std::vector<double> TSS_dat;
	for (size_t jj=0; jj<ivec.size(); jj++) {
		if (ivec[jj].date>=day_end) break;
		if (ivec[jj].date>=day_start) {
			TSS_dat.push_back( ivec[jj](MeteoData::TSS) );
		}
	}
	
	o_TSS_daily_min = Interpol1D::min_element(TSS_dat);
	o_TSS_daily_max = Interpol1D::max_element(TSS_dat);
	o_TSS_daily_mean = Interpol1D::arithmeticMean(TSS_dat);
}

//daily values for TSG-based correction
double FilterDeGrass::getDailyTSGVariance(const std::vector<MeteoData>& ivec, const Date day_start)
{
	const Date day_end( day_start + 1 );
	
	//extract values for the day
	std::vector<double> TSG_dat;
	for (size_t jj=0; jj<ivec.size(); jj++) {
		if (ivec[jj].date>=day_end) break;
		if (ivec[jj].date>=day_start) {
			TSG_dat.push_back( ivec[jj](MeteoData::TSG) );
		}
	}
	
	return Interpol1D::variance(TSG_dat);
}

/**
 * @brief For a given date, find the start of the day, considering that for midnight we return the day before!
 * (as is necessary for daily averages, sums, etc that can be provided at midnight for the day before)
 * @param resampling_date current date
 * @return start of the day or start of the day before in case of midnight
 */
Date FilterDeGrass::getDailyStart(const Date& resampling_date)
{
	Date Start( resampling_date );
	Start.rnd(24*3600, Date::DOWN);
	if (Start==resampling_date) //if resampling_date=midnight GMT, the rounding lands on the exact same date
		Start -= 1.;
	
	return Start;
}

} //end namespace
