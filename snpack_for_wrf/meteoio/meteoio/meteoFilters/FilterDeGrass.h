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
#ifndef FilterDeGrass_H
#define FilterDeGrass_H

#include <meteoio/meteoFilters/ProcessingBlock.h> //use this one for all others
#include <vector>
#include <string>

namespace mio {

/**
 * @class FilterDeGrass
 * @ingroup processing
 * @brief This filter is used to distinguish if snow (HS) is on the ground or not
 * @details This is useful because the
 * ultrasonic sensor cannot distinguish between snow or vegetation/grass on the ground. 
 * The filter is based on total snow depth (HS), snow surface temperature (TSS), ground surface temperature (TSG)
 * and reflected shortwave radiation (RSWR). 
 * Different steps to do: 
 *    -# calculate possible offset of TSS (reason: at some stations in some springs the TSS increases 
 * although snow is still on the ground) 
 *    -# calculate correlation of TSS and TSG in spring (normally both temperatures increase at the same 
 * time in spring which results in a high correlation; low correlation if TSS and TSG increase not parallel 
 * which leads in connection with a high offset of TSS to the assumption, that TSS is false), then:
 *        -# if TSS has a low offset and the correlation between TSS and TSG is high, the algorithm 
 * analyses based on the daily Max/Min/Mean of TSS if snow is on the ground or not 
 *        -# if no TSS is available or the offset of TSS is high/correlation of TSS and TSG is low, the 
 * algorithm analyses based on the variance of TSG and the value of TSG if snow is on the ground or 
 * not.
 * 
 * References/Literature: Tilg, A.-M., Marty C. and G. Klein, <i>"An automatic algorithm for validating snow 
 * depth measurements of IMIS stations"</i>, 2015. Swiss Geoscience Meeting 2015
 * 
 * Normally, the filter computes any potential offset on TSS before using it in its algorithms. But it is possible
 * to provide the offset that must be used, with the optional TSS_OFFSET argument (then TSS_meas + TSS_offset is compared to given thresholds to
 * determine if snow can exist on the ground, so you can give a negative offset to more easily keep snow on the ground). 
 * 
 * Example of use:
 * @code
 * HS::filter1	= DETECT_GRASS
 * @endcode
 * 
 *\note
 * - two of the used criterias are currently only valid for mid-northern latitudes (months when the season might start);
 * - it is probably a good idea to use a FilterRate filter after this one in order to remove some potential left-over peaks.
 *
 * @author Anna-Maria Tilg and Mathias Bavay
 * @date   2015-12-16
 */

class FilterDeGrass : public ProcessingBlock {
	public:
		FilterDeGrass(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);

	private:
		void filterOnTsg(const unsigned int& param, const size_t& ii, std::vector<MeteoData>& ovec);
		void filterOnTss(const unsigned int& param, const size_t& ii, const double& tss_offset, std::vector<MeteoData>& ovec);
		
		static double getTssTsgCorrelation(const std::vector<MeteoData>& ovec, const size_t& firstWarmDay_idx);
		static void findFirstWarmDay(const std::vector<MeteoData>& ovec, size_t &tssWarmDay_idx, size_t &tsgWarmDay_idx);
		static double getTSSOffset(const unsigned int& param, const std::vector<MeteoData>& ivec);
		static bool getDailyParameters(const std::vector<MeteoData>& ivec, const Date day_start, double &HS_daily_median, double &TSS_daily_median, double &RSWR_daily_10pc);
		static void getTSSDailyPpt(const std::vector<MeteoData>& ivec, const Date day_start, double &o_TSS_daily_min, double &o_TSS_daily_max, double &o_TSS_daily_mean);
		static double getDailyTSGVariance(const std::vector<MeteoData>& ivec, const Date day_start);
		static Date getDailyStart(const Date& resampling_date);
		
		Date prev_day;
		double TSS_daily_max, TSS_daily_min, TSS_daily_mean, TSG_daily_var;
		double TSS_user_offset; ///< The user can optionally provide his/her own TSS offset instead of relying on an automatic one
		int month;
};

} //end namespace

#endif
