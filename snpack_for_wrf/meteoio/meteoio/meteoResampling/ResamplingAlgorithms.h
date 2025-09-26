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
#ifndef RESAMPLINGALGORITHMS_H
#define RESAMPLINGALGORITHMS_H

#include <meteoio/dataClasses/MeteoData.h>
#include <meteoio/IOUtils.h>

#include <string>
#include <vector>

#ifdef _MSC_VER
	#pragma warning(disable:4512) //we don't need any = operator!
#endif

namespace mio {
/**
 * @class ResamplingAlgorithms
 * @brief Interface class for the temporal resampling algorithms
 * @details
 * These models generate data points that are missing based on neighbouring points in a time series.
 *
 * @ingroup stats
 * @author Mathias Bavay - Thomas Egger
 * @date   2013-05-24
 */
class ResamplingAlgorithms {

	public:
		enum ResamplingPosition {
			exact_match,
			before,
			after,
			begin,
			end
		};

		ResamplingAlgorithms(const std::string& i_algoname, const std::string& i_parname, const double& dflt_window_size, const std::vector< std::pair<std::string, std::string> >& /*vecArgs*/)
		                    : algo(i_algoname), parname(i_parname), window_size(dflt_window_size), gaps() {}

		virtual ~ResamplingAlgorithms() {}

		const std::string getAlgo() const {return algo;}

		virtual void resample(const std::string& stationHash, const size_t& index, const ResamplingPosition& position, const size_t& paramindex,
		              const std::vector<MeteoData>& vecM, MeteoData& md) = 0;
		
		void resetResampling() {gaps.clear();} //invalidate all gaps, usually after rebuffering

		virtual std::string toString() const = 0;

 	protected:
		static double partialAccumulateAtLeft(const std::vector<MeteoData>& vecM, const size_t& paramindex,
		                                      const size_t& pos, const Date& curr_date);
		static double partialAccumulateAtRight(const std::vector<MeteoData>& vecM, const size_t& paramindex,
		                                       const size_t& pos, const Date& curr_date);
		void getNearestValidPts(const std::string& stationHash, const size_t& pos, const size_t& paramindex, const std::vector<MeteoData>& vecM, const Date& resampling_date,
		                               const double& i_window_size, size_t& indexP1, size_t& indexP2);
		static double linearInterpolation(const double& x1, const double& y1,
		                                  const double& x2, const double& y2, const double& x3);
		static Date getDailyStart(const Date& resampling_date);
		static size_t getDailyValue(const std::vector<MeteoData>& vecM, const size_t& paramindex, size_t pos, const Date& intervalStart, const Date& intervalEnd);

		const std::string algo, parname;
		double window_size;
		static const double soil_albedo, snow_albedo, snow_thresh; ///< These thresholds are used to handle solar radiation
	private:
		typedef struct GAP_INFO {
			GAP_INFO() : start(), end(), startIdx(IOUtils::npos), endIdx(IOUtils::npos) {}
			void extend(const size_t& idx, const std::vector<MeteoData>& vecM) {if (idx<startIdx) setStart(idx, vecM); if (idx>endIdx) setEnd(idx, vecM);}
			void setStart(const size_t& idx, const std::vector<MeteoData>& vecM) {if (idx>=vecM.size()) return; startIdx=idx; start=vecM[idx].date;}
			void setEnd(const size_t& idx, const std::vector<MeteoData>& vecM) {if (idx>=vecM.size()) return; endIdx=idx; end=vecM[idx].date;}
			std::string toString() const {std::ostringstream os; os << start.toString(Date::ISO) << " (" << startIdx << ") - " << end.toString(Date::ISO) << " (" << endIdx << ")"; return os.str();}
			Date start, end;
			size_t startIdx, endIdx;
		} gap_info;
		
		size_t searchBackward(gap_info &last_gap, const size_t& pos, const size_t& paramindex, const std::vector<MeteoData>& vecM, const Date& resampling_date,
                                              const double& i_window_size);
		size_t searchForward(gap_info &last_gap, const size_t& pos, const size_t& paramindex, const std::vector<MeteoData>& vecM, const Date& resampling_date,
                                              const double& i_window_size, const size_t& indexP1);
		std::map<std::string, gap_info> gaps;
};

class ResamplingAlgorithmsFactory {
	public:
		static ResamplingAlgorithms* getAlgorithm(const std::string& i_algoname, const std::string& parname, const double& window_size, const std::vector< std::pair<std::string, std::string> >& vecArgs);
};

} //end namespace
#endif
