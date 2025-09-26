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
#ifndef PROCESSINGBLOCK_H
#define PROCESSINGBLOCK_H

#include <meteoio/dataClasses/MeteoData.h>
#include <meteoio/Config.h>
#include <vector>
#include <string>
#include <set>
#include <map>

#ifdef _MSC_VER
	#pragma warning(disable:4512) //we don't need any = operator!
#endif

namespace mio {

class ProcessingProperties {
	public:
		typedef enum PROC_STAGE { none, ///< never activate this block
		                     first, ///< activate at first stage
		                     second, ///< activate at second stage
		                     both ///< activate at both first and second stage
		                     //once ///< activate at stage one or two, but only once
		                   } proc_stage;

		ProcessingProperties() : time_before(0., 0.), time_after(0., 0.),
		                         points_before(0), points_after(0),
		                         stage(first) {}

		ProcessingProperties(const Duration& t_before, const Duration& t_after, const size_t& pt_before, const size_t& pt_after, const proc_stage& i_stage)
		: time_before(t_before), time_after(t_after), points_before(pt_before), points_after(pt_after), stage(i_stage) {}

		const std::string toString() const;

		Duration time_before;
		Duration time_after;

		size_t points_before;
		size_t points_after;

		proc_stage stage;
};

/**
 * @class  ProcessingBlock
 * @brief  An abstract class
 * @author Thomas Egger
 * @date   2011-01-02
 */
class ProcessingBlock {
	public:
		typedef struct DATES_RANGE {
			DATES_RANGE() : start(), end() {}
			DATES_RANGE(const Date& d1, const Date& d2) : start(d1), end(d2) {}
			bool operator<(const DATES_RANGE& a) const { //needed for "sort"
				return start < a.start;
			}

			Date start, end;
		} dates_range;
		
		typedef struct OFFSET_SPEC {
			OFFSET_SPEC() : date(), offset(0.) {}
			OFFSET_SPEC(const Date& d1, const double& i_offset) : date(d1), offset(i_offset) {}
			bool operator<(const OFFSET_SPEC& a) const { //needed for "sort"
				return date < a.date;
			}

			Date date;
			double offset;
		} offset_spec;
		
		virtual ~ProcessingBlock() {}

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec) = 0;
		
		//this call is for some timefilters
		virtual void process(Date &dateStart, Date &dateEnd) {(void)dateStart; (void)dateEnd; throw InvalidArgumentException("Invalid call for this kind of filter", AT);}

		std::string getName() const {return block_name;}
		const ProcessingProperties& getProperties() const {return properties;}
		const std::string toString() const;
		bool skipStation(const std::string& station_id) const;
		bool noStationsRestrictions() const {return excluded_stations.empty() && kept_stations.empty();}

		static void readCorrections(const std::string& filter, const std::string& filename, std::vector<double> &X, std::vector<double> &Y);
		static void readCorrections(const std::string& filter, const std::string& filename, std::vector<double> &X, std::vector<double> &Y1, std::vector<double> &Y2);
		static std::vector<double> readCorrections(const std::string& filter, const std::string& filename, const size_t& col_idx, const char& c_type, const double& init);
		static std::vector<offset_spec> readCorrections(const std::string& filter, const std::string& filename, const double& TZ, const size_t& col_idx=2);
		static std::map< std::string, std::vector<dates_range> > readDates(const std::string& filter, const std::string& filename, const double& TZ);
		static std::set<std::string> initStationSet(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& keyword);

	protected:
		ProcessingBlock(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name); ///< protected constructor only to be called by children

		static void extract_dbl_vector(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                               std::vector<double>& ovec);
		static void extract_dbl_vector(const unsigned int& param, const std::vector<const MeteoData*>& ivec,
                                               std::vector<double>& ovec);
		
		const std::set<std::string> excluded_stations, kept_stations;
		ProcessingProperties properties;
		const std::string block_name;

		static const double soil_albedo, snow_albedo, snow_thresh; ///< parametrize the albedo from HS
};

class BlockFactory {
	public:
		static ProcessingBlock* getBlock(const std::string& blockname, const std::vector< std::pair<std::string, std::string> >& vecArgs, const Config& cfg);
		static ProcessingBlock* getTimeBlock(const std::string& blockname, const std::vector< std::pair<std::string, std::string> >& vecArgs, const Config& cfg);
};

} //end namespace

#endif
