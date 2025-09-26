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
#include <meteoio/meteoFilters/WindowedFilter.h>

#include <algorithm>

using namespace std;

namespace mio {

/**
 * @brief Construct a WindowedFilter Object. This is for filters that require a certain window of data.
 * @param[in] vecArgs Vector containing all the filter's arguments
 * @param[in] name Name of the filter (used to report errors)
 * @param[in] skipWindowParams if set to true, do NOT read and initialize the Window parameters (default: false) so they can be initialized later.
 */
WindowedFilter::WindowedFilter(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name, const bool& skipWindowParams)
               : ProcessingBlock(vecArgs, name), min_time_span(0.0, 0.), centering(WindowedFilter::center), min_data_points(1),
                 last_start(0), last_end(0), vec_window(), is_soft(false)
{
	if (!skipWindowParams) setWindowFParams( vecArgs );
}

/**
 * @brief Parse the arguments in order to retrieve the user parameters for the data window.
 * The following parameters are recognized:
 *  - CENTERING: the time-centering of the data window  can be either left, right or center;
 *  - MIN_PTS: minimum number of points that the window must contain;
 *  - MIN_SPAN: minimum time width of the data window (in seconds).
 *
 * @param[in] vecArgs Vector containing all the filter's arguments
 * NOTE: the "soft" argument is also processed.
 */
void WindowedFilter::setWindowFParams(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	const std::string where( "Filters::"+block_name );
	bool has_min_span = false, has_min_pts = false;

	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="CENTERING") {
			const std::string cntr_spec( IOUtils::strToUpper(vecArgs[ii].second) );
			if (cntr_spec=="LEFT")
				centering = WindowedFilter::left;
			else if (cntr_spec=="CENTER")
				centering = WindowedFilter::center;
			else if (cntr_spec=="RIGHT")
				centering = WindowedFilter::right;
			else
				throw InvalidArgumentException("Invalid window specification for "+where, AT);
		} else if (vecArgs[ii].first=="MIN_PTS") {
			IOUtils::parseArg(vecArgs[ii], where, min_data_points);
			has_min_pts = true;
		} else if (vecArgs[ii].first=="MIN_SPAN") {
			double min_span;
			IOUtils::parseArg(vecArgs[ii], where, min_span);
			min_time_span = Duration(min_span / 86400.0, 0.);
			has_min_span = true;
		} else if (vecArgs[ii].first=="SOFT") {
			IOUtils::parseArg(vecArgs[ii], where, is_soft);
		}
	}

	if (!has_min_span && !has_min_pts)
		throw InvalidArgumentException("Please provide a window width specification (MIN_PTS and MIN_SPAN) for "+where, AT);
}

/**
 * @brief A function that cuts out the desired window for the 'index' element within
 *        ivec, the window elements are stored into vec_window
 *        Calls to this function have to start with index 0, then 1, 2, 3, ...
 *        vec_window is not allowed to be changed between two calls
 * @param index The index of the element in ivec that requires a window
 * @param ivec The original sequence of data points
 */
const std::vector<const MeteoData*>& WindowedFilter::get_window(const size_t& index,
                                                                const std::vector<MeteoData>& ivec)
{
	if (index==0) {
		vec_window.clear();
		vec_window.reserve(min_data_points*2); //to have enough margin for the time criteria
	}

	size_t start, end;
	if (!get_window_specs(index, ivec, start, end)) {
		vec_window.clear();
		vec_window.push_back( &ivec[index] );
	}

	if (index==0) {
		for (size_t ii=start; ii<=end; ii++) vec_window.push_back( &ivec[ii] );
	} else {
		if (last_start<start) {
			vec_window.erase( vec_window.begin(),  vec_window.begin()+(start-last_start));
		}
		if (last_start>start) {
			const std::vector<const MeteoData*>::iterator it = vec_window.begin();
			for (size_t ii=start; ii<=last_start; ii++) vec_window.insert(it, &ivec[ii]);
		}
		if (last_end<end) {
			for (size_t ii=last_end+1; ii<=end; ii++) vec_window.push_back( &ivec[ii] );
		}
		if (last_end>end) {
			vec_window.erase( vec_window.end()-(last_end-end), vec_window.end() );
		}
	}

	//save window metadata
	last_start = start;
	last_end = end;
	return vec_window;
}

/**
 * @brief A function that computes the start and end for a window for the 'index' element from ivec
 * The principle is to compute the first index that matches the minimum number of points criteria,
 * and the one that matches the minimum time window, then combine them (with the equivalent of OR: we take the MIN index).
 * Afterward, we compute the last index [...] for number of points and the last index [...] for the time window
 * and combine them (with the equivalent of OR: we take the MIN index) or vice versa for right centering.
 * @param index The index of the element in ivec that requires a window
 * @param ivec The original sequence of data points
 * @param start the start index of the window
 * @param end the end index of the window
 * @return true if success, false if a window could not be computed
 */
bool WindowedFilter::get_window_specs(const size_t& index, const std::vector<MeteoData>& ivec, size_t& start, size_t& end) const
{
	const Date date = ivec[index].date;
	start = end = index; //for proper initial value, so we can bail out without worries

	if (centering == WindowedFilter::left) {
		//get start of window
		size_t start_elements = min_data_points - 1; //start elements criteria
		if (start_elements>index) {
			if (!is_soft) return false;
			start_elements = index; //as many as possible
		}
		const Date start_date = date - min_time_span;
		size_t start_time_idx = IOUtils::seek(start_date, ivec, false); //start time criteria
		if (start_time_idx!=IOUtils::npos) start_time_idx = (start_time_idx>0)? start_time_idx-1 : IOUtils::npos;
		if (start_time_idx==IOUtils::npos) {
			if (!is_soft) return false;
			start_time_idx = 0; //first possible element
		}
		const size_t elements_left = max(index - start_time_idx, start_elements);
		start = index - elements_left;

		//get end of window
		if (!is_soft) return true; //with end=index
		const size_t end_elements = (min_data_points>(elements_left+1))? min_data_points - (elements_left + 1) : 0;
		const Date end_date = ivec[start].date+min_time_span;
		size_t end_time_idx = (end_date>date)? IOUtils::seek(end_date, ivec, false) : index; //end time criteria
		if (end_time_idx==IOUtils::npos) {
			if (!is_soft) return false;
			end_time_idx = ivec.size()-1; //last possible element
		}
		const size_t elements_right = max(end_time_idx - index, end_elements);
		end = index + elements_right;
		if (end>=ivec.size()) return false; //no way to cut the requested window
	}

	if (centering == WindowedFilter::right) {
		//get end of window
		size_t end_elements = min_data_points - 1; //end elements criteria
		if (end_elements>(ivec.size()-1-index)) {
			if (!is_soft) return false;
			end_elements = (ivec.size()-1-index); //as many as possible
		}
		const Date end_date = date+min_time_span;
		size_t end_time_idx = IOUtils::seek(end_date, ivec, false); //end time criteria
		if (end_time_idx==IOUtils::npos) {
			if (!is_soft) return false;
			end_time_idx = ivec.size()-1; //last possible element
		}
		const size_t elements_right = max(end_time_idx - index, end_elements);
		end = index + elements_right;

		//get start of window
		if (!is_soft) return true; //with start=index
		const size_t start_elements = (min_data_points>(elements_right+1))? min_data_points - (elements_right + 1) : 0;
		const Date start_date = ivec[end].date-min_time_span;
		size_t start_time_idx = (start_date<date)?IOUtils::seek(start_date, ivec, false):index; //start time criteria
		if (start_time_idx!=IOUtils::npos && start_time_idx!=index) start_time_idx = (start_time_idx>0)? start_time_idx-1 : IOUtils::npos;
		if (start_time_idx==IOUtils::npos) {
			if (!is_soft) return false;
			end_time_idx = 0; //first possible element
		}
		const size_t elements_left = max(index - start_time_idx, start_elements);
		if (elements_left>index) return false; //no way to cut the requested window
		start = index - elements_left;
	}

	if (centering == WindowedFilter::center) {
		//get start of ideal window
		size_t start_elements = min_data_points/2; //start elements criteria
		if (start_elements>index) {
			if (!is_soft) return false;
			start_elements = index; //as many as possible
		}
		const Date start_date = date - min_time_span/2;
		size_t start_time_idx = IOUtils::seek(start_date, ivec, false); //start time criteria
		if (start_time_idx!=IOUtils::npos) start_time_idx = (start_time_idx>0)? start_time_idx-1 : IOUtils::npos;
		if (start_time_idx==IOUtils::npos) {
			if (!is_soft) return false;
			start_time_idx = 0; //first possible element
		}
		const size_t elements_left = max(index - start_time_idx, start_elements);
		start = index - elements_left; //we know that index>=elements_left

		//get end of ideal window
		size_t end_elements = min_data_points/2; //end elements criteria
		if (end_elements>(ivec.size()-1-index)) {
			if (!is_soft) return false;
			end_elements = (ivec.size()-1-index); //as many as possible
		}
		const Date end_date = date+min_time_span/2;
		size_t end_time_idx = IOUtils::seek(end_date, ivec, false); //end time criteria
		if (end_time_idx==IOUtils::npos) {
			if (!is_soft) return false;
			end_time_idx = ivec.size()-1; //last possible element
		}
		const size_t elements_right = max(end_time_idx - index, end_elements);
		end = index + elements_right;

		//now, check (and modify) if the window could not be centered
		if (elements_left==elements_right) return true;
		if (!is_soft) return false;
		if (elements_left<elements_right) { //we hit the left border
			//get again the end of window
			const size_t end_elems = (min_data_points>(elements_left+1))? min_data_points - (elements_left + 1) : 0;
			const Date end_dt = ivec[start].date+min_time_span;
			size_t end_tm_idx = (end_dt>date)? IOUtils::seek(end_dt, ivec, false) : index; //end time criteria
			if (end_tm_idx==IOUtils::npos) {
				end_tm_idx = ivec.size()-1; //last possible element
			}
			const size_t elems_right = max(end_tm_idx - index, end_elems);
			end = index + elems_right;
			if (end>ivec.size()) return false; //no way to cut the requested window
		} else { //we hit the right border
			//get again the start of window
			const size_t start_elems = (min_data_points>(elements_right+1))? min_data_points - (elements_right + 1) : 0;
			const Date start_dt = ivec[end].date-min_time_span;
			size_t start_tm_idx = (start_dt<date)? IOUtils::seek(start_dt, ivec, false) : index; //start time criteria
			if (start_tm_idx!=IOUtils::npos && start_tm_idx!=index) start_tm_idx = (start_tm_idx>0)? start_tm_idx-1 : IOUtils::npos;
			if (start_tm_idx==IOUtils::npos) {
				end_time_idx = 0; //first possible element
			}
			const size_t elems_left = max(index - start_tm_idx, start_elems);
			if (elems_left>index) return false; //no way to cut the requested window
			start = index - elems_left;
		}
	}

	return true;
}

} //namespace
