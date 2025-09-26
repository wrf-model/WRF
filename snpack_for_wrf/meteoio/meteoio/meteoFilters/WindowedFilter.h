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
#ifndef WINDOWEDFILTER_H
#define WINDOWEDFILTER_H

#include <meteoio/meteoFilters/ProcessingBlock.h>
#include <vector>
#include <string>

namespace mio {

/**
 * @class  WindowedFilter
 * @brief
 * @author Thomas Egger
 * @date   2011-01-22
 */

class WindowedFilter : public ProcessingBlock {
	public:
		enum Centering {
			left,   ///< left centered window
			center, ///< centered window
			right   ///< right centered window
		};

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec) = 0;

	protected:
		WindowedFilter(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name, const bool& skipWindowParams=false);

		void setWindowFParams(const std::vector< std::pair<std::string, std::string> >& vecArgs);
		const std::vector<const MeteoData*>& get_window(const size_t& index,
		                                                const std::vector<MeteoData>& ivec);
		bool get_window_specs(const size_t& index, const std::vector<MeteoData>& ivec,
		                      size_t& start, size_t& end) const;

		Duration min_time_span; ///< This is filled in the constructor by calling setWindowFParams
		Centering centering; ///< This is filled in the constructor by calling setWindowFParams
		size_t min_data_points; ///< This is filled in the constructor by calling setWindowFParams
		size_t last_start, last_end;

	private:
		std::vector<const MeteoData*> vec_window;

	protected:
		bool is_soft; //placed here for alignement
};

} //end namespace

#endif
