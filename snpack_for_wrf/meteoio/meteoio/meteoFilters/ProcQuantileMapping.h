/***********************************************************************************/
/*  Copyright 2018 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef PROCQUANTILEMAPPING_H
#define PROCQUANTILEMAPPING_H

#include <meteoio/meteoFilters/ProcessingBlock.h>
#include <vector>
#include <string>

namespace mio {

/**
 * @class  ProcQuantileMapping
 * @ingroup processing
 * @brief Quantile Mapping correction
 * @details The statistical distribution of the chosen parameter is computed and the multiplying factors provided as arguments
 * are used to correct each provided quantiles (see https://rcmes.jpl.nasa.gov/content/statistical-downscaling#download).The 
 * quantiles must be provided as increasing decimal numbers between 0 (0%) and 1 (100%) and the full range must be covered. 
 * Values given for quantiles less than 0.5 (50%) are assumed to extend toward 0 while values above are assumed to extend toward 1 (100%).
 * 
 * It takes the following arguments:
 *  - TYPE: either ADD (add the correction coefficient) or MULT (multiply by the correction coefficient);
 *  - PERIOD: either YEARLY, MONTHLY, WEEKLY or DAILY. This describes the period over which the quantiles were calculated (and is currently 
 * used as a sliding window). If no argument is given, it takes the whole dataset at once (optional);
 *  - CORRECTIONS: the file and path containing the corrections to apply;
 *  - WRITE_QUANTILES: write out the average value of each quantile (if no corrections file is provided, it will generate 10 quantiles).
 *
 * This example write out the yearly deciles without performing any corrections to the data (so the corrections factor can be later computed):
 * @code
 * VW::filter1    = QM
 * VW::arg1::period = yearly
 * VW::arg1::write_quantiles = true
 * @endcode
 * 
 * This example applies pre-computed correction factors (quantiles computed over the whole dataset):
 * @code
 * TA::filter1    = QM
 * TA::arg1::corrections = correctionsTA.dat
 * TA::arg1::type = add
 * @endcode
 *
 * Example of correction file (quantiles in the first column and correction factors as second column):
 * @code
 * 0 1.2
 * 0.25 1.2
 * 0.5 1.05
 * 0.8 0.95
 * 0.9 0.89
 * @endcode
 */

class ProcQuantileMapping : public ProcessingBlock {
	public:
		ProcQuantileMapping(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name, const std::string& i_root_path);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);

	protected:
		void correctPeriod(const unsigned int& param, const size_t& idx_start, const size_t& idx_end, const std::vector<MeteoData>& ivec, std::vector<MeteoData>& ovec) const;
		void writeQuantiles(const std::vector<double>& thresholds, const std::vector<double>& vecX, const std::string& parameter, const std::string& station) const;
		double getCorrection(const std::vector<double>& thresholds, const double& value) const;
		static size_t getQuantile(const std::vector<double>& thresholds, const double& value);
		std::vector< std::pair<size_t, size_t> > getStarts(const std::vector<MeteoData>& ivec) const;
		void parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs);

		std::vector<double> quantiles, corrections;
		std::string root_path;
		double period_duration;
		char type;
		bool write_quantiles;
};

} //end namespace

#endif
