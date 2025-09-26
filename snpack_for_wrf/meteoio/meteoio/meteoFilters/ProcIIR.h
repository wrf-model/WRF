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
#ifndef PROCIIR_H
#define PROCIIR_H

#include <meteoio/meteoFilters/ProcessingBlock.h>
#include <vector>
#include <string>

namespace mio {

/**
 * @class  ProcIIR
 * @ingroup processing
 * @brief Infinite Impulse Response (IIR) filter.
 * @details
 * This filter can either be used as a low pass or high pass filter. It is based on a Critically Damped, 2 poles filter (considering that
 * it is better avoid overshooting even at the cost of a gentler falloff). It takes the following arguments:
 *  - FREQ_RESPONSE: frequency response, either LP (for *Low Pass*) or HP (for *High Pass*);
 *  - CUTOFF: The cutoff <b>period</b> (defined as the frequency at a -3dB gain) given in seconds;
 *  - TYPE: either CRITICALLY_DAMPED (default), BUTTERWORTH or BESSEL (see figure below);
 *  - SINGLE_PASS: Normally, the phase is removed by bidirectional filtering, ie. running the filter twice,
 * first backward and then forward (this also squares the amplitude response). If set to TRUE, this bidirectional filtering
 * is disabled.
 *
 * \image html IIR_filter.png "Infinite Impulse Response filter: step response, LP bidirectional filtering over 24 hours"
 * \image latex IIR_filter.eps "Infinite Impulse Response filter: step response, LP bidirectional filtering over 24 hours" width=0.9\textwidth
 *
 * @code
 * HS::filter1      = IIR
 * HS::arg1::freq_response   = LP
 * HS::arg1::type   = CRITICALLY_DAMPED
 * HS::arg1::cutoff = 10800 ;ie. 3 hours
 * @endcode
 *
 * To know more: http://unicorn.us.com/trading/allpolefilters.html and http://www.dspguide.com/ch19/4.htm.
 */

class ProcIIR : public ProcessingBlock {
	public:
		ProcIIR(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);

	private:
		typedef enum IIR_TYPE {
			BUTTERWORTH,
			CRITICALLY_DAMPED,
			BESSEL
		} IIR_Type;

		static void getFilterParameters(const IIR_Type& i_type, const bool& isLowPass, const double& n, double &i_g, double &i_p, double &i_c);
		static double filterPoint(const double& raw_val, const double A[3], const double B[3], std::vector<double> &X, std::vector<double> &Y);
		void computeCoefficients(const double& fs, const double& f0, double A[3], double B[3]) const;

		void parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs);

		double cutoff;
		double g, p, c; ///< filter definition: number of passes, polynomial coefficients, 3dB cutoff correction
		IIR_Type type;
		bool bidirectional, low_pass;
};

} //end namespace

#endif
