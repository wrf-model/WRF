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
#include <meteoio/meteoFilters/ProcIIR.h>
#include <meteoio/MathOptim.h>
#include <cmath>

using namespace std;

namespace mio {

ProcIIR::ProcIIR(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name)
                  : ProcessingBlock(vecArgs, name), cutoff(0.), g(0.), p(0.), c(0.), type(CRITICALLY_DAMPED), bidirectional(true), low_pass(true)
{
	parse_args(vecArgs);

	properties.points_before = 2;
	properties.stage = ProcessingProperties::first;
	getFilterParameters(type, low_pass, 2., g, p, c);
}

void ProcIIR::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
                        std::vector<MeteoData>& ovec)
{
	ovec = ivec;
	if (ivec.size()<2 || cutoff==0.) return;

	const double days = ivec.back().date.getJulian() - ivec.front().date.getJulian();
	const size_t nr_data_pts = ivec.size();
	const double sampling_rate = static_cast<double>(nr_data_pts-1) / (days*24.*3600.); //in Hz

	double A[3], B[3];
	computeCoefficients(sampling_rate, 1./cutoff, A, B);

	std::vector<double> X(3, IOUtils::nodata), Y(3, IOUtils::nodata);
	if (!bidirectional) {
		//only forward filter
		for (size_t ii=0; ii<ovec.size(); ++ii)
			ovec[ii](param) = filterPoint(ivec[ii](param), A, B, X, Y);
	} else { //bidirectional filtering
		//backward filter
		std::vector<double> vecTmp(nr_data_pts);
		for (size_t ii=ovec.size(); ii--> 0;)
			vecTmp[ii] = filterPoint(ivec[ii](param), A, B, X, Y);

		//forward filter
		std::fill(X.begin(), X.end(), IOUtils::nodata);
		for (size_t ii=0; ii<ovec.size(); ++ii)
			ovec[ii](param) =  filterPoint(vecTmp[ii], A, B, X, Y);
	}
}

void ProcIIR::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	const std::string where( "Filters::"+block_name );
	bool has_type=false, has_cutoff=false;

	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="SINGLE_PASS") {
			IOUtils::parseArg(vecArgs[ii], where, bidirectional);
		} else if (vecArgs[ii].first=="CUTOFF") {
			IOUtils::parseArg(vecArgs[ii], where, cutoff);
			has_cutoff = true;
		} else if (vecArgs[ii].first=="FREQ_RESPONSE") {
			const std::string frequency( vecArgs[ii].second );
			if (frequency=="LP") low_pass = true;
			else if (frequency=="HP") low_pass = false;
			else
				throw InvalidArgumentException("Invalid freq_response \""+vecArgs[ii].second+"\" for \""+where+"\"", AT);
			has_type = true;
		} else if (vecArgs[ii].first=="TYPE") {
			const std::string type_str( vecArgs[ii].second );
			if (type_str=="BUTTERWORTH") type = BUTTERWORTH;
			else if (type_str=="CRITICALLY_DAMPED") type = CRITICALLY_DAMPED;
			else if (type_str=="BESSEL") type = BESSEL;
			else
				throw InvalidArgumentException("Invalid type \""+vecArgs[ii].second+"\" for \""+where+"\"", AT);
		}
	}

	if (!has_type) throw InvalidArgumentException("Please provide a freq_response for "+where, AT);
	if (!has_cutoff) throw InvalidArgumentException("Please provide a cutoff period for "+where, AT);
}

double ProcIIR::filterPoint(const double& raw_val, const double A[3], const double B[3], std::vector<double> &X, std::vector<double> &Y)
{
	//propagate in X and Y
	X[2] = X[1]; X[1] = X[0]; X[0] = raw_val;
	Y[2] = Y[1]; Y[1] = Y[0]; Y[0] = raw_val; //Y[0] will be overwritten but in case of nodata we still propagate a value
	if (X[2]==IOUtils::nodata || X[1]==IOUtils::nodata || X[0]==IOUtils::nodata) return Y[0];
	if (Y[2]==IOUtils::nodata || Y[1]==IOUtils::nodata) return Y[0];

	Y[0] = A[0]*X[0] + A[1]*X[1] + A[2]*X[2] + B[1]*Y[1] + B[2]*Y[2];
	return Y[0];
}

//this computes the filter coefficients for a low pass filter.
//the filter parameters are computed based on the filter polynomial coefficients and the cutoff correction.
void ProcIIR::computeCoefficients(const double& fs, const double& f0, double A[3], double B[3]) const
{
	//using the filter polynomials, the number of passes and the cutoff correction, compute the filter coefficients
	const double f_star = (low_pass)? c*f0/fs : 0.5 - c*f0/fs; //corrected cutoff frequency
	const double w_0 = tan(Cst::PI*f_star); //warp cutoff frequency

	if ((low_pass && f_star>=0.25) || (!low_pass && f_star<=0.25)) {
		std::cerr << "[W] in the '" << getName() << "' filter, the chosen cutoff period is incompatible with the sampling rate (unstable behavior): ";
		std::cerr << "f* = " << f_star << " 1/f0 = " << 1./f0 << " 1/fs = " << 1./fs << "\n";
	}

	const double K1 = p * w_0;
	const double K2 = g * w_0*w_0;

	A[0] = K2 / (1. + K1 + K2);
	A[1] = 2. * A[0];
	A[2] = A[0];

	B[1] = 2*A[0] * (1./K2 - 1.);
	B[2] = 1. - (A[0] + A[1] + A[2] + B[1]);

	if (!low_pass) { //some signs are different for High Pass
		A[1] = -A[1];
		B[1] = -B[1];
	}
}

void ProcIIR::getFilterParameters(const IIR_Type& i_type, const bool& isLowPass, const double& n, double &i_g, double &i_p, double &i_c)
{
	if (i_type==BUTTERWORTH) {
		i_g = 1.;
		i_p = sqrt(2.);
		i_c = 1. / pow( pow(2, 1./n) - 1., 1./4. ); //3dB cutoff correction
	} else if (i_type==CRITICALLY_DAMPED) {
		i_g = 1.;
		i_p = 2.;
		i_c = sqrt( pow(2., 1./(2.*n)) - 1. ); //3dB cutoff correction
	} else if (i_type==BESSEL) {
		i_g = 3.;
		i_p = 3.;
		i_c = sqrt( sqrt(pow(2., 1/n) - 3./4.) - 0.5 ) / sqrt(3.); //3dB cutoff correction
	} else
		throw UnknownValueException("The requested IIR filter type has not been defined", AT);

	if (isLowPass) i_c = 1. / i_c;
}

}
