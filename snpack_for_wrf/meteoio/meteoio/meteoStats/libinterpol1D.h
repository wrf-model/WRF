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
#ifndef LIBINTERPOL1D_H
#define LIBINTERPOL1D_H

#include <vector>
#include <string>

namespace mio {

/**
 * @class Interpol1D
 * @brief A class to perform basic 1D statistics.
 * Each method is static
 *
 * @ingroup stats
 * @author Mathias Bavay
 * @date   2009-01-20
 */
class Interpol1D {
 	public:
		static double min_element(const std::vector<double>& X);
		static double max_element(const std::vector<double>& X);
		static std::vector<double> quantiles(const std::vector<double>& X, const std::vector<double>& quartiles);
		static std::vector<double> quantiles_core(std::vector<double> X, const std::vector<double>& quartiles);
		static std::vector<double> derivative(const std::vector<double>& X, const std::vector<double>& Y);
		static void sort(std::vector<double>& X, std::vector<double>& Y, const bool& keep_nodata=true);
		static void equalBin(const unsigned int k, std::vector<double> &X, std::vector<double> &Y);
		static void equalCountBin(const unsigned int k, std::vector<double> &X, std::vector<double> &Y);
		static double weightedMean(const double& d1, const double& d2, const double& weight=1.);
		static double weightedMean(const std::vector<double>& vecData, const std::vector<double>& weight);
		static double arithmeticMean(const std::vector<double>& vecData);
		static double getMedian(const std::vector<double>& vecData, const bool& keep_nodata=true);
		static double getMedianAverageDeviation(std::vector<double> vecData, const bool& keep_nodata=true);
		static double variance(const std::vector<double>& X);
		static double std_dev(const std::vector<double>& X);
		static double covariance(const std::vector<double>& z1, const std::vector<double>& z2);
		static double corr(const std::vector<double>& z1, const std::vector<double>& z2);
		static double R2(const std::vector<double>& obs, const std::vector<double>& sim);
		static double NashSuttcliffe(const std::vector<double>& obs, const std::vector<double>& sim);
		static double getBoxMuller();

		static void LinRegression(const std::vector<double>& X, const std::vector<double>& Y, double& a, double& b, double& r, std::string& mesg, const bool& fixed_rate=false);
		static void NoisyLinRegression(const std::vector<double>& in_X, const std::vector<double>& in_Y, double& A, double& B, double& R, std::string& mesg, const bool& fixed_rate=false);
		static void twoLinRegression(const std::vector<double>& in_X, const std::vector<double>& in_Y, const double& bilin_inflection, std::vector<double>& coeffs);
		static void LogRegression(const std::vector<double>& X, const std::vector<double>& Y, double& a, double& b, double& r, std::string& mesg);
		static void ExpRegression(const std::vector<double>& X, const std::vector<double>& Y, double& a, double& b, double& r, std::string& mesg);

	private:
		static double getMedianCore(std::vector<double> vecData);
		static bool ptOK(const double& x, const double& y);
		static void LinRegressionFixedRate(const std::vector<double>& X, const std::vector<double>& Y, double& a, double& b, double& r, std::string& mesg);
		static bool pair_comparator(const std::pair<double, double>& l, const std::pair<double, double>& r);
		static double pt_line_distance(const double& x, const double& y, const double& a, const double& b);
};

} //end namespace

#endif
