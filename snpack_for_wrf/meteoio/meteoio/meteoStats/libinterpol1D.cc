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
#include <meteoio/meteoStats/libinterpol1D.h>
#include <meteoio/IOExceptions.h>
#include <meteoio/IOUtils.h>
#include <meteoio/MathOptim.h>
#include <meteoio/meteoLaws/Meteoconst.h>

#include <algorithm>
#include <numeric>
#include <cmath>
#include <iomanip>

using namespace std;

namespace mio {

double Interpol1D::min_element(const std::vector<double>& X)
{
	double Xmin = Cst::dbl_max;
	for (size_t ii=0; ii<X.size(); ii++) {
		if (X[ii]==IOUtils::nodata) continue;
		if (X[ii]<Xmin) Xmin=X[ii];
	}
	if (Xmin==Cst::dbl_max) return IOUtils::nodata;

	return Xmin;
}

double Interpol1D::max_element(const std::vector<double>& X)
{
	double Xmax = -Cst::dbl_max;
	for (size_t ii=0; ii<X.size(); ii++) {
		if (X[ii]==IOUtils::nodata) continue;
		if (X[ii]>Xmax) Xmax=X[ii];
	}
	if (Xmax==-Cst::dbl_max) return IOUtils::nodata;

	return Xmax;
}

/**
 * @brief This function returns a vector of quantiles.
 * The vector does not have to be sorted. See https://secure.wikimedia.org/wikipedia/en/wiki/Quartile for more.
 * This code is heavily inspired by Ken Wilder, https://sites.google.com/site/jivsoft/Home/compute-ranks-of-elements-in-a-c---array-or-vector
 * (quantile method, replacing the nth-element call by direct access to a sorted vector).
 * @param X vector to classify
 * @param quartiles vector of quartiles, between 0 and 1
 * @return vector of ordinates of the quantiles
 */
std::vector<double> Interpol1D::quantiles(const std::vector<double>& X, const std::vector<double>& quartiles)
{
	const size_t Xsize = X.size();
	if (Xsize == 0)
		throw NoDataException("Trying to calculate quantiles with no data points", AT);

	//in order to properly escape nodata points, we need to copy in a temporary vector
	std::vector<double> vecTemp;
	for (size_t i=0; i<Xsize; i++) {
		const double value=X[i];
		if (value!=IOUtils::nodata) vecTemp.push_back(value);
	}

	return quantiles_core(vecTemp, quartiles);
}

/**
 * @brief This function returns a vector of quantiles, but does not filter out nodata values!
 * The vector does not have to be sorted. See https://secure.wikimedia.org/wikipedia/en/wiki/Quartile for more.
 * This code is heavily inspired by Ken Wilder, https://sites.google.com/site/jivsoft/Home/compute-ranks-of-elements-in-a-c---array-or-vector
 * (quantile method, replacing the nth-element call by direct access to a sorted vector).
 * @param X vector to classify (nodata values processed as normal values)
 * @param quartiles vector of quartiles, between 0 and 1
 * @return vector of ordinates of the quantiles
 */
std::vector<double> Interpol1D::quantiles_core(std::vector<double> X, const std::vector<double>& quartiles)
{
	const size_t Qsize = quartiles.size();
	if (Qsize == 0)
		throw NoDataException("No quantiles specified", AT);

	//we will store results in a new vector
	std::vector<double> vecResults(Qsize, IOUtils::nodata);

	const size_t vecSize = X.size();
	if (vecSize == 0) return vecResults; //ie: nodata values
	if (vecSize == 1) {
		std::fill(vecResults.begin(), vecResults.end(), X[0]);
		return vecResults;
	}

	//compute quantiles
	std::sort( X.begin(), X.end()); //since we will process several values, we sort the vector
	for (size_t ii=0; ii<Qsize; ii++) {
		const double q = quartiles[ii];
		if (q<=0.) vecResults[ii] = X.front();
		else if (q>=1.) vecResults[ii] = X.back();
		else {
			const double pos = static_cast<double>(vecSize - 1) * q;
			const size_t ind = static_cast<size_t>(pos);
			const double delta = pos - static_cast<double>(ind);
			const double i1 = X[ind];
			const double i2 = X[ind+1];
			vecResults[ii] = i1 * (1.0 - delta) + i2 * delta;
		}
	}
	return vecResults;
}

//small helper function for checking if a point can be used when computing a vector derivative
inline bool Interpol1D::ptOK(const double& x, const double& y) {
	return (x!=IOUtils::nodata && y!=IOUtils::nodata);
}

/**
 * @brief This function returns the vector of local derivatives, given a vector of abscissae and ordinates.
 * The vectors must be sorted by ascending x. The derivatives will be centered if possible, left or right otherwise or nodata
 * if nothing else can be computed.
 * @param X vector of abscissae
 * @param Y vector of ordinates
 * @return vector of local derivatives
 */
std::vector<double> Interpol1D::derivative(const std::vector<double>& X, const std::vector<double>& Y)
{
	const size_t n = X.size();
	if (n!=Y.size()) {
		ostringstream ss;
		ss << "X vector and Y vector don't match! " << n << "!=" << Y.size() << "\n";
		throw InvalidArgumentException(ss.str(), AT);
	}
	if (n<2) {
		ostringstream ss;
		ss << "X and Y vector only contain " << n << "points, it is not possible to compute a derivative!\n";
		throw InvalidArgumentException(ss.str(), AT);
	}

	std::vector<double> der(n, IOUtils::nodata);
	//right hand derivative
	{
		const double x=X[0], x1=X[1];
		const double y=Y[0], y1=Y[1];
		const double Dx_r=x1-x;
		if (ptOK(x,y) && ptOK(x1,y1) && Dx_r!=0.)
			der[0] = (y1-y) / Dx_r;
	}

	//centered derivative if possible
	for (size_t i=1; i<(n-1); i++) {
		const double x0=X[i-1], x=X[i], x1=X[i+1];
		const double y0=Y[i-1], y=Y[i], y1=Y[i+1];
		const double Dx_r=x1-x, Dx_c=x1-x0, Dx_l=x-x0;

		const double right = (ptOK(x,y) && ptOK(x1,y1) && Dx_r!=0.)? (y1-y) / Dx_r : IOUtils::nodata;
		const double left = (ptOK(x,y) && ptOK(x0,y0) && Dx_l!=0.)? (y-y0) / Dx_l : IOUtils::nodata;
		const double centered = (ptOK(x0,y0) && ptOK(x1,y1) && Dx_c!=0.)? (y1-y0) / Dx_c : IOUtils::nodata;

		if (centered!=IOUtils::nodata) der[i] = centered;
		else if (right!=IOUtils::nodata) der[i] = right;
		else if (left!=IOUtils::nodata) der[i] = left;
	}

	//left hand derivative
	{
		const size_t last = n-1;
		const double x0=X[last-1], x=X[last];
		const double y0=Y[last-1], y=Y[last];
		const double Dx_l=x-x0;
		if (ptOK(x,y) && ptOK(x0,y0) && Dx_l!=0.)
			der[last] = (y-y0) / Dx_l;
	}

	return der;
}

/**
 * @brief data binning method
 * This bins the data into k classes of equal width (see https://en.wikipedia.org/wiki/Data_binning)
 * @param k number of classes
 * @param X vector of abscissae
 * @param Y vector of ordinates
 */
void Interpol1D::equalBin(const unsigned int k, std::vector<double> &X, std::vector<double> &Y)
{
	const size_t Xsize = X.size();
	if (Xsize!=Y.size()) {
		ostringstream ss;
		ss << "X vector and Y vector don't match! " << Xsize << "!=" << Y.size() << "\n";
		throw InvalidArgumentException(ss.str(), AT);
	}
	if (k>=Xsize || Xsize<2) return;

	const double Xmin = min_element(X);
	const double Xmax = max_element(X);
	if (Xmin==IOUtils::nodata || Xmax==IOUtils::nodata) return;
	const double width = (Xmax - Xmin) / k;

	std::vector<double> bins(k, 0.);
	std::vector<size_t> counts(k, 0);
	for (size_t ii=0; ii<Xsize; ii++) {
		if (X[ii]==IOUtils::nodata || Y[ii]==IOUtils::nodata) continue;

		const size_t index = (X[ii]!=Xmax)? static_cast<size_t>(Optim::floor( (X[ii]-Xmin) / width )) : static_cast<size_t>(k-1);
		bins[index] += Y[ii];
		counts[index]++;
	}

	X.clear();
	Y.clear();
	for (size_t ii=0; ii<k; ii++) {
		if (counts[ii]>0) {
			X.push_back( static_cast<double>(ii)*width + Xmin + .5*width);
			Y.push_back( bins[ii] / static_cast<double>(counts[ii]) );
		}
	}
}

/**
 * @brief data binning method
 * This bins the data into k classes of equal number of elements (see https://en.wikipedia.org/wiki/Data_binning).
 * The number of elements per classes is adjusted in order to reduce unevenness between casses: for example
 * when distributing 100 elements in 8 classes, this will generate 4 classes of 13 elements and 4 classes of 12 elements.
 * @param k number of classes
 * @param X vector of abscissae
 * @param Y vector of ordinates
 */
void Interpol1D::equalCountBin(const unsigned int k, std::vector<double> &X, std::vector<double> &Y)
{
	if (X.size()!=Y.size()) {
		ostringstream ss;
		ss << "X vector and Y vector don't match! " << X.size() << "!=" << Y.size() << "\n";
		throw InvalidArgumentException(ss.str(), AT);
	}
	if (k==0) throw InvalidArgumentException("It is not possible to bin into 0 classes!", AT);

	sort(X, Y, false); //also remove nodata points
	const size_t Xsize = X.size();
	if (k>=Xsize || Xsize==0) return;

	const size_t count_per_class = Xsize/k;
	const size_t remainder = Xsize % k;

	double sum_X=0, sum_Y=0.;
	size_t count=0, class_count=1;
	std::vector<double> X_bin, Y_bin;
	for (size_t ii=0; ii<Xsize; ii++) {
		sum_X += X[ii];
		sum_Y += Y[ii];
		count++;

		//the first classes receive count_per_class+1 point
		if ((class_count<=remainder && count>count_per_class) ||
		    (class_count>remainder && count>=count_per_class)) {
			X_bin.push_back( sum_X/static_cast<double>(count) );
			Y_bin.push_back( sum_Y/static_cast<double>(count) );
			sum_X=0.;
			sum_Y=0.;
			count=0;
			class_count++;
		}
	}
	if (count>0) { //close last class
		X_bin.push_back( sum_X/static_cast<double>(count) );
		Y_bin.push_back( sum_Y/static_cast<double>(count) );
	}

	X = X_bin;
	Y = Y_bin;
}

inline bool Interpol1D::pair_comparator(const std::pair<double, double>& l, const std::pair<double, double>& r) {
	return l.first < r.first;
}

/**
 * @brief This function sorts the X and Y vectors by increasing X.
 * The nodata values (both in X and Y) are removed, meaning that the vector length might not
 * be kept.
 * @param X vector of abscissae
 * @param Y vector of ordinates
 * @param keep_nodata should nodata values be kept? (default=true)
 */
void Interpol1D::sort(std::vector<double>& X, std::vector<double>& Y, const bool& keep_nodata)
{
	const size_t Xsize = X.size();
	if (Xsize!=Y.size()) {
		ostringstream ss;
		ss << "X vector and Y vector don't match! " << Xsize << "!=" << Y.size() << "\n";
		throw InvalidArgumentException(ss.str(), AT);
	}
	if (Xsize==0) return;

	std::vector< std::pair<double,double> > new_vec;
	for (size_t i=0; i<Xsize; i++) {
		if ( !keep_nodata && (X[i]==IOUtils::nodata || Y[i]==IOUtils::nodata) ) continue;
		const std::pair<double,double> tmp(X[i],Y[i]);
		new_vec.push_back( tmp );
	}

	std::sort( new_vec.begin(), new_vec.end(), pair_comparator );

	const size_t newSize = new_vec.size();
	X.resize( newSize );
	Y.resize( newSize );
	for (size_t i=0; i<newSize; i++) {
		X[i] = new_vec[i].first;
		Y[i] = new_vec[i].second;
	}
}

/**
 * @brief This function returns the weighted arithmetic mean of two numbers.
 * A weight of 0 returns d1, a weight of 1 returns d2, a weight of 0.5 returns a centered mean.
 * See https://secure.wikimedia.org/wikipedia/en/wiki/Weighted_mean for more...
 * @param d1 first value
 * @param d2 second value
 * @param weight weight to apply to the mean
 * @return weighted arithmetic mean
 */
double Interpol1D::weightedMean(const double& d1, const double& d2, const double& weight)
{
	const double tmp = abs(d1 - d2);
	if (d1 < d2) {
		return (d1 + tmp*weight);
	} else {
		return (d1 - tmp*weight);
	}
}

/**
 * @brief This function returns the weighted arithmetic mean of a vector.
 * See https://secure.wikimedia.org/wikipedia/en/wiki/Weighted_mean for more...
 * @param vecData vector of values
 * @param weight weights to apply to the mean
 * @return weighted arithmetic mean
 */
double Interpol1D::weightedMean(const std::vector<double>& vecData, const std::vector<double>& weight)
{
	const size_t nPts = vecData.size();
	if (nPts != weight.size()) {
		std::ostringstream ss;
		ss << "Computing weighted mean of a vector of size " << nPts;
		ss << " with vector of weights of size " << weight.size();
		throw InvalidArgumentException(ss.str(), AT);
	}

	double sum = 0., count = 0.;
	for (size_t ii=0; ii<nPts; ii++){
		const double value = vecData[ii];
		if (value!=IOUtils::nodata) {
			const double w = weight[ii];
			sum += value*w;
			count += w;
		}
	}

	if (count>0.)
		return (sum/count);
	else
		return IOUtils::nodata;
}

double Interpol1D::arithmeticMean(const std::vector<double>& vecData)
{
	const size_t nPts = vecData.size();

	unsigned int count=0;
	double sum = 0.0;
	for (size_t ii=0; ii<nPts; ii++){
		const double value = vecData[ii];
		if (value!=IOUtils::nodata) {
			sum += value;
			count++;
		}
	}

	if (count>0)
		return (sum/(double)count);
	else
		return IOUtils::nodata;
}

double Interpol1D::getMedianCore(std::vector<double> vecData)
{
//This uses a sorting algorithm for getting middle element
//as much more efficient than full sorting (O(n) compared to O(n log(n))
	const size_t vecSize = vecData.size();
	if (vecSize == 0)
		return IOUtils::nodata;

	if ((vecSize % 2) == 1){ //uneven
		const int middle = (int)(vecSize/2);
		nth_element(vecData.begin(), vecData.begin()+middle, vecData.end());
		return *(vecData.begin()+middle);
	} else { //use arithmetic mean of element n/2 and n/2-1
		const int middle = (int)(vecSize/2);
		nth_element(vecData.begin(), vecData.begin()+middle-1, vecData.end());
		const double m1 = *(vecData.begin()+middle-1);
		nth_element(vecData.begin(), vecData.begin()+middle, vecData.end());
		const double m2 = *(vecData.begin()+middle);
		return weightedMean( m1, m2, 0.5);
	}
}


double Interpol1D::getMedian(const std::vector<double>& vecData, const bool& keep_nodata)
{
//This uses a sorting algorithm for getting middle element
//as much more efficient than full sorting (O(n) compared to O(n log(n))
	if (keep_nodata) {
		if (vecData.empty()) return IOUtils::nodata;

		vector<double> vecTemp;
		for (size_t i=0; i<vecData.size(); i++) {
			const double value = vecData[i];
			if (value!=IOUtils::nodata)
				vecTemp.push_back(value);
		}

		return getMedianCore(vecTemp);
	} else {
		return getMedianCore(vecData);
	}
}

double Interpol1D::getMedianAverageDeviation(std::vector<double> vecData, const bool& keep_nodata)
{
	if (vecData.empty()) return IOUtils::nodata;

	const double median = Interpol1D::getMedian(vecData, keep_nodata);
	if (median==IOUtils::nodata)
		return IOUtils::nodata;

	//Calculate vector of deviations and write each value back into the vecData
	for (size_t ii=0; ii<vecData.size(); ii++){
		double& value = vecData[ii];
		if (value!=IOUtils::nodata)
			value = std::abs(value - median);
	}

	//Calculate the median of the deviations
	const double mad = Interpol1D::getMedian(vecData, keep_nodata);

	return mad;
}

/**
 * @brief Compute the variance of a vector of data
 * It is computed using a compensated variance algorithm,
 * (see https://secure.wikimedia.org/wikipedia/en/wiki/Algorithms_for_calculating_variance)
 * in order to be more robust to small variations around the mean.
 * @param X vector of data
 * @return variance or IOUtils::nodata
 */
double Interpol1D::variance(const std::vector<double>& X)
{
	const size_t n = X.size();
	size_t count=0;
	double sum=0.;

	for (size_t i=0; i<n; i++) {
		const double value = X[i];
		if (value!=IOUtils::nodata) {
			sum += value;
			count++;
		}
	}

	if (count<=1) return IOUtils::nodata;

	const double mean = sum/(double)count;
	double sum2=0., sum3=0.;
	for (size_t i=0; i<n; i++) {
		const double value = X[i];
		if (value!=IOUtils::nodata) {
			const double delta = value - mean;
			sum2 += delta*delta;
			sum3 += delta;
		}
	}
	const double variance = (sum2 - sum3*sum3/static_cast<double>(count)) / static_cast<double>(count - 1);
	return variance;
}

double Interpol1D::std_dev(const std::vector<double>& X) {
	const double var = variance(X);
	if (var==IOUtils::nodata) return IOUtils::nodata;
	return sqrt(var);
}

double Interpol1D::covariance(const std::vector<double>& X, const std::vector<double>& Y)
{//this is a simple but still compensated covariance computation (see the notes on the variance)
	const size_t Xsize = X.size();
	if (Xsize!=Y.size())
		throw IOException("Vectors should have the same size for covariance!", AT);
	if (Xsize==0) return IOUtils::nodata;

	const double X_mean = Interpol1D::arithmeticMean(X);
	const double Y_mean = Interpol1D::arithmeticMean(Y);
	if (X_mean==IOUtils::nodata || Y_mean==IOUtils::nodata) return IOUtils::nodata;

	size_t count=0;
	double sum=0.;
	for (size_t i=0; i<Xsize; i++) {
		if (X[i]!=IOUtils::nodata && Y[i]!=IOUtils::nodata) {
			sum += (X[i] - X_mean) * (Y[i] - Y_mean);
			count++;
		}
	}
	if (count<=1) return IOUtils::nodata;
	return sum/((double)count-1.);
}

/**
* @brief Computes the Pearson product-moment correlation coefficient
* This should be equivalent to the default R "corr" method.
* @param X first vector of data
* @param Y second vector of data
* @return correlation coefficient
*/
double Interpol1D::corr(const std::vector<double>& X, const std::vector<double>& Y)
{
	const double sigma_x = std_dev(X);
	const double sigma_y = std_dev(Y);
	
	if (sigma_x==IOUtils::nodata || sigma_y==IOUtils::nodata) return IOUtils::nodata;
	if (sigma_x==0. || sigma_y==0.) return IOUtils::nodata;
	
	const double cov = covariance(X, Y);
	if (cov==IOUtils::nodata) return IOUtils::nodata;
	
	return ( cov / (sigma_x*sigma_y));
}

/**
* @brief Computes the R2 coefficient of determination
* See https://en.wikipedia.org/wiki/Coefficient_of_determination and https://en.wikipedia.org/wiki/Fraction_of_variance_unexplained
* @param obs vector of observed data
* @param sim vector of simulated data
* @return coefficient of determination
*/
double Interpol1D::R2(const std::vector<double>& obs, const std::vector<double>& sim)
{
	const size_t n = obs.size();
	if (n!=sim.size())
		throw IOException("Vectors should have the same size for the R2 coefficient of determination!", AT);
	if (n==0) return IOUtils::nodata;

	//no special processing for the nodata values
	const double ObsMean = arithmeticMean( obs );
	if (ObsMean==IOUtils::nodata) return IOUtils::nodata;

	double ss_err = 0., ss_tot = 0.;
	for (size_t ii=0; ii<n; ii++) {
		if (obs[ii]==IOUtils::nodata) continue;
		ss_err += Optim::pow2( obs[ii] - sim[ii] );
		ss_tot += Optim::pow2( obs[ii] - ObsMean );
	}

	if (ss_tot==0. && ss_err==0) return 1.;
	if (ss_tot==0.) return IOUtils::nodata; //this can happen if the vector is filled with constant values
	return 1. - ss_err / ss_tot;
}

/**
* @brief Computes the Nash-Suttcliffe correlation coefficient for two vectors
* It is assumed that the same indices contain the same timesteps. A value of 1
* means a perfect match, a value of zero that no variance is reproduced (see https://en.wikipedia.org/wiki/Nash%E2%80%93Sutcliffe_model_efficiency_coefficient)
* @param obs vector of observed data
* @param sim vector of simulated data
* @return Nash-Suttcliffe correlation coefficient, between ]-∞, 1]
*/
double Interpol1D::NashSuttcliffe(const std::vector<double>& obs, const std::vector<double>& sim)
{
	const size_t n = obs.size();
	if (n!=sim.size())
		throw IOException("Vectors should have the same size for Nash-Suttcliffe!", AT);
	if (n==0) return IOUtils::nodata;

	//no special processing is performed on nodata values
	const double mean =  std::accumulate(obs.begin(), obs.end(), 0.0) / static_cast<double>(n);

	//now compute the numerator and denominator
	double denominator = 0., numerator = 0.;
	for (size_t ii=0; ii<n; ii++) {
		numerator += Optim::pow2(obs[ii] - sim[ii]);
		denominator += Optim::pow2(obs[ii] - mean);
	}

	if (denominator==0.) return IOUtils::nodata; //this can happen if the vector is filled with constant values
	return 1. - numerator / denominator;
}

/**
* @brief Box–Muller method for normally distributed random numbers.
* @details This generate a normally distributed signal of mean=0 and std_dev=1.
* For numerical reasons, the extremes will always be less than 7 * std_dev from the mean.
* See https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform
* @note Do not forget to initialize the (pseudo) random number generator! Something like "srand( static_cast<unsigned int>(time(NULL)) );"
* @return normally distributed number
*/
double Interpol1D::getBoxMuller()
{
	const double U = static_cast<double>(rand())/(RAND_MAX);
	const double V = static_cast<double>(rand())/(RAND_MAX);
	return  Optim::fastSqrt_Q3(-2.*log(U)) * cos(2.*Cst::PI*V);
}

/**
* @brief Computes the distance between a point (x,y) and a line y=ax+b
* @param x x coordinates of the point
* @param y y coordinates of the point
* @param a slope of the line
* @param c origin of the line
* @return distance of the point to the line
*/
double Interpol1D::pt_line_distance(const double& x, const double& y, const double& a, const double& c) {
	if (a==0.) return abs(y-c); //horizontal line

	//for ax+by+c=0; for us, b=-1
	static const double b = -1.;
	const double d = abs(a*x +b*y + c) * Optim::invSqrt( a*a + b*b );
	return d;
}

/**
* @brief Computes the linear regression coefficients fitting the points given as X and Y in two vectors
* the linear regression has the form Y = aX + b with a regression coefficient r (it is nodata safe)
* @param X vector of X coordinates
* @param Y vector of Y coordinates (same order as X)
* @param a slope of the linear regression
* @param b origin of the linear regression
* @param r absolute value of linear regression coefficient
* @param mesg information message if something fishy is detected
* @param fixed_rate force the lapse rate? (default=false)
*/
void Interpol1D::LinRegression(const std::vector<double>& X, const std::vector<double>& Y, double& a, double& b, double& r, std::string& mesg, const bool& fixed_rate)
{
	if (fixed_rate) {
		LinRegressionFixedRate(X, Y, a, b, r, mesg);
		return;
	}

	//check arguments
	const size_t n = X.size();
	if (n<2)
		throw NoDataException("Trying to calculate linear regression with too few data points", AT);
	if (n!=Y.size())
		throw IOException("Vectors should have the same size for linear regression!", AT);

	//computing x_avg and y_avg
	double x_avg=0., y_avg=0.;
	size_t count=0;
	for (size_t ii=0; ii<n; ii++) {
		if (X[ii]!=IOUtils::nodata && Y[ii]!=IOUtils::nodata) {
			x_avg += X[ii];
			y_avg += Y[ii];
			count++;
		}
	}
	if (count<2)
		throw NoDataException("Trying to calculate linear regression with too few valid data points", AT);
	x_avg /= (double)count;
	y_avg /= (double)count;

	//computing sx, sy, sxy
	double sx=0., sy=0., sxy=0.;
	for (size_t ii=0; ii<n; ii++) {
		if (X[ii]!=IOUtils::nodata && Y[ii]!=IOUtils::nodata) {
			sx += (X[ii]-x_avg) * (X[ii]-x_avg);
			sy += (Y[ii]-y_avg) * (Y[ii]-y_avg);
			sxy += (X[ii]-x_avg) * (Y[ii]-y_avg);
		}
	}

	//computing the regression line
	static const double epsilon = 1e-6;
	if (sx <= abs(x_avg)*epsilon) { //sx and sy are always positive
		//all points have same X -> we return a constant value that is the average
		a = 0.;
		b = y_avg;
		r = 1.;
		mesg = "[W] Computing linear regression on data at identical X\n";
		return;
	}
	a = sxy / sx;
	b = y_avg - a*x_avg;
	if (sy==0) {
		//horizontal line: all y's are equals
		r = 1.;
	} else {
		//any other line
		r = abs( sxy / sqrt(sx*sy) );
	}
}

/**
* @brief Computes the linear regression coefficients fitting the points given as X and Y in two vectors
* the linear regression has the form Y = aX + b with a regression coefficient r (it is nodata safe) while forcing the value of a
* @param X vector of X coordinates
* @param Y vector of Y coordinates (same order as X)
* @param a slope of the linear regression (forced)
* @param b origin of the linear regression
* @param r absolute value of linear regression coefficient
* @param mesg information message if something fishy is detected
*/
void Interpol1D::LinRegressionFixedRate(const std::vector<double>& X, const std::vector<double>& Y, double& a, double& b, double& r, std::string& mesg)
{	//check arguments
	const size_t n = X.size();
	if (n==0)
		throw NoDataException("Trying to calculate linear regression with no data points", AT);
	if (n!=Y.size())
		throw IOException("Vectors should have the same size for linear regression!", AT);

	//computing x_avg and y_avg
	int count=0;
	double x_avg=0, y_avg=0;
	for (size_t ii=0; ii<n; ii++) {
		if (X[ii]!=IOUtils::nodata && Y[ii]!=IOUtils::nodata) {
			x_avg += X[ii];
			y_avg += Y[ii];
			count++;
		}
	}
	if (count==0)
		throw NoDataException("Trying to calculate linear regression with no valid data points", AT);
	x_avg /= (double)count;
	y_avg /= (double)count;

	//computing the regression line
	b = y_avg - a*x_avg;

	double TSS=0, SSR=0; //Total Sum of Squares and Sum of Squared Residuals
	for (size_t ii=0; ii<n; ii++) {
		if (X[ii]!=IOUtils::nodata && Y[ii]!=IOUtils::nodata) {
			SSR += Optim::pow2( Y[ii] - (a*X[ii]+b) );
			TSS += Optim::pow2( Y[ii] );
		}
	}
	if (TSS!=0) {
		r = 1. - SSR/TSS;
	} else {
		r = 1.; //when all Y[i]=0 we automatically pick up the best possible fit. But r does not mean anything...
		mesg = "[W] Computing fixed lapse rate linear regression on data all at Y=0\n";
	}
}

/**
* @brief Computes the linear regression coefficients fitting the points given as X and Y in two vectors
* the linear regression has the form Y = aX + b with a regression coefficient r. If the regression coefficient is not good enough, tries to remove bad points (up to 15% of the initial data set can be removed, keeping at least 4 points)
* @param in_X vector of X coordinates
* @param in_Y vector of Y coordinates (same order as X)
* @param A slope of the linear regression
* @param B origin of the linear regression
* @param R linear regression coefficient
* @param mesg information message if something fishy is detected
* @param fixed_rate force the lapse rate? (default=false)
*/
void Interpol1D::NoisyLinRegression(const std::vector<double>& in_X, const std::vector<double>& in_Y, double& A, double& B, double& R, std::string& mesg, const bool& fixed_rate)
{
	//finds the linear regression for points (x,y,z,Value)
	static const double r_thres = 0.7;
	const size_t nb_pts = in_X.size();
	//we want at least 4 points AND 75% of the initial data set kept in the regression
	const size_t min_dataset = (size_t)Optim::floor( 0.75*(double)nb_pts );
	const size_t min_pts = (min_dataset>4)? min_dataset : 4;

	std::ostringstream ss;

	LinRegression(in_X, in_Y, A, B, R, mesg, fixed_rate);
	if (R>=r_thres) return;
	ss << mesg;

	std::vector<double> X(in_X), Y(in_Y);
	size_t nb_valid_pts = nb_pts;

	while (R<r_thres && nb_valid_pts>min_pts) {
		//we try to remove the one point in the data set that is the worst
		size_t index_bad=0;
		double max_dist = -1.;
		for (size_t ii=0; ii<nb_pts; ii++) {
			if (Y[ii]==IOUtils::nodata) continue;
			const double dist = pt_line_distance(X[ii], Y[ii], A, B);
			if (dist>max_dist) {
				max_dist = dist;
				index_bad = ii;
			}
		}
		//the worst point has been found, we remove it
		Y[index_bad] = IOUtils::nodata;
		nb_valid_pts--;
		LinRegression(X, Y, A, B, R, mesg, fixed_rate);
		ss << mesg;
	}

	//check if r is reasonable
	if (R<r_thres) {
		ss << "\n[W] Poor regression coefficient: " << std::setprecision(2) << R << "\n";
	}
	mesg = ss.str();
}

/**
* @brief Computes the bi-linear regression coefficients fitting the points given as X and Y in two vectors
* We consider that the regression can be made with 2 linear segments with a fixed inflection point. It relies on Interpol1D::NoisyLinRegression.
* @param in_X vector of X coordinates
* @param in_Y vector of Y coordinates (same order as X)
* @param bilin_inflection inflection point absissa
* @param coeffs a,b,r coefficients in a vector
*/
void Interpol1D::twoLinRegression(const std::vector<double>& in_X, const std::vector<double>& in_Y, const double& bilin_inflection, std::vector<double>& coeffs)
{
	//build segments
	std::vector<double> X1, Y1, X2, Y2;
	for (size_t ii=0; ii<in_X.size(); ii++) {
		if (in_X[ii]<bilin_inflection) { //first segment
			X1.push_back( in_X[ii] );
			Y1.push_back( in_Y.at(ii) );
		} else if (in_X[ii]>bilin_inflection) { //second segment
			X2.push_back( in_X[ii] );
			Y2.push_back( in_Y.at(ii) );
		} else { //point belongs to both segments
			X1.push_back( in_X[ii] );
			Y1.push_back( in_Y.at(ii) );
			X2.push_back( in_X[ii] );
			Y2.push_back( in_Y.at(ii) );
		}
	}

	//first segment
	double a1, b1, r1;
	std::string mesg1;
	NoisyLinRegression(X1, Y1, a1, b1, r1, mesg1);

	//second segment
	double a2, b2, r2;
	std::string mesg2;
	NoisyLinRegression(X2, Y2, a2, b2, r2, mesg2);

	coeffs.push_back(a1); coeffs.push_back(b1);
	coeffs.push_back(a2); coeffs.push_back(b2);
}

/**
* @brief Computes the Log regression coefficients fitting the points given as X and Y in two vectors
* the log regression has the form Y = a*ln(X) + b with a regression coefficient r (it is nodata safe)
* @param X vector of X coordinates
* @param Y vector of Y coordinates (same order as X)
* @param a slope of the regression
* @param b origin of the regression
* @param r regression coefficient
* @param mesg information message if something fishy is detected
*/
void Interpol1D::LogRegression(const std::vector<double>& X, const std::vector<double>& Y, double& a, double& b, double& r, std::string& mesg)
{
	std::vector<double> x( X.size() );

	for (size_t i=0; i<X.size(); i++) {
		const double val = X[i];
		x[i] = (val!=IOUtils::nodata)? log(val) : IOUtils::nodata;
	}

	LinRegression(x, Y, a, b, r, mesg); //HACK: how should we transform r?
}

/**
* @brief Computes the power regression coefficients fitting the points given as X and Y in two vectors
* the power regression has the form Y = b*X^a with a regression coefficient r (it is nodata safe)
* @param X vector of X coordinates
* @param Y vector of Y coordinates (same order as X)
* @param a slope of the regression
* @param b origin of the regression
* @param r regression coefficient
* @param mesg information message if something fishy is detected
*/
void Interpol1D::ExpRegression(const std::vector<double>& X, const std::vector<double>& Y, double& a, double& b, double& r, std::string& mesg)
{
	std::vector<double> y( Y.size() );

	for (size_t i=0; i<Y.size(); i++) {
		const double val = Y[i];
		y[i] = (val!=IOUtils::nodata)? log(val) : IOUtils::nodata;
	}

	LinRegression(X, y, a, b, r, mesg); //HACK: how should we transform r?
}

} //namespace
