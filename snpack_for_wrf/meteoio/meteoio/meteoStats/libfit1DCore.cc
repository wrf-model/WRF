/***********************************************************************************/
/*  Copyright 2011 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#include <meteoio/meteoStats/libfit1DCore.h>
#include <meteoio/meteoStats/libinterpol1D.h>
#include <meteoio/IOUtils.h>
#include <algorithm>
#include <cmath>
#include <iomanip>

using namespace std;

namespace mio {

std::vector<double> FitModel::getParams() const
{
	if (fit_ready!=true)
		throw InvalidArgumentException("The regression has not yet been computed!", AT);
	return Lambda;
}

void FitModel::setGuess(const std::vector<double>& lambda_in)
{
	const size_t nGuess = lambda_in.size();
	if (nGuess!=nParam) {
		ostringstream ss;
		ss << "Provided " << nGuess << " guesses for " << nParam << " parameters ";
		ss << "for regression model " << regname << "!";
		throw InvalidArgumentException(ss.str(), AT);
	}

	Lambda = lambda_in;
	fit_ready = true;
}

FitModel& FitModel::operator =(const FitModel& source)
{
	if (this != &source) {
		fit_ready = source.fit_ready;
		infoString = source.infoString;
		Lambda = source.Lambda;
		X = source.X;
		Y = source.Y;
	}
	return *this;
}

bool FitModel::checkInputs()
{
	nPts=X.size();

	if ( nPts!=Y.size() ) {
		ostringstream ss;
		ss << "X vector and Y vector don't match! " << nPts << "!=" << Y.size() << "\n";
		throw InvalidArgumentException(ss.str(), AT);
	}

	if (nPts<min_nb_pts) {
		ostringstream ss;
		ss << "Only " << nPts << " data points for " << regname << " regression model.";
		ss << " Expecting at least " << min_nb_pts << " for this model!\n";
		infoString = ss.str();
		return false;
	}

	return true;
}


std::string FitModel::toString() const
{
	std::ostringstream os;
	os << "<FitModel>\n";
	os << regname << " model with " << nParam << " parameters (nr_data_points >= " << min_nb_pts << ")\n";
	if (!X.empty() && !Y.empty()) {
		const double Xmin = *std::min_element(X.begin(),X.end());
		const double Xmax = *std::max_element(X.begin(),X.end());
		const double Ymin = *std::min_element(Y.begin(),Y.end());
		const double Ymax = *std::max_element(Y.begin(),Y.end());
		os << nPts << " calibration point(s) in\t";
		os << "X[" << Xmin << " - " << Xmax << "]\tY[" << Ymin << " - " << Ymax << "]\n";
	} else {
		os << "0 calibration points provided\n";
	}

	if (!Lambda.empty()) {
		os << "Model parameters:       \t";
		for (size_t ii=0; ii<Lambda.size(); ii++) os << Lambda[ii] << " ";
		os << "\n";
	}
	os << "</FitModel>\n";

	return os.str();
}

////////////////////////////////////////////////////////////
//// Least square fit class
const double FitLeastSquare::lambda_init = 1.; //initial default guess
const double FitLeastSquare::delta_init_abs = 1.; //initial delta, absolute
const double FitLeastSquare::delta_init_rel = 0.2; //initial delta, relative
const double FitLeastSquare::eps_conv = 1e-6; //convergence criteria
const unsigned int FitLeastSquare::max_iter = 50; //maximum number of iterations

void FitLeastSquare::setData(const std::vector<double>& in_X, const std::vector<double>& in_Y)
{
	X = in_X;
	Y = in_Y;
	Interpol1D::sort(X, Y); //sort the data by increasing X
	setDefaultGuess();
	fit_ready = false;
}

void FitLeastSquare::setDefaultGuess()
{
	for (size_t i=0; i<nParam; i++) {
		Lambda.push_back( lambda_init );
	}
}

bool FitLeastSquare::fit()
{
	if (!checkInputs()) return false;
	return computeFit();
}

////////////////////////////////////////////////////////////
//// End of public methods

//see http://mathworld.wolfram.com/NonlinearLeastSquaresFitting.html
bool FitLeastSquare::computeFit()
{
	double max_delta;
	initLambda();
	Matrix dLambda; //parameters variations
	initDLambda(dLambda);

	Matrix A(nPts, nParam);
	Matrix dBeta(nPts, (size_t)1);

	unsigned int iter = 0;
	do {
		iter++;
		//set dBeta matrix
		for (size_t m=1; m<=nPts; m++) {
			dBeta(m,1) = Y[m-1] - f(X[m-1]); //X and Y are vectors
		}

		//set A matrix
		for (size_t m=1; m<=nPts; m++) {
			for (size_t n=1; n<=nParam; n++) {
				const double value = DDer( X[m-1], n ); //X is a vector
				A(m,n) = value;
			}
		}

		//calculate parameters deltas
		const Matrix a( A.getT() * A );
		const Matrix b( A.getT() * dBeta );
		if (!Matrix::solve(a, b, dLambda)) return false;

		//apply the deltas to the parameters, record maximum delta
		max_delta = 0.;
		for (size_t n=1; n<=nParam; n++) {
			Lambda[n-1] += dLambda(n,1); //Lambda is a vector
			if ( fabs(dLambda(n,1))>max_delta ) max_delta=fabs(dLambda(n,1));
		}

	} while (max_delta>eps_conv && iter<max_iter);

	//compute R2
	const double R2 = Matrix::dot(dBeta, dBeta);

	//building infoString
	ostringstream ss;
	ss << "Computed regression with " << regname << " model ";
	ss << "- Sum of square residuals = " << std::setprecision(2) << R2 << " , max_delta = " << max_delta << " ";
	ss << "- " << iter << " iterations";
	infoString = ss.str();

	if (max_delta>eps_conv) { //it did not converge
		fit_ready = false;
		return false;
	} else {		 //it did converge
		fit_ready = true;
		return true;
	}
}

void FitLeastSquare::initLambda()
{
	if (Lambda.empty()) //else, setGuess has been called
		Lambda.resize(nParam, lambda_init);
}

void FitLeastSquare::initDLambda(Matrix& dLambda) const
{
	dLambda.resize(nParam,1);
	for (size_t m=1; m<=nParam; m++) {
		const double var = Lambda[m-1]; //Lambda is a vector
		if (var==0) {
			dLambda(m,1) = delta_init_abs * 0.5;
		} else {
			dLambda(m,1) = delta_init_rel * var * 0.5;
		}
	}
}

double FitLeastSquare::getDelta(const double& var) const
{
//calculate a sensible delta for the partial derivative
	if (var==0) {
		return (delta_init_abs * 0.5);
	} else {
		return (delta_init_rel * var * 0.5);
	}
}

double FitLeastSquare::DDer(const double& x, const size_t& index)
{
	const double var = Lambda[index-1]; //Lambda is a vector
	const double delta = getDelta(var);
	const double v1 = var - delta;
	const double v2 = var + delta;

	Lambda[index-1] = v1;
	const double y1 = f(x);
	Lambda[index-1] = v2;
	const double y2 = f(x);

	Lambda[index-1] = var; //restore initial value

	return (y2-y1)/(2.*delta);
}

} //namespace
