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
#include <meteoio/meteoStats/libfit1D.h>
#include <meteoio/meteoStats/libinterpol1D.h>
#include <meteoio/IOExceptions.h>
#include <meteoio/MathOptim.h>

#include <cmath>
#include <algorithm>
#include <iomanip>

using namespace std;

namespace mio {

Fit1D::Fit1D(const regression& regType, const std::vector<double>& in_X, const std::vector<double>& in_Y, const bool& updatefit) : model(NULL)
{
	const bool status = setModel(regType, in_X, in_Y, updatefit);
	if (updatefit && status==false)
		throw NoDataException("The provided data was insufficient when constructing the regression model '"+model->getName()+"'", AT);
}

Fit1D::Fit1D(const std::string& regType, const std::vector<double>& in_X, const std::vector<double>& in_Y, const bool& updatefit) : model(NULL)
{
	const bool status = setModel(regType, in_X, in_Y, updatefit);
	if (updatefit && status==false)
		throw NoDataException("The provided data was insufficient when constructing the regression model '"+model->getName()+"'", AT);
}

Fit1D::Fit1D(const Fit1D& i_fit) : model(NULL) {
	*this = i_fit;
}

Fit1D& Fit1D::operator=(const Fit1D& source) {
	if (this != &source) {
		model = new SimpleLinear; //this is only for memory allocation
		*model = *(source.model); //copy what is pointed to
	}
	return *this;
}

bool Fit1D::setModel(const std::string& i_regType, const std::vector<double>& in_X, const std::vector<double>& in_Y, const bool& updatefit)
{
	regression regType;
	if (i_regType=="ZERO") regType=ZERO;
	else if (i_regType=="SIMPLE_LINEAR") regType=SIMPLE_LINEAR;
	else if (i_regType=="NOISY_LINEAR") regType=NOISY_LINEAR;
	else if (i_regType=="LINVARIO") regType=LINVARIO;
	else if (i_regType=="EXPVARIO") regType=EXPVARIO;
	else if (i_regType=="SPHERICVARIO") regType=SPHERICVARIO;
	else if (i_regType=="RATQUADVARIO") regType=RATQUADVARIO;
	else if (i_regType=="LINEARLS") regType=LINEARLS;
	else if (i_regType=="QUADRATIC") regType=QUADRATIC;
	else if (i_regType=="POLYNOMIAL") regType=POLYNOMIAL;
	else {
		throw IOException("The regression algorithm '"+i_regType+"' is not implemented" , AT);
	}

	return setModel(regType, in_X, in_Y, updatefit);
}

bool Fit1D::setModel(const regression& regType, const std::vector<double>& in_X, const std::vector<double>& in_Y, const bool& updatefit)
{//HACK: if model is already the right type, do not delete but reset
	if (model!=NULL) delete model;

	if (regType==ZERO) model=new Zero;
	if (regType==SIMPLE_LINEAR) model=new SimpleLinear;
	if (regType==NOISY_LINEAR) model=new NoisyLinear;
	if (regType==LINVARIO) model=new LinVario;
	if (regType==EXPVARIO) model=new ExpVario;
	if (regType==SPHERICVARIO) model=new SphericVario;
	if (regType==RATQUADVARIO) model=new RatQuadVario;
	if (regType==LINEARLS) model=new LinearLS;
	if (regType==QUADRATIC) model=new Quadratic;
	if (regType==POLYNOMIAL) model=new PolynomialRegression;

	//remove nodata points
	std::vector<double> X, Y;
	for (size_t ii=0; ii<in_X.size(); ii++) {
		if (in_X[ii]!=IOUtils::nodata && in_Y[ii]!=IOUtils::nodata) {
			X.push_back( in_X[ii] );
			Y.push_back( in_Y[ii] );
		}
	}

	model->setData(X, Y);
	if (updatefit)
		return fit();
	else
		return true;
}

//////////////////////////////////////////////////////////////
// regression models
void SimpleLinear::setData(const std::vector<double>& in_X, const std::vector<double>& in_Y) {
	X = in_X;
	Y = in_Y;

	fit_ready = false;
}

double SimpleLinear::f(const double& x) const {
	return Lambda.at(0)*x + Lambda.at(1);
}

bool SimpleLinear::fit()
{
	if (!checkInputs())
		return false;

	Lambda.clear();
	double a,b,r;
	std::string mesg;
	std::ostringstream ss;

	if (fixed_lapse_rate==IOUtils::nodata) {
		Interpol1D::LinRegression(X, Y, a, b, r, mesg);
		ss << mesg << "Computed regression with " << regname << " model - r=" << std::setprecision(2) << r;
	} else {
		a = fixed_lapse_rate;
		if (a!=0.) {
			Interpol1D::LinRegression(X, Y, a, b, r, mesg, true);
			ss << mesg << "Computed regression with " << regname << " model ";
			ss << "(fixed lapse rate=" << a << ") - r=" << std::setprecision(2) << r;
		} else {
			a=0.;
			b=0.;
			ss << mesg << "Computed regression with " << regname << " model";
		}
	}
	Lambda.push_back(a);
	Lambda.push_back(b);
	infoString = ss.str();
	fit_ready = true;
	return true;
}

bool NoisyLinear::fit()
{
	if (!checkInputs()) return false;

	Lambda.clear();
	double a,b,r;
	std::string mesg;
	std::ostringstream ss;

	if (fixed_lapse_rate==IOUtils::nodata) {
		Interpol1D::NoisyLinRegression(X, Y, a, b, r, mesg);
		ss << mesg  << "Computed regression with " << regname << " model - r=" << std::setprecision(2) << r;
	} else {
		a = fixed_lapse_rate;
		if (a!=0.) {
			Interpol1D::NoisyLinRegression(X, Y, a, b, r, mesg, true);
			ss << mesg  << "Computed regression with " << regname << " model ";
			ss << "(fixed lapse rate=" << a << ") - r=" << std::setprecision(2) << r;
		} else {
			a=0.;
			b=0.;
			ss << mesg << "Computed regression with " << regname << " model";
		}
	}
	Lambda.push_back(a);
	Lambda.push_back(b);
	infoString = ss.str();
	fit_ready = true;
	return true;
}

void PolynomialRegression::setData(const std::vector<double>& in_X, const std::vector<double>& in_Y)
{
	X = in_X;
	Y = in_Y;

	fit_ready = false;
}

double PolynomialRegression::f(const double& x) const
{
	double fittedValue = Lambda.at(0);
	for (size_t ii=1; ii<=degree; ii++){
		fittedValue = fittedValue + Lambda.at(ii)*pow(x,(double)ii);
	}
	return fittedValue;
}

bool PolynomialRegression::fit()
{
	if (!checkInputs())
		return false;

	const size_t N = X.size();
	const size_t n = degree;

	// Xh = vector that stores values of sigma(xi^2n)
	std::vector<double> Xh(2*n + 1);
	for (size_t i = 0; i < (2*n + 1); i++){
		Xh[i] = 0;
		for (size_t j = 0; j < N; j++){
			Xh[i] += pow(X[j], (double)i);
		}
	}

	// BM = normal augmented matrix that stores the equations.
	Matrix BM(n+1,n+1);
	for (size_t i = 0; i <= n; i++){
		for (size_t j = 0; j <= n; j++){
			BM(j+1,i+1)=Xh[i+j];
		}
	}

	// YM = vector to store values of sigma(xi^n * yi)
	Matrix YM(n+1,(size_t)1);
	for (size_t i = 0; i < (n+1); i++){
		YM(i+1,1) = 0;
		for (size_t j = 0; j < N; j++){
			YM(i+1,1) += pow(X[j],(double)i)*Y[j];
		}
	}

	//bM = vector to store the resulting coefficients
	Matrix bM(n+1,(size_t)1);
	if (!Matrix::solve(BM,YM,bM))
		return false;

	//set the coefficients to Lambda
	Lambda.resize(n+1);
	for (size_t i = 0; i < (n+1); i++){
		Lambda[i] = bM(i+1,1);
	}
	fit_ready = true;
	return true;
}

//regression models using the standard least square algorithm
double SphericVario::f(const double& x) const {
	if (x==0) return 0;
	//c0>=0, cs>=0, as>=0
	const double c0 = Lambda.at(0);
	const double cs = Lambda.at(1);
	const double as = Lambda.at(2);

	const double abs_x = fabs(x);
	if (abs_x>0 && abs_x<=as) {
		const double val = abs_x/as;
		const double y = c0 + cs * ( 1.5*val - 0.5*Optim::pow3(val) );
		return y;
	} else {
		return (c0+cs);
	}
}

void SphericVario::setDefaultGuess() {
	Lambda.push_back( *min_element(Y.begin(), Y.end()) );
	Lambda.push_back( *max_element(Y.begin(), Y.end()) );
	Lambda.push_back( *max_element(X.begin(), X.end()) );
}

double LinVario::f(const double& x) const {
	if (x==0) {
		return 0;
	} else {
		//c0>=0, b1>=0
		const double c0 = Lambda.at(0);
		const double bl = Lambda.at(1);
		const double y = c0 + bl * abs(x);
		return y;
	}
}

void LinVario::setDefaultGuess() {
	double xzero=X[0];
	size_t xzero_idx=0;
	for (size_t i=1; i<X.size(); i++) {
		if (abs(X[i])<xzero) { xzero=X[i]; xzero_idx=i;}
	}
	const double slope = Interpol1D::arithmeticMean( Interpol1D::derivative(X, Y) );
	Lambda.push_back( Y[xzero_idx] );
	Lambda.push_back( slope );
}

double ExpVario::f(const double& x) const {
	if (x==0) {
		return 0;
	} else {
		//c0>=0, ce>=0, ae>=0
		const double c0 = Lambda.at(0);
		const double ce = Lambda.at(1);
		const double ae = Lambda.at(2);
		const double y = c0 + ce * (1. - exp(-abs(x)/ae) );
		return y;
	}
}

void ExpVario::setDefaultGuess() {
	double xzero=X[0];
	size_t xzero_idx=0;
	for (size_t i=1; i<X.size(); i++) {
		if (abs(X[i])<xzero) { xzero=X[i]; xzero_idx=i;}
	}
	Lambda.push_back( Y[xzero_idx] );
	Lambda.push_back( Y.back() - Y[xzero_idx] );
	Lambda.push_back( 1. );
}

double RatQuadVario::f(const double& x) const {
	if (x==0) {
		return 0;
	} else {
		//c0>=0, cr>=0, ar>=0
		const double c0 = Lambda.at(0);
		const double cr = Lambda.at(1);
		const double ar = Lambda.at(2);
		const double y = c0 + cr*x*x / (1. + x*x/ar);
		return y;
	}
}

void RatQuadVario::setDefaultGuess() {
	double xzero=X[0];
	size_t xzero_idx=0;
	for (size_t i=1; i<X.size(); i++) {
		if (abs(X[i])<xzero) { xzero=X[i]; xzero_idx=i;}
	}
	Lambda.push_back( Y[xzero_idx] );
	Lambda.push_back( *( std::max_element( Y.begin(), Y.end() ) ) );
	Lambda.push_back( 1. );
}

double LinearLS::f(const double& x) const {
	const double y = Lambda.at(0)*x + Lambda.at(1); //Lambda is a vector
	return y;
}

void LinearLS::setDefaultGuess() {
	double xzero=X[0];
	size_t xzero_idx=0;
	for (size_t i=1; i<X.size(); i++) {
		if (abs(X[i])<xzero) { xzero=X[i]; xzero_idx=i;}
	}

	const double slope = Interpol1D::arithmeticMean( Interpol1D::derivative(X, Y) );
	Lambda.push_back( slope );
	Lambda.push_back( Y[xzero_idx] );
}

double Quadratic::f(const double& x) const {
	const double y = Lambda.at(0)*x*x + Lambda.at(1)*x + Lambda.at(2); //Lambda is a vector
	return y;
}

void Quadratic::setDefaultGuess() {
	const std::vector<double> der( Interpol1D::derivative(X, Y) );
	const double acc = 0.5 * Interpol1D::arithmeticMean( Interpol1D::derivative(X, der) );
	double xzero=der[0];
	size_t xzero_idx=0;
	for (size_t i=1; i<der.size(); i++) {
		if (abs(der[i])<xzero) { xzero=der[i]; xzero_idx=i;}
	}

	Lambda.push_back( acc ); //0
	Lambda.push_back( der[xzero_idx] ); //1
	if (acc>0.)
		Lambda.push_back( *( std::min_element( Y.begin(), Y.end() ) ) );
	else
		Lambda.push_back( *( std::max_element( Y.begin(), Y.end() ) ) );
}


/**
* @brief Add one data point to the model
* @details Before running the fit, it needs to be provided the data it will be fitted against. This is done by calling
* multiple times this method, each time for a new data point.
*
* @param[in] vecPreds predictors (for example, altitude, easting, northing)
* @param[out] obs observed value
*/
void FitMult::addData(const std::vector<double>& vecPreds, const double& obs)
{
	const size_t nrInputPreds = vecPreds.size();
	if (nPreds!=0 && nPreds!=nrInputPreds)
		throw InvalidArgumentException("Each observation MUST provide the same number of predictors!", AT);

	//check for nodata in obs and vecPreds
	if (obs==IOUtils::nodata) return;
	for (size_t ii=0; ii<nrInputPreds; ii++)
		if (vecPreds[ii]==IOUtils::nodata) return;

	if (nPreds==0) nPreds=nrInputPreds;

	//append to the predictors and observations vectors
	predictors.push_back( vecPreds );
	observations.push_back( obs );
}

bool FitMult::fit()
{
	//The Beta0 is handled by considering that it is B0*Z_j where Z_j=1 for all j
	const size_t nObs = predictors.size();
	if (nPreds==0)
		throw NoDataException("No predictors have been provided for computing the multiple linear regression!", AT);
	if (nObs<=nPreds) { // <= since there is also Beta0 that is added afterwards
		ostringstream ss;
		ss << "Only " << nObs << " data points for " << regname << " regression model.";
		ss << " Expecting at least " << nPreds+1 << " for this model!\n";
		infoString = ss.str();
		return false;
	}

	//build the Y and Z matrix
	//we know that observations and predictors do not contain nodata values
	Matrix Z(nObs, nPreds+1);
	Matrix Y(nObs, static_cast<size_t>(1));
	for (size_t jj=0; jj<nObs; jj++) {
		Y(jj+1, 1) = observations[jj];

		Z(jj+1, 1) = 1.;
		for (size_t ii=0; ii<nPreds; ii++)
			Z(jj+1, ii+2) = predictors[jj][ii];
	}

	//compute the Betas
	const Matrix Z_T( Z.getT() );
	Beta.resize(nPreds+1, 1);
	Beta = (Z_T * Z).getInv() * Z_T * Y;
	fit_ready = true;

	//compute R2
	const double ObsMean = Interpol1D::arithmeticMean( observations );
	double ss_err = 0., ss_tot = 0.;
	for (size_t ii=0; ii<nObs; ii++) {
		const double y_sim = f( predictors[ii] );
		ss_err += Optim::pow2( observations[ii] - y_sim );
		ss_tot += Optim::pow2( observations[ii] - ObsMean );
	}

	double R2 = IOUtils::nodata;
	if (ss_tot==0. && ss_err==0) R2 = 1.;
	if (ss_tot!=0.) R2 = 1. - ss_err / ss_tot;

	std::ostringstream ss;
	ss << "Computed regression with " << regname << " model - r2=" << std::setprecision(2) << R2;
	infoString = ss.str();
	return true;
}

double FitMult::f(const std::vector<double>& x) const
{
	if (!fit_ready)
		throw InvalidArgumentException("The regression has not yet been computed!", AT);
	if (x.size() != nPreds)
		throw InvalidArgumentException("Wrong number of predictors provided", AT);

	double sum = Beta(1,1); //this is Beta0
	for (size_t ii=0; ii<nPreds; ii++) {
		const double pred = x[ii];
		if (pred==IOUtils::nodata) return IOUtils::nodata;
		sum += Beta(ii+2, 1) * x[ii]; //Beta starts at 1 and we only need to apply from Beta1 = (2,1)
	}
	return sum;
}

std::vector<double> FitMult::getParams() const
{
	if (!fit_ready)
		throw InvalidArgumentException("The regression has not yet been computed!", AT);

	std::vector<double> coefficients;
	for (size_t ii=1; ii<=Beta.getNy(); ii++) //for consistency with the other models, we give them in reverse order
		coefficients.push_back( Beta(ii, 1) );

	return coefficients;
}

std::string FitMult::toString() const
{
	std::ostringstream os;
	os << "<FitMult>\n";
	if (!fit_ready) {
		os << regname << " model, not initialized\n";
	} else {
		os << infoString << " - " << Beta.getNy() << " predictors\n";
		os << "Model parameters:       \t";
		for (size_t ii=Beta.getNy(); ii>=1; ii--) os << Beta(ii, 1) << " ";
		os << "\n";
	}
	os << "</FitMult>\n";

	return os.str();
}

FitMult& FitMult::operator =(const FitMult& source)
{
	if (this != &source) {
		predictors = source.predictors;
		observations = source.observations;
		Beta = source.Beta;
		infoString = source.infoString;
		nPreds = source.nPreds;
		fit_ready = source.fit_ready;
	}
	return *this;
}

} //namespace
