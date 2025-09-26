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
#ifndef LIBFIT1D_H
#define LIBFIT1D_H

#include <meteoio/meteoStats/libfit1DCore.h>
#include <meteoio/IOUtils.h>

#include <vector>

namespace mio {

class Zero : public FitModel {
	public:
		Zero() : FitModel("Zero", 0, 0) {fit_ready=true;}
		void setData(const std::vector<double>& /*in_X*/, const std::vector<double>& /*in_Y*/) {}
		bool fit() { return true;}
		double f(const double& /*x*/) const {return 0.;}
};

class SimpleLinear : public FitModel {
	public:
		SimpleLinear() : FitModel("SimpleLinear", 2, 2), fixed_lapse_rate(IOUtils::nodata) {fit_ready=false;}
		SimpleLinear(const std::string& i_regname) : FitModel(i_regname, 2, 2), fixed_lapse_rate(IOUtils::nodata) {fit_ready=false;}
		void setData(const std::vector<double>& in_X, const std::vector<double>& in_Y);
		bool fit();
		double f(const double& x) const;
		void setLapseRate(const double& in_lapse_rate) {fixed_lapse_rate = in_lapse_rate; fit_ready = false; min_nb_pts--;}
	protected:
		double fixed_lapse_rate;
};

class NoisyLinear : public SimpleLinear {
	public:
		NoisyLinear() : SimpleLinear("NoisyLinear") {fit_ready=false;}
		bool fit();
};

class SphericVario : public FitLeastSquare {
	public:
		SphericVario() : FitLeastSquare("SphericVario", 3, 4) {fit_ready = false;}
		void setDefaultGuess();
		double f(const double& x) const;
};

class LinVario : public FitLeastSquare {
	public:
		LinVario() : FitLeastSquare("LinVario", 2, 3) {fit_ready = false;}
		void setDefaultGuess();
		double f(const double& x) const;
};

class ExpVario : public FitLeastSquare {
	public:
		ExpVario() : FitLeastSquare("ExpVario", 3, 4) {fit_ready = false;}
		void setDefaultGuess();
		double f(const double& x) const;
};

class RatQuadVario : public FitLeastSquare {
	public:
		RatQuadVario() : FitLeastSquare("RatQuadVario", 3, 4) {fit_ready = false;}
		void setDefaultGuess();
		double f(const double& x) const;
};

class LinearLS : public FitLeastSquare {
	public:
		LinearLS() : FitLeastSquare("LinearLS", 2, 3) {fit_ready = false;}
		void setDefaultGuess();
		double f(const double& x) const;
};

class Quadratic : public FitLeastSquare {
	public:
		Quadratic() : FitLeastSquare("Quadratic", 3, 4) {fit_ready = false;}
		void setDefaultGuess();
		double f(const double& x) const;
};

  /**
 * @class PolynomialRegression
 * @brief A class to perform 1D polynomial regression
 * @details
 * Polynomial Regression aims to fit a non-linear relationship to a set of
 * points. It approximates this by solving a series of linear equations using
 * a least-squares approach.
 *
 * We can model the expected value y as an nth degree polynomial, yielding
 * the general polynomial regression model:
 *
 * yi = b0 + b1*xi + b2*xi^2 + ... + bm*xi^m, with i=1...n
 * m corresponds to the degree of the polynomial equation
 * n is the number of data points (for fitting)
 *
 * see https://en.wikipedia.org/wiki/Polynomial_regression
 *
 * @ingroup stats
 * @author Thiemo Theile
 * @date   2018-08-20
 */
class PolynomialRegression : public FitModel {
	public:
	    //the default-constructor sets the degree to 2 (= quadratic regression)
		PolynomialRegression() : FitModel("POLYNOMIAL", 3, 3), degree(2) {fit_ready=false;}
		PolynomialRegression(const std::string& i_regname, const size_t& degreeOfRegression) :
                            FitModel(i_regname, degreeOfRegression+1, degreeOfRegression+1),
                            degree(degreeOfRegression){fit_ready=false;}
		void setData(const std::vector<double>& in_X, const std::vector<double>& in_Y);
		bool fit();
		double f(const double& x) const;
		void setDegree(const size_t& in_degree) {degree = in_degree; fit_ready = false; min_nb_pts=in_degree+1; nParam=in_degree+1; }
	protected:
		size_t degree;
};

/**
 * @class Fit1D
 * @brief A class to perform 1D regressions
 * @details
 * It works on a time serie and uses either ad-hoc methods or matrix arithmetic to perform an arbitrary fit.
 * Currently, the following models are supported:
 * - Specific fits:
 *    - SimpleLinear
 *    - NoisyLinear
 *    - PolynomialRegression
 * - Least Square fits:
 *    - SphericVario
 *    - LinVario
 *    - ExpVario
 *    - RatQuadVario
 *    - LinearLS
 *    - Quadratic
 *
 * The various variogram models can be found in
 * <i>"Statistics for spatial data"</i>, Noel A. C. Cressie, John Wiley & Sons, revised edition, 1993, pp63.
 *
 *
 * @ingroup stats
 * @author Mathias Bavay
 * @date   2011-01-20
 */
class Fit1D {
 	public:
		///Keywords for regression model
		typedef enum REGRESSION {
			ZERO, ///< always return zero (this is a way to disable detrending)
			SIMPLE_LINEAR, ///< basic, cheap linear fit
			NOISY_LINEAR, ///< same as SIMPLE_LINEAR but trying to remove outliers
			LINVARIO, ///< linear variogram
			EXPVARIO, ///< exponential variogram
			SPHERICVARIO, ///< spherical variogram
			RATQUADVARIO, ///< rational quadratic variogram
			LINEARLS, ///< linear, using least squares
			QUADRATIC, ///< quadratic
			POLYNOMIAL ///< polynomial regression
		} regression;

		/**
		* @brief Empty Constructor. The model must be set afterwards.
		* If the model has not been set before calling other methods, a NULL pointer exception will be thrown.
		*/
		Fit1D() : model(NULL) {}

		/**
		* @brief Constructor.
		* @param regType regression model to use
		* @param in_X vector of data points abscissae
		* @param in_Y vector of data points ordinates
		* @param updatefit should the fit be redone? (default=true, otherwise you must manually call fit())
		*/
		Fit1D(const regression& regType, const std::vector<double>& in_X, const std::vector<double>& in_Y, const bool& updatefit=true);

		/**
		* @brief Constructor for user provided model type.
		* @param regType regression model to use
		* @param in_X vector of data points abscissae
		* @param in_Y vector of data points ordinates
		* @param updatefit should the fit be redone? (default=true, otherwise you must manually call fit())
		*/
		Fit1D(const std::string& regType, const std::vector<double>& in_X, const std::vector<double>& in_Y, const bool& updatefit=true);

		/**
		* @brief Copy constructor.
		* @param i_fit Object to copy
		*/
		Fit1D(const Fit1D& i_fit);

		~Fit1D() {delete model;}

		/**
		* @brief Set or reset the regression model.
		* @param i_regType regression model to use
		* @param in_X vector of data points abscissae
		* @param in_Y vector of data points ordinates
		* @param updatefit should the fit be redone? (default=true, otherwise you must manually call fit())
		* @return false if could not compute the parameters. if !updatefit, always return true
		*/
		bool setModel(const regression& i_regType, const std::vector<double>& in_X, const std::vector<double>& in_Y, const bool& updatefit=true);

		/**
		* @brief Set or reset the regression model.
		* @param i_regType regression model to use
		* @param in_X vector of data points abscissae
		* @param in_Y vector of data points ordinates
		* @param updatefit should the fit be redone? (default=true, otherwise you must manually call fit())
		* @return false if could not compute the parameters. if !updatefit, always return true
		*/
		bool setModel(const std::string& i_regType, const std::vector<double>& in_X, const std::vector<double>& in_Y, const bool& updatefit=true);

		/**
		* @brief Provide a set of initial values for the model parameters.
		* The model can be used right after providing the guesses, and it would use those guesses as parameters,
		* thus allowing the user to force his model parameters.
		* @param lambda_in one initial value per model parameter
		*/
		void setGuess(const std::vector<double>& lambda_in) {model->setGuess(lambda_in);}

		/**
		* @brief Set a forced lapse rate for linear regressions
		* This will throw an exception for all other regression models!
		* @param lapse_rate lapse rate to set
		*/
		void setLapseRate(const double& lapse_rate) {model->setLapseRate(lapse_rate);}

		/**
		* @brief Set the degree of the polynomial regression
		* This will throw an exception for all other regression models!
		* @param degree degree of the polynomial regression to set
		*/
		void setDegree(const size_t& degree) {model->setDegree(degree);}

		/**
		* @brief Compute the regression parameters
		* @return false if could not compute the parameters
		*/
		bool fit() {return model->fit();}

		/**
		* @brief Calculate a value using the computed least square fit.
		* The fit has to be computed before.
		* @param x abscissa
		* @return f(x) using the computed least square fit
		*/
		double f(const double& x) const {return model->f(x);}

		/**
		* @brief Calculate the parameters of the fit.
		* The fit has to be computed before.
		* @return vector containing the coefficients
		*/
		std::vector<double> getParams() const { return model->getParams();}

		/**
		* @brief Return the name of the fit model.
		* @return model name
		*/
		std::string getName() const {return model->getName();}

		/**
		* @brief Return a string of information about the fit.
		* The fit has to be computed before.
		* @return info string
		*/
		std::string getInfo() const {return model->getInfo();}

		/**
		* @brief Set the information string.
		* This is useful to append some extra information to the information string.
		* This should be called <b>after</b> computing the fit (otherwise it will be overwritten).
		* @param info string
		*/
		void setInfo(const std::string& info) {model->setInfo(info);}

		Fit1D& operator =(const Fit1D& source);

		/**
		* @brief Calculate a value using the computed least square fit.
		* The fit has to be computed before.
		* @param x abscissa
		* @return f(x) using the computed least square fit
		*/
		double operator ()(const double& x) const {return model->f(x);}

		std::string toString() const {return model->toString();}

	private:
		FitModel *model;
};

/**
 * @class FitMult
 * @brief A class to perform multiple linear regressions.
 * @details This class performs linear regressions with multiple predictors. For example,
 * to compute air temperature trends based on elevation, easting, northing (ie predictors).
 * See www.public.iastate.edu/~maitra/stat501/lectures/MultivariateRegression.pdf
 *
 * It solves the multiple linear regression as
 * \f[
 * \bm{Y = Z\beta + \epsilon}
 * \f]
 * where \f$ \bm{Y} \f$ is the matrix of the observations, \f$ \bm{Z} \f$ the matrix of the predictors, \f$ \bm{\beta} \f$ the matrix of the regression
 * coefficients and \f$ \bm{\epsilon} \f$ the matrix of errors. The \f$ \bm{Y} \f$ and \f$ \bm{Z} \f$ matrices are filled with the \f$ \bm{n} \f$ observations
 * and \f$ \bm{r} \f$ predictors (for example with 3 predictors, altitude, easting and northing) so the previous equation becomes:
 * \f[
 * \underbrace{
 * \left(
 * \begin{array}{c}
 * obs_1 \\
 * obs_2 \\
 * \vdots \\
 * obs_n
 * \end{array}
 * \right)
 * }_{Y}
 * \qquad
 * =
 * \qquad
 * \underbrace{
 * \left(
 * \begin{array}{cccc}
 * 1 & alt_1 & east_1 & north_1 \\
 * 1 & alt_2 & east_2 & north_2 \\
 * \vdots & \vdots & \vdots & \vdots \\
 * 1 & alt_n & east_n & north_n
 * \end{array}
 * \right)
 * }_{Z}
 * \cdot
 * \underbrace{
 * \left(
 * \begin{array}{c}
 * \beta_0 \\
 * \beta_1  \\
 * \vdots \\
 * \beta_r
 * \end{array}
 * \right)
 * }_{\beta}
 * +
 * \underbrace{
 * \left(
 * \begin{array}{c}
 * \epsilon_1 \\
 * \epsilon_2 \\
 * \vdots \\
 * \epsilon_n
 * \end{array}
 * \right)
 * }_{\epsilon}
 * \f]
 *
 * If we write \f$ \bm{\hat{\beta}} \f$ the least square estimate of \f$ \bm{\beta} \f$, then \f$ \bm{ \hat{\beta} = (Z^\top Z)^{-1}Z^\top Y} \f$
 * and the predicted values are computed as \f$ \bm{ \hat{Y} = Z\hat{\beta} } \f$.
 *
 * @ingroup stats
 */
class FitMult {
	public:
		FitMult() : predictors(), observations(), Beta(), regname("MultiLinear"), infoString(), nPreds(0), fit_ready(false) {}

		void addData(const std::vector<double>& vecPreds, const double& obs);
		bool fit();
		double f(const std::vector<double>& x) const;
		double operator ()(const std::vector<double>& x) const { return f(x);}
		std::vector<double> getParams() const;
		std::string getName() const {return regname;}
		std::string getInfo() const {return infoString;}
		void setInfo(const std::string& info) {infoString=info;}
		FitMult& operator =(const FitMult& source);
		bool isReady() const {return fit_ready;}
		std::string toString() const;
	private:
		std::vector< std::vector<double> > predictors;
		std::vector<double> observations;
		Matrix Beta;
		const std::string regname; //model name
		std::string infoString;
		size_t nPreds; ///< number of predictors
		bool fit_ready;
};

} //end namespace

#endif
