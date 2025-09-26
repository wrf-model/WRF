/***********************************************************************************/
/*  Copyright 2019 Avalanche Warning Service Tyrol                  LWD-TIROL      */
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

#include <meteoio/meteoFilters/FilterKalman.h>
#include <meteoio/IOUtils.h>

namespace mio {

FilterKalman::FilterKalman(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name)
        : ProcessingBlock(vecArgs, name),
		  mat_in_xx(""), mat_in_AA(""), mat_in_HH(""), mat_in_PP("1"), mat_in_QQ("0"), mat_in_RR(""), mat_in_BB("0"), mat_in_uu("0"),
          meas_params(), error_params(), QQ_params(), RR_params(),
          filter_all_params(false), out_error_stddev(false), be_verbose(true), unrecognized_keys("")
{
	parse_args(vecArgs);
	properties.stage = ProcessingProperties::first;

	cleanBrackets(mat_in_xx); cleanBrackets(mat_in_AA); cleanBrackets(mat_in_HH); cleanBrackets(mat_in_PP); //remove all brackets
	cleanBrackets(mat_in_QQ); cleanBrackets(mat_in_RR); cleanBrackets(mat_in_BB); cleanBrackets(mat_in_uu); //(they are just visual help)
}

/**
 * @brief The filtering routine as called by the processing toolchain.
 * @param[in] param Parameter index the filter is asked to run on by the user.
 * @param[in] ivec Meteo data to filter.
 * @param[out] ovec Filtered meteo data. Cf. main documentation.
 */
void FilterKalman::process(const unsigned int& param, const std::vector<MeteoData>& ivec, std::vector<MeteoData>& ovec)
{

	/* INITIALIZATION */

	if (!unrecognized_keys.empty() && be_verbose) //because we have so many input keys
		std::cerr << "[W] Unrecognized ini key(s) ignored for Kalman filter: \"" + unrecognized_keys.substr(0, unrecognized_keys.length()-2) + "\"\n";

	const size_t TT = ivec.size(); //number of time steps
	const std::vector<double> vecT = buildTimeVector(ivec);

	std::vector<std::string> xx_str;
	size_t nx = IOUtils::readLineToVec(mat_in_xx, xx_str, ','); //fix number of internal states

	Matrix zz; //Note: the matrix class starts its indices at 1
	std::vector<size_t> meas_idx; //indices of given observables in meteo set
	const size_t nz = buildObservationsMatrix(param, ivec, nx, zz, meas_idx); //checks observations dimension - must match nx

	if (nx == 0) //special case: no initial state given
		nx = nz; //--> number of observations fixes the states

	Matrix xx( buildInitialStates(xx_str, meas_idx, ivec, nz) ); //once the observables are known, go back to parsing the initial states
	const std::vector<std::string> AA_str( parseSystemMatrix(mat_in_AA, nx) ); //keep as strings so we can do substitutions

	//Now that we know how many states and observables there are, we can size the rest of the matrices.
	//We keep nz for clarity, but this implementation forces nz=nx (not all have to actually be used).
	Matrix HH(nz, nx);
	if (mat_in_HH.empty()) { //use identity matrix
		HH.identity(nz);
		if (be_verbose) std::cerr << "[W] No Kalman filter model found to relate observations to states. Setting to \"identity\", but this may not make much sense.\n";
	} else {
		HH = bloatMatrix(mat_in_HH, nz, nx, "observation model");
	}
	
	Matrix PP( bloatMatrix(mat_in_PP, nx, nx, "initial trust") ); //trust in initial state
	Matrix QQ( bloatMatrix(mat_in_QQ, nx, nx, "process noise") ); //process noise covariance
	Matrix RR( bloatMatrix(mat_in_RR, nz, nz, "observation noise") ); //observation noise covariance
	bool has_RR_params, has_QQ_params;
	assertInputCovariances(ivec, nx, has_RR_params, has_QQ_params);

	//at last, we input an optional control signal, either as scalar, matrix, or "meteo" data:
	Matrix BB( bloatMatrix(mat_in_BB, nx, nx, "control relation") ); //relates control input to state
	Matrix uu( buildControlSignal(nx, TT, ivec) );

	/* KALMAN FILTER */
	Matrix KK; //Kalman gain
	Matrix II;
	II.identity(nx);
	ovec = ivec; //copy with all special parameters etc.

	for (size_t pp = 0; pp < error_params.size(); ++pp) //create parameters to output PP if they don't exist
		for (size_t ii = 0; ii < ovec.size(); ++ii)
			ovec[ii].addParameter(error_params[pp]);

	bool saw_nodata(false);
	size_t last_valid_idx(0);

	for (size_t kk = 0; kk < TT; ++kk) { //for each time step...
		const double dt = (kk == 0)? 0 : vecT[kk] - vecT[last_valid_idx]; //initial state is at time of 1st measurement
		const Matrix AA( buildSystemMatrix(AA_str, nx, dt, ivec, kk) );

    	if (has_RR_params) //if desired, get RR and QQ from input data at each time step
    		RR = extractInputMatrix(RR_params, ivec, kk);
    	if (has_QQ_params)
    		QQ = extractInputMatrix(QQ_params, ivec, kk);
    	const bool has_nodata = checkNodata(zz.getCol(kk+1));

    	if (!has_nodata) { //we can calculate

    		//prediction:
    		xx = ( AA * xx + BB * uu.getCol(kk+1) );
    		PP = ( AA * PP * AA.getT() + QQ );

    		//update:
    		KK = PP * HH.getT() * (HH * PP * HH.getT() + RR).getInv();
    		xx = ( xx + KK * (zz.getCol(kk+1) - HH * xx) );
    		PP = ( (II - KK * HH) * PP );

    		ovec[kk](param) = xx(1, 1); //filter the parameter the filter runs on
    		if (filter_all_params) //the following will filter all specified parameters, not just param!
    			for (size_t ii = 1; ii < nz; ++ii) //nx = nz, The user is responsible to provide enough output fields.
    				ovec[kk](meas_idx[ii]) = xx(ii+1, 1);

    		last_valid_idx = kk; //dt is calculated between valid data values

    		if (error_params.size() == nx) //output estimated error
    			for (size_t mm = 0; mm < nx; ++mm)
    				ovec[kk](error_params[mm]) = out_error_stddev? sqrt(PP(mm+1, mm+1)) : PP(mm+1, mm+1); //save diagonal elements only

    	} else { //all states stay as if this time step wasn't encountered
    		saw_nodata = true;
    	} //endif nodata
    } //endfor kk

    if (be_verbose && saw_nodata) std::cerr << "[W] Nodata value(s) or missing parameter encountered in Kalman filter; some values were ignored. You should probably resample beforehand.\n";
}

/**
 * @brief Parse the initial states of the system.
 * @details This function gets the initial states from either meteo data, substitutions, any other meteo parameter,
 * or double values.
 * @param[in] xx_str_in Original ini input line for the states.
 * @param[in] meas_idx Indices of the specified observation parameters.
 * @param[in] ivec Meteo data set the filter runs on.
 * @param[in] nz Number of observations provided.
 * @return Vector with the initial system states.
 */
Matrix FilterKalman::buildInitialStates(const std::vector<std::string>& xx_str_in, const std::vector<size_t>& meas_idx,
        const std::vector<MeteoData>& ivec, const size_t& nz) const
{
	bool all_nodata(false);
	std::vector<double> vecRet(nz);
	if (xx_str_in.empty()) { //rely on number of observations given
		for (size_t jj = 0; jj < nz; ++jj) { //guaranteed to be at least 1 - the one the filter runs on
			all_nodata = all_nodata || !findFirstDatapoint(ivec, meas_idx[jj], vecRet[jj]); //initial_state[t] = observation[t]
		}
	} else { //not empty
		for (size_t ii = 0; ii < nz; ++ii) {
			const std::string xx_str( IOUtils::trim(xx_str_in[ii]) );
			if (xx_str == "" || xx_str == "1st") { //some values are given, but not this one --> fill with 1st observation
				all_nodata = all_nodata || !findFirstDatapoint(ivec, meas_idx[ii], vecRet[ii]);
			} else if (xx_str == "average") { //take the average of all specified observables at t=0
				double av_sum = 0.;
				for (size_t jj = 0; jj < nz; ++jj) {
					double tmp(0.); //if this default is reached, an error is thrown anyway
					all_nodata = all_nodata || !findFirstDatapoint(ivec, meas_idx[jj], tmp); //once all_nodata -> always all_nodata
					av_sum += tmp;
				}
				vecRet[ii] = av_sum / (double)nz;
			} else if ( xx_str.size() > 6 && (xx_str.compare(0, 6, "meteo(") == 0) ) { //meteo parameters, e. g. "meteo(RH)"
				const size_t param_idx = ivec.front().getParameterIndex(xx_str.substr(6, xx_str.length()-7)); //e. g. "RH"
				all_nodata = all_nodata || !findFirstDatapoint(ivec, param_idx, vecRet[ii]);
			} else { //double value expected if we arrive here
				std::istringstream iss(xx_str);
				iss >> vecRet[ii];
				if (iss.fail())
					throw InvalidArgumentException("The Kalman filter could not read initial state \"" + xx_str	+ "\".", AT);
			}
		} //endfor ii
	} //endif empty

	if (all_nodata)
		throw NoDataException("The Kalman filter's automatic state initialization failed because some parameter was not present throughout the dataset.", AT);
	Matrix mRet(nz, 1, vecRet);
	return mRet;
}

/**
 * @brief Get user-specified observation parameters from the meteo data set and return in a dedicated matrix.
 * @details In addition the the parameter the filter runs on, the user can specify more observations. Those are
 * read from the meteo data at each time step, and put into a matrix.
 * @param[in] param The parameter index the filter runs on.
 * @param[in] ivec Meteo data set the filter runs on.
 * @param[out] zz Return matrix that is built from the observations; ready to use for all calculations.
 * @param[out] meas_idx Convenience vector with the indices of the observation parameters.
 */
size_t FilterKalman::buildObservationsMatrix(const unsigned int& param, const std::vector<MeteoData>& ivec, const size_t& nx,
        Matrix& zz, std::vector<size_t>& meas_idx) const
{ //extract observables from the meteo set to a dedicated vector
	meas_idx.clear(); //index map of observables
	meas_idx.push_back(param); //1st one is always the one the filter runs on

	for (size_t jj = 0; jj < meas_params.size(); ++jj) { //then come the ones specified in ADD_OBSERVABLES
		const size_t param_idx = ivec.front().getParameterIndex(meas_params[jj]);
		if (param_idx == IOUtils::npos)
			throw NoDataException("Parameter name \"" + meas_params[jj] + "\" listed in ADD_OBSERVABLES not found by the Kalman filter.", AT);
		meas_idx.push_back(param_idx);
	}

	if ( (meas_idx.size() != nx) && (nx != 0) ) //nx = 0: pick input states --> no error
		throw InvalidArgumentException("You need to provide as many observation variables as you have state variables for the Kalman filter ("
		        + IOUtils::toString(nx) + " including the one the filter runs on).", AT);

	zz.resize(meas_idx.size(), ivec.size()); //fill observation matrix with desired values from meteo set
	for (size_t kk = 0; kk < ivec.size(); ++kk) {
		for (size_t jj = 0; jj < meas_idx.size(); ++jj)
			zz(jj+1, kk+1) = ivec[kk](meas_idx[jj]);
	}
	return meas_idx.size();
}

/**
 * @brief Get the control signal ready from various input possibilities.
 * @details The external control signal can be input as a single value (all matrix elements will be this value), as a vector
 * (that will be repeated for each column), or in the meteo data set (with one parameter per state).
 * @param[in] nx Number of states.
 * @param[in] TT Number of time steps.
 * @param[in] ivec Meteo data set to extract control signal from.
 * @return Complete control signal matrix ready for the whole calculation.
 */
Matrix FilterKalman::buildControlSignal(const size_t& nx, const size_t& TT, const std::vector<MeteoData>& ivec) const
{
	Matrix uu(nx, TT);

	std::vector<std::string> vecU;
	const size_t nr_uu = IOUtils::readLineToVec(mat_in_uu, vecU, ',');

	double aa;
	std::istringstream iss(vecU.front());
	iss >> aa; //test-read double

	if (!iss.fail()) { //there's at least a numerical value in the beginning, so it should not be a meteo param name
		if (nr_uu == 1) { //case 1: single value --> replicate to whole matrix
			uu.resize(nx, TT, 1.);
			uu *= aa;
		} else if (nr_uu == nx) { //case 2: nx values --> repeat column vector
			for (size_t ii = 0; ii < nx; ++ii) {
				iss.clear();
				iss.str(vecU[ii]);
				iss >> aa;
				if (iss.fail())
					throw InvalidArgumentException("Kalman filter could not parse element " + vecU[ii] + " in control signal.", AT);
				for (size_t jj = 0; jj < TT; ++jj)
					uu(ii+1, jj+1) = aa;
			}
		} else {
			throw InvalidArgumentException("Control signal vector for Kalman filter ill-formatted (expected 1 or " +
			        IOUtils::toString(nx) + " elements).", AT);
		}
	} else { //case 3: parameter names given --> separate vector each timestep
		if (nr_uu != nx)
			throw InvalidArgumentException("Control signal vector for Kalman filter cannot be constructed (expected: " +
			        IOUtils::toString(nx) + " parameter names).", AT);
		for (size_t jj = 0; jj < TT; ++jj)
			for (size_t ii = 0; ii < nx; ++ii) {
				IOUtils::trim(vecU[ii]);
				uu(ii+1, jj+1) = ivec[jj](vecU[ii]);
			}
	}
	return uu;
}

/**
 * @brief Parse a string to a matrix.
 * @details This is the "core" parser when no substitutions are necessary and only double values are read.
 * @param[in] line String to parse the matrix from.
 * @param[in] rows Number of rows of the resulting matrix.
 * @param[in] cols Number of columns of the resulting matrix.
 * @param[in] blockname Name of the matrix that is being parsed (for error localization).
 * @return The parsed matrix with double values.
 */
Matrix FilterKalman::parseMatrix(const std::string& line, const size_t& rows, const size_t& cols,
        const std::string& blockname) const
{ //matrix parsing where no substitutions are done - i. e. double values only
	std::vector<double> vecElements;
	const size_t nr_elements = IOUtils::readLineToVec(line, vecElements, ',');

	if (rows*cols != nr_elements)
		throw InvalidArgumentException("The Kalman filter encountered an unfit " + blockname + " input matrix size (expected: " +
		        IOUtils::toString(rows) + "x" + IOUtils::toString(cols) + ").", AT);

	Matrix mRet(rows, cols, vecElements); //copy vector to matrix
	return mRet;
}

/**
 * @brief Parse the system matrix from an input string.
 * @details The system matrix is built from an input string, but its elements are still kept as strings at this point. This
 * is to allow for substitutions later. A single input value is put on the diagonal. We used a rolled up index for the
 * 1-dimensional vector to act as a matrix.
 * @param[in] line String to parse the system matrix from.
 * @param[in] sz Number of rows (= number of columns) of the system matrix.
 * @return Vector of strings that constitute the system matrix (all elements saved after each other).
 */
std::vector<std::string> FilterKalman::parseSystemMatrix(const std::string& line, const size_t& sz) const
{
	std::vector<std::string> vecRet; //keep as strings to be able to do substitutions
	const size_t nr_elements = IOUtils::readLineToVec(line, vecRet, ',');

	if (nr_elements == 1) { //single value --> put on diagonal, rest is 0
		vecRet.resize(sz*sz);
		for (size_t ii = 0; ii < sz; ++ii)
			for (size_t jj = 0; jj < sz; ++jj)
				vecRet[ii*sz + jj] = (ii == jj)? vecRet.front() : "0";
	} else if (nr_elements == sz) { //vector --> put on diagonal, rest is 0
		const std::vector<std::string> vecTmp(vecRet); //guard aliasing
		vecRet.resize(sz*sz);
		for (size_t ii = 0; ii < sz; ++ii)
			for (size_t jj = 0; jj < sz; ++jj)
				vecRet[ii*sz + jj] = (ii == jj)? vecTmp[ii] : "0";
	} else if (nr_elements != sz*sz) { //it's a square matrix
		throw InvalidArgumentException("The Kalman filter encountered an unfit state input matrix size (expected: " +
		        IOUtils::toString(sz) + "x" + IOUtils::toString(sz) + ").", AT);
	}
	return vecRet;
}

/**
 * @brief Construct a double-valued matrix from a string matrix with substitutions.
 * @details The system matrix is kept as strings until right before the calculation. At each time step, the strings are
 * then parsed and substitutions are performed to get the real arithmetic matrix.
 * @param[in] AA_str The system matrix in string form.
 * @param[in] sz Number of rows (= number of columns) of the system matrix.
 * @param[in] dt Current time delta between measurements, can be substituted for with "dt".
 * @param[in] ivec Meteo data set to get substitutions from.
 * @param[in] kk Current time step.
 * @return Double-valued system matrix with all substitutions in place.
 */
Matrix FilterKalman::buildSystemMatrix(const std::vector<std::string>& AA_str, const size_t& sz, const double& dt,
        const std::vector<MeteoData>& ivec, const size_t& kk) const
{ //substitutions in system matrix
	Matrix AA(sz, sz);
	for (size_t ii = 0; ii < sz; ++ii)
		for (size_t jj = 0; jj < sz; ++jj)
			AA(ii+1, jj+1) = substitute(AA_str[ii*sz + jj], dt, ivec, kk);
	return AA;
}

/**
 * @brief Perform substitutions in the system matrix.
 * @details At each time step the user can substitute some values in the system matrix, e. g. the time step and meteo parameters.
 * Additionally, this function performs the conversion from string to double for the system matrix.
 * @param[in] dt Current time delta that will be replicated as return value when substituting "dt".
 * @param[in] ivec Meteo data to pull meteo value substitutions from.
 * @param[in] kk Current time step, i. e. index of the meteo data set used to pull substitution values from.
 * @return Double value for the current entry of the system matrix.
 */
double FilterKalman::substitute(const std::string& expr, const double& dt, const std::vector<MeteoData>& ivec, const size_t& kk) const
{
	const std::string texp( IOUtils::trim(expr) );
	if (texp == "dt") { //current time step (time between measurements)
		return dt;
	} else if ( texp.size() > 6 && (texp.compare(0, 6, "meteo(") == 0) ) { //meteo parameters
		return ivec[kk](texp.substr(6, texp.length()-7));
	} else { //double values
		std::istringstream ss(texp);
		double aa;
		ss >> aa;
		if (ss.fail())
			throw InvalidArgumentException("Unrecognized value in the Kalman filter's system matrix: \"" + texp + "\".", AT);
		return aa;
	}
}

/**
 * @brief Read a number of meteo parameters and construct diagonal matrix from them.
 * @details This function searches a meteo data set for a list of given parameters and puts those at the diagonal of
 * an otherwise zero-valued matrix.
 * @param[in] vecParams Vector of parameter names (the matrix will be vecParams.size() x vecParams.size()).
 * @param[in] mvec Meteo data set the values are extracted from.
 * @return Matrix with the extracted meteo values on the diagonal.
 */
Matrix FilterKalman::extractInputMatrix(const std::vector<std::string>& vecParams, const std::vector<MeteoData>& mvec,
        const size_t& kk) const
{ //read meteo parameters (given in a vector) to a diagonal matrix
	Matrix dia(vecParams.size(), vecParams.size(), 0.);
	for (size_t ii = 0; ii < vecParams.size(); ++ii)
		dia(ii+1, ii+1) = mvec[kk](vecParams[ii]);
	return dia;
}

/**
 * @brief Check if meteo data can be used to read covariance matrices.
 * @detail The process covariance matrix QQ and the observation covariance matrix RR can be read from the meteo
 * data set. For this, one parameter per matrix column (that is specified by the user) must be present since
 * this value is later put on the diagonal. This function checks if every specified parameter is actually accessible.
 * @param[in] nx Number of states (= number of expected values per matrix).
 * @param[in] ivec Meteo data set that is checked for the existance of parameters.
 * @param[out] has_RR_params Set to true if the RR parameters exists (at least for the first measurement).
 * @param[out] has_QQ_params True if the QQ parameters exist.
 */
void FilterKalman::assertInputCovariances(const std::vector<MeteoData>& ivec, const size_t& nx, bool& has_RR_params,
        bool& has_QQ_params) const
{ //check if all given parameters to input RR and/or QQ exist
	if (RR_params.size() == nx) {
		has_RR_params = true;
		for (size_t rr = 0; rr < RR_params.size(); ++rr) {
			if ( ivec.front().getParameterIndex(RR_params[rr]) == IOUtils::npos ) { //check 1st one only...
				has_RR_params = false;
				break;
			}
		}
	} else  {
		has_RR_params = false;
	}
	if (QQ_params.size() == nx) {
		has_QQ_params = true;
		for (size_t qq = 0; qq < QQ_params.size(); ++qq) {
			if ( ivec.front().getParameterIndex(QQ_params[qq]) == IOUtils::npos ) {
				has_QQ_params = false;
				break;
			}
		}
	} else  {
		has_QQ_params = false;
	}
	if (mat_in_RR.empty() && !has_RR_params) //no data here, and no fixed input matrix
		throw InvalidArgumentException("The Kalman filter needs an OBSERVATION_COVARIANCE matrix. No valid parameters found in meteo data either.", AT);
	if (has_RR_params && !mat_in_RR.empty())
		if (be_verbose) std::cerr << "[W] The Kalman filter's observation covariance matrix is provided as single matrix and in the meteo data. Using the latter.\n";
	if (has_QQ_params && !mat_in_QQ.empty())
		if (be_verbose) std::cerr << "[W] The Kalman filter's process covariance matrix is provided as single matrix and in the meteo data. Using the latter.\n";
}

/**
 * @brief Builds a matrix from a scalar, vector, or input string.
 * @details If no elements are given the function returns an empty vector. If one element (a scalar) is given then it is put on the diagonal
 * of an otherwise zero-valued matrix. If a vector matching the number of rows and columns is given then this vector is put on the diagonal.
 * If the whole matrix is given in the input string then this is parsed and returned.
 * @param[in] line Input string to build a matrix from.
 * @param[in] rows Number of rows of the matrix.
 * @param[in] cols Number of column of the input matrix.
 * @param[in] blockname Name of the matrix that is being parsed (for error localization).
 * @return The matrix that was built from the input string.
 */
Matrix FilterKalman::bloatMatrix(const std::string& line, const size_t& rows, const size_t& cols, const std::string& blockname) const
{ //if a scalar is given put it on the diagonal, same with a vector, else read the complete matrix
	Matrix ret;
	if (line.length() == 0)
		return ret;
	
	const size_t nr = IOUtils::count(line, ",");
	if ( (nr == rows - 1) && (rows == cols) ) { //parse as column vector and put that on diagonal
		const Matrix tmp( parseMatrix(line, rows, 1, blockname) );
		ret.resize(rows, rows, 0.);
		for (size_t ii = 1; ii <= rows; ++ii)
			ret(ii, ii) = tmp(ii, 1);
	} else if (nr > 0) { //not an isolated double
		ret = parseMatrix(line, rows, cols, blockname);
	} else { //single value: put on diagonal
		std::istringstream ss(line);
		double aa;
		ss >> aa;
		if (ss.fail())
			throw InvalidArgumentException("Kalman filter could not parse value \"" + line + "\" for " + blockname + " matrix.", AT);
		ret.identity(rows);
		ret *= aa;
	}
	return ret;
}

/**
 * @brief Associate time spans with vector indices.
 * @details To evaluate functions, as well as to interpolate, it is meaningful to look at how far data points are apart
 * in time (as opposed to looking at the vector index). For this, the first data point is set to time 0, the second one
 * to time 1, and every other distance is relative to that.
 * @param[in] ivec Meteo data vector holding values and dates.
 * @return Vector holding the normalized time for each index.
 */
std::vector<double> FilterKalman::buildTimeVector(const std::vector<MeteoData>& ivec) const
{ //time representation of the index: shift and normalize to t(0)=0, t(1)=1
	const double base_time = ivec.front().date.getJulian();
	double dt(1.);

	if (ivec.size() > 1)
		dt = ivec[1].date.getJulian() - base_time;

	std::vector<double> vecRet(ivec.size());
	for (size_t ii = 0; ii < ivec.size(); ++ii)
		vecRet[ii] = (ivec[ii].date.getJulian() - base_time) / dt;

	return vecRet;
} //NOTE: this is duplicate code found in FilterParticle.cc as well

/**
 * @brief Checks if any element in a vector is nodata.
 * @param[in] ivec Vector with values to look through.
 * @return True if any element is nodata, false if none of them are.
 */
bool FilterKalman::checkNodata(const Matrix& ivec) const
{ //is any vector element nodata?
	for (size_t ii = 1; ii <= ivec.getNx(); ++ii)
		if (ivec(ii, 1) == IOUtils::nodata)
			return true;
	return false;
}

/**
 * @brief Find first data point that is not nodata.
 * @param[in] ivec Meteo data to search.
 * @param[in] param Parameter that is searched for.
 * @param[out] retval Firts valid data point value, nodata if none is found.
 * @return True if data was found, false if everything is nodata.
 */
bool FilterKalman::findFirstDatapoint(const std::vector<MeteoData>& ivec, const size_t& param, double& retval) const
{
	for (size_t ii = 0; ii < ivec.size(); ++ii) {
		if (ivec[ii](param) != IOUtils::nodata) {
			retval = ivec[ii](param);
			return true;
		}
	}
	retval = IOUtils::nodata;
	return false;
}

/**
 * @brief Called by the processing chain to read input settings.
 * @param[in] vecArgs Vector of string-pairs holding ini keys and values.
 */
void FilterKalman::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	const std::string where("Filters::" + block_name);

	for (size_t ii = 0; ii < vecArgs.size(); ii++) {
		if (vecArgs[ii].first == "STATE_DYNAMICS") {
			mat_in_AA = vecArgs[ii].second;
		} else if (vecArgs[ii].first == "INITIAL_STATE") {
			mat_in_xx = vecArgs[ii].second;
		} else if (vecArgs[ii].first == "INITIAL_TRUST") {
			mat_in_PP = vecArgs[ii].second;
		} else if (vecArgs[ii].first == "PROCESS_COVARIANCE") {
			mat_in_QQ = vecArgs[ii].second;
		}

		else if (vecArgs[ii].first == "ADD_OBSERVABLES") {
			(void) IOUtils::readLineToVec(vecArgs[ii].second, meas_params);
		} else if (vecArgs[ii].first == "FILTER_ALL_PARAMETERS") {
			IOUtils::parseArg(vecArgs[ii], where, filter_all_params);
		} else if (vecArgs[ii].first == "OBSERVATION_RELATION") {
			mat_in_HH = vecArgs[ii].second;
		} else if (vecArgs[ii].first == "OBSERVATION_COVARIANCE") {
			mat_in_RR = vecArgs[ii].second;
		}

		else if (vecArgs[ii].first == "CONTROL_SIGNAL") {
			mat_in_uu = vecArgs[ii].second;
		} else if (vecArgs[ii].first == "CONTROL_RELATION") {
			mat_in_BB = vecArgs[ii].second;
		}

		else if (vecArgs[ii].first == "OUT_ESTIMATED_ERROR") {
			(void) IOUtils::readLineToVec(vecArgs[ii].second, error_params);
		} else if (vecArgs[ii].first == "OUT_ERROR_AS_STDDEV") {
			IOUtils::parseArg(vecArgs[ii], where, out_error_stddev);
		} else if (vecArgs[ii].first == "PROCESS_COVARIANCE_PARAMS") {
			(void) IOUtils::readLineToVec(vecArgs[ii].second, QQ_params);
		} else if (vecArgs[ii].first == "OBSERVATION_COVARIANCE_PARAMS") {
			(void) IOUtils::readLineToVec(vecArgs[ii].second, RR_params);
		} else if (vecArgs[ii].first == "VERBOSE") {
			IOUtils::parseArg(vecArgs[ii], where, be_verbose);
		} else {
			unrecognized_keys += vecArgs[ii].first + ", "; //constructor is always called twice - show only when processing
		}
	} //endfor vecArgs

	if (mat_in_AA.empty())
		throw InvalidArgumentException("The Kalman filter needs a linear model, i. e. a matrix given in STATE_DYNAMICS.", AT);
}

/** @brief Removes brackets from a string.
 * @details This function allows to ignore brackets that may be used as visual help for matrix input. Expected there are
 * comma-separated values, and by removing/replacing brackets formats such as "[1, 2, 3][4, 5, 6]" are possible.
 * @param[in,out] iline String that will be modified by removing/replacing brackets.
 */
void FilterKalman::cleanBrackets(std::string& iline) const
{ //allow input with brackets to no effect
	IOUtils::replace_all(iline, "][", ", ");
	IOUtils::replace_all(iline, "] [", ", ");
	IOUtils::replace_all(iline, "[", "");
	IOUtils::replace_all(iline, "]", "");
}

} //namespace
