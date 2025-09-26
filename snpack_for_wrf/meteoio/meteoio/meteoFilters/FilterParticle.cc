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

#include <meteoio/meteoFilters/FilterParticle.h>
#include <meteoio/meteoStats/libfit1D.h>

#include <fstream> //for the dump files
#include <limits>
#include <sstream> //for readLineToVec
#include <cerrno>
#include <cstring>

namespace mio {

FilterParticle::FilterParticle(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name)
        : ProcessingBlock(vecArgs, name), filter_alg(PF_SIR), resample_alg(PF_SYSTEMATIC), NN(500), path_resampling(true),
          model_expression(""), obs_model_expression(""), fit_expression(""), fit_param(""), fit_degree(3), model_x0(IOUtils::nodata),
		  resample_percentile(0.5), estim_measure(PF_MEAN), rng_model(), rng_obs(), rng_prior(), resample_seed(),
          be_verbose(true), unrecognized_keys(""), dump_particles_file(""), dump_states_file(""), input_states_file("")
{
	parse_args(vecArgs);
	properties.stage = ProcessingProperties::first;
}

/**
 * @brief The filtering routine as called by the processing toolchain.
 * @param[in] param Parameter index the filter is asked to run on by the user.
 * @param[in] ivec Meteo data to filter.
 * @param[out] ovec Filtered meteo data. Cf. main documentation.
 */
void FilterParticle::process(const unsigned int& param, const std::vector<MeteoData>& ivec, std::vector<MeteoData>& ovec)
{

	/* INITIALIZATION */

	if (!unrecognized_keys.empty() && be_verbose) //because we have so many input keys
		std::cerr << "[W] Unrecognized ini key(s) ignored for particle filter: \"" + unrecognized_keys.substr(0, unrecognized_keys.length()-2) + "\"\n";
	const size_t TT = ivec.size(); //number of time steps
	const bool has_model = !model_expression.empty(); //work with model or data fit?
	ovec = ivec; //copy with all special parameters etc.

	const bool nodata_check = checkInitialState(ivec, param); //if not provided, find first valuable meteo data point for x0
	if (!nodata_check)
		return; //nothing to do, keep all values as nodata

	//init random number generators:
	RandomNumberGenerator RNGU(rng_model.algorithm, rng_model.distribution, rng_model.parameters); //process noise
	RandomNumberGenerator RNGV(rng_obs.algorithm, rng_obs.distribution, rng_obs.parameters); //observation pdf
	RandomNumberGenerator RNG0(rng_prior.algorithm, rng_prior.distribution, rng_prior.parameters); //prior pdf
	RandomNumberGenerator RNU; //uniforms for resampling
	seedGeneratorsFromIni(RNGU, RNGV, RNG0, RNU);

	//init states:
	Matrix xx(NN, TT); //particles
	Matrix zz(TT, (size_t)1); //observations
	vecMeteoToMatrix(ivec, zz, param);
	Matrix ww(NN, TT); //weights of particles
	const std::vector<double> tVec = buildTimeVector(ivec);

	bool instates_success(false);
	
	if (!input_states_file.empty()) //there is data saved from a previous run
		instates_success = readInternalStates(xx, ww);  //online data aggregation	
	if (!instates_success) { //start from the initial value
		xx(1, 1) = model_x0;
		ww(1, 1) = 1. / NN;
		for (size_t nn = 1; nn < NN; ++nn) { //draw from prior pdf for initial state of particles at T=0
			xx(nn+1, 1) = xx(1, 1) + RNG0.doub();
			ww(nn+1, 1) = 1. / NN; //starting up, all particles have the same weight
		}
	}

	//prepare system model and observation model expressions:
	std::vector<std::string> sub_expr, sub_params;
	parseSubstitutionStrings(model_expression, obs_model_expression, sub_expr, sub_params); //get substitution strings and index map for the meteo parameters
	std::vector<double> sub_values(sub_expr.size()); //empty so far but with reserved memory to point to

	te_variable *te_vars = new te_variable[sub_expr.size()];
	initFunctionVars(te_vars, sub_expr, sub_values); //build te_variables from substitution vectors
	te_expr *expr_model = NULL; //only compile if available
	te_expr *expr_obs = compileExpression(obs_model_expression, te_vars, sub_expr.size());

	/*
	 * SUBSTITUTIONS:
	 *     [0]: index (k)
	 *     [1]: time index (t)
	 *     [2]: value (x_k) - only in observations equation as x_meas (e. g. mean)
	 *     [3]: previous value (x_km1) - arbitrarily x0 for k=0 in observations equation
	 *   [4-?]: meteo values
	 */
	static const size_t nr_hardcoded_sub = 4; //in order not to forget to keep substitutions in sync

	Fit1D model_fit;
	std::vector<double> model_data_points;

	if (has_model) {
		expr_model = compileExpression(model_expression, te_vars, sub_expr.size()); //empty string would fail
	} else { //prepare a fit ready to evaluate
		model_data_points.resize(ivec.size());
		for (size_t ii = 0; ii < ivec.size(); ++ii) //read the modeled data points
			model_data_points[ii] = ivec[ii](fit_param);

		model_fit.setModel(fit_expression, tVec, model_data_points, false);
		if (IOUtils::strToLower(fit_expression) == "polynomial")
			model_fit.setDegree(fit_degree);
		model_fit.fit();
	} //endif has_model

	/* PARTICLE FILTER */

	bool saw_nodata(false);
	try {
		for (size_t kk = 1; kk < TT; ++kk) { //for each TIME STEP (starting at 2nd)...
			double col_weight_sum(0.);
			if (has_model) {
				sub_values[0] = (double)kk; //this vector is linked to tinyexpr expression memory
				sub_values[1] = tVec[kk];
				for (size_t jj = 0; jj < sub_params.size(); ++jj) //fill current meteo parameters
					sub_values[jj+nr_hardcoded_sub] = ivec[kk](IOUtils::strToUpper(sub_params[jj]));
			} //endif has_model

			if (ivec[kk](param) == IOUtils::nodata) {
				xx.setCol(kk+1, xx.getCol(kk)); //repeat particles for nodata values
				ww.setCol(kk+1, ww.getCol(kk)); //the mean gets skewed a little
				saw_nodata = true;
			} else {
				if (filter_alg == PF_SIR) { //SIR algorithm, algorithm 4 of Ref. [AM+02]:
					for (size_t nn = 0; nn < NN; ++nn) { //for each PARTICLE...
						double res;
						if (has_model) { //arithmetic equation
							sub_values[3] = xx(nn+1, kk);
							res = te_eval(expr_model); //evaluate expression with current substitution values
						} else { //model data points
							res = model_fit.f(tVec[kk]);
						}
						xx(nn+1, kk+1) = res + RNGU.doub(); //generate system noise
						sub_values[2] = xx(nn+1, kk+1);
						const double res_obs = te_eval(expr_obs);
						ww(nn+1, kk+1) = ww(nn+1, kk) * RNGV.pdf( zz(kk+1, 1) - res_obs ); //Ref. [AM+02] Eq. (63)
						col_weight_sum += ww(nn+1, kk+1);
					} //endfor nn
				} else { //should currently not be reachable since we don't yet read any ini key for this
					throw InvalidArgumentException("This algorithm is not supported in the particle filter. Only SIR is available for now.", AT);
				}
			} //endif nodata

			for (size_t ii = 0; ii < NN; ++ii)
				ww(ii+1, kk+1) /= col_weight_sum;

			if (path_resampling)
				resamplePaths(xx, ww, kk, RNU);

		} //endfor kk
	} catch (...) { //we could get a "nonexistent meteo parameter" error above
		if (has_model)
			te_free(expr_model);
		te_free(expr_obs);
		delete[] te_vars;
		throw;
	}

	Matrix xx_meas(TT, (size_t)1); //most probable particle
	if (estim_measure == PF_MAX_WEIGHT) { //find the highest weight and pick this path
		size_t max_row, max_col;
		for (size_t kk = 0; kk < TT; ++kk) {
			(void) ww.getCol(kk+1).maxCoeff(max_row, max_col);
			xx_meas(kk+1, 1) = xx(max_row, max_col);
		}
	} else { //average by multiplying all particles with their weights
		for (size_t kk = 0; kk < TT; ++kk) {
			xx_meas(kk+1, 1) = 0.;
			for (size_t nn = 0; nn < NN; ++nn)
				xx_meas(kk+1, 1) += xx(nn+1, kk+1) * ww(nn+1, kk+1);
		}
	}

	for (size_t kk = 0; kk < TT; ++kk) {
		sub_values[0] = (double)kk;
		sub_values[1] = tVec[kk];
		sub_values[2] = xx_meas(kk+1, 1);
		sub_values[3] = (kk == 0)? model_x0 : xx_meas(kk, 1); //somewhat arbitrary at T=0 - this substitution is meant for the system model
		for (size_t jj = 0; jj < sub_params.size(); ++jj) //fill current meteo parameters
			sub_values[jj+nr_hardcoded_sub] = ivec[kk](sub_params[jj]);
		const double res = te_eval(expr_obs); //filtered observation (model function of mean state [= estimated likely state])
		ovec[kk](param) = isNan(res)? IOUtils::nodata : res; //NaN to nodata
	}

	if (has_model)
		te_free(expr_model);
	te_free(expr_obs);
	delete[] te_vars;

	if (be_verbose && saw_nodata) std::cerr << "[W] Nodata value(s) encountered in particle filter. For this, the previous particle was repeated. You should probably resample beforehand.\n";
	if (!dump_states_file.empty())
		dumpInternalStates(xx, ww);
	if (!dump_particles_file.empty())
		dumpParticlePaths(xx);

}

/**
 * @brief Particle path resampling to mitigate degeneracy problem.
 * @details The paths may lead to very low weights that will eat up a lot of computational power and not yield the most
 * usable results. Hence, the paths can be resampled (cf. main documentation).
 * @param[in,out] xx The particles to resample.
 * @param[in,out] ww The particles' weights.
 * @param[in] RNU A generator that we keep in scope that is used for uniform random numbers.
 */
void FilterParticle::resamplePaths(Matrix& xx, Matrix& ww, const size_t& kk, RandomNumberGenerator& RNU) const
{ //if a lot of computational power is devoted to particles with low contribution (low weight), resample the paths
	if (resample_alg == PF_SYSTEMATIC) { //algorithm 2 of Ref. [AM+02]
		double N_eff = 0.; //effective sample size, Ref. [AM+02] Eq. (50)
		for (size_t nn = 0; nn < NN; ++nn)
			N_eff += ww(nn+1, kk+1)*ww(nn+1, kk+1);
		N_eff = 1. / N_eff; //a small N_eff indicates severe degeneracy

		if (N_eff < resample_percentile * (double)NN)
		{ //Strictly, SIR resamples at each point. Practically, a simple heuristic is more suitable (e. g. Ref. [DJ09])
			std::vector<double> cdf(NN);
			cdf.front() = 0.;
			for (size_t nn = 1; nn < NN; ++nn)
				cdf[nn] = cdf[nn-1] + ww(nn+1, kk+1); //construct cumulative density function
			cdf.back() = 1.0; //round-off protection

			double rr = RNU.doub() / NN;

			for (size_t nn = 0; nn < NN; ++nn) //for each PARTICLE...
			{ //at the selected index the cdf is likely to have jumped --> particle with high weight --> reuse that one
				size_t jj = 0;
				while (rr > cdf[jj])
					++jj; //check which range in the cdf the random number belongs to...
				xx(nn+1, kk+1) = xx(jj+1, kk+1); //... and use that index
				ww(nn+1, kk+1) = 1. / NN; //all resampled particles have the same weight
				rr += 1. / NN; //move along cdf
			} //note: bottleneck for parallelization since all particles must be known at this point

		} //endif N_eff

	} else {
		throw InvalidArgumentException("Resampling strategy for particle filter not implemented.", AT);
	} //endif resample_alg
}

/**
 * @brief Checks if an initial state was given and if not tries to find a valid one.
 * @details If no initial state is provided by the user, this function will run through the meteo set until it
 * finds a valid data point (i. e. not nodata) to start with.
 * @param[in] ivec Meteo data vector to check as given by .process().
 * @param[in] param Parameter index the filter runs on.
 * @return True if some valid state was found, false if everything is nodata.
 */
bool FilterParticle::checkInitialState(const std::vector<MeteoData>& ivec, const size_t& param)
{ //check for nodata values in the input meteo data
	if (model_x0 == IOUtils::nodata) {
		for (size_t kk = 0; kk < ivec.size(); ++kk) { //find 1st data element for starting point
			if (ivec[kk](param) != IOUtils::nodata) {
				model_x0 = ivec[kk](param); //if there are many the filter will run through, but the user should really resample
				break;
			}
		}
		if (be_verbose) std::cerr << "[W] No initial state value x_0 provided for particle filter; using 1st available measurement.\n";
		if (model_x0 == IOUtils::nodata) //all values are nodata
			return false;
	} //endif nodata
	return true;
}

/**
 * @brief Helper function to put substitutions in tinyexpr's format.
 * @param[out] vars tinyexpr formatted substitutions.
 * @param[in] expr List of variable names to substitute.
 * @param[in] values List of variable values to substitute for.
 */
void FilterParticle::initFunctionVars(te_variable* vars, const std::vector<std::string>& expr, const std::vector<double>& values) const
{ //build a substitutions expression for tinyexpr
	if (values.size() != expr.size())
		throw InvalidArgumentException("Particle filter: error mapping meteo(param) fields to meteo values. Are all fields available?\n", AT);

	for (size_t ii = 0; ii < expr.size(); ++ii) {
		vars[ii].name = expr[ii].c_str();
		vars[ii].address = &values.data()[ii];
		vars[ii].type = 0;
		vars[ii].context = 0;
	}
}

/**
 * @brief Helper function to call tinyexpr compilation and throw an error if this fails.
 * @param[in] expression The model equation to compile.
 * @param[in] te_vars Substitutions carried out in the model expression (variables).
 * @param[in] sz Number of substitutions.
 * @return The compiled tinyexpr expression ready to evaluate the function.
 */
te_expr* FilterParticle::compileExpression(const std::string& expression, const te_variable* te_vars, const size_t& sz) const
{ //ready the lazy expressions (with syntax check)
	int te_err;
	te_expr *expr = te_compile(expression.c_str(), te_vars, (int)sz, &te_err);
	if (!expr)
		throw InvalidFormatException("Arithmetic expression \"" + expression +
		        "\" could not be evaluated for particle filter; parse error at " + IOUtils::toString(te_err), AT);
	return expr;
}

/**
 * @brief Construct a mapping of currently used substitutions such as meteo(TA) to their respective parameter names (TA).
 * @details Substitutions can be done in the system and observation equations. This function parses them both for all
 * used substitutions, adds the hardcoded ones, and prepares the given equations in a way that they can be evaluated by
 * tinyexpr (i. e. no capital letters or brackets in subsitution names).
 * @param[in,out] line_m First model equation to parse - will be modified for "good" variable names (e. g. "meteo(TA)" -> "meteota").
 * @param[in, out] line_o Second model equation to parse - will also be modified.
 * @param[out] sub_expr Vector with all variable names that should be substitutions in tinyexpr evaluation (e. g. "meteota").
 * @param[out] sub_params Convenience vector with the names of the meteo parameters to map to (e. g. "TA").
 */
void FilterParticle::parseSubstitutionStrings(std::string& line_m, std::string& line_o, std::vector<std::string>& sub_expr,
        std::vector<std::string>& sub_params) const
{ //list names of meto parameters given by meteo(...) in input matrix
	sub_expr.clear();
	sub_params.clear();
	sub_expr.push_back("kk"); //hard-coded parameters
	sub_expr.push_back("tt");
	sub_expr.push_back("xx");
	sub_expr.push_back("x_km1");

	parseBracketExpression(line_m, sub_expr, sub_params);
	parseBracketExpression(line_o, sub_expr, sub_params);
}

/**
 * @brief Helper function to parse meteo parameters of the form meteo(XX).
 * @details Helps the parseSubstitutionStrings function and parses the meteo parameter part of the substitutions.
 * @param[in,out] line_m Model equation to parse
 * @param[out] sub_expr Vector with all variable names
 * @param[out] sub_params Vector with the names of the meteo parameters
 */
void FilterParticle::parseBracketExpression(std::string& line, std::vector<std::string>& sub_expr,
        std::vector<std::string>& sub_params) const
{
	static const std::string prefix("meteo(");
	static const size_t len = prefix.length();
	size_t pos1 = 0, pos2;

	while (true) {
		pos1 = line.find(prefix, pos1);
		if (pos1 == std::string::npos)
			break; //done
		pos2 = line.find(")", pos1+len);
		if (pos2 == std::string::npos || pos2-pos1-len == 0) //no closing bracket
			throw InvalidArgumentException("Missing closing bracket in meteo(...) part of particle filter's system model.", AT);

		const std::string pname = IOUtils::strToLower(line.substr(pos1+len, pos2-pos1-len));
		sub_params.push_back(IOUtils::strToUpper(pname)); //meteo name
		line.replace(pos1, pos2-pos1+1, prefix.substr(0, prefix.length() - 1) + pname); //to make parseable with tinyexpr: 'meteo(RH)' --> 'meteorh'
		sub_expr.push_back(prefix.substr(0, prefix.length() - 1) + pname); //full expression, lower case and without brackets; duplicates may occur
		pos1 += len;
	} //end while
}

/**
 * @brief Puts the last time step of the particle ensemble in a file.
 * @details Since for the calculations only the previous step is needed, the evolution of the states is incorporated
 * in the last step that is calculated. This is written to an output file here, and can be read back in with readInternalStates.
 * @param[in] particles The particles matrix with particles as the rows, and time steps as the columns. Last column is output.
 * @param[in] weights The weights associated with the particles.
 */
void FilterParticle::dumpInternalStates(Matrix& particles, Matrix& weights) const
{ //using this, we are able to resume our filter without having to recalculate the past if new data arrives
	std::ofstream oss(dump_states_file.c_str(), std::ofstream::out);
	if (oss.fail()) {
		std::ostringstream ss;
		ss << "Particle filter could not dump internal states to \"" << dump_states_file;
		ss << "\", possible reason: " << std::strerror(errno);
		throw AccessException(ss.str(), AT);
	}
	oss << "# This file was generated by MeteoIO's particle filter and holds the particles and weights at the last time step." << std::endl;
	oss << "# [particles]   [weights]" << std::endl;
	static const int digits = std::numeric_limits<double>::digits10;
	oss.precision(digits);
	oss.setf(std::ios::fixed);
	for (size_t ii = 0; ii < particles.getNy(); ++ii) {
		oss << std::setw(digits) << particles(ii+1, particles.getNx()) << "   ";
		oss << std::setw(digits) << weights(ii+1, weights.getNx()) << std::endl;
	}
	oss.close();
}

/**
 * @brief Reads particle paths from the file system.
 * @details Particles that were output via dumpInternalStates can be read back in by this routine. This is to
 * be able to resume the filter if new data arrives without having to recalculate the past.
 * @param[in] particles Matrix that will receive the particles of the previous run at the last time step as first column.
 * @param[in] weights Same as for particles.
 * @return True if reading was successful (file exists and has particles that fit the current settings).
 */
bool FilterParticle::readInternalStates(Matrix& particles, Matrix& weights) const
{
	std::vector<double> xx, ww;
	try {
		readCorrections(block_name, input_states_file, xx, ww);
	} catch (...) { //file not available yet - will become active on next run
		return false;
	}

	if ( (unsigned int)particles.getNy() != xx.size() || (unsigned int)weights.getNy() != ww.size() ) {
		if (be_verbose) std::cerr << "[W] Particle filter file input via INPUT_STATES_FILE does not match the number of particles. Using INITIAL_STATE.\n";
		return false;
	}

	for (size_t ii = 0; ii < particles.getNy(); ++ii) { //fill first columns
		particles(ii+1, 1) = xx[ii];
		weights(ii+1, 1) = ww[ii];
	}

	return true;
}

/**
 * @brief Write particle paths to file.
 * @details The end result of the filter aggregates the paths to get a likely estimation of the states. To have a look
 * at the evolution of the particles you can output them to the file system. In the example code in /doc/examples/ there
 * is a mini MATLAB routine hidden in a comment that visualizes the kernel density.
 * @param[in] particles Particle paths with rows denoting the particles, and columns the time steps.
 */
void FilterParticle::dumpParticlePaths(Matrix& particles) const
{ //to plot paths and kernel density outside of MeteoIO
	std::ofstream oss(dump_particles_file.c_str(), std::ofstream::out);
	if (oss.fail()) {
		std::ostringstream ss;
		ss << "Particle filter could not dump particle paths states to \"" << dump_states_file;
		ss << "\", possible reason: " << std::strerror(errno);
		throw AccessException(ss.str(), AT);
	}
	oss << "# This file was generated by MeteoIO's particle filter and holds the paths of all particles." << std::endl;
	oss << "# Rows are the particles, columns the time steps." << std::endl;
	static const int digits = std::numeric_limits<double>::digits10;
	oss << particles.toString(digits, false);
	oss.close();
}

/**
 * @brief Associate time spans with vector indices.
 * @details To evaluate functions, as well as to interpolate, it is meaningful to look at how far data points are apart
 * in time (as opposed to looking at the vector index). For this, the first data point is set to time 0, the second one
 * to time 1, and every other distance is relative to that.
 * @param[in] ivec Meteo data vector holding values and dates.
 * @return Vector holding the normalized time for each index.
 */
std::vector<double> FilterParticle::buildTimeVector(const std::vector<MeteoData>& ivec) const
{ //time representation of the index: shift and normalize to t(0)=0, t(1)=1
	double base_time = ivec.front().date.getJulian();
	double dt(1.);

	if (ivec.size() > 1)
		dt = ivec[1].date.getJulian() - base_time;

	std::vector<double> vecRet(ivec.size());
	for (size_t ii = 0; ii < ivec.size(); ++ii) {
		const double tt = (ivec[ii].date.getJulian() - base_time) / dt;
		vecRet[ii] = tt;
	}

	return vecRet;
}

/**
 * @brief Called by the processing chain to read input settings.
 * @param[in] vecArgs Vector of string-pairs holding ini keys and values.
 */
void FilterParticle::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	const std::string where("Filters::" + block_name);
	bool has_prior(false), has_obs(false);

	for (size_t ii = 0; ii < vecArgs.size(); ii++) {

		/*** MODEL FUNCTION settings ***/
		if (vecArgs[ii].first == "MODEL_FUNCTION") {
			if (vecArgs[ii].second.substr(0, 4) == "FIT ") {
				std::vector<std::string> args;
				IOUtils::readLineToVec(vecArgs[ii].second, args);
				fit_expression = args[1];
				if (args.size() > 2) //the syntax is e. g. FIT POLYNOMIAL 3
					(void) IOUtils::convertString(fit_degree, args[2]); //extra parameter if used by the fit
			} else {
				model_expression = vecArgs[ii].second;
			}
		} else if (vecArgs[ii].first == "MODEL_FIT_PARAM") {
			fit_param = vecArgs[ii].second;
		} else if (vecArgs[ii].first == "INITIAL_STATE") {
			IOUtils::parseArg(vecArgs[ii], where, model_x0);
		} else if (vecArgs[ii].first == "OBS_MODEL_FUNCTION") {
			obs_model_expression = vecArgs[ii].second;
		} else if (vecArgs[ii].first == "ESTIMATION_MEASURE") {
			if (vecArgs[ii].second == "MAX_WEIGHT")
				estim_measure = PF_MAX_WEIGHT; //all others default to PF_ESTIMATE_MEAN
		}

		/*** FILTER settings ***/
		else if (vecArgs[ii].first == "NO_OF_PARTICLES") {
			IOUtils::parseArg(vecArgs[ii], where, NN);
		} else if (vecArgs[ii].first == "PATH_RESAMPLING") {
			IOUtils::parseArg(vecArgs[ii], where, path_resampling);
		} else if (vecArgs[ii].first == "RESAMPLE_PERCENTILE") {
			IOUtils::parseArg(vecArgs[ii], where, resample_percentile);
		}

		/*** MISC settings ***/
		else if (vecArgs[ii].first == "DUMP_PARTICLES_FILE") {
			dump_particles_file = vecArgs[ii].second;
		} else if (vecArgs[ii].first == "DUMP_INTERNAL_STATES_FILE") {
			dump_states_file = vecArgs[ii].second;
		} else if (vecArgs[ii].first == "INPUT_INTERNAL_STATES_FILE") {
			input_states_file = vecArgs[ii].second;
		} else if (vecArgs[ii].first == "VERBOSE") {
			IOUtils::parseArg(vecArgs[ii], where, be_verbose);
		}

		/*** RNG settings ***/
		else if (vecArgs[ii].first == "MODEL_RNG_ALGORITHM") { //model RNG settings
			rng_model.algorithm = RandomNumberGenerator::strToRngtype(vecArgs[ii].second);
		} else if (vecArgs[ii].first == "MODEL_RNG_DISTRIBUTION") { //everything RNG will be defaulted if not provided - cf. RNG doc
			rng_model.distribution = RandomNumberGenerator::strToRngdistr(vecArgs[ii].second); //convert from int to enum RNG_DISTR
		} else if (vecArgs[ii].first == "MODEL_RNG_PARAMETERS") {
			IOUtils::readLineToVec(vecArgs[ii].second, rng_model.parameters);
		} else if (vecArgs[ii].first == "MODEL_RNG_SEED") {
			readLineToVec(vecArgs[ii].second, rng_model.seed);
		} else if (vecArgs[ii].first == "PRIOR_RNG_ALGORITHM") { //prior pdf RNG settings
			rng_prior.algorithm = RandomNumberGenerator::strToRngtype(vecArgs[ii].second);
			has_prior = true;
		} else if (vecArgs[ii].first == "PRIOR_RNG_DISTRIBUTION") {
			rng_prior.distribution = RandomNumberGenerator::strToRngdistr(vecArgs[ii].second); //convert from int to enum RNG_DISTR
			has_prior = true;
		} else if (vecArgs[ii].first == "PRIOR_RNG_PARAMETERS") {
			IOUtils::readLineToVec(vecArgs[ii].second, rng_prior.parameters);
			has_prior = true;
		} else if (vecArgs[ii].first == "PRIOR_RNG_SEED") {
			readLineToVec(vecArgs[ii].second, rng_prior.seed);
			has_prior = true;
		} else if (vecArgs[ii].first == "OBS_RNG_ALGORITHM") { //observation pdf RNG settings
			rng_obs.algorithm = RandomNumberGenerator::strToRngtype(vecArgs[ii].second);
			has_obs = true;
		} else if (vecArgs[ii].first == "OBS_RNG_DISTRIBUTION") {
			rng_obs.distribution = RandomNumberGenerator::strToRngdistr(vecArgs[ii].second); //convert from int to enum RNG_DISTR
			has_obs = true;
		} else if (vecArgs[ii].first == "OBS_RNG_PARAMETERS") {
			IOUtils::readLineToVec(vecArgs[ii].second, rng_obs.parameters);
			has_obs = true;
		} else if (vecArgs[ii].first == "OBS_RNG_SEED") {
			readLineToVec(vecArgs[ii].second, rng_obs.seed);
			has_obs = true;
		} else if (vecArgs[ii].first == "RESAMPLE_RNG_SEED") {
			readLineToVec(vecArgs[ii].second, resample_seed);
		} else {
			unrecognized_keys += vecArgs[ii].first + ", "; //constructor is always called twice - show only when processing
		}

	} //endfor vecArgs

	if (model_expression.empty()) {
		if (fit_expression.empty()) {
			throw InvalidArgumentException("No model function supplied for the particle filter. No parameter to fit a curve against given either.", AT);
		} else {
			if (fit_param.empty())
				throw InvalidArgumentException("The particle filter has been requested to fit meteo data as a model, but no parameter was given via MODEL_FIT_PARAM.", AT);
		}
	} else {
		if (!fit_expression.empty())
			if (be_verbose) std::cerr << "[W] Both a model expression and a fit parameter are given to the particle filter. Ignoring the fit.\n";
	}


	if (obs_model_expression.empty()) {
		if (be_verbose) std::cerr << "[W] Model to relate observations to state is missing for particle filter. Picking obs(k)=state(k).\n";
		obs_model_expression = "xx";
	}
	if (!has_prior) //not one prior pdf RNG setting -> pick importance density
		rng_prior = rng_model;
	if (!has_obs) //no dedicated observation noise pdf - use model for importance sampling
		rng_obs = rng_model;
}

/**
 * @brief Helper function to set random number generator seeds.
 * @details This function checks if there are seeds available from the user, and if so, sets them.
 * @param[in] RNGU 1st RNG to seed.
 * @param[in] RNGU 2nd RNG to seed.
 * @param[in] RNGU 3rd RNG to seed.
 * @param[in] RNGU 4th RNG to seed.
 */
void FilterParticle::seedGeneratorsFromIni(RandomNumberGenerator& RNGU, RandomNumberGenerator& RNGV, RandomNumberGenerator& RNG0,
        RandomNumberGenerator& RNU) const
{ //to keep process(...) less cluttered
	if (!rng_model.seed.empty()) //seed integers given in ini file
		RNGU.setState(rng_model.seed);
	if (!rng_obs.seed.empty())
		RNGV.setState(rng_obs.seed);
	if (!rng_prior.seed.empty())
		RNG0.setState(rng_prior.seed);
	if (!resample_seed.empty())
		RNU.setState(resample_seed);
}

/**
 * @brief Copy data from a meteo set into a vector.
 * @details One specified parameter of the meteo data is extracted to a double vector.
 * @param[in] vec Vector holding the meteo data as given to .process().
 * @param[out] eig Output vector with only one parameter's values.
 * @param[in] param Index of the desired meteo parameter.
 */
void FilterParticle::vecMeteoToMatrix(const std::vector<MeteoData>& vec, Matrix& mat, const unsigned int& param) const
{ //naive copy of 1 meteo parameter in STL vector to Matrix class vector
	mat.resize(vec.size(), 1);
	for (size_t ii = 0; ii < vec.size(); ++ii)
		mat(ii+1, 1) = vec[ii](param);
}

/**
 * @brief Read an ini line to a vector.
 * @details There are global versions of this in IOUtils, this one reads into an uint64_t vector and is used for
 * uint64_t random number generator seeds.
 * @param[in] line_in The space-separated input line.
 * @param[out] vec_out Vector with the split up line as elements.
 */
void FilterParticle::readLineToVec(const std::string& line_in, std::vector<uint64_t>& vec_out) const
{ //uint64 type version
	vec_out.clear();
	std::istringstream iss(line_in);
	uint64_t val;
	while (!iss.eof()) {
		iss >> std::skipws >> val;
		if (iss.fail())
			throw InvalidFormatException("Unable to parse process noise seed integers for particle filter.", AT);
		vec_out.push_back(val);
	}
}

/**
 * @brief Check if a number is 'nan'.
 * @param[in] Number to test against 'nan'.
 * @return True if the number is 'nan'.
 */
bool FilterParticle::isNan(const double& xx) const
{
	return (xx != xx);
}

} //namespace
