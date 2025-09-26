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

#include <meteoio/IOUtils.h>
#include <meteoio/meteoLaws/Meteoconst.h>
#include <meteoio/meteoFilters/FilterMaths.h>

using namespace std;

namespace mio {

FilterMaths::FilterMaths(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name) :
        ProcessingBlock(vecArgs, name), logic_equations(), substitutions(),
        formula(""), formula_else(""), connective("AND"), assign_param(""), skip_nodata(false)
{
	parse_args(vecArgs);
	properties.stage = ProcessingProperties::first;
	buildSubstitutions(); //collect all substitutions at the beginning
}

/**
 * @brief The filtering routine as called by the processing toolchain.
 * @param[in] param Parameter index the filter is asked to run on by the user.
 * @param[in] ivec Meteo data to filter.
 * @param[out] ovec Filtered meteo data. Cf. main documentation.
 */
void FilterMaths::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
    std::vector<MeteoData>& ovec)
{
	ovec = ivec;

	//We init all of this not in the constructor but here to have more control over the memory management.
	te_variable *te_vars = new te_variable[substitutions.size()]; //substitutions are built in constructor
	initExpressionVars(te_vars); //build te_variables from substitution vectors
	te_expr *expr_formula = compileExpression(formula, te_vars, substitutions.size()); //main formula
	te_expr *expr_formula_else = NULL; //only compile if available
	if (!formula_else.empty()) {
		try {
			expr_formula_else = compileExpression(formula_else, te_vars, substitutions.size());
		} catch (...) { //roll back main formula on error
			te_free(expr_formula);
			delete[] te_vars;
			throw;
		}
	}

	//logic_equations is input directly from the ini file. The following is storage for the same pairs of expressions in compiled
	//form, where the index matches the one from logic_equations:
	std::map< size_t, std::pair<te_expr*, te_expr*> > compiled_expressions;

	std::map<size_t, logic_eq>::iterator it;
	for (it = logic_equations.begin(); it != logic_equations.end(); ++it) {
		if (it->second.op.substr(0, 3) != "STR") { //nothing to compile for string evaluations
			bool arrived_at_second(false);
			try {
				compiled_expressions[it->first].first = compileExpression(it->second.expression, te_vars, substitutions.size());
				arrived_at_second = true; //flag to check which compilation failed
				compiled_expressions[it->first].second = compileExpression(it->second.compare, te_vars, substitutions.size());
			} catch (...) { //unroll all previous expression compilations
				if (arrived_at_second)
					te_free(compiled_expressions[it->first].first); //.compare has an error -> clean .expression
				for (; it-- != logic_equations.begin(); ) {
					te_free(compiled_expressions[it->first].first);
					te_free(compiled_expressions[it->first].second);
				}
				te_free(expr_formula);
				if (!formula_else.empty())
					te_free(expr_formula_else);
				delete[] te_vars;
				throw;
			} //end try
		} //endif STR
	}

	for (size_t ii = 0; ii < ivec.size(); ++ii) {
		if (skip_nodata && ivec[ii](param) == IOUtils::nodata)
			continue; //not even a comparison to nodata is evaluated with this key

		doSubstitutions(ivec, ii); //do all substitutions
		bool current_logic; //iterative result of AND resp. OR operations
		if (logic_equations.empty())
			current_logic = true; //no conditions --> always evaluate to true
		else
			current_logic = (connective == "OR"? false : true); //default: AND

		for (it = logic_equations.begin(); it != logic_equations.end(); ++it) {
			bool cond;
			if (it->second.op.substr(0, 3) != "STR") { //arithmetic evaluation
				double res_ex, res_cond;
				res_ex = te_eval(compiled_expressions[it->first].first); //condition expression
				res_cond = te_eval(compiled_expressions[it->first].second); //comparison expression
				cond = assertCondition(res_ex, res_cond, it->second.op); //evaluate the complete condition
			} else { //string evaluation
				const std::string tmp_exp( doStringSubstitutions(it->second.expression, ivec[ii]) );
				cond = assertStringCondition(tmp_exp, it->second.compare, it->second.op);
			}

			if (connective == "OR")
				current_logic = current_logic || cond;
			else
				current_logic = current_logic && cond;
		}

		double result;
		if (current_logic) { //conditions evaluated to true together
			result = te_eval(expr_formula);
		} else { //conditions evaluated to false together
			if (!formula_else.empty())
				result = te_eval(expr_formula_else);
			else
				result = ivec[ii](param); //default: unchanged
		}

		if (assign_param.empty()) { //output to same parameter as the filter runs on
			ovec[ii](param) = isNan(result)? IOUtils::nodata : result;
		} else { //output to a different parameter
			ovec[ii].addParameter(assign_param); //NOTE: will not be respected by filters running afterwards
			ovec[ii](assign_param) = isNan(result)? IOUtils::nodata : result; //--> must be last filter!
		}

	} //endfor ii

	std::map< size_t, std::pair<te_expr*, te_expr*> >::iterator it_expr; //cleanup
	for (it_expr = compiled_expressions.begin(); it_expr != compiled_expressions.end(); ++it_expr) {
		te_free(it_expr->second.first);
		te_free(it_expr->second.second);
	}
	te_free(expr_formula);
	if (!formula_else.empty())
		te_free(expr_formula_else);
	delete[] te_vars;
}

/**
 * @brief Test two values with a comparison operator.
 * @param[in] condition_value 1st part of condition: the value to compare against.
 * @param[in] condition_compare 2nd part of condition: the value to compare.
 * @param[in] condition_operator 3rd part of condition: the comparison operator.
 * @return if (condition_value condition_operator conditon_compare) then true.
 */
bool FilterMaths::assertCondition(const double& condition_value, const double& condition_compare,
        const std::string& condition_operator) const
{
	if (condition_operator == "LT") {
		if (condition_value < condition_compare)
			return true;
	} else if (condition_operator == "LE") {
		if (condition_value <= condition_compare)
			return true;
	} else if (condition_operator == "GT") {
		if (condition_value > condition_compare)
			return true;
	} else if (condition_operator == "GE") {
		if (condition_value >= condition_compare)
			return true;
	} else if (condition_operator == "EQ") {
		if (condition_value == condition_compare)
			return true;
	} else if (condition_operator == "NE") {
		if (condition_value != condition_compare)
			return true;
	}
	return false;
}

/**
 * @brief Compare two strings with a custom operator.
 * @details With STRCMP the string must match completely; for STRBEG line2 must be the start of line1; for STREND
 * line2 must be at the end of line1; for STRINC line2 must be found in line1.
 * @param[in] line1 Line to compare against.
 * @param[in] line2 Line to compare with.
 * @param[in] op Comparison operator.
 */
bool FilterMaths::assertStringCondition(const std::string& line1, const std::string& line2, const std::string& op)
{
	if (op == "STRCMP") {
		if (line1 == line2)
			return true;
	} else if (op == "STRBEG") { //is line2 start of line1?
		if (line1.substr(0, line2.length()) == line2)
			return true;
	} else if (op == "STREND") { //is line2 end of line1?
		if (line1.length() >= line2.length()) {
			if (line1.substr(line1.length() - line2.length()) == line2)
				return true;
		}
	} else if (op == "STRINC") { //is line2 found in line1?
		if (IOUtils::count(line1, line2) > 0) //it is secured that neither string is emtpy
			return true;
	}
	return false;
}

/**
 * @brief Searches a string for meteo(XX) subsitutions.
 * @details Found substitutions are saved to a map, and the input lines are modified to fit
 * tinyexpr substitution syntax (i. e. no capital letters, no brackets).
 * @param[in] line Input string to parse.
 * @return A map with the substitution strings as keys, and nodata as starting values.
 */
std::map<std::string, double> FilterMaths::parseBracketExpression(std::string& line)
{
	const std::string where("Filters::" + block_name);
	std::map<std::string, double> mapRet;
	static const std::string prefix("meteo(");
	static const size_t len = prefix.length();

	size_t pos1 = 0, pos2;

	while (true) {
		pos1 = line.find(prefix, pos1);
		if (pos1 == std::string::npos)
			break; //done
		pos2 = line.find(")", pos1 + len);
		if (pos2 == std::string::npos || pos2-pos1-len == 0) //no closing bracket
			throw InvalidArgumentException("Missing closing bracket in meteo(...) substitution for " + where, AT);

		const std::string pname( IOUtils::strToLower(line.substr(pos1+len, pos2-pos1-len)) );
		line.replace(pos1, pos2-pos1+1, prefix.substr(0, prefix.length() - 1) + pname); //to make parseable with tinyexpr: 'meteo(RH)' --> 'meteorh'
		mapRet[prefix.substr(0, prefix.length() - 1) + pname] = IOUtils::nodata; //full expression, lower case and without brackets
		pos1 += len;
	} //end while

	return mapRet; //this is a map of all substitutions found in line
}

/**
 * @brief Run through all arithmetic equations in ini file and parse substitutions.
 */
void FilterMaths::buildSubstitutions()
{
	substitutions.clear();

	//main formulas:
	std::map<std::string, double> formula_items( parseBracketExpression(formula) );
	substitutions.insert(formula_items.begin(), formula_items.end());
	formula_items = parseBracketExpression(formula_else);
	substitutions.insert(formula_items.begin(), formula_items.end());

	//logical expressions:
	map<size_t, logic_eq>::iterator it;
	for (it = logic_equations.begin(); it != logic_equations.end(); ++it) {
		std::map<std::string, double> items;
		items = parseBracketExpression(it->second.expression);
		substitutions.insert(items.begin(), items.end());
		items = parseBracketExpression(it->second.compare);
		substitutions.insert(items.begin(), items.end());
	}

	//hardcoded part:
	static const size_t nr_sub = 14;
	static const std::string sub[nr_sub] = {"nodata", "year", "month", "day", "hour", "minute", "julian",
	        "altitude", "azimuth", "slope", "latitude", "longitude", "easting", "northing"};
	for (size_t ii = 0; ii < nr_sub; ++ii) {
		std::pair<std::string, double> item(sub[ii], IOUtils::nodata);
		substitutions.insert(item);
	}
	substitutions["stdpress"] = Cst::std_press; //constant parameters
	substitutions["stdtemp"] = Cst::std_temp;
	substitutions["boltzmann"] = Cst::stefan_boltzmann;

	/*
	 * Now 'substitutions' is a global map with all desired substitution strings as keys.
	 * Some constant values are filled here once, the rest is filled per time step.
	 * In any case it is fixed memory that we can point to for tinyexpr.
	 */
}

/**
 * @brief Perform substitutions for all keys in the global map that do not have constant values.
 */
void FilterMaths::doSubstitutions(const std::vector<MeteoData>& ivec, const size_t& idx)
{
	int year, month, day, hour, minute;
	ivec[idx].date.getDate(year, month, day, hour, minute);
	substitutions["year"] = (double)year;
	substitutions["month"] = (double)month;
	substitutions["day"] = (double)day;
	substitutions["hour"] = (double)hour;
	substitutions["minute"] = (double)minute;
	substitutions["julian"] = ivec[idx].date.getJulian();
	substitutions["altitude"] = ivec[idx].meta.getAltitude();
	substitutions["azimuth"] = ivec[idx].meta.getAzimuth();
	substitutions["slope"] = ivec[idx].meta.getSlopeAngle();
	substitutions["latitude"] = ivec[idx].meta.getPosition().getLat();
	substitutions["longitude"] = ivec[idx].meta.getPosition().getLon();
	substitutions["easting"] = ivec[idx].meta.getPosition().getEasting();
	substitutions["northing"] = ivec[idx].meta.getPosition().getNorthing();
	//TODO: max, min?

	std::map<std::string, double>::iterator it_sub;
	for (it_sub = substitutions.begin(); it_sub != substitutions.end(); ++it_sub) {
		if (it_sub->first.substr(0, 5) == "meteo") {
			const size_t param_idx = ivec[idx].getParameterIndex( IOUtils::strToUpper(it_sub->first.substr(5)) );
			if (param_idx == IOUtils::npos) //parameter unavailable, nodata instead of error
				it_sub->second = IOUtils::nodata;
			else
				it_sub->second = ivec[idx](param_idx); //place values in tinyexpr memory location
		} //endif "meteo"
	} //endfor it_sub
}

/**
 * @brief Perform substitutions for string comparisons.
 * @param[in] line_in String to do substitutions in.
 * @param[in] md Current meteo data point.
 * @return String with substituted values.
 */
std::string FilterMaths::doStringSubstitutions(const std::string& line_in, const MeteoData& md) const
{
	std::string line(line_in);
	IOUtils::replace_all(line, "stationid", md.getStationID());
	IOUtils::replace_all(line, "stationname", md.meta.stationName);
	return line;
}

/**
 * @brief Helper function to call tinyexpr compilation and throw an error if this fails.
 * @param[in] expression The arithmetic equation to compile.
 * @param[in] te_vars Substitutions carried out in the model expression (variables).
 * @param[in] sz Number of substitutions.
 * @return The compiled tinyexpr expression ready to evaluate the function.
 */
te_expr* FilterMaths::compileExpression(const std::string& expression, const te_variable* te_vars, const size_t& sz) const
{ //ready the lazy expressions (with syntax check)
	const std::string where("Filters::" + block_name);
	int te_err;
	te_expr *expr = te_compile(expression.c_str(), te_vars, (int)sz, &te_err);
	if (!expr)
		throw InvalidFormatException("Arithmetic expression \"" + expression +
		        "\" could not be evaluated for " + where + "; parse error at " + IOUtils::toString(te_err), AT);
	return expr;
}

/**
 * @brief Helper function to put substitutions in tinyexpr's format.
 * @details This will iterate through the global substitutions map and put the substitutions in tinyexpr's format.
 * @param[out] vars Tinyexpr formatted substitutions.
 */
void FilterMaths::initExpressionVars(te_variable* vars)
{ //build a substitutions expression for tinyexpr

	std::map<std::string, double>::iterator it;
	size_t cc = 0;
	for (it = substitutions.begin(); it != substitutions.end(); ++it) {
		vars[cc].name = it->first.c_str();
		vars[cc].address = &it->second;
		vars[cc].type = 0;
		vars[cc].context = 0;
		cc++;
	}
}

/**
 * @brief Called by the processing chain to read input settings.
 * @param[in] vecArgs Vector of string-pairs holding ini keys and values.
 */
void FilterMaths::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	const std::string where("Filters::" + block_name);

	for (size_t ii=0; ii<vecArgs.size(); ++ii) {
		if (vecArgs[ii].first == "FORMULA") {
			formula = vecArgs[ii].second;
		} else if (vecArgs[ii].first == "FORMULA_ELSE") {
			formula_else = vecArgs[ii].second;
		} else if (vecArgs[ii].first == "CONNECTIVE") {
			connective = IOUtils::strToUpper(vecArgs[ii].second);
		} else if (vecArgs[ii].first.substr(0, 10) == "EXPRESSION") {
			size_t eq_nr; //the number following EXPRESSION
			const bool convert_success = IOUtils::convertString(eq_nr, vecArgs[ii].first.substr(10));
			if (!convert_success || vecArgs[ii].first.length() == 10)
				throw InvalidArgumentException("Could not enumerate \"" + vecArgs[ii].first + "\" in " + where + "; the format is \"EXPRESSION<nr>\".", AT);
			logic_equations[eq_nr].expression = vecArgs[ii].second;
		} else if (vecArgs[ii].first.substr(0, 8) == "OPERATOR") {
			size_t eq_nr;
			const bool convert_success = IOUtils::convertString(eq_nr, vecArgs[ii].first.substr(8));
			if (!convert_success || vecArgs[ii].first.length() == 8)
				throw InvalidArgumentException("Could not enumerate \"" + vecArgs[ii].first + "\" in " + where + "; the format is \"OPERATOR<nr>\".", AT);
			checkOperator(vecArgs[ii].second); //throws if the operator is unknown
			logic_equations[eq_nr].op = vecArgs[ii].second;
		} else if (vecArgs[ii].first.substr(0, 7) == "COMPARE") {
			size_t eq_nr;
			const bool convert_success = IOUtils::convertString(eq_nr, vecArgs[ii].first.substr(7));
			if (!convert_success || vecArgs[ii].first.length() == 7)
				throw InvalidArgumentException("Could not enumerate \"" + vecArgs[ii].first + "\" in " + where + "; the format is \"COMPARE<nr>\".", AT);
			logic_equations[eq_nr].compare = vecArgs[ii].second;
		} else if (vecArgs[ii].first == "ASSIGN") {
			assign_param = vecArgs[ii].second;
			IOUtils::replace_all(assign_param, "meteo(", "");
			IOUtils::replace_all(assign_param, ")", "");
			IOUtils::toUpper(assign_param);
		} else if (vecArgs[ii].first == "SKIP_NODATA") {
			IOUtils::parseArg(vecArgs[ii], where, skip_nodata);
		}
	} //endfor ii

	if (formula.empty())
		throw InvalidArgumentException("No FORMULA given for " + where, AT);
	if (connective != "AND" && connective != "OR")
		throw InvalidArgumentException("Logical connective \"" + connective + "\"unknown for " + where + "; only AND and OR are available.", AT);

	map<size_t, logic_eq>::iterator it;
	for (it = logic_equations.begin(); it != logic_equations.end(); ++it) //check integrity of each expression
	{
		if (it->second.expression.empty() || it->second.op.empty() || it->second.compare.empty())
			throw InvalidArgumentException("Logical expression number " + IOUtils::toString(it->first) + " is missing either EXPRESSION, OPERATOR, or COMPARE.", AT);
	}
}

/**
 * @brief Check if a number is 'nan'.
 * @param[in] Number to test against 'nan'.
 * @return True if the number is 'nan'.
 */
bool FilterMaths::isNan(const double& xx) const
{
	return (xx != xx);
}

/**
 * @brief Test if the given operator is known.
 * @param[in] op The operator in the ini file.
 */
void FilterMaths::checkOperator(const std::string& op)
{
	const std::string where("Filters::" + block_name);
	static const size_t nr_op_test = 10;
	static const std::string op_test[nr_op_test] = {"LT", "LE", "GT", "GE", "EQ", "NE", "STRCMP", "STRBEG", "STREND", "STRINC"};
	bool found(false);
	for (size_t jj = 0; jj < nr_op_test; ++jj) {
		if (op == op_test[jj]) {
			found = true;
			break;
		}
	}
	if (!found)
		throw InvalidArgumentException("Operator \"" + op + "\" not known for " + where, AT);
}

} //namespace
