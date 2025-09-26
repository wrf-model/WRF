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

#ifndef FILTERMATHS_H
#define FILTERMATHS_H

#include <map>
#include <meteoio/meteoFilters/ProcessingBlock.h>
#include <meteoio/thirdParty/tinyexpr.h>

namespace mio {

/**
 * @class FilterMaths
 * @ingroup processing
 * @author Michael Reisecker
 * @date   2019-09-01
 * @brief A filter that evaluates formulas dependent on conditions.
 * @details
 *
 *
 * This filter evaluates an arithmetic expression and filters the meteo parameters to the result. You can use a number of
 * substitutions from your data and meta data.
 *
 * \ref mathsinikeys
 *
 * **Example**: Convert from degrees Rankine to Kelvin:
 * @code
 * TA::FILTER1       = MATHS
 * TA::ARG1::FORMULA = meteo(TA) * 5/9
 * @endcode
 *
 * You can define a condition that must be met for the filter to take action. The `EXPRESSION1` and `COMPARE1` keywords denote
 * two formulas and their evaluated results will be compared by the operator given with `OPERATOR1`.
 *
 * **Example**: Allow no snow below 500 m altitude:
 * @code
 * HS::FILTER1           = MATHS
 * HS::ARG1::FORMULA     = 0
 * HS::ARG1::EXPRESSION1 = altitude
 * HS::ARG1::OPERATOR1   = LT
 * HS::ARG1::COMPARE1    = 500
 * @endcode
 *
 * *Operators:* The following comparison operators are available: `LT` (less than), `LE` (less than or equal), `GT` (greater than),
 * `GE` (greater equal), `EQ` (equal), `NE` (not equal), `STRCMP` (string comparison), `STRBEG` (string comparison at the beginning
 * of a string), `STREND` (string comparison at the end of a string), and `STRINC` (search in string).
 *
 * *Expressions:* Arithmetic expressions are evaluated with tinyexpr (<a href="https://github.com/codeplea/tinyexpr#functions-supported">
 * developer's repo</a>). You can use the standard arithmetic operations, angular functions, exponentials, and some combinatorics.
 *
 * @note Here is a full list of the available functions: addition (`+`), subtraction/negation (`-`), multiplication (`*`), division (`/`),
 * exponentiation (`^`), modulus (`%`), `abs`, `acos`, `asin`, `atan`, `atan2`, `ceil`, `cos`, `cosh`, `exp`, `floor`, `ln` (calls to log),
 * `log` (calls to log10), `log10`, `pow`, `sin`, `sinh`, `sqrt`, `tan`, `tanh`, `fac`, `ncr`, `npr`. It also features the constants `e` and `pi`.
 *
 * *Substitutions:* The following substitutions are available in all arithmetic expressions:
 *  - `meteo(PARAM)` where `PARAM` is any available meteo parameter,
 *  - `year`, `month`, `day`, `hour`, `minute`, `julian` for the measurement's date,
 *  - `altitute`, `latitude`, `longitude`, `easting`, `northing` for the station's position,
 *  - `angle`, `azimuth` for slope parameters,
 *  - `stdpress`, `stdtemp`, `boltzmann` for (meteorological) constants,
 *  - `nodata` for the nodata value (usually -999).
 *
 * The following substitutions are available in string comparisons:
 *  - `stationid`, `stationname` for station identification.
 *
 * **Example:** Add offset to specific station:
 * @code
 * HS::FILTER1           = MATHS
 * HS::ARG1::FORMULA     = meteo(HS) + 0.38
 * HS::ARG1::EXPRESSION1 = stationid
 * HS::ARG1::OPERATOR1   = STRCMP
 * HS::ARG1::COMPARE     = ISEE3
 * @endcode
 *
 * If the condition is _not_ met, you can use a different expression to evaluate via the `FORMULA_ELSE` key.
 *
 * **Example:** If there is no wind direction available then throw away the wind speed. Else, use a logarithmic scale:
 * @code
 * VW::FILTER1            = MATHS
 * VW::ARG1::FORMULA      = nodata
 * VW::ARG1::FORMULA_ELSE = log(meteo(VW))
 * VW::ARG1::EXPRESSION1  = meteo(DW)
 * VW::ARG1::OPERATOR1    = EQ
 * VW::ARG1::COMPARE1     = nodata
 * @endcode
 *
 * You can make use of a simple logic parser. You can supply as many conditions as you wish, but they are
 * always combined with the same logical operator, namely `CONNECTIVE` can be `AND` (default) or `OR`.
 *
 * **Example**: Allow no snow between June and August:
 * @code
 * HS::FILTER1           = MATHS
 * HS::ARG1::FORMULA     = 0
 * HS::ARG1::EXPRESSION1 = month
 * HS::ARG1::OPERATOR1   = GE
 * HS::ARG1::COMPARE1    = 6
 * HS::ARG1::EXPRESSION2 = month ;AND is implied
 * HS::ARG1::OPERATOR1   = LE
 * HS::ARG1::COMPARE1    = 8
 * @endcode
 *
 * Or alternatively:
 * @code
 * HS::FILTER1           = MATHS
 * HS::ARG1::FORMULA     = 0
 * HS::ARG1::CONNECTIVE  = OR ;switch logical connective to OR for all
 * HS::ARG1::EXPRESSION1 = month
 * HS::ARG1::OPERATOR1   = EQ
 * HS::ARG1::COMPARE1    = 6
 * HS::ARG1::EXPRESSION2 = month
 * HS::ARG1::OPERATOR2   = EQ
 * HS::ARG1::COMPARE2    = 7
 * HS::ARG1::EXPRESSION3 = month
 * HS::ARG1::OPERATOR3   = EQ
 * HS::ARG1::COMPARE3    = 8
 * @endcode
 *
 * **Important:** Save for the `SKIP_NODATA` keyword (see below), nodata values (usually -999) are kept in the calculations and so
 * far it is up to the user to check against this!
 *
 * **Example:** Approximate the dew point only if air temperature and relative humidity are both available:
 * @code
 * TD::FILTER1            = MATHS
 * TD::ARG1::FORMULA      = meteo(TA) - ((100 - meteo(RH)*100) / 5)
 * TD::ARG1::FORMULA_ELSE = nodata
 * TD::ARG1::EXPRESSION1  = meteo(RH)
 * TD::ARG1::OPERATOR1    = NE
 * TD::ARG1::COMPARE1     = nodata
 * TD::ARG1::EXPRESSION2  = meteo(TA)
 * TD::ARG1::OPERATOR2    = NE
 * TD::ARG1::COMPARE2     = nodata
 * TD::ARG1::EXPRESSION3  = meteo(RH)
 * TD::ARG1::OPERATOR3    = GT
 * TD::ARG1::COMPARE3     = 0.5 ;formula within 1 degree only for RH > 50 %
 * @endcode
 *
 * **Example:** Two stations are called ISEE1 and ISEE2. Suppose their humidity sensors start reporting garbage on 1st of August 2019 and
 * we want to make sure they are not used in any calculations:
 * @code
 * RH::FILTER1           = MATHS
 * RH::ARG1::FORMULA     = nodata ;throw all data away
 * RH::ARG1::EXPRESSION1 = stationid
 * RH::ARG1::OPERATOR1   = STRBEG ;compare beginning of string
 * RH::ARG1::COMPARE1    = ISEE ;this string is as-is, i. e. no substitutions
 * RH::ARG1::EXPRESSION2 = julian
 * RH::ARG1::OPERATOR2   = GE
 * RH::ARG1::COMPARE2    = 2458696.5 ;2019-08-01
 * @endcode
 *
 * With the `ASSIGN` key you can provide a parameter that the result is saved to (instead of the one the filter runs on).
 *
 * **Important:** For technical reasons, this key only works as expected if it's the last filter that is run on the set.
 *
 * **Example:** A copy-heavy hack to make a `SHADE` processor run on a specific station only - without even
 * calculating the heavy sun stuff for others (we must control the filter ordering to be able to do this):
 * @code
 * [INPUT]
 * ISWR_TEMP::COPY               = ISWR ;CREATE would refill
 *
 * [FILTERS]
 * ISWR_TEMP::FILTER1            = MATHS ;keep only if station name matches
 * ISWR_TEMP::ARG1::FORMULA      = meteo(ISWR_TEMP)
 * ISWR_TEMP::ARG1::FORMULA_ELSE = nodata
 * ISWR_TEMP::ARG1::EXPRESSION1  = stationname
 * ISWR_TEMP::ARG1::OPERATOR1    = STRCMP
 * ISWR_TEMP::ARG1::COMPARE1     = Seegrube

 * ISWR_TEMP::FILTER2            = SHADE ;only 1 station has non-nodata values

 * ISWR_TEMP::FILTER3            = MATHS ;copy back if not nodata
 * ISWR_TEMP::ARG3::FORMULA      = meteo(ISWR_TEMP)
 * ISWR_TEMP::ARG3::FORMULA_ELSE = meteo(ISWR)
 * ISWR_TEMP::ARG3::EXPRESSION1  = meteo(ISWR_TEMP)
 * ISWR_TEMP::ARG3::OPERATOR1    = NE
 * ISWR_TEMP::ARG3::COMPARE1     = nodata
 * ISWR_TEMP::ARG3::ASSIGN       = meteo(ISWR) ;must be last filter to work!
 * @endcode
 *
 * @note With the same method you could build a chain like `if (A or B) and C` if you must.
 *
 * The last remaining key is `SKIP_NODATA` to transport nodata values unaltered from input to output. It is false by default
 * which means that all formulas are evaluated with it (and you could catch it with `meteo(XX) EQ nodata`).
 *
 * @section mathsinikeys List of ini keys
 * <table>
 *  <tr><th>Keyword</th><th>Meaning</th><th>Optional</th><th>Default Value</th></tr>
 *  <tr><td>FORMULA</td><td>Arithmetic expression to evaluate for meteo parameter.</td><td>no</td><td>-</td></tr>
 *  <tr><td>FORMULA_ELSE</td><td>Expression if the conditions below are *not* met.</td><td>yes</td><td>empty, meaning the data point is left alone</td></tr>
 *  <tr><td>CONNECTIVE</td><td>Logical connective for the conditions below.</td><td>yes</td><td>AND (choices: OR)</td></tr>
 *  <tr><td>EXPRESSION#</td><td>1st part of condition: Expression that will be compared.</td><td>yes, if  neither OPERATOR# nor COMPARE# exist.</td><td>empty, meaning FORMULA will always apply</td></tr>
 *  <tr><td>OPERATOR#</td><td>2nd part of condition: Comparison operator.</td><td>yes, if neither EXPRESSION# nor COMPARE# exist.</td><td>empty (choices: LT, GT, EQ, ...)</td></tr>
 *  <tr><td>COMPARE#</td><td>3rd part of condition: EXPRESSION is compared against this.</td><td>yes, if neither EXPRESSION# nor OPERATOR# exist.</td><td>empty</td></tr>
 *  <tr><td>ASSIGN</td><td>Change parameter to store final result in (only if it's the last filter running).</td><td>yes</td><td>empty (choices: meteo(PARAM) or PARAM)</td></tr>
 *  <tr><td>SKIP_NODATA</td><td>Any input nodata stays untouched. Checks for nodata in COMPARE will not be called.</td><td>yes</td><td>false</td></tr>
 * </table>
 *
 */

class FilterMaths : public ProcessingBlock {
	public:
		FilterMaths(const std::vector< std::pair<std::string, std::string> >& vecArgs,
		    const std::string& name);
		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		    std::vector<MeteoData>& ovec);

	private:
		bool assertCondition(const double& condition_value, const double& condition_compare,
		        const std::string& condition_operator) const;
		bool assertStringCondition(const std::string& line1, const std::string& line2, const std::string& op);
		std::map<std::string, double> parseBracketExpression(std::string& line);
		void buildSubstitutions();
		void doSubstitutions(const std::vector<MeteoData>& ivec, const size_t& idx);
		std::string doStringSubstitutions(const std::string& line_in, const MeteoData& ielem) const;
		te_expr* compileExpression(const std::string& expression, const te_variable* te_vars, const size_t& sz) const;
		void initExpressionVars(te_variable* vars);
		void parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs);
		bool isNan(const double& xx) const;
		void checkOperator(const std::string& op);

		struct logic_eq {
			std::string expression;
			std::string op;
			std::string compare;
			logic_eq() : expression(""), op(""), compare("") {}
		};
		std::map<size_t, logic_eq> logic_equations; //collection of conditions the user supplies
		std::map<std::string, double> substitutions; //fixed memory for substitutions

		std::string formula; //calculation expression
		std::string formula_else; //expression to use when evaluating to false
		std::string connective; //AND or OR
		std::string assign_param; //assign final value to this parameter

		bool skip_nodata; //continue on nodata early on
};

} //namespace

#endif // FILTERMATHS_H
