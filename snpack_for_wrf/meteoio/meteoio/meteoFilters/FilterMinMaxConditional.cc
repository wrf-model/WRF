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

#include <meteoio/meteoFilters/FilterMinMaxConditional.h>

using namespace std;

namespace mio {

FilterMinMaxConditional::FilterMinMaxConditional(const std::vector< std::pair<std::string,
    std::string> >& vecArgs, const std::string& name) :
    ProcessingBlock(vecArgs, name), min_val(IOUtils::nodata), max_val(IOUtils::nodata), min_soft(0.), max_soft(0.),
    nodata_reset(IOUtils::nodata), is_soft(false), condition_param(""), condition_operator(""), condition_compare(0.),
    ignore_missing_param(false)
{
	parse_args(vecArgs);
	properties.stage = ProcessingProperties::both;
}

void FilterMinMaxConditional::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
    std::vector<MeteoData>& ovec)
{
	const std::string where("Filters::" + block_name);
	ovec = ivec;
	for (size_t ii = 0; ii < ovec.size(); ++ii) {
		if (!ivec[ii].param_exists(condition_param)) { //allow to run over invalid access
			if (ignore_missing_param) continue;
			throw UnknownValueException("No parameter " + condition_param + " at " +
			    ivec[ii].date.toString(Date::ISO) + " for " + where, AT);
		}

		const double cond_val = ivec[ii](condition_param);
		double& val = ovec[ii](param);
		if (val == IOUtils::nodata) continue; //preserve nodata values

		if (assert_condition(cond_val)) {
			if ( (val < min_val) && (min_val != IOUtils::nodata) ) //min_val & max_val are optional
				val = is_soft? min_soft : IOUtils::nodata;
			else if ( (val > max_val) && (max_val != IOUtils::nodata) )
				val = is_soft? max_soft : IOUtils::nodata;
			else if ( (min_val == IOUtils::nodata) && (max_val == IOUtils::nodata) )
				val = nodata_reset; //min and max not supplied -> always filter
		}
	} //end for
}

/**
 * @brief Check via 2nd parameter if the filter should run
 * @param[in] condition_value Value of the 2nd parameter at current time step
 * @return `True` if the min/max filter should be applied
 */
bool FilterMinMaxConditional::assert_condition(const double& condition_value)
{
	const std::string where("Filters::" + block_name);
	if (condition_operator == "LT") {
		if (condition_value < condition_compare) return true;
	} else if (condition_operator == "LE") {
		if (condition_value <= condition_compare) return true;
	} else if (condition_operator == "GT") {
		if (condition_value > condition_compare) return true;
	} else if (condition_operator == "GE") {
		if (condition_value >= condition_compare) return true;
	} else if (condition_operator == "EQ") {
		if (condition_value == condition_compare) return true;
	} else if (condition_operator == "NE") {
		if (condition_value != condition_compare) return true;
	} else {
		throw InvalidArgumentException("Unknown comparison operator " + condition_operator + " for " + where, AT);
	}

	return false;
}

void FilterMinMaxConditional::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	const std::string where("Filters::" + block_name);
	bool has_min(false), has_max(false);
	bool has_min_reset(false), has_max_reset(false);
	bool has_parameter(false), has_operator(false), has_compare(false);

	for (size_t ii=0; ii<vecArgs.size(); ++ii) {
		if (vecArgs[ii].first == "SOFT") {
			IOUtils::parseArg(vecArgs[ii], where, is_soft);
		} else if (vecArgs[ii].first == "MIN") {
			IOUtils::parseArg(vecArgs[ii], where, min_val);
			has_min = true;
		} else if (vecArgs[ii].first == "MIN_RESET") {
			IOUtils::parseArg(vecArgs[ii], where, min_soft);
			has_min_reset = true;
		} else if (vecArgs[ii].first == "MAX") {
			IOUtils::parseArg(vecArgs[ii], where, max_val);
			has_max = true;
		} else if (vecArgs[ii].first == "MAX_RESET") {
			IOUtils::parseArg(vecArgs[ii], where, max_soft);
			has_max_reset = true;
		} else if (vecArgs[ii].first == "RESET") {
			IOUtils::parseArg(vecArgs[ii], where, nodata_reset);
		} else if (vecArgs[ii].first == "PARAMETER") {
			IOUtils::parseArg(vecArgs[ii], where, condition_param);
			has_parameter = true;
		} else if (vecArgs[ii].first == "OPERATOR") {
			IOUtils::parseArg(vecArgs[ii], where, condition_operator);
			has_operator = true;
		} else if (vecArgs[ii].first == "COMPARE") {
			IOUtils::parseArg(vecArgs[ii], where, condition_compare);
			has_compare = true;
		} else if (vecArgs[ii].first == "IGNORE_MISSING") {
			IOUtils::parseArg(vecArgs[ii], where, ignore_missing_param);
		}
	}

	//lenient check for non-optional keywords:
	if (!has_parameter)
		throw InvalidArgumentException("Please provide a PARAMETER to compare against for " + where, AT);
	if ( (has_operator && !has_compare) || (has_compare && !has_operator) )
		throw InvalidArgumentException("If you provide a comparison OPERATOR, you must provide a COMPARE value and vice versa for " + where, AT);
	if (is_soft && !has_min && !has_max && !has_min_reset && !has_max_reset) //if min is filtered but only max present, then this is up to the user
		throw InvalidArgumentException("For a SOFT " + where + " you need to supply MIN, MAX, MIN_RESET or MAX_RESET.", AT);

	if (!has_compare) { //if one is missing, both are missing (assertion above)
		condition_operator = "EQ"; //default: filter if param=nodata
		condition_compare = IOUtils::nodata;
	}

	if (!has_min_reset) //filter to filter limits
		min_soft = min_val;
	if (!has_max_reset)
		max_soft = max_val;
}

} //namespace
