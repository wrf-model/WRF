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
#include <meteoio/dataClasses/MeteoData.h>
#include <meteoio/dataClasses/StationData.h>
#include <meteoio/IOUtils.h>

#include <cmath>
#include <limits>
#include <iomanip>
#include <sstream>

using namespace std;
namespace mio {

/************************************************************
 * static section                                           *
 ************************************************************/
const size_t MeteoGrids::nrOfParameters =  MeteoGrids::lastparam - MeteoGrids::firstparam + 1;
std::vector<std::string> MeteoGrids::paramname, MeteoGrids::description, MeteoGrids::units;
const bool MeteoGrids::__init = MeteoGrids::initStaticData();

bool MeteoGrids::initStaticData()
{
	//the order must be the same as in the enum
	paramname.push_back("TA");					description.push_back("Air Temperature");						units.push_back("K");
	paramname.push_back("RH");					description.push_back("Relative Humidity");					units.push_back("-");
	paramname.push_back("QI");					description.push_back("Specific Humidity");					units.push_back("g/g");
	paramname.push_back("TD");					description.push_back("Dew Point Temperature");			units.push_back("K");
	paramname.push_back("VW");				description.push_back("Wind Velocity");							units.push_back("m/s");
	paramname.push_back("DW");				description.push_back("Wind Direction");						units.push_back("°");
	paramname.push_back("VW_MAX");		description.push_back("Gust Wind Velocity");					units.push_back("m/s");
	paramname.push_back("ISWR");				description.push_back("Incoming Short Wave Radiation");				units.push_back("W/m2");
	paramname.push_back("RSWR");			description.push_back("Reflected Short Wave Radiation");				units.push_back("W/m2");
	paramname.push_back("ISWR_DIFF");		description.push_back("Incoming Diffuse Short Wave Radiation");	units.push_back("W/m2");
	paramname.push_back("ISWR_DIR");		description.push_back("Incoming Direct Short Wave Radiation");	units.push_back("W/m2");
	paramname.push_back("ILWR");				description.push_back("Incoming Long Wave Radiation");				units.push_back("W/m2");
	paramname.push_back("OLWR");			description.push_back("Outgoing Long Wave Radiation");				units.push_back("W/m2");
	paramname.push_back("TAU_CLD");		description.push_back("Atmospheric Transmissivity");					units.push_back("-");
	paramname.push_back("HS");					description.push_back("Snow Height");							units.push_back("m");
	paramname.push_back("PSUM");			description.push_back("Precipitation Sum");					units.push_back("kg/m2");
	paramname.push_back("PSUM_PH");		description.push_back("Precipitation Phase");					units.push_back("-");
	paramname.push_back("PSUM_L");			description.push_back("Precipitation Sum of the Liquid Phase");	units.push_back("kg/m2");
	paramname.push_back("PSUM_S");			description.push_back("Precipitation Sum of the Solid Phase");		units.push_back("kg/m2");
	paramname.push_back("TSG");				description.push_back("Ground Surface Temperature");					units.push_back("K");
	paramname.push_back("TSS");				description.push_back("Surface Temperature");				units.push_back("K");
	paramname.push_back("TSOIL");			description.push_back("Soil Temperature");					units.push_back("K");
	paramname.push_back("P");					description.push_back("Air Pressure");							units.push_back("Pa");
	paramname.push_back("P_SEA");			description.push_back("Air Pressure at Sea Level");		units.push_back("Pa");
	paramname.push_back("U");					description.push_back("Wind Velocity East Component");				units.push_back("m/s");
	paramname.push_back("V");					description.push_back("Wind Velocity North Component");			units.push_back("m/s");
	paramname.push_back("W");					description.push_back("Wind Velocity Vertical Component");			units.push_back("m/s");
	paramname.push_back("SWE");				description.push_back("Snow Water Equivalent");			units.push_back("kg/m2");
	paramname.push_back("RSNO");			description.push_back("Snow Mean Density");				units.push_back("kg/m3");
	paramname.push_back("ROT");				description.push_back("Total generated runoff");			units.push_back("gk/m2");
	paramname.push_back("ALB");				description.push_back("Albedo");									units.push_back("-");
	paramname.push_back("DEM");				description.push_back("Altitude above Sea Level");		units.push_back("m");
	paramname.push_back("SHADE");			description.push_back("Hillshade");								units.push_back("-");
	paramname.push_back("SLOPE");			description.push_back("Slope Angle");							units.push_back("degree");
	paramname.push_back("AZI");				description.push_back("Slope Aspect");							units.push_back("degree");

	return true;
}

const std::string& MeteoGrids::getParameterName(const size_t& parindex)
{
	if (parindex >= MeteoGrids::nrOfParameters)
		throw IndexOutOfBoundsException("Trying to get name for parameter that does not exist", AT);

	return paramname[parindex];
}
const std::string& MeteoGrids::getParameterDescription(const size_t& parindex)
{
	if (parindex >= MeteoGrids::nrOfParameters)
		throw IndexOutOfBoundsException("Trying to get description for parameter that does not exist", AT);

	return description[parindex];
}

const std::string& MeteoGrids::getParameterUnits(const size_t& parindex)
{
	if (parindex >= MeteoGrids::nrOfParameters)
		throw IndexOutOfBoundsException("Trying to get units for parameter that does not exist", AT);

	return units[parindex];
}

size_t MeteoGrids::getParameterIndex(const std::string& parname)
{
	for (size_t ii=0; ii<MeteoGrids::nrOfParameters; ii++) {
		if (paramname[ii] == parname)
			return ii;
	}

	return IOUtils::npos; //parameter not a part of MeteoGrids
}

/************************************************************
 * static section                                           *
 ************************************************************/
const double MeteoData::epsilon = 1e-5;
const size_t MeteoData::nrOfParameters =  MeteoData::lastparam - MeteoData::firstparam + 1;
std::vector<std::string> MeteoData::s_default_paramname;
const bool MeteoData::__init = MeteoData::initStaticData();
MeteoData::flag_field MeteoData::zero_flag; //for easy initialization

bool MeteoData::initStaticData()
{
	//Since the parameters enum starts at 0, this is enough to associate an index with its name
	s_default_paramname.push_back("P");
	s_default_paramname.push_back("TA");
	s_default_paramname.push_back("RH");
	s_default_paramname.push_back("TSG");
	s_default_paramname.push_back("TSS");
	s_default_paramname.push_back("HS");
	s_default_paramname.push_back("VW");
	s_default_paramname.push_back("DW");
	s_default_paramname.push_back("VW_MAX");
	s_default_paramname.push_back("RSWR");
	s_default_paramname.push_back("ISWR");
	s_default_paramname.push_back("ILWR");
	s_default_paramname.push_back("TAU_CLD");
	s_default_paramname.push_back("PSUM");
	s_default_paramname.push_back("PSUM_PH");

	zero_flag.resampled = false; //init data qa flag that is initially set
	zero_flag.generated = false;
	zero_flag.filtered = false;
	zero_flag.created = false;
	zero_flag.offset = (float)IOUtils::nodata;

	return true;
}

const std::string& MeteoData::getParameterName(const size_t& parindex)
{
	if (parindex >= MeteoData::nrOfParameters)
		throw IndexOutOfBoundsException("Trying to access meteo parameter that does not exist", AT);

	return MeteoData::s_default_paramname[parindex];
}

size_t MeteoData::getStaticParameterIndex(const std::string& parname)
{
	for (size_t ii = 0; ii<MeteoData::nrOfParameters; ii++) {
		if (s_default_paramname[ii] == parname)
			return ii;
	}

	return IOUtils::npos; //parameter not a part of MeteoData
}

/************************************************************
 * non-static section                                       *
 ************************************************************/

const std::string& MeteoData::getNameForParameter(const size_t& parindex) const
{
	if (parindex >= MeteoData::nrOfAllParameters)
		throw IndexOutOfBoundsException("Trying to get name for parameter that does not exist", AT);

	if (parindex<MeteoData::nrOfParameters) return MeteoData::s_default_paramname[parindex];
	return extra_param_name[parindex-MeteoData::nrOfParameters];
}

bool MeteoData::param_exists(const std::string& i_paramname) const
{
	for (size_t ii = 0; ii<MeteoData::nrOfParameters; ii++) {
		if (s_default_paramname[ii] == i_paramname)
			return true;
	}

	for (size_t ii = 0; ii<extra_param_name.size(); ii++) {
		if (extra_param_name[ii] == i_paramname)
			return true;
	}

	return false;
}

size_t MeteoData::addParameter(const std::string& i_paramname)
{
	//check if name is already taken
	const size_t current_index = getParameterIndex(i_paramname);
	if (current_index != IOUtils::npos)
		return current_index; //do nothing, because parameter is already present

	//add parameter
	extra_param_name.push_back(i_paramname);
	data.push_back(IOUtils::nodata);
	flags.push_back(zero_flag);

	//Increase nrOfAllParameters
	nrOfAllParameters++;

	return (nrOfAllParameters - 1);
}

MeteoData::MeteoData()
         : date(0.0, 0.), meta(), extra_param_name(), data(MeteoData::nrOfParameters, IOUtils::nodata), nrOfAllParameters(MeteoData::nrOfParameters),
           resampled(false), flags(MeteoData::nrOfParameters, zero_flag)
{ }

MeteoData::MeteoData(const Date& date_in)
         : date(date_in), meta(), extra_param_name(), data(MeteoData::nrOfParameters, IOUtils::nodata), nrOfAllParameters(MeteoData::nrOfParameters),
           resampled(false), flags(MeteoData::nrOfParameters, zero_flag)
{ }

MeteoData::MeteoData(const Date& date_in, const StationData& meta_in)
         : date(date_in), meta(meta_in), extra_param_name(), data(MeteoData::nrOfParameters, IOUtils::nodata), nrOfAllParameters(MeteoData::nrOfParameters),
           resampled(false), flags(MeteoData::nrOfParameters, zero_flag)
{ }

void MeteoData::reset()
{
	std::fill(data.begin(), data.end(), IOUtils::nodata);
}

/**
* @brief Standardize nodata values
* The plugin-specific nodata values are replaced by MeteoIO's internal nodata value
*/
void MeteoData::standardizeNodata(const double& plugin_nodata)
{
	for (size_t ii=0; ii<nrOfAllParameters; ii++) {
		//loop through all meteo params and check whether they're nodata values
		if (data[ii] <= plugin_nodata)
			data[ii] = IOUtils::nodata;
	}
}

bool MeteoData::operator==(const MeteoData& in) const
{
	//An object is equal if the date is equal and all meteo parameters are equal
	if (date != in.date) return false;

	if (nrOfAllParameters != in.nrOfAllParameters) //the number of meteo parameters has to be consistent
		return false;

	for (size_t ii=0; ii<nrOfAllParameters; ii++) {
		//const double epsilon_rel = (fabs(data[ii]) < fabs(in.data[ii]) ? fabs(in.data[ii]) : fabs(data[ii])) * std::numeric_limits<double>::epsilon(); // Hack not working...
		//const double epsilon_rel = (fabs(data[ii]) < fabs(in.data[ii]) ? fabs(in.data[ii]) : fabs(data[ii])) * 0.0000001; // Hack not working with 0 == 0 ....
		if ( !IOUtils::checkEpsilonEquality(data[ii], in.data[ii], epsilon) ) return false;
	}

	return true;
}

double& MeteoData::operator()(const size_t& parindex)
{
#ifndef NOSAFECHECKS
	if (parindex >= nrOfAllParameters)
		throw IndexOutOfBoundsException("Trying to access meteo parameter that does not exist", AT);
#endif
	return data[parindex];
}

const double& MeteoData::operator()(const size_t& parindex) const
{
#ifndef NOSAFECHECKS
	if (parindex >= nrOfAllParameters)
		throw IndexOutOfBoundsException("Trying to access meteo parameter that does not exist", AT);
#endif
	return data[parindex];
}

double& MeteoData::operator()(const std::string& parname)
{
	const size_t index = getParameterIndex(parname);
	if (index == IOUtils::npos)
		throw IndexOutOfBoundsException("Trying to access meteo parameter that does not exist: " + parname, AT);

	return operator()(index);
}

const double& MeteoData::operator()(const std::string& parname) const
{
	const size_t index = getParameterIndex(parname);
	if (index == IOUtils::npos)
		throw IndexOutOfBoundsException("Trying to access meteo parameter that does not exist: " + parname, AT);

	return operator()(index);
}

size_t MeteoData::getParameterIndex(const std::string& parname) const
{
	for (size_t ii = 0; ii<MeteoData::nrOfParameters; ii++) {
		if (s_default_paramname[ii] == parname)
			return ii;
	}

	for (size_t ii=0; ii<extra_param_name.size(); ii++) {
		if (extra_param_name[ii] == parname)
			return ii+MeteoData::nrOfParameters;
	}

	return IOUtils::npos; //parameter not a part of MeteoData
}

bool MeteoData::isNodata() const
{
	for (size_t ii=0; ii<data.size(); ii++) {
		if (data[ii]!=IOUtils::nodata) return false;
	}
	
	return true;
}

void MeteoData::setFiltered(const size_t& param, const bool& in_filtered /* = true */)
{
	flags[param].filtered = in_filtered;
}

bool MeteoData::isFiltered(const size_t& param) const
{
	return flags[param].filtered;
}

void MeteoData::setGenerated(const size_t& param, const bool& in_generated /* = true */)
{
	flags[param].generated = in_generated;
}

bool MeteoData::isGenerated(const size_t& param) const
{
	return flags[param].generated;
}

void MeteoData::setResampledParam(const size_t& param, const bool& in_resampled /* = true */)
{
	flags[param].resampled = in_resampled;
}

bool MeteoData::isResampledParam(const size_t& param) const
{
	return flags[param].resampled;
}

const std::string MeteoData::toString(const FORMATS format) const {
	std::ostringstream os;
	
	if (format==DFLT) {
		os << "<meteo>\n";
		os << meta.toString();
		os << date.toString(Date::FULL) << "\n";
		os << setw(8) << nrOfAllParameters << " parameters\n";

		for (size_t ii=0; ii<nrOfAllParameters; ii++) {
			const double& value = operator()(ii);
			if (value != IOUtils::nodata)
				os << setw(8) << getNameForParameter(ii) << ":" << setw(15) << value << endl;
		}
		os << "</meteo>\n";
	} else if (format==COMPACT) {
		os << "<meteo>\t";
		os << meta.stationID << " @ " << date.toString(Date::ISO) << " -> ";
		for (size_t ii=0; ii<nrOfAllParameters; ii++) {
			const double& value = operator()(ii);
			if (value != IOUtils::nodata)
				os <<  getNameForParameter(ii) << ":" << value << " ";
		}
		os << "</meteo>";
	}
	
	return os.str();
}

std::ostream& operator<<(std::ostream& os, const MeteoData& data) {
	os << data.date;
	os << data.meta;
	const size_t s_vector = data.extra_param_name.size();
	os.write(reinterpret_cast<const char*>(&s_vector), sizeof(size_t));
	for (size_t ii=0; ii<s_vector; ii++) {
		const size_t s_string = data.extra_param_name[ii].size();
		os.write(reinterpret_cast<const char*>(&s_string), sizeof(size_t));
		os.write(reinterpret_cast<const char*>(&data.extra_param_name[ii][0]), s_string*sizeof(data.extra_param_name[ii][0]));
	}

	const size_t s_data = data.data.size();
	os.write(reinterpret_cast<const char*>(&s_data), sizeof(s_data));
	os.write(reinterpret_cast<const char*>(&data.data[0]), s_data*sizeof(data.data[0]));

	os.write(reinterpret_cast<const char*>(&data.nrOfAllParameters), sizeof(data.nrOfAllParameters));
	os.write(reinterpret_cast<const char*>(&data.resampled), sizeof(data.resampled));
	return os;
}

std::istream& operator>>(std::istream& is, MeteoData& data) {
	is >> data.date;
	is >> data.meta;
	size_t s_vector;
	is.read(reinterpret_cast<char*>(&s_vector), sizeof(size_t));
	data.extra_param_name.resize(s_vector);
	for (size_t ii=0; ii<s_vector; ii++) {
		size_t s_string;
		is.read(reinterpret_cast<char*>(&s_string), sizeof(size_t));
		data.extra_param_name[ii].resize(s_string);
		is.read(reinterpret_cast<char*>(&data.extra_param_name[ii][0]), s_string*sizeof(data.extra_param_name[ii][0]));
	}

	size_t s_data;
	is.read(reinterpret_cast<char*>(&s_data), sizeof(size_t));
	data.data.resize(s_data);
	is.read(reinterpret_cast<char*>(&data.data[0]), s_data*sizeof(data.data[0]));

	is.read(reinterpret_cast<char*>(&data.nrOfAllParameters), sizeof(data.nrOfAllParameters));
	is.read(reinterpret_cast<char*>(&data.resampled), sizeof(data.resampled));
	return is;
}

MeteoData::Merge_Type MeteoData::getMergeType(std::string merge_type)
{
	IOUtils::toUpper( merge_type );
	if (merge_type=="STRICT_MERGE") return STRICT_MERGE;
	else if (merge_type=="EXPAND_MERGE") return EXPAND_MERGE;
	else if (merge_type=="FULL_MERGE") return FULL_MERGE;
	else
		throw UnknownValueException("Unknown merge type '"+merge_type+"'", AT);
}

/*
 * In the cases != STRICT_MERGE, it matters if vec2 is bigger than vec1. So we define the following indices
 * in order to store the information about the insertion positions:
 *
 * ----------------[-----------------]----------------------------	vec1
 *                 ↓                 ↓
 *             vec1_start        vec1_end
 *                 ↓                 ↓
 * ------[---------|-----------------|--------------]-------------	vec2
 */
void MeteoData::mergeTimeSeries(std::vector<MeteoData>& vec1, const std::vector<MeteoData>& vec2, const Merge_Type& strategy)
{
	if (vec2.empty()) return; //nothing to merge
	if (strategy==STRICT_MERGE && vec1.empty()) return; //optimization for STRICT_MERGE

	//adding the necessary extra parameters to vec1 elements, no matter which merge strategy
	if (!vec1.empty()) {
		const size_t nrExtra2 = vec2.back().nrOfAllParameters - nrOfParameters;
		for (size_t pp=0; pp<nrExtra2; pp++) {
			const std::string extra_name( vec2.back().extra_param_name[pp] );
			if (vec1.back().getParameterIndex(extra_name)==IOUtils::npos) {
				for (size_t ii=0; ii<vec1.size(); ii++) vec1[ii].addParameter( extra_name );
			}
		}
	}

	if (strategy==STRICT_MERGE) { //optimization for STRICT_MERGE
		if (vec1.back().date<vec2.front().date) return; //vec1 is before vec2
		if (vec1.front().date>vec2.back().date) return; //vec1 is after vec2
	}

	size_t vec1_start = 0; //the index in vec2 that matches the original start of vec1
	size_t vec1_end = 0; //the index in vec2 that matches the original end of vec1

	//filling data before vec1
	if (strategy!=STRICT_MERGE && vec1.front().date>vec2.front().date) {
		const Date start_date( vec1.front().date );
		vec1_start = vec2.size(); //if no overlap is found, take all vec2
		for(size_t ii=0; ii<vec2.size(); ii++) { //find the range of elements to add
			if (vec2[ii].date>=start_date) {
				vec1_start = ii;
				break;
			}
		}

		MeteoData md_pattern( vec1.front() ); //This assumes that station1 is not moving!
		md_pattern.reset(); //keep metadata and extra params
		vec1.insert(vec1.begin(), vec1_start, md_pattern);
		for (size_t ii=0; ii<vec1_start; ii++) {
			vec1[ii].date = vec2[ii].date;
			vec1[ii].merge( vec2[ii] );
		}
	}

	//general case: merge one timestamp at a time
	if (strategy==FULL_MERGE) {
		std::vector<MeteoData> tmp;
		tmp.reserve( vec1.size() + (vec2.size() - vec1_start)); //"worst case" scenario: all elements will be added
		MeteoData md_pattern( vec1.front() ); //This assumes that station1 is not moving!
		md_pattern.reset(); //keep metadata and extra params

		size_t idx2 = vec1_start; //all previous elements were handled before
		size_t last_v1 = vec1_start; //last element from vec1 that will have to be invalidated
		for(size_t ii=vec1_start; ii<vec1.size(); ii++) {
			const Date curr_date( vec1[ii].date );
			while ((idx2<vec2.size()) && (curr_date>vec2[idx2].date)) {
				tmp.push_back( md_pattern );
				tmp.back().date = vec2[idx2].date;
				tmp.back().merge( vec2[idx2] ); //so the extra params are properly handled
				idx2++;
			}
			if (idx2==vec2.size())  break; //nothing left to merge

			if (curr_date==vec2[idx2].date) {
				vec1[ii].merge( vec2[idx2] );
				idx2++;
			}
			tmp.push_back( vec1[ii] );
			last_v1 = ii;
		}

		const size_t new_count = last_v1 - vec1_start + 1;
		if (new_count<tmp.size())
			vec1.insert( vec1.begin() + vec1_start, tmp.size()-new_count, tmp.front()); //so room for the extra params is allocated

		for(size_t ii=0; ii<tmp.size(); ii++)
			vec1[vec1_start+ii] = tmp[ii];

		vec1_end = idx2;
	} else {
		size_t idx2 = vec1_start;
		for (size_t ii=vec1_start; ii<vec1.size(); ii++) { //loop over the timestamps. If some elements were inserted, vec1 now starts at vec1_start. If not, vec1_start==0
			const Date curr_date( vec1[ii].date );
			while ((idx2<vec2.size()) && (curr_date>vec2[idx2].date)) idx2++;

			if (idx2==vec2.size()) return; //nothing left to merge
			if (curr_date==vec2[idx2].date) vec1[ii].merge( vec2[idx2] ); //merging
		}
		vec1_end = idx2;
	}
	
	//filling data after vec1
	if (strategy!=STRICT_MERGE && vec1.back().date<vec2.back().date) {
		if (vec1_end!=vec2.size()) {
			MeteoData md_pattern( vec1.back() ); //This assumes that station1 is not moving!
			md_pattern.reset(); //keep metadata and extra params
			for (size_t ii=vec1_end+1; ii<vec2.size(); ii++) {
				vec1.push_back( md_pattern );
				vec1.back().date = vec2[ii].date;
				vec1.back().merge( vec2[ii] );
			}
		}
	}
}

void MeteoData::merge(std::vector<MeteoData>& vec1, const std::vector<MeteoData>& vec2, const bool& simple_merge)
{
	if (vec2.empty()) return;

	if (simple_merge || vec1.empty()) {
		vec1.reserve( vec1.size()+vec2.size() );
		for (size_t ii=0; ii<vec2.size(); ii++) vec1.push_back( vec2[ii] );
	} else {
		for (size_t ii=0; ii<vec2.size(); ii++) merge(vec1, vec2[ii]);
	}
}

void MeteoData::merge(std::vector<MeteoData>& vec, const MeteoData& meteo2, const bool& simple_merge)
{
	if (!simple_merge) {
		for (size_t ii=0; ii<vec.size(); ii++) {
			//two stations are considered the same if they point to the same 3D position
			if (vec[ii].meta.position==meteo2.meta.position) {
				vec[ii].merge(meteo2);
				return;
			}
		}
	}

	//the station was not found in the vector -> adding it
	vec.push_back( meteo2 );
}

void MeteoData::merge(std::vector<MeteoData>& vec)
{
	const size_t nElems = vec.size();
	if (nElems<2) return;

	std::vector<MeteoData> vecResult;
	std::vector<size_t> mergeIdx(nElems);
	for (size_t ii=0; ii<nElems; ii++) mergeIdx[ii] = ii;

	for (size_t ii=0; ii<nElems; ii++) {
		if (mergeIdx[ii]==IOUtils::npos) continue; //this element has already been merged, skip
		for (size_t jj=ii+1; jj<nElems; jj++) {
			if (vec[ii].meta.position==vec[jj].meta.position) {
				vec[ii].merge( vec[jj] );
				mergeIdx[jj]=IOUtils::npos; //this element will be skipped in the next loops
			}
		}
		vecResult.push_back( vec[ii] );
	}

	vec.swap( vecResult );
}

MeteoData MeteoData::merge(MeteoData meteo1, const MeteoData& meteo2)
{
	meteo1.merge(meteo2);
	return meteo1;
}

void MeteoData::merge(const MeteoData& meteo2)
{
	if (!date.isUndef() && !meteo2.date.isUndef() && date!=meteo2.date) {
		//the data must be time synchronized!
		std::ostringstream ss;
		ss << "Trying to merge MeteoData at " << date.toString(Date::ISO);
		ss << " with MeteoData at " << meteo2.date.toString(Date::ISO);
		throw InvalidArgumentException(ss.str(), AT);
	}

	if (date.isUndef()) date=meteo2.date;
	meta.merge(meteo2.meta);

	if (meteo2.resampled==true ) resampled=true;

	//merge standard parameters
	for (size_t ii=0; ii<nrOfParameters; ii++) {
		if (data[ii]==IOUtils::nodata) {
			data[ii]=meteo2.data[ii];
			flags[ii]=meteo2.flags[ii];
		}
	}

	//for each meteo2 extra parameter, check if a matching parameter exist
	const size_t nrExtra2 = meteo2.nrOfAllParameters - nrOfParameters;
	for (size_t ii=0; ii<nrExtra2; ii++) {
		const string extra_name = meteo2.extra_param_name[ii];
		const size_t extra_param_idx = getParameterIndex(extra_name);
		if (extra_param_idx==IOUtils::npos) { //no such parameter in current object
			const size_t new_idx = addParameter( extra_name );
			data[new_idx] = meteo2.data[nrOfParameters+ii];
			flags[new_idx] = meteo2.flags[nrOfParameters+ii];
		} else if (data[extra_param_idx]==IOUtils::nodata) {
			data[extra_param_idx] = meteo2.data[nrOfParameters+ii];
			flags[extra_param_idx] = meteo2.flags[nrOfParameters+ii];
		}
	}
}

std::set<std::string> MeteoData::listAvailableParameters(const std::vector<MeteoData>& vecMeteo)
{
	std::set<std::string> results;
	std::set<size_t> tmp; //for efficiency, we assume that through the vector, the indices remain identical for any given parameter

	for (size_t ii=0; ii<vecMeteo.size(); ii++) {
		for (size_t jj=0; jj<vecMeteo[ii].getNrOfParameters(); jj++)
			if (vecMeteo[ii](jj) != IOUtils::nodata && tmp.count(jj)==0) { //for efficiency, we compare on the index
				tmp.insert( jj );
				results.insert( vecMeteo[ii].getNameForParameter(jj) );
			}
	}

	return results;
}

} //namespace
