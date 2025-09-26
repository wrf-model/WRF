/***********************************************************************************/
/*  Copyright 2013 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#include <ctime>
#include <cstdlib>
#include <cstring>
#include <cerrno>
#include <algorithm>
#include <math.h>

#include <meteoio/meteoFilters/TimeFilters.h>
#include <meteoio/meteoFilters/ProcessingStack.h>
#include <meteoio/FileUtils.h>

using namespace std;

namespace mio {

static inline bool IsUndef (const MeteoData& md) { return md.date.isUndef(); }

TimeSuppr::TimeSuppr(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name, const std::string& root_path, const double& TZ)
          : ProcessingBlock(vecArgs, name), suppr_dates(), range(IOUtils::nodata), width(IOUtils::nodata), op_mode(NONE)
{
	const std::string where( "Filters::"+block_name );
	properties.stage = ProcessingProperties::second;	
	bool has_type=false, has_frac=false, has_width=false, has_file=false;

	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="TYPE") {
			const std::string type_str( IOUtils::strToUpper( vecArgs[ii].second ) );
			if (type_str=="CLEANUP") {
				op_mode = CLEANUP;
			} else if (type_str=="FRAC") {
				op_mode = FRAC;
			} else if (type_str=="BYDATES") {
				op_mode = BYDATES;
			} else {
				throw InvalidArgumentException("Unknown type '" + vecArgs[ii].second + "' for " + where, AT);
			}
			has_type = true;
		} else if (vecArgs[ii].first=="FRAC") {
			if (!IOUtils::convertString(range, vecArgs[ii].second))
				throw InvalidArgumentException("Invalid range \""+vecArgs[ii].second+"\" specified for "+where, AT);
			if (range<0. || range>1.)
				throw InvalidArgumentException("Wrong range for " + where + ", it should be between 0 and 1", AT);
			has_frac = true;
		} else if (vecArgs[ii].first=="WIDTH") {
			if (!IOUtils::convertString(width, vecArgs[ii].second))
				throw InvalidArgumentException("Invalid width \""+vecArgs[ii].second+"\" specified for "+where, AT);
			if (width<=0.)
				throw InvalidArgumentException("Wrong width for " + where + ", it should be > 0", AT);
			has_width = true;
		} else if (vecArgs[ii].first=="FILE") {
			const std::string in_filename( vecArgs[ii].second );
			const std::string prefix = ( FileUtils::isAbsolutePath(in_filename) )? "" : root_path+"/";
			const std::string path( FileUtils::getPath(prefix+in_filename, true) );  //clean & resolve path
			const std::string filename( path + "/" + FileUtils::getFilename(in_filename) );

			suppr_dates = ProcessingBlock::readDates(block_name, filename, TZ);
			has_file = true;
		}
	}
	
	if (!has_type) throw InvalidArgumentException("Please provide a TYPE for "+where, AT);
	if (op_mode==CLEANUP && (has_frac || has_file)) throw InvalidArgumentException("The CLEANUP type for " + where + " does not take extra arguments", AT);
	if (has_frac && has_file) throw InvalidArgumentException("It is not possible tp provide both the FILE and FRAC arguments for " + where, AT);
	if (op_mode==BYDATES && !has_file) throw InvalidArgumentException("Please provide the FILE argument for " + where, AT);
	if (op_mode==FRAC && !has_frac) throw InvalidArgumentException("Please provide the FRAC argument for " + where, AT);
	if (op_mode!=FRAC && has_width) throw InvalidArgumentException("For "+where+" the WIDTH argument is only supported for type FRAC", AT);
}

void TimeSuppr::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
                        std::vector<MeteoData>& ovec)
{
	if (param!=IOUtils::unodata)
		throw InvalidArgumentException("The filter "+block_name+" can only be applied to TIME", AT);

	ovec = ivec;
	if (ovec.empty()) return;
	
	switch(op_mode) {
		case CLEANUP : supprInvalid(ovec); break;
		case FRAC : supprFrac(ovec); break;
		case BYDATES : supprByDates(ovec); break;
		default :
			throw InvalidArgumentException("The filter type has not been defined for the filter "+block_name, AT);
	}
}

//this assumes that the DATES_RANGEs in suppr_dates have been sorted by increasing starting dates
void TimeSuppr::supprByDates(std::vector<MeteoData>& ovec) const
{
	const std::string station_ID( ovec[0].meta.stationID ); //we know it is not empty
	const std::map< std::string, std::vector<dates_range> >::const_iterator station_it( suppr_dates.find( station_ID ) );
	if (station_it==suppr_dates.end()) return;

	const std::vector<dates_range> &suppr_specs = station_it->second;
	const size_t Nset = suppr_specs.size();
	size_t curr_idx = 0; //we know there is at least one
	for (size_t ii=0; ii<ovec.size(); ii++) {
		if (ovec[ii].date<suppr_specs[curr_idx].start) continue;

		if (ovec[ii].date<=suppr_specs[curr_idx].end) { //suppress the interval
			ovec[ii].date.setUndef(true); //mark the point to be removed
			continue;
		} else { //look for the next interval
			curr_idx++;
			if (curr_idx>=Nset) break; //all the suppression points have been processed
		}
	}
	
	//now really remove the points from the vector
	ovec.erase( std::remove_if(ovec.begin(), ovec.end(), IsUndef), ovec.end());
}

void TimeSuppr::supprFrac(std::vector<MeteoData>& ovec) const
{
	static const double sec_to_days = 1. / (24. * 3600.);
	const size_t set_size = ovec.size();
	const size_t nrRemove = static_cast<size_t>( round( (double)set_size*range ) );

	srand( static_cast<unsigned int>(time(NULL)) );
	size_t ii=1;
	if (width==IOUtils::nodata) { //remove individual points
		while (ii<nrRemove) {
			const size_t idx = (unsigned)rand() % set_size;
			if (ovec[idx].date.isUndef()) continue; //the point was already removed

			ovec[idx].date.setUndef(true);
			ii++;
		}
	} else {
		//remove a full time periode, the width has been provided by the user
		while (ii<nrRemove) {
			const size_t last_idx = set_size-1;
			const size_t idx = (unsigned)rand() % set_size;
			if (ovec[idx].date.isUndef()) continue; //the point was already removed
			if (idx==0 || idx==last_idx) continue; //first and last point must remain
			
			//get the start and end indices
			const Date start_period( ovec[idx].date );
			const Date end_period = start_period + width * sec_to_days;
			size_t jj = idx + 1;
			while (jj<last_idx) {
				if (ovec[jj].date.isUndef()) { //coming into a previously marked period
					jj=last_idx;
					break; //aborting
				}
				if (ovec[jj].date > end_period) break; //so we will cut from idx to jj-1
				jj++;
			}
			if (jj==last_idx) continue; //the period could not be fully contained before the end of data
			if (ovec[jj].date.isUndef()) continue; //we want at least one point after the period
			
			//now delete the data within the period
			for (size_t ll=idx; ll<jj; ll++) 
				ovec[ll].date.setUndef(true);
			ii += jj - idx + 1;
		}
	}
	
	//now really remove the points from the vector
	ovec.erase( std::remove_if(ovec.begin(), ovec.end(), IsUndef), ovec.end());
}

void TimeSuppr::supprInvalid(std::vector<MeteoData>& ovec) const
{
	const std::string stationID( ovec.front().getStationID() );
	Date previous_date( ovec.front().date );
	size_t previous_idx = 0;
	
	for (size_t ii=1; ii<ovec.size(); ++ii) {
		const Date current_date( ovec[ii].date );
		if (current_date<=previous_date) {
			if (ovec[ii].isNodata()) 
				std::cerr << "[W] " << stationID << ", deleting empty duplicate/out-of-order timestamp " << ovec[ii].date.toString(Date::ISO) << "\n";
			else
				std::cerr << "[W] " << stationID << ", deleting duplicate/out-of-order timestamp " << ovec[ii].date.toString(Date::ISO) << "\n";
			if (current_date==previous_date) ovec[previous_idx].merge( ovec[ii] );
			ovec[ii].date.setUndef(true);
		} else {
			previous_date = current_date;
			previous_idx = ii;
		}
	}
	
	//now really remove the points from the vector
	ovec.erase( std::remove_if(ovec.begin(), ovec.end(), IsUndef), ovec.end());
}


TimeUnDST::TimeUnDST(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name, const std::string& root_path, const double& TZ)
        : ProcessingBlock(vecArgs, name), dst_changes()
{
	const std::string where( "Filters::"+block_name );
	properties.stage = ProcessingProperties::second;
	const size_t nrArgs = vecArgs.size();
	
	if (nrArgs!=1)
		throw InvalidArgumentException("Wrong number of arguments for " + where, AT);

	if (vecArgs[0].first=="CORRECTIONS") {
		const std::string in_filename( vecArgs[0].second );
		const std::string prefix = ( FileUtils::isAbsolutePath(in_filename) )? "" : root_path+"/";
		const std::string path( FileUtils::getPath(prefix+in_filename, true) );  //clean & resolve path
		const std::string filename( path + "/" + FileUtils::getFilename(in_filename) );

		dst_changes = ProcessingBlock::readCorrections(block_name, filename, TZ, 2);
		if (dst_changes.empty())
			throw InvalidArgumentException("Please provide at least one DST correction for " + where, AT);
	} else
		throw UnknownValueException("Unknown option '"+vecArgs[0].first+"' for "+where, AT);
}

void TimeUnDST::process(const unsigned int& param, const std::vector<MeteoData>& ivec, std::vector<MeteoData>& ovec)
{
	if (param!=IOUtils::unodata)
		throw InvalidArgumentException("The filter "+block_name+" can only be applied to TIME", AT);

	static const double sec2Jul = 1./(24.*3600.);
	ovec = ivec;
	if (ovec.empty()) return;
	
	const size_t Nset = dst_changes.size(); //we know there is at least one
	size_t next_idx=Nset;
	double offset = 0.;
	Date prev_date = ovec[0].date - 1.; //so we are sure to be < when checking if timestamps are increasing
	size_t ii=0;

	do { //look for the latest relevant correction to start with
		next_idx--;
		if (dst_changes[next_idx].date <= ovec.front().date)
			break;
	} while (next_idx > 0);

	for (; ii<ovec.size(); ii++) {
		bool apply_change = (ovec[ii].date>=dst_changes[next_idx].date);
		
		//when reverting back to winter time, timestamps are not in increasing order for an overlap period
		if (ovec[ii].date<=prev_date) {
			const double overlap = (dst_changes[next_idx].offset*sec2Jul-offset);
			if (ovec[ii].date>=(dst_changes[next_idx].date - overlap))
				apply_change = true;
		}
		
		if (apply_change) {
			offset = dst_changes[next_idx].offset * sec2Jul;
			next_idx++;
			if (next_idx==Nset) break; //no more new corrections to expect
		}
		
		prev_date = ovec[ii].date;
		if (offset!=0.) ovec[ii].date += offset;
	}
	
	if (offset==0) return; //no more corrections to apply
	
	//if some points remained after the last DST correction date, process them
	for (; ii<ovec.size(); ii++) {
		ovec[ii].date += offset;
	}
}


TimeSort::TimeSort(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name)
        : ProcessingBlock(vecArgs, name)
{
	const std::string where( "Filters::"+block_name );
	properties.stage = ProcessingProperties::second;
	const size_t nrArgs = vecArgs.size();
	
	if (nrArgs!=0)
		throw InvalidArgumentException("Wrong number of arguments for " + where, AT);
}

void TimeSort::process(const unsigned int& param, const std::vector<MeteoData>& ivec, std::vector<MeteoData>& ovec)
{
	if (param!=IOUtils::unodata)
		throw InvalidArgumentException("The filter "+block_name+" can only be applied to TIME", AT);

	ovec = ivec;
	if (ovec.empty()) return;
	
	std::sort(ovec.begin(), ovec.end());
}


TimeLoop::TimeLoop(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name, const double& TZ)
        : ProcessingBlock(vecArgs, name), req_start(), req_end(), match_date(), ref_start(), ref_end()
{
	const std::string where( "Filters::"+block_name );
	properties.stage = ProcessingProperties::first; //for the rest: default values
	const size_t nrArgs = vecArgs.size();
	
	if (nrArgs!=3) throw InvalidArgumentException("Wrong number of arguments for " + where, AT);
	
	bool has_refstart=false, has_refend=false, has_match_date=false;
	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="REF_START") {
			if (!IOUtils::convertString(ref_start, vecArgs[ii].second, TZ))
				throw InvalidArgumentException("The \"ref_start\" key specified for "+where+" must be an ISO formatted date", AT);
			has_refstart = true;
		} else if (vecArgs[ii].first=="REF_END") {
			if (!IOUtils::convertString(ref_end, vecArgs[ii].second, TZ))
				throw InvalidArgumentException("The \"ref_end\" key specified for "+where+" must be an ISO formatted date", AT);
			has_refend = true;
		} else if (vecArgs[ii].first=="MATCH_DATE") {
			if (!IOUtils::convertString(match_date, vecArgs[ii].second, TZ))
				throw InvalidArgumentException("The \"match_date\" key specified for "+where+" must be an ISO formatted date", AT);
			has_match_date = true;
		} else
			throw UnknownValueException("Unknown option '"+vecArgs[ii].first+"' for "+where, AT);
	}
	
	if (!has_refstart) throw InvalidArgumentException("Please provide a ref_start date for "+where, AT);
	if (!has_refend) throw InvalidArgumentException("Please provide a ref_end date for "+where, AT);
	if (!has_match_date) throw InvalidArgumentException("Please provide a match_date date for "+where, AT);
}

void TimeLoop::process(Date &dateStart, Date &dateEnd)
{
	req_start = dateStart;
	req_end = dateEnd;
	
	dateStart = ref_start;
	dateEnd = ref_end;
}

void TimeLoop::process(const unsigned int& param, const std::vector<MeteoData>& ivec, std::vector<MeteoData>& ovec)
{
	if (param!=IOUtils::unodata)
		throw InvalidArgumentException("The filter "+block_name+" can only be applied to TIME", AT);

	if (ivec.empty()) {
		ovec = ivec;
		return;
	}
	
	//to convert between reference and requested periods, we need the ref. range
	const double ref_period_range = ref_end.getJulian() - ref_start.getJulian();
	
	//Which date in the reference period ivec matches with the requested start date?
	const double offset_match_req = req_start.getJulian() - match_date.getJulian(); //offset to convert between matching and requested dates
	const Date ref_start_date(fmod(offset_match_req, ref_period_range) +  ref_start.getJulian(), req_start.getTimeZone()); //req_start in ref period
	//find the reference index for the requested start date
	size_t ii=0;
	for (; ii<ivec.size(); ii++) if (ivec[ii].date>=ref_start_date) break; //get the ivec index matching req_start
	if (ii>=ivec.size()) throw IOException("Invalid index, please report it to the developers", AT);
	
	//offset to convert between reference and requested dates
	const double offset_ref_req = req_start.getJulian() - ref_start_date.getJulian();
	int loop_counter = -1; //the first loop starts already into the reference period at the precomputed ii
	Date dt( req_start );
	while (dt<=req_end) {
		MeteoData md( ivec[ii] ); //in the reference period
		md.date += offset_ref_req + loop_counter * ref_period_range; //shift the date in the reference period to the requested period
		dt = md.date;
		ovec.push_back( md );
		
		ii++;
		//have we reached the end of the reference period data?
		if (ii>=ivec.size()-1) { //The "-1" is here to prevent overlap between the first and the last timestep in ref
			ii=0;
			loop_counter++;
		}
	}
}


////////////////////////////////////////////////////////////// Time Processing Stack //////////////////////////////////////
const std::string TimeProcStack::timeParamName( "TIME" );
TimeProcStack::TimeProcStack(const Config& cfg) : filter_stack()
{
	//extract each filter and its arguments, then build the filter stack
	const std::vector< std::pair<std::string, std::string> > vecFilters( cfg.getValues(timeParamName+ProcessingStack::filter_key, "FILTERS") );
	for (size_t ii=0; ii<vecFilters.size(); ii++) {
		const std::string block_name( IOUtils::strToUpper( vecFilters[ii].second ) );
		if (block_name=="NONE") continue;
		
		const std::vector< std::pair<std::string, std::string> > vecArgs( ProcessingStack::parseArgs(cfg, vecFilters[ii].first, timeParamName) );
		filter_stack.push_back( BlockFactory::getTimeBlock(block_name, vecArgs, cfg) );
	}
}

void TimeProcStack::process(Date &dateStart, Date &dateEnd)
{
	const size_t nr_of_filters = filter_stack.size();
	for (size_t jj=0; jj<nr_of_filters; jj++) {
		if (!filter_stack[jj]->noStationsRestrictions())
			throw InvalidArgumentException("Filter "+filter_stack[jj]->getName()+" is not allowed to have restrictions on StationIDs", AT);

		//only first stage filters have to be called to edit the requested time range
		const ProcessingProperties::proc_stage filter_stage( filter_stack[jj]->getProperties().stage );
		if ((filter_stage==ProcessingProperties::second) || (filter_stage==ProcessingProperties::none))
			continue;

		filter_stack[jj]->process(dateStart, dateEnd);
	}
}

//ivec is passed by value, so it makes an efficient copy
void TimeProcStack::process(std::vector< std::vector<MeteoData> >& ivec)
{
	const size_t nr_of_filters = filter_stack.size();
	const size_t nr_stations = ivec.size();

	std::vector<MeteoData> ovec;
	ovec.reserve( ivec.size() );
	for (size_t ii=0; ii<nr_stations; ii++) { //for every station
		if ( ivec[ii].empty() ) continue; //no data, nothing to do!
		
		const std::string statID( ivec[ii].front().meta.getStationID() ); //we know there is at least 1 element (we've already skipped empty vectors)
		//Now call the filters one after another for the current station and parameter
		for (size_t jj=0; jj<nr_of_filters; jj++) {
			if (filter_stack[jj]->skipStation( statID ))
				continue;

			filter_stack[jj]->process(IOUtils::unodata, ivec[ii], ovec);
			ivec[ii] = ovec;
		}
	}
}

/** 
 * @brief check that timestamps are unique and in increasing order
 * @param[in] vecVecMeteo all the data for all the stations
*/
void TimeProcStack::checkUniqueTimestamps(std::vector<METEO_SET> &vecVecMeteo)
{
	for (size_t stat_idx=0; stat_idx<vecVecMeteo.size(); ++stat_idx) { //for each station
		const size_t nr_timestamps = vecVecMeteo[stat_idx].size();
		if (nr_timestamps==0) continue;

		Date previous_date( vecVecMeteo[stat_idx].front().date );
		for (size_t ii=1; ii<nr_timestamps; ++ii) {
			const Date current_date( vecVecMeteo[stat_idx][ii].date );
			if (current_date<=previous_date) {
				const StationData& station( vecVecMeteo[stat_idx][ii].meta );
				if (current_date==previous_date) 
					throw IOException("[E] timestamp error for station \""+station.stationName+"\" ("+station.stationID+") at time "+current_date.toString(Date::ISO)+" : timestamps must be unique! (either correct your data or declare a time filter)", AT);
				else
					throw IOException("[E] timestamp error for station \""+station.stationName+"\" ("+station.stationID+"): jumping from "+previous_date.toString(Date::ISO)+" to "+current_date.toString(Date::ISO)+"  (either correct your data or declare a time filter)", AT);
			}
			previous_date = current_date;
		}
	}
}

} //end namespace
