/***********************************************************************************/
/*  Copyright 2012 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#include <meteoio/meteoFilters/ProcAdd.h>
#include <meteoio/FileUtils.h>
#include <meteoio/meteoStats/libinterpol1D.h>

#include <ctime>
#include <cstdlib>

using namespace std;

namespace mio {

ProcAdd::ProcAdd(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name, const std::string& i_root_path)
        : ProcessingBlock(vecArgs, name), vecCorrections(), root_path(i_root_path), correction(0.), range(0.), type('N'), distribution('N'), period('N')
{
	parse_args(vecArgs);
	properties.stage = ProcessingProperties::first; //for the rest: default values
}

void ProcAdd::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
                        std::vector<MeteoData>& ovec)
{
	ovec = ivec;

	if (type=='c') { //constant offset
		for (size_t ii=0; ii<ovec.size(); ii++){
			double& tmp = ovec[ii](param);
			if (tmp == IOUtils::nodata) continue; //preserve nodata values

			tmp += correction;
		}
	} else if (type=='f') { //corrections from file
		if (period=='m') {
			int year, month, day;
			for (size_t ii=0; ii<ovec.size(); ii++){
				double& tmp = ovec[ii](param);
				if (tmp == IOUtils::nodata) continue; //preserve nodata values

				ovec[ii].date.getDate(year, month, day);
				tmp += vecCorrections[ static_cast<size_t>(month-1) ]; //indices start at 0
			}
		} else if (period=='d') {
			for (size_t ii=0; ii<ovec.size(); ii++){
				double& tmp = ovec[ii](param);
				if (tmp == IOUtils::nodata) continue; //preserve nodata values

				tmp += vecCorrections[ static_cast<size_t>(ovec[ii].date.getJulianDayNumber()-1) ]; //indices start at 0 while day numbers start at 1
			}
		} else if (period=='h') {
			int year, month, day, hour;
			for (size_t ii=0; ii<ovec.size(); ii++){
				double& tmp = ovec[ii](param);
				if (tmp == IOUtils::nodata) continue; //preserve nodata values

				ovec[ii].date.getDate(year, month, day, hour);
				tmp += vecCorrections[ static_cast<size_t>(hour) ];
			}
		}
	} else if (type=='n') { //noise
		srand( static_cast<unsigned int>(time(NULL)) );
		if (distribution=='u') {
			uniform_noise(param, ovec);
		} else if (distribution=='n') {
			normal_noise(param, ovec);
		}
	}
}

void ProcAdd::uniform_noise(const unsigned int& param, std::vector<MeteoData>& ovec) const
{
	for (size_t ii=0; ii<ovec.size(); ii++){
		double& tmp = ovec[ii](param);
		if (tmp == IOUtils::nodata) continue; //preserve nodata values

		tmp += (2.*static_cast<double>(rand())/(RAND_MAX) - 1.) * range;
	}
}

void ProcAdd::normal_noise(const unsigned int& param, std::vector<MeteoData>& ovec) const
{
	for (size_t ii=0; ii<ovec.size(); ii++){
		double& tmp = ovec[ii](param);
		if (tmp == IOUtils::nodata) continue; //preserve nodata values

		tmp += Interpol1D::getBoxMuller() * range;
	}
}

void ProcAdd::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	const std::string where( "Filters::"+block_name );
	const std::string corr_name = (block_name=="ADD")? "offset" : "factor";
	bool has_type=false, has_cst=false, has_period=false, has_distribution=false, has_range=false;
	size_t column=2; //default: use second column, ie one column after the date index
	std::string filename;

	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="TYPE") {
			const std::string type_str( IOUtils::strToUpper(vecArgs[ii].second) );
			if (type_str=="CST") type = 'c';
			else if (type_str=="FILE") type = 'f';
			else if (type_str=="NOISE") type = 'n';
			else
				throw InvalidArgumentException("Invalid type \""+type_str+"\" specified for the "+where, AT);
			has_type = true;
		} else if (vecArgs[ii].first=="CST") {
			IOUtils::parseArg(vecArgs[ii], where, correction);
			has_cst = true;
		} else if (vecArgs[ii].first=="PERIOD") {
			const std::string period_str( IOUtils::strToUpper(vecArgs[ii].second) );
			if (period_str=="MONTHLY") {
				period='m';
			} else if (period_str=="DAILY") {
				period='d';
			} else if (period_str=="HOURLY") {
				period='h';
			} else
				throw InvalidArgumentException("Invalid period \""+period_str+"\" specified for "+where, AT);
			has_period = true;
		} else if (vecArgs[ii].first=="CORRECTIONS") {
			//if this is a relative path, prefix the path with the current path
			const std::string in_filename( vecArgs[ii].second );
			const std::string prefix = ( FileUtils::isAbsolutePath(in_filename) )? "" : root_path+"/";
			const std::string path( FileUtils::getPath(prefix+in_filename, true) );  //clean & resolve path
			filename = path + "/" + FileUtils::getFilename(in_filename);
		} else if (vecArgs[ii].first=="COLUMN") {
			if (!IOUtils::convertString(column, vecArgs[ii].second))
				throw InvalidArgumentException("Invalid column index \""+vecArgs[ii].second+"\" specified for "+where, AT);
		} else if (vecArgs[ii].first=="DISTRIBUTION") {
			const std::string distribution_str( IOUtils::strToUpper(vecArgs[ii].second) );
			if (distribution_str=="UNIFORM") {
				distribution='u';
			} else if (distribution_str=="NORMAL") {
				distribution='n';
			} else
				throw InvalidArgumentException("Invalid distribution \""+distribution_str+"\" specified for "+where, AT);
			has_distribution = true;
		} else if (vecArgs[ii].first=="RANGE") {
			if (!IOUtils::convertString(range, vecArgs[ii].second))
				throw InvalidArgumentException("Invalid range specified for "+where, AT);
			has_range = true;
		}
	}

	if (!has_type) throw InvalidArgumentException("Please provide a type for "+where, AT);
	if (type=='c' && !has_cst) throw InvalidArgumentException("Please provide "+corr_name+" for "+where, AT);
	if (type=='f') {
		if (!has_period) throw InvalidArgumentException("Please provide a period for "+where, AT);
		if (filename.empty()) throw InvalidArgumentException("Please provide a correction file for "+where, AT);
		vecCorrections = ProcessingBlock::readCorrections(getName(), filename, column, period, 0.);
	}
	if (type=='n') {
		if (!has_distribution) throw InvalidArgumentException("Please provide a noise distribution for "+where, AT);
		if (!has_range) throw InvalidArgumentException("Please provide a noise range for "+where, AT);
	}
}

} //end namespace
