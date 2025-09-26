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
#include <meteoio/meteoFilters/ProcMult.h>
#include <meteoio/FileUtils.h>
#include <meteoio/meteoStats/libinterpol1D.h>

#include <ctime>
#include <cstdlib>

using namespace std;

namespace mio {

ProcMult::ProcMult(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name, const std::string& i_root_path)
         : ProcAdd(vecArgs, name, i_root_path)
{
	parse_args(vecArgs);
	properties.stage = ProcessingProperties::first; //for the rest: default values
}

void ProcMult::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
                        std::vector<MeteoData>& ovec)
{
	ovec = ivec;

	if (type=='c') { //constant offset
		for (size_t ii=0; ii<ovec.size(); ii++){
			double& tmp = ovec[ii](param);
			if (tmp == IOUtils::nodata) continue; //preserve nodata values

			tmp *= correction;
		}
	} else if (type=='f') { //corrections from file
		if (period=='m') {
			int year, month, day;
			for (size_t ii=0; ii<ovec.size(); ii++){
				double& tmp = ovec[ii](param);
				if (tmp == IOUtils::nodata) continue; //preserve nodata values

				ovec[ii].date.getDate(year, month, day);
				tmp *= vecCorrections[ static_cast<size_t>(month-1) ]; //indices start at 0
			}
		} else if (period=='d') {
			for (size_t ii=0; ii<ovec.size(); ii++){
				double& tmp = ovec[ii](param);
				if (tmp == IOUtils::nodata) continue; //preserve nodata values

				tmp *= vecCorrections[ static_cast<size_t>(ovec[ii].date.getJulianDayNumber()-1) ]; //indices start at 0 while day numbers start at 1
			}
		} else if (period=='h') {
			int year, month, day, hour;
			for (size_t ii=0; ii<ovec.size(); ii++){
				double& tmp = ovec[ii](param);
				if (tmp == IOUtils::nodata) continue; //preserve nodata values

				ovec[ii].date.getDate(year, month, day, hour);
				tmp *= vecCorrections[ static_cast<size_t>(hour) ];
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

void ProcMult::uniform_noise(const unsigned int& param, std::vector<MeteoData>& ovec) const
{
	for (size_t ii=0; ii<ovec.size(); ii++){
		double& tmp = ovec[ii](param);
		if (tmp == IOUtils::nodata) continue; //preserve nodata values

		tmp *= (1. + (2.*static_cast<double>(rand())/(RAND_MAX)-1.) * range);
	}
}

void ProcMult::normal_noise(const unsigned int& param, std::vector<MeteoData>& ovec) const
{
	for (size_t ii=0; ii<ovec.size(); ii++){
		double& tmp = ovec[ii](param);
		if (tmp == IOUtils::nodata) continue; //preserve nodata values

		tmp += (1. + Interpol1D::getBoxMuller() * range);
	}
}


} //end namespace
