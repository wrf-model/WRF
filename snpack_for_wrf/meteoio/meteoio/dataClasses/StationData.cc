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
#include <meteoio/dataClasses/StationData.h>
#include <meteoio/IOUtils.h>
#include <cmath>
#include <sstream>

using namespace std;

namespace mio {

//Default constructor initializing every double attribute to nodata and strings to  ""
StationData::StationData() : position(), stationID(), stationName(), extra(), 
                             slope(IOUtils::nodata), azi(IOUtils::nodata) {}

StationData::StationData(const Coords& i_position, const std::string& i_id, const std::string& i_name)
            : position(i_position), stationID(i_id), stationName(i_name), extra(), slope(IOUtils::nodata), azi(IOUtils::nodata) {}

void StationData::setStationData(const Coords& i_position, const std::string& i_id, const std::string& i_name)
{
	position    = i_position;
	stationID   = i_id;
	stationName = i_name;
}

void StationData::setSlope(const double& in_slope_angle, const double& in_azimuth)
{
	if (in_slope_angle!=IOUtils::nodata) {
		slope = fmod(in_slope_angle, 360.);
		//normalizing the slope between 0 and 90
		if (slope>90. && slope <=180.) slope = 180. - slope;
		if (slope>180. && slope <=270.) slope = slope - 180.;
		if (slope>270. && slope <=360.) slope = 360. - slope;
	} else
		slope = IOUtils::nodata;

	if (in_azimuth!=IOUtils::nodata)
		azi = fmod(in_azimuth, 360.);
	else
		azi =  IOUtils::nodata;
}

//Comparison operator
bool StationData::operator==(const StationData& in) const {
	return ( (position == in.position) &&
	         (stationID == in.stationID) &&
	         (slope==in.slope) &&
	         (azi==in.azi) );// && (stationName == in.stationName));
}

bool StationData::operator!=(const StationData& in) const {
	return !(*this==in);
}

StationData StationData::merge(StationData sd1, const StationData& sd2) {
	sd1.merge(sd2);
	return sd1;
}

void StationData::merge(const StationData& sd2) 
{
	if (stationID.empty()) stationID = sd2.stationID;
	if (stationName.empty()) stationName = sd2.stationName;
	if (slope==IOUtils::nodata) slope = sd2.slope;
	if (azi==IOUtils::nodata) azi = sd2.azi;
	if (extra.empty() && !sd2.extra.empty()) extra = sd2.extra;
	position.merge(sd2.position);
}

bool StationData::unique(std::vector<StationData> &vecStation, const bool& position_only)
{
	//this uses swap and pop_back for greater efficiency
	bool status = false;
	if (position_only) {
		for (size_t ii=0; ii<vecStation.size(); ii++) {
			size_t jj=ii+1;
			while (jj<vecStation.size()) { //so we can rescan the last removed duplicate
				if (vecStation[ii].position==vecStation[jj].position) {
					std::swap( vecStation[jj], vecStation.back() );
					vecStation.pop_back();
					status = true;
				} else {
					jj++;
				}
			}
		}
	} else {
		for (size_t ii=0; ii<vecStation.size(); ii++) {
			size_t jj=ii+1;
			while (jj<vecStation.size()) { //so we can rescan the last removed duplicate
				if (vecStation[ii]==vecStation[jj]) {
					std::swap( vecStation[jj], vecStation.back() );
					vecStation.pop_back();
					status = true;
				} else {
					jj++;
				}
			}
		}
	}
	
	return status;
}

const std::string StationData::toString() const {
	std::ostringstream os;
	os << "<station>" << "\n"
	   << std::setprecision(10) << position.toString()
	   << "ID:    " << getStationID() << "\n"
	   << "Name:  " << getStationName() << "\n"
	   << "Slope: " << getSlopeAngle() << " bearing: " << getAzimuth() << "\n";

	if (!extra.empty()) {
		os << "Extra metadata: \n";
		for (std::map<string,string>::const_iterator it = extra.begin(); it != extra.end(); ++it){
			os << "\t" << (*it).first << " -> " << (*it).second << "\n";
		}
	}

	os << "</station>" << endl;
	return os.str();
}

std::ostream& operator<<(std::ostream& os, const StationData& station) {
	os << station.position;

	const size_t s_ID = station.stationID.size();
	os.write(reinterpret_cast<const char*>(&s_ID), sizeof(size_t));
	os.write(reinterpret_cast<const char*>(&station.stationID[0]), s_ID*sizeof(station.stationID[0]));
	const size_t s_name = station.stationName.size();
	os.write(reinterpret_cast<const char*>(&s_name), sizeof(size_t));
	os.write(reinterpret_cast<const char*>(&station.stationName[0]), s_name*sizeof(station.stationName[0]));
	
	const size_t s_map = station.extra.size();
	os.write(reinterpret_cast<const char*>(&s_map), sizeof(size_t));
	for (map<string,string>::const_iterator it = station.extra.begin(); it != station.extra.end(); ++it){
		const string& key = (*it).first;
		const size_t s_key = key.size();
		os.write(reinterpret_cast<const char*>(&s_key), sizeof(size_t));
		os.write(reinterpret_cast<const char*>(&key[0]), s_key*sizeof(key[0]));

		const string& value = (*it).second;
		const size_t s_value = value.size();
		os.write(reinterpret_cast<const char*>(&s_value), sizeof(size_t));
		os.write(reinterpret_cast<const char*>(&value[0]), s_value*sizeof(value[0]));
	}

	os.write(reinterpret_cast<const char*>(&station.slope), sizeof(station.slope));
	os.write(reinterpret_cast<const char*>(&station.azi), sizeof(station.azi));
	return os;
}

std::istream& operator>>(std::istream& is, StationData& station) {
	is >> station.position;

	size_t s_ID, s_name;
	is.read(reinterpret_cast<char*>(&s_ID), sizeof(size_t));
	station.stationID.resize(s_ID);
	is.read(reinterpret_cast<char*>(&station.stationID[0]), s_ID*sizeof(station.stationID[0]));
	is.read(reinterpret_cast<char*>(&s_name), sizeof(size_t));
	station.stationName.resize(s_name);
	is.read(reinterpret_cast<char*>(&station.stationName[0]), s_name*sizeof(station.stationName[0]));

	station.extra.clear();
	size_t s_map;
	is.read(reinterpret_cast<char*>(&s_map), sizeof(size_t));
	for (size_t ii=0; ii<s_map; ii++) {
		size_t s_key, s_value;
		is.read(reinterpret_cast<char*>(&s_key), sizeof(size_t));
		string key;
		key.resize(s_key);
		is.read(reinterpret_cast<char*>(&key[0]), s_key*sizeof(key[0]));

		is.read(reinterpret_cast<char*>(&s_value), sizeof(size_t));
		string value;
		value.resize(s_value);
		is.read(reinterpret_cast<char*>(&value[0]), s_value*sizeof(value[0]));

		station.extra[key] = value;
	}

	is.read(reinterpret_cast<char*>(&station.slope), sizeof(station.slope));
	is.read(reinterpret_cast<char*>(&station.azi), sizeof(station.azi));
	return is;
}

} //end namespace
