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

#include <cmath>
#include <cstdio>
#include <iomanip>
#include <iostream>
#include <ctime>

#include <meteoio/dataClasses/Date.h>
#include <meteoio/IOUtils.h>
#include <meteoio/MathOptim.h>

using namespace std;

namespace mio {

#ifdef __MINGW32__
	//some version of MINGW have a buggy 64 bits implementation of difftime
	//this is Mingw bug 2152
	static __inline__
	double difftime( time_t __t1, time_t __t0 ) {
		if (sizeof(time_t)==8) { //time_t is 64 bits
			return (double)((long double)(__t1) - (long double)(__t0));
		} else {
			//return (double)((__int64)(__t1) - (__int64)(__t0));
			return (double)__t1 - (double)__t0;
		}
	}
#endif

const double Date::DST_shift = 1.0; //in hours
const double Date::MJD_offset = 2400000.5; ///<offset between julian date and modified julian date
const double Date::Unix_offset = 2440587.5; ///<offset between julian date and Unix Epoch time
const double Date::RFC868_offset = 2415020.5; ///< offset between julian date and RFC868 time (ref is 1900-01-01T00:00 GMT)
const double Date::Excel_offset = 2415018.5;  ///<offset between julian date and Excel dates (note that excel invented some days...)
const double Date::Matlab_offset = 1721058.5; ///<offset between julian date and Matlab dates

const double Date::epsilon_sec = 0.01; //minimum difference between two dates, in seconds
const double Date::epsilon = epsilon_sec / (24.*3600.025); ///< minimum difference between two dates in days. 3600.025 is intentional to support high sampling rates
std::map< std::string, double> Date::TZAbbrev;
const int Date::daysLeapYear[12] = {31,29,31,30,31,30,31,31,30,31,30,31};
const int Date::daysNonLeapYear[12] = {31,28,31,30,31,30,31,31,30,31,30,31};
const bool Date::__init = Date::initStaticData();
//NOTE: For the comparison operators, we assume that dates are positive so we can bypass a call to abs()

//see https://en.wikipedia.org/wiki/List_of_time_zone_abbreviations
bool Date::initStaticData()
{
	TZAbbrev["ACDT"] = 10.5;     TZAbbrev["ACST"] = 9.5;      TZAbbrev["ACT"] = -5;        TZAbbrev["ACWST"] = 8.75;    TZAbbrev["ADT"] = -3;
	TZAbbrev["AEDT"] = 11;       TZAbbrev["AEST"] = 10;       TZAbbrev["AFT"] = 4.5;       TZAbbrev["AKDT"] = -8;       TZAbbrev["AKST"] = -9;
	TZAbbrev["AMST"] = -3;       TZAbbrev["AMT"] = -4;        TZAbbrev["AMT"] = 4;         TZAbbrev["ART"] = -3;        TZAbbrev["AST"] = 3;
	TZAbbrev["AST"] = -4;        TZAbbrev["AWST"] = 8;        TZAbbrev["AZOST"] = 0;       TZAbbrev["AZOT"] = -1;       TZAbbrev["AZT"] = 4;
	TZAbbrev["BDT"] = 8;         TZAbbrev["BIOT"] = 6;        TZAbbrev["BIT"] = -12;       TZAbbrev["BOT"] = -4;        TZAbbrev["BRST"] = -2;
	TZAbbrev["BRT"] = -3;        TZAbbrev["BST"] = 6;         TZAbbrev["BST"] = 11;        TZAbbrev["BST"] = 1;         TZAbbrev["BTT"] = 6;
	TZAbbrev["CAT"] = 2;         TZAbbrev["CCT"] = 6.5;       TZAbbrev["CDT"] = -5;        TZAbbrev["CDT"] = -4;        TZAbbrev["CEST"] = 2;
	TZAbbrev["CET"] = 1;         TZAbbrev["CHADT"] = 13.75;   TZAbbrev["CHAST"] = 12.75;   TZAbbrev["CHOT"] = 8;        TZAbbrev["CHOST"] = 9;
	TZAbbrev["CHST"] = 10;       TZAbbrev["CHUT"] = 10;       TZAbbrev["CIST"] = -8;       TZAbbrev["CIT"] = 8;         TZAbbrev["CKT"] = -10;
	TZAbbrev["CLST"] = -3;       TZAbbrev["CLT"] = -4;        TZAbbrev["COST"] = -4;       TZAbbrev["COT"] = -5;        TZAbbrev["CST"] = -6;
	TZAbbrev["CST"] = 8;         TZAbbrev["CST"] = -5;        TZAbbrev["CT"] = 8;          TZAbbrev["CVT"] = -1;        TZAbbrev["CWST"] = 8.75;
	TZAbbrev["CXT"] = 7;         TZAbbrev["DAVT"] = 7;        TZAbbrev["DDUT"] = 10;       TZAbbrev["DFT"] = 1;         TZAbbrev["EASST"] = -5;
	TZAbbrev["EAST"] = -6;       TZAbbrev["EAT"] = 3;         TZAbbrev["ECT"] = -4;        TZAbbrev["ECT"] = -5;        TZAbbrev["EDT"] = -4;
	TZAbbrev["EEST"] = 3;        TZAbbrev["EET"] = 2;         TZAbbrev["EGST"] = 0;        TZAbbrev["EGT"] = -1;        TZAbbrev["EIT"] = 9;
	TZAbbrev["EST"] = -5;        TZAbbrev["FET"] = 3;         TZAbbrev["FJT"] = 12;        TZAbbrev["FKST"] = -3;       TZAbbrev["FKT"] = -4;
	TZAbbrev["FNT"] = -2;        TZAbbrev["GALT"] = -6;       TZAbbrev["GAMT"] = -9;       TZAbbrev["GET"] = 4;         TZAbbrev["GFT"] = -3;
	TZAbbrev["GILT"] = 12;       TZAbbrev["GIT"] = -9;        TZAbbrev["GMT"] = 0;         TZAbbrev["GST"] = -2;        TZAbbrev["GST"] = 4;
	TZAbbrev["GYT"] = -4;        TZAbbrev["HDT"] = -9;        TZAbbrev["HAEC"] = 2;        TZAbbrev["HST"] = -10;       TZAbbrev["HKT"] = 8;
	TZAbbrev["HMT"] = 5;         TZAbbrev["HOVST"] = 8;       TZAbbrev["HOVT"] = 7;        TZAbbrev["ICT"] = 7;         TZAbbrev["IDT"] = 3;
	TZAbbrev["IOT"] = 3;         TZAbbrev["IRDT"] = 4.5;      TZAbbrev["IRKT"] = 8;        TZAbbrev["IRST"] = 3.5;      TZAbbrev["IST"] = 5.5;
	TZAbbrev["IST"] = 1;         TZAbbrev["IST"] = 2;         TZAbbrev["JST"] = 9;         TZAbbrev["KGT"] = 6;         TZAbbrev["KOST"] = 11;
	TZAbbrev["KRAT"] = 7;        TZAbbrev["KST"] = 9;         TZAbbrev["LHST"] = 10.5;     TZAbbrev["LHST"] = 11;       TZAbbrev["LINT"] = 14;
	TZAbbrev["MAGT"] = 12;       TZAbbrev["MART"] = -9.5;     TZAbbrev["MAWT"] = 5;        TZAbbrev["MDT"] = -6;        TZAbbrev["MET"] = 1;
	TZAbbrev["MEST"] = 2;        TZAbbrev["MHT"] = 12;        TZAbbrev["MIST"] = 11;       TZAbbrev["MIT"] = -9.5;      TZAbbrev["MMT"] = 6.5;
	TZAbbrev["MSK"] = 3;         TZAbbrev["MST"] = 8;         TZAbbrev["MST"] = -7;        TZAbbrev["MUT"] = 4;         TZAbbrev["MVT"] = 5;
	TZAbbrev["MYT"] = 8;         TZAbbrev["NCT"] = 11;        TZAbbrev["NDT"] = -2.5;      TZAbbrev["NFT"] = 11;        TZAbbrev["NPT"] = 5.75;
	TZAbbrev["NST"] = -3.5;      TZAbbrev["NT"] = -3.5;       TZAbbrev["NUT"] = -11;       TZAbbrev["NZDT"] = 13;       TZAbbrev["NZST"] = 12;
	TZAbbrev["OMST"] = 6;        TZAbbrev["ORAT"] = 5;        TZAbbrev["PDT"] = -7;        TZAbbrev["PET"] = -5;        TZAbbrev["PETT"] = 12;
	TZAbbrev["PGT"] = 10;        TZAbbrev["PHOT"] = 13;       TZAbbrev["PHT"] = 8;         TZAbbrev["PKT"] = 5;         TZAbbrev["PMDT"] = -2;
	TZAbbrev["PMST"] = -3;       TZAbbrev["PONT"] = 11;       TZAbbrev["PST"] = -8;        TZAbbrev["PST"] = 8;         TZAbbrev["PYST"] = -3;
	TZAbbrev["PYT"] = -4;        TZAbbrev["RET"] = 4;         TZAbbrev["ROTT"] = -3;       TZAbbrev["SAKT"] = 11;       TZAbbrev["SAMT"] = 4;
	TZAbbrev["SAST"] = 2;        TZAbbrev["SBT"] = 11;        TZAbbrev["SCT"] = 4;         TZAbbrev["SDT"] = -10;       TZAbbrev["SGT"] = 8;
	TZAbbrev["SLST"] = 5.5;      TZAbbrev["SRET"] = 11;       TZAbbrev["SRT"] = -3;        TZAbbrev["SST"] = -11;       TZAbbrev["SST"] = 8;
	TZAbbrev["SYOT"] = 3;        TZAbbrev["TAHT"] = -10;      TZAbbrev["THA"] = 7;         TZAbbrev["TFT"] = 5;         TZAbbrev["TJT"] = 5;
	TZAbbrev["TKT"] = 13;        TZAbbrev["TLT"] = 9;         TZAbbrev["TMT"] = 5;         TZAbbrev["TRT"] = 3;         TZAbbrev["TOT"] = 13;
	TZAbbrev["TVT"] = 12;        TZAbbrev["ULAST"] = 9;       TZAbbrev["ULAT"] = 8;        TZAbbrev["USZ1"] = 2;        TZAbbrev["UYST"] = -2;
	TZAbbrev["UYT"] = -3;        TZAbbrev["UZT"] = 5;         TZAbbrev["VET"] = -4;        TZAbbrev["VLAT"] = 10;       TZAbbrev["VOLT"] = 4;
	TZAbbrev["VOST"] = 6;        TZAbbrev["VUT"] = 11;        TZAbbrev["WAKT"] = 12;       TZAbbrev["WAST"] = 2;        TZAbbrev["WAT"] = 1;
	TZAbbrev["WEST"] = 1;        TZAbbrev["WET"] = 0;         TZAbbrev["WIT"] = 7;         TZAbbrev["WST"] = 8;         TZAbbrev["YAKT"] = 9;
	TZAbbrev["YEKT"] = 5;

	return true;
}


// CONSTUCTORS
/**
* @brief Default constructor: timezone is set to GMT without DST, julian date is set to 0 (meaning -4713-01-01T12:00)
*/
Date::Date() : timezone(0.), gmt_julian(0.),
               dst(false), undef(true)
{
}

/**
* @brief Julian date constructor.
* @param julian_in julian date to set
* @param in_timezone timezone as an offset to GMT (in hours, optional)
* @param in_dst is it DST? (default: no)
*/
Date::Date(const double& julian_in, const double& in_timezone, const bool& in_dst)
         : timezone(0.), gmt_julian(0.),
           dst(false), undef(true)
{
	setDate(julian_in, in_timezone, in_dst);
}

/**
* @brief Unix date constructor.
* @param in_time unix time (ie: as number of seconds since Unix Epoch, always UTC)
* @param in_dst is it DST? (default: no)
*/
Date::Date(const time_t& in_time, const bool& in_dst)
         : timezone(in_dst), gmt_julian(0.),
           dst(false), undef(true)
{
	setDate(in_time, in_dst);
}

/**
* @brief Date constructor by elements.
* All values are checked for plausibility.
* @param in_year in 4 digits
* @param in_month please keep in mind that first month of the year is 1 (ie: not 0!)
* @param in_day please keep in mind that first day of the month is 1 (ie: not 0!)
* @param in_hour
* @param in_minute
* @param in_timezone timezone as an offset to GMT (in hours, optional)
* @param in_dst is it DST? (default: no)
*/
Date::Date(const int& in_year, const int& in_month, const int& in_day, const int& in_hour, const int& in_minute, const double& in_timezone, const bool& in_dst)
         : timezone(in_timezone), gmt_julian(0.),
           dst(false), undef(true)
{
	setDate(in_year, in_month, in_day, in_hour, in_minute, in_timezone, in_dst);
}

Date::Date(const int& in_year, const int& in_month, const int& in_day, const int& in_hour, const int& in_minute, const int& in_second, const double& in_timezone, const bool& in_dst)
         : timezone(in_timezone), gmt_julian(0.),
           dst(false), undef(true)
{
	setDate(in_year, in_month, in_day, in_hour, in_minute, in_second, in_timezone, in_dst);
}

Date::Date(const int& in_year, const int& in_month, const int& in_day, const int& in_hour, const int& in_minute, const double& in_second, const double& in_timezone, const bool& in_dst)
         : timezone(in_timezone), gmt_julian(0.),
           dst(false), undef(true)
{
	setDate(in_year, in_month, in_day, in_hour, in_minute, in_second, in_timezone, in_dst);
}

/**
* @brief Julian Day Number (JDN) constructor
* @param year year to set
* @param jdn Julian Day Number within the provided year
* @param in_timezone timezone as an offset to GMT (in hours, optional)
* @param in_dst is it DST? (default: no)
*/
Date::Date(const int& year, const double& jdn, const double& in_timezone, const bool& in_dst)
         : timezone(in_timezone), gmt_julian(0.),
           dst(false), undef(true)
{
	setDate(year, jdn, in_timezone, in_dst);
}

// SETTERS
void Date::setUndef(const bool& flag) {
	undef = flag;
}

/**
* @brief Set internal gmt time from system time as well as system time zone.
*/
void Date::setFromSys() {
	const time_t curr( time(NULL) );// current time in UTC
	tm local = *gmtime(&curr);// current time in UTC, stored as tm
	const time_t utc( mktime(&local) );// convert GMT tm to GMT time_t
#ifndef __MINGW32__
	double tz = - difftime(utc,curr) / 3600.; //time zone shift (sign so that if curr>utc, tz>0)
#else //workaround for Mingw bug 2152
	double tz = - mio::difftime(utc,curr) / 3600.; //time zone shift (sign so that if curr>utc, tz>0)
#endif
	setDate( curr ); //Unix time_t setter, always in gmt
	setTimeZone( tz );
}

/**
* @brief Set timezone and Daylight Saving Time flag.
* @param in_timezone timezone as an offset to GMT (in hours)
* @param in_dst is it DST?
*/
void Date::setTimeZone(const double& in_timezone, const bool& in_dst) {
//please keep in mind that timezone might be fractional (ie: 15 minutes, etc)
	if (abs(in_timezone) > 12) {
		throw InvalidArgumentException("[E] Time zone can NOT be greater than +/-12!!", AT);
	}

	timezone = in_timezone;
	dst = in_dst;
}

/**
* @brief Copy setter.
* @param in_date Date object to copy
*/
void Date::setDate(const Date& in_date)
{
	if (in_date.isUndef()) {
		dst = false;
		undef = true;
	} else {
		setDate(in_date.getJulian(), in_date.getTimeZone(), in_date.getDST());
	}
}

/**
* @brief Set date by elements.
* All values are checked for plausibility.
* @param i_year in 4 digits
* @param i_month please keep in mind that first month of the year is 1 (ie: not 0!)
* @param i_day please keep in mind that first day of the month is 1 (ie: not 0!)
* @param i_hour
* @param i_minute
* @param i_second
* @param i_timezone timezone as an offset to GMT (in hours, optional)
* @param i_dst is it DST? (default: no)
*/
void Date::setDate(const int& i_year, const int& i_month, const int& i_day, const int& i_hour, const int& i_minute, const double& i_second, const double& i_timezone, const bool& i_dst)
{
	plausibilityCheck(i_year, i_month, i_day, i_hour, i_minute, i_second); //also checks leap years
	setTimeZone(i_timezone, i_dst);
	if (timezone==0 && dst==false) { //data is GMT and no DST
		gmt_julian = rnd( calculateJulianDate(i_year, i_month, i_day, i_hour, i_minute, i_second), epsilon_sec);
	} else {
		//computing local julian date
		const double local_julian = calculateJulianDate(i_year, i_month, i_day, i_hour, i_minute, i_second);
		gmt_julian = rnd( localToGMT(local_julian), epsilon_sec);
	}
	undef = false;
}

void Date::setDate(const int& year, const unsigned int& month, const unsigned int& day, const unsigned int& hour, const unsigned int& minute, const double& second, const double& in_timezone, const bool& in_dst)
{
	setDate(year, (signed)month, (signed)day, (signed)hour, (signed)minute, second, in_timezone, in_dst);
}

/**
* @brief Set date by elements.
* All values are checked for plausibility.
* @param i_year in 4 digits
* @param i_month please keep in mind that first month of the year is 1 (ie: not 0!)
* @param i_day please keep in mind that first day of the month is 1 (ie: not 0!)
* @param i_hour
* @param i_minute
* @param i_second
* @param i_timezone timezone as an offset to GMT (in hours, optional)
* @param i_dst is it DST? (default: no)
*/
void Date::setDate(const int& i_year, const int& i_month, const int& i_day, const int& i_hour, const int& i_minute, const int& i_second, const double& i_timezone, const bool& i_dst)
{
	setDate(i_year, i_month, i_day, i_hour, i_minute, static_cast<double>(i_second), i_timezone, i_dst);
}

void Date::setDate(const int& year, const unsigned int& month, const unsigned int& day, const unsigned int& hour, const unsigned int& minute, const unsigned int& second, const double& in_timezone, const bool& in_dst)
{
	setDate(year, (signed)month, (signed)day, (signed)hour, (signed)minute, static_cast<double>(second), in_timezone, in_dst);
}

/**
* @brief Set date by elements.
* All values are checked for plausibility.
* @param i_year in 4 digits
* @param i_month please keep in mind that first month of the year is 1 (ie: not 0!)
* @param i_day please keep in mind that first day of the month is 1 (ie: not 0!)
* @param i_hour
* @param i_minute
* @param i_timezone timezone as an offset to GMT (in hours, optional)
* @param i_dst is it DST? (default: no)
*/
void Date::setDate(const int& i_year, const int& i_month, const int& i_day, const int& i_hour, const int& i_minute, const double& i_timezone, const bool& i_dst)
{
	setDate(i_year, i_month, i_day, i_hour, i_minute, 0., i_timezone, i_dst);
}

void Date::setDate(const int& year, const unsigned int& month, const unsigned int& day, const unsigned int& hour, const unsigned int& minute, const double& in_timezone, const bool& in_dst)
{
	setDate(year, (signed)month, (signed)day, (signed)hour, (signed)minute, 0., in_timezone, in_dst);
}

/**
* @brief Set date from a julian date (JD).
* @param julian_in julian date to set
* @param in_timezone timezone as an offset to GMT (in hours, optional)
* @param in_dst is it DST? (default: no)
*/
void Date::setDate(const double& julian_in, const double& in_timezone, const bool& in_dst)
{
	setTimeZone(in_timezone, in_dst);
	gmt_julian = rnd( localToGMT(julian_in), epsilon_sec);
	undef = false;
}

/**
* @brief Set date from a year and Julian Day Number (JDN).
* @param year year to set
* @param jdn Julian Day Number within the provided year (1 to 365/366), starting at 00:00
* @param in_timezone timezone as an offset to GMT (in hours, optional)
* @param in_dst is it DST? (default: no)
*/
void Date::setDate(const int& year, const double& jdn, const double& in_timezone, const bool& in_dst)
{
	setTimeZone(in_timezone, in_dst);
	//jdn starts at 1, so we need to remove 1 day. Julian_date starts at noon, so we need to remove 0.5 day
	const double local_julian = static_cast<double>( getJulianDayNumber(year, 1, 1) ) + jdn-1.5;
	gmt_julian = rnd( localToGMT(local_julian), epsilon_sec);
	undef = false;
}

/**
* @brief Set date from a Unix date.
* @param i_time unix time (ie: as number of seconds since Unix Epoch, always UTC)
* @param i_dst is it DST? (default: no)
*/
void Date::setDate(const time_t& i_time, const bool& i_dst) {
	setUnixDate(i_time, i_dst);
}

/**
* @brief Set date from a modified julian date (MJD).
* @param julian_in julian date to set
* @param i_timezone timezone as an offset to GMT (in hours, optional)
* @param i_dst is it DST? (default: no)
*/
void Date::setModifiedJulianDate(const double& julian_in, const double& i_timezone, const bool& i_dst) {
	const double tmp_julian = julian_in + MJD_offset;
	setDate(tmp_julian, i_timezone, i_dst);
}

/**
* @brief Set date from an RFC868 date (time since 1900-01-01T00:00 GMT).
* @param julian_in julian date to set
* @param i_timezone timezone as an offset to GMT (in hours, optional)
* @param i_dst is it DST? (default: no)
*/
void Date::setRFC868Date(const double& julian_in, const double& i_timezone, const bool& i_dst) {
	const double tmp_julian = julian_in + RFC868_offset;
	setDate(tmp_julian, i_timezone, i_dst);
}

/**
* @brief Set date from a Unix date.
* @param in_time unix time (ie: as number of seconds since Unix Epoch, always UTC)
* @param in_dst is it DST? (default: no)
*/
void Date::setUnixDate(const time_t& in_time, const bool& in_dst) {
	const double in_julian = (double)(in_time)/(24.*3600.) + Unix_offset;
	setDate(in_julian, 0., in_dst);
}

/**
* @brief Set date from an Excel date.
* @param excel_in Excel date to set
* @param i_timezone timezone as an offset to GMT (in hours, optional)
* @param i_dst is it DST? (default: no)
*/
void Date::setExcelDate(const double excel_in, const double& i_timezone, const bool& i_dst) {
	//TODO: handle date < 1900-01-00 and date before 1900-03-01
	//see http://www.mathworks.com/help/toolbox/finance/x2mdate.html
	const double tmp_julian = excel_in + Excel_offset;
	setDate(tmp_julian, i_timezone, i_dst);
}

/**
* @brief Set date from an Matlab date.
* @param matlab_in Matlab date to set
* @param i_timezone timezone as an offset to GMT (in hours, optional)
* @param i_dst is it DST? (default: no)
*/
void Date::setMatlabDate(const double matlab_in, const double& i_timezone, const bool& i_dst) {
	const double tmp_julian = matlab_in + Matlab_offset;
	setDate(tmp_julian, i_timezone, i_dst);
}


// GETTERS
/**
* @brief Returns timezone.
* @return timezone as an offset to GMT
*/
double Date::getTimeZone() const {
	if (undef==true)
		throw UnknownValueException("Date object is undefined!", AT);

	return timezone;
}

/**
* @brief Returns Daylight Saving Time flag.
* @return dst enabled?
*/
bool Date::getDST() const {
	if (undef==true)
		throw UnknownValueException("Date object is undefined!", AT);

	return dst;
}

/**
* @brief Return julian date (JD).
* The julian date is defined as the fractional number of days since -4713-01-01T12:00 UTC.
* @param gmt convert returned value to GMT? (default: false)
* @return julian date in the current timezone / in GMT depending on the gmt parameter
*/
double Date::getJulian(const bool& gmt) const {
	if (undef==true)
		throw UnknownValueException("Date object is undefined!", AT);

	if (gmt) {
		return gmt_julian;
	} else {
		return GMTToLocal(gmt_julian);
	}
}

/**
* @brief Return modified julian date (MJD).
* The modified julian date is defined as the fractional number of days since 1858-11-17T00:00 UTC
* (definition by the Smithsonian Astrophysical Observatory, MA).
* @param gmt convert returned value to GMT? (default: false)
* @return modified julian date in the current timezone / in GMT depending on the gmt parameter
*/
double Date::getModifiedJulianDate(const bool& gmt) const {
	if (undef==true)
		throw UnknownValueException("Date object is undefined!", AT);

	if (gmt) {
		return (gmt_julian - MJD_offset);
	} else {
		return (GMTToLocal(gmt_julian) - MJD_offset);
	}
}

/**
* @brief Return RFC868 date.
* The RFC868 date is defined as the fractional number of days since 1900-01-01T00:00 GMT
* @param gmt convert returned value to GMT? (default: false)
* @return RFC868 julian date in the current timezone / in GMT depending on the gmt parameter
*/
double Date::getRFC868Date(const bool& gmt) const {
	if (undef==true)
		throw UnknownValueException("Date object is undefined!", AT);

	if (gmt) {
		return (gmt_julian - RFC868_offset);
	} else {
		return (GMTToLocal(gmt_julian) - RFC868_offset);
	}
}

/**
* @brief Return truncated julian date (TJD).
* The truncated julian date is defined as the julian day shifted to start at 00:00 and modulo 10000 days.
* The last origin (ie: 0) was 1995-10-10T00:00
* (definition by National Institute of Standards and Technology).
* @param gmt convert returned value to GMT? (default: false)
* @return truncated julian date in the current timezone / in GMT depending on the gmt parameter
*/
double Date::getTruncatedJulianDate(const bool& gmt) const {
	if (undef==true)
		throw UnknownValueException("Date object is undefined!", AT);

	if (gmt) {
		return (fmod( (gmt_julian - 0.5), 10000. ));
	} else {
		const double local_julian = GMTToLocal(gmt_julian);
		return (fmod( (local_julian - 0.5), 10000. ));
	}
}

/**
* @brief Return Unix time (or POSIX time).
* The Unix time is defined as the number of seconds since 1970-01-01T00:00 UTC (Unix Epoch).
* It is therefore ALWAYS in GMT.
* (defined as IEEE P1003.1 POSIX. See http://www.mail-archive.com/leapsecs@rom.usno.navy.mil/msg00109.html
* for some technical, historical and funny insight into the standardization process)
* @return Unix time in the current timezone / in GMT depending on the gmt parameter
*/
time_t Date::getUnixDate() const {
	if (undef==true)
		throw UnknownValueException("Date object is undefined!", AT);

	if (gmt_julian < Unix_offset)
		throw IOException("Dates before 1970 cannot be displayed in Unix epoch time", AT);

	return ( (time_t)floor( (gmt_julian - Unix_offset) * (24*60*60) ));
}

/**
* @brief Return Excel date.
* The (sick) Excel date is defined as the number of days since 1900-01-00T00:00 (no, this is NOT a typo).
* Moreover, it (wrongly) considers that 1900 was a leap year (in order to remain compatible with an old Lotus123 bug).
* This practically means that for dates after 1900-03-01, an Excel date really represents the number of days since 1900-01-01T00:00 PLUS 2.
* @param gmt convert returned value to GMT? (default: false)
* @return Excel date in the current timezone / in GMT depending on the gmt parameter
*/
double Date::getExcelDate(const bool& gmt) const {
	if (undef==true)
		throw UnknownValueException("Date object is undefined!", AT);

	if (gmt_julian < Excel_offset)
		throw IOException("Dates before 1900 cannot be converted to Excel date", AT);

	if (gmt) {
		return ( gmt_julian - Excel_offset);
	} else {
		return ( GMTToLocal(gmt_julian) - Excel_offset);
	}
}

/**
* @brief Return Matlab date.
* This is the number of days since 0000-01-01T00:00:00. See http://www.mathworks.com/help/techdoc/ref/datenum.html
* @param gmt convert returned value to GMT? (default: false)
* @return Matlab date in the current timezone / in GMT depending on the gmt parameter
*/
double Date::getMatlabDate(const bool& gmt) const {
	if (undef==true)
		throw UnknownValueException("Date object is undefined!", AT);

	if (gmt) {
		return ( gmt_julian - Matlab_offset);
	} else {
		return ( GMTToLocal(gmt_julian) - Matlab_offset);
	}
}


/**
* @brief Retrieve julian date.
* This method is a candidate for deletion: it should now be obsolete.
* @param julian_out julian date (in local time zone or GMT depending on the gmt flag)
* @param gmt convert returned value to GMT? (default: false)
*/
void Date::getDate(double& julian_out, const bool& gmt) const {
	if (undef==true)
		throw UnknownValueException("Date object is undefined!", AT);

	julian_out = (gmt)? gmt_julian : GMTToLocal(gmt_julian);
}

/**
* @brief Return year.
* @param gmt convert returned value to GMT? (default: false)
* @return year
*/
int Date::getYear(const bool& gmt) const {
	if (undef==true)
		throw UnknownValueException("Date object is undefined!", AT);

	const double julian = (gmt)? gmt_julian : GMTToLocal(gmt_julian);
	int year, month, day;
	calculateDate(julian, year, month, day);
	return year;
}


/**
* @brief Return time of the day.
* @param hour_out
* @param minute_out
* @param gmt convert returned value to GMT? (default: false)
*/
void Date::getTime(int& hour_out, int& minute_out, const bool& gmt) const {
	if (undef==true)
		throw UnknownValueException("Date object is undefined!", AT);
	
	const double julian = (gmt)? gmt_julian : GMTToLocal(gmt_julian);
	double seconds;
	calculateTime(julian, hour_out, minute_out, seconds);
}

/**
* @brief Return time of the day.
* @param hour_out
* @param minute_out
* @param second_out
* @param gmt convert returned value to GMT? (default: false)
*/
void Date::getTime(int& hour_out, int& minute_out, double& second_out, const bool& gmt) const {
	if (undef==true)
		throw UnknownValueException("Date object is undefined!", AT);

	const double julian = (gmt)? gmt_julian : GMTToLocal(gmt_julian);
	calculateTime(julian, hour_out, minute_out, second_out);
}

/**
* @brief Return year, month, day.
* @param year_out
* @param month_out
* @param day_out
* @param gmt convert returned value to GMT? (default: false)
*/
void Date::getDate(int& year_out, int& month_out, int& day_out, const bool& gmt) const {
	if (undef==true)
		throw UnknownValueException("Date object is undefined!", AT);

	const double julian = (gmt)? gmt_julian : GMTToLocal(gmt_julian);
	calculateDate(julian, year_out, month_out, day_out);
}

/**
* @brief Return year, month, day.
* @param year_out
* @param month_out
* @param day_out
* @param hour_out
* @param gmt convert returned value to GMT? (default: false)
*/
void Date::getDate(int& year_out, int& month_out, int& day_out, int& hour_out, const bool& gmt) const {
	if (undef==true)
		throw UnknownValueException("Date object is undefined!", AT);

	const double julian = (gmt)? gmt_julian : GMTToLocal(gmt_julian);
	int minute;
	double second;
	calculateValues(julian, year_out, month_out, day_out, hour_out, minute, second);
}

/**
* @brief Return year, month, day.
* @param year_out
* @param month_out
* @param day_out
* @param hour_out
* @param minute_out
* @param gmt convert returned value to GMT? (default: false)
*/
void Date::getDate(int& year_out, int& month_out, int& day_out, int& hour_out, int& minute_out, const bool& gmt) const {
	if (undef==true)
		throw UnknownValueException("Date object is undefined!", AT);

	const double julian = (gmt)? gmt_julian : GMTToLocal(gmt_julian);
	double second;
	calculateValues(julian, year_out, month_out, day_out, hour_out, minute_out, second);
}

/**
* @brief Return year, month, day.
* @param year_out
* @param month_out
* @param day_out
* @param hour_out
* @param minute_out
* @param second_out
* @param gmt convert returned value to GMT? (default: false)
*/
void Date::getDate(int& year_out, int& month_out, int& day_out, int& hour_out, int& minute_out, double& second_out, const bool& gmt) const {
	if (undef==true)
		throw UnknownValueException("Date object is undefined!", AT);

	const double julian = (gmt)? gmt_julian : GMTToLocal(gmt_julian);
	calculateValues(julian, year_out, month_out, day_out, hour_out, minute_out, second_out);
}

void Date::getDate(int& year_out, int& month_out, int& day_out, int& hour_out, int& minute_out, int& second_out, const bool& gmt) const {
	if (undef==true)
		throw UnknownValueException("Date object is undefined!", AT);

	const double julian = (gmt)? gmt_julian : GMTToLocal(gmt_julian);
	double second;
	calculateValues(julian, year_out, month_out, day_out, hour_out, minute_out, second);
	second_out = static_cast<int>( second+.5 );
}

/**
* @brief Return the day of the week for the current date.
* The day of the week is between 1 (Monday) and 7 (Sunday).
* @param gmt convert returned value to GMT? (default: false)
* @return day of the week in [1, 7]
*/
unsigned short Date::getDayOfWeek(const bool& gmt) const {
//principle: start from day julian=0 that is a Monday
	if (gmt) {
		const unsigned int dow = static_cast<unsigned int>(gmt_julian+.5) % 7 + 1;
		return static_cast<unsigned short>(dow);
	} else {
		const double local_julian = GMTToLocal(gmt_julian);
		const unsigned int dow = static_cast<unsigned int>(local_julian+.5) % 7 + 1;
		return static_cast<unsigned short>(dow);
	}
}

/**
* @brief Return the ISO 8601 week number
* The week number range from 1 to 53 for a leap year. The first week is the week that contains
* the first Thursday of the year. Previous days are attributed to the last week of the previous
* year (See https://en.wikipedia.org/wiki/ISO_week_date).
* @param[out] ISO_year get filled with the matching ISO year (since the fist/last few days might belong to the previous/next year)
* @param gmt convert returned value to GMT? (default: false)
* @return ISO 8601 week number in [1, 53]
*/
unsigned short Date::getISOWeekNr(int &ISO_year, const bool& gmt) const
{
	ISO_year = getYear(gmt);
	const double jdn = getJulianDayNumber(gmt);
	Date newYear(*this - jdn + 1);
	const unsigned short newYear_dow = newYear.getDayOfWeek(gmt);
	const int firstThursday = (7 - newYear_dow + 4) % 7 + 1; //first Thursday of the year belongs to week 1
	const int firstWeekMonday = firstThursday - 3; //this could be <0, for example if Jan 01 is a Thursday

	if (jdn>=359) { //handle the last few days before the new year that might belong to week 1
		const bool is_leapYear = isLeapYear();
		const int jdn_last = (is_leapYear)? 366 : 365;
		const unsigned char week_offset = (is_leapYear)? 1 : 0; //for leap years, dec. 31 is one dow later as jan. 1st
		const double lastDay_dow = (newYear_dow + week_offset - 1) % 7 + 1;
		const double lastMonday = jdn_last - lastDay_dow + 1; //dow starts at 1
		if (jdn>=lastMonday && lastDay_dow<4) {
			ISO_year++;
			return 1;
		}
	}

	//these are certainly normal days, ie no special case
	if (jdn>=firstWeekMonday) { //at worst, we are in week 01, otherwise after...
		return static_cast<unsigned short>( Optim::intPart( (jdn+3-(double)firstThursday) / 7 ) + 1);
	} else {
		//handle the first few days of the new year that are before week 1
		//we are *before* the Monday of the first week. This implies that dow>4 (otherwise, the current week would be week 01)
		//so these few days belong to the last week of the previous year
		ISO_year--;
		if (newYear_dow==5) return 53; // Friday indicates a leap year
		if (newYear_dow==7) return 52; // Sunday is no leap year

		//Saturday depends on the year before...
		if (isLeapYear(ISO_year)) return 53;
		else return 52;
	}
}

/**
* @brief Return the ISO 8601 week number
* The week number range from 1 to 53 for a leap year. The first week is the week that contains
* the first Thursday of the year. Previous days are attributed to the last week of the previous
* year (See https://en.wikipedia.org/wiki/ISO_week_date).
* @param gmt convert returned value to GMT? (default: false)
* @return ISO 8601 week number in [1, 53]
*/
unsigned short Date::getISOWeekNr(const bool& gmt) const
{
	int ISO_year;
	return getISOWeekNr(ISO_year, gmt);
}

/**
* @brief Return the julian day for the current date.
* Return the day of the year index for the current Date object
* @param gmt convert returned value to GMT? (default: false)
* @return julian day number, starting from 1
*/
int Date::getJulianDayNumber(const bool& gmt) const {
	if (undef==true)
		throw UnknownValueException("Date object is undefined!", AT);

	if (gmt) {
		const int gmt_year = getYear();
		const double first_day_of_year = static_cast<double>(getJulianDayNumber(gmt_year, 1, 1));
		return static_cast<int>(gmt_julian - first_day_of_year + 1.5);
	} else {
		const double local_julian = GMTToLocal(gmt_julian);
		int local_year, local_month, local_day;
		calculateDate(local_julian, local_year, local_month, local_day);
		return static_cast<int>(Optim::intPart(local_julian+0.5)) - static_cast<int>(getJulianDayNumber(local_year, 1, 1)) + 1;
	}
}

/**
* @brief Return true if the current year is a leap year
* @return true if the current year is a leap year
*/
bool Date::isLeapYear() const {
	if (undef==true)
		throw UnknownValueException("Date object is undefined!", AT);

	return (isLeapYear( getYear() ));
}

/**
 * @brief Modulus of a julian date by a given number of seconds.
 * This returns the modulus (in seconds) of a given date by the given number of seconds.
 * @param[in] julian input date
 * @param[in] seconds period (in seconds)
 * @return modulus in seconds
 */
unsigned int Date::mod(const double& julian, const unsigned int& seconds)
{
	const unsigned int julian_mod = static_cast<unsigned int>( round( fmod(julian*24.*3600., seconds) ) ); //this reduces the rounding errors and allows second precision
	if (julian_mod==seconds) return 0; //since after "round" it is possible that julian_mod==seconds
	else return julian_mod;
}

/**
 * @brief Modulus of a date by a given number of seconds.
 * This returns the modulus (in seconds) of a given date by the given number of seconds.
 * @param[in] indate input date
 * @param[in] seconds period (in seconds)
 * @return modulus in seconds
 */
unsigned int Date::mod(const Date& indate, const unsigned int& seconds)
{
	return mod(indate.getJulian(), seconds);
}

/**
 * @brief Round a julian date to a given precision.
 * If you want to round a local date, do NOT provide it as gmt julian but as local julian,
 * otherwise things like rounding to the next day would be shifted by TimeZone.
 * @param julian date to round
 * @param precision round date to the given precision, in seconds
 * @param type rounding strategy (default: CLOSEST)
 * @return rounded date
 */
double Date::rnd(const double& julian, const double& precision, const RND& type)
{
	if (precision <= 0)
		throw InvalidArgumentException("Can not round dates to 0 seconds precision!", AT);

	double integral;
	const double fractional = modf(julian-.5, &integral);
	const double rnd_factor = (3600.*24.) / precision;

	if (type==CLOSEST)
		return integral + (double)Optim::round( fractional*rnd_factor ) / rnd_factor + .5;
	if (type==UP)
		return integral + (double)Optim::ceil( fractional*rnd_factor ) / rnd_factor + .5;
	if (type==DOWN)
		return integral + (double)Optim::floor( fractional*rnd_factor ) / rnd_factor + .5;

	throw UnknownValueException("Unknown rounding strategy!", AT);
}

/**
 * @brief Round date to a given precision
 * @param precision round date to the given precision, in seconds
 * @param type rounding strategy (default: CLOSEST)
 */
void Date::rnd(const double& precision, const RND& type) {
	if (!undef) {
		const double rnd_julian = rnd( getJulian(false), precision, type ); //round local time
		setDate(rnd_julian, timezone, dst);
	}
}

const Date Date::rnd(const Date& indate, const double& precision, const RND& type) {
	Date tmp(indate);
	if (!tmp.undef) {
		const double rnd_julian = rnd( tmp.getJulian(false), precision, type ); //round local time
		tmp.setDate(rnd_julian, tmp.getTimeZone(), tmp.getDST());
	}

	return tmp;
}

// OPERATORS //HACK this will have to handle Durations
Date& Date::operator+=(const Date& indate) {
	if (undef==true || indate.isUndef()) {
		undef=true;
		return *this;
	}
	gmt_julian += indate.gmt_julian;
	rnd(epsilon_sec); //round to internal precision
	return *this;
}

Date& Date::operator-=(const Date& indate) {
	if (undef==true || indate.isUndef()) {
		undef=true;
		return *this;
	}
	gmt_julian -= indate.gmt_julian;
	rnd(epsilon_sec); //round to internal precision
	return *this;
}

Date& Date::operator+=(const double& indate) {
	if (undef==false) {
		gmt_julian += indate;
		rnd(epsilon_sec); //round to internal precision
	}
	return *this;
}

Date& Date::operator-=(const double& indate) {
	if (undef==false) {
		gmt_julian -= indate;
		rnd(epsilon_sec); //round to internal precision
	}
	return *this;
}

Date& Date::operator*=(const double& value) {
	if (undef==false) {
		gmt_julian *= value;
		rnd(epsilon_sec); //round to internal precision
	}
	return *this;
}

Date& Date::operator/=(const double& value) {
	if (undef==false) {
		gmt_julian /= value;
		rnd(epsilon_sec); //round to internal precision
	}
	return *this;
}

bool Date::operator==(const Date& indate) const {
	if (undef==true || indate.isUndef()) {
		return( undef==true && indate.isUndef() );
	}

	return (fabs(gmt_julian - indate.gmt_julian) <= epsilon);
}

bool Date::operator!=(const Date& indate) const {
	return !(*this==indate);
}

bool Date::operator<(const Date& indate) const {
	if (undef==true || indate.isUndef()) {
		throw UnknownValueException("Date object is undefined!", AT);
	}

#ifdef NEGATIVE_JULIAN
	if (*this==indate) return false;
	return (gmt_julian < indate.gmt_julian);
#else
	return (gmt_julian < (indate.gmt_julian-epsilon));
#endif
}

bool Date::operator<=(const Date& indate) const {
	if (undef==true || indate.isUndef()) {
		throw UnknownValueException("Date object is undefined!", AT);
	}

#ifdef NEGATIVE_JULIAN
	if (*this==indate) return true;
	return (gmt_julian <= indate.gmt_julian);
#else
	return (gmt_julian <= (indate.gmt_julian+epsilon));
#endif
}

bool Date::operator>(const Date& indate) const {
	if (undef==true || indate.isUndef()) {
		throw UnknownValueException("Date object is undefined!", AT);
	}

#ifdef NEGATIVE_JULIAN
	if (*this==indate) return false;
	return (gmt_julian > indate.gmt_julian);
#else
	return (gmt_julian > (indate.gmt_julian+epsilon));
#endif
}

bool Date::operator>=(const Date& indate) const {
	if (undef==true || indate.isUndef()) {
		throw UnknownValueException("Date object is undefined!", AT);
	}

#ifdef NEGATIVE_JULIAN
	if (*this==indate) return true;
	return (gmt_julian >= indate.gmt_julian);
#else
	return (gmt_julian >= (indate.gmt_julian-epsilon));
#endif
}

const Date Date::operator+(const Date& indate) const {
	if (undef==true || indate.isUndef()) {
		Date tmp; //create an Undef date
		return tmp;
	}

	Date tmp(gmt_julian + indate.gmt_julian, 0.);
	tmp.setTimeZone(timezone);
	return tmp;
}

const Date Date::operator-(const Date& indate) const {
	if (undef==true || indate.isUndef()) {
		Date tmp; //create an Undef date
		return tmp;
	}

	Date tmp(gmt_julian - indate.gmt_julian, 0.);
	tmp.setTimeZone(timezone);
	return tmp;
}

const Date Date::operator+(const double& indate) const {
	//remains undef if undef
	Date tmp(gmt_julian + indate, 0.);
	tmp.setTimeZone(timezone);
	return tmp;
}

const Date Date::operator-(const double& indate) const {
	//remains undef if undef
	Date tmp(gmt_julian - indate, 0.);
	tmp.setTimeZone(timezone);
	return tmp;
}

const Date Date::operator*(const double& value) const {
	//remains undef if undef
	Date tmp(gmt_julian * value, 0.);
	tmp.setTimeZone(timezone);
	return tmp;
}

const Date Date::operator/(const double& value) const {
	//remains undef if undef
	Date tmp(gmt_julian / value, 0.);
	tmp.setTimeZone(timezone);
	return tmp;
}

/**
* @brief Parse an ISO 8601 formatted time zone specification.
* Time zones MUST be specified right after a date/time/combined representation
* according to the following formats:
*   - 'Z' like in 2013-02-13T19:43Z, meaning GMT
*   - '+01' like in 2013-02-13T20:43+01 meaning GMT+1
*   - '+0130' like in 2013-02-13T21:13+0130 meaning GMT+1.5
*   - '+01:30' like in 2013-02-13T21:13+01:30 meaning GMT+1.5
*   - '-0515' like in 2013-02-13T15:28-0515 meaning GMT-5.25
* See https://en.wikipedia.org/wiki/ISO_8601 for more information
* @param timezone_iso time zone string
* @return time zone/shift in hours
*/
double Date::parseTimeZone(const std::string& timezone_iso)
{
	if (timezone_iso.empty()) //just in case, to avoid a segfault
		return 0.;

	if (timezone_iso=="Z") { //time as Z
		return 0.;
	} else if (timezone_iso[0]=='+' || timezone_iso[0]=='-') {
		const char *c_str = timezone_iso.c_str();
		const size_t str_size = timezone_iso.size();
		switch(str_size) {
			case 6: { //timezone as +01:00
				int tz_h, tz_min;
				if ( sscanf(c_str, "%d:%d", &tz_h, &tz_min) == 2 ) {
					//if there is a "-", apply it to the minutes
					if (tz_h>=0.)
						return (double)tz_h + (double)tz_min/60.;
					else
						return (double)tz_h - (double)tz_min/60.;
				} else {
					return IOUtils::nodata;
				}
			}
			case 5: { //timezone as +0100
				int tz_tmp;
				if ( sscanf(c_str, "%d", &tz_tmp) == 1 ) {
					const int tz_h = tz_tmp/100;
					const int tz_min = tz_tmp-100*tz_h;
					return (double)tz_h + (double)tz_min/60.;
				} else {
					return IOUtils::nodata;
				}
			}
			case 3: { //timezone as -01
				int tz_h;
				if ( sscanf(c_str, "%d", &tz_h) == 1 )
					return (double)tz_h;
				else
					return IOUtils::nodata;
			}
			default: {
				return IOUtils::nodata;
			}
		}
	} else if (TZAbbrev.count(timezone_iso)) {
		return TZAbbrev[ timezone_iso ];
	} else {
		return IOUtils::nodata;
	}
}

/**
* @brief Nicely format an hour given as fractional day into a human readable hour.
* @param fractional fractional day (ie: fractional part of a julian date)
* @return string containing a human readable time
*/
std::string Date::printFractionalDay(const double& fractional) {
	const double hours = floor(fractional*24.);
	const double minutes = floor((fractional*24.-hours)*60.);
	const double seconds = fractional*24.*3600.-hours*3600.-minutes*60.;

	std::ostringstream tmp;
	tmp << std::fixed << std::setfill('0') << std::setprecision(0);
	tmp << std::setw(2) << hours << ":";
	tmp << std::setw(2) << minutes << ":";
	tmp << std::setw(2) << seconds;

	return tmp.str();
}

/**
* @brief Return a nicely formated string.
* @param type select the formating to apply (see the definition of Date::FORMATS)
* @param gmt convert returned value to GMT? (default: false)
* @return formatted time in a string
*/
const string Date::toString(const FORMATS& type, const bool& gmt) const
{
	if (undef==true)
		throw UnknownValueException("Date object is undefined!", AT);
		//return std::string("[Undef]"); //for debug purposes
	
	//the date are displayed in LOCAL timezone (more user friendly)
	const double julian_out = (gmt || (type==ISO_Z))? gmt_julian : GMTToLocal(gmt_julian);
	int year_out, month_out, day_out, hour_out, minute_out;
	double second_out;
	calculateValues(julian_out, year_out, month_out, day_out, hour_out, minute_out, second_out);
	double whole_sec;
	const double subseconds = modf( second_out, &whole_sec);
	std::string subsec_str;
	if (subseconds>=5e-4) { //to be consistent with the resolution below
		std::ostringstream subsec_tmp;
		subsec_tmp << std::fixed << '.' << setw(3) << setprecision(3) << setfill('0') << (int)(subseconds*1000. + .5);
		subsec_str = subsec_tmp.str();
	}

	std::ostringstream tmpstr;
	switch(type) {
		case(ISO_TZ):
		case(ISO_Z):
		case(ISO):
			tmpstr
			<< setw(4) << setfill('0') << year_out << "-"
			<< setw(2) << setfill('0') << month_out << "-"
			<< setw(2) << setfill('0') << day_out << "T"
			<< setw(2) << setfill('0') << hour_out << ":"
			<< setw(2) << setfill('0') << minute_out << ":"
			<< setw(2) << setfill('0') << whole_sec << subsec_str;
			if (type==ISO_Z) {
				tmpstr << "Z";
			} else if (type==ISO_TZ) {
				int tz_h, tz_min;
				if (timezone>=0.) {
					tz_h = static_cast<int>(timezone);
					tz_min = static_cast<int>( (timezone - (double)tz_h)*60. + .5 ); //round to closest
					tmpstr << "+";
				} else {
					tz_h = -static_cast<int>(timezone);
					tz_min = -static_cast<int>( (timezone + (double)tz_h)*60. + .5 ); //round to closest
					tmpstr << "-";
				}
				tmpstr << setw(2) << setfill('0') << tz_h << ":"
				<< setw(2) << setfill('0') << tz_min;
			}
			break;
		case(ISO_DATE):
			tmpstr
			<< setw(4) << setfill('0') << year_out << "-"
			<< setw(2) << setfill('0') << month_out << "-"
			<< setw(2) << setfill('0') << day_out;
			break;
		case(NUM):
			tmpstr
			<< setw(4) << setfill('0') << year_out
			<< setw(2) << setfill('0') << month_out
			<< setw(2) << setfill('0') << day_out
			<< setw(2) << setfill('0') << hour_out
			<< setw(2) << setfill('0') << minute_out
			<< setw(2) << setfill('0') << whole_sec;
			break;
		case(FULL):
			tmpstr
			<< setw(4) << setfill('0') << year_out << "-"
			<< setw(2) << setfill('0') << month_out << "-"
			<< setw(2) << setfill('0') << day_out << "T"
			<< setw(2) << setfill('0') << hour_out << ":"
			<< setw(2) << setfill('0') << minute_out <<":"
			<< setw(2) << setfill('0') << whole_sec << subsec_str << " ("
			<< setprecision(10) << julian_out << ") GMT"
			<< setw(2) << setfill('0') << showpos << timezone << noshowpos;
			break;
		case(DIN):
			tmpstr
			<< setw(2) << setfill('0') << day_out << "."
			<< setw(2) << setfill('0') << month_out << "."
			<< setw(4) << setfill('0') << year_out << " "
			<< setw(2) << setfill('0') << hour_out << ":"
			<< setw(2) << setfill('0') << minute_out << ":"
			<< setw(2) << setfill('0') << whole_sec << subsec_str;
			break;
		case(ISO_WEEK):
		{
			int ISO_year;
			const int ISO_week = getISOWeekNr(ISO_year, gmt);
			tmpstr
			<< setw(4) << setfill('0') << ISO_year << "-W"
			<< setw(2) << setfill('0') << ISO_week << "-"
			<< setw(2) << setfill('0') << getDayOfWeek(gmt);
			break;
		}
		default:
			throw InvalidFormatException("Unsupported time format", AT);
	}

	return tmpstr.str();
}

const std::string Date::toString() const {
	std::ostringstream os;
	os << "<date>\n";
	if (undef==true)
		os << "Undefined\n";
	else {
		os << toString(Date::ISO) << "\n";
		os << "TZ=GMT" << showpos << timezone << noshowpos << "\t\t" << "DST=" << dst << "\n";
		os << "julian:\t\t\t" << setprecision(10) << getJulian() << "\t(GMT=" << getJulian(true) << ")\n";
		os << "ModifiedJulian:\t\t" << getModifiedJulianDate() << "\n";
		//os << "TruncatedJulian:\t" << getTruncatedJulianDate() << "\n";
		//os << "MatlabJulian:\t\t" << getMatlabDate() << "\n";
		try {
			os << "Unix:\t\t\t" << getUnixDate() << "\n";
		} catch (...) {}
		try {
			os << "Excel:\t\t\t" << getExcelDate() << "\n";
		} catch (...) {}
	}
	os << "</date>\n";
	return os.str();
}

std::ostream& operator<<(std::ostream& os, const Date& date) {
	os.write(reinterpret_cast<const char*>(&date.timezone), sizeof(date.timezone));
	os.write(reinterpret_cast<const char*>(&date.gmt_julian), sizeof(date.gmt_julian));

	os.write(reinterpret_cast<const char*>(&date.dst), sizeof(date.dst));
	os.write(reinterpret_cast<const char*>(&date.undef), sizeof(date.undef));
	return os;
}

std::istream& operator>>(std::istream& is, Date& date) {
	is.read(reinterpret_cast<char*>(&date.timezone), sizeof(date.timezone));
	is.read(reinterpret_cast<char*>(&date.gmt_julian), sizeof(date.gmt_julian));

	is.read(reinterpret_cast<char*>(&date.dst), sizeof(date.dst));
	is.read(reinterpret_cast<char*>(&date.undef), sizeof(date.undef));
	return is;
}

// PRIVATE METHODS
double Date::calculateJulianDate(const int& i_year, const int& i_month, const int& i_day, const int& i_hour, const int& i_minute, const double& i_second)
{
	const long julday = getJulianDayNumber(i_year, i_month, i_day);
	const double frac = (i_hour-12.)/24. + i_minute/(24.*60.) + i_second/(24.*3600.); //the julian date reference is at 12:00

	return (((double)julday) + frac);
}

/**
 * @brief Calculate the date components (y,m,d) from a given julian date
 * @details This comes from Fliegel, H. F. and van Flandern, T. C. 1968. <i>Letters to the editor: a machine algorithm for 
 * processing calendar dates</i>; Commun. ACM 11, <b>10</b> (Oct. 1968), 657. DOI= http://doi.acm.org/10.1145/364096.364097
 * @param[in] i_julian julian date
 * @param[out] o_year the extracted year
 * @param[out] o_month the extracted month
 * @param[out] o_day the extracted day
 */
void Date::calculateDate(const double& i_julian, int& o_year, int& o_month, int& o_day)
{
	//we round the given julian date to our current resolution
	const double tmp_julian = rnd(i_julian, epsilon_sec);

	const long julday = Optim::floor(tmp_julian+0.5);
	long t1 = julday + 68569L;
	const long t2 = 4L * t1 / 146097L;
	t1 = t1 - ( 146097L * t2 + 3L ) / 4L;
	const long yr = 4000L * ( t1 + 1L ) / 1461001L;
	t1 = t1 - 1461L * yr / 4L + 31L;
	const long mo = 80L * t1 / 2447L;

	o_day = (int) ( t1 - 2447L * mo / 80L );
	t1 = mo / 11L;
	o_month = (int) ( mo + 2L - 12L * t1 );
	o_year = (int) ( 100L * ( t2 - 49L ) + yr + t1 );

	// Correct for BC years -> astronomical year, that is from year -1 to year 0
	if ( o_year <= 0 ) o_year--;
}

/**
 * @brief Calculate the time components (y,m,d) from a given julian date
 * @param[in] i_julian julian date
 * @param[out] o_hour the extracted hour of the day
 * @param[out] o_minute the extracted minutes
 * @param[out] o_second the extracted seconds
 */
void Date::calculateTime(const double& i_julian, int& o_hour, int& o_minute, double& o_second)
{
	//we round the given julian date to our current resolution
	const double tmp_julian = rnd(i_julian, epsilon_sec);
	double integral;
	const double total_sec = (double)Optim::round( modf(tmp_julian+.5, &integral) * (24.*3600.) / epsilon_sec) * epsilon_sec; //the julian date reference is at 12:00
	
	o_hour   = static_cast<int>(total_sec/3600.);
	o_minute = static_cast<int>((total_sec - static_cast<double>(3600*o_hour)) / 60.);
	o_second = total_sec - static_cast<double>(3600*o_hour + 60*o_minute);
}

void Date::calculateValues(const double& i_julian, int& o_year, int& o_month, int& o_day, int& o_hour, int& o_minute, double& o_second)
{
	calculateDate(i_julian, o_year, o_month, o_day);
	calculateTime(i_julian, o_hour, o_minute, o_second);
}

bool Date::isLeapYear(const int& i_year) {
	//Using the leap year rule: years that can be divided by 4 if they are not centuries
	//For centuries, they are leap years only if they can be divided by 400
	const bool is_leapYear = (i_year%4 == 0 && (i_year %100 != 0 || i_year%400 == 0));
	return is_leapYear;
}

long Date::getJulianDayNumber(const int& i_year, const int& i_month, const int& i_day)
{ //given year, month, day, calculate the matching julian day. Watch out: Julian Dates start at noon, not at midnight!!
 //see Fliegel, H. F. and van Flandern, T. C. 1968. Letters to the editor: a machine algorithm for processing calendar dates. Commun. ACM 11, 10 (Oct. 1968), 657. DOI= http://doi.acm.org/10.1145/364096.364097
	const long lmonth = (long) i_month, lday = (long) i_day;
	long lyear = (long) i_year;

	// Correct for BC years -> astronomical year, that is from year -1 to year 0
	if ( lyear < 0 ) lyear++;

	const long jdn = lday - 32075L +
	                  1461L * ( lyear + 4800L + ( lmonth - 14L ) / 12L ) / 4L +
	                  367L * ( lmonth - 2L - ( lmonth - 14L ) / 12L * 12L ) / 12L -
	                  3L * ( ( lyear + 4900L + ( lmonth - 14L ) / 12L ) / 100L ) / 4L;

	return jdn;
}

void Date::plausibilityCheck(const int& in_year, const int& in_month, const int& in_day, const int& in_hour, const int& in_minute, const double& in_second) {
	if ((in_year < -4713) || (in_year >3000)
	    || (in_month < 1) || (in_month > 12)
	    || (in_day < 1) || ((in_day > daysNonLeapYear[in_month-1]) && !isLeapYear(in_year))
	    || ((in_day > daysLeapYear[in_month-1]) && isLeapYear(in_year))
	    || (in_hour < 0) || (in_hour > 24)
	    || (in_minute < 0) || (in_minute > 59)
	    || (in_second < 0.) || (in_second >= 60.)) {
		ostringstream ss;
		ss << std::fixed << std::setfill('0') << std::setprecision(0);
		ss << "Invalid Date requested: " << std::setw(4) << in_year << "-";
		ss << std::setw(2) << in_month << "-";
		ss << std::setw(2) << in_day << "T";
		ss << std::setw(2) << in_hour << ":";
		ss << std::setw(2) << in_minute << ":";
		ss << std::setw(2) << in_second;
		throw IOException(ss.str(), AT);
	}

	if ((in_hour == 24) && (in_minute != 0) && (in_second != 0.)) {
		ostringstream ss;
		ss << std::fixed << std::setfill('0') << std::setprecision(0);
		ss << "Invalid Date requested: " << std::setw(4) << in_year << "-";
		ss << std::setw(2) << in_month << "-";
		ss << std::setw(2) << in_day << "T";
		ss << std::setw(2) << in_hour << ":";
		ss << std::setw(2) << in_minute << ":";
		ss << std::setw(2) << in_second;
		throw IOException(ss.str(), AT);
	}
}

double Date::localToGMT(const double& i_julian) const {
	if (dst) {
		return (i_julian - timezone/24. - DST_shift/24.);
	} else {
		return (i_julian - timezone/24.);
	}
}

double Date::GMTToLocal(const double& i_gmt_julian) const {
	if (dst) {
		return (i_gmt_julian + timezone/24. + DST_shift/24.);
	} else {
		return (i_gmt_julian + timezone/24.);
	}
}

} //namespace
