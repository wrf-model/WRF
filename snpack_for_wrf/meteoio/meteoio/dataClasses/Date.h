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
#ifndef DATE_H
#define DATE_H

#include <meteoio/IOExceptions.h>

#include <string>
#include <sstream>
#include <map>

//#define NEGATIVE_JULIAN
namespace mio {

/**
 * @class Date
 * @brief  A class to handle timestamps.
 * This class handles conversion between different time display formats (ISO, numeric) as well as different
 * time representation (julian date, modified julian date, etc). It also handles time zones as well as
 * very basic Daylight Saving Time (DST). Since the activation dates of DST are political and not technical,
 * it can not be automatically calculated. Therefore, it has to be provided by the caller: when the dst flag
 * is set, the dst time shift is automatically applied. When the dst flag ceases to be set, the dst time shift
 * is no longer applied. This is very crude, but please keep in mind that using DST for monitoring data is
 * usually a bad idea... Finally, we assume that dates are positive. If this would not be the case, this
 * class has to be recompiled with the proper define.
 *
 * Internally, the date is stored as true julian date in GMT.
 * The maximal precision is 1 second (however with the limitation that leap seconds are currently not handled).
 *
 * Please see Date::FORMATS for supported display formats and http://en.wikipedia.org/wiki/Julian_day for
 * the various date representation definitions. The following date representation are currently supported:
 * - julian date, see Date::getJulian
 * - modified julian date, see Date::getModifiedJulianDate
 * - truncated julian date, see Date::getTruncatedJulianDate
 * - Unix date, see Date::getUnixDate
 * - Excel date, see Date::getExcelDate
 *
 * \anchor date_formats
 * When parsing a string to extract a date (using IOUtils::convertString), various string representations are supported (the timezone term {TZ} is optional \em if a
 * fallback timezone is available, most probably as Input::TIME_ZONE in the configuration file) with their <em>reduced accuracy</em> variations (see below):
 * - <A HREF="https://en.wikipedia.org/wiki/ISO_8601">ISO 8601</A> variations:
 *     - YYYY-MM-DDThh:mm:ss{TZ}, for example 2017-02-02T12:35:00 in the fallback time zone
 *     - YYYY-MM-DDThh:mm{TZ}, for example 2017-02-02T12:35+01:00
 * - similar to ISO but without the 'T' marker (some systems wrongfully reject it):
 *     - YYYY-MM-DD hh:mm:ss{TZ}, for example 2017-02-02 11:35:00Z
 *     - YYYY-MM-DD hh:mm{TZ}, for example 2017-02-02 12:35+01
 * - simplified:
 *     - YYYY-MM-DD{TZ}, for example 2017-02-01
 *     - hh:mm{TZ}, for example 15:12+01:30
 * - numeric:
 *     - YYYYMMDDHHmmss{TZ}
 *     - YYYYMMDDHHmm{TZ}
 *     - YYYYMMDDHH{TZ}
 * - relative, with keyword (in this case a fallback time zone must be available from somewhere else):
 *     - NOW
 *     - NOW±{offset} (the offset is in seconds), for example NOW+3600
 *     - NOW±hh:mm (the offset is in hours and minutes), for example NOW-01:30
 *
 * The timezone information {TZ} is as laid out in Date::parseTimeZone. <em>Reduced accuracy</em> means that any number of values may be dropped from the last
 * to the most significant from the date and time representation.
 *
 * @ingroup data_str
 * @author Mathias Bavay
 * @date 2010-04-15
 */

class Date {
	public:
		///Keywords for selecting the date formats (the subsecond resolution is dropped when not needed)
		typedef enum {
			ISO, ///< ISO 8601 extended format combined date: YYYY-MM-DDTHH:mm:SS.sss (fields might be dropped, in the least to the most significant order)
			ISO_TZ, ///< ISO 8601 format (same as ISO) but with time zone specification
			ISO_Z, ///< ISO 8601 format, forcing GMT and Zulu (Z) timezone specification
			FULL, ///< ISO 8601 followed by the julian date (in parenthesis)
			NUM, ///< ISO 8601 basic format date: YYYYMMDDHHmmSS (fields might be dropped, in the least to the most significant order)
			DIN, ///<DIN5008 format: DD.MM.YYYY HH:MM:SS.sss
			ISO_WEEK, ///< ISO 8601 week date: YYYY-Www-D (for example: 2014-W41-1)
			ISO_DATE ///< ISO 8601 date format without the time (ie YYYY-MM-DD)
		} FORMATS;

		///Keywords for selecting rounding strategy
		typedef enum RND_TYPE {
			UP, ///< rounding toward highest absolute value
			DOWN, ///< rounding toward smallest absolute value
			CLOSEST ///< rounding toward closest
		} RND;

		static const double DST_shift;
		static const double MJD_offset;
		static const double RFC868_offset;
		static const double Unix_offset;
		static const double Excel_offset;
		static const double Matlab_offset;
		static const double epsilon_sec;

		Date();
		Date(const double& julian_in, const double& in_timezone, const bool& in_dst=false);
		Date(const int& year, const int& month, const int& day, const int& hour, const int& minute, const double& in_timezone, const bool& in_dst=false);
		Date(const int& year, const int& month, const int& day, const int& hour, const int& minute, const int& second, const double& in_timezone, const bool& in_dst=false);
		Date(const int& year, const int& month, const int& day, const int& hour, const int& minute, const double& second, const double& in_timezone, const bool& in_dst=false);
		Date(const time_t&, const bool& in_dst=false);
		Date(const int& year, const double& jdn, const double& in_timezone, const bool& in_dst=false);

		void setFromSys();
		void setTimeZone(const double& in_timezone, const bool& in_dst=false);
		void setDate(const Date& in_date);
		void setDate(const double& julian_in, const double& in_timezone, const bool& in_dst=false);
		void setDate(const int& year, const int& month, const int& day, const int& hour, const int& minute, const double& in_timezone, const bool& in_dst=false);
		void setDate(const int& year, const int& month, const int& day, const int& hour, const int& minute, const int& second, const double& in_timezone, const bool& in_dst=false);
		void setDate(const int& year, const int& month, const int& day, const int& hour, const int& minute, const double& second, const double& in_timezone, const bool& in_dst=false);
		void setDate(const int& year, const unsigned int& month, const unsigned int& day, const unsigned int& hour, const unsigned int& minute, const double& in_timezone, const bool& in_dst=false);
		void setDate(const int& year, const unsigned int& month, const unsigned int& day, const unsigned int& hour, const unsigned int& minute, const unsigned int& second, const double& in_timezone, const bool& in_dst=false);
		void setDate(const int& year, const unsigned int& month, const unsigned int& day, const unsigned int& hour, const unsigned int& minute, const double& second, const double& in_timezone, const bool& in_dst=false);
		void setDate(const int& year, const double& jdn, const double& in_timezone, const bool& in_dst=false);
		void setDate(const time_t& in_time, const bool& in_dst=false);
		void setModifiedJulianDate(const double& julian_in, const double& in_timezone, const bool& in_dst=false);
		void setRFC868Date(const double& julian_in, const double& in_timezone, const bool& in_dst=false);
		void setUnixDate(const time_t& in_time, const bool& in_dst=false);
		void setExcelDate(const double excel_in, const double& in_timezone, const bool& in_dst=false);
		void setMatlabDate(const double matlab_in, const double& in_timezone, const bool& in_dst=false);
		void setUndef(const bool& flag);

		bool isUndef() const {return undef;}
		double getTimeZone() const;
		bool getDST() const;
		double getJulian(const bool& gmt=false) const;
		double getModifiedJulianDate(const bool& gmt=false) const;
		double getTruncatedJulianDate(const bool& gmt=false) const;
		double getRFC868Date(const bool& gmt=false) const;
		time_t getUnixDate() const;
		double getExcelDate(const bool& gmt=false) const;
		double getMatlabDate(const bool& gmt=false) const;

		void getDate(double& julian_out, const bool& gmt=false) const;
		void getDate(int& year, int& month, int& day, const bool& gmt=false) const;
		void getDate(int& year, int& month, int& day, int& hour, const bool& gmt=false) const;
		void getDate(int& year, int& month, int& day, int& hour, int& minute, const bool& gmt=false) const;
		void getDate(int& year, int& month, int& day, int& hour, int& minute, double& second, const bool& gmt=false) const;
		void getDate(int& year, int& month, int& day, int& hour, int& minute, int& second, const bool& gmt=false) const;
		void getTime(int& hour_out, int& minute_out, const bool& gmt=false) const;
		void getTime(int& hour_out, int& minute_out, double& second_out, const bool& gmt=false) const;
		int getYear(const bool& gmt=false) const;

		unsigned short getDayOfWeek(const bool& gmt=false) const;
		unsigned short getISOWeekNr(const bool& gmt=false) const;
		unsigned short getISOWeekNr(int &ISO_year, const bool& gmt=false) const;
		int getJulianDayNumber(const bool& gmt=false) const;
		bool isLeapYear() const;

		static unsigned int mod(const double& julian, const unsigned int& seconds);
		static unsigned int mod(const Date& indate, const unsigned int& seconds);
		static double rnd(const double& julian, const double& precision, const RND& type=CLOSEST);
		void rnd(const double& precision, const RND& type=CLOSEST);
		static const Date rnd(const Date& indate, const double& precision, const RND& type=CLOSEST);
		static double parseTimeZone(const std::string& timezone_iso);

		static std::string printFractionalDay(const double& fractional);
		const std::string toString(const FORMATS& type, const bool& gmt=false) const;
		const std::string toString() const;
		friend std::ostream& operator<<(std::ostream& os, const Date& date);
		friend std::istream& operator>>(std::istream& is, Date& date);

		//Operator Prototypes
		bool operator==(const Date&) const;
		bool operator!=(const Date&) const;
		bool operator<(const Date&) const;
		bool operator<=(const Date&) const;
		bool operator>(const Date&) const;
		bool operator>=(const Date&) const;

		///Intervals arithmetic
		///Can be used to add an interval to an existing Date object.
		///For example, you can add 1 day to a Date object `dt` simply with `dt += 1.0`.
		///Please use the Duration type instead of Date for such calculations!
		Date& operator+=(const Date&);
		Date& operator-=(const Date&);
		Date& operator+=(const double&);
		Date& operator-=(const double&);
		Date& operator*=(const double&);
		Date& operator/=(const double&);

		const Date operator+(const Date&) const;
		const Date operator-(const Date&) const;
		const Date operator+(const double&) const;
		const Date operator-(const double&) const;
		const Date operator*(const double&) const;
		const Date operator/(const double&) const;
		
		static const double epsilon;

	protected:
		double localToGMT(const double& in_julian) const;
		double GMTToLocal(const double& in_gmt_julian) const;
		static double calculateJulianDate(const int& in_year, const int& in_month, const int& in_day, const int& in_hour, const int& in_minute, const double& i_second);
		static void calculateDate(const double& i_julian, int& out_year, int& out_month, int& out_day);
		static void calculateTime(const double& i_julian, int& o_hour, int& o_minute, double& o_second);
		static void calculateValues(const double& i_julian, int& out_year, int& out_month, int& out_day, int& out_hour, int& out_minute, double& o_second);
		static long getJulianDayNumber(const int&, const int&, const int&);
		static bool isLeapYear(const int&);
		static void plausibilityCheck(const int& in_year, const int& in_month, const int& in_day, const int& in_hour, const int& in_minute, const double& in_second);
		static bool initStaticData();///<initialize the static map TZAbbrev

		static std::map< std::string, double> TZAbbrev;
		static const int daysLeapYear[];
		static const int daysNonLeapYear[];
		static const bool __init;
		double timezone;
		double gmt_julian;
		bool dst;
		bool undef;
};

typedef Date Duration; //so that later, we can implement a true Interval/Duration class

} //end namespace

#endif
