/***********************************************************************************/
/*                   Copyright GridGroup, EIA-FR 2010                              */
/*  Copyright 2010-2013 WSL Institute for Snow and Avalanche Research  SLF-DAVOS   */
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

#include <stdio.h>
#if defined _WIN32 || defined __MINGW32__
	#include <windows.h>
	#undef max
	#undef min
#else
	#include <sys/time.h>
#endif

#include "Timer.h"

namespace mio {

/**
* @brief Default constructor.
* Initialize internal variables. It does NOT start timing.
*/
Timer::Timer()
     : start_point(0.L), elapsed(0.), isRunning(false) {}

/**
* @brief Start the timer.
*/
void Timer::start() {
	if (!isRunning) {
		isRunning = true;
		start_point = getCurrentTime();
	}
}

/**
* @brief Reset and start the timer.
*/
void Timer::restart() {
	reset();
	isRunning = true;
}

/**
* @brief Stop the timer.
* It can be restarted afterward, adding time to what was already timed.
*/
void Timer::stop() {
	if (isRunning) {
		elapsed += static_cast<double>( getCurrentTime()-start_point );
		isRunning = false;
	}
}

/**
* @brief Reset the timer to zero.
*/
void Timer::reset() {
	elapsed = 0.;
	start_point = getCurrentTime();
}

/**
* @brief Get total elapsed time.
* It returns the sum of all the elapsed time between all the start/stop sessions since
* the timer was created or the last call to reset. Time is in seconds with microsecond resolution.
*/
double Timer::getElapsed() const {
	if (isRunning) {
		return elapsed + static_cast<double>(getCurrentTime()-start_point);
	}
	return elapsed;
}

/**
* @brief Get the current time
* @return the current time as seconds since Unix Epoch
*/
#if defined _WIN32 || defined __MINGW32__
long double Timer::getCurrentTime() {
	SYSTEMTIME systemTime;
	GetSystemTime( &systemTime );

	FILETIME fileTime;
	SystemTimeToFileTime( &systemTime, &fileTime );

	ULARGE_INTEGER uli;
	uli.LowPart = fileTime.dwLowDateTime;
	uli.HighPart = fileTime.dwHighDateTime;

	const ULONGLONG units_convert = 10000*1000ULL; //it gives the time since 1 January 1601 (UTC) in units of 100ns
	const ULONGLONG offset_to_epoch = 11644473600ULL; //offset in seconds to Unix epoch, 134774 days * 24*3600
	return static_cast<long double>(uli.QuadPart - offset_to_epoch*units_convert) / units_convert;
}
#else
long double Timer::getCurrentTime() {
	timeval tp;
	gettimeofday(&tp,NULL);
	const long double t = static_cast<long double>(tp.tv_sec) + static_cast<long double>(tp.tv_usec)*1.e-6L;
	return t;
}


#endif

#if !defined _WIN32 && !defined __MINGW32__
const int UsageTimer::who = RUSAGE_SELF;

UsageTimer::UsageTimer() : start_usage(), current_usage(), user_time(0.), sys_time(0.), elapsed(0.), is_running(false) {}

void UsageTimer::start()
{
	if (!is_running) {
		is_running = true;
		getrusage(UsageTimer::who, &start_usage);
	}
}

void UsageTimer::stop()
{
	if (is_running) {
		getElapsedTimes();
		is_running = false;
	}
}

void UsageTimer::restart()
{
	is_running = false;
	reset();
	start();
}

void UsageTimer::reset() {
	user_time = sys_time = elapsed = 0.;
}

double UsageTimer::getElapsed()
{
	if (is_running) {
		getElapsedTimes();
	}

	return elapsed;
}

double UsageTimer::getElapsedUserTime()
{
	if (is_running) {
		getElapsedTimes();
	}

	return user_time;
}

double UsageTimer::getElapsedSystemTime()
{
	if (is_running) {
		getElapsedTimes();
	}

	return sys_time;
}

void UsageTimer::getElapsedTimes()
{
	getrusage(UsageTimer::who, &current_usage);

	//calculate start point
	const long double start_user_time = static_cast<long double>(start_usage.ru_utime.tv_sec) + static_cast<long double>(start_usage.ru_utime.tv_usec)*1e-6L;
	const long double start_sys_time  = static_cast<long double>(start_usage.ru_stime.tv_sec) + static_cast<long double>(start_usage.ru_stime.tv_usec)*1e-6L;

	//calculate end point
	const long double end_user_time = static_cast<long double>(current_usage.ru_utime.tv_sec) + static_cast<long double>(current_usage.ru_utime.tv_usec)*1e-6L;
	const long double end_sys_time  = static_cast<long double>(current_usage.ru_stime.tv_sec) + static_cast<long double>(current_usage.ru_stime.tv_usec)*1e-6L;

	//calculate different elapsed times
	user_time = static_cast<double>( end_user_time - start_user_time );
	sys_time = static_cast<double>( end_sys_time - start_sys_time );
	elapsed = static_cast<double>( sys_time + user_time );
}
#endif

} //namespace

