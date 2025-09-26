/***********************************************************************************/
/*                   Copyright GridGroup, EIA-FR 2010                              */
/*  Copyright 2010 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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

#ifndef TIMER_H
#define TIMER_H

#if !defined _WIN32 && !defined __MINGW32__
	#include <sys/resource.h>
#endif

#include <meteoio/IOExceptions.h>

namespace mio {

/**
 * @class Timer
 * @author Tuan Anh Nguyen (original implementation in popc), Mathias Bavay (port and rewrite for Alpine3D and MeteoIO)
 * @brief Time code execution with at least 1 us resolution.
 * The time resolution can be stored up to .1 ns resolution, but is measured to the following accuracy:
 *    - 1 us on Posix systems (Linux, osX, BSD);
 *    - 1 ns on Windows.
 */
class Timer {
public:
	Timer();
	void start();
	void restart();
	void stop();
	void reset();
	double getElapsed() const;
	static long double getCurrentTime();

protected:
	long double start_point;
	double elapsed;
	bool isRunning;
};

#if !defined _WIN32 && !defined __MINGW32__
/**
 * @class UsageTimer
 * @author Thomas Egger
 * @brief Process usage timer for Posix
 * This is based on \em getrusage and thus returns detailed timing information about how the time was spend (userland, system time).
 */
class UsageTimer {
 public:
	UsageTimer();
	void start();
	void restart();
	void stop();
	void reset();

	double getElapsed();
	double getElapsedUserTime();
	double getElapsedSystemTime();

 protected:
	void getElapsedTimes();

	static const int who;

	struct rusage start_usage, current_usage;
	double user_time, sys_time, elapsed;
	bool is_running;
};
#endif

} //end namespace mio
#endif
