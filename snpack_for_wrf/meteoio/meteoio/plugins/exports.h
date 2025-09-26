/***********************************************************************************/
/*  Copyright 2011 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#if defined(_MSC_VER)
	#if defined(OSG_LIBRARY_STATIC)
		#define METEOIO_EXPORT
	#elif defined(COMPILE_PLUGIN)
		#define METEOIO_EXPORT   __declspec(dllexport)
	#else
		#define METEOIO_EXPORT   __declspec(dllimport)
	#endif
#else
	#define METEOIO_EXPORT
#endif
