/*
 *  SNOWPACK stand-alone
 *
 *  Copyright WSL Institute for Snow and Avalanche Research SLF, DAVOS, SWITZERLAND
*/
/*  This file is part of Snowpack.
    Snowpack is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Snowpack is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Snowpack.  If not, see <http://www.gnu.org/licenses/>.
*/
/**
 * @file Utils.h
 * @version 10.02
 * @date -
 * @brief This header file contains the definition of the structures required to handle I/O
 */

#ifndef UTILS_H
#define UTILS_H

#include <snowpack/DataClasses.h>

#include <cstdarg> // needed for va_list
#include <string>
#include <cstring>
#include <vector>

#ifdef _MSC_VER
//Microsoft still does NOT support C99...
//This replacement is not fully compatible, so
//switch to a real, standard compliant compiler
	#define snprintf _snprintf
#endif

/**
* @brief Return the library version
* @return library version string
*/
namespace snowpack {
std::string getLibVersion();
}

#ifdef GNU	//in this case, GCC can check the format arguments for types, number, ...
void prn_msg(const char *theFile, const int theLine, const char *msg_type, const mio::Date& date_in, const char *format, ...)
__attribute__ ((format (printf, 5, 6)));
#else
void prn_msg(const char *theFile, const int theLine, const char *msg_type, const mio::Date& date_in, const char *format, ...);
#endif

bool booleanTime(const double& JulianDate, double days_between,
                 const double& start, const double& calculation_step_length);

void deleteOldOutputFiles(const std::string& outdir, const std::string& experiment,
                          const std::string& stationID, const unsigned int& nSlopes,
                          const std::vector<std::string>& vecExtensions);

void averageFluxTimeSeries(const size_t& n_steps, const bool& useCanopyModel,
                           SurfaceFluxes& Sdata, SnowStation& Xdata);

void typeToCode(int *F1, int *F2, int *F3, int type);

double unitConversion(const double val, char* unitIn, char* unitOut);

bool massBalanceCheck(const SnowStation& Xdata, const SurfaceFluxes& Sdata, double& tot_mass_in);

size_t findUpperNode(const double& z, const std::vector<NodeData>& Ndata, const size_t& nN);

double forcedErosion(const double hs, SnowStation& Xdata);

void deflateInflate(const CurrentMeteo& Mdata, SnowStation& Xdata, double& dhs_corr, double& mass_corr);

double logisticFunction(const double input, const double threshold, const double width);

void cumulate(double& accu, const double value);

void checkOldOutputFiles(const mio::Date& i_date, const std::string& stationID);

double getPerpSensorPosition(const bool& useSoilLayers, const double& z_vert, const double& hs_ref, const double& Ground, const double& SlopeAngle);

/**
 * @name MACRO definitions (to be replaced by proper functions some day)
 * TODO Move this definitions to Util.h, as functions to Util.c
 */
//@{
/// @name Conversion macros
//@{
/// @brief For lengths
#define M_TO_CM( l ) ( (l) * 100. )  // meter to centimeters
#define M_TO_MM( l ) ( (l) * 1000. ) // meter to millimeters
#define CM_TO_M( l ) ( (l) / 100. )  // centimeter to meter
#define MM_TO_M( l ) ( (l) / 1000. ) // millimeter to meter
#define MM_TO_CM( l ) ( (l) / 10. )  // millimeter to centimeter
/// @brief For time
#define D_TO_H( t ) ( (t) * 24. )    // day to hours
#define D_TO_M( t ) ( (t) * 1440. )  // day to minutes
#define D_TO_S( t ) ( (t) * 86400. ) // day to seconds
#define H_TO_D( t ) ( (t) / 24.0 )   // hour to day
#define H_TO_M( t ) ( (t) * 60. )    // hour to minutes
#define H_TO_S( t ) ( (t) * 3600. )  // hour to seconds
#define M_TO_D( t ) ( (t) / 1440. )  // minute to day
#define M_TO_H( t ) ( (t) / 60. )    // minute to hour
#define M_TO_S( t ) ( (t) * 60. )    // minute to seconds
#define S_TO_D( t ) ( (t) / 86400. ) // second to day
#define S_TO_H( t ) ( (t) / 3600. )  // second to hour
#define S_TO_M( t ) ( (t) / 60. )    // second to minute
//@}
//@}

#endif //End of Utils.h
