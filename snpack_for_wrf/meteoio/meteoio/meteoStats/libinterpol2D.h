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
/**
 * @file libinterpol2D.h
 * This is the two 2D meteo interpolation statistical library.
 */
#ifndef INTERPOL2D_H
#define INTERPOL2D_H

#include <meteoio/dataClasses/StationData.h>
#include <meteoio/dataClasses/DEMObject.h>
#include <meteoio/meteoStats/libfit1D.h>
#include <meteoio/meteoStats/libinterpol1D.h>
#include <vector>

namespace mio {

/**
 * @class Interpol2D
 * @brief A class to perform 2D spatial interpolations.
 * Each parameter to be interpolated declares which interpolation method to use.
 * Then the class computes the interpolation for each 2D grid point,
 * combining the inputs provided by the available data sources.
 *
 * @ingroup stats
 * @author Mathias Bavay
 */

class Interpol2D {
	public:
		///Keywords for selecting the regression algorithm to use
		typedef enum REG_TYPES {
			R_CST, ///< no elevation dependence (ie: constant)
			R_LIN ///< linear elevation dependence
		} reg_types;

		static void stdPressure(const DEMObject& dem, Grid2DObject& grid);
		static void constant(const double& value, const DEMObject& dem, Grid2DObject& grid);
		static void IDW(const std::vector<double>& vecData_in, const std::vector<StationData>& vecStations_in,
                                const DEMObject& dem, Grid2DObject& grid, const double& scale, const double& alpha=1.);
		static void LocalLapseIDW(const std::vector<double>& vecData_in,
		                          const std::vector<StationData>& vecStations_in,
		                          const DEMObject& dem, const size_t& nrOfNeighbors,
		                          Grid2DObject& grid, const double& scale, const double& alpha=1.);
		static void ListonWind(const DEMObject& i_dem, Grid2DObject& VW, Grid2DObject& DW);
		static void CurvatureCorrection(DEMObject& dem, const Grid2DObject& ta, Grid2DObject& grid);
		static void SteepSlopeRedistribution(const DEMObject& dem, const Grid2DObject& ta, Grid2DObject& grid);
		static void PrecipSnow(const DEMObject& dem, const Grid2DObject& ta, Grid2DObject& grid);
		static void ODKriging(const std::vector<double>& vecData,
		                      const std::vector<StationData>& vecStations,
		                      const DEMObject& dem, const Fit1D& variogram, Grid2DObject& grid);

		static void RyanWind(const DEMObject& dem, Grid2DObject& VW, Grid2DObject& DW);
		static void Winstral(const DEMObject& dem, const Grid2DObject& TA, const double& dmax, const double& in_bearing, Grid2DObject& grid);
		static void Winstral(const DEMObject& dem, const Grid2DObject& TA, const Grid2DObject& DW, const Grid2DObject& VW, const double& dmax, Grid2DObject& grid);

		static bool allZeroes(const std::vector<double>& vecData);

		static double getTanMaxSlope(const Grid2DObject& dem, const double& dmin, const double& dmax, const double& bearing, const size_t& ii, const size_t& jj);
	private:
		//generic functions
		static double InvHorizontalDistance(const double& X1, const double& Y1, const double& X2, const double& Y2);
		static double HorizontalDistance(const double& X1, const double& Y1, const double& X2, const double& Y2);
		static double HorizontalDistance(const DEMObject& dem, const int& i, const int& j,
		                                 const double& X2, const double& Y2);
		static void getNeighbors(const double& x, const double& y,
		                         const std::vector<StationData>& vecStations,
		                         std::vector< std::pair<double, size_t> >& list);
		static void buildPositionsVectors(const std::vector<StationData>& vecStations,
		                                  std::vector<double>& vecEastings, std::vector<double>& vecNorthings);

		//core methods
		static double IDWCore(const double& x, const double& y,
		                      const std::vector<double>& vecData_in,
		                      const std::vector<double>& vecEastings, const std::vector<double>& vecNorthings, const double& scale, const double& alpha=1.);
		static double IDWCore(const std::vector<double>& vecData_in, const std::vector<double>& vecDistance_sq, const double& scale, const double& alpha=1.);
		static double LLIDW_pixel(const size_t& i, const size_t& j,
		                          const std::vector<double>& vecData_in,
		                          const std::vector<StationData>& vecStations_in,
		                          const DEMObject& dem, const size_t& nrOfNeighbors, const double& scale, const double& alpha=1.);

		static void steepestDescentDisplacement(const DEMObject& dem, const Grid2DObject& grid, const size_t& ii, const size_t& jj, char &d_i_dest, char &d_j_dest);
		static double depositAroundCell(const DEMObject& dem, const size_t& ii, const size_t& jj, const double& precip, Grid2DObject &grid);
		
		static void WinstralSX(const DEMObject& dem, const double& dmax, const double& in_bearing, Grid2DObject& grid);
		static void WinstralSX(const DEMObject& dem, const double& dmax, const Grid2DObject& DW, Grid2DObject& grid);

		//weighting methods
		static double weightInvDist(const double& d2);
		static double weightInvDistSqrt(const double& d2);
		static double weightInvDist2(const double& d2);
		double weightInvDistN(const double& d2);
		double dist_pow; //power for the weighting method weightInvDistN
};

} //end namespace

#endif
