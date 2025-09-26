/***********************************************************************************/
/*  Copyright 2018 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef IDWSLOPE_ALGORITHM_H
#define IDWSLOPE_ALGORITHM_H

#include <meteoio/spatialInterpolations/InterpolationAlgorithms.h>

namespace mio {

/**
 * @class IDWSlopesAlgorithm
 * @brief Inverse Distance Weighting interpolation with splitted N/E/S/W slopes and flats.
 * @details
 * This is designed around the data produced by the Snowpack model: each station produces virtual slopes
 * (38°) for each of the main 4 aspects. This algorithm interpolates each N/E/S/W group of stations separately
 * (IDW with elevation lapse rate) and then recombines them with weights that depend on the cell's slope and aspect.
 * It takes the following arguments:
 *  - SCALE: this is a scaling parameter to smooth the IDW distribution. In effect, this is added to the distance in order
 * to move into the tail of the 1/d distribution (default: 1000m);
 *  - ALPHA: this is an exponent to the 1/d distribution (default: 1);
 * @ingroup spatialization
 */
class IDWSlopesAlgorithm : public InterpolationAlgorithm {
	public:
		IDWSlopesAlgorithm(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& i_algo, const std::string& i_param, TimeSeriesManager& i_tsm);
		virtual double getQualityRating(const Date& i_date);
		virtual void calculate(const DEMObject& dem, Grid2DObject& grid);
	private:
		enum Slopes {firstSlope=0, FLAT=firstSlope, NORTH, EAST, SOUTH, WEST, lastSlope=WEST};
		
		Grid2DObject computeAspect(const DEMObject& dem, const Slopes& curr_slope);
		
		std::vector< std::vector<double> > vecDataCache;
		std::vector< std::vector<StationData> > vecMetaCache;
		Trend trend;
		double scale, alpha; ///<a scale parameter to smooth out the 1/dist and an exponent
		static const double min_slope, max_slope;
		static const size_t nrSlopes;
};

} //end namespace mio

#endif
