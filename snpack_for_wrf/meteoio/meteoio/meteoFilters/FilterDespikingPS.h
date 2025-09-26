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
#ifndef FilterDespikingPS_H
#define FilterDespikingPS_H

#include <meteoio/meteoFilters/ProcessingBlock.h>
#include <meteoio/meteoStats/libfit1D.h>

#include <vector>
#include <string>

namespace mio {

/**
 * @class FilterDespikingPS
 * @brief Despiking filter using phase space.
 * @details
 * This filter iteratively finds and replaces spikes (outliers) in a data sequence until no more spikes are found. The filter was developed to despike Acoustic
 * Doppler Velocimeter (ADV) data, but might also be useful to remove outliers from other signals.
 *
 * \image html DespikingFilter_typicalFilterResult.png "Example of filter performance for one hour of ADV-data. Outliers are successfully removed by the filter."
 *
 * Two versions of this filter are implemented:
 *   - The "Goring"-version is implemented according to following paper:
 *      Goring, D.G. and Nikora, V.I. (2002). <i>"Despiking Acoustic Doppler Velocimeter Data"</i> J. Hydraul. Eng., 128(1): 117-126
 *   - And the "Mori"-version is implemented according to this paper:
 *      Mori, N., Suzuki, T. and Kakuno, S. (2007). <i>"Noise of Acoustic Doppler Velocimeter Data in Bubbly Flows"</i> J. Eng. Mech., 133(1):122-125
 *
 * Method:
 * - Detection of the spikes:
 *  - calculate first and second derivatives of the whole signal
 *  - the data points plotted in phase space are enclosed by an ellipsoid defined
 *     by the standard deviations and the universal threshold (=sqrt(2*ln(number of data points))) (see figure below)
 *  - points outside this ellipsoid are designated as spikes (the Goring-implementation uses three 2D-projections of the ellipsoid to identify
 *                                                            the outliers, while the Mori-implementation finds the outliers in 3D)
 * - Replacement of the spikes:
 *  - find a fit for data points around the spike
 *  - replace the spike with a fitted value
 *
 * It takes the following arguments
 *  - METHOD: decide which implementation to use: "Mori" or "Goring". The differences are small between both implementations.
 *    According to Mori the "Mori"-version performs slightly better. However, we suggest to use the Goring-method, because it is better tested.
 *  - SENSITIVITY: control the sensitivity of the filter. The universal threshold is divided by
 *      this value. Thus a sensitivity of 1 is the default value. The larger the sensitivity the smaller the threshold (=the smaller
 *      the ellipsoid) and thus the more spikes will be detected.
 *  - INTERPOL_DEG: degree of the fit for the replacement. 1 is linear, 2 is quadratic, ..., 0 means "off"
 *    (removed spikes will be nodata and can be resampled later via [Interpolations1D]). This is useful in rare cases where the cubic fit happens
 *    to produce an unwanted peak by itself.
 *    Default value is 3 (cubic fit) as proposed in the paper by Goring.
 *  - INTERPOL_PTS: number of points used for the fitting (replacement) of the spikes. Default is 24 (just as proposed in the
 *    paper by Goring).
 *
 * There are also some hard-coded parameters:
 *    - the maximum number of iterations for the spike detection. This is set to 50.
 *    - When fitting, extrapolation is set to false.
 *
 * \image html DespikingFilter_phaceSpacePlots.png "2D projections of the phase space plots. Points outside the ellipses are spikes."
 *
 * @ingroup processing
 * @author Thiemo Theile
 * @date   2018-08-15
 * @code
 * VW::filter1	= despiking
 * VW::arg1::sensitivity = 1
 * VW::arg1::method = GORING
 * @endcode
 * @code
 * HS::filter1            = despiking
 * HS::arg1::sensitivity  = 1.4
 * HS::arg1::method       = GORING
 * HS::arg1::interpol_deg = 0 ;remove spikes
 * @endcode
 */


class FilterDespikingPS : public ProcessingBlock {
	public:
		FilterDespikingPS(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name);

		virtual void process(const unsigned int& param, const std::vector<MeteoData>& ivec,
		                     std::vector<MeteoData>& ovec);

	private:
		typedef enum IMPLEMENTATION_TYPE {
			GORING,
			MORI
		} implementation_type;

		void parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs);

		std::vector<int> findSpikesGoring(const std::vector<double>& timeVec, const std::vector<double>& ivec, unsigned int& nNewSpikes);
		std::vector<int> findSpikesMori(const std::vector<double>& timeVec, const std::vector<double>& ivec, unsigned int& nNewSpikes);
		void replaceSpikes(const std::vector<double>& timeVec, std::vector<double>& uVec, std::vector<int>& spikesVec);

		std::vector<double> calculateDerivatives(const std::vector<double>& ivec, const std::vector<double>& timeVec);
		double calculateCrossCorrelation(const std::vector<double>& aVec,const std::vector<double>& bVec);
		unsigned int nNodataElements(const std::vector<double>& iVec);
		void findPointsOutsideEllipse(const std::vector<double>& xVec,const std::vector<double>& yVec,
                                      const double a,const double b,const double theta, std::vector<int>& outsideVec);
		void findPointsOutsideEllipsoid(const std::vector<double>& xVec,const std::vector<double>& yVec, const std::vector<double>& zVec,
                                                const double a,const double b,const double c, std::vector<int>& outsideVec);
		void getWindowForInterpolation(const size_t index,const std::vector<double>& timeVec, const std::vector<double>& uVec,
                                       const std::vector<int>& spikesVec, std::vector<double>& xVec, std::vector<double>& yVec);
		bool checkIfWindowForInterpolationIsSufficient(const std::vector<double>& xVec,const double time,const unsigned int minPoints,
                                                       const bool avoidExtrapolation);
		//helper functions:
		void solve2X2LinearEquations(const double* a, const double* b, const double* c, double* x);
		const std::vector<double> helperGetDoubleVectorOutOfMeteoDataVector(const std::vector<MeteoData>& ivec, const unsigned int& param);
		const std::vector<double> helperGetTimeVectorOutOfMeteoDataVector(const std::vector<MeteoData>& ivec);

		double sensitivityParam; //this parameter controls the sensitivity of the filter. standard value: 1
		implementation_type methodParam; //this parameter controls which implementation of the filter is used: according to Mori or Goring
		int nIterations;   //this counts the iterations
		int maxIterations; //this is a hard-coded parameter to stop the iteration

		unsigned int degreeOfInterpolation; //1: linear fit, 2: quadratic fit, 3: cubic fit
		unsigned int windowWidth; //wished width of the window for interpolation
};

} //end namespace

#endif
