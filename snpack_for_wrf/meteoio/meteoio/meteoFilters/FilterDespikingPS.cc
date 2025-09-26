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
#include <meteoio/meteoFilters/FilterDespikingPS.h>
#include <meteoio/meteoStats/libinterpol1D.h>
#include <meteoio/MathOptim.h>

#include <iostream>
#include <fstream>

using namespace std;

namespace mio {

FilterDespikingPS::FilterDespikingPS(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name)
          : ProcessingBlock(vecArgs, name), sensitivityParam(1), methodParam(GORING), nIterations(0), maxIterations(50),
			degreeOfInterpolation(3), windowWidth(24)
{
	parse_args(vecArgs);
	properties.stage = ProcessingProperties::first;
}

/**
 * @brief This function is the main function to do the filtering:
            1. subtract the mean from the signal
            2. iteratively find and replace spikes in the signal
            3. add the mean back to the signal
 * @param param which value from the MeteoData should be processed (for example temperature)
 * @param ivec the input data (MeteoData-vector)
 * @param ovec the filtered data (MeteoData-vector)
 */
void FilterDespikingPS::process(const unsigned int& param, const std::vector<MeteoData>& ivec,
                                                   std::vector<MeteoData>& ovec)
{
	ovec = ivec;
	//the time vector (a vector of double-values):
	std::vector<double> timeVec( helperGetTimeVectorOutOfMeteoDataVector(ivec) );
	//the signal (a vector of double-values) we will despike:
	std::vector<double> doubleVec( helperGetDoubleVectorOutOfMeteoDataVector(ivec,param) );

	bool keepLookingForSpikes = true;
	unsigned int nNewSpikes=0;
	nIterations=0;
	while(keepLookingForSpikes == true){
		//1. subtract mean from signal
		const double mean = Interpol1D::arithmeticMean(doubleVec);
		for (size_t ii=0; ii<doubleVec.size(); ii++){
			if (doubleVec[ii] != IOUtils::nodata)
				doubleVec[ii] -= mean;
		}
		//2. find spikes
		std::vector<int> spikesVec;
		if (methodParam==MORI){
			spikesVec = findSpikesMori(timeVec,doubleVec,nNewSpikes);
		} else {
			spikesVec = findSpikesGoring(timeVec,doubleVec,nNewSpikes);
		}

		//3. replace spikes
		const double stdDev0 = Interpol1D::std_dev(doubleVec);
		replaceSpikes(timeVec, doubleVec, spikesVec);
		const double stdDev1 = Interpol1D::std_dev(doubleVec);
		nIterations++;

		//there are three stopping-criteria for the iterations:
		//1. we reached the maximum number of iterations
		//2. no new spike was detected
		//3. the standard deviation of the signal is not decreasing
		if (nIterations >= maxIterations) keepLookingForSpikes=false;
		if (nNewSpikes==0) keepLookingForSpikes=false;
		if (stdDev0 <= stdDev1) keepLookingForSpikes=false;
		
		//4. add mean back to signal again
		for (size_t ii=0; ii<doubleVec.size(); ii++){
			if (doubleVec[ii] != IOUtils::nodata)
				doubleVec[ii] += mean;
		}
	}

	//5. set output
	for (size_t ii=0; ii<ovec.size(); ii++){
		double& value = ovec[ii](param);
		value = doubleVec[ii];
	}
}

/**
 * @brief This function parses the argument-vector.
 * @param vecArgs
 */
void FilterDespikingPS::parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
{
	const std::string where( "Filters::"+block_name );
	//if the filter is based on WindowedFilter, its constructor will automatically read the window parameters as well as the "soft" argument

	//to perform syntax checks (see after the "for" loop)
	bool hasSensitivityParam=false;
	bool hasMethodParam=false;

	//parse the arguments (the keys are all upper case)
	for (size_t ii=0; ii<vecArgs.size(); ii++) {
		if (vecArgs[ii].first=="SENSITIVITY") {
			IOUtils::parseArg(vecArgs[ii], where, sensitivityParam);
			hasSensitivityParam=true;
		} else if (vecArgs[ii].first=="METHOD") {
			const std::string type_str( vecArgs[ii].second );
			if (type_str=="MORI") methodParam = MORI;
			else if (type_str=="GORING") methodParam = GORING;
			else throw InvalidArgumentException("Invalid type \""+vecArgs[ii].second+"\" for \""+where+"\". Please use \"MORI\" or \"GORING\".", AT);
			hasMethodParam=true;
		} else if (vecArgs[ii].first=="INTERPOL_DEG") {
				IOUtils::parseArg(vecArgs[ii], where, degreeOfInterpolation);
		} else if (vecArgs[ii].first=="INTERPOL_PTS") {
				IOUtils::parseArg(vecArgs[ii], where, windowWidth);
		}
	}

	//second part of the syntax check
	if (!hasSensitivityParam) throw InvalidArgumentException("Please provide a sensitivity-argument "+where, AT);
	if (!hasMethodParam) throw InvalidArgumentException("Please provide a method-argument "+where, AT);
}

/**
 * @brief This function calculates the derivative of a signal: d(ivec)(ii)=(ivec(i2)-ivec(i1))/(timeVec(i2)-timeVec(i1)) with i2=ii+1 and i1=ii-1
 *        Care has to be taken about the first and last value of the signal and about nodata values:
 *           - the derivative of a nodata-value is also a nodata-value. (even though there could be a valid point before and after...)
 *           - if ivec(i2) or ivec(i1) is a nodata-value, we increment i2 and decrement i1 until both are not nodata.
 * @param ivec the input-vector
 * @param timeVec time-vector (same length as iVec)
 * @param return a double-vector containing the derivatives of the input-vector
 */
std::vector<double> FilterDespikingPS::calculateDerivatives(const std::vector<double>& ivec, const std::vector<double>& timeVec)
{
	std::vector<double> ovec;
	ovec.reserve( ivec.size() );

	for (size_t ii=0; ii<ivec.size(); ii++) {
		if (ivec[ii]==IOUtils::nodata){
			ovec.push_back(IOUtils::nodata);
		} else {
			int i1 = static_cast<int>(ii);
			int i2 = static_cast<int>(ii);
			bool stop=false;
			const int maxSteps = 100;
			while (stop==false){
				i1 = i1-1;
				i2 = i2+1;
				if (i1<0){
					stop=true;
					i1=static_cast<int>(ii);
				}
				if (i2>=static_cast<int>(ivec.size())){
					stop=true;
					i2=static_cast<int>(ii);
				}
				if (ivec[i1]!=IOUtils::nodata && ivec[i2]!=IOUtils::nodata){
					stop=true;
				}
				if (i1+maxSteps < static_cast<int>(ii) || i2 > static_cast<int>(ii)+maxSteps){
					stop=true;
				}
			}
			const double dt = timeVec[i2]-timeVec[i1];
			if (dt != 0 && ivec[i1] != IOUtils::nodata && ivec[i2] != IOUtils::nodata){
				const double derivative = (ivec[i2]-ivec[i1]) / dt;
				ovec.push_back(derivative);
			} else {
				ovec.push_back(IOUtils::nodata);
			}
		}
	}
	return ovec;
}

/**
 * @brief This function calculates the cross correlation of two vectors and returns the cross correlation as a double value.
 *         The two vectors have to be of equal length. The cross correlation is defined as:
 *               cross-correlation = sum over i( a(i)*b(i) ) / sum over i( b(i)^2 )
 * @param aVec input double-vector
 * @param bvec input double-vector
 * @param return the calculated cross correlation
 */
double FilterDespikingPS::calculateCrossCorrelation(const std::vector<double>& aVec,const std::vector<double>& bVec)
{
	if (aVec.size() != bVec.size())
		return IOUtils::nodata;

	double ab=0;
	double bb=0;
	for (size_t ii=0; ii<aVec.size(); ii++) {
		if (aVec[ii]!=IOUtils::nodata && bVec[ii]!=IOUtils::nodata){
			ab = ab + (aVec[ii]*bVec[ii]);
		}
		if (bVec[ii] != IOUtils::nodata){
			bb = bb + (bVec[ii]*bVec[ii]);
		}
	}

	if (bb==0) {
		return IOUtils::nodata;
	} else {
		return (ab/bb);
	}
}

/**
 * @brief This function returns how many nodata-values are in a vector.
 * @param iVec input vector
 * @param return number of nodata-elements
 */
unsigned int FilterDespikingPS::nNodataElements(const std::vector<double>& iVec)
{
	unsigned int nNodata=0;
	for (size_t ii=0; ii<iVec.size(); ii++) {
		if (iVec[ii]==IOUtils::nodata)
			nNodata++;
	}
	return nNodata;
}

/**
 * @brief This function finds points (described by xVec and yVec) which lie outside of the ellipse (described by a,b and theta)
 *        and sets outsideVec to 1 (if a point lies outside of the ellipse).
 *           xVec, yVec and outsideVec all have the same size.
 *           The condition for a point to be outside the ellipse is:
 *               (x*cos(theta)+y*sin(theta))^2+(x*sin(theta)-y*cos(theta))^2 > 1
 * @param xVec input x-coordinates
 * @param yVec input y-coordinates
 * @param a input. the major axis of the ellipse
 * @param b input. the minor axis of the ellipse
 * @param theta input. the rotation angle of the principal axis of the ellipse
 * @param outsideVec output: outsideVector is set to 1 if the point is outside of the ellipse
 */
void FilterDespikingPS::findPointsOutsideEllipse(const std::vector<double>& xVec,const std::vector<double>& yVec,
                                                const double a,const double b,const double theta, std::vector<int>& outsideVec)
{
	if(xVec.size() != yVec.size()) return;

	for (size_t ii=0; ii<xVec.size(); ii++) {
		const double x = xVec[ii];
		const double y = yVec[ii];
		if (x!=IOUtils::nodata && y!=IOUtils::nodata){
			const double helper = Optim::pow2(x*cos(theta)+y*sin(theta))/(a*a) + Optim::pow2(x*sin(theta)-y*cos(theta))/(b*b);
			if (helper > 1)
				outsideVec[ii]=1;
		}
	}
}


/**
 * @brief This function detects spikes in a signal according to the paper by Derek G. Goring (2002).
 * @param uVec input-signal
 * @param timeVec time-vector (same length as uVec)
 * @param nNewSpikes output: the number of new detected spikes.
 * @param return a vector of the same length as uVec, which is set to 1 if there is a spike in the signal, 0 otherwise
 */
std::vector<int> FilterDespikingPS::findSpikesGoring(const std::vector<double>& timeVec, const std::vector<double>& uVec, unsigned int& nNewSpikes)
{
	std::vector<int> spikesVec(uVec.size(),0); //this vector has the same length as uVec. 0 means no spike, 1 means here is a spike.

	//step 1: calculate the first and second derivatives:
	const std::vector<double> duVec( calculateDerivatives(uVec,timeVec) );
	const std::vector<double> du2Vec( calculateDerivatives(duVec,timeVec) );

	//step 2: calculate the standard deviations:
	const double uStdDev = Interpol1D::std_dev( uVec );
	const double duStdDev = Interpol1D::std_dev( duVec );
	const double du2StdDev = Interpol1D::std_dev( du2Vec );

	//step 3: calculate the rotation angle of the principal axis of du2Vec versus uVec:
	const double crossCorrelation = calculateCrossCorrelation(du2Vec, uVec);
	const double theta = atan(crossCorrelation);
	const double cosTheta2 = Optim::pow2( cos(theta) );
	const double sinTheta2 = Optim::pow2( sin(theta) );

	//step 4: calculate ellipses
	const double nElements = static_cast<double>(uVec.size()) - nNodataElements(uVec);
	double universalThreshold = sqrt(2*log(nElements));
	//make the filter a little bit adjustable by the sensitivity parameter
	//the larger the parameter the smaller the threshold and the more spikes are detected
	universalThreshold=universalThreshold/sensitivityParam;

	const double a1 = universalThreshold*uStdDev;
	const double b1 = universalThreshold*duStdDev;
	const double a2 = universalThreshold*duStdDev;
	const double b2 = universalThreshold*du2StdDev;
	double a_[2] = {cosTheta2, sinTheta2};
	double b_[2] = {sinTheta2, cosTheta2};
	double c_[2] = {Optim::pow2(a1), Optim::pow2(b2)};
	double x_[2]={0., 0.};
	solve2X2LinearEquations(a_,b_,c_,x_);
	double a3 = sqrt(x_[0]); //todo: check if x_[0] or x_[1] are negative!
	double b3 = sqrt(x_[1]);

	//step 5: identify the points that lie outside the ellipses:
	findPointsOutsideEllipse(uVec,duVec,a1,b1,0,spikesVec);
	findPointsOutsideEllipse(duVec,du2Vec,a2,b2,0,spikesVec);
	findPointsOutsideEllipse(uVec,du2Vec,a3,b3,theta,spikesVec);

	//step 6: count number of detected spikes:
	nNewSpikes=0;
	for (size_t ii=0; ii<spikesVec.size(); ii++){
		nNewSpikes=nNewSpikes+spikesVec[ii];
	}

	return spikesVec;
}

/**
 * @brief This function finds points (described by xVec, yVec, zVec) which lie outside of the ellipsoid (described by a,b and c)
 *        and sets outsideVec to 1 (if a point lies outside of the ellipsoid).
 *           xVec, yVec, zVec and outsideVec all have the same size.
 *           The condition for a point to be outside the ellipsoid is:
 *               x^2/a^2 + y^2/b^2 + z^2/c^2 > 1
 * @param xVec input x-coordinates
 * @param yVec input y-coordinates
 * @param zVec input z-coordinates
 * @param a input. the major axis of the ellipsoid
 * @param b input. the minor axis of the ellipsoid
 * @param c input. second minor axis of the ellipsoid
 * @param outsideVec output: outsideVector is set to 1 if the point is outside of the ellipsoid, 0 otherwise.
 */
void FilterDespikingPS::findPointsOutsideEllipsoid(const std::vector<double>& xVec,const std::vector<double>& yVec, const std::vector<double>& zVec,
                                                   const double a,const double b,const double c, std::vector<int>& outsideVec)
{
	if(xVec.size() != yVec.size()) return;
	if(xVec.size() != zVec.size()) return;

	for (size_t ii=0; ii<xVec.size(); ii++) {
		const double x = xVec[ii];
		const double y = yVec[ii];
		const double z = zVec[ii];
		if (x!=IOUtils::nodata && y!=IOUtils::nodata && z!=IOUtils::nodata){
			const double helper = (x*x)/(a*a) + (y*y)/(b*b) + (z*z)/(c*c);
			if (helper > 1){
				outsideVec[ii]=1;
			}
		}
	}
}

/**
 * @brief This function detects spikes in a signal according to the paper by Nobuhito Mori (2005), which is a further development of the
 *        algorithm proposed by Goring and Nikora (implemented in FilterDespikingPS::findeSpikesGoring).
 * @param uVec input-signal
 * @param timeVec time-vector (same length as uVec)
 * @param nNewSpikes output: the number of new detected spikes.
 * @param return a vector of the same length as uVec, which is set to 1 if there is a spike in the signal, 0 otherwise
 */
std::vector<int> FilterDespikingPS::findSpikesMori(const std::vector<double>& timeVec, const std::vector<double>& uVec, unsigned int& nNewSpikes)
{
	std::vector<int> spikesVec(uVec.size(),0); //this vector has the same length as uVec. 0 means no spike, 1 means here is a spike.

	//step 1: calculate the first and second derivatives:
	const std::vector<double> duVec( calculateDerivatives(uVec,timeVec) );
	const std::vector<double> du2Vec( calculateDerivatives(duVec,timeVec) );

	//step 2: calculate the universal threshold:
	const double nElements = static_cast<double>(uVec.size()) - nNodataElements(uVec);
	double universalThreshold = sqrt(2*log(nElements));
	//make the filter a little bit adjustable by the sensitivity parameter
	//the larger the parameter the smaller the threshold and the more spikes are detected
	universalThreshold=universalThreshold/sensitivityParam;

	//step 3: calculate the rotation angle of the principal axis of du2Vec versus uVec:
	const double crossCorrelation = calculateCrossCorrelation(du2Vec, uVec);
	const double theta = atan(crossCorrelation);

	//step 4: rotate all points by theta:
	std::vector<double> X(uVec.size(),0);
	std::vector<double> Y(uVec.size(),0);
	std::vector<double> Z(uVec.size(),0);

	for (size_t ii=0; ii<uVec.size(); ii++) {
		if (uVec[ii] != IOUtils::nodata && du2Vec[ii] != IOUtils::nodata){
			X[ii] = uVec[ii]*cos(theta) + du2Vec[ii]*sin(theta);
			Y[ii] = duVec[ii];
			Z[ii] = uVec[ii]*(-sin(theta)) + du2Vec[ii]*cos(theta);
		} else {
			X[ii] = IOUtils::nodata;
			Y[ii] = IOUtils::nodata;
			Z[ii] = IOUtils::nodata;
		}
	}

	//step 5: calculate the standard deviations:
	const double XStdDev = Interpol1D::std_dev( X );
	const double YStdDev = Interpol1D::std_dev( Y );
	const double ZStdDev = Interpol1D::std_dev( Z );
	//major and minor axes of ellipsoid:
	const double a = universalThreshold*XStdDev;
	const double b = universalThreshold*YStdDev;
	const double c = universalThreshold*ZStdDev;

	//step 5: identify the points that lie outside the ellipsoid:
	findPointsOutsideEllipsoid(X,Y,Z,a,b,c,spikesVec);

	//step 6: count number of detected spikes:
	nNewSpikes=0;
	for (size_t ii=0; ii<spikesVec.size(); ii++){
		nNewSpikes=nNewSpikes+spikesVec[ii];
	}

	return spikesVec;
}

/**
 * @brief This function creates a window of data points around the index-position. These points are used as input for interpolation.
 *        Care has to be taken for data points which are noData or spikes.
 *        For numerical reasons the x-values are shifted by the time at the index position.
 * @param index the index around which the window has to be found
 * @param timeVec time-vector (same length as uVec)
 * @param uVec the whole input signal
 * @param spikesVec the vector indicating the spikes
 * @param windowWidth the wished width of the window (in # of data points). The value should be even. Half of the data points are
 *        left of the index and the other half right of the index.
 * @param xVec output: x-coordinates (=time)
 * @param yVec output: y-coordinates (=the values of the signal)
 */
void FilterDespikingPS::getWindowForInterpolation(const size_t index,const std::vector<double>& timeVec, const std::vector<double>& uVec,
                                                  const std::vector<int>& spikesVec, std::vector<double>& xVec, std::vector<double>& yVec)
{
	xVec.clear();
	yVec.clear();
	const unsigned int windowRadius = windowWidth/2;
	size_t ii = index;
	unsigned int nLeftPointsFound=0;
	double timeShift = timeVec[index];
	while (nLeftPointsFound < windowRadius && ii > 0){
		ii--;
		if (uVec[ii] != IOUtils::nodata && spikesVec[ii]==0){
			nLeftPointsFound++;
		}
	}
	while (ii < index){
		if (uVec[ii] != IOUtils::nodata && spikesVec[ii]==0){
			xVec.push_back(timeVec[ii]-timeShift);
			yVec.push_back(uVec[ii]);
		}
		ii++;
	}
	unsigned int nRightPointsFound=0;
	ii=index;
	while (nRightPointsFound < windowRadius && ii < uVec.size()-1){
		ii++;
		if (uVec[ii] != IOUtils::nodata && spikesVec[ii]==0){
			nRightPointsFound++;
			xVec.push_back(timeVec[ii]-timeShift);
			yVec.push_back(uVec[ii]);
		}
	}
}

/**
 * @brief This function checks if a window of data points is sufficient for interpolation.
 * @param xVec a vector of x-values (=time) of the window
 * @param time the time around which the window was build (x-values)
 * @param minPoints the minimum number of points in the window necessary for interpolation (e.g. for a
 *                   quadratic interpolation we need at least 3 points)
 * @param avoidExtrapolation if we want to avoid extrapolation, there should be at least one data point left
 *                               and right of the center time.
 * @param return true if a window which fulfills the requirements was found, false otherwise
 */
bool FilterDespikingPS::checkIfWindowForInterpolationIsSufficient(const std::vector<double>& xVec,const double time,
                                                                  const unsigned int minPoints, const bool avoidExtrapolation)
{
	if(xVec.size()==0) return false;

	if(avoidExtrapolation){
		if (xVec[0]>=time || xVec[xVec.size()-1]<=time){
			return false;
		}
	}
	bool sufficient = false;
	if(xVec.size() >= minPoints){
		sufficient = true;
	}
	return sufficient;
}



/**
 * @brief This function replaces the spikes with fitted values. Here we use a quadratic (!!!) fit for interpolation at the spikes.
 *        Algorithm: We go through the input-signal (uVec), wherever there is a spike (spikesVec[ii] != 0) we replace the spike with
 *        a fitted value. Therefore we take a window of the input-signal (the center of the window is at the spike if possible) and use this
 *        window for interpolation.
 * @param timeVec time-vector (same length as uVec)
 * @param uVec This vector is used as input (signal with spikes) and output (signal with replaced spikes).
 * @param spikesVec The vector indicating the spikes
 */
void FilterDespikingPS::replaceSpikes(const std::vector<double>& timeVec, std::vector<double>& uVec, std::vector<int>& spikesVec)
{
	std::vector<double> xVec;
	std::vector<double> yVec;
	static const unsigned int minPointsForInterpolation = degreeOfInterpolation+1;
	bool avoidExtrapolation = true; //to avoid extrapolation, we need data points left and right of the spike

	for (size_t ii=0; ii<uVec.size(); ii++) {
		if (spikesVec[ii]!=0){ //here we have a spike. replace its value:
			if (degreeOfInterpolation>0) {
				getWindowForInterpolation(ii,timeVec,uVec,spikesVec,xVec,yVec);
				if(checkIfWindowForInterpolationIsSufficient(xVec,0,minPointsForInterpolation,avoidExtrapolation)){
					try{
						//interpolate the spike data point:
						Fit1D polyFit = Fit1D("POLYNOMIAL",xVec,yVec,false);
						polyFit.setDegree(degreeOfInterpolation);
						polyFit.fit();
						double interpolatedValue = polyFit.f(0);
						uVec[ii] = interpolatedValue;
					} catch (const std::exception &e) {
						std::cerr << "An exception occurred: " << e.what() << std::endl;
					}
				} //endif getWindow
			} else {
				uVec[ii] = IOUtils::nodata;
			} //endif degree
		}
	}
}
//------------------------------------ below here are some helper functions: ----------------------------------------------

/**
 * @brief This function solves the 2x2 linear equations.
            a1*x1+b1*x2=c1
            a2*x1+b2*x2=c2
          where a,b and c are given and x has to be determined.
 * @param a input 2-element vector
 * @param b input 2-element vector
 * @param c input 2-element vector
 * @param x output 2-element vector
 */
void FilterDespikingPS::solve2X2LinearEquations(const double* a, const double* b, const double* c, double* x)
{
	const double denominator = a[0]*b[1]-b[0]*a[1];
	if(denominator==0){                     //todo: check if denominator is close to zero!???
		x[0] = IOUtils::nodata;
		x[1] = IOUtils::nodata;
	}else{
		x[0] = (c[0]*b[1]-b[0]*c[1]) / denominator;
		x[1] = (a[0]*c[1]-c[0]*a[1]) / denominator;
	}
}

/**
 * @brief This function creates a double vector out of a MeteoData-vector.
 * @param ivec input meteo-data-vector
 * @param param which values of the meteo-data-vector do you want to get (e.g. temperature)
 */
const std::vector<double> FilterDespikingPS::helperGetDoubleVectorOutOfMeteoDataVector(const std::vector<MeteoData>& ivec,
                                                                                      const unsigned int& param)
{
	std::vector<double> ovec( ivec.size() );
	for (size_t ii=0; ii<ivec.size(); ii++) {
		const MeteoData meteoValue = ivec[ii];
		ovec[ii] = meteoValue(param);
	}
	return ovec;
}

/**
 * @brief This function creates a double vector out of a MeteoData-vector containing the time of each data point.
 *        The time is shifted so that the first time is 0 and scaled so that the first time step is 1.
 *        The shifting and scaling is done for numerical reasons.
 * @param ivec input meteo-data-vector
 */
const std::vector<double> FilterDespikingPS::helperGetTimeVectorOutOfMeteoDataVector(const std::vector<MeteoData>& ivec)
{
	std::vector<double> ovec( ivec.size() );
	double time0 = 0;
	double dt = 1;
	if (ivec.size()>1){
		time0 = ivec[0].date.getJulian();
		dt = ivec[1].date.getJulian() - ivec[0].date.getJulian();
	}
	for (size_t ii=0; ii<ivec.size(); ii++) {
		const MeteoData meteoValue = ivec[ii];
		const double time = (meteoValue.date.getJulian()-time0)/dt;
		ovec[ii] = time;
	}
	return ovec;
}

} //end namespace
