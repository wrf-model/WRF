/*
 * jnative.h
 *
 *  Created on: 08.01.2010
 *      Author: perot
 */


#ifndef _Included_JNative
#define _Included_JNative

#if  defined(_METEOIO_JNI) ||  defined(_METEOIO_JNA)

#include <meteoio/MeteoIO.h>


void loadMeteoAndStationData(double* cMetadata, double* cData,
		const int nbStation, const int nbDataPerStation,
		const std::string& algorithm,
		const std::string metaCoordinateSystem, std::vector<mio::StationData>& vecStation, 
                std::vector<mio::MeteoData>& vecData, enum mio::MeteoData::Parameters& interpolation_type);

void fulfillDoubleArray(const mio::Grid2DObject&  p, const std::string& cellOrder, double* dest);


#endif // defined(_METEOIO_JNI) ||  defined(_METEOIO_JNA)


#ifdef _METEOIO_JNA

/**
 *
 * Originally, these methods are dedicated to be called from JAVA with JNA framework.
 *
 *
 */


double* executeInterpolationSubDem
  (char*, char*, char*,char*, double, double, double, double,double*, int, double*, int, char*, char*);


double* executeInterpolation
(char*, char*, char*, char*, double*, int, double*, int, char*, char*);


#endif //_METEOIO_JNA

#endif//_Included_JNative
