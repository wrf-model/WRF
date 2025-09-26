/*
 * jnative.cc
 *
 *  Created on: 08.01.2010
 *      Author: perot
 */


#if  defined(_METEOIO_JNI) ||  defined(_METEOIO_JNA)

#include "jnative.h"
#include "DEMLoader.h"
#include <meteoio/MeteoIO.h>

#include <time.h>

using namespace mio;

const double nodata=-999.;

void loadMeteoAndStationData(double* cMetadata, double* cData,
		const int nbStation, const int nbDataPerStation,
		const std::string& algorithm,
		const std::string metaCoordinateSystem,
		std::vector<StationData>& vecStation, std::vector<MeteoData>& vecData,
		enum MeteoData::Parameters& interpolation_type){

	std::cerr << "loadMeteoAndStationData "  << algorithm << "*"  << nbStation <<  std::endl;

	const Date date_in;
	Coords position(metaCoordinateSystem, "");

	for (int i = 0; i < nbStation; i++) {
		const double latitude = cMetadata[3*i];
		const double longitude = cMetadata[3*i+1];
		const double altitude = cMetadata[3*i+2];

		//building StationData
		if (altitude<nodata ) continue;
		position.setXY(latitude, longitude, altitude);
		const StationData station(position, "");
		vecStation.push_back(station);

		//building MeteoData
		double p=IOUtils::nodata, psum=IOUtils::nodata, ta=IOUtils::nodata, rh=IOUtils::nodata;
		double vw=IOUtils::nodata, dw=IOUtils::nodata, iswr=IOUtils::nodata, ilwr=IOUtils::nodata;
		double tsg=IOUtils::nodata, tss=IOUtils::nodata, hs=IOUtils::nodata, rswr=IOUtils::nodata;
		if (algorithm =="P"){
			interpolation_type=MeteoData::P;
			p=cData[nbDataPerStation*i];
		} else if (algorithm =="PSUM" ){
			interpolation_type=MeteoData::PSUM;
			psum=cData[nbDataPerStation*i];
		} else if (algorithm =="TA" ){
			interpolation_type=MeteoData::TA;
			ta=cData[nbDataPerStation*i];
		} else if (algorithm =="RH" ) {
			interpolation_type=MeteoData::RH;
			rh=cData[nbDataPerStation*i];
			if (nbDataPerStation>1) ta=cData[nbDataPerStation*i+1];
		} else if (algorithm =="VW" ){
			interpolation_type=MeteoData::VW;
			vw=cData[nbDataPerStation*i];
			if (nbDataPerStation>1) dw=cData[nbDataPerStation*i+1];
		} else if (algorithm =="DW" ){
			interpolation_type=MeteoData::DW;
			dw=cData[nbDataPerStation*i];
		} else if (algorithm =="ISWR" ){
			interpolation_type=MeteoData::ISWR;
			iswr=cData[nbDataPerStation*i];
		} else if (algorithm =="ILWR"){
			interpolation_type=MeteoData::ILWR;
			ilwr=cData[nbDataPerStation*i];
		} else {
			throw InvalidArgumentException("Invalid interpolation algorithm selected!", AT);
		}
		MeteoData meteo(date_in);
		meteo(MeteoData::TA)   = ta;
		meteo(MeteoData::ISWR) = iswr;
		meteo(MeteoData::VW)   = vw;
		meteo(MeteoData::DW)   = dw;
		meteo(MeteoData::RH)   = rh;
		meteo(MeteoData::ILWR) = ilwr;
		meteo(MeteoData::PSUM)  = psum;
		meteo(MeteoData::TSG)  = tsg;
		meteo(MeteoData::TSS)  = tss;
		meteo(MeteoData::HS)   = hs;
		meteo(MeteoData::RSWR) = rswr;
		meteo(MeteoData::P)    = p;
		vecData.push_back(meteo);
	}
}

void fulfillDoubleArray(const Grid2DObject&  p,
		const std::string& cellOrder,
		double* dest){

	dest[0] = 1.;//code for success
	dest[1] = p.ncols; //width
	dest[2] = p.nrows; //height
	dest[3] = 1.; //reserved ...
	dest[4] = 1.; //reserved ...
	dest[5] = 1.; //reserved ...

	if (cellOrder == "llur"){
		size_t knx = 6;
		size_t iknx = 0;
		for (size_t kk = 0; kk < p.nrows; kk++){
			for (size_t ll=0; ll < p.ncols; ll++)
				dest[knx + ll] = p(ll+iknx);
			knx+=p.ncols;
			iknx+=p.ncols;
		}
	}
	else if (cellOrder == "urll" ){
		size_t knx = (p.nrows-1)*p.ncols+6;
		size_t iknx = 0;
		for (size_t kk = p.nrows-1; kk >=0; kk--){
			for (size_t ll=p.ncols -1; ll >=0; ll--)
				dest[knx + ll] = p(p.ncols -1-ll+iknx);
			knx-=p.ncols;
			iknx+=p.ncols;
		}
	}
	else if (cellOrder == "lrul" ){
		size_t knx = 6;
		size_t iknx = 0;
		for (size_t kk = 0; kk < p.nrows; kk++){
			for (size_t ll=p.ncols -1; ll >=0; ll--)
				dest[knx+ ll] = p(p.ncols -1-ll+iknx);
			knx+=p.ncols;
			iknx+=p.ncols;
		}
	}
	else if (cellOrder == "ullr"){
		size_t knx = (p.nrows-1)*p.ncols+6;
		size_t iknx = 0;
		for (size_t kk = (signed)p.nrows-1; kk >=0; kk--){
			for (size_t ll=0; ll < p.ncols; ll++)
				dest[knx + ll] = p(ll+iknx);
			knx-=p.ncols;
			iknx+=p.ncols;
		}
	}
	else{
		size_t knx = 6;
		size_t iknx = 0;
		for (size_t kk = 0; kk < p.nrows; kk++){
			for (size_t ll=0; ll < p.ncols; ll++)
				dest[knx + ll] = p(ll+iknx);
			knx+=p.ncols;
			iknx+=p.ncols;
		}
	}
}

#endif


#ifdef _METEOIO_JNA

double* makeError ( double errorCode){
	//PS. the array allocated here should be automatically deleted (and memory freed) when the
	//Java mapped object (com.sun.jna.ptr.DoubleByReference here) is "Garbage Collected"
	double* dest = (double*) malloc( sizeof(double));
	dest[0] = errorCode;
	return dest;
}

double* executeInterpolationSubDem(char* algorithm, char* iointerface,
		  char* demFile,char* demCoordSystem,
		  double demXll, double demYll, double demXrt, double demYrt,
		  double* metadata, int nbStation,
		  double* data, int nbDataPerStation, char* metaCoordSystem, char* cellOrder){

	clock_t tmpStart;
	clock_t tmpEnd;

	//get dem
	tmpStart = clock(); //start
	const DEMObject& dem = (demXll > -1 && demYrt> -1)?
			DEMLoader::loadSubDEM(demFile, demCoordSystem, iointerface, demXll, demYll, demXrt, demYrt) :
				DEMLoader::loadFullDEM(demFile, demCoordSystem, iointerface);
	if (dem.ncols<1 || dem.ncols<2  ){
		std::cerr << "Problem with DEM creation : "  << std::endl;
		//error
		return makeError(-2.);
	}
	tmpEnd = clock(); //end
	const double msDemLoading = (tmpEnd - tmpStart)/1000.0;


	//Create MeteoData and StationData vectors
	tmpStart = clock(); //start
	std::vector<MeteoData> vecData;
	std::vector<StationData> vecStation;
	enum MeteoData::Parameters interpolation_type;
	//initialize MeteoData and StationData vectors
	loadMeteoAndStationData(metadata, data, nbStation, nbDataPerStation, algorithm,
		metaCoordSystem, vecStation, vecData, interpolation_type);
	tmpEnd = clock(); //end
	const double msDataLoading = (tmpEnd - tmpStart)/1000.0;


	//Interpolation
	tmpStart = clock(); //start
	Grid2DObject  p(dem.ncols, dem.nrows, dem.cellsize, dem.llcorner);
	bool success = true;
	try {
		const Config& cfg = DEMLoader::getConfig();
		Meteo2DInterpolator mi(cfg, dem, vecData, vecStation);
		mi.interpolate(interpolation_type, p);
	}
	catch(const IOException){
		std::cerr << "Interpolation failed " << std::endl;
		success = false;
	}
	catch(...){
		std::cerr << "Interpolation failed for some reason ?!? " <<  std::endl;
		success = false;
	}

	//PS. the array allocated here should be automatically deleted (and memory freed) when the
	//Java mapped object (com.sun.jna.ptr.DoubleByReference here) is "Garbage Collected"
	double* out = (success)?
			(double*) malloc( (6 + p.nrows*p.ncols)* sizeof(double)):
			(double*) malloc( 6 * sizeof(double));
	//copy the interpolation result into a double array
	if (success)
		fulfillDoubleArray( p, cellOrder, out);
	else{
		out[0] = -1.;//code for failing
		out[1] = 0;
		out[2] = 0;
	}

	tmpEnd = clock(); //end
	const double msInterpolation = (tmpEnd - tmpStart)/1000.0;

	std::cerr << " - time to load DEM : "  << msDemLoading << std::endl;
	std::cerr << " - time to load Data : "  << msDataLoading << std::endl;
	std::cerr << " - time to interpolate: "  << msInterpolation << std::endl;
	//put the different process in the result
	out[3] = msDemLoading;
	out[4] = msDataLoading;
	out[5] = msInterpolation;

	vecData.clear();
	vecStation.clear();
	return out;
}

double* executeInterpolation(char* algorithm, char* iointerface,
		  char* demFile,char* demCoordSystem,
		  double* metadata, int nbStation,
		  double* data, int nbDataPerStation,
		  char* metaCoordSystem, char* cellOrder){

	return executeInterpolationSubDem(algorithm,  iointerface, demFile,demCoordSystem,
			  -1.,-1.,-1.,-1.,
			 metadata, nbStation,
			 data, nbDataPerStation,
			 metaCoordSystem, cellOrder);
}

#endif
