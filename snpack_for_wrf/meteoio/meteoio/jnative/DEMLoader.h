/*
 * DEMLoader.h
 *
 *  Created on: 19.01.2010
 *      Author: perot
 */
#ifndef _Included_DEMLoader
#define _Included_DEMLoader

#include <meteoio/IOInterface.h>
#include <meteoio/Grid2DObject.h>
#include <meteoio/dataClasses/DEMObject.h>
#include <meteoio/dataClasses/Coords.h>


typedef std::map<std::string, mio::DEMObject>  demMapType;
typedef std::pair<std::string, mio::DEMObject>  demPairType;


/**
 * Class with an unique instance loading and storing DEMobjects.
 */
class DEMLoader
{
private:
  DEMLoader () { }
  ~DEMLoader () {
	  demMap.clear();
  }


  static DEMLoader *getInstance (){
    if (NULL == singleton){
        singleton =  new DEMLoader;
     }
    return singleton;
  }

  const mio::DEMObject& internal_loadFullDEM(const std::string cDemFile,
 			const std::string  cDemCoordSystem, const std::string cInterfaceType);

  const mio::DEMObject& internal_loadSubDEM(const std::string cDemFile,
  			const std::string  cDemCoordSystem, const std::string cInterfaceType ,
  			const double xll , const double yll, const double xur, const double yur);

  /**
   * Returns an implementation of IOInterface with the appropriate
   * configuration. Each call generates a new Instance -> delete it after use !
   * TODO : Store the instances and reuse them, even in a concurent acceses context.
   *        Pb of the Config member which cannot be mutualized throught different interplation !
   *
   */
  mio::IOInterface* generateIOInterface(const std::string cDemFile,
		  const std::string  cDemCoordSystem,
		  const std::string cInterfaceType);

public:

	static const mio::DEMObject& loadFullDEM(const std::string cDemFile,
			const std::string  cDemCoordSystem, const std::string cInterfaceType){
		return DEMLoader::getInstance()->internal_loadFullDEM(cDemFile, cDemCoordSystem, cInterfaceType);
	}

	static const mio::DEMObject& loadSubDEM(const std::string cDemFile,
			const std::string  cDemCoordSystem, const std::string cInterfaceType ,
			const double xll , const double yll, const double xur, const double yur){

		return DEMLoader::getInstance()->internal_loadSubDEM(cDemFile,
				cDemCoordSystem,cInterfaceType , xll , yll, xur,yur);
	}

	static const mio::Config& getConfig(){
		return getInstance ()->cfg;
	}

	/**
	* Public static access to destroy (interest ?!?)
	*/
	static void kill (){
		if (NULL != singleton) {
			delete singleton; //call destructor
			singleton = NULL;
		  }
	  }

private:

	/**
	 * Structure containing the DEMObjects. The key is the dem file name.
	 */
	demMapType  demMap;

	/**
	 * Shared Config for every interpolation
	 */
	mio::Config cfg;

	/**
	 * Static pointer to the unique instance, if null the instance had not been initialized yet
	 */
	static DEMLoader *singleton;
};

#endif
