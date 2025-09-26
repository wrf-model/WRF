INCLUDE_DIRECTORIES("${PROJECT_SOURCE_DIR}/")
LIST(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/tools/cmake/")

CONFIGURE_FILE( ${PROJECT_SOURCE_DIR}/meteoio/IOHandler.cmake.cc ${PROJECT_SOURCE_DIR}/meteoio/IOHandler.cc )

IF(PLUGIN_ALPUG)
	SET(plugins_sources ${plugins_sources} plugins/ALPUG.cc)
ENDIF(PLUGIN_ALPUG)

IF(PLUGIN_ARCIO)
	SET(plugins_sources ${plugins_sources} plugins/ARCIO.cc)
ENDIF(PLUGIN_ARCIO)

IF(PLUGIN_ARGOSIO)
	SET(plugins_sources ${plugins_sources} plugins/Argos.cc)
ENDIF(PLUGIN_ARGOSIO)

IF(PLUGIN_A3DIO)
	SET(plugins_sources ${plugins_sources} plugins/A3DIO.cc)
ENDIF(PLUGIN_A3DIO)

IF(PLUGIN_ARPSIO)
	SET(plugins_sources ${plugins_sources} plugins/ARPSIO.cc)
ENDIF(PLUGIN_ARPSIO)

IF(PLUGIN_COSMOXMLIO)
	FIND_PACKAGE(LibXml2 REQUIRED)
	INCLUDE_DIRECTORIES(SYSTEM ${LIBXML2_INCLUDE_DIR})
	SET(plugin_libs ${plugin_libs} ${LIBXML2_LIBRARIES})
	SET(plugins_sources ${plugins_sources} plugins/CosmoXMLIO.cc)
ENDIF(PLUGIN_COSMOXMLIO)

IF(PLUGIN_CSVIO)
	SET(plugins_sources ${plugins_sources} plugins/CsvIO.cc)
ENDIF(PLUGIN_CSVIO)

IF(PLUGIN_DBO)
	FIND_PACKAGE(CURL REQUIRED)
	INCLUDE_DIRECTORIES(SYSTEM ${CURL_INCLUDE_DIRS})
	SET(plugin_libs ${plugin_libs} ${CURL_LIBRARIES})
	SET(plugins_sources ${plugins_sources} plugins/DBO.cc)
ENDIF(PLUGIN_DBO)

IF(PLUGIN_GOESIO)
	SET(plugins_sources ${plugins_sources} plugins/Goes.cc)
ENDIF(PLUGIN_GOESIO)

IF(PLUGIN_GRASSIO)
	SET(plugins_sources ${plugins_sources} plugins/GrassIO.cc)
ENDIF(PLUGIN_GRASSIO)

IF(PLUGIN_GRIBIO)
	FIND_PACKAGE(ecCodes REQUIRED)
	INCLUDE_DIRECTORIES(SYSTEM ${ecCodes_INCLUDE_DIRS})
	SET(plugin_libs ${plugin_libs} ${ecCodes_LIBRARIES})
	SET(plugins_sources ${plugins_sources} plugins/GRIBIO.cc)
ENDIF(PLUGIN_GRIBIO)

IF(PLUGIN_GEOTOPIO)
	SET(plugins_sources ${plugins_sources} plugins/GeotopIO.cc)
ENDIF(PLUGIN_GEOTOPIO)

IF(PLUGIN_IMISIO)
	FIND_PACKAGE(OCCI REQUIRED)
	INCLUDE_DIRECTORIES(SYSTEM ${OCCI_INCLUDE_DIR})
	SET(plugin_libs ${plugin_libs} ${OCCI_LIBRARIES})
	SET(plugins_sources ${plugins_sources} plugins/ImisIO.cc)
ENDIF(PLUGIN_IMISIO)

IF(PLUGIN_OSHDIO)
	FIND_PACKAGE(Matio REQUIRED)
	INCLUDE_DIRECTORIES(SYSTEM ${Matio_INCLUDE_DIR})
	SET(plugin_libs ${plugin_libs} ${Matio_LIBRARY})
	SET(plugins_sources ${plugins_sources} plugins/libMatioWrapper.cc plugins/OshdIO.cc)
ENDIF(PLUGIN_OSHDIO)

IF(PLUGIN_PGMIO)
	SET(plugins_sources ${plugins_sources} plugins/PGMIO.cc)
ENDIF(PLUGIN_PGMIO)

IF(PLUGIN_PMODIO)
	SET(plugins_sources ${plugins_sources} plugins/PmodIO.cc)
ENDIF(PLUGIN_PMODIO)

IF(PLUGIN_PNGIO)
	FIND_PACKAGE(PNG REQUIRED)
	INCLUDE_DIRECTORIES(SYSTEM ${PNG_INCLUDE_DIR})
	SET(plugin_libs ${plugin_libs} ${PNG_LIBRARY})
	SET(plugins_sources ${plugins_sources} plugins/PNGIO.cc)
ENDIF(PLUGIN_PNGIO)

IF(PLUGIN_PSQLIO)
	FIND_PACKAGE(PostgreSQL REQUIRED)
	INCLUDE_DIRECTORIES(SYSTEM ${PostgreSQL_INCLUDE_DIR})
	SET(plugin_libs ${plugin_libs} ${PostgreSQL_LIBRARY})
	SET(plugins_sources ${plugins_sources} plugins/PSQLIO.cc)
ENDIF(PLUGIN_PSQLIO)

IF(PLUGIN_SASEIO)
	FIND_PACKAGE(MySQL REQUIRED)
	INCLUDE_DIRECTORIES(SYSTEM ${MYSQL_INCLUDE_DIR})
	SET(plugin_libs ${plugin_libs} ${MYSQL_LIBRARY})
	SET(plugins_sources ${plugins_sources} plugins/SASEIO.cc)
ENDIF(PLUGIN_SASEIO)

IF(PLUGIN_NETCDFIO)
	FIND_PACKAGE(NetCDF REQUIRED)
	INCLUDE_DIRECTORIES(SYSTEM ${NETCDF_INCLUDE_DIR})
	SET(plugin_libs ${plugin_libs} ${NETCDF_LIBRARIES})
	SET(plugins_sources ${plugins_sources} plugins/libncpp.cc)
	SET(plugins_sources ${plugins_sources} plugins/NetCDFIO.cc)
ENDIF(PLUGIN_NETCDFIO)

IF(PLUGIN_SMETIO)
	SET(plugins_sources ${plugins_sources} plugins/SMETIO.cc)
ENDIF(PLUGIN_SMETIO)

IF(PLUGIN_SNIO)
	SET(plugins_sources ${plugins_sources} plugins/SNIO.cc)
ENDIF(PLUGIN_SNIO)

IF(PLUGIN_ZRXPIO)
	SET(plugins_sources ${plugins_sources} plugins/ZRXPIO.cc)
ENDIF(PLUGIN_ZRXPIO)
