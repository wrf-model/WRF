INCLUDE_DIRECTORIES("${PROJECT_SOURCE_DIR}/")

INCLUDE("${PROJECT_SOURCE_DIR}/meteoio/thirdParty/CMakeLists.txt")
INCLUDE("${PROJECT_SOURCE_DIR}/meteoio/dataClasses/CMakeLists.txt")
INCLUDE("${PROJECT_SOURCE_DIR}/meteoio/plugins/CMakeLists.txt")
INCLUDE("${PROJECT_SOURCE_DIR}/meteoio/meteoLaws/CMakeLists.txt")
INCLUDE("${PROJECT_SOURCE_DIR}/meteoio/meteoStats/CMakeLists.txt")
INCLUDE("${PROJECT_SOURCE_DIR}/meteoio/meteoFilters/CMakeLists.txt")
INCLUDE("${PROJECT_SOURCE_DIR}/meteoio/meteoResampling/CMakeLists.txt")
INCLUDE("${PROJECT_SOURCE_DIR}/meteoio/spatialInterpolations/CMakeLists.txt")
INCLUDE("${PROJECT_SOURCE_DIR}/meteoio/dataGenerators/CMakeLists.txt")

IF(PROJ4)
	FIND_PATH(LIBPROJ_INCLUDE_DIR "proj_api.h")
	FIND_LIBRARY(LIBPROJ proj)
	INCLUDE_DIRECTORIES(${LIBPROJ_INCLUDE_DIR})
	IF(MSVC)
		ADD_DEFINITIONS(/DPROJ4) #it looks like some VC++ versions don't support -D syntax
	ELSE(MSVC)
		ADD_DEFINITIONS(-DPROJ4)
	ENDIF(MSVC)
ENDIF(PROJ4)

SET(meteoio_sources
	${thirdParty_sources}
	${dataClasses_sources}
	Timer.cc
	Config.cc
	IOExceptions.cc
	IOUtils.cc
	FileUtils.cc
	Graphics.cc
	GridsManager.cc
	TimeSeriesManager.cc
	IOManager.cc
	IOHandler.cc
	MeteoProcessor.cc
	Meteo1DInterpolator.cc
	Meteo2DInterpolator.cc
	DataCreator.cc
	DataGenerator.cc
	plugins/libsmet.cc
	IOInterface.cc
	${meteoResampling_sources}
	${spatialInterpolations_sources}
	${plugins_sources}
	${meteoLaws_sources}
	${meteoStats_sources}
	${meteoFilters_sources}
	${dataGenerators_sources}
)

IF(!MSVC)
	set_source_files_properties(IOUtils.cc PROPERTIES COMPILE_FLAGS -Wno-date-time)
ENDIF(!MSVC)

IF(GUI_EXCEPTIONS)
	IF(MSVC)
		ADD_DEFINITIONS(/DMSG_BOX)
	ELSE(MSVC)
		ADD_DEFINITIONS(-DMSG_BOX)
	ENDIF(MSVC)
	IF(UNIX AND NOT APPLE AND NOT CYGWIN)
		FIND_PACKAGE(X11)
		IF(X11_FOUND)
			INCLUDE_DIRECTORIES(SYSTEM ${X11_INCLUDE_DIR})
			SET(GUI_LIBS ${X11_LIBRARIES})
			LIST(APPEND meteoio_sources MessageBoxX11.cc)
		ELSE(X11_FOUND)
			SET(GUI_EXCEPTIONS) #unsets the variable
		ENDIF(X11_FOUND)
	ENDIF(UNIX AND NOT APPLE AND NOT CYGWIN)
ENDIF(GUI_EXCEPTIONS)

#shared library.
IF(BUILD_SHARED_LIBS)
	SET(SHAREDNAME ${PROJECT_NAME})
	ADD_LIBRARY(${SHAREDNAME} ${meteoio_sources})
	TARGET_LINK_LIBRARIES(${SHAREDNAME} ${plugin_libs} ${LIBPROJ} ${EXTRA_LINK_FLAGS} ${GUI_LIBS})
	SET_TARGET_PROPERTIES(${SHAREDNAME} PROPERTIES
		MACOSX_RPATH TRUE
		PREFIX "${LIBPREFIX}"
		LIBRARY_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/lib"
		ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/lib"
		RUNTIME_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/lib"
		CLEAN_DIRECT_OUTPUT 1
		VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}"
		SOVERSION "${VERSION_MAJOR}"
	)
	INSTALL(TARGETS ${SHAREDNAME}
		RUNTIME DESTINATION lib COMPONENT libraries
		LIBRARY DESTINATION lib COMPONENT libraries
		ARCHIVE DESTINATION lib COMPONENT libraries
	)
ENDIF(BUILD_SHARED_LIBS)

#static library. Please keep in mind that for Windows, a dll is a runtime
IF(BUILD_STATIC_LIBS)
	SET(STATICNAME ${PROJECT_NAME}_STATIC)
	SET(STATICLIBNAME ${PROJECT_NAME})
	ADD_LIBRARY(${STATICNAME} STATIC ${meteoio_sources})
	TARGET_LINK_LIBRARIES(${STATICNAME} ${plugin_libs} ${LIBPROJ} ${EXTRA_LINK_FLAGS} ${GUI_LIBS})
	SET_TARGET_PROPERTIES(${STATICNAME} PROPERTIES
		MACOSX_RPATH TRUE
		PREFIX "${LIBPREFIX}"
		LIBRARY_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/lib"
		ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/lib"
		RUNTIME_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/lib"
		CLEAN_DIRECT_OUTPUT 1
		OUTPUT_NAME ${STATICLIBNAME}
	)
	INSTALL(TARGETS ${STATICNAME}
		RUNTIME DESTINATION lib COMPONENT libraries
		LIBRARY DESTINATION lib COMPONENT libraries
		ARCHIVE DESTINATION lib COMPONENT libraries
	)
ENDIF(BUILD_STATIC_LIBS)

